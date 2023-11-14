unit paxhttp.client;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, paxhttp.Messages, paxhttp.encoders, ssockets, fgl;

const
  ReadBufLen = 1024 * 1024;

type
  TDefaultHTTPClient = class;

  { TMiddleware }

  TMiddleware = class
  private
    Factive: boolean;
    procedure Setactive(AValue: boolean);
  protected
    function process(aClient: TDefaultHttpClient; aRequest: THttpRequest; aResponse: THttpResponse; args: TStrings): boolean; virtual; abstract;
  public
    constructor Create; virtual;
    property active: boolean read Factive write Setactive;
  end;

  { TAuthorizationMiddleware }

  TAuthorizationMiddleware = class(TMiddleware)
  private
    FPassword: string;
    FUserName: string;
    procedure SetPassword(AValue: string);
    procedure SetUserName(AValue: string);
  public
    property UserName: string read FUserName write SetUserName;
    property Password: string read FPassword write SetPassword;
  end;

  TMiddlewareList = specialize TFPGList<TMiddleware>;
  TEncoderList = specialize TFPGList<TEncoder>;


  { TDefaultHTTPClient }

  TDefaultHTTPClient = class(TPersistent)
  private
    FAjaxMiddleware: TMiddleware;
    FAuthorization: TAuthorizationMiddleware;
    FKeepAlive: boolean;
    FProxy: TAuthorizationMiddleware;
    FRedirect: TMiddleware;
    function GetAjax: boolean;
    procedure SetAjax(AValue: boolean);
    procedure SetKeepAlive(AValue: boolean);
    procedure SetTimeOut(AValue: int64);
  protected
    FSocket: TInetSocket;
    FCurrentHandler: TSocketHandler;
    FTimeOut: int64;
    FKeepConnection: boolean;
    FHeadersOnly: boolean;
    FTerminated: boolean;
    FBuffer: ansistring;
    FPreprocessList: TMiddlewareList;
    FPostProcessList: TMiddlewareList;
    FEncoders: TEncoderList;
  protected
    function ParseStatusLine(var AResponse: THTTPResponse; AStatusLine: string): integer;
    function ReadResponseHeaders(var AResponse: THTTPResponse): integer;
    function ReadString(out StringResult: string): boolean;
    function GetSocketHandler(const UseSSL: boolean): TSocketHandler; virtual;
    procedure connect(AHost: string; aPort: word; UseSSL: boolean);
    procedure disconnect;
    function prepareHeader(aRequest: THTTPRequest): string; virtual;
    procedure handleKeepConnection(var aRequest: THTTPRequest);
    function Terminated: boolean;
    procedure sendRequest(aRequest: THTTPRequest; var aResponse: THTTPResponse); virtual;
    procedure receiveResponse(aRequest: THTTPRequest; var aResponse: THTTPResponse); virtual;
    procedure resetResponse(var aResponse: THTTPResponse); virtual;
    function isSSL(aValue: string): boolean;
    function processNormalRequest(aRequest: THTTPRequest; var aResponse: THTTPResponse): word;
    function processKeepAliveRequest(aRequest: THTTPRequest; var aResponse: THTTPResponse): word;
    function getServerUrl(ARequest: THTTPRequest): string;
  protected
    function performRequest(aRequest: THTTPRequest; var aResponse: THTTPResponse): word; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AbortRequest;
    function request(aRequest: THTTPRequest; var aResponse: THTTPResponse): word; virtual;
    procedure addPostProcessMiddleware(aMiddleware: TMiddleware);
    procedure addPreProcessMiddleware(aMiddleware: TMiddleware);
    procedure removePostProcessMiddleware(aMiddleware: TMiddleware);
    procedure removePreProcessMiddleware(aMiddleware: TMiddleware);
    property KeepAlive: boolean read FKeepAlive write SetKeepAlive;
    property TimeOut: int64 read FTimeOut write SetTimeOut;

    property Ajax: boolean read GetAjax write SetAjax;
    property Redirect: TMiddleware read FRedirect;

    property Authorization: TAuthorizationMiddleware read FAuthorization write FAuthorization;
    property Proxy: TAuthorizationMiddleware read FProxy;
  end;

implementation

uses
  base64, paxhttp.middlewares
  {$if not defined(hasamiga)}
  , sslsockets, openssl
  {$IF FPC_FULLVERSION>=30200}
  , sslbase
  {$IFNDEF USEGNUTLS}
  , fpopenssl, opensslsockets
  {$else}
  , gnutls, gnutlssockets
  {$endif}
  {$endif}
{$endif} ;

{ TAuthorizationMiddleware }

procedure TAuthorizationMiddleware.SetPassword(AValue: string);
begin
  if FPassword = AValue then
    Exit;
  FPassword := AValue;
end;

procedure TAuthorizationMiddleware.SetUserName(AValue: string);
begin
  if FUserName = AValue then
    Exit;
  FUserName := AValue;
end;


{ TMiddleware }

procedure TMiddleware.Setactive(AValue: boolean);
begin
  if Factive = AValue then
    Exit;
  Factive := AValue;
end;

constructor TMiddleware.Create;
begin
  Factive := True;
end;


{ TDefaultHTTPClient }

function TDefaultHTTPClient.GetSocketHandler(const UseSSL: boolean): TSocketHandler;
begin
  Result := nil;
  if (Result = nil) then
  {$if not defined(HASAMIGA)}
    if UseSSL then
    begin
      InitSSLInterface;
      {$IF FPC_FULLVERSION>=30200}
      Result := TSSLSocketHandler.GetDefaultHandler;
     {$else}
      Result := TSSLSocketHandler.Create;
      {$EndIf}
    end
    else
  {$endif}
    begin
      Result := TSocketHandler.Create;
    end;
  FCurrentHandler := Result;
end;

procedure TDefaultHTTPClient.connect(AHost: string; aPort: word; UseSSL: boolean);
var
  G: TSocketHandler;
begin
  if assigned(FCurrentHandler) and FCurrentHandler.Connect then
    disconnect;
  if (Aport = 0) then
    if UseSSL then
      Aport := 443
    else
      Aport := 80;
  G := GetSocketHandler(UseSSL);
  FSocket := TInetSocket.Create(AHost, APort, G);
  try
    if FTimeOut > 0 then
      FSocket.IOTimeout := FTimeOut;
    FSocket.Connect;
  except
    FreeAndNil(FSocket);
    raise;
  end;
end;

procedure TDefaultHTTPClient.disconnect;
begin
  FreeAndNil(FSocket);
  FCurrentHandler := nil;
end;

function TDefaultHTTPClient.prepareHeader(aRequest: THTTPRequest): string;
var
  buffer: ansistring;
var
  headerString: string;
  header: THTTPHeader;
begin
  buffer := (uppercase(aRequest.Method) + ' ' + getServerUrl(aRequest) + ' HTTP/' + aRequest.Version) + CRLF;
  if Assigned(aRequest.Body) and (StrToInt64Def(aRequest.getHeader('Content-Length'), -1) = -1) then
    aRequest.setHeader('Content-Length', aRequest.Body.Size.ToString);
  for header in aRequest.getHeaders() do
  begin
    headerString := header.toString;
    buffer += headerString + CRLF;
  end;
  Result := Buffer + CRLF;
end;

procedure TDefaultHTTPClient.handleKeepConnection(var aRequest: THTTPRequest);
begin
  if FKeepConnection then
    aRequest.setHeader('Connection', 'Keep-Alive')
  else
    aRequest.setHeader('Connection', 'Close');
end;

function TDefaultHTTPClient.Terminated: boolean;
begin
  Result := FTerminated;
end;

constructor TDefaultHTTPClient.Create;
begin
  FSocket := nil;
  FTimeOut := -1;
  FKeepConnection := True;
  FTerminated := False;
  FPostProcessList := TMiddlewareList.Create;
  FPreprocessList := TMiddlewareList.Create;

  FAjaxMiddleware := TAjaxMiddlewarePreprocess.Create;
  FProxy := TProxyRequestPreprocess.Create();
  FAuthorization := TBasicAuthorizationRequestPreprocess.Create;
  FPreprocessList.Add(TCookieMiddlewarePreprocess.Create);
  FPreprocessList.Add(THostPreprocess.Create);
  FPreprocessList.Add(FAjaxMiddleware);
  FPreprocessList.Add(FProxy);
  FPreprocessList.Add(FAuthorization);

  FRedirect := TRedirectPostProcess.Create;

  FPostProcessList.Add(FRedirect);
  FEncoders := TEncoderList.Create;
  FEncoders.Add(TEncoderWWWFormUrlEncoded.Create);
  FEncoders.Add(TEncoderMultiPartFormData.Create);
  FAuthorization.active := False;
  FAjaxMiddleware.active := False;
  FProxy.active := False;
end;

destructor TDefaultHTTPClient.Destroy;
var
  idx: integer;
  m: TMiddleware;
  e: TEncoder;
begin
  disconnect;
  for idx := FPreprocessList.Count - 1 downto 0 do
  begin
    M := FPreprocessList[idx];
    FPreprocessList.Delete(idx);
    FreeAndNil(M);
  end;
  for idx := FPostProcessList.Count - 1 downto 0 do
  begin
    M := FPostProcessList[idx];
    FPostProcessList.Delete(idx);
    FreeAndNil(M);
  end;
  for idx := FEncoders.Count - 1 downto 0 do
  begin
    e := FEncoders[idx];
    FEncoders.Delete(idx);
    FreeAndNil(e);
  end;
  FreeAndNil(FEncoders);
  FreeAndNil(FPostProcessList);
  FreeAndNil(FPreprocessList);
  FAjaxMiddleware := nil;
  inherited Destroy;
end;

procedure TDefaultHTTPClient.sendRequest(aRequest: THTTPRequest; var aResponse: THTTPResponse);
var
  Buffer: string;
begin
  Buffer := prepareHeader(aRequest);
  FTerminated := False;
  if not Terminated then
  begin
    FSocket.WriteBuffer(PChar(Buffer)^, Length(Buffer));
  end;
  if Assigned(aRequest.Body) and not Terminated then
  begin
    aRequest.Body.Position := 0;
    FSocket.CopyFrom(aRequest.Body, aRequest.Body.Size);
  end;
end;

function TDefaultHTTPClient.ReadString(out StringResult: string): boolean;

  function FillBuffer: boolean;
  var
    R: integer;
  begin
    if Terminated then
      Exit(False);
    SetLength(FBuffer, ReadBufLen);
    FillChar(FBuffer[1], ReadBufLen, #32);
    r := FSocket.Read(FBuffer[1], ReadBufLen);
    if (r = 0) or Terminated then
      Exit(False);
    if (r < 0) then
      raise ENetworkReadException.Create(FSocket.LastError);
    if (r < ReadBuflen) then
      SetLength(FBuffer, r);
    Result := r > 0;
  end;

var
  CheckLF: boolean;
  P, L: integer;
begin
  StringResult := '';
  Result := False;
  CheckLF := False;
  repeat
    if Length(FBuffer) = 0 then
      if not FillBuffer then
        Break;
    if Length(FBuffer) = 0 then
      Result := True
    else if CheckLF then
    begin
      if (FBuffer[1] <> #10) then
        StringResult := StringResult + #13
      else
      begin
        System.Delete(FBuffer, 1, 1);
        Result := True;
      end;
    end;
    if not Result then
    begin
      P := Pos(#13#10, FBuffer);
      if P = 0 then
      begin
        L := Length(FBuffer);
        CheckLF := FBuffer[L] = #13;
        if CheckLF then
          StringResult := StringResult + Copy(FBuffer, 1, L - 1)
        else
          StringResult := StringResult + FBuffer;
        FBuffer := '';
      end
      else
      begin
        StringResult += Copy(FBuffer, 1, P - 1);
        System.Delete(FBuffer, 1, P + 1);
        Result := True;
      end;
    end;
  until Result or Terminated;
end;


function GetNextWord(var S: string): string;
const
  WhiteSpace = [' ', #9];
var
  P: integer;
begin
  while (Length(S) > 0) and (S[1] in WhiteSpace) do
    Delete(S, 1, 1);
  P := Pos(' ', S);
  if (P = 0) then
    P := Pos(#9, S);
  if (P = 0) then
    P := Length(S) + 1;
  Result := Copy(S, 1, P - 1);
  Delete(S, 1, P);
end;

function TDefaultHTTPClient.GetAjax: boolean;
begin
  Result := FAjaxMiddleware.active;
end;

procedure TDefaultHTTPClient.SetAjax(AValue: boolean);
begin
  FAjaxMiddleware.active := AValue;
end;

procedure TDefaultHTTPClient.SetKeepAlive(AValue: boolean);
begin
  if FKeepAlive = AValue then
    Exit;
  FKeepAlive := AValue;
end;

procedure TDefaultHTTPClient.SetTimeOut(AValue: int64);
begin
  if FTimeOut = AValue then
    Exit;
  FTimeOut := AValue;
end;

function TDefaultHTTPClient.ParseStatusLine(var AResponse: THTTPResponse; AStatusLine: string): integer;
var
  S: string;
begin
  AResponse.StatusLine := AStatusLine;
  S := Uppercase(GetNextWord(AStatusLine));
  if (Copy(S, 1, 5) <> 'HTTP/') then
    raise EHTTPNetworkInvalidProtocolException.CreateFmt('%s', [S]);
  System.Delete(S, 1, 5);
  S := GetNextWord(AStatusLine);
  Result := StrToIntDef(S, -1);
  AResponse.StatusCode := word(Result);
  if Result = -1 then
    raise EHTTPUnreadableResponseException.CreateFmt('%s', [S]);
end;


function TDefaultHTTPClient.ReadResponseHeaders(var AResponse: THTTPResponse): integer;
var
  StatusLine, S: string;
begin
  //TLogLogger.GetLogger('HTTP').Enter(self, 'ReadResponseHeaders');
  if not ReadString(StatusLine) then
    Exit(0);
  Result := ParseStatusLine(AResponse, StatusLine);
  //TLogLogger.GetLogger('HTTP').Trace(StatusLine);
  repeat
    if ReadString(S) and (S <> '') then
    begin
      AResponse.getHeaders.Add(S);
      //TLogLogger.GetLogger('HTTP').Trace(S);
    end
  until (S = '') or Terminated;
  //TLogLogger.GetLogger('HTTP').Leave(self, 'ReadResponseHeaders');
end;

procedure TDefaultHTTPClient.receiveResponse(aRequest: THTTPRequest; var aResponse: THTTPResponse);


  function Transfer(LB: integer): integer;
  begin
    Result := FSocket.Read(FBuffer[1], LB);
    if Result < 0 then
      raise ENetworkReadException.Create(FSocket.LastError);
    if (Result > 0) then
    begin
      aResponse.Body.Write(FBuffer[1], Result);
    end;
  end;

  procedure ReadChunkedResponse;
  { HTTP 1.1 chunked response:
    There is no content-length. The response consists of several chunks of
    data, each
    - beginning with a line
      - starting with a hex number DataSize,
      - an optional parameter,
      - ending with #13#10,
    - followed by the data,
    - ending with #13#10 (not in DataSize),
    It ends when the DataSize is 0.
    After the last chunk there can be a some optional entity header fields.
    This trailer is not yet implemented. }
  var
    BufPos: integer;

    function FetchData(out Cnt: integer): boolean;
    begin
      Result := False;
      if Terminated then
        exit;
      SetLength(FBuffer, ReadBuflen);
      Cnt := FSocket.Read(FBuffer[1], length(FBuffer));
      if Cnt < 0 then
        raise ENetworkReadException.Create(FSocket.LastError);
      SetLength(FBuffer, Cnt);
      BufPos := 1;
      Result := Cnt > 0;
    end;

    function ReadData(Data: pbyte; Cnt: integer): integer;
    var
      l: integer;
    begin
      Result := 0;
      while Cnt > 0 do
      begin
        l := length(FBuffer) - BufPos + 1;
        if l = 0 then
          if not FetchData(l) then
            exit; // end of stream
        if l > Cnt then
          l := Cnt;
        System.Move(FBuffer[BufPos], Data^, l);
        Inc(BufPos, l);
        Inc(Data, l);
        Inc(Result, l);
        Dec(Cnt, l);
      end;
    end;

  var
    c: char;
    ChunkSize: integer;
    l: integer;
  begin
    BufPos := 1;
    repeat
      // read ChunkSize
      ChunkSize := 0;
      repeat
        if ReadData(@c, 1) < 1 then
          exit;
        case c of
          '0'..'9': ChunkSize := ChunkSize * 16 + Ord(c) - Ord('0');
          'a'..'f': ChunkSize := ChunkSize * 16 + Ord(c) - Ord('a') + 10;
          'A'..'F': ChunkSize := ChunkSize * 16 + Ord(c) - Ord('A') + 10;
          else
            break;
        end;
        if ChunkSize > 1000000 then
          raise ENetworkChunkTooBigException.Create;
      until Terminated;
      // read till line end
      while (c <> #10) and not Terminated do
        if ReadData(@c, 1) < 1 then
          exit;
      if ChunkSize = 0 then
        exit;
      // read data
      repeat
        if Terminated then
          exit;
        l := length(FBuffer) - BufPos + 1;
        if l = 0 then
          if not FetchData(l) then
            exit; // end of stream
        if l > ChunkSize then
          l := ChunkSize;
        if l > 0 then
        begin
          // copy chunk data to output
          aResponse.Body.Write(FBuffer[BufPos], l);
          Inc(BufPos, l);
          Dec(ChunkSize, l);
        end;
      until ChunkSize = 0;
      // read #13#10
      if ReadData(@c, 1) < 1 then
        exit;
      if not Terminated then
      begin
        if c <> #13 then
          raise ENetworkChunkEndLineMissingException.Create();
        if ReadData(@c, 1) < 1 then
          exit;
        if c <> #10 then
          raise ENetworkChunkEndLineMissingException.Create();
        // next chunk
      end;
    until Terminated;
  end;

var
  contentLength: int64;
  LengthBuffer, Readed: integer;
  ResponseStatusCode: word;
  Result: boolean;
begin
  resetResponse(aResponse);
  SetLength(FBuffer, 0);
  ResponseStatusCode := ReadResponseHeaders(aResponse);
  Result := ResponseStatusCode > 0;
  if not Result then
    Exit;
  if FHeadersOnly or (FRedirect.active and isRedirectStatus(aResponse.StatusCode)) then
    exit;
  if CompareText(aResponse.getHeader(TransferEncoding), 'chunked') = 0 then
    ReadChunkedResponse
  else
  begin
    LengthBuffer := Length(FBuffer);
    if (LengthBuffer > 0) then
      aResponse.Body.WriteBuffer(FBuffer[1], LengthBuffer);
    SetLength(FBuffer, ReadBuflen);
    contentLength := StrToInt64Def(aResponse.getHeader('content-length'), -1);
    if (contentLength > LengthBuffer) then
    begin
      contentLength := contentLength - LengthBuffer;
      repeat
        LengthBuffer := ReadBufLen;
        if (LengthBuffer > contentLength) then
          LengthBuffer := contentLength;
        Readed := Transfer(LengthBuffer);
        contentLength := contentLength - Readed;
      until (contentLength = 0) or (Readed = 0) or Terminated;
    end
    else if (contentLength < 0) and (not ((ResponseStatusCode div 100) = 1) or ((ResponseStatusCode = 204) or (ResponseStatusCode = 304))) then
    begin
      repeat
        Readed := Transfer(ReadBufLen);
      until (Readed = 0) or Terminated;
    end;
  end;
  aResponse.Body.Seek(0, 0);
end;

procedure TDefaultHTTPClient.resetResponse(var aResponse: THTTPResponse);
begin
  aResponse.getHeaders.Clear;
  aResponse.Body := TMemoryStream.Create;
  aResponse.StatusCode := 0;
  aResponse.StatusLine := '';
end;

function TDefaultHTTPClient.isSSL(aValue: string): boolean;
begin
  Result := 'https' = aValue;
end;

function TDefaultHTTPClient.processNormalRequest(aRequest: THTTPRequest; var aResponse: THTTPResponse): word;
var
  loop: boolean;
  middleware: TMiddleware;
  args: TStrings;
begin
  try
    args := TStringList.Create;
    try
      connect(aRequest.Url.Host, aRequest.Url.Port, isSSL(aRequest.Url.Scheme));
    except
      raise;
    end;
    repeat
      for middleware in FPreprocessList do
        middleware.process(self, aRequest, aResponse, args);
      sendRequest(aRequest, aResponse);
      receiveResponse(aRequest, aResponse);
      for middleware in FPostProcessList do
        middleware.process(self, aRequest, aResponse, args);
      loop := FRedirect.active and (aResponse.StatusLine <> '');
      if loop then
      begin
        loop := isRedirectStatus(aResponse.StatusCode);
      end;
    until not loop;
  except
    FreeAndNil(args);
    raise;
  end;
  FreeAndNil(args);
  Result := aResponse.StatusCode;
  disconnect;
end;

function TDefaultHTTPClient.processKeepAliveRequest(aRequest: THTTPRequest; var aResponse: THTTPResponse): word;
var
  loop: boolean;
  middleware: TMiddleware;
  args: TStrings;
begin
  args := TStringList.Create;
  try
    repeat
      try
        connect(aRequest.Url.Host, aRequest.Url.Port, isSSL(aRequest.Url.Scheme));
      except
        raise;
      end;
      for middleware in FPreprocessList do
      begin
        if middleware.active then
          middleware.process(self, aRequest, aResponse, args);
      end;
      sendRequest(aRequest, aResponse);
      receiveResponse(aRequest, aResponse);
      for middleware in FPostProcessList do
      begin
        if middleware.active then
          middleware.process(self, aRequest, aResponse, args);
      end;

      loop := FRedirect.active and (aResponse.StatusLine <> '');
      if loop and SameText(aResponse.getHeader('Connection'), 'close') then
        loop := False;
      if loop then
      begin
        loop := isRedirectStatus(aResponse.StatusCode);
      end;
      disconnect;
    until not loop;
  except
    FreeAndNil(args);
    raise;
  end;
  FreeAndNil(args);
  Result := aResponse.StatusCode;
end;

function TDefaultHTTPClient.getServerUrl(ARequest: THTTPRequest): string;
begin
  Result := ARequest.URL.getServerUrl;
end;

procedure TDefaultHTTPClient.AbortRequest;
begin
  FTerminated := True;
end;

function TDefaultHTTPClient.request(aRequest: THTTPRequest; var aResponse: THTTPResponse): word;
var
  e: TEncoder;
  encodedRequest: THttpRequest = nil;
begin
  if aRequest is TFormDataRequest then
  begin
    for e in FEncoders do
      if e.encode(aRequest, encodedRequest) then
        break;
    if encodedRequest <> nil then
    begin
      encodedRequest.setHeader(ContentLength, encodedRequest.Body.Size.ToString);
      Result := performRequest(encodedRequest, aResponse);
      FreeAndNil(encodedRequest);
    end;
  end
  else
  begin
    Result := performRequest(aRequest, aResponse);
  end;
end;

function TDefaultHTTPClient.performRequest(aRequest: THTTPRequest; var aResponse: THTTPResponse): word;
begin
  if FKeepConnection then
    Result := processKeepAliveRequest(aRequest, aResponse)
  else
    Result := processNormalRequest(aRequest, aResponse);
end;

procedure TDefaultHTTPClient.addPostProcessMiddleware(aMiddleware: TMiddleware);
begin
  FPostProcessList.Add(aMiddleware);
end;

procedure TDefaultHTTPClient.addPreProcessMiddleware(aMiddleware: TMiddleware);
begin
  FPreprocessList.Add(aMiddleware);
end;

procedure TDefaultHTTPClient.removePostProcessMiddleware(aMiddleware: TMiddleware);
begin
  FPostProcessList.Remove(aMiddleware);
end;

procedure TDefaultHTTPClient.removePreProcessMiddleware(aMiddleware: TMiddleware);
begin
  FPreprocessList.Remove(aMiddleware);
end;

end.
