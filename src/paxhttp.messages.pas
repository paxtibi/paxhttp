{
@see https://developer.mozilla.org/it/docs/Web/HTTP
}
unit paxhttp.Messages;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, fgl;

const
  Accept  = 'Accept';//    Accept: text/html  Permanent
  AcceptCharset = 'Accept-Charset';//    Accept-Charset: utf-8  Permanent
  AcceptDatetime = 'Accept-Datetime';
  //    Accept-Datetime: Thu, 31 May 2007 20:35:00 GMT  Provisional
  AcceptEncoding = 'Accept-Encoding';//    Accept-Encoding: gzip, deflate  Permanent
  AcceptLanguage = 'Accept-Language';//    Accept-Language: en-US  Permanent
  AccessControlRequestHeaders = 'Access-Control-Request-Headers';
  AccessControlRequestMethod = 'Access-Control-Request-Method';
  //    Access-Control-Request-Method: GET  Permanent: standard
  AIM     = 'A-IM';//    A-IM: feed  Permanent
  Authorization = 'Authorization';
  //    Authorization: Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==  Permanent
  CacheControl = 'Cache-Control';//    Cache-Control: no-cache  Permanent
  Connection = 'Connection';//    Connection: keep-alive  Permanent
  ContentDisposition = 'Content-Disposition';
  ContentLength = 'Content-Length';//    Content-Length: 348  Permanent
  ContentMD5 = 'Content-MD5';//    Content-MD5: Q2hlY2sgSW50ZWdyaXR5IQ==  Obsolete[11]
  ContentType = 'Content-Type';
  //    Content-Type: application/x-www-form-urlencoded  Permanent
  ContentTransferEncoding = 'Content-Transfer-Encoding';
  Cookie  = 'Cookie';//    Cookie: $Version=1; Skin=new;  Permanent: standard
  Date    = 'Date';  //    Date: Tue, 15 Nov 1994 08:12:31 GMT  Permanent
  DNT     = 'DNT';   //    DNT: 1 (Do Not Track Enabled)
  Expect  = 'Expect';//    Expect: 100-continue  Permanent
  Forwarded = 'Forwarded';
  //    Forwarded: for=192.0.2.60;proto=http;by=203.0.113.43 Forwarded: for=192.0.2.43, for=198.51.100.17  Permanent
  From    = 'From';//    From: user@example.com  Permanent
  FrontEndHttps = 'Front-End-Https';//    Front-End-Https: on
  Host    = 'Host';//    Host: en.wikipedia.org:8080  Permanent
  IfMatch = 'If-Match';//    If-Match: "737060cd8c284d8af7ad3082f209582d"  Permanent
  IfModifiedSince = 'If-Modified-Since';
  //    If-Modified-Since: Sat, 29 Oct 1994 19:43:31 GMT  Permanent
  IfNoneMatch = 'If-None-Match';
  //    If-None-Match: "737060cd8c284d8af7ad3082f209582d"  Permanent
  IfRange = 'If-Range';//    If-Range: "737060cd8c284d8af7ad3082f209582d"  Permanent
  IfUnmodifiedSince = 'If-Unmodified-Since';
  //    If-Unmodified-Since: Sat, 29 Oct 1994 19:43:31 GMT  Permanent
  MaxForwards = 'Max-Forwards';//    Max-Forwards: 10  Permanent
  Origin  = 'Origin';//    Origin: http://www.example-social-network.com  Permanent: standard
  pgradeInsecureRequests = 'pgrade-Insecure-Requests';//    Upgrade-Insecure-Requests: 1
  Pragma  = 'Pragma';//    Pragma: no-cache  Permanent
  ProxyAuthorization = 'Proxy-Authorization';
  //    Proxy-Authorization: Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==  Permanent
  ProxyConnection = 'Proxy-Connection';//    Proxy-Connection: keep-alive
  Range   = 'Range';//    Range: bytes=500-999  Permanent
  Referer = 'Referer';//    Referer: http://en.wikipedia.org/wiki/Main_Page  Permanent
  SaveData = 'Save-Data';//    Save-Data: on
  SetCookie = 'Set-Cookie';
  TE      = 'TE';//    TE: trailers, deflate  Permanent
  TransferEncoding = 'Transfer-Encoding';
  Upgrade = 'Upgrade';//    Upgrade: h2c, HTTPS/1.3, IRC/6.9, RTA/x11, websocket  Permanent
  UserAgent = 'User-Agent';
  //    User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:12.0) Gecko/20100101 Firefox/12.0  Permanent
  Via     = 'Via';//    Via: 1.0 fred, 1.1 example.com (Apache/1.1)  Permanent
  Warning = 'Warning';//    Warning: 199 Miscellaneous warning  Permanent
  XATTDeviceId = 'X-ATT-DeviceId';//    X-Att-Deviceid: GT-P7320/P7320XXLPG
  XCorrelationID = 'X-Correlation-ID';
  XCsrfToken = 'X-Csrf-Token';//    X-Csrf-Token: i8XNjC4b8KVok4uw5RftR38Wgp2BFwql
  XForwardedFor = 'X-Forwarded-For';//    X-Forwarded-For: client1, proxy1, proxy2
  XForwardedHost = 'X-Forwarded-Host';//    X-Forwarded-Host: en.wikipedia.org:8080
  XForwardedProto = 'X-Forwarded-Proto';//    X-Forwarded-Proto: https
  XHttpMethodOverride = 'X-Http-Method-Override';//    X-HTTP-Method-Override: DELETE
  XRequestedWith = 'X-Requested-With';//    X-Requested-With: XMLHttpRequest
  XRequestID = 'X-Request-ID';//    X-Request-ID: f058ebd6-02f7-4d3f-942e-904344e8cde5
  XUIDH   = 'X-UIDH';//    X-UIDH: ...
  XWapProfile = 'X-Wap-Profile';
//    x-wap-profile: http://wap.samsungmobile.com/uaprof/SGH-I777.xml


type
  TStringArray = array of string;
  TSameSite    = (Strict, Lax);
  THTTPVersion = (http09, http10, http11, http20);

type
  { THTTPHeader }

  THTTPHeader = class
  private
    FName:  string;
    FValue: string;
    function GetName: string;
    function GetValue: string;
    procedure SetName(AValue: string);
    procedure SetValue(AValue: string);
  public
    function ToString: ansistring; override;
  published
    property Name: string read GetName write SetName;
    property Value: string read GetValue write SetValue;
  end;

  THTTPHeaders = class;
  { THTTPHeaderEnumerator }

  THTTPHeaderEnumerator = class
  protected
    FList:     THTTPHeaders;
    FPosition: int64;
  public
    constructor Create(aList: THTTPHeaders);
    function GetCurrent: THTTPHeader;
    function moveNext: boolean;
    property Current: THTTPHeader read GetCurrent;
  end;


  THTTPHeaderList = specialize TFPGObjectList<THTTPHeader>;

  TURL = class;
  { THTTPHeaders }

  THTTPHeaders = class
  protected
    FContainer: THttpHeaderList;
    FLock:      TRTLCriticalSection;
  private
    function GetHeader(index: integer): THTTPHeader;
    procedure SetHeader(index: integer; AValue: THTTPHeader);
  public
    constructor Create;
    destructor Destroy; override;
    function getEnumerator: THTTPHeaderEnumerator;
    function Count: int64;
    procedure Add(AHeader: string); overload;
    procedure Add(AHeader: THTTPHeader); overload;
    procedure removeHeader(aName: string);
    function indexOf(aName: string): integer;
    procedure Clear;
    property Header[index: integer]: THTTPHeader read GetHeader write SetHeader; default;
  end;

  { THTTPMessage }

  THTTPMessage = class(TPersistent)
  protected
    FHeaders: THTTPHeaders;
    FBody:    TStream;
  protected
    function GetBody: TStream; virtual;
    procedure SetBody(AValue: TStream);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function getHeaders: THTTPHeaders;
    function hasHeader(Name: string): boolean;
    procedure removeHeader(Name: string);
    function getHeader(Name: string): string;
    procedure setHeader(Name, Value: string);
    procedure addHeader(Name, Value: string);
  published
    property Headers: THTTPHeaders read FHeaders;
    property Body: TStream read GetBody write SetBody;
  end;

  { THttpRequest }

  THttpRequest = class(THTTPMessage)
  private
    FMethod:  string;
    FURL:     TURL;
    FVersion: string;
    function GetMethod: string;
    function GetURL: TURL;
    function GetVersion: string;
    procedure SetMethod(AValue: string);
    procedure SetURL(AValue: TURL);
    procedure SetVersion(AValue: string);
  public
    constructor Create; override; overload;
    constructor Create(aUri: string); overload;
    destructor Destroy; override;
  published
    property Method: string read GetMethod write SetMethod;
    property URL: TURL read GetURL write SetURL;
    property Version: string read GetVersion write SetVersion;
  end;

  THttpRequestClass = class of THttpRequest;

  { TFormData }

  TFormData = class
  private
    FName:  string;
    FValue: string;
    procedure SetName(AValue: string);
    procedure SetValue(AValue: string);
  public
    property Name: string read FName write SetName;
    property Value: string read FValue write SetValue;
  end;

  { TFormDataFile }

  TFormDataFile = class(TFormData)
  private
    FStream: TStream;
    function GetFileName: string;
    procedure SetFileName(AValue: string);
    procedure SetStream(AValue: TStream);
  public
    property FileName: string read GetFileName write SetFileName;
    property Stream: TStream read FStream write SetStream;
  end;


  TForm = specialize TFPGObjectList<TFormData>;

  { TFormDataRequest }

  TFormDataRequest = class(THttpRequest)
  private
    FBoundary: string;
    FFormData: TForm;
    procedure SetformData(AValue: TForm);
  public
    constructor Create; override;
    destructor Destroy; override;
    property CurrentBundary: string read FBoundary;
    property formData: TForm read FformData write SetformData;
  end;

  { THttpResponse }

  THttpResponse = class(THTTPMessage)
  private
    FReasonPhase: string;
    FStatusCode:  word;
    FStatusLine:  string;
    function GetReasonPhase: string;
    function GetStatusCode: word;
    function GetStatusLine: string;
    procedure SetReasonPhase(AValue: string);
    procedure SetStatusCode(AValue: word);
    procedure SetStatusLine(AValue: string);
  public
    constructor Create; override;
  published
    property StatusLine: string read GetStatusLine write SetStatusLine;
    property StatusCode: word read GetStatusCode write SetStatusCode;
    property ReasonPhase: string read GetReasonPhase write SetReasonPhase;
  end;

  { TUri }

  TUri = class
  private
    Fautority: string;
    Ffragment: string;
    Fpath:     string;
    FqueryString: string;
    FScheme:   string;
    procedure Setautority(AValue: string);
    procedure Setfragment(AValue: string);
    procedure Setpath(AValue: string);
    procedure SetqueryString(AValue: string);
    procedure SetScheme(AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    class function parse(auri: RawByteString): TURI; virtual;
    procedure Assign(auri: RawByteString); virtual;
    property Scheme: string read FScheme write SetScheme;
    property autority: string read Fautority write Setautority;
    property path: string read Fpath write Setpath;
    property queryString: string read FqueryString write SetqueryString;
    property fragment: string read Ffragment write Setfragment;
    function getServerUrl: string;
    function ToString: ansistring; override;
  end;

  { TUrl }

  TUrl = class(TUri)
  private
    FHost:     string;
    FPort:     word;
    FUserInfo: string;
    function GetAutority: string;
    procedure Sethost(AValue: string);
    procedure Setport(AValue: word);
    procedure Setuserinfo(AValue: string);
  public
    class function parse(auri: string): TURL; reintroduce;
    procedure Assign(auri: RawByteString); override;
  public
    property autority: string read GetAutority;
    property userInfo: string read Fuserinfo write Setuserinfo;
    property host: string read Fhost write Sethost;
    property port: word read Fport write Setport;
  end;

  { EHTTPMessageException }

  EHTTPMessageException = class(Exception)
    constructor Create;
  end;

  ENetworkException = class(EHTTPMessageException)

  end;

  EHTTPNetworkInvalidProtocolException = class(EHTTPMessageException)

  end;

  EHTTPUnreadableResponseException = class(EHTTPMessageException)

  end;

  { ENetworkReadException }

  ENetworkReadException = class(ENetworkException)
    constructor Create(ErrorCode: integer); virtual;
  end;

  ENetworkChunkTooBigException = class(ENetworkException)

  end;

  ENetworkChunkEndLineMissingException = class(ENetworkException)
  end;


const
  CRLF = #13#10;

function isRedirectStatus(aStatus: word): boolean;


implementation


function isRedirectStatus(aStatus: word): boolean;
begin
  case aStatus of
    301,
    302,
    303,
    307,
    308: Result := True;
    else
      Result := False;
  end;
end;

{ TFormDataFile }

procedure TFormDataFile.SetFileName(AValue: string);
begin
  Value := AValue;
end;

function TFormDataFile.GetFileName: string;
begin
  Result := Value;
end;

procedure TFormDataFile.SetStream(AValue: TStream);
begin
  if FStream = AValue then
    Exit;
  FStream := AValue;
end;

{ TFormData }

procedure TFormData.SetName(AValue: string);
begin
  if FName = AValue then
    Exit;
  FName := AValue;
end;

procedure TFormData.SetValue(AValue: string);
begin
  if FValue = AValue then
    Exit;
  FValue := AValue;
end;

{ TFormDataRequest }

procedure TFormDataRequest.SetformData(AValue: TForm);
begin
  if FformData = AValue then
    Exit;
  FformData := AValue;
end;

constructor TFormDataRequest.Create;
begin
  inherited Create;
  FBoundary := Format('%8x', [Random($FFFFFFFF)]);
  FformData := TForm.Create;
end;

destructor TFormDataRequest.Destroy;
begin
  FformData.Free;
  inherited Destroy;
end;


{ ENetworkReadException }

constructor ENetworkReadException.Create(ErrorCode: integer);
begin
  inherited CreateFmt('%d', [ErrorCode]);
end;

{ TUrl }

procedure TUrl.Sethost(AValue: string);
begin
  if Fhost = AValue then
    Exit;
  Fhost := AValue;
end;

function TUrl.GetAutority: string;
begin
  Result := '';
  if userInfo <> '' then
    Result += userInfo + '@';
  Result   += host;
  if port <> 0 then
    Result += ':' + FPort.ToString;
end;

procedure TUrl.Setport(AValue: word);
begin
  if Fport = AValue then
    Exit;
  Fport := AValue;
end;

procedure TUrl.Setuserinfo(AValue: string);
begin
  if Fuserinfo = AValue then
    Exit;
  Fuserinfo := AValue;
end;

class function TUrl.parse(auri: string): TURL;
begin
  Result := TUrl.Create;
  Result.Assign(auri);
end;

procedure TUrl.Assign(auri: RawByteString);
var
  position: integer;
  buffer: RawByteString;
begin
  inherited Assign(auri);
  FPort     := 0;
  FHost     := '';
  FUserInfo := '';

  if FAutority <> '' then
  begin
    buffer    := FAutority;
    FAutority := '';
    position  := LastDelimiter('@', buffer);
    if position > 0 then
    begin
      FUserInfo := Copy(buffer, 1, position - 1);
      Delete(buffer, 1, position);
    end;
    position := LastDelimiter(':', buffer);
    if position > 0 then
    begin
      FPort := StrToIntDef(Copy(buffer, position + 1, Length(buffer)), 0);
      Delete(buffer, position, Length(buffer));
    end;
    FHost := buffer;

    if FUserInfo <> '' then
      FAutority += FUserInfo + '@';
    FAutority   += Fhost;
    if FPort <> 0 then
    begin
      Fautority += ':' + FPort.ToString;
    end;
  end;
end;


{ TUri }

function TUri.getServerUrl: string;
begin
  Result := '/';
  if FPath <> '' then
    Result += FPath;
  if FQueryString <> '' then
    Result += '?' + FQueryString;
  if FFragment <> '' then
    Result += '#' + FFragment;
  Result   := StringReplace(Result, '//', '/', [rfReplaceAll]);
end;

procedure TUri.Setautority(AValue: string);
begin
  if Fautority = AValue then
    Exit;
  Fautority := AValue;
end;

procedure TUri.Setfragment(AValue: string);
begin
  if Ffragment = AValue then
    Exit;
  Ffragment := AValue;
end;

procedure TUri.Setpath(AValue: string);
begin
  if Fpath = AValue then
    Exit;
  Fpath := AValue;
end;

procedure TUri.SetqueryString(AValue: string);
begin
  if FqueryString = AValue then
    Exit;
  FqueryString := AValue;
end;

procedure TUri.SetScheme(AValue: string);
begin
  if FScheme = AValue then
    Exit;
  FScheme := AValue;
end;

constructor TUri.Create;
begin

end;

destructor TUri.Destroy;
begin
  inherited Destroy;
end;

class function TUri.parse(auri: RawByteString): TURI;
  //scheme:[//authority]path[?query][#fragment]
begin
  Result := TUri.Create;
  Result.Assign(auri);
end;

procedure TUri.Assign(auri: RawByteString);
var
  position: integer;
begin
  position  := Pos('://', auri);
  Fautority := '';
  FScheme   := '';
  Fpath     := '';
  FqueryString := '';
  Ffragment := '';
  if position > 0 then
  begin
    FScheme := copy(auri, 1, position - 1);
    Delete(auri, 1, position + 2);
  end;
  position := LastDelimiter('#', auri);
  if position > 0 then
  begin
    FFragment := copy(auri, LastDelimiter('#', auri) + 1, Length(auri));
    Delete(auri, position, Length(auri));
  end;
  position := LastDelimiter('?', auri);
  if position > 0 then
  begin
    FQueryString := copy(auri, position + 1, Length(auri));
    Delete(auri, position, Length(auri));
  end;
  position := pos('/', auri);
  if position > 0 then
  begin
    FPath := copy(auri, position + 1, Length(auri));
    Delete(auri, position, Length(auri));
  end;
  Fautority := auri;
end;

function TUri.ToString: ansistring;
begin
  Result := '';
  if FScheme <> '' then
    Result += scheme + '://';
  if Autority <> '' then
    Result += autority;
  Result   += getServerUrl;
end;


{ THTTPHeaderEnumerator }

constructor THTTPHeaderEnumerator.Create(aList: THTTPHeaders);
begin
  inherited Create;
  FList     := AList;
  FPosition := -1;
end;

function THTTPHeaderEnumerator.GetCurrent: THTTPHeader;
begin
  Result := FList[FPosition];
end;

function THTTPHeaderEnumerator.moveNext: boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;


{ THTTPHeaders }

function THTTPHeaders.GetHeader(index: integer): THTTPHeader;
begin
  EnterCriticalSection(FLock);
  Result := FContainer[index];
  LeaveCriticalsection(FLock);
end;

procedure THTTPHeaders.SetHeader(index: integer; AValue: THTTPHeader);
begin
  EnterCriticalsection(FLock);
  FContainer[index] := AValue;
  LeaveCriticalsection(FLock);
end;

constructor THTTPHeaders.Create;
begin
  inherited Create;
  FContainer := THttpHeaderList.Create(True);
  InitCriticalSection(FLock);
end;

destructor THTTPHeaders.Destroy;
begin
  EnterCriticalsection(FLock);
  FContainer.Clear;
  FreeAndNil(FContainer);
  LeaveCriticalsection(FLock);
  DoneCriticalsection(FLock);
  inherited Destroy;
end;

function THTTPHeaders.getEnumerator: THTTPHeaderEnumerator;
begin
  Result := THTTPHeaderEnumerator.Create(self);
end;

function THTTPHeaders.Count: int64;
begin
  try
    EnterCriticalsection(FLock);
    Result := FContainer.Count;
    LeaveCriticalsection(FLock);
  except
  end;
end;

procedure THTTPHeaders.Add(AHeader: string);
var
  aName: string;
  aValue: string;
  c, p: PChar;
  h: THTTPHeader;
begin
  p := PChar(AHeader);
  c := p;
  while (c <> #0) and (c^ <> ':') do
  begin
    Inc(c);
  end;
  SetString(aName, p, c - p);
  aValue := Copy(AHeader, (c - p + 2));
  aValue := Trim(aValue);
  h      := THTTPHeader.Create;
  h.Value := Trim(aValue);
  h.Name := aName;
  try
    add(h);
  except
    on e: Exception do
    begin
      Writeln(e.Message);
    end;
  end;
end;

procedure THTTPHeaders.Add(AHeader: THTTPHeader);
begin
  EnterCriticalsection(FLock);
  FContainer.Add(AHeader);
  LeaveCriticalsection(FLock);
end;

procedure THTTPHeaders.removeHeader(aName: string);
var
  idx: integer;
  h: THTTPHeader;
begin
  EnterCriticalsection(FLock);
  idx := indexOf(aName);
  if idx >= 0 then
  begin
    h := THTTPHeader(FContainer[idx]);
    FContainer.Remove(h);
    h := nil;
  end;
  LeaveCriticalsection(FLock);
end;

function THTTPHeaders.indexOf(aName: string): integer;
var
  idx: integer;
begin
  Result := -1;
  for idx := 0 to Count - 1 do
  begin
    if CompareText(Header[idx].Name, aName) = 0 then
    begin
      exit(idx);
    end;
  end;
end;

procedure THTTPHeaders.Clear;
begin
  EnterCriticalsection(FLock);
  FContainer.Clear;
  LeaveCriticalsection(FLock);
end;

{ THTTPHeader }

procedure THTTPHeader.SetName(AValue: string);
begin
  if FName = AValue then
    Exit;
  FName := AValue;
end;

function THTTPHeader.GetName: string;
begin
  Result := FName;
end;

function THTTPHeader.GetValue: string;
begin
  Result := FValue;
end;

procedure THTTPHeader.SetValue(AValue: string);
begin
  if FValue = AValue then
    Exit;
  FValue := AValue;
end;

function THTTPHeader.ToString: ansistring;
begin
  Result := Format('%s: %s', [Name, Value]);
end;


{ EHTTPMessageException }

constructor EHTTPMessageException.Create;
begin
  inherited Create('');
end;

{ THttpResponse }

procedure THttpResponse.SetReasonPhase(AValue: string);
begin
  if FReasonPhase = AValue then
    Exit;
  FReasonPhase := AValue;
end;

function THttpResponse.GetReasonPhase: string;
begin
  Result := FReasonPhase;
end;

function THttpResponse.GetStatusCode: word;
begin
  Result := FStatusCode;
end;

function THttpResponse.GetStatusLine: string;
begin
  Result := FStatusLine;
end;

procedure THttpResponse.SetStatusCode(AValue: word);
begin
  if FStatusCode = AValue then
    Exit;
  FStatusCode := AValue;
end;

procedure THttpResponse.SetStatusLine(AValue: string);
begin
  if FStatusLine = AValue then
    Exit;
  FStatusLine := AValue;
end;

constructor THttpResponse.Create;
begin
  inherited Create;
end;

{ THttpRequest }

procedure THttpRequest.SetMethod(AValue: string);
begin
  if FMethod = AValue then
    Exit;
  FMethod := AValue;
end;

function THttpRequest.GetMethod: string;
begin
  Result := FMethod;
end;

function THttpRequest.GetURL: TURL;
begin
  Result := FURL;
end;

function THttpRequest.GetVersion: string;
begin
  Result := FVersion;
end;

procedure THttpRequest.SetURL(AValue: TURL);
begin
  FreeAndnil(FURL);
  FURL := AValue;
end;

procedure THttpRequest.SetVersion(AValue: string);
begin
  if FVersion = AValue then
    Exit;
  FVersion := AValue;
end;

constructor THttpRequest.Create;
begin
  inherited Create;
  FVersion := '1.0';
  FMethod  := 'GET';
  FURL     := TUrl.Create;
end;

constructor THttpRequest.Create(aUri: string);
begin
  Create;
  FURL := TUrl.parse(aUri);
end;

destructor THttpRequest.Destroy;
begin
  FreeAndNil(FURL);
  inherited Destroy;
end;

{ THTTPMessage }

constructor THTTPMessage.Create;
begin
  FHeaders := THTTPHeaders.Create;
  FBody    := TMemoryStream.Create;
end;

destructor THTTPMessage.Destroy;
begin
  FreeAndNil(FHeaders);
  FreeAndNil(FBody);
  inherited Destroy;
end;

function THTTPMessage.getHeaders: THTTPHeaders;
begin
  Result := FHeaders;
end;

function THTTPMessage.GetBody: TStream;
begin
  Result := FBody;
end;

procedure THTTPMessage.SetBody(AValue: TStream);
begin
  if assigned(FBody) then
    FreeAndNil(FBody);
  FBody := AValue;
end;

function THTTPMessage.hasHeader(Name: string): boolean;
begin
  Result := FHeaders.indexOf(Name) >= 0;
end;

procedure THTTPMessage.removeHeader(Name: string);
begin
  FHeaders.removeHeader(Name);
end;

function THTTPMessage.getHeader(Name: string): string;
var
  idx: integer;
begin
  Result := '';
  idx    := FHeaders.indexOf(Name);
  if idx >= 0 then
    Result := FHeaders.Header[idx].Value;
end;

procedure THTTPMessage.setHeader(Name, Value: string);
var
  idx: integer;
begin
  idx := FHeaders.indexOf(Name);
  if idx >= 0 then
    FHeaders.Header[idx].Value := Value
  else
    addHeader(Name, Value);
end;

procedure THTTPMessage.addHeader(Name, Value: string);
begin
  FHeaders.Add(Format('%s: %s', [Name, Value]));
end;


end.
