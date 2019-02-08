unit paxhttp.middlewares;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, paxhttp.client, paxhttp.Messages;

type

  { TCookieMiddlewarePreprocess }

  TCookieMiddlewarePreprocess = class(TMiddleware)
    function process(aClient: TDefaultHttpClient; aRequest: THttpRequest;
      aResponse: THttpResponse; args: TStrings): boolean; override;
  end;

  { TAjaxMiddlewarePreprocess }

  TAjaxMiddlewarePreprocess = class(TMiddleware)
    function process(aClient: TDefaultHttpClient; aRequest: THttpRequest;
      aResponse: THttpResponse; args: TStrings): boolean; override;
  end;

  { TReferredMiddlewarePreprocess }

  TReferredMiddlewarePostProcess = class(TMiddleware)
    function process(aClient: TDefaultHttpClient; aRequest: THttpRequest;
      aResponse: THttpResponse; args: TStrings): boolean; override;
  end;

  { TCorrelationIdPreprocess }

  TCorrelationIdPreprocess = class(TMiddleware)
    function process(aClient: TDefaultHttpClient; aRequest: THttpRequest;
      aResponse: THttpResponse; args: TStrings): boolean; override;
  end;

  { THostPreprocess }

  THostPreprocess = class(TMiddleware)
    function process(aClient: TDefaultHttpClient; aRequest: THttpRequest;
      aResponse: THttpResponse; args: TStrings): boolean; override;
  end;

  { TRedirectPostProcess }

  TRedirectPostProcess = class(TMiddleware)
    function process(aClient: TDefaultHttpClient; aRequest: THttpRequest;
      aResponse: THttpResponse; args: TStrings): boolean; override;
  end;


  { TBasicAuthorizationRequestPreprocess }

  TBasicAuthorizationRequestPreprocess = class(TAuthorizationMiddleware)
  private
    FPassword: string;
    FUserName: string;
    procedure SetPassword(AValue: string);
    procedure SetUserName(AValue: string);
  public
    constructor Create; override;
    function process(aClient: TDefaultHttpClient; aRequest: THttpRequest;
      aResponse: THttpResponse; args: TStrings): boolean; override;
  end;

  { TBearerAuthorization }

  TBearerAuthorization = class(TMiddleware)
  private
    FToken: string;
    procedure SetToken(AValue: string);
  public
    constructor Create; override;
    property Token: string read FToken write SetToken;
    function process(aClient: TDefaultHttpClient; aRequest: THttpRequest;
      aResponse: THttpResponse; args: TStrings): boolean; override;
  end;

  { TProxyRequestPreprocess }

  TProxyRequestPreprocess = class(TBasicAuthorizationRequestPreprocess)
  private
    Fproxy: string;
    procedure Setproxy(AValue: string);
  public
    function process(aClient: TDefaultHttpClient; aRequest: THttpRequest;
      aResponse: THttpResponse; args: TStrings): boolean; override;
    property proxy: string read Fproxy write Setproxy;
  end;

implementation

uses
  base64;


{ TBearerAuthorization }

procedure TBearerAuthorization.SetToken(AValue: string);
begin
  if FToken = AValue then
    Exit;
  FToken := AValue;
end;

constructor TBearerAuthorization.Create;
begin
  inherited Create;
end;

function TBearerAuthorization.process(aClient: TDefaultHttpClient;
  aRequest: THttpRequest; aResponse: THttpResponse; args: TStrings): boolean;
begin
  if (FToken <> '') and active then
  begin
    aRequest.setHeader('Authorization', Format('Bearer  %s', [FToken]));
  end;
end;


{ TRedirectPostProcess }

function TRedirectPostProcess.process(aClient: TDefaultHttpClient;
  aRequest: THttpRequest; aResponse: THttpResponse; args: TStrings): boolean;
var
  newLocation: string;
  newURI: TURL;
begin
  if isRedirectStatus(aResponse.StatusCode) then
  begin
    newLocation := aResponse.getHeader('location');
    newURI := TURL.parse(newLocation);
    if newURI.Scheme <> '' then
      aRequest.URL := newURI
    else
    begin
      aRequest.URL.path := aRequest.URL.path + newURI.path;
      newLocation := aRequest.URL.getServerUrl;
      newLocation := StringReplace(newLocation, '//', '/', [rfReplaceAll]);
      newLocation := StringReplace(newLocation, '/./', '/', [rfReplaceAll]);
      aRequest.URL.path := newLocation;
    end;
  end;
  Result := True;
end;

{ THostPreprocess }

function THostPreprocess.process(aClient: TDefaultHttpClient;
  aRequest: THttpRequest; aResponse: THttpResponse; args: TStrings): boolean;
var
  formatStr: string = '%s:%d';
begin
  if not aRequest.hasHeader('host') then
  begin
    if aRequest.URL.Port = 0 then
    begin
      formatStr := '%s';
    end;
    aRequest.addHeader('Host', Format(formatStr, [aRequest.URL.host,
      aRequest.URL.port]));
  end;
  Result := True;
end;

{ TReferredMiddlewarePreprocess }

function TReferredMiddlewarePostProcess.process(aClient: TDefaultHttpClient;
  aRequest: THttpRequest; aResponse: THttpResponse; args: TStrings): boolean;
begin
  Result := True;
  if aResponse.hasHeader('Referred') then
    aRequest.setHeader('Referred', aResponse.getHeader('Location'))
  else
    aRequest.setHeader('Referred', aRequest.URL.ToString);
end;

{ TAjaxMiddlewarePreprocess }

function TAjaxMiddlewarePreprocess.process(aClient: TDefaultHttpClient;
  aRequest: THttpRequest; aResponse: THttpResponse; args: TStrings): boolean;
begin
  if Active and (not aRequest.hasHeader('X-Requested-With')) then
    aRequest.setHeader('X-Requested-With', 'XMLHttpRequest');
  Result := True;
end;


{ TProxyRequestPreprocess }

procedure TProxyRequestPreprocess.Setproxy(AValue: string);
begin
  if Fproxy = AValue then
    Exit;
  Fproxy := AValue;
end;

function TProxyRequestPreprocess.process(aClient: TDefaultHttpClient;
  aRequest: THttpRequest; aResponse: THttpResponse; args: TStrings): boolean;
begin
  if (UserName <> '') and active then
    aRequest.addHeader('Proxy-Authorization', Format('Basic %s',
      [EncodeStringBase64(UserName + ':' + UserName)]));
  Result := True;
end;

{ TBasicAuthorizationRequestPreprocess }

procedure TBasicAuthorizationRequestPreprocess.SetPassword(AValue: string);
begin
  if FPassword = AValue then
    Exit;
  FPassword := AValue;
end;

procedure TBasicAuthorizationRequestPreprocess.SetUserName(AValue: string);
begin
  if FUserName = AValue then
    Exit;
  FUserName := AValue;
end;

constructor TBasicAuthorizationRequestPreprocess.Create;
begin
  FUserName := '';
  FPassword := '';
end;

function TBasicAuthorizationRequestPreprocess.process(aClient: TDefaultHttpClient;
  aRequest: THttpRequest; aResponse: THttpResponse; args: TStrings): boolean;
begin
  if active then
    aRequest.setHeader('Authorization', Format('Basic %s',
      [EncodeStringBase64(UserName + ':' + Password)]));
end;

{ TCorrelationIdPreprocess }

function TCorrelationIdPreprocess.process(aClient: TDefaultHttpClient;
  aRequest: THttpRequest; aResponse: THttpResponse; args: TStrings): boolean;
begin
  if aResponse.hasHeader('X-Correlation-ID') then
    aRequest.setHeader('X-Correlation-ID', aResponse.getHeader('X-Correlation-ID'))
  else
    aRequest.addHeader('X-Correlation-ID', TGuid.NewGuid.ToString(True));
  if aResponse.hasHeader('X-Request-ID') then
    aRequest.setHeader('X-Correlation-ID', TGuid.NewGuid.ToString(True));
  Result := True;
end;

{ TCookieMiddlewarePreprocess }

function TCookieMiddlewarePreprocess.process(aClient: TDefaultHttpClient;
  aRequest: THttpRequest; aResponse: THttpResponse; args: TStrings): boolean;
var
  cookies: TStringList;
  h: THTTPHeader;
  cookieValues: TStringArray;
  s: string;
begin
  cookies := TStringList.Create;
  cookies.LineBreak := ';';
  cookies.Duplicates := dupIgnore;
  cookieValues := aRequest.getHeader('Cookie').Split(';');
  for s in cookieValues do
    cookies.Add(s);
  setLength(cookieValues, 0);

  for h in aResponse.getHeaders do
    if (CompareText(h.Name, paxhttp.Messages.SetCookie) = 0) then
    begin
      cookieValues := h.Value.Split(';');
      cookies.Add(cookieValues[0]);
      setLength(cookieValues, 0);
    end;
  aRequest.setHeader('Cookie', cookies.Text);
  cookies.Clear;
  FreeAndNil(cookies);
  Result := True;
end;

end.
