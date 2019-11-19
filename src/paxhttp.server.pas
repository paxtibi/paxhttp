unit paxhttp.server;
{$D+}
{$mode objfpc}{$H+}

interface

uses
  fgl, Classes, SysUtils, custhttpapp, custweb, HTTPDefs, RegExpr, fphttpserver;

type
  TRouteProcedure = procedure(aReq: TRequest; aResp: TResponse; args: TStrings);
  TRouteMethod = procedure(aReq: TRequest; aResp: TResponse; args: TStrings) of object;

  TRoute = class
    procedure handleRequest(aReq: TRequest; aResp: TResponse; args: TStrings); virtual; abstract;
  end;

  IRoute = interface
    ['{D8FA16D3-D3B8-4A54-96B9-FEA071BA1201}']
    procedure handleRequest(aReq: TRequest; aResp: TResponse; args: TStrings);
  end;

  { TRouteContainer }

  TRouteContainer = class
  private
    FrequestMethod: string;
    FurlPattern: string;
    FRegExpression: string;
    procedure SetrequestMethod(AValue: string);
    procedure SeturlPattern(AValue: string);
  protected
    FRegExpr: TRegExpr;
  protected
    class function produceURLPattern(AInputStr: string): string;
    class function produceParameters(AInputStr: string): TStringList;
    function getNormalizedUrl(aUrl: string): string;
    function extractArgs(aRequest: TRequest): TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure HandleRequest(aRequest: TRequest; aResponse: TResponse; args: TStrings); virtual;
    function urlMatchPattern(aUrl: string): boolean;
    procedure Execute(aReq: TRequest; aResp: TResponse); virtual; abstract;
  published
    property urlPattern: string read FurlPattern write SeturlPattern;
    property requestMethod: string read FrequestMethod write SetrequestMethod;
  end;

  TRouteContainerList = specialize TFPGObjectList<TRouteContainer>;

  { TRouteContainerMethod }

  TRouteContainerMethod = class(TRouteContainer)
  private
    FTarget: TRouteMethod;
    procedure SetTarget(AValue: TRouteMethod);
  public
    procedure Execute(aReq: TRequest; aResp: TResponse); override;
  published
    property Target: TRouteMethod read FTarget write SetTarget;
  end;

  { TRouteContainerObject }

  TRouteContainerObject = class(TRouteContainer)
  private
    FTarget: TRoute;
    procedure SetTarget(AValue: TRoute);
  public
    procedure Execute(aReq: TRequest; aResp: TResponse); override;
  published
    property Target: TRoute read FTarget write SetTarget;
  end;

  { TRouteContainerInterface }

  TRouteContainerInterface = class(TRouteContainer)
  private
    FTarget: IRoute;
    procedure SetTarget(AValue: IRoute);
  public
    procedure Execute(aReq: TRequest; aResp: TResponse); override;
  published
    property Target: IRoute read FTarget write SetTarget;
  end;

  { TRouteContainerProcedure }

  TRouteContainerProcedure = class(TRouteContainer)
  private
    FTarget: TRouteProcedure;
    procedure SetTarget(AValue: TRouteProcedure);
  public
    procedure Execute(aReq: TRequest; aResp: TResponse); override;
  public
    property Target: TRouteProcedure read FTarget write SetTarget;
  end;

  TCustomSlimHttpApplication = class;

  { TServerMiddleware }

  TServerMiddleware = class(TComponent)
  private
    FActive: boolean;
    FApplication: TCustomSlimHttpApplication;
    procedure SetActive(AValue: boolean);
    procedure SetApplication(AValue: TCustomSlimHttpApplication);
  protected
    function invoke(ARequest: TRequest; AResponse: TResponse): boolean; virtual;
  public
    constructor Create(anApplication: TCustomSlimHttpApplication); virtual;
    property Application: TCustomSlimHttpApplication read FApplication write SetApplication;
    property Active: boolean read FActive write SetActive;
  end;

  TServerMiddlewareList = specialize TFPGObjectList<TServerMiddleware>;

  TServerRequestEvent = procedure(Sender: TObject; ARequest: TRequest; AResponse: TResponse) of object;

  { TCustomSlimHttpApplication }

  TCustomSlimHttpApplication = class(TCustomHTTPApplication)
  private
    FAfterServe: TServerRequestEvent;
    FBeforeServe: TServerRequestEvent;
    procedure SetAfterServe(AValue: TServerRequestEvent);
    procedure SetBeforeServe(AValue: TServerRequestEvent);
  protected
    FRoutesCriticalSection: TRTLCriticalSection;
    FRoutes: TRouteContainerList;
    FMiddleware: TServerMiddlewareList;
    function InitializeWebHandler: TWebHandler; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure AddRoute(aMethod, aUrlPattern: string; delegate: TRouteProcedure);
    procedure AddRoute(aMethod, aUrlPattern: string; delegate: TRoute);
    procedure AddRoute(aMethod, aUrlPattern: string; delegate: IRoute);
    procedure AddRoute(aMethod, aUrlPattern: string; delegate: TRouteMethod);
    procedure ClearRouters;
    function getCandidates(aRequest: TRequest): TRouteContainerList;
    function getRoutersList: TRouteContainerList;
    property BeforeServe: TServerRequestEvent read FBeforeServe write SetBeforeServe;
    property AfterServe: TServerRequestEvent read FAfterServe write SetAfterServe;
  end;

  { TSlimEmbeddedHttpServer }

  TSlimEmbeddedHttpServer = class(TEmbeddedHttpServer)
  public
    function CreateRequest: TFPHTTPConnectionRequest; override;
  end;

  { TSlimRequest }

  TSlimRequest = class(TFPHTTPConnectionRequest)
  private
    FSession: TCustomSession;
    procedure SetSession(AValue: TCustomSession);
  public
    constructor Create; override;
    destructor Destroy; override;
    property Session: TCustomSession read FSession write SetSession;
  end;

  { THTTPServerApplicationHandler }

  THTTPServerApplicationHandler = class(TFPHTTPServerHandler)
  protected
    function CreateServer: TEmbeddedHttpServer; override;
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  published
  end;


procedure defaultFavIcon(aReq: TRequest; aResp: TResponse; args: TStrings);

implementation

uses
  fphttp;

const
  RegString = '(\[?\/?\{([\w_][\w\d_-]*|[\w_][\w\d_-]*(:"(.*)"))\}\]?)';

procedure defaultFavIcon(aReq: TRequest; aResp: TResponse; args: TStrings);
begin
  aResp.Code := 404;
  aResp.CodeText := 'Not found';
  if FileExists('favicon.ico') then
  begin
    aResp.Code := 200;
    aResp.CodeText := 'OK';
    aResp.ContentStream := TFileStream.Create('favicon.ico', fmOpenRead or fmShareDenyWrite);
  end;
end;

{ TSlimEmbeddedHttpServer }

function TSlimEmbeddedHttpServer.CreateRequest: TFPHTTPConnectionRequest;
begin
  Result := TSlimRequest.Create;
end;

{ TSlimRequest }

procedure TSlimRequest.SetSession(AValue: TCustomSession);
begin
  if FSession = AValue then
  begin
    Exit;
  end;
  FSession := AValue;
end;

constructor TSlimRequest.Create;
begin
  inherited Create;
  FSession := nil;
end;

destructor TSlimRequest.Destroy;
begin
  FreeAndNil(FSession);
  inherited Destroy;
end;

{ TServerMiddleware }

procedure TServerMiddleware.SetApplication(AValue: TCustomSlimHttpApplication);
begin
  if FApplication = AValue then
  begin
    Exit;
  end;
  FApplication := AValue;
end;

procedure TServerMiddleware.SetActive(AValue: boolean);
begin
  if FActive = AValue then
  begin
    Exit;
  end;
  FActive := AValue;
end;

constructor TServerMiddleware.Create(anApplication: TCustomSlimHttpApplication);
begin
  FApplication := anApplication;
  FActive := True;
end;

function TServerMiddleware.invoke(ARequest: TRequest; AResponse: TResponse): boolean;
begin
  Result := False;
end;

{ TRouteContainerProcedure }

procedure TRouteContainerProcedure.SetTarget(AValue: TRouteProcedure);
begin
  if FTarget = AValue then
  begin
    Exit;
  end;
  FTarget := AValue;
end;

procedure TRouteContainerProcedure.Execute(aReq: TRequest; aResp: TResponse);
var
  args: TStringList;
begin
  args := extractArgs(areq);
  FTarget(aReq, aResp, args);
  Args.Free;
end;

{ TRouteContainerInterface }

procedure TRouteContainerInterface.SetTarget(AValue: IRoute);
begin
  if FTarget = AValue then
  begin
    Exit;
  end;
  FTarget := AValue;
end;

procedure TRouteContainerInterface.Execute(aReq: TRequest; aResp: TResponse);
var
  args: TStringList;
begin
  args := extractArgs(areq);
  FTarget.handleRequest(aReq, aResp, args);
  Args.Free;
end;

{ TRouteContainerObject }

procedure TRouteContainerObject.SetTarget(AValue: TRoute);
begin
  if FTarget = AValue then
  begin
    Exit;
  end;
  FTarget := AValue;
end;

procedure TRouteContainerObject.Execute(aReq: TRequest; aResp: TResponse);
var
  args: TStringList;
begin
  args := extractArgs(areq);
  FTarget.handleRequest(aReq, aResp, args);
  Args.Free;
end;

{ TRouteContainerMethod }

procedure TRouteContainerMethod.SetTarget(AValue: TRouteMethod);
begin
  if FTarget = AValue then
  begin
    Exit;
  end;
  FTarget := AValue;
end;

procedure TRouteContainerMethod.Execute(aReq: TRequest; aResp: TResponse);
var
  args: TStringList;
begin
  args := extractArgs(areq);
  FTarget(aReq, aResp, args);
  Args.Free;
end;

{ TRouteContainer }

class function TRouteContainer.produceURLPattern(AInputStr: string): string;
var
  regExp: TRegExpr;
var
  PrevPos: PtrInt;
  optional: string;
  currentMatch: string;
  separator: string;
begin
  RegExp := TRegExpr.Create(regString);
  regExp.ModifierG := True;
  with regExp do
  begin
    Result := '^';
    PrevPos := 1;
    if Exec(AInputStr) then
    begin
      repeat
        optional := '';
        separator := '/';
        Result += System.Copy(AInputStr, PrevPos, MatchPos[0] - PrevPos);
        if (MatchLen[4] > 0) then
        begin
          currentMatch := Match[4];
        end
        else
        begin
          currentMatch := '[\w\d_ ~\-]*';
        end;
        if Match[0][1] = '[' then
        begin
          optional := '?';
          if Match[0][2] <> '/' then
          begin
            separator := '';
          end;
        end;
        Result += '(' + separator + currentMatch + ')' + optional;
        PrevPos := MatchPos[0] + MatchLen[0];
      until not ExecNext;
    end;
    Result := Result + System.Copy(AInputStr, PrevPos, MaxInt);
  end;
  regExp.Free;
  Result += '$';
end;

class function TRouteContainer.produceParameters(AInputStr: string): TStringList;
var
  regExp: TRegExpr;
begin
  Result := TStringList.Create;
  RegExp := TRegExpr.Create(regString);
  regExp.ModifierG := True;
  with regExp do
  begin
    if Exec(AInputStr) then
    begin
      repeat
        if MatchLen[4] = 0 then
        begin
          Result.add(Match[2]);
        end
        else
        begin
          Result.add(Match[2].Split(':')[0]);
        end;
      until not ExecNext;
    end;
  end;
  regExp.Free;
end;

function TRouteContainer.getNormalizedUrl(aUrl: string): string;
begin
  Result := aUrl;
  if Pos('?', Result) > 0 then
  begin
    Delete(Result, Pos('?', Result), length(Result));
  end;
  if Pos('#', Result) > 0 then
  begin
    Delete(Result, Pos('#', Result), length(Result));
  end;
  Result := StringReplace(Result, '//', '/', [rfReplaceAll]);
end;

function TRouteContainer.urlMatchPattern(aUrl: string): boolean;
begin
  Result := FRegExpr.Exec(getNormalizedUrl(aUrl));
end;

procedure TRouteContainer.SetrequestMethod(AValue: string);
begin
  if FrequestMethod = AValue then
  begin
    Exit;
  end;
  FrequestMethod := AValue;
end;

procedure TRouteContainer.SeturlPattern(AValue: string);
begin
  if FurlPattern = AValue then
  begin
    Exit;
  end;
  FurlPattern := AValue;
  FreeAndNil(FRegExpr);
  FRegExpression := ProduceURLPattern(AValue);
  FRegExpr := TRegExpr.Create(FRegExpression);
end;

function TRouteContainer.extractArgs(aRequest: TRequest): TStringList;
var
  idx: integer;
  args: TStringArray;
  arg: string;
begin
  Result := produceParameters(urlPattern);
  Result.LineBreak := '<BR>';
  if Result.Count > 0 then
  begin
    with FRegExpr do
    begin
      if Exec(getNormalizedUrl(aRequest.URL)) then
      begin
        idx := 0;
        repeat
          Result[idx] := Result[idx] + '=' + Copy(Match[1], 2, Length(Match[1]));
          idx += 1;
        until not ExecNext;
      end;
    end;
  end;
  args := aRequest.QueryString.Split('?');
  for arg in args do
  begin
    Result.Add(arg);
  end;
end;

constructor TRouteContainer.Create;
begin
  FRegExpr := nil;
end;

destructor TRouteContainer.Destroy;
begin
  FreeAndNil(FRegExpr);
  inherited Destroy;
end;

procedure TRouteContainer.HandleRequest(aRequest: TRequest; aResponse: TResponse; args: TStrings);
begin

end;


{ THTTPServerApplicationHandler }

function THTTPServerApplicationHandler.CreateServer: TEmbeddedHttpServer;
begin
  Result := TSlimEmbeddedHttpServer.Create(self);
end;

procedure THTTPServerApplicationHandler.HandleRequest(ARequest: TRequest; AResponse: TResponse);
var
  cwebapp: TCustomSlimHttpApplication;
  list: TRouteContainerList;
  route: TRouteContainer;
  middleware: TServerMiddleware;
  stopProcess: boolean;
begin
  try
    cwebapp := GetOwner as TCustomSlimHttpApplication;
    if assigned(cwebapp.BeforeServe) then
    begin
      cwebapp.BeforeServe(cwebapp, ARequest, AResponse);
    end;
    list := cwebapp.getCandidates(ARequest);
    stopProcess := False;
    for middleware in cwebapp.FMiddleware do
    begin
      if middleware.invoke(ARequest, AResponse) then
      begin
        break;
      end;
    end;
    if not stopProcess then
    begin
      for route in list do
      begin
        route.Execute(ARequest, AResponse);
      end;
      try
        if list.Count = 0 then
        begin
          inherited HandleRequest(ARequest, AResponse);
        end;
      finally
        list.Free;
      end;
    end;
    if assigned(cwebapp.AfterServe) then
    begin
      cwebapp.AfterServe(cwebapp, ARequest, AResponse);
    end;
  except
    On E: Exception do
      ShowRequestException(AResponse, E);
  end;
end;


{ TCustomSlimHttpApplication }

procedure TCustomSlimHttpApplication.SetAfterServe(AValue: TServerRequestEvent);
begin
  if FAfterServe = AValue then
  begin
    Exit;
  end;
  FAfterServe := AValue;
end;

procedure TCustomSlimHttpApplication.SetBeforeServe(AValue: TServerRequestEvent);
begin
  if FBeforeServe = AValue then
  begin
    Exit;
  end;
  FBeforeServe := AValue;
end;

function TCustomSlimHttpApplication.InitializeWebHandler: TWebHandler;
begin
  Result := THTTPServerApplicationHandler.Create(Self);
end;

constructor TCustomSlimHttpApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Threaded := True;
  InitCriticalSection(FRoutesCriticalSection);
  FRoutes := TRouteContainerList.Create(True);
  FMiddleware := TServerMiddlewareList.Create(True);
end;

destructor TCustomSlimHttpApplication.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FRoutes);
  FreeAndNil(FMiddleware);
  DoneCriticalsection(FRoutesCriticalSection);
end;

procedure TCustomSlimHttpApplication.AddRoute(aMethod, aUrlPattern: string; delegate: TRouteProcedure);
var
  container: TRouteContainerProcedure;
begin
  EnterCriticalsection(FRoutesCriticalSection);
  container := TRouteContainerProcedure.Create;
  container.urlPattern := aUrlPattern;
  container.requestMethod := aMethod;
  container.Target := delegate;
  FRoutes.Add(container);
  LeaveCriticalsection(FRoutesCriticalSection);
end;

procedure TCustomSlimHttpApplication.AddRoute(aMethod, aUrlPattern: string; delegate: TRoute);
var
  container: TRouteContainerObject;
begin
  EnterCriticalsection(FRoutesCriticalSection);
  container := TRouteContainerObject.Create;
  container.urlPattern := aUrlPattern;
  container.requestMethod := aMethod;
  container.Target := delegate;
  FRoutes.Add(container);
  LeaveCriticalsection(FRoutesCriticalSection);
end;

procedure TCustomSlimHttpApplication.AddRoute(aMethod, aUrlPattern: string; delegate: IRoute);
var
  container: TRouteContainerInterface;
begin
  EnterCriticalsection(FRoutesCriticalSection);
  container := TRouteContainerInterface.Create;
  container.urlPattern := aUrlPattern;
  container.requestMethod := aMethod;
  container.Target := delegate;
  FRoutes.Add(container);
  LeaveCriticalsection(FRoutesCriticalSection);
end;

procedure TCustomSlimHttpApplication.AddRoute(aMethod, aUrlPattern: string; delegate: TRouteMethod);
var
  container: TRouteContainerMethod;
begin
  EnterCriticalsection(FRoutesCriticalSection);
  container := TRouteContainerMethod.Create;
  container.urlPattern := aUrlPattern;
  container.requestMethod := aMethod;
  container.Target := delegate;
  FRoutes.Add(container);
  LeaveCriticalsection(FRoutesCriticalSection);
end;

procedure TCustomSlimHttpApplication.ClearRouters;
begin
  EnterCriticalsection(FRoutesCriticalSection);
  FRoutes.Clear;
  LeaveCriticalsection(FRoutesCriticalSection);
end;

function TCustomSlimHttpApplication.getCandidates(aRequest: TRequest): TRouteContainerList;
var
  c: TRouteContainer;
  url: string;
begin
  url := aRequest.URL;
  Result := TRouteContainerList.Create(False);
  for c in FRoutes do
  begin
    if (compareText(c.requestMethod, aRequest.Method) = 0) and (c.urlMatchPattern(URL)) then
    begin
      Result.add(c);
    end;
  end;
end;

function TCustomSlimHttpApplication.getRoutersList: TRouteContainerList;
begin
  Result := FRoutes;
end;

end.
