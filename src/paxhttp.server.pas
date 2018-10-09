unit paxhttp.server;

{$mode objfpc}{$H+}

interface

uses
  fgl, Classes, SysUtils, custhttpapp, custweb, HTTPDefs, httproute, RegExpr;

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

  { TCustomSlimHttpApplication }

  TCustomSlimHttpApplication = class(TCustomHTTPApplication)
  protected
    FRoutesCriticalSection: TRTLCriticalSection;
    FRoutes: TRouteContainerList;
    function InitializeWebHandler: TWebHandler; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure AddRoute(aMethod, aUrlPattern: string; delegate: TRouteProcedure);
    procedure AddRoute(aMethod, aUrlPattern: string; delegate: TRoute);
    procedure AddRoute(aMethod, aUrlPattern: string; delegate: IRoute);
    procedure AddRoute(aMethod, aUrlPattern: string; delegate: TRouteMethod);
    function getCandidates(aRequest: TRequest): TRouteContainerList;
  end;

  { THTTPServerApplicationHandler }

  THTTPServerApplicationHandler = class(TFPHTTPServerHandler)
  protected
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  published
  end;

implementation

const
  RegString = '(\[?\/?\{([\w_][\w\d_-]*|[\w_][\w\d_-]*(:"(.*)"))\}\]?)';

{ TRouteContainerProcedure }

procedure TRouteContainerProcedure.SetTarget(AValue: TRouteProcedure);
begin
  if FTarget = AValue then
    Exit;
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
    Exit;
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
    Exit;
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
    Exit;
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
      repeat
        Writeln(MatchLen[4]: 10, ' ', MatchLen[3]: 10, ' ', MatchLen[2]: 10, ' ', MatchLen[1]: 10);
        Writeln(Match[4]: 10, ' ', Match[3]: 10, ' ', Match[2]: 10, ' ', Match[1]: 10);
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
            separator := '';
        end;
        Result += '(' + separator + currentMatch + ')' + optional;
        PrevPos := MatchPos[0] + MatchLen[0];
      until not ExecNext;
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
      repeat
        Writeln(MatchLen[4]: 10, ' ', MatchLen[3]: 10, ' ', MatchLen[2]: 10, ' ', MatchLen[1]: 10);
        Writeln(Match[4]: 10, ' ', Match[3]: 10, ' ', Match[2]: 10, ' ', Match[1]: 10);
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
  regExp.Free;
end;

function TRouteContainer.getNormalizedUrl(aUrl: string): string;
begin
  result := aUrl;
  if Pos('?', result) > 0 then
  begin
    delete(result, Pos('?', Result), length(result));
  end;
  if Pos('#', result) > 0 then
  begin
    delete(result, Pos('#', result), length(result));
  end;
end;

function TRouteContainer.urlMatchPattern(aUrl: string): boolean;
begin
  result := FRegExpr.Exec(getNormalizedUrl(aUrl));
end;

procedure TRouteContainer.SetrequestMethod(AValue: string);
begin
  if FrequestMethod = AValue then
    Exit;
  FrequestMethod := AValue;
end;

procedure TRouteContainer.SeturlPattern(AValue: string);
begin
  if FurlPattern = AValue then
    Exit;
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
  result := produceParameters(urlPattern);
  Result.LineBreak := '<BR>';
  if result.Count > 0 then
    with FRegExpr do
    begin
      if Exec(getNormalizedUrl(aRequest.URL)) then
      begin
        idx := 0;
        repeat
          //Writeln(MatchLen[4]: 10, ' ', MatchLen[3]: 10, ' ', MatchLen[2]: 10, ' ', MatchLen[1]: 10);
          //Writeln(Match[4]: 10, ' ', Match[3]: 10, ' ', Match[2]: 10, ' ', Match[1]: 10);
          result[idx] := result[idx] + '=' + Copy(Match[1], 2, Length(Match[1]));
          idx += 1;
        until not ExecNext;
      end;
    end;
  args := aRequest.QueryString.Split('?');
  for arg in args do
  begin
    result.Add(arg);
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

procedure THTTPServerApplicationHandler.HandleRequest(ARequest: TRequest; AResponse: TResponse);
var
  cwebapp: TCustomSlimHttpApplication;
  list: TRouteContainerList;
  route: TRouteContainer;
begin
  try
    cwebapp := GetOwner as TCustomSlimHttpApplication;
    list := cwebapp.getCandidates(ARequest);
    for route in list do
      route.Execute(ARequest, AResponse);
    try
      if list.Count = 0 then
        inherited HandleRequest(ARequest, AResponse);
    finally
      list.Free;
    end;
  except
    On E: Exception do
      ShowRequestException(AResponse, E);
  end;
end;


{ TCustomSlimHttpApplication }

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
end;

destructor TCustomSlimHttpApplication.Destroy;
begin
  inherited Destroy;
  FRoutes.Free;
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

function TCustomSlimHttpApplication.getCandidates(aRequest: TRequest): TRouteContainerList;
var
  c: TRouteContainer;
  url: string;
begin
  url := aRequest.URL;
  result := TRouteContainerList.Create(False);
  for c in FRoutes do
  begin
    if (compareText(c.requestMethod, aRequest.Method) = 0) and (c.urlMatchPattern(URL)) then
    begin
      result.add(c);
    end;
  end;
end;

end.
