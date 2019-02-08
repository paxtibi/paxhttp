unit paxhttp.server.sessions;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, fphttp, httpdefs, IniFiles;

type
  { TSlimSession }

  TSlimSession = class(TCustomSession)
  private
    FSessionStarted: boolean;
    FCached: boolean;
    FServerFile: TMemInifile;
    FSessionDir: string;
    FTerminated: boolean;
    SID: string;
    procedure UpdateIniFile;
  protected
    function CreateIniFile(const AFN: string): TMemIniFile; virtual;
    procedure FreeIniFile;
    procedure CheckSession;
    function GetSessionID: string; override;
    function GetSessionVariable(VarName: string): string; override;
    procedure SetSessionVariable(VarName: string; const AValue: string); override;
    property Cached: boolean read FCached write FCached;
    property SessionDir: string read FSessionDir write FSessionDir;
    property ServerFile: TMemIniFile read FServerFile write FServerFile;
  public
    destructor Destroy; override;
    procedure Terminate; override;
    procedure UpdateResponse(AResponse: TResponse); override;
    procedure InitSession(ARequest: TRequest; OnNewSession, OnExpired: TNotifyEvent);
      override;
    procedure InitResponse(AResponse: TResponse); override;
    procedure RemoveVariable(VariableName: string); override;
    function GetSessionDir: string;
    function isStarted: boolean;
    function start(ARequest: TRequest; ARsponse: TResponse;
      OnNewSession: TNotifyEvent = nil; OnExpired: TNotifyEvent = nil): boolean;
    function whenStarted: TDateTime;
  end;

  TSlimSessionClass = class of TSlimSession;

  { TSlimSessionFactory }

  TSlimSessionFactory = class(TSessionFactory)
  private
    FCached: boolean;
    FSessionDir: string;
    procedure SetCached(const AValue: boolean);
    procedure SetSessionDir(const AValue: string);
  protected
    procedure DeleteSessionFile(const AFileName: string); virtual;
    function SessionExpired(Ini: TMemIniFile): boolean;
    procedure CheckSessionDir; virtual;
    function DoCreateSession(ARequest: TRequest): TCustomSession; override;
    procedure DoCleanupSessions; override;
    procedure DoDoneSession(var ASession: TCustomSession); override;
    function SessionFilePrefix: string; virtual;
  public
    property SessionDir: string read FSessionDir write SetSessionDir;
    property Cached: boolean read FCached write SetCached;
  end;

const
  MaxIniCreate = 5;

implementation


const
  // Sections in ini file
  SSession = 'Session';
  SData = 'Data';

  KeyStart = 'Start';         // Start time of session
  KeyLast = 'Last';          // Last seen time of session
  KeyTimeOut = 'Timeout';       // Timeout in seconds;

resourcestring
  SErrSessionTerminated = 'No web session active: Session was terminated';
  SErrNoSession = 'No web session active: Session was not started';

{ TSlimSessionFactory }

procedure TSlimSessionFactory.SetCached(const AValue: boolean);
begin
  if FCached = AValue then
    exit;
  FCached := AValue;
end;

procedure TSlimSessionFactory.SetSessionDir(const AValue: string);
begin
  if FSessionDir = AValue then
    exit;
  FSessionDir := AValue;
end;

procedure TSlimSessionFactory.DeleteSessionFile(const AFileName: string);
begin
  if FileExists(AFileName) then
    DeleteFile(AFileName);
end;

function TSlimSessionFactory.SessionExpired(Ini: TMemIniFile): boolean;
var
  L: TDateTime;
  T: integer;
begin
  L := Ini.ReadDateTime(SSession, KeyLast, 0);
  T := Ini.ReadInteger(SSession, KeyTimeOut, DefaultTimeOutMinutes);
  Result := ((Now - L) > (T / (24 * 60)));
end;

procedure TSlimSessionFactory.CheckSessionDir;
var
  TD: string;
begin
  if (FSessionDir = '') then
  begin
    TD := IncludeTrailingPathDelimiter(GetTempDir(True));
    FSessionDir := TD + 'slimsession' + PathDelim;
    if not ForceDirectories(FSessionDir) then
      FSessionDir := TD;
  end;
end;


function TSlimSessionFactory.DoCreateSession(ARequest: TRequest): TCustomSession;
var
  S: TSlimSession;
begin
  CheckSessionDir;
  S := TSlimSession.Create(nil);
  S.SessionDir := SessionDir;
  S.SessionCookie := SessionCookie;
  S.SessionCookiePath := SessionCookiePath;
  S.Cached := Cached;
  Result := S;
end;

procedure TSlimSessionFactory.DoCleanupSessions;
var
  Info: TSearchRec;
  Ini: TMemIniFile;
  FN: string;
begin
  CheckSessionDir;
  if FindFirst(SessionDir + AllFilesMask, 0, info) = 0 then
    try
      repeat
        if (Info.Attr and faDirectory = 0) then
        begin
          FN := SessionDir + Info.Name;
          Ini := TMeminiFile.Create(FN);
          try
            if SessionExpired(Ini) then
              DeleteSessionFile(FN);
          finally
            Ini.Free;
          end;
        end;
      until FindNext(Info) <> 0;
    finally
      FindClose(Info);
    end;
end;

procedure TSlimSessionFactory.DoDoneSession(var ASession: TCustomSession);
begin
  FreeAndNil(ASession);
end;

function TSlimSessionFactory.SessionFilePrefix: string;
begin
  Result := 'slimsession';
end;

{ TSlimSession }

function TSlimSession.GetSessionDir: string;
begin
  Result := SessionDir;
end;

function TSlimSession.isStarted: boolean;
begin
  Result := FSessionStarted;
end;

function TSlimSession.start(ARequest: TRequest; ARsponse: TResponse;
  OnNewSession: TNotifyEvent; OnExpired: TNotifyEvent): boolean;
begin
  InitSession(ARequest, OnNewSession, OnExpired);
  InitResponse(ARsponse);
end;

function TSlimSession.whenStarted: TDateTime;
begin
  if assigned(FServerFile) then
  begin
    Result := FServerFile.ReadDateTime(SSession, KeyStart, 0);
  end
  else
    Result := 0;
end;

function TSlimSession.GetSessionID: string;
var
  GUID: TGUID;
begin
  if SID = '' then
  begin
    CreateGUID(GUID);
    SetLength(SID, 32);
    StrLFmt(PChar(SID), 32, '%.8x%.4x%.4x%.2x%.2x%.2x%.2x%.2x%.2x%.2x%.2x',
      [longint(GUID.D1), GUID.D2, GUID.D3, GUID.D4[0], GUID.D4[1], GUID.D4[2],
      GUID.D4[3], GUID.D4[4], GUID.D4[5], GUID.D4[6], GUID.D4[7]]);
  end;
  Result := SID;
end;

procedure TSlimSession.UpdateIniFile;
var
  ACount: integer;
  OK: boolean;
begin
  ACount := 0;
  OK := False;
  repeat
    Inc(ACount);
    try
      TMemIniFile(FServerFile).UpdateFile;
      OK := True;
    except
      On E: EFCreateError do
      begin
        if ACount > MaxIniCreate then
          raise;
        Sleep(20);
      end;
      On E: EFOpenError do
      begin
        if ACount > MaxIniCreate then
          raise;
        Sleep(20);
      end;
      On E: Exception do
        raise;
    end;
  until OK;
end;

function TSlimSession.CreateIniFile(const AFN: string): TMemIniFile;
var
  ACount: integer;
begin
  ACount := 0;
  Result := nil;
  repeat
    Inc(ACount);
    try
      Result := TMemIniFile.Create(AFN, False);
    except
      On E: EFCreateError do
      begin
        if ACount > MaxIniCreate then
          raise;
        Sleep(20);
      end;
      On E: EFOpenError do
      begin
        if ACount > MaxIniCreate then
          raise;
        Sleep(20);
      end;
      On E: Exception do
        raise;
    end;
  until (Result <> nil);
end;

procedure TSlimSession.FreeIniFile;
begin
  if Cached and Assigned(FServerFile) then
    UpdateIniFile;
  FreeAndNil(FServerFile);
end;


procedure TSlimSession.CheckSession;
begin
  if not Assigned(FServerFile) then
    if FTerminated then
      raise EWebSessionError.Create(SErrSessionTerminated)
    else
      raise EWebSessionError.Create(SErrNoSession);
end;

function TSlimSession.GetSessionVariable(VarName: string): string;
begin
  CheckSession;
  Result := FServerFile.ReadString(SData, VarName, '');
end;

procedure TSlimSession.SetSessionVariable(VarName: string; const AValue: string);
begin
  CheckSession;
  FServerFile.WriteString(SData, VarName, AValue);
  if not Cached then
    UpdateIniFile;
end;

destructor TSlimSession.Destroy;
begin
  // In case an exception occurred and UpdateResponse is not called,
  // write the updates to disk and free FServerFile
  FreeIniFile;
  inherited Destroy;
end;

procedure TSlimSession.Terminate;
begin
  FTerminated := True;
  if Assigned(FServerFile) then
  begin
    DeleteFile(FServerFile.FileName);
    FreeAndNil(FServerFile);
  end;
end;

procedure TSlimSession.UpdateResponse(AResponse: TResponse);
begin
  FreeIniFile;
end;

procedure TSlimSession.InitSession(ARequest: TRequest;
  OnNewSession, OnExpired: TNotifyEvent);

var
  S, FN: string;
  SF: TSlimSessionFactory;
begin
  SF := SessionFactory as TSlimSessionFactory;
  // First initialize all session-dependent properties to their default, because
  // in Apache-modules or fcgi programs the session-instance is re-used
  SID := '';
  FSessionStarted := False;
  FTerminated := False;
  // If a exception occurred during a prior request FServerFile is still not freed
  if assigned(FServerFile) then
    FreeIniFile;
  if (SessionCookie = '') then
    SessionCookie := SFPWebSession;
  S := ARequest.CookieFields.Values[SessionCookie];
  if (S <> '') then
  begin
    FN := IncludeTrailingPathDelimiter(SessionDir) + SF.SessionFilePrefix + S;
    if not FileExists(FN) then
    begin
      FN := IncludeTrailingPathDelimiter(SessionDir) + SF.SessionFilePrefix + SessionID;
    end;
    FServerFile := CreateIniFile(FN);
    if SF.SessionExpired(FServerFile) then
    begin
      if Assigned(OnExpired) then
        OnExpired(Self);
      SF.DeleteSessionFile(FServerFile.FileName);
      FreeAndNil(FServerFile);
      S := '';
    end
    else
      SID := S;
  end;
  if S = '' then
  begin
    if Assigned(OnNewSession) then
      OnNewSession(Self);
    GetSessionID;
    S := IncludeTrailingPathDelimiter(SessionDir) + SF.SessionFilePrefix + SessionID;
    FServerFile := CreateIniFile(S);
    FServerFile.WriteDateTime(SSession, KeyStart, Now);
    FServerFile.WriteInteger(SSession, KeyTimeOut, Self.TimeOutMinutes);
    FSessionStarted := True;
  end;
  FServerFile.WriteDateTime(SSession, KeyLast, Now);
  if not FCached then
    UpdateIniFile;
end;

procedure TSlimSession.InitResponse(AResponse: TResponse);
var
  C: TCookie;
begin
  C := AResponse.Cookies.FindCookie(SessionCookie);
  if (C = nil) then
  begin
    C := AResponse.Cookies.Add;
    C.Name := SessionCookie;
  end;
  if FTerminated then
  begin
    C.Value := '';
  end
  else
  begin
    C.Value := SID;
    C.Path := SessionCookiePath;
  end;
end;

procedure TSlimSession.RemoveVariable(VariableName: string);
begin
  CheckSession;
  FServerFile.DeleteKey(SData, VariableName);
  if not Cached then
    UpdateIniFile;
end;


initialization
  SessionFactoryClass := TSlimSessionFactory;
end.
