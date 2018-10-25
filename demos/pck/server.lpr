program server;

{$mode objfpc}{$H+}

uses {$IFDEF UseCThreads}
  cthreads, {$ENDIF}
  Classes,
  SysUtils,
  RegExpr,
  HTTPDefs,
  paxhttp.server,
  paxhttp.server.sessions,
  MainHandler { you can add units after this };

var
  Application: TCustomSlimHttpApplication;

{$R *.res}


  procedure HandleIndex(aRequest: TRequest; aResponse: TResponse; args: TStrings);
  begin
    aResponse.Content := 'INDEX';
  end;

  procedure ToggleAutoSession(aRequest: TRequest; aResponse: TResponse; args: TStrings);
  begin
    Application.SessionHandler.AutoStart := not Application.SessionHandler.AutoStart;
    aResponse.Content := 'AUTOSTART SESSIONS : ' + BoolToStr(Application.SessionHandler.AutoStart, True);
  end;

  procedure StopSession(aRequest: TRequest; aResponse: TResponse; args: TStrings);
  var
    session: TSlimSession;
  begin
    session := ((aRequest as TSlimRequest).Session as TSlimSession);
    if session.isStarted then
      aResponse.Content := 'Session : ' + session.SessionID + ' Terminated';
    session.Terminate;
  end;

  procedure StartSession(aRequest: TRequest; aResponse: TResponse; args: TStrings);
  var
    session: TSlimSession;
  begin
    session := ((aRequest as TSlimRequest).Session as TSlimSession);
    if not (Application.SessionHandler.AutoStart) then
    begin
      if not session.isStarted then
      begin
        session.start(aRequest, aResponse, nil, nil);
      end;
    end;
    if session.Variables['Counter'] = '' then
    begin
      session.Variables['Counter'] := '1';
    end
    else
    begin
      session.Variables['Counter'] := (session.Variables['Counter'].ToInteger + 1).ToString;
    end;
    aResponse.Content := 'Session : ' + session.SessionID + '<BR>' + 'Started :' + DateTimeToStr(session.whenStarted);
  end;

var
  Main: TMainHandler;
begin
  Application := TCustomSlimHttpApplication.Create(nil);
  Application.Title := 'HTTPServerApplication';
  Application.SessionHandler.Active := True;
  Application.SessionHandler.SessionName := 'demoserver';
  Application.SessionHandler.SessionPath := '/';
  Application.SessionHandler.SessionStorePath := '../data/sessions';
  Application.SessionHandler.AutoStart := False;
  Application.SessionHandler.Exires := 60;

  Main := TMainHandler.Create(Application);

  Application.AddRoute('get', '/', @HandleIndex);
  Application.AddRoute('get', '/session/start', @StartSession);
  Application.AddRoute('get', '/session/stop', @StopSession);
  Application.AddRoute('get', '/session/toggle', @ToggleAutoSession);
  Application.AddRoute('get', '/login', @Main.HandleIndex);
  Application.AddRoute('get', '/{argument}/help', @Main.HandleHelp);
  Application.AddRoute('get', '/user/{id:"[\d]*"}', @Main.HandleUserInfo);
  Application.AddRoute('update', '/user/{id}', @Main.HandleIndex);
  Application.Run;
  Application.Free;
end.
