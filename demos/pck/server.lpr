program server;

{$mode objfpc}{$H+}

uses
  {$IFDEF UseCThreads}
  cthreads, {$ENDIF}
  Classes,
  SysUtils,
  RegExpr,
  HTTPDefs,
  iniwebsession,
  paxhttp.server,
  paxhttp.server.sessions,
  MainHandler { you can add units after this };

var
  Application: TCustomSlimHttpApplication;

  {$R *.res}

var
  Main: TMainHandler;
begin
  Application := TCustomSlimHttpApplication.Create(nil);
  Application.Title := 'HTTPServerApplication';
  Application.Port := 8080;
  Main := TMainHandler.Create(Application);

  Application.AddRoute('get', '/', @Main.HandleIndex);
  Application.AddRoute('get', '/login', @Main.HandleLogin);
  Application.AddRoute('get', '/{argument}/help', @Main.HandleHelp);
  Application.AddRoute('get', '/user/{id:"([\d]*)"}', @Main.HandleUserInfo);
  Application.AddRoute('update', '/user/{id}', @Main.HandleIndex);
  Application.Run;
  Application.Free;
end.
