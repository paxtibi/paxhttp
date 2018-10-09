program server;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  RegExpr,
  paxhttp.server,
  MainHandler { you can add units after this };

var
  Application: TCustomSlimHttpApplication;

{$R *.res}

var
  Main: TMainHandler;

begin
  Application := TCustomSlimHttpApplication.Create(nil);
  Application.Title := 'HTTPServerApplication';

  Main := TMainHandler.Create(Application);
  Application.AddRoute('get', '/', @Main.HandleIndex);
  Application.AddRoute('get', '/login', @Main.HandleIndex);
  Application.AddRoute('get', '/{argument}/help', @Main.HandleHelp);
  Application.AddRoute('get', '/user/{id:"[\d]*"}', @Main.HandleUserInfo);
  Application.AddRoute('update', '/user/{id}', @Main.HandleIndex);

  Application.Run;
  Application.Free;
end.

