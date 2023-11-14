program stoppableserver;

{$D+}
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  paxhttp.Messages,
  custhttpapp,
  HTTPDefs,
  fpmustache,
  paxhttp.server;

  {******************************************************************************}
type
  { TStoppableServer }
  {****************************************************************************}
  TStoppableServer = class(TCustomSlimHttpApplication)
  protected
    procedure DoServerStop(aReq: TRequest; aResp: TResponse; args: TStrings);
    procedure DoEcho(aReq: TRequest; aResp: TResponse; args: TStrings);
    procedure DoHomePage(aReq: TRequest; aResp: TResponse; args: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
  {****************************************************************************}

var
  Application: TStoppableServer;

const
  HTML = '<body><a href="/server-stop">Stop</a><br><a href="/echo/prova">Messagio di Prova</a> <body>';

  {****************************************************************************}
  procedure TStoppableServer.DoServerStop(aReq: TRequest; aResp: TResponse; args: TStrings);
  begin
    aResp.Content := 'Bye!';
    Application.Terminate;
  end;

  procedure TStoppableServer.DoEcho(aReq: TRequest; aResp: TResponse; args: TStrings);
  begin
    aResp.Content := args.Text;
  end;

  procedure TStoppableServer.DoHomePage(aReq: TRequest; aResp: TResponse; args: TStrings);
  begin
    aResp.Content := HTML;
  end;

  {****************************************************************************}
  constructor TStoppableServer.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    AddRoute('GET', '/server-stop', @DoServerStop);
    AddRoute('GET', '/echo/{message}', @DoEcho);
    AddRoute('GET', '/', @DoHomePage);
  end;

  {****************************************************************************}
  destructor TStoppableServer.Destroy;
  begin
    inherited Destroy;
  end;

  {****************************************************************************}
begin
  Application := TStoppableServer.Create(nil);
  Application.Title := 'Stoppable Server';
  Application.Port := 3000;
  Application.Run;
  Application.Free;
end.
{******************************************************************************}
