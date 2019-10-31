program server;

{$mode objfpc}{$H+}

uses {$IFDEF UseCThreads}
  cthreads, {$ENDIF}
  Classes,
  SysUtils,
  RegExpr,
  HTTPDefs,
  paxhttp.jwt,
  paxhttp.server,
  paxhttp.jwt.claims;

var
  Application: TCustomSlimHttpApplication;

  procedure HandleIndex(aRequest: TRequest; aResponse: TResponse; args: TStrings);
  begin
    aResponse.Content := 'INDEX';
  end;

begin
  Application := TCustomSlimHttpApplication.Create(nil);
  Application.Title := 'ServerDemo';

  Application.AddRoute('get', '/', @HandleIndex);
  Application.Run;
  Application.Free;
end.
