program ServerTest;

{$mode objfpc}{$H+}

uses
  Classes,
  consoletestrunner,
  Match,
  paxhttp_package, ssl, paxtibi.openssl;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  end;

var
  Application: TMyTestRunner;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
