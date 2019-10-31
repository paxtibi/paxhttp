unit ssl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  TTestOpenSSL= class(TTestCase)
  published
    procedure TestHookUp;
  end;

implementation
Uses
  OpenSSL;

procedure TTestOpenSSL.TestHookUp;
begin
  AssertTrue(InitSSLInterface);
end;



initialization

  RegisterTest(TTestOpenSSL);
end.

