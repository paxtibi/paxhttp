{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit http;

{$warn 5023 off : no warning about unused units}
interface

uses
  http.client, http.messages, http.encoders, http.middlewares, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('http', @Register);
end.
