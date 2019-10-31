unit paxhttp.jwt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, openssl, fpjson;

implementation

initialization
  openssl.InitSSLInterface;

finalization
  openssl.DestroySSLInterface;

end.
