unit paxtibi.openssl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TEncryptMethod = (HS256, HS384, HS512, RS256, RS384, RS512, ES256, ES384, ES512, PS256, PS384, PS512);

function encrypt(method: TEncryptMethod; data: string): string;

implementation

uses
  openssl;

function encrypt(method: TEncryptMethod; data: string): string;
begin

end;


end.
