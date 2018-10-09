unit MainHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HTTPDefs;

type

  { TMainHandler }

  TMainHandler = class(TComponent)
    procedure HandleIndex(aRequest: TRequest; aResponse: TResponse; args: TStrings);
    procedure HandleLogin(aRequest: TRequest; aResponse: TResponse; args: TStrings);
    procedure HandleUserInfo(aRequest: TRequest; aResponse: TResponse; args: TStrings);
    procedure HandleHelp(aRequest: TRequest; aResponse: TResponse; args: TStrings);
  end;

implementation

{ TMainHandler }

procedure TMainHandler.HandleIndex(aRequest: TRequest; aResponse: TResponse; args: TStrings);
begin
  aResponse.Content := 'INDEX<BR>' + args.Text;
end;

procedure TMainHandler.HandleLogin(aRequest: TRequest; aResponse: TResponse; args: TStrings);
begin
  aResponse.Content := 'LOGIN<BR>' + args.Text;
end;

procedure TMainHandler.HandleUserInfo(aRequest: TRequest; aResponse: TResponse; args: TStrings);
begin
  aResponse.Content := 'USER INFO<BR>' + args.Text;
end;

procedure TMainHandler.HandleHelp(aRequest: TRequest; aResponse: TResponse; args: TStrings);
begin
  aResponse.Content := 'HELP<BR>' + args.Text;
end;

end.








