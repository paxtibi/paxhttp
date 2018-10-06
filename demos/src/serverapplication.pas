unit ServerApplication;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, custhttpapp, custweb, HTTPDefs, httproute;

type

  { THTTPServerApplication }

  THTTPServerApplication = class(TCustomHTTPApplication)
  protected
    function InitializeWebHandler: TWebHandler; override;
  end;

  { THTTPServerApplicationHandler }

  THTTPServerApplicationHandler = class(TFPHTTPServerHandler)
  private
  protected
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  public
  end;

implementation

{ THTTPServerApplicationHandler }

procedure THTTPServerApplicationHandler.HandleRequest(ARequest: TRequest; AResponse: TResponse);
begin
  try
    inherited HandleRequest(ARequest, AResponse);
  except
    On E: Exception do
      ShowRequestException(AResponse, E);
  end;
end;


{ THTTPServerApplication }

function THTTPServerApplication.InitializeWebHandler: TWebHandler;
begin
  Result := THTTPServerApplicationHandler.Create(Self);
end;


end.
