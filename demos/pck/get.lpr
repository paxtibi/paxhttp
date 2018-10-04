program get;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  paxhttp.client,
  paxhttp.messages,
  CustApp { you can add units after this };

const
  url: string = 'http://www.google.com/';
type

  { TGetDemo }

  TGetDemo = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { TGetDemo }

  procedure TGetDemo.DoRun;
  var
    client: TDefaultHTTPClient;
    FRequest: THttpRequest;
    FResponse: THttpResponse;
    outputString: string;
  begin
    client := TDefaultHTTPClient.Create;
    FResponse := THttpResponse.Create;
    FRequest := THttpRequest.Create(url);
    FRequest.Method := 'GET';
    client.request(FRequest, FResponse);
    Writeln(FResponse.ReasonPhase);
    if FResponse.StatusCode = 200 then
    begin
      FResponse.Body.Position := 0;
      SetLength(outputString, FResponse.Body.Size);
      FResponse.Body.ReadBuffer(outputString[1], Length(outputString));
      Writeln(outputString);
    end;
    FreeAndNil(client);
    Terminate;
  end;

  constructor TGetDemo.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TGetDemo.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TGetDemo.WriteHelp;
  begin
    { add your help code here }
    writeln('Usage: ', ExeName, ' -h');
  end;

var
  Application: TGetDemo;

{$R *.res}

begin
  Application := TGetDemo.Create(nil);
  Application.Title := 'GET DEMO';
  Application.Run;
  Application.Free;
end.
