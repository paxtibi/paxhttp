program custom;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  http.client,
  http.messages,
  CustApp { you can add units after this };

const
  url: string = 'http://localhost:9090/test/simpleform.php';


type

  { TCustomMethod }

  TCustomMethod = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { TCustomMethod }

  procedure TCustomMethod.DoRun;
  var
    client: TDefaultHTTPClient;
    FRequest: THttpRequest;
    FResponse: THttpResponse;
    outputString: string;
  begin
    client := TDefaultHTTPClient.Create;
    FResponse := THttpResponse.Create;
    FRequest := THttpRequest.Create(url);
    FRequest.setHeader(ContentType, 'application/x-www-form-urlencoded');
    FRequest.Method := 'RETRIEVE';
    outputString := '{ id=10000 }';
    FRequest.Body.WriteBuffer(outputString[1], Length(outputString));
    client.request(FRequest, FResponse);
    Writeln(FResponse.ReasonPhase);
    if FResponse.StatusCode = 200 then
    begin
      FResponse.Body.Position := 0;
      SetLength(outputString, FResponse.Body.Size);
      FResponse.Body.ReadBuffer(outputString[1], Length(outputString));
      Writeln(outputString);
    end;

    FRequest.Method := 'CREATE';
    outputString := '{ id=10000, name="description", price=10.00 }';
    FRequest.Body.WriteBuffer(outputString[1], Length(outputString));
    client.request(FRequest, FResponse);
    Writeln(FResponse.ReasonPhase);
    if FResponse.StatusCode = 200 then
    begin
      FResponse.Body.Position := 0;
      SetLength(outputString, FResponse.Body.Size);
      FResponse.Body.ReadBuffer(outputString[1], Length(outputString));
      Writeln(outputString);
    end;

    FRequest.Method := 'UPDATE';
    outputString := '{ id=10000, price=15.00 }';
    FRequest.Body.WriteBuffer(outputString[1], Length(outputString));
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

  constructor TCustomMethod.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TCustomMethod.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TCustomMethod.WriteHelp;
  begin
    { add your help code here }
    writeln('Usage: ', ExeName, ' -h');
  end;

var
  Application: TCustomMethod;

{$R *.res}

begin
  Application := TCustomMethod.Create(nil);
  Application.Title := 'CustomMethodDemo';
  Application.Run;
  Application.Free;
end.


