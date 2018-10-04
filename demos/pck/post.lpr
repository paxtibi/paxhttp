program post;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  paxhttp.client,
  paxhttp.messages { you can add units after this };

const
  url: string = 'http://localhost:9090/test/simpleform.php';

type

  { TPOSTDemo }

  TPOSTDemo = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { TPOSTDemo }

  procedure TPOSTDemo.DoRun;
  var
    client: TDefaultHTTPClient;
    fd: TFormData;
    FFormDataRequest: TFormDataRequest;
    FResponse: THttpResponse;
    outputString: string;
  begin
    client := TDefaultHTTPClient.Create;
    FResponse := THttpResponse.Create;
    FFormDataRequest := TFormDataRequest.Create(url);
    FFormDataRequest.setHeader(ContentType, 'application/x-www-form-urlencoded');
    fd := TFormData.Create;
    fd.Name := 'text input';
    fd.Value := 'a input value';
    FFormDataRequest.formData.Add(fd);
    fd := TFormData.Create;
    fd.Name := 'checkinput';
    fd.Value := 'on';
    FFormDataRequest.formData.Add(fd);
    client.request(FFormDataRequest, FResponse);
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



  constructor TPOSTDemo.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TPOSTDemo.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TPOSTDemo.WriteHelp;
  begin
    { add your help code here }
    writeln('Usage: ', ExeName, ' -h');
  end;

var
  Application: TPOSTDemo;

{$R *.res}

begin
  Application := TPOSTDemo.Create(nil);
  Application.Title := 'Post Demo';
  Application.Run;
  Application.Free;
end.
