program upload;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  http,
  http.client,
  http.messages { you can add units after this };

const
  uploadUrl = 'localhost:9090/test/simpleupload.php';

type

  { THTTPDemo1 }

  THTTPDemo1 = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { THTTPDemo1 }

  procedure THTTPDemo1.DoRun;
  var
    client: TDefaultHTTPClient;
    fdf: TFormDataFile;
    FFormDataRequest: TFormDataRequest;
    FResponse: THttpResponse;
  begin
    client := TDefaultHTTPClient.Create;
    client.Debug := True;
    FResponse := THttpResponse.Create;
    FFormDataRequest := TFormDataRequest.Create;
    FFormDataRequest.URL.assign(uploadUrl);
    FFormDataRequest.setHeader(ContentType, 'mulipart/form-data');
    fdf := TFormDataFile.Create;
    fdf.Name := 'custom_upload_fieldname';
    fdf.Value := 'afile.json';
    fdf.Stream := TFileStream.Create('..\..\data\petstore.swagger.json', fmOpenRead or fmShareDenyWrite);
    FFormDataRequest.formData.Add(fdf);
    client.request(FFormDataRequest, FResponse);
    if FResponse.StatusCode = 200 then
    begin
      Writeln('Upload Ok');
    end;
    FreeAndNil(client);
    Terminate;
  end;

  constructor THTTPDemo1.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor THTTPDemo1.Destroy;
  begin
    inherited Destroy;
  end;

  procedure THTTPDemo1.WriteHelp;
  begin
    { add your help code here }
    writeln('Usage: ', ExeName, ' -h');
  end;

var
  Application: THTTPDemo1;

{$R *.res}

begin
  Application := THTTPDemo1.Create(nil);
  Application.Title := 'Demo1';
  Application.Run;
  Application.Free;
end.
