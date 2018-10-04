unit http.encoders;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, http.messages;

type

  { TEncoder }

  TEncoder = class
  protected
    function cloneRequest(aRequest: THttpRequest; newClass: THttpRequestClass): THttpRequest;
  public
    function encode(aRequest: THttpRequest; out encoded: THttpRequest): boolean; virtual; abstract;
  end;

  { TEncoderMultiPartFormData }

  TEncoderMultiPartFormData = class(TEncoder)
    function encode(aRequest: THttpRequest; out encoded: THttpRequest): boolean; override;
  end;

  { TEncoderWWWFormUrlEncoded }

  TEncoderWWWFormUrlEncoded = class(TEncoder)
    function encode(aRequest: THttpRequest; out encoded: THttpRequest): boolean; override;
  end;

implementation

uses
  httpprotocol;

{ TEncoder }

function TEncoder.cloneRequest(aRequest: THttpRequest; newClass: THttpRequestClass): THttpRequest;
var
  h: THTTPHeader;
begin
  result := newClass.Create;
  for h in aRequest.getHeaders do
  begin
    result.addHeader(h.Name, h.Value);
  end;
  result.Method := aRequest.Method;
  result.URL.Scheme := aRequest.URL.Scheme;
  result.URL.path := aRequest.URL.path;
  result.URL.queryString := aRequest.URL.queryString;
  result.URL.fragment := aRequest.URL.fragment;
  result.URL.host := aRequest.URL.host;
  result.URL.port := aRequest.URL.port;
  result.URL.userInfo := aRequest.URL.userInfo;
end;


{ TEncoderWWWFormUrlEncoded }

function TEncoderWWWFormUrlEncoded.encode(aRequest: THttpRequest; out encoded: THttpRequest): boolean;
var
  buffer: RawByteString;
  fd: TFormData;
begin
  result := False;
  if (aRequest is TFormDataRequest) and (CompareText(aRequest.getHeader(ContentType), 'application/x-www-form-urlencoded') = 0) then
  begin
    encoded := CloneRequest(aRequest, TFormDataRequest);
    encoded.Method := 'POST';
    encoded.setHeader(ContentType, 'application/x-www-form-urlencoded;charset=UTF-8');
    buffer := '';
    for fd in TFormDataRequest(aRequest).formData do
    begin
      buffer += Format('&%s=%s', [HTTPEncode(fd.Name), HTTPEncode(fd.Value)]);
    end;
    buffer[1] := ' ';
    buffer := trim(buffer);
    encoded.Body.WriteBuffer(buffer[1], length(buffer));
    result := True;
  end;
end;

{ TEncoderMultiPartFormData }

function TEncoderMultiPartFormData.encode(aRequest: THttpRequest; out encoded: THttpRequest): boolean;
var
  buffer: string;
  bundary: string;
  fd: TFormData;
  uploadStream: TStream;
begin
  result := False;
  buffer := '';
  if (aRequest is TFormDataRequest) and (CompareText(aRequest.getHeader(ContentType), 'mulipart/form-data') = 0) then
  begin
    bundary := (aRequest as TFormDataRequest).CurrentBundary + '_file_upload';

    encoded := CloneRequest(aRequest, THttpRequest);
    encoded.Method := 'POST';
    encoded.setHeader(ContentType, Format('multipart/form-data; boundary=%s', [bundary]));
    for fd in TFormDataRequest(aRequest).formData do
    begin
      buffer += '--' + bundary + CRLF;
      if fd is TFormDataFile then
      begin
        uploadStream := (fd as TFormDataFile).Stream;
        buffer += Format('Content-Disposition: form-data; name="%s"; filename="%s"', [fd.Name, fd.Value]) + CRLF;
        buffer += 'Content-Type: application/octet-string' + CRLF + CRLF;
        encoded.Body.WriteBuffer(buffer[1], Length(Buffer));
        uploadStream.Position := 0;
        encoded.Body.CopyFrom(uploadStream, uploadStream.Size);
      end
      else
      begin
        buffer += Format('Content-Disposition: form-data; name="%s"', [fd.Name]) + CRLF;
        buffer += fd.Value + CRLF;
        encoded.Body.WriteBuffer(buffer[1], Length(Buffer));
      end;
    end;
    buffer := CRLF + '--' + bundary + '--' + CRLF;
    encoded.Body.WriteBuffer(buffer[1], Length(Buffer));
    encoded.Body.Position := 0;
    result := True;
  end;
end;


end.

