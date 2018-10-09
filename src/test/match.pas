unit Match;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, paxhttp.Server;

type
  { TMatchTest }

  TMatchTest = class(TTestCase)
  protected
    FRouteList: TRouteContainerList;
    function createRoute(urlPattern: string): TRouteContainer;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRoot;
    procedure TestMathSimpleUrl;
    procedure TestMathSimpleParameter;
    procedure TestMathParameterWithPattern;
  end;

implementation

procedure TMatchTest.TestMathSimpleUrl;
const
  testUrl: string = '/simple/static/url';
var
  rc: TRouteContainer;
begin
  for rc in FRouteList do
  begin
    AssertEquals(rc.urlMatchPattern(testUrl), rc.urlPattern.EndsWith('/url'));
  end;
end;

procedure TMatchTest.TestMathSimpleParameter;
const
  testUrl = '/simple/static/url/with/parameter1';
var
  rc: TRouteContainer;
begin
  for rc in FRouteList do
  begin
    AssertEquals(rc.urlMatchPattern(testUrl), rc.urlPattern.EndsWith('{param2}'));
  end;
end;

procedure TMatchTest.TestMathParameterWithPattern;
const
  testUrl = '/user/ac123456';
var
  rc: TRouteContainer;
begin
  for rc in FRouteList do
  begin
    AssertEquals(rc.urlMatchPattern(testUrl), rc.urlPattern.StartsWith('/user/'));
  end;
end;

function TMatchTest.createRoute(urlPattern: string): TRouteContainer;
begin
  result := TRouteContainer.Create;
  Result.urlPattern := urlPattern;
end;

procedure TMatchTest.SetUp;
begin
  FRouteList := TRouteContainerList.Create(True);
  FRouteList.Add(createRoute('/'));
  FRouteList.Add(createRoute('/simple/static/url'));
  FRouteList.Add(createRoute('/simple/static/url/with/{param2}'));
  FRouteList.Add(createRoute('/user/{user-id:"[\w\d]*"}'));
end;

procedure TMatchTest.TearDown;
begin
  FRouteList.Free;
end;

procedure TMatchTest.TestRoot;
var
  rc: TRouteContainer;
begin
  for rc in FRouteList do
  begin
    AssertEquals(rc.urlMatchPattern('/'), rc.urlPattern = '/');
  end;
end;

initialization

  RegisterTest(TMatchTest);
end.

