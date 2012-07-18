unit testglobals;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

var
  XDBTestSuite: TTestSuite = nil;

procedure AddToXDBTestSuite(ATestClass: TClass);

implementation

procedure AddToXDBTestSuite(ATestClass: TClass);
begin
  XDBTestSuite.AddTestSuiteFromClass(ATestClass);
end;

initialization
  XDBTestSuite := TTestSuite.Create('Xeletor DB tests');
  GetTestRegistry.AddTest(XDBTestSuite);
end.

