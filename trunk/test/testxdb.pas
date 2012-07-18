{
 Test with:
     ./runtests --format=plain --suite=TTestXDB
     ./runtests --format=plain --suite=TestReadXDB
}
unit TestXDB;

{$mode objfpc}{$H+}

{$DEFINE VerboseTestXDB}

interface

uses
  Classes, SysUtils, testglobals, XDBFiles, fpcunit;

type

  { TTestXDB }

  TTestXDB = class(TTestCase)
  published
    procedure TestReadXDB;
  end;

implementation

{ TTestXDB }

procedure TTestXDB.TestReadXDB;
var
  Root: TXDBRootNode;
begin
  Root:=nil;
  try
    ReadXDBTree(Root,SetDirSeparators('xml/MKPosteriorAnalyticsLat.xml'));
  finally
    Root.Free;
  end;
end;

initialization
  AddToXDBTestSuite(TTestXDB);
end.

