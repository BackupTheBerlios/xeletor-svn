{
 Test with:
     ./runtests --format=plain --suite=TTestXDB
     ./runtests --format=plain --suite=TestReadXDB
     ./runtests --format=plain --suite=TestXDBEnumerators
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
  private
  published
    procedure TestReadXDB;
    procedure TestXDBEnumerators;
  end;

function GetXDBNodeName(Node: TXDBNode): string;

implementation

function GetXDBNodeName(Node: TXDBNode): string;
begin
  if Node=nil then
    Result:='nil'
  else
    Result:=Node.GetName;
end;

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

procedure TTestXDB.TestXDBEnumerators;
var
  Root: TXDBRootNode;
  Node: TXDBNode;
  LastNode: TXDBNode;
  ExpectedNode: TXDBNode;
begin
  Root:=nil;
  try
    ReadXDBTree(Root,SetDirSeparators('xml/enumeration.xml'));
    LastNode:=nil;
    for Node in Root.GetEnumeratorAllChildren do begin
      if Node.GetPrev<>LastNode then begin
        ExpectedNode:=Node.GetPrev;
        if ExpectedNode=Root then ExpectedNode:=nil;
        AssertEquals('GetEnumeratorAllChildren match'
          +' Expected='+GetXDBNodeName(ExpectedNode)+' Last='+GetXDBNodeName(LastNode),
          true,ExpectedNode=LastNode);
      end;
      LastNode:=Node;
    end;
  finally
    Root.Free;
  end;
end;

initialization
  AddToXDBTestSuite(TTestXDB);
end.

