{ Copyright (C) 2011  Mattias Gaertner  mattias@freepascal.org

  Abstract:
    Basic classes and functions.
}
unit XDBUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AVL_Tree, CodeToolsStructs, FileProcs;

type
  PFPList = ^TFPList;

  { TXDBAVLTreeNodeMemManager }

  TXDBAVLTreeNodeMemManager = class(TAVLTreeNodeMemManager)
  public
    procedure DisposeNode(ANode: TAVLTreeNode); override;
    function NewNode: TAVLTreeNode; override;
  end;

  { TXDBAVLTree }

  TXDBAVLTree = class(TAVLTree)
  protected
    fNodeManager: TAVLTreeNodeMemManager;
  public
    constructor Create(OnCompareMethod: TListSortCompare);
    destructor Destroy; override;
  end;

  { TGenericAVLTreeEnumerator }

  generic TGenericAVLTreeEnumerator<TData> = class
  private
    FTree: TAVLTree;
    FCurrent: TAVLTreeNode;
    function GetCurrent: TData;
  public
    constructor Create(Tree: TAVLTree);
    function MoveNext: boolean;
    property Current: TData read GetCurrent;
  end;

  { TBaseStringMap }

  TBaseStringMap = class
  private
    type
      TBaseStringMapItem = record
        Name: string;
      end;
    PBaseStringMapItem = ^TBaseStringMapItem;
  private
    FCompareKeyItemFunc: TListSortCompare;
    FTree: TMTAVLTree;// tree of PGenericStringMapItem
    FCaseSensitive: boolean;
    function GetCompareItemsFunc: TListSortCompare;
    function FindNode(const s: string): TAVLTreeNode;
    procedure DisposeItem(NodeData: Pointer); virtual; abstract;
    function IsDataEqual(NodeData1, NodeData2: Pointer): boolean; virtual; abstract;
  public
    constructor Create(TheCaseSensitive: boolean);
    destructor Destroy; override;
    procedure Clear;
    function Contains(const s: string): boolean;
    function ContainsIdentifier(P: PChar): boolean;
    function FindNodeWithIdentifierAsPrefix(P: PChar): TAVLTreeNode;
    procedure GetNames(List: TStrings);
    procedure Remove(const Name: string);
    property CaseSensitive: boolean read FCaseSensitive;
    property Tree: TMTAVLTree read FTree; // tree of PGenericStringMapItem
    function Equals(OtherTree: TBaseStringMap): boolean; reintroduce;
    procedure WriteDebugReport;
    function CalcMemSize: PtrUint;
    property CompareItemsFunc: TListSortCompare read GetCompareItemsFunc;
    property CompareKeyItemFunc: TListSortCompare read FCompareKeyItemFunc;
    procedure SetCompareFuncs(
            const NewCompareItemsFunc, NewCompareKeyItemFunc: TListSortCompare);
  end;

  { TGenericStringMap }

  generic TGenericStringMap<TData> = class(TBaseStringMap)
  public
    type
      TGenericStringMapItem = record
        Name: string;
        Value: TData;
      end;
      PGenericStringMapItem = ^TGenericStringMapItem;
      TSpecStringMapEnumerator = specialize TGenericAVLTreeEnumerator<TData>;
  private
    function GetItems(const s: string): TData;
    procedure SetItems(const s: string; const AValue: TData);
    procedure DisposeItem(NodeData: Pointer); override;
    function IsDataEqual(NodeData1, NodeData2: Pointer): boolean; override;
  public
    function Get(const Name: string; out Value: TData): boolean;
    procedure Add(const Name: string; const Value: TData);
    property Items[const s: string]: TData read GetItems write SetItems; default;
    procedure Assign(Source: TGenericStringMap);
    function GetEnumerator: TSpecStringMapEnumerator;
  end;

  { TXDBStringTree }

  TXDBStringTree = class(TStringTree)
  protected
    fNodeManager: TAVLTreeNodeMemManager;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TXDBStringToStringTree }

  TXDBStringToStringTree = class(TStringToStringTree)
  protected
    fNodeManager: TAVLTreeNodeMemManager;
  public
    constructor Create(TheCaseSensitive: boolean);
    destructor Destroy; override;
  end;

  { TXDBStringToPointerTree }

  TXDBStringToPointerTree = class(TStringToPointerTree)
  protected
    fNodeManager: TAVLTreeNodeMemManager;
  public
    constructor Create(TheCaseSensitive: boolean);
    destructor Destroy; override;
  end;

  { TXDBFilenameToStringTree }

  TXDBFilenameToStringTree = class(TFilenameToStringTree)
  protected
    fNodeManager: TAVLTreeNodeMemManager;
  public
    constructor Create(TheCaseSensitive: boolean); // false = system default
    destructor Destroy; override;
  end;

  { TXDBFilenameToPointerTree }

  TXDBFilenameToPointerTree = class(TFilenameToPointerTree)
  protected
    fNodeManager: TAVLTreeNodeMemManager;
  public
    constructor Create(TheCaseSensitive: boolean); // false = system default
    destructor Destroy; override;
  end;

// string operations
function UTF8FindNearestCharStart(UTF8Str: PChar; Len: integer;
  BytePos: integer): integer;
function ExtractFirstURLPath(var Path: string): string;
function StringToList(const LongOpts: string): TStrings;

// xml strings
function GetXMLNameLength(Name: PChar): integer;
function GetXMLName(Name: PChar): string;
function CompareXMLNames(Name1, Name2: PChar): integer;
function CompareXMLNamesPtrs(Name1, Name2: Pointer): integer; inline;
function GetXMLAttriNameLength(Name: PChar): integer;
function GetXMLAttrName(Name: PChar): string;
function CompareXMLAttrNames(Name1, Name2: PChar): integer;
function CompareXMLAttrValue(Value1, Value2: PChar): integer;

// environment
function GetProgramSearchPath: string;
function FindDefaultExecutablePath(const Executable: string): string;

// date, time, age
function DateTimeToXDBStr(const ADateTime: TDateTime): string;
function FileAgeToXDBStr(aFileAge: longint): string;

// misc
function ComparePointer(Data1, Data2: Pointer): integer;

var
  IsXMLNameStartChar, IsXMLNameChar: array[char] of boolean;

implementation

function ComparePointer(Data1, Data2: Pointer): integer;
begin
  if Data1>Data2 then Result:=-1
  else if Data1<Data2 then Result:=1
  else Result:=0;
end;

{ Find the start of the UTF8 character which contains BytePos,
  Len is length in byte, BytePos starts at 0 }
function UTF8FindNearestCharStart(UTF8Str: PChar; Len: integer;
  BytePos: integer): integer;
begin
  Result:=0;
  if (UTF8Str<>nil) and (Len>0) and (BytePos>=0) then begin
    Result:=BytePos;
    if Result>Len then Result:=Len-1;
    if (Result>0) and (ord(UTF8Str[Result]) and %11000000=%10000000) then begin
      dec(Result);
      if (Result>0) and (ord(UTF8Str[Result]) and %11000000=%10000000) then begin
        dec(Result);
        if (Result>0) and (ord(UTF8Str[Result]) and %11000000=%10000000) then begin
          dec(Result);
          // should be four byte character
          if (ord(UTF8Str[Result]) and %11111000<>%11110000) then begin
            // broken UTF8 character
            inc(Result,3);
          end else begin
            // is four byte character
          end;
        end else if (ord(UTF8Str[Result]) and %11110000<>%11100000) then begin
          // broken UTF8 character, should be three byte
          inc(Result,2);
        end else
        begin
          // is three byte character
        end;
      end else if (ord(UTF8Str[Result]) and %11100000<>%11000000) then begin
        // broken UTF8 character, should be two byte
        inc(Result);
      end else
      begin
        // is two byte character
      end;
    end;
  end;
end;

function ExtractFirstURLPath(var Path: string): string;
var
  p: SizeInt;
begin
  p:=Pos('/',Path);
  if p<1 then p:=length(Path)+1;
  Result:=copy(Path,1,p-1);
  Path:=copy(Path,p+1,length(Path));
end;

function StringToList(const LongOpts: string): TStrings;
const
  SepChars = ' '#10#13#9;
var
  L : TStringList;
  Len,I,J : Integer;
begin
  l:=TStringList.Create;
  I:=1;
  Len:=Length(LongOpts);
  while I<=Len do begin
    while Isdelimiter(SepChars,LongOpts,I) do
      Inc(I);
    J:=I;
    while (J<=Len) and Not IsDelimiter(SepChars,LongOpts,J) do
      Inc(J);
    if (I<=J) then
      L.Add(Copy(LongOpts,I,(J-I)));
    I:=J+1;
  end;
  Result:=l;
end;

function GetXMLNameLength(Name: PChar): integer;
begin
  Result:=0;
  if (Name=nil) or (not IsXMLNameStartChar[Name^]) then exit;
  inc(Name);
  inc(Result);
  while IsXMLNameChar[Name^] do begin
    inc(Name);
    inc(Result);
  end;
end;

function GetXMLName(Name: PChar): string;
begin
  SetLength(Result,GetXMLNameLength(Name));
  if Result='' then exit;
  Move(Name^,Result[1],length(Result));
end;

function CompareXMLNames(Name1, Name2: PChar): integer;
begin
  if (Name1<>nil) then begin
    if (Name2<>nil) then begin
      while (Name1^=Name2^) do begin
        if (IsXMLNameChar[Name1^]) then begin
          inc(Name1);
          inc(Name2);
        end else begin
          Result:=0; // for example  'aaA;' 'aAa;'
          exit;
        end;
      end;
      if (IsXMLNameChar[Name1^]) then begin
        if (IsXMLNameChar[Name2^]) then begin
          if Name1^>Name2^ then
            Result:=-1 // for example  'aab' 'aaa'
          else
            Result:=1; // for example  'aaa' 'aab'
        end else begin
          Result:=-1; // for example  'aaa' 'aa;'
        end;
      end else begin
        if (IsXMLNameChar[Name2^]) then
          Result:=1 // for example  'aa;' 'aaa'
        else
          Result:=0; // for example  'aa;' 'aa,'
      end;
    end else begin
      Result:=-1; // for example  'aaa' nil
    end;
  end else begin
    if (Name2<>nil) then begin
      Result:=1; // for example  nil 'bbb'
    end else begin
      Result:=0; // for example  nil nil
    end;
  end;
end;

function CompareXMLNamesPtrs(Name1, Name2: Pointer): integer;
begin
  Result:=CompareXMLNames(PChar(Name1),PChar(Name2));
end;

function GetXMLAttriNameLength(Name: PChar): integer;
begin
  Result:=0;
  if (Name=nil) or (not IsXMLNameStartChar[Name^]) then exit;
  inc(Name);
  inc(Result);
  while IsXMLNameChar[Name^] do begin
    inc(Name);
    inc(Result);
  end;
  if Name^<>':' then exit;
  inc(Result);
  inc(Name);
  while IsXMLNameChar[Name^] do begin
    inc(Name);
    inc(Result);
  end;
end;

function GetXMLAttrName(Name: PChar): string;
begin
  SetLength(Result,GetXMLAttriNameLength(Name));
  if Result='' then exit;
  Move(Name^,Result[1],length(Result));
end;

function CompareXMLAttrNames(Name1, Name2: PChar): integer;
var
  ColonFound: Boolean;
begin
  if (Name1<>nil) then begin
    if (Name2<>nil) then begin
      ColonFound:=false;
      while (Name1^=Name2^) do begin
        if (IsXMLNameChar[Name1^]) then begin
          inc(Name1);
          inc(Name2);
        end else if (Name1^=':') and not ColonFound then begin
          ColonFound:=true;
          inc(Name1);
          inc(Name2);
        end else begin
          Result:=0; // for example  'aaA;' 'aAa;'
          exit;
        end;
      end;
      if (IsXMLNameChar[Name1^]) then begin
        if (IsXMLNameChar[Name2^]) then begin
          if Name1^>Name2^ then
            Result:=-1 // for example  'aab' 'aaa'
          else
            Result:=1; // for example  'aaa' 'aab'
        end else begin
          Result:=-1; // for example  'aaa' 'aa;'
        end;
      end else begin
        if (IsXMLNameChar[Name2^]) then
          Result:=1 // for example  'aa;' 'aaa'
        else
          Result:=0; // for example  'aa;' 'aa,'
      end;
    end else begin
      Result:=-1; // for example  'aaa' nil
    end;
  end else begin
    if (Name2<>nil) then begin
      Result:=1; // for example  nil 'bbb'
    end else begin
      Result:=0; // for example  nil nil
    end;
  end;
end;

function CompareXMLAttrValue(Value1, Value2: PChar): integer;
begin
  if (Value1<>nil) then begin
    if (Value2<>nil) then begin
      while (Value1^=Value2^) do begin
        if (Value1^<>#0) then begin
          inc(Value1);
          inc(Value2);
        end else begin
          Result:=0; // for example  'aaA;' 'aAa;'
          exit;
        end;
      end;
      if Value1^>Value2^ then
        Result:=-1 // for example  'aab' 'aaa'
      else
        Result:=1; // for example  'aaa' 'aab'
    end else begin
      Result:=-1; // for example  'aaa' nil
    end;
  end else begin
    if (Value2<>nil) then begin
      Result:=1; // for example  nil 'bbb'
    end else begin
      Result:=0; // for example  nil nil
    end;
  end;
end;

function GetProgramSearchPath: string;
begin
  Result := GetEnvironmentVariableUTF8('PATH');
end;

function FindDefaultExecutablePath(const Executable: string): string;
begin
  if FilenameIsAbsolute(Executable) then
    Result:=Executable
  else
    Result:=SearchFileInPath(Executable,'',GetProgramSearchPath,':',
                             ctsfcDefault);
end;

function DateTimeToXDBStr(const ADateTime: TDateTime): string;
var
  Year, Month, Day: word;
  Hour, Minute, Second, MilliSecond: word;
begin
  Year:=1900;
  Month:=1;
  Day:=1;
  Hour:=0;
  Minute:=0;
  Second:=0;
  MilliSecond:=0;
  DecodeDate(ADateTime,Year,Month,Day);
  DecodeTime(ADateTime,Hour,Minute,Second,MilliSecond);
  Result:=IntToStr(Year)+'-'+IntToStr(Month)+'-'+IntToStr(Day)
          +' '+IntToStr(Hour)+':'+IntToStr(Minute)+':'+IntToStr(Second);
end;

function FileAgeToXDBStr(aFileAge: longint): string;
begin
  Result:=DateTimeToXDBStr(FileDateToDateTime(aFileAge));
end;

{ TXDBFilenameToPointerTree }

constructor TXDBFilenameToPointerTree.Create(TheCaseSensitive: boolean);
begin
  inherited Create(TheCaseSensitive);
  fNodeManager:=TXDBAVLTreeNodeMemManager.Create;
  Tree.SetNodeManager(fNodeManager);
end;

destructor TXDBFilenameToPointerTree.Destroy;
begin
  inherited Destroy;
  FreeAndNil(fNodeManager);
end;

{ TXDBStringToPointerTree }

constructor TXDBStringToPointerTree.Create(TheCaseSensitive: boolean);
begin
  inherited Create(TheCaseSensitive);
  fNodeManager:=TXDBAVLTreeNodeMemManager.Create;
  Tree.SetNodeManager(fNodeManager);
end;

destructor TXDBStringToPointerTree.Destroy;
begin
  inherited Destroy;
  FreeAndNil(fNodeManager);
end;

{ TXDBFilenameToStringTree }

constructor TXDBFilenameToStringTree.Create(TheCaseSensitive: boolean);
begin
  inherited Create(TheCaseSensitive);
  fNodeManager:=TXDBAVLTreeNodeMemManager.Create;
  Tree.SetNodeManager(fNodeManager);
end;

destructor TXDBFilenameToStringTree.Destroy;
begin
  inherited Destroy;
  FreeAndNil(fNodeManager);
end;

{ TXDBStringToStringTree }

constructor TXDBStringToStringTree.Create(TheCaseSensitive: boolean);
begin
  inherited Create(TheCaseSensitive);
  fNodeManager:=TXDBAVLTreeNodeMemManager.Create;
  Tree.SetNodeManager(fNodeManager);
end;

destructor TXDBStringToStringTree.Destroy;
begin
  inherited Destroy;
  FreeAndNil(fNodeManager);
end;

{ TXDBStringTree }

constructor TXDBStringTree.Create;
begin
  inherited Create;
  fNodeManager:=TXDBAVLTreeNodeMemManager.Create;
  Tree.SetNodeManager(fNodeManager);
end;

destructor TXDBStringTree.Destroy;
begin
  inherited Destroy;
  FreeAndNil(fNodeManager);
end;

{ TXDBAVLTreeNodeMemManager }

procedure TXDBAVLTreeNodeMemManager.DisposeNode(ANode: TAVLTreeNode);
begin
  ANode.Free;
end;

function TXDBAVLTreeNodeMemManager.NewNode: TAVLTreeNode;
begin
  Result:=TAVLTreeNode.Create;
end;

{ TXDBAVLTree }

constructor TXDBAVLTree.Create(OnCompareMethod: TListSortCompare);
begin
  inherited Create(OnCompareMethod);
  fNodeManager:=TXDBAVLTreeNodeMemManager.Create;
  SetNodeManager(fNodeManager);
end;

destructor TXDBAVLTree.Destroy;
begin
  inherited Destroy;
  FreeAndNil(fNodeManager);
end;

procedure Init;
var
  c: Char;
begin
  for c:=low(char) to high(char) do begin
    IsXMLNameStartChar[c]:=c in ['a'..'z','A'..'Z','_',#192..#255];
    IsXMLNameChar[c]:=c in ['a'..'z','A'..'Z','_','0'..'9',#128..#255];
  end;
end;

{ TGenericAVLTreeEnumerator }

function TGenericAVLTreeEnumerator.GetCurrent: TData;
begin
  Result:=TData(FCurrent.Data);
end;

constructor TGenericAVLTreeEnumerator.Create(Tree: TAVLTree);
begin
  FTree:=Tree;
end;

function TGenericAVLTreeEnumerator.MoveNext: boolean;
begin
  if FCurrent=nil then
    FCurrent:=FTree.FindLowest
  else
    FCurrent:=FTree.FindSuccessor(FCurrent);
  Result:=FCurrent<>nil;
end;

{ TBaseStringMap }

function TBaseStringMap.GetCompareItemsFunc: TListSortCompare;
begin
  Result:=Tree.OnCompare;
end;

function TBaseStringMap.FindNode(const s: string): TAVLTreeNode;
begin
  Result:=FTree.FindKey(Pointer(s),FCompareKeyItemFunc)
end;

constructor TBaseStringMap.Create(TheCaseSensitive: boolean);
begin
  FCaseSensitive:=TheCaseSensitive;
  if CaseSensitive then begin
    FCompareKeyItemFunc:=@CompareStringAndStringToStringTreeItem;
    FTree:=TMTAVLTree.Create(@CompareStringToStringItems);
  end else begin
    FCompareKeyItemFunc:=@CompareStringAndStringToStringTreeItemI;
    FTree:=TMTAVLTree.Create(@CompareStringToStringItemsI);
  end;
end;

destructor TBaseStringMap.Destroy;
begin
  Clear;
  FTree.Free;
  FTree:=nil;
  inherited Destroy;
end;

procedure TBaseStringMap.Clear;
var
  Node: TAVLTreeNode;
begin
  Node:=FTree.FindLowest;
  while Node<>nil do begin
    DisposeItem(Node.Data);
    Node:=FTree.FindSuccessor(Node);
  end;
  FTree.Clear;
end;

function TBaseStringMap.Contains(const s: string): boolean;
begin
  Result:=FindNode(s)<>nil;
end;

function TBaseStringMap.ContainsIdentifier(P: PChar): boolean;
begin
  if CaseSensitive then
    Result:=FTree.FindKey(p,@CompareIdentifierAndStringToStringTreeItem)<>nil
  else
    Result:=FTree.FindKey(p,@CompareIdentifierAndStringToStringTreeItemI)<>nil;
end;

function TBaseStringMap.FindNodeWithIdentifierAsPrefix(P: PChar): TAVLTreeNode;
begin
  if CaseSensitive then
    Result:=FTree.FindKey(p,@CompareIdentifierPrefixAndStringToStringTreeItem)
  else
    Result:=FTree.FindKey(p,@CompareIdentifierPrefixAndStringToStringTreeItemI);
end;

procedure TBaseStringMap.GetNames(List: TStrings);
var
  Node: TAVLTreeNode;
  Item: PBaseStringMapItem;
begin
  Node:=Tree.FindLowest;
  while Node<>nil do begin
    Item:=PBaseStringMapItem(Node.Data);
    List.Add(Item^.Name);
    Node:=Tree.FindSuccessor(Node);
  end;
end;

procedure TBaseStringMap.Remove(const Name: string);
var
  Node: TAVLTreeNode;
  Item: Pointer;
begin
  Node:=FindNode(Name);
  if Node<>nil then begin
    Item:=Node.Data;
    FTree.Delete(Node);
    DisposeItem(Item);
  end;
end;

function TBaseStringMap.Equals(OtherTree: TBaseStringMap): boolean;
var
  Node: TAVLTreeNode;
  OtherNode: TAVLTreeNode;
  OtherItem: PBaseStringMapItem;
  Item: PBaseStringMapItem;
begin
  Result:=false;
  if OtherTree=nil then exit;
  if OtherTree.ClassType<>ClassType then exit;
  if Tree.Count<>OtherTree.Tree.Count then exit;
  Node:=Tree.FindLowest;
  OtherNode:=OtherTree.Tree.FindLowest;
  while Node<>nil do begin
    if OtherNode=nil then exit;
    Item:=PBaseStringMapItem(Node.Data);
    OtherItem:=PBaseStringMapItem(OtherNode.Data);
    if (Item^.Name<>OtherItem^.Name)
    or (not IsDataEqual(Item,OtherItem)) then exit;
    OtherNode:=OtherTree.Tree.FindSuccessor(OtherNode);
    Node:=Tree.FindSuccessor(Node);
  end;
  if OtherNode<>nil then exit;
  Result:=true;
end;

procedure TBaseStringMap.WriteDebugReport;
var
  Node: TAVLTreeNode;
  Item: PStringToStringTreeItem;
begin
  DebugLn(['TGenericStringMap.WriteDebugReport ',Tree.Count]);
  Node:=Tree.FindLowest;
  while Node<>nil do begin
    Item:=PStringToStringTreeItem(Node.Data);
    DebugLn([Item^.Name]);
    Node:=Tree.FindSuccessor(Node);
  end;
end;

function TBaseStringMap.CalcMemSize: PtrUint;
var
  Node: TAVLTreeNode;
  Item: PBaseStringMapItem;
begin
  Result:=PtrUInt(InstanceSize)
    +PtrUInt(FTree.InstanceSize)
    +PtrUint(FTree.Count)*SizeOf(TAVLTreeNode);
  Node:=FTree.FindLowest;
  while Node<>nil do begin
    Item:=PBaseStringMapItem(Node.Data);
    inc(Result,MemSizeString(Item^.Name)
       +SizeOf(TBaseStringMapItem));
    Node:=FTree.FindSuccessor(Node);
  end;
end;

procedure TBaseStringMap.SetCompareFuncs(const NewCompareItemsFunc,
  NewCompareKeyItemFunc: TListSortCompare);
begin
  FCompareKeyItemFunc:=NewCompareKeyItemFunc;
  Tree.OnCompare:=NewCompareItemsFunc;
end;

{ TGenericStringMap }

function TGenericStringMap.GetItems(const s: string): TData;
var
  Node: TAVLTreeNode;
begin
  Node:=FindNode(s);
  if Node<>nil then
    Result:=PGenericStringMapItem(Node.Data)^.Value
  else
    Result:=''
end;

procedure TGenericStringMap.SetItems(const s: string; const AValue: TData);
var
  Node: TAVLTreeNode;
  NewItem: PStringToStringTreeItem;
begin
  Node:=FindNode(s);
  if Node<>nil then begin
    PGenericStringMapItem(Node.Data)^.Value:=AValue;
  end else begin
    New(NewItem);
    NewItem^.Name:=s;
    NewItem^.Value:=AValue;
    FTree.Add(NewItem);
  end;
end;

procedure TGenericStringMap.DisposeItem(NodeData: Pointer);
var
  Item: PGenericStringMapItem absolute NodeData;
begin
  DisposeItem(Item);
end;

function TGenericStringMap.IsDataEqual(NodeData1, NodeData2: Pointer): boolean;
var
  Item1: PGenericStringMapItem absolute NodeData1;
  Item2: PGenericStringMapItem absolute NodeData2;
begin
  Result:=Item1^.Value=Item2^.Value;
end;

function TGenericStringMap.Get(const Name: string; out Value: TData): boolean;
var
  Node: TAVLTreeNode;
begin
  Node:=FindNode(Name);
  if Node<>nil then begin
    Value:=PGenericStringMapItem(Node.Data)^.Value;
    Result:=true;
  end else begin
    Result:=false;
  end;
end;

procedure TGenericStringMap.Add(const Name: string; const Value: TData);
begin
  Items[Name]:=Value;
end;

procedure TGenericStringMap.Assign(Source: TGenericStringMap);
var
  Node: TAVLTreeNode;
  Item: PGenericStringMapItem;
begin
  Clear;
  Node:=Source.Tree.FindLowest;
  while Node<>nil do begin
    Item:=PGenericStringMapItem(Node.Data);
    Items[Item^.Name]:=Item^.Value;
    Node:=Source.Tree.FindSuccessor(Node);
  end;
end;

function TGenericStringMap.GetEnumerator: TSpecStringMapEnumerator;
begin
  Result:=TSpecStringMapEnumerator.Create(Tree);
end;

initialization
  Init;

end.

