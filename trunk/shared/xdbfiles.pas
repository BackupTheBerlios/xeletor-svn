{ Copyright (C) 2011  Mattias Gaertner  mattias@freepascal.org

  Abstract:
    Classes for storing xml files, directories, documents and nodes
}
unit XDBFiles;

{$mode objfpc}{$H+}

{ $DEFINE XDBNodesConsistencyCheck}

interface

uses
  Classes, SysUtils, AVL_Tree, FileProcs, CodeToolsStructs, fgl,
  xdbutils, laz2_DOM, laz2_XMLRead;

const
  XDBTypeArraySize = High(integer) div 2;
type
  TXDBListFlag = (
    xlfAddFiles,
    xlfAddDirectories
    );
  TXDBListFlags = set of TXDBListFlag;
const
  xlfAll = [low(TXDBListFlag)..high(TXDBListFlag)];

type
  TXDBNode = class;
  TXDBTreeNode = class;

  { TXDBNodeEnumerator - enumerates direct children }

  TXDBNodeEnumerator = class
  private
    FNode: TXDBTreeNode;
    FCurrent: TXDBNode;
    FPosition: integer;
  public
    constructor Create(Node: TXDBNode);
    function MoveNext: boolean;
    property Current: TXDBNode read FCurrent;
  end;

  { TXDBNodeAllChildEnumerator }

  TXDBNodeAllChildEnumerator = class
  private
    FNode: TXDBTreeNode;
    FCurrent: TXDBNode;
    FEnd: TXDBNode;
  public
    constructor Create(Node: TXDBNode);
    function MoveNext: boolean;
    property Current: TXDBNode read FCurrent;
    function GetEnumerator: TXDBNodeAllChildEnumerator; // including grand children
  end;

  TXDBNodeIterateEvent = procedure(Node: TXDBNode;
                                   var Abort, SkipChildren: boolean) of object;

  { TXDBNode - optimized for memory size }

  TXDBNode = class
  protected
    function GetChild({%H-}Index: integer): TXDBNode; virtual;
  public
    Parent: TXDBTreeNode;
    IndexInParent: integer; // 0 = first

    // name
    function GetName: string; virtual;
    function CompareName(aName: PChar): integer; virtual;
    function GetSubDPath: string; // name[IndexInParent]
    function GetSubXPath: string; // [IndexInParent]
    function GetDPath(WithRoot: boolean = false): string; // name[IndexInParent]/...
    function GetXPath(WithRoot: boolean = false): string; // [IndexInParent]/...
    function GetNodeDPath(WithRoot: boolean = false): string; // doc()/name[IndexInParent]/...
    function GetNodeXPath(WithRoot: boolean = false): string; // doc()/[IndexInParent]/...

    // children
    function GetChildCount: integer; virtual;
    property Items[Index: integer]: TXDBNode read GetChild; default;
    function GetFirstChild: TXDBNode;
    function GetLastChild: TXDBNode;
    function IndexOfName(Name: PChar): integer;
    function FindChildWithName(Name: PChar;
                               ExceptionOnNotFound: boolean = false): TXDBNode;
    function Insert({%H-}Index: integer; {%H-}Child: TXDBNode): TXDBNode; virtual;
    function Add(Child: TXDBNode): TXDBNode;
    function Remove({%H-}Child: TXDBNode): TXDBNode; virtual;

    // attributes
    function IndexOfAttribute(const {%H-}AttributeName: string): integer;
    function IndexOfAttributeP(const {%H-}AttributeName: PChar): integer; virtual;
    function GetAttribute(const {%H-}AttributeName: string): string;
    function GetAttributeP(const {%H-}AttributeName: PChar): string; virtual;
    procedure SetAttribute(const {%H-}AttributeName, {%H-}aValue: string);
    procedure SetAttributeP(const {%H-}AttributeName, {%H-}aValue: PChar); virtual;

    // text
    function GetText: string;
    function GetXML(Level: integer = 0; WithTags: boolean = true): string;
    procedure WriteToStream(s: TStream; Level: integer = 0; WithTags: boolean = true);

    // neighborhood
    function GetLevel: integer; // # of parents, 0 = no parent
    function GetEnumerator: TXDBNodeEnumerator; // children (not grand children)
    function GetNextSibling: TXDBNode;
    function GetPrevSibling: TXDBNode;
    function GetNext: TXDBNode; // first child, then next sibling, then next sibling of parent, ...
    function GetNextSkipChildren: TXDBNode; // first next sibling, then next sibling of parent, ...
    function GetPrev: TXDBNode; // the reverse of GetNext
    function GetLastLeaf: TXDBNode; // get last child of last child of ...
    function GetRoot: TXDBNode; // get top most parent
    function GetFullFilename: string; // GetFullFilename of document
    function GetEnumeratorAllChildren: TXDBNodeAllChildEnumerator; // all children including grand children
    function CompareStartPosition(Node: TXDBNode): integer;  // -1 if Self starts in tree before Node, 0 if Self=Node, 1 if Self starts after, if not in same tree raise exception
    function InsertAfter(Child: TXDBNode): TXDBNode;
    function InsertBefore(Child: TXDBNode): TXDBNode;

    // ancestors
    function FindSharedParent(Node: TXDBNode): TXDBNode; // returns deepest shared parent
    function FindParentWithName(Name: PChar;
                            ExceptionOnNotFound: boolean = false): TXDBTreeNode;

    // search
    function FindChildWithPath(const Path: String;
           const OnIterate: TXDBNodeIterateEvent = nil): TXDBNode;
    function FindChildWithPath(Path: PChar;
           const OnIterate: TXDBNodeIterateEvent = nil): TXDBNode; // e.g. A/B/*/C//D, a /*/ means one arbitrary node, // means any number of nodes in between
    function CheckSimpleExpr(Expr: PChar; Position: integer): boolean; // e.g. [@xml:id='Value']
    function CheckSimpleTerm(var Expr: PChar; Position: integer): boolean; // e.g. @xml:id='Value'

    procedure CheckConsistency; virtual;
  end;
  PXDBNode = ^TXDBNode;
  TXDBNodeList = specialize TFPGList<TXDBNode>;

  TXDBAttribute = record
    Name: string;
    Value: string;
  end;
  PXDBAttribute = ^TXDBAttribute;

  { TXDBNodeWithAttributes }

  TXDBNodeWithAttributes = class(TXDBNode)
  public
    Name: string;
    Attributes: PXDBAttribute;
    AttributeCount: integer;
    destructor Destroy; override;
    function GetName: string; override;
    function CompareName(aName: PChar): integer; override;
    function IndexOfAttributeP(const AttributeName: PChar): integer; override;
    function GetAttributeP(const AttributeName: PChar): string; override;
    procedure SetAttributeP(const AttributeName, aValue: PChar); override;
  end;

  { TXDBLeafNode }

  TXDBLeafNode = class(TXDBNodeWithAttributes)
  public
    Value: string;
  end;

  { TXDBTreeNode }

  TXDBTreeNode = class(TXDBNodeWithAttributes)
  protected
    function GetChild(Index: integer): TXDBNode; override;
  public
    Children: PXDBNode;
    ChildCount: integer;
    destructor Destroy; override;
    function GetChildCount: integer; override;
    function Insert(Index: integer; Child: TXDBNode): TXDBNode; override;
    function Remove(Child: TXDBNode): TXDBNode; override;
    procedure CheckConsistency; override;
  end;

  TXDBDocument = class;

  { TXDBRootNode }

  TXDBRootNode = class(TXDBTreeNode)
  public
    Doc: TXDBDocument;
    procedure CheckConsistency; override;
  end;

  TXDBDirectory = class;

  { TXDBFile - base for a file or a directory }

  TXDBFile = class
  private
    FDirectory: TXDBDirectory;
  public
    Filename: string;
    FullFilename: string;
    constructor Create(aFilename: string);
    destructor Destroy; override;
    function GetFullFilename: string;
    property Directory: TXDBDirectory read FDirectory;
    function CheckDocPath(const DocPath: string; out ChildMightFit: boolean;
        MaxDepth: integer = 1000): boolean;
  end;
  TXDBFileList = specialize TFPGList<TXDBFile>;

  { TXDBDocument - a single xml file }

  TXDBDocument = class(TXDBFile)
  public
    Root: TXDBRootNode;
    XMLDoc: TXMLDocument;
    ErrorMsg: string;
    FileAge: longint; // the date on disk
    LoadAge: longint; // the date on disk, when the file was successfully loaded
    DBAge: TDateTime; // the date when the file was ready in the server
    constructor Create(aFilename: string);
    destructor Destroy; override;
    procedure CreateTreeFromXML;
    procedure WriteToStream(s: TStream; Level: integer = 0);
    procedure ClearXMLDoc;
  end;

  { TXDBDirectoryEnumerator }

  TXDBDirectoryEnumerator = class
  private
    FDir: TXDBDirectory;
    FCurrent: TAVLTreeNode;
    function GetCurrent: TXDBFile;
  public
    constructor Create(Dir: TXDBDirectory);
    function MoveNext: boolean;
    property Current: TXDBFile read GetCurrent;
  end;

  TXDBFindNodesFlag = (
    xfnfFindFirst, // stop after first hit
    xfnfContinueInNextFile, // on abort continue search in next file
    xfnfDoNotCollect // do not add nodes to NodeList
    );
  TXDBFindNodesFlags = set of TXDBFindNodesFlag;

  { TXDBDirectory }

  TXDBDirectory = class(TXDBFile)
  private
    FFiles: TXDBAVLTree;
  public
    constructor Create(aFilename: string);
    destructor Destroy; override;
    procedure Clear;
    procedure Add(aFile: TXDBFile); virtual;
    procedure Remove(aFile: TXDBFile); virtual;
    procedure Delete(aFile: TXDBFile); virtual;
    function FindFile(aFilename: string): TXDBFile;
    property Files: TXDBAVLTree read FFiles; // tree of TXDBFile
    function GetEnumerator: TXDBDirectoryEnumerator;
    procedure ListFiles(DocPath: string; var FileList: TFPList;
                        Flags: TXDBListFlags);
    function FindFirstNode(NodePath: string;
                         ExceptionOnNotFound: boolean = false;
                         const OnIterate: TXDBNodeIterateEvent = nil): TXDBNode;
    procedure FindNodes(NodePath: string; var NodeList: TFPList;
                        Flags: TXDBFindNodesFlags = [];
                        const OnIterate: TXDBNodeIterateEvent = nil);
  end;

  { TXDBRootDirectory }

  TXDBRootDirectory = class(TXDBDirectory)
  public
    LongFileName: string;
    constructor Create(aFilename, aLongFilename: string);
  end;

  { TXDBRootDirectories }

  TXDBRootDirectories = class(TXDBDirectory)
  public
    procedure Add(aFile: TXDBFile); override;
    function FindRoot(aFilename: string): TXDBRootDirectory;
    function FindLongFileName(const aLongFileName: string): TXDBRootDirectory;
  end;

  { TXPathIndexItem }

  TXPathIndexItem = class
  public
    Node: TXDBNode;
    Key: string;
  end;

  {  Get a stack optimized list of parents }
type

  { TXDBStackNodeList }

  TXDBStackNodeList = object
  public
    Count: integer;
    Stack: array[0..15] of TXDBNode;
    DynCapacity: integer;
    Dyn: PXDBNode;  // if there are more than 16 use heap
    procedure Init;
    procedure Clear;
    function Get(Index: integer): TXDBNode;
    procedure Add(Node: TXDBNode);
  end;

procedure GetXDBNodeParents(Node: TXDBNode; out Parents: TXDBStackNodeList); // returns list of parents, starting with Node

procedure CreateXDBTree(XMLDoc: TXMLDocument; out Root: TXDBRootNode;
  CombineStrings: boolean = false);
procedure ReadXDBTree(out Root: TXDBRootNode; XMLStream: TStream;
  CombineStrings: boolean = false);
procedure ReadXDBTree(out Root: TXDBRootNode; const Filename: string;
  CombineStrings: boolean = false);

procedure SplitNodePath(const NodePath: string; out DocPath, XPath: string);

function CompareXDBFiles(XDBFile1, XDBFile2: Pointer): integer;
function CompareFilenameWithXDBFile(aFilenameStr, aXDBFile: Pointer): integer;

function CompareXPIIStr(Item1, Item2: Pointer): integer;

implementation

procedure GetXDBNodeParents(Node: TXDBNode; out Parents: TXDBStackNodeList);
begin
  Parents.Init;
  while Node<>nil do begin
    Parents.Add(Node);
    Node:=Node.Parent;
  end;
end;

procedure CreateXDBTree(XMLDoc: TXMLDocument; out Root: TXDBRootNode;
  CombineStrings: boolean);
{off $DEFINE VerboseCreateDBNodes}
var
  AllStrings: TXDBStringTree;

  procedure CreateNodes(DOMNode: TDOMNode; XDBNode: TXDBTreeNode);
  var
    DOMChild: TDOMNode;
    Size: Integer;
    i: Integer;
    XDBChild: TXDBNode;
    Element: TDOMElement;
    XDBElem: TXDBTreeNode;
    XDBAttr: PXDBAttribute;
    DomAttr: TDOMNode;
    j: Integer;
    DOMText: TDOMText;
    XDBText: TXDBLeafNode;
    XDBAttrNode: TXDBNodeWithAttributes;
    Cnt: Integer;
  begin
    // create children
    Cnt:=DOMNode.ChildNodes.Count;
    Size:=SizeOf(TXDBNode)*Cnt;
    XDBNode.Children:=GetMem(Size);
    FillByte(XDBNode.Children^,Size,0);
    for i:=0 to Cnt-1 do begin
      DOMChild:=DOMNode.ChildNodes.Item[i];
      XDBChild:=nil;
      XDBAttrNode:=nil;
      if DOMChild is TDOMElement then begin
        // element
        Element:=TDOMElement(DOMChild);
        {$IFDEF VerboseCreateDBNodes}
        debugln([Space(XDBNode.GetLevel*2),'CreateNodes Element=',Element.NodeName,':',DbgSName(DOMChild),' NodeValue="',dbgstr(Element.NodeValue),'"']);
        {$ENDIF}
        XDBElem:=TXDBTreeNode.Create;
        XDBAttrNode:=XDBElem;
        XDBElem.Name:=Element.NodeName;
        if AllStrings<>nil then AllStrings.ReplaceString(XDBElem.Name);
        XDBChild:=XDBElem;
      end else if DOMChild is TDOMText then begin
        // text node
        DOMText:=TDOMText(DOMChild);
        XDBText:=TXDBLeafNode.Create;
        XDBAttrNode:=XDBText;
        XDBText.Name:=DOMText.NodeName;
        if AllStrings<>nil then AllStrings.ReplaceString(XDBText.Name);
        XDBText.Value:=DOMText.NodeValue;
        {$IFDEF VerboseCreateDBNodes}
        debugln([Space(XDBNode.GetLevel*2),'CreateNodes Leaf=',XDBText.Name,' Text="',dbgstr(XDBText.Value),'"']);
        {$ENDIF}
        XDBChild:=XDBText;
      end else begin
        // unsupported node
        {$IFDEF VerboseCreateDBNodes}
        debugln([Space(XDBNode.GetLevel*2),'CreateNodes skipped ',DbgSName(DOMChild)]);
        {$ENDIF}
        continue;
      end;
      if (XDBAttrNode<>nil)
      and (DOMChild.Attributes<>nil) and (DOMChild.Attributes.Length>0) then begin
        // create attributes
        XDBAttrNode.AttributeCount:=DOMChild.Attributes.Length;
        Size:=XDBAttrNode.AttributeCount*SizeOf(TXDBAttribute);
        XDBAttrNode.Attributes:=Getmem(Size);
        FillByte(XDBAttrNode.Attributes^,Size,0);
        for j:=0 to XDBAttrNode.AttributeCount-1 do begin
          XDBAttr:=@XDBAttrNode.Attributes[j];
          DomAttr:=DOMChild.Attributes.Item[j];
          XDBAttr^.Name:=DomAttr.NodeName;
          if AllStrings<>nil then AllStrings.ReplaceString(XDBAttr^.Name);
          XDBAttr^.Value:=DomAttr.NodeValue;
        end;
      end;
      XDBChild.IndexInParent:=XDBNode.ChildCount;
      XDBNode.Children[XDBChild.IndexInParent]:=XDBChild;
      inc(XDBNode.ChildCount);
      XDBChild.Parent:=XDBNode;
      if XDBChild is TXDBTreeNode then
        CreateNodes(DOMChild,TXDBTreeNode(XDBChild));
    end;
    Size:=SizeOf(TXDBNode)*XDBNode.ChildCount;
    ReAllocMem(XDBNode.Children,Size);
  end;

begin
  Root:=nil;
  if XMLDoc=nil then exit;
  Root:=TXDBRootNode.Create;
  AllStrings:=nil;
  if CombineStrings then
    AllStrings:=TXDBStringTree.Create;
  try
    CreateNodes(XMLDoc,Root);
    {$IFDEF XDBNodesConsistencyCheck}
    Root.CheckConsistency;
    {$ENDIF}
  finally
    AllStrings.Free;
  end;
end;

procedure ReadXDBTree(out Root: TXDBRootNode; XMLStream: TStream;
  CombineStrings: boolean);
var
  doc: TXMLDocument;
begin
  doc:=nil;
  try
    ReadXMLFile(doc,XMLStream);
    CreateXDBTree(doc,Root,CombineStrings);
  finally
    doc.Free;
  end;
end;

procedure ReadXDBTree(out Root: TXDBRootNode; const Filename: string;
  CombineStrings: boolean);
var
  fs: TFileStream;
begin
  fs:=TFileStream.Create(Filename,fmOpenRead+fmShareDenyWrite);
  try
    ReadXDBTree(Root,fs,CombineStrings);
  finally
    fs.Free;
  end;
end;

procedure SplitNodePath(const NodePath: string; out DocPath, XPath: string);
// NodePath is doc(DocPath)XPath or XPath

  procedure RaiseError;
  begin
    raise Exception.Create('invalid NodePath '+NodePath);
  end;

var
  p: PChar;
  Level: Integer;
  StartPos: PChar;
begin
  DocPath:='';
  XPath:='';
  if NodePath='' then exit;
  p:=PChar(NodePath);
  if (p^='d') and (p[1]='o') and (p[2]='c') and (p[3]='(') then begin
    inc(p,4);
    StartPos:=p;
    Level:=1;
    repeat
      if p^=#0 then
        RaiseError
      else if p^='(' then begin
        inc(Level);
        inc(p);
      end else if p^=')' then begin
        dec(Level);
        if Level=0 then break;
        inc(p);
      end else if p^='/' then begin
        inc(p);
        if p^=#0 then RaiseError;
        inc(p,UTF8CharacterLength(p));
      end else
        inc(p);
    until false;
    DocPath:=copy(NodePath,5,p-StartPos);
    XPath:=copy(NodePath,p-PChar(NodePath)+2,length(NodePath));
  end else begin
    XPath:=NodePath;
  end;
end;

function CompareXDBFiles(XDBFile1, XDBFile2: Pointer): integer;
var
  File1: TXDBFile absolute XDBFile1;
  File2: TXDBFile absolute XDBFile2;
begin
  Result:=CompareFilenames(File1.Filename,File2.Filename);
end;

function CompareFilenameWithXDBFile(aFilenameStr, aXDBFile: Pointer): integer;
var
  aFile: TXDBFile absolute aXDBFile;
begin
  Result:=CompareFilenames(AnsiString(aFilenameStr),aFile.Filename);
end;

function CompareXPIIStr(Item1, Item2: Pointer): integer;
var
  IndexItem1: TXPathIndexItem absolute Item1;
  IndexItem2: TXPathIndexItem absolute Item2;
begin
  Result:=CompareStr(IndexItem1.Key,IndexItem2.Key);
end;

type

  { TFindNodeHandler }

  TFindNodeHandler = class
  public
    Nodes: PFPList;
    Abort: boolean;
    Found: boolean;
    Handler: TXDBNodeIterateEvent;
    procedure OnIterate(Node: TXDBNode; var TheAbort, SkipChildren: boolean);
  end;

{ TXDBStackNodeList }

function TXDBStackNodeList.Get(Index: integer): TXDBNode;
var
  i: Integer;
begin
  i:=Index-High(Stack)-1;
  if i<0 then
    Result:=Stack[Index]
  else
    Result:=Dyn[i];
end;

procedure TXDBStackNodeList.Init;
begin
  Count:=0;
  Dyn:=nil;
  DynCapacity:=0;
end;

procedure TXDBStackNodeList.Clear;
begin
  if Dyn<>nil then begin
    FreeMem(Dyn,0);
    Dyn:=nil;
    DynCapacity:=0;
  end;
  Count:=0;
end;

procedure TXDBStackNodeList.Add(Node: TXDBNode);
var
  i: Integer;
begin
  i:=Count-High(Stack)-1;
  if i<0 then
    Stack[Count]:=Node
  else begin
    if i>=DynCapacity then begin
      DynCapacity:=DynCapacity+16;
      ReAllocMem(Dyn,SizeOf(PXDBNode)*DynCapacity);
    end;
    Dyn[i]:=Node;
  end;
  inc(Count);
end;

{ TXDBNodeAllChildEnumerator }

constructor TXDBNodeAllChildEnumerator.Create(Node: TXDBNode);
begin
  if Node is TXDBTreeNode then
    FNode:=TXDBTreeNode(Node);
end;

function TXDBNodeAllChildEnumerator.MoveNext: boolean;
begin
  if FNode=nil then exit(false);
  if FCurrent=nil then begin
    FCurrent:=FNode.GetNext;
    FEnd:=FNode.GetNextSkipChildren;
  end else
    FCurrent:=FCurrent.GetNext;
  if FCurrent=FEnd then
    FCurrent:=nil;
  Result:=(FCurrent<>nil);
end;

function TXDBNodeAllChildEnumerator.GetEnumerator: TXDBNodeAllChildEnumerator;
begin
  Result:=Self;
end;

{ TFindNodeHandler }

procedure TFindNodeHandler.OnIterate(Node: TXDBNode; var TheAbort,
  SkipChildren: boolean);
begin
  //debugln(['TFindNodeHandler.OnIterate ',Node.GetDPath]);
  Found:=true;
  if Nodes<>nil then begin
    if Nodes^=nil then
      Nodes^:=TFPList.Create;
    Nodes^.Add(Node);
  end;
  if Abort then TheAbort:=Abort;
  if Assigned(Handler) then
    Handler(Node,TheAbort,SkipChildren);
end;

{ TXDBRootNode }

procedure TXDBRootNode.CheckConsistency;
begin
  if (Doc<>nil) and (Doc.Root<>Self) then
    raise Exception.Create('');
  inherited CheckConsistency;
end;

{ TXDBDirectoryEnumerator }

function TXDBDirectoryEnumerator.GetCurrent: TXDBFile;
begin
  Result:=TXDBFile(FCurrent.Data);
end;

constructor TXDBDirectoryEnumerator.Create(Dir: TXDBDirectory);
begin
  FDir:=Dir;
end;

function TXDBDirectoryEnumerator.MoveNext: boolean;
begin
  if FCurrent=nil then
    FCurrent:=FDir.FFiles.FindLowest
  else
    FCurrent:=FDir.FFiles.FindSuccessor(FCurrent);
  Result:=FCurrent<>nil;
end;

{ TXDBNodeEnumerator }

constructor TXDBNodeEnumerator.Create(Node: TXDBNode);
begin
  if Node is TXDBTreeNode then
    FNode:=TXDBTreeNode(Node);
  FPosition:=-1;
end;

function TXDBNodeEnumerator.MoveNext: boolean;
begin
  if FNode<>nil then begin
    inc(FPosition);
    if FPosition<FNode.ChildCount then begin
      Result:=true;
      FCurrent:=FNode.Children[FPosition];
    end else begin
      Result:=false;
      FCurrent:=nil;
    end;
  end else begin
    Result:=false;
  end;
  //debugln(['TXDBNodeEnumerator.MoveNext Result=',Result,' FCurrent=',DbgSName(FCurrent)]);
end;

{ TXDBRootDirectory }

constructor TXDBRootDirectory.Create(aFilename, aLongFilename: string);
begin
  LongFileName:=aLongFilename;
  inherited Create(aFilename);
end;

{ TXDBRootDirectories }

procedure TXDBRootDirectories.Add(aFile: TXDBFile);
begin
  if not (aFile is TXDBRootDirectory) then
    raise Exception.Create('TXDBDirectories.Add file must be TXDBRootDirectory');
  if TXDBRootDirectory(aFile).LongFileName='' then
    raise Exception.Create('TXDBDirectories.Add directory without LongFileName');
  inherited Add(aFile);
end;

function TXDBRootDirectories.FindRoot(aFilename: string): TXDBRootDirectory;
begin
  Result:=FindFile(aFilename) as TXDBRootDirectory;
end;

function TXDBRootDirectories.FindLongFileName(const aLongFileName: string
  ): TXDBRootDirectory;
var
  Node: TAVLTreeNode;
begin
  Node:=FFiles.FindLowest;
  while Node<>nil do begin
    Result:=TXDBRootDirectory(Node.Data);
    if CompareFilenames(Result.LongFileName,aLongFileName)=0 then exit;
    Node:=FFiles.FindSuccessor(Node);
  end;
  Result:=nil;
end;

{ TXDBNode }

function TXDBNode.GetChild(Index: integer): TXDBNode;
begin
  raise EListError.Create('Index '+IntToStr(Index)+' out of bounds '+IntToStr(GetChildCount));
  Result:=nil;
end;

function TXDBNode.GetLevel: integer;
var
  Node: TXDBNode;
begin
  Result:=0;
  Node:=Parent;
  while Node<>nil do begin
    inc(Result);
    Node:=Node.Parent;
  end;
end;

function TXDBNode.GetEnumerator: TXDBNodeEnumerator;
begin
  Result:=TXDBNodeEnumerator.Create(Self);
end;

function TXDBNode.GetFirstChild: TXDBNode;
begin
  if (Self is TXDBTreeNode) and (TXDBTreeNode(Self).ChildCount>0) then
    Result:=TXDBTreeNode(Self).Children[0]
  else
    Result:=nil;
end;

function TXDBNode.GetLastChild: TXDBNode;
begin
  if (Self is TXDBTreeNode) and (TXDBTreeNode(Self).ChildCount>0) then
    Result:=TXDBTreeNode(Self).Children[TXDBTreeNode(Self).ChildCount-1]
  else
    Result:=nil;
end;

function TXDBNode.IndexOfName(Name: PChar): integer;
var
  i: Integer;
  Child: TXDBNode;
begin
  for i:=0 to GetChildCount-1 do begin
    Child:=GetChild(i);
    if not (Child is TXDBNodeWithAttributes) then continue;
    if CompareXMLNamesPtrs(Name,Pointer(TXDBNodeWithAttributes(Child).Name))=0
    then begin
      Result:=i;
      exit;
    end;
  end;
  Result:=-1;
end;

function TXDBNode.FindChildWithName(Name: PChar; ExceptionOnNotFound: boolean
  ): TXDBNode;

  procedure RaiseNotFound;
  begin
    raise Exception.Create('node '+GetXMLName(Name)+' not found in '+GetNodeDPath);
  end;

var
  i: Integer;
begin
  i:=IndexOfName(Name);
  if i>=0 then
    Result:=GetChild(i)
  else if ExceptionOnNotFound then
    RaiseNotFound
  else
    Result:=nil;
end;

function TXDBNode.Insert(Index: integer; Child: TXDBNode): TXDBNode;
begin
  raise Exception.Create('this node type can not have child nodes');
  Result:=nil;
end;

function TXDBNode.Add(Child: TXDBNode): TXDBNode;
begin
  Result:=Insert(GetChildCount,Child);
end;

function TXDBNode.Remove(Child: TXDBNode): TXDBNode;
begin
  raise Exception.Create('this node type can not have child nodes');
  Result:=nil;
end;

function TXDBNode.GetNextSibling: TXDBNode;
var
  i: Integer;
begin
  i:=IndexInParent+1;
  if (Parent<>nil) and (i<Parent.ChildCount) then
    Result:=Parent.Children[i]
  else
    Result:=nil;
end;

function TXDBNode.GetPrevSibling: TXDBNode;
var
  i: Integer;
begin
  i:=IndexInParent-1;
  if (Parent<>nil) and (i>=0) then
    Result:=Parent.Children[i]
  else
    Result:=nil;
end;

function TXDBNode.GetNext: TXDBNode;
begin
  if (Self is TXDBTreeNode) and (0<TXDBTreeNode(Self).ChildCount) then
    Result:=TXDBTreeNode(Self).Children[0]
  else
    Result:=GetNextSkipChildren;
end;

function TXDBNode.GetNextSkipChildren: TXDBNode;
begin
  Result:=Self;
  while (Result.Parent<>nil) do begin
    if (Result.IndexInParent+1<Result.Parent.ChildCount) then begin
      Result:=Result.Parent.Children[Result.IndexInParent+1];
      exit;
    end;
    Result:=Result.Parent;
  end;
  Result:=nil;
end;

function TXDBNode.GetPrev: TXDBNode;
var
  Node: TXDBNode;
begin
  if IndexInParent=0 then begin
    Result:=Parent;
    exit;
  end;
  Result:=Parent.Children[IndexInParent-1];
  Node:=Result.GetLastLeaf;
  if Node<>nil then
    Result:=Node;
end;

function TXDBNode.GetLastLeaf: TXDBNode;
var
  Node: TXDBNode;
begin
  Result:=GetLastChild;
  if Result=nil then exit;
  repeat
    Node:=Result.GetLastChild;
    if Node=nil then exit;
    Result:=Node;
  until false;
end;

function TXDBNode.GetRoot: TXDBNode;
begin
  Result:=Self;
  while Result.Parent<>nil do
    Result:=Result.Parent;
end;

function TXDBNode.GetFullFilename: string;
var
  Root: TXDBNode;
begin
  Root:=GetRoot;
  if (Root is TXDBRootNode) and (TXDBRootNode(Root).Doc<>nil) then
    Result:=TXDBRootNode(Root).Doc.GetFullFilename;
end;

function TXDBNode.GetEnumeratorAllChildren: TXDBNodeAllChildEnumerator;
begin
  Result:=TXDBNodeAllChildEnumerator.Create(Self);
end;

function TXDBNode.CompareStartPosition(Node: TXDBNode): integer;
{ -1 if Self starts in tree before Node,
  0 if Self=Node,
  1 if Self starts after,
  if not in same tree raise exception
}
  function CompareIndex(Node1, Node2: TXDBNode): integer;
  var
    i: Integer;
  begin
    i:=Node1.IndexInParent-Node2.IndexInParent;
    if i<0 then Result:=-1
    else if i>0 then Result:=1
    else Result:=0;
  end;

var
  SelfParents, OtherParents: TXDBStackNodeList;
  SelfIndex: Integer;
  OtherIndex: Integer;
begin
  // quick test for common cases
  if Node=nil then
    raise Exception.Create('TXDBNode.CompareStartPosition Node=nil');
  if Node.Parent=Parent then
    exit(CompareIndex(Self,Node));
  // get shared parent
  GetXDBNodeParents(Self,SelfParents);
  GetXDBNodeParents(Node,OtherParents);
  SelfIndex:=SelfParents.Count-1;
  OtherIndex:=OtherParents.Count-1;
  while (SelfIndex>=0) and (OtherIndex>=0)
  and (SelfParents.Get(SelfIndex)=OtherParents.Get(OtherIndex)) do begin
    dec(SelfIndex);
    dec(OtherIndex);
  end;
  if SelfIndex>=0 then begin
    if OtherIndex>=0 then begin
      // nodes share a parent
      Result:=CompareIndex(SelfParents.Get(SelfIndex),OtherParents.Get(OtherIndex));
    end else begin
      // Node is parent of Self
      Result:=1;
    end;
  end else begin
    if OtherIndex>=0 then begin
      // Self if parent of Node
      Result:=-1;
    end else begin
      // Self is not in the same tree as Node
      Result:=-2;
    end;
  end;
  // free memory
  SelfParents.Clear;
  OtherParents.Clear;
  if Result=-2 then
    raise Exception.Create('TXDBNode.CompareStartPosition nodes not in same tree');
end;

function TXDBNode.InsertAfter(Child: TXDBNode): TXDBNode;
begin
  Result:=Parent.Insert(IndexInParent+1,Child);
end;

function TXDBNode.InsertBefore(Child: TXDBNode): TXDBNode;
begin
  Result:=Parent.Insert(IndexInParent,Child);
end;

function TXDBNode.FindSharedParent(Node: TXDBNode): TXDBNode;
var
  SelfParents, OtherParents: TXDBStackNodeList;
  SelfIndex: Integer;
  OtherIndex: Integer;
begin
  GetXDBNodeParents(Self,SelfParents);
  GetXDBNodeParents(Node,OtherParents);
  SelfIndex:=SelfParents.Count-1;
  OtherIndex:=OtherParents.Count-1;
  Result:=nil;
  while (SelfIndex>=0) and (OtherIndex>=0)
  and (SelfParents.Get(SelfIndex)=OtherParents.Get(OtherIndex)) do begin
    dec(SelfIndex);
    dec(OtherIndex);
  end;
  inc(SelfIndex);
  if SelfIndex<SelfParents.Count then
    Result:=SelfParents.Get(SelfIndex);
  SelfParents.Clear;
  OtherParents.Clear;
end;

function TXDBNode.FindParentWithName(Name: PChar; ExceptionOnNotFound: boolean
  ): TXDBTreeNode;

  procedure RaiseNotFound;
  begin
    raise Exception.Create('ancestor '+GetXMLName(Name)+' not found of '+GetNodeDPath);
  end;

begin
  Result:=Parent;
  while Result<>nil do begin
    if CompareXMLNamesPtrs(Name,Pointer(Result.Name))=0 then exit;
    Result:=Result.Parent;
  end;
  if ExceptionOnNotFound then
    RaiseNotFound;
end;

function TXDBNode.FindChildWithPath(const Path: String;
  const OnIterate: TXDBNodeIterateEvent): TXDBNode;
begin
  Result:=FindChildWithPath(PChar(Path),OnIterate);
end;

function TXDBNode.FindChildWithPath(Path: PChar;
  const OnIterate: TXDBNodeIterateEvent): TXDBNode;
{ A/B/*/C//D[@xml:id='value']
}

  procedure RaiseError;
  begin
    raise Exception.Create('TXDBNode.FindChildWithPath: invalid syntax: '+Path);
  end;

  function NodeFound(var Node: TXDBNode; out SkipChildren: boolean): boolean;
  // returns true to abort search
  var
    Abort: boolean;
  begin
    SkipChildren:=true;
    if Assigned(OnIterate) then begin
      Abort:=false;
      OnIterate(Node,Abort,SkipChildren);
      if Abort then exit(true);
      Node:=nil;
      Result:=false;
    end else begin
      Result:=true;
    end;
  end;

  procedure RaiseUnexpectedChar(Expected: char; Expr: PChar);
  begin
    raise Exception.Create('TXDBNode.FindChildWithPath: expected '+Expected+', but found "'+dbgstr(Expr)+'"');
  end;

var
  l: Integer;
  Child: TXDBNode;
  i: Integer;
  Last: TXDBNode;
  NameStart: PChar;
  SkipChildren: boolean;
  Position: Integer;
  Cnt: Integer;
  p: PChar;
begin
  Result:=nil;
  if (Path=nil) or (Path^=#0) then exit;
  l:=GetXMLNameLength(Path);
  //debugln([Space(GetLevel*2),'TXDBNode.FindChildWithPath Current=',GetDPath,'[',IndexInParent,'] Path=',Path,' Sub=',GetXMLName(Path)]);
  if l>0 then begin
    // search node with name
    NameStart:=Path;
    inc(Path,l);
    if not (Path^ in [#0,'/','[']) then
      RaiseError;
    Position:=-1;
    for i:=0 to GetChildCount-1 do begin
      Child:=GetChild(i);
      //debugln(['TXDBNode.FindChildWithPath NameStart=',GetXMLName(NameStart),' Child=',Child.GetName,' Cmp=',Child.CompareName(NameStart)]);
      if Child.CompareName(NameStart)<>0 then continue;
      inc(Position);
      if Path^=#0 then begin
        Result:=Child;
        if NodeFound(Result,SkipChildren) then exit;
      end else if Path^='[' then begin
        if Child.CheckSimpleExpr(Path,Position) then begin
          Result:=Child;
          if NodeFound(Result,SkipChildren) then exit;
        end;
      end else begin
        if Path[1]='/' then
          // double slash '//'
          Result:=Child.FindChildWithPath(Path,OnIterate)
        else
          // single slash '/'
          Result:=Child.FindChildWithPath(Path+1,OnIterate);
        if Result<>nil then exit;
      end;
    end;
  end else if Path^='*' then begin
    inc(Path);
    if Path^=#0 then begin
      // e.g. A/*
      // return all direct children
      for i:=0 to GetChildCount-1 do begin
        Result:=GetChild(i);
        if NodeFound(Result,SkipChildren) then exit;
      end;
    end else if (Path^<>'/') then begin
      // invalid syntax, e.g. *A
      RaiseError;
    end else if (Path[1]='/') then begin
      // invalid syntax, e.g. *//
      RaiseError;
    end else begin
      //  */...
      // test all children
      inc(Path);
      for i:=0 to GetChildCount-1 do begin
        //debugln([GetIndentStr(GetLevel*2),'TXDBNode.FindChildWithPath ',i,'/',GetChildCount]);
        Result:=GetChild(i).FindChildWithPath(Path,OnIterate);
        if Result<>nil then exit;
      end;
    end;
  end else if (Path^='/') and (Path[1]='/') then begin
    inc(Path,2);
    if (Path^='/') or (Path^='*') then begin
      // invalid syntax, e.g. /// or //*
      RaiseError;
    end;
    // test root
    Result:=FindChildWithPath(Path,OnIterate);
    if Result<>nil then exit;
    // test all children and grand children
    Child:=GetNext;
    if Child<>nil then begin
      Last:=GetNextSkipChildren;
      while Child<>Last do begin
        SkipChildren:=false;
        if Path^=#0 then begin
          Result:=Child;
          if NodeFound(Result,SkipChildren) then exit;
        end else begin
          Result:=Child.FindChildWithPath(Path,OnIterate);
          if Result<>nil then exit;
        end;
        if SkipChildren then
          Child:=Child.GetNextSkipChildren
        else
          Child:=Child.GetNext;
      end;
    end;
  end else if Path^='[' then begin
    //debugln(['TXDBNode.FindChildWithPath Self=',GetNodeDPath,' Path=',Path]);
    if Path[1] in ['0'..'9'] then begin
      // check if [number]
      p:=Path+1;
      Position:=0;
      Cnt:=GetChildCount;
      repeat
        Position:=Position*10+ord(p^)-ord('0');
        if Position>=Cnt then
          exit;
        inc(p);
      until not (p^ in ['0'..'9']);
      if p^<>']' then
        RaiseUnexpectedChar(']',p);
      // child at position found
      inc(p);
      Child:=GetChild(Position);
      if p^=#0 then begin
        Result:=Child;
        if NodeFound(Result,SkipChildren) then exit;
      end;
      // search grandchild
      if p^<>'/' then
        RaiseUnexpectedChar('/',p);
      Path:=p+1;
      Result:=Child.FindChildWithPath(Path,OnIterate);
      exit;
    end else begin
      // [expression]
      Position:=0;
      for Position:=0 to GetChildCount-1 do begin
        Child:=GetChild(Position);
        //debugln(['TXDBNode.FindChildWithPath NameStart=',GetXMLName(NameStart),' Child=',Child.GetName,' Cmp=',Child.CompareName(NameStart)]);
        if Child.CheckSimpleExpr(Path,Position) then begin
          Result:=Child;
          if NodeFound(Result,SkipChildren) then exit;
        end;
      end;
    end;
  end else begin
    // invalid syntax
    RaiseError;
  end;
end;

function TXDBNode.CheckSimpleExpr(Expr: PChar; Position: integer): boolean;
// for example [@xml:id='value'] [@sameAs] [3]

  procedure RaiseUnexpectedChar(Expected: char);
  begin
    raise Exception.Create('TXDBNode.CheckSimpleExpr: expected '+Expected+', but found "'+dbgstr(Expr)+'"');
  end;

begin
  Result:=false;
  //debugln([Space(GetLevel*2),'TXDBNode.CheckSimpleExpr Current=',GetDPath,' Expr=',Expr]);
  if Expr^<>'[' then
    RaiseUnexpectedChar('[');
  inc(Expr);
  Result:=CheckSimpleTerm(Expr,Position);
  if not Result then exit;
  if Expr^<>']' then
    RaiseUnexpectedChar(']');
  inc(Expr);
  if Expr^<>#0 then
    RaiseUnexpectedChar(']');
end;

function TXDBNode.CheckSimpleTerm(var Expr: PChar; Position: integer): boolean;
// for example [@xml:id='value'] [@sameAs] [3]

  procedure RaiseInvalidSyntax;
  begin
    raise Exception.Create('TXDBNode.CheckSimpleTerm invalid syntax '+dbgstr(Expr));
  end;

  procedure RaiseUnexpectedChar(Expected: char);
  begin
    raise Exception.Create('TXDBNode.CheckSimpleTerm: expected '+Expected+', but found "'+dbgstr(Expr)+'"');
  end;

var
  i: Integer;
  StartPos: PChar;
  AttrNode: TXDBNodeWithAttributes;
  Attr: PXDBAttribute;
  EndPos: PChar;
  len: PtrInt;
  Child: TXDBNode;
  OldExpr: PChar;
begin
  Result:=false;
  //debugln([Space(GetLevel*2),'TXDBNode.CheckSimpleTerm Current=',GetDPath,' Expr=',Expr]);
  if Expr^='@' then begin
    if not (Self is TXDBNodeWithAttributes) then exit;
    AttrNode:=TXDBNodeWithAttributes(Self);
    inc(Expr);
    i:=IndexOfAttribute(Expr);
    //debugln([Space(GetLevel*2),'TXDBNode.CheckSimpleTerm  ',i,' ',GetXMLAttrName(Expr)]);
    if i<0 then exit;
    inc(Expr,GetXMLAttriNameLength(Expr));
    if Expr^<>'=' then
      RaiseUnexpectedChar('=');
    inc(Expr);
    if Expr^<>'''' then
      RaiseUnexpectedChar('''');
    inc(Expr);
    StartPos:=Expr;
    while not (Expr^ in ['''',#0]) do inc(Expr);
    if Expr^<>'''' then
      RaiseUnexpectedChar('''');
    EndPos:=Expr;
    inc(Expr);
    Attr:=@AttrNode.Attributes[i];
    len:=EndPos-StartPos;
    //debugln([Space(GetLevel*2),'TXDBNode.CheckSimpleTerm  Attr=',Attr^.Name,' Value=',Attr^.Value]);
    if (length(Attr^.Value)<>len)
    or ((len>0) and (not CompareMem(StartPos,Pointer(Attr^.Value),len))) then
      exit; // value does not fit
    //debugln([Space(GetLevel*2),'TXDBNode.CheckSimpleTerm  Attr=',Attr^.Name,' Value=',Attr^.Value,' fits']);
    Result:=true;
  end else if Expr^ in ['0'..'9'] then begin
    i:=0;
    repeat
      i:=i*10+ord(Expr^)-ord('0');
      if i>10000000 then
        RaiseUnexpectedChar(']');
      inc(Expr);
    until not (Expr^ in ['0'..'9']);
    Result:=i=Position;
  end else if IsXMLNameStartChar[Expr^] then begin
    //debugln([Space(GetLevel*2),'TXDBNode.CheckSimpleTerm searching child: ',GetXMLName(Expr)]);
    for i:=0 to GetChildCount-1 do begin
      Child:=GetChild(i);
      //debugln([Space(GetLevel*2),'TXDBNode.CheckSimpleTerm child=',Child.GetName,' ',Child.CompareName(Expr)]);
      if Child.CompareName(Expr)<>0 then continue;
      OldExpr:=Expr;
      inc(Expr,GetXMLNameLength(Expr));
      //debugln([Space(GetLevel*2),'TXDBNode.CheckSimpleTerm found child ',Child.GetName,' currentexpr=',Expr]);
      if Expr^='/' then begin
        inc(Expr);
        if Child.CheckSimpleTerm(Expr,Position) then
          exit(true);
      end else if Expr^ in [#0..' ',']','|',')'] then
        exit(true);
      Expr:=OldExpr;
    end;
  end else
    RaiseUnexpectedChar('@');
end;

procedure TXDBNode.WriteToStream(s: TStream; Level: integer; WithTags: boolean);

  procedure w(const Value: string; Encode: boolean = false);
  var
    p: PChar;
    StartPos: PChar;

    procedure Flush;
    begin
      if p=StartPos then exit;
      s.Write(StartPos^,p-StartPos);
      StartPos:=p;
    end;

    procedure WriteSpecialChar(Value: PChar);
    begin
      Flush;
      inc(p);
      StartPos:=p;
      s.Write(Value^,Strlen(Value));
    end;

  begin
    //debugln(['TXDBNode.WriteToStream.w Value="',dbgstr(Value),'"']);
    if Value='' then exit;
    if not Encode then
      s.Write(Value[1],length(Value))
    else begin
      if Value<>'' then begin
        p:=PChar(Value);
        StartPos:=p;
        while p^<>#0 do begin
          case p^ of
          '&': WriteSpecialChar('&amp;');
          '<': WriteSpecialChar('&lt;');
          '>': WriteSpecialChar('&gt;');
          '''': WriteSpecialChar('&apos;');
          '"': WriteSpecialChar('&quot;');
          else inc(p);
          end;
        end;
        Flush;
      end;
    end;
  end;

  procedure WriteAttrValue(const Value: string);
  begin
    w('"');
    w(Value,true);
    w('"');
  end;

  procedure WriteNode(CurNode: TXDBNode; CurLevel: integer);
  var
    AttrNode: TXDBNodeWithAttributes;
    i: Integer;
    Attr: PXDBAttribute;
    LeafNode: TXDBLeafNode;
    TreeNode: TXDBTreeNode;
    ChildNode: TXDBNode;
    j: Integer;
  begin
    //debugln(['WriteNode ',DbgSName(CurNode)]);
    if CurNode is TXDBNodeWithAttributes then begin
      AttrNode:=TXDBNodeWithAttributes(CurNode);
      if CurNode is TXDBLeafNode then begin
        // text
        LeafNode:=TXDBLeafNode(CurNode);
        if LeafNode.Value<>'' then
          w(LeafNode.Value,true);
      end else begin
        w('<');
        w(AttrNode.Name);
        if AttrNode.AttributeCount>0 then begin
          for i:=0 to AttrNode.AttributeCount-1 do begin
            Attr:=@AttrNode.Attributes[i];
            w(' ');
            w(Attr^.Name);
            w('=');
            WriteAttrValue(Attr^.Value);
          end;
        end;
        if CurNode is TXDBTreeNode then begin
          TreeNode:=TXDBTreeNode(CurNode);
          if TreeNode.ChildCount=0 then
            w('/>')
          else begin
            w('>');
            for i:=0 to TreeNode.ChildCount-1 do begin
              ChildNode:=TreeNode.Children[i];
              if (not (ChildNode is TXDBLeafNode))
              and ((i=0) or (not (TreeNode.Children[i-1] is TXDBLeafNode)))
              then begin
                // new line
                w(#10);
                for j:=0 to CurLevel do
                  w('  ');
              end;
              WriteNode(ChildNode,CurLevel+1);
            end;
            w('</');
            w(AttrNode.Name);
            w('>');
          end;
        end else
          w('/>');
      end;
    end;
  end;

var
  i: Integer;
begin
  //debugln(['TXDBNode.WriteToStream ',DbgSName(Node)]);
  if (Self is TXDBRootNode) or (not WithTags) then begin
    //debugln(['TXDBNode.WriteToStream ',RootNode.ChildCount]);
    for i:=0 to GetChildCount-1 do
      WriteNode(GetChild(i),Level);
  end else
    WriteNode(Self,Level);
end;

procedure TXDBNode.CheckConsistency;
begin
  if Parent<>nil then begin
    if IndexInParent<0 then
      raise Exception.Create('');
    if IndexInParent>=Parent.ChildCount then
      raise Exception.Create('');
    if Parent.Children[IndexInParent]<>Self then
      raise Exception.Create('');
  end else begin

  end;
end;

function TXDBNode.GetName: string;
begin
  Result:='';
end;

function TXDBNode.CompareName(aName: PChar): integer;
begin
  if (aName=nil) or (aName^=#0) then
    Result:=0
  else
    Result:=-1;
end;

function TXDBNode.GetSubDPath: string;
begin
  Result:=GetName;
  if (IndexInParent>0) or ((Parent<>nil) and (Parent.ChildCount>1)) then
    Result:=Result+'['+IntToStr(IndexInParent)+']';
end;

function TXDBNode.GetSubXPath: string;
begin
  Result:='['+IntToStr(IndexInParent)+']';
end;

function TXDBNode.GetDPath(WithRoot: boolean): string;
var
  Node: TXDBNode;
begin
  Result:=GetSubDPath;
  Node:=Self;
  repeat
    Node:=Node.Parent;
    if Node=nil then exit;
    if (not WithRoot) and (Node is TXDBRootNode) then exit;
    Result:=Node.GetSubDPath+'/'+Result;
  until false;
end;

function TXDBNode.GetXPath(WithRoot: boolean): string;
var
  Node: TXDBNode;
begin
  Result:=GetSubXPath;
  Node:=Self;
  repeat
    Node:=Node.Parent;
    if Node=nil then exit;
    if (not WithRoot) and (Node is TXDBRootNode) then exit;
    Result:=Node.GetSubXPath+'/'+Result;
  until false;
end;

function TXDBNode.GetNodeDPath(WithRoot: boolean): string;
begin
  Result:='doc('+GetFullFilename+')'+GetDPath(WithRoot);
end;

function TXDBNode.GetNodeXPath(WithRoot: boolean): string;
begin
  Result:='doc('+GetFullFilename+')'+GetXPath(WithRoot);
end;

function TXDBNode.GetChildCount: integer;
begin
  Result:=0;
end;

function TXDBNode.IndexOfAttribute(const AttributeName: string): integer;
begin
  if AttributeName='' then exit(-1);
  Result:=IndexOfAttributeP(PChar(AttributeName));
end;

function TXDBNode.IndexOfAttributeP(const AttributeName: PChar): integer;
begin
  Result:=-1;
end;

function TXDBNode.GetAttribute(const AttributeName: string): string;
begin
  if AttributeName='' then
    Result:=''
  else
    Result:=GetAttributeP(PChar(AttributeName));
end;

function TXDBNode.GetAttributeP(const AttributeName: PChar): string;
begin
  Result:='';
end;

procedure TXDBNode.SetAttribute(const AttributeName, aValue: string);
begin
  SetAttributeP(PChar(AttributeName),PChar(aValue));
end;

procedure TXDBNode.SetAttributeP(const AttributeName, aValue: PChar);
begin

end;

function TXDBNode.GetText: string;
var
  Child: TXDBNode;
  Cnt: Integer;
begin
  Result:='';
  Cnt:=GetChildCount;
  if Cnt=0 then exit;
  if (Cnt=1) then begin
    Child:=GetFirstChild;
    if Child is TXDBLeafNode then begin
      Result:=TXDBLeafNode(Child).Value;
      exit;
    end;
  end;
  Result:=GetXML(0,false);
end;

function TXDBNode.GetXML(Level: integer; WithTags: boolean): string;
var
  ss: TStringStream;
begin
  ss:=TStringStream.Create('');
  try
    WriteToStream(ss,Level,WithTags);
    Result:=ss.DataString;
  finally
    ss.Free;
  end;
end;

{ TXDBTreeNode }

function TXDBTreeNode.GetChild(Index: integer): TXDBNode;
begin
  if (Index>=0) and (Index<ChildCount) then
    Result:=Children[Index]
  else
    Result:=inherited GetChild(Index);
end;

destructor TXDBTreeNode.Destroy;
var
  i: Integer;
begin
  //writeln(Space(GetLevel*2),'TXDBTreeNode.Destroy ',GetName);
  if Children<>nil then begin
    for i:=0 to ChildCount-1 do begin
      //writeln(Space(GetLevel*2),'TXDBTreeNode.Destroy Self=',DbgSName(Self),' ',GetName);
      //writeln(Space(GetLevel*2),'TXDBTreeNode.Destroy Children[',i,']=',DbgSName(Children[i]));
      Children[i].Free;
    end;
    ReAllocMem(Children,0);
  end;
  inherited Destroy;
end;

function TXDBTreeNode.GetChildCount: integer;
begin
  Result:=ChildCount;
end;

function TXDBTreeNode.Insert(Index: integer; Child: TXDBNode): TXDBNode;

  procedure RaiseOutOfBounds;
  begin
    raise EListError.Create('Index '+IntToStr(Index)+' out of bounds '+IntToStr(GetChildCount));
  end;

var
  i: Integer;
begin
  if Child.Parent<>nil then
    Child.Parent.Remove(Child);
  if (Index<0) or (Index>ChildCount) then
    RaiseOutOfBounds;
  inc(ChildCount);
  ReAllocMem(Children,SizeOf(PXDBNode)*ChildCount);
  if Index<ChildCount-1 then
    System.Move(Children[Index],Children[Index+1],SizeOf(PXDBNode)*(ChildCount-Index-1));
  Children[Index]:=Child;
  Child.Parent:=Self;
  for i:=Index to ChildCount-1 do
    Children[i].IndexInParent:=i;
  Result:=Child;
end;

function TXDBTreeNode.Remove(Child: TXDBNode): TXDBNode;
var
  i: Integer;
begin
  if Child.Parent<>Self then
    raise Exception.Create('node is not my child');
  Child.Parent:=nil;
  i:=Child.IndexInParent;
  if i+1<ChildCount then
    System.Move(Children[i+1],Children[i],SizeOf(PXDBNode)*(ChildCount-i-1));
  dec(ChildCount);
  ReAllocMem(Children,SizeOf(PXDBNode)*ChildCount);
  for i:=Child.IndexInParent to ChildCount-1 do
    Children[i].IndexInParent:=i;
  Child.IndexInParent:=-1;
  Result:=Child;
end;

procedure TXDBTreeNode.CheckConsistency;
var
  i: Integer;
begin
  inherited CheckConsistency;
  if (ChildCount=0) xor (Children=nil) then
    raise Exception.Create('');
  for i:=0 to ChildCount-1 do
    Children[i].CheckConsistency;
end;

{ TXDBNodeWithAttributes }

destructor TXDBNodeWithAttributes.Destroy;
var
  i: Integer;
begin
  if Attributes<>nil then begin
    for i:=0 to AttributeCount-1 do begin
      Attributes[i].Name:='';
      Attributes[i].Value:='';
    end;
    ReAllocMem(Attributes,0);
  end;
  inherited Destroy;
end;

function TXDBNodeWithAttributes.GetName: string;
begin
  Result:=Name;
end;

function TXDBNodeWithAttributes.CompareName(aName: PChar): integer;
begin
  Result:=CompareXMLNames(aName,PChar(Name));
end;

function TXDBNodeWithAttributes.IndexOfAttributeP(const AttributeName: PChar
  ): integer;
begin
  Result:=AttributeCount-1;
  while (Result>=0)
  and (CompareXMLAttrNames(AttributeName,Pointer(Attributes[Result].Name))<>0) do
    dec(Result);
end;

function TXDBNodeWithAttributes.GetAttributeP(const AttributeName: PChar
  ): string;
var
  i: Integer;
begin
  i:=IndexOfAttributeP(AttributeName);
  if i>=0 then
    Result:=Attributes[i].Value
  else
    Result:='';
end;

procedure TXDBNodeWithAttributes.SetAttributeP(const AttributeName,
  aValue: PChar);
var
  i: Integer;
  Attr: PXDBAttribute;
begin
  i:=IndexOfAttributeP(AttributeName);
  if i>=0 then
    // replace value
    Attributes[i].Value:=aValue
  else begin
    // add new attribute
    ReAllocMem(Attributes,SizeOf(TXDBAttribute)*(AttributeCount+1));
    Attr:=@Attributes[AttributeCount];
    FillByte(Attr^,SizeOf(TXDBAttribute),0);
    Attr^.Name:=AttributeName;
    Attr^.Value:=aValue;
    inc(AttributeCount);
  end;
end;

{ TXDBDocument }

constructor TXDBDocument.Create(aFilename: string);
begin
  inherited Create(aFilename);
end;

destructor TXDBDocument.Destroy;
begin
  ClearXMLDoc;
  FreeAndNil(Root);
  inherited Destroy;
end;

procedure TXDBDocument.CreateTreeFromXML;
begin
  FreeAndNil(Root);
  CreateXDBTree(XMLDoc,Root,true);
  Root.Doc:=Self;
end;

procedure TXDBDocument.WriteToStream(s: TStream; Level: integer
  );
begin
  if Root<>nil then
    Root.WriteToStream(s,Level);
end;

procedure TXDBDocument.ClearXMLDoc;
begin
  FreeAndNil(XMLDoc);
end;

{ TXDBDirectory }

constructor TXDBDirectory.Create(aFilename: string);
begin
  inherited Create(aFilename);
  FFiles:=TXDBAVLTree.Create(@CompareXDBFiles);
end;

destructor TXDBDirectory.Destroy;
begin
  Clear;
  FreeAndNil(FFiles);
  inherited Destroy;
end;

procedure TXDBDirectory.Clear;
begin
  FFiles.FreeAndClear;
end;

procedure TXDBDirectory.Add(aFile: TXDBFile);
begin
  if aFile.FDirectory<>nil then
    raise Exception.Create('TXDBDirectory.Add file already in a directory');
  if aFile.Filename='' then
    raise Exception.Create('TXDBDirectory.Add file without filename');
  FFiles.Add(aFile);
  aFile.FDirectory:=Self;
  aFile.FullFilename:='';
end;

procedure TXDBDirectory.Remove(aFile: TXDBFile);
var
  Node: TAVLTreeNode;
begin
  Node:=FFiles.Find(aFile);
  if Node=nil then exit;
  aFile.FullFilename:='';
  aFile.FDirectory:=nil;
  FFiles.Delete(Node);
end;

procedure TXDBDirectory.Delete(aFile: TXDBFile);
begin
  Remove(aFile);
  aFile.Free;
end;

function TXDBDirectory.FindFile(aFilename: string): TXDBFile;
var
  Node: TAVLTreeNode;
begin
  if aFilename='' then exit(nil);
  Node:=FFiles.FindKey(Pointer(aFilename),@CompareFilenameWithXDBFile);
  if Node=nil then exit(nil);
  Result:=TXDBFile(Node.Data);
end;

function TXDBDirectory.GetEnumerator: TXDBDirectoryEnumerator;
begin
  Result:=TXDBDirectoryEnumerator.Create(Self);
end;

procedure TXDBDirectory.ListFiles(DocPath: string; var FileList: TFPList;
  Flags: TXDBListFlags);

  procedure Found(aFile: TXDBFile);
  begin
    if (aFile.Directory=nil) and (aFile.Filename='') then
      exit; // do not add the root directory
    if aFile is TXDBDirectory then begin
      if not (xlfAddDirectories in Flags) then exit;
    end else begin
      if not (xlfAddFiles in Flags) then exit;
    end;
    if FileList=nil then
      FileList:=TFPList.Create;
    FileList.Add(aFile);
  end;

  procedure Search(aFile: TXDBFile);
  var
    ChildMightFit: boolean;
    SubFile: TXDBFile;
  begin
    if aFile.CheckDocPath(DocPath,ChildMightFit) then
      Found(aFile);
    if ChildMightFit and (aFile is TXDBDirectory) then
      for SubFile in TXDBDirectory(aFile) do
        Search(SubFile);
  end;

begin
  Search(Self);
end;

function TXDBDirectory.FindFirstNode(NodePath: string;
  ExceptionOnNotFound: boolean; const OnIterate: TXDBNodeIterateEvent
  ): TXDBNode;

  procedure RaiseNotFound;
  begin
    raise Exception.Create('Node not found: '+NodePath);
  end;

var
  NodeList: TFPList;
begin
  Result:=nil;
  NodeList:=nil;
  try
    FindNodes(NodePath,NodeList,[xfnfFindFirst],OnIterate);
    if (NodeList<>nil) and (NodeList.Count>0) then
      Result:=TXDBNode(NodeList[0])
    else if ExceptionOnNotFound then
      RaiseNotFound;
  finally
    NodeList.Free;
  end;
end;

procedure TXDBDirectory.FindNodes(NodePath: string; var NodeList: TFPList;
  Flags: TXDBFindNodesFlags; const OnIterate: TXDBNodeIterateEvent);
var
  DocPath, XPath: string;
  Handler: TFindNodeHandler;

  procedure Search(aFile: TXDBFile; IgnoreDocPath: boolean);
  // returns true to abort any further search
  var
    SubFile: TXDBFile;
    Fits: Boolean;
    ChildMightFit: boolean;
    Doc: TXDBDocument;
  begin
    if (DocPath='') or IgnoreDocPath then begin
      Fits:=true;
      ChildMightFit:=true;
    end else
      Fits:=aFile.CheckDocPath(DocPath,ChildMightFit);
    //debugln(['Search ',afile.GetFullFilename,' Fits=',Fits,' ChildMightFit=',ChildMightFit]);
    if (aFile is TXDBDirectory) and ChildMightFit then begin
      for SubFile in TXDBDirectory(aFile) do begin
        Search(SubFile,Fits);
        if Handler.Found
        and (xfnfFindFirst in Flags)
        and (not (xfnfContinueInNextFile in Flags))
        then exit;
      end;
    end else if (aFile is TXDBDocument) and Fits then begin
      Doc:=TXDBDocument(aFile);
      if (Doc.Root<>nil) then begin
        Handler.Found:=false;
        Doc.Root.FindChildWithPath(XPath,@Handler.OnIterate);
      end;
    end;
  end;

begin
  SplitNodePath(NodePath,DocPath,XPath);
  //debugln(['TXDBDirectory.FindNodes DocPath=',DocPath,' XPath=',XPath]);
  Handler:=TFindNodeHandler.Create;
  try
    if not (xfnfDoNotCollect in Flags) then
      Handler.Nodes:=@NodeList;
    Handler.Abort:=xfnfFindFirst in Flags;
    Handler.Handler:=OnIterate;
    Search(Self,false);
  finally
    Handler.Free;
  end;
end;

{ TXDBFile }

constructor TXDBFile.Create(aFilename: string);
begin
  Filename:=aFilename;
end;

destructor TXDBFile.Destroy;
begin
  inherited Destroy;
end;

function TXDBFile.GetFullFilename: string;
var
  Dir: TXDBDirectory;
begin
  if FullFilename<>'' then begin
    Result:=FullFilename;
    exit;
  end;
  Result:=Filename;
  Dir:=Directory;
  while Dir<>nil do begin
    if (Dir.Filename='') and (Dir.Directory=nil) then break;
    Result:=Dir.Filename+PathDelim+Result;
    Dir:=Dir.Directory;
  end;
  {$IFDEF Unix}
  if (Result<>'') and (Result[1]<>'/') then
    Result:='/'+Result;
  {$ENDIF}
  FullFilename:=Result;
end;

function TXDBFile.CheckDocPath(const DocPath: string; out
  ChildMightFit: boolean; MaxDepth: integer): boolean;
{ returns true if DocPath fits FullFilename
  ChildMightFit: If a child might fit the DocPath
   \ = treat next UTF-8 character as normal character
   ? = any character, but /
   * = any number of any character, but /
   /**/ = any number of any directory
   multiple // are treated as one /
   () = logical OR divided by pipe |
   the first / can be omitted
}

  function Search(MaskPos,FilePos: PChar): boolean;
  // returns true if syntax error or fits
  var
    MaskCharLen: Integer;
    FileCharLen: Integer;
    MaskStart: PChar;
    FileStart: PChar;
    Level: Integer;
  begin
    dec(MaxDepth);
    if MaxDepth<=0 then
      raise Exception.Create('TXDBFile.CheckDocPath doc path too complex, increase MaxDepth');
    Result:=false;
    while (MaskPos^='/') do inc(MaskPos);
    while (FilePos^='/') do inc(FilePos);
    repeat
      //debugln(['TXDBFile.CheckDocPath.Search MaskPos=',dbgstr(MaskPos^),' FilePos=',dbgstr(FilePos^)]);
      if MaskPos^=#0 then begin
        if FilePos^=#0 then begin
          CheckDocPath:=true;
          Result:=true;
        end;
        exit;
      end else if MaskPos^='/' then begin
        while (MaskPos^='/') do inc(MaskPos);
        if FilePos^=#0 then begin
          ChildMightFit:=true;
          if MaskPos^=#0 then begin
            CheckDocPath:=true;
            Result:=true;
          end;
          exit;
        end else if FilePos^='/' then begin
          // one directory fits
          while (FilePos^='/') do inc(FilePos);
          // check next directory ...
        end else begin
          exit; // does not fit
        end;
      end else if MaskPos^='?' then begin
        // any character except /
        if FilePos^ in ['/',#0] then exit;
        inc(MaskPos);
        inc(FilePos,UTF8CharacterLength(FilePos));
      end else if MaskPos^ in ['(','|'] then begin
        // OR
        MaskStart:=MaskPos;
        inc(MaskPos);
        // on pipe: skip other alternatives
        // on bracket open: try first alternative
        if (MaskStart^='(') and Search(MaskPos,FilePos) then exit;
        Level:=1;
        repeat
          if MaskPos^=#0 then
            exit(true) // syntax error
          else if MaskPos^='(' then begin
            inc(Level);
            inc(MaskPos);
          end else if (MaskPos^='|') and (MaskStart^='(') then begin
            // try this alternative
            if (Level=1) and Search(MaskPos+1,FilePos) then exit;
            inc(MaskPos);
          end else if MaskPos^=')' then begin
            dec(Level);
            if (Level=0) then begin
              if (MaskStart^='(') then
                exit // all alternatives failed
              else
                break;
            end;
            inc(MaskPos);
          end else if MaskPos^='\' then begin
            inc(MaskPos);
            if MaskPos^=#0 then
              exit(true); // syntax error
            inc(MaskPos,UTF8CharacterLength(MaskPos));
          end;
        until false;
        inc(MaskPos);
      end else if MaskPos^=')' then begin
        inc(MaskPos);
      end else if MaskPos^='*' then begin
        if MaskPos[1]='*' then begin
          // ** double asterisk
          if (MaskPos<>PChar(DocPath)) and (MaskPos[-1]<>'/') then begin
            // syntax error, only /** is allowed
            exit(true);
          end;
          ChildMightFit:=true;
          inc(MaskPos,2);
          if MaskPos^=#0 then begin
            // ** at end => fits anything
            CheckDocPath:=true;
            exit;
          end else if MaskPos^<>'/' then begin
            // syntax error
            exit(true);
          end;
          while MaskPos^='/' do inc(MaskPos);
          // try every directory
          repeat
            if Search(MaskPos,FilePos) then exit(true);
            if FilePos^=#0 then exit;
            while not (FilePos^ in [#0,'/']) do inc(FilePos);
            while FilePos^='/' do inc(FilePos);
          until false;
        end else begin
          // * single asterisk
          inc(MaskPos);
          while not (FilePos^ in [#0,'/']) do begin
            if Search(MaskPos,FilePos) then exit;
            inc(FilePos,UTF8CharacterLength(FilePos));
          end;
        end;
      end else if MaskPos^='\' then begin
        // treat next special character as normal character
        inc(MaskPos);
        if MaskPos^=#0 then
          exit(true); // invalid syntax
        if FilePos^ in ['/',#0] then exit; // does not fit
        MaskCharLen:=UTF8CharacterLength(MaskPos);
        FileCharLen:=UTF8CharacterLength(FilePos);
        if CompareFilenames(MaskPos,MaskCharLen,FilePos,FileCharLen)<>0 then
          exit; // does not fit
        inc(MaskPos,MaskCharLen);
        inc(FilePos,FileCharLen);
      end else begin
        // normal characters
        if FilePos^ in ['/',#0] then exit;
        MaskStart:=MaskPos;
        FileStart:=FilePos;
        while (not (MaskPos^ in [#0,'/','?','*','\','(',')','|']))
        and (not (FilePos^ in [#0,'/'])) do begin
          inc(MaskPos,UTF8CharacterLength(MaskPos));
          inc(FilePos,UTF8CharacterLength(FilePos));
        end;
        if CompareFilenames(MaskStart,MaskPos-MaskStart,FileStart,FilePos-FileStart)<>0
        then
          exit; // does not fit
      end;
    until false;
  end;

begin
  //debugln(['TXDBFile.CheckDocPath GetFullFilename=',GetFullFilename]);
  Result:=false;
  ChildMightFit:=false;
  if (DocPath='') then exit;
  if (Filename='') and (Directory=nil) then begin
    // root directory
    Result:=false;
    ChildMightFit:=true;
  end else begin
    GetFullFilename;
    Search(PChar(DocPath),PChar(FullFilename));
    if Result then
      ChildMightFit:=true;
  end;
end;

end.

