{ Copyright (C) 2013  Mattias Gaertner  mattias@freepascal.org

  Abstract:
    Functions to export TXDBNodes to JSON.
}
unit XDBExport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, XDBFiles;

(* SaveNodeAsJSON:
  elements are streamed by name
  attributes are prepended with '-': -name
  text as #text<n>

  For example:
    <element attr1="attrvalue">
      <sub attr2="1">
        Line1<br/>
        Line2
      </sub>
      <leaf>Text</leaf>
    </element>

    {
      "element" : {
        "-attr1" : "attrvalue",
        "sub" : {
          "-attr2" : "1",
          "#text1" : "\n    Line1",
          "br" : {},
          "#text2" : "\n    Line2\n  "
        }
        "leaf" : {
          "#text1" : "Text"
        }
      }
    }

  Special characters:
    #0: skipped, because invalid in XML
    '\' : '\\'
    '/' : '\/'
    '"' : '\"'
    #8  : '\b'
    #9  : '\t'
    #10 : '\n'
    #12 : '\f'
    #13 : '\r'
*)
procedure SaveNodeAsJSON(Node: TXDBNode; aStream: TStream; Indent: integer = 0);
function NodeAsJSON(Node: TXDBNode; Indent: integer = 0): string;
procedure WriteJSONString(const Value: string; aStream: TStream);

implementation

procedure SaveNodeAsJSON(Node: TXDBNode; aStream: TStream; Indent: integer);

  procedure WriteIndent;
  var
    i: Integer;
  begin
    for i:=1 to Indent do
      aStream.WriteByte(ord(' '));
  end;

  procedure WriteStr(const s: string);
  begin
    if s='' then exit;
    aStream.Write(s[1],length(s));
  end;

var
  Attr: PXDBAttribute;
  NeedComma: Boolean;
  Child: PXDBNode;
  i: Integer;
  TextID: Integer;
begin
  if Node is TXDBLeafNode then begin
    WriteIndent;
    WriteStr('#text : ');
    WriteJSONString(TXDBLeafNode(Node).Value,aStream);
    exit;
  end;

  // write header: "elementname" : {
  WriteIndent;
  WriteJSONString(Node.GetName,aStream);
  WriteStr(' : {');
  inc(Indent,2);

  NeedComma:=false;
  if Node is TXDBNodeWithAttributes then begin
    // write attributes
    Attr:=TXDBNodeWithAttributes(Node).Attributes;
    for i:=1 to TXDBNodeWithAttributes(Node).AttributeCount do begin
      if NeedComma then
        aStream.WriteByte(ord(','));
      aStream.WriteByte(10);
      WriteIndent;
      WriteJSONString('-'+Attr^.Name,aStream);
      WriteStr(' : ');
      WriteJSONString('-'+Attr^.Value,aStream);
      NeedComma:=true;
      inc(Attr);
    end;
  end;

  // write children
  if Node is TXDBTreeNode then begin
    Child:=TXDBTreeNode(Node).Children;
    TextID:=0;
    for i:=1 to TXDBTreeNode(Node).ChildCount do begin
      if NeedComma then
        aStream.WriteByte(ord(','));
      aStream.WriteByte(10);
      if Child^ is TXDBLeafNode then begin
        inc(TextID);
        WriteIndent;
        WriteStr('"#text'+IntToStr(TextID)+'" : ');
        WriteJSONString(TXDBLeafNode(Child^).Value,aStream);
      end else begin
        SaveNodeAsJSON(Child^,aStream,Indent+2);
      end;
      NeedComma:=true;
      inc(Child);
    end;
  end;

  // write footer: }
  if NeedComma then begin
    aStream.WriteByte(10);
    dec(Indent,2);
    WriteIndent;
  end;
  aStream.WriteByte(ord('}'));
end;

function NodeAsJSON(Node: TXDBNode; Indent: integer): string;
var
  ms: TMemoryStream;
begin
  ms:=TMemoryStream.Create;
  try
    SaveNodeAsJSON(Node,ms,Indent);
    SetLength(Result,ms.Size);
    if Result<>'' then
      System.Move(ms.Memory^,Result[1],length(Result));
  finally
    ms.Free;
  end;
end;

procedure WriteJSONString(const Value: string; aStream: TStream);
// write "Value" and escape special characters
var
  FlushedPos, p: PChar;

  procedure Flush;
  begin
    if FlushedPos=p then exit;
    aStream.Write(FlushedPos^,p-FlushedPos);
    FlushedPos:=p;
  end;

  procedure WriteEscape(c: char);
  begin
    Flush;
    aStream.WriteByte(ord('\'));
    aStream.WriteByte(ord(c));
    inc(FlushedPos);
  end;

begin
  aStream.WriteByte(ord('"'));
  if Value<>'' then
  begin
    p:=PChar(Value);
    FlushedPos:=p;
    while true do begin
      case p^ of
      #0:
        if p-PChar(Value)=length(Value) then
          break;
      '\' : WriteEscape('\');
      '/' : WriteEscape('/');
      '"' : WriteEscape('"');
      #8  : WriteEscape('b');
      #9  : WriteEscape('t');
      #10 : WriteEscape('n');
      #12 : WriteEscape('f');
      #13 : WriteEscape('r');
      end;
      inc(p);
    end;
    Flush;
  end;
  aStream.WriteByte(ord('"'));
end;

end.

