{ Copyright (C) 2013  Mattias Gaertner  mattias@freepascal.org

  Abstract:
    Functions to export TXDBNodes to JSON.
}
unit XDBExport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, XDBFiles, LazLogger;

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
procedure SaveNodeAsJSON(Node: TXDBNode; aStream: TStream; Indent: integer = 0;
  WithEnvelope: boolean = true);
function NodeAsJSON(Node: TXDBNode; Indent: integer = 0;
  WithEnvelope: boolean = true): string;
procedure WriteJSONString(const Value: string; aStream: TStream);
function ValueAsJSONString(const Value: string): string;

implementation

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

function ValueAsJSONString(const Value: string): string;
var
  i: Integer;
  ms: TMemoryStream;
begin
  if length(Value)>100 then begin
    ms:=TMemoryStream.Create;
    try
      WriteJSONString(Value,ms);
      SetLength(Result,ms.Size);
      if Result='' then exit;
      Move(ms.Memory^,Result[1],ms.Size);
    finally
      ms.Free;
    end;
  end else begin
    Result:=Value;
    // encode
    for i:=length(Result) downto 1 do begin
      case Result[i] of
      #0: Delete(Result,i,1);
      '\','/','"' : Insert('/',Result,i);
      #8  : ReplaceSubstring(Result,i,1,'\b');
      #9  : ReplaceSubstring(Result,i,1,'\t');
      #10 : ReplaceSubstring(Result,i,1,'\n');
      #12 : ReplaceSubstring(Result,i,1,'\f');
      #13 : ReplaceSubstring(Result,i,1,'\r');
      end;
    end;
    Result:='"'+Result+'"';
  end;
end;

procedure SaveNodeAsJSON(Node: TXDBNode; aStream: TStream; Indent: integer;
  WithEnvelope: boolean);

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
  Other: PXDBNode;
  j: Integer;
  ChildCount: Integer;
  Children: PXDBNode;
begin
  if Node is TXDBLeafNode then begin
    if WithEnvelope then begin
      WriteIndent;
      WriteStr('#text : ');
    end;
    WriteJSONString(TXDBLeafNode(Node).Value,aStream);
    exit;
  end;

  if WithEnvelope then begin
    // write header: "elementname" : {
    WriteIndent;
    WriteJSONString(Node.GetName,aStream);
    WriteStr(' : {');
    inc(Indent,2);
  end;

  NeedComma:=false;
  if Node is TXDBNodeWithAttributes then begin
    // write attributes
    Attr:=TXDBNodeWithAttributes(Node).Attributes;
    for i:=1 to TXDBNodeWithAttributes(Node).AttributeCount do begin
      if NeedComma then
        aStream.WriteByte(ord(','));
      if NeedComma or WithEnvelope then
        aStream.WriteByte(10);
      WriteIndent;
      WriteJSONString('-'+Attr^.Name,aStream);
      WriteStr(' : ');
      WriteJSONString(Attr^.Value,aStream);
      NeedComma:=true;
      inc(Attr);
    end;
  end;

  // write children
  if Node is TXDBTreeNode then begin
    ChildCount:=TXDBTreeNode(Node).ChildCount;
    i:=ChildCount*SizeOf(TXDBNode);
    GetMem(Children,i);
    try
      Move(TXDBTreeNode(Node).Children^,Children^,i);
      Child:=Children;
      TextID:=0;
      for i:=1 to ChildCount do begin
        if Child^=nil then begin
        end else if Child^ is TXDBLeafNode then begin
          // a new text element => write comma, linebreak
          if NeedComma then
            aStream.WriteByte(ord(','));
          if NeedComma or WithEnvelope then
            aStream.WriteByte(10);
          // write "#text<n>" : "text"
          inc(TextID);
          WriteIndent;
          WriteStr('"#text'+IntToStr(TextID)+'" : ');
          WriteJSONString(TXDBLeafNode(Child^).Value,aStream);
        end else begin
          // a new element => write comma, linebreak
          if NeedComma then
            aStream.WriteByte(ord(','));
          if NeedComma or WithEnvelope then
            aStream.WriteByte(10);

          // check if there is another element with this name
          j:=i;
          Other:=Child;
          repeat
            inc(j);
            if j>ChildCount then break;
            inc(Other);
          until (Other^<>nil) and (Other^.GetName=Child^.GetName);
          if j<=ChildCount then begin
            // multiple elements with this name => write as array
            WriteIndent;
            WriteJSONString(Child^.GetName,aStream);
            WriteStr(' : [');
            inc(Indent,2);
            Other:=Child;
            for j:=i to ChildCount do begin
              if (Other^<>nil) and (Other^.GetName=Child^.GetName) then begin
                if j>i then
                  aStream.WriteByte(ord(','));
                aStream.WriteByte(10);
                WriteIndent;
                WriteStr('{'#10);
                SaveNodeAsJSON(Other^,aStream,Indent+2,false);
                if Other<>Child then
                  Other^:=nil; // mark as nil to skip
                aStream.WriteByte(10);
                WriteIndent;
                WriteStr('}');
              end;
              inc(Other);
            end;
            dec(Indent,2);
            aStream.WriteByte(10);
            WriteIndent;
            WriteStr(']');
          end else begin
            // unique element name => write as single object
            SaveNodeAsJSON(Child^,aStream,Indent,true);
          end;
        end;
        NeedComma:=true;
        inc(Child);
      end;
    finally
      FreeMem(Children);
    end;
  end;

  if WithEnvelope then begin
    // write footer: }
    if NeedComma then begin
      aStream.WriteByte(10);
      dec(Indent,2);
      WriteIndent;
    end;
    aStream.WriteByte(ord('}'));
  end;
end;

function NodeAsJSON(Node: TXDBNode; Indent: integer = 0;
  WithEnvelope: boolean = true): string;
var
  ms: TMemoryStream;
begin
  ms:=TMemoryStream.Create;
  try
    SaveNodeAsJSON(Node,ms,Indent,WithEnvelope);
    SetLength(Result,ms.Size);
    if Result<>'' then
      System.Move(ms.Memory^,Result[1],length(Result));
  finally
    ms.Free;
  end;
end;

end.

