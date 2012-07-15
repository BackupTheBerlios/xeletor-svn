unit XDBCGI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HTTPDefs, LazFileUtils;

function QueryParams: TStrings;
function GetQueryParam(const Name: string): string;

function GetRightFilePath(Filename: string; Index: integer = 1): string;
function ChompFilePaths(const Filename: string; DirCount: integer = 1): string;
function NormalizeSpace(const s: string; TrimStart: boolean = true;
  TrimEnd: boolean = true): string;

implementation

var
  FQueryParams: TStringList = nil;

procedure ParseQueryString;
var
  query: String;
  p: Integer;
  StartPos: Integer;
begin
  if FQueryParams<>nil then exit;
  FQueryParams:=TStringList.Create;
  query:=GetEnvironmentVariable('QUERY_STRING');
  if query='' then exit;
  p:=1;
  while p<=length(query) do begin
    while (p<=length(query)) and (query[p]='&') do inc(p);
    StartPos:=p;
    while (p<=length(query)) and (query[p]<>'&') do inc(p);
    if p>StartPos then
      FQueryParams.Add(HTTPDecode(copy(query,StartPos,p-StartPos)));
  end;
end;

function QueryParams: TStrings;
begin
  if FQueryParams=nil then
    ParseQueryString;
  Result:=FQueryParams;
end;

function GetQueryParam(const Name: string): string;
begin
  Result:=QueryParams.Values[Name];
end;

function GetRightFilePath(Filename: string; Index: integer = 1): string;
{ returns the Index-th directory at the end, the last part aka file name is Index 1
  for example: /docpath/file
    file is Index 1, docpath is Index 2
}
begin
  Result:=ExtractFileName(ChompFilePaths(Filename,Index-1));
end;

function ChompFilePaths(const Filename: string; DirCount: integer): string;
begin
  Result:=Filename;
  while DirCount>=1 do begin
    Result:=ChompPathDelim(ExtractFilePath(Result));
    dec(DirCount);
  end;
end;

function NormalizeSpace(const s: string; TrimStart: boolean = true;
  TrimEnd: boolean = true): string;
var
  p: Integer;
  StartPos: Integer;
begin
  Result:=s;
  p:=1;
  while p<=length(Result) do begin
    if Result[p] in [' ',#9,#10,#13] then begin
      StartPos:=p;
      while (p<=length(Result)) and (Result[p] in [' ',#9,#10,#13]) do inc(p);
      Result:=copy(Result,1,StartPos-1)+' '+copy(Result,p,length(Result));
    end else begin
      inc(p);
    end;
  end;
  if TrimStart then begin
    if TrimEnd then
      Result:=Trim(Result)
    else
      Result:=TrimLeft(Result);
  end else begin
    if TrimEnd then
      Result:=TrimRight(Result);
  end;
end;

finalization
  FreeAndNil(FQueryParams);

end.

