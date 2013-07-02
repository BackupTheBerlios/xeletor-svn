{ Daemon to search xml files via xpath expressions.

  Copyright (C) 2011  Mattias Gaertner  mattias@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
{
  ToDo:
    linux init.d script
    HUP signal
    Update xml directory:
      inotify
      directory change notifier
      - register directory change handler (Linux: inotify)
    real XPath
    Index:
      simple: node path + sort for attributes
    laz2_XMLRead:
      improve error message: "Unmatching element end tag" show start tag
}
program xeletor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, BaseUnix,
  {$ENDIF}
  //heaptrc,
  Classes, SysUtils, Sockets, fgl, CustApp, AVL_Tree, laz2_XMLRead, laz2_DOM,
  CodeToolManager, FileProcs, MTProcs, xdbhttpserver, xdbfphttpserver,
  xdbHTTPDefs, CodeToolsStructs, xdbutils, xdbfiles, xdblog, xdbcentral;

const
  Version = '1.1';
type

  { TAllowDenyFromItem }

  TAllowDenyFromItem = class
  public
    Allow: boolean; // false = Deny
    IP: in_addr;
    Net: in_addr;
    constructor Create(AnAllow: boolean; const anIP, aNet: in_addr);
  end;
  TBaseAllowDenyFromList = specialize TFPGList<TAllowDenyFromItem>;

  { TAllowDenyFromList }

  TAllowDenyFromList = class(TBaseAllowDenyFromList)
  public
    function Add(Allow: boolean; const IP, Net: in_addr): TAllowDenyFromItem; overload;
    function Add(Allow: boolean; const Subnet: string): TAllowDenyFromItem; overload;
    function IsAllowed(IP: in_addr): boolean;
  end;

  { TXeletorApplication }

  TXeletorApplication = class(TCustomApplication)
  private
    FSingleThreaded: boolean;
    procedure ErrorRespond(ARequest: TFPHTTPConnectionRequest;
      AResponse: TFPHTTPConnectionResponse; Code: integer;
      Msg: string);
    function GetCaption: string;
    procedure SendGreeting(var AResponse: TFPHTTPConnectionResponse);
    procedure ServerAllowConnect(Sender: TObject; {%H-}ASocket: Longint;
      var Allow: Boolean);
    procedure ServerRequest(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure HandleRequestDoc(Path: string;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure HandleRequestListDocs(Path: string;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse;
      ExtendedFormat: boolean);
    procedure HandleRequestFindDocs(Path: string;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure HandleRequestFindNodes(Path: string;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure HandleRequestRescan(Path: string;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure HandleRequestListErrors(Path: string;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure HandleRequestStop(Path: string;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    // Tests
    procedure TestRead1;
    procedure TestListErrors;
    procedure TestMultiReadExclusiveWrite;
    procedure TestStrToIP(Quiet: boolean = true);
    procedure TestAllowDeny(Quiet: boolean = true);
    procedure TestFindNodes(const NodePath: string);
    procedure TestForAllGrandChildren(const NodePath: string);
    procedure TestFindNodesViaNodePath(const NodePath: string);
    procedure UpdateRootDirectories;
  protected
    // parameters, configs
    fParams: TStringList;
    FDBDirParams: TXDBStringToStringTree;
    procedure ParamError(const Msg: string);
    function GetParams(Index: Integer): String; override;
    function GetParamCount: Integer; override; // returns maximum index
    procedure InsertOptionsFile(Filename: string; ParamPos: integer);
    procedure ReadConfig;
  protected
    Storage: TXDBStorage;
    Port: Integer;
    AllowDeny: TAllowDenyFromList;
    FileScanner: TXDBScanThread;
    FileRescanNeeded: boolean;
    RescanIntervalInSec: integer;
    LastRescan: TDateTime;
    procedure DoRun; override;
    function GetAllowDenyFromParams(FatalErrors: boolean): TAllowDenyFromList;

    // scan files
    procedure RescanFiles;
    procedure WaitTillFilesScanned;
    function GetRootDirectoriesFromParams(FatalErrors: boolean): TXDBStringToStringTree;
    procedure FileScannerExecuted(Sender: TObject);
  public
    // log
    fLog: TXDBLog;
    procedure DoLog(EventType: TEventType; const Msg: String); override;
    procedure ClientLog(EventType: TEventType; Request: TFPHTTPConnectionRequest;
        Msg: array of const); overload;
    procedure ClientLog(EventType: TEventType; Connection : TFPHTTPConnection;
        Msg: array of const); overload;
    procedure ClientLog(EventType: TEventType; const SockAddr: TSockAddr;
        Msg: array of const); overload;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp(WithHeader: boolean); virtual;
    procedure Terminate; override;

    procedure ReloadConfiguration;

    property SingleThreaded: boolean read FSingleThreaded write FSingleThreaded;
  end;

var
  Application: TXeletorApplication;

{$IFDEF Unix}
procedure SignalCapture(Signal: longint); cdecl;
begin
  case signal of
    SIGHUP:
      begin
        Application.Log(etInfo,'Signal HUP');
        Application.ReloadConfiguration;
      end;
    SIGINT:
      begin
        Application.Log(etInfo,'Signal INT');
        Application.Terminate;
      end;
    SIGQUIT:
      begin
        Application.Log(etInfo,'Signal QUIT');
        Application.Terminate;
      end;
    SIGKILL:
      begin
        Application.Log(etInfo,'Signal KILL');
        Application.Terminate;
      end;
    SIGTerm:
      begin
        Application.Log(etInfo,'Signal TERM');
        Application.Terminate;
      end;
    else
      Application.Log(etInfo,'Unknown signal: '+IntToStr(Signal));
  end;
end;

procedure AddTrap(Signal: longint);
var
  NewAction, OldAction: PSigActionRec;
begin
  New(NewAction);
  New(OldAction);

  NewAction^.sa_Handler := SigActionHandler(@SignalCapture);
  FillByte(NewAction^.Sa_Mask,SizeOf(NewAction^.sa_mask),0);
  NewAction^.Sa_Flags := 0;
  {$IFDEF Linux} // Linux specific
    NewAction^.Sa_Restorer := Nil;
  {$ENDIF}
  if FPSigaction(Signal, NewAction, OldAction) <> 0 then begin
    if Signal <> SIGKILL then begin
      Application.Log(etError,'unable to trap signal: '+IntToStr(fpgeterrno));
      halt(1);
    end;
  end;

  Dispose(NewAction);
  Dispose(OldAction);
end;

procedure TrapSignals;
begin
  AddTrap(SIGHUP);
  AddTrap(SIGINT);
  AddTrap(SIGQUIT);
  AddTrap(SIGTerm);
  AddTrap(SIGKILL);
end;
{$ENDIF}

constructor TAllowDenyFromItem.Create(AnAllow: boolean; const anIP,
  aNet: in_addr);
begin
  Allow:=AnAllow;
  IP:=anIP;
  Net:=aNet;
end;

{ TAllowDenyFromList }

function TAllowDenyFromList.Add(Allow: boolean; const IP, Net: in_addr
  ): TAllowDenyFromItem;
begin
  Result:=TAllowDenyFromItem.Create(Allow,IP,Net);
  inherited Add(Result);
end;

function TAllowDenyFromList.Add(Allow: boolean; const Subnet: string
  ): TAllowDenyFromItem;
var
  IP: in_addr;
  Net: in_addr;
begin
  StrToSubNet(Subnet,IP,Net);
  //debugln(['TAllowDenyFromList.Add SubNet=',SubNet,' IP=',dbgs(IP),'/',dbgs(Net)]);
  Result:=TAllowDenyFromItem.Create(Allow,IP,Net);
  inherited Add(Result);
end;

function TAllowDenyFromList.IsAllowed(IP: in_addr): boolean;
var
  Item: TAllowDenyFromItem;
begin
  if Count=0 then exit(true);
  Result:=false;
  for Item in Self do begin
    //debugln(['TAllowDenyFromList.IsAllowed IP=',dbgs(IP),' Item.IP=',dbgs(Item.IP),'/',dbgs(Item.Net),' Item.Allow=',Item.Allow]);
    if (IP.s_addr and Item.Net.s_addr)<>(Item.IP.s_addr and Item.Net.s_addr) then
      continue;
    //debugln(['TAllowDenyFromList.IsAllowed FITS IP=',dbgs(IP),' Item.IP=',dbgs(Item.IP),'/',dbgs(Item.Net),' Item.Allow=',Item.Allow]);
    if Item.Allow then
      Result:=true
    else
      exit(false);
  end;
end;

{ TXeletorApplication }

procedure TXeletorApplication.DoRun;
var
  i: Integer;
  Server: TXDBHttpServer;
begin
  ReadConfig;

  fLog.LogFile:=GetOptionValue('l','logfile');
  Log(etInfo,'starting '+GetCaption+' ...');
  if fLog.Verbose then begin
    for i:=0 to ParamCount do
      Log(etInfo,'Parameter: '+GetParams(i));
  end;

  SingleThreaded:=HasOption('singlethreaded');
  if SingleThreaded then
    ProcThreadPool.MaxThreadCount:=1;
  if HasOption('p','port') then begin
    Port:=StrToIntDef(GetOptionValue('p','port'),-1);
    if (Port<1) or (Port>65535) then
      ParamError('invalid port '+GetOptionValue('p','port'));
  end;
  RescanIntervalInSec:=StrToIntDef(GetOptionValue('rescaninterval'),0);
  fLog.WatchDogIntervalInSec:=StrToIntDef(GetOptionValue('watchdoginterval'),
                                                    fLog.WatchDogIntervalInSec);

  if SingleThreaded then
    Log(etInfo,'singlethreaded');
  Log(etInfo,'WatchDogIntervalInSec='+IntToStr(fLog.WatchDogIntervalInSec));

  AllowDeny:=GetAllowDenyFromParams(true);

  TestStrToIP;
  TestAllowDeny;
  //TestMultiReadExclusiveWrite;
  //Halt;

  // read all xml files
  FDBDirParams:=GetRootDirectoriesFromParams(true);
  UpdateRootDirectories;

  RescanFiles;

  // test
  //WaitTillFilesScanned;
  //TestListErrors;
  //TestRead1;
  //for i:=0 to 1000 do Sleep(1000);
  //TestFindNodes('doc(darems/manuscriptsWithoutScans/arab.MSS)//msIdentifier');
  //TestFindNodes('doc(daretexts)//(bibl|titleStmt)');
  //TestFindNodes('doc(daretexts)//titleStmt[@xml:id=''FT1'']');
  //TestFindNodes('doc(darems/manuscriptsWithScans/BOOK-DARE-M-VA-VAT-BAV-Urb.Lat.221/structure.xml)TEI/facsimile/surface[352]');
  //TestFindNodes('doc(darems/**/BOOK-DARE-H-AT-AD-STB-Ms480-M)//');
  //TestFindNodes('doc(darems)//surface[desc/item/link/@target=''FT103'']');
  //TestFindNodes('doc(darems)//surface[desc/item/link/@corresp=''3'']');
  //TestFindNodes('//author[0]');
  //TestFindNodesViaNodePath('//author');
  //TestForAllGrandChildren('//fileDesc');

  Log(etInfo,'starting on port '+IntToStr(Port));
  //debugln(['TXeletorApplication.DoRun ',fLog.Verbose]);
  fLog.FlushStdOutLog;
  Server:=TXDBHttpServer.Create(nil);
  try
    Server.Log:=fLog;
    Server.Port:=Port;
    Server.Threaded:=not SingleThreaded;
    Server.OnAllowConnect:=@ServerAllowConnect;
    Server.OnRequest:=@ServerRequest;
    Server.ActivateViaThread;
    {$IFDEF Unix}
    TrapSignals;
    {$ENDIF}
    LastRescan:=Now;
    repeat
      Sleep(50);
      fLog.FlushStdOutLog;
      if Server.ErrorMsg<>'' then break;
      fLog.WatchDog;
      if (RescanIntervalInSec>0)
      and (Abs(Now-LastRescan)*86400>=RescanIntervalInSec) then begin
        if (FileScanner=nil) then
          RescanFiles;
      end;
    until Terminated;
  finally
    Server.Free;
  end;
  // stop program loop
  Terminate;

  WaitTillFilesScanned;
end;

function TXeletorApplication.GetAllowDenyFromParams(FatalErrors: boolean
  ): TAllowDenyFromList;
const
  AllowFromOpt = '--allow-from=';
  DenyFromOpt = '--deny-from=';
var
  i: Integer;
  p: String;
  SubNet: String;
  Allow: Boolean;
  ErrMsg: String;
begin
  Result:=TAllowDenyFromList.Create;
  i:=0;
  while i<ParamCount do begin
    inc(i);
    p:=GetParams(i);
    SubNet:=#0;
    Allow:=true;
    if copy(p,1,length(AllowFromOpt))=AllowFromOpt then begin
      SubNet:=copy(p,length(AllowFromOpt)+1,length(p));
    end else if copy(p,1,length(DenyFromOpt))=DenyFromOpt then begin
      Allow:=false;
      SubNet:=copy(p,length(DenyFromOpt)+1,length(p));
    end;
    if SubNet=#0 then continue;
    ErrMsg:='';
    try
      Result.Add(Allow,SubNet);
    except
      on E: Exception do
        ErrMsg:=E.Message;
    end;
    if (ErrMsg<>'') and FatalErrors then
      ParamError('invalid param '+p+': '+ErrMsg);
  end;
end;

procedure TXeletorApplication.RescanFiles;
begin
  Storage.BeginWriting;
  try
    if FileScanner<>nil then begin
      FileRescanNeeded:=true;
      exit;
    end;
    FileRescanNeeded:=false;
    FileScanner:=TXDBScanThread.Create(true);
    FileScanner.Storage:=Storage;
    FileScanner.OnExecuted:=@FileScannerExecuted;
    FileScanner.FreeOnTerminate:=true;
    FileScanner.Start;
  finally
    Storage.EndWriting;
  end;
end;

procedure TXeletorApplication.WaitTillFilesScanned;
begin
  while FileScanner<>nil do
    Sleep(10);
end;

function TXeletorApplication.GetRootDirectoriesFromParams(FatalErrors: boolean
  ): TXDBStringToStringTree;
const
  DBDirOpt = '--dbdir=';
var
  i: Integer;
  p: String;
  DBDir: string;
  t: SizeInt;
  DBDirName: String;
begin
  Result:=TXDBStringToStringTree.Create(false);

  if not HasOption('d','dbdir') then begin
    if FatalErrors then begin
      writeln('ERROR: Missing option -d or --dbdir');
      writeln('       Option -h for help.');
      Terminate;
      Halt;
    end;
    Log(etError,'ERROR: Missing option -d or --dbdir');
  end;

  i:=0;
  while i<ParamCount do begin
    inc(i);
    p:=GetParams(i);
    DBDir:=#0;
    if p='-d' then begin
      inc(i);
      DBDir:=GetParams(i);
    end else if copy(p,1,length(DBDirOpt))=DBDirOpt then begin
      DBDir:=copy(p,length(DBDirOpt)+1,length(p));
    end;
    if DBDir<>#0 then begin
      t:=System.Pos('=',DBDir);
      if t<1 then begin
        if FatalErrors then
          ParamError('dbdir must start with a name: '+DBDir)
        else
          Log(etError,'dbdir must start with a name: '+DBDir);
        continue;
      end;
      DBDirName:=copy(DBDir,1,t-1);
      DBDir:=copy(DBDir,t+1,length(DBDir));
      if (DBDirName='') or (not IsValidIdent(DBDirName)) then begin
        if FatalErrors then
          ParamError('invalid dbdir name: '+DBDirName)
        else
          Log(etError,'invalid dbdir name: '+DBDirName);
        continue;
      end;
      if Result.Contains(DBDirName) then begin
        if FatalErrors then begin
          writeln('ERROR: duplicate db directory ',DBDirName);
          Terminate;
          Halt;
        end else begin
          Log(etError,'duplicate db directory '+DBDirName);
          continue;
        end;
      end;
      DBDir:=TrimAndExpandDirectory(DBDir);
      if not DirPathExists(DBDir) then begin
        if FatalErrors then
          ParamError('dbdir not found: '+DBDir)
        else
          Log(etError,'dbdir not found: '+DBDir);
        continue;
      end;
      Result[DBDirName]:=DBDir;
    end;
  end;
end;

procedure TXeletorApplication.FileScannerExecuted(Sender: TObject);
begin
  Storage.BeginWriting;
  try
    FileScanner:=nil;
    if FileRescanNeeded then
      RescanFiles;
    LastRescan:=Now;
  finally
    Storage.EndWriting;
  end;
end;

procedure TXeletorApplication.ServerAllowConnect(Sender: TObject; ASocket: Longint;
  var Allow: Boolean);
var
  len: Integer;
  SockAddr: TSockAddr;
begin
  Allow:=false;
  len := SizeOf(TSockAddr);
  if fpGetPeerName(ASocket, @SockAddr, @len) <> 0 then
    FillChar(SockAddr, SizeOf(SockAddr), 0);
  //debugln(['TXeletorApplication.ServerAllowConnect ',dbgs(SockAddr.sin_addr)]);
  Allow:=AllowDeny.IsAllowed(SockAddr.sin_addr);
  if not Allow then
    ClientLog(etInfo,SockAddr,['address access denied']);
end;

procedure TXeletorApplication.ErrorRespond(ARequest: TFPHTTPConnectionRequest;
  AResponse: TFPHTTPConnectionResponse; Code: integer; Msg: string);
var
  ss: TStringStream;
begin
  ClientLog(etError,ARequest,[Code,'-',Msg]);
  //AResponse.ContentType:='text/html';
  ss:=TStringStream.Create('<HTML><BODY>'
    +'<H1>Error: '+dbgs(Code)+'-'+Msg+'</H1>'
    +'<H3>'+GetCaption+'</H3></BODY></HTML>');
  try
    AResponse.ContentType:='text/html';
    AResponse.Code:=Code;
    AResponse.ContentLength:=ss.Size;
    AResponse.ContentStream:=ss;
    AResponse.SendContent;
    AResponse.ContentStream:=nil;
  finally
    ss.Free;
  end;
end;

function TXeletorApplication.GetCaption: string;
begin
  Result:='Xeletor version '+Version;
end;

procedure TXeletorApplication.SendGreeting(var AResponse: TFPHTTPConnectionResponse);
var
  ss: TStringStream;
begin
  ss:=TStringStream.Create('<HTML><BODY>'#13#10
    +'<H1>'+GetCaption+' is running.</H1>'#13#10
    +'<H3>Usage:</H3>'#13#10
    +'<ul>'#13#10

    +'  <li>doc?dbname/filepath<br>'#13#10
    +'Return the xml document specified by database name and path.<br>'#13#10
    +'Example: doc?NameOfYourDB/dir/test.xml'#13#10
    +'  </li>'#13#10

    +'  <li>listdocs?DocPath<br>'#13#10
    +'Return an xml document with the list of files matching the DocPath.<br>'#13#10
    +'DocPath:<br>'#13#10
    +'<ul>'#13#10
    +'  <li>? = any character, but /</li>'#13#10
    +'  <li>* = any number of any character, but /</li>'#13#10
    +'  <li>/**/ = any number of any directory</li>'#13#10
    +'  <li>multiple // are treated as one /</li>'#13#10
    +'  <li>() = logical OR divided by pipe |</li>'#13#10
    +'  <li>\ = treat next UTF-8 character as normal character</li>'#13#10
    +'  <li>the first / can be omitted</li>'#13#10
    +'</ul>'#13#10
    +'Example: listdocs?**/*.xml  to list all xml files'#13#10
    +'  </li>'#13#10

    +'  <li>listdocsext?DocPath<br>'#13#10
    +'As listdocs but with extra attributes like fileage.<br>'#13#10
    +'Example: listdocsext:db1/*.xml'#13#10
    +'  </li>'#13#10

    +'  <li>finddocs?doc(DocPath)XPath<br>'#13#10
    +'Return an xml document with all files matching the path.<br>'#13#10
    +'The XPath of the first matching node is returned as well.<br>'#13#10
    +'Optionally the path can be prepended with a doc(DocPath) specifying a'
    +' DocPath for the directories/files.<br>'#13#10
    +'Example: finddocs:doc(db1)//graphics'#13#10
    +'  </li>'#13#10

    +'  <li>findnodes?doc(DocPath)XPath<br>'#13#10
    +'Return an xml document with all nodes matching the path.<br>'#13#10
    +'Optionally the path can be prepended with a doc(DocPath) specifying a'
    +' DocPath for the directories/files.<br>'#13#10
    +'Example: finddocs:doc(db1)//graphics'#13#10
    +'  </li>'#13#10

    +'  <li>rescan<br>'#13#10
    +'Tell the server to rescan all directories<br>'#13#10
    +'Example: rescan'#13#10
    +'  </li>'#13#10

    +'  <li>listerrors<br>'#13#10
    +'List all files with load/syntax errors<br>'#13#10
    +'Example: listerrors'#13#10
    +'  </li>'#13#10

    +'  <li>stop<br>'#13#10
    +'Shut down Xeletor gracefully'#13#10
    +'  </li>'#13#10

    +'</ul>'#13#10
    +'</BODY></HTML>'#13#10);
  try
    AResponse.ContentType:='text/html';
    AResponse.ContentLength:=ss.Size;
    AResponse.ContentStream:=ss;
    AResponse.SendContent;
    AResponse.ContentStream:=nil;
  finally
    ss.Free;
  end;
end;

procedure TXeletorApplication.ServerRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
{ Called whenever a client created a connection
}
var
  URL : String;
  p: SizeInt;
  Scheme: String;
begin
  //debugln(['TXeletorApplication.ServerRequest Command=',ARequest.Command,' CommandLine=',ARequest.CommandLine,' URI=',ARequest.URI,' QueryString=',ARequest.QueryString]);
  URL:=ARequest.Url;
  ClientLog(etInfo,ARequest,['URL: '+URL]);

  // check for test connection and send greeting and help
  if (URL='/') or (URL='') then begin
    SendGreeting(AResponse);
    exit;
  end;
  if URL[1]='/' then System.Delete(URL,1,1);

  // check scheme
  p:=Pos('?',URL);
  Scheme:=lowercase(copy(URL,1,p-1));
  URL:=copy(URL,p+1,length(URL));
  URL:=HTTPDecode(URL);
  if Scheme='doc' then begin
    HandleRequestDoc(URL,ARequest,AResponse);
  end else if Scheme='listdocs' then begin
    HandleRequestListDocs(URL,ARequest,AResponse,false);
  end else if Scheme='listdocsext' then begin
    HandleRequestListDocs(URL,ARequest,AResponse,true);
  end else if Scheme='finddocs' then begin
    HandleRequestFindDocs(URL,ARequest,AResponse);
  end else if Scheme='findnodes' then begin
    HandleRequestFindNodes(URL,ARequest,AResponse);
  end else if (Scheme='') and (URL='rescan') then begin
    HandleRequestRescan(URL,ARequest,AResponse);
  end else if (Scheme='') and (URL='listerrors') then begin
    HandleRequestListErrors(URL,ARequest,AResponse);
  end else if (Scheme='') and (URL='stop') then begin
    HandleRequestStop(URL,ARequest,AResponse);
  end else begin
    ErrorRespond(ARequest,AResponse,404,'invalid scheme "'+dbgstr(Scheme)+'"');
    exit;
  end;
end;

procedure TXeletorApplication.HandleRequestDoc(Path: string;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
{ Handle requests of the form
  DBName/Path
  or
  /DBName/Path
}
{ $DEFINE VerboseDocRequest}
var
  Doc: TXDBDocument;
  ms: TMemoryStream;
begin
  ms:=TMemoryStream.Create;
  try
    try
      Storage.BeginReading;
      try
        Doc:=Storage.FindDocument(Path,true,false);
        Doc.WriteToStream(ms);
        ms.Position:=0;
        {$IFDEF VerboseDocRequest}
        debugln(['TXeletorApplication.HandleRequestDoc ',dbgs(ms)]);
        {$ENDIF}
      finally
        Storage.EndReading;
      end;
    except
      on E: Exception do begin
        ErrorRespond(ARequest,AResponse,404,E.Message);
        exit;
      end;
    end;
    AResponse.ContentType:='text/xml';
    AResponse.ContentLength:=ms.Size;
    ClientLog(etInfo,ARequest,['Serving file: "',Path,'". MimeType: ',AResponse.ContentType,' Length=',AResponse.ContentLength]);
    AResponse.ContentStream:=ms;
    AResponse.SendContent;
    AResponse.ContentStream:=Nil;
  finally
    ms.Free;
  end;
end;

procedure TXeletorApplication.HandleRequestListDocs(Path: string;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse; ExtendedFormat: boolean);
{ Handle requests of the form
  DBName/Path
  or
  /DBName/Path
}
{ $DEFINE VerboseListDocsRequest}
var
  ms: TMemoryStream;

  procedure w(Line: string);
  begin
    Line:=Line+#13#10;
    ms.Write(Line[1],length(Line));
  end;

var
  Docs: TFPList;
  i: Integer;
  aFile: TXDBFile;
  s: String;
  Doc: TXDBDocument;
begin
  ms:=TMemoryStream.Create;
  Docs:=TFPList.Create;
  try
    try
      Storage.BeginReading;
      try
        Storage.Roots.ListFiles(Path,Docs,[xlfAddFiles]);
        w('<?xml version="1.0" encoding="UTF-8"?>');
        w('<listing path="'+StrToXMLValue(Path)+'">');
        for i:=0 to Docs.Count-1 do begin
          aFile:=TXDBFile(Docs[i]);
          s:='path="'+StrToXMLValue(aFile.GetFullFilename)+'"';
          if ExtendedFormat then begin
            if aFile is TXDBDocument then begin
              Doc:=TXDBDocument(aFile);
              s:=s+' fileage="'+FileAgeToXDBStr(Doc.FileAge)+'"';
              s:=s+' loadage="'+FileAgeToXDBStr(Doc.LoadAge)+'"';
              s:=s+' dbage="'+DateTimeToXDBStr(Doc.DBAge)+'"';
            end;
          end;
          w('<file '+s+'/>');
        end;
        w('</listing>');
        ms.Position:=0;
        {$IFDEF VerboseListDocsRequest}
        debugln(['TXeletorApplication.HandleRequestListDocs ',dbgs(ms)]);
        {$ENDIF}
      finally
        Storage.EndReading;
        FreeAndNil(Docs);
      end;
    except
      on E: Exception do begin
        ErrorRespond(ARequest,AResponse,404,E.Message);
        exit;
      end;
    end;
    AResponse.ContentType:='text/xml';
    AResponse.ContentLength:=ms.Size;
    ClientLog(etInfo,ARequest,['Serving listing: "',Path,'". MimeType: ',AResponse.ContentType,' Length=',AResponse.ContentLength]);
    AResponse.ContentStream:=ms;
    AResponse.SendContent;
    AResponse.ContentStream:=Nil;
  finally
    Docs.Free;
    ms.Free;
  end;
end;

procedure TXeletorApplication.HandleRequestFindDocs(Path: string;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  ms: TMemoryStream;

  procedure w(Line: string);
  begin
    Line:=Line+#13#10;
    ms.Write(Line[1],length(Line));
  end;

var
  Nodes: TFPList;
  i: Integer;
  Node: TXDBNode;
begin
  ms:=TMemoryStream.Create;
  Nodes:=TFPList.Create;
  try
    try
      Storage.BeginReading;
      try
        Storage.Roots.FindNodes(Path,Nodes,[xfnfFindFirst,xfnfContinueInNextFile]);
        w('<?xml version="1.0" encoding="UTF-8"?>');
        w('<nodes path="'+StrToXMLValue(Path)+'">');
        for i:=0 to Nodes.Count-1 do begin
          Node:=TXDBNode(Nodes[i]);
          w('<node file="'+Node.GetFullFilename+'" xpath="'+Node.GetDPath+'"/>');
        end;
        w('</nodes>');
        ms.Position:=0;
      finally
        Storage.EndReading;
        FreeAndNil(Nodes);
      end;
    except
      on E: Exception do begin
        ErrorRespond(ARequest,AResponse,404,E.Message);
        exit;
      end;
    end;
    AResponse.ContentType:='text/xml';
    AResponse.ContentLength:=ms.Size;
    ClientLog(etInfo,ARequest,['Serving nodes: "',Path,'". MimeType: ',AResponse.ContentType,' Length=',AResponse.ContentLength]);
    AResponse.ContentStream:=ms;
    AResponse.SendContent;
    AResponse.ContentStream:=Nil;
  finally
    Nodes.Free;
    ms.Free;
  end;
end;

procedure TXeletorApplication.HandleRequestFindNodes(Path: string;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  ms: TMemoryStream;

  procedure w(Line: string);
  begin
    Line:=Line+#13#10;
    ms.Write(Line[1],length(Line));
  end;

var
  Nodes: TFPList;
  i: Integer;
  Node: TXDBNode;
  CurRoot: TXDBNode;
  LastRoot: TXDBNode;
begin
  ms:=TMemoryStream.Create;
  Nodes:=TFPList.Create;
  try
    try
      Storage.BeginReading;
      try
        Storage.Roots.FindNodes(Path,Nodes);
        w('<?xml version="1.0" encoding="UTF-8"?>');
        w('<nodes path="'+StrToXMLValue(Path)+'">');
        LastRoot:=nil;
        for i:=0 to Nodes.Count-1 do begin
          Node:=TXDBNode(Nodes[i]);
          CurRoot:=Node.GetRoot;
          if CurRoot<>LastRoot then begin
            if LastRoot<>nil then
              w('  </file>');
            w('  <file docpath="'+CurRoot.GetFullFilename+'">');
            LastRoot:=CurRoot;
          end;
          w('    <node xpath="'+Node.GetDPath+'">');
          Node.WriteToStream(ms,3);
          w('    </node>');
        end;
        if LastRoot<>nil then
          w('  </file>');
        w('</nodes>');
        ms.Position:=0;
      finally
        Storage.EndReading;
        FreeAndNil(Nodes);
      end;
    except
      on E: Exception do begin
        ErrorRespond(ARequest,AResponse,404,E.Message);
        exit;
      end;
    end;
    AResponse.ContentType:='text/xml';
    AResponse.ContentLength:=ms.Size;
    ClientLog(etInfo,ARequest,['Serving nodes: "',Path,'". MimeType: ',AResponse.ContentType,' Length=',AResponse.ContentLength]);
    AResponse.ContentStream:=ms;
    AResponse.SendContent;
    AResponse.ContentStream:=Nil;
  finally
    Nodes.Free;
    ms.Free;
  end;
end;

procedure TXeletorApplication.HandleRequestRescan(Path: string;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  ss: TStringStream;
begin
  AResponse.ContentType:='text/html';
  ss:=TStringStream.Create('<HTML><BODY>'#13#10
           +'Rescan started ...'#13#10
           +'</BODY></HTML>'#13#10);
  try
    AResponse.ContentLength:=ss.Size;
    ClientLog(etInfo,ARequest,['Starting rescan. MimeType: ',AResponse.ContentType,' Length=',AResponse.ContentLength]);
    AResponse.ContentStream:=ss;
    AResponse.SendContent;
    AResponse.ContentStream:=Nil;
    if Path<>'' then ;
    RescanFiles;
  finally
    ss.Free;
  end;
end;

procedure TXeletorApplication.HandleRequestListErrors(Path: string;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  ms: TMemoryStream;

  procedure w(Line: string);
  begin
    Line:=Line+#13#10;
    ms.Write(Line[1],length(Line));
  end;

var
  Item: PStringToPointerTreeItem;
  ScanError: TXDBScanError;
begin
  ms:=TMemoryStream.Create;
  try
    try
      Storage.BeginReading;
      try
        w('<?xml version="1.0" encoding="UTF-8"?>');
        w('<listing>');
        for Item in Storage.ErrorFiles do begin
          ScanError:=TXDBScanError(Item^.Value);
          w('  <file path="'+StrToXMLValue(Item^.Name)+'" error="'+StrToXMLValue(ScanError.Msg)+'"/>');
        end;
        w('</listing>');
        ms.Position:=0;
        {$IFDEF VerboseListDocsRequest}
        debugln(['TXeletorApplication.HandleRequestListErrors ',dbgs(ms)]);
        {$ENDIF}
      finally
        Storage.EndReading;
      end;
    except
      on E: Exception do begin
        ErrorRespond(ARequest,AResponse,404,E.Message);
        exit;
      end;
    end;
    AResponse.ContentType:='text/xml';
    AResponse.ContentLength:=ms.Size;
    ClientLog(etInfo,ARequest,['Serving listing: "',Path,'". MimeType: ',AResponse.ContentType,' Length=',AResponse.ContentLength]);
    AResponse.ContentStream:=ms;
    AResponse.SendContent;
    AResponse.ContentStream:=Nil;
  finally
    ms.Free;
  end;
end;

procedure TXeletorApplication.HandleRequestStop(Path: string;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  ss: TStringStream;
begin
  AResponse.ContentType:='text/html';
  ss:=TStringStream.Create('<HTML><BODY>'#13#10
           +'Stopping Xeletor ...'#13#10
           +'</BODY></HTML>'#13#10);
  try
    AResponse.ContentLength:=ss.Size;
    ClientLog(etInfo,ARequest,['Stopping Xeletor on client request.']);
    AResponse.ContentStream:=ss;
    AResponse.SendContent;
    AResponse.ContentStream:=Nil;
    if Path<>'' then ;
    Terminate;
  finally
    ss.Free;
  end;
end;

procedure TXeletorApplication.TestRead1;
var
  Filename: String;
  XDoc: TXDBDocument;
  XMLDoc: TXMLDocument;
begin
  debugln(['TXeletorApplication.TestRead1 START']);
  Filename:='../../dare/fulltexts/production/AverroesJuniorArab.xml';
  laz2_XMLRead.ReadXMLFile(XMLDoc,Filename);
  XDoc:=TXDBDocument.Create(ExtractFileName(Filename));
  XDoc.XMLDoc:=XMLDoc;
  XMLDoc:=nil;
  XDoc.CreateTreeFromXML;
  writeln(XDoc.Root.GetXML);
  debugln(['TXeletorApplication.TestRead1 END']);
end;

procedure TXeletorApplication.TestListErrors;
var
  Item: PStringToPointerTreeItem;
  ScanError: TXDBScanError;
begin
  WaitTillFilesScanned;
  debugln(['TXeletorApplication.TestListErrors Count=',Storage.ErrorFiles.Count]);
  for Item in Storage.ErrorFiles do begin
    ScanError:=TXDBScanError(Item^.Value);
    debugln(['  <file path="'+StrToXMLValue(Item^.Name)+'" error="'+StrToXMLValue(ScanError.Msg)+'"/>']);
  end;
end;

procedure TXeletorApplication.TestMultiReadExclusiveWrite;
type
  TInfo = record
    Reading: boolean;
    Writing: boolean;
  end;
var
  Infos: array[0..2] of TInfo;
  MultiReadSupported: boolean;
  SingleWriteSupported: boolean;

  procedure ClearInfos;
  var
    i: Integer;
  begin
    for i:=Low(Infos) to High(Infos) do FillByte(Infos[i],SizeOf(TInfo),0);
  end;

  procedure ParallelReadTest(Index: PtrInt; {%H-}Data: Pointer;
                                {%H-}Item: TMultiThreadProcItem);
  // test if multiple threads can read in parallel
  var
    ReadCount: Integer;
    i: Integer;
  begin
    //fLog.Log(etInfo,'ParallelReadTest START Index='+IntToStr(Index));
    Storage.BeginReading;
    try
      //fLog.Log(etInfo,'ParallelReadTest before sleep Index='+IntToStr(Index));
      Infos[Index].Reading:=true;
      Sleep(300);
      ReadCount:=0;
      for i:=Low(Infos) to High(Infos) do
        if Infos[i].Reading then inc(ReadCount);
      if ReadCount>1 then
        MultiReadSupported:=true;
      Sleep(300);
      Infos[Index].Reading:=false;
      //fLog.Log(etInfo,'ParallelReadTest after sleep Index='+IntToStr(Index));
    finally
      Storage.EndReading;
    end;
    //Log(etInfo,'ParallelReadTest END Index='+IntToStr(Index));
  end;

  procedure ParallelWriteTest(Index: PtrInt; {%H-}Data: Pointer;
                                {%H-}Item: TMultiThreadProcItem);
  // test if multiple threads can not write in parallel
  var
    WriteCount: Integer;
    i: Integer;
  begin
    //fLog.Log(etInfo,'ParallelWriteTest START Index='+IntToStr(Index));
    Storage.BeginWriting;
    try
      //fLog.Log(etInfo,'ParallelWriteTest before sleep Index='+IntToStr(Index));
      Infos[Index].Writing:=true;
      Sleep(300);
      WriteCount:=0;
      for i:=Low(Infos) to High(Infos) do
        if Infos[i].Writing then inc(WriteCount);
      if WriteCount>1 then
        SingleWriteSupported:=false;
      Sleep(300);
      Infos[Index].Writing:=false;
      //fLog.Log(etInfo,'ParallelWriteTest after sleep Index='+IntToStr(Index));
    finally
      Storage.EndWriting;
    end;
    //Log(etInfo,'ParallelWriteTest END Index='+IntToStr(Index));
  end;

begin
  fLog.AlwaysFlush:=true;
  fLog.Log(etInfo,'TXeletorApplication.TestMultiReadExclusiveWrite START');

  // check if multiple threads can read in parallel
  ClearInfos;
  MultiReadSupported:=false;
  ProcThreadPool.DoParallelLocalProc(@ParallelReadTest,0,2);
  fLog.Log(etInfo,['TXeletorApplication.TestMultiReadExclusiveWrite MultiReadSupported=',MultiReadSupported]);

  // check if multiple threads can not write in parallel
  ClearInfos;
  SingleWriteSupported:=true;
  ProcThreadPool.DoParallelLocalProc(@ParallelWriteTest,0,2);
  fLog.Log(etInfo,['TXeletorApplication.TestMultiReadExclusiveWrite SingleWriteSupported=',SingleWriteSupported]);

  // check if one thread can nest BeginWriting
  Storage.BeginWriting;
  try
    Storage.BeginWriting;
    try
    finally
      Storage.EndWriting;
    end;
  finally
    Storage.EndWriting;
  end;
  fLog.Log(etInfo,['TXeletorApplication.TestMultiReadExclusiveWrite NestedBeginwrite=',true]);

  fLog.Log(etInfo,'TXeletorApplication.TestMultiReadExclusiveWrite END');
end;

procedure TXeletorApplication.TestStrToIP(Quiet: boolean);

  procedure CheckInAddr(const Msg: string; const a: in_addr; i1,i2,i3,i4: byte);
  begin
    if (a.s_bytes[1]=i1) and ((a.s_bytes[2]=i2)) and (a.s_bytes[3]=i3) and (a.s_bytes[4]=i4) then exit;
    debugln(['Check failed: expected ',i1,'.',i2,'.',i3,'.',i4,' but found ',dbgs(a),': ',Msg]);
    Halt;
  end;

  procedure CheckIPNet(const Msg: string; const IP, Net: in_addr; i1,i2,i3,i4, n1,n2,n3,n4: byte);
  begin
    if (IP.s_bytes[1]=i1) and ((IP.s_bytes[2]=i2)) and (IP.s_bytes[3]=i3) and (IP.s_bytes[4]=i4) then exit;
    if (Net.s_bytes[1]=n1) and ((Net.s_bytes[2]=n2)) and (Net.s_bytes[3]=n3) and (Net.s_bytes[4]=n4) then exit;
    debugln(['Check failed: expected ',i1,'.',i2,'.',i3,'.',i4,'/',n1,'.',n2,'.',n3,'.',n4,' but found ',dbgs(IP),'/',dbgs(Net),': ',Msg]);
    Halt;
  end;

var
  IP: in_addr;
  Net: in_addr;
begin
  if not Quiet then
    debugln(['TXeletorApplication.TestStrToIP START']);
  //CheckInAddr('StrToHostAddr(''1.2.3.4'')',StrToHostAddr('1.2.3.4'),1,2,3,4);

  CheckInAddr('StrToNetAddr(''1.2.3.4'')',StrToNetAddr('1.2.3.4'),1,2,3,4);

  StrToSubNet('1.2.3.4',IP,Net);
  CheckIPNet('StrToSubNet(''1.2.3.4'')',IP,Net,1,2,3,4,255,255,255,255);

  StrToSubNet('1.2.3.4/32',IP,Net);
  CheckIPNet('StrToSubNet(''1.2.3.4'')',IP,Net,1,2,3,4,255,255,255,255);

  StrToSubNet('1.2.3.4/30',IP,Net);
  CheckIPNet('StrToSubNet(''1.2.3.4/22'')',IP,Net,1,2,3,4,255,255,255,252);

  StrToSubNet('1.2.3/22',IP,Net);
  CheckIPNet('StrToSubNet(''1.2.3/22'')',IP,Net,1,2,3,0,255,255,252,0);

  StrToSubNet('10.1.2.3/24',IP,Net);
  CheckIPNet('StrToSubNet(''10.1.2.3/24'')',IP,Net,10,1,2,3,255,255,255,0);

  StrToSubNet('1.2.3.4/255.255.255.248',IP,Net);
  CheckIPNet('StrToSubNet(''1.2.3.4/255.255.255.248'')',IP,Net,1,2,3,4,255,255,255,248);

  StrToSubNet('1.2.3/255.255.255.248',IP,Net);
  CheckIPNet('StrToSubNet(''1.2.3/255.255.255.248'')',IP,Net,1,2,3,0,255,255,255,248);

  StrToSubNet('1.2.3/255.255.255.0',IP,Net);
  CheckIPNet('StrToSubNet(''1.2.3/255.255.255.0'')',IP,Net,1,2,3,0,255,255,255,0);

  if not Quiet then
    debugln(['TXeletorApplication.TestStrToIP END']);
end;

procedure TXeletorApplication.TestAllowDeny(Quiet: boolean);
var
  l: TAllowDenyFromList;
  Item: TAllowDenyFromItem;

  procedure Check(IP: string; Allow: boolean);
  begin
    if Allow=l.IsAllowed(StrToHost(IP)) then exit;
    raise Exception.Create('[TXeletorApplication.TestAllowDeny] IP='+IP+' ShouldAllow='+dbgs(Allow));
  end;

begin
  if not Quiet then
    debugln(['TXeletorApplication.TestAllowDeny START']);
  l:=TAllowDenyFromList.Create;
  Item:=l.Add(true,'127.0.0.1');
  if not Quiet then
    debugln(['TXeletorApplication.TestAllowDeny Item: ',Item.Allow,' ',dbgs(Item.IP),'/',dbgs(Item.Net)]);
  Item:=l.Add(true,'10.1.2.3/24');
  if not Quiet then
    debugln(['TXeletorApplication.TestAllowDeny Item: ',Item.Allow,' ',dbgs(Item.IP),'/',dbgs(Item.Net)]);
  Check('1.2.3.4',false);
  Check('127.0.0.1',true);
  Check('127.0.1.1',false);
  Check('10.1.2.3',true);
  Check('10.1.2.254',true);
  Check('10.1.3.3',false);
  l.Free;
  if not Quiet then
    debugln(['TXeletorApplication.TestAllowDeny END']);
end;

procedure TXeletorApplication.TestFindNodes(const NodePath: string);
var
  Nodes: TFPList;
  i: Integer;
  Node: TXDBNode;
begin
  debugln(['TXeletorApplication.TestFindNodes START ',NodePath]);
  Nodes:=TFPList.Create;
  try
    Storage.Roots.FindNodes(NodePath,Nodes);
    for i:=0 to Nodes.Count-1 do begin
      Node:=TXDBNode(Nodes[i]);
      debugln(['TXeletorApplication.TestFindNodes ',i,' file="'+Node.GetFullFilename+'" xpath="'+Node.GetXPath+'" dpath="',Node.GetDPath,'"']);
    end;
  finally
    Nodes.Free;
  end;
  debugln(['TXeletorApplication.TestFindDocs END']);
end;

procedure TXeletorApplication.TestForAllGrandChildren(const NodePath: string);
var
  Node: TXDBNode;
  Child: TXDBNode;
begin
  debugln(['TXeletorApplication.TestForAllGrandChildren START NodePath=',NodePath]);
  Node:=Storage.Roots.FindFirstNode(NodePath,true);
  for Child in Node.GetEnumeratorAllChildren do
    writeln(Child.GetDPath);
  debugln(['TXeletorApplication.TestForAllGrandChildren END']);
end;

procedure TXeletorApplication.TestFindNodesViaNodePath(const NodePath: string);
var
  Nodes: TFPList;
  i: Integer;
  Node: TXDBNode;
  CurPath: String;
  Node2: TXDBNode;
begin
  debugln(['TXeletorApplication.TestFindNodesViaNodePath START ',NodePath]);
  Nodes:=TFPList.Create;
  try
    Storage.Roots.FindNodes(NodePath,Nodes);
    for i:=0 to Nodes.Count-1 do begin
      Node:=TXDBNode(Nodes[i]);
      CurPath:=Node.GetNodeXPath;
      debugln(['TXeletorApplication.TestFindNodesViaNodePath ',i,' path="'+CurPath+'"']);
      Node2:=Storage.Roots.FindFirstNode(CurPath);
      if Node2=Node then continue;
      // error
      if Node2=nil then
        debugln(['ERROR: TXeletorApplication.TestFindNodesViaNodePath path="'+CurPath+'" not found'])
      else
        debugln(['ERROR: TXeletorApplication.TestFindNodesViaNodePath path="'+CurPath+'" found wrong node: "',Node2.GetNodeDPath,'"']);
    end;
  finally
    Nodes.Free;
  end;
  debugln(['TXeletorApplication.TestFindNodesViaNodePath END']);
end;

procedure TXeletorApplication.UpdateRootDirectories;

  function Update(Writing: boolean): boolean;
  // returns true if changed
  var
    DBDirParam: PStringToStringTreeItem;
    DeleteRootDir: TXDBRootDirectory;
    DBFile: TXDBFile;
    RootDir: TXDBRootDirectory;
  begin
    Result:=false;
    // remove old root directories
    repeat
      DeleteRootDir:=nil;
      for DBFile in Storage.Roots do begin
        RootDir:=DBFile as TXDBRootDirectory;
        if (not FDBDirParams.Contains(RootDir.Filename))
        or (FDBDirParams[RootDir.Filename]<>RootDir.LongFileName) then begin
          DeleteRootDir:=RootDir;
        end;
      end;
      if DeleteRootDir=nil then break;
      if not Writing then exit(true);
      Storage.Roots.Delete(DeleteRootDir);
    until false;

    // create new root directories
    for DBDirParam in FDBDirParams do begin
      if Storage.FindRootWithName(DBDirParam^.Name)<>nil then continue;
      if not Writing then exit(true);
      RootDir:=Storage.CreateRootDirectory(DBDirParam^.Name, DBDirParam^.Value);
      if Writing then
        fLog.Log(etInfo,['new root directory ',RootDir.LongFileName,' ...']);
    end;
    if Writing then
      fLog.FlushStdOutLog;
  end;
begin
  Storage.BeginReading;
  try
    if not Update(false) then exit;
  finally
    Storage.EndReading;
  end;
  // apply
  Storage.BeginWriting;
  try
    Update(true);
  finally
    Storage.EndWriting;
  end;
end;

procedure TXeletorApplication.ParamError(const Msg: string);
begin
  writeln('Error: ',Msg);
  writeln;
  WriteHelp(false);
  Log(etError,'TFTPMirror.ParamError '+Msg);
  Halt;
end;

function TXeletorApplication.GetParams(Index: Integer): String;
begin
  Result:=fParams[Index];
end;

function TXeletorApplication.GetParamCount: Integer;
begin
  Result:=fParams.Count-1;
end;

procedure TXeletorApplication.InsertOptionsFile(Filename: string; ParamPos: integer);
var
  sl: TStringList;
  i: Integer;
  s: String;
begin
  if not FileExistsUTF8(Filename) then
    ParamError('Config file not found: '+Filename);

  sl:=TStringList.Create;
  try
    sl.LoadFromFile(Filename);
    for i:=0 to sl.Count-1 do begin
      s:=Trim(sl[i]);
      if (s='') or (s[1]='#') then continue;
      fParams.Insert(ParamPos,s);
      inc(ParamPos);
    end;
  finally
    sl.Free;
  end;
end;

procedure TXeletorApplication.ReadConfig;
const
  ShortOpts = 'hc:l:d:p:vqV';
  LongOpts = 'help config: log: dbdir: port: allow-from: deny-from:'
    +' rescaninterval: watchdoginterval: singlethreaded verbose quiet version';
  ConfigOpt = '--config=';
var
  LongOptions: TStrings;
  i: Integer;
  p: String;

  procedure CheckOpts;
  var
    Opts,NonOpts: TStrings;
    ErrorMsg: String;
    i: Integer;
  begin
    Opts:=TStringList.Create;
    NonOpts:=TStringList.Create;
    try
      ErrorMsg:=CheckOptions(ShortOpts,LongOptions,Opts,NonOpts);
      if ErrorMsg<>'' then begin
        ShowException(Exception.Create(ErrorMsg));
        Halt;
      end;
      for i:=0 to NonOpts.Count-1 do
        if NonOpts[i]<>'' then
          ParamError('invalid parameter "'+NonOpts[i]+'"');
    finally
      Opts.Free;
      NonOpts.Free;
    end;
    fLog.Verbose:=HasOption('V','verbose');
    fLog.Quiet:=HasOption('q','quiet');
  end;

begin
  LongOptions:=StringToList(LongOpts);
  try
    CheckOpts;

    // parse parameters
    if HasOption('h','help') then begin
      WriteHelp(true);
      Halt;
    end;

    // parse parameters
    if HasOption('v','version') then begin
      writeln(Version);
      Halt;
    end;

    i:=1;
    while i<=ParamCount do begin
      p:=GetParams(i);
      //debugln(['TXeletorApplication.ReadConfig ',i,'/',ParamCount,' ',p]);
      if p='-c' then begin
        inc(i);
        InsertOptionsFile(GetParams(i),i+1);
        CheckOpts;
      end else if copy(p,1,length(ConfigOpt))=ConfigOpt then begin
        p:=copy(p,length(ConfigOpt)+1,length(p));
        InsertOptionsFile(p,i+1);
        CheckOpts;
      end;
      inc(i);
    end;
  finally
    LongOptions.Free;
  end;
end;

procedure TXeletorApplication.DoLog(EventType: TEventType; const Msg: String);
begin
  fLog.Log(EventType, Msg);
end;

procedure TXeletorApplication.ClientLog(EventType: TEventType;
  Request: TFPHTTPConnectionRequest; Msg: array of const);
begin
  if Request=nil then
    Log(EventType,'no client '+DbgS(Msg))
  else
    ClientLog(EventType,Request.Connection,Msg);
end;

procedure TXeletorApplication.ClientLog(EventType: TEventType;
  Connection: TFPHTTPConnection; Msg: array of const);
var
  s: String;
begin
  s:='';
  if Connection=nil then
    s:='no connection'
  else if Connection.Socket=nil then
    s:='no socket'
  else begin
    s:=dbgs(Connection.Socket.RemoteAddress);
  end;
  Log(EventType,s+' '+DbgS(Msg));
end;

procedure TXeletorApplication.ClientLog(EventType: TEventType;
  const SockAddr: TSockAddr; Msg: array of const);
var
  s: String;
begin
  s:=dbgs(SockAddr);
  Log(EventType,s+' '+DbgS(Msg));
end;

constructor TXeletorApplication.Create(TheOwner: TComponent);
var
  i: Integer;
begin
  fLog:=TXDBLog.Create;
  fParams:=TStringList.Create;
  for i:=0 to System.ParamCount do
    fParams.Add(System.ParamStr(i));
  inherited Create(TheOwner);
  StopOnException:=True;
  Storage:=TXDBStorage.Create;
  Storage.Log:=fLog;
  Port:=5588;
end;

destructor TXeletorApplication.Destroy;
begin
  Storage.BeginWriting;
  try

  finally
    Storage.EndWriting;
  end;
  FreeAndNil(Storage);
  FreeAndNil(FDBDirParams);
  Log(etInfo,'Daemon stopped.');
  fLog.FlushStdOutLog;
  FreeAndNil(AllowDeny);
  inherited Destroy;
  FreeAndNil(fLog);
  FreeAndNil(fParams);
end;

procedure TXeletorApplication.WriteHelp(WithHeader: boolean);
begin
  writeln(GetCaption);
  writeln;
  if WithHeader then begin
    writeln('Xeletor is a lightweight XML database.');
    writeln('It reads directories of xml files into memory and monitors them');
    writeln('to update automatically when files change on disk.');
    writeln('It provides a simple webserver for various types of queries.');
    writeln('To get some help about the supported queries download the default');
    writeln('webpage at http://localhost:',Port);
    writeln;
    writeln('Official homepage: http://developer.berlios.de/projects/xeletor/');
    writeln;
    writeln;
  end;
  writeln('Usage: ',ExeName,' -d db1=/path/to/your/xml/files');
  writeln;
  writeln('  -h');
  writeln('  --help          : write this help');
  writeln('  -c <configfile> : file with more options. One line per option.');
  writeln('                    Lines beginning with # are comments.');
  writeln('  -l <logfile>');
  writeln('  --logfile=<logfile>');
  writeln('  -d <name=dbdir>');
  writeln('  --dbdir=<name=dbdir> : directory of all your xml files, including sub directories');
  writeln('                         Can be given multiple times.');
  writeln('  --singlethreaded : run with least amount of threads');
  writeln('  -p <port>');
  writeln('  --port=<port>   : TCP port, default: ',Port);
  writeln('  --allow-from=<ip or subnet> :');
  writeln('    Allow connections from this ip address or subnet.');
  writeln('      Format: 1.2.3.4, 1.2.3/24, 1.2.3.4/255.255.255.0');
  writeln('      Multiple --allow-from and --deny-from can be added.');
  writeln('      If no --allow-from and now --deny-from is specified then all addresses are allowed.');
  writeln('      If at least one --allow-from or --deny-from is specified, an address');
  writeln('      needs at least one --allow-from and no --deny-from.');
  writeln('      Example for only allowing local connections: --allow-from=127.0.0.1');
  writeln('  --deny-from=<ip or subnet> :');
  writeln('      Deny connections from this ip address or subnet.');
  writeln('      See --allow-from');
  writeln('  --rescaninterval=<in seconds> : rescan every n seconds');
  writeln('  --watchdoginterval=<in seconds> : write an alive message to the log');
  writeln('  -v');
  writeln('  --version       : write version and exit');
  writeln('  -V');
  writeln('  -verbose        : write what is going on');
  writeln('  -q');
  writeln('  -quiet          : write less information');
end;

procedure TXeletorApplication.Terminate;
begin
  inherited Terminate;
  if Storage<>nil then
    Storage.Terminating:=true;
end;

procedure TXeletorApplication.ReloadConfiguration;
begin
  Log(etInfo,'reloading configuration not yet implemented, rescanning files ...');
  RescanFiles;
end;

begin
  Application:=TXeletorApplication.Create(nil);
  Application.Run;
  Application.Free;
end.

