{ Copyright (C) 2011  Mattias Gaertner  mattias@freepascal.org

  Abstract:
    Storage for multiple root directories.
}
unit xdbcentral;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  {$IFDEF Linux}
  inotify,
  {$ENDIF}
  CodeToolsStructs, laz2_DOM, laz2_XMLRead, LazLogger, LazFileUtils, MTProcs,
  xdblog, XFileWatch, xdbfiles, xdbutils;

type
  TXDBStorage = class;

  { TXDBScanThread }

  TXDBScanThread = class(TThread)
  public
    Storage: TXDBStorage;
    OnExecuted: TNotifyEvent;
    procedure Execute; override;
  end;

  { TXDBScanError }

  TXDBScanError = class
  public
    Filename: string;
    Age: integer; // FileAge when last parsed
    Msg: string;
    constructor Create(const aFilename: string; AnAge: integer; const aMsg: string);
  end;

  { TXDBStorage }

  TXDBStorage = class
  private
    fMultiReadExclusiveWrite: TMultiReadExclusiveWriteSynchronizer;
    procedure ParallelReadXMLFile(Index: PtrInt; Data: Pointer;
                                  {%H-}Item: TMultiThreadProcItem);
  public
    ErrorFiles: TXDBFilenameToPointerTree; // dbfilename to TXDBScanError
    Roots: TXDBRootDirectories;
    Watch: TXDBFileSystemWatch;
    Log: TXDBLog;
    Terminating: boolean;

    constructor Create;
    destructor Destroy; override;
    function FindDocument(Path: string;
                ExceptionIfNotFound, CreateIfNotExists: boolean): TXDBDocument;
    function FindDirectory(Path: string;
                ExceptionIfNotFound, CreateIfNotExists: boolean): TXDBDirectory;
    function FindRootWithName(const DBName: string): TXDBRootDirectory;
    function FindRootWithDir(const Filename: string): TXDBRootDirectory;
    function DBPathToFilename(DBPath: string): string;
    function CreateRootDir(const ShortFilename, LongFilename: string): TXDBRootDirectory;

    // update
    procedure ReadAllXMLFiles; // scan all root directories on disk and delete vanished files and load modified/new
    function CreateRootDirectory(aName, aPath: string): TXDBRootDirectory;
    procedure DeleteVanishedFiles(KeepFiles: TXDBFilenameToStringTree);
    function ReadXMLFile(const DBFilename: string; out doc: TXDBDocument): boolean;
    function AddOrReplaceDoc(const DBDir: string; doc: TXDBDocument): TXDBDocument;

    procedure BeginReading;
    procedure EndReading;
    procedure BeginWriting;
    procedure EndWriting;
  end;

implementation

{ TXDBScanError }

constructor TXDBScanError.Create(const aFilename: string; AnAge: integer;
  const aMsg: string);
begin
  Filename:=aFilename;
  Age:=AnAge;
  Msg:=aMsg;
end;

{ TXDBScanThread }

procedure TXDBScanThread.Execute;
begin
  try
    Storage.ReadAllXMLFiles;
  except
    on E: Exception do begin
      if (Storage<>nil) and (Storage.Log<>nil) then
        Storage.Log.Log(etError,'[TXDBScanThread.Execute] Exception: '+E.Message);
    end;
  end;
  if Assigned(OnExecuted) then
    OnExecuted(Self);
end;

{ TXDBStorage }

constructor TXDBStorage.Create;
begin
  fMultiReadExclusiveWrite:=TMultiReadExclusiveWriteSynchronizer.Create;
  Roots:=TXDBRootDirectories.Create('');
  Watch:=TXDBFileSystemWatch.Create;
  ErrorFiles:=TXDBFilenameToPointerTree.Create(false);
  ErrorFiles.FreeValues:=true;
end;

destructor TXDBStorage.Destroy;
begin
  BeginWriting;
  EndWriting;
  ErrorFiles.Clear;
  FreeAndNil(ErrorFiles);
  FreeAndNil(Roots);
  FreeAndNil(Watch);
  inherited Destroy;
  FreeAndNil(fMultiReadExclusiveWrite);
end;

function TXDBStorage.FindDocument(Path: string; ExceptionIfNotFound,
  CreateIfNotExists: boolean): TXDBDocument;
var
  Dir: TXDBDirectory;
  aFile: TXDBFile;
  FileName: String;
  p: Integer;
begin
  Result:=nil;
  p:=length(Path);
  while (p>0) and (Path[p]<>'/') do dec(p);
  Dir:=FindDirectory(copy(Path,1,p-1),ExceptionIfNotFound,CreateIfNotExists);
  if Dir=nil then exit;
  Filename:=copy(Path,p+1,length(Path));
  aFile:=Dir.FindFile(Filename);
  if aFile<>nil then begin
    if aFile is TXDBDocument then begin
      Result:=TXDBDocument(aFile);
    end else begin
      if not ExceptionIfNotFound then exit;
      raise Exception.Create('file is directory: '+DbgStr(Path));
    end;
  end else if CreateIfNotExists then begin
    Result:=TXDBDocument.Create(FileName);
    Dir.Add(Result);
  end else begin
    if not ExceptionIfNotFound then exit;
    raise Exception.Create('file not found: '+dbgstr(Path));
  end;
end;

function TXDBStorage.FindDirectory(Path: string; ExceptionIfNotFound,
  CreateIfNotExists: boolean): TXDBDirectory;
var
  Dir: TXDBDirectory;
  FileName: String;
  aFile: TXDBFile;
begin
  Result:=nil;
  if (length(Path)>0) and (Path[1]='/') then
    Delete(Path,1,1);
  Dir:=Roots;
  while Path<>'' do begin
    FileName:=ExtractFirstURLPath(Path);
    aFile:=Dir.FindFile(FileName);
    if aFile<>nil then begin
      if aFile is TXDBDocument then begin
        if not ExceptionIfNotFound then exit;
        raise Exception.Create('[TXDBCentral.FindDirectory] Path is a document: "'+dbgstr(aFile.GetFullFilename)+'"');
      end else if aFile is TXDBDirectory then begin
        Dir:=TXDBDirectory(aFile);
      end else begin
        if not ExceptionIfNotFound then exit;
        raise Exception.Create('[TXDBCentral.FindDirectory] Path is not a directory: "'+dbgstr(aFile.GetFullFilename)+'"');
      end;
    end else if CreateIfNotExists and (Dir<>Roots) then begin
      aFile:=TXDBDirectory.Create(Filename);
      Dir.Add(aFile);
      Dir:=TXDBDirectory(aFile);
    end else begin
      if not ExceptionIfNotFound then exit;
      if Dir=Roots then
        raise Exception.Create('[TXDBCentral.FindDirectory] DB not found: "'+FileName+'"')
      else
        raise Exception.Create('[TXDBCentral.FindDirectory] Path not found: "'+dbgstr(Dir.GetFullFilename)+PathDelim+FileName+'"');
    end;
  end;
  Result:=Dir;
end;

function TXDBStorage.FindRootWithName(const DBName: string): TXDBRootDirectory;
begin
  Result:=TXDBRootDirectory(Roots.FindFile(DBName));
end;

function TXDBStorage.FindRootWithDir(const Filename: string
  ): TXDBRootDirectory;
begin
  Result:=Roots.FindLongFileName(Filename);
end;

function TXDBStorage.DBPathToFilename(DBPath: string): string;
var
  DBName: String;
  Dir: TXDBRootDirectory;
begin
  if (length(DBPath)>0) and (DBPath[1]='/') then
    Delete(DBPath,1,1);
  DBName:=ExtractFirstURLPath(DBPath);
  if DBName='' then
    Dir:=nil
  else
    Dir:=Roots.FindRoot(DBName);
  if Dir=nil then
    raise Exception.Create('DB not found: '+dbgstr(DBName));
  Result:=AppendPathDelim(Dir.LongFileName)+SetDirSeparators(DBPath);
end;

function TXDBStorage.CreateRootDir(const ShortFilename, LongFilename: string
  ): TXDBRootDirectory;
begin
  Result:=FindRootWithName(ShortFilename);
  if Result<>nil then
    raise Exception.Create('TXDBCentral.CreateRootDir '+ShortFilename+' already exists: '+Result.LongFileName);
  Result:=TXDBRootDirectory.Create(ShortFilename,LongFilename);
  Roots.Add(Result);
end;

procedure TXDBStorage.DeleteVanishedFiles(KeepFiles: TXDBFilenameToStringTree);
var
  Writing: boolean;
  Changed: Boolean;
  DeletedFiles: TStringList;

  procedure CleanUp(DBDir: TXDBDirectory);
  var
    DBFile: TXDBFile;
    Filename: String;
    Files: TXDBFileList;
  begin
    Files:=TXDBFileList.Create;
    try
      // create a list of files in DB before deleting files
      for DBFile in DBDir do
        Files.Add(DBFile);
      for DBFile in Files do begin
        if DBFile is TXDBDirectory then begin
          CleanUp(TXDBDirectory(DBFile));
        end else begin
          Filename:=DBFile.GetFullFilename;
          if not KeepFiles.Contains(Filename) then begin
            // file does not exist any more => delete
            Changed:=true;
            if Writing then begin
              if (Log<>nil) and Log.Verbose then begin
                if DeletedFiles=nil then
                  DeletedFiles:=TStringList.Create;
                DeletedFiles.Add('file vanished: '+Filename);
              end;
              DBDir.Delete(DBFile);
            end;
          end;
        end;
      end;
    finally
      Files.Free;
    end;
    if (DBDir is TXDBRootDirectory) or (DBDir.Files.Count>0) then
      exit;
    // delete empty directory
    Changed:=true;
    if Writing then
      DBDir.Directory.Delete(DBDir);
  end;

  procedure CleanUpErrorFiles;
  var
    Files: TStringList;
    FileItem: PStringToPointerTreeItem;
    i: Integer;
  begin
    Files:=TStringList.Create;
    try
      for FileItem in ErrorFiles do
        Files.Add(FileItem^.Name);
      for i:=0 to Files.Count-1 do begin
        if not KeepFiles.Contains(Files[i]) then begin
          Changed:=true;
          if Writing then
            ErrorFiles.Remove(Files[i]);
        end;
      end;
    finally
      Files.Free;
    end;
  end;

begin
  DeletedFiles:=nil;
  // fetch WriteLock only if needed
  Changed:=false;
  Writing:=false;
  BeginReading;
  try
    CleanUp(Roots);
    CleanUpErrorFiles;
  finally
    EndReading;
    FreeAndNil(DeletedFiles);
  end;
  if not Changed then exit;
  Writing:=true;
  BeginWriting;
  try
    CleanUp(Roots);
    CleanUpErrorFiles;
  finally
    EndWriting;
    if DeletedFiles<>nil then begin
      // log deleted files
      if Log<>nil then
        Log.Log(etInfo,DeletedFiles);
      DeletedFiles.Free;
    end;
  end;
end;

function TXDBStorage.AddOrReplaceDoc(const DBDir: string; doc: TXDBDocument
  ): TXDBDocument;
var
  Dir: TXDBDirectory;
  aFile: TXDBFile;
  DBFilename: String;
begin
  Result:=nil;
  BeginWriting;
  try
    DBFilename:=DBDir+'/'+doc.Filename;
    Dir:=FindDirectory(DBDir,true,true);
    aFile:=Dir.FindFile(doc.Filename);
    if aFile<>nil then begin
      if aFile is TXDBDirectory then begin
        raise Exception.Create('[TXDBCentral.AddOrReplace] file is directory: '+aFile.GetFullFilename);
      end else if aFile is TXDBFile then begin
        Dir.Delete(aFile);
      end else begin
        raise Exception.Create('[TXDBCentral.AddOrReplace] file '+aFile.GetFullFilename+' is '+aFile.ClassName);
      end;
    end;
    Dir.Add(doc);
    doc.DBAge:=Now;
    Result:=doc;
    ErrorFiles.Remove(DBFilename);
    if FindDocument(DBFilename,false,false)=nil then
      raise Exception.Create('[TXDBCentral.AddOrReplace] added file not found: '+DBFilename);
  finally
    EndWriting;
  end;
end;

function TXDBStorage.ReadXMLFile(const DBFilename: string; out doc: TXDBDocument
  ): boolean;
// DBFilename = DBName+DBPath

  function ReadFile(const Filename: string;
    var ErrMsg: string; var Age: integer): boolean;
  var
    ms: TMemoryStream;
    XMLDoc: TXMLDocument;
    p: SizeInt;
    NewDoc: TXDBDocument;
  begin
    Result:=false;
    try
      Age:=FileAgeUTF8(Filename);
      XMLDoc:=nil;
      ms:=nil;
      NewDoc:=nil;
      try
        // load file into memory
        ms:=TMemoryStream.Create;
        try
          ms.LoadFromFile(Filename);
        except
          on E: Exception do begin
            ErrMsg:='Read error: '+E.Message;
            if Log<>nil then
              Log.Log(etError,['[TXDBCentral.ReadXMLFile] ','unable to read file "'+Filename+'": '+E.Message]);
            exit;
          end;
        end;
        // parse xml
        ms.Position:=0;
        try
          laz2_XMLRead.ReadXMLFile(XMLDoc,ms);
        except
          on E: Exception do begin
            ErrMsg:='Parse error: '+E.Message;
            if Log<>nil then
              Log.Log(etError,['[TXDBCentral.ReadXMLFile] ','Error parsing file "'+Filename+'": '+E.Message]);
            exit;
          end;
        end;
        // convert xml doc to xdb doc
        p:=length(DBFilename);
        while (p>0) and (DBFilename[p]<>'/') do dec(p);
        NewDoc:=TXDBDocument.Create(copy(DBFilename,p+1,length(DBFilename)));
        NewDoc.XMLDoc:=XMLDoc;
        XMLDoc:=nil;
        NewDoc.CreateTreeFromXML;
        NewDoc.ClearXMLDoc;
        // add/replace to storage
        NewDoc.FileAge:=Age;
        NewDoc.LoadAge:=NewDoc.FileAge;
        doc:=AddOrReplaceDoc(copy(DBFilename,1,p-1),NewDoc);
        // success
        NewDoc:=nil;
        Result:=true;
      finally
        NewDoc.Free;
        ms.Free;
        XMLDoc.Free;
      end;
    except
      on E: Exception do begin
        ErrMsg:=E.Message;
        if Log<>nil then
          Log.Log(etError,'[TXDBCentral.ReadXMLFile] DBFilename="'+DBFilename+'": '+E.Message);
        exit;
      end;
    end;
  end;

var
  Filename: String;
  OldDoc: TXDBDocument;
  Age: LongInt;
  ErrMsg: String;
begin
  Result:=false;
  try
    BeginReading;
    try
      Filename:=DBPathToFilename(DBFilename);
    finally
      EndReading;
    end;
  except
    on E: Exception do begin
      Log.Log(etError,'[TXDBCentral.ReadXMLFile] invalid DBFilename "'+DBFilename+'": '+E.Message);
      exit;
    end;
  end;
  //debugln(['TXDBCentral.ReadXMLFile Filename=',Filename]);

  Age:=-1;
  ErrMsg:='';
  Result:=ReadFile(Filename,ErrMsg,Age);
  if Result then exit;
  // read or parsed failed
  // => update FileAge to avoid loading it again
  BeginWriting;
  try
    OldDoc:=FindDocument(DBFilename,false,false);
    if OldDoc<>nil then
      OldDoc.FileAge:=Age;
    ErrorFiles[DBFilename]:=TXDBScanError.Create(DBFilename,Age,ErrMsg);
  finally
    EndWriting;
  end;
end;

function TXDBStorage.CreateRootDirectory(aName, aPath: string
  ): TXDBRootDirectory;
var
  Dir: TXDBRootDirectory;
begin
  Result:=nil;
  Dir:=FindRootWithName(aName);
  if Dir<>nil then
    raise Exception.Create('[TXDBCentral.CreateRootDirectory] There is already a directory with the name "'+aName+'": '+Dir.GetFullFilename);
  Result:=TXDBRootDirectory.Create(aName,ChompPathDelim(aPath));
  Roots.Add(Result);
end;

procedure TXDBStorage.ParallelReadXMLFile(Index: PtrInt; Data: Pointer;
  Item: TMultiThreadProcItem);
var
  DBFilename: String;
  doc: TXDBDocument;
begin
  if Terminating then exit;
  DBFilename:=TStringList(Data)[Index];
  ReadXMLFile(DBFilename,doc);
end;

procedure TXDBStorage.ReadAllXMLFiles;
var
  FileTree: TXDBFilenameToStringTree;

  procedure GatherFiles(const DiskDir, DBDir: string);
  var
    FileInfo: TSearchRec;
    aPath: String;
    DBFilename: TFilename;
  begin
    if Terminating then exit;
    aPath:=AppendPathDelim(DiskDir);
    if FindFirstUTF8(aPath+AllFilesMask,faAnyFile,FileInfo)=0 then begin
      repeat
        if (FileInfo.Name='') or (FileInfo.Name[1]='.') then
          continue;
        if (faDirectory and FileInfo.Attr)>0 then begin
          GatherFiles(aPath+FileInfo.Name,DBDir+'/'+FileInfo.Name);
        end else if CompareFileExt(FileInfo.Name,'xml',false)=0 then begin
          DBFilename:=DBDir+'/'+FileInfo.Name;
          FileTree[DBFilename]:=IntToStr(FileInfo.Time);
        end;
      until FindNextUTF8(FileInfo)<>0;
    end;
    FindCloseUTF8(FileInfo);
  end;

var
  DBFileList: TStringList;
  RootDir: TXDBRootDirectory;
  DBFile: TXDBFile;
  FileItem: PStringToStringTreeItem;
  OldDoc: TXDBDocument;
  Age: Integer;
  LastAge: Integer;
  ScanError: TXDBScanError;
begin
  if Terminating then exit;
  FileTree:=TXDBFilenameToStringTree.Create(false);
  DBFileList:=TStringList.Create;
  try
    // search xml files in all root directories
    BeginReading;
    try
      for DBFile in Roots do begin
        RootDir:=DBFile as TXDBRootDirectory;
        GatherFiles(RootDir.LongFileName,'/'+RootDir.Filename);
        if Terminating then exit;
      end;
    finally
      EndReading;
    end;

    // delete vanished files
    DeleteVanishedFiles(FileTree);
    if Terminating then exit;

    // check if some files have changed
    BeginReading;
    try
      for FileItem in FileTree do begin
        Age:=StrToInt(FileItem^.Value);
        OldDoc:=FindDocument(FileItem^.Name,false,false);
        if (OldDoc<>nil) then begin
          if (OldDoc.FileAge=Age) then continue; // no change since last load
          if Log.Verbose then
            Log.Log(etDebug,'loaded file changed on disk: '+FileItem^.Name);
        end else if ErrorFiles.Contains(FileItem^.Name) then begin
          ScanError:=TXDBScanError(ErrorFiles[FileItem^.Name]);
          LastAge:=ScanError.Age;
          if LastAge=Age then continue; // no change since last load
          if Log.Verbose then
            Log.Log(etDebug,'broken file changed on disk: '+FileItem^.Name);
        end else begin
          if Log.Verbose then
            Log.Log(etDebug,'new file found on disk: '+FileItem^.Name);
        end;
        DBFileList.Add(FileItem^.Name);
      end;
    finally
      EndReading;
    end;
    if Terminating then exit;

    // rescan
    if DBFileList.Count>0 then begin
      if Log<>nil then begin
        Log.Log(etInfo,['parsing ',DBFileList.Count,' files with ',ProcThreadPool.MaxThreadCount,' threads ...']);
        Log.FlushStdOutLog;
      end;
      ProcThreadPool.DoParallel(@ParallelReadXMLFile,0,DBFileList.Count-1,DBFileList);
      if Log<>nil then begin
        Log.Log(etInfo,['parsed ',DBFileList.Count,' files']);
        Log.FlushStdOutLog;
      end;
    end;
  finally
    FileTree.Free;
    DBFileList.Free;
  end;
end;

procedure TXDBStorage.BeginReading;
begin
  fMultiReadExclusiveWrite.Beginread;
end;

procedure TXDBStorage.EndReading;
begin
  fMultiReadExclusiveWrite.Endread;
end;

procedure TXDBStorage.BeginWriting;
begin
  fMultiReadExclusiveWrite.Beginwrite;
end;

procedure TXDBStorage.EndWriting;
begin
  fMultiReadExclusiveWrite.Endwrite;
end;

end.

