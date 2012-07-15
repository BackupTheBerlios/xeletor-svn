unit xdblog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileProcs;

type

  { TXDBLog }

  TXDBLog = class
  private
    FAlwaysFlush: boolean;
    FLogCritSect: TRTLCriticalSection;
    fStdOutLog: TStringList;
    fLastWatchDogMsg: TDateTime;
  public
    Verbose: boolean;
    Quiet: boolean;
    LogFile: string;
    WatchDogIntervalInSec: integer;// write an alive message to log
    constructor Create;
    destructor Destroy; override;
    procedure WatchDog;
    procedure FlushStdOutLog;
    procedure Log(EventType: TEventType; Msgs: TStrings); overload;
    procedure Log(EventType: TEventType; const Msg: String); overload;
    procedure Log(EventType: TEventType; Msg: array of const); overload;
    property AlwaysFlush: boolean read FAlwaysFlush write FAlwaysFlush;
  end;

implementation

{ TXDBLog }

constructor TXDBLog.Create;
begin
  fStdOutLog:=TStringList.Create;
  WatchDogIntervalInSec:=60;
  InitCriticalSection(FLogCritSect);
  fLastWatchDogMsg:=Now;
end;

destructor TXDBLog.Destroy;
begin
  inherited Destroy;
  DoneCriticalsection(FLogCritSect);
  FreeAndNil(fStdOutLog);
end;

procedure TXDBLog.WatchDog;
var
  n: TDateTime;
begin
  n:=Now;
  if Abs(fLastWatchDogMsg-n)*86400>=WatchDogIntervalInSec then begin
    fLastWatchDogMsg:=n;
    Log(etInfo,'WatchDog');
    FlushStdOutLog;
  end;
end;

procedure TXDBLog.FlushStdOutLog;
begin
  EnterCriticalsection(FLogCritSect);
  try
    dbgout(fStdOutLog.Text);
    fStdOutLog.Clear;
  finally
    LeaveCriticalsection(FLogCritSect);
  end;
end;

procedure TXDBLog.Log(EventType: TEventType; Msgs: TStrings);
const
  EventNames: array[TEventType] of string = (
    'Custom','Info','Warning','Error','Debug'
    );
var
  fs: TFileStream;
  Prefix: String;
  i: Integer;
  s: String;
begin
  if (Msgs=nil) or (Msgs.Count=0) then exit;
  Prefix:=FormatDateTime('YYYYMMDD HH:NN:SS',Now)+' '+EventNames[EventType]+': ';
  s:='';
  for i:=0 to Msgs.Count-1 do
    if Msgs[i]<>'' then
      s:=s+Prefix+Msgs[i]+LineEnding;
  if s='' then exit;
  EnterCriticalsection(FLogCritSect);
  try
    if (Verbose or (LogFile='')) and (fStdOutLog<>nil) then begin
      for i:=0 to Msgs.Count-1 do
        if Msgs[i]<>'' then
          fStdOutLog.Add(Prefix+Msgs[i]);
      if AlwaysFlush then
        FlushStdOutLog;
    end;
    if LogFile='' then exit;
    try
      if FileExistsUTF8(LogFile) then
        fs:=TFileStream.Create(UTF8ToSys(LogFile),fmOpenWrite)
      else
        fs:=TFileStream.Create(UTF8ToSys(LogFile),fmCreate);
      try
        fs.Seek(0,fsFromEnd);
        fs.Write(s[1],length(s));
      finally
        fs.Free;
      end;
    except
      on E: Exception do begin
        fStdOutLog.Add('TFTPMirror.Log unable to log to file: '+E.Message);
      end;
    end;
  finally
    LeaveCriticalsection(FLogCritSect);
  end;
end;

procedure TXDBLog.Log(EventType: TEventType; const Msg: String);
var
  Msgs: TStringList;
begin
  if Msg='' then exit;
  Msgs:=TStringList.Create;
  try
    Msgs.Add(Msg);
    Log(EventType,Msgs);
  finally
    Msgs.Free;
  end;
end;

procedure TXDBLog.Log(EventType: TEventType; Msg: array of const);
begin
  Log(EventType,dbgs(Msg));
end;

end.

