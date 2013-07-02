{ Copyright (C) 2011  Mattias Gaertner  mattias@freepascal.org

  Abstract:
    Classes for storing xml files, directories, documents and nodes
}
unit xdbhttpserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileProcs, xdbfphttpserver, Sockets,
  xdblog;

type
  TXDBHttpServer = class;

  { TXDBHttpServerThread }

  TXDBHttpServerThread = class(TThread)
  public
    Server: TXDBHttpServer;
    constructor Create(aServer: TXDBHttpServer);
    procedure Execute; override;
  end;

  { TXDBHttpServer }

  TXDBHttpServer = class(TFPCustomHttpServer)
  public
    Log: TXDBLog;
    Thread: TXDBHttpServerThread;
    ErrorMsg: string;
    procedure ActivateViaThread;
    destructor Destroy; override;
  published
    Property Active;
    Property Port;
    Property QueueSize;
    Property OnAllowConnect;
    property Threaded;
    Property OnRequest;
  end;

procedure StrToSubNet(const s: string; out IP, Net: in_addr);
function StrToHost(const s: string): in_addr;
function dbgs(const a: TSockAddr): string; overload;
function dbgs(const a: in_addr): string; overload;

implementation

procedure StrToSubNet(const s: string; out IP, Net: in_addr);
var
  p: PChar;
  i: Integer;
  n: Integer;
begin
  IP:=NoAddress;
  Net:=NoNet;
  if s='' then
    raise Exception.Create('empty string found, but IP expected');
  p:=PChar(s);
  i:=1;
  while true do begin
    // read number
    n:=0;
    if (p^ in ['0'..'9']) then begin
      while p^ in ['0'..'9'] do begin
        n:=n*10+ord(p^)-ord('0');
        if n>255 then
          raise Exception.Create('number out of bounds (0-255)');
        inc(p);
      end;
    end else
      raise Exception.Create('decimal expected, but '+dbgstr(p^)+' found');
    if i<=4 then
      IP.s_bytes[i]:=n
    else
      Net.s_bytes[i-4]:=n;
    if p^='.' then begin
      if (i=4) or (i=8) then
        raise Exception.Create('more than four numbers found');
      inc(i);
    end else if p^='/' then begin
      if i<=4 then begin
        inc(i);
        while i<=4 do begin
          IP.s_bytes[i]:=0;
          inc(i);
        end;
      end else
        raise Exception.Create('invalid /');
    end else if p^=#0 then begin
      if i<4 then
        raise Exception.Create('an IP needs four decimals');
      if (i=6) or (i=7) then
        raise Exception.Create('a network mask needs four decimals');
      break;
    end else
      raise Exception.Create('invalid character '+dbgstr(p^));
    inc(p);
  end;
  // i in 4,5,8
  if i=4 then begin
    // single IP adress
    for i:=1 to 4 do Net.s_bytes[i]:=255;
  end else if i=5 then begin
    // IP/Number
    n:=Net.s_bytes[i-4];
    if n>32 then
      raise Exception.Create('invalid mask');
    Net.s_addr:=htonl(cardinal($ffffffff) shl (32-n));
  end else begin
    // IP/Mask
    inc(i);
    while i<=8 do begin
      Net.s_bytes[i-4]:=0;
      inc(i);
    end;
  end;
end;

function StrToHost(const s: string): in_addr;
var
  p: PChar;
  i: Integer;
  n: Integer;
begin
  Result:=NoAddress;
  if s='' then
    raise Exception.Create('empty string found, but IP expected');
  p:=PChar(s);
  i:=1;
  while true do begin
    // read number
    n:=0;
    if (p^ in ['0'..'9']) then begin
      while p^ in ['0'..'9'] do begin
        n:=n*10+ord(p^)-ord('0');
        if n>255 then
          raise Exception.Create('number out of bounds (0-255)');
        inc(p);
      end;
    end else
      raise Exception.Create('decimal expected, but '+dbgstr(p^)+' found');
    Result.s_bytes[i]:=n;
    if p^='.' then begin
      if (i=4) then
        raise Exception.Create('more than four numbers found');
      inc(i);
    end else if p^=#0 then begin
      if i<4 then
        raise Exception.Create('an IP needs four decimals');
      break;
    end else
      raise Exception.Create('invalid character '+dbgstr(p^));
    inc(p);
  end;
end;

function dbgs(const a: TSockAddr): string;
begin
  Result:=dbgs(a.sin_addr)+':'+dbgs(a.sin_port);
end;

function dbgs(const a: in_addr): string;
begin
  Result:=dbgs(a.s_bytes[1])+'.'+
          dbgs(a.s_bytes[2])+'.'+
          dbgs(a.s_bytes[3])+'.'+
          dbgs(a.s_bytes[4]);
end;

procedure TXDBHttpServer.ActivateViaThread;
begin
  TXDBHttpServerThread.Create(Self);
end;

destructor TXDBHttpServer.Destroy;
begin
  inherited Destroy;
  //while Thread<>nil do
  //  Sleep(10);
end;

{ TXDBHttpServerThread }

constructor TXDBHttpServerThread.Create(aServer: TXDBHttpServer);
begin
  Server:=aServer;
  Server.Thread:=Self;
  FreeOnTerminate:=true;
  inherited Create(false);
end;

procedure TXDBHttpServerThread.Execute;
begin
  try
    Server.Active:=true;
  except
    on E: Exception do begin
      Server.ErrorMsg:=E.Message;
      Server.Log.Log(etError,'[TXDBHttpServerThread] '+E.Message);
    end;
  end;
  Server.Thread:=nil;
  Server:=nil;
end;

end.

