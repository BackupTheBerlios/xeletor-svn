{ Copyright (C) 2012 Mattias Gaertner mattias@freepascal.org

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

program runtests;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, fpcunitconsolerunner, consoletestrunner, dom, fpcunit, testregistry,
  testxdb, testglobals;

type

  { TXeletorTestRunner }

  TXeletorTestRunner = class(TTestRunner)
  private
    FServerHost: string;
  public
    procedure AppendLongOpts; override;
    procedure ParseOptions; override;
    procedure WriteCustomHelp; override;

    procedure ExtendXmlDocument(Doc: TXMLDocument); override;

    property ServerHost: string read FServerHost write FServerHost;
  end;

{ TXeletorTestRunner }

procedure TXeletorTestRunner.AppendLongOpts;
begin
  inherited AppendLongOpts;
  LongOpts.Add('host');
end;

procedure TXeletorTestRunner.ParseOptions;
begin
  inherited ParseOptions;
  if HasOption('host') then
    ServerHost:=GetOptionValue('host');
end;

procedure TXeletorTestRunner.WriteCustomHelp;
begin
  inherited WriteCustomHelp;
  writeln('  --host=<server host> : if given execute server tests');
end;

procedure TXeletorTestRunner.ExtendXmlDocument(Doc: TXMLDocument);
var
  env: TDOMElement;
  procedure AddElement(const name, value: string);
  var
    n: TDOMElement;
  begin
    n := Doc.CreateElement(UTF8Decode(name));
    n.AppendChild(Doc.CreateTextNode(UTF8Decode(value)));
    env.AppendChild(n);
  end;
begin
  inherited ExtendXmlDocument(Doc);
  env := Doc.CreateElement('Environment');
  AddElement('serverhost','');
  Doc.FirstChild.AppendChild(env);
end;

var
  App: TXeletorTestRunner;
begin
  App := TXeletorTestRunner.Create(nil);
  App.Initialize;
  App.Title := 'FPCUnit Console runner for the Xeletor Test Suite.';
  App.Run;
  App.Free;
end.

