{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit XeletorClient;

interface

uses
  XDBUtils, XDBFiles, XDBProcess, xdblog, XFileWatch, XDBCGI, XPath_Simple, 
  xdbfphttpclient, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('XeletorClient', @Register);
end.
