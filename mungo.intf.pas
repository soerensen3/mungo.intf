{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit mungo.intf;

{$warn 5023 off : no warning about unused units}
interface

uses
  mungo.intf.action, mungo.intf.editor, mungo.intf.extension, mungo.intf.filepointer, mungo.intf.git, mungo.intf.package, 
  mungo.intf.toolbar, mungo.intf.jsonfile, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('mungo.intf.extension', @mungo.intf.extension.Register);
end;

initialization
  RegisterPackage('mungo.intf', @Register);
end.
