unit mungo.intf.extension;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Generics.Collections,

  LazarusPackageIntf,

  mungo.intf.editor;

type

  { TExtensionIntf }

  TExtensionIntf = class
    protected
      FTools: specialize TList < TEditorToolClass >;
      FContextTools: specialize TList < TEditorContextToolClass >;
      FFileEditors: specialize TList<TEditorFileClass>;

      procedure RegisterUnit( const TheUnitName: string; RegisterProc: TRegisterProc );

    public
      constructor Create;
      destructor Destroy; override;

      procedure RegisterTool( AToolClass: TEditorToolClass );
      procedure RegisterContextTool( AToolClass: TEditorContextToolClass );
      procedure RegisterFileEditor( AEditorClass: TEditorFileClass );

      property Tools: specialize TList < TEditorToolClass > read FTools;
      property ContextTools: specialize TList < TEditorContextToolClass > read FContextTools;
      property FileEditors: specialize TList < TEditorFileClass > read FFileEditors;
  end;

  procedure ExtensionIntfInit;
  procedure ExtensionIntfFinalize;

  procedure Register;


var
  ExtensionIntf: TExtensionIntf = nil;

implementation

procedure ExtensionIntfInit;
begin
  if ( not Assigned( ExtensionIntf )) then begin
    ExtensionIntf:= TExtensionIntf.Create;
    RegisterUnitProc:= @ExtensionIntf.RegisterUnit;
  end;
end;

procedure ExtensionIntfFinalize;
begin
  FreeAndNil( ExtensionIntf );
end;

procedure Register;
begin
  ExtensionIntfInit;
end;

{ TExtensionIntf }

constructor TExtensionIntf.Create;
begin
  FTools:= specialize TList < TEditorToolClass >.Create;
  FContextTools:= specialize TList < TEditorContextToolClass >.Create;
  FFileEditors:= specialize TList < TEditorFileClass >.Create;
end;

destructor TExtensionIntf.Destroy;
begin
  FreeAndNil( FTools );
  FreeAndNil( FContextTools );
  FreeAndNil( FFileEditors );
  inherited Destroy;
end;

procedure TExtensionIntf.RegisterTool(AToolClass: TEditorToolClass);
begin
  WriteLn( 'Register tool ', AToolClass.ClassName );
  Tools.Add( AToolClass );
end;

procedure TExtensionIntf.RegisterContextTool(AToolClass: TEditorContextToolClass);
begin
  WriteLn( 'Register context tool ', AToolClass.ClassName );
  ContextTools.Add( AToolClass );
end;

procedure TExtensionIntf.RegisterFileEditor(AEditorClass: TEditorFileClass);
begin
  WriteLn( 'Register file editor ', AEditorClass.ClassName );
  FileEditors.Add( AEditorClass );
end;

procedure TExtensionIntf.RegisterUnit(const TheUnitName: string; RegisterProc: TRegisterProc);
begin
  WriteLn( 'Register unit ', TheUnitName );
  RegisterProc();
end;

initialization
  ExtensionIntfInit;

finalization
  ExtensionIntfFinalize;

end.

