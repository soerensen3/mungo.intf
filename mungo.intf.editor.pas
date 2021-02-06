unit mungo.intf.editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson,

  Generics.Collections,

  mungo.intf.filepointer,
  mungo.intf.toolbar;

type
  { TEditor }

  TEditorContainerAlignment = ( ecaLeft, ecaRight, ecaCenter, ecaBottom );

  // Interface to all mungo editors
  TEditor = class abstract
    protected
      FContainer: TEditorContainerAlignment;
      FContainerIdx: Integer;
      FControl: TObject;
      FTabIdx: Integer;
      FToolBar: TToolBarIntf;

    public
      constructor Create( ARootControl: TObject ); overload;
      constructor Create; virtual; abstract; overload;
      destructor Destroy; override;

      procedure EditorFocus; virtual; abstract;
      procedure RestoreState( AState: TJSONObject ); virtual;
      function SaveState: TJSONObject; virtual;

      property Container: TEditorContainerAlignment read FContainer write FContainer;
      property Control: TObject read FControl write FControl;
      property TabIdx: Integer read FTabIdx write FTabIdx;
      property ToolBar: TToolBarIntf read FToolBar write FToolBar;
  end;

  { TEditorTool }

  // Generic tool that is does not have a context to an active file editor
  TEditorTool = class abstract ( TEditor )
    class function GetToolName: String; virtual; abstract;
  end;

  { TEditorContextTool }

  // A tool that has a context to the active file editor. It is notified when another editor becomes focused.
  TEditorContextTool = class abstract ( TEditorTool )
    protected
      FContext: TEditor;

      procedure SetContext(AValue: TEditor); virtual;

    public
      constructor Create; override;

    published
      property Context: TEditor read FContext write SetContext;
  end;

  TEditorToolClass = class of TEditorTool;
  TEditorContextToolClass = class of TEditorContextTool;

  { TEditorToolList }

  TEditorToolList = class ( specialize TObjectList < TEditorTool >)
    function FindByTabCtrl( AControl: TObject ): Integer;
    function FindByTabIdx(AContainer: TEditorContainerAlignment; AIdx: Integer): Integer;
    function FindByClass( AToolClass: TEditorToolClass ): Integer;
  end;

  { TEditorContextToolList }

  TEditorContextToolList = class ( specialize TObjectList < TEditorContextTool >)
    function FindByTabCtrl( AControl: TObject ): Integer;
    function FindByTabIdx(AContainer: TEditorContainerAlignment; AIdx: Integer): Integer;
    function FindByClass( AToolClass: TEditorContextToolClass ): Integer;
  end;

  { TEditorFile }

  TEditorFile = class abstract ( TEditor )
    protected type
      TOnChangeEvent = procedure ( ASender: TEditor; AModified: Boolean ) of object;

    protected
      FOnModify: TOnChangeEvent;
      FFileInfo: TFilePointer;
      FModified: Boolean;
      FPersistent: Boolean;
      FOnSetPersistent: TOnChangeEvent;

      procedure SetPersistent(AValue: Boolean); virtual;
      procedure SetModified(AValue: Boolean); virtual;

    public
      constructor Create( ARootControl: TObject; AFileInfo: TFilePointer ); virtual; overload;
      constructor Create( ARootControl: TObject; AFileName: String ); overload;

      function SaveState: TJSONObject; override;
      class function FileMatch( AFileInfo: TFilePointer ): Boolean; virtual; abstract;

      property FileInfo: TFilePointer read FFileInfo;
      property Modified: Boolean read FModified write SetModified;
      property OnModify: TOnChangeEvent read FOnModify write FOnModify;
      property OnSetPersistent: TOnChangeEvent read FOnSetPersistent write FOnSetPersistent;
      property Persistent: Boolean read FPersistent write SetPersistent;
  end;

  TEditorFileClass = class of TEditorFile;

  { TEditorFileList }

  TEditorFileList = class ( specialize TObjectList < TEditorFile >)
    function FindByTabCtrl( AControl: TObject ): Integer;
    function FindByTabIdx( AContainerIdx: Integer; AIdx: Integer ): Integer;
    function FindByTabFilePointer( AFilePointer: TFilePointer ): Integer;
  end;

  { TEditorIntf }

  TEditorIntf = class abstract
    protected
      FFileEditors: TEditorFileList;
      FNonPersistentEditor: TEditorFile;
      FTools: TEditorToolList;
      FContextTools: TEditorContextToolList;

      procedure SetNonPersistentEditor(AValue: TEditorFile);

    public
      constructor Create;
      destructor Destroy; override;

      function AddFileTab( AFileInfo: TFilePointer; const APersistent: Boolean = True  ): TEditorFile; virtual; abstract;
      function ActivateOrAddFileTab( AFileInfo: TFilePointer; const APersistent: Boolean = True ): TEditorFile; virtual; abstract;
      procedure CloseFileTab( AEditor: TEditorFile ); virtual; abstract;
      function AddToolTab( AClass: TEditorToolClass ): TEditorTool; virtual; abstract;
      function ActivateOrAddToolTab( AClass: TEditorToolClass ): TEditorTool; virtual; abstract;
      function AddContextToolTab( AClass: TEditorContextToolClass ): TEditorContextTool; virtual; abstract;
      function CreateToolBar( ARootControl: TObject ): TToolBarIntf; virtual; abstract;
      function SaveState: TJSONObject; virtual;
      procedure RestoreState( AState: TJSONObject ); virtual;

    published
      property FileEditors: TEditorFileList read FFileEditors;
      property NonPersistentEditor: TEditorFile read FNonPersistentEditor write SetNonPersistentEditor;
      property Tools: TEditorToolList read FTools;
      property ContextTools: TEditorContextToolList read FContextTools;
  end;

var
  EditorIntf: TEditorIntf = nil;


implementation

{ TEditor }

constructor TEditor.Create(ARootControl: TObject);
begin
  Control:= ARootControl;
  Container:= ecaLeft;
  Create;
end;

destructor TEditor.Destroy;
begin
  FreeAndNil( FToolBar );
  FreeAndNil( FControl );
  inherited Destroy;
end;

procedure TEditor.RestoreState(AState: TJSONObject);
begin
  Container:= TEditorContainerAlignment( AState.Get( 'container', 0 ));
end;

function TEditor.SaveState: TJSONObject;
begin
  Result:= TJSONObject.Create();
  Result.Add( 'container', Integer( Container ));
end;

{ TEditorContextTool }

constructor TEditorContextTool.Create;
begin
  Container:= ecaRight;
end;

procedure TEditorContextTool.SetContext(AValue: TEditor);
begin
  if FContext=AValue then Exit;
  FContext:=AValue;
end;

{ TEditorContextToolList }

function TEditorContextToolList.FindByTabCtrl(AControl: TObject): Integer;
var
  i: Integer;
begin
  Result:= -1;

  for i:= 0 to Count - 1 do
    if ( Items[ i ].Control = AControl ) then begin
      Result:= i;
      break;
    end;
end;

function TEditorContextToolList.FindByTabIdx(AContainer: TEditorContainerAlignment; AIdx: Integer): Integer;
var
  i: Integer;
begin
  Result:= -1;

  for i:= 0 to Count - 1 do
    if (( Items[ i ].Container = AContainer ) and ( Items[ i ].TabIdx = AIdx )) then begin
      Result:= i;
      break;
    end;
end;

function TEditorContextToolList.FindByClass(AToolClass: TEditorContextToolClass): Integer;
var
  i: Integer;
begin
  Result:= -1;

  for i:= 0 to Count - 1 do
    if ( Items[ i ].ClassType = AToolClass ) then begin
      Result:= i;
      break;
    end;
end;

{ TEditorToolList }

function TEditorToolList.FindByTabCtrl(AControl: TObject): Integer;
var
  i: Integer;
begin
  Result:= -1;

  for i:= 0 to Count - 1 do
    if ( Items[ i ].Control = AControl ) then begin
      Result:= i;
      break;
    end;
end;

function TEditorToolList.FindByTabIdx(AContainer: TEditorContainerAlignment; AIdx: Integer): Integer;
var
  i: Integer;
begin
  Result:= -1;

  for i:= 0 to Count - 1 do
    if (( Items[ i ].Container = AContainer ) and (  Items[ i ].TabIdx = AIdx )) then begin
      Result:= i;
      break;
    end;
end;

function TEditorToolList.FindByClass(AToolClass: TEditorToolClass): Integer;
var
  i: Integer;
begin
  Result:= -1;

  for i:= 0 to Count - 1 do
    if ( Items[ i ].ClassType = AToolClass ) then begin
      Result:= i;
      break;
    end;
end;


{ TEditorFileList }

function TEditorFileList.FindByTabCtrl(AControl: TObject): Integer;
var
  i: Integer;
begin
  Result:= -1;

  for i:= 0 to Count - 1 do
    if ( Items[ i ].Control = AControl ) then begin
      Result:= i;
      break;
    end;
end;

function TEditorFileList.FindByTabIdx( AContainerIdx: Integer; AIdx: Integer ): Integer;
var
  i: Integer;
begin
  Result:= -1;

  for i:= 0 to Count - 1 do
    if ({( Items[ i ].ContainerIdx = AContainerIdx ) and }( Items[ i ].TabIdx = AIdx )) then begin
      Result:= i;
      break;
    end;
end;

function TEditorFileList.FindByTabFilePointer( AFilePointer: TFilePointer ): Integer;
var
  i: Integer;
begin
  Result:= -1;

  for i:= 0 to Count - 1 do
    if ( Items[ i ].FileInfo = AFilePointer ) then begin
      Result:= i;
      break;
    end;
end;

{ TEditorFile }

procedure TEditorFile.SetPersistent(AValue: Boolean);
begin
//  if FPersistent=AValue then Exit;
  FPersistent:=AValue;
  if (( AValue ) and ( Self = EditorIntf.FNonPersistentEditor )) then
    EditorIntf.FNonPersistentEditor:= nil;
  if ( Assigned( OnSetPersistent )) then
    OnSetPersistent( Self, AValue );
end;

procedure TEditorFile.SetModified(AValue: Boolean);
begin
  if FModified=AValue then Exit;
  FModified:=AValue;

  if ( Assigned( FOnModify )) then
    FOnModify( Self, Modified );

  if ( AValue ) then
    Persistent:= True;
end;

function TEditorFile.SaveState: TJSONObject;
begin
  Result:= inherited SaveState;
  Result.Add( 'file', FileInfo.FileName );
  Result.Add( 'class', ClassName );
end;


constructor TEditorFile.Create(ARootControl: TObject; AFileInfo: TFilePointer);
begin
  FFileInfo:= AFileInfo;
  inherited Create( ARootControl );
  Container:= ecaCenter;
end;

constructor TEditorFile.Create(ARootControl: TObject; AFileName: String);
begin
  Create( ARootControl, FilePointers.GetFilePointer( AFileName ));
end;

{ TEditorIntf }

procedure TEditorIntf.SetNonPersistentEditor(AValue: TEditorFile);
begin
  if FNonPersistentEditor=AValue then Exit;

  if ( Assigned( FNonPersistentEditor )) then
    CloseFileTab( FNonPersistentEditor );

  FNonPersistentEditor:=AValue;
end;

constructor TEditorIntf.Create;
begin
  FFileEditors:= TEditorFileList.Create();
  FTools:= TEditorToolList.Create();
  FContextTools:= TEditorContextToolList.Create();
end;

destructor TEditorIntf.Destroy;
begin
  FreeAndNil( FFileEditors );
  FreeAndNil( FTools );
  FreeAndNil( FContextTools );
  inherited Destroy;
end;

function TEditorIntf.SaveState: TJSONObject;
  function SaveFileEditors: TJSONArray;
  var
    Editor: TEditorFile;
  begin
    Result:= TJSONArray.Create();
    for Editor in FileEditors do
      Result.Add( Editor.SaveState );
  end;

  function SaveTools: TJSONObject;
  var
    Editor: TEditorTool;
  begin
    Result:= TJSONObject.Create();
    for Editor in Tools do
      Result.Add( Editor.ClassName, Editor.SaveState );
  end;

  function SaveContextTools: TJSONObject;
  var
    Editor: TEditorContextTool;
  begin
    Result:= TJSONObject.Create();
    for Editor in ContextTools do
      Result.Add( Editor.ClassName, Editor.SaveState );
  end;

begin
  Result:= TJSONObject.Create();

  Result.Add( 'file-editors', SaveFileEditors );
  Result.Add( 'tools', SaveTools );
  Result.Add( 'context-tools', SaveContextTools );
end;

procedure TEditorIntf.RestoreState(AState: TJSONObject);
  procedure RestoreTools( AState: TJSONObject );
  var
    Editor: TEditorTool;
    J: TJSONObject;
  begin
    for Editor in Tools do begin
      J:= AState.Get( Editor.ClassName, default( TJSONObject ));
      if ( Assigned( J )) then
         Editor.RestoreState( J );
    end;
  end;

  procedure RestoreContextTools( AState: TJSONObject );
  var
    Editor: TEditorContextTool;
    J: TJSONObject;
  begin
    for Editor in ContextTools do begin
      J:= AState.Get( Editor.ClassName, default( TJSONObject ));
      if ( Assigned( J )) then
         Editor.RestoreState( J );
    end;
  end;

  procedure RestoreFileEditors( AState: TJSONArray );
  var
    Editor: TEditorFile;
    S: TJSONUnicodeStringType;
    F: TJSONEnum;
  begin
    for F in AState do begin
      S:= TJSONObject( F.Value ).Get( 'file', '' );

      if ( S > '' ) then begin
         Editor:= AddFileTab( FilePointers.GetFilePointer( S ));
         Editor.RestoreState( TJSONObject( F.Value ));
      end;
    end;
  end;


var
  j: TJSONObject;
  ja: TJSONArray;
begin
  j:= AState.Get( 'tools', default( TJSONObject ));
  if ( Assigned( j )) then
    RestoreTools( j );

  j:= AState.Get( 'context-tools', default( TJSONObject ));
  if ( Assigned( j )) then
    RestoreContextTools( j );

  ja:= AState.Get( 'file-editors', default( TJSONArray ));
  if ( Assigned( ja )) then
    RestoreFileEditors( ja );
end;

end.

