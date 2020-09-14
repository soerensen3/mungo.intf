unit mungo.intf.editor;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,

  Generics.Collections,

  mungo.intf.filepointer;

type
  TToolBarIntf = class;

  { TEditor }

  TEditor = class abstract
    protected
      FContainerIdx: Integer;
      FControl: TObject;
      FTabIdx: Integer;
      FToolBar: TToolBarIntf;

    public
      constructor Create( ARootControl: TObject ); overload;
      constructor Create; virtual; abstract; overload;
      destructor Destroy; override;

      procedure EditorFocus; virtual; abstract;

      property Control: TObject read FControl write FControl;
      property TabIdx: Integer read FTabIdx write FTabIdx;
      property ContainerIdx: Integer read FContainerIdx write FContainerIdx;
      property ToolBar: TToolBarIntf read FToolBar write FToolBar;
  end;

  { TEditorTool }

  TEditorTool = class abstract ( TEditor )
    class function GetToolName: String; virtual; abstract;
  end;

  { TEditorContextTool }

  TEditorContextTool = class abstract ( TEditorTool )
    private
      FContext: TEditor;

    protected
      procedure SetContext(AValue: TEditor); virtual;

    published
      property Context: TEditor read FContext write SetContext;
  end;

  TEditorContextToolClass = class of TEditorContextTool;
  TEditorToolClass = class of TEditorTool;

  { TEditorToolList }

  TEditorToolList = class ( TObjectList < TEditorTool >)
    function FindByTabIdx( AContainerIdx: Integer; AIdx: Integer ): Integer;
    function FindByClass( AToolClass: TEditorToolClass ): Integer;
  end;

  { TEditorContextToolList }

  TEditorContextToolList = class ( TObjectList < TEditorContextTool >)
    function FindByTabIdx( AContainerIdx: Integer; AIdx: Integer ): Integer;
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

      class function FileMatch( AFileInfo: TFilePointer ): Boolean; virtual; abstract;

      property FileInfo: TFilePointer read FFileInfo;
      property Modified: Boolean read FModified write SetModified;
      property OnModify: TOnChangeEvent read FOnModify write FOnModify;
      property OnSetPersistent: TOnChangeEvent read FOnSetPersistent write FOnSetPersistent;
      property Persistent: Boolean read FPersistent write SetPersistent;
  end;

  { TEditorFileList }

  TEditorFileList = class ( TObjectList < TEditorFile >)
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
      function AddContextToolTabBottom( AClass: TEditorContextToolClass ): TEditorContextTool; virtual; abstract;
      function CreateToolBar( ARootControl: TObject ): TToolBarIntf; virtual; abstract;

    published
      property FileEditors: TEditorFileList read FFileEditors;
      property NonPersistentEditor: TEditorFile read FNonPersistentEditor write SetNonPersistentEditor;
      property Tools: TEditorToolList read FTools;
      property ContextTools: TEditorContextToolList read FContextTools;
  end;

  TSourceTreeNodeData = record
    NodeType: Integer;
    Caption: String;
    TextPos: TPoint;
  end;

  PSourceTreeNodeData = ^TSourceTreeNodeData;

  { TSourceTreeIntf }

  TSourceTreeIntf = class abstract ( TEditorContextTool )
    public
      constructor Create; override;
      destructor Destroy; override;

      function AddNode( AParent: Pointer; ANodeType: Integer; AText: String; ATextPos: TPoint ): Pointer; virtual; abstract;
      procedure RemoveNode( ANode: Pointer ); virtual; abstract;
      procedure ClearNodes; virtual; abstract;
  end;

  TMessagesData = record
    MsgType: Integer;
    Message: String;
    TextPos: TPoint;
  end;

  PMessageData = ^TMessagesData;


  { TMessageIntf }

  TMessageIntf = class abstract ( TEditorContextTool )
    public
      constructor Create; override;
      destructor Destroy; override;

      function AddMessage( AMsgType: Integer; AText: String; ATextPos: TPoint ): Pointer; virtual; abstract;
      procedure ClearMessages; virtual; abstract;
  end;

  { TAction }

  TAction = class ( TPersistent ) // implements FPObserver
    private
      FCaption: String;
      FEnabled: Boolean;
      FGlyphIndex: Integer;
      FHint: String;
      FImages: TPersistent;
      FOnExecute: TNotifyEvent;
      FIsUpdating: Boolean;

    protected
      procedure SeString(AValue: String); virtual;
      procedure SetGlyphIndex(AValue: Integer); virtual;
      procedure SetHint(AValue: String); virtual;
      procedure SetImages(AValue: TPersistent); virtual;
      procedure SetEnabled(AValue: Boolean); virtual;
      procedure DoNotifyOnChanged;

    public
      constructor Create( ACaption: String; AOnExecute: TNotifyEvent; const AHint: String = ''; const AGlyphIndex: Integer = -1 ); virtual;

      procedure Execute;
      procedure BeginUpdate;
      procedure EndUpdate;

    published
      property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
      property GlyphIndex: Integer read FGlyphIndex write SetGlyphIndex;
      property Images: TPersistent read FImages write SetImages;
      property Caption: String read FCaption write SeString;
      property Hint: String read FHint write SetHint;
      property Enabled: Boolean read FEnabled write SetEnabled;
  end;

  TActionList = class ( TObjectList < TEditorFile >);

  { TActionIntf }

  TActionIntf = class
    private
      FActions: TActionList;

    public
      procedure RegisterAction( AAction: TAction ); virtual;
      procedure UnregisterAction( AAction: TAction ); virtual;

    published
      property Actions: TActionList read FActions;
  end;

  { TToolBarIntf }

  TToolBarIntf = class
    protected
      FControl: TObject;

    public
      function AddButton( AAction: TAction ): TObject; virtual; abstract;
      function AddSpacer(): TObject; virtual; abstract;

    published
      property Control: TObject read FControl write FControl;
  end;


var
  EditorIntf: TEditorIntf = nil;
  SourceTreeIntf: TSourceTreeIntf = nil;
  MessageIntf: TMessageIntf = nil;
  ActionIntf: TActionIntf = nil;


implementation

{ TActionIntf }

procedure TActionIntf.RegisterAction(AAction: TAction);
begin

end;

procedure TActionIntf.UnregisterAction(AAction: TAction);
begin

end;

{ TEditor }

constructor TEditor.Create(ARootControl: TObject);
begin
  Control:= ARootControl;
  Create;
end;

destructor TEditor.Destroy;
begin
  FreeAndNil( FToolBar );
  FreeAndNil( FControl );
  inherited Destroy;
end;

{ TAction }

procedure TAction.SetEnabled(AValue: Boolean);
begin
  if FEnabled=AValue then Exit;
  FEnabled:=AValue;

  DoNotifyOnChanged;
end;

procedure TAction.DoNotifyOnChanged;
begin
  if ( not FIsUpdating ) then
    FPONotifyObservers( Self, ooChange, nil );
end;

constructor TAction.Create(ACaption: String; AOnExecute: TNotifyEvent; const AHint: String; const AGlyphIndex: Integer);
begin
  inherited Create;

  BeginUpdate;
  Caption:= ACaption;
  OnExecute:= AOnExecute;
  Hint:= AHint;
  GlyphIndex:= AGlyphIndex;
  Enabled:= True;
  EndUpdate;
end;

procedure TAction.Execute;
begin
  if ( Assigned( OnExecute )) then
    OnExecute( Self );
end;

procedure TAction.BeginUpdate;
begin
  FIsUpdating:= True;
end;

procedure TAction.EndUpdate;
begin
  FIsUpdating:= False;
  DoNotifyOnChanged;
end;

procedure TAction.SeString(AValue: String);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;

  DoNotifyOnChanged;
end;

procedure TAction.SetGlyphIndex(AValue: Integer);
begin
  if FGlyphIndex=AValue then Exit;
  FGlyphIndex:=AValue;

  DoNotifyOnChanged;
end;

procedure TAction.SetHint(AValue: String);
begin
  if FHint=AValue then Exit;
  FHint:=AValue;

  DoNotifyOnChanged;
end;

procedure TAction.SetImages(AValue: TPersistent);
begin
  if FImages=AValue then Exit;
  FImages:=AValue;

  DoNotifyOnChanged;
end;

{ TMessageIntf }

constructor TMessageIntf.Create;
begin
  if ( not Assigned( MessageIntf )) then
    MessageIntf:= Self;
end;

destructor TMessageIntf.Destroy;
begin
  if ( MessageIntf = Self ) then
    MessageIntf:= nil;

  inherited Destroy;
end;

{ TSourceTreeIntf }

constructor TSourceTreeIntf.Create;
begin
  if ( not Assigned( SourceTreeIntf )) then
    SourceTreeIntf:= Self;
end;

destructor TSourceTreeIntf.Destroy;
begin
  if ( SourceTreeIntf = Self ) then
    SourceTreeIntf:= nil;

  inherited Destroy;
end;

{ TEditorContextTool }

procedure TEditorContextTool.SetContext(AValue: TEditor);
begin
  if FContext=AValue then Exit;
  FContext:=AValue;
end;

{ TEditorContextToolList }

function TEditorContextToolList.FindByTabIdx(AContainerIdx: Integer; AIdx: Integer): Integer;
var
  i: Integer;
begin
  Result:= -1;

  for i:= 0 to Count - 1 do
    if (( Items[ i ].ContainerIdx = AContainerIdx ) and (  Items[ i ].TabIdx = AIdx )) then begin
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

function TEditorToolList.FindByTabIdx(AContainerIdx: Integer; AIdx: Integer): Integer;
var
  i: Integer;
begin
  Result:= -1;

  for i:= 0 to Count - 1 do
    if (( Items[ i ].ContainerIdx = AContainerIdx ) and (  Items[ i ].TabIdx = AIdx )) then begin
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


constructor TEditorFile.Create(ARootControl: TObject; AFileInfo: TFilePointer);
begin
  FFileInfo:= AFileInfo;
  inherited Create( ARootControl );
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

end.

