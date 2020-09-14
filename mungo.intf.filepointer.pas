unit mungo.intf.filepointer;

{$mode delphi}{$H+}

interface

uses
  generics.Collections,
  LazFileUtils,
  Classes, SysUtils;

type

  TFilePointer = class;

  // callback is executed if the file on the disk is changed. If do reload is set to True in the callback the file is reloaded afterwards.
  TFilePointerOnFileChanged = procedure ( Sender: TFilePointer; out DoReload: Boolean ) of object;
  // callback is executed if the file name property is changed.
  TFilePointerOnFileNameChanged = procedure ( Sender: TFilePointer; var ANewValue: String ) of object;

  { TFilePointer

    A pointer to a file which may or may not exist. It is used internally to manage all open files. If Reload is executed the file
    status is updated. True is returned if the file changed. If assigned the file changed callback is run.
  }
  TFilePointer = class ( TPersistent )
    private
      FExtension: String;
      FLastFileAge: LongInt;
      FFileName: String;
      FOnFileChange: TFilePointerOnFileChanged;
      FOnFileNameChange: TFilePointerOnFileNameChanged;
      FUserPointer: Pointer;

      function GetFileAge: LongInt;
      procedure SetFileName( AValue: String );
      procedure FileChanged( out DoReload: Boolean ); virtual;
      procedure FileNameChanged( var AFileName: String ); virtual;

    public
      constructor Create( AFileName: String; AUserPointer: Pointer = nil ); // Creates a file pointer with the given name. Optionally a user
                                                                            // may be stored with the file pointer. The instance of the file
                                                                            // pointer is automatically added to the P3DFilePointers var.
      destructor Destroy; override;

      function CheckForChange: Boolean; // Checks if a file on the disk has changed. If assigned the file changed callback is run on change.
                                        // If the file does not exist the return value is false. The file status is only updated by this function
                                        // if the DoReload parameter in the callback is set to true.
      function CheckFileExists: Boolean; // Checks if the target of the file pointer is valid.
      procedure Reload; virtual; // Updates the file's status.

      property UserPointer: Pointer read FUserPointer write FUserPointer; // A user pointer to store custom values.

    published
      property FileName: String read FFileName write SetFileName; // Full filename to a file that may or may not exist.
      property LastFileAge: LongInt read FLastFileAge; // The age of the file when it was changed. The returned value is a LongInt generated
                                                       // by SysUtil's FileAge function. If the file does not exist this value is -1.
      property Extension: String read FExtension;
      property OnFileChange: TFilePointerOnFileChanged read FOnFileChange write FOnFileChange;
      // Executed by CheckForChange
      property OnFileNameChange: TFilePointerOnFileNameChanged read FOnFileNameChange write FOnFileNameChange;
      // Is executed when the filename property is changed
  end;

  { TFilePointerList

    Internally used to manage files opened by the engine. CheckForChange needs to be called in a regular interval if you want notifications to work. This
    is should be done in the main loop of the application with FilePointers.CheckForChange.
  }

  TFilePointerList = class( TObjectList < TFilePointer > )
    function GetFilePointer( AFileName: String ): TFilePointer;
    function FindFile( AFileName: String ): TFilePointer;
    procedure CheckForChange;
  end;

var
  FilePointers: TFilePointerList = nil;

implementation

function TFilePointerList.GetFilePointer(AFileName: String): TFilePointer;
begin
  Result:= FindFile( AFileName );
  if ( not Assigned( Result )) then
    Result:= TFilePointer.Create( AFileName );
end;

function TFilePointerList.FindFile(AFileName: String): TFilePointer;
var
  F: TFilePointer;
  FileName: String;
begin
  Result:= nil;
  FileName:= CreateAbsolutePath( AFileName, GetCurrentDirUTF8 );
  for F in Self do
    if ( F.FileName = FileName ) then begin
      Result:= F;
      break;
    end;
end;

procedure TFilePointerList.CheckForChange;
var
  Watch: TFilePointer;
begin
  for Watch in Self do
    Watch.CheckForChange;
end;


{ TFilePointer }

function TFilePointer.GetFileAge: LongInt;
begin
  Result:= FileAge( FFileName );
end;

procedure TFilePointer.SetFileName(AValue: String);
var
  FN: String;
begin
  if ( FFileName = AValue ) then
    Exit;

  FN:= CreateAbsolutePath( AValue, GetCurrentDirUTF8 );
  FileNameChanged( FN );
  FFileName:= FN;
  Reload;
end;

procedure TFilePointer.FileChanged(out DoReload: Boolean);
begin
  if ( Assigned( OnFileChange )) then
    OnFileChange( Self, DoReload )
  else
    DoReload:= False;
end;

procedure TFilePointer.FileNameChanged(var AFileName: String);
begin
  if ( Assigned( OnFileNameChange )) then
    OnFileNameChange( Self, AFileName );
end;

constructor TFilePointer.Create(AFileName: String; AUserPointer: Pointer);
begin
  inherited Create;
  FileName:= AFileName;
  UserPointer:= AUserPointer;
  Reload;

  if ( Assigned( FilePointers )) then
    FilePointers.Add( Self );

  //Add Self to FileWatches
  //TP3DData has Filewatch
  //TP3DData has also a LibName which should be unique in Scope
  //Scope means the engine's searchpaths
  //TP3DDatablock has only a string to the corresponding object, <LibName> or similar will reference a different library
  //Descendants, like textures or meshes, can define their own filewatch if needed
end;

destructor TFilePointer.Destroy;
begin
  if ( Assigned( FilePointers )) then
    FilePointers.Remove( Self );
  inherited Destroy;
end;

function TFilePointer.CheckForChange: Boolean;
var
  DoReload: Boolean;
begin
  Result:= ( GetFileAge <> FLastFileAge );

  if (( Result )) then begin
    FileChanged( DoReload );
    if ( DoReload ) then // only reload if the callback asks for it
      Reload;
  end;
end;

function TFilePointer.CheckFileExists: Boolean;
begin
  Result:= SysUtils.FileExists( FileName );
end;

procedure TFilePointer.Reload;
begin
  FLastFileAge:= GetFileAge;
  FExtension:= ExtractFileExt( FileName );
end;

initialization
  FilePointers:= TFilePointerList.Create();

finalization
  FreeAndNil( FilePointers );

end.

