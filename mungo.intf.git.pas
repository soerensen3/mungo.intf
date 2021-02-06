unit mungo.intf.git;

{$mode objfpc}{$H+}
{$ModeSwitch nestedprocvars}

interface

uses
  Classes, SysUtils;

type

  TGitFileStatusFlag = ( gfsUnmodified,
                         gfsUntracked,       gfsIgnored,
                         gfsStagedModified,  gfsUnstagedModified,
                         gfsStagedAdded,     gfsUnstagedAdded,
                         gfsStagedDeleted,   gfsUnstagedDeleted,
                         gfsStagedRenamed,   gfsUnstagedRenamed,
                         gfsStagedCopied,    gfsUnstagedCopied
                       );

  TGitFileStatus = set of TGitFileStatusFlag;

  TGitStatusFileRecord = record
    FileName: String;
    OrigFileName: String;
    Status: TGitFileStatus;
  end;

  TGitStatusCallBack = procedure ( AFileStatus: TGitStatusFileRecord ) is nested;

  { TGitStatusIntf }

  TGitStatusIntf = class abstract
    constructor Create;
    destructor Destroy; override;

    procedure GetRepoStatus( ARootPath: String; ACallBack: TGitStatusCallBack ); virtual; abstract;
  end;

var
  GitStatus: TGitStatusIntf = nil;

implementation

{ TGitStatusIntf }

constructor TGitStatusIntf.Create;
begin
  GitStatus:= Self;
end;

destructor TGitStatusIntf.Destroy;
begin
  if ( GitStatus = Self ) then
    GitStatus:= nil;
  inherited Destroy;
end;

finalization
  FreeAndNil( GitStatus );

end.

