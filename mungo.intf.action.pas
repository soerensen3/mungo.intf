unit mungo.intf.action;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  Generics.Collections;

type

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
      FOnToggle: TNotifyEvent;

    protected
      procedure SeString(AValue: String); virtual;
      procedure SetGlyphIndex(AValue: Integer); virtual;
      procedure SetHint(AValue: String); virtual;
      procedure SetImages(AValue: TPersistent); virtual;
      procedure SetEnabled(AValue: Boolean); virtual;
      procedure DoNotifyOnChanged;

    public
      constructor Create( ACaption: String; AOnExecute: TNotifyEvent; const AHint: String = ''; const AGlyphIndex: Integer = -1 ); virtual;

      procedure Execute; virtual;
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

  { TToggleAction }

  TToggleAction = class ( TAction )
    private
      FToggled: Boolean;

    protected
      procedure SetToggled(AValue: Boolean); virtual;

    public
      procedure Execute; override;

    published
      property OnToggle: TNotifyEvent read FOnToggle write FOnToggle;
      property Toggled: Boolean read FToggled write SetToggled;
  end;


  TActionList = class ( specialize TObjectList < TAction >);

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

var
  ActionIntf: TActionIntf = nil;


implementation

{ TToggleAction }

procedure TToggleAction.SetToggled(AValue: Boolean);
begin
  if FToggled=AValue then Exit;
  FToggled:=AValue;

  if ( Assigned( OnExecute )) then
    OnExecute( Self );
  DoNotifyOnChanged;
end;

procedure TToggleAction.Execute;
begin
  Toggled:= not Toggled;
end;


{ TActionIntf }

procedure TActionIntf.RegisterAction(AAction: TAction);
begin

end;

procedure TActionIntf.UnregisterAction(AAction: TAction);
begin

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

end.

