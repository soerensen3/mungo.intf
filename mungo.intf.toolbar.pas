unit mungo.intf.toolbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  mungo.intf.action;

type
  { TToolBarIntf }

  TToolBarIntf = class
    protected
      FControl: TObject;

    public
      function AddButton( AAction: TAction ): TObject; virtual; abstract;
      function AddToggleButton( AAction: TToggleAction ): TObject; virtual; abstract;
      function AddSpacer(): TObject; virtual; abstract;

    published
      property Control: TObject read FControl write FControl;
  end;

implementation



end.

