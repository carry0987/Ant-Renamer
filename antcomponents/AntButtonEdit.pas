(*

(c) 2002-2003 Antoine Potten
software@antp.be
htt://www.antp.be/software/

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

*)

unit AntButtonEdit;

interface

uses
  Graphics, Classes, Controls, ExtCtrls, StdCtrls, Buttons;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

type
  TAntButtonEdit = class(TCustomEdit)
  private
    FEdit: TEdit;
    FButton: TSpeedButton;
    FEnabled: boolean;
    FOnChange: TNotifyEvent;
    FOnButtonClick: TNotifyEvent;
    procedure SetEdit(newValue: string);
    function GetEdit: string;
    procedure EditChange(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    function GetTabStop: Boolean;
    procedure SetTabStop(const Value: Boolean);
    function GetButtonHint: string;
    procedure SetButtonHint(Value: string);
    function GetGlyph: TBitmap;
    procedure SetGlyph(Value: TBitmap);
//    function GetButtonWidth: Integer;
//    procedure SetButtonWidth(Value: Integer);
//    function GetButtonFlat: Boolean;
//    procedure SetButtonFlat(Value: Boolean);
//    procedure ResetSize;
//    procedure EditEnter(Sender: TObject);
  protected
    procedure SetEnabled(Value: Boolean); override;
//    procedure Loaded; override;
  public
    constructor Create(Owner: TComponent);override;
    destructor Destroy;override;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property ButtonHint: string read GetButtonHint write SetButtonHint;
    property ButtonGlyph: TBitmap read GetGlyph write SetGlyph;
//    property ButtonWidth: Integer read GetButtonWidth write SetButtonWidth;
//    property ButtonFlat: Boolean read GetButtonFlat write SetButtonFlat default False;
    property Anchors;
    property TabOrder;
    property Enabled;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseDown;
    property TabStop: Boolean read GetTabStop write SetTabStop;
    property Text: string read GetEdit write SetEdit;
  end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

implementation

uses
  Forms, SysUtils,
  AntCommon;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

constructor TAntButtonEdit.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  inherited Tabstop := False;
  ControlStyle := ControlStyle + [csAcceptsControls] - [csSetCaption];
  borderStyle := bsSingle;
  Color := clBtnFace;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  Height := 21;
//  Width := 121;
  FEdit := TEdit.Create(self);
  FButton := TSpeedButton.Create(self);
  FEdit.Parent := self;
  FButton.Parent := self;
//  InsertControl(FEdit);
//  InsertControl(FButton);
  FEdit.BorderStyle := bsNone;
//  FButton.Height := Height-4;
//  FEdit.Height := Height;
//  FEdit.Width := Width - FButton.Width;
//  FButton.Left := FEdit.Width;
  FButton.Align := alRight;
  FButton.Width := 18;
  FEdit.Align := alClient;
//  ResetSize;
  FEdit.OnChange := EditChange;
  FButton.OnClick := ButtonClick;
//  FEdit.Anchors := [akLeft, akTop, akBottom, akRight];
//  FButton.Anchors := [akRight, akTop, akBottom];
  FEdit.BevelInner := bvSpace;
  FEdit.BevelOuter := bvNone;
  FEdit.BevelKind := bkFlat;
//  OnEnter := EditEnter;
  FEnabled := true;
  FButton.Cursor := crArrow;
  FButton.Flat := IsThemedXP;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

destructor TAntButtonEdit.Destroy;
begin
  FEdit.Free;
  FButton.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAntButtonEdit.EditChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TAntButtonEdit.GetEdit: string;
begin
  result := FEdit.Text;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAntButtonEdit.SetEdit(newValue: string);
begin
  FEdit.Text := newValue;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAntButtonEdit.SetEnabled(Value: Boolean);
begin
  inherited;
  FButton.Enabled := Value;
  FEdit.Enabled := Value;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TAntButtonEdit.GetTabStop: Boolean;
begin
  result := FEdit.TabStop;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAntButtonEdit.SetTabStop(const Value: Boolean);
begin
  FEdit.TabStop := Value;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAntButtonEdit.ButtonClick(Sender: TObject);
begin
  if FEdit.Enabled then
    FEdit.SetFocus;
  if assigned(FOnButtonClick) then
    self.FOnButtonClick(Sender);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TAntButtonEdit.GetButtonHint: string;
begin
  result := FButton.Hint;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAntButtonEdit.SetButtonHint(Value: string);
begin
  FButton.Hint := Value;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TAntButtonEdit.GetGlyph: TBitmap;
begin
  Result := FButton.Glyph;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAntButtonEdit.SetGlyph(Value: TBitmap);
begin
  FButton.Glyph := Value;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}
(*
function TAntButtonEdit.GetButtonWidth: integer;
begin
  result := FButton.Width;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAntButtonEdit.SetButtonWidth(Value: integer);
begin
  FEdit.Width := Width - Value - 4;
  FButton.Width := Value;
  FButton.Left := FEdit.Width;
end;
*)
{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}
(*
procedure TAntButtonEdit.Loaded;
begin
  inherited;
  ResetSize;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAntButtonEdit.ResetSize;
begin
//  FButton.Width := 21;
  FButton.Height := Height;
  FEdit.Height := Height;
  FEdit.Width := Width - FButton.Width;
  FButton.Left := FEdit.Width;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAntButtonEdit.EditEnter(Sender: TObject);
begin
  if FEdit.Visible and FEdit.Enabled then
    FEdit.SetFocus;
end;
*)
{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}
(*
function TAntButtonEdit.GetButtonFlat: Boolean;
begin
  Result := FButton.Flat;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAntButtonEdit.SetButtonFlat(Value: Boolean);
begin
  FButton.Flat := Value;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}
*)
end.
