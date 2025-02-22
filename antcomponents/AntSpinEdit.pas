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

unit AntSpinEdit;

interface

uses
  Graphics, Classes, Controls, ExtCtrls, StdCtrls, Buttons, ComCtrls;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

type
  TAntEdit = class(TEdit)
  private
    FAlignment: TAlignment;
    procedure SetAlignment(const Value: TAlignment);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  published
    constructor Create(AOwner: TComponent); override;
    property Alignment: TAlignment read FAlignment write SetAlignment default taRightJustify;
  end;

type
  TAntSpinEdit = class(TCustomEdit)
  private
    FEdit: TAntEdit;
    FSpin: TUpDown;
    FAllowEmpty: boolean;
    FEmptyValue: smallint;
    FEnabled: boolean;
    FOnChange: TNotifyEvent;
    procedure SetEdit(newValue: string);
    function GetEdit: string;
    procedure SetEditValue(newValue: smallint);
    function GetEditValue: smallint;
    procedure SetMin(newMin: smallint);
    function GetMin: smallint;
    procedure SetMax(newMax: smallint);
    function GetMax: smallint;
    procedure EditChange(Sender: TObject);
//    procedure EditEnter(Sender: TObject);
    procedure ClickUpDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    function GetTabStop: Boolean;
    procedure SetTabStop(const Value: Boolean);
  protected
    procedure SetEnabled(Value: Boolean); override;
  public
    constructor Create(Owner: TComponent);override;
    destructor Destroy;override;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Anchors;
    property TabOrder;
    property Enabled;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseDown;
    property TabStop: Boolean read GetTabStop write SetTabStop;
    property Text: string read GetEdit write SetEdit;
    property Value: smallint read GetEditValue write SetEditValue;
    property Min: smallint read GetMin write SetMin default 0;
    property Max: smallint read GetMax write SetMax default 9999;
    property EmptyValue: smallint read FEmptyValue write FEmptyValue default -1;
    property AllowEmpty: boolean read FAllowEmpty write FAllowEmpty default false;
  end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

implementation

uses
  Windows, Forms, SysUtils;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

constructor TAntSpinEdit.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  inherited Tabstop := False;
  ControlStyle := ControlStyle + [csAcceptsControls] - [csSetCaption];
  borderStyle := bsSingle;
  Color := clBtnFace;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  Height := 21;
  Width := 121;
  FEdit := TAntEdit.Create(self);
  FSpin := TUpDown.Create(self);
  FEdit.Parent := self;
  FSpin.Parent := self;
//  InsertControl(FEdit);
//  InsertControl(FSpin);
  FEdit.TabStop := False;
  FSpin.TabStop := False;
  FEdit.BorderStyle := bsNone;
  FSpin.Associate := FEdit;
//  FSpin.Width := 16;
//  FSpin.Height := Height;
//  FEdit.Height := Height;
//  FEdit.Width := Width - FSpin.Width;
//  FSpin.Left := FEdit.Width;
  FSpin.Align := alRight;
  FEdit.Align := alClient;
  FEmptyValue := -1;
  FAllowEmpty := false;
  FSpin.Max := 9999;
  FSpin.Min := 0;
  FEdit.OnChange := EditChange;
  FSpin.OnMouseDown := ClickUpDown;
//  OnEnter := EditEnter;
//  FEdit.Anchors := [akLeft, akTop, akBottom, akRight];
//  FSpin.Anchors := [akRight, akTop, akBottom];
  FSpin.Thousands := false;
  FEdit.BevelInner := bvSpace;
  FEdit.BevelOuter := bvNone;
  FEdit.BevelKind := bkFlat;
  FEnabled := True;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

destructor TAntSpinEdit.Destroy;
begin
  FEdit.Free;
  FSpin.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAntSpinEdit.EditChange(Sender: TObject);
begin
  if FAllowEmpty and (FEdit.Text = '') then
    exit;
  FEdit.Text := IntToStr(FSpin.Position);
  if Assigned(FOnChange) then
    FOnChange(self);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAntSpinEdit.ClickUpDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FEdit.Enabled and FEdit.Visible then
    FEdit.SetFocus;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TAntSpinEdit.GetEdit: string;
begin
  result := FEdit.Text;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TAntSpinEdit.GetEditValue: smallint;
begin
  if (FEdit.Text = '') and AllowEmpty then
    result := FEmptyValue
  else
    result := FSpin.Position;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TAntSpinEdit.GetMax: smallint;
begin
  result := FSpin.Max;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TAntSpinEdit.GetMin: smallint;
begin
  Result := FSpin.Min;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAntSpinEdit.SetEdit(newValue: string);
var
  newValueInt: smallint;
begin
  if (newValue = '') then
  begin
    if not FAllowEmpty then
      exit
    else FEdit.Text := '';
  end else
  begin
    newValueInt := strToIntDef(newValue, Min-1);
    if (newValueInt < Min) or (newValueInt > Max) then
      exit;
  end;
  FEdit.Text := newValue;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAntSpinEdit.SetEditValue(newValue: smallint);
begin
  FSpin.Position := newValue;
  if AllowEmpty and (newValue = EmptyValue) then
  begin
    FSpin.Position := EmptyValue;
    FEdit.Text := ''
  end else
  begin
    FEdit.Text := intToStr(newValue);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAntSpinEdit.SetMax(newMax: smallint);
begin
  FSpin.Max := newMax;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAntSpinEdit.SetMin(newMin: smallint);
begin
  FSpin.Min := newMin;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAntSpinEdit.SetEnabled(Value: Boolean);
begin
  inherited;
  FSpin.Enabled := Value;
  FEdit.Enabled := Value;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TAntSpinEdit.GetTabStop: Boolean;
begin
  result := FEdit.TabStop;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAntSpinEdit.SetTabStop(const Value: Boolean);
begin
  FEdit.TabStop := Value;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

constructor TAntEdit.Create(AOwner: TComponent);
begin
  inherited;
  FAlignment := taRightJustify;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAntEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array[TAlignment] of Cardinal = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or Alignments[FAlignment];
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAntEdit.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;
(*
{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAntSpinEdit.EditEnter(Sender: TObject);
begin
  if FEdit.Visible and FEdit.Enabled then
    FEdit.SetFocus;
end;
*)
{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

end.
