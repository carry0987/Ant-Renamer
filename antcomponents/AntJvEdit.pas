{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvNewEdit.PAS, released on 2002-mm-dd.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

This unit is a merging of the original TAntJvEdit, TAntJvExEdit, TAntJvCaretEdit,TAntJvAlignedEdit,
TAntJvSingleLineMemo.
Merging done 2002-06-05 by Peter Thornqvist [peter3@peter3.com]

  MERGE NOTES:
    * TAntJvCustomEdit has been removed from JvComponent and put here instead.
    * The HotTrack property only works if BorderStyle := bsSingle and BevelKind := bvNone
    * Added ClipboardCommands

Contributor(s):
  Anthony Steele [asteele@iafrica.com]
  Peter Below [100113.1101@compuserve.com]
  Rob den Braasem [rbraasem@xs4all.nl] (GroupIndex property - using several TAntJvEdits with the same GroupIndex
    will clear the text from the other edits when something is typed into one of them.
    To disable GroupIndex, set it to -1)
  André Snepvangers [asn@xs4all.nl] ( clx compatible version )

Last Modified: 2004-01-29

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I ant.inc}

unit AntJvEdit;

interface

uses
  SysUtils, Classes,
  Windows, Messages, Graphics, Controls, StdCtrls, Forms, Menus,
  AntJvExControls, AntJvTypes;

type
  TAntJvCustomEdit = class(TAntJvExCustomEdit)
  private
    FOver: Boolean;
    FColor: TColor;
    FSaved: TColor;
    FFlat: Boolean;
    FOnParentColorChanged: TNotifyEvent;
    FGroupIndex: Integer;
    FAlignment: TAlignment;
    FHotTrack: Boolean;
    FDisabledColor: TColor;
    FDisabledTextColor: TColor;
    FProtectPassword: Boolean;
    FStreamedSelLength: Integer;
    FStreamedSelStart: Integer;
    FUseFixedPopup: Boolean;
    FAutoHint: Boolean;
    FOldHint: TCaption;
    function GetPasswordChar: Char;
    procedure SetAlignment(Value: TAlignment);
    procedure SetDisabledColor(const Value: TColor); virtual;
    procedure SetDisabledTextColor(const Value: TColor); virtual;
    procedure SetPasswordChar(Value: Char);
    procedure SetHotTrack(const Value: Boolean);
    function GetText: TCaption;
    procedure SetText(const Value: TCaption);
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure SetGroupIndex(const Value: Integer);
    function GetFlat: Boolean;
    procedure SetAutoHint(const Value: Boolean);
  protected
    procedure DoClipboardCut; override;
    procedure DoClipboardPaste; override;
    procedure DoClearText; override;
    procedure DoUndo; override;

    procedure UpdateEdit;
    procedure SetClipboardCommands(const Value: TAntJvClipboardCommands); override;
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure SetSelLength(Value: Integer); override;
    procedure SetSelStart(Value: Integer); override;
    function GetPopupMenu: TPopupMenu; override;

    procedure DoSetFocus(FocusedWnd: HWND); override;
    procedure DoKillFocus(FocusedWnd: HWND); override;
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; override;
    procedure EnabledChanged; override;
    procedure ParentColorChanged; override;
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure SetFlat(Value: Boolean); virtual;
    procedure UpdateAutoHint; dynamic;
    procedure Resize; override;
  public
    function IsEmpty: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DefaultHandler(var Msg); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
  protected
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AutoHint: Boolean read FAutoHint write SetAutoHint default False; 
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property PasswordChar: Char read GetPasswordChar write SetPasswordChar;
    // set to True to disable read/write of PasswordChar and read of Text
    property ProtectPassword: Boolean read FProtectPassword write FProtectPassword default False;
    property DisabledTextColor: TColor read FDisabledTextColor write SetDisabledTextColor default clGrayText;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor default clWindow;
    property Text: TCaption read GetText write SetText;
    property UseFixedPopup: Boolean read FUseFixedPopup write FUseFixedPopup default True;
    property HintColor: TColor read FColor write FColor default clInfoBk;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property Flat: Boolean read GetFlat write SetFlat;
  end;

  TAntJvEdit = class(TAntJvCustomEdit)
  published
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BiDiMode;
    property DragCursor;
    property DragKind;
    property ImeMode;
    property ImeName;
    property OEMConvert;
    property ParentBiDiMode;
    property UseFixedPopup; // asn: clx not implemented yet
    property DisabledTextColor;
    property DisabledColor;
    property HotTrack;
    property PasswordChar;
    property PopupMenu;
    property ProtectPassword;
    property Align;
    property Alignment;
    property ClipboardCommands;
    property HintColor;
    property GroupIndex;
    property Modified;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;

    property Anchors;
    property AutoSelect;
    property AutoSize;
    property AutoHint;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnStartDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

implementation

uses
  AntJvFixedEditPopup, AntJvToolEdit,
  Math;

constructor TAntJvCustomEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor := clInfoBk;
  FOver := False;
  FAlignment := taLeftJustify;
  // ControlStyle := ControlStyle + [csAcceptsControls];
  ClipboardCommands := [caCopy..caUndo];
  FDisabledColor := clWindow;
  FDisabledTextColor := clGrayText;
  FHotTrack := False;
  FStreamedSelLength := 0;
  FStreamedSelStart := 0;
  FUseFixedPopup := True;  // asn: clx not implemented yet
  FGroupIndex := -1;
end;

destructor TAntJvCustomEdit.Destroy;
begin
  inherited Destroy;
end;

procedure TAntJvCustomEdit.Loaded;
begin
  inherited Loaded;
  SelStart := FStreamedSelStart;
  SelLength := FStreamedSelLength;
end;

procedure TAntJvCustomEdit.Change;
begin
  inherited Change;
  if not HasParent then
    Exit;
  UpdateAutoHint;
end;

procedure TAntJvCustomEdit.CreateParams(var Params: TCreateParams);
const
  Styles: array [TAlignment] of DWORD = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or Styles[FAlignment];
  if (FAlignment <> taLeftJustify) and (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and
    (Win32MajorVersion = 4) and (Win32MinorVersion = 0) then
    Params.Style := Params.Style or ES_MULTILINE; // needed for Win95
end;

procedure TAntJvCustomEdit.MouseEnter(AControl: TControl);
var
  I, J: Integer;
begin
  if csDesigning in ComponentState then
    Exit;
  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FColor;
    if FHotTrack then
    begin
      I := SelStart;
      J := SelLength;
      Flat := False;
      SelStart := I;
      SelLength := J;
    end;
    FOver := True;
//    UpdateAutoHint;
  end;
  inherited MouseEnter(AControl);
end;

procedure TAntJvCustomEdit.MouseLeave(AControl: TControl);
var
  I, J: Integer;
begin
  if csDesigning in ComponentState then
    Exit;
  if FOver then
  begin
    Application.HintColor := FSaved;
    if FHotTrack then
    begin
      I := SelStart;
      J := SelLength;
      Flat := True;
      SelStart := I;
      SelLength := J;
    end;
    FOver := False;
  end;
  inherited MouseLeave(AControl);
end;

procedure TAntJvCustomEdit.ParentColorChanged;
begin
  inherited ParentColorChanged;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TAntJvCustomEdit.SetHotTrack(const Value: Boolean);
begin
  FHotTrack := Value;
  Flat := FHotTrack;
end;

function TAntJvCustomEdit.IsEmpty: Boolean;
begin
  Result := (Length(Text) = 0);
end;

procedure TAntJvCustomEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

procedure TAntJvCustomEdit.SetDisabledColor(const Value: TColor);
begin
  if FDisabledColor <> Value then
  begin
    FDisabledColor := Value;
    if not Enabled then
      Invalidate;
  end;
end;

procedure TAntJvCustomEdit.SetDisabledTextColor(const Value: TColor);
begin
  if FDisabledTextColor <> Value then
  begin
    FDisabledTextColor := Value;
    if not Enabled then
      Invalidate;
  end;
end;

function StrFillChar(Ch: Char; Length: Cardinal): string;
begin
  SetLength(Result, Length);
  if Length > 0 then
    FillChar(Result[1], Length, Ch);
end;

function TAntJvCustomEdit.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
var
  R: TRect;
begin
  if Enabled then
    Result := inherited DoPaintBackground(Canvas, Param)
  else
  begin
    Canvas.Brush.Color := FDisabledColor;
    Canvas.Brush.Style := bsSolid;
    R := ClientRect;
    Canvas.FillRect(R);
    Result := True;
  end;
end;

procedure TAntJvCustomEdit.WMPaint(var Msg: TWMPaint);
var
  Canvas: TControlCanvas;
  S: TCaption;
begin
  if csDestroying in ComponentState then
    Exit;
  if Enabled then
    inherited
  else
  begin
    if PasswordChar = #0 then
      S := Text
    else
      S := StrFillChar(PasswordChar, Length(Text));
    Canvas := nil;
    try
      if not PaintEdit(Self, S, FAlignment, False, {0,} FDisabledTextColor,
         Focused, Canvas, Msg) then
        inherited;
    finally
      Canvas.Free;
    end;
  end;
end;

procedure TAntJvCustomEdit.DoSetFocus(FocusedWnd: HWND);
begin
  inherited DoSetFocus(FocusedWnd);
end;

procedure TAntJvCustomEdit.DoKillFocus(FocusedWnd: HWND);
begin
  inherited DoKillFocus(FocusedWnd);
end;

procedure TAntJvCustomEdit.EnabledChanged;
begin
  inherited EnabledChanged;
  Invalidate;
end;

procedure TAntJvCustomEdit.DoClearText;
begin
  if not ReadOnly then
    inherited DoClearText;
end;

procedure TAntJvCustomEdit.DoUndo;
begin
  if not ReadOnly then
    inherited DoUndo;
end;

procedure TAntJvCustomEdit.DoClipboardCut;
begin
  if not ReadOnly then
    inherited DoClipboardCut;
end;

procedure TAntJvCustomEdit.DoClipboardPaste;
begin
  if not ReadOnly then
    inherited DoClipboardPaste;
  UpdateEdit;
end;

procedure TAntJvCustomEdit.SetGroupIndex(const Value: Integer);
begin
  FGroupIndex := Value;
  UpdateEdit;
end;

procedure TAntJvCustomEdit.UpdateEdit;
var
  I: Integer;
begin
  for I := 0 to Self.Owner.ComponentCount - 1 do
    if Self.Owner.Components[I] is TAntJvCustomEdit then
      if ({(Self.Owner.Components[I].Name <> Self.Name)}
         (Self.Owner.Components[I] <> Self) and // (ahuser) this is better
        ((Self.Owner.Components[I] as TAntJvCustomEdit).GroupIndex <> -1) and
        ((Self.Owner.Components[I] as TAntJvCustomEdit).fGroupIndex = Self.FGroupIndex)) then
        (Self.Owner.Components[I] as TAntJvCustomEdit).Caption := '';
end;

procedure TAntJvCustomEdit.SetText(const Value: TCaption);
begin
  inherited Text := Value;
  UpdateAutoHint;
end;

function TAntJvCustomEdit.GetText: TCaption;
var
  Tmp: Boolean;
begin
  Tmp := ProtectPassword;
  try
    ProtectPassword := False;
    Result := inherited Text;
  finally
    ProtectPassword := Tmp;
  end;
end;

procedure TAntJvCustomEdit.SetPasswordChar(Value: Char);
var
  Tmp: Boolean;
begin
  Tmp := ProtectPassword;
  try
    ProtectPassword := False;
    if HandleAllocated then
      inherited PasswordChar := Char(SendMessage(Handle, EM_GETPASSWORDCHAR, 0, 0));
    inherited PasswordChar := Value;
  finally
    ProtectPassword := Tmp;
  end;
end;

procedure TAntJvCustomEdit.DefaultHandler(var Msg);
begin
  if ProtectPassword then
    with TMessage(Msg) do
      case Msg of
        WM_CUT, WM_COPY, WM_GETTEXT, WM_GETTEXTLENGTH, EM_SETPASSWORDCHAR:
          Result := 0;
      else
        inherited
      end
  else
    inherited;
end;

function TAntJvCustomEdit.GetPasswordChar: Char;
begin
  if HandleAllocated then
    Result := Char(SendMessage(Handle, EM_GETPASSWORDCHAR, 0, 0))
  else
    Result := inherited PasswordChar;
end;

procedure TAntJvCustomEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  UpdateEdit;
  inherited KeyDown(Key, Shift);
end;

procedure TAntJvCustomEdit.SetSelLength(Value: Integer);
begin
  if csReading in ComponentState then
    FStreamedSelLength := Value
  else
    inherited SetSelLength(Value);
end;

procedure TAntJvCustomEdit.SetSelStart(Value: Integer);
begin
  if csReading in ComponentState then
    FStreamedSelStart := Value
  else
    inherited SetSelStart(Value);
end;

function TAntJvCustomEdit.GetPopupMenu: TPopupMenu;
begin
  Result := inherited GetPopupMenu;
 // user has not assigned his own popup menu, so use fixed default
  if (Result = nil) and UseFixedPopup then
    Result := FixedDefaultEditPopUp(self);
end;

function TAntJvCustomEdit.GetFlat: Boolean;
begin
  FFlat := Ctl3D; // update
  Result := FFlat;
end;

procedure TAntJvCustomEdit.SetFlat(Value: Boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    Ctl3D := FFlat;
  end;
end;

procedure TAntJvCustomEdit.SetClipboardCommands(const Value: TAntJvClipboardCommands);
begin
  if ClipboardCommands <> Value then
  begin
    inherited SetClipboardCommands(Value);
    ReadOnly := ClipboardCommands <= [caCopy];
  end;
end;

procedure TAntJvCustomEdit.UpdateAutoHint;
var
  C: TControlCanvas;
  Size: TSize;
begin
  if AutoHint and HandleAllocated then
  begin
    // (p3) empty original Hint indicates that we always want to replace Hint with Text
    if FOldHint = '' then
    begin
      Hint := Text;
      Exit;
    end;
    C := TControlCanvas.Create;
    try
      C.Control := self;
      if GetTextExtentPoint32(C.Handle, PChar(Text), Length(Text), Size) then
      begin
        if (ClientWidth <= Size.cx) then
          Hint := Text
        else
          Hint := FOldHint;
      end
      else
        Hint := FOldHint;
    finally
      C.Free;
    end;
  end;
end;

procedure TAntJvCustomEdit.SetAutoHint(const Value: Boolean);
begin
  if FAutoHint <> Value then
  begin
    if Value then
      FOldHint := Hint
    else
      Hint := FOldHint;
    FAutoHint := Value;
    UpdateAutoHint;
  end;
end;

procedure TAntJvCustomEdit.Resize;
begin
  inherited Resize;
  UpdateAutoHint;
end;

end.

