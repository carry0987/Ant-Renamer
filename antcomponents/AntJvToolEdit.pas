{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvToolEdit.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Last Modified: 2004-02-09

Contributers:
  Rob den Braasem [rbraasem@xs4all.nl]
  Polaris Software
  rblaurindo

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I ant.inc}

unit AntJvToolEdit;

interface

uses
  Windows,
  Messages, Graphics, Controls, Forms, Dialogs, StdCtrls, Menus, Buttons,
  ImgList, ActnList, ExtDlgs,
  SysUtils, Classes,
  RTLConsts, Variants,
  AntJvVCLUtils, AntJvTypes, AntJvExControls, AntJvSpeedButton;

const
  scAltDown = scAlt + VK_DOWN;
  DefEditBtnWidth = 21;

type

  TCloseUpEvent = procedure(Sender: TObject; Accept: Boolean) of object;
  TPopupAlign = (epaRight, epaLeft);

  TAntJvPopupWindow = class(TCustomForm)
  private
    FEditor: TWinControl;
    FCloseUp: TCloseUpEvent;
    procedure WMMouseActivate(var Msg: TMessage); message WM_MOUSEACTIVATE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    function GetValue: Variant; virtual; abstract;
    procedure SetValue(const Value: Variant); virtual; abstract;
    procedure InvalidateEditor;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure CloseUp(Accept: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function GetPopupText: string; virtual;
    procedure Hide;
    procedure Show(Origin: TPoint); virtual; // Polaris
    property OnCloseUp: TCloseUpEvent read FCloseUp write FCloseUp;
  end;

  TAntJvEditButton = class(TAntJvImageSpeedButton)
  private
    FNoAction: Boolean;
    procedure WMContextMenu(var Msg: TWMContextMenu); message WM_CONTEXTMENU;
  protected
    {$IFDEF DELPHI7_UP}
    FDrawThemedDropDownBtn: Boolean;
    {$ENDIF}
    FStandard: Boolean; // Polaris
    FDrawGlyph: Boolean;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure PaintImage(Canvas: TCanvas; ARect: TRect; const Offset: TPoint;
      AState: TAntJvButtonState; DrawMark: Boolean); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  end;

  TGlyphKind = (gkCustom, gkDefault, gkDropDown, gkEllipsis);
  TAntJvImageKind = (ikCustom, ikDefault, ikDropDown, ikEllipsis);

  TAntJvCustomComboEdit = class;

  TAntJvCustomComboEditActionLink = class(TWinControlActionLink)
  protected
    function IsCaptionLinked: Boolean; override;
    function IsHintLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    function IsOnExecuteLinked: Boolean; override;
    function IsShortCutLinked: Boolean; override;

    procedure SetHint(const Value: string); override;
    procedure SetImageIndex(Value: Integer); override;
    procedure SetOnExecute(Value: TNotifyEvent); override;
    procedure SetShortCut(Value: TShortCut); override;
  end;

  TAntJvCustomComboEditActionLinkClass = class of TAntJvCustomComboEditActionLink;

  TAntJvCustomComboEdit = class(TAntJvExCustomEdit)
  private
    FBtnControl: TWinControl;
    FOnButtonClick: TNotifyEvent;
    FClickKey: TShortCut;
    FReadOnly: Boolean;
    FDirectInput: Boolean;
    FAlwaysEnable: Boolean;
    FAlignment: TAlignment;
    FPopupAlign: TPopupAlign;
    { (rb) Only used for backwards compatibility; eventually remove: }
    FGlyph: TBitmap;
    FGroupIndex: Integer; // RDB
    FDisabledColor: TColor; // RDB
    FDisabledTextColor: TColor; // RDB
    FOnKeyDown: TKeyEvent; // RDB
    FImages: TCustomImageList;
    FImageIndex: TImageIndex;
    FImageKind: TAntJvImageKind;
    FStreamedButtonWidth: Integer;
    function BtnWidthStored: Boolean;
    function GetButtonFlat: Boolean;
    function GetButtonHint: string;
    function GetButtonWidth: Integer;
    function GetDirectInput: Boolean;
    function GetGlyph: TBitmap;
    function GetGlyphKind: TGlyphKind;
    function GetMinHeight: Integer;
    function GetNumGlyphs: TNumGlyphs;
    function GetPopupVisible: Boolean;
    function GetTextHeight: Integer;
    function IsImageIndexStored: Boolean;
    procedure EditButtonClick(Sender: TObject);
    procedure ReadGlyphKind(Reader: TReader);
    procedure ReadNumGlyphs(Reader: TReader);
    procedure RecreateGlyph;
    procedure SetAlignment(Value: TAlignment);
    procedure SetButtonFlat(const Value: Boolean);
    procedure SetButtonHint(const Value: string);
    procedure SetButtonWidth(Value: Integer);
    procedure SetEditRect; 
    procedure SetGlyph(Value: TBitmap);
    procedure SetGlyphKind(Value: TGlyphKind);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImageKind(const Value: TAntJvImageKind);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure UpdateBtnBounds;
    procedure UpdateEdit; // RDB

    procedure CMBiDiModeChanged(var Msg: TMessage); message CM_BIDIMODECHANGED;
    procedure CMCancelMode(var Msg: TCMCancelMode); message CM_CANCELMODE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CNCtlColor(var Msg: TMessage); message CN_CTLCOLOREDIT;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT; // RDB
    {$IFDEF DELPHI7_UP}
    procedure WMNCPaint(var Msg: TWMNCPaint); message WM_NCPAINT;
    procedure WMNCCalcSize(var Msg: TWMNCCalcSize); message WM_NCCALCSIZE;
    {$ENDIF}
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
  protected
    FButton: TAntJvEditButton; // Polaris
    FPopupVisible: Boolean; // Polaris
    FFocused: Boolean; // Polaris
    FPopup: TWinControl;
    procedure DoClearText; override;
    procedure DoClipboardCut; override;
    procedure DoClipboardPaste; override;
    procedure DoBoundsChanged; override;
    procedure DoKillFocus(FocusedWnd: HWND); override;
    procedure DoSetFocus(FocusedWnd: HWND); override;
    procedure EnabledChanged; override;
    procedure FontChanged; override;
    procedure DoEnter; override;
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; override;
    class function DefaultImageIndex: TImageIndex; virtual;
    class function DefaultImages: TCustomImageList; virtual;
    function AcceptPopup(var Value: Variant): Boolean; virtual;
    function GetActionLinkClass: TControlActionLinkClass; override;
    function GetPopupValue: Variant; virtual;
    function GetReadOnly: Boolean; virtual;
    procedure AcceptValue(const Value: Variant); virtual;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure AdjustHeight;
    procedure ButtonClick; dynamic;
    procedure Change; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoChange; virtual; //virtual Polaris
    procedure HidePopup; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure LocalKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); // RDB
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PopupChange; virtual;
    procedure PopupCloseUp(Sender: TObject; Accept: Boolean); virtual; //virtual Polaris
    procedure PopupDropDown(DisableEdit: Boolean); virtual;
    procedure SetClipboardCommands(const Value: TAntJvClipboardCommands); override; // RDB
    procedure SetDirectInput(Value: Boolean); // Polaris
    procedure SetDisabledColor(const Value: TColor); virtual; // RDB
    procedure SetDisabledTextColor(const Value: TColor); virtual; // RDB
    procedure SetGroupIndex(const Value: Integer); // RDB
    procedure SetPopupValue(const Value: Variant); virtual;
    procedure SetReadOnly(Value: Boolean); virtual;
    procedure SetShowCaret; // Polaris
    procedure ShowPopup(Origin: TPoint); virtual;
    procedure UpdatePopupVisible;

    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AlwaysEnable: Boolean read FAlwaysEnable write FAlwaysEnable default False;
    property Button: TAntJvEditButton read FButton;
    property ButtonFlat: Boolean read GetButtonFlat write SetButtonFlat;
    property ButtonHint: string read GetButtonHint write SetButtonHint;
    property ButtonWidth: Integer read GetButtonWidth write SetButtonWidth stored BtnWidthStored;
    property ClickKey: TShortCut read FClickKey write FClickKey default scAltDown;
    property DirectInput: Boolean read GetDirectInput write SetDirectInput default True;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor default clWindow; // RDB
    property DisabledTextColor: TColor read FDisabledTextColor write SetDisabledTextColor default clGrayText; // RDB
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex stored IsImageIndexStored default -1;
    property ImageKind: TAntJvImageKind read FImageKind write SetImageKind default ikCustom;
    property Images: TCustomImageList read FImages write SetImages;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property PopupAlign: TPopupAlign read FPopupAlign write FPopupAlign default epaRight;
    property PopupVisible: Boolean read GetPopupVisible;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoClick;
    procedure SelectAll;
    { Backwards compatibility; moved to public&published; eventually remove }
    property GlyphKind: TGlyphKind read GetGlyphKind write SetGlyphKind;
    property NumGlyphs: TNumGlyphs read GetNumGlyphs write SetNumGlyphs;
  published
    property Glyph: TBitmap read GetGlyph write SetGlyph stored False;
  end;

  TAntJvComboEdit = class(TAntJvCustomComboEdit)
  public
    property Button;
  published
    //Polaris
    property Action;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property DragCursor;
    property DragKind;
    property ImeMode;
    property ImeName;
    property OEMConvert;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
    property BorderStyle;
    property ButtonFlat;
    property ButtonHint;
    property ButtonWidth;
    property CharCase;
    property ClickKey;
    property ClipboardCommands; // RDB
    property Color;
    property Constraints;
    property DirectInput;
    property DisabledColor; // RDB
    property DisabledTextColor; // RDB
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImageIndex;
    property ImageKind;
    property Images;
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown; // RDB
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

  TAntJvComboEditXP = class(TAntJvComboEdit)
  protected
    procedure Loaded; override;
  end;

  EComboEditError = class(Exception);

{ Utility routines }

function EditorTextMargins(Editor: TCustomEdit): TPoint;

function PaintComboEdit(Editor: TAntJvCustomComboEdit; const AText: string;
  AAlignment: TAlignment; StandardPaint: Boolean;
  var ACanvas: TControlCanvas; var Msg: TWMPaint): Boolean;

function PaintEdit(Editor: TCustomEdit; const AText: string;
  AAlignment: TAlignment; PopupVisible: Boolean;
  DisabledTextColor: TColor; StandardPaint: Boolean;
  var ACanvas: TControlCanvas; var Msg: TWMPaint): Boolean;

function LoadDefaultBitmap(Bmp: TBitmap; Item: Integer): Boolean;

function IsInWordArray(Value: Word; const A: array of Word): Boolean;

implementation

uses
  ShellAPI, Consts,
  {$IFDEF DELPHI7_UP}
  AntJvTheme, Themes,
  {$ENDIF}
  AntCommon,
  Math;

type
  TWinControlHack = class(TWinControl);
  TCustomEditHack = class(TCustomEdit);
  TCustomFormHack = class(TCustomForm);

var
  GDefaultComboEditImagesList: TImageList = nil;

//=== Local procedures =======================================================

function LoadDefaultBitmap(Bmp: TBitmap; Item: Integer): Boolean;
begin
  Bmp.Handle := LoadBitmap(0, PChar(Item));
  Result := Bmp.Handle <> 0;
end;

function IsInWordArray(Value: Word; const A: array of Word): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(A) do
    if A[I] = Value then
      Exit;
  Result := True;
end;

procedure DestroyLocals;
begin
  GDefaultComboEditImagesList.Free;
  GDefaultComboEditImagesList := nil;
end;

//=== Global procedures ======================================================

function EditorTextMargins(Editor: TCustomEdit): TPoint;
var
  DC: HDC;
  I: Integer;
  SaveFont: HFONT;
  SysMetrics, Metrics: TTextMetric;
  ed: TCustomEditHack;
begin
  ed := TCustomEditHack(Editor);
  if NewStyleControls then
  begin
    if ed.BorderStyle = bsNone then
      I := 0
    else
    if ed.Ctl3D then
      I := 1
    else
      I := 2;
    if GetWindowLong(ed.Handle, GWL_STYLE) and ES_MULTILINE = 0 then
      Result.X := (SendMessage(ed.Handle, EM_GETMARGINS, 0, 0) and $0000FFFF) + I
    else
      Result.X := I;
    Result.Y := I;
  end
  else
  begin
    if ed.BorderStyle = bsNone then
      I := 0
    else
    begin
      DC := GetDC(0);
      GetTextMetrics(DC, SysMetrics);
      SaveFont := SelectObject(DC, ed.Font.Handle);
      GetTextMetrics(DC, Metrics);
      SelectObject(DC, SaveFont);
      ReleaseDC(0, DC);
      I := SysMetrics.tmHeight;
      if I > Metrics.tmHeight then
        I := Metrics.tmHeight;
      I := I div 4;
    end;
    Result.X := I;
    Result.Y := I;
  end;
end;

function PaintComboEdit(Editor: TAntJvCustomComboEdit; const AText: string;
  AAlignment: TAlignment; StandardPaint: Boolean;
  var ACanvas: TControlCanvas; var Msg: TWMPaint): Boolean;
begin
  if not (csDestroying in Editor.ComponentState) then
  begin
    Result := PaintEdit(Editor, AText, AAlignment, Editor.PopupVisible,
      Editor.FDisabledTextColor, StandardPaint, ACanvas, Msg);
  end
  else
    Result := True;
end;

function PaintEdit(Editor: TCustomEdit; const AText: string;
  AAlignment: TAlignment; PopupVisible: Boolean; 
  DisabledTextColor: TColor; StandardPaint: Boolean;
  var ACanvas: TControlCanvas; var Msg: TWMPaint): Boolean;
const
  AlignStyle: array [Boolean, TAlignment] of DWORD =
    ((WS_EX_LEFT, WS_EX_RIGHT, WS_EX_LEFT),
    (WS_EX_RIGHT, WS_EX_LEFT, WS_EX_LEFT));
var
  LTextWidth, X: Integer;
  EditRect: TRect;
  DC: HDC;
  PS: TPaintStruct;
  S: string;
  ExStyle: DWORD;
  ed: TCustomEditHack;
begin
  Result := True;
  if csDestroying in Editor.ComponentState then
    Exit;
  ed := TCustomEditHack(Editor);
  if ed.UseRightToLeftAlignment then
    ChangeBiDiModeAlignment(AAlignment);
  if StandardPaint and not (csPaintCopy in ed.ControlState) then
  begin
    if SysLocale.MiddleEast and ed.HandleAllocated and (ed.IsRightToLeft) then
    begin { This keeps the right aligned text, right aligned }
      ExStyle := DWORD(GetWindowLong(ed.Handle, GWL_EXSTYLE)) and (not WS_EX_RIGHT) and
        (not WS_EX_RTLREADING) and (not WS_EX_LEFTSCROLLBAR);
      if ed.UseRightToLeftReading then
        ExStyle := ExStyle or WS_EX_RTLREADING;
      if ed.UseRightToLeftScrollBar then
        ExStyle := ExStyle or WS_EX_LEFTSCROLLBAR;
      ExStyle := ExStyle or
        AlignStyle[ed.UseRightToLeftAlignment, AAlignment];
      if DWORD(GetWindowLong(ed.Handle, GWL_EXSTYLE)) <> ExStyle then
        SetWindowLong(ed.Handle, GWL_EXSTYLE, ExStyle);
    end;
    Result := False;
    { return false if we need to use standard paint handler }
    Exit;
  end;
  { Since edit controls do not handle justification unless multi-line (and
    then only poorly) we will draw right and center justify manually unless
    the edit has the focus. }
  if ACanvas = nil then
  begin
    ACanvas := TControlCanvas.Create;
    ACanvas.Control := Editor;
  end;
  DC := Msg.DC;
  if DC = 0 then
    DC := BeginPaint(ed.Handle, PS);
  ACanvas.Handle := DC;
  try
    ACanvas.Font := ed.Font;
    with ACanvas do
    begin
      SendMessage(Editor.Handle, EM_GETRECT, 0, Integer(@EditRect));
      if not (NewStyleControls and ed.Ctl3D) and (ed.BorderStyle = bsSingle) then
      begin
        Brush.Color := clWindowFrame;
        FrameRect(ed.ClientRect);
      end;
      S := AText;
      LTextWidth := TextWidth(S);
      if PopupVisible then
        X := EditRect.Left
      else
      begin
        case AAlignment of
          taLeftJustify:
            X := EditRect.Left;
          taRightJustify:
            X := EditRect.Right - LTextWidth;
        else
          X := (EditRect.Right + EditRect.Left - LTextWidth) div 2;
        end;
      end;
      if SysLocale.MiddleEast then
        UpdateTextFlags;
      if not ed.Enabled then
      begin
        // if PS.fErase then // (p3) fErase is not set to true when control is disabled
        ed.Perform(WM_ERASEBKGND, ACanvas.Handle, 0);

        SaveDC(ACanvas.Handle);
        try
          ACanvas.Brush.Style := bsClear;
          ACanvas.Font.Color := DisabledTextColor;
          ACanvas.TextRect(EditRect, X, EditRect.Top, S);
        finally
          RestoreDC(ACanvas.Handle, -1);
        end;
      end
      else
      begin
        Brush.Color := ed.Color;
        ACanvas.TextRect(EditRect, X, EditRect.Top, S);
      end;
    end;
  finally
    ACanvas.Handle := 0;
    if Msg.DC = 0 then
      EndPaint(ed.Handle, PS);
  end;
end;

//=== TAntJvCustomComboEdit =====================================================

constructor TAntJvCustomComboEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csCaptureMouse];
  //  AutoSize := False;   // Polaris
  FDirectInput := True;
  FClickKey := scAltDown;
  FPopupAlign := epaRight;
  FBtnControl := TWinControl.Create(Self);
  with FBtnControl do
    ControlStyle := ControlStyle + [csReplicatable];
  FBtnControl.Width := DefEditBtnWidth;
  FBtnControl.Height := 17;
  FBtnControl.Visible := True;
  FBtnControl.Parent := Self;
  FButton := TAntJvEditButton.Create(Self);
  FButton.SetBounds(0, 0, FBtnControl.Width, FBtnControl.Height);
  FButton.Visible := True;
  FButton.Parent := FBtnControl;
  TAntJvEditButton(FButton).OnClick := EditButtonClick;
  Height := 21;
  (* ++ RDB ++ *)
  FDisabledColor := clWindow;
  FDisabledTextColor := clGrayText;
  FGroupIndex := -1;
  FGlyph := TBitmap.Create;
  FStreamedButtonWidth := -1;
  FImageKind := ikCustom;
  FImageIndex := -1;
  inherited OnKeyDown := LocalKeyDown;
  (* -- RDB -- *)
end;

destructor TAntJvCustomComboEdit.Destroy;
begin
  FButton.OnClick := nil;
  FGlyph.Free;
  inherited Destroy;
end;

function TAntJvCustomComboEdit.AcceptPopup(var Value: Variant): Boolean;
begin
  Result := True;
end;

procedure TAntJvCustomComboEdit.AcceptValue(const Value: Variant);
begin
  if Text <> VarToStr(Value) then
  begin
    Text := Value;
    Modified := True;
    UpdatePopupVisible;
    DoChange;
  end;
end;

procedure TAntJvCustomComboEdit.ActionChange(Sender: TObject;
  CheckDefaults: Boolean);
begin
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if not CheckDefaults or not Assigned(Self.Images) then
        Self.Images := ActionList.Images;
      if not CheckDefaults or Self.Enabled then
        Self.Enabled := Enabled;
      if not CheckDefaults or (Self.HelpContext = 0) then
        Self.HelpContext := HelpContext;
      if not CheckDefaults or (Self.Hint = '') then
        Self.ButtonHint := Hint;
      if not CheckDefaults or (Self.ImageIndex = -1) then
        Self.ImageIndex := ImageIndex;
      if not CheckDefaults or (Self.ClickKey = scNone) then
        Self.ClickKey := ShortCut;
      if not CheckDefaults or Self.Visible then
        Self.Visible := Visible;
      if not CheckDefaults or not Assigned(Self.OnButtonClick) then
        Self.OnButtonClick := OnExecute;
    end;
end;

procedure TAntJvCustomComboEdit.AdjustHeight;
var
  DC: HDC;
  SaveFont: HFONT;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  if NewStyleControls then
  begin
    if Ctl3D then
      I := 8
    else
      I := 6;
    I := GetSystemMetrics(SM_CYBORDER) * I;
  end
  else
  begin
    I := SysMetrics.tmHeight;
    if I > Metrics.tmHeight then
      I := Metrics.tmHeight;
    I := I div 4 + GetSystemMetrics(SM_CYBORDER) * 4;
  end;
  if Height < Metrics.tmHeight + I then
    Height := Metrics.tmHeight + I;
end;

function TAntJvCustomComboEdit.BtnWidthStored: Boolean;
begin
  if (FImageKind = ikDefault) and (DefaultImages <> nil) and (DefaultImageIndex >= 0) then
    Result := ButtonWidth <> Max(DefaultImages.Width + 6, DefEditBtnWidth)
  else
  if FImageKind = ikDropDown then
    Result := ButtonWidth <> GetSystemMetrics(SM_CXVSCROLL)
  else
    Result := ButtonWidth <> DefEditBtnWidth;
end;

procedure TAntJvCustomComboEdit.ButtonClick;
begin
  if Assigned(FOnButtonClick) then
    FOnButtonClick(Self);
  if FPopup <> nil then
  begin
    if FPopupVisible then
      PopupCloseUp(FPopup, True)
    else
      PopupDropDown(True);
  end;
end;

procedure TAntJvCustomComboEdit.Change;
begin
  if not PopupVisible then
    DoChange
  else
    PopupChange;
end;

procedure TAntJvCustomComboEdit.CMBiDiModeChanged(var Msg: TMessage);
begin
  inherited;
  if FPopup <> nil then
    FPopup.BiDiMode := BiDiMode;
end;

procedure TAntJvCustomComboEdit.CMCancelMode(var Msg: TCMCancelMode);
begin
  if (Msg.Sender <> Self) and (Msg.Sender <> FPopup) and
    (Msg.Sender <> FButton) and ((FPopup <> nil) and
    not FPopup.ContainsControl(Msg.Sender)) then
    PopupCloseUp(FPopup, False);
end;

procedure TAntJvCustomComboEdit.CNCtlColor(var Msg: TMessage);
var
  TextColor: Longint;
begin
  inherited;
  if NewStyleControls then
  begin
    TextColor := ColorToRGB(Font.Color);
    if not Enabled and (ColorToRGB(Color) <> ColorToRGB(clGrayText)) then
      TextColor := ColorToRGB(clGrayText);
    SetTextColor(Msg.WParam, TextColor);
  end;
end;

procedure TAntJvCustomComboEdit.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  UpdateBtnBounds;
end;

procedure TAntJvCustomComboEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array [TAlignment] of LongWord = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN
    or Alignments[FAlignment];
end;

procedure TAntJvCustomComboEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

class function TAntJvCustomComboEdit.DefaultImageIndex: TImageIndex;
begin
  Result := -1;
end;

class function TAntJvCustomComboEdit.DefaultImages: TCustomImageList;
begin
  if GDefaultComboEditImagesList = nil then
    GDefaultComboEditImagesList := TImageList.CreateSize(14, 12);
  Result := GDefaultComboEditImagesList;
end;

procedure TAntJvCustomComboEdit.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('NumGlyphs', ReadNumGlyphs, nil, False);
  Filer.DefineProperty('GlyphKind', ReadGlyphKind, nil, False);
end;

procedure TAntJvCustomComboEdit.DoChange;
begin
  inherited Change;
end;

procedure TAntJvCustomComboEdit.DoClick;
begin
  EditButtonClick(Self);
end;

procedure TAntJvCustomComboEdit.DoEnter;
begin
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
  inherited DoEnter;
end;

procedure TAntJvCustomComboEdit.EditButtonClick(Sender: TObject);
begin
  if (not FReadOnly) or AlwaysEnable then
    ButtonClick;
end;

procedure TAntJvCustomComboEdit.EnabledChanged;
begin
  inherited EnabledChanged;
  Invalidate;
  FButton.Enabled := Enabled;
end;

procedure TAntJvCustomComboEdit.FontChanged;
begin
  inherited FontChanged;
  if HandleAllocated then
    SetEditRect;
end;

function TAntJvCustomComboEdit.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TAntJvCustomComboEditActionLink;
end;

function TAntJvCustomComboEdit.GetButtonFlat: Boolean;
begin
  Result := FButton.Flat;
end;

function TAntJvCustomComboEdit.GetButtonHint: string;
begin
  Result := FButton.Hint;
end;

function TAntJvCustomComboEdit.GetButtonWidth: Integer;
begin
  Result := FButton.Width;
end;

function TAntJvCustomComboEdit.GetDirectInput: Boolean;
begin
  Result := FDirectInput;
end;

function TAntJvCustomComboEdit.GetGlyph: TBitmap;
begin
  Result := FGlyph;
end;

function TAntJvCustomComboEdit.GetGlyphKind: TGlyphKind;
begin
  Result := TGlyphKind(FImageKind);
end;

function TAntJvCustomComboEdit.GetMinHeight: Integer;
var
  I: Integer;
begin
  I := GetTextHeight;
  if BorderStyle = bsSingle then
    I := I + GetSystemMetrics(SM_CYBORDER) * 4 + 1;
  Result := I;
end;

function TAntJvCustomComboEdit.GetNumGlyphs: TNumGlyphs;
begin
  Result := 1;
end;

function TAntJvCustomComboEdit.GetPopupValue: Variant;
begin
  if FPopup <> nil then
    Result := TAntJvPopupWindow(FPopup).GetValue
  else
    Result := '';
end;

function TAntJvCustomComboEdit.GetPopupVisible: Boolean;
begin
  Result := (FPopup <> nil) and FPopupVisible;
end;

function TAntJvCustomComboEdit.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

function TAntJvCustomComboEdit.GetTextHeight: Integer;
var
  DC: HDC;
  SaveFont: HFONT;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  try
    GetTextMetrics(DC, SysMetrics);
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
  finally
    ReleaseDC(0, DC);
  end;
  //  Result := Min(SysMetrics.tmHeight, Metrics.tmHeight);  // Polaris
  Result := Metrics.tmHeight; // Polaris
end;

procedure TAntJvCustomComboEdit.HidePopup;
begin
  TAntJvPopupWindow(FPopup).Hide;
end;

function TAntJvCustomComboEdit.IsImageIndexStored: Boolean;
begin
  Result :=
    not (ActionLink is TAntJvCustomComboEditActionLink) or
    not (ActionLink as TAntJvCustomComboEditActionLink).IsImageIndexLinked;
end;

procedure TAntJvCustomComboEdit.KeyDown(var Key: Word; Shift: TShiftState);
//Polaris
var
  Form: TCustomForm;
begin
  //Polaris
  Form := GetParentForm(Self);
  if (ssCtrl in Shift) then
    case Key of
      VK_RETURN:
        if (Form <> nil) {and Form.KeyPreview} then
        begin
          TWinControlHack(Form).KeyDown(Key, Shift);
          Key := 0;
        end;
      VK_TAB:
        if (Form <> nil) {and Form.KeyPreview} then
        begin
          TWinControlHack(Form).KeyDown(Key, Shift);
          Key := 0;
        end;
    end;
  //Original
  inherited KeyDown(Key, Shift);
  if (FClickKey = ShortCut(Key, Shift)) and (ButtonWidth > 0) then
  begin
    EditButtonClick(Self);
    Key := 0;
  end;
end;

procedure TAntJvCustomComboEdit.KeyPress(var Key: Char);
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);

  //Polaris  if (Key = Char(VK_RETURN)) or (Key = Char(VK_ESCAPE)) then
//  if (Key = Char(VK_RETURN)) or (Key = Char(VK_ESCAPE)) or ((Key = #10) and PopupVisible) then
  if (Key = #13) or (Key = #27) or ((Key = #10) and PopupVisible) then
  begin
    if PopupVisible then
    begin
      //Polaris      PopupCloseUp(FPopup, Key = Char(VK_RETURN));
//      PopupCloseUp(FPopup, Key <> Char(VK_ESCAPE));
      PopupCloseUp(FPopup, Key <> #27);
      Key := #0;
    end
    else
    begin
      { must catch and remove this, since is actually multi-line }
      GetParentForm(Self).Perform(CM_DIALOGKEY, Byte(Key), 0);
//      if Key = Char(VK_RETURN) then
      if Key = #13 then
      begin
        inherited KeyPress(Key);
        Key := #0;
        Exit;
      end;
    end;
  end;
  //Polaris
  if Key in [#10, #9] then
  begin
    Key := #0;
    if (Form <> nil) {and Form.KeyPreview} then
      TWinControlHack(Form).KeyPress(Key);
  end;
  //Polaris
  inherited KeyPress(Key);
end;

procedure TAntJvCustomComboEdit.Loaded;
begin
  inherited Loaded;
  if FStreamedButtonWidth >= 0 then
    SetButtonWidth(FStreamedButtonWidth);
  DoBoundsChanged;
end;

procedure TAntJvCustomComboEdit.LocalKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  UpdateEdit;
  if Assigned(FOnKeyDown) then
    FOnKeyDown(Sender, Key, Shift);
end;

procedure TAntJvCustomComboEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (FPopup <> nil) and (Button = mbLeft) then
  begin
    if CanFocus then
      SetFocus;
    if not FFocused then
      Exit;
    if FPopupVisible then
      PopupCloseUp(FPopup, False);
    {else if (not ReadOnly or AlwaysEnable) and (not DirectInput) then
      PopupDropDown(True);}
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TAntJvCustomComboEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImages) then
    Images := nil;
end;

procedure TAntJvCustomComboEdit.PopupChange;
begin
end;

procedure TAntJvCustomComboEdit.PopupCloseUp(Sender: TObject; Accept: Boolean);
var
  AValue: Variant;
begin
  if (FPopup <> nil) and FPopupVisible then
  begin
    if GetCapture <> 0 then
      SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    AValue := GetPopupValue;
    HidePopup;
    try
      try
        if CanFocus then
        begin
          SetFocus;
          if GetFocus = Handle then
            SetShowCaret;
        end;
      except
        { ignore exceptions }
      end;
      SetDirectInput(DirectInput);
      Invalidate;
      if Accept and AcceptPopup(AValue) and not FReadOnly then
      begin
        AcceptValue(AValue);
        if FFocused then
          inherited SelectAll;
      end;
    finally
      FPopupVisible := False;
    end;
  end;
end;

procedure TAntJvCustomComboEdit.PopupDropDown(DisableEdit: Boolean);
var
  P: TPoint;
  Y: Integer;
begin
  if (FPopup <> nil) and not (ReadOnly or FPopupVisible) then
  begin
    P := Parent.ClientToScreen(Point(Left, Top));
    Y := P.Y + Height;
    if Y + FPopup.Height > Screen.Height then
      Y := P.Y - FPopup.Height;
    case FPopupAlign of
      epaRight:
        begin
          Dec(P.X, FPopup.Width - Width);
          if P.X < 0 then
            Inc(P.X, FPopup.Width - Width);
        end;
      epaLeft:
        begin
          if P.X + FPopup.Width > Screen.Width then
            Dec(P.X, FPopup.Width - Width);
        end;
    end;
    if P.X < 0 then
      P.X := 0
    else
    if P.X + FPopup.Width > Screen.Width then
      P.X := Screen.Width - FPopup.Width;
    if Text <> '' then
      SetPopupValue(Text)
    else
      SetPopupValue(NULL);
    if CanFocus then
      SetFocus;
    ShowPopup(Point(P.X, Y));
    FPopupVisible := True;
    if DisableEdit then
    begin
      inherited ReadOnly := True;
      HideCaret(Handle);
    end;
  end;
end;

procedure TAntJvCustomComboEdit.ReadGlyphKind(Reader: TReader);
const
  sEnumValues: array [TGlyphKind] of string =
    ('gkCustom', 'gkDefault', 'gkDropDown', 'gkEllipsis');
var
  S: string;
  GlyphKind: TGlyphKind;
begin
  { No need to drag in TypInfo.pas }
  S := Reader.ReadIdent;
  for GlyphKind := Low(TGlyphKind) to High(TGlyphKind) do
    if SameText(S, sEnumValues[GlyphKind]) then
    begin
      ImageKind := TAntJvImageKind(GlyphKind);
      Break;
    end;
end;


type
  TOpenReader = class(TReader); // (ahuser) Delphi 5's TReader.SkipValue is protected  

procedure TAntJvCustomComboEdit.ReadNumGlyphs(Reader: TReader);
begin
  TOpenReader(Reader).SkipValue;
end;

procedure TAntJvCustomComboEdit.RecreateGlyph;
var
  NewGlyph: TBitmap;

  function CreateEllipsisGlyph: TBitmap;
  var
    W, g, I: Integer;
  begin
    Result := TBitmap.Create;
    with Result do
    try
      Monochrome := True;
      Width := Max(1, FButton.Width - 6);
      Height := 4;
      W := 2;
      g := (Result.Width - 3 * W) div 2;
      if g <= 0 then
        g := 1;
      if g > 3 then
        g := 3;
      I := (Width - 3 * W - 2 * g) div 2;
      PatBlt(Canvas.Handle, I, 1, W, W, BLACKNESS);
      PatBlt(Canvas.Handle, I + g + W, 1, W, W, BLACKNESS);
      PatBlt(Canvas.Handle, I + 2 * g + 2 * W, 1, W, W, BLACKNESS);
    except
      Free;
      raise;
    end;
  end;

begin
  FButton.FDrawGlyph := FImageKind in [ikDropDown, ikEllipsis];

  case FImageKind of
    ikDropDown:
      begin
        {$IFDEF DELPHI7_UP}
        { When XP Themes are enabled, ButtonFlat = False, GlyphKind = gkDropDown then
          the glyph is the default themed dropdown button. When ButtonFlat = True, we
          can't use that default dropdown button (because we then use toolbar buttons,
          and there is no themed dropdown toolbar button) }
        FButton.FDrawThemedDropDownBtn :=
          ThemeServices.ThemesEnabled and not ButtonFlat;
        if FButton.FDrawThemedDropDownBtn then
        begin
          TAntJvxButtonGlyph(FButton.ButtonGlyph).Glyph := nil;
          FButton.Invalidate;
        end
        else
        {$ENDIF}
        begin
          LoadDefaultBitmap(TAntJvxButtonGlyph(FButton.ButtonGlyph).Glyph, OBM_COMBO);
          FButton.Invalidate;
        end;
      end;
    ikEllipsis:
      begin
        NewGlyph := CreateEllipsisGlyph;
        try
          TAntJvxButtonGlyph(FButton.ButtonGlyph).Glyph := NewGlyph;
          FButton.Invalidate;
        finally
          NewGlyph.Destroy;
        end;
      end;
  else
    TAntJvxButtonGlyph(FButton.ButtonGlyph).Glyph := nil;
    FButton.Invalidate;
  end;
end;

procedure TAntJvCustomComboEdit.SelectAll;
begin
  if DirectInput then
    inherited SelectAll;
end;

procedure TAntJvCustomComboEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

procedure TAntJvCustomComboEdit.SetButtonFlat(const Value: Boolean);
begin
  FButton.Flat := Value;
  {$IFDEF DELPHI7_UP}
  { When XP Themes are enabled, ButtonFlat = False, GlyphKind = gkDropDown then
    the glyph is the default themed dropdown button. When ButtonFlat = True, we
    can't use that default dropdown button, so we have to recreate the glyph
    in this special case }
  if ThemeServices.ThemesEnabled and (ImageKind = ikDropDown) then
    RecreateGlyph;
  {$ENDIF}
end;

procedure TAntJvCustomComboEdit.SetButtonHint(const Value: string);
begin
  FButton.Hint := Value;
end;

procedure TAntJvCustomComboEdit.SetButtonWidth(Value: Integer);
begin
  if csLoading in ComponentState then
    FStreamedButtonWidth := Value
  else
  if ButtonWidth <> Value then
  begin
    FBtnControl.Visible := Value > 1;
    if csCreating in ControlState then
    begin
      FBtnControl.Width := Value;
      FButton.Width := Value;
      with FButton do
        ControlStyle := ControlStyle - [csFixedWidth];
      RecreateGlyph;
    end
      //    else if (Value <> ButtonWidth) and (Value < ClientWidth) then begin
      //Polaris
    else
    if (Value <> ButtonWidth) and
      ((Assigned(Parent) and (Value < ClientWidth)) or
      (not Assigned(Parent) and (Value < Width))) then
    begin
      FButton.Width := Value;
      with FButton do
        ControlStyle := ControlStyle - [csFixedWidth];
      if HandleAllocated then
        RecreateWnd;
      RecreateGlyph;
    end;
  end;
end;

procedure TAntJvCustomComboEdit.SetClipboardCommands(const Value: TAntJvClipboardCommands);
begin
  if ClipboardCommands <> Value then
  begin
    inherited SetClipboardCommands(Value);
    ReadOnly := ClipboardCommands <= [caCopy];
  end;
end;

procedure TAntJvCustomComboEdit.SetDirectInput(Value: Boolean);
begin
  inherited ReadOnly := not Value or FReadOnly;
  FDirectInput := Value;
end;

procedure TAntJvCustomComboEdit.SetDisabledColor(const Value: TColor);
begin
  if FDisabledColor <> Value then
  begin
    FDisabledColor := Value;
    if not Enabled then
      Invalidate;
  end;
end;

procedure TAntJvCustomComboEdit.SetDisabledTextColor(const Value: TColor);
begin
  if FDisabledTextColor <> Value then
  begin
    FDisabledTextColor := Value;
    if not Enabled then
      Invalidate;
  end;
end;

procedure TAntJvCustomComboEdit.SetEditRect;
const
  CPixelsBetweenEditAndButton = 2;
var
  Loc: TRect;
  LLeft: Integer;
  LTop: Integer;
  LRight: Integer;
begin
  AdjustHeight;

  LTop := 0;
  LLeft := 0;
  LRight := 0;

  {$IFDEF DELPHI7_UP}
  { If flat and themes are enabled, move the left edge of the edit rectangle
    to the right, otherwise the theme edge paints over the border }
  if ThemeServices.ThemesEnabled then
  begin
    if BorderStyle = bsSingle then
    begin
      if not Ctl3D then
        LLeft := 3
      else
      begin
        LLeft := 1;
        LTop := 1;
      end;
    end;
  end;
  {$ENDIF}

  if NewStyleControls and (BorderStyle = bsSingle) then
  begin
    if Ctl3D then
      LRight := 1
    else
      LRight := 2;
  end;

  SetRect(Loc, LLeft, LTop, FBtnControl.Left + LRight - CPixelsBetweenEditAndButton, ClientHeight - 1);
  SendMessage(Handle, EM_SETRECTNP, 0, Longint(@Loc));

  //Polaris
  //  SendMessage(Handle, EM_SETMARGINS, EC_RIGHTMARGIN, MakeLong(0, FBtnControl.Width));
end;

procedure TAntJvCustomComboEdit.SetGlyph(Value: TBitmap);
begin
  {FButton.Glyph := Value;}
  ImageKind := ikCustom;
end;

procedure TAntJvCustomComboEdit.SetGlyphKind(Value: TGlyphKind);
begin
  ImageKind := TAntJvImageKind(Value);
end;

procedure TAntJvCustomComboEdit.SetGroupIndex(const Value: Integer);
begin
  FGroupIndex := Value;
  UpdateEdit;
end;

procedure TAntJvCustomComboEdit.SetImageIndex(const Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    if FImageKind = ikCustom then
      FButton.ImageIndex := FImageIndex;
  end;
end;

procedure TAntJvCustomComboEdit.SetImageKind(const Value: TAntJvImageKind);
begin
  if FImageKind <> Value then
  begin
    FImageKind := Value;
    RecreateGlyph;
    case FImageKind of
      ikCustom:
        begin
          FButton.Images := FImages;
          FButton.ImageIndex := FImageIndex;
        end;
      ikDefault:
        begin
          FButton.Images := DefaultImages;
          FButton.ImageIndex := DefaultImageIndex;
          { Default glyphs have a default width }
          if Assigned(FButton.Images) and (FButton.ImageIndex >= 0) then
            ButtonWidth := Max(FButton.Images.Width + 6, FButton.Width)
        end;
      ikDropDown:
        begin
          { Dropdown has a default width }
          ButtonWidth := GetSystemMetrics(SM_CXVSCROLL);
          with FButton do
            ControlStyle := ControlStyle + [csFixedWidth];
        end;
      ikEllipsis: ;
    end;
  end;
end;

procedure TAntJvCustomComboEdit.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
  if FImages <> nil then
    FImages.FreeNotification(Self)
  else
    SetImageIndex(-1);
  if ImageKind = ikCustom then
  begin
    FButton.Images := FImages;
    FButton.ImageIndex := FImageIndex;
  end;
end;

procedure TAntJvCustomComboEdit.SetNumGlyphs(Value: TNumGlyphs);
begin
  { Nothing }
end;

procedure TAntJvCustomComboEdit.SetPopupValue(const Value: Variant);
begin
  if FPopup <> nil then
    TAntJvPopupWindow(FPopup).SetValue(Value);
end;

procedure TAntJvCustomComboEdit.SetReadOnly(Value: Boolean);
begin
  if Value <> FReadOnly then
  begin
    FReadOnly := Value;
    inherited ReadOnly := Value or not FDirectInput;
  end;
end;

procedure TAntJvCustomComboEdit.SetShowCaret;
const
  CaretWidth: array [Boolean] of Byte = (1, 2);
begin
  CreateCaret(Handle, 0, CaretWidth[fsBold in Font.Style], GetTextHeight);
  ShowCaret(Handle);
end;

procedure TAntJvCustomComboEdit.ShowPopup(Origin: TPoint);
begin
  TAntJvPopupWindow(FPopup).Show(Origin);
end;

procedure TAntJvCustomComboEdit.UpdateBtnBounds;
var
  BtnRect: TRect;
begin
  if NewStyleControls then
    {$IFDEF DELPHI7_UP}
    if ThemeServices.ThemesEnabled then
    begin
      if BorderStyle = bsSingle then
      begin
        if Ctl3D then
          BtnRect := Bounds(Width - FButton.Width - 2, 0,
            FButton.Width, Height - 2)
        else
          BtnRect := Bounds(Width - FButton.Width - 1, 1,
            FButton.Width, Height - 2);
      end
      else
        BtnRect := Bounds(Width - FButton.Width, 0,
          FButton.Width, Height);
    end
    else
    {$ENDIF}
    begin
      if BorderStyle = bsSingle then
      begin
        if Ctl3D then
          BtnRect := Bounds(Width - FButton.Width - 4, 0,
            FButton.Width, Height - 4)
        else
          BtnRect := Bounds(Width - FButton.Width - 2, 2,
            FButton.Width, Height - 4)
      end
      else
        BtnRect := Bounds(Width - FButton.Width, 0,
          FButton.Width, Height);
    end
  else
    BtnRect := Bounds(Width - FButton.Width, 0, FButton.Width, Height);
  with BtnRect do
    FBtnControl.SetBounds(Left, Top, Right - Left, Bottom - Top);
  FButton.Height := FBtnControl.Height;
  SetEditRect;
end;

procedure TAntJvCustomComboEdit.UpdateEdit;
var
  I: Integer;
begin
  if Owner <> nil then
    for I := 0 to Owner.ComponentCount - 1 do
      if Owner.Components[I] is TAntJvCustomComboEdit then
        if ((Owner.Components[I].Name <> Self.Name) and
          ((Owner.Components[I] as TAntJvCustomComboEdit).FGroupIndex <> -1) and
          ((Owner.Components[I] as TAntJvCustomComboEdit).FGroupIndex = Self.FGroupIndex)) then
          (Owner.Components[I] as TAntJvCustomComboEdit).Caption := '';
end;

procedure TAntJvCustomComboEdit.UpdatePopupVisible;
begin
  FPopupVisible := (FPopup <> nil) and FPopup.Visible;
end;

procedure TAntJvCustomComboEdit.DoClearText;
begin
  Text := '';
end;

procedure TAntJvCustomComboEdit.DoClipboardCut;
begin
  if FDirectInput and not ReadOnly then
    inherited DoClipboardCut;
end;

procedure TAntJvCustomComboEdit.DoClipboardPaste;
begin
  if FDirectInput and not ReadOnly then
    inherited DoClipboardPaste;
  UpdateEdit;
end;

function TAntJvCustomComboEdit.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  Result := True;
  if csDestroying in ComponentState then
    Exit;
  if Enabled then
    Result := inherited DoPaintBackground(Canvas, Param)
  else
  begin
    Canvas.Brush.Color := FDisabledColor;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(ClientRect);
  end;
end;

procedure TAntJvCustomComboEdit.DoKillFocus(FocusedWnd: HWND);
begin
  inherited DoKillFocus(FocusedWnd);
  FFocused := False;
  PopupCloseUp(FPopup, False);
end;

{$IFDEF DELPHI7_UP}
procedure TAntJvCustomComboEdit.WMNCCalcSize(var Msg: TWMNCCalcSize);
begin
  if ThemeServices.ThemesEnabled and Ctl3D and (BorderStyle = bsSingle) then
    with Msg.CalcSize_Params^ do
      InflateRect(rgrc[0], 1, 1);
  inherited;
end;
{$ENDIF}

procedure TAntJvCustomComboEdit.WMNCHitTest(var Msg: TWMNCHitTest);
var
  P: TPoint;
begin
  inherited;
  if Msg.Result = HTCLIENT then
  begin
    P := Point(FBtnControl.Left, FBtnControl.Top);
    Windows.ClientToScreen(Handle, P);
    if Msg.XPos > P.X then
      Msg.Result := HTCAPTION;
  end;
end;

{$IFDEF DELPHI7_UP}
procedure TAntJvCustomComboEdit.WMNCPaint(var Msg: TWMNCPaint);
var
  DC: HDC;
  DrawRect: TRect;
  Details: TThemedElementDetails;
begin
  if ThemeServices.ThemesEnabled and Ctl3D and (BorderStyle = bsSingle) then
  begin
    DC := GetWindowDC(Handle);
    try
      GetWindowRect(Handle, DrawRect);
      OffsetRect(DrawRect, -DrawRect.Left, -DrawRect.Top);
      with DrawRect do
        ExcludeClipRect(DC, Left + 1, Top + 1, Right - 1, Bottom - 1);

      Details := ThemeServices.GetElementDetails(teEditTextNormal);
      ThemeServices.DrawElement(DC, Details, DrawRect);
    finally
      ReleaseDC(Handle, DC);
    end;
    Msg.Result := 0;
  end
  else
    inherited;
end;
{$ENDIF}

procedure TAntJvCustomComboEdit.WMPaint(var Msg: TWMPaint);
var
  Canvas: TControlCanvas;
begin
  if Enabled then
    inherited
  else
  begin
    Canvas := nil;
    if not PaintEdit(Self, Text, FAlignment, PopupVisible,
      DisabledTextColor, Focused and not PopupVisible, Canvas, Msg) then
      inherited;
    Canvas.Free;
  end;
end;

procedure TAntJvCustomComboEdit.DoSetFocus(FocusedWnd: HWND);
begin
  inherited DoSetFocus(FocusedWnd);
  FFocused := True;
  SetShowCaret;
end;

procedure TAntJvCustomComboEdit.DoBoundsChanged;
var
  MinHeight: Integer;
begin
  inherited DoBoundsChanged;
  if not (csLoading in ComponentState) then
  begin
    MinHeight := GetMinHeight;
    { text edit bug: if size to less than MinHeight, then edit ctrl does
      not display the text }
    if Height < MinHeight then
    begin
      Height := MinHeight;
      Exit;
    end;
  end
  else
  begin
    if (FPopup <> nil) and (csDesigning in ComponentState) then
      FPopup.SetBounds(0, Height + 1, 10, 10);
  end;
  UpdateBtnBounds;
end;

//=== TAntJvCustomComboEditActionLink ===========================================

function TAntJvCustomComboEditActionLink.IsCaptionLinked: Boolean;
begin
  Result := False;
end;

function TAntJvCustomComboEditActionLink.IsHintLinked: Boolean;
begin
  Result := (Action is TCustomAction) and (FClient is TAntJvCustomComboEdit) and
    ((FClient as TAntJvCustomComboEdit).ButtonHint = (Action as TCustomAction).Hint);
end;

function TAntJvCustomComboEditActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and (FClient is TAntJvCustomComboEdit) and
    ((FClient as TAntJvCustomComboEdit).ImageIndex = (Action as TCustomAction).ImageIndex);
end;

function TAntJvCustomComboEditActionLink.IsOnExecuteLinked: Boolean;
begin
  Result := (Action is TCustomAction) and (FClient is TAntJvCustomComboEdit) and
    (@(FClient as TAntJvCustomComboEdit).OnButtonClick = @Action.OnExecute);
end;

function TAntJvCustomComboEditActionLink.IsShortCutLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and (FClient is TAntJvCustomComboEdit) and
    ((FClient as TAntJvCustomComboEdit).ClickKey = (Action as TCustomAction).ShortCut);
end;

procedure TAntJvCustomComboEditActionLink.SetHint(const Value: string);
begin
  if IsHintLinked then
    (FClient as TAntJvCustomComboEdit).ButtonHint := Value;
end;

procedure TAntJvCustomComboEditActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked then
    (FClient as TAntJvCustomComboEdit).ImageIndex := Value;
end;

procedure TAntJvCustomComboEditActionLink.SetOnExecute(Value: TNotifyEvent);
begin
  if IsOnExecuteLinked then
    (FClient as TAntJvCustomComboEdit).OnButtonClick := Value;
end;

procedure TAntJvCustomComboEditActionLink.SetShortCut(Value: TShortCut);
begin
  if IsShortCutLinked then
    (FClient as TAntJvCustomComboEdit).ClickKey := Value;
end;

//=== TAntJvEditButton ==========================================================

constructor TAntJvEditButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStandard := True; // Polaris
  ControlStyle := ControlStyle + [csReplicatable];
  ParentShowHint := True;
end;

procedure TAntJvEditButton.Click;
begin
  if not FNoAction then
    inherited Click
  else
    FNoAction := False;
end;

procedure TAntJvEditButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and (Owner <> nil) then
    with TAntJvCustomComboEdit(Owner) do
    begin
      FNoAction := (FPopup <> nil) and FPopupVisible;
      if not FPopupVisible then
      begin
        if TabStop and CanFocus and (GetFocus <> Handle) then
          SetFocus;
      end
      else
        PopupCloseUp(FPopup, FStandard); // Polaris
    end;
end;

procedure TAntJvEditButton.Paint;
{$IFDEF DELPHI7_UP}
var
  ThemedState: TThemedComboBox;
  Details: TThemedElementDetails;
  R: TRect;
{$ENDIF}
begin
  {$IFDEF DELPHI7_UP}
  if ThemeServices.ThemesEnabled then
  begin
    if FDrawThemedDropDownBtn then
    begin
      if not Enabled then
        ThemedState := tcDropDownButtonDisabled
      else
      if FState in [rbsDown, rbsExclusive] then
        ThemedState := tcDropDownButtonPressed
      else
      if MouseInControl or IsDragging then
        ThemedState := tcDropDownButtonHot
      else
        ThemedState := tcDropDownButtonNormal;
      R := ClientRect;
      Details := ThemeServices.GetElementDetails(ThemedState);
      ThemeServices.DrawElement(Canvas.Handle, Details, R);
    end
    else
      inherited Paint;
  end
  else
  {$ENDIF}
  begin
    inherited Paint;
    if FState <> rbsDown then
      with Canvas do
      begin
        if NewStyleControls then
          Pen.Color := clBtnFace
        else
          Pen.Color := clBtnShadow;
        MoveTo(0, 0);
        LineTo(0, Self.Height - 1);
        Pen.Color := clBtnHighlight;
        MoveTo(1, 1);
        LineTo(1, Self.Height - 2);
      end;
  end;
end;

procedure TAntJvEditButton.PaintImage(Canvas: TCanvas; ARect: TRect;
  const Offset: TPoint; AState: TAntJvButtonState; DrawMark: Boolean);
begin
  if FDrawGlyph then
    TAntJvxButtonGlyph(ButtonGlyph).Draw(Canvas, ARect, Offset, '', Layout,
      Margin, Spacing, False, AState, 0)
  else
    inherited PaintImage(Canvas, ARect, Offset, AState, DrawMark);
end;

procedure TAntJvEditButton.WMContextMenu(var Msg: TWMContextMenu);
begin
  { (rb) Without this, we get 2 context menu's (1 from the form, another from
         the combo edit; don't know exactly what is causing this. (I guess
         it's related to FBtnControl being a TWinControl) }
  Msg.Result := 1;
end;

//=== TAntJvPopupWindow =========================================================

constructor TAntJvPopupWindow.Create(AOwner: TComponent);
begin
  // (p3) have to use CreateNew for VCL as well since there is no dfm
  inherited CreateNew(AOwner);
  FEditor := TWinControl(AOwner);
  ControlStyle := ControlStyle + [csNoDesignVisible, csReplicatable, csAcceptsControls];
  Visible := False;
  Ctl3D := False;
  ParentCtl3D := False;
  Parent := FEditor;
  // use same size on small and large font:
  Scaled := False;
end;

procedure TAntJvPopupWindow.CloseUp(Accept: Boolean);
begin
  if Assigned(FCloseUp) then
    FCloseUp(Self, Accept);
end;

procedure TAntJvPopupWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_POPUP or WS_BORDER or WS_CLIPCHILDREN;
    ExStyle := WS_EX_TOOLWINDOW;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
  end;
end;

function TAntJvPopupWindow.GetPopupText: string;
begin
  Result := '';
end;

procedure TAntJvPopupWindow.Hide;
begin
  SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
    SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
  Visible := False;
end;

procedure TAntJvPopupWindow.InvalidateEditor;
var
  R: TRect;
begin
  if FEditor is TAntJvCustomComboEdit then
    with TAntJvCustomComboEdit(FEditor) do
      SetRect(R, 0, 0, ClientWidth - FBtnControl.Width {Polaris - 2}, ClientHeight + 1)
  else
    R := FEditor.ClientRect;
  InvalidateRect(FEditor.Handle, @R, False);
  UpdateWindow(FEditor.Handle);
end;

procedure TAntJvPopupWindow.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
    CloseUp(PtInRect(ClientRect, Point(X, Y)));
end;

procedure TAntJvPopupWindow.Show(Origin: TPoint);
var
  Monitor: TMonitor;
begin
  if GetParentForm(Self) = nil then
  begin
    if Screen.ActiveCustomForm <> nil then
      Monitor := Screen.ActiveCustomForm.Monitor
    else
      Monitor := Application.MainForm.Monitor;
    Inc(Origin.X, Monitor.Left);
    Inc(Origin.Y, Monitor.Top);
  end;
  SetWindowPos(Handle, HWND_TOP, Origin.X, Origin.Y, 0, 0,
    SWP_NOACTIVATE or SWP_SHOWWINDOW or SWP_NOSIZE);
  Visible := True;
end;

procedure TAntJvPopupWindow.WMMouseActivate(var Msg: TMessage);
begin
  Msg.Result := MA_NOACTIVATE;
end;

//=== TAntJvComboEditXP =========================================================

procedure TAntJvComboEditXP.Loaded;
begin
  inherited;
//  ButtonFlat := IsThemedXP;
end;

initialization

finalization
  DestroyLocals;

end.
