{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExStdCtrls.pas, released on 2004-01-04

The Initial Developer of the Original Code is Andreas Hausladen [Andreas.Hausladen@gmx.de]
Portions created by Andreas Hausladen are Copyright (C) 2003 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

Last Modified: 2004-01-16

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I ant.inc}
{$R-}

unit AntJvExControls;
interface
uses
  Windows, Messages, Graphics, Controls, Forms, StdCtrls,
  Classes, SysUtils,
  {$IFDEF DELPHI7_UP}
  AntJvTheme,
  {$ENDIF}
  AntJvTypes;

type
  IJvEditControlEvents = interface
    ['{C1AE5EF8-F6C4-4BD4-879E-17946FD0FBAB}']
    procedure DoClipboardPaste;
    procedure DoClipboardCopy;
    procedure DoClipboardCut;
    procedure DoUndo;
    procedure DoClearText;
  end;

  IJvWinControlEvents = interface
    ['{B5F7FB62-78F0-481D-AFF4-7A24ED6776A0}']
    procedure DoBoundsChanged;
    procedure CursorChanged;
    procedure ShowingChanged;
    procedure ShowHintChanged;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean);
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean);
    procedure DoGetDlgCode(var Code: TDlgCodes); // WM_GETDLGCODE
    procedure DoSetFocus(FocusedWnd: HWND);  // WM_SETFOCUS
    procedure DoKillFocus(FocusedWnd: HWND); // WM_KILLFOCUS
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; // WM_ERASEBKGND
  end;

  IJvControlEvents = interface
    ['{61FC57FF-D4DA-4840-B871-63DE804E9921}']
    procedure VisibleChanged;
    procedure EnabledChanged;
    procedure TextChanged;
    procedure FontChanged;
    procedure ColorChanged;
    procedure ParentFontChanged;
    procedure ParentColorChanged;
    procedure ParentShowHintChanged;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; // CM_DIALOGCHAR
    function HintShow(var HintInfo: THintInfo): Boolean;
    function HitTest(X, Y: Integer): Boolean; // CM_HITTEST
    procedure MouseEnter(AControl: TControl);
    procedure MouseLeave(AControl: TControl);
    procedure DoFocusChanged(Control: TWinControl);
    procedure SetAutoSize(Value: Boolean);
  end;

  TAntJvExCustomEdit = class(TCustomEdit,  IJvEditControlEvents, IJvWinControlEvents, IJvControlEvents)
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  protected
    procedure CMFocusChanged(var Msg: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure DoFocusChanged(Control: TWinControl); dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
    procedure DoSetFocus(FocusedWnd: HWND); dynamic;
    procedure DoKillFocus(FocusedWnd: HWND); dynamic;
    procedure DoBoundsChanged; dynamic;
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  private
    FClipboardCommands: TAntJvClipboardCommands;
  protected
    procedure DoUndo; dynamic;
    procedure DoClearText; dynamic;
    procedure DoClipboardPaste; dynamic;
    procedure DoClipboardCopy; dynamic;
    procedure DoClipboardCut; dynamic;
    procedure SetClipboardCommands(const Value: TAntJvClipboardCommands); virtual;

    property ClipboardCommands: TAntJvClipboardCommands read FClipboardCommands
      write SetClipboardCommands default [caCopy..caUndo];
  end;

  PInterface = ^IInterface;

  TFreeNotificationHelper = class(TComponent)
  private
    FInstance: TComponent;
    FIntfPtr: PInterface;
  protected
    procedure Notification(Component: TComponent; Operation: TOperation); override;
  public
    constructor Create(AInstance: TComponent; AIntfPtr: PInterface); reintroduce;
    destructor Destroy; override;
    function IsValid: Boolean;
  end;

  TAntJvExGraphicControl = class(TGraphicControl, IJvControlEvents)
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
  public
    procedure Dispatch(var Msg); override;
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  protected
    procedure CMFocusChanged(var Msg: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure DoFocusChanged(Control: TWinControl); dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

function InheritMsg(Instance: TControl; Msg: Integer; WParam, LParam: Integer): Integer; overload;
function InheritMsg(Instance: TControl; Msg: Integer): Integer; overload;
procedure DispatchMsg(Instance: TControl; var Msg);

function ShiftStateToKeyData(Shift: TShiftState): Longint;

procedure Control_ControlsListChanging(Instance: TControl; Control: TControl;
  Inserting: Boolean);
procedure Control_ControlsListChanged(Instance: TControl; Control: TControl;
  Inserting: Boolean);

procedure TCustomEdit_Undo(Instance: TWinControl);
procedure TCustomEdit_Copy(Instance: TWinControl);
procedure TCustomEdit_Paste(Instance: TWinControl);
procedure TCustomEdit_Cut(Instance: TWinControl);

type
  TDisptachMethod = procedure(Self: TObject; var Msg: TMessage);

implementation

function ShiftStateToKeyData(Shift: TShiftState): Longint;
const
  AltMask = $20000000;
begin
  Result := 0;
  if ssAlt in Shift then
    Result := Result or AltMask;
end;

procedure Control_ControlsListChanging(Instance: TControl; Control: TControl;
  Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Instance, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Instance, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure Control_ControlsListChanged(Instance: TControl; Control: TControl;
  Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Instance, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Instance, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

constructor TFreeNotificationHelper.Create(AInstance: TComponent; AIntfPtr: PInterface);
begin
  inherited Create(nil);
  FInstance := AInstance;
  FIntfPtr := AIntfPtr;
  FInstance.FreeNotification(Self);
end;

destructor TFreeNotificationHelper.Destroy;
begin
  if Assigned(FInstance) then
    FInstance.RemoveFreeNotification(Self);
  inherited Destroy;
end;

function TFreeNotificationHelper.IsValid: Boolean;
begin
  Result := FIntfPtr <> nil;
end;

procedure TFreeNotificationHelper.Notification(Component: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (Component = FInstance) then
  begin
    FInstance.RemoveFreeNotification(Self);
    FInstance := nil;
    FIntfPtr^ := nil;
    FIntfPtr := nil;
  end;
end;

procedure DispatchMsg(Instance: TControl; var Msg);
var
  {$IFDEF DELPHI7_UP}
  Temp: IJvDenySubClassing;
  {$ENDIF}
  IntfControl: IJvControlEvents;
  IntfWinControl: IJvWinControlEvents;
  IntfEditControl: IJvEditControlEvents;
  PMsg: PMessage;
  CallInherited: Boolean;
  Canvas: TCanvas;
  DlgCodes: TDlgCodes;
  IdSaveDC: Integer;
  Helper: TFreeNotificationHelper;
begin
  CallInherited := True;
  PMsg := @Msg;
  {$IFDEF DELPHI7_UP}
  if PMsg^.Msg = CM_DENYSUBCLASSING then
  begin
    PMsg^.Result := Ord(Instance.GetInterface(IJvDenySubClassing, Temp));
    Temp := nil; // does not destroy the control because it is derived from TComponent
   // Let the control handle CM_DENYSUBCLASSING the old way, too.
  end;
  {$ENDIF}

  { GetInterface is no problem because Instance is a TComponent derived class that
    is not released by an interface "Release". }
  if Instance.GetInterface(IJvControlEvents, IntfControl) then
  begin
    CallInherited := False;
    try
      with IntfControl do
        case PMsg^.Msg of
          CM_VISIBLECHANGED:
            VisibleChanged;
          CM_ENABLEDCHANGED:
            EnabledChanged;
          CM_FONTCHANGED:
            FontChanged;
          CM_COLORCHANGED:
            ColorChanged;
          CM_PARENTFONTCHANGED:
            ParentFontChanged;
          CM_PARENTCOLORCHANGED:
            ParentColorChanged;
          CM_PARENTSHOWHINTCHANGED:
            ParentShowHintChanged;
          CM_TEXTCHANGED:
            TextChanged;
          CM_HINTSHOW:
            PMsg^.Result := Integer(HintShow(TCMHintShow(PMsg^).HintInfo^));
          CM_HITTEST:
            with TCMHitTest(PMsg^) do
              Result := Integer(HitTest(XPos, YPos));
          CM_MOUSEENTER:
              MouseEnter(TControl(PMsg^.LParam));
          CM_MOUSELEAVE:
              MouseLeave(TControl(PMsg^.LParam));
          CM_DIALOGCHAR:
            with TCMDialogChar(PMsg^) do
              Result := Ord(WantKey(CharCode, KeyDataToShiftState(KeyData), WideChar(CharCode)));
          // CM_FOCUSCHANGED: handled by a message handler in the JvExVCL classes
        else
          CallInherited := True;
        end;
    finally
      IntfControl := nil;
    end;
  end;

  if CallInherited then
  begin
    if Instance.GetInterface(IJvWinControlEvents, IntfWinControl) then
    begin
      CallInherited := False;
      try
        with IntfWinControl do
          case PMsg^.Msg of
            CM_CURSORCHANGED:
              CursorChanged;
            CM_SHOWINGCHANGED:
              ShowingChanged;
            CM_SHOWHINTCHANGED:
              ShowHintChanged;
            CM_CONTROLLISTCHANGE:
              if PMsg^.LParam <> 0 then
                ControlsListChanging(TControl(PMsg^.WParam), True)
              else
                ControlsListChanged(TControl(PMsg^.WParam), False);
            CM_CONTROLCHANGE:
              if PMsg^.LParam = 0 then
                ControlsListChanging(TControl(PMsg^.WParam), False)
              else
                ControlsListChanged(TControl(PMsg^.WParam), True);

            WM_GETDLGCODE:
              begin
                Helper := TFreeNotificationHelper.Create(Instance, @IntfWinControl);
                try
                  PMsg^.Result := InheritMsg(Instance, PMsg^.Msg, PMsg^.WParam, PMsg^.LParam);

                  DlgCodes := [dcNative];
                  if PMsg^.Result and DLGC_WANTARROWS <> 0 then
                    Include(DlgCodes, dcWantArrows);
                  if PMsg^.Result and DLGC_WANTTAB <> 0 then
                    Include(DlgCodes, dcWantTab);
                  if PMsg^.Result and DLGC_WANTALLKEYS <> 0 then
                    Include(DlgCodes, dcWantAllKeys);
                  if PMsg^.Result and DLGC_WANTCHARS <> 0 then
                    Include(DlgCodes, dcWantChars);
                  if PMsg^.Result and DLGC_BUTTON <> 0 then
                    Include(DlgCodes, dcButton);

                  if Helper.IsValid then
                  begin
                    DoGetDlgCode(DlgCodes);

                    if not (dcNative in DlgCodes) then
                    begin
                      PMsg^.Result := 0;
                      if dcWantAllKeys in DlgCodes then
                        PMsg^.Result := PMsg^.Result or DLGC_WANTALLKEYS;
                      if dcWantArrows in DlgCodes then
                        PMsg^.Result := PMsg^.Result or DLGC_WANTARROWS;
                      if dcWantTab in DlgCodes then
                        PMsg^.Result := PMsg^.Result or DLGC_WANTTAB;
                      if dcWantChars in DlgCodes then
                        PMsg^.Result := PMsg^.Result or DLGC_WANTCHARS;
                      if dcButton in DlgCodes then
                        PMsg^.Result := PMsg^.Result or DLGC_BUTTON;
                    end;
                  end;
                finally
                  Helper.Free;
                end;
              end;
            WM_SETFOCUS:
              begin
                Helper := TFreeNotificationHelper.Create(Instance, @IntfWinControl);
                try
                  with PMsg^ do
                    Result := InheritMsg(Instance, Msg, WParam, LParam);
                  if Helper.IsValid then
                    DoSetFocus(HWND(PMsg^.WParam));
                finally
                  Helper.Free;
                end;
              end;
            WM_KILLFOCUS:
              begin
                Helper := TFreeNotificationHelper.Create(Instance, @IntfWinControl);
                try
                  with PMsg^ do
                    Result := InheritMsg(Instance, Msg, WParam, LParam);
                  if Helper.IsValid then
                    DoKillFocus(HWND(PMsg^.WParam));
                finally
                  Helper.Free;
                end;
              end;
            WM_SIZE:
              begin
                DoBoundsChanged;
                IntfWinControl := nil;
                with PMsg^ do
                  Result := InheritMsg(Instance, Msg, WParam, LParam);
              end;
          WM_ERASEBKGND:
            begin
              IdSaveDC := SaveDC(HDC(PMsg^.WParam)); // protect DC against Stock-Objects from Canvas
              Canvas := TCanvas.Create;
              try
                Canvas.Handle := HDC(PMsg^.WParam);
                PMsg^.Result := Ord(DoPaintBackground(Canvas, PMsg^.LParam));
              finally
                Canvas.Handle := 0;
                Canvas.Free;
                RestoreDC(HDC(PMsg^.WParam), IdSaveDC);
              end;
            end;
          else
            CallInherited := True;
          end;
      finally
        IntfWinControl := nil;
      end;
    end;
  end;

  if CallInherited then
  begin
    if Instance.GetInterface(IJvEditControlEvents, IntfEditControl) then
    begin
      CallInherited := False;
      try
        with IntfEditControl do
          case PMsg^.Msg of
            WM_PASTE:
              DoClipboardPaste;
            WM_COPY:
              DoClipboardCopy;
            WM_CUT:
              DoClipboardCut;
            WM_UNDO:
              DoUndo;
            WM_CLEAR:
              DoClearText;
          else
            CallInherited := True;
          end;
      finally
        IntfEditControl := nil;
      end;
    end;
  end;

  if CallInherited then
    with PMsg^ do
      Result := InheritMsg(Instance, Msg, WParam, LParam);
end;

function InheritMsg(Instance: TControl; Msg: Integer; WParam, LParam: Integer): Integer;
var
  Proc: TDisptachMethod;
  Mesg: TMessage;
begin
  Mesg.Msg := Msg;
  Mesg.WParam := WParam;
  Mesg.LParam := LParam;
  Mesg.Result := 0;
  Proc := @TObject.Dispatch;
  Proc(Instance, Mesg);
  Result := Mesg.Result;
end;

function InheritMsg(Instance: TControl; Msg: Integer): Integer;
begin
  Result := InheritMsg(Instance, Msg, 0, 0);
end;

procedure TAntJvExCustomEdit.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TAntJvExCustomEdit.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TAntJvExCustomEdit.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TAntJvExCustomEdit.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TAntJvExCustomEdit.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TAntJvExCustomEdit.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TAntJvExCustomEdit.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TAntJvExCustomEdit.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TAntJvExCustomEdit.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TAntJvExCustomEdit.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TAntJvExCustomEdit.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TAntJvExCustomEdit.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TAntJvExCustomEdit.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TAntJvExCustomEdit.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TAntJvExCustomEdit.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TAntJvExCustomEdit.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TAntJvExCustomEdit.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TAntJvExCustomEdit.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TAntJvExCustomEdit.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

procedure TAntJvExCustomEdit.CMFocusChanged(var Msg: TCMFocusChanged);
begin
  inherited;
  DoFocusChanged(Msg.Sender);
end;

procedure TAntJvExCustomEdit.DoFocusChanged(Control: TWinControl);
begin
end;
procedure TAntJvExCustomEdit.DoBoundsChanged;
begin
end;

procedure TAntJvExCustomEdit.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

procedure TAntJvExCustomEdit.DoSetFocus(FocusedWnd: HWND);
begin
end;

procedure TAntJvExCustomEdit.DoKillFocus(FocusedWnd: HWND);
begin
end;

function TAntJvExCustomEdit.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
end;

constructor TAntJvExCustomEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClipboardCommands := [caCopy..caUndo];
end;

destructor TAntJvExCustomEdit.Destroy;
begin

  inherited Destroy;
end;

procedure TAntJvExCustomEdit.DoClearText;
begin
 // (ahuser) there is no caClear so we restrict it to caCut
  if caCut in ClipboardCommands then
    InheritMsg(Self, WM_CLEAR, 0, 0);
end;

procedure TAntJvExCustomEdit.DoUndo;
begin
  if caUndo in ClipboardCommands then
    TCustomEdit_Undo(Self);
end;

procedure TAntJvExCustomEdit.DoClipboardPaste;
begin
  if caPaste in ClipboardCommands then
    TCustomEdit_Paste(Self);
end;

procedure TAntJvExCustomEdit.DoClipboardCopy;
begin
  if caCopy in ClipboardCommands then
    TCustomEdit_Copy(Self);
end;

procedure TAntJvExCustomEdit.DoClipboardCut;
begin
  if caCut in ClipboardCommands then
    TCustomEdit_Cut(Self);
end;

procedure TAntJvExCustomEdit.SetClipboardCommands(const Value: TAntJvClipboardCommands);
begin
  FClipboardCommands := Value;
end;

procedure TCustomEdit_Undo(Instance: TWinControl);
begin
  InheritMsg(Instance, WM_UNDO, 0, 0);
end;

procedure TCustomEdit_Copy(Instance: TWinControl);
begin
  InheritMsg(Instance, WM_COPY, 0, 0);
end;

procedure TCustomEdit_Cut(Instance: TWinControl);
begin
  InheritMsg(Instance, WM_CUT, 0, 0);
end;

procedure TCustomEdit_Paste(Instance: TWinControl);
begin
  InheritMsg(Instance, WM_PASTE, 0, 0);
end;

procedure TAntJvExGraphicControl.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TAntJvExGraphicControl.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TAntJvExGraphicControl.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TAntJvExGraphicControl.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TAntJvExGraphicControl.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TAntJvExGraphicControl.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TAntJvExGraphicControl.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TAntJvExGraphicControl.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TAntJvExGraphicControl.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TAntJvExGraphicControl.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TAntJvExGraphicControl.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TAntJvExGraphicControl.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TAntJvExGraphicControl.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TAntJvExGraphicControl.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TAntJvExGraphicControl.CMFocusChanged(var Msg: TCMFocusChanged);
begin
  inherited;
  DoFocusChanged(Msg.Sender);
end;

procedure TAntJvExGraphicControl.DoFocusChanged(Control: TWinControl);
begin
end;

constructor TAntJvExGraphicControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

destructor TAntJvExGraphicControl.Destroy;
begin

  inherited Destroy;
end;

end.
