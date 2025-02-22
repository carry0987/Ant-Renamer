{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvVCLUtils.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

unit AntJvVCLUtils;

interface

uses
  RTLConsts, Variants,
  Windows, Classes, Graphics, Forms, Controls, Dialogs, ImgList,

  AntJvTypes;

function RectHeight(R: TRect): Integer;
function RectWidth(R: TRect): Integer;

function MakeStr(C: Char; N: Integer): string; overload;
function MS(C: Char; N: Integer): string;
{ MakeStr return a string of length N filled with character C. }
function ReplaceStr(const S, Srch, Replace: string): string;
{ Returns string with every occurrence of Srch string replaced with
  Replace string. }
function DelChars(const S: string; Chr: Char): string;
{ DelChars return a string with all Chr characters removed. }
function DelBSpace(const S: string): string;
{ DelBSpace trims leading spaces from the given string. }
function DelESpace(const S: string): string;
{ DelESpace trims trailing spaces from the given string. }
function DelRSpace(const S: string): string;
{ DelRSpace trims leading and trailing spaces from the given string. }
procedure DrawBitmapTransparent(Dest: TCanvas; DstX, DstY: Integer;
  Bitmap: TBitmap; TransparentColor: TColor);
procedure DrawBitmapRectTransparent(Dest: TCanvas; DstX, DstY: Integer;
  SrcRect: TRect; Bitmap: TBitmap; TransparentColor: TColor);
procedure StretchBitmapRectTransparent(Dest: TCanvas; DstX, DstY, DstW,
  DstH: Integer; SrcRect: TRect; Bitmap: TBitmap; TransparentColor: TColor);
function MakeBitmap(ResID: PChar): TBitmap;
function MakeBitmapID(ResID: Word): TBitmap;
function MakeModuleBitmap(Module: THandle; ResID: PChar): TBitmap;
function CreateTwoColorsBrushPattern(Color1, Color2: TColor): TBitmap;
function CreateDisabledBitmap_NewStyle(FOriginal: TBitmap; BackColor: TColor): TBitmap;
function CreateDisabledBitmapEx(FOriginal: TBitmap; OutlineColor, BackColor,
  HighlightColor, ShadowColor: TColor; DrawHighlight: Boolean): TBitmap;
function CreateDisabledBitmap(FOriginal: TBitmap; OutlineColor: TColor): TBitmap;
function ChangeBitmapColor(Bitmap: TBitmap; Color, NewColor: TColor): TBitmap;
procedure AssignBitmapCell(Source: TGraphic; Dest: TBitmap; Cols, Rows,
  Index: Integer);
procedure ImageListDrawDisabled(Images: TCustomImageList; Canvas: TCanvas;
  X, Y, Index: Integer; HighLightColor, GrayColor: TColor;
  DrawHighlight: Boolean);
function MakeIcon(ResID: PChar): TIcon;
function MakeIconID(ResID: Word): TIcon;
function MakeModuleIcon(Module: THandle; ResID: PChar): TIcon;
function CreateBitmapFromIcon(Icon: TIcon; BackColor: TColor): TBitmap;
function CreateIconFromBitmap(Bitmap: TBitmap; TransparentColor: TColor): TIcon;

{ Service routines }

procedure NotImplemented;
procedure ResourceNotFound(ResID: PChar);
function PointInRect(const P: TPoint; const R: TRect): Boolean;
function PointInPolyRgn(const P: TPoint; const Points: array of TPoint): Boolean;
function PaletteColor(Color: TColor): Longint;
function WidthOf(R: TRect): Integer;
function HeightOf(R: TRect): Integer;
procedure PaintInverseRect(const RectOrg, RectEnd: TPoint);
procedure DrawInvertFrame(ScreenRect: TRect; Width: Integer);
procedure CopyParentImage(Control: TControl; Dest: TCanvas);
procedure Delay(MSecs: Longint);
procedure CenterControl(Control: TControl);
procedure ShowMDIClientEdge(ClientHandle: THandle; ShowEdge: Boolean);
function MakeVariant(const Values: array of Variant): Variant;
function CreateRotatedFont(Font: TFont; Angle: Integer): HFONT;
function MsgBox(const Caption, Text: string; Flags: Integer): Integer;
function MsgDlg(const Msg: string; AType: TMsgDlgType;
  AButtons: TMsgDlgButtons; HelpCtx: Longint): Word;
function FindPrevInstance(const MainFormClass, ATitle: string): HWND;
function ActivatePrevInstance(const MainFormClass, ATitle: string): Boolean;
function IsForegroundTask: Boolean;
procedure MergeForm(AControl: TWinControl; AForm: TForm; Align: TAlign;
  Show: Boolean);
function GetAveCharSize(Canvas: TCanvas): TPoint;
function MinimizeText(const Text: string; Canvas: TCanvas;
  MaxWidth: Integer): string;
procedure FreeUnusedOle;
procedure Beep;
function GetWindowsVersion: string;
function LoadDLL(const LibName: string): THandle;
function RegisterServer(const ModuleName: string): Boolean;

procedure UpdateTrackFont(TrackFont, Font: TFont; TrackOptions: TAntJvTrackFontOptions);

{ Gradient filling routine }

type
  TFillDirection = (fdTopToBottom, fdBottomToTop, fdLeftToRight, fdRightToLeft);

procedure GradientFillRect(Canvas: TCanvas; ARect: TRect; StartColor,
  EndColor: TColor; Direction: TFillDirection; Colors: Byte);

{ String routines }

function GetEnvVar(const VarName: string): string;
function AnsiUpperFirstChar(const S: string): string;
function StringToPChar(var S: string): PChar;
function StrPAlloc(const S: string): PChar;
procedure SplitCommandLine(const CmdLine: string; var ExeName,
  Params: string);
function DropT(const S: string): string;

{ Memory routines }

function AllocMemo(Size: Longint): Pointer;
function ReallocMemo(fpBlock: Pointer; Size: Longint): Pointer;
procedure FreeMemo(var fpBlock: Pointer);
function GetMemoSize(fpBlock: Pointer): Longint;
function CompareMem(fpBlock1, fpBlock2: Pointer; Size: Cardinal): Boolean;

{ Manipulate huge pointers routines }

procedure HugeInc(var HugePtr: Pointer; Amount: Longint);
procedure HugeDec(var HugePtr: Pointer; Amount: Longint);
function HugeOffset(HugePtr: Pointer; Amount: Longint): Pointer;
procedure HugeMove(Base: Pointer; Dst, Src, Size: Longint);
procedure HMemCpy(DstPtr, SrcPtr: Pointer; Amount: Longint);

{ Standard Windows colors that are not defined by Delphi }

const
  clCream = TColor($A6CAF0);
  clMoneyGreen = TColor($C0DCC0);
  clSkyBlue = TColor($FFFBF0);

{ Cursor routines }

const
  WaitCursor: TCursor = crHourGlass;

procedure StartWait;
procedure StopWait;
function DefineCursor(Instance: THandle; ResID: PChar): TCursor;
function LoadAniCursor(Instance: THandle; ResID: PChar): HCURSOR;

{ Windows API level routines }

procedure StretchBltTransparent(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
  SrcDC: HDC; SrcX, SrcY, SrcW, SrcH: Integer; Palette: HPALETTE;
  TransparentColor: TColorRef);
procedure DrawTransparentBitmap(DC: HDC; Bitmap: HBITMAP;
  DstX, DstY: Integer; TransparentColor: TColorRef);
function PaletteEntries(Palette: HPALETTE): Integer;
function WindowClassName(Wnd: HWND): string;
function ScreenWorkArea: TRect;
procedure SwitchToWindow(Wnd: HWND; Restore: Boolean);
procedure ActivateWindow(Wnd: HWND);
procedure ShowWinNoAnimate(Handle: HWND; CmdShow: Integer);
procedure CenterWindow(Wnd: HWND);
procedure ShadeRect(DC: HDC; const Rect: TRect);
procedure KillMessage(Wnd: HWND; Msg: Cardinal);

{ Convert dialog units to pixels and backwards }

function DialogUnitsToPixelsX(DlgUnits: Word): Word;
function DialogUnitsToPixelsY(DlgUnits: Word): Word;
function PixelsToDialogUnitsX(PixUnits: Word): Word;
function PixelsToDialogUnitsY(PixUnits: Word): Word;

{ Grid drawing }

type
  TVertAlignment = (vaTopJustify, vaCenter, vaBottomJustify);

procedure WriteText(ACanvas: TCanvas; ARect: TRect; DX, DY: Integer;
  const Text: string; Alignment: TAlignment; WordWrap: Boolean; ARightToLeft: Boolean = False);
procedure DrawCellText(Control: TCustomControl; ACol, ARow: Longint;
  const S: string; const ARect: TRect; Align: TAlignment;
  VertAlign: TVertAlignment); overload;
procedure DrawCellTextEx(Control: TCustomControl; ACol, ARow: Longint;
  const S: string; const ARect: TRect; Align: TAlignment;
  VertAlign: TVertAlignment; WordWrap: Boolean); overload;
procedure DrawCellText(Control: TCustomControl; ACol, ARow: Longint;
  const S: string; const ARect: TRect; Align: TAlignment;
  VertAlign: TVertAlignment; ARightToLeft: Boolean); overload;
procedure DrawCellTextEx(Control: TCustomControl; ACol, ARow: Longint;
  const S: string; const ARect: TRect; Align: TAlignment;
  VertAlign: TVertAlignment; WordWrap: Boolean; ARightToLeft: Boolean); overload;
procedure DrawCellBitmap(Control: TCustomControl; ACol, ARow: Longint;
  Bmp: TGraphic; Rect: TRect);

type
  TJvScreenCanvas = class(TCanvas)
  private
    FDeviceContext: HDC;
  protected
    procedure CreateHandle; override;
  public
    destructor Destroy; override;
    procedure SetOrigin(X, Y: Integer);
    procedure FreeHandle;
  end;

function CheckWin32(OK: Boolean): Boolean; { obsolete, use Win32Check }
procedure RaiseWin32Error(ErrorCode: DWORD);

implementation

uses
  SysUtils, Messages, Consts, Math,
  CommCtrl,
  SysConst;

const
  PaletteMask = $02000000;

function DelChars(const S: string; Chr: Char): string;
var
  I: Integer;
begin
  Result := S;
  for I := Length(Result) downto 1 do
  begin
    if Result[I] = Chr then
      Delete(Result, I, 1);
  end;
end;

function OSCheck(RetVal: Boolean): Boolean;
begin
  if not RetVal then
    RaiseLastOSError;
  Result := RetVal;
end;

{ Exceptions }

procedure ResourceNotFound(ResID: PChar);
var
  S: string;
begin
  if LongRec(ResID).Hi = 0 then
    S := IntToStr(LongRec(ResID).Lo)
  else
    S := StrPas(ResID);
  raise EResNotFound.CreateFmt(SResNotFound, [S]);
end;

{ Bitmaps }

function MakeModuleBitmap(Module: THandle; ResID: PChar): TBitmap;
begin
  Result := TBitmap.Create;
  try
    if Module <> 0 then
    begin
      if LongRec(ResID).Hi = 0 then
        Result.LoadFromResourceID(Module, LongRec(ResID).Lo)
      else
        Result.LoadFromResourceName(Module, StrPas(ResID));
    end
    else
    begin
      Result.Handle := LoadBitmap(Module, ResID);
      if Result.Handle = 0 then
        ResourceNotFound(ResID);
    end;
  except
    Result.Free;
    Result := nil;
  end;
end;

function MakeBitmap(ResID: PChar): TBitmap;
begin
  Result := MakeModuleBitmap(hInstance, ResID);
end;

function MakeBitmapID(ResID: Word): TBitmap;
begin
  Result := MakeModuleBitmap(hInstance, MakeIntResource(ResID));
end;

procedure AssignBitmapCell(Source: TGraphic; Dest: TBitmap;
  Cols, Rows, Index: Integer);
var
  CellWidth, CellHeight: Integer;
begin
  if (Source <> nil) and (Dest <> nil) then
  begin
    if Cols <= 0 then
      Cols := 1;
    if Rows <= 0 then
      Rows := 1;
    if Index < 0 then
      Index := 0;
    CellWidth := Source.Width div Cols;
    CellHeight := Source.Height div Rows;
    with Dest do
    begin
      Width := CellWidth;
      Height := CellHeight;
    end;
    if Source is TBitmap then
    begin
      Dest.Canvas.CopyRect(Bounds(0, 0, CellWidth, CellHeight),
        TBitmap(Source).Canvas, Bounds((Index mod Cols) * CellWidth,
        (Index div Cols) * CellHeight, CellWidth, CellHeight));
      Dest.TransparentColor := TBitmap(Source).TransparentColor;
    end
    else
    begin
      Dest.Canvas.Brush.Color := clSilver;
      Dest.Canvas.FillRect(Bounds(0, 0, CellWidth, CellHeight));
      Dest.Canvas.Draw(-(Index mod Cols) * CellWidth,
        -(Index div Cols) * CellHeight, Source);
    end;
    Dest.Transparent := Source.Transparent;
  end;
end;

type
  TJvParentControl = class(TWinControl);

procedure CopyParentImage(Control: TControl; Dest: TCanvas);
var
  I, Count, X, Y, SaveIndex: Integer;
  DC: HDC;
  R, SelfR, CtlR: TRect;
begin
  if (Control = nil) or (Control.Parent = nil) then
    Exit;
  Count := Control.Parent.ControlCount;
  DC := Dest.Handle;
  with Control.Parent do
    ControlState := ControlState + [csPaintCopy];
  try
    with Control do
    begin
      SelfR := Bounds(Left, Top, Width, Height);
      X := -Left;
      Y := -Top;
    end;
    { Copy parent control image }
    SaveIndex := SaveDC(DC);
    try
      SetViewportOrgEx(DC, X, Y, nil);
      IntersectClipRect(DC, 0, 0, Control.Parent.ClientWidth,
        Control.Parent.ClientHeight);
      with TJvParentControl(Control.Parent) do
      begin
        Perform(WM_ERASEBKGND, DC, 0);
        PaintWindow(DC);
      end;
    finally
      RestoreDC(DC, SaveIndex);
    end;
    { Copy images of graphic controls }
    for I := 0 to Count - 1 do
    begin
      if Control.Parent.Controls[I] = Control then
        Break
      else
      if (Control.Parent.Controls[I] <> nil) and
        (Control.Parent.Controls[I] is TGraphicControl) then
      begin
        with TGraphicControl(Control.Parent.Controls[I]) do
        begin
          CtlR := Bounds(Left, Top, Width, Height);
          if Bool(IntersectRect(R, SelfR, CtlR)) and Visible then
          begin
            ControlState := ControlState + [csPaintCopy];
            SaveIndex := SaveDC(DC);
            try
              SaveIndex := SaveDC(DC);
              SetViewportOrgEx(DC, Left + X, Top + Y, nil);
              IntersectClipRect(DC, 0, 0, Width, Height);
              Perform(WM_PAINT, DC, 0);
            finally
              RestoreDC(DC, SaveIndex);
              ControlState := ControlState - [csPaintCopy];
            end;
          end;
        end;
      end;
    end;
  finally
    with Control.Parent do
      ControlState := ControlState - [csPaintCopy];
  end;
end;

{ Transparent bitmap }

procedure StretchBltTransparent(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
  SrcDC: HDC; SrcX, SrcY, SrcW, SrcH: Integer; Palette: HPALETTE;
  TransparentColor: TColorRef);
var
  Color: TColorRef;
  bmAndBack, bmAndObject, bmAndMem, bmSave: HBITMAP;
  bmBackOld, bmObjectOld, bmMemOld, bmSaveOld: HBITMAP;
  MemDC, BackDC, ObjectDC, SaveDC: HDC;
  palDst, palMem, palSave, palObj: HPALETTE;
begin
  { Create some DCs to hold temporary data }
  BackDC := CreateCompatibleDC(DstDC);
  ObjectDC := CreateCompatibleDC(DstDC);
  MemDC := CreateCompatibleDC(DstDC);
  SaveDC := CreateCompatibleDC(DstDC);
  { Create a bitmap for each DC }
  bmAndObject := CreateBitmap(SrcW, SrcH, 1, 1, nil);
  bmAndBack := CreateBitmap(SrcW, SrcH, 1, 1, nil);
  bmAndMem := CreateCompatibleBitmap(DstDC, DstW, DstH);
  bmSave := CreateCompatibleBitmap(DstDC, SrcW, SrcH);
  { Each DC must select a bitmap object to store pixel data }
  bmBackOld := SelectObject(BackDC, bmAndBack);
  bmObjectOld := SelectObject(ObjectDC, bmAndObject);
  bmMemOld := SelectObject(MemDC, bmAndMem);
  bmSaveOld := SelectObject(SaveDC, bmSave);
  { Select palette }
  palDst := 0;
  palMem := 0;
  palSave := 0;
  palObj := 0;
  if Palette <> 0 then
  begin
    palDst := SelectPalette(DstDC, Palette, True);
    RealizePalette(DstDC);
    palSave := SelectPalette(SaveDC, Palette, False);
    RealizePalette(SaveDC);
    palObj := SelectPalette(ObjectDC, Palette, False);
    RealizePalette(ObjectDC);
    palMem := SelectPalette(MemDC, Palette, True);
    RealizePalette(MemDC);
  end;
  { Set proper mapping mode }
  SetMapMode(SrcDC, GetMapMode(DstDC));
  SetMapMode(SaveDC, GetMapMode(DstDC));
  { Save the bitmap sent here }
  BitBlt(SaveDC, 0, 0, SrcW, SrcH, SrcDC, SrcX, SrcY, SRCCOPY);
  { Set the background color of the source DC to the color,         }
  { contained in the parts of the bitmap that should be transparent }
  Color := SetBkColor(SaveDC, PaletteColor(TransparentColor));
  { Create the object mask for the bitmap by performing a BitBlt()  }
  { from the source bitmap to a monochrome bitmap                   }
  BitBlt(ObjectDC, 0, 0, SrcW, SrcH, SaveDC, 0, 0, SRCCOPY);
  { Set the background color of the source DC back to the original  }
  SetBkColor(SaveDC, Color);
  { Create the inverse of the object mask }
  BitBlt(BackDC, 0, 0, SrcW, SrcH, ObjectDC, 0, 0, NOTSRCCOPY);
  { Copy the background of the main DC to the destination }
  BitBlt(MemDC, 0, 0, DstW, DstH, DstDC, DstX, DstY, SRCCOPY);
  { Mask out the places where the bitmap will be placed }
  StretchBlt(MemDC, 0, 0, DstW, DstH, ObjectDC, 0, 0, SrcW, SrcH, SRCAND);
  { Mask out the transparent colored pixels on the bitmap }
  BitBlt(SaveDC, 0, 0, SrcW, SrcH, BackDC, 0, 0, SRCAND);
  { XOR the bitmap with the background on the destination DC }
  StretchBlt(MemDC, 0, 0, DstW, DstH, SaveDC, 0, 0, SrcW, SrcH, SRCPAINT);
  { Copy the destination to the screen }
  BitBlt(DstDC, DstX, DstY, DstW, DstH, MemDC, 0, 0, SRCCOPY);
  { Restore palette }
  if Palette <> 0 then
  begin
    SelectPalette(MemDC, palMem, False);
    SelectPalette(ObjectDC, palObj, False);
    SelectPalette(SaveDC, palSave, False);
    SelectPalette(DstDC, palDst, True);
  end;
  { Delete the memory bitmaps }
  DeleteObject(SelectObject(BackDC, bmBackOld));
  DeleteObject(SelectObject(ObjectDC, bmObjectOld));
  DeleteObject(SelectObject(MemDC, bmMemOld));
  DeleteObject(SelectObject(SaveDC, bmSaveOld));
  { Delete the memory DCs }
  DeleteDC(MemDC);
  DeleteDC(BackDC);
  DeleteDC(ObjectDC);
  DeleteDC(SaveDC);
end;

procedure DrawTransparentBitmapRect(DC: HDC; Bitmap: HBITMAP; DstX, DstY,
  DstW, DstH: Integer; SrcRect: TRect; TransparentColor: TColorRef);
var
  hdcTemp: HDC;
begin
  hdcTemp := CreateCompatibleDC(DC);
  try
    SelectObject(hdcTemp, Bitmap);
    with SrcRect do
      StretchBltTransparent(DC, DstX, DstY, DstW, DstH, hdcTemp,
        Left, Top, Right - Left, Bottom - Top, 0, TransparentColor);
  finally
    DeleteDC(hdcTemp);
  end;
end;

procedure DrawTransparentBitmap(DC: HDC; Bitmap: HBITMAP;
  DstX, DstY: Integer; TransparentColor: TColorRef);
var
  BM: Windows.TBitmap;
begin
  GetObject(Bitmap, SizeOf(BM), @BM);
  DrawTransparentBitmapRect(DC, Bitmap, DstX, DstY, BM.bmWidth, BM.bmHeight,
    Rect(0, 0, BM.bmWidth, BM.bmHeight), TransparentColor);
end;

procedure StretchBitmapTransparent(Dest: TCanvas; Bitmap: TBitmap;
  TransparentColor: TColor; DstX, DstY, DstW, DstH, SrcX, SrcY,
  SrcW, SrcH: Integer);
var
  CanvasChanging: TNotifyEvent;
begin
  if DstW <= 0 then
    DstW := Bitmap.Width;
  if DstH <= 0 then
    DstH := Bitmap.Height;
  if (SrcW <= 0) or (SrcH <= 0) then
  begin
    SrcX := 0;
    SrcY := 0;
    SrcW := Bitmap.Width;
    SrcH := Bitmap.Height;
  end;
  if not Bitmap.Monochrome then
    SetStretchBltMode(Dest.Handle, STRETCH_DELETESCANS);
  CanvasChanging := Bitmap.Canvas.OnChanging;
  Bitmap.Canvas.Lock;
  try
    Bitmap.Canvas.OnChanging := nil;
    if TransparentColor = clNone then
    begin
      StretchBlt(Dest.Handle, DstX, DstY, DstW, DstH, Bitmap.Canvas.Handle,
        SrcX, SrcY, SrcW, SrcH, Dest.CopyMode);
    end
    else
    begin
      if TransparentColor = clDefault then
        TransparentColor := Bitmap.Canvas.Pixels[0, Bitmap.Height - 1];
      if Bitmap.Monochrome then
        TransparentColor := clWhite
      else
        TransparentColor := ColorToRGB(TransparentColor);
      StretchBltTransparent(Dest.Handle, DstX, DstY, DstW, DstH,
        Bitmap.Canvas.Handle, SrcX, SrcY, SrcW, SrcH, Bitmap.Palette,
        TransparentColor);
    end;
  finally
    Bitmap.Canvas.OnChanging := CanvasChanging;
    Bitmap.Canvas.Unlock;
  end;
end;

procedure StretchBitmapRectTransparent(Dest: TCanvas; DstX, DstY,
  DstW, DstH: Integer; SrcRect: TRect; Bitmap: TBitmap;
  TransparentColor: TColor);
begin
  with SrcRect do
    StretchBitmapTransparent(Dest, Bitmap, TransparentColor,
      DstX, DstY, DstW, DstH, Left, Top, Right - Left, Bottom - Top);
end;

procedure DrawBitmapRectTransparent(Dest: TCanvas; DstX, DstY: Integer;
  SrcRect: TRect; Bitmap: TBitmap; TransparentColor: TColor);
begin
  with SrcRect do
    StretchBitmapTransparent(Dest, Bitmap, TransparentColor,
      DstX, DstY, Right - Left, Bottom - Top, Left, Top, Right - Left,
      Bottom - Top);
end;

procedure DrawBitmapTransparent(Dest: TCanvas; DstX, DstY: Integer;
  Bitmap: TBitmap; TransparentColor: TColor);
begin
  StretchBitmapTransparent(Dest, Bitmap, TransparentColor, DstX, DstY,
    Bitmap.Width, Bitmap.Height, 0, 0, Bitmap.Width, Bitmap.Height);
end;

{ ChangeBitmapColor. This function create new TBitmap object.
  You must destroy it outside by calling TBitmap.Free method. }

function ChangeBitmapColor(Bitmap: TBitmap; Color, NewColor: TColor): TBitmap;
var
  R: TRect;
begin
  Result := TBitmap.Create;
  try
    with Result do
    begin
      Height := Bitmap.Height;
      Width := Bitmap.Width;
      R := Bounds(0, 0, Width, Height);
      Canvas.Brush.Color := NewColor;
      Canvas.FillRect(R);
      Canvas.BrushCopy(R, Bitmap, R, Color);
    end;
  except
    Result.Free;
    raise;
  end;
end;

{ CreateDisabledBitmap. Creating TBitmap object with disable button glyph
  image. You must destroy it outside by calling TBitmap.Free method. }

const
  ROP_DSPDxax = $00E20746;

function CreateDisabledBitmap_NewStyle(FOriginal: TBitmap; BackColor: TColor): TBitmap;
var
  MonoBmp: TBitmap;
  R: TRect;
  DestDC, SrcDC: HDC;
begin
  R := Rect(0, 0, FOriginal.Width, FOriginal.Height);
  Result := TBitmap.Create;
  try
    Result.Width := FOriginal.Width;
    Result.Height := FOriginal.Height;
    Result.Canvas.Brush.Color := BackColor;
    Result.Canvas.FillRect(R);

    MonoBmp := TBitmap.Create;
    try
      MonoBmp.Width := FOriginal.Width;
      MonoBmp.Height := FOriginal.Height;
      MonoBmp.Canvas.Brush.Color := clWhite;
      MonoBmp.Canvas.FillRect(R);
      DrawBitmapTransparent(MonoBmp.Canvas, 0, 0, FOriginal, BackColor);
      MonoBmp.Monochrome := True;

      SrcDC := MonoBmp.Canvas.Handle;
      { Convert Black to clBtnHighlight }
      Result.Canvas.Brush.Color := clBtnHighlight;
      DestDC := Result.Canvas.Handle;
      Windows.SetTextColor(DestDC, clWhite);
      Windows.SetBkColor(DestDC, clBlack);
      BitBlt(DestDC, 1, 1, FOriginal.Width, FOriginal.Height, SrcDC, 0, 0, ROP_DSPDxax);
      { Convert Black to clBtnShadow }
      Result.Canvas.Brush.Color := clBtnShadow;
      DestDC := Result.Canvas.Handle;
      Windows.SetTextColor(DestDC, clWhite);
      Windows.SetBkColor(DestDC, clBlack);
      BitBlt(DestDC, 0, 0, FOriginal.Width, FOriginal.Height, SrcDC, 0, 0, ROP_DSPDxax);
    finally
      MonoBmp.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function CreateDisabledBitmapEx(FOriginal: TBitmap; OutlineColor, BackColor,
  HighlightColor, ShadowColor: TColor; DrawHighlight: Boolean): TBitmap;
var
  MonoBmp: TBitmap;
  IRect: TRect;
begin
  IRect := Rect(0, 0, FOriginal.Width, FOriginal.Height);
  Result := TBitmap.Create;
  try
    Result.Width := FOriginal.Width;
    Result.Height := FOriginal.Height;
    MonoBmp := TBitmap.Create;
    try
      with MonoBmp do
      begin
        Width := FOriginal.Width;
        Height := FOriginal.Height;
        Canvas.CopyRect(IRect, FOriginal.Canvas, IRect);
        HandleType := bmDDB;
        Canvas.Brush.Color := OutlineColor;
        if Monochrome then
        begin
          Canvas.Font.Color := clWhite;
          Monochrome := False;
          Canvas.Brush.Color := clWhite;
        end;
        Monochrome := True;
      end;
      with Result.Canvas do
      begin
        Brush.Color := BackColor;
        FillRect(IRect);
        if DrawHighlight then
        begin
          Brush.Color := HighlightColor;
          SetTextColor(Handle, clBlack);
          SetBkColor(Handle, clWhite);
          BitBlt(Handle, 1, 1, WidthOf(IRect), HeightOf(IRect),
            MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
        end;
        Brush.Color := ShadowColor;
        SetTextColor(Handle, clBlack);
        SetBkColor(Handle, clWhite);
        BitBlt(Handle, 0, 0, WidthOf(IRect), HeightOf(IRect),
          MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
      end;
    finally
      MonoBmp.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function CreateDisabledBitmap(FOriginal: TBitmap; OutlineColor: TColor): TBitmap;
begin
  Result := CreateDisabledBitmapEx(FOriginal, OutlineColor,
    clBtnFace, clBtnHighlight, clBtnShadow, True);
end;

procedure ImageListDrawDisabled(Images: TCustomImageList; Canvas: TCanvas;
  X, Y, Index: Integer; HighLightColor, GrayColor: TColor;
  DrawHighlight: Boolean);
var
  Bmp: TBitmap;
  SaveColor: TColor;
begin
  SaveColor := Canvas.Brush.Color;
  Bmp := TBitmap.Create;
  try
    Bmp.Width := Images.Width;
    Bmp.Height := Images.Height;
    with Bmp.Canvas do
    begin
      Brush.Color := clWhite;
      FillRect(Rect(0, 0, Images.Width, Images.Height));
      ImageList_Draw(Images.Handle, Index, Handle, 0, 0, ILD_MASK);
    end;
    Bmp.Monochrome := True;
    if DrawHighlight then
    begin
      Canvas.Brush.Color := HighLightColor;
      SetTextColor(Canvas.Handle, clWhite);
      SetBkColor(Canvas.Handle, clBlack);
      BitBlt(Canvas.Handle, X + 1, Y + 1, Images.Width,
        Images.Height, Bmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
    end;
    Canvas.Brush.Color := GrayColor;
    SetTextColor(Canvas.Handle, clWhite);
    SetBkColor(Canvas.Handle, clBlack);
    BitBlt(Canvas.Handle, X, Y, Images.Width,
      Images.Height, Bmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
  finally
    Bmp.Free;
    Canvas.Brush.Color := SaveColor;
  end;
end;

{ Brush Pattern }

function CreateTwoColorsBrushPattern(Color1, Color2: TColor): TBitmap;
var
  X, Y: Integer;
begin
  Result := TBitmap.Create;
  Result.Width := 8;
  Result.Height := 8;
  with Result.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := Color1;
    FillRect(Rect(0, 0, Result.Width, Result.Height));
    for Y := 0 to 7 do
      for X := 0 to 7 do
        if (Y mod 2) = (X mod 2) then { toggles between even/odd pixles }
          Pixels[X, Y] := Color2; { on even/odd rows }
  end;
end;

{ Icons }

function MakeIcon(ResID: PChar): TIcon;
begin
  Result := MakeModuleIcon(hInstance, ResID);
end;

function MakeIconID(ResID: Word): TIcon;
begin
  Result := MakeModuleIcon(hInstance, MakeIntResource(ResID));
end;

function MakeModuleIcon(Module: THandle; ResID: PChar): TIcon;
begin
  Result := TIcon.Create;
  Result.Handle := LoadIcon(Module, ResID);
  if Result.Handle = 0 then
  begin
    Result.Free;
    Result := nil;
  end;
end;

{ Create TBitmap object from TIcon }

function CreateBitmapFromIcon(Icon: TIcon; BackColor: TColor): TBitmap;
var
  IWidth, IHeight: Integer;
begin
  IWidth := Icon.Width;
  IHeight := Icon.Height;
  Result := TBitmap.Create;
  try
    Result.Width := IWidth;
    Result.Height := IHeight;
    with Result.Canvas do
    begin
      Brush.Color := BackColor;
      FillRect(Rect(0, 0, IWidth, IHeight));
      Draw(0, 0, Icon);
    end;
  except
    Result.Free;
    raise;
  end;
end;

function CreateIconFromBitmap(Bitmap: TBitmap; TransparentColor: TColor): TIcon;
begin
  with TImageList.CreateSize(Bitmap.Width, Bitmap.Height) do
  try
    if TransparentColor = clDefault then
      TransparentColor := Bitmap.TransparentColor;
    AllocBy := 1;
    AddMasked(Bitmap, TransparentColor);
    Result := TIcon.Create;
    try
      GetIcon(0, Result);
    except
      Result.Free;
      raise;
    end;
  finally
    Free;
  end;
end;

{ Dialog units }

function DialogUnitsToPixelsX(DlgUnits: Word): Word;
begin
  Result := (DlgUnits * LoWord(GetDialogBaseUnits)) div 4;
end;

function DialogUnitsToPixelsY(DlgUnits: Word): Word;
begin
  Result := (DlgUnits * HiWord(GetDialogBaseUnits)) div 8;
end;

function PixelsToDialogUnitsX(PixUnits: Word): Word;
begin
  Result := PixUnits * 4 div LoWord(GetDialogBaseUnits);
end;

function PixelsToDialogUnitsY(PixUnits: Word): Word;
begin
  Result := PixUnits * 8 div HiWord(GetDialogBaseUnits);
end;

{ Service routines }

type
  TJvHack = class(TCustomControl);

function LoadDLL(const LibName: string): THandle;
var
  ErrMode: Cardinal;
begin
  ErrMode := SetErrorMode(SEM_NOOPENFILEERRORBOX);
  Result := LoadLibrary(PChar(LibName));
  SetErrorMode(ErrMode);
  if Result < HINSTANCE_ERROR then
    OSCheck(False);
end;

function RegisterServer(const ModuleName: string): Boolean;
{ RegisterServer procedure written by Vladimir Gaitanoff, 2:50/430.2 }
type
  TProc = procedure;
var
  Handle: THandle;
  DllRegServ: Pointer;
begin
  Result := False;
  Handle := LoadDLL(ModuleName);
  try
    DllRegServ := GetProcAddress(Handle, 'DllRegisterServer');
    if Assigned(DllRegServ) then
    begin
      TProc(DllRegServ);
      Result := True;
    end;
  finally
    FreeLibrary(Handle);
  end;
end;

procedure Beep;
begin
  MessageBeep(0);
end;

procedure FreeUnusedOle;
begin
  FreeLibrary(GetModuleHandle('OleAut32'));
end;

procedure NotImplemented;
begin
  Screen.Cursor := crDefault;
  MessageDlg('Function not yet implemented', mtInformation, [mbOk], 0);
  Abort;
end;

procedure PaintInverseRect(const RectOrg, RectEnd: TPoint);
var
  DC: HDC;
  R: TRect;
begin
  DC := GetDC(0);
  try
    R := Rect(RectOrg.X, RectOrg.Y, RectEnd.X, RectEnd.Y);
    InvertRect(DC, R);
  finally
    ReleaseDC(0, DC);
  end;
end;

procedure DrawInvertFrame(ScreenRect: TRect; Width: Integer);
var
  DC: HDC;
  I: Integer;
begin
  DC := GetDC(0);
  try
    for I := 1 to Width do
    begin
      DrawFocusRect(DC, ScreenRect);
      InflateRect(ScreenRect, -1, -1);
    end;
  finally
    ReleaseDC(0, DC);
  end;
end;

function WidthOf(R: TRect): Integer;
begin
  Result := R.Right - R.Left;
end;

function HeightOf(R: TRect): Integer;
begin
  Result := R.Bottom - R.Top;
end;

function PointInRect(const P: TPoint; const R: TRect): Boolean;
begin
  with R do
    Result := (Left <= P.X) and (Top <= P.Y) and
      (Right >= P.X) and (Bottom >= P.Y);
end;

function PointInPolyRgn(const P: TPoint; const Points: array of TPoint): Boolean;
type
  PPoints = ^TPoints;
  TPoints = array [0..0] of TPoint;
var
  Rgn: HRGN;
begin
  Rgn := CreatePolygonRgn(PPoints(@Points)^, High(Points) + 1, WINDING);
  try
    Result := PtInRegion(Rgn, P.X, P.Y);
  finally
    DeleteObject(Rgn);
  end;
end;

function PaletteColor(Color: TColor): Longint;
begin
  Result := ColorToRGB(Color) or PaletteMask;
end;

procedure KillMessage(Wnd: HWND; Msg: Cardinal);
{ Delete the requested message from the queue, but throw back }
{ any WM_QUIT msgs that PeekMessage may also return.          }
{ Copied from DbGrid.pas                                      }
var
  M: TMsg;
begin
  M.Message := 0;
  if PeekMessage(M, Wnd, Msg, Msg, PM_REMOVE) and (M.Message = WM_QUIT) then
    PostQuitMessage(M.WParam);
end;

function CreateRotatedFont(Font: TFont; Angle: Integer): HFONT;
var
  LogFont: TLogFont;
begin
  FillChar(LogFont, SizeOf(LogFont), 0);
  with LogFont do
  begin
    lfHeight := Font.Height;
    lfWidth := 0;
    lfEscapement := Angle * 10;
    lfOrientation := 0;
    if fsBold in Font.Style then
      lfWeight := FW_BOLD
    else
      lfWeight := FW_NORMAL;
    lfItalic := Ord(fsItalic in Font.Style);
    lfUnderline := Ord(fsUnderline in Font.Style);
    lfStrikeOut := Byte(fsStrikeOut in Font.Style);
    lfCharSet := Byte(Font.Charset);
    if AnsiCompareText(Font.Name, 'Default') = 0 then
      StrPCopy(lfFaceName, DefFontData.Name)
    else
      StrPCopy(lfFaceName, Font.Name);
    lfQuality := DEFAULT_QUALITY;
    lfOutPrecision := OUT_DEFAULT_PRECIS;
    lfClipPrecision := CLIP_DEFAULT_PRECIS;
    case Font.Pitch of
      fpVariable:
        lfPitchAndFamily := VARIABLE_PITCH;
      fpFixed:
        lfPitchAndFamily := FIXED_PITCH;
    else
      lfPitchAndFamily := DEFAULT_PITCH;
    end;
  end;
  Result := CreateFontIndirect(LogFont);
end;

procedure Delay(MSecs: Longint);
var
  FirstTickCount, Now: Longint;
begin
  FirstTickCount := GetTickCount;
  repeat
    Application.ProcessMessages;
    { allowing access to other controls, etc. }
    Now := GetTickCount;
  until (Now - FirstTickCount >= MSecs) or (Now < FirstTickCount);
end;

function PaletteEntries(Palette: HPALETTE): Integer;
begin
  GetObject(Palette, SizeOf(Integer), @Result);
end;

procedure CenterControl(Control: TControl);
var
  X, Y: Integer;
begin
  X := Control.Left;
  Y := Control.Top;
  if Control is TForm then
  begin
    with Control do
    begin
      if (TForm(Control).FormStyle = fsMDIChild) and
        (Application.MainForm <> nil) then
      begin
        X := (Application.MainForm.ClientWidth - Width) div 2;
        Y := (Application.MainForm.ClientHeight - Height) div 2;
      end
      else
      begin
        X := (Screen.Width - Width) div 2;
        Y := (Screen.Height - Height) div 2;
      end;
    end;
  end
  else
  if Control.Parent <> nil then
  begin
    with Control do
    begin
      Parent.HandleNeeded;
      X := (Parent.ClientWidth - Width) div 2;
      Y := (Parent.ClientHeight - Height) div 2;
    end;
  end;
  if X < 0 then
    X := 0;
  if Y < 0 then
    Y := 0;
  with Control do
    SetBounds(X, Y, Width, Height);
end;

procedure FitRectToScreen(var Rect: TRect);
var
  X, Y, Delta: Integer;
begin
  X := GetSystemMetrics(SM_CXSCREEN);
  Y := GetSystemMetrics(SM_CYSCREEN);
  with Rect do
  begin
    if Right > X then
    begin
      Delta := Right - Left;
      Right := X;
      Left := Right - Delta;
    end;
    if Left < 0 then
    begin
      Delta := Right - Left;
      Left := 0;
      Right := Left + Delta;
    end;
    if Bottom > Y then
    begin
      Delta := Bottom - Top;
      Bottom := Y;
      Top := Bottom - Delta;
    end;
    if Top < 0 then
    begin
      Delta := Bottom - Top;
      Top := 0;
      Bottom := Top + Delta;
    end;
  end;
end;

procedure CenterWindow(Wnd: HWND);
var
  R: TRect;
begin
  GetWindowRect(Wnd, R);
  R := Rect((GetSystemMetrics(SM_CXSCREEN) - R.Right + R.Left) div 2,
    (GetSystemMetrics(SM_CYSCREEN) - R.Bottom + R.Top) div 2,
    R.Right - R.Left, R.Bottom - R.Top);
  FitRectToScreen(R);
  SetWindowPos(Wnd, 0, R.Left, R.Top, 0, 0, SWP_NOACTIVATE or
    SWP_NOSIZE or SWP_NOZORDER);
end;

procedure MergeForm(AControl: TWinControl; AForm: TForm; Align: TAlign;
  Show: Boolean);
var
  R: TRect;
  AutoScroll: Boolean;
begin
  AutoScroll := AForm.AutoScroll;
  AForm.Hide;
  TJvHack(AForm).DestroyHandle;
  with AForm do
  begin
    BorderStyle := bsNone;
    BorderIcons := [];
    Parent := AControl;
  end;
  AControl.DisableAlign;
  try
    if Align <> alNone then
      AForm.Align := Align
    else
    begin
      R := AControl.ClientRect;
      AForm.SetBounds(R.Left + AForm.Left, R.Top + AForm.Top, AForm.Width,
        AForm.Height);
    end;
    AForm.AutoScroll := AutoScroll;
    AForm.Visible := Show;
  finally
    AControl.EnableAlign;
  end;
end;

{ ShowMDIClientEdge function has been copied from Inprise's FORMS.PAS unit,
  Delphi 4 version }

procedure ShowMDIClientEdge(ClientHandle: THandle; ShowEdge: Boolean);
var
  Style: Longint;
begin
  if ClientHandle <> 0 then
  begin
    Style := GetWindowLong(ClientHandle, GWL_EXSTYLE);
    if ShowEdge then
      if Style and WS_EX_CLIENTEDGE = 0 then
        Style := Style or WS_EX_CLIENTEDGE
      else
        Exit
    else
    if Style and WS_EX_CLIENTEDGE <> 0 then
      Style := Style and not WS_EX_CLIENTEDGE
    else
      Exit;
    SetWindowLong(ClientHandle, GWL_EXSTYLE, Style);
    SetWindowPos(ClientHandle, 0, 0, 0, 0, 0, SWP_FRAMECHANGED or SWP_NOACTIVATE or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
  end;
end;

function MakeVariant(const Values: array of Variant): Variant;
begin
  if High(Values) - Low(Values) > 1 then
    Result := VarArrayOf(Values)
  else
  if High(Values) - Low(Values) = 1 then
    Result := Values[Low(Values)]
  else
    Result := Null;
end;

{ Shade rectangle }

procedure ShadeRect(DC: HDC; const Rect: TRect);
const
  HatchBits: array [0..7] of Word = ($11, $22, $44, $88, $11, $22, $44, $88);
var
  Bitmap: HBITMAP;
  SaveBrush: HBrush;
  SaveTextColor, SaveBkColor: TColorRef;
begin
  Bitmap := CreateBitmap(8, 8, 1, 1, @HatchBits);
  SaveBrush := SelectObject(DC, CreatePatternBrush(Bitmap));
  try
    SaveTextColor := SetTextColor(DC, clWhite);
    SaveBkColor := SetBkColor(DC, clBlack);
    with Rect do
      PatBlt(DC, Left, Top, Right - Left, Bottom - Top, $00A000C9);
    SetBkColor(DC, SaveBkColor);
    SetTextColor(DC, SaveTextColor);
  finally
    DeleteObject(SelectObject(DC, SaveBrush));
    DeleteObject(Bitmap);
  end;
end;

function ScreenWorkArea: TRect;
begin
  if not SystemParametersInfo(SPI_GETWORKAREA, 0, @Result, 0) then
    with Screen do
      Result := Bounds(0, 0, Width, Height);
end;

function WindowClassName(Wnd: HWND): string;
var
  Buffer: array [0..255] of Char;
begin
  SetString(Result, Buffer, GetClassName(Wnd, Buffer, SizeOf(Buffer) - 1));
end;

function GetAnimation: Boolean;
var
  Info: TAnimationInfo;
begin
  Info.cbSize := SizeOf(TAnimationInfo);
  if SystemParametersInfo(SPI_GETANIMATION, SizeOf(Info), @Info, 0) then
    Result := Info.iMinAnimate <> 0
  else
    Result := False;
end;

procedure SetAnimation(Value: Boolean);
var
  Info: TAnimationInfo;
begin
  Info.cbSize := SizeOf(TAnimationInfo);
  BOOL(Info.iMinAnimate) := Value;
  SystemParametersInfo(SPI_SETANIMATION, SizeOf(Info), @Info, 0);
end;

procedure ShowWinNoAnimate(Handle: HWND; CmdShow: Integer);
var
  Animation: Boolean;
begin
  Animation := GetAnimation;
  if Animation then
    SetAnimation(False);
  ShowWindow(Handle, CmdShow);
  if Animation then
    SetAnimation(True);
end;

procedure SwitchToWindow(Wnd: HWND; Restore: Boolean);
begin
  if IsWindowEnabled(Wnd) then
  begin
    SetForegroundWindow(Wnd);
    if Restore and IsWindowVisible(Wnd) then
    begin
      if not IsZoomed(Wnd) then
        SendMessage(Wnd, WM_SYSCOMMAND, SC_RESTORE, 0);
      SetFocus(Wnd);
    end;
  end;
end;

function GetWindowParent(Wnd: HWND): HWND;
begin
  Result := GetWindowLong(Wnd, GWL_HWNDPARENT);
end;

procedure ActivateWindow(Wnd: HWND);
begin
  if Wnd <> 0 then
  begin
    ShowWinNoAnimate(Wnd, SW_SHOW);
    SetForegroundWindow(Wnd);
  end;
end;

function FindPrevInstance(const MainFormClass, ATitle: string): HWND;
var
  BufClass, BufTitle: PChar;
begin
  Result := 0;
  if (MainFormClass = '') and (ATitle = '') then
    Exit;
  BufClass := nil;
  BufTitle := nil;
  if (MainFormClass <> '') then
    BufClass := StrPAlloc(MainFormClass);
  if (ATitle <> '') then
    BufTitle := StrPAlloc(ATitle);
  try
    Result := FindWindow(BufClass, BufTitle);
  finally
    StrDispose(BufTitle);
    StrDispose(BufClass);
  end;
end;

function WindowsEnum(Handle: HWND; Param: Longint): Bool; export; stdcall;
begin
  if WindowClassName(Handle) = 'TAppBuilder' then
  begin
    Result := False;
    PLongint(Param)^ := 1;
  end
  else
    Result := True;
end;

function ActivatePrevInstance(const MainFormClass, ATitle: string): Boolean;
var
  PrevWnd, PopupWnd, ParentWnd: HWND;
  IsDelphi: Longint;
begin
  Result := False;
  PrevWnd := FindPrevInstance(MainFormClass, ATitle);
  if PrevWnd <> 0 then
  begin
    ParentWnd := GetWindowParent(PrevWnd);
    while (ParentWnd <> GetDesktopWindow) and (ParentWnd <> 0) do
    begin
      PrevWnd := ParentWnd;
      ParentWnd := GetWindowParent(PrevWnd);
    end;
    if WindowClassName(PrevWnd) = 'TApplication' then
    begin
      IsDelphi := 0;
      EnumThreadWindows(GetWindowTask(PrevWnd), @WindowsEnum,
        LPARAM(@IsDelphi));
      if Boolean(IsDelphi) then
        Exit;
      if IsIconic(PrevWnd) then
      begin { application is minimized }
        SendMessage(PrevWnd, WM_SYSCOMMAND, SC_RESTORE, 0);
        Result := True;
        Exit;
      end
      else
        ShowWinNoAnimate(PrevWnd, SW_SHOWNOACTIVATE);
    end
    else
      ActivateWindow(PrevWnd);
    PopupWnd := GetLastActivePopup(PrevWnd);
    if (PrevWnd <> PopupWnd) and IsWindowVisible(PopupWnd) and
      IsWindowEnabled(PopupWnd) then
    begin
      SetForegroundWindow(PopupWnd);
    end
    else
      ActivateWindow(PopupWnd);
    Result := True;
  end;
end;

{ Standard Windows MessageBox function }

function MsgBox(const Caption, Text: string; Flags: Integer): Integer;
begin
  Result := Application.MessageBox(PChar(Text), PChar(Caption), Flags);
end;

function MsgDlg(const Msg: string; AType: TMsgDlgType; AButtons: TMsgDlgButtons; HelpCtx: Longint): Word;
begin
  Result := MessageDlg(Msg, AType, AButtons, HelpCtx);
end;

{ Gradient fill procedure - displays a gradient beginning with a chosen    }
{ color and ending with another chosen color. Based on TGradientFill       }
{ component source code written by Curtis White, cwhite@teleport.com.      }

procedure GradientFillRect(Canvas: TCanvas; ARect: TRect; StartColor,
  EndColor: TColor; Direction: TFillDirection; Colors: Byte);
var
  StartRGB: array [0..2] of Byte; { Start RGB values }
  RGBDelta: array [0..2] of Integer; { Difference between start and end RGB values }
  ColorBand: TRect; { Color band rectangular coordinates }
  I, Delta: Integer;
  Brush: HBrush;
begin
  if IsRectEmpty(ARect) then
    Exit;
  if Colors < 2 then
  begin
    Brush := CreateSolidBrush(ColorToRGB(StartColor));
    FillRect(Canvas.Handle, ARect, Brush);
    DeleteObject(Brush);
    Exit;
  end;
  StartColor := ColorToRGB(StartColor);
  EndColor := ColorToRGB(EndColor);
  case Direction of
    fdTopToBottom, fdLeftToRight:
      begin
        { Set the Red, Green and Blue colors }
        StartRGB[0] := GetRValue(StartColor);
        StartRGB[1] := GetGValue(StartColor);
        StartRGB[2] := GetBValue(StartColor);
        { Calculate the difference between begin and end RGB values }
        RGBDelta[0] := GetRValue(EndColor) - StartRGB[0];
        RGBDelta[1] := GetGValue(EndColor) - StartRGB[1];
        RGBDelta[2] := GetBValue(EndColor) - StartRGB[2];
      end;
    fdBottomToTop, fdRightToLeft:
      begin
        { Set the Red, Green and Blue colors }
        { Reverse of TopToBottom and LeftToRight directions }
        StartRGB[0] := GetRValue(EndColor);
        StartRGB[1] := GetGValue(EndColor);
        StartRGB[2] := GetBValue(EndColor);
        { Calculate the difference between begin and end RGB values }
        { Reverse of TopToBottom and LeftToRight directions }
        RGBDelta[0] := GetRValue(StartColor) - StartRGB[0];
        RGBDelta[1] := GetGValue(StartColor) - StartRGB[1];
        RGBDelta[2] := GetBValue(StartColor) - StartRGB[2];
      end;
  end;
  { Calculate the color band's coordinates }
  ColorBand := ARect;
  if Direction in [fdTopToBottom, fdBottomToTop] then
  begin
    Colors := Max(2, Min(Colors, HeightOf(ARect)));
    Delta := HeightOf(ARect) div Colors;
  end
  else
  begin
    Colors := Max(2, Min(Colors, WidthOf(ARect)));
    Delta := WidthOf(ARect) div Colors;
  end;
  with Canvas.Pen do
  begin { Set the pen style and mode }
    Style := psSolid;
    Mode := pmCopy;
  end;
  { Perform the fill }
  if Delta > 0 then
  begin
    for I := 0 to Colors do
    begin
      case Direction of
        { Calculate the color band's top and bottom coordinates }
        fdTopToBottom, fdBottomToTop:
          begin
            ColorBand.Top := ARect.Top + I * Delta;
            ColorBand.Bottom := ColorBand.Top + Delta;
          end;
        { Calculate the color band's left and right coordinates }
        fdLeftToRight, fdRightToLeft:
          begin
            ColorBand.Left := ARect.Left + I * Delta;
            ColorBand.Right := ColorBand.Left + Delta;
          end;
      end;
      { Calculate the color band's color }
      Brush := CreateSolidBrush(RGB(
        StartRGB[0] + MulDiv(I, RGBDelta[0], Colors - 1),
        StartRGB[1] + MulDiv(I, RGBDelta[1], Colors - 1),
        StartRGB[2] + MulDiv(I, RGBDelta[2], Colors - 1)));
      FillRect(Canvas.Handle, ColorBand, Brush);
      DeleteObject(Brush);
    end;
  end;
  if Direction in [fdTopToBottom, fdBottomToTop] then
    Delta := HeightOf(ARect) mod Colors
  else
    Delta := WidthOf(ARect) mod Colors;
  if Delta > 0 then
  begin
    case Direction of
      { Calculate the color band's top and bottom coordinates }
      fdTopToBottom, fdBottomToTop:
        begin
          ColorBand.Top := ARect.Bottom - Delta;
          ColorBand.Bottom := ColorBand.Top + Delta;
        end;
      { Calculate the color band's left and right coordinates }
      fdLeftToRight, fdRightToLeft:
        begin
          ColorBand.Left := ARect.Right - Delta;
          ColorBand.Right := ColorBand.Left + Delta;
        end;
    end;
    case Direction of
      fdTopToBottom, fdLeftToRight:
        Brush := CreateSolidBrush(EndColor);
    else {fdBottomToTop, fdRightToLeft }
      Brush := CreateSolidBrush(StartColor);
    end;
    FillRect(Canvas.Handle, ColorBand, Brush);
    DeleteObject(Brush);
  end;
end;

function MinimizeText(const Text: string; Canvas: TCanvas;
  MaxWidth: Integer): string;
var
  I: Integer;
begin
  Result := Text;
  I := 1;
  while (I <= Length(Text)) and (Canvas.TextWidth(Result) > MaxWidth) do
  begin
    Inc(I);
    Result := Copy(Text, 1, Max(0, Length(Text) - I)) + '...';
  end;
end;

function GetAveCharSize(Canvas: TCanvas): TPoint;
var
  I: Integer;
  Buffer: array [0..51] of Char;
begin
  for I := 0 to 25 do
    Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do
    Buffer[I + 26] := Chr(I + Ord('a'));
  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;

{ Memory routines }

function AllocMemo(Size: Longint): Pointer;
begin
  if Size > 0 then
    Result := GlobalAllocPtr(HeapAllocFlags or GMEM_ZEROINIT, Size)
  else
    Result := nil;
end;

function ReallocMemo(fpBlock: Pointer; Size: Longint): Pointer;
begin
  Result := GlobalReallocPtr(fpBlock, Size,
    HeapAllocFlags or GMEM_ZEROINIT);
end;

procedure FreeMemo(var fpBlock: Pointer);
begin
  if fpBlock <> nil then
  begin
    GlobalFreePtr(fpBlock);
    fpBlock := nil;
  end;
end;

function GetMemoSize(fpBlock: Pointer): Longint;
var
  hMem: THandle;
begin
  Result := 0;
  if fpBlock <> nil then
  begin
    hMem := GlobalHandle(fpBlock);
    if hMem <> 0 then
      Result := GlobalSize(hMem);
  end;
end;

function CompareMem(fpBlock1, fpBlock2: Pointer; Size: Cardinal): Boolean; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,fpBlock1
        MOV     EDI,fpBlock2
        MOV     ECX,Size
        MOV     EDX,ECX
        XOR     EAX,EAX
        AND     EDX,3
        SHR     ECX,2
        REPE    CMPSD
        JNE     @@2
        MOV     ECX,EDX
        REPE    CMPSB
        JNE     @@2
@@1:    INC     EAX
@@2:    POP     EDI
        POP     ESI
end;

{ Manipulate huge pointers routines by Ray Lischner, The Waite Group, Inc. }

procedure HugeInc(var HugePtr: Pointer; Amount: Longint);
begin
  HugePtr := PChar(HugePtr) + Amount;
end;

procedure HugeDec(var HugePtr: Pointer; Amount: Longint);
begin
  HugePtr := PChar(HugePtr) - Amount;
end;

function HugeOffset(HugePtr: Pointer; Amount: Longint): Pointer;
begin
  Result := PChar(HugePtr) + Amount;
end;

procedure HMemCpy(DstPtr, SrcPtr: Pointer; Amount: Longint);
begin
  Move(SrcPtr^, DstPtr^, Amount);
end;

procedure HugeMove(Base: Pointer; Dst, Src, Size: Longint);
var
  SrcPtr, DstPtr: PChar;
begin
  SrcPtr := PChar(Base) + Src * SizeOf(Pointer);
  DstPtr := PChar(Base) + Dst * SizeOf(Pointer);
  Move(SrcPtr^, DstPtr^, Size * SizeOf(Pointer));
end;

{ String routines }

{ function GetParamStr copied from SYSTEM.PAS unit of Delphi 2.0 }

function GetParamStr(P: PChar; var Param: string): PChar;
var
  Len: Integer;
  Buffer: array [Byte] of Char;
begin
  while True do
  begin
    while (P[0] <> #0) and (P[0] <= ' ') do
      Inc(P);
    if (P[0] = '"') and (P[1] = '"') then
      Inc(P, 2)
    else
      Break;
  end;
  Len := 0;
  while P[0] > ' ' do
    if P[0] = '"' then
    begin
      Inc(P);
      while (P[0] <> #0) and (P[0] <> '"') do
      begin
        Buffer[Len] := P[0];
        Inc(Len);
        Inc(P);
      end;
      if P[0] <> #0 then
        Inc(P);
    end
    else
    begin
      Buffer[Len] := P[0];
      Inc(Len);
      Inc(P);
    end;
  SetString(Param, Buffer, Len);
  Result := P;
end;

function ParamCountFromCommandLine(CmdLine: PChar): Integer;
var
  S: string;
  P: PChar;
begin
  P := CmdLine;
  Result := 0;
  while True do
  begin
    P := GetParamStr(P, S);
    if S = '' then
      Break;
    Inc(Result);
  end;
end;

function ParamStrFromCommandLine(CmdLine: PChar; Index: Integer): string;
var
  P: PChar;
begin
  P := CmdLine;
  while True do
  begin
    P := GetParamStr(P, Result);
    if (Index = 0) or (Result = '') then
      Break;
    Dec(Index);
  end;
end;

procedure SplitCommandLine(const CmdLine: string; var ExeName,
  Params: string);
var
  Buffer: PChar;
  Cnt, I: Integer;
  S: string;
begin
  ExeName := '';
  Params := '';
  Buffer := StrPAlloc(CmdLine);
  try
    Cnt := ParamCountFromCommandLine(Buffer);
    if Cnt > 0 then
    begin
      ExeName := ParamStrFromCommandLine(Buffer, 0);
      for I := 1 to Cnt - 1 do
      begin
        S := ParamStrFromCommandLine(Buffer, I);
        if Pos(' ', S) > 0 then
          S := '"' + S + '"';
        Params := Params + S;
        if I < Cnt - 1 then
          Params := Params + ' ';
      end;
    end;
  finally
    StrDispose(Buffer);
  end;
end;

function AnsiUpperFirstChar(const S: string): string;
var
  Temp: string[1];
begin
  Result := AnsiLowerCase(S);
  if S <> '' then
  begin
    Temp := Result[1];
    Temp := AnsiUpperCase(Temp);
    Result[1] := Temp[1];
  end;
end;

function StrPAlloc(const S: string): PChar;
begin
  Result := StrPCopy(StrAlloc(Length(S) + 1), S);
end;

function StringToPChar(var S: string): PChar;
begin
  Result := PChar(S);
end;

function DropT(const S: string): string;
begin
  if (UpCase(S[1]) = 'T') and (Length(S) > 1) then
    Result := Copy(S, 2, MaxInt)
  else
    Result := S;
end;

{ Cursor routines }

function LoadAniCursor(Instance: THandle; ResID: PChar): HCURSOR;
{ Unfortunately I don't know how we can load animated cursor from
  executable resource directly. So I write this routine using temporary
  file and LoadCursorFromFile function. }
var
  S: TFileStream;
  Path, FileName: array [0..MAX_PATH] of Char;
  Rsrc: HRSRC;
  Res: THandle;
  Data: Pointer;
begin
  Result := 0;
  Rsrc := FindResource(Instance, ResID, RT_ANICURSOR);
  if Rsrc <> 0 then
  begin
    OSCheck(GetTempPath(MAX_PATH, Path) <> 0);
    OSCheck(GetTempFileName(Path, 'ANI', 0, FileName) <> 0);
    try
      Res := LoadResource(Instance, Rsrc);
      try
        Data := LockResource(Res);
        if Data <> nil then
        try
          S := TFileStream.Create(StrPas(FileName), fmCreate);
          try
            S.WriteBuffer(Data^, SizeOfResource(Instance, Rsrc));
          finally
            S.Free;
          end;
          Result := LoadCursorFromFile(FileName);
        finally
          UnlockResource(Res);
        end;
      finally
        FreeResource(Res);
      end;
    finally
      Windows.DeleteFile(FileName);
    end;
  end;
end;

function DefineCursor(Instance: THandle; ResID: PChar): TCursor;
var
  Handle: HCURSOR;
begin
  Handle := LoadCursor(Instance, ResID);
  if Handle = 0 then
    Handle := LoadAniCursor(Instance, ResID);
  if Handle = 0 then
    ResourceNotFound(ResID);
  for Result := 100 to High(TCursor) do { Look for an unassigned cursor index }
    if (Screen.Cursors[Result] = Screen.Cursors[crDefault]) then
    begin
      Screen.Cursors[Result] := Handle;
      Exit;
    end;
  DestroyCursor(Handle);
  raise EOutOfResources.Create(SOutOfResources);
end;

// (rom) changed to var
var
  WaitCount: Integer = 0;
  SaveCursor: TCursor = crDefault;

procedure StartWait;
begin
  if WaitCount = 0 then
  begin
    SaveCursor := Screen.Cursor;
    Screen.Cursor := WaitCursor;
  end;
  Inc(WaitCount);
end;

procedure StopWait;
begin
  if WaitCount > 0 then
  begin
    Dec(WaitCount);
    if WaitCount = 0 then
      Screen.Cursor := SaveCursor;
  end;
end;

{ Grid drawing }

// (rom) changed to var
var
  DrawBitmap: TBitmap = nil;

procedure UsesBitmap;
begin
  if DrawBitmap = nil then
    DrawBitmap := TBitmap.Create;
end;

procedure ReleaseBitmap; far;
begin
  if DrawBitmap <> nil then
    DrawBitmap.Free;
  DrawBitmap := nil;
end;

procedure WriteText(ACanvas: TCanvas; ARect: TRect; DX, DY: Integer;
  const Text: string; Alignment: TAlignment; WordWrap: Boolean;
  ARightToLeft: Boolean = False);
const
  AlignFlags: array [TAlignment] of Integer =
    (DT_LEFT or DT_EXPANDTABS or DT_NOPREFIX,
     DT_RIGHT or DT_EXPANDTABS or DT_NOPREFIX,
     DT_CENTER or DT_EXPANDTABS or DT_NOPREFIX);
  WrapFlags: array [Boolean] of Integer = (0, DT_WORDBREAK);
  RTL: array [Boolean] of Integer = (0, DT_RTLREADING);
var
  B, R: TRect;
  I, Left: Integer;
begin
  UsesBitmap;
  I := ColorToRGB(ACanvas.Brush.Color);
  if not WordWrap and (Integer(GetNearestColor(ACanvas.Handle, I)) = I) and
    (Pos(#13, Text) = 0) then
  begin { Use ExtTextOut for solid colors }
    { In BiDi, because we changed the window origin, the text that does not
    change alignment, actually gets its alignment changed. }
    if (ACanvas.CanvasOrientation = coRightToLeft) and (not ARightToLeft) then
      ChangeBiDiModeAlignment(Alignment);
    case Alignment of
      taLeftJustify:
        Left := ARect.Left + DX;
      taRightJustify:
        Left := ARect.Right - ACanvas.TextWidth(Text) - 3;
    else { taCenter }
      Left := ARect.Left + (ARect.Right - ARect.Left) shr 1
        - (ACanvas.TextWidth(Text) shr 1);
    end;
    ACanvas.TextRect(ARect, Left, ARect.Top + DY, Text);
  end
  else
  begin { Use FillRect and DrawText for dithered colors }
    DrawBitmap.Canvas.Lock;
    try
      with DrawBitmap, ARect do { Use offscreen bitmap to eliminate flicker and }
      begin { brush origin tics in painting / scrolling.    }
        Width := Max(Width, Right - Left);
        Height := Max(Height, Bottom - Top);
        R := Rect(DX, DY, Right - Left - 1,
          Bottom - Top - 1);
        B := Rect(0, 0, Right - Left, Bottom - Top);
      end;
      with DrawBitmap.Canvas do
      begin
        Font := ACanvas.Font;
        Font.Color := ACanvas.Font.Color;
        Brush := ACanvas.Brush;
        Brush.Style := bsSolid;
        FillRect(B);
        SetBkMode(Handle, TRANSPARENT);
        if (ACanvas.CanvasOrientation = coRightToLeft) then
          ChangeBiDiModeAlignment(Alignment);
        DrawText(Handle, PChar(Text), Length(Text), R, AlignFlags[Alignment]
          or RTL[ARightToLeft] or WrapFlags[WordWrap]);
      end;
      ACanvas.CopyRect(ARect, DrawBitmap.Canvas, B);
    finally
      DrawBitmap.Canvas.Unlock;
    end;
  end;
end;

procedure DrawCellTextEx(Control: TCustomControl; ACol, ARow: Longint;
  const S: string; const ARect: TRect; Align: TAlignment;
  VertAlign: TVertAlignment; WordWrap: Boolean; ARightToLeft: Boolean);
const
  MinOffs = 2;
var
  H: Integer;
begin
  case VertAlign of
    vaTopJustify:
      H := MinOffs;
    vaCenter:
      with TJvHack(Control) do
        H := Max(1, (ARect.Bottom - ARect.Top -
          Canvas.TextHeight('W')) div 2);
  else {vaBottomJustify}
    begin
      with TJvHack(Control) do
        H := Max(MinOffs, ARect.Bottom - ARect.Top -
          Canvas.TextHeight('W'));
    end;
  end;
  WriteText(TJvHack(Control).Canvas, ARect, MinOffs, H, S, Align, WordWrap,
    ARightToLeft);
end;

procedure DrawCellText(Control: TCustomControl; ACol, ARow: Longint;
  const S: string; const ARect: TRect; Align: TAlignment;
  VertAlign: TVertAlignment; ARightToLeft: Boolean);
begin
  DrawCellTextEx(Control, ACol, ARow, S, ARect, Align, VertAlign,
    Align = taCenter, ARightToLeft);
end;

procedure DrawCellTextEx(Control: TCustomControl; ACol, ARow: Longint;
  const S: string; const ARect: TRect; Align: TAlignment;
  VertAlign: TVertAlignment; WordWrap: Boolean);
const
  MinOffs = 2;
var
  H: Integer;
begin
  case VertAlign of
    vaTopJustify:
      H := MinOffs;
    vaCenter:
      with TJvHack(Control) do
        H := Max(1, (ARect.Bottom - ARect.Top -
          Canvas.TextHeight('W')) div 2);
  else {vaBottomJustify}
    begin
      with TJvHack(Control) do
        H := Max(MinOffs, ARect.Bottom - ARect.Top -
          Canvas.TextHeight('W'));
    end;
  end;
  WriteText(TJvHack(Control).Canvas, ARect, MinOffs, H, S, Align, WordWrap);
end;

procedure DrawCellText(Control: TCustomControl; ACol, ARow: Longint;
  const S: string; const ARect: TRect; Align: TAlignment;
  VertAlign: TVertAlignment);
begin
  DrawCellTextEx(Control, ACol, ARow, S, ARect, Align, VertAlign,
    Align = taCenter);
end;

procedure DrawCellBitmap(Control: TCustomControl; ACol, ARow: Longint;
  Bmp: TGraphic; Rect: TRect);
begin
  Rect.Top := (Rect.Bottom + Rect.Top - Bmp.Height) div 2;
  Rect.Left := (Rect.Right + Rect.Left - Bmp.Width) div 2;
  TJvHack(Control).Canvas.Draw(Rect.Left, Rect.Top, Bmp);
end;

destructor TJvScreenCanvas.Destroy;
begin
  FreeHandle;
  inherited Destroy;
end;

procedure TJvScreenCanvas.CreateHandle;
begin
  if FDeviceContext = 0 then
    FDeviceContext := GetDC(0);
  Handle := FDeviceContext;
end;

procedure TJvScreenCanvas.FreeHandle;
begin
  if FDeviceContext <> 0 then
  begin
    Handle := 0;
    ReleaseDC(0, FDeviceContext);
    FDeviceContext := 0;
  end;
end;

procedure TJvScreenCanvas.SetOrigin(X, Y: Integer);
var
  FOrigin: TPoint;
begin
  SetWindowOrgEx(Handle, -X, -Y, @FOrigin);
end;

procedure RaiseWin32Error(ErrorCode: DWORD);
var
  Error: EOSError;
begin
  if ErrorCode <> ERROR_SUCCESS then
  begin
    Error := EOSError.CreateFmt(SOSError, [ErrorCode, SysErrorMessage(ErrorCode)]);
    Error.ErrorCode := ErrorCode;
    raise Error;
  end;
end;

{ Win32Check is used to check the return value of a Win32 API function
  which returns a BOOL to indicate success. }

function CheckWin32(OK: Boolean): Boolean;
begin
  Result := OSCheck(Ok);
end;

{ Check if this is the active Windows task }
{ Copied from implementation of FORMS.PAS  }

type
  PCheckTaskInfo = ^TCheckTaskInfo;
  TCheckTaskInfo = record
    FocusWnd: HWND;
    Found: Boolean;
  end;

function CheckTaskWindow(Window: HWND; Data: Longint): WordBool;
  stdcall;
begin
  Result := True;
  if PCheckTaskInfo(Data)^.FocusWnd = Window then
  begin
    Result := False;
    PCheckTaskInfo(Data)^.Found := True;
  end;
end;

function IsForegroundTask: Boolean;
var
  Info: TCheckTaskInfo;
begin
  Info.FocusWnd := GetActiveWindow;
  Info.Found := False;
  EnumThreadWindows(GetCurrentThreadID, @CheckTaskWindow, Longint(@Info));
  Result := Info.Found;
end;

function GetWindowsVersion: string;
const
  sWindowsVersion = 'Windows %s %d.%.2d.%.3d %s';
var
  Ver: TOsVersionInfo;
  Platfrm: string[4];
begin
  Ver.dwOSVersionInfoSize := SizeOf(Ver);
  GetVersionEx(Ver);
  with Ver do
  begin
    case dwPlatformId of
      VER_PLATFORM_WIN32s:
        Platfrm := '32s';
      VER_PLATFORM_WIN32_WINDOWS:
        begin
          dwBuildNumber := dwBuildNumber and $0000FFFF;
          if (dwMajorVersion > 4) or ((dwMajorVersion = 4) and
            (dwMinorVersion >= 10)) then
            Platfrm := '98'
          else
            Platfrm := '95';
        end;
      VER_PLATFORM_WIN32_NT: Platfrm := 'NT';
    end;
    Result := Trim(Format(sWindowsVersion, [Platfrm, dwMajorVersion,
      dwMinorVersion, dwBuildNumber, szCSDVersion]));
  end;
end;

// (rom) moved to file end to minimize W- switch impact at end of function

{$W+}
function GetEnvVar(const VarName: string): string;
var
  S: array [0..2048] of Char;
begin
  if GetEnvironmentVariable(PChar(VarName), S, SizeOf(S) - 1) > 0 then
    Result := StrPas(S)
  else
    Result := '';
end;
{$W-}

function DelBSpace(const S: string): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] = ' ') do
    Inc(I);
  Result := Copy(S, I, MaxInt);
end;

function DelESpace(const S: string): string;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and (S[I] = ' ') do
    Dec(I);
  Result := Copy(S, 1, I);
end;

function DelRSpace(const S: string): string;
begin
  Result := DelBSpace(DelESpace(S));
end;

function ReplaceStr(const S, Srch, Replace: string): string;
var
  I: Integer;
  Source: string;
begin
  Source := S;
  Result := '';
  repeat
    I := Pos(Srch, Source);
    if I > 0 then
    begin
      Result := Result + Copy(Source, 1, I - 1) + Replace;
      Source := Copy(Source, I + Length(Srch), MaxInt);
    end
    else
      Result := Result + Source;
  until I <= 0;
end;

function MakeStr(C: Char; N: Integer): string; overload;
begin
  if N < 1 then
    Result := ''
  else
  begin
    SetLength(Result, N);
    FillChar(Result[1], Length(Result), C);
  end;
end;

function MS(C: Char; N: Integer): string;
begin
  Result := MakeStr(C, N);
end;

procedure UpdateTrackFont(TrackFont, Font: TFont; TrackOptions:
  TAntJvTrackFontOptions);
begin
  if hoFollowFont in TrackOptions then
  begin
    if not (hoPreserveCharSet in TrackOptions) then
      TrackFont.CharSet := Font.CharSet;
    if not (hoPreserveColor in TrackOptions) then
      TrackFont.Color := Font.Color;
    if not (hoPreserveHeight in TrackOptions) then
      TrackFont.Height := Font.Height;
    if not (hoPreserveName in TrackOptions) then
      TrackFont.Name := Font.Name;
    if not (hoPreservePitch in TrackOptions) then
      TrackFont.Pitch := Font.Pitch;
    if not (hoPreserveStyle in TrackOptions) then
      TrackFont.Style := Font.Style;
  end;
end;

function RectHeight(R: TRect): Integer;
begin
  Result := R.Bottom - R.Top;
end;

function RectWidth(R: TRect): Integer;
begin
  Result := R.Right - R.Left;
end;


initialization

finalization
  ReleaseBitmap;
end.

