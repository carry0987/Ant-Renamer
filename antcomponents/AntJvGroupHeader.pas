{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvGroupHeader.PAS, released on 2002-09-02.

The Initial Developer of the Original Code is Fernando Silva [fernando.silva@myrealbox.com]
Portions created by Fernando Silva are Copyright (C) 2002 Fernando Silva.
All Rights Reserved.

Contributor(s):

Last Modified: 2004-01-06

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

unit AntJvGroupHeader;

interface
{$I Ant.inc}

uses
  Classes,
  Windows, Messages, Graphics, Controls,
  {$IFDEF DELPHI7_UP}
  AntJvTheme,
  {$ENDIF}
  ExtCtrls;

type
  TJvBevelStyle = (bsShape, bsLowered, bsRaised);
  TJvLayout = (lTop, lCenter, lBottom);

  TAntJvGroupHeaderOptions = class(TPersistent)
  private
    FBrush: TBrush;
    FHeight: Integer;
    FPen: TPen;
    FShape: TShapeType;
    FStyle: TJvBevelStyle;
    FOnChange: TNotifyEvent;
    procedure SetBrush(Value: TBrush);
    procedure SetHeight(Value: Integer);
    procedure SetPen(Value: TPen);
    procedure SetStyle(Value: TJvBevelStyle);
    procedure SetShape(Value: TShapeType);
    procedure DoChange;
  public
    constructor Create;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Brush: TBrush read FBrush write SetBrush;
    property Height: Integer read FHeight write SetHeight default 2;
    property Pen: TPen read FPen write SetPen;
    property Shape: TShapeType read FShape write SetShape default stRectangle;
    property Style: TJvBevelStyle read FStyle write SetStyle default bsLowered;
  end;

  TAntJvGroupHeader = class(TGraphicControl)
  private
    FAlignment: TAlignment;
    FLayout: TJvLayout;
    FLabelOptions: TAntJvGroupHeaderOptions;
    FBevelOptions: TAntJvGroupHeaderOptions;
    FBevelSpace: Integer;
    function GetTransparent: Boolean;
    procedure SetAlignment(Value: TAlignment);
    procedure SetTransparent(Value: Boolean);
    procedure SetLayout(Value: TJvLayout);
    procedure SetBevelOptions(Value: TAntJvGroupHeaderOptions);
    procedure SetBevelSpace(Value: Integer);
    procedure SetLabelOptions(Value: TAntJvGroupHeaderOptions);
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure StyleChanged(Sender: TObject);
    procedure BevelLine(C: TColor; X, Y, Width: Integer);
    procedure DoDrawText(var Rect: TRect; Flags: Longint);
    function GetLabelText: string;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Canvas;
  published
    property Align;
    property Alignment: TAlignment read FAlignment write SetAlignment default
      taLeftJustify;
    property Anchors;
    property BiDiMode;
    property DragCursor;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
    property Caption;
    property Color;
    property Constraints;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property BevelOptions: TAntJvGroupHeaderOptions read FBevelOptions write SetBevelOptions;
    property BevelSpace: Integer read FBevelSpace write SetBevelSpace default 12;
    property LabelOptions: TAntJvGroupHeaderOptions read FLabelOptions write SetLabelOptions;
    property Transparent: Boolean read GetTransparent write SetTransparent default False;
    property Layout: TJvLayout read FLayout write SetLayout default lTop;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

implementation

//=== TAntJvGroupHeaderOptions ==================================================

constructor TAntJvGroupHeaderOptions.Create;
begin
  inherited Create;
  FPen := TPen.Create;
  FPen.OnChange := FOnChange;

  FBrush := TBrush.Create;
  FBrush.OnChange := FOnChange;

  FShape := stRectangle;
  FStyle := bsLowered;
  FHeight := 2;
end;

procedure TAntJvGroupHeaderOptions.SetBrush(Value: TBrush);
begin
  FBrush.Assign(Value);
end;

procedure TAntJvGroupHeaderOptions.SetHeight(Value: Integer);
begin
  if Value <> FHeight then
  begin
    FHeight := Value;
    DoChange;
  end;
end;

procedure TAntJvGroupHeaderOptions.SetPen(Value: TPen);
begin
  FPen.Assign(Value);
end;

procedure TAntJvGroupHeaderOptions.SetStyle(Value: TJvBevelStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    DoChange;
  end;
end;

procedure TAntJvGroupHeaderOptions.SetShape(Value: TShapeType);
begin
  if Value <> FShape then
  begin
    FShape := Value;
    DoChange;
  end;
end;

procedure TAntJvGroupHeaderOptions.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//=== TAntJvGroupHeader =========================================================

constructor TAntJvGroupHeader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csReplicatable];
  {$IFDEF DELPHI7_UP}
  IncludeThemeStyle(Self, [csParentBackground]);
  {$ENDIF}
  Font.Name := 'Tahoma';
  Width := 200;
  Height := 17;
  FBevelOptions := TAntJvGroupHeaderOptions.Create;
  FBevelOptions.OnChange := StyleChanged;
  FBevelSpace := 12;
  FLabelOptions := TAntJvGroupHeaderOptions.Create;
  FLabelOptions.OnChange := StyleChanged;
end;

function TAntJvGroupHeader.GetLabelText: string;
begin
  Result := Caption;
end;

procedure TAntJvGroupHeader.DoDrawText(var Rect: TRect; Flags: Longint);
var
  Text: string;
begin
  Text := GetLabelText;
  Flags := Flags or DT_NOPREFIX;
  Flags := DrawTextBiDiModeFlags(Flags);
  Canvas.Font := Font;
  if not Enabled then
  begin
    OffsetRect(Rect, 1, 1);
    Canvas.Font.Color := clBtnHighlight;
    DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
    OffsetRect(Rect, -1, -1);
    Canvas.Font.Color := clBtnShadow;
    DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
  end
  else
    DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
end;

procedure TAntJvGroupHeader.Paint;
const
  Alignments: array [TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array [Boolean] of Word = (0, DT_WORDBREAK);
var
  // Text
  Rect, CalcRect: TRect;
  DrawStyle: Longint;
  // Standard Bevel
  Color1, Color2: TColor;
  lbHeight, lbWidth: Integer;
  LX1, LX2, LX3, LX4, LY: Integer;
  // Shape Bevel
  X, Y, W, H, S: Integer;
begin
  // D R A W  T E X T
  // ----------------
  Color1 := clBlack;
  Color2 := clBlack; // (p3) just to remove warnings...
  with Canvas do
  begin
    if not Transparent then
    begin
      Brush.Color := Self.Color;
      Brush.Style := bsSolid;
      {$IFDEF DELPHI7_UP}
      DrawThemedBackground(Self, Canvas, ClientRect);
      {$ENDIF}
    end;
    Brush.Style := bsClear;
    Rect := ClientRect;
    // DoDrawText takes care of BiDi alignments
    DrawStyle := DT_EXPANDTABS or WordWraps[False] or Alignments[FAlignment];
    // Calculate vertical layout
    if FLayout <> lTop then
    begin
      CalcRect := Rect;
      DoDrawText(CalcRect, DrawStyle or DT_CALCRECT);
      if FLayout = lBottom then
        OffsetRect(Rect, 0, Height - CalcRect.Bottom)
      else
        OffsetRect(Rect, 0, (Height - CalcRect.Bottom) div 2);
    end;
    DoDrawText(Rect, DrawStyle);
  end;

  // C A L C U L A T E  P O S I T I O N S
  // --------------------------------------
  lbHeight := Canvas.TextHeight(GetLabelText);
  lbWidth := Canvas.TextWidth(GetLabelText);

  LX1 := 0;
  LX2 := 0;
  LX3 := 0;
  LX4 := 0;
  case FAlignment of
    taLeftJustify:
      begin
        LX1 := lbWidth + FBevelSpace;
        LX2 := Width - lbWidth - FBevelSpace;
      end;
    taCenter:
      begin
        LX1 := 0;
        LX2 := (Width div 2) - (lbWidth div 2);
        LX3 := (Width div 2) + (lbWidth div 2);
        LX4 := Width;
      end;
    taRightJustify:
      begin
        LX1 := 0;
        LX2 := Width - lbWidth - FBevelSpace;
      end;
  end;

  LY := 0;
  case FLayout of
    lTop:
      LY := lbHeight div 2;
    lCenter:
      LY := Height div 2;
    lBottom:
      LY := Height - (lbHeight div 2);
  end;

  // D R A W  B E V E L
  // ------------------
  if BevelOptions.Style <> bsShape then
  begin
    with Canvas do
    begin
      // Assign colors
      case BevelOptions.Style of
        bsLowered:
          begin
            Color1 := clBtnShadow;
            Color2 := clBtnHighlight;
          end;
        bsRaised:
          begin
            Color1 := clBtnHighlight;
            Color2 := clBtnShadow;
          end;
      end;

      if csDesigning in ComponentState then
      begin
        Pen.Style := psSolid;
        Pen.Mode := pmCopy;
        Pen.Color := clBlack;
        Brush.Style := bsSolid;
      end;

      Pen.Width := 1;

      // Locate and draw the line

      BevelLine(Color1, LX1, LY, LX2);
      BevelLine(Color2, LX1, LY + 1, LX2);
      if FAlignment = taCenter then // Draw right bevel
      begin
        BevelLine(Color1, LX3, LY, LX4);
        BevelLine(Color2, LX3, LY + 1, LX4);
      end;
    end;
  end
  else
    with Canvas do
    begin
      Pen := BevelOptions.Pen;
      Brush := BevelOptions.Brush;
      X := LX1 + (Pen.Width div 2);
      Y := LY - (BevelOptions.Height div 2) + (Pen.Width div 2);
      W := LX2 - Pen.Width + 1;
      H := BevelOptions.Height - Pen.Width + 1;
      if Pen.Width = 0 then
      begin
        Dec(W);
        Dec(H);
      end;
      if W < H then
        S := W
      else
        S := H;
      if BevelOptions.Shape in [stSquare, stRoundSquare, stCircle] then
      begin
        Inc(X, (W - S) div 2);
        Inc(Y, (H - S) div 2);
        W := S;
        H := S;
      end;
      case BevelOptions.Shape of
        stRectangle, stSquare:
          Rectangle(X, Y, X + W, Y + H);
        stRoundRect, stRoundSquare:
          RoundRect(X, Y, X + W, Y + H, S div 4, S div 4);
        stCircle, stEllipse:
          Ellipse(X, Y, X + W, Y + H);
      end;
    end;
end;

procedure TAntJvGroupHeader.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

function TAntJvGroupHeader.GetTransparent: Boolean;
begin
  Result := not (csOpaque in ControlStyle);
end;

procedure TAntJvGroupHeader.SetTransparent(Value: Boolean);
begin
  if Transparent <> Value then
  begin
    if Value then
      ControlStyle := ControlStyle - [csOpaque]
    else
      ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end;
end;

procedure TAntJvGroupHeader.SetLayout(Value: TJvLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TAntJvGroupHeader.CMTextChanged(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TAntJvGroupHeader.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TAntJvGroupHeader.SetBevelSpace(Value: Integer);
begin
  if Value <> FBevelSpace then
  begin
    FBevelSpace := Value;
    Invalidate;
  end;
end;

procedure TAntJvGroupHeader.StyleChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TAntJvGroupHeader.BevelLine(C: TColor; X, Y, Width: Integer);
begin
  with Canvas do
  begin
    Pen.Color := C;
    MoveTo(X, Y);
    LineTo(X + Width, Y);
  end;
end;

procedure TAntJvGroupHeader.SetBevelOptions(Value: TAntJvGroupHeaderOptions);
begin
  //
end;

procedure TAntJvGroupHeader.SetLabelOptions(Value: TAntJvGroupHeaderOptions);
begin
  //
end;

end.

