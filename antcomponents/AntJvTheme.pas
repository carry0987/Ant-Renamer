{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

Contents of this file comes from JvThemes.PAS, released on 2003-09-25

The Initial Developers of the Original Code are: Andreas Hausladen <Andreas.Hausladen@gmx.de>
All Rights Reserved.

Last Modified: 2002-10-09

Contributers:

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

unit AntJvTheme;

interface
{$I Ant.inc}
uses
  Windows, Messages,
  Contnrs,
  Themes,
  Controls, StdCtrls, Graphics, Buttons,
  SysUtils, Classes;

type
  TThemeStyle = TControlStyle;
  TWinControlThemeInfo = class(TWinControl)
  public
    property Color;
  end;

  { Add IJvDenySubClassing to the base class list if the control should not
    be themed by the ThemeManager (www.delphi-gems.de).
    This only works with JvExVCL derived classes. }
  IJvDenySubClassing = interface
    ['{76942BC0-2A6E-4DC4-BFC9-8E110DB7F601}']
  end;

const
 // Add a message handler to a component that is themed by the ThemeManager but
 // should not be themed.
  CM_DENYSUBCLASSING = CM_BASE + 2000; // from ThemeMgr.pas

procedure IncludeThemeStyle(Control: TControl; Style: TThemeStyle);
procedure ExcludeThemeStyle(Control: TControl; Style: TThemeStyle);
function GetThemeStyle(Control: TControl): TThemeStyle;

{ DrawThemedBackground fills R with Canvas.Brush.Color/Color. If the control uses
  csParentBackground and the color is that of it's parent the Rect is not filled
  because then it is done by the JvThemes/VCL7. }
procedure DrawThemedBackground(Control: TControl; Canvas: TCanvas;
  const R: TRect; NeedsParentBackground: Boolean = True); overload;
procedure DrawThemedBackground(Control: TControl; Canvas: TCanvas;
  const R: TRect; Color: TColor; NeedsParentBackground: Boolean = True); overload;
procedure DrawThemedBackground(Control: TControl; DC: HDC; const R: TRect;
  Brush: HBRUSH; NeedsParentBackground: Boolean = True); overload;

{ PerformEraseBackground sends a WM_ERASEBKGND message to the Control's parent. }
procedure PerformEraseBackground(Control: TControl; DC: HDC; Offset: TPoint;
  R: PRect = nil); overload;
procedure PerformEraseBackground(Control: TControl; DC: HDC; R: PRect = nil); overload;

implementation

procedure IncludeThemeStyle(Control: TControl; Style: TThemeStyle);
begin
  with Control do
    ControlStyle := ControlStyle + (Style * [csNeedsBorderPaint, csParentBackground]);
end;

procedure ExcludeThemeStyle(Control: TControl; Style: TThemeStyle);
begin
  with Control do
    ControlStyle := ControlStyle - (Style * [csNeedsBorderPaint, csParentBackground]);
end;

function GetThemeStyle(Control: TControl): TThemeStyle;
begin
  with Control do
    Result := ControlStyle * [csNeedsBorderPaint, csParentBackground];
end;

procedure DrawThemedBackground(Control: TControl; Canvas: TCanvas;
  const R: TRect; NeedsParentBackground: Boolean = True);
begin
  DrawThemedBackground(Control, Canvas, R, Canvas.Brush.Color,
    NeedsParentBackground);
end;

procedure DrawThemedBackground(Control: TControl; Canvas: TCanvas;
  const R: TRect; Color: TColor; NeedsParentBackground: Boolean = True);
var
  cl: TColor;
begin
  if (not (csDesigning in Control.ComponentState)) and
    (Control.Parent <> nil) and
    ((Color = TWinControlThemeInfo(Control.Parent).Color) or
    (ColorToRGB(Color) = ColorToRGB(TWinControlThemeInfo(Control.Parent).Color))) and
    (ThemeServices.ThemesEnabled) and
    ((not NeedsParentBackground) or
    (csParentBackground in GetThemeStyle(Control))) then
  begin
    if Control is TWinControl then
    begin
      if TWinControl(Control).DoubleBuffered then
        PerformEraseBackground(Control, Canvas.Handle, @R)
      else
        ThemeServices.DrawParentBackground(TWinControl(Control).Handle, Canvas.Handle, nil, False, @R);
    end
    else
      PerformEraseBackground(Control, Canvas.Handle, @R)
  end
  else
  begin
    cl := Canvas.Brush.Color;
    if cl <> Color then
      Canvas.Brush.Color := Color;
    Canvas.FillRect(R);
    if cl <> Canvas.Brush.Color then
      Canvas.Brush.Color := cl;
  end;
end;

procedure DrawThemedBackground(Control: TControl; DC: HDC; const R: TRect;
  Brush: HBRUSH; NeedsParentBackground: Boolean = True);
var
  LogBrush: TLogBrush;
begin
  GetObject(Brush, SizeOf(LogBrush), @LogBrush);
  if (not (csDesigning in Control.ComponentState)) and
    (Control.Parent <> nil) and
    (LogBrush.lbColor = Cardinal(ColorToRGB(TWinControlThemeInfo(Control.Parent).Color))) and
    (ThemeServices.ThemesEnabled) and
    ((not NeedsParentBackground) or
    (csParentBackground in GetThemeStyle(Control))) then
  begin
    if Control is TWinControl then
    begin
      if TWinControl(Control).DoubleBuffered then
        PerformEraseBackground(Control, DC, @R)
      else
        ThemeServices.DrawParentBackground(TWinControl(Control).Handle, DC, nil, False, @R);
    end
    else
      PerformEraseBackground(Control, DC, @R)
  end
  else
    FillRect(DC, R, Brush);
end;

procedure PerformEraseBackground(Control: TControl; DC: HDC; Offset: TPoint; R: PRect = nil);
var
  WindowOrg: TPoint;
  OrgRgn, Rgn: THandle;
begin
  if Control.Parent <> nil then
  begin
    if (Offset.X <> 0) and (Offset.Y <> 0) then
    begin
      GetWindowOrgEx(DC, WindowOrg);
      SetWindowOrgEx(DC, WindowOrg.X + Offset.X, WindowOrg.Y + Offset.Y, nil);
    end;

    OrgRgn := 0;
    if R <> nil then
    begin
      OrgRgn := CreateRectRgn(0, 0, 1, 1);
      if GetClipRgn(DC, OrgRgn) = 0 then
      begin
        DeleteObject(OrgRgn);
        OrgRgn := 0;
      end;
      Rgn := CreateRectRgnIndirect(R^);
      SelectClipRgn(DC, Rgn);
      DeleteObject(Rgn);
    end;

    try
      Control.Parent.Perform(WM_ERASEBKGND, DC, DC); // force redraw
    finally
      if (Offset.X <> 0) and (Offset.Y <> 0) then
        SetWindowOrgEx(DC, WindowOrg.X, WindowOrg.Y, nil);

      if OrgRgn <> 0 then
      begin
        SelectClipRgn(DC, OrgRgn);
        DeleteObject(OrgRgn);
      end;
    end;
  end;
end;

procedure PerformEraseBackground(Control: TControl; DC: HDC; R: PRect = nil);
begin
  PerformEraseBackground(Control, DC, Point(Control.Left, Control.Top), R);
end;

end.
