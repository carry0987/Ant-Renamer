(************************************************************************
 *                                                                      *
 *   (C) 2002-2006 Antoine Potten                                       *
 *   http://www.antp.be/software                                        *
 *                                                                      *
 ************************************************************************
 *                                                                      *
 *   This program is free software; you can redistribute it and/or      *
 *   modify it under the terms of the GNU General Public License        *
 *   as published by the Free Software Foundation; either version 2     *
 *   of the License, or (at your option) any later version.             *
 *                                                                      *
 *   This program is distributed in the hope that it will be useful,    *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of     *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *
 *   GNU General Public License for more details.                       *
 *                                                                      *
 ************************************************************************)

unit functions_gui;

interface

uses
  Forms, Windows, Graphics, ComCtrls, Controls, StdCtrls, Messages;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure MoveItems(aListView: TListView; up: Boolean);
procedure Grayscale(APicture: TBitmap);
procedure Blend(APicture: TBitmap; BaseColor: TColor = clWhite);
procedure EnableControl(const Control: TWinControl; const Enabled: Boolean);
procedure SetWaitCursor;
procedure SetAppWaitCursor;
procedure RestoreCursor;
procedure FitDropDownToContents(Combo: TCustomComboBox);

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

implementation

uses
  SysUtils, Contnrs, Math,

  functions_str;

var
  CursorsStack: TStack;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure MoveItems(aListView: TListView; up: Boolean);
var
  oldItem, newItem: TListItem;
  aIndex: Integer;
begin
  with aListView do
  begin
    Items.BeginUpdate;
    try
      oldItem := TListItem.Create(Items);
      try
        oldItem.Assign(Selected);
        if up then
          aIndex := Selected.Index - 1
        else
          aIndex := Selected.Index + 1;
        if not aIndex in [0..Items.Count-1] then Exit;
        Selected.Delete;
        newItem := Items.Insert(aIndex);
        newItem.Assign(oldItem);
        Selected := newItem;
        Selected.Focused := true;
        Selected.MakeVisible(false);
      finally
        oldItem.Free;
      end;
    finally
      Items.EndUpdate;
    end;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

type
  pRGBArray  =  ^TRGBArray;
  TRGBArray  =  array [0..32767] of TRGBTriple;
procedure Grayscale(APicture: TBitmap);
var
  i, j: Integer;
  LinePtr: PRGBArray;
  AColor: Byte;
begin
  with APicture do
  begin
    PixelFormat := pf24bit;
    for i := 0 to Height-1 do
    begin
      LinePtr := ScanLine[i];
      for j := 0 to Width-1 do
        with LinePtr[j] do
        begin
          AColor := (rgbtBlue + rgbtGreen + rgbtRed) div 3;
          rgbtBlue := AColor;
          rgbtGreen := AColor;
          rgbtRed := AColor;
        end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Procedure "Blend" comes from TB2Item.pas, when patched with TBSkin+
-------------------------------------------------------------------------------}

procedure Blend(APicture: TBitmap; BaseColor: TColor = clWhite);
var
  i, j: Integer;
  Ptr: ^Cardinal;
  S, C, CBRB, CBG: Cardinal;
  Wt1, Wt2: Cardinal;
begin
  Wt2 := 180; // opacity
  Wt1 := 255 - Wt2;
  with APicture do
  begin
    PixelFormat := pf32bit;
    for i := 0 to Height-1 do
    begin
      Ptr := ScanLine[i];
      for j := 0 to Width-1 do
      begin
        S := Ptr^;
        {$WARNINGS OFF}
        CBRB := (BaseColor and $00FF00FF) * Wt1;
        CBG  := (BaseColor and $0000FF00) * Wt1;
        C := ((Integer(S and $FF00FF)) * Wt2 + CBRB) and $FF00FF00 + ((S and $00FF00) * Wt2 + CBG) and $00FF0000;
        {$WARNINGS ON}
        Ptr^ := C shr 8;
        Inc(Ptr);
      end;
    end;
    PixelFormat := pf24bit;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure EnableControl(const Control: TWinControl; const Enabled: Boolean);
begin
  if Control is TEdit then
  begin
    TEdit(Control).ReadOnly := not Enabled;
    if Enabled then
      TEdit(Control).Color := clWindow
    else
      TEdit(Control).ParentColor := True;
  end
  else
  if Control is TComboBox then
  begin
    TComboBox(Control).Enabled := Enabled;
    if Enabled then
      TComboBox(Control).Color := clWindow
    else
      TComboBox(Control).ParentColor := True;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure SetWaitCursor;
begin
  CursorsStack.Push(Pointer(Screen.Cursor));
  Screen.Cursor := crHourGlass;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure SetAppWaitCursor;
begin
  CursorsStack.Push(Pointer(Screen.Cursor));
  Screen.Cursor := crAppStart;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure RestoreCursor;
var
  c: TCursor;
begin
  if CursorsStack.Count > 0 then
    c := TCursor(CursorsStack.Pop)
  else
    c := crDefault;
  Screen.Cursor := c;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure FitDropDownToContents(Combo: TCustomComboBox);
var
  i, MaxWidth: Integer;
begin
  MaxWidth := Combo.Width;
  for i := 0 to Combo.Items.Count-1 do
    MaxWidth := Max(MaxWidth, Trunc(Combo.Canvas.TextWidth(Combo.Items[i]) * 1.1));
  SendMessage(Combo.Handle, CB_SETDROPPEDWIDTH, MaxWidth, 0);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

initialization
  CursorsStack := TStack.Create;
finalization
  FreeAndNil(CursorsStack);
end.
