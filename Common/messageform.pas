(************************************************************************
 *                                                                      *
 *   (C) 2002-2017 Antoine Potten, Mickaël Vanneufville                 *
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

unit messageform;

{
  This form can be called with its Execute method.
  The return value is the index of the clicked button, begining at 1.
  It returns 0 when the window is closed with the "X" icon from the corner.
  The buttons can be constants mbOk, mbCancel, etc. or simple strings for
  custom buttons captions.
  It uses unicode (WideStrings) if ANTUNICODE is defined.
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  {$IFDEF ANTUNICODE}
  TntStdCtrls,
  {$ENDIF}

  AntCorelButton, AntStringList;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

type
  TMessageWin = class(TForm)
    btn1: TCorelButton;
    btn2: TCorelButton;
    btn3: TCorelButton;
    btn4: TCorelButton;
    Image1: TImage;
    CheckBox1: TCheckBox;
    Captions: TAntStringList;
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    btns: array [0..3] of TCorelButton;
    FRect: TRect;
    {$IFDEF ANTUNICODE}
    FText: WideString;
    LMessage: TTntLabel;
    function ExecuteDlg(const AText, ACaption: WideString; Buttons: array of Variant; DefaultButton: Integer = 1): Integer;
    function GetFormText: WideString;
    {$ELSE}
    FText: string;
    LMessage: TLabel;
    function ExecuteDlg(const AText, ACaption: string; Buttons: array of Variant; DefaultButton: Integer = 1): Integer;
    function GetFormText: string;
    {$ENDIF}
    procedure CalcSize(NumberOfButtons: Integer);
  public
    {$IFDEF ANTUNICODE}
    function Execute(const AText: WideString; CaptionIdx: TMsgDlgType; var CheckBoxValue: Boolean; const CheckBoxText: string; Buttons: array of Variant; DefaultButton: Integer = 1): Integer; overload;
    function Execute(const AText: WideString; CaptionIdx: TMsgDlgType; Buttons: array of Variant; DefaultButton: Integer = 1): Integer; overload;
    function Execute(const AText, ACaption: WideString; var CheckBoxValue: Boolean; const CheckBoxText: string; Buttons: array of Variant; DefaultButton: Integer = 1): Integer; overload;
    function Execute(const AText, ACaption: WideString; Buttons: array of Variant; DefaultButton: Integer = 1): Integer; overload;
    {$ELSE}
    function Execute(const AText: string; CaptionIdx: TMsgDlgType; var CheckBoxValue: Boolean; const CheckBoxText: string; Buttons: array of Variant; DefaultButton: Integer = 1): Integer; overload;
    function Execute(const AText: string; CaptionIdx: TMsgDlgType; Buttons: array of Variant; DefaultButton: Integer = 1): Integer; overload;
    function Execute(const AText, ACaption: string; var CheckBoxValue: Boolean; const CheckBoxText: string; Buttons: array of Variant; DefaultButton: Integer = 1): Integer; overload;
    function Execute(const AText, ACaption: string; Buttons: array of Variant; DefaultButton: Integer = 1): Integer; overload;
    {$ENDIF}
    procedure Translate;
  end;

const
  FirstBtn = 4;


{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

implementation

{$R *.dfm}

uses
  Clipbrd, Types, Math,
  {$IFDEF ANTTRANSLATOR}
  Global,
  {$ENDIF}
  {$IFDEF ANTUNICODE}
  TntWindows, TntClipbrd, functions_gui,
  {$ENDIF}

  StrUtils;

var
  IconIDs: array[TMsgDlgType] of PChar = (IDI_EXCLAMATION, IDI_HAND, IDI_ASTERISK, IDI_QUESTION, nil);

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMessageWin.FormCreate(Sender: TObject);
begin
  Font.Name := Graphics.DefFontData.Name;
  Font.Charset := Graphics.DefFontData.Charset;
  {$IFDEF ANTUNICODE}
  LMessage := TTntLabel.Create(Self);
  {$ELSE}
  LMessage := TLabel.Create(Self);
  {$ENDIF}
  with LMessage do
  begin
    Left := 50;
    Top := 10;
    AutoSize := False;
    Parent := Self;
    WordWrap := True;
  end;
  btn1.ModalResult := -1;
  btn2.ModalResult := -2;
  btn3.ModalResult := -3;
  btn4.ModalResult := -4;
  btns[0] := btn1;
  btns[1] := btn2;
  btns[2] := btn3;
  btns[3] := btn4;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMessageWin.FormShow(Sender: TObject);
begin
  if btn1.Default then
    btn1.SetFocus
  else if btn2.Default then
    btn2.SetFocus
  else if btn3.Default then
    btn3.SetFocus
  else if btn4.Default then
    btn4.SetFocus
  else
    btn1.SetFocus;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMessageWin.CalcSize(NumberOfButtons: Integer);
begin
  SetRect(FRect, 0, 0, 400, 0);
  {$IFDEF ANTUNICODE}
  Tnt_DrawTextW(Canvas.Handle, PWideChar(FText), Length(FText)+1, FRect, DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK);
  {$ELSE}
  DrawText(Canvas.Handle, PChar(FText), Length(FText)+1, FRect, DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK);
  {$ENDIF}
  ClientHeight := Max(FRect.Bottom, Image1.Height) + 35 + btn2.Height;
  if CheckBox1.Visible then
    ClientHeight := ClientHeight + CheckBox1.Height + 5;
  ClientWidth := Max(FRect.Right, 300) + 30 + Image1.Width;
  btn1.Visible := NumberOfButtons > 0;
  btn2.Visible := NumberOfButtons > 1;
  btn3.Visible := NumberOfButtons > 2;
  btn4.Visible := NumberOfButtons > 3;
  case NumberOfButtons of
    1:  begin
          btn1.Left := (ClientWidth div 2) - (btn1.Width div 2);
        end;
    2:  begin
          btn1.Left := (ClientWidth div 2) - btn1.Width - 5;
          btn2.Left := (ClientWidth div 2) + 5;
        end;
    3:  begin
          btn2.Left := (ClientWidth div 2) - (btn2.Width div 2);
          btn1.Left := btn2.Left - btn1.Width - 10;
          btn3.Left := btn2.Left + btn2.Width + 10;
        end;
    4:  begin
          btn1.Left := (ClientWidth div 2) - (2 * btn1.Width) - 15;
          btn2.Left := (ClientWidth div 2) - btn1.Width - 5;
          btn3.Left := (ClientWidth div 2) + 5;
          btn4.Left := (ClientWidth div 2) + btn1.Width + 15;
        end;
  end;
  btn1.Top := ClientHeight - btn1.Height - 10;
  if CheckBox1.Visible then
    btn1.Top := btn1.Top - CheckBox1.Height;
  btn2.Top := btn1.Top;
  btn3.Top := btn1.Top;
  btn4.Top := btn1.Top;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMessageWin.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key in [ord('C'), VK_INSERT]) and (Shift = [ssCtrl]) then
{$IFDEF ANTUNICODE}
    TntClipboard.AsWideText := GetFormText;
{$ELSE}
    Clipboard.AsText := GetFormText;
{$ENDIF}
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMessageWin.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Word(Key) = VK_ESCAPE) then
    ModalResult := mrCancel;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

{$IFDEF ANTUNICODE}
function TMessageWin.Execute(const AText: WideString; CaptionIdx: TMsgDlgType; var CheckBoxValue: Boolean; const CheckBoxText: string; Buttons: array of Variant; DefaultButton: Integer): Integer;
{$ELSE}
function TMessageWin.Execute(const AText: string; CaptionIdx: TMsgDlgType; var CheckBoxValue: Boolean; const CheckBoxText: string; Buttons: array of Variant; DefaultButton: Integer): Integer;
{$ENDIF}
begin
  CheckBox1.Visible := True;
  CheckBox1.Caption := CheckBoxText;
  CheckBox1.Checked := CheckBoxValue;
  Image1.Visible := True;
  Image1.Picture.Icon.Handle := LoadIcon(0, IconIDs[CaptionIdx]);
  Result := ExecuteDlg(AText, Captions.Strings[Ord(CaptionIdx)], Buttons, DefaultButton);
  CheckBoxValue := CheckBox1.Checked;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

{$IFDEF ANTUNICODE}
function TMessageWin.Execute(const AText: WideString; CaptionIdx: TMsgDlgType; Buttons: array of Variant; DefaultButton: Integer): Integer;
{$ELSE}
function TMessageWin.Execute(const AText: string; CaptionIdx: TMsgDlgType; Buttons: array of Variant; DefaultButton: Integer): Integer;
{$ENDIF}
begin
  CheckBox1.Visible := False;
  Image1.Visible := True;
  Image1.Picture.Icon.Handle := LoadIcon(0, IconIDs[CaptionIdx]);
  Result := ExecuteDlg(AText, Captions.Strings[Ord(CaptionIdx)], Buttons, DefaultButton);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}


{$IFDEF ANTUNICODE}
function TMessageWin.Execute(const AText, ACaption: WideString; var CheckBoxValue: Boolean; const CheckBoxText: string; Buttons: array of Variant; DefaultButton: Integer): Integer;
{$ELSE}
function TMessageWin.Execute(const AText, ACaption: string; var CheckBoxValue: Boolean; const CheckBoxText: string; Buttons: array of Variant; DefaultButton: Integer): Integer;
{$ENDIF}
begin
  CheckBox1.Visible := True;
  CheckBox1.Caption := CheckBoxText;
  CheckBox1.Checked := CheckBoxValue;
  Image1.Visible := False;
  Result := ExecuteDlg(AText, ACaption, Buttons, DefaultButton);
  CheckBoxValue := CheckBox1.Checked;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

{$IFDEF ANTUNICODE}
function TMessageWin.Execute(const AText, ACaption: WideString; Buttons: array of Variant; DefaultButton: Integer): Integer;
{$ELSE}
function TMessageWin.Execute(const AText, ACaption: string; Buttons: array of Variant; DefaultButton: Integer): Integer;
{$ENDIF}
begin
  CheckBox1.Visible := False;
  Image1.Visible := False;
  Result := ExecuteDlg(AText, ACaption, Buttons, DefaultButton);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

{$IFDEF ANTUNICODE}
function TMessageWin.ExecuteDlg(const AText, ACaption: WideString; Buttons: array of Variant; DefaultButton: Integer): Integer;
{$ELSE}
function TMessageWin.ExecuteDlg(const AText, ACaption: string; Buttons: array of Variant; DefaultButton: Integer): Integer;
{$ENDIF}
var
  i: Integer;
begin
  for i := 0 to Min(Length(Buttons), 4) - 1 do
    if VarType(Buttons[i]) = varString then
    begin
      btns[i].Caption := Buttons[i];
    end else
    begin
      btns[i].Caption := Captions.Strings[Buttons[i] + FirstBtn];
    end;
  for i := 0 to 3 do
    btns[i].Default := (i = DefaultButton - 1);
  FText := AText;
  Caption := ACaption;
  CalcSize(Min(Length(Buttons), 4));
  with LMessage do
  begin
    Caption := FText;
    Left := 20 + IfThen(Image1.Visible, Image1.Width);
    Width := FRect.Right;
    Top := 10;
    Height := FRect.Bottom;
  end;
  CheckBox1.Top := ClientHeight - CheckBox1.Height - 5;
  Position := poScreenCenter;
  Result := ShowModal;
  if Result = mrCancel then
    Result := 0
  else
    Result := - Result;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

{$IFDEF ANTUNICODE}
function TMessageWin.GetFormText: WideString;
{$ELSE}
function TMessageWin.GetFormText: string;
{$ENDIF}
var
  DividerLine, ButtonCaptions, CheckBoxCaption: string;
  i: integer;
begin
  DividerLine := StringOfChar('-', 27) + sLineBreak;
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TButton then
      with Components[i] as TButton do
        if Visible then
          ButtonCaptions := ButtonCaptions + Caption + '   ';
  if CheckBox1.Visible then
    CheckBoxCaption := IfThen(CheckBox1.Checked, '[x] ', '[ ] ') + CheckBox1.Caption
  else
    CheckBoxCaption := '';
  ButtonCaptions := StringReplace(ButtonCaptions,'&','', [rfReplaceAll]);
  Result := DividerLine + Caption + sLineBreak + DividerLine + LMessage.Caption + sLineBreak + DividerLine + ButtonCaptions + sLineBreak + DividerLine + CheckBoxCaption;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMessageWin.Translate;
begin
  {$IFDEF ANTTRANSLATOR}
  Translator.Translate(Self);
  Font.Name := Graphics.DefFontData.Name;
  Font.Charset := Graphics.DefFontData.Charset;
  {$ENDIF}
end;

end.
