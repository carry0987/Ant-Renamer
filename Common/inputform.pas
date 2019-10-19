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

unit inputform;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, base, StdCtrls, ComCtrls, ExtCtrls,

  AntCorelButton, AntAutoHintLabel;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

type
  TInputWin = class(TBaseDlg)
    lblPrompt: TLabel;
    edtInput: TEdit;
    procedure FormShow(Sender: TObject);
  private
  public
    function Execute(const WindowCaption, Prompt: string; var InputText: string): Boolean;
  end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

implementation

{$R *.dfm}

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TInputWin.Execute(const WindowCaption, Prompt: string; var InputText: string): Boolean;
begin
  Caption := WindowCaption;
  lblPrompt.Caption := Prompt;
  edtInput.Text := InputText;
  Result := ShowModal = mrOk;
  if Result then
    InputText := edtInput.Text;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TInputWin.FormShow(Sender: TObject);
begin
  edtInput.SetFocus;
  edtInput.SelectAll;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

end.
