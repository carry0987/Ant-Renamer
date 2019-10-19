(************************************************************************
 *                                                                      *
 *   (C) 2006 Antoine Potten                                            *
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

unit memoform;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, base, StdCtrls, AntCorelButton, AntAutoHintLabel, ExtCtrls;

type
  TMemoWin = class(TBaseDlg)
    edtMemo: TMemo;
    procedure FormShow(Sender: TObject);
  private
  public
    function Execute(const WindowCaption: string; var MemoText: string): Boolean;
  end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

implementation

{$R *.dfm}

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMemoWin.FormShow(Sender: TObject);
begin
  inherited;
  edtMemo.SetFocus;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TMemoWin.Execute(const WindowCaption: string; var MemoText: string): Boolean;
begin
  Caption := WindowCaption;
  edtMemo.Lines.Text := MemoText;
  Result := ShowModal = mrOk;
  if Result then
    MemoText := edtMemo.Lines.Text;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

end.
