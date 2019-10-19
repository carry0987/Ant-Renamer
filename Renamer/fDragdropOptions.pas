(************************************************************************
 *                                                                      *
 *   Ant Renamer 2.x                                                    *
 *   (C) 2005-2006 Antoine Potten                                       *
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

unit fDragdropOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,

  AntAutoHintLabel, AntCorelButton,

  base, frameAddFolders;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

type
  TDragdropOptionsForm = class(TBaseDlg)
    lblOptions: TLabel;
    Options: TAddFoldersFrame;
    cbxNotAgain: TCheckBox;
    procedure btn3Click(Sender: TObject);
    procedure btn1Click(Sender: TObject);
  private
    FShiftCtrl: Boolean;
  public
    function Execute: TModalResult;
  end;

var
  DragdropOptionsForm: TDragdropOptionsForm;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

implementation

{$R *.dfm}

uses
  Global, ProgramSettings;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TDragdropOptionsForm.btn3Click(Sender: TObject);
var
  CheckOptions: TAddFoldersOptions;
begin
  if not FShiftCtrl then
  begin
    CheckOptions := Self.Options.Options;
    with Settings.Root.Options.Display.DragDropOptions do
    begin
      AddFiles := afoFiles in CheckOptions;
      AddFolders := afoFolders in CheckOptions;
      Recursive := afoRecursive in CheckOptions;
    end;
    if cbxNotAgain.Checked then
      Settings.Root.Options.Display.DragDropNoAsk := True;
  end;
  ModalResult := mrOk;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TDragdropOptionsForm.Execute: TModalResult;
begin
  with Settings.Root.Options.Display do
  begin
    with DragDropOptions do
      Options.SetOptionsBool(AddFiles, AddFolders, Recursive);
    FShiftCtrl := DragDropNoAsk and ((GetKeyState(VK_SHIFT) and $8000 = $8000) or (GetKeyState(VK_CONTROL) and $8000 = $8000));
    if DragDropNoAsk and not FShiftCtrl then
      Result := mrOk
    else
    begin
      cbxNotAgain.Enabled := not FShiftCtrl;
      Result := ShowModal;
    end;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TDragdropOptionsForm.btn1Click(Sender: TObject);
begin
  LaunchHelp;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

end.
