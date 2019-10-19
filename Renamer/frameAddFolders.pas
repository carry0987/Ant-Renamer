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

unit frameAddFolders;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls,

  Global;

type
  TAddFoldersFrame = class(TFrame)
    cbxFiles: TCheckBox;
    cbxFolders: TCheckBox;
    cbxRecursive: TCheckBox;
    procedure ClickCheck(Sender: TObject);
  private
    function GetOptions: TAddFoldersOptions;
    procedure SetOptions(Values: TAddFoldersOptions);
  protected
    procedure SetEnabled(State: Boolean); override;
  public
    property Options: TAddFoldersOptions read GetOptions write SetOptions;
    procedure SetOptionsBool(const AddFiles, AddFolders, Recursive: Boolean);
  end;

implementation

{$R *.dfm}

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TAddFoldersFrame.GetOptions: TAddFoldersOptions;
begin
  Result := [];
  if cbxFiles.Checked then
    Include(Result, afoFiles);
  if cbxFolders.Checked then
    Include(Result, afoFolders);
  if cbxRecursive.Checked then
    Include(Result, afoRecursive);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAddFoldersFrame.SetOptions(Values: TAddFoldersOptions);
begin
  cbxFiles.Checked := afoFiles in Values;
  cbxFolders.Checked := afoFolders in Values;
  cbxRecursive.Checked := afoRecursive in Values;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAddFoldersFrame.ClickCheck(Sender: TObject);
begin
  if (Sender = cbxFolders) and (cbxFolders.Checked = False) then
    cbxFiles.Checked := True
  else
  if (Sender = cbxFiles) and (cbxFiles.Checked = False) then
    cbxFolders.Checked := True
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAddFoldersFrame.SetOptionsBool(const AddFiles, AddFolders, Recursive: Boolean);
begin
  cbxFiles.Checked := AddFiles;
  cbxFolders.Checked := AddFolders;
  cbxRecursive.Checked := Recursive;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAddFoldersFrame.SetEnabled(State: Boolean);
begin
  inherited;
  cbxFiles.Enabled := State;
  cbxFolders.Enabled := State;
  cbxRecursive.Enabled := State;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

end.
