(************************************************************************
 *                                                                      *
 *   Ant Renamer 2.x                                                    *
 *   (C) 2003-2015 Antoine Potten                                       *
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

program Renamer;

uses
  Forms,
  TntSysUtils,
  TntSystem,
  Graphics,
  Windows,
  AntJvTranslator,
  fMain in 'fMain.pas' {MainForm},
  fAbout in 'fAbout.pas' {AboutForm},
  ProgramSettings in 'ProgramSettings.pas',
  fOptions in 'fOptions.pas' {OptionsForm},
  Files in 'Files.pas',
  Actions in 'Actions.pas',
  ConstValues in 'ConstValues.pas',
  Log in 'Log.pas',
  thread in 'thread.pas',
  SettingsBase in '..\Common\SettingsBase.pas',
  base in '..\Common\base.pas' {BaseDlg},
  messageform in '..\Common\messageform.pas' {MessageWin},
  frameLanguage in '..\Common\frameLanguage.pas' {LanguageFrame: TFrame},
  functions_files in '..\Common\functions_files.pas',
  functions_str in '..\Common\functions_str.pas',
  functions_xml in '..\Common\functions_xml.pas',
  VarMessages in 'VarMessages.pas',
  functions_gui in '..\Common\functions_gui.pas',
  fSelectDirectory in 'fSelectDirectory.pas' {SelectDirectoryForm},
  frameAddFolders in 'frameAddFolders.pas' {AddFoldersFrame: TFrame},
  fDragdropOptions in 'fDragdropOptions.pas' {DragdropOptionsForm},
  functions_sys in '..\Common\functions_sys.pas',
  VirtualTreeUtils in '..\Common\VirtualTreeUtils.pas',
  functions_strformat in '..\Common\functions_strformat.pas',
  Global in 'Global.pas',
  ExtractID3 in '..\Common\ExtractID3.pas';

{$R *.res}
{$R toolbars.RES}

begin
  if IsWindowsNT then
    Graphics.DefFontData.Name := 'MS Shell Dlg 2'
  else
    Graphics.DefFontData.Name := 'MS Shell Dlg';
  Graphics.DefFontData.Charset := DEFAULT_CHARSET;
  Application.Initialize;
  Application.Title := 'Ant Renamer 2';
  Settings := TRenamerSettings.Create(strDirData + strPrefsFile);
  Translator := TAntJvTranslator.Create(nil);
  try
    Settings.LoadFromFile;
    Application.CreateForm(TMainForm, MainForm);
    Application.CreateForm(TMessageWin, MessageWin);
    Application.OnException := MainForm.OnException;
    Application.Run;
    Settings.SaveToFile;
  finally
    Translator.Free;
    Settings.Free;
  end;
end.
