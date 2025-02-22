(************************************************************************
 *                                                                      *
 *   Ant Renamer 2.x                                                    *
 *   (C) 2003-2024 Antoine Potten                                       *
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

unit ConstValues;

interface

uses
  Dialogs, SysUtils,
  TntSystem, TntSysUtils;

const
  strDate     = '2024-12-14';
  strCaption  = 'Ant Renamer %s';

var
  strDirApp: TWideFileName;
  strDirStart: TWideFileName;
  strDirData: TWideFileName;
  strVersion: string;
  strPrefsFile: TWideFileName;
  isPortableVersion: Boolean;

const
  strDirToolbars = 'Toolbars\';
  strDirLanguages = 'Languages\';

  tbiTabFiles          =  0;
  tbiTabActions        =  1;
  tbiTabLog            =  2;
  tbiAddFiles          =  3;
  tbiAddFolder         =  4;
  tbiRemoveSelected    =  5;
  tbiRemoveAll         =  6;
  tbiRemoveDead        =  7;
  tbiPreview           =  8;
  tbiMoveUp            =  9;
  tbiMoveDown          = 10;
  tbiMoveTop           = 11;
  tbiMoveBottom        = 12;
  tbiBatchAddAction    = 13;
  tbiBatchRemoveSel    = 14;
  tbiBatchClear        = 15;
  tbiBatchOpen         = 16;
  tbiBatchSave         = 17;
  tbiLogClear          = 18;
  tbiLogSave           = 19;
  tbiLogCopy           = 20;
  tbiLogFilter         = 21;
  tbiGo                = 22;
  tbiStop              = 23;
  tbiUndo              = 24;
  tbiAlwaysOnTop       = 25;
  tbiOptions           = 26;
  tbiHelp              = 27;
  tbiAbout             = 28;
  tbiExit              = 29;

  DialogOpenOptions = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing];
  DialogSaveOptions = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofNoReadOnlyReturn, ofEnableSizing];

implementation

uses
  SHFolder,
  functions_files;

initialization
  strDirStart := WideGetCurrentDir;
  strDirApp := WideExtractFilePath(WideParamStr(0));
  strPrefsFile := WideChangeFileExt(WideExtractFileName(WideParamStr(0)), '.xml');
  isPortableVersion := WideFileExists(strDirApp + strPrefsFile);
  strDirData := WideIncludeTrailingPathDelimiter(GetShellPath(CSIDL_COMMON_APPDATA)) + 'Ant Renamer\';
  if (isPortableVersion) or (Win32MajorVersion < 5) or (not WideDirectoryExists(strDirData)) then
  begin     // Portable version or Installed version without data folder
    strDirData := strDirApp;
  end;
  strVersion := GetBuild3(WideParamStr(0));
end.
