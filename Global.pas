(************************************************************************
 *                                                                      *
 *   Ant Renamer 2.x                                                    *
 *   (C) 2003-2006 Antoine Potten                                       *
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

unit Global;

interface

uses
  Controls, SysUtils,

  AntJvTranslator, tb2item,

  MessageForm, InputForm,
  ProgramSettings;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

type
  TAddFoldersOption = (afoFiles, afoFolders, afoRecursive);
  TAddFoldersOptions = set of TAddFoldersOption;

var
  Translator: TAntJvTranslator;
  Settings: TRenamerSettings;
//  ToolbarImagesNormal: TTBImageList;
  ToolbarImagesHot: TImageList;
  GlobalStop: Boolean = False;
  GlobalSettings: record
    ForceDir: Boolean;
    DetectAbsdir: Boolean;
    FolderExt: Boolean;
    Copy: Boolean;
    Refresh: Boolean;
    Log: Boolean;
  end;
  MessageWin: TMessageWin;
  InputWin: TInputWin;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

implementation

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

end.
