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

unit VarMessages;

interface

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

var
  // Filters
  strfltAllFiles:   string = 'All files';
  strfltTextFiles:  string = 'Text files';
  strfltBatchFiles: string = 'Ant Renamer batch files';
  // Messages/Status
  strmsgSelectFiles:        string = 'Select files';
  strmsgSelectFolder:       string = 'Select folder';
  strmsgStatusAddingFiles:  string = 'Adding files';
  strmsgStatusRenaming:     string = 'Renaming files';
  strmsgFilesCount:         string = '%d files';
  strmsgBusy:               string = 'Application is busy: Renaming operation in progress.';
  strmsgNothingToDo:        string = 'Nothing to do: please select at least one file and an action to perform.';
  // Log info
  strlogStartRename:        string = 'Renaming process started';
  strlogStartPreview:       string = 'Refreshing preview';
  strlogStartRevert:        string = 'Revert to previous names';
  strlogEndRename:          string = 'Renaming process finished';
  strlogEndPreview:         string = 'Preview process finished';
  strlogEndRevert:          string = 'Revert process finished';
  strlogStoppedByUser:      string = 'Operation stopped by user';
  // Actions descriptions
  stradUnknown:         string = 'Do nothing';
  stradInclExt:         string = ', also apply to extension';
  stradOnlyExt:         string = ', apply to extension instead of name';
  stradChangeExt:       string = 'Change extension to "%s"';
  stradChangeExtRmv:    string = 'Remove extension';
  stradChangeExtNoRepl: string = 'Append extension "%s"';
  stradStringRepl:      string = 'Replace "%s" by "%s"';
  stradStringReplAll:   string = ', replace all occurences';
  stradStringReplCase:  string = ', case-sensitive';
  stradMultstrRepl:     string = 'Replace multiple strings using set "%s"';
  stradStringIns:       string = 'Insert "%s" at position %d';
  stradMoveStr:         string = 'Move %d characters from position %d%s, to position %d%s';
  stradCharDel:         string = 'Delete %d characters';
  stradCharDelPos:      string = ' at position %d';
  stradCharDelStr:      string = ' after the string "%s"';
  stradCharDelStr2:     string = ' before the string "%s"';
  stradEnum:            string = 'Generate an enumeration with "%s" from %d to %d by %d';
  stradMP3:             string = 'Generate namees with MP3 tags, based on the mask "%s"';
  stradMP3TwoDigit:     string = ', insert ''0'' in track number if required';
  stradFromBegin:       string = ', from begin';
  stradFromEnd:         string = ', from end';
  stradDateTime:        string = 'Generate names from files'' date and time, based on the mask "%s"';
  stradDateTimeCreat:   string = ' using creation date';
  stradDateTimeModif:   string = ' using last modification date';
  stradDateTimeSuffix:  string = ', add a suffix if required';
  stradRandom:          string = 'Generate random names using mask "%s"';
  stradRandomNumbers:   string = ', with numbers';
  stradRandomTick:      string = ', using Windows'' tick counter';
  stradRandomGUID:      string = ', using GUIDs';
  stradCase:            string = 'Change case:';
  stradCaseWords:       string = ' upper first letter following any of the characters "%s"';
  stradCaseFirst:       string = ' upper first letter of the name';
  stradCaseUpper:       string = ' all in uppercase';
  stradCaseLower:       string = ' all in lowercase';
  stradCaseLocale:      string = ', using Windows'' locale settings';
  stradFromList:        string = 'Rename files using specified list';
  stradFromListExt:     string = ', append original extension';
  stradFromListOnlyExt: string = ', use items as extensions instead of names';
  stradRegexp:          string = 'Rename files using a regular expression';
  stradExif:            string = 'Generate names with EXIF info, based on the mask "%s"';
  stradShiftDT:         string = 'Shift date/time by %d days and %d seconds'; 
  // Actions errors
  straeFromListEmpty:   string = 'Empty line';
  straeFromListEnd:     string = 'End of list';
  straeExifNoInfo:      string = 'No EXIF info found';
  // Files status
  strfsNotRenamed:    string = '%s was not renamed : %s';
  strfsRenamed:       string = '%s renamed to %s';
  strfsReverted:      string = '%s reverted to previous name %s';
  strfsRenError:      string = 'Could not rename %s to %s: %s';
  strfsNoOldName:     string = 'No previous name stored for %s to revert old name';
  strfs404:           string = 'Source file not found';
  strfsAlreadyExist:  string = 'A file or a folder with this name already exists';
  // Files errors
  strfeIdentical:     string = 'new name is the same than old name';
  // Hints
  strhintNotAgain:      string = 'Do not show this hint again';
  strhintShowPreview:   string = 'Preview column is now visible. If you want to hide it, click with the right mouse button on the column headers of the files list.';

implementation

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

end.
