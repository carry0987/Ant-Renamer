(************************************************************************
 *                                                                      *
 *   (C) 2002-2012 Antoine Potten, Mickaël Vanneufville                 *
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

unit functions_files;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Classes, Windows, SysUtils, Graphics,
  {$IFDEF ANTUNICODE}
  TntSysUtils,
  {$ENDIF}
  Controls;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

type
  TFileSizeUnit = (fsuB, fsuKB, fsuMB, fsuGB);
  {$IFDEF ANTUNICODE}
  TAntFileName = TWideFileName;
  {$ELSE}
  TAntFileName = TFileName;
  {$ENDIF}

var
  strHelpFile: TAntFileName;

procedure CopyAllFiles(const strSourceMask: string; const strDestFolder: string);
function CopyWithoutOverwrite(const strSourceFile, strDestPath: string): string;
procedure LaunchHelp(const Sender: TControl); overload;
procedure LaunchHelp(const HelpContext: Integer); overload;
{$IFDEF ANTUNICODE}
procedure LaunchProg(const CommandLine: WideString); overload;
procedure LaunchProg(const CommandLine, Folder: WideString); overload;
procedure LaunchProg(const CommandLine, Params, Folder: WideString); overload;
procedure LaunchExplorer(const FileName: WideString); overload;
{$ELSE}
procedure LaunchProg(const CommandLine: string); overload;
procedure LaunchProg(const CommandLine, Folder: string); overload;
procedure LaunchProg(const CommandLine, Params, Folder: string); overload;
procedure LaunchExplorer(const FileName: string); overload;
{$ENDIF}
procedure DumpToFile(const FileName, Value: string);
procedure LogToFile(const FileName, Value: string);
function ValidateFileName(const AFileName: string): string;
procedure ExtractFileSize(const strFileName: string; MakeString: Boolean; var FileSize: string; var NumberOfFiles: Integer; SizeUnit: TFileSizeUnit);
function GetFileSizeUnitId(SizeUnit: TFileSizeUnit): Integer;
function Deltree(sDir: string): Boolean;
function GetBuild3(const AFileName: TAntFileName): string;
function GetBuild4(const AFileName: TAntFileName): string;
function GetFileSize(const FileName: TAntFileName): Int64;
function GetShellIcon(csidl: Integer; const Large: Boolean): TIcon;
function GetVolumeLabel(const DriveLetter: Char): string;
{$IFDEF ANTUNICODE}
function MakeUniqueFileName(const strFileName: WideString; const Separator: WideString = ''): WideString;
function GetIcon(const FileName: WideString; const Large: Boolean): TIcon;
function GetShellPath(csidl: Integer): WideString;
function GetShellCaption(csidl: Integer): WideString;
function GetDirectoryIcon(const Folder: WideString; const Large: Boolean): TIcon;
function GetFileDates(const FileName: WideString; out Created, Modified, Access: TDateTime): Boolean;
function GetFileModifiedDate(const FileName: WideString): TDateTime;
function FileOrDirExists(const FileName: WideString): Boolean;
function WideSetCurrentDir(const FileName: WideString): Boolean;
{$ELSE}
function MakeUniqueFileName(const strFileName: string; const Separator: string = ''): string;
function GetIcon(const FileName: string; const Large: Boolean): TIcon;
function GetShellPath(csidl: Integer): string;
function GetShellCaption(csidl: Integer): string;
function GetDirectoryIcon(const Folder: string; const Large: Boolean): TIcon;
function GetFileDates(const FileName: string; out Created, Modified, Access: TDateTime): Boolean;
function GetFileModifiedDate(const FileName: string): TDateTime;
function FileOrDirExists(const FileName: string): Boolean;
{$ENDIF}
function CountFolderContents(const Folder: TAntFileName; const CountFolders, CountFiles, Recursive: Boolean): Integer;
function DirectoryIsEmpty(Directory: string): Boolean;
function ListDirectory(const ADir: TFileName; const AMask: string): string;
function DeleteFolder(ADirName: string; OnlyIfEmpty: Boolean): boolean;
function CopyFolder(ASourceDir, ATargetDir: string): boolean;
function GetRealPictureExt(AStream: TStream; const IfPNG, IfJPG, IfGIF, IfBMP, Current: string): string;
function IsSameFileAttr(const File1, File2: TFileName; const Allow1hDiff: Boolean = False): Boolean;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

implementation

uses
  Math, StrUtils,
  ShlObj, ActiveX, ShellAPI, DateUtils, functions_sys, functions_str;

{$IFDEF ANTUNICODE}
// bug in ShellAPI
function SHGetFileInfoW2(pszPath: PWideChar; dwFileAttributes: DWORD; var psfi: TSHFileInfoW; cbFileInfo, uFlags: UINT): DWORD; stdcall; external 'shell32.dll' name 'SHGetFileInfoW';
{$ENDIF}

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure CopyAllFiles(const strSourceMask: string; const strDestFolder: string);
var
  SearchInfo: TSearchRec;
  FileFound: integer;
begin
  FileFound := FindFirst(strSourceMask, faAnyFile, SearchInfo);
  while FileFound = 0 do
  begin
    CopyFile(PChar(ExtractFilePath(strSourceMask)+'\'+SearchInfo.Name), PChar(strDestFolder+'\'+ExtractFileName(SearchInfo.Name)), false);
    FileFound := FindNext(SearchInfo);
  end;
  FindClose(SearchInfo);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function CopyWithoutOverwrite(const strSourceFile, strDestPath: string): string;
var
  strDestFile: string;
begin
  strDestFile := MakeUniqueFileName(strDestPath + ExtractFileName(strSourceFile));
  if CopyFile(PChar(strSourceFile), PChar(strDestFile), True) then
    Result := ExtractFileName(strDestFile)
  else
    Result := '';
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

{$IFDEF ANTUNICODE}
function MakeUniqueFileName(const strFileName: WideString; const Separator: WideString): WideString;
begin
  if FileOrDirExists(strFileName) then
    repeat
      Sleep(Random(24)+1);
      Result := WideFormat('%s%s%d%s', [WideChangeFileExt(strFileName, ''), Separator, GetTickCount, WideExtractFileExt(strFileName)]);
    until not FileOrDirExists(Result)
  else
    Result := strFileName;
end;
{$ELSE}
function MakeUniqueFileName(const strFileName: string; const Separator: string): string;
begin
  Result := strFileName;
  while FileOrDirExists(Result) do
  begin
    Sleep(Random(24)+1);
    Result := Format('%s%s%d%s', [ChangeFileExt(strFileName, ''), Separator, GetTickCount, ExtractFileExt(strFileName)]);
  end;
end;
{$ENDIF}

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure LaunchHelp(const Sender: TControl);
begin
  LaunchHelp(Sender.HelpContext);
end;

procedure LaunchHelp(const HelpContext: Integer);
var
  f: TAntFileName;
begin
  {$IFDEF ANTUNICODE}
  f := WideExtractShortPathName(strHelpFile); // hh.exe seems unable to open a file with unicode path
  if f = '' then
  {$ENDIF}
    f := strHelpFile;
  LaunchProg( 'hh.exe', Format('-mapid %d ', [HelpContext]) + f, '');
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

{$IFDEF ANTUNICODE}

procedure LaunchProg(const CommandLine: WideString);
begin
  if IsWindowsNT then
    ShellExecuteW(0, nil, PWideChar(CommandLine), nil, nil, SW_NORMAL)
  else
    ShellExecute(0, nil, PChar(AnsiString(CommandLine)), nil, nil, SW_NORMAL);
end;

procedure LaunchProg(const CommandLine, Folder: WideString);
begin
  if IsWindowsNT then
    ShellExecuteW(0, nil, PWideChar(CommandLine), nil, PWideChar(Folder), SW_NORMAL)
  else
    ShellExecute(0, nil, PChar(AnsiString(CommandLine)), nil, PChar(AnsiString(Folder)), SW_NORMAL);
end;

procedure LaunchProg(const CommandLine, Params, Folder: WideString); overload;
begin
  if IsWindowsNT then
    ShellExecuteW(0, nil, PWideChar(CommandLine), PWideChar(Params), PWideChar(Folder), SW_NORMAL)
  else
    ShellExecute(0, nil, PChar(AnsiString(CommandLine)), PChar(AnsiString(Params)), PChar(AnsiString(Folder)), SW_NORMAL);
end;

procedure LaunchExplorer(const FileName: WideString); overload;
begin
  if IsWindowsNT then
    ShellExecuteW(0, nil, PWideChar(WideString('explorer.exe')), PWideChar(WideString('/select, "' + FileName + '"')), nil, SW_NORMAL)
  else
    ShellExecute(0, nil, PChar(AnsiString('explorer.exe')), PChar(AnsiString('/select, "' + FileName + '"')), nil, SW_NORMAL);
end;

{$ELSE}

procedure LaunchProg(const CommandLine: string);
begin
  ShellExecute(0, nil, PChar(CommandLine), nil, nil, SW_NORMAL);
end;

procedure LaunchProg(const CommandLine, Folder: string);
begin
  ShellExecute(0, nil, PChar(CommandLine), nil, PChar(Folder), SW_NORMAL);
end;

procedure LaunchProg(const CommandLine, Params, Folder: string); overload;
begin
  ShellExecute(0, nil, PChar(CommandLine), PChar(Params), PChar(Folder), SW_NORMAL);
end;

procedure LaunchExplorer(const FileName: string); overload;
begin
  ShellExecute(0, nil, PChar('explorer.exe'), PChar('/select, "' + FileName + '"'), nil, SW_NORMAL);
end;

{$ENDIF}

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure DumpToFile(const FileName, Value: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmCreate);
  try
    fs.Size := 0;
    fs.WriteBuffer(Value[1], Length(Value));
  finally
    fs.Free;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure LogToFile(const FileName, Value: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmOpenReadWrite);
  try
    fs.Seek(fs.Size, soFromBeginning);
    fs.WriteBuffer(sLinebreak[1], Length(sLinebreak));
    fs.WriteBuffer(Value[1], Length(Value))
  finally
    fs.Free;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function ValidateFileName(const AFileName: string): string;
var
  i: Integer;
begin
  if AFileName <> '' then
  begin
    Result := AFileName;
    for i := 1 to Length(Result) do
      case Result[i] of
        '"' :
          Result[i] := '''';
        '?' :
          Result[i] := '.';
        '\', ':', '/', '*', '<', '>', '|', '¦' :
          Result[i] := '-';
      end;
  end else
  begin
    Sleep(50);
    Result := IntToStr(GetTickCount);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

type
  PFixedFileInfo = ^TFixedFileInfo;
  TFixedFileInfo = record
     Signature: DWord;
     StrucVersion: DWord;
     Minor: Word;
     Major: Word;
     Build: Word;
     Release: Word;
     FileFlagsMask: DWord;
     FileFlags: DWord;
     FileOS: DWord;
     FileType: DWord;
     FileSubtype: DWord;
     FileDateMS: DWord;
     FileDateLS: DWord;
  end;

function GetFileInfo(const AFileName: TAntFileName): TFixedFileInfo;
var
  Handle, VersionSize: DWORD;
  SubBlock: string;
  Temp: Pointer;
  Data: Pointer;
  ok: Boolean;
begin
  SubBlock := '\';
  {$IFDEF ANTUNICODE}
  if IsWindowsNT then
    VersionSize := GetFileVersionInfoSizeW(PWideChar(AFileName), Handle)
  else
  {$ENDIF}
    VersionSize := GetFileVersionInfoSize(PChar(AnsiString(AFileName)), Handle);
  if VersionSize > 0 then
  begin
    GetMem(Temp, VersionSize);
    try
      {$IFDEF ANTUNICODE}
      if IsWindowsNT then
        ok := GetFileVersionInfoW(PWideChar(AFileName), Handle, VersionSize, Temp)
      else
      {$ENDIF}
        ok := GetFileVersionInfo(PChar(AnsiString(AFileName)), Handle, VersionSize, Temp);
      if ok and VerQueryValue(Temp, PChar(SubBlock), Data, VersionSize) then
        Result := PFixedFileInfo(Data)^;
    finally
      FreeMem(Temp);
    end;
  end else
    RaiseLastOSError;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function GetBuild3(const AFileName: TAntFileName): string;
begin
  with GetFileInfo(AFileName) do
    Result := Format('%d.%d%d.%d', [Major, Minor, Release, Build]);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function GetBuild4(const AFileName: TAntFileName): string;
begin
  with GetFileInfo(AFileName) do
    Result := Format('%d.%d.%d.%d', [Major, Minor, Release, Build]);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure ExtractFileSize(const strFileName: string; MakeString: Boolean; var FileSize: string; var NumberOfFiles: Integer; SizeUnit: TFileSizeUnit);
var
  iSize: Int64;
  SearchRec: TSearchRec;
begin
  if FindFirst(strFileName, faAnyFile, SearchRec) = 0 then
  begin
    iSize := (Int64(SearchRec.FindData.nFileSizeHigh) shl 32) + SearchRec.FindData.nFileSizeLow;
    if SizeUnit > fsuMB then
      iSize := iSize div 1024;
    if SizeUnit > fsuKB then
      iSize := iSize div 1024;
    if SizeUnit > fsuB then
      iSize := iSize div 1024;
    if FileSize = '' then
    begin
      FileSize := intToStr(iSize);
    end else
    begin
      if makeString then
        FileSize := Format('%s+%d', [FileSize, iSize])
      else
        try
          FileSize := IntToStr(strToInt(FileSize) + iSize);
        except
        end;
    end;
    FindClose(SearchRec);
    Inc(NumberOfFiles);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function GetFileSizeUnitId(SizeUnit: TFileSizeUnit): Integer;
begin
  Result := -1;
  case SizeUnit of
    fsuB: Result := 0;
    fsuKB: Result := 1;
    fsuMB: Result := 2;
    fsuGB: Result := 3;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function Deltree(sDir: string): Boolean;
var
  iIndex: Integer;
  SearchRec: TSearchRec;
  sFileName: string;
begin
  sDir := sDir + '\*.*';
  iIndex := FindFirst(sDir, faAnyFile, SearchRec);
  while iIndex = 0 do
  begin
    sFileName := ExtractFileDir(sDir) + '\' + SearchRec.Name;
    if SearchRec.Attr = faDirectory then
    begin
    if (SearchRec.Name <> '' ) and
       (SearchRec.Name <> '.') and
       (SearchRec.Name <> '..') then
       Deltree(sFileName);
    end else
    begin
      if SearchRec.Attr <> 0 then
        FileSetAttr(sFileName, 0);
      DeleteFile(sFileName);
    end;
    iIndex := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
  RemoveDir(ExtractFileDir(sDir));
  Result := True;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}


{$IFDEF ANTUNICODE}
function GetIcon(const FileName: WideString; const Large: Boolean): TIcon;
{$ELSE}
function GetIcon(const FileName: string; const Large: Boolean): TIcon;
{$ENDIF}
var
  {$IFDEF ANTUNICODE}
  sfiW: TSHFileInfoW;
  {$ENDIF}
  sfi: TSHFileInfo;
  s: string;
  i: Integer;
begin
  Result := nil;
  try
    Result := TIcon.Create;
    if Large then
      i := SHGFI_LARGEICON
    else
      i := SHGFI_SMALLICON;
    {$IFDEF ANTUNICODE}
    if IsWindowsNT then
    begin
      SHGetFileInfoW2(PWideChar(FileName), FILE_ATTRIBUTE_NORMAL, sfiW, SizeOf(sfiW), SHGFI_ICON or SHGFI_USEFILEATTRIBUTES or i);
      if sfiW.hIcon = 0 then
        FreeAndNil(Result)
      else
        Result.Handle := sfiW.hIcon;
    end
    else
    {$ENDIF}
    begin
      s := FileName;
      SHGetFileInfo(PChar(s), FILE_ATTRIBUTE_NORMAL, sfi, SizeOf(sfi), SHGFI_ICON or SHGFI_USEFILEATTRIBUTES or i);
      if sfi.hIcon = 0 then
        FreeAndNil(Result)
      else
        Result.Handle := sfi.hIcon;
    end;
  except
    FreeAndNil(Result);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

{$IFDEF ANTUNICODE}
function GetDirectoryIcon(const Folder: WideString; const Large: Boolean): TIcon;
{$ELSE}
function GetDirectoryIcon(const Folder: string; const Large: Boolean): TIcon;
{$ENDIF}
var
  {$IFDEF ANTUNICODE}
  sfiW: TSHFileInfoW;
  {$ENDIF}
  sfi: TSHFileInfo;
  s: string;
  i: Integer;
begin
  Result := nil;
  try
    Result := TIcon.Create;
    if Large then
      i := SHGFI_LARGEICON
    else
      i := SHGFI_SMALLICON;
    {$IFDEF ANTUNICODE}
    if IsWindowsNT then
    begin
      SHGetFileInfoW2(PWideChar(Folder), FILE_ATTRIBUTE_DIRECTORY, sfiW, SizeOf(sfiW), SHGFI_ICON or SHGFI_USEFILEATTRIBUTES or i);
      Result.Handle := sfiW.hIcon;
    end
    else
    {$ENDIF}
    begin
      s := Folder;
      SHGetFileInfo(PChar(s), FILE_ATTRIBUTE_DIRECTORY, sfi, SizeOf(sfi), SHGFI_ICON or SHGFI_USEFILEATTRIBUTES or i);
      Result.Handle := sfi.hIcon;
    end;
  except
    if Result <> nil then
      FreeAndNil(Result);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}
               
{$IFDEF ANTUNICODE}
function GetShellPath(csidl: Integer): WideString;
{$ELSE}
function GetShellPath(csidl: Integer): string;
{$ENDIF}
var
  pidl: PItemIDList;
  buf: array[0..MAX_PATH] of Char;
  {$IFDEF ANTUNICODE}
  bufW: array[0..MAX_PATH] of WideChar;
  {$ENDIF}
begin
  Result := '';
  if Succeeded(ShGetSpecialFolderLocation(0, csidl, pidl)) then
  begin
    {$IFDEF ANTUNICODE}
    if IsWindowsNT then
    begin
      if ShGetPathfromIDListW(pidl, bufW) then
        Result := bufW;
    end
    else
    {$ENDIF}
    begin
      if ShGetPathfromIDList(pidl, buf) then
        Result := buf;
    end;
    CoTaskMemFree(pidl);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function GetShellIcon(csidl: Integer; const Large: Boolean): TIcon;
var
  pidl: PItemIDList;
  {$IFDEF ANTUNICODE}
  sfiW: TShFileInfoW;
  {$ENDIF}
  sfi: TShFileInfo;
  i: Integer;
begin
  Result := nil;
  if Succeeded(ShGetSpecialFolderLocation(0, csidl, pidl)) then
  begin
    try
      Result := TIcon.Create;
      if Large then
        i := SHGFI_LARGEICON
      else
        i := SHGFI_SMALLICON;
      {$IFDEF ANTUNICODE}
      if IsWindowsNT then
      begin
        ShGetFileInfoW2(PWideChar(pidl), SHGFI_USEFILEATTRIBUTES, sfiW, SizeOf(sfiW), SHGFI_ICON or SHGFI_PIDL or i);
        Result.Handle := sfiW.hIcon;
      end
      else
      {$ENDIF}
      begin
        ShGetFileInfo(PChar(pidl), SHGFI_USEFILEATTRIBUTES, sfi, SizeOf(sfi), SHGFI_ICON or SHGFI_PIDL or i);
        Result.Handle := sfi.hIcon;
      end;
    except
      if Result <> nil then
        FreeAndNil(Result);
    end;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

{$IFDEF ANTUNICODE}
function GetShellCaption(csidl: Integer): WideString;
{$ELSE}
function GetShellCaption(csidl: Integer): string;
{$ENDIF}
var
  pidl: PItemIDList;
  {$IFDEF ANTUNICODE}
  sfiW: TShFileInfoW;
  {$ENDIF}
  sfi: TShFileInfo;
begin
  Result := '';
  if Succeeded(ShGetSpecialFolderLocation(0, csidl, pidl)) then
  begin
    {$IFDEF ANTUNICODE}
    if IsWindowsNT then
    begin
      ShGetFileInfoW2(PWideChar(pidl), SHGFI_USEFILEATTRIBUTES, sfiW, SizeOf(sfiW), SHGFI_DISPLAYNAME or SHGFI_PIDL);
      Result := sfiW.szDisplayName;
    end
    else
    {$ENDIF}
    begin
      ShGetFileInfo(PChar(pidl), SHGFI_USEFILEATTRIBUTES, sfi, SizeOf(sfi), SHGFI_DISPLAYNAME or SHGFI_PIDL);
      Result := sfi.szDisplayName;
    end;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function GetFileSize(const FileName: TAntFileName): Int64;
var
{$IFDEF ANTUNICODE}
  SearchRec: TSearchRecW;
{$ELSE}
  SearchRec: TSearchRec;
{$ENDIF}
begin
  Result := -1;
  {$IFDEF ANTUNICODE}
  if WideFindFirst(FileName, faAnyFile, SearchRec) = 0 then
  {$ELSE}
  if FindFirst(FileName, faAnyFile, SearchRec) = 0 then
  {$ENDIF}
    Result := (Int64(SearchRec.FindData.nFileSizeHigh) shl 32) + SearchRec.FindData.nFileSizeLow;
  {$IFDEF ANTUNICODE}
  WideFindClose(SearchRec);
  {$ELSE}
  FindClose(SearchRec);
  {$ENDIF}
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

{$IFDEF ANTUNICODE}
function GetFileDates(const FileName: WideString; out Created, Modified, Access: TDateTime): Boolean;
var
  SearchRec: TSearchRecW;
{$ELSE}
function GetFileDates(const FileName: string; out Created, Modified, Access: TDateTime): Boolean;
var
  SearchRec: TSearchRec;
{$ENDIF}
  function Convert(const ft: TFileTime): TDateTime;
  var
    lt: TFileTime;
    st: TSystemTime;
  begin
    Result := MinDouble;
    if FileTimeToLocalFileTime(ft, lt) then
      if FileTimeToSystemTime(lt, st) then
        with st do
          Result := EncodeDateTime(wYear, wMonth, wDay, wHour, wMinute, wSecond, wMilliseconds);
  end;
begin
  Result := False;
  {$IFDEF ANTUNICODE}
  if WideFindFirst(FileName, faAnyFile, SearchRec) = 0 then
  {$ELSE}
  if FindFirst(FileName, faAnyFile, SearchRec) = 0 then
  {$ENDIF}
  begin
    Created := Convert(SearchRec.FindData.ftCreationTime);
    Modified := Convert(SearchRec.FindData.ftLastWriteTime);
    Access := Convert(SearchRec.FindData.ftLastAccessTime);
    Result := True;
  end;
  {$IFDEF ANTUNICODE}
  WideFindClose(SearchRec);
  {$ELSE}
  FindClose(SearchRec);
  {$ENDIF}
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

{$IFDEF ANTUNICODE}
function GetFileModifiedDate(const FileName: WideString): TDateTime;
{$ELSE}
function GetFileModifiedDate(const FileName: string): TDateTime;
{$ENDIF}
var
  DateCreate, DateModif, DateAccess: TDateTime;
begin
  if GetFileDates(FileName, DateCreate, DateModif, DateAccess) then
    Result := DateModif
  else
    Result := 0;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function GetVolumeLabel(const DriveLetter: Char): string;
var
  Drive: string;
  DriveName: PChar;
  p1, p2: Cardinal;
begin
  Result := '';
  Drive := DriveLetter + ':\';
  DriveName := StrAlloc(128);
  if GetVolumeInformation(PChar(Drive), DriveName, 128, nil, p1, p2, nil, 0) then
    SetString(Result, DriveName, StrLen(DriveName));
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

{$IFDEF ANTUNICODE}
function FileOrDirExists(const FileName: WideString): Boolean;
var
  SearchRec: TSearchRecW;
begin
  Result := WideFindFirst(FileName, faAnyFile, SearchRec) = 0;
  WideFindClose(SearchRec);
end;
{$ELSE}
function FileOrDirExists(const FileName: string): Boolean;
var
  SearchRec: TSearchRec;
begin
  Result := FindFirst(FileName, faAnyFile, SearchRec) = 0;
  FindClose(SearchRec);
end;
{$ENDIF}

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

{$IFDEF ANTUNICODE}
function WideSetCurrentDir(const FileName: WideString): Boolean;
begin
  if IsWindowsNT then
    Result := SetCurrentDirectoryW(PWideChar(FileName))
  else
    Result := SetCurrentDir(FileName);
end;
{$ENDIF}

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function CountFolderContents(const Folder: TAntFileName; const CountFolders, CountFiles, Recursive: Boolean): Integer;
var
{$IFDEF ANTUNICODE}
  SearchRec: TSearchRecW;
{$ELSE}
  SearchRec: TSearchRec;
{$ENDIF}
  rc: Integer;
begin
  Result := 0;
  {$IFDEF ANTUNICODE}
  rc := WideFindFirst(Folder + '*.*', faAnyFile, SearchRec);
  {$ELSE}
  rc := FindFirst(Folder + '*.*', faAnyFile, SearchRec);
  {$ENDIF}
  while rc = 0 do
  begin
    if  not ((SearchRec.Name = '.') or (SearchRec.Name = '..')) then
    begin
      if SearchRec.Attr and faDirectory <> 0 then
      begin
        if CountFolders then
          Inc(Result);
        if Recursive then
          Result := Result + CountFolderContents(Folder + SearchRec.Name + PathDelim, CountFolders, CountFiles, True);
      end
      else
        if CountFiles then
          Inc(Result);
    end;
    {$IFDEF ANTUNICODE}
    rc := WideFindNext(SearchRec);
    {$ELSE}
    rc := FindNext(SearchRec);
    {$ENDIF}
  end;
  {$IFDEF ANTUNICODE}
  WideFindClose(SearchRec);
  {$ELSE}
  FindClose(SearchRec);
  {$ENDIF}
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function DirectoryIsEmpty(Directory: string): Boolean;
var
{$IFDEF ANTUNICODE}
  SearchRec: TSearchRecW;
{$ELSE}
  SearchRec: TSearchRec;
{$ENDIF}
  rc, i: Integer;
begin
  Result := False;
  {$IFDEF ANTUNICODE}
  rc := WideFindFirst(IncludeTrailingPathDelimiter(Directory) + '*', faAnyFile, SearchRec);
  {$ELSE}
  rc := FindFirst(IncludeTrailingPathDelimiter(Directory) + '*', faAnyFile, SearchRec);
  {$ENDIF}
  if rc = 0 then
    for i := 1 to 2 do
      if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
        {$IFDEF ANTUNICODE}
        Result := WideFindNext(SearchRec) <> 0;
        {$ELSE}
        Result := FindNext(SearchRec) <> 0;
        {$ENDIF}
  {$IFDEF ANTUNICODE}
  WideFindClose(SearchRec);
  {$ELSE}
  FindClose(SearchRec);
  {$ENDIF}
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function ListDirectory(const ADir: TFileName; const AMask: string): string;
var
  lst: TStringList;
  rc: Integer;
  sr: TSearchRec;
  s: string;
  dt: TDateTime;
begin
  Result := '';
  lst := TStringList.Create;
  try
    rc := FindFirst(IncludeTrailingPathDelimiter(ADir) + AMask, faAnyFile, sr);
    while rc = 0 do
    begin
      dt := FileDateToDateTime(sr.Time);
      s := Format('%s'#9'%d'#9'%s'#9'%s', [
        sr.Name,
        sr.Size,
        FormatDateTime('yyyy"-"mm"-"dd hh":"nn":"ss', dt),
        IfThen((sr.Attr and faDirectory) <> 0, 'D', '')
      ]);
      lst.Add(s);
      rc := FindNext(sr);
    end;
    FindClose(sr);
    Result := lst.Text;
  finally
    lst.Free;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function DeleteFolder(ADirName: string; OnlyIfEmpty: Boolean): boolean;
var fo: TSHFileopStruct; 
begin 
  ADirName := ExcludeTrailingPathDelimiter(Trim(ADirName)); 

  if OnlyIfEmpty then
    if DirectoryIsEmpty(ADirName) then
      Result := RemoveDir(ADirName)
    else
      Result := False
  else
  begin
    FillChar(fo, SizeOf(fo),0);
    with fo do
    begin
      wFunc := FO_DELETE;
      pFrom := PChar(ADirName + #0);
      fFlags := FOF_SILENT or FOF_NOCONFIRMATION;
    end;
    Result := ShFileOperation(fo) = 0;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function CopyFolder(ASourceDir, ATargetDir: string): boolean;
var fo: TSHFileopStruct;
begin
  ASourceDir := ExcludeTrailingPathDelimiter(Trim(ASourceDir));
  ATargetDir := ExcludeTrailingPathDelimiter(Trim(ATargetDir));

  FillChar(fo, SizeOf(fo),0);
  with fo do
  begin
    wFunc := FO_COPY;
    pFrom := PChar(ASourceDir + #0);
    pTo   := PChar(ATargetDir + #0);
    fFlags := FOF_SILENT or FOF_NOCONFIRMATION or FOF_NOCONFIRMMKDIR; 
  end; 
  Result := ShFileOperation(fo) = 0; 
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function GetRealPictureExt(AStream: TStream; const IfPNG, IfJPG, IfGIF, IfBMP, Current: string): string;
var
  Header: string;
begin
  AStream.Seek(0, soFromBeginning);
  SetLength(Header, 10);
  AStream.Read(Header[1], 10);
  if Pos('PNG', Header) > 0 then
    Result := IfPNG
  else
  if Pos('JFIF', Header) > 0 then
    Result := IfJPG
  else
  if Pos('GIF', Header) > 0 then
    Result := IfGIF
  else
  if Pos('BM', Header) = 1 then
    Result := IfBMP
  else
    Result := Current;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

const
  OneHour = 1 / 24;
  OneSecond = OneHour / 60 / 60;

function IsSameFileAttr(const File1, File2: TFileName; const Allow1hDiff: Boolean = False): Boolean;
var
  Size1, Size2: Int64;
  Date1, Date2: TDateTime;
begin
  Size1 := GetFileSize(File1);
  Size2 := GetFileSize(File2);
  Result := Size1 = Size2;
  if Result then
  begin
    Date1 := GetFileModifiedDate(File1);
    Date2 := GetFileModifiedDate(File2);
    Result := SameValue(Date1, Date2, OneSecond);
    if not Result and Allow1hDiff then
      Result := SameValue(Date1, Date2 - OneHour, OneSecond) or SameValue(Date1, Date2 + OneHour, OneSecond);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

initialization
  Randomize;
end.
