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

unit functions_sys;

interface

uses
  Classes, SysUtils;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function IsMSHTMLInstalled: Boolean;
function IsGeckoInstalled: Boolean;
procedure AssociateFileExtension(IconPath, ProgramName, Path, Extension: string);
procedure RebuildIconCache;
function IsKeyDown(const PKey: Byte): Boolean;
function GetFolderPath(const PathType: Integer): TFileName;
procedure ClearLastVisitedMRU(AppExeName: string);

var
  IsWindowsNT,
  IsWindowsXP,
  IsWindows7,
  IsThemedXP: Boolean;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

implementation

uses
  Windows, Registry, Messages, Shfolder,
  AntCommon,
  functions_str;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function IsMSHTMLInstalled: Boolean;
begin
  Result := False;
  with TRegistry.Create(KEY_EXECUTE) do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly('\SOFTWARE\Microsoft\Internet Explorer') then
      begin
        Result := StrToIntTrunc(ReadString('Version'), 0) >= 4;
        CloseKey;
      end;
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function IsGeckoInstalled: Boolean;
begin
  Result := False;
  with TRegistry.Create(KEY_EXECUTE) do
    try
      RootKey := HKEY_CLASSES_ROOT;
      if OpenKeyReadOnly('\CLSID\{1339B54C-3453-11D2-93B9-000000000000}\InprocServer32') then
      begin
        Result := Pos('mozctlx.dll', ReadString('')) > 0;
        CloseKey;
      end;
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure AssociateFileExtension(IconPath, ProgramName, Path, Extension: string);
  // comes from JVCL
begin
  with TRegistry.Create do
  begin
    RootKey := HKEY_CLASSES_ROOT;
    OpenKey(ProgramName, True);
    WriteString('', ProgramName);
    if IconPath <> '' then
    begin
      OpenKey('DefaultIcon', True);
      WriteString('', IconPath);
    end;
    CloseKey;
    OpenKey(ProgramName, True);
    OpenKey('shell', True);
    OpenKey('open', True);
    OpenKey('command', True);
    WriteString('', '"' + Path + '" "%1"');
    Free;
  end;
  with TRegistry.Create do
  begin
    RootKey := HKEY_CLASSES_ROOT;
    OpenKey('.' + extension, True);
    WriteString('', ProgramName);
    Free;
  end;
  RebuildIconCache;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure RebuildIconCache;
  // comes from JVCL
var
  Dummy: DWORD;
begin
  SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, SPI_SETNONCLIENTMETRICS,
    Longint(PChar('WindowMetrics')), SMTO_NORMAL or SMTO_ABORTIFHUNG, 10000, Dummy);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function IsKeyDown(const PKey: Byte): Boolean;
begin
  Result := ((GetKeyState(PKey) and $80) = $80);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function GetFolderPath(const PathType: Integer): TFileName;
var
  szPath: array[0..MAX_PATH] of Char;
begin
  SHGetFolderPath(0, PathType or CSIDL_FLAG_CREATE, 0, 0, szPath);
  Result := IncludeTrailingPathDelimiter(AnsiString(szPath));
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure ClearLastVisitedMRU(AppExeName: string);
var
  path: string;
  reg :TRegistry;

  function StartWithAppExeName(buffer: TByteArray; size: Integer): boolean;
  var
    len, j, k: Integer;
  begin
    result := False;
    len := Length(AppExeName);
    for j := 1 to len do
    begin
      k := (j - 1) * 2;
      if k + 1 >= size then
        break;
      if (buffer[k] <> Byte(AppExeName[j])) or (buffer[k + 1] <> 0) then
        break;
      if j = len then
        result := true;
    end;
  end;

  procedure RemoveAppMRU;
  var
    i, n: Integer;
    value: string;
    size, mruSize, mruPos: Integer;
    buffer, mruBuffer: TByteArray;
  begin
    try
      mruPos := 0;
      mruSize := reg.ReadBinaryData('MRUListEx', mruBuffer, Length(mruBuffer));
      if (mruSize > 0) and (mruSize mod SizeOf(Integer) = 0) then
      begin
        while mruPos < mruSize do
        begin
          n := Integer(Pointer(@(mruBuffer[mruPos]))^);
          if n > -1 then
          begin
            value := IntToStr(n);
            size := reg.ReadBinaryData(value, buffer, Length(buffer));
            if StartWithAppExeName(buffer, size) then
            begin
              reg.DeleteValue(value);
              for i := mruPos to mruSize - SizeOf(Integer) do
                mruBuffer[i] := mruBuffer[i + SizeOf(Integer)];
              mruPos := mruPos - SizeOf(Integer);
              mruSize := mruSize - SizeOf(Integer);
            end;
          end;
          mruPos := mruPos + SizeOf(Integer);
        end;
        reg.WriteBinaryData('MRUListEx', mruBuffer, mruSize)
      end;
    except
    end;
  end;

begin
  if IsWindows7 then
  begin
    AppExeName := ExtractFileName(AppExeName);
    reg := TRegistry.Create(KEY_QUERY_VALUE + KEY_WRITE);
    try
      reg.RootKey := HKEY_CURRENT_USER;
      path := 'Software\Microsoft\Windows\CurrentVersion\Explorer\ComDlg32';
      if reg.OpenKey(path + '\LastVisitedPidlMRULegacy', False) then
      begin
        RemoveAppMRU;
        reg.CloseKey;
      end;
      if reg.OpenKey(path + '\LastVisitedPidlMRU', False) then
      begin
        RemoveAppMRU;
        reg.CloseKey;
      end;
    finally
      reg.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

initialization
  IsWindowsNT := Win32Platform = VER_PLATFORM_WIN32_NT;
  IsWindowsXP := (IsWindowsNT) and (((Win32MajorVersion = 5) and (Win32MinorVersion >= 1)) or (Win32MajorVersion > 5));
  IsWindows7 := (IsWindowsNT) and (((Win32MajorVersion = 6) and (Win32MinorVersion >= 1)) or (Win32MajorVersion > 6));
  IsThemedXP := AntCommon.IsThemedXP;
end.
