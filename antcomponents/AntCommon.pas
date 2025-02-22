(*

(c) 2002-2003 Antoine Potten
software@antp.be
htt://www.antp.be/software/

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

*)

unit AntCommon;

interface

{$I Ant.inc}

uses
  Windows, Classes,
  {$IFDEF DELPHI7_UP}
  Themes,
  {$ENDIF}
  SysUtils;

var
  IsThemedXP: Boolean;
  ShellVersion: Integer;

function GetShellVersion: Cardinal;

implementation

{$IFNDEF DELPHI7_UP}
function GetFileVersion(const AFileName: string): Cardinal;
var
  FileName: string;
  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
begin
  Result := 0;
  // GetFileVersionInfo modifies the filename parameter data while parsing.
  // Copy the string const into a local variable to create a writeable copy.
  FileName := AFileName;
  UniqueString(FileName);
  InfoSize := GetFileVersionInfoSize(PChar(FileName), Wnd);
  if InfoSize <> 0 then
  begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(PChar(FileName), Wnd, InfoSize, VerBuf) then
        if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
          Result := FI.dwFileVersionMS;
    finally
      FreeMem(VerBuf);
    end;
  end;
end;
{$ENDIF}

function GetShellVersion: Cardinal;
begin
  if ShellVersion = 0 then
    ShellVersion := GetFileVersion('shell32.dll');
  Result := ShellVersion;
end;

{$IFDEF DELPHI7_UP}
initialization
  IsThemedXP := Themes.ThemeServices.ThemesEnabled;
{$ELSE}
var
  ThemeDLL: THandle = 0;
  IsThemeActive: function: Boolean; stdcall;
initialization
  IsThemedXP := False;
  // if >= WinXP
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and ((Win32MajorVersion > 5) or ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1))) then
  begin
    ThemeDLL := LoadLibrary('UxTheme.dll');
    if ThemeDLL <> 0 then
    begin
      @IsThemeActive := GetProcAddress(ThemeDLL, 'IsThemeActive');
      IsThemedXP := IsThemeActive;
      FreeLibrary(ThemeDLL);
    end;
  end;
{$ENDIF}
end.
