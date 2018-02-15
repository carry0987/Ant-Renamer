; Innosetup 5.4.2

#define DirDataName "Ant Renamer"

[Setup]
AppName=Ant Renamer
AppVerName=Ant Renamer 2.12.0
AppCopyright=Copyright © 2000-2015 Antoine Potten
AppPublisher=Ant Software
AppPublisherURL=http://www.antp.be/software/
AppUpdatesURL=http://www.antp.be/software/renamer/download/
AppVersion=2.12.0
AppId=Ant Renamer 2

DefaultDirName={pf}\Ant Renamer
DefaultGroupName=Ant Renamer

LicenseFile=license.txt

WindowVisible=false
OutputBaseFilename=antrenamer2_install
UninstallDisplayName=Ant Renamer
AllowNoIcons=true

Compression=lzma
SolidCompression=yes

[Dirs]
Name: "{commonappdata}\{#DirDataName}"; Permissions: users-modify; MinVersion: 0,4.0

[Files]
Source: Renamer.exe; DestDir: {app}; Flags: ignoreversion
Source: default.xml; DestName: Renamer.xml; DestDir: {app}; Flags: onlyifdoesntexist; MinVersion: 4.0,0
Source: default.xml; DestName: Renamer.xml; DestDir: "{commonappdata}\{#DirDataName}"; Flags: onlyifdoesntexist; MinVersion: 0,4.0
Source: license.txt; DestDir: {app}; Flags: ignoreversion

Source: Toolbars\Windows XP.bmp; DestDir: {app}\Toolbars; Components: Icons; Flags: ignoreversion
Source: Toolbars\Windows XP Large.bmp; DestDir: {app}\Toolbars; Components: Icons; Flags: ignoreversion

Source: Languages\*.*; Excludes: English.*; DestDir: {app}\Languages; Flags: ignoreversion; Components: Lang
Source: Languages\English.*; DestDir: {app}\Languages; Flags: ignoreversion

[Icons]
Name: {group}\Ant Renamer; Filename: {app}\renamer.exe; WorkingDir: {app}
;Name: {group}\Uninstall Ant Renamer; Filename: {uninstallexe}; WorkingDir: {app}

[Components]
Name: Icons; Description: Toolbar icon sets; Types: custom full
Name: Lang; Description: Language files; Types: custom full

[InstallDelete]
Type: files; Name: "{app}\Toolbars\Scrows.bmp"

[UninstallDelete]
Type: files; Name: "{app}\renamer.xml"
Type: files; Name: "{commonappdata}\{#DirDataName}\renamer.xml"; MinVersion: 0,5.0
Type: dirifempty; Name: {app}\Toolbars
Type: dirifempty; Name: {app}\Languages
Type: dirifempty; Name: {app}

