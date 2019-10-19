(************************************************************************
 *                                                                      *
 *   Ant Renamer 2.x                                                    *
 *   (C) 2003-2008 Antoine Potten                                       *
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

unit Files;

interface

uses
  Classes, SysUtils, Contnrs;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

type
  TRenState = (rsNotRenamed, rsOk, rsError, rsInfo);

  TRenError = record
    Description: WideString;
    Status: TRenState;
  end;

  TRenFileEvent = procedure (Sender: TObject; const SourceName, DestName: WideString) of object;

  TRenFile = class(TObject)
  private
    FPreview: WideString;
    FOldName: WideString;
    FFolder: WideString;
    FName: WideString;
    FPosition: Integer;
    FDescription: WideString;
    FStatus: TRenState;
    FListItem: Pointer;
    FIsFolder: Boolean;
    FSize: Int64;
    FDates: Integer;
    FAccessDate: TDateTime;
    FCreatedDate: TDateTime;
    FModifiedDate: TDateTime;
    FCopyDone: Boolean;
    function GetFileName: WideString;
    function GetFileExt: WideString;
    function GetFileFolder: WideString;
    function GetFilePath: WideString;
    function GetSize: Int64;
    function GetDates: Boolean;
    function GetAccessDate: TDateTime;
    function GetCreatedDate: TDateTime;
    function GetModifiedDate: TDateTime;
  public
    OnRenamed: TRenFileEvent;
    property Preview: WideString read FPreview write FPreview;
    property FileName: WideString read GetFileName;
    property FileExt: WideString read GetFileExt;
    property FileFolder: WideString read GetFileFolder;
    property FilePath: WideString read GetFilePath;
    property Position: Integer read FPosition write FPosition;
    property Description: WideString read FDescription;
    property Status: TRenState read FStatus;
    property ListItem: Pointer read FListItem write FListItem;
    property IsFolder: Boolean read FIsFolder;
    property Size: Int64 read GetSize;
    property AccessDate: TDateTime read GetAccessDate;
    property CreatedDate: TDateTime read GetCreatedDate;
    property ModifiedDate: TDateTime read GetModifiedDate;
    function Rename(ANewName: WideString): Boolean;
    function Revert: Boolean;
    function Exists: Boolean;
    procedure SetError(const ADescription: WideString; const AStatus: TRenState);
    constructor Create(const AFilePath: WideString; const ANode: Pointer);
    destructor Destroy; override;
    procedure InitBeforeJob;
    procedure CheckPathForUpdate(const SourceName, DestName: WideString);
  end;

  TRenFiles = class(TObjectList)
  private
  public
    function    Add(const APath: WideString; const ANode: Pointer): TRenFile; reintroduce;
    procedure   Sort; reintroduce;
    procedure   FileRenamed(Sender: TObject; const SourceName, DestName: WideString);
  end;

  TFileNode = record
    RenFile: TRenFile;
  end;
  PFileNode = ^TFileNode;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

implementation

uses
  {MinDouble}Math,
  TntWindows, TntSysUtils, TntWideStrUtils,
  functions_files,
  Global, VarMessages;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

constructor TRenFile.Create(const AFilePath: WideString; const ANode: Pointer);
begin
  FName := WideExtractFileName(AFilePath);
  FFolder := WideExtractFilePath(AFilePath);
  FOldName := '';
  FDescription := '';
  FStatus := rsNotRenamed;
  FListItem := ANode;
  FIsFolder := WideDirectoryExists(AFilePath);
  FSize := -2;
  FDates := -2;
  FCreatedDate := 0;
  FAccessDate := 0;
  FModifiedDate := 0;
  FCopyDone := False;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

destructor TRenFile.Destroy;
begin
  inherited;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenFile.Exists: Boolean;
begin
  Result := FileOrDirExists(GetFilePath);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenFile.GetFileExt: WideString;
begin
  Result := WideExtractFileExt(FName);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenFile.GetFileFolder: WideString;
begin
  Result := FFolder;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenFile.GetFileName: WideString;
begin
  Result := FName;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenFile.GetFilePath: WideString;
begin
  Result := FFolder + FName;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenFile.Rename(ANewName: WideString): Boolean;
var
  LE: Integer;
  SourceName: WideString;
begin
  if not ((GlobalSettings.DetectAbsdir) and ((Copy(ANewName, 2, 2) = ':\') or (Copy(ANewName, 1, 2) = '\\'))) then
    ANewName := FFolder + ANewName;
  SourceName := FilePath;
  if ANewName = SourceName then
  begin
    Result := False;
    FDescription := WideFormat(strfsNotRenamed, [FName, strfeIdentical]);
    FStatus := rsNotRenamed;
  end else
  begin
    if GlobalSettings.ForceDir then
      WideForceDirectories(WideExtractFilePath(ANewName));
    if FCopyDone or not GlobalSettings.Copy then
    begin
      FOldName := SourceName;
      Result := Tnt_MoveFileW(PWideChar(SourceName), PWideChar(ANewName));
    end
    else
    begin
      FOldName := '';
      Result := Tnt_CopyFileW(PWideChar(SourceName), PWideChar(ANewName), True);
      if Result then
        FCopyDone := True;
    end;
    if Result then
    begin
      if Assigned(OnRenamed) then
        OnRenamed(Self, SourceName, ANewName);
      FName := WideExtractFileName(ANewName);
      if FFolder = WideExtractFilePath(ANewName) then
        FDescription := WideFormat(strfsRenamed, [SourceName, FName])
      else
      begin
        FDescription := WideFormat(strfsRenamed, [SourceName, ANewName]);
        FFolder := WideExtractFilePath(ANewName);
      end;
      FStatus := rsOk;
    end
    else
    begin
      if FileOrDirExists(ANewName) then
        FDescription := WideFormat(strfsRenError, [SourceName, ANewName, strfsAlreadyExist])
      else
      begin
        LE := GetLastError();
        FDescription := WideFormat(strfsRenError, [SourceName, ANewName, SysErrorMessage(LE)]);
      end;
      FStatus := rsError;
      FOldName := '';
    end;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenFile.Revert: Boolean;
var
  LE: Integer;
  SourceName: WideString;
begin
  Result := True;
  if FOldName <> '' then
  begin
    SourceName := FilePath;
    Result := Tnt_MoveFileW(PWideChar(SourceName), PWideChar(FOldName));
    if(Result) then
    begin
      if Assigned(OnRenamed) then
        OnRenamed(Self, SourceName, FOldName);
      FDescription := WideFormat(strfsReverted, [FName, WideExtractFileName(FOldName)]);
      FStatus := rsOk;
      FName := WideExtractFileName(FOldName);
      FFolder := WideExtractFilePath(FOldName);
      FOldName := '';
    end
    else
    begin
      if FileOrDirExists(FOldName) then
        FDescription := WideFormat(strfsRenError, [SourceName, FOldName, strfsAlreadyExist])
      else
      begin
        LE := GetLastError();
        FDescription := WideFormat(strfsRenError, [SourceName, FOldName, SysErrorMessage(LE)]);
      end;
      FStatus := rsError;
    end;
  end
  else
  begin
    FDescription := WideFormat(strfsNoOldName, [FName]);
    FStatus := rsNotRenamed;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenFile.SetError(const ADescription: WideString; const AStatus: TRenState);
begin
  FDescription := ADescription;
  FStatus := AStatus;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenFiles.Add(const APath: WideString; const ANode: Pointer): TRenFile;
begin
  Result := TRenFile.Create(APath, ANode);
  Result.OnRenamed := FileRenamed;
  inherited Add(Result);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenFile.GetDates: Boolean;
begin
  if FDates = -2 then
  begin
    if GetFileDates(FilePath, FCreatedDate, FModifiedDate, FAccessDate) then
      FDates := 0
    else
      FDates := -1;
  end;
  Result := FDates = 0;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenFile.GetAccessDate: TDateTime;
begin
  if GetDates then
    Result := FAccessDate
  else
    Result := MinDouble;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenFile.GetCreatedDate: TDateTime;
begin
  if GetDates then
    Result := FCreatedDate
  else
    Result := MinDouble;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenFile.GetModifiedDate: TDateTime;
begin
  if GetDates then
    Result := FModifiedDate
  else
    Result := MinDouble;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenFile.GetSize: Int64;
begin
  if FSize = -2 then
    try
      FSize := GetFileSize(FilePath)
    except
      FSize := -1;
    end;
  Result := FSize;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenFile.InitBeforeJob;
begin
  FPreview := '';
  FCopyDone := False;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenFile.CheckPathForUpdate(const SourceName, DestName: WideString);
begin
  if WideSameText(Copy(FFolder, 1, Length(SourceName)), SourceName) then
    FFolder := Tnt_WideStringReplace(FFolder, SourceName, DestName, [rfIgnoreCase]);
  if WideSameText(Copy(WideExtractFilePath(FOldName), 1, Length(SourceName)), SourceName) then
    FOldName := Tnt_WideStringReplace(FOldName, SourceName, DestName, [rfIgnoreCase]);
 end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function RenFilesSort(Item1, Item2: Pointer): Integer;
begin
  Result := TRenFile(Item1).Position - TRenFile(Item2).Position;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenFiles.Sort;
begin
  inherited Sort(RenFilesSort);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenFiles.FileRenamed(Sender: TObject; const SourceName, DestName: WideString);
var
  RenFile: TRenFile;
  i: Integer;
begin
  if Sender is TRenFile then
  begin
    RenFile := TRenFile(Sender);
    if RenFile.IsFolder then
    begin
      for i := 0 to Count-1 do
      begin
        if Items[i] <> RenFile then
          TRenFile(Items[i]).CheckPathForUpdate(IncludeTrailingPathDelimiter(SourceName), IncludeTrailingPathDelimiter(DestName));
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

end.
