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

unit FileManager;

interface

uses
  Classes, SysUtils, Dialogs;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

type

  TFileEvent = procedure (Sender: TObject; OldFileName: TFileName; NewFileName: TFileName; var Result: Boolean) of object;
  TChangeEvent = procedure (Sender: TObject; AFileName: TFileName) of object;
  TBoolEvent = procedure (Sender: TObject; State: Boolean) of object;

  TFileManager = class(TObject)
  private
    FMessageSaveQuery: string;
    FCurrentFile: TFileName;
    FModified: Boolean;
    FAskToSave: Boolean;
    FOpenDialog: TOpenDialog;
    FSaveDialog: TSaveDialog;
    FOnBeforeNewFile: TFileEvent;
    FOnNewFile: TChangeEvent;
    FOnBeforeOpenFile: TFileEvent;
    FOnOpenFile: TChangeEvent;
    FOnBeforeSaveFile: TFileEvent;
    FOnSaveFile: TChangeEvent;
    FOnCloseFile: TNotifyEvent;
    FOnFileChange: TChangeEvent;
    FOnFileModified: TBoolEvent;
    function CheckUnsavedFile: Boolean;
    procedure SetModified(State: Boolean);
  protected
  public
    property MessageSaveQuery: string read FMessageSaveQuery write FMessageSaveQuery ;
    property CurrentFile: TFileName read FCurrentFile;
    property Modified: Boolean read FModified write SetModified;
    property AskToSave: Boolean read FAskToSave write FAskToSave;
    property OpenDialog: TOpenDialog read FOpenDialog;
    property SaveDialog: TSaveDialog read FSaveDialog;
    property OnBeforeNewFile: TFileEvent read FOnBeforeNewFile write FOnBeforeNewFile;
    property OnNewFile: TChangeEvent read FOnNewFile write FOnNewFile;
    property OnBeforeOpenFile: TFileEvent read FOnBeforeOpenFile write FOnBeforeOpenFile;
    property OnOpenFile: TChangeEvent read FOnOpenFile write FOnOpenFile;
    property OnBeforeSaveFile: TFileEvent read FOnBeforeSaveFile write FOnBeforeSaveFile;
    property OnSaveFile: TChangeEvent read FOnSaveFile write FOnSaveFile;
    property OnCloseFile: TNotifyEvent read FOnCloseFile write FOnCloseFile;
    property OnFileChange: TChangeEvent read FOnFileChange write FOnFileChange;
    property OnFileModified: TBoolEvent read FOnFileModified write FOnFileModified;
    function New: Boolean;
    function Open: Boolean; overload;
    function Open(AFileName: TFileName): Boolean; overload;
    function Save: Boolean;
    function SaveAs: Boolean;
    function Close: Boolean;
    constructor Create(AOwner: TComponent = nil);
    destructor Destroy; override;
  end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

implementation

uses
  StrUtils, Controls,

  Global;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

constructor TFileManager.Create(AOwner: TComponent = nil);
begin
  FMessageSaveQuery := 'The file has been modified. Do you want to save it now ?';
  FCurrentFile := '';
  FModified := False;
  FAskToSave := True;
  FOpenDialog := TOpenDialog.Create(AOwner);
  FSaveDialog := TSaveDialog.Create(AOwner);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

destructor TFileManager.Destroy;
begin
  FOpenDialog.Free;
  FSaveDialog.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TFileManager.New: Boolean;
begin
  Result := CheckUnsavedFile;
  if Result then
  begin
    if Assigned(FOnBeforeNewFile) then
      FOnBeforeNewFile(Self, FCurrentFile, '', Result);
    if not Result then
      Exit;
    FCurrentFile := '';
    Modified := False;
    if Assigned(FOnNewFile) then
      FOnNewFile(Self, FCurrentFile);
    if Assigned(FOnFileChange) then
      FOnFileChange(Self, FCurrentFile);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TFileManager.Open: Boolean;
begin
  Result := False;
  with FOpenDialog do
  begin
    FileName := '';
    if Execute then
      Result := Open(FileName);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TFileManager.Open(AFileName: TFileName): Boolean;
begin
  Result := False;
  if AFileName = '' then
    Result := Open;
  if CheckUnsavedFile then
  begin
    Result := True;
    if Assigned(FOnBeforeOpenFile) then
      FOnBeforeOpenFile(Self, FCurrentFile, AFileName, Result);
    if not Result then
      Exit;
    FCurrentFile := AFileName;
    Modified := False;
    if Assigned(FOnOpenFile) then
      FOnOpenFile(Self, FCurrentFile);
    if Assigned(FOnFileChange) then
      FOnFileChange(Self, FCurrentFile);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TFileManager.Save: Boolean;
begin
  if FCurrentFile = '' then
    Result := SaveAs
  else
  begin
    Result := True;
    if Assigned(FOnBeforeSaveFile) then
      FOnBeforeSaveFile(Self, FCurrentFile, FCurrentFile, Result);
    if not Result then
      Exit;
    Modified := False;
    if Assigned(FOnSaveFile) then
      FOnSaveFile(Self, FCurrentFile);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TFileManager.SaveAs: Boolean;
begin
  Result := False;
  with FSaveDialog do
  begin
    FileName := ExtractFileName(FCurrentFile);
    if Execute then
    begin
      Result := True;
      if Assigned(FOnBeforeSaveFile) then
        FOnBeforeSaveFile(Self, FCurrentFile, FileName, Result);
      if not Result then
        Exit;
      FCurrentFile := FileName;
      Modified := False;
      if Assigned(FOnSaveFile) then
        FOnSaveFile(Self, FCurrentFile);
      if Assigned(FOnFileChange) then
        FOnFileChange(Self, FCurrentFile);
    end;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TFileManager.Close: Boolean;
begin
  Result := CheckUnsavedFile;
  if Result then
  begin
    FModified := False;
    FCurrentFile := '';
    if Assigned(FOnFileChange) then
      FOnFileChange(Self, FCurrentFile);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TFileManager.CheckUnsavedFile: Boolean;
begin
  Result := True;
  if Modified and FAskToSave then
    case MessageWin.Execute(FMessageSaveQuery + IfThen(FCurrentFile <> '', sLineBreak + sLineBreak + FCurrentFile), mtConfirmation, [mbYes, mbNo, mbCancel]) of
      1:
        Result := Save;
      0,3:
        Result := False;
    end;
  if Result and Assigned(FOnCloseFile) then
    FOnCloseFile(Self);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TFileManager.SetModified(State: Boolean);
begin
  if State <> FModified then
  begin
    FModified := State;
    if Assigned(FOnFileModified) then
      FOnFileModified(Self, State);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

end.
