(************************************************************************
 *                                                                      *
 *   Ant Renamer 2.x                                                    *
 *   (C) 2005-2006 Antoine Potten                                       *
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

unit fSelectDirectory;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ImgList,

  VirtualTrees, TntClasses, AntAutoHintLabel, AntCorelButton,

  base, frameAddFolders;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

type
  TSelectDirectoryForm = class(TBaseDlg)
    Options: TAddFoldersFrame;
    Tree: TVirtualStringTree;
    imglstIcons: TImageList;
    procedure btn3Click(Sender: TObject);
    procedure TreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure FormCreate(Sender: TObject);
    procedure TreeGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure TreeExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var Allowed: Boolean);
    procedure btn4Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btn1Click(Sender: TObject);
  private
    FSelectedFolders: TTntStrings;
    FNetworkFolder: WideString;
    procedure SetSelectedFolder(const Value: WideString);
    function GetSelectedFolder: WideString;
    procedure CreateTree;
    procedure AddContents(ParentNode: PVirtualNode; const Path: WideString);
    function HasContents(const Path: WideString): Boolean;
  protected
    procedure LoadOptions; override;
    procedure SaveOptions; override;
  public
    property SelectedFolders: TTntStrings read FSelectedFolders;
    property SelectedFolder: WideString read GetSelectedFolder write SetSelectedFolder;
  end;

  TFolderNode = record
    FullPath: WideString;
    Caption: WideString;
    ImageIndex: Integer;
    IsDrive: Boolean;
    IsRemovableDrive: Boolean;
  end;
  PFolderNode = ^TFolderNode;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

implementation

{$R *.dfm}

uses
  CommCtrl, ShlObj, ShellAPI,
  TntSysUtils, TntFileCtrl,
  functions_files, functions_sys, Global, ProgramSettings;

var
  FolderIcon: Integer;
  DesktopFolder, DocumentsFolder: WideString;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TSelectDirectoryForm.btn3Click(Sender: TObject);
var
  CheckOptions: TAddFoldersOptions;
  CurNode: PVirtualNode;
  NodeData: PFolderNode;
begin
  CheckOptions := Options.Options;
  with Settings.Root.Forms.SelectDirectoryOptions do
  begin
    AddFiles := afoFiles in CheckOptions;
    AddFolders := afoFolders in CheckOptions;
    Recursive := afoRecursive in CheckOptions;
  end;
  FSelectedFolders.Clear;
  CurNode := Tree.GetFirstSelected;
  while CurNode <> nil do
  begin
    NodeData := Tree.GetNodeData(CurNode);
    FSelectedFolders.Add(NodeData.FullPath);
    CurNode := Tree.GetNextSelected(CurNode);
  end;
  ModalResult := mrOk;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TSelectDirectoryForm.GetSelectedFolder: WideString;
begin
  if FSelectedFolders.Count > 0 then
    Result := FSelectedFolders[0];
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TSelectDirectoryForm.SetSelectedFolder(const Value: WideString);
var
  Cur, Cap, Rem: WideString;
  CurNode, LastNode: PVirtualNode;
  CurData: PFolderNode;
begin
  FNetworkFolder := '';
  Tree.ClearSelection;
  if Value = '' then
    Exit;
  if Pos('\\', Value) = 1 then
  begin
    FNetworkFolder := Value;
    Exit;
  end;
  Screen.Cursor := crHourGlass;
  try
    if Pos(DesktopFolder, Value) = 1 then
    begin
      Rem := Copy(Value, Length(DesktopFolder) + 2, Length(Value)) + '\';
      Cur := DesktopFolder;
    end
    else
    if Pos(DocumentsFolder, Value) = 1 then
    begin
      Rem := Copy(Value, Length(DocumentsFolder) + 2, Length(Value)) + '\';
      Cur := DocumentsFolder;
    end
    else
    begin
      Rem := Value + '\';
      Cur := Copy(Rem, 1, Pos('\', Rem) - 1);
    end;
    CurNode := nil;
    LastNode := nil;
    while (Cur <> '') and (Cur <> '\') do
    begin
      if CurNode = nil then
        CurNode := Tree.GetFirst
      else
        CurNode := Tree.GetFirstChild(CurNode);
      Rem := Copy(Rem, Length(Cur) + 2, Length(Rem));
      while (CurNode <> nil) do
      begin
        CurData := Tree.GetNodeData(CurNode);
        Cap := CurData.Caption;
        if CurData.IsDrive then
          Cap := Copy(Cap, 1, Pos(':', Cap));
        if Cap = Cur then
          Break
        else
          CurNode := Tree.GetNext(CurNode);
      end;
      if CurNode = nil then
        Break;
      Tree.Expanded[CurNode] := True;
      LastNode := CurNode;
      Cur := Copy(Rem, 1, Pos('\', Rem) - 1);
    end;
    if LastNode <> nil then
    begin
      Tree.Selected[LastNode] := True;
      Tree.FocusedNode := LastNode;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TSelectDirectoryForm.TreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  btn3.Enabled := (Tree.SelectedCount > 0);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TSelectDirectoryForm.FormCreate(Sender: TObject);
begin
  inherited;
  FSelectedFolders := TTntStringList.Create;
  FNetworkFolder := '';
  if IsWindowsXP then
    imglstIcons.Handle := ImageList_Create(16, 16, ILC_COLOR32 or ILC_MASK, 0, 4);
  Tree.NodeDataSize := SizeOf(TFolderNode);
  Screen.Cursor := crHourGlass;
  try
    CreateTree;
  finally
    Screen.Cursor := crDefault;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TSelectDirectoryForm.LoadOptions;
var
  FontName: string;
begin
  with Settings.Root do
  begin
    with Forms do
    begin
      Self.Width := SelectDirectory.Width;
      Self.Height := SelectDirectory.Height;
      with SelectDirectoryOptions do
        Self.Options.SetOptionsBool(AddFiles, AddFolders, Recursive);
    end;
    with Options do
    begin
      if IsWindowsNT then
      begin
        if (Display.ForceFont) and (Screen.Fonts.IndexOf(Display.FontName) > -1) then
          FontName := Display.FontName
        else
          FontName := 'MS Shell Dlg 2';
        Tree.Font.Name := FontName;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TSelectDirectoryForm.SaveOptions;
begin
  with Settings.Root do
  begin
    with Forms.SelectDirectory do
    begin
      Width := Self.Width;
      Height := Self.Height;
    end;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TSelectDirectoryForm.CreateTree;
var
  NewData: PFolderNode;
  NewNode: PVirtualNode;
  NewIcon: TIcon;
  Drives, Mask: DWord;
  CurDrive: Char;
  CurDriveLabel: string;
begin
  Tree.Clear;
  imglstIcons.Clear;
  NewIcon := GetDirectoryIcon('', False);
  if NewIcon <> nil then
    FolderIcon := imglstIcons.AddIcon(NewIcon)
  else
    FolderIcon := -1;
  NewIcon.Free;
  // Desktop
  NewNode := Tree.AddChild(nil);
  NewData := Tree.GetNodeData(NewNode);
  DesktopFolder := GetShellPath(CSIDL_DESKTOPDIRECTORY);
  NewData.FullPath := DesktopFolder;
  NewData.Caption := GetShellCaption(CSIDL_DESKTOP);
  NewData.IsDrive := False;
  NewIcon := GetShellIcon(CSIDL_DESKTOP, False);
  if NewIcon <> nil then
    NewData.ImageIndex := imglstIcons.AddIcon(NewIcon)
  else
    NewData.ImageIndex := -1;
  NewIcon.Free;
  if HasContents(DesktopFolder) then
    Tree.AddChild(NewNode);
  // My documents
  NewNode := Tree.AddChild(nil);
  NewData := Tree.GetNodeData(NewNode);
  DocumentsFolder := GetShellPath(CSIDL_PERSONAL);
  NewData.FullPath := DocumentsFolder;
  NewData.Caption := GetShellCaption(CSIDL_PERSONAL);
  NewData.IsDrive := False;
  NewIcon := GetShellIcon(CSIDL_PERSONAL, False);
  if NewIcon <> nil then
    NewData.ImageIndex := imglstIcons.AddIcon(NewIcon)
  else
    NewData.ImageIndex := -1;
  NewIcon.Free;
  if HasContents(DocumentsFolder) then
    Tree.AddChild(NewNode);
  // drives
  Drives := GetLogicalDrives;
  CurDrive := 'A';
  Mask := 1;
  while Mask <> 0 do
  begin
    if Drives and Mask <> 0 then
    begin
      NewNode := Tree.AddChild(nil);
      NewData := Tree.GetNodeData(NewNode);
      NewData.FullPath := CurDrive + ':';
      NewData.Caption := NewData.FullPath;
      NewData.IsDrive := True;
      NewData.IsRemovableDrive := GetDriveType(PChar(CurDrive + ':\')) in [DRIVE_REMOVABLE, DRIVE_REMOTE, DRIVE_CDROM];
      if not NewData.IsRemovableDrive then
      begin
        CurDriveLabel := GetVolumeLabel(CurDrive);
        if CurDriveLabel <> '' then
          NewData.Caption := Format('%s [%s]', [NewData.FullPath, CurDriveLabel]);
      end;
      NewIcon := GetDirectoryIcon(NewData.FullPath + '\', False);
      if NewIcon <> nil then
        NewData.ImageIndex := imglstIcons.AddIcon(NewIcon)
      else
        NewData.ImageIndex := -1;
      NewIcon.Free;
      if NewData.IsRemovableDrive or HasContents(NewData.FullPath) then
        Tree.AddChild(NewNode);
    end;
    Inc(CurDrive);
    Mask := Mask shl 1;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TSelectDirectoryForm.TreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
begin
  ImageIndex := PFolderNode(Sender.GetNodeData(Node)).ImageIndex;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TSelectDirectoryForm.TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
begin
  CellText := PFolderNode(Sender.GetNodeData(Node)).Caption;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TSelectDirectoryForm.AddContents(ParentNode: PVirtualNode; const Path: WideString);
var
  SearchRec: TSearchRecW;
  Code: Integer;
  NewNode: PVirtualNode;
  NewData: PFolderNode;
  Contents: TTntStringList;
begin
  Tree.DeleteChildren(ParentNode, True);
  Contents := TTntStringList.Create;
  try
    Contents.CaseSensitive := False;
    Contents.Sorted := True;
    Contents.BeginUpdate;
    try
      Code := WideFindFirst(Path + '\*.*', faAnyFile, SearchRec);
      while Code = 0 do
      begin
        if (SearchRec.Attr and faDirectory <> 0) and (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
          Contents.Add(SearchRec.Name);
        Code := WideFindNext(SearchRec);
      end;
      WideFindClose(SearchRec);
    finally
      Contents.EndUpdate;
    end;
    for Code := 0 to Contents.Count-1 do
    begin
      NewNode := Tree.AddChild(ParentNode);
      NewData := Tree.GetNodeData(NewNode);
      NewData.ImageIndex := FolderIcon;
      NewData.Caption := Contents[Code];
      NewData.FullPath := WideFormat('%s\%s', [Path, NewData.Caption]);
      NewData.IsDrive := False;
      if HasContents(NewData.FullPath) then
        Tree.AddChild(NewNode);
    end;
  finally
    Contents.Free;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TSelectDirectoryForm.HasContents(const Path: WideString): Boolean;
var
  SearchRec: TSearchRecW;
  Code: Integer;
begin
  Result := False;
  Code := WideFindFirst(Path + '\*.*', faAnyFile, SearchRec);
  while (Code = 0) and (not Result) do
  begin
    if (SearchRec.Attr and faDirectory <> 0) and (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      Result := True;
    Code := WideFindNext(SearchRec);
  end;
  WideFindClose(SearchRec);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TSelectDirectoryForm.TreeExpanding(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var Allowed: Boolean);
var
  NodeData: PFolderNode;
begin
  if Self.Visible then
    Screen.Cursor := crHourglass;
  try
    if Node.Parent <> nil then
    begin
      NodeData := Sender.GetNodeData(Node);
      AddContents(Node, NodeData.FullPath);
    end;
  finally
    if Self.Visible then
      Screen.Cursor := crDefault;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TSelectDirectoryForm.btn4Click(Sender: TObject);
begin
  if WideSelectDirectory('', '::{208D2C60-3AEA-1069-A2D7-08002B30309D}', FNetworkFolder) then
  begin
    FSelectedFolders.Clear;
    FSelectedFolders.Add(FNetworkFolder);
    ModalResult := mrOk;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TSelectDirectoryForm.FormActivate(Sender: TObject);
begin
  OnActivate := nil;
  Tree.SetFocus;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TSelectDirectoryForm.btn1Click(Sender: TObject);
begin
  LaunchHelp;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

end.
