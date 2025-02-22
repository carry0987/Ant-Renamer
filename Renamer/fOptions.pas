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

unit fOptions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Buttons,

  TntDialogs, TntSysUtils, TntStdCtrls,
  AntCorelButton, AntAutoHintLabel, AntJvGroupHeader,
  AntJvExControls, AntJvEdit, AntJvSpin,

  frameLanguage, frameAddFolders, base;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

type
  TOptionsForm = class(TBaseDlg)
    PageControl1: TPageControl;
    tshDisplay: TTabSheet;
    tshAdding: TTabSheet;
    tshProcessing: TTabSheet;
    tshLanguage: TTabSheet;
    LanguageFrame: TLanguageFrame;
    cbxForceDir: TTntCheckBox;
    cbxDetectAbsdir: TTntCheckBox;
    cbxCopy: TTntCheckBox;
    cbxDragdropNoAsk: TTntCheckBox;
    DragdropOptions: TAddFoldersFrame;
    lblForceFont: TLabel;
    lblDropdownMax: TLabel;
    cbxRealTimeUpdate: TTntCheckBox;
    cbxResizeColsFiles: TTntCheckBox;
    cbxFilesIcons: TTntCheckBox;
    cbxForceFont: TTntCheckBox;
    cmbForceFont: TComboBox;
    lblIconSet: TLabel;
    cmbIconSet: TComboBox;
    cbxGenerLog: TTntCheckBox;
    cbxLaunchFile: TTntCheckBox;
    cbxDropdownComplete: TTntCheckBox;
    lbhLists: TAntJvGroupHeader;
    lbhFonts: TAntJvGroupHeader;
    lbhDropdown: TAntJvGroupHeader;
    lbhToolbars: TAntJvGroupHeader;
    lbhDragdrop: TAntJvGroupHeader;
    lbhBehaviour: TAntJvGroupHeader;
    lbhRenaming: TAntJvGroupHeader;
    edtDropdownMax: TAntJvSpinEdit;
    tshEvents: TTabSheet;
    lbhWhenStarted: TAntJvGroupHeader;
    cbxStartSwitch: TTntCheckBox;
    cmbStartSwitch: TComboBox;
    cbxStartClearLog: TTntCheckBox;
    lbhWhenFinished: TAntJvGroupHeader;
    cbxFinishSwitch: TTntCheckBox;
    cbxClearFilesList: TTntCheckBox;
    cbxClearBatchList: TTntCheckBox;
    cbxSaveLog: TTntCheckBox;
    cbxSaveLogAppend: TTntCheckBox;
    edtSaveLog: TTntEdit;
    btnSaveLog: TCorelButton;
    cmbFinishSwitch: TComboBox;
    lbhFoldersRules: TAntJvGroupHeader;
    cbxFolderExt: TTntCheckBox;
    cbxDSTRelativeToFile: TTntCheckBox;
    edtForceFont: TAntJvSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbxStartSwitchClick(Sender: TObject);
    procedure cbxFinishSwitchClick(Sender: TObject);
    procedure cbxForceFontClick(Sender: TObject);
    procedure cbxDragdropNoAskClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure cbxSaveLogClick(Sender: TObject);
    procedure btnSaveLogClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
  protected
    procedure LoadOptions; override;
    procedure SaveOptions; override;
    procedure StoreOptions;
  public
    class function Execute(const ShowOnlyLanguages: Boolean = False): Boolean;
  end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

//var
//  OptionsForm: TOptionsForm;

implementation

{$R *.dfm}

uses
  ProgramSettings, ConstValues, Global, functions_sys, VarMessages;

{-------------------------------------------------------------------------------
  Form events
-------------------------------------------------------------------------------}

procedure TOptionsForm.FormCreate(Sender: TObject);
var
  SearchRecord: TSearchRecW;
  RemaindFiles: integer;
begin
  with LanguageFrame.lstLanguages do
  begin
    Height := LanguageFrame.lblVersion.Top - 4;
    Width := LanguageFrame.Width;
  end;
  with cmbIconSet do
  begin
    WideSetCurrentDir(strDirApp + strDirToolbars);
    RemaindFiles := WideFindFirst('*.bmp', 0, SearchRecord);
    while RemaindFiles = 0 do
    begin
      Items.Add(ChangeFileExt(SearchRecord.Name, ''));
      RemaindFiles := WideFindNext(SearchRecord);
    end;
  end;
  WideFindClose(SearchRecord);
  cbxForceFont.Enabled := IsWindowsNT;
  if IsWindowsNT then
    cmbForceFont.Items.AddStrings(Screen.Fonts);
  inherited;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TOptionsForm.btn3Click(Sender: TObject);
begin
  StoreOptions;
  ModalResult := mrOk;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TOptionsForm.FormShow(Sender: TObject);
begin
  inherited;
  edtDropdownMax.Left := lblDropdownMax.Width + lblDropdownMax.Left + 12;
end;

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

procedure TOptionsForm.LoadOptions;
begin
  with Settings.Root do
  begin
{
    with Forms.Options do
    begin
      Self.Width := Width;
      Self.Height := Height;
    end;
}    
    with Options do
    begin
//      cbxOfficeXP.Checked := Display.ToolbarOfficeXP;
      cmbIconSet.ItemIndex := cmbIconSet.Items.IndexOf(Display.ToolbarIconSet);
      if cmbIconSet.ItemIndex = -1 then
        cmbIconSet.ItemIndex := 0;
//      cmbColorType.ItemIndex := Display.ToolbarColorType;
      cbxRealTimeUpdate.Checked := Display.RealtimeUpdate;
      cbxResizeColsFiles.Checked := Display.ResizeColsFiles;
      cbxFilesIcons.Checked := Display.ShowFilesIcons;
      if cbxForceFont.Enabled then
      begin
        cbxForceFont.Checked := Display.ForceFont;
        with cmbForceFont do
        begin
          ItemIndex := Items.IndexOf(Display.FontName);
          if ItemIndex = -1 then
            ItemIndex := 0;
        end;
        edtForceFont.Value := Display.FontHeight;
      end;
      cbxForceFontClick(cbxForceFont);
      edtDropdownMax.Value := Display.DropdownMax;
      cbxDropdownComplete.Checked := Display.DropdownComplete;
      cbxDragdropNoAsk.Checked := Display.DragDropNoAsk;
      cbxDragdropNoAskClick(cbxDragdropNoAsk);
      with Display.DragDropOptions do
        DragdropOptions.SetOptionsBool(AddFiles, AddFolders, Recursive);
      cbxLaunchFile.Checked := Display.LaunchFile;
      cbxForceDir.Checked := Processing.ForceDir;
      cbxDetectAbsdir.Checked := Processing.DetectAbsdir;
      cbxFolderExt.Checked := Processing.FolderExt;
      cbxCopy.Checked := Processing.Copy;
      cbxGenerLog.Checked := Processing.GenerLog;
      cbxDSTRelativeToFile.Checked := Processing.DSTRelativeToFile;
      cbxStartSwitch.Checked := Processing.StartedSwitch <> -1;
      cmbStartSwitch.ItemIndex := Processing.StartedSwitch;
      cbxStartClearLog.Checked := Processing.StartedClearLog;
      cbxFinishSwitch.Checked := Processing.FinishedSwitch <> -1;
      cmbFinishSwitch.ItemIndex := Processing.FinishedSwitch;
      cbxClearFilesList.Checked := Processing.FinishedClearFiles;
      cbxClearBatchList.Checked := Processing.FinishedClearBatch;
      cbxSaveLog.Checked := Processing.SaveLog;
      cbxSaveLogClick(cbxSaveLog);
      cbxSaveLogAppend.Checked := Processing.SaveLogAppend;
      edtSaveLog.Text := UTF8Decode(Processing.SaveLogFile);
    end;
  end;
  cbxStartSwitchClick(cbxStartSwitch);
  cbxFinishSwitchClick(cbxFinishSwitch);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TOptionsForm.SaveOptions;
begin
{
  with Settings.Root.Forms.Options do
  begin
    Width := Self.Width;
    Height := Self.Height;
  end;
}  
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TOptionsForm.StoreOptions;
var
  CheckOptions: TAddFoldersOptions;
begin
  with Settings.Root.Options do
  begin
    if LanguageFrame.LanguagesLoaded then
      Settings.Root.Options.Language := LanguageFrame.SelectedLanguageFile;
    if Self.Tag = 0 then
    begin
//      Display.ToolbarOfficeXP := cbxOfficeXP.Checked;
      if cmbIconSet.ItemIndex < 1 then
        Display.ToolbarIconSet := ''
      else
        Display.ToolbarIconSet := cmbIconSet.Text;
//      Display.ToolbarColorType := cmbColorType.ItemIndex;
      Display.RealtimeUpdate := cbxRealTimeUpdate.Checked;
      Display.ResizeColsFiles := cbxResizeColsFiles.Checked;
      Display.ShowFilesIcons := cbxFilesIcons.Checked;
      if cbxForceFont.Enabled then
      begin
        Display.ForceFont := cbxForceFont.Checked;
        if cmbForceFont.ItemIndex > -1 then
          Display.FontName := cmbForceFont.Items[cmbForceFont.ItemIndex];
        Display.FontHeight := edtForceFont.AsInteger;
      end;
      Display.DropdownMax := edtDropdownMax.AsInteger;
      Display.DropdownComplete := cbxDropdownComplete.Checked;
      Display.DragDropNoAsk := cbxDragdropNoAsk.Checked;
      if cbxDragdropNoAsk.Checked then
      begin
        CheckOptions := DragdropOptions.Options;
        with Display.DragDropOptions do
        begin
          AddFiles := afoFiles in CheckOptions;
          AddFolders := afoFolders in CheckOptions;
          Recursive := afoRecursive in CheckOptions;
        end;
      end;
      Display.LaunchFile := cbxLaunchFile.Checked;
      Processing.ForceDir := cbxForceDir.Checked;
      Processing.DetectAbsdir := cbxDetectAbsdir.Checked;
      Processing.FolderExt := cbxFolderExt.Checked;
      Processing.Copy := cbxCopy.Checked;
      Processing.GenerLog := cbxGenerLog.Checked;
      Processing.DSTRelativeToFile := cbxDSTRelativeToFile.Checked;
      if cbxStartSwitch.Checked then
        Processing.StartedSwitch := cmbStartSwitch.ItemIndex
      else
        Processing.StartedSwitch := -1;
      Processing.StartedClearLog := cbxStartClearLog.Checked;
      if cbxFinishSwitch.Checked then
        Processing.FinishedSwitch := cmbFinishSwitch.ItemIndex
      else
        Processing.FinishedSwitch := -1;
      Processing.FinishedClearFiles := cbxClearFilesList.Checked;
      Processing.FinishedClearBatch := cbxClearBatchList.Checked;
      Processing.SaveLog := cbxSaveLog.Checked;
      Processing.SaveLogAppend := cbxSaveLogAppend.Checked;
      Processing.SaveLogFile := UTF8Encode(edtSaveLog.Text);
    end;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TOptionsForm.cbxStartSwitchClick(Sender: TObject);
begin
  cmbStartSwitch.Enabled := cbxStartSwitch.Checked;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TOptionsForm.cbxFinishSwitchClick(Sender: TObject);
begin
  cmbFinishSwitch.Enabled := cbxFinishSwitch.Checked;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TOptionsForm.cbxForceFontClick(Sender: TObject);
begin
  cmbForceFont.Enabled := cbxForceFont.Checked;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TOptionsForm.cbxDragdropNoAskClick(Sender: TObject);
begin
  DragdropOptions.Enabled := cbxDragdropNoAsk.Checked;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TOptionsForm.cbxSaveLogClick(Sender: TObject);
begin
  edtSaveLog.Enabled := cbxSaveLog.Checked;
  btnSaveLog.Enabled := cbxSaveLog.Checked;
  cbxSaveLogAppend.Enabled := cbxSaveLog.Checked;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TOptionsForm.PageControl1Change(Sender: TObject);
begin
  if (PageControl1.ActivePage = tshLanguage) then
  begin
//    LanguageFrame.Refresh;
    if (not LanguageFrame.LanguagesLoaded) then
    begin
      LanguageFrame.LoadLanguages(lfXML, strDirApp + strDirLanguages, '*.lng');
      LanguageFrame.SelectedLanguageFile := Settings.Root.Options.Language;
    end;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

class function TOptionsForm.Execute(const ShowOnlyLanguages: Boolean = False): Boolean;
begin
  with TOptionsForm.Create(Application) do
    try
      if ShowOnlyLanguages then
      begin
        PageControl1.ActivePage := tshLanguage;
        tshDisplay.TabVisible := False;
        tshAdding.TabVisible := False;
        tshProcessing.TabVisible := False;
        tshEvents.TabVisible := False;
        btn1.Visible := False;
        LanguageFrame.LoadLanguages(lfXML, strDirApp + strDirLanguages, '*.lng');
        LanguageFrame.lstLanguages.OnDblClick := btn3.OnClick;
        Tag := 1;
      end
      else
      begin
        ToolbarImagesHot.GetIcon(tbiOptions, Icon);
        PageControl1.ActivePage := tshDisplay;
        Tag := 0;
      end;
      Result := ShowModal = mrOk;
    finally
      Release;
    end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TOptionsForm.btn1Click(Sender: TObject);
begin
  LaunchHelp;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TOptionsForm.btnSaveLogClick(Sender: TObject);
begin
  with TTntSaveDialog.Create(Self) do
    try
      FileName := WideExtractFileName(edtSaveLog.Text);
      InitialDir := WideExtractFilePath(edtSaveLog.Text);
      if not WideDirectoryExists(InitialDir) then
        InitialDir := '';
      Options := DialogSaveOptions;
      Filter := Format('%s (*.log, *.txt)|*.log;*.txt', [strfltTextFiles]);
      DefaultExt := 'log';
      if Execute then
        edtSaveLog.Text := FileName;
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TOptionsForm.FormActivate(Sender: TObject);
begin
  inherited;
//  LanguageFrame.Refresh;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

end.
