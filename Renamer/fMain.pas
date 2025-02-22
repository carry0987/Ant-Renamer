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

unit fMain;

interface

uses
  //Delphi
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, ImgList, ComCtrls, ToolWin, ExtCtrls,
  Menus, XPMan, ActiveX, Grids,

  //3rd
  TB2MRU, TB2Item, TB2Dock, TB2Toolbar,
  SpTBXItem, SpTBXControls, SpTBXMDIMRU,
  VirtualTrees, VTHeaderPopup,
  TntClasses, TntComCtrls, TntStdCtrls, TntGrids,

  //Perso
  AntStringList, AntJvTranslator, AntJvLinkLabel, AntJvDragDrop,
  AntCorelButton, AntJvSpecialProgress, AntJvExControls, AntJvEdit, AntJvSpin,
  Global, Files, Actions, FileManager, thread;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

type
  TMainForm = class(TForm)
    ActionActionGo: TAction;
    ActionBatchAddAction: TAction;
    ActionActionStop: TAction;
    ActionActionUndo: TAction;
    ActionAddFiles: TAction;
    ActionAddFolders: TAction;
    ActionAlwaysOnTop: TAction;
    ActionBatchNoRecent: TAction;
    ActionBatchOpen: TAction;
    ActionBatchClear: TAction;
    ActionBatchRemoveSel: TAction;
    ActionBatchSaveAs: TAction;
    ActionExit: TAction;
    ActionFromListClear: TAction;
    ActionFromListOpen: TAction;
    ActionFromListSave: TAction;
    ActionHelpAbout: TAction;
    ActionHelpContents: TAction;
    ActionItemsSelectAll: TAction;
    ActionItemsSelectInvert: TAction;
    ActionItemsSelectNone: TAction;
    ActionItemsUnsort: TAction;
    ActionList1: TActionList;
    ActionLogClear: TAction;
    ActionLogCopy: TAction;
    ActionLogSave: TAction;
    ActionMoveBottom: TAction;
    ActionMoveDown: TAction;
    ActionMoveTop: TAction;
    ActionMoveUp: TAction;
    ActionOptions: TAction;
    ActionPreview: TAction;
    ActionRemoveAll: TAction;
    ActionRemoveDead: TAction;
    ActionRemoveSelected: TAction;
    ActionTabActions: TAction;
    ActionTabFiles: TAction;
    ActionTabLog: TAction;
    cbxCaseIncludeExt: TTntCheckBox;
    cbxCaseLocale: TTntCheckBox;
    cbxChangeExtNoRepl: TTntCheckBox;
    cbxDTSuffix: TTntCheckBox;
    cbxFromListExt: TTntCheckBox;
    cbxMP3TwoDigit: TTntCheckBox;
    cbxStrReplAll: TTntCheckBox;
    cbxStrReplCase: TTntCheckBox;
    cbxStrReplExt: TTntCheckBox;
    edtCaseAfter: TTntComboBox;
    edtChangeExt: TTntComboBox;
    edtCharDelStr: TTntComboBox;
    edtDTMask: TTntComboBox;
    edtEnum: TTntComboBox;
    edtFromList: TTntMemo;
    edtMP3Mask: TTntComboBox;
    edtRandomMask: TTntComboBox;
    edtStrInsStr: TTntComboBox;
    edtStrReplBy: TTntComboBox;
    edtStrReplSearch: TTntComboBox;
    grpBatch: TGroupBox;
    grpPreview: TGroupBox;
    imglstFiles: TImageList;
    imglstHot: TImageList;
    lblChangeExt: TLabel;
    lblChangeExtNotes: TAntJvLinkLabel;
    lblCharDel: TLabel;
    lblCharDelNotes: TAntJvLinkLabel;
    lblDTMask: TLabel;
    lblDTNotes: TLabel;
    lblDTNotes2: TMemo;
    lblEnumDigits: TLabel;
    lblEnumIncr: TLabel;
    lblEnumMask: TLabel;
    lblEnumNotes: TAntJvLinkLabel;
    lblEnumStart: TLabel;
    lblFromListNotes: TAntJvLinkLabel;
    lblMP3Mask: TLabel;
    lblMP3Notes: TAntJvLinkLabel;
    lblPreviewNew: TLabel;
    lblPreviewNewName: TTntLabel;
    lblPreviewOld: TLabel;
    lblPreviewOldName: TTntLabel;
    lblRandomMask: TLabel;
    lblRandomMethod: TLabel;
    lblRandomNotes: TAntJvLinkLabel;
    lblStrInsNotes: TAntJvLinkLabel;
    lblStrInsPos: TLabel;
    lblStrInsStr: TLabel;
    lblStrReplBy: TLabel;
    lblStrReplSearch: TLabel;
    lstActions: TListBox;
    lstBatch: TVirtualStringTree;
    lstFiles: TVirtualStringTree;
    Panel1: TPanel;
    pgctrlAction: TPageControl;
    pgctrlMain: TPageControl;
    pmActions: TSpTBXPopupMenu;
    pmBatch: TSpTBXPopupMenu;
    pmFiles: TSpTBXPopupMenu;
    pmhFiles: TVTHeaderPopupMenu;
    pmLog: TSpTBXPopupMenu;
    pnlCharDelPos: TPanel;
    ProgressBar1: TAntJvSpecialProgress;
    rbtCaseFirst: TTntRadioButton;
    rbtCaseLower: TTntRadioButton;
    rbtCaseUpper: TTntRadioButton;
    rbtCaseWords: TTntRadioButton;
    rbtCharDelBegin: TTntRadioButton;
    rbtCharDelEnd: TTntRadioButton;
    rbtCharDelPos: TTntRadioButton;
    rbtCharDelStr: TTntRadioButton;
    rbtRandomGUID: TTntRadioButton;
    rbtRandomNumber: TTntRadioButton;
    rbtRandomTick: TTntRadioButton;
    rbtStrInsBegin: TTntRadioButton;
    rbtStrInsEnd: TTntRadioButton;
    splBatch: TSplitter;
    Splitter1: TSplitter;
    StatusBar1: TSpTBXStatusBar;
    tbActions: TSpTBXToolbar;
    tbbAbout: TSpTBXItem;
    tbbAddToBatch: TSpTBXItem;
    tbbAlwaysOnTop: TSpTBXItem;
    tbbBatchNoRecent: TSpTBXItem;
    tbbBatchOpen: TSpTBXSubmenuItem;
    tbbBatchRemoveAll: TSpTBXItem;
    tbbBatchRemoveSel: TSpTBXItem;
    tbbBatchSave: TSpTBXItem;
    tbbExit: TSpTBXItem;
    tbbFilesAdd: TSpTBXItem;
    tbbFilesFolder: TSpTBXItem;
    tbbFilesPreview: TSpTBXItem;
    tbbFilesRemoveAll: TSpTBXItem;
    tbbFilesRemoveDead: TSpTBXItem;
    tbbFilesRemoveSel: TSpTBXItem;
    tbbGo: TSpTBXItem;
    tbbLog: TSpTBXItem;
    tbbLogClear: TSpTBXItem;
    tbbLogCopy: TSpTBXItem;
    tbbLogSave: TSpTBXItem;
    tbbMoveBottom: TSpTBXItem;
    tbbMoveDown: TSpTBXItem;
    tbbMoveTop: TSpTBXItem;
    tbbMoveUp: TSpTBXItem;
    tbbOptions: TSpTBXItem;
    tbbTabActions: TSpTBXItem;
    tbbTabFiles: TSpTBXItem;
    tbdMain: TSpTBXDock;
    tbFiles: TSpTBXToolbar;
    tbmBatchRemoveAll: TSpTBXItem;
    tbmFileSelectNone: TSpTBXItem;
    tbbHelp: TSpTBXItem;
    tbbStop: TSpTBXItem;
    tbmBatchAdd: TSpTBXItem;
    tbmFileSelectInvert: TSpTBXItem;
    tbmFilePreview: TSpTBXItem;
    tbmFileRemoveSel: TSpTBXItem;
    tbmBatchRemoveSel: TSpTBXItem;
    tbmFileRemoveAll: TSpTBXItem;
    tbmLogClear: TSpTBXItem;
    tbmFileSelectAll: TSpTBXItem;
    tbLog: TSpTBXToolbar;
    tbMove: TSpTBXToolbar;
    tbProgram: TSpTBXToolbar;
    tbsBatchFile: TSpTBXSeparatorItem;
    tbsHelp: TSpTBXSeparatorItem;
    tbsExit: TSpTBXSeparatorItem;
    tbmFileSep: TSpTBXSeparatorItem;
    tbsFilesPreview: TSpTBXSeparatorItem;
    tbsFilesRemove: TSpTBXSeparatorItem;
    tbsProgram: TSpTBXSeparatorItem;
    tbTabs: TSpTBXToolbar;
    tbdBottom: TSpTBXDock;
    tbmLogCopy: TSpTBXItem;
    tshAction: TTabSheet;
    tshCase: TTabSheet;
    tshChangeExt: TTabSheet;
    tshCharDel: TTabSheet;
    tshDateTime: TTabSheet;
    tshEmpty: TTabSheet;
    tshEnum: TTabSheet;
    tshFiles: TTabSheet;
    tshFromList: TTabSheet;
    tshLog: TTabSheet;
    tshMP3Tag: TTabSheet;
    tshRandom: TTabSheet;
    tshStringInsert: TTabSheet;
    tshStringRepl: TTabSheet;
    VarMessages: TAntJvTranslatorStrings;
    XPManifest1: TXPManifest;
    tshMultstrRepl: TTabSheet;
    edtMultstrRepl: TTntStringGrid;
    lblMultstrSearch: TLabel;
    lblMultstrReplBy: TLabel;
    lblMultstrReplSet: TLabel;
    edtMultstrReplSet: TTntComboBox;
    btnFromListClear: TCorelButton;
    btnFromListOpen: TCorelButton;
    btnFromListSave: TCorelButton;
    btnMultstrReplSave: TCorelButton;
    btnMultstrReplDelete: TCorelButton;
    ActionMultistrReplSave: TAction;
    ActionMultistrReplClear: TAction;
    ActionMultistrReplDelete: TAction;
    btnMultstrReplClear: TCorelButton;
    cbxMultstrReplExt: TTntCheckBox;
    lblMultstrReplNotes: TAntJvLinkLabel;
    lstLog: TVirtualStringTree;
    tbbUndo: TSpTBXItem;
    cbxStrReplOnlyExt: TTntCheckBox;
    cbxStrInsExt: TTntCheckBox;
    cbxCharDelExt: TTntCheckBox;
    cbxCaseOnlyExt: TTntCheckBox;
    cbxFromListOnlyExt: TTntCheckBox;
    tshMoveString: TTabSheet;
    lblMoveStrFrom: TLabel;
    lblMoveStrCount: TLabel;
    lblMoveStrTo: TLabel;
    pnlMoveStrFrom: TPanel;
    rbtMoveStrFromBegin: TTntRadioButton;
    rbtMoveStrFromEnd: TTntRadioButton;
    pnlMoveStrTo: TPanel;
    rbtMoveStrToBegin: TTntRadioButton;
    rbtMoveStrToEnd: TTntRadioButton;
    lblMoveStrNotes: TAntJvLinkLabel;
    rbtDTWhichCreation: TTntRadioButton;
    lblDTWhich: TLabel;
    rbtDTWhichModif: TTntRadioButton;
    cbxEnumRestart: TTntCheckBox;
    cbxPreview: TSpTBXCheckBox;
    cbxBatch: TSpTBXCheckBox;
    ActionLogFilter: TAction;
    ActionLogFilterOk: TAction;
    ActionLogFilterError: TAction;
    ActionLogFilterNot: TAction;
    tbbLogFilter: TSpTBXSubmenuItem;
    tbsLogFilterNot: TSpTBXItem;
    tbsLogFilterError: TSpTBXItem;
    tbbLogFilterOk: TSpTBXItem;
    tbsLogFilter: TSpTBXSeparatorItem;
    rbtCharDelStr2: TTntRadioButton;
    tshRegexp: TTabSheet;
    lblRegexp: TLabel;
    edtRegexp: TTntComboBox;
    lblRegexpRepl: TLabel;
    edtRegexpRepl: TTntComboBox;
    tshExif: TTabSheet;
    lblExifMask: TLabel;
    edtExifMask: TTntComboBox;
    lblExifNotes: TAntJvLinkLabel;
    lblRegexpNotes: TAntJvLinkLabel;
    edtStrInsPos: TAntJvSpinEdit;
    edtMoveStrFrom: TAntJvSpinEdit;
    edtMoveStrCount: TAntJvSpinEdit;
    edtMoveStrTo: TAntJvSpinEdit;
    edtCharDel: TAntJvSpinEdit;
    edtCharDelPos: TAntJvSpinEdit;
    edtEnumStart: TAntJvSpinEdit;
    edtEnumDigits: TAntJvSpinEdit;
    edtEnumIncr: TAntJvSpinEdit;
    StatusBarPanel1: TSpTBXLabelItem;
    StatusBarPanel2: TSpTBXLabelItem;
    StatusBarProgress: TTBControlItem;
    mruBatch: TSpTBXMRUListItem;
    cbxMultstrCase: TTntCheckBox;
    edtDTOffset: TAntJvSpinEdit;
    lblDTOffset: TLabel;
    lblDTOffsetSec: TLabel;
    lblExifOffset: TLabel;
    edtExifOffset: TAntJvSpinEdit;
    lblExifOffsetSec: TLabel;
    btnMultstrReplPaste: TCorelButton;
    ActionMultistrReplCopy: TAction;
    ActionMultistrReplPaste: TAction;
    btnMultstrReplCopy: TCorelButton;
    procedure FormCreate(Sender: TObject);
    procedure ActionHelpAboutExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionOptionsExecute(Sender: TObject);
    procedure ActionAddFilesExecute(Sender: TObject);
    procedure ActionAddFoldersExecute(Sender: TObject);
    procedure ActionMoveUpExecute(Sender: TObject);
    procedure ActionMoveDownExecute(Sender: TObject);
    procedure ActionMoveTopExecute(Sender: TObject);
    procedure ActionMoveBottomExecute(Sender: TObject);
    procedure ActionRemoveSelectedExecute(Sender: TObject);
    procedure ActionRemoveAllExecute(Sender: TObject);
    procedure ActionRemoveDeadExecute(Sender: TObject);
    procedure ActionActionGoExecute(Sender: TObject);
    procedure ActionActionStopExecute(Sender: TObject);
    procedure ActionTabFilesExecute(Sender: TObject);
    procedure ActionBatchAddActionExecute(Sender: TObject);
    procedure ActionBatchOpenExecute(Sender: TObject);
    procedure ActionBatchSaveAsExecute(Sender: TObject);
    procedure ActionLogClearExecute(Sender: TObject);
    procedure ActionAlwaysOnTopExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lstFilesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lstFilesKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure lstActionsClick(Sender: TObject);
    procedure lblChangeExtNotesLinkClick(Sender: TObject; LinkNumber: Integer; LinkText: String);
    procedure lblCharDelNotesLinkClick(Sender: TObject; LinkNumber: Integer; LinkText: String);
    procedure lstFilesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure lstBatchGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure lstBatchResize(Sender: TObject);
    procedure ActionLogCopyExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ActionBatchRemoveSelExecute(Sender: TObject);
    procedure ActionBatchClearExecute(Sender: TObject);
    procedure ActionList1Execute(Action: TBasicAction;
      var Handled: Boolean);
    procedure ActionFromListClearExecute(Sender: TObject);
    procedure ActionFromListOpenExecute(Sender: TObject);
    procedure ActionFromListSaveExecute(Sender: TObject);
    procedure lstActionsContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure ActionItemsSelectAllExecute(Sender: TObject);
    procedure ActionItemsSelectInvertExecute(Sender: TObject);
    procedure UpdateActionPreview(Sender: TObject);
    procedure lstFilesCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure lstFilesHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure lstFilesGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure tbbBatchOpenPopup(Sender: TTBCustomItem; FromLink: Boolean);
    procedure lstFilesChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure pmhFilesPopup(Sender: TObject);
    procedure ActionItemsUnsortExecute(Sender: TObject);
    procedure cbxBatchClick(Sender: TObject);
    procedure splBatchCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure lstFilesBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure lstFilesDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
      var Effect: Integer; var Accept: Boolean);
    procedure lstFilesDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure edtMultstrReplSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: WideString);
    procedure ActionMultistrReplClearExecute(Sender: TObject);
    procedure ActionMultistrReplSaveExecute(Sender: TObject);
    procedure ActionMultistrReplDeleteExecute(Sender: TObject);
    procedure edtMultstrReplKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtMultstrReplSetClick(Sender: TObject);
    procedure lstLogGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure lstLogPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure lstLogEnter(Sender: TObject);
    procedure lstFilesDblClick(Sender: TObject);
    procedure lblEnumNotesLinkClick(Sender: TObject; LinkNumber: Integer;
      LinkText: String);
    procedure lblMP3NotesLinkClick(Sender: TObject; LinkNumber: Integer;
      LinkText: String);
    procedure lblRandomNotesLinkClick(Sender: TObject; LinkNumber: Integer;
      LinkText: String);
    procedure ActionHelpContentsExecute(Sender: TObject);
    procedure lblStrInsNotesLinkClick(Sender: TObject; LinkNumber: Integer;
      LinkText: String);
    procedure lblMoveStrNotesLinkClick(Sender: TObject;
      LinkNumber: Integer; LinkText: String);
    procedure ActionLogFilterExecute(Sender: TObject);
    procedure ActionLogFilterChange(Sender: TObject);
    procedure lblExifNotesLinkClick(Sender: TObject;
      LinkNumber: Integer; LinkText: String);
    procedure lblRegexpNotesLinkClick(Sender: TObject; LinkNumber: Integer;
      LinkText: String);
    procedure mruBatchClick(Sender: TObject; const Filename: WideString);
    procedure ActionMultistrReplCopyExecute(Sender: TObject);
    procedure ActionMultistrReplPasteExecute(Sender: TObject);
  private
    lstFilesExt: TStringList;
    FFilesList: TRenFiles;
    FActionsList: TRenActions;
    FClearBatch: Boolean;
    FCurrentLanguage: string;
    procedure LoadOptions;
    procedure SaveOptions;
    procedure ApplyOptions(const Startup: Boolean);
    procedure SetToolbarIcons;
    procedure SetMessageStrings;
    procedure AddFile(const AFileName: WideString; ACounter: integer = -1);
    procedure AddFiles(AList: TTntStrings; Options: TAddFoldersOptions);
    procedure AddFolder(const AFolder: WideString; Options: TAddFoldersOptions);
    procedure SetStatus; overload;
    procedure SetStatus(APercent: Integer); overload;
    procedure SetStatus(ACaption: string); overload;
    procedure SetStatus(APercent: Integer; ACaption: string); overload;
    procedure UpdateHistoryList(Combo: TComboBox); overload;
    procedure UpdateHistoryList(Combo: TTntComboBox); overload;
    procedure ThreadTerminated(Sender: TObject);
    procedure RemoveListItemsSelected(const TargetList: TVirtualStringTree);
    procedure RemoveListItemsAll(const TargetList: TVirtualStringTree);
    procedure RemoveListItemsDead(const TargetList: TVirtualStringTree);
    procedure RenumberFiles;
    procedure RenumberActions;
    function  GetCurrentList: TVirtualStringTree;
    procedure SetActionData(const AAction: TRenAction; const UpdateHistory: Boolean);
    procedure FilesDrop(Sender: TObject; Pos: TPoint; Value: TTntStringList); overload;
  public
    procedure AddToLog(const ADescription: WideString; const AStatus: TRenState);
    procedure Refresh(Sender: TRenFile; BatchPosition, FilePosition: Integer);
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure OnException(Sender: TObject; E: Exception);
    function OnAppHelp(Command: Word; Data: Longint; var CallHelp: Boolean): Boolean;
  end;

var
  MainForm: TMainForm;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

implementation

uses
  //Delphi
  StrUtils, Clipbrd, stdActns, commctrl, Math, DateUtils,
  //3rd
  TntDialogs, TntSysUtils, TntSystem, TntClipbrd, VirtualTreeUtils,
  //Themes
  TBXDefaultTheme, TBXOfficeXPTheme,
  //Forms
  fAbout, fOptions, fSelectDirectory, fDragdropOptions,
  //Perso
  ProgramSettings, ConstValues, Log, SettingsBase, VarMessages,
  functions_sys, functions_files, functions_str, functions_xml, functions_gui;

{$R *.dfm}

const
  COL_FOLDER    = 0;
  COL_FILENAME  = 1;
  COL_FULLPATH  = 2;
  COL_EXT       = 3;
  COL_PREVIEW   = 4;
  COL_SIZE      = 5;
  COL_CREATED   = 6;
  COL_MODIFIED  = 7;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function GridToString(const Grid: TTntStringGrid): WideString;
var
  i: Integer;
  List: TTntStringList;
begin
  List := TTntStringList.Create;
  try
    for i := 0 to Grid.RowCount-2 do
      List.Add(Grid.Cells[0, i] + #9 + Grid.Cells[1, i]);
    Result := List.Text;
  finally
    List.Free;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure StringToGrid(const Grid: TTntStringGrid; const AString: WideString);
var
  i, tab: Integer;
  List: TTntStringList;
  ws: WideString;
begin
  List := TTntStringList.Create;
  try
    List.Text := AString;
    Grid.RowCount := List.Count + 1;
    for i := 0 to List.Count-1 do
    begin
      ws := List[i];
      tab := Pos(#9, ws);
      if tab = 0 then
        tab := Length(ws) + 1;
      Grid.Cells[0, i] := Copy(ws, 1, tab - 1);
      Grid.Cells[1, i] := Copy(ws, tab + 1, Length(ws));
    end;
    Grid.Cells[0, Grid.RowCount-1] := '';
    Grid.Cells[1, Grid.RowCount-1] := '';
  finally
    List.Free;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.OnException(Sender: TObject; E: Exception);
begin
  with TStringList.Create do
    try
      Add('Unhandled exception !');
      Add('');
      Add('Sender: ' + Sender.ClassName);
      Add('Exception: ' + E.ClassName);
      Add('Message: ' + E.Message);
      if E is EOSError then
        with E as EOSError do
          Add(Format('Win32 error code %d: %s', [ErrorCode, SysErrorMessage(ErrorCode)]));
      Add('');
      Add('(Press Ctrl+C if you want to copy these information to clipboard)');
      MessageWin.Execute(Text, mtWarning, [mbOk]);
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
  Form events
-------------------------------------------------------------------------------}

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
  ico: TIcon;
begin
  Font.Name := Graphics.DefFontData.Name;
  Font.Charset := Graphics.DefFontData.Charset;
  if IsWindowsXP then
    imglstFiles.Handle := ImageList_Create(16, 16, ILC_COLOR32 or ILC_MASK, 0, 4);
  with TJvDragDrop.Create(Self) do
  begin
    OnDrop := FilesDrop;
    AcceptDrag := True;
    Parent := Self;
  end;
  lstFiles.NodeDataSize := SizeOf(TFileNode);
  lstBatch.NodeDataSize := SizeOf(TActionNode);
  lstLog.NodeDataSize := SizeOf(TLogNode);
//  lstLog.Columns.Items[0].Width := -2;
  lstFilesExt := TStringList.Create;
  lstFilesExt.Add('*');
  Ico := GetDirectoryIcon('', False);
  imglstFiles.AddIcon(Ico);
  Ico.Free;
  FFilesList := TRenFiles.Create;
  FActionsList := TRenActions.Create;
//  StatusBar1.ControlStyle := ControlStyle + [csAcceptsControls];
//  ProgressBar1.Parent := StatusBar1;
//  ToolbarImagesNormal := imglstNormal;
  ToolbarImagesHot := imglstHot;
  Caption := Format(strCaption, [Copy(strVersion, 1, LastDelimiter('.', strVersion)-1)]);
  SetToolbarIcons;
  SetMessageStrings;
  with pgctrlAction do
    for i := 0 to PageCount-1 do
      Pages[i].TabVisible := False;
  lstActionsClick(Self);
  with pgctrlMain do
    for i := 0 to PageCount-1 do
      Pages[i].TabVisible := False;
  FRenThread := nil;
  FCurrentLanguage := '';
  Font.Name := Graphics.DefFontData.Name;
  PostMessage(edtMultstrReplSet.Handle, CB_SETDROPPEDWIDTH, 300, 0);
  LoadOptions;
  Application.OnHelp := OnAppHelp;
  edtStrInsPos.MaxValue := MAX_PATH;
  edtMoveStrFrom.MaxValue := MAX_PATH;
  edtMoveStrCount.MaxValue := MAX_PATH;
  edtMoveStrTo.MaxValue := MAX_PATH;
  edtCharDel.MaxValue := MAX_PATH;
  edtCharDelPos.MaxValue := MAX_PATH;
  edtEnumDigits.MaxValue := MAX_PATH;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.FormDestroy(Sender: TObject);
begin
//  ActionRemoveAll.Execute;
  FFilesList.Free;
  FActionsList.Free;
  lstFilesExt.Free;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveOptions;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  GlobalStop := True;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.FormShow(Sender: TObject);
begin
  ActionTabFiles.Execute;
  if Settings.Root.Options.Language = '?' then
    TOptionsForm.Execute(True);
  ApplyOptions(True);
end;

{-------------------------------------------------------------------------------
  lstFiles events
-------------------------------------------------------------------------------}

procedure TMainForm.lstFilesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_DELETE) and (Shift = []) then
  begin
    if Sender = lstFiles then
      ActionRemoveSelected.Execute
    else
    if Sender = lstBatch then
      ActionBatchRemoveSel.Execute
    else
    if Sender = lstLog then
      ActionLogClear.Execute;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.lstFilesKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = ord('A')) and (Shift = [ssCtrl]) then
  begin
    if Sender = lstFiles then
      lstFiles.SelectAll(False)
    else
    if Sender = lstBatch then
      lstBatch.SelectAll(False);
  end;
end;

{-------------------------------------------------------------------------------
  Actions
-------------------------------------------------------------------------------}

procedure TMainForm.ActionAddFilesExecute(Sender: TObject);
begin
  with TTntOpenDialog.Create(self) do
    try
      HelpContext := 1210;
      Title := strmsgSelectFiles;
      Options := DialogOpenOptions + [ofDontAddToRecent, ofNoDereferenceLinks, ofAllowMultiSelect, ofShowHelp];
      InitialDir := UTF8Decode(Settings.Root.Folders.AddFiles.Path);
      if not WideDirectoryExists(InitialDir) then
        InitialDir := '';
      Filter := Format('%s (*.*)|*.*', [strfltAllFiles]);
      if Execute then
      begin
        Settings.Root.Folders.AddFiles.Path := UTF8Encode(WideExtractFilePath(FileName));
        AddFiles(Files, []);
      end;
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionAddFoldersExecute(Sender: TObject);
begin
  with TSelectDirectoryForm.Create(Self) do
    try
      ToolbarImagesHot.GetIcon(tbiAddFolder, Icon);
      SelectedFolder := UTF8Decode(Settings.Root.Folders.AddFolder.Path);
      if ShowModal = mrOk then
      begin
        Settings.Root.Folders.AddFolder.Path := UTF8Encode(SelectedFolder);
        AddFiles(SelectedFolders, Options.Options);
      end;
    finally
      Release;
    end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionMoveUpExecute(Sender: TObject);
var
  lst: TVirtualStringTree;
  node, prev: PVirtualNode;
begin
  lst := GetCurrentList;
  if lst <> nil then
  begin
    node := lst.GetFirst;
    while node <> nil do
    begin
      if vsSelected in node.States then
      begin
        prev := lst.GetPrevious(node);
        if (prev <> nil) and not (vsSelected in prev.States) then
          lst.MoveTo(node, prev, amInsertBefore, False);
      end;
      node := lst.GetNext(node);
    end;
    if (lst = lstFiles) and (lstFiles.SelectedCount > 0) then
      ActionItemsUnsort.Execute;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionMoveDownExecute(Sender: TObject);
var
  lst: TVirtualStringTree;
  node, next: PVirtualNode;
begin
  lst := GetCurrentList;
  if lst <> nil then
  begin
    node := lst.GetLast(nil);
    while node <> nil do
    begin
      if vsSelected in node.States then
      begin
        next := lst.GetNext(node);
        if (next <> nil) and not (vsSelected in next.States) then
          lst.MoveTo(node, next, amInsertAfter, False);
      end;
      node := lst.GetPrevious(node);
    end;
    if (lst = lstFiles) and (lstFiles.SelectedCount > 0) then
      ActionItemsUnsort.Execute;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionMoveTopExecute(Sender: TObject);
var
  lst: TVirtualStringTree;
  node, top: PVirtualNode;
begin
  lst := GetCurrentList;
  if lst <> nil then
  begin
    node := lst.GetFirstSelected;
    top := nil;
    while node <> nil do
    begin
      lst.MoveTo(node, top, amInsertAfter, False);
      top := node;
      node := lst.GetNextSelected(node);
    end;
    if (lst = lstFiles) and (lstFiles.SelectedCount > 0) then
      ActionItemsUnsort.Execute;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionMoveBottomExecute(Sender: TObject);
var
  lst: TVirtualStringTree;
  node, first, bottom: PVirtualNode;
begin
  lst := GetCurrentList;
  if lst <> nil then
  begin
    first := nil;
    node := lst.GetFirstSelected;
    bottom := lst.GetLast(nil);
    while (node <> nil) and (node <> first) do
    begin
      if first = nil then
        first := node;
      lst.MoveTo(node, bottom, amInsertAfter, False);
      bottom := node;
      node := lst.GetFirstSelected;
    end;
    if (lst = lstFiles) and (lstFiles.SelectedCount > 0) then
      ActionItemsUnsort.Execute;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.lstFilesDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
begin
  if (Source = Sender) and (Mode <> dmNoWhere) then
    Accept := True;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.lstFilesDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var
  CurNode: PVirtualNode;
  NodesList: TList;
  i: Integer;
begin
  if Source = Sender then
  begin
    NodesList := TList.Create;
    try
      CurNode := Sender.GetFirstSelected;
      while CurNode <> nil do
      begin
        NodesList.Add(CurNode);
        CurNode := Sender.GetNextSelected(CurNode);
      end;
      if Mode in [dmAbove] then
      begin
        for i := 0 to NodesList.Count-1 do
          Sender.MoveTo(PVirtualNode(NodesList.Items[i]), Sender.DropTargetNode, amInsertBefore, False);
        ActionItemsUnsort.Execute;
      end
      else
      if Mode in [dmBelow, dmOnNode] then
      begin
        for i := NodesList.Count-1 downto 0 do
          Sender.MoveTo(PVirtualNode(NodesList.Items[i]), Sender.DropTargetNode, amInsertAfter, False);
        ActionItemsUnsort.Execute;
      end;
    finally
      NodesList.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionItemsSelectAllExecute(Sender: TObject);
var
  lst: TVirtualStringTree;
begin
  lst := GetCurrentList;
  if lst <> nil then
  begin
    if Sender = ActionItemsSelectAll then
      lst.SelectAll(False)
    else
      lst.ClearSelection;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionItemsSelectInvertExecute(Sender: TObject);
var
  CurNode: PVirtualNode;
  lst: TVirtualStringTree;
begin
  lst := GetCurrentList;
  if lst <> nil then
  begin
    CurNode := lst.GetFirst;
    while CurNode <> nil do
    begin
      lst.Selected[CurNode] := not lst.Selected[CurNode];
      CurNode := lst.GetNext(CurNode);
    end;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionRemoveSelectedExecute(Sender: TObject);
begin
  RemoveListItemsSelected(lstFiles);
  SetStatus;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionRemoveAllExecute(Sender: TObject);
begin
  RemoveListItemsAll(lstFiles);
  FFilesList.Clear;
  SetStatus;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionRemoveDeadExecute(Sender: TObject);
begin
  RemoveListItemsDead(lstFiles);
  SetStatus;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionBatchRemoveSelExecute(Sender: TObject);
begin
  RemoveListItemsSelected(lstBatch);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionBatchClearExecute(Sender: TObject);
begin
  RemoveListItemsAll(lstBatch);
  FActionsList.Clear;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.RemoveListItemsSelected(const TargetList: TVirtualStringTree);
var
  PrevNode, CurNode: PVirtualNode;
  ItemData: Pointer;
begin
  with TargetList do
  begin
    BeginUpdate;
    try
      CurNode := GetFirstSelected;
      PrevNode := nil;
      while CurNode <> nil do
      begin
        ItemData := GetNodeData(CurNode);
        PrevNode := CurNode;
        CurNode := GetNextSelected(CurNode);
        if TargetList = lstFiles then
          FFilesList.Remove(PFileNode(ItemData)^.RenFile)
        else
        if TargetList = lstBatch then
          FActionsList.Remove(PActionNode(ItemData)^.RenAction);
      end;
      CurNode := GetNext(PrevNode);
      DeleteSelectedNodes;
      if (CurNode = nil) and (PrevNode <> nil) then
        CurNode := GetLast(nil);
      Selected[CurNode] := True;
      FocusedNode := CurNode;
    finally
      EndUpdate;
    end;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.RemoveListItemsAll(const TargetList: TVirtualStringTree);
begin
  TargetList.Clear;
//  TargetList.SelectAll(False);
//  RemoveListItemsSelected(TargetList);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.RemoveListItemsDead(const TargetList: TVirtualStringTree);
var
  CurNode, PrevNode: PVirtualNode;
  TheData: PFileNode;
begin
  if TargetList <> lstFiles then
    raise Exception.Create('TMainForm.RemoveListItemsDead : Cannot call this function with something else than lstFiles');
  with TargetList do
  begin
    BeginUpdate;
    try
      CurNode := GetFirst;
      while CurNode <> nil do
      begin
        TheData := GetNodeData(CurNode);
        PrevNode := CurNode;
        CurNode := GetNext(CurNode);
        if not TheData^.RenFile.Exists then
        begin
          FFilesList.Remove(TheData^.RenFile);
          DeleteNode(PrevNode);
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionActionGoExecute(Sender: TObject);
var
  Operation: TRenActionOperation;
  PreviewHintNotAgain: Boolean;
  i: Integer;
begin
  if Sender = ActionActionGo then
    Operation := raoRename
  else
  if Sender = ActionPreview then
    Operation := raoPreview
  else
  if Sender = ActionActionUndo then
    Operation := raoRevert
  else
    raise Exception.Create('Unexpected object for "Sender" in TMainForm.ActionActionGoExecute');
  if (FFilesList.Count = 0) or ((FActionsList.Count = 0) and (lstActions.ItemIndex < 0) and (Operation <> raoRevert)) then
  begin
    MessageWin.Execute(strmsgNothingToDo, mtWarning, [mbOk]);
    ActionTabActions.Execute;
    Exit;
  end;
  if Operation <> raoPreview then
  begin
    with Settings.Root.Options.Processing do
    begin
      case StartedSwitch of
        tbiTabFiles: ActionTabFiles.Execute;
        tbiTabActions: ActionTabActions.Execute;
        tbiTabLog: ActionTabLog.Execute;
      end;
      if StartedClearLog then
        ActionLogClear.Execute;
    end;
  end
  else
  begin
    ActionTabFiles.Execute;
    with lstFiles.Header.Columns.Items[COL_PREVIEW] do
      if not (coVisible in Options) then
      begin
        Options := Options + [coVisible];
        if Settings.Root.Hints.ShowPreview then
        begin
          PreviewHintNotAgain := False;
          MessageWin.Execute(strhintShowPreview, mtInformation, PreviewHintNotAgain, strhintNotAgain, [mbOk]);
          if PreviewHintNotAgain then
            Settings.Root.Hints.ShowPreview := False;
        end;
      end;
  end;
  case Operation of
    raoRename:
      AddToLog(strlogStartRename, rsInfo);
    raoPreview:
      AddToLog(strlogStartPreview, rsInfo);
    raoRevert:
      AddToLog(strlogStartRevert, rsInfo);
  end;
  RenumberFiles;
  RenumberActions;
  ActionActionStop.Enabled := True;
  ActionActionGo.Enabled := False;
  SetStatus(0, strmsgStatusRenaming);
  if (Operation in [raoRename, raoPreview]) and (FActionsList.Count = 0) then
    // if rename or preview and batch list is empty, create a temp item and clear batch at end
  begin
    ActionBatchAddActionExecute(ActionActionGo);
    FClearBatch := True;
  end
  else
    // if batch list is not empty, do not add temp item, and clear only if asked (and if it is a real rename, not a preview)
    FClearBatch := (Operation in [raoRename]) and (Settings.Root.Options.Processing.FinishedClearBatch);
  // Update the undo list with all the actions that will be performed
  { *** todo: also add empty undo items to files  to synchronize their list with this one, or something like that
              or use a number to match actions<>oldnames
  }
(*
  if Operation = raoRename then
    with lstUndo.Strings do
    begin
      BeginUpdate;
      try
        for i := 0 to FActionsList.Count-1 do
        begin
          Insert(0, TRenAction(FActionsList.Items[i]).Description);
        end;
      finally
        EndUpdate;
      end;
    end;
*)
  if Operation = raoRename then
  begin
    for i := 0 to FActionsList.Count-1 do
      AddToLog(WideFormat('%d. %s', [i + 1, TRenAction(FActionsList.Items[i]).Description]), rsInfo);
  end;
  FRenThread := TRenThread.Create(True);
  FRenThread.FActionsList := FActionsList;
  FRenThread.FFilesList := FFilesList;
  FRenThread.FOperation := Operation;
  FRenThread.OnTerminate := ThreadTerminated;
  GlobalStop := False;
  FRenThread.Resume;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.RenumberFiles;
var
  CurNode: PVirtualNode;
  i: Integer;
begin
  i := 0;
  with lstFiles do
  begin
    CurNode := GetFirst;
    while CurNode <> nil do
    begin
      PFileNode(GetNodeData(CurNode))^.RenFile.Position := i;
      Inc(i);
      CurNode := GetNext(CurNode);
    end;
  end;
  FFilesList.Sort;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.RenumberActions;
var
  CurNode: PVirtualNode;
  i: Integer;
begin
  i := 0;
  with lstBatch do
  begin
    CurNode := GetFirst;
    while CurNode <> nil do
    begin
      PActionNode(GetNodeData(CurNode))^.RenAction.Position := i;
      Inc(i);
      CurNode := GetNext(CurNode);
    end;
  end;
  FActionsList.Sort;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ThreadTerminated(Sender: TObject);
var
  CurNode: PVirtualNode;
begin
  if GlobalStop then
    AddToLog(strlogStoppedByUser, rsInfo);
  case FRenThread.FOperation of
    raoRename:
      AddToLog(strlogEndRename, rsInfo);
    raoRevert:
      AddToLog(strlogEndRevert, rsInfo);
    raoPreview:
      begin
        AddToLog(strlogEndPreview, rsInfo);
        with lstFiles.Header do
          if SortColumn = 2 then
            lstFiles.SortTree(SortColumn, SortDirection, False);
      end;
  end;
  if FRenThread.FOperation <> raoPreview then
    with Settings.Root.Options.Processing do
      case FinishedSwitch of
        tbiTabFiles: ActionTabFiles.Execute;
        tbiTabActions: ActionTabActions.Execute;
        tbiTabLog: ActionTabLog.Execute;
      end;
  FRenThread := nil;
  if FClearBatch then
    ActionBatchClear.Execute;
  with Settings.Root.Options.Processing do
  begin
    if FinishedClearFiles then
      ActionRemoveAll.Execute;
    if SaveLog then
      with TLogWriter.Create(UTF8Decode(SaveLogFile), SaveLogAppend) do
      begin
        CurNode := lstLog.GetFirst;
        while CurNode <> nil do
        begin
          Add(lstLog.GetNodeData(CurNode));
          CurNode := lstLog.GetNext(CurNode);
        end;
        Free;
      end;
  end;
//  tbbGo.Action := ActionActionGo;
//  tbbClassicGo.Action := ActionActionGo;
  ActionActionGo.Enabled := True;
  ActionActionStop.Enabled := False;
  SetStatus;
  lstFiles.Invalidate;
  lstLog.Invalidate;
  UpdateActionPreview(ActionActionStop);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.Refresh(Sender: TRenFile; BatchPosition, FilePosition: Integer);
var
  divby: Integer;
begin
  if Sender <> nil then
  begin
    if GlobalSettings.Refresh then
      lstFiles.InvalidateNode(Sender.ListItem);
    if GlobalSettings.Log then
      AddToLog(Sender.Description, Sender.Status);
  end;
  if GlobalSettings.Refresh then
  begin
    divby := FFilesList.Count * FActionsList.Count;
    if divby > 0 then
      SetStatus((((BatchPosition * FFilesList.Count) + FilePosition + 1) * 100) div (divby));
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionActionStopExecute(Sender: TObject);
begin
  GlobalStop := True;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionOptionsExecute(Sender: TObject);
begin
  if TOptionsForm.Execute then
    ApplyOptions(False);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionHelpAboutExecute(Sender: TObject);
begin
  with TAboutForm.Create(self) do
    try
      ToolbarImagesHot.GetIcon(tbiAbout, Icon);
      ShowModal;
    finally
      Release;
    end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionHelpContentsExecute(Sender: TObject);
begin
  LaunchHelp(Self);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionExitExecute(Sender: TObject);
begin
  Close;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionTabFilesExecute(Sender: TObject);
begin
  with Sender as TAction do
  begin
    pgctrlMain.ActivePageIndex := Tag;
    Checked := True
  end;
  tbActions.Visible := Sender = ActionTabActions;
  tbLog.Visible := Sender = ActionTabLog;
  tbMove.Visible := ((Sender = ActionTabActions) and (cbxBatch.Checked)) or (Sender = ActionTabFiles);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionBatchAddActionExecute(Sender: TObject);
var
  NewAction: TRenAction;
  TheData: PActionNode;
begin
  NewAction := FActionsList.Add(TRenActionItem(lstActions.ItemIndex));
  SetActionData(NewAction, True);
  with lstBatch do
    TheData := GetNodeData(AddChild(nil));
  TheData^.RenAction := NewAction;
  if (Sender = ActionBatchAddAction) and (not cbxBatch.Checked) then
  begin
    cbxBatch.Checked := True;
    cbxBatchClick(cbxBatch);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionBatchOpenExecute(Sender: TObject);
begin
  with TTntOpenDialog.Create(self) do
    try
      HelpContext := 1253;
      Options := DialogOpenOptions + [ofShowHelp];
      DefaultExt := 'arb';
      InitialDir := UTF8Decode(Settings.Root.Folders.Batch.Path);
      if not WideDirectoryExists(InitialDir) then
        InitialDir := '';
      Filter := Format('%s (*.arb)|*.arb', [strfltBatchFiles]);
      if Execute then
      begin
        Settings.Root.Folders.Batch.Path := UTF8Encode(WideExtractFilePath(FileName));
        mruBatch.MRUAdd(FileName);
        mruBatchClick(ActionBatchOpen, FileName);
      end;
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.mruBatchClick(Sender: TObject; const Filename: WideString);
var
  i: Integer;
begin
  try
    FActionsList.LoadFromFile(Filename);
  except
    on e: Exception do
    begin
      MessageWin.Execute(e.Message, mtError, [mbOk]);
    end;
  end;
  lstBatch.Clear;
  for i := 0 to FActionsList.Count-1 do
    PActionNode(lstBatch.GetNodeData(lstBatch.AddChild(nil)))^.RenAction := FActionsList.Items[i] as TRenAction;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.tbbBatchOpenPopup(Sender: TTBCustomItem; FromLink: Boolean);
begin
  tbbBatchNoRecent.Visible := mruBatch.Count = 0;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionBatchSaveAsExecute(Sender: TObject);
begin
  with TTntSaveDialog.Create(self) do
    try
      HelpContext := 1253;
      Options := DialogSaveOptions + [ofShowHelp];
      DefaultExt := 'arb';
      InitialDir := UTF8Decode(Settings.Root.Folders.Batch.Path);
      if not WideDirectoryExists(InitialDir) then
        InitialDir := '';
      Filter := Format('%s (*.arb)|*.arb', [strfltBatchFiles]);
      if Execute then
      begin
        Settings.Root.Folders.Batch.Path := UTF8Encode(WideExtractFilePath(FileName));
        mruBatch.MRUAdd(FileName);
        FActionsList.SaveToFile(FileName);
      end;
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionLogClearExecute(Sender: TObject);
begin
  RemoveListItemsAll(lstLog);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionLogCopyExecute(Sender: TObject);
var
  CurNode: PVirtualNode;
begin
  with TTntStringList.Create do
    try
      CurNode := lstLog.GetFirst;
      while CurNode <> nil do
      begin
        Add(PLogNode(lstLog.GetNodeData(CurNode)).Description);
        CurNode := lstLog.GetNext(CurNode);
      end;
      if Sender = ActionLogCopy then
      begin
        TntClipboard.AsWideText := Text;
      end
      else
      if Sender = ActionLogSave then
      begin
        with TTntSaveDialog.Create(Self) do
          try
            Options := DialogSaveOptions;
            Filter := Format('%s (*.log, *.txt)|*.log;*.txt', [strfltTextFiles]);
            DefaultExt := 'log';
            if Execute then
              SaveToFile(FileName);
          finally
            Free;
          end;
      end;
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionLogFilterExecute(Sender: TObject);
begin
//
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionLogFilterChange(Sender: TObject);
var
  CurNode: PVirtualNode;
begin
  lstLog.BeginUpdate;
  try
    CurNode := lstLog.GetFirst;
    while CurNode <> nil do
    begin
      case PLogNode(lstLog.GetNodeData(CurNode))^.Status of
        rsOk:
          if ActionLogFilterOk.Checked then
            Include(CurNode.States, vsVisible)
          else
            Exclude(CurNode.States, vsVisible);
        rsError:
          if ActionLogFilterError.Checked then
            Include(CurNode.States, vsVisible)
          else
            Exclude(CurNode.States, vsVisible);
        rsNotRenamed:
          if ActionLogFilterNot.Checked then
            Include(CurNode.States, vsVisible)
          else
            Exclude(CurNode.States, vsVisible);
      end;
      CurNode := lstLog.GetNext(CurNode);
    end;
  finally
    lstLog.EndUpdate;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionAlwaysOnTopExecute(Sender: TObject);
begin
  ActionAlwaysOnTop.Checked := not ActionAlwaysOnTop.Checked;
  if ActionAlwaysOnTop.Checked then
    SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE)
  else
    SetWindowPos(Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE)
end;

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

procedure TMainForm.LoadOptions;
var
  TempList: TStringList;
  i: Integer;
begin
  with Settings.Root do
  begin
    with Forms.Main do
    begin
      Self.WindowState := TWindowState(State);
      if Self.WindowState = wsNormal then
      begin
        Self.Width := Width;
        Self.Height := Height;
        if (Left <> -1) or (Top <> -1) then
        begin
          Self.Left := Left;
          Self.Top := Top;
        end
        else
        begin
          Self.Left := Screen.Width div 2 - Self.Width div 2;
          Self.Top := Screen.Height div 2 - Self.Height div 2;
        end;
      end;
      ActionAlwaysOnTop.Checked := not OnTop;
    end; // forms.main
    with Forms.MainOptions do
    begin
      if FilesSortOrder then
        lstFiles.Header.SortDirection := sdAscending
      else
        lstFiles.Header.SortDirection := sdDescending;
      lstFiles.Header.SortColumn := FilesSortColumn;
      lstActions.Width := ActionsListWidth;
      cbxBatch.Checked := EnableBatch;
      cbxPreview.Checked := EnableSample;
      cbxBatchClick(cbxBatch);
      ActionLogFilterOk.Checked := LogFilterOk;
      ActionLogFilterError.Checked := LogFilterError;
      ActionLogFilterNot.Checked := LogFilterNot;
    end; // options
    TBCustomLoadPositions(Self, XmlReadInt, XmlReadString, Forms.MainOptions.Toolbars.Node);
    LoadVTColumns(lstFiles.Header, Forms.MainOptions.FilesListColumns.Node);
    with LastValues do
    begin
      TempList := TStringList.Create;
      try
        MRU.Batch.Load(TempList);
        for i := 0 to TempList.Count - 1 do
          mruBatch.MRUAdd(UTF8Decode(TempList[i]));

        edtChangeExt.Text := UTF8Decode(ChangeExt.NewExt);
        ChangeExt.NewExtList.Load(TempList);
        UTF8ListDecode(TempList, edtChangeExt.Items);
        cbxChangeExtNoRepl.Checked := ChangeExt.NoReplace;

        edtStrReplSearch.Text := UTF8Decode(StrRepl.Search);
        StrRepl.SearchList.Load(TempList);
        UTF8ListDecode(TempList, edtStrReplSearch.Items);
        edtStrReplBy.Text := UTF8Decode(StrRepl.Repl);
        StrRepl.ReplList.Load(TempList);
        UTF8ListDecode(TempList, edtStrReplBy.Items);
        cbxStrReplAll.Checked := StrRepl.AllOccurences;
        cbxStrReplCase.Checked := StrRepl.CaseSensitive;
        cbxStrReplExt.Checked := StrRepl.IncludeExt;
        cbxStrReplOnlyExt.Checked := StrRepl.OnlyExt;

        MultstrRepl.Sets.Names(TempList);
        UTF8ListDecode(TempList, edtMultstrReplSet.Items);
        edtMultstrReplSet.Text := UTF8Decode(MultstrRepl.LastSet);
        StringToGrid(edtMultstrRepl, UTF8Decode(MultstrRepl.CurrentList.Value));
        cbxMultstrReplExt.Checked := MultstrRepl.IncludeExt;
        cbxMultstrCase.Checked := MultstrRepl.CaseSensitive;

        edtStrInsStr.Text := UTF8Decode(StrIns.Str);
        StrIns.StrList.Load(TempList);
        UTF8ListDecode(TempList, edtStrInsStr.Items);
        edtStrInsPos.Value := StrIns.Pos;
        if StrIns.FromBegin then
          rbtStrInsBegin.Checked := True
        else
          rbtStrInsEnd.Checked := True;
        cbxStrInsExt.Checked := StrIns.Ext;

        edtMoveStrFrom.Value := MoveStr.FromPos;
        if MoveStr.FromBegin then
          rbtMoveStrFromBegin.Checked := True
        else
          rbtMoveStrFromEnd.Checked := True;
        edtMoveStrCount.Value := MoveStr.Count;
        edtMoveStrTo.Value := MoveStr.ToPos;
        if MoveStr.ToBegin then
          rbtMoveStrToBegin.Checked := True
        else
          rbtMoveStrToEnd.Checked := True;

        edtCharDel.Value := CharDel.Nb;
        case CharDel.AfterString of
          1:  rbtCharDelStr.Checked := True;
          2:  rbtCharDelStr2.Checked := True;
        else
          rbtCharDelPos.Checked := True;
        end;
        edtCharDelPos.Value := CharDel.Pos;
        if CharDel.FromBegin then
          rbtCharDelBegin.Checked := True
        else
          rbtCharDelEnd.Checked := True;
        edtCharDelStr.Text := UTF8Decode(CharDel.Str);
        CharDel.StrList.Load(TempList);
        UTF8ListDecode(TempList, edtCharDelStr.Items);
        cbxCharDelExt.Checked := CharDel.Ext;

        edtEnum.Text := UTF8Decode(Enum.Mask);
        Enum.MaskList.Load(TempList);
        UTF8ListDecode(TempList, edtEnum.Items);
        edtEnumStart.Value := Enum.Start;
        edtEnumDigits.Value := Enum.Digits;
        edtEnumIncr.Value := Enum.Incr;
        cbxEnumRestart.Checked := Enum.Restart;

        edtMP3Mask.Text := UTF8Decode(MP3.Mask);
        MP3.MaskList.Load(TempList);
        UTF8ListDecode(TempList, edtMP3Mask.Items);
        cbxMP3TwoDigit.Checked := MP3.TwoDigit;

        edtDTMask.Text := UTF8Decode(DateTime.Mask);
        DateTime.MaskList.Load(TempList);
        UTF8ListDecode(TempList, edtDTMask.Items);
        cbxDTSuffix.Checked := DateTime.AddSuffix;
        if DateTime.WhichDate = 0 then
          rbtDTWhichCreation.Checked := True
        else
          rbtDTWhichModif.Checked := True;
        edtDTOffset.AsInteger := DateTime.Offset;

        edtRandomMask.Text := UTF8Decode(Random.Mask);
        Random.MaskList.Load(TempList);
        UTF8ListDecode(TempList, edtRandomMask.Items);
        case Random.Method of
          1: rbtRandomTick.Checked := True;
          2: rbtRandomGUID.Checked := True;
        else rbtRandomNumber.Checked := True;
        end;
        case ChangeCase.Option of
          1: rbtCaseFirst.Checked := True;
          2: rbtCaseUpper.Checked := True;
          3: rbtCaseLower.Checked := True;
        else rbtCaseWords.Checked := True;
        end;

        edtCaseAfter.Text := UTF8Decode(ChangeCase.AfterChars);
        ChangeCase.CharsList.Load(TempList);
        UTF8ListDecode(TempList, edtCaseAfter.Items);
        cbxCaseLocale.Checked := ChangeCase.UseLocale;
        cbxCaseIncludeExt.Checked := ChangeCase.IncludeExt;
        cbxCaseOnlyExt.Checked := ChangeCase.OnlyExt;

        edtFromList.Lines.Text := UTF8Decode(FromList.ListContents.Value);
        cbxFromListExt.Checked := FromList.AppendExt;
        cbxFromListOnlyExt.Checked := FromList.OnlyExt;

        edtRegexp.Text := UTF8Decode(Regexp.Expr);
        Regexp.ExprList.Load(TempList);
        UTF8ListDecode(TempList, edtRegexp.Items);
        edtRegexpRepl.Text := Regexp.Repl;
        Regexp.ReplList.Load(TempList);
        UTF8ListDecode(TempList, edtRegexpRepl.Items);

        edtExifMask.Text := UTF8Decode(Exif.Mask);
        Exif.MaskList.Load(TempList);
        UTF8ListDecode(TempList, edtExifMask.Items);
        edtExifOffset.AsInteger := Exif.Offset;
      finally
        TempList.Free;
      end;
    end; // lastvalues
  end; // settings.root
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.SaveOptions;
var
  TempList: TStringList;
  i, MaxItems: Integer;
begin
  with Settings.Root do
  begin
    with Forms.Main do
    begin
      State := Integer(Self.WindowState);
      if Self.WindowState = wsNormal then
      begin
        Width := Self.Width;
        Height := Self.Height;
        Left := Self.Left;
        Top := Self.Top;
      end;
      OnTop := ActionAlwaysOnTop.Checked;
    end; // forms.main
    with Forms.MainOptions do
    begin
      FilesSortOrder := lstFiles.Header.SortDirection = sdAscending;
      FilesSortColumn := lstFiles.Header.SortColumn;
      ActionsListWidth := lstActions.Width;
      if cbxBatch.Checked then
        BatchListHeight := grpBatch.Height;
      EnableBatch := cbxBatch.Checked;
      EnableSample := cbxPreview.Checked;
      LogFilterOk := ActionLogFilterOk.Checked;
      LogFilterError := ActionLogFilterError.Checked;
      LogFilterNot := ActionLogFilterNot.Checked;
    end; // options
    Forms.MainOptions.Toolbars.RecreateNode;
    TBCustomSavePositions(Self, XmlWriteInt, XmlWriteString, Forms.MainOptions.Toolbars.Node);
    Forms.MainOptions.FilesListColumns.RecreateNode;
    SaveVTColumns(lstFiles.Header, Forms.MainOptions.FilesListColumns.Node);

    with LastValues do
    begin
      MaxItems := Options.Display.DropdownMax;
      TempList := TStringList.Create;
      try
        for i := 0 to mruBatch.Count - 1 do
          TempList.Add(UTF8Encode((mruBatch.Items[i] as TSpTBXMRUItem).MRUString));
        MRU.Batch.Save(TempList);

        ChangeExt.NewExt := UTF8Encode(edtChangeExt.Text);
        UTF8ListEncode(edtChangeExt.Items, TempList, MaxItems);
        ChangeExt.NewExtList.Save(TempList);
        ChangeExt.NoReplace := cbxChangeExtNoRepl.Checked;

        StrRepl.Search := UTF8Encode(edtStrReplSearch.Text);
        UTF8ListEncode(edtStrReplSearch.Items, TempList, MaxItems);
        StrRepl.SearchList.Save(TempList);
        StrRepl.Repl := UTF8Encode(edtStrReplBy.Text);
        UTF8ListEncode(edtStrReplBy.Items, TempList, MaxItems);
        StrRepl.ReplList.Save(TempList);
        StrRepl.AllOccurences := cbxStrReplAll.Checked;
        StrRepl.CaseSensitive := cbxStrReplCase.Checked;
        StrRepl.IncludeExt := cbxStrReplExt.Checked;
        StrRepl.OnlyExt := cbxStrReplOnlyExt.Checked;

        MultstrRepl.LastSet := UTF8Encode(edtMultstrReplSet.Text);
        MultstrRepl.CurrentList.Value := UTF8Encode(GridToString(edtMultstrRepl));
        MultstrRepl.IncludeExt := cbxMultstrReplExt.Checked;
        MultstrRepl.CaseSensitive := cbxMultstrCase.Checked;

        StrIns.Str := UTF8Encode(edtStrInsStr.Text);
        UTF8ListEncode(edtStrInsStr.Items, TempList, MaxItems);
        StrIns.StrList.Save(TempList);
        StrIns.Pos := edtStrInsPos.AsInteger;
        StrIns.FromBegin := rbtStrInsBegin.Checked;
        StrIns.Ext := cbxStrInsExt.Checked;

        MoveStr.FromPos := edtMoveStrFrom.AsInteger;
        MoveStr.FromBegin := rbtMoveStrFromBegin.Checked;
        MoveStr.Count := edtMoveStrCount.AsInteger;
        MoveStr.ToPos := edtMoveStrTo.AsInteger;
        MoveStr.ToBegin := rbtMoveStrToBegin.Checked;

        CharDel.Nb := edtCharDel.AsInteger;
        if rbtCharDelStr.Checked then
          CharDel.AfterString := 1
        else
        if rbtCharDelStr2.Checked then
          CharDel.AfterString := 2
        else
          CharDel.AfterString := 0;
        CharDel.Pos := edtCharDelPos.AsInteger;
        CharDel.FromBegin := rbtCharDelBegin.Checked;
        CharDel.Str := UTF8Encode(edtCharDelStr.Text);
        UTF8ListEncode(edtCharDelStr.Items, TempList, MaxItems);
        CharDel.StrList.Save(TempList);
        CharDel.Ext := cbxCharDelExt.Checked;

        Enum.Mask := UTF8Encode(edtEnum.Text);
        UTF8ListEncode(edtEnum.Items, TempList, MaxItems);
        Enum.MaskList.Save(TempList);
        Enum.Start := edtEnumStart.AsInteger;
        Enum.Digits := edtEnumDigits.AsInteger;
        Enum.Incr := edtEnumIncr.AsInteger;
        Enum.Restart := cbxEnumRestart.Checked;

        MP3.Mask := UTF8Encode(edtMP3Mask.Text);
        UTF8ListEncode(edtMP3Mask.Items, TempList, MaxItems);
        MP3.MaskList.Save(TempList);
        MP3.TwoDigit := cbxMP3TwoDigit.Checked;

        DateTime.Mask := UTF8Encode(edtDTMask.Text);
        UTF8ListEncode(edtDTMask.Items, TempList, MaxItems);
        DateTime.MaskList.Save(TempList);
        DateTime.AddSuffix := cbxDTSuffix.Checked;
        if rbtDTWhichCreation.Checked then
          DateTime.WhichDate := 0
        else
          DateTime.WhichDate := 1;
        DateTime.Offset := edtDTOffset.AsInteger;

        Random.Mask := UTF8Encode(edtRandomMask.Text);
        UTF8ListEncode(edtRandomMask.Items, TempList, MaxItems);
        Random.MaskList.Save(TempList);
        if rbtRandomTick.Checked then Random.Method := 1
        else if rbtRandomGUID.Checked then Random.Method := 2
        else Random.Method := 0;

        if rbtCaseFirst.Checked then ChangeCase.Option := 1
        else if rbtCaseUpper.Checked then ChangeCase.Option := 2
        else if rbtCaseLower.Checked then ChangeCase.Option := 3
        else ChangeCase.Option := 0;
        ChangeCase.AfterChars := UTF8Encode(edtCaseAfter.Text);
        UTF8ListEncode(edtCaseAfter.Items, TempList, MaxItems);
        ChangeCase.CharsList.Save(TempList);
        ChangeCase.UseLocale := cbxCaseLocale.Checked;
        ChangeCase.IncludeExt := cbxCaseIncludeExt.Checked;
        ChangeCase.OnlyExt := cbxCaseOnlyExt.Checked;

        FromList.ListContents.Value := UTF8Encode(edtFromList.Lines.Text);
        FromList.AppendExt := cbxFromListExt.Checked;
        FromList.OnlyExt := cbxFromListOnlyExt.Checked;

        Regexp.Expr := UTF8Encode(edtRegexp.Text);
        UTF8ListEncode(edtRegexp.Items, TempList, MaxItems);
        Regexp.ExprList.Save(TempList);
        Regexp.Repl := edtRegexpRepl.Text;
        UTF8ListEncode(edtRegexpRepl.Items, TempList, MaxItems);
        Regexp.ReplList.Save(TempList);

        Exif.Mask := UTF8Encode(edtExifMask.Text);
        UTF8ListEncode(edtExifMask.Items, TempList, MaxItems);
        Exif.MaskList.Save(TempList);
        Exif.Offset := edtExifOffset.AsInteger;
      finally
        TempList.Free;
      end;
    end; // lastvalues
  end; // settings.root
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ApplyOptions(const Startup: Boolean);
var
  bmpTBNormal, bmpTBHot: TBitmap;
  IconSize, FontHeight: Integer;
//  ToolbarColorType: Integer;
//  ToolbarOfficeXP: Boolean;
  FontName: string;
  b: Boolean;
{
  procedure SetImageList(const AList: TCustomImageList);
  begin
    tbProgram.Images := AList;
    tbTabs.Images := AList;
    tbFiles.Images := AList;
    tbActions.Images := AList;
    tbLog.Images := AList;
    tbMove.Images := AList;
    pmFiles.Images := AList;
    pmBatch.Images := AList;
    pmLog.Images := AList;
    pmActions.Images := AList;
  end;
  }
  function UpdateFont(const Font: TFont; const Name: String; const Height: Integer): TFont;
  begin
    Result := Font;
    Result.Name := Name;
    Result.Height := -Height;
  end;
begin
  with Settings.Root.Options do
  begin
//    ToolbarColorType := Display.ToolbarColorType;
//    ToolbarOfficeXP := Display.ToolbarOfficeXP;
    bmpTBNormal := TBitmap.Create;
    bmpTBHot := TBitmap.Create;
    try
      if (Display.ToolbarIconSet = '?') then
        Display.ToolbarIconSet := '';
      if Display.ToolbarIconSet <> '' then
        try
          WideSetCurrentDir(strDirApp + strDirToolbars);
          bmpTBHot.LoadFromFile(Display.ToolbarIconSet + '.bmp');
        except
          bmpTBHot.LoadFromResourceName(HInstance, 'ToolbarScrows');
        end
      else
        bmpTBHot.LoadFromResourceName(HInstance, 'ToolbarScrows');
      bmpTBHot.PixelFormat := pf24bit;
      bmpTBNormal.Assign(bmpTBHot);
      IconSize := bmpTBNormal.Height;
{
      if ToolbarColorType = 0 then
        SetImageList(imglstHot)
      else
      begin
        SetImageList(imglstNormal);
        case ToolbarColorType of
          1: GrayScale(bmpTBNormal);
          2: Blend(bmpTBNormal, clWhite);
          3: Blend(bmpTBNormal, clDkGray);
        end;
        with imglstNormal do
        begin
          Clear;
          Height := IconSize;
          Width := IconSize;
          AddMasked(bmpTBNormal, bmpTBNormal.Canvas.Pixels[0,0]);
        end;
      end;
}
      with imglstHot do
      begin
        Clear;
        Height := IconSize;
        Width := IconSize;
        AddMasked(bmpTBHot, bmpTBHot.Canvas.Pixels[0,0]);
      end;
      //if ToolbarOfficeXP then
    finally
      bmpTBNormal.Free;
      bmpTBHot.Free;
    end; // try
    with lstFiles.Header do
      if Display.ResizeColsFiles then
      begin
        AutoSizeIndex := COL_FILENAME;
        Options := Options + [hoAutoResize];
      end
      else
      begin
        AutoSizeIndex := -1;
        Options := Options - [hoAutoResize];
      end;
    if Display.ShowFilesIcons then
    begin
      lstFiles.Images := imglstFiles;
      lstFiles.OnGetImageIndex := lstFilesGetImageIndex;
    end
    else
    begin
      lstFiles.Images := nil;
      lstFiles.OnGetImageIndex := nil;
    end;
    if IsWindowsNT then
    begin
      FontName := Graphics.DefFontData.Name;
      FontHeight := -Graphics.DefFontData.Height;
      if Display.ForceFont then
      begin
        if Display.FontName = '?' then
        begin
          if Screen.Fonts.IndexOf('Noto Sans') > -1 then
          begin
            FontName := 'Noto Sans';
            Display.FontName := FontName;
          end
          else
          if Screen.Fonts.IndexOf('DejaVu Sans') > -1 then
          begin
            FontName := 'DejaVu Sans';
            Display.FontName := FontName;
          end
          else
          if Screen.Fonts.IndexOf('Arial Unicode MS') > -1 then
          begin
            FontName := 'Arial Unicode MS';
            Display.FontName := FontName;
          end
          else
          begin
            Display.ForceFont := False;
          end;
        end
        else if (Screen.Fonts.IndexOf(Display.FontName) > -1) then
          FontName := Display.FontName;
        if Display.FontHeight > 0 then
          FontHeight := Display.FontHeight;
      end;
      if Display.ForceFont or not Startup then
      begin
        lstFiles.Font := UpdateFont(lstFiles.Font, FontName, FontHeight);
        lstBatch.Font := UpdateFont(lstBatch.Font, FontName, FontHeight);
        lstLog.Font := UpdateFont(lstLog.Font, FontName, FontHeight);
        edtChangeExt.Font := UpdateFont(edtChangeExt.Font, FontName, FontHeight);
        edtStrReplSearch.Font := UpdateFont(edtStrReplSearch.Font, FontName, FontHeight);
        edtStrReplBy.Font := UpdateFont(edtStrReplBy.Font, FontName, FontHeight);
        edtMultstrRepl.Font := UpdateFont(edtMultstrRepl.Font, FontName, FontHeight);
        edtMultstrReplSet.Font := UpdateFont(edtMultstrReplSet.Font, FontName, FontHeight);
        edtStrInsStr.Font := UpdateFont(edtStrInsStr.Font, FontName, FontHeight);
        edtCharDelStr.Font := UpdateFont(edtCharDelStr.Font, FontName, FontHeight);
        edtEnum.Font := UpdateFont(edtEnum.Font, FontName, FontHeight);
        edtMP3Mask.Font := UpdateFont(edtMP3Mask.Font, FontName, FontHeight);
        edtDTMask.Font := UpdateFont(edtDTMask.Font, FontName, FontHeight);
        edtRandomMask.Font := UpdateFont(edtRandomMask.Font, FontName, FontHeight);
        edtCaseAfter.Font := UpdateFont(edtCaseAfter.Font, FontName, FontHeight);
        edtFromList.Font := UpdateFont(edtFromList.Font, FontName, FontHeight);
        edtRegexp.Font := UpdateFont(edtRegexp.Font, FontName, FontHeight);
        edtRegexpRepl.Font := UpdateFont(edtRegexpRepl.Font, FontName, FontHeight);
        edtExifMask.Font := UpdateFont(edtExifMask.Font, FontName, FontHeight);
        lblPreviewOldName.Font := UpdateFont(lblPreviewOldName.Font, FontName, FontHeight);
        lblPreviewNewName.Font := UpdateFont(lblPreviewNewName.Font, FontName, FontHeight);
      end;
    end;
    b := Display.DropdownComplete;
    edtChangeExt.AutoComplete := b;
    edtStrReplSearch.AutoComplete := b;
    edtStrReplBy.AutoComplete := b;
    edtStrInsStr.AutoComplete := b;
    edtCharDelStr.AutoComplete := b;
    edtEnum.AutoComplete := b;
    edtMP3Mask.AutoComplete := b;
    edtDTMask.AutoComplete := b;
    edtRandomMask.AutoComplete := b;
    edtCaseAfter.AutoComplete := b;
    edtRegexp.AutoComplete := b;
    edtRegexpRepl.AutoComplete := b;
    edtExifMask.AutoComplete := b;
    // global settings
    GlobalSettings.ForceDir := Processing.ForceDir;
    GlobalSettings.DetectAbsdir := Processing.DetectAbsdir;
    GlobalSettings.FolderExt := Processing.FolderExt;
    GlobalSettings.Copy := Processing.Copy;
    GlobalSettings.Refresh := Display.RealtimeUpdate;
    GlobalSettings.Log := Processing.GenerLog;
    FileDateTimeDSTRelativeToFile := Processing.DSTRelativeToFile;
    if Language = '?' then
      Language := '';
    if Language <> FCurrentLanguage then
    begin
      FCurrentLanguage := Language;
      if FCurrentLanguage <> '' then
        Translator.Translate(strDirApp + strDirLanguages + FCurrentLanguage);
      Font.Name := Graphics.DefFontData.Name;
      Font.Charset := Graphics.DefFontData.Charset;
    end;
    strHelpFile := WideChangeFileExt(strDirApp + strDirLanguages + FCurrentLanguage, '.chm');
    if (FCurrentLanguage = '') or (not WideFileExists(strHelpFile)) then
      strHelpFile := strDirApp + strDirLanguages + 'English.chm';
  end; // with Settings.Root.Options
  SetStatus;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.SetToolbarIcons;
begin
  ActionTabFiles.ImageIndex := tbiTabFiles;
  ActionTabActions.ImageIndex := tbiTabActions;
  ActionTabLog.ImageIndex := tbiTabLog;
  ActionAddFiles.ImageIndex := tbiAddFiles;
  ActionAddFolders.ImageIndex := tbiAddFolder;
  ActionRemoveSelected.ImageIndex := tbiRemoveSelected;
  ActionRemoveAll.ImageIndex := tbiRemoveAll;
  ActionRemoveDead.ImageIndex := tbiRemoveDead;
  ActionPreview.ImageIndex := tbiPreview;
  ActionMoveUp.ImageIndex := tbiMoveUp;
  ActionMoveDown.ImageIndex := tbiMoveDown;
  ActionMoveTop.ImageIndex := tbiMoveTop;
  ActionMoveBottom.ImageIndex := tbiMoveBottom;
  ActionBatchAddAction.ImageIndex := tbiBatchAddAction;
  ActionBatchRemoveSel.ImageIndex := tbiBatchRemoveSel;
  ActionBatchClear.ImageIndex := tbiBatchClear;
  ActionBatchOpen.ImageIndex := tbiBatchOpen;
  ActionBatchSaveAs.ImageIndex := tbiBatchSave;
  ActionLogClear.ImageIndex := tbiLogClear;
  ActionLogSave.ImageIndex := tbiLogSave;
  ActionLogCopy.ImageIndex := tbiLogCopy;
  ActionLogFilter.ImageIndex := tbiLogFilter;  
  ActionActionGo.ImageIndex := tbiGo;
  ActionActionStop.ImageIndex := tbiStop;
  ActionActionUndo.ImageIndex := tbiUndo;
  ActionAlwaysOnTop.ImageIndex := tbiAlwaysOnTop;
  ActionOptions.ImageIndex := tbiOptions;
  ActionHelpContents.ImageIndex := tbiHelp;
  ActionHelpAbout.ImageIndex := tbiAbout;
  ActionExit.ImageIndex := tbiExit;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.SetMessageStrings;
begin
  with VarMessages do
  begin                     
    Add('strfltAllFiles', strfltAllFiles);
    Add('strfltTextFiles', strfltTextFiles);
    Add('strfltBatchFiles', strfltBatchFiles);
    Add('strmsgSelectFiles', strmsgSelectFiles);
    Add('strmsgSelectFolder', strmsgSelectFolder);
    Add('strmsgStatusAddingFiles', strmsgStatusAddingFiles);
    Add('strmsgStatusRenaming', strmsgStatusRenaming);
    Add('strmsgFilesCount', strmsgFilesCount);
    Add('strmsgBusy', strmsgBusy);
    Add('strmsgNothingToDo', strmsgNothingToDo);
    Add('strlogStartRename', strlogStartRename);
    Add('strlogStartPreview', strlogStartPreview);
    Add('strlogStartRevert', strlogStartRevert);
    Add('strlogEndRename', strlogEndRename);
    Add('strlogEndPreview', strlogEndPreview);
    Add('strlogEndRevert', strlogEndRevert);
    Add('strlogStoppedByUser', strlogStoppedByUser);
    Add('stradUnknown', stradUnknown);
    Add('stradInclExt', stradInclExt);
    Add('stradOnlyExt', stradOnlyExt);
    Add('stradChangeExt', stradChangeExt);
    Add('stradChangeExtRmv', stradChangeExtRmv);
    Add('stradChangeExtNoRepl', stradChangeExtNoRepl);
    Add('stradStringRepl', stradStringRepl);
    Add('stradStringReplAll', stradStringReplAll);
    Add('stradStringReplCase', stradStringReplCase);
    Add('stradMultstrRepl', stradMultstrRepl);
    Add('stradStringIns', stradStringIns);
    Add('stradMoveStr', stradMoveStr);
    Add('stradCharDel', stradCharDel);
    Add('stradCharDelPos', stradCharDelPos);
    Add('stradCharDelStr', stradCharDelStr);
    Add('stradCharDelStr2', stradCharDelStr2);
    Add('stradEnum', stradEnum);
    Add('stradMP3', stradMP3);
    Add('stradMP3TwoDigit', stradMP3TwoDigit);
    Add('stradFromBegin', stradFromBegin);
    Add('stradFromEnd', stradFromEnd);
    Add('stradDateTime', stradDateTime);
    Add('stradDateTimeCreat', stradDateTimeCreat);
    Add('stradDateTimeModif', stradDateTimeModif);
    Add('stradDateTimeSuffix', stradDateTimeSuffix);
    Add('stradDateTimeOffset', stradDateTimeOffset);
    Add('stradRandom', stradRandom);
    Add('stradRandomNumbers', stradRandomNumbers);
    Add('stradRandomTick', stradRandomTick);
    Add('stradRandomGUID', stradRandomGUID);
    Add('stradCase', stradCase);
    Add('stradCaseWords', stradCaseWords);
    Add('stradCaseFirst', stradCaseFirst);
    Add('stradCaseUpper', stradCaseUpper);
    Add('stradCaseLower', stradCaseLower);
    Add('stradCaseLocale', stradCaseLocale);
    Add('stradFromList', stradFromList);
    Add('stradFromListExt', stradFromListExt);
    Add('stradRegexp', stradRegexp);
    Add('stradExif', stradExif);
    //Add('stradShiftDT', stradShiftDT);
    Add('straeFromListEmpty', straeFromListEmpty);
    Add('straeFromListEnd', straeFromListEnd);
    Add('straeExifNoInfo', straeExifNoInfo);
    Add('strfsNotRenamed', strfsNotRenamed);
    Add('strfsRenamed', strfsRenamed);
    Add('strfsReverted', strfsReverted);
    Add('strfsRenError', strfsRenError);
    Add('strfsNoOldName', strfsNoOldName);
    Add('strfs404', strfs404);
    Add('strfsAlreadyExist', strfsAlreadyExist);
    Add('strfeIdentical', strfeIdentical);
    Add('strhintNotAgain', strhintNotAgain);
    Add('strhintShowPreview', strhintShowPreview);
  end;
end;

{-------------------------------------------------------------------------------
  Files
-------------------------------------------------------------------------------}

procedure TMainForm.AddFile(const AFileName: WideString; ACounter: integer = -1);
var
  NewNode: PVirtualNode;
  TheData: PFileNode;
begin
  with lstFiles do
  begin
    NewNode := AddChild(nil);
    TheData := GetNodeData(NewNode);
  end;
  TheData^.RenFile := FFilesList.Add(AFileName, NewNode);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.AddFiles(AList: TTntStrings; Options: TAddFoldersOptions);
var
  i: Integer;
begin
  SetStatus(0, strmsgStatusAddingFiles);
  Screen.Cursor := crHourGlass;
  lstFiles.BeginUpdate;
  try
    for i := 0 to AList.Count-1 do
    begin
      if (Options <> []) and (WideDirectoryExists(AList.Strings[i])) then
      begin
        if afoFolders in Options then
          AddFile(AList.Strings[i]);
        AddFolder(AList.Strings[i], Options);
      end else
      begin
        if (Options = []) or ((afoFiles in Options) and WideFileExists(AList.Strings[i])) then
          AddFile(AList.Strings[i]);
      end;
    end;
  finally
    lstFiles.EndUpdate;
    Screen.Cursor := crDefault;
    SetStatus;
    UpdateActionPreview(lstFiles);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.AddFolder(const AFolder: WideString; Options: TAddFoldersOptions);
  procedure AddContents(AFolder: WideString);
  var
    SearchRec: TSearchRecW;
    Code: Integer;
  begin
    if AFolder[Length(AFolder)] <> '\' then
      AFolder := AFolder + '\';
    Code := WideFindFirst(AFolder + '*.*', faAnyFile, SearchRec);
    while Code = 0 do
    begin
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        if (SearchRec.Attr and faDirectory <> 0) then
        begin
          if (afoRecursive in Options) then
          begin
            if (afoFolders in Options) then
              AddFile(AFolder + SearchRec.Name);
            AddContents(AFolder + SearchRec.Name);
          end;
        end
        else
        if (afoFiles in Options) then
          AddFile(AFolder + SearchRec.Name)
      end;
      Code := WideFindNext(SearchRec);
    end;
    WideFindClose(SearchRec);
  end;
begin
  Screen.Cursor := crHourGlass;
  try
    AddContents(AFolder);
  finally
    Screen.Cursor := crDefault;
  end;
end;

{-------------------------------------------------------------------------------
  Status
-------------------------------------------------------------------------------}

procedure TMainForm.SetStatus;
begin
  StatusBarPanel2.Caption := '';
  StatusBarPanel1.Caption := Format(strmsgFilesCount, [FFilesList.Count]);
  ProgressBar1.Position := 0;
//  ProgressBar1.Visible := False;
  ProgressBar1.TextOption := toNoText;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.SetStatus(APercent: Integer; ACaption: string);
begin
  if APercent >= 0 then
  begin
    ProgressBar1.Position := APercent;
    if APercent > 0 then
      ProgressBar1.TextOption := toPercent
    else
      ProgressBar1.TextOption := toNoText;
//    ProgressBar1.Visible := (APercent > 0);
  end;
  if ACaption <> '' then
  begin
    StatusBarPanel2.Caption := ACaption;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.SetStatus(APercent: Integer);
begin
  SetStatus(APercent, '');
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.SetStatus(ACaption: string);
begin
  SetStatus(-1, ACaption);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TMainForm.GetCurrentList: TVirtualStringTree;
begin
  case pgctrlMain.ActivePageIndex of
    0:  Result := lstFiles;
    1:  Result := lstBatch;
//    2:  Result := lstLog;
  else
    Result := nil;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.lstActionsClick(Sender: TObject);
begin
  pgctrlAction.ActivePageIndex := lstActions.ItemIndex + 1;
  UpdateActionPreview(lstActions);
  if Visible then
    Perform(WM_NEXTDLGCTL, 0, 0);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.lblChangeExtNotesLinkClick(Sender: TObject; LinkNumber: Integer; LinkText: String);
begin
  lstActions.ItemIndex := Ord(raiEnum);
  lstActionsClick(Sender);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.lblStrInsNotesLinkClick(Sender: TObject; LinkNumber: Integer; LinkText: String);
begin
  case LinkNumber of
    0:
      begin
        lstActions.ItemIndex := Ord(raiStringRepl);
        lstActionsClick(Sender);
      end;
    1:
      begin
        LaunchHelp(1255);
      end;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.lblMoveStrNotesLinkClick(Sender: TObject; LinkNumber: Integer; LinkText: String);
begin
  case LinkNumber of
    0:
      begin
        lstActions.ItemIndex := Ord(raiStringRepl);
        lstActionsClick(Sender);
      end;
    1:
      begin
        lstActions.ItemIndex := Ord(raiStringInsert);
        lstActionsClick(Sender);
      end;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.lblCharDelNotesLinkClick(Sender: TObject; LinkNumber: Integer; LinkText: String);
begin
  lstActions.ItemIndex := Ord(raiStringRepl);
  lstActionsClick(Sender);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.lblEnumNotesLinkClick(Sender: TObject; LinkNumber: Integer; LinkText: String);
begin
  LaunchHelp(1255);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.lblMP3NotesLinkClick(Sender: TObject; LinkNumber: Integer; LinkText: String);
begin
  LaunchHelp(1255);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.lblRandomNotesLinkClick(Sender: TObject; LinkNumber: Integer; LinkText: String);
begin
  LaunchHelp(1255);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.lblExifNotesLinkClick(Sender: TObject; LinkNumber: Integer; LinkText: String);
begin
  LaunchHelp(1255);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.lblRegexpNotesLinkClick(Sender: TObject; LinkNumber: Integer; LinkText: String);
begin
  LaunchHelp(1260);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.lstBatchResize(Sender: TObject);
begin
  with lstBatch do
    with Header.Columns do
    begin
      Items[1].Width := ClientWidth - Items[0].Width;
    end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.lstFilesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
var
  TheData: PFileNode;
  f: Double;
  dt: TDateTime;
begin
  TheData := lstFiles.GetNodeData(Node);
  if TheData^.RenFile = nil then
    Exit;
  case Column of
    COL_FOLDER:
      CellText := TheData^.RenFile.FileFolder;
    COL_FILENAME:
      CellText := TheData^.RenFile.FileName;
    COL_FULLPATH:
      CellText := TheData^.RenFile.FilePath;
    COL_PREVIEW:
      CellText := TheData^.RenFile.Preview;
    COL_EXT:
      CellText := TheData^.RenFile.FileExt;
    COL_SIZE:
      begin
        f := TheData^.RenFile.Size;
        if f < 0 then
          CellText := ''
        else
          CellText := Format('%.0n', [f]);
      end;
    COL_CREATED:
      begin
        dt := TheData^.RenFile.CreatedDate;
        if dt > MinDouble then
          CellText := DateTimeToStr(dt)
        else
          CellText := '';
      end;
    COL_MODIFIED:
      begin
        dt := TheData^.RenFile.ModifiedDate;
        if dt > MinDouble then
          CellText := DateTimeToStr(dt)
        else
          CellText := '';
      end;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.lstFilesGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  TheFile: TRenFile;
  FileName, Ext: string;
  Ico: TIcon;
  ExtIndex: Integer;
begin
  with (Sender as TVirtualStringTree).Header do
    if (Column in [COL_EXT, COL_PREVIEW, COL_SIZE, COL_CREATED, COL_MODIFIED]) or
      (((Column = COL_FOLDER) and (Columns[COL_FOLDER].Position > Columns[COL_FILENAME].Position)) or
       ((Column = COL_FILENAME) and (Columns[COL_FILENAME].Position > Columns[COL_FOLDER].Position))) then
      Exit;
  TheFile := PFileNode(Sender.GetNodeData(Node))^.RenFile;
  if TheFile.IsFolder then
  begin
    ImageIndex := 0;
  end
  else
  begin
    FileName := LowerCase(TheFile.FilePath);
    Ext := ExtractFileExt(FileName);
    if IndexStr(Ext, ['.exe', '.ico', '.lnk', '.pif']) <> -1 then
      Ext := FileName;
    ExtIndex := lstFilesExt.IndexOf(Ext);
    if ExtIndex < 0 then
    begin
      Ico := GetIcon(FileName, False);
      if Ico <> nil then
      begin
        try
          ImageIndex := imglstFiles.AddIcon(Ico);
          ExtIndex := lstFilesExt.Add(Ext);
          if ImageIndex <> ExtIndex then
            raise Exception.Create(Format('ImageIndex from image list (%d) does not match extension list (%d)', [ImageIndex, ExtIndex]));
        finally
          Ico.Free;
        end;
      end;
    end
    else
    begin
      if ExtIndex >= imglstFiles.Count then
        raise Exception.Create(Format('ImageIndex from extension list (%d) too big for image list (%d)', [ExtIndex, imglstFiles.Count]));
      ImageIndex := ExtIndex;
    end;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.lstBatchGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
var
  TheData: PActionNode;
begin
  TheData := lstBatch.GetNodeData(Node);
  case Column of
    0:
      CellText := IntToStr(Node.Index + 1);
    1:
      CellText := TheData^.RenAction.Description;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}
{
procedure TMainForm.lstLogGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
var
  TheData: PLogNode;
begin
  TheData := lstLog.GetNodeData(Node);
  CellText := TheData^.Description;
end;
}
{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}
{
procedure TMainForm.lstLogPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
var
  TheData: PLogNode;
begin
  TheData := lstLog.GetNodeData(Node);
  case TheData^.Status of
    rsNotRenamed:
      TargetCanvas.Font.Color := clNavy;
    rsOk:
      TargetCanvas.Font.Color := clGreen;
    rsError:
      TargetCanvas.Font.Color := clMaroon;
    rsInfo:
      TargetCanvas.Font.Color := clDkGray;
  end;
end;
}
{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.UpdateHistoryList(Combo: TComboBox);
var
  idx: Integer;
begin
  with Combo do
    if Text <> '' then
    begin
      idx := Items.IndexOf(Text);
      if idx = -1 then
        Items.Insert(0, Text)
      else
        Items.Move(idx, 0);
      ItemIndex := 0;
    end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.UpdateHistoryList(Combo: TTntComboBox);
var
  idx: Integer;
begin
  with Combo do
    if Text <> '' then
    begin
      idx := Items.IndexOf(Text);
      if idx = -1 then
        Items.Insert(0, Text)
      else
        Items.Move(idx, 0);
      ItemIndex := 0;
    end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.FormActivate(Sender: TObject);
var
  param: WideString;
  i, nbParam: Integer;
  paramGo, paramExit: Boolean;
  paramAdd: TAddFoldersOptions;
  procedure ParamAddExecute(const AFileName: WideString);
  var
    sr: TSearchRecW;
    code: Integer;
    curFilePath: WideString;
  begin
    WideSetCurrentDir(WideExtractFilePath(WideExpandFileName(AFileName)));
    code := WideFindFirst(AFileName, faAnyFile, sr);
    while code = 0 do
    begin
      if (sr.Name <> '.') and (sr.Name <> '..') then
      begin
        curFilePath := WideExpandFileName(sr.Name);
        if (sr.Attr and faDirectory <> 0) then
        begin
          if (afoFolders in paramAdd) then
            AddFile(curFilePath);
          if (afoRecursive in paramAdd) then
            AddFolder(curFilePath, paramAdd);
        end
        else
        if (afoFiles in paramAdd) then
          AddFile(curFilePath);
      end;
      code := WideFindNext(sr);
    end;
    WideFindClose(sr);
  end;
begin
  OnActivate := nil;
  ActionAlwaysOnTopExecute(Self);
  UpdateActionPreview(Self);
  lstFiles.BeginUpdate;
  lstBatch.BeginUpdate;
  try
    paramGo := False;
    paramExit := False;
    i := 1;
    nbParam := WideParamCount;
    while i <= nbParam do
    begin
      SetCurrentDir(strDirStart);
      param := WideParamStr(i);
      if (param = '-b') and (i < nbParam) {and SameText(ExtractFileExt(param), '.arb')} then
      begin
        mruBatchClick(Self, WideExpandFileName(WideParamStr(i+1)));
        Inc(i);
      end
      else
      if (param = '-g') then
        paramGo := True
      else
      if (param = '-x') then
        paramExit := True
      else
      if StartsStr('-a', param) then
      begin
        paramAdd := [];
        if Pos('f', param) <> 0 then
          Include(paramAdd, afoFiles);
        if Pos('F', param) <> 0 then
          Include(paramAdd, afoFolders);
        if Pos('r', param) <> 0 then
          Include(paramAdd, afoRecursive);
      end
      else
      begin
        // adding files
        if (paramAdd = []) then
        begin
          // compatibility with version 2.07
          if SameText(ExtractFileExt(param), '.arb') then
            mruBatchClick(Self, param)
          else
          if WideDirectoryExists(param) then
            AddFolder(param, [afoFiles, afoRecursive]);
        end
        else
          // new param style, adding files according to "-a" param
          ParamAddExecute(WideExcludeTrailingPathDelimiter(param));
      end;
      Inc(i);
    end;
  finally
    lstFiles.EndUpdate;
    lstBatch.EndUpdate;
  end;
  if (ParamGo) and (FFilesList.Count > 0) then
    ActionActionGo.Execute;
  if ParamExit then
  begin
    while FRenThread <> nil do
      Application.HandleMessage;
    ActionExit.Execute;
  end;
  edtEnumDigits.MinValue := 1;
  edtMoveStrCount.MinValue := 1;
  edtCharDel.MinValue := 1;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionList1Execute(Action: TBasicAction; var Handled: Boolean);
begin
  if (FRenThread <> nil) and (Action <> ActionActionStop)
    and (Action <> ActionTabFiles) and (Action <> ActionTabActions) and (Action <> ActionTabLog) then
  begin
    Handled := True;
    MessageWin.Execute(strmsgBusy, mtWarning, [mbOk]);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionFromListClearExecute(Sender: TObject);
begin
  edtFromList.Clear;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionFromListOpenExecute(Sender: TObject);
begin
  with TTntOpenDialog.Create(self) do
    try
      Options := DialogOpenOptions;
      InitialDir := UTF8Decode(Settings.Root.Folders.FromList.Path);
      if not WideDirectoryExists(InitialDir) then
        InitialDir := '';
      Filter := Format('%s (*.lst, *.txt)|*.lst;*.txt|%s (*.*)|*.*', [strfltTextFiles, strfltAllFiles]);
      if Execute then
      begin
        Settings.Root.Folders.FromList.Path := UTF8Encode(WideExtractFilePath(FileName));
        edtFromList.Lines.LoadFromFile(FileName);
      end;
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionFromListSaveExecute(Sender: TObject);
begin
  with TTntSaveDialog.Create(self) do
    try
      Options := DialogSaveOptions;
      DefaultExt := 'lst';
      InitialDir := UTF8Decode(Settings.Root.Folders.FromList.Path);
      if not WideDirectoryExists(InitialDir) then
        InitialDir := '';
      Filter := Format('%s (*.lst, *.txt)|*.lst;*.txt|%s (*.*)|*.*', [strfltTextFiles, strfltAllFiles]);
      if Execute then
      begin
        Settings.Root.Folders.FromList.Path := UTF8Encode(WideExtractFilePath(FileName));
        edtFromList.Lines.SaveToFile(FileName);
      end;
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionMultistrReplClearExecute(Sender: TObject);
begin
  with edtMultstrRepl do
  begin
    RowCount := 1;
    Cells[0,0] := '';
    Cells[1,0] := '';
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionMultistrReplCopyExecute(Sender: TObject);
var
  gridtext: WideString;
  i: Integer;
begin
  gridtext := '';
  for i := 0 to edtMultstrRepl.RowCount-1 do
  begin
    if i > 0 then
      gridtext := gridtext + sLineBreak;
    gridtext := gridtext + edtMultstrRepl.Cells[0, i] + Chr(9) + edtMultstrRepl.Cells[1, i];
  end;
  TntClipboard.AsWideText := gridtext;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionMultistrReplPasteExecute(Sender: TObject);
var
  lines: TTntStringList;
  part1, part2: WideString;
  i, c: Integer;
begin
  lines := TTntStringList.Create;
  try
    lines.Text := TntClipboard.AsWideText;
    c := lines.Count;
    edtMultstrRepl.RowCount := c + 1;
    for i := 0 to c - 1 do
    begin
      Split(lines[i], Chr(9), part1, part2, True);
      edtMultstrRepl.Cells[0,i] := part1;
      edtMultstrRepl.Cells[1,i] := Tnt_WideStringReplace(part2, Chr(9), '', [rfReplaceAll]);
    end;
    edtMultstrRepl.Cells[0,c] := '';
    edtMultstrRepl.Cells[1,c] := '';
  finally
    lines.Free;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionMultistrReplSaveExecute(Sender: TObject);
var
  i: Integer;
begin
  if edtMultstrReplSet.Text <> '' then
  begin
    Settings.Root.LastValues.MultstrRepl.Sets.Save(UTF8Encode(edtMultstrReplSet.Text), UTF8Encode(GridToString(edtMultstrRepl)));
    i := edtMultstrReplSet.Items.IndexOf(edtMultstrReplSet.Text);
    if i = -1 then
      edtMultstrReplSet.Items.Add(edtMultstrReplSet.Text);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.edtMultstrReplSetClick(Sender: TObject);
var
  Value: string;
begin
  if Settings.Root.LastValues.MultstrRepl.Sets.Load(UTF8Encode(edtMultstrReplSet.Text), Value) then
  begin
    StringToGrid(edtMultstrRepl, UTF8Decode(Value));
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionMultistrReplDeleteExecute(Sender: TObject);
var
  i: Integer;
begin
  if edtMultstrReplSet.Text <> '' then
  begin
    Settings.Root.LastValues.MultstrRepl.Sets.Delete(UTF8Encode(edtMultstrReplSet.Text));
    i := edtMultstrReplSet.Items.IndexOf(edtMultstrReplSet.Text);
    if i <> -1 then
      edtMultstrReplSet.Items.Delete(i);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.lstActionsContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  idx: Integer;
begin
  idx := lstActions.ItemAtPos(MousePos, True);
  if idx > -1 then
  begin
    lstActions.ItemIndex := idx;
    lstActionsClick(lstActions);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.UpdateActionPreview(Sender: TObject);
var
  NewAction: TRenAction;
  NewList: TRenFiles;
  NewFile: TRenFile;
  CurNode: PVirtualNode;
begin
  if (Self.Visible) and (FRenThread = nil) then
  begin
    if cbxPreview.Checked then
    begin
      lblPreviewOldName.Caption := '';
      lblPreviewNewName.Caption := '';
      if grpPreview.Height <> 65 then
        grpPreview.Height := 65;
      CurNode := lstFiles.GetFirstSelected;
      if CurNode = nil then
        CurNode := lstFiles.GetFirst;
      if CurNode = nil then
        Exit;
      GlobalStop := False;
      NewAction := TRenActions.CreateAction(TRenActionItem(lstActions.ItemIndex));
      try
        SetActionData(NewAction, False);
        NewList := TRenFiles.Create;
        try
          NewFile := NewList.Add(PFileNode(lstFiles.GetNodeData(CurNode))^.RenFile.FilePath, nil);
          NewAction.Perform(NewList, raoPreview);
          lblPreviewOldName.Caption := NewFile.FileName;
          lblPreviewNewName.Caption := NewFile.Preview;
        finally
          NewList.Free;
        end;
      finally
        NewAction.Free;
      end;
    end
    else
      if grpPreview.Height <> 20 then
        grpPreview.Height := 20;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.AddToLog(const ADescription: WideString; const AStatus: TRenState);
var
  TheNode: PVirtualNode;
  TheData: PLogNode;
begin
  if ADescription <> '' then
  begin
    TheNode := lstLog.AddChild(nil);
    TheData := lstLog.GetNodeData(TheNode);
    TheData^.Description := ADescription;
    TheData^.Status := AStatus;
    if pgctrlMain.ActivePage = tshLog then
      lstLog.ScrollIntoView(TheNode, False);
{
    with lstLog.Items do
      with Add do
      begin
        Caption := ADescription;
        Data := Pointer(AStatus);
        if ActionTabLog.Checked then
          MakeVisible(False);
      end;
}
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.lstLogGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
begin
  with PLogNode(Sender.GetNodeData(Node))^ do
  begin
    CellText := Description;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.lstLogPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
begin
  with PLogNode(Sender.GetNodeData(Node))^ do
  begin
    case Status of
      rsNotRenamed:
        TargetCanvas.Font.Color := clNavy;
      rsOk:
        TargetCanvas.Font.Color := clGreen;
      rsError:
        TargetCanvas.Font.Color := clMaroon;
      rsInfo:
        TargetCanvas.Font.Color := clDkGray;
    end;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.SetActionData(const AAction: TRenAction; const UpdateHistory: Boolean);
begin
  case AAction.ActionType of
    raiChangeExt:
      with TRenActionChangeExt(AAction).Settings do
      begin
        NewExtension := edtChangeExt.Text;
        if (NewExtension <> '') and (NewExtension[1] <> '.') then
          NewExtension := '.' + NewExtension;
        if NewExtension = '.' then
          NewExtension := '';
        NoReplace := cbxChangeExtNoRepl.Checked;
        if UpdateHistory then
          UpdateHistoryList(edtChangeExt);
      end;
    raiStringRepl:
      with TRenActionStringRepl(AAction).Settings do
      begin
        Search := edtStrReplSearch.Text;
        Replace := edtStrReplBy.Text;
        All := cbxStrReplAll.Checked;
        CaseSens := cbxStrReplCase.Checked;
        InclExt := cbxStrReplExt.Checked;
        OnlyExt := cbxStrReplOnlyExt.Checked;
        if UpdateHistory then
        begin
          UpdateHistoryList(edtStrReplSearch);
          UpdateHistoryList(edtStrReplBy);
        end;
      end;
    raiMultstrRepl:
      with TRenActionMultstrRepl(AAction).Settings do
      begin
        SetName := edtMultstrReplSet.Text;
        SetChars := GridToString(edtMultstrRepl);
        IncludeExt := cbxMultstrReplExt.Checked;
        CaseSens := cbxMultstrCase.Checked;
      end;
    raiStringInsert:
      with TRenActionStringIns(AAction).Settings do
      begin
        NewString := edtStrInsStr.Text;
        Position := edtStrInsPos.AsInteger;
        FromBegin := rbtStrInsBegin.Checked;
        Ext := cbxStrInsExt.Checked;
        if UpdateHistory then
          UpdateHistoryList(edtStrInsStr);
      end;
    raiMoveString:
      with TRenActionMoveStr(AAction).Settings do
      begin
        FromPos := edtMoveStrFrom.AsInteger;
        FromBegin := rbtMoveStrFromBegin.Checked;
        Count := edtMoveStrCount.AsInteger;
        ToPos := edtMoveStrTo.AsInteger;
        ToBegin := rbtMoveStrToBegin.Checked;
      end;
    raiCharDel:
      with TRenActionCharDel(AAction).Settings do
      begin
        NbChar := edtCharDel.AsInteger;
        if rbtCharDelStr.Checked then
          AfterString := 1
        else
        if rbtCharDelStr2.Checked then
          AfterString := 2
        else
          AfterString := 0;
        Position := edtCharDelPos.AsInteger;
        FromBegin := rbtCharDelBegin.Checked;
        Str := edtCharDelStr.Text;
        Ext := cbxCharDelExt.Checked;
        if UpdateHistory then
          UpdateHistoryList(edtCharDelStr);
      end;
    raiEnum:
      with TRenActionEnum(AAction).Settings do
      begin
        Mask := edtEnum.Text;
        StartAt := edtEnumStart.AsInteger;
        Digits := edtEnumDigits.AsInteger;
        Increment := edtEnumIncr.AsInteger;
        Restart := cbxEnumRestart.Checked;
        if UpdateHistory then
          UpdateHistoryList(edtEnum);
      end;
    raiMP3Tag:
      with TRenActionMP3(AAction).Settings do
      begin
        Mask := edtMP3Mask.Text;
        TwoDigit := cbxMP3TwoDigit.Checked;
        if UpdateHistory then
          UpdateHistoryList(edtMP3Mask);
      end;
    raiDateTime:
      with TRenActionDateTime(AAction).Settings do
      begin
        Mask := edtDTMask.Text;
        AddSuffix := cbxDTSuffix.Checked;
        WhichDate := IfThen(rbtDTWhichCreation.Checked, 0, 1);
        Offset := edtDTOffset.AsInteger;
        if UpdateHistory then
          UpdateHistoryList(edtDTMask);
      end;
    raiRandom:
      with TRenActionRandom(AAction).Settings do
      begin
        Mask := edtRandomMask.Text;
        if rbtRandomTick.Checked then Method := 1
        else if rbtRandomGUID.Checked then Method := 2
        else Method := 0;
        if UpdateHistory then
          UpdateHistoryList(edtRandomMask);
      end;
    raiCase:
      with TRenActionCase(AAction).Settings do
      begin
        if rbtCaseFirst.Checked then CaseType := 1
        else if rbtCaseUpper.Checked then CaseType := 2
        else if rbtCaseLower.Checked then CaseType := 3
        else CaseType := 0;
        AfterChars := edtCaseAfter.Text;
        if UpdateHistory then
          UpdateHistoryList(edtCaseAfter);
        UseLocale := cbxCaseLocale.Checked;
        IncludeExt := cbxCaseIncludeExt.Checked;
        OnlyExt := cbxCaseOnlyExt.Checked;
      end;
    raiFromList:
      with TRenActionFromList(AAction).Settings do
      begin
        ListContents := edtFromList.Lines.Text;
        AppendExt := cbxFromListExt.Checked;
        OnlyExt := cbxFromListOnlyExt.Checked;
      end;
    raiRegexp:
      with TRenActionRegexp(AAction).Settings do
      begin
        Expr := edtRegexp.Text;
        Repl := edtRegexpRepl.Text;
        if UpdateHistory then
        begin
          UpdateHistoryList(edtRegexp);
          UpdateHistoryList(edtRegexpRepl);
        end;
      end;
    raiExif:
      with TRenActionExif(AAction).Settings do
      begin
        Mask := edtExifMask.Text;
        Offset := edtExifOffset.AsInteger;
        if UpdateHistory then
          UpdateHistoryList(edtExifMask);
      end;
      (*
    raiShiftDT:
      with TRenActionShiftDT(AAction).Settings do
      begin
        TimePos := edtShiftDTTime.AsInteger;
        TimeAdd := Round(OneSecond * edtShiftDTTimeAdd.Time);
        DatePos := edtShiftDTDate.AsInteger;
        DateAdd := edtShiftDTDateAdd.AsInteger;
      end;
      *)
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TMainForm.ExecuteAction(Action: TBasicAction): Boolean;
begin
  if (Action is THintAction) then
  begin
    StatusBarPanel2.Caption := THintAction(Action).Hint;
    Result := True;
  end
  else
    Result := inherited ExecuteAction(Action);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.lstFilesCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  File1, File2: TRenFile;
begin
  File1 := PFileNode(Sender.GetNodeData(Node1))^.RenFile;
  File2 := PFileNode(Sender.GetNodeData(Node2))^.RenFile;
  if Assigned(File1) and Assigned(File2) then
    case Column of
      COL_FOLDER:
        Result := WideCompareText(File1.FileFolder, File2.FileFolder);
      COL_FILENAME:
        Result := WideCompareText(File1.FileName, File2.FileName);
      COL_FULLPATH:
        Result := WideCompareText(File1.FilePath, File2.FilePath);
      COL_PREVIEW:
        Result := WideCompareText(File1.Preview, File2.Preview);
      COL_EXT:
        Result := WideCompareText(File1.FileExt, File2.FileExt);
      COL_SIZE:
        Result := File1.Size - File2.Size;
      COL_CREATED:
        Result := CompareDateTime(File1.CreatedDate, File2.CreatedDate);
      COL_MODIFIED:
        Result := CompareDateTime(File1.ModifiedDate, File2.ModifiedDate);
    end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.lstFilesHeaderClick(Sender: TVTHeader;
  Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if Button = mbLeft then
  begin
    with Sender do
    begin
      if (SortColumn = NoColumn) or (SortColumn <> Column) then
      begin
        SortColumn := Column;
        SortDirection := sdAscending;
      end
      else
        if SortDirection = sdAscending then
          SortDirection := sdDescending
        else
          SortDirection := sdAscending;
      Treeview.SortTree(SortColumn, SortDirection, False);
    end;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.lstFilesChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  UpdateActionPreview(lstFiles);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.FilesDrop(Sender: TObject; Pos: TPoint; Value: TTntStringList);
begin
  with TDragdropOptionsForm.Create(Self) do
    try
      ToolbarImagesHot.GetIcon(tbiAddFiles, Icon);
      if Execute = mrOk then
      begin
        AddFiles(Value, Options.Options);
      end;
    finally
      Release;
    end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.pmhFilesPopup(Sender: TObject);
var
  NewItem: TMenuItem;
begin
  if lstFiles.Header.SortColumn <> -1 then
  begin
    NewItem := TMenuItem.Create(pmhFiles);
    with NewItem do
      Caption := '-';
    pmhFiles.Items.Add(NewItem);
    NewItem := TMenuItem.Create(pmhFiles);
    with NewItem do
      Action := ActionItemsUnsort;
    pmhFiles.Items.Add(NewItem);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.ActionItemsUnsortExecute(Sender: TObject);
begin
  with lstFiles.Header do
  begin
    SortColumn := VirtualTrees.NoColumn;
    SortDirection := VirtualTrees.sdAscending;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.cbxBatchClick(Sender: TObject);
begin
  if cbxBatch.Checked then
  begin
    if Sender = cbxBatch then
      grpBatch.Height := Settings.Root.Forms.MainOptions.BatchListHeight;
    lstBatch.Visible := True;
    tbMove.Visible := True;
  end
  else
  begin
    lstBatch.Visible := False;
    if Sender = cbxBatch then
      grpBatch.Height := 20;
    tbMove.Visible := False;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.splBatchCanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
begin
  Accept := True;
  cbxBatch.Checked := NewSize > 20;
  cbxBatchClick(splBatch);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

var
  FolderColor: TColor;
procedure TMainForm.lstFilesBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
begin
  if PFileNode(Sender.GetNodeData(Node))^.RenFile.IsFolder then
  begin
    TargetCanvas.Brush.Color := FolderColor;
    TargetCanvas.FillRect(CellRect);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.edtMultstrReplSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: WideString);
var
  rc: Integer;
begin
  with edtMultstrRepl do
  begin
    rc := RowCount;
     // add new row if last one is not empty
    if (Cells[0, rc-1] <> '') or (Cells[1, rc-1] <> '') then
    begin
      RowCount := RowCount+1;
      Cells[0, rc] := '';
      Cells[1, rc] := '';
    end
    else
     // delete last row if it is empty (and previous is too)
    if (rc > 1) and (Cells[0, rc-2] = '') and (Cells[1, rc-2] = '') and (Cells[0, rc-1] = '') and (Cells[1, rc-1] = '') then
      RowCount := RowCount-1;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.edtMultstrReplKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i: Integer;
begin
  with edtMultstrRepl do
    if Shift = [ssCtrl] then
    begin
      if Key = VK_ADD then
      begin
        RowCount := RowCount + 1;
        for i := RowCount-2 downto Row do
        begin
          Cells[0, i+1] := Cells[0, i];
          Cells[1, i+1] := Cells[1, i];
        end;
        Cells[0, Row] := '';
        Cells[1, Row] := '';
      end
      else
      if Key = VK_SUBTRACT then
      begin
        if Row < RowCount-1 then
        begin
          for i := Row to RowCount-2 do
          begin
            Cells[0, i] := Cells[0, i+1];
            Cells[1, i] := Cells[1, i+1];
          end;
          Cells[0, RowCount-2] := '';
          Cells[1, RowCount-2] := '';
          RowCount := RowCount-1;
        end;
      end;
    end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.lstLogEnter(Sender: TObject);
begin
  FocusControl(nil);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TMainForm.lstFilesDblClick(Sender: TObject);
var
  NodeData: PFileNode;
begin
  NodeData := lstFiles.GetNodeData(lstFiles.GetFirstSelected);
  if Assigned(NodeData) and (Settings.Root.Options.Display.LaunchFile) then
  begin
    LaunchProg(NodeData^.RenFile.FileName, NodeData^.RenFile.FileFolder);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TMainForm.OnAppHelp(Command: Word; Data: Integer; var CallHelp: Boolean): Boolean;
begin
  if Data <> Self.HelpContext then
    LaunchHelp(Data);
  CallHelp := False;
  Result := False;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

initialization
  FolderColor := GetSysColor(COLOR_WINDOW);
  FolderColor := (((FolderColor and $FF0000) shr 16) + (FolderColor and $00FF00) + ((FolderColor and $0000FF) shl 16)) - $080808;
  if FolderColor < 0 then
    FolderColor := clDkGray;
end.
