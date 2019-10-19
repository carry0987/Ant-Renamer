(************************************************************************
 *                                                                      *
 *   Ant Renamer 2.x                                                    *
 *   (C) 2003-2015 Antoine Potten                                       *
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

unit ProgramSettings;

interface

uses
  Classes, SysUtils, JvSimpleXml, SettingsBase;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

type

  TSettingsRoot = class;
  TSettingsForms = class;
    TSettingsMainOptions = class;
    TSettingsAddFolders = class;
  TSettingsFolders = class;
  TSettingsOptions = class;
    TSettingsDisplay = class;
    TSettingsProcessing = class;
  TSettingsHints = class;
  TSettingsLastValues = class;
    TSettingsMRU = class;
    TSettingsChangeExt = class;
    TSettingsStrRepl = class;
    TSettingsMultstrRepl = class;
    TSettingsStrIns = class;
    TSettingsMoveStr = class;
    TSettingsCharDel = class;
    TSettingsEnum = class;
    TSettingsMP3 = class;
    TSettingsDateTime = class;
    TSettingsRandom = class;
    TSettingsChangeCase = class;
    TSettingsFromList = class;
    TSettingsRegexp = class;
    TSettingsExif = class;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

  TSettingsRoot = class(TSettingsBase)
  private
  protected
    procedure Init; override;
    procedure Clean; override;
  public
    Forms: TSettingsForms;
    Options: TSettingsOptions;
    Folders: TSettingsFolders;
    Hints: TSettingsHints;
    LastValues: TSettingsLastValues;
  end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

  TSettingsForms = class(TSettingsBase)
  private
  protected
    procedure Init; override;
    procedure Clean; override;
  public
    Main: TSettingsForm;
    MainOptions: TSettingsMainOptions;
//    Options: TSettingsForm;
    SelectDirectory: TSettingsForm;
    SelectDirectoryOptions: TSettingsAddFolders;
  end;

  TSettingsMainOptions = class(TSettingsBase)
  private
  protected
    procedure Init; override;
    procedure Clean; override;
  public
    Toolbars: TSettingsCustom;
    FilesListColumns: TSettingsCustom;
    property FilesSortOrder: Boolean index 0 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property FilesSortColumn: Integer index 1 read GetOptionIntegerIndex write SetOptionIntegerIndex;
    property ActionsListWidth: Integer index 2 read GetOptionIntegerIndex write SetOptionIntegerIndex;
    property BatchListHeight: Integer index 3 read GetOptionIntegerIndex write SetOptionIntegerIndex;
    property EnableSample: Boolean index 4 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property EnableBatch: Boolean index 5 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property LogFilterOk: Boolean index 6 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property LogFilterError: Boolean index 7 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property LogFilterNot: Boolean index 8 read GetOptionBooleanIndex write SetOptionBooleanIndex;
  end;

  TSettingsAddFolders = class(TSettingsBase)
  private
  protected
    procedure Init; override;
    procedure Clean; override;
  public
    property AddFiles: Boolean index 0 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property AddFolders: Boolean index 1 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property Recursive: Boolean index 2 read GetOptionBooleanIndex write SetOptionBooleanIndex;
  end;

  TSettingsFolders = class(TSettingsBase)
  private
  protected
    procedure Init; override;
    procedure Clean; override;
  public
    AddFiles: TSettingsFolder;
    AddFolder: TSettingsFolder;
    FromList: TSettingsFolder;
    Batch: TSettingsFolder;
  end;

  TSettingsOptions = class(TSettingsBase)
  private
  protected
    procedure Init; override;
    procedure Clean; override;
  public
    Display: TSettingsDisplay;
    Processing: TSettingsProcessing;
    property Language: string index 0 read GetOptionStringIndex write SetOptionStringIndex;
  end;

  TSettingsHints = class(TSettingsBase)
  private
  protected
    procedure Init; override;
    procedure Clean; override;
  public
    property ShowPreview: Boolean index 0 read GetOptionBooleanIndex write SetOptionBooleanIndex;
  end;

  TSettingsLastValues = class(TSettingsBase)
  private
  protected
    procedure Init; override;
    procedure Clean; override;
  public
    MRU: TSettingsMRU;
    ChangeExt: TSettingsChangeExt;
    StrRepl: TSettingsStrRepl;
    MultstrRepl: TSettingsMultstrRepl;
    StrIns: TSettingsStrIns;
    MoveStr: TSettingsMoveStr;
    CharDel: TSettingsCharDel;
    Enum: TSettingsEnum;
    MP3: TSettingsMP3;
    DateTime: TSettingsDateTime;
    Random: TSettingsRandom;
    ChangeCase: TSettingsChangeCase;
    FromList: TSettingsFromList;
    Regexp: TSettingsRegexp;
    Exif: TSettingsExif;
  end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

  TSettingsDisplay = class(TSettingsBase)
  private
  protected
    procedure Init; override;
    procedure Clean; override;
  public
    DragDropOptions: TSettingsAddFolders;
//    property ToolbarOfficeXP: Boolean index 0 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property ToolbarIconSet: string index 1 read GetOptionStringIndex write SetOptionStringIndex;
//    property ToolbarColorType: Integer index 2 read GetOptionIntegerIndex write SetOptionIntegerIndex;
//    property ToolbarClassic: Boolean index 3 read GetOptionBooleanIndex write SetOptionBooleanIndex;
//    property ClassicCaptions: Boolean index 4 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property RealtimeUpdate: Boolean index 5 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property ResizeColsFiles: Boolean index 6 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property ShowFilesIcons: Boolean index 7 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property ForceFont: Boolean index 8 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property FontName: string index 9 read GetOptionStringIndex write SetOptionStringIndex;
    property DropdownMax: Integer index 10 read GetOptionIntegerIndex write SetOptionIntegerIndex;
    property DragDropNoAsk: Boolean index 11 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property LaunchFile: Boolean index 12 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property DropdownComplete: Boolean index 13 read GetOptionBooleanIndex write SetOptionBooleanIndex;
  end;

  TSettingsProcessing = class(TSettingsBase)
  private
  protected
    procedure Init; override;
    procedure Clean; override;
  public
    property ForceDir: Boolean index 0 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property DetectAbsdir: Boolean index 1 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property FolderExt: Boolean index 2 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property Copy: Boolean index 3 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property GenerLog: Boolean index 4 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property StartedSwitch: Integer index 5 read GetOptionIntegerIndex write SetOptionIntegerIndex;
    property StartedClearLog: Boolean index 6 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property FinishedSwitch: Integer index 7 read GetOptionIntegerIndex write SetOptionIntegerIndex;
    property FinishedClearFiles: Boolean index 8 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property FinishedClearBatch: Boolean index 9 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property SaveLog: Boolean index 10 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property SaveLogAppend: Boolean index 11 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property SaveLogFile: string index 12 read GetOptionStringIndex write SetOptionStringIndex;
  end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

  TSettingsMRU = class(TSettingsBase)
  private
  protected
    procedure Init; override;
    procedure Clean; override;
  public
    Batch: TSettingsList;
  end;

  TSettingsChangeExt = class(TSettingsBase)
  private
  protected
    procedure Init; override;
    procedure Clean; override;
  public
    NewExtList: TSettingsList;
    property NewExt: string index 0 read GetOptionStringIndex write SetOptionStringIndex;
    property NoReplace: Boolean index 1 read GetOptionBooleanIndex write SetOptionBooleanIndex;
  end;

  TSettingsStrRepl = class(TSettingsBase)
  private
  protected
    procedure Init; override;
    procedure Clean; override;
  public
    SearchList: TSettingsList;
    ReplList: TSettingsList;
    property Search: string index 0 read GetOptionStringIndex write SetOptionStringIndex;
    property Repl: string index 1 read GetOptionStringIndex write SetOptionStringIndex;
    property AllOccurences: Boolean index 2 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property CaseSensitive: Boolean index 3 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property IncludeExt: Boolean index 4 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property OnlyExt: Boolean index 5 read GetOptionBooleanIndex write SetOptionBooleanIndex;
  end;

  TSettingsMultstrRepl = class(TSettingsBase)
  private
  protected
    procedure Init; override;
    procedure Clean; override;
  public
    CurrentList: TSettingsCDATA;
    Sets: TSettingsCDATAList;
    property LastSet: string index 0 read GetOptionStringIndex write SetOptionStringIndex;
    property IncludeExt: Boolean index 1 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property CaseSensitive: Boolean index 2 read GetOptionBooleanIndex write SetOptionBooleanIndex;
  end;

  TSettingsStrIns = class(TSettingsBase)
  private
  protected
    procedure Init; override;
    procedure Clean; override;
  public
    StrList: TSettingsList;
    property Str: string index 0 read GetOptionStringIndex write SetOptionStringIndex;
    property Pos: Integer index 1 read GetOptionIntegerIndex write SetOptionIntegerIndex;
    property FromBegin: Boolean index 2 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property Ext: Boolean index 3 read GetOptionBooleanIndex write SetOptionBooleanIndex;
  end;

  TSettingsMoveStr = class(TSettingsBase)
  private
  protected
    procedure Init; override;
    procedure Clean; override;
  public
    property FromPos: Integer index 0 read GetOptionIntegerIndex write SetOptionIntegerIndex;
    property FromBegin: Boolean index 1 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property Count: Integer index 2 read GetOptionIntegerIndex write SetOptionIntegerIndex;
    property ToPos: Integer index 3 read GetOptionIntegerIndex write SetOptionIntegerIndex;
    property ToBegin: Boolean index 4 read GetOptionBooleanIndex write SetOptionBooleanIndex;
  end;

  TSettingsCharDel = class(TSettingsBase)
  private
  protected
    procedure Init; override;
    procedure Clean; override;
  public
    StrList: TSettingsList;
    property Nb: Integer index 0 read GetOptionIntegerIndex write SetOptionIntegerIndex;
    property AfterString: Integer index 1 read GetOptionIntegerIndex write SetOptionIntegerIndex;
    property Pos: Integer index 2 read GetOptionIntegerIndex write SetOptionIntegerIndex;
    property FromBegin: Boolean index 3 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property Str: string index 4 read GetOptionStringIndex write SetOptionStringIndex;
    property Ext: Boolean index 5 read GetOptionBooleanIndex write SetOptionBooleanIndex;
  end;

  TSettingsEnum = class(TSettingsBase)
  private
  protected
    procedure Init; override;
    procedure Clean; override;
  public
    MaskList: TSettingsList;
    property Mask: string index 0 read GetOptionStringIndex write SetOptionStringIndex;
    property Start: Integer index 1 read GetOptionIntegerIndex write SetOptionIntegerIndex;
    property Digits: Integer index 2 read GetOptionIntegerIndex write SetOptionIntegerIndex;
    property Incr: Integer index 3 read GetOptionIntegerIndex write SetOptionIntegerIndex;
    property Restart: Boolean index 4 read GetOptionBooleanIndex write SetOptionBooleanIndex;
  end;

  TSettingsMP3 = class(TSettingsBase)
  private
  protected
    procedure Init; override;
    procedure Clean; override;
  public
    MaskList: TSettingsList;
    property Mask: string index 0 read GetOptionStringIndex write SetOptionStringIndex;
    property TwoDigit: Boolean index 1 read GetOptionBooleanIndex write SetOptionBooleanIndex;
  end;

  TSettingsDateTime = class(TSettingsBase)
  private
  protected
    procedure Init; override;
    procedure Clean; override;
  public
    MaskList: TSettingsList;
    property Mask: string index 0 read GetOptionStringIndex write SetOptionStringIndex;
    property AddSuffix: Boolean index 1 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property WhichDate: Integer index 2 read GetOptionIntegerIndex write SetOptionIntegerIndex;
  end;

  TSettingsRandom = class(TSettingsBase)
  private
  protected
    procedure Init; override;
    procedure Clean; override;
  public
    MaskList: TSettingsList;
    property Mask: string index 0 read GetOptionStringIndex write SetOptionStringIndex;
    property Method: Integer index 1 read GetOptionIntegerIndex write SetOptionIntegerIndex;
  end;

  TSettingsChangeCase = class(TSettingsBase)
  private
  protected
    procedure Init; override;
    procedure Clean; override;
  public
    CharsList: TSettingsList;
    property Option: Integer index 0 read GetOptionIntegerIndex write SetOptionIntegerIndex;
    property AfterChars: string index 1 read GetOptionStringIndex write SetOptionStringIndex;
    property UseLocale: Boolean index 2 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property IncludeExt: Boolean index 3 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property OnlyExt: Boolean index 4 read GetOptionBooleanIndex write SetOptionBooleanIndex;
  end;

  TSettingsFromList = class(TSettingsBase)
  private
  protected
    procedure Init; override;
    procedure Clean; override;
  public
    ListContents: TSettingsCDATA;
    property AppendExt: Boolean index 0 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    property OnlyExt: Boolean index 1 read GetOptionBooleanIndex write SetOptionBooleanIndex;
  end;

  TSettingsRegexp = class(TSettingsBase)
  private
  protected
    procedure Init; override;
    procedure Clean; override;
  public
    ExprList: TSettingsList;
    ReplList: TSettingsList;
    property Expr: string index 0 read GetOptionStringIndex write SetOptionStringIndex;
    property Repl: string index 1 read GetOptionStringIndex write SetOptionStringIndex;
  end;

  TSettingsExif = class(TSettingsBase)
  private
  protected
    procedure Init; override;
    procedure Clean; override;
  public
    MaskList: TSettingsList;
    property Mask: string index 0 read GetOptionStringIndex write SetOptionStringIndex;
    property DateFormat: string index 1 read GetOptionStringIndex write SetOptionStringIndex;
    property TimeFormat: string index 2 read GetOptionStringIndex write SetOptionStringIndex;
  end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

  TRenamerSettings = class(TSettingsFile)
  private
    FRoot: TSettingsRoot;
  public
    destructor Destroy; override;
    procedure LoadFromFile(const AFileName: TJvSimpleXmlFileName = ''); override;
    property Root: TSettingsRoot read FRoot;
  end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

implementation

uses
  functions_files, ConstValues;

{-------------------------------------------------------------------------------
  TRenamerSettings
-------------------------------------------------------------------------------}

destructor TRenamerSettings.Destroy;
begin
  FRoot.Free;
  inherited;
end;

procedure TRenamerSettings.LoadFromFile(const AFileName: TJvSimpleXmlFileName = '');
var
  xmlh: TJvSimpleXmlElemHeader;
  PrevVersion: string;
begin
  FRoot.Free;
  inherited;
  if FXML.Root.Name <> 'AntRenamer' then
  begin
    FXML.Root.Clear;
    FXML.Root.Name := 'AntRenamer';
  end;

  PrevVersion := FXML.Root.Properties.Value('Version', strVersion);

  FXML.Prolog.Clear;
  xmlh := TJvSimpleXmlElemHeader.Create;
  xmlh.Encoding := 'UTF-8';
  FXML.Prolog.Add(xmlh);

  FXML.Root.Properties.Clear;
  FXML.Root.Properties.Add('Version', strVersion);
  FXML.Root.Properties.Add('Date', DateToStr(Date));
  FRoot := TSettingsRoot.Create(FXML.Root);

  if PrevVersion < '2.05' then
    try
      Root.Options.Display.ResizeColsFiles := False;
      Root.Forms.MainOptions.GetRoot().Properties.Delete('FilesColumns');
      Root.Forms.MainOptions.Toolbars.GetRoot().Assign(Root.Options.FRoot.Items.ItemNamed['MainToolbars']);
      Root.Options.FRoot.Items.Delete('MainToolbars');
    except
    end;
  if PrevVersion < '2.06' then
    try
      Root.Forms.FRoot.Items.Delete('Options');
      Root.Forms.MainOptions.Toolbars.Node.Clear;
    except
    end;
end;

{-------------------------------------------------------------------------------
  TSettingsRoot
-------------------------------------------------------------------------------}

procedure TSettingsRoot.Init;
begin
  Forms := TSettingsForms.Create(FRoot, 'Forms');
  Options := TSettingsOptions.Create(FRoot, 'Options');
  Folders := TSettingsFolders.Create(FRoot, 'Folders');
  Hints := TSettingsHints.Create(FRoot, 'Hints');
  LastValues := TSettingsLastValues.Create(FRoot, 'LastValues');
end;

procedure TSettingsRoot.Clean;
begin
  Forms.Free;
  Options.Free;
  Folders.Free;
  Hints.Free;
  LastValues.Free;
end;

{-------------------------------------------------------------------------------
  TSettingsForms
-------------------------------------------------------------------------------}

procedure TSettingsForms.Init;
begin
  Main := TSettingsForm.Create(FRoot, 'Main');
  Main.SetDefaults(0, 730, 500, -1, -1, False);
  MainOptions := TSettingsMainOptions.Create(FRoot, 'Main');
//  Options := TSettingsForm.Create(FRoot, 'Options');
//  Options.SetDefaults(0, 432, 420, -1, -1, False);
  SelectDirectory := TSettingsForm.Create(FRoot, 'SelectDirectory');
  SelectDirectory.SetDefaults(0, 350, 450, -1, -1, False);
  SelectDirectoryOptions := TSettingsAddFolders.Create(FRoot, 'SelectDirectory');
end;

procedure TSettingsForms.Clean;
begin
  Main.Free;
  MainOptions.Free;
//  Options.Free;
  SelectDirectory.Free;
  SelectDirectoryOptions.Free;
end;

{-------------------------------------------------------------------------------
  TSettingsMainOptions
-------------------------------------------------------------------------------}

procedure TSettingsMainOptions.Init;
begin
  SetOptionsLength(9);
  OptionString[0] := 'FilesSortOrder';
  OptionString[1] := 'FilesSortColumn';
  OptionString[2] := 'ActionsListWidth';
  OptionString[3] := 'BatchListHeight';
  OptionString[4] := 'EnableSample';
  OptionString[5] := 'EnableBatch';
  OptionString[6] := 'LogFilterOk';
  OptionString[7] := 'LogFilterError';
  OptionString[8] := 'LogFilterNot';
  DefaultBoolean[0] := True;
  DefaultInteger[1] := -1;
  DefaultInteger[2] := 150;
  DefaultInteger[3] := 100;
  DefaultBoolean[4] := True;
  DefaultBoolean[5] := False;
  DefaultBoolean[6] := True;
  DefaultBoolean[7] := True;
  DefaultBoolean[8] := True;
  Toolbars := TSettingsCustom.Create(FRoot, 'Toolbars');
  FilesListColumns := TSettingsCustom.Create(FRoot, 'FilesListColumns');
end;

procedure TSettingsMainOptions.Clean;
begin
  Toolbars.Free;
  FilesListColumns.Free;
end;

{-------------------------------------------------------------------------------
  TSettingsAddFolders
-------------------------------------------------------------------------------}

procedure TSettingsAddFolders.Init;
begin
  SetOptionsLength(3);
  OptionString[0] := 'AddFiles';
  OptionString[1] := 'AddFolders';
  OptionString[2] := 'Recursive';
  DefaultBoolean[0] := True;
  DefaultBoolean[1] := False;
  DefaultBoolean[2] := True;
end;

procedure TSettingsAddFolders.Clean;
begin

end;

{-------------------------------------------------------------------------------
  TSettingsFolders
-------------------------------------------------------------------------------}

procedure TSettingsFolders.Init;
begin
  AddFiles := TSettingsFolder.Create(FRoot, 'AddFiles');
  AddFolder := TSettingsFolder.Create(FRoot, 'AddFolder');
  FromList := TSettingsFolder.Create(FRoot, 'FromList');
  Batch := TSettingsFolder.Create(FRoot, 'Batch');
end;

procedure TSettingsFolders.Clean;
begin
  AddFiles.Free;
  AddFolder.Free;
  FromList.Free;
  Batch.Free;
end;

{-------------------------------------------------------------------------------
  TSettingsOptions
-------------------------------------------------------------------------------}

procedure TSettingsOptions.Init;
begin
  SetOptionsLength(1);
  OptionString[0] := 'Language';
  DefaultString[0] := '?';
  Display := TSettingsDisplay.Create(FRoot, 'Display');
  Processing := TSettingsProcessing.Create(FRoot, 'Processing');
end;

procedure TSettingsOptions.Clean;
begin
  Display.Free;
  Processing.Free;
end;

{-------------------------------------------------------------------------------
  TSettingsDisplay
-------------------------------------------------------------------------------}

procedure TSettingsDisplay.Init;
begin
  SetOptionsLength(14);
  OptionString[1] := 'ToolbarIconSet';
  OptionString[5] := 'RealtimeUpdate';
  OptionString[6] := 'ResizeColsFiles';
  OptionString[7] := 'ShowFilesIcons';
  OptionString[8] := 'ForceFont';
  OptionString[9] := 'FontName';
  OptionString[10] := 'DropdownMax';
  OptionString[11] := 'DragDropNoAsk';
  OptionString[12] := 'LaunchFile';
  OptionString[13] := 'DropdownComplete';
  DefaultString[1] := '?';
  DefaultBoolean[5] := True;
  DefaultBoolean[6] := False;
  DefaultBoolean[7] := True;
  DefaultBoolean[8] := True;
  DefaultString[9] := 'Arial Unicode MS';
  DefaultInteger[10] := 20;
  DefaultBoolean[11] := False;
  DefaultBoolean[12] := False;
  DefaultBoolean[13] := False;
  DragDropOptions := TSettingsAddFolders.Create(FRoot, 'DragDropOptions');
end;

procedure TSettingsDisplay.Clean;
begin
  DragDropOptions.Free;
end;

{-------------------------------------------------------------------------------
  TSettingsProcessing
-------------------------------------------------------------------------------}

procedure TSettingsProcessing.Init;
begin
  SetOptionsLength(13);
  OptionString[0] := 'ForceDir';
  OptionString[1] := 'DetectAbsdir';
  OptionString[2] := 'FolderExt';
  OptionString[3] := 'Copy';
  OptionString[4] := 'GenerLog';
  OptionString[5] := 'StartedSwitch';
  OptionString[6] := 'StartedClearLog';
  OptionString[7] := 'FinishedSwitch';
  OptionString[8] := 'FinishedClearFiles';
  OptionString[9] := 'FinishedClearBatch';
  OptionString[10] := 'SaveLog';
  OptionString[11] := 'SaveLogAppend';
  OptionString[12] := 'SaveLogFile';
  DefaultBoolean[0] := True;
  DefaultBoolean[1] := True;
  DefaultBoolean[2] := False;
  DefaultBoolean[3] := False;
  DefaultBoolean[4] := True;
  DefaultInteger[5] := -1;
  DefaultBoolean[6] := True;
  DefaultInteger[7] := -1;
  DefaultBoolean[8] := False;
  DefaultBoolean[9] := False;
  DefaultBoolean[10] := False;
  DefaultBoolean[11] := True;
  DefaultString[12] := '';
end;

procedure TSettingsProcessing.Clean;
begin
end;

{-------------------------------------------------------------------------------
  TSettingsHints
-------------------------------------------------------------------------------}

procedure TSettingsHints.Init;
begin
  SetOptionsLength(1);
  OptionString[0] := 'ShowPreview';
  DefaultBoolean[0] := True;
end;

procedure TSettingsHints.Clean;
begin
end;

{-------------------------------------------------------------------------------
  TSettingsLastValues
-------------------------------------------------------------------------------}

procedure TSettingsLastValues.Init;
begin
  MRU := TSettingsMRU.Create(FRoot, 'MRU');
  ChangeExt := TSettingsChangeExt.Create(FRoot, 'ChangeExt');
  StrRepl := TSettingsStrRepl.Create(FRoot, 'StrRepl');
  MultstrRepl := TSettingsMultstrRepl.Create(FRoot, 'MultstrRepl');
  StrIns := TSettingsStrIns.Create(FRoot, 'StrIns');
  MoveStr := TSettingsMoveStr.Create(FRoot, 'MoveStr');
  CharDel := TSettingsCharDel.Create(FRoot, 'CharDel');
  Enum := TSettingsEnum.Create(FRoot, 'Enum');
  MP3 := TSettingsMP3.Create(FRoot, 'MP3');
  DateTime := TSettingsDateTime.Create(FRoot, 'DateTime');
  Random := TSettingsRandom.Create(FRoot, 'Random');
  ChangeCase := TSettingsChangeCase.Create(FRoot, 'ChangeCase');
  FromList := TSettingsFromList.Create(FRoot, 'FromList');
  Regexp := TSettingsRegexp.Create(FRoot, 'Regexp');
  Exif := TSettingsExif.Create(FRoot, 'Exif');
end;

procedure TSettingsLastValues.Clean;
begin
  MRU.Free;
  ChangeExt.Free;
  StrRepl.Free;
  MultstrRepl.Free;
  StrIns.Free;
  MoveStr.Free;
  CharDel.Free;
  Enum.Free;
  MP3.Free;
  DateTime.Free;
  Random.Free;
  ChangeCase.Free;
  FromList.Free;
  Regexp.Free;
end;

{-------------------------------------------------------------------------------
  TSettingsMRU
-------------------------------------------------------------------------------}

procedure TSettingsMRU.Init;
begin
  Batch := TSettingsList.Create(FRoot, 'BatchFile', 'Path');
end;

procedure TSettingsMRU.Clean;
begin
  Batch.Free;
end;

{-------------------------------------------------------------------------------
  TSettingsChangeExt
-------------------------------------------------------------------------------}

procedure TSettingsChangeExt.Init;
begin
  SetOptionsLength(2);
  OptionString[0] := 'NewExt';
  OptionString[1] := 'NoReplace';
  DefaultString[0] := 'ext';
  DefaultBoolean[1] := False;
  NewExtList := TSettingsList.Create(FRoot, 'NewExt', 'Value');
end;

procedure TSettingsChangeExt.Clean;
begin
  NewExtList.Free;
end;

{-------------------------------------------------------------------------------
  TSettingsStrRepl
-------------------------------------------------------------------------------}

procedure TSettingsStrRepl.Init;
begin
  SetOptionsLength(6);
  OptionString[0] := 'Search';
  OptionString[1] := 'Repl';
  OptionString[2] := 'AllOccurences';
  OptionString[3] := 'CaseSensitive';
  OptionString[4] := 'IncludeExt';
  OptionString[5] := 'OnlyExt';
  DefaultString[0] := 'old';
  DefaultString[1] := 'new';
  DefaultBoolean[2] := True;
  DefaultBoolean[3] := True;
  DefaultBoolean[4] := False;
  DefaultBoolean[5] := False;
  SearchList := TSettingsList.Create(FRoot, 'Search', 'Value');
  ReplList := TSettingsList.Create(FRoot, 'Repl', 'Value');
end;

procedure TSettingsStrRepl.Clean;
begin
  SearchList.Free;
  ReplList.Free;
end;

{-------------------------------------------------------------------------------
  TSettingsMultstrRepl
-------------------------------------------------------------------------------}

procedure TSettingsMultstrRepl.Init;
begin
  SetOptionsLength(3);
  OptionString[0] := 'LastSet';
  OptionString[1] := 'IncludeExt';
  OptionString[2] := 'CaseSensitive';
  DefaultString[0] := '';
  DefaultBoolean[1] := False;
  DefaultBoolean[2] := True;
  CurrentList := TSettingsCDATA.Create(FRoot, 'CurrentList');
  Sets := TSettingsCDATAList.Create(FRoot, 'Sets', 'Set', 'Name');
end;

procedure TSettingsMultstrRepl.Clean;
begin
  CurrentList.Free;
  Sets.Free;
end;

{-------------------------------------------------------------------------------
  TSettingsStrIns
-------------------------------------------------------------------------------}

procedure TSettingsStrIns.Init;
begin
  SetOptionsLength(4);
  OptionString[0] := 'Str';
  OptionString[1] := 'Pos';
  OptionString[2] := 'FromBegin';
  OptionString[3] := 'Ext';
  DefaultString[0] := 'new';
  DefaultInteger[1] := 0;
  DefaultBoolean[2] := True;
  DefaultBoolean[3] := False;
  StrList := TSettingsList.Create(FRoot, 'Str', 'Value');
end;

procedure TSettingsStrIns.Clean;
begin
  StrList.Free;
end;

{-------------------------------------------------------------------------------
  TSettingsMoveStr
-------------------------------------------------------------------------------}

procedure TSettingsMoveStr.Init;
begin
  SetOptionsLength(5);
  OptionString[0] := 'FromPos';
  OptionString[1] := 'FromBegin';
  OptionString[2] := 'Count';
  OptionString[3] := 'ToPos';
  OptionString[4] := 'ToBegin';
  DefaultInteger[0] := 0;
  DefaultBoolean[1] := True;
  DefaultInteger[2] := 1;
  DefaultInteger[3] := 0;
  DefaultBoolean[4] := True;
end;

procedure TSettingsMoveStr.Clean;
begin
end;

{-------------------------------------------------------------------------------
  TSettingsCharDel
-------------------------------------------------------------------------------}

procedure TSettingsCharDel.Init;
begin
  SetOptionsLength(6);
  OptionString[0] := 'Nb';
  OptionString[1] := 'AfterString';
  OptionString[2] := 'Pos';
  OptionString[3] := 'FromBegin';
  OptionString[4] := 'Str';
  OptionString[5] := 'Ext';
  DefaultInteger[0] := 1;
  DefaultInteger[1] := 0;
  DefaultInteger[2] := 0;
  DefaultBoolean[3] := True;
  DefaultString[4] := 'str';
  DefaultBoolean[5] := False;
  StrList := TSettingsList.Create(FRoot, 'Str', 'Value');
end;

procedure TSettingsCharDel.Clean;
begin
  StrList.Free;
end;

{-------------------------------------------------------------------------------
  TSettingsEnum
-------------------------------------------------------------------------------}

procedure TSettingsEnum.Init;
begin
  SetOptionsLength(5);
  OptionString[0] := 'Mask';
  OptionString[1] := 'Start';
  OptionString[2] := 'Digits';
  OptionString[3] := 'Incr';
  OptionString[4] := 'Restart';
  DefaultString[0] := 'File %num%%ext%';
  DefaultInteger[1] := 1;
  DefaultInteger[2] := 3;
  DefaultInteger[3] := 1;
  DefaultBoolean[4] := False;
  MaskList := TSettingsList.Create(FRoot, 'Mask', 'Value');
end;

procedure TSettingsEnum.Clean;
begin
  MaskList.Free;
end;

{-------------------------------------------------------------------------------
  TSettingsMP3
-------------------------------------------------------------------------------}

procedure TSettingsMP3.Init;
begin
  SetOptionsLength(2);
  OptionString[0] := 'Mask';
  OptionString[1] := 'TwoDigit';
  DefaultString[0] := '%author% - %title%%ext';
  DefaultBoolean[1] := False;
  MaskList := TSettingsList.Create(FRoot, 'Mask', 'Value');
end;

procedure TSettingsMP3.Clean;
begin
  MaskList.Free;
end;

{-------------------------------------------------------------------------------
  TSettingsDateTime
-------------------------------------------------------------------------------}

procedure TSettingsDateTime.Init;
begin
  SetOptionsLength(3);
  OptionString[0] := 'Mask';
  OptionString[1] := 'AddSuffix';
  OptionString[2] := 'WhichDate';
  DefaultString[0] := 'yyyy''-''mm''-''dde';
  DefaultBoolean[1] := False;
  DefaultInteger[2] := 1;
  MaskList := TSettingsList.Create(FRoot, 'Mask', 'Value');
end;

procedure TSettingsDateTime.Clean;
begin
  MaskList.Free;
end;

{-------------------------------------------------------------------------------
  TSettingsRandom
-------------------------------------------------------------------------------}

procedure TSettingsRandom.Init;
begin
  SetOptionsLength(2);
  OptionString[0] := 'Mask';
  OptionString[1] := 'Method';
  DefaultString[0] := '%random%%ext%';
  DefaultInteger[1] := 0;
  MaskList := TSettingsList.Create(FRoot, 'Mask', 'Value');
end;

procedure TSettingsRandom.Clean;
begin
  MaskList.Free;
end;

{-------------------------------------------------------------------------------
  TSettingsChangeCase
-------------------------------------------------------------------------------}

procedure TSettingsChangeCase.Init;
begin
  SetOptionsLength(5);
  OptionString[0] := 'Option';
  OptionString[1] := 'AfterChars';
  OptionString[2] := 'UseLocale';
  OptionString[3] := 'IncludeExt';
  OptionString[4] := 'OnlyExt';
  DefaultInteger[0] := 0;
  DefaultString[1] := '- .+(';
  DefaultBoolean[2] := True;
  DefaultBoolean[3] := False;
  DefaultBoolean[4] := False;
  CharsList := TSettingsList.Create(FRoot, 'Chars', 'Value');
end;

procedure TSettingsChangeCase.Clean;
begin
  CharsList.Free;
end;

{-------------------------------------------------------------------------------
  TSettingsFromList
-------------------------------------------------------------------------------}

procedure TSettingsFromList.Init;
begin
  SetOptionsLength(2);
  OptionString[0] := 'AppendExt';
  OptionString[1] := 'OnlyExt';
  DefaultBoolean[0] := False;
  DefaultBoolean[1] := False;
  ListContents := TSettingsCDATA.Create(FRoot, 'ListContents');
end;

procedure TSettingsFromList.Clean;
begin
  ListContents.Free;
end;

{-------------------------------------------------------------------------------
  TSettingsRegexp
-------------------------------------------------------------------------------}

procedure TSettingsRegexp.Init;
begin
  SetOptionsLength(2);
  OptionString[0] := 'Expr';
  OptionString[1] := 'Repl';
  DefaultString[0] := '';
  DefaultString[1] := '';
  ExprList := TSettingsList.Create(FRoot, 'Expr', 'Value');
  ReplList := TSettingsList.Create(FRoot, 'Repl', 'Value');
end;

procedure TSettingsRegexp.Clean;
begin
  ExprList.Free;
  ReplList.Free;
end;

{-------------------------------------------------------------------------------
  TSettingsExif
-------------------------------------------------------------------------------}

procedure TSettingsExif.Init;
begin
  SetOptionsLength(1);
  OptionString[0] := 'Mask';
  DefaultString[0] := '%datetime%%ext%';
  MaskList := TSettingsList.Create(FRoot, 'Mask', 'Value');
end;

procedure TSettingsExif.Clean;
begin
  MaskList.Free;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

end.

