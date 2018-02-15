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

unit Actions;

interface

uses
  Classes, SysUtils, Contnrs,

  TntSysUtils, TntClasses, JvSimpleXml, RegExpr, dEXIF,

  Files, ExtractID3;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

type

  TRenActionItem = (
    raiUnknown = -1,
    raiChangeExt = 0,
    raiStringRepl = 1,
    raiMultstrRepl = 2,
    raiStringInsert = 3,
    raiMoveString = 4,
    raiCharDel = 5,
    raiEnum = 6,
    raiMP3Tag = 7,
    raiDateTime = 8,
    raiRandom = 9,
    raiCase = 10,
    raiFromList = 11,
    raiRegexp = 12,
    raiExif = 13
  );

  TRenActionOperation = (
    raoRename, raoPreview, raoRevert
  );

  TRenAction = class(TObject)
  private
    FType:          TRenActionItem;
    FPosition:      Integer;
    FLastError:     TRenError;
    FFileName:      WideString;
    FIsFolder:      Boolean;
  protected
    function    GetTagValue(const ATag: WideString; out AValue: WideString): Boolean; virtual;
    function    MakeNewName(const AFileName: WideString; IsFolder: Boolean): WideString; virtual;
    procedure   Init; virtual;
    procedure   Finish; virtual;
    function    GetLastError: TRenError; virtual;
    procedure   SaveToFile(BatchNode: TJvSimpleXmlElem); virtual;
    procedure   LoadFromFile(ActionNode: TJvSimpleXmlElem); virtual;
  public
    constructor Create(AType: TRenActionItem);
    procedure   Perform(AList: TRenFiles; Operation: TRenActionOperation);
    property    ActionType: TRenActionItem read FType;
    property    Position: Integer read FPosition write FPosition;
    function    Description: WideString; virtual;
  end;

  TRenActionChangeExt = class(TRenAction)
  private
  protected
    function    MakeNewName(const AFileName: WideString; IsFolder: Boolean): WideString; override;
    procedure   SaveToFile(BatchNode: TJvSimpleXmlElem); override;
    procedure   LoadFromFile(ActionNode: TJvSimpleXmlElem); override;
  public
    Settings: record
      NewExtension: WideString;
      NoReplace: Boolean;
    end;
    function    Description: WideString; override;
  end;

  TRenActionStringRepl = class(TRenAction)
  private
  protected
    function    MakeNewName(const AFileName: WideString; IsFolder: Boolean): WideString; override;
    procedure   SaveToFile(BatchNode: TJvSimpleXmlElem); override;
    procedure   LoadFromFile(ActionNode: TJvSimpleXmlElem); override;
  public
    Settings:   record
      Search:     WideString;
      Replace:    WideString;
      All:        Boolean;
      CaseSens:   Boolean;
      InclExt:    Boolean;
      OnlyExt:    Boolean;
    end;
    function    Description: WideString; override;
  end;

  TRenActionMultstrRepl = class(TRenAction)
  private
  protected
    function    MakeNewName(const AFileName: WideString; IsFolder: Boolean): WideString; override;
    procedure   SaveToFile(BatchNode: TJvSimpleXmlElem); override;
    procedure   LoadFromFile(ActionNode: TJvSimpleXmlElem); override;
  public
    Settings:   record
      SetName:    WideString;
      SetChars:   WideString;
      IncludeExt: Boolean;
      CaseSens:   Boolean;
    end;
    function    Description: WideString; override;
  end;

  TRenActionStringIns = class(TRenAction)
  private
  protected
    function    MakeNewName(const AFileName: WideString; IsFolder: Boolean): WideString; override;
    procedure   SaveToFile(BatchNode: TJvSimpleXmlElem); override;
    procedure   LoadFromFile(ActionNode: TJvSimpleXmlElem); override;
  public
    Settings:   record
      NewString:  WideString;
      Position:   Integer;
      FromBegin:  Boolean;
      Ext:        Boolean;
    end;
    function    Description: WideString; override;
  end;

  TRenActionMoveStr = class(TRenAction)
  private
  protected
    function    MakeNewName(const AFileName: WideString; IsFolder: Boolean): WideString; override;
    procedure   SaveToFile(BatchNode: TJvSimpleXmlElem); override;
    procedure   LoadFromFile(ActionNode: TJvSimpleXmlElem); override;
  public
    Settings:   record
      FromPos:    Integer;
      FromBegin:  Boolean;
      Count:      Integer;
      ToPos:      Integer;
      ToBegin:    Boolean;
    end;
    function    Description: WideString; override;
  end;

  TRenActionCharDel = class(TRenAction)
  private
  protected
    function    MakeNewName(const AFileName: WideString; IsFolder: Boolean): WideString; override;
    procedure   SaveToFile(BatchNode: TJvSimpleXmlElem); override;
    procedure   LoadFromFile(ActionNode: TJvSimpleXmlElem); override;
  public
    Settings:   record
      NbChar:     Integer;
      AfterString:Integer;
      Position:   Integer;
      FromBegin:  Boolean;
      Str:        WideString;
      Ext:        Boolean;
    end;
    function    Description: WideString; override;
  end;

  TRenActionEnum = class(TRenAction)
  private
  protected
    function    GetTagValue(const ATag: WideString; out AValue: WideString): Boolean; override;
    function    MakeNewName(const AFileName: WideString; IsFolder: Boolean): WideString; override;
    procedure   SaveToFile(BatchNode: TJvSimpleXmlElem); override;
    procedure   Init; override;
    procedure   LoadFromFile(ActionNode: TJvSimpleXmlElem); override;
  public
    Settings:   record
      Mask:       WideString;
      StartAt:    Integer;
      Digits:     Integer;
      Increment:  Integer;
      Restart:    Boolean;
    end;
    CurPos:     Integer;
    PrevFolder: WideString;
    function    Description: WideString; override;
  end;

  TRenActionMP3 = class(TRenaction)
  private
    ID3Info:    TID3Info;
    DigitMask:  WideString;
  protected
    function    GetTagValue(const ATag: WideString; out AValue: WideString): Boolean; override;
    function    MakeNewName(const AFileName: WideString; IsFolder: Boolean): WideString; override;
    procedure   SaveToFile(BatchNode: TJvSimpleXmlElem); override;
    procedure   LoadFromFile(ActionNode: TJvSimpleXmlElem); override;
    procedure   Init; override;
  public
    Settings:   record
      Mask:       WideString;
      TwoDigit:   Boolean;
    end;
    function    Description: WideString; override;
  end;

  TRenActionDateTime = class(TRenAction)
  private
  protected
    function    MakeNewName(const AFileName: WideString; IsFolder: Boolean): WideString; override;
    procedure   SaveToFile(BatchNode: TJvSimpleXmlElem); override;
    procedure   LoadFromFile(ActionNode: TJvSimpleXmlElem); override;
  public
    Settings:   record
      Mask:       WideString;
      AddSuffix:  Boolean;
      WhichDate:  Integer;
    end;
    function    Description: WideString; override;
  end;

  TRenActionRandom = class(TRenAction)
  private
  protected
    function    GetTagValue(const ATag: WideString; out AValue: WideString): Boolean; override;
    function    MakeNewName(const AFileName: WideString; IsFolder: Boolean): WideString; override;
    procedure   SaveToFile(BatchNode: TJvSimpleXmlElem); override;
    procedure   LoadFromFile(ActionNode: TJvSimpleXmlElem); override;
  public
    Settings:   record
      Mask:       WideString;
      Method:     Integer;
    end;
    function    Description: WideString; override;
  end;

  TRenActionCase = class(TRenAction)
  private
  protected
    function    MakeNewName(const AFileName: WideString; IsFolder: Boolean): WideString; override;
    procedure   SaveToFile(BatchNode: TJvSimpleXmlElem); override;
    procedure   LoadFromFile(ActionNode: TJvSimpleXmlElem); override;
  public
    Settings:   record
      CaseType:   Integer;
      AfterChars: WideString;
      UseLocale:  Boolean;
      IncludeExt: Boolean;
      OnlyExt:    Boolean;
    end;
    function    Description: WideString; override;
  end;

  TRenActionFromList = class(TRenAction)
  private
    FListContents: TTntStringList;
  protected
    function    MakeNewName(const AFileName: WideString; IsFolder: Boolean): WideString; override;
    procedure   SaveToFile(BatchNode: TJvSimpleXmlElem); override;
    procedure   LoadFromFile(ActionNode: TJvSimpleXmlElem); override;
    procedure   Init; override;
    procedure   Finish; override;
  public
    Settings:   record
      ListContents: WideString;
      AppendExt:    Boolean;
      OnlyExt:      Boolean;
    end;
    function    Description: WideString; override;
  end;

  TRenActionRegexp = class(TRenAction)
  private
    FRegexp: TRegExpr;
    FCompileError: WideString;
  protected
    function    MakeNewName(const AFileName: WideString; IsFolder: Boolean): WideString; override;
    procedure   SaveToFile(BatchNode: TJvSimpleXmlElem); override;
    procedure   LoadFromFile(ActionNode: TJvSimpleXmlElem); override;
    procedure   Init; override;
    procedure   Finish; override;
  public
    Settings:   record
      Expr:       WideString;
      Repl:       WideString;
    end;
    function    Description: WideString; override;
  end;

  TRenActionExif = class(TRenAction)
  private
    FImgData:   TImgData;
  protected
    function    GetTagValue(const ATag: WideString; out AValue: WideString): Boolean; override;
    function    MakeNewName(const AFileName: WideString; IsFolder: Boolean): WideString; override;
    procedure   SaveToFile(BatchNode: TJvSimpleXmlElem); override;
    procedure   LoadFromFile(ActionNode: TJvSimpleXmlElem); override;
    procedure   Init; override;
    procedure   Finish; override;
  public
    Settings:   record
      Mask:         WideString;
    end;
    function    Description: WideString; override;
  end;

  (*
  TRenActionShiftDT = class(TRenAction)
  private
  protected
    function    MakeNewName(const AFileName: WideString; IsFolder: Boolean): WideString; override;
    procedure   SaveToFile(BatchNode: TJvSimpleXmlElem); override;
    procedure   LoadFromFile(ActionNode: TJvSimpleXmlElem); override;
    procedure   Init; override;
    procedure   Finish; override;
  public
    Settings:   record
      TimePos:    Integer;
      TimeAdd:    Integer;
      DatePos:    Integer;
      DateAdd:    Integer;
    end;
    function    Description: WideString; override;
  end;
  *)

  TRenActions = class(TObjectList)
  protected
  public
    class function  CreateAction(AAction: TRenActionItem): TRenAction;
    function        Add(AAction: TRenActionItem): TRenAction; reintroduce;
    procedure       Perform(AList: TRenFiles; Operation: TRenActionOperation);
    procedure       Sort; reintroduce;
    procedure       SaveToFile(const AFileName: TWideFileName);
    procedure       LoadFromFile(const AFileName: TWideFileName);
  end;

  TActionNode = record
    RenAction: TRenAction;
  end;
  PActionNode = ^TActionNode;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

implementation

uses
  Windows, ComObj, StrUtils,

  TntWideStrUtils,

  thread, Global, VarMessages, ConstValues,
  functions_files, functions_str, functions_strformat, dIPTC;

{-------------------------------------------------------------------------------
  TRenActions
-------------------------------------------------------------------------------}

class function TRenActions.CreateAction(AAction: TRenActionItem): TRenAction;
begin
  case AAction of
    raiChangeExt:
      Result := TRenActionChangeExt.Create(AAction);
    raiStringRepl:
      Result := TRenActionStringRepl.Create(AAction);
    raiMultstrRepl:
      Result := TRenActionMultstrRepl.Create(AAction);
    raiStringInsert:
      Result := TRenActionStringIns.Create(AAction);
    raiMoveString:
      Result := TRenActionMoveStr.Create(AAction);
    raiCharDel:
      Result := TRenActionCharDel.Create(AAction);
    raiEnum:
      Result := TRenActionEnum.Create(AAction);
    raiMP3Tag:
      Result := TRenActionMP3.Create(AAction);
    raiDateTime:
      Result := TRenActionDateTime.Create(AAction);
    raiRandom:
      Result := TRenActionRandom.Create(AAction);
    raiCase:
      Result := TRenActionCase.Create(AAction);
    raiFromList:
      Result := TRenActionFromList.Create(AAction);
    raiRegexp:
      Result := TRenActionRegexp.Create(AAction);
    raiExif:
      Result := TRenActionExif.Create(AAction);
      (*
    raiShiftDT:
      Result := TRenActionShiftDT.Create(AAction);
      *)
  else
    Result := TRenAction.Create(raiUnknown);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenActions.Add(AAction: TRenActionItem): TRenAction;
begin
  Result := CreateAction(AAction);
  inherited Add(Result);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActions.Perform(AList: TRenFiles; Operation: TRenActionOperation);
var
  i: Integer;
  RevertAction: TRenAction;
begin
  for i := 0 to AList.Count-1 do
  begin
    TRenFile(AList.Items[i]).InitBeforeJob;
  end;
  if Operation = raoRevert then
  begin
    RevertAction := TRenAction.Create(raiUnknown);
    try
      RevertAction.Perform(AList, raoRevert);
    finally
      RevertAction.Free;
    end;
  end
  else
    for i := 0 to Count-1 do
    begin
      if GlobalStop then
        Break;
      TRenAction(Items[i]).Perform(AList, Operation);
    end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function RenActionsSort(Item1, Item2: Pointer): Integer;
begin
  Result := TRenAction(Item1).Position - TRenAction(Item2).Position;
end;

procedure TRenActions.Sort;
begin
  inherited Sort(RenActionsSort);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActions.SaveToFile(const AFileName: TWideFileName);
var
  i: Integer;
  BatchRoot: TJvSimpleXmlElem;
  xmlh: TJvSimpleXmlElemHeader;
begin
  with TJvSimpleXml.Create(nil) do
    try
      Prolog.Clear;
      xmlh := TJvSimpleXmlElemHeader.Create;
      xmlh.Encoding := 'UTF-8';
      Prolog.Add(xmlh);
      Root.Name := 'AntRenamer';
      Root.Properties.Add('Version', strVersion);
      Root.Properties.Add('Date', DateToStr(Date));
      BatchRoot := Root.Items.Add('Batch');
      for i := 0 to Self.Count-1 do
        (Items[i] as TRenAction).SaveToFile(BatchRoot);
      SaveToFile(AFileName);
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActions.LoadFromFile(const AFileName: TWideFileName);
var
  i: Integer;
  CurItem: TJvSimpleXmlElem;
begin
  with TJvSimpleXml.Create(nil) do
    try
      LoadFromFile(AFileName);
      Self.Clear;
      if Root.Name = 'AntRenamer' then
        if (Root.Items.Count > 0) and (Root.Items.Item[0].Name = 'Batch') then
          with Root.Items.Item[0] do
            for i := 0 to Items.Count-1 do
            begin
              CurItem := Items.Item[i];
              with CurItem do
              begin
                if Name = 'ChangeExt' then
                  Self.Add(raiChangeExt).LoadFromFile(CurItem)
                else if Name = 'StrRepl' then
                  Self.Add(raiStringRepl).LoadFromFile(CurItem)
                else if Name = 'MultstrRepl' then
                  Self.Add(raiMultstrRepl).LoadFromFile(CurItem)
                else if Name = 'StrIns' then
                  Self.Add(raiStringInsert).LoadFromFile(CurItem)
                else if Name = 'MoveStr' then
                  Self.Add(raiMoveString).LoadFromFile(CurItem)
                else if Name = 'CharDel' then
                  Self.Add(raiCharDel).LoadFromFile(CurItem)
                else if Name = 'Enum' then
                  Self.Add(raiEnum).LoadFromFile(CurItem)
                else if Name = 'MP3' then
                  Self.Add(raiMP3Tag).LoadFromFile(CurItem)
                else if Name = 'DateTime' then
                  Self.Add(raiDateTime).LoadFromFile(CurItem)
                else if Name = 'Random' then
                  Self.Add(raiRandom).LoadFromFile(CurItem)
                else if Name = 'ChangeCase' then
                  Self.Add(raiCase).LoadFromFile(CurItem)
                else if Name = 'FromList' then
                  Self.Add(raiFromList).LoadFromFile(CurItem)
                else if Name = 'Regexp' then
                  Self.Add(raiRegexp).LoadFromFile(CurItem)
                else if Name = 'Exif' then
                  Self.Add(raiExif).LoadFromFile(CurItem)
                  (*
                else if Name = 'ShiftDT' then
                  Self.Add(raiShiftDT).LoadFromFile(CurItem)
                  *)
              end;
            end;
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
  TRenAction
-------------------------------------------------------------------------------}

constructor TRenAction.Create(AType: TRenActionItem);
begin
  FType := AType;
  with FLastError do
  begin
    Description := '';
    Status := rsNotRenamed;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenAction.Description: WideString;
begin
  Result := stradUnknown;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenAction.GetLastError: TRenError;
begin
  Result := FLastError;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenAction.Init;
begin
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenAction.Finish;
begin
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenAction.GetTagValue(const ATag: WideString; out AValue: WideString): Boolean;
var
  N: Integer;
  CurPath: WideString;
begin
  AValue := '';
  Result := False;
  if ATag = 'name' then
  begin
    AValue := WideChangeFileExt(WideExtractFileName(FFileName), '');
    Result := True;
  end
  else
  if ATag = 'ext' then
  begin
    if not (FIsFolder and not GlobalSettings.FolderExt) then
      AValue := WideExtractFileExt(FFileName)
    else
      AValue := '';
    Result := True;
  end
  else
  if StartsStr('folder', ATag) then
  begin
    CurPath := FFileName;
    N := StrToIntDef(Copy(ATag, Length('folder') + 1, MaxInt), 0);
    if N < 0 then
    begin
      if StartsStr('\\', CurPath) then
        Delete(CurPath, 1, 2);
      while N < 0 do
      begin
        Split(CurPath, '\', AValue, CurPath, False);
        Inc(N);
      end;
      AValue := Tnt_WideStringReplace(AValue, ':', '', []); // for drive letter, in case N = -1 
    end
    else
    begin
      while N > 0 do
      begin
        CurPath := WideExcludeTrailingBackslash(WideExtractFilePath(CurPath));
        Dec(N);
      end;
      AValue := WideExtractFileName(CurPath);
    end;
    Result := True;
  end
  else
  if StartsStr('count', ATag) then
  begin
    AValue := IntToStr(CountFolderContents(WideIncludeTrailingPathDelimiter(FFileName), not StartsStr('countfiles', ATag), not StartsStr('countfolders', ATag), EndsStr('r', ATag)));
    Result := True;
  end
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenAction.MakeNewName(const AFileName: WideString; IsFolder: Boolean): WideString;
begin
  FFileName := AFileName;
  FIsFolder := IsFolder;
  Result := '';
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenAction.Perform(AList: TRenFiles; Operation: TRenActionOperation);
var
  i: Integer;
  NewName: WideString;
begin
  if Operation <> raoRevert then
    Init;
  try
    with AList do
      for i := 0 to Count-1 do
        with TRenFile(Items[i]) do
        begin

          if GlobalStop then
            Break;

          if (Operation <> raoPreview) then
          begin
            if not Exists then
            begin
              FLastError.Description := WideFormat(strfsNotRenamed, [FilePath, strfs404]);
              FLastError.Status := rsError;
            end
            else
              if Operation = raoRename then
                NewName := MakeNewName(FilePath, IsFolder);
            if FLastError.Status <> rsNotRenamed then
            begin
              SetError(FLastError.Description, rsNotRenamed);
              FLastError.Status := rsNotRenamed;
              FLastError.Description := '';
            end
            else
              case Operation of
                raoRename:
                  Rename(NewName);
                raoRevert:
                  Revert;
              end;
          end
          else
          begin
            SetError('', rsNotRenamed);
            if Preview = '' then
              Preview := FileName;
            Preview := MakeNewName(FileFolder + Preview, IsFolder);
          end;

          Refresh(Items[i] as TRenFile, Self.Position, i);
        end;
  finally
    Finish;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenAction.SaveToFile(BatchNode: TJvSimpleXmlElem);
begin
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenAction.LoadFromFile(ActionNode: TJvSimpleXmlElem);
begin
end;

{-------------------------------------------------------------------------------
  TRenActionChangeExt
-------------------------------------------------------------------------------}

function TRenActionChangeExt.Description: WideString;
begin
  if Settings.NoReplace then
  begin
    if Settings.NewExtension = '' then
      Result := stradUnknown
    else
      Result := WideFormat(stradChangeExtNoRepl, [Settings.NewExtension]);
  end
  else
  begin
    if Settings.NewExtension = '' then
      Result := stradChangeExtRmv
    else
      Result := WideFormat(stradChangeExt, [Settings.NewExtension]);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenActionChangeExt.MakeNewName(const AFileName: WideString; IsFolder: Boolean): WideString;
begin
  inherited MakeNewName(AFileName, IsFolder);
  Result := WideExtractFileName(AFileName);
  if IsFolder and not GlobalSettings.FolderExt then
    Exit;
  if not Settings.NoReplace then
    Result := WideChangeFileExt(Result, Settings.NewExtension)
  else
    Result := Result + Settings.NewExtension
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionChangeExt.SaveToFile(BatchNode: TJvSimpleXmlElem);
begin
  with BatchNode.Items.Add('ChangeExt') do
  begin
    Properties.Add('NewExt', UTF8Encode(Settings.NewExtension));
    Properties.Add('NoReplace', Settings.NoReplace);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionChangeExt.LoadFromFile(ActionNode: TJvSimpleXmlElem);
begin
  Settings.NewExtension := UTF8Decode(ActionNode.Properties.Value('NewExt', 'ext'));
  Settings.NoReplace := ActionNode.Properties.BoolValue('NoReplace', False);
end;

{-------------------------------------------------------------------------------
  TRenActionStringRepl
-------------------------------------------------------------------------------}

function TRenActionStringRepl.Description: WideString;
begin
  Result := WideFormat(stradStringRepl, [Settings.Search, Settings.Replace]);
  if Settings.All then
    Result := Result + stradStringReplAll;
  if Settings.CaseSens then
    Result := Result + stradStringReplCase;
  if Settings.OnlyExt then
    Result := Result + stradOnlyExt
  else
    if Settings.InclExt then
      Result := Result + stradInclExt;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenActionStringRepl.MakeNewName(const AFileName: WideString; IsFolder: Boolean): WideString;
var
  Opt: TReplaceFlags;
  OldName: WideString;
begin
  inherited MakeNewName(AFileName, IsFolder);
  if Settings.All then
    Include(Opt, rfReplaceAll);
  if not Settings.CaseSens then
    Include(Opt, rfIgnoreCase);
  if Settings.OnlyExt then
  begin
    if IsFolder and not GlobalSettings.FolderExt then
    begin
      Result := WideExtractFileName(AFileName);
      Exit;
    end;
    OldName := Copy(WideExtractFileExt(AFileName), 2, MaxInt);
  end else
  begin
    if Settings.InclExt or (IsFolder and not GlobalSettings.FolderExt)  then
      OldName := WideExtractFileName(AFileName)
    else
      OldName := WideChangeFileExt(WideExtractFileName(AFileName), '');
  end;
  Result := Tnt_WideStringReplace(OldName, Settings.Search, Settings.Replace, Opt);
  if Settings.OnlyExt then
    Result := WideChangeFileExt(WideExtractFileName(AFileName), '.' + Result)
  else
    if not Settings.InclExt then
      Result := Result + WideExtractFileExt(AFileName);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionStringRepl.SaveToFile(BatchNode: TJvSimpleXmlElem);
begin
  with BatchNode.Items.Add('StrRepl') do
  begin
    Properties.Add('Search', UTF8Encode(Settings.Search));
    Properties.Add('Repl', UTF8Encode(Settings.Replace));
    Properties.Add('AllOccurences', Settings.All);
    Properties.Add('CaseSensitive', Settings.CaseSens);
    Properties.Add('IncludeExt', Settings.InclExt);
    Properties.Add('OnlyExt', Settings.OnlyExt);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionStringRepl.LoadFromFile(ActionNode: TJvSimpleXmlElem);
begin
  Settings.Search := UTF8Decode(ActionNode.Properties.Value('Search', 'old'));
  Settings.Replace := UTF8Decode(ActionNode.Properties.Value('Repl', 'new'));
  Settings.All := ActionNode.Properties.BoolValue('AllOccurences', True);
  Settings.CaseSens := ActionNode.Properties.BoolValue('CaseSensitive', True);
  Settings.InclExt := ActionNode.Properties.BoolValue('IncludeExt', False);
  Settings.OnlyExt := ActionNode.Properties.BoolValue('OnlyExt', False);
end;

{-------------------------------------------------------------------------------
  TRenActionMultstrRepl
-------------------------------------------------------------------------------}

function TRenActionMultstrRepl.Description: WideString;
begin
  Result := WideFormat(stradMultstrRepl, [Settings.SetName]);
  if Settings.IncludeExt then
    Result := Result + stradInclExt;
  if Settings.CaseSens then
    Result := Result + stradStringReplCase;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenActionMultstrRepl.MakeNewName(const AFileName: WideString; IsFolder: Boolean): WideString;
var
  i: Integer;
  Chars: TTntStringList;
  Source, Dest: WideString;
  Opt: TReplaceFlags;
  IncludeExt: Boolean;
begin
  inherited MakeNewName(AFileName, IsFolder);
  Result := WideExtractFileName(AFileName);
  IncludeExt := Settings.IncludeExt;
  if IsFolder and not GlobalSettings.FolderExt then
    IncludeExt := True;
  if not IncludeExt then
    Result := WideChangeFileExt(Result, '');
  Opt := [rfReplaceAll];
  if not Settings.CaseSens then
    Include(Opt, rfIgnoreCase);
  Chars := TTntStringList.Create;
  try
    Chars.Text := Settings.SetChars;
    for i := 0 to Chars.Count-1 do
    begin
      Source := Copy(Chars[i], 1, Pos(#9, Chars[i])-1);
      Dest := Copy(Chars[i], Pos(#9, Chars[i])+1, Length(Chars[i]));
      if Source = '' then
        Continue;
      if Settings.CaseSens and (Length(Source) = 1) and (Length(Dest) = 1) then
        Result := WideCharReplace(Result, Source[1], Dest[1])
      else
        Result := Tnt_WideStringReplace(Result, Source, Dest, Opt);
    end;
  finally
    Chars.Free;
  end;
  if not IncludeExt then
    Result := Result + WideExtractFileExt(AFileName);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionMultstrRepl.SaveToFile(BatchNode: TJvSimpleXmlElem);
begin
 with BatchNode.Items.Add('MultstrRepl') do
 begin
   Properties.Add('SetName', UTF8Encode(Settings.SetName));
   Properties.Add('IncludeExt', Settings.IncludeExt);
   Properties.Add('CaseSensitive', Settings.CaseSens);
   Items.Add('SetChars').Items.AddCData('', UTF8Encode(Settings.SetChars));
 end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionMultstrRepl.LoadFromFile(ActionNode: TJvSimpleXmlElem);
var
  Found: TJvSimpleXmlElem;
begin
  Settings.SetName := UTF8Decode(ActionNode.Properties.Value('SetName', ''));
  Settings.IncludeExt := ActionNode.Properties.BoolValue('IncludeExt', False);
  Settings.CaseSens := ActionNode.Properties.BoolValue('CaseSensitive', True);
  Found := ActionNode.Items.ItemNamed['SetChars'];
  if (Found <> nil) and (Found.Items.Count > 0) and (Found.Items.Item[0] is TJvSimpleXmlElemCData) then
    Settings.SetChars := UTF8Decode((Found.Items.Item[0] as TJvSimpleXmlElemCData).Value)
  else
    Settings.SetChars := '';
end;

{-------------------------------------------------------------------------------
  TRenActionStringIns
-------------------------------------------------------------------------------}

function TRenActionStringIns.Description: WideString;
begin
  Result := WideFormat(stradStringIns, [Settings.NewString, Settings.Position]);
  if Settings.FromBegin then
    Result := Result + stradFromBegin
  else
    Result := Result + stradFromEnd;
  if Settings.Ext then
    Result := Result + stradOnlyExt;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenActionStringIns.MakeNewName(const AFileName: WideString; IsFolder: Boolean): WideString;
var
  InsPos: Integer;
begin
  inherited MakeNewName(AFileName, IsFolder);
  if not (IsFolder and not GlobalSettings.FolderExt) then
  begin
    if Settings.Ext then
      Result := Copy(WideExtractFileExt(AFileName), 2, MaxInt)
    else
      Result := WideChangeFileExt(WideExtractFileName(AFileName), '');
  end
  else
  begin
    Result := WideExtractFileName(AFileName);
    if Settings.Ext then
      Exit;
  end;
  if Settings.FromBegin then
    InsPos := Settings.Position + 1
  else
    InsPos := Length(Result) - Settings.Position + 1;
  Insert(AdvancedFormat(Settings.NewString, GetTagValue), Result, InsPos);
  if not (IsFolder and not GlobalSettings.FolderExt) then
  begin
    if Settings.Ext then
      Result := WideChangeFileExt(WideExtractFileName(AFileName), '.' + Result)
    else
      Result := WideFormat('%s%s', [Result, WideExtractFileExt(AFileName)]);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionStringIns.SaveToFile(BatchNode: TJvSimpleXmlElem);
begin
  with BatchNode.Items.Add('StrIns') do
  begin
    Properties.Add('Str', UTF8Encode(Settings.NewString));
    Properties.Add('Pos', Settings.Position);
    Properties.Add('FromBegin', Settings.FromBegin);
    Properties.Add('Ext', Settings.Ext);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionStringIns.LoadFromFile(ActionNode: TJvSimpleXmlElem);
begin
  Settings.NewString := UTF8Decode(ActionNode.Properties.Value('Str', 'new'));
  Settings.Position := ActionNode.Properties.IntValue('Pos', 0);
  Settings.FromBegin := ActionNode.Properties.BoolValue('FromBegin', True);
  Settings.Ext := ActionNode.Properties.BoolValue('Ext', False);
end;

{-------------------------------------------------------------------------------
  TRenActionMoveStr 
-------------------------------------------------------------------------------}

function TRenActionMoveStr.Description: WideString;
begin
  Result := WideFormat(stradMoveStr, [Settings.Count, Settings.FromPos, IfThen(Settings.FromBegin, stradFromBegin, stradFromEnd), Settings.ToPos, IfThen(Settings.ToBegin, stradFromBegin, stradFromEnd)]);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenActionMoveStr.MakeNewName(const AFileName: WideString; IsFolder: Boolean): WideString;
var
  Moved: WideString;
  RealPos: Integer;
begin
  inherited MakeNewName(AFileName, IsFolder);
  if not (IsFolder and not GlobalSettings.FolderExt) then
    Result := WideChangeFileExt(WideExtractFileName(AFileName), '')
  else
    Result := WideExtractFileName(AFileName);
  if Settings.FromBegin then
    RealPos := Settings.FromPos + 1
  else
    RealPos := Length(Result) - Settings.FromPos - Settings.Count + 1;
  Moved := Copy(Result, RealPos, Settings.Count);
  Delete(Result, RealPos, Settings.Count);
  if Settings.ToBegin then
    RealPos := Settings.ToPos + 1
  else
    RealPos := Length(Result) - Settings.ToPos + 1;
  Insert(Moved, Result, RealPos);
  if not (IsFolder and not GlobalSettings.FolderExt) then
    Result := Result + WideExtractFileExt(AFileName);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionMoveStr.SaveToFile(BatchNode: TJvSimpleXmlElem);
begin
  with BatchNode.Items.Add('MoveStr') do
  begin
    Properties.Add('FromPos', Settings.FromPos);
    Properties.Add('FromBegin', Settings.FromBegin);
    Properties.Add('Count', Settings.Count);
    Properties.Add('ToPos', Settings.ToPos);
    Properties.Add('ToBegin', Settings.ToBegin);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionMoveStr.LoadFromFile(ActionNode: TJvSimpleXmlElem);
begin
  Settings.FromPos := ActionNode.Properties.IntValue('FromPos', 0);
  Settings.FromBegin := ActionNode.Properties.BoolValue('FromBegin', True);
  Settings.Count := ActionNode.Properties.IntValue('Count', 1);
  Settings.ToPos := ActionNode.Properties.IntValue('ToPos', 0);
  Settings.ToBegin := ActionNode.Properties.BoolValue('ToBegin', True);
end;

{-------------------------------------------------------------------------------
  TRenActionCharDel
-------------------------------------------------------------------------------}

function TRenActionCharDel.Description: WideString;
begin
  Result := WideFormat(stradCharDel, [Settings.Nbchar]);
  case Settings.AfterString of
    1:  Result := Result + WideFormat(stradCharDelStr, [Settings.Str]);
    2:  Result := Result + WideFormat(stradCharDelStr2, [Settings.Str]);
  else
    Result := Result + WideFormat(stradCharDelPos, [Settings.Position]);
    if Settings.FromBegin then
      Result := Result + stradFromBegin
    else
      Result := Result + stradFromEnd;
  end;
  if Settings.Ext then
    Result := Result + stradOnlyExt;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenActionCharDel.MakeNewName(const AFileName: WideString; IsFolder: Boolean): WideString;
var
  DelPos, DelLength: Integer;
begin
  inherited MakeNewName(AFileName, IsFolder);
  if not (IsFolder and not GlobalSettings.FolderExt) then
  begin
    if Settings.Ext then
      Result := Copy(WideExtractFileExt(AFileName), 2, MaxInt)
    else
      Result := WideChangeFileExt(WideExtractFileName(AFileName), '');
  end
  else
  begin
    Result := WideExtractFileName(AFileName);
    if Settings.Ext then
      Exit;
  end;
  case Settings.AfterString of
    1:
      if Pos(Settings.Str, Result) > 0 then
        Delete(Result, Pos(Settings.Str, Result) + Length(Settings.Str), Settings.NbChar);
    2:
      if Pos(Settings.Str, Result) > 0 then
      begin
        DelPos := Pos(Settings.Str, Result) - Settings.NbChar;
        if DelPos < 1 then
        begin
          DelLength := Settings.NbChar - 1 + DelPos;
          DelPos := 1;
        end
        else
          DelLength := Settings.NbChar;
        Delete(Result, DelPos, DelLength);
      end;
  else
    if Settings.FromBegin then
      DelPos := Settings.Position + 1
    else
      DelPos := Length(Result) - Settings.Position - Settings.NbChar + 1;
    Delete(Result, DelPos, Settings.NbChar);
  end;
  if not (IsFolder and not GlobalSettings.FolderExt) then
  begin
    if Settings.Ext then
      Result := WideChangeFileExt(WideExtractFileName(AFileName), '.' + Result)
    else
      Result := WideFormat('%s%s', [Result, WideExtractFileExt(AFileName)]);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionCharDel.SaveToFile(BatchNode: TJvSimpleXmlElem);
begin
  with BatchNode.Items.Add('CharDel') do
  begin
    Properties.Add('Nb', Settings.NbChar);
    Properties.Add('AfterString', Settings.AfterString);
    Properties.Add('Pos', Settings.Position);
    Properties.Add('FromBegin', Settings.FromBegin);
    Properties.Add('Str', UTF8Encode(Settings.Str));
    Properties.Add('Ext', Settings.Ext);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionCharDel.LoadFromFile(ActionNode: TJvSimpleXmlElem);
begin
  Settings.NbChar := ActionNode.Properties.IntValue('Nb', 1);
  Settings.AfterString := ActionNode.Properties.IntValue('AfterString', 0);
  Settings.Position := ActionNode.Properties.IntValue('Pos', 0);
  Settings.FromBegin := ActionNode.Properties.BoolValue('FromBegin', True);
  Settings.Str := UTF8Decode(ActionNode.Properties.Value('Str', 'str'));
  Settings.Ext := ActionNode.Properties.BoolValue('Ext', False);
end;

{-------------------------------------------------------------------------------
  TRenActionEnum
-------------------------------------------------------------------------------}

function TRenActionEnum.Description: WideString;
begin
  with Settings do
    Result := WideFormat(stradEnum, [Mask, StartAt, Digits, Increment]);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionEnum.Init;
begin
  CurPos := Settings.StartAt;
  PrevFolder := '';
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenActionEnum.GetTagValue(const ATag: WideString; out AValue: WideString): Boolean;
begin
  if ATag = 'num' then
  begin
    AValue := WideFormat('%.' + IntToStr(Settings.Digits) + 'd', [CurPos]);
    Result := True;
  end
  else
    Result := inherited GetTagValue(ATag, AValue);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenActionEnum.MakeNewName(const AFileName: WideString; IsFolder: Boolean): WideString;
var
  CurFolder: WideString;
begin
  inherited MakeNewName(AFileName, IsFolder);
  if Settings.Restart then
  begin
    CurFolder := WideExtractFilePath(AFileName);
    if CurFolder <> PrevFolder then
    begin
      CurPos := Settings.StartAt;
      PrevFolder := CurFolder;
    end;
  end;
  Result := AdvancedFormat(Settings.Mask, GetTagValue);
  CurPos := CurPos + Settings.Increment;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionEnum.SaveToFile(BatchNode: TJvSimpleXmlElem);
begin
  with BatchNode.Items.Add('Enum') do
  begin
    Properties.Add('Mask', UTF8Encode(Settings.Mask));
    Properties.Add('Start', Settings.StartAt);
    Properties.Add('Digits', Settings.Digits);
    Properties.Add('Incr', Settings.Increment);
    Properties.Add('Restart', Settings.Increment);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionEnum.LoadFromFile(ActionNode: TJvSimpleXmlElem);
begin
  Settings.Mask := UTF8Decode(ActionNode.Properties.Value('Mask', 'File %num%%ext%'));
  Settings.StartAt := ActionNode.Properties.IntValue('Start', 1);
  Settings.Digits := ActionNode.Properties.IntValue('Digits', 3);
  Settings.Increment := ActionNode.Properties.IntValue('Incr', 1);
  Settings.Restart := ActionNode.Properties.BoolValue('Restart', False);
end;

{-------------------------------------------------------------------------------
  TRenActionMP3
-------------------------------------------------------------------------------}

function TRenActionMP3.Description: WideString;
begin
  Result := WideFormat(stradMP3, [Settings.Mask]);
  if Settings.TwoDigit then
    Result := Result + stradMP3TwoDigit;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenActionMP3.GetTagValue(const ATag: WideString; out AValue: WideString): Boolean;
begin
  if ATAg = 'author' then
  begin
    AValue := TrimRight(ID3Info.Author);
    Result := True;
  end
  else
  if ATag = 'title' then
  begin
    AValue := TrimRight(ID3Info.Title);
    Result := True;
  end
  else
  if ATag = 'album' then
  begin
    AValue := TrimRight(ID3Info.Album);
    Result := True;
  end
  else
  if ATag = 'year' then
  begin
    AValue := TrimRight(ID3Info.Year);
    Result := True;
  end
  else
  if ATag = 'genre' then
  begin
    if ID3Info.Genre <> $FF then
      AValue := ID3Genres[Integer(ID3Info.Genre)]
    else
      AValue := '';
    Result := True;
  end
  else
  if ATag = 'comm' then
  begin
    AValue := TrimRight(ID3Info.Comment);
    Result := True;
  end
  else
  if ATag = 'track' then
  begin
    if ID3Info.Track <> $00 then
      AValue := Format(DigitMask, [Integer(ID3Info.Track)])
    else
      AValue := '';
    Result := True;
  end
  else
    Result := inherited GetTagValue(ATag, AValue);
  AValue := ValidateFileName(AValue);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenActionMP3.MakeNewName(const AFileName: WideString; IsFolder: Boolean): WideString;
begin;
  Result := inherited MakeNewName(AFileName, IsFolder);
  try
    ExtractID3Info(AFileName, ID3Info);
    Result := AdvancedFormat(Settings.Mask, GetTagValue, [afoAllowIf,afoAllowQuotes]);
  except
    on E: Exception do
      with FLastError do
      begin
        Description := WideFormat(strfsNotRenamed, [AFileName, E.Message]);
        Status := rsInfo;
      end;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionMP3.Init;
begin
  if Settings.TwoDigit then
    DigitMask := '%.2d'
  else
    DigitMask := '%d';
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionMP3.SaveToFile(BatchNode: TJvSimpleXmlElem);
begin
  with BatchNode.Items.Add('MP3') do
  begin
    Properties.Add('Mask', UTF8Encode(Settings.Mask));
    Properties.Add('TwoDigit', Settings.TwoDigit);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionMP3.LoadFromFile(ActionNode: TJvSimpleXmlElem);
begin
  Settings.Mask := UTF8Decode(ActionNode.Properties.Value('Mask', '%author% - %title%%ext'));
  Settings.TwoDigit := ActionNode.Properties.BoolValue('TwoDigit', False);
end;

{-------------------------------------------------------------------------------
  TRenActionDateTime
-------------------------------------------------------------------------------}

function TRenActionDateTime.Description: WideString;
begin
  Result := WideFormat(stradDateTime, [Settings.Mask]);
  case Settings.WhichDate of
    0:  Result := Result + stradDateTimeCreat;
    1:  Result := Result + stradDateTimeModif;
  end;
  if Settings.AddSuffix then
    Result := Result + stradDateTimeSuffix;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenActionDateTime.MakeNewName(const AFileName: WideString; IsFolder: Boolean): WideString;
var
  Inside: Boolean;
  i{, FileDate}: Integer;
  FileDate, DateCreated, DateAccess, DateModified: TDateTime;
begin
  Result := inherited MakeNewName(AFileName, IsFolder);
{
  FileDate := FileAge(AFileName);
  if FileDate < 0 then
    FileDate := DateTimeToFileDate(Now);
}
  if GetFileDates(AFileName, DateCreated, DateModified, DateAccess) then
  begin
    if Settings.WhichDate = 0 then
      FileDate := DateCreated
    else
      FileDate := DateModified;
  end
  else
    FileDate := Now;
  Inside := False;
  for i := 1 to Length(Settings.Mask) do
  begin
    if (not inside) and (Settings.Mask[i] = 'f') then
    begin
      Result := Result + WideFormat('"%s"', [WideChangeFileExt(WideExtractFileName(AFileName), '')]);
    end else
    if (not inside) and (Settings.Mask[i] = 'e') then
    begin
      if not (IsFolder and not GlobalSettings.FolderExt) then
        Result := Result + WideFormat('"%s"', [WideExtractFileExt(AFileName)]);
    end else
    begin
      if Settings.Mask[i] = '"' then
        Inside := not Inside;
      Result := Result + Settings.Mask[i];
    end;
  end;
  Result := FormatDateTime(Result, FileDate); // looses non-ANSI characters, but there is no unicode version of FormatDateTime
  if Settings.AddSuffix then
    Result := WideExtractFileName(MakeUniqueFileName(WideExtractFilePath(AFileName) + Result));
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionDateTime.SaveToFile(BatchNode: TJvSimpleXmlElem);
begin
  with BatchNode.Items.Add('DateTime') do
  begin
    Properties.Add('Mask', UTF8Encode(Settings.Mask));
    Properties.Add('AddSuffix', Settings.AddSuffix);
    Properties.Add('WhichDate', Settings.WhichDate);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionDateTime.LoadFromFile(ActionNode: TJvSimpleXmlElem);
begin
  Settings.Mask := UTF8Decode(ActionNode.Properties.Value('Mask', 'yyyy''-''mm''-''dde'));
  Settings.AddSuffix := ActionNode.Properties.BoolValue('AddSuffix', False);
  Settings.WhichDate := ActionNode.Properties.IntValue('WhichDate', 1);
end;

{-------------------------------------------------------------------------------
  TRenActionRandom
-------------------------------------------------------------------------------}

function TRenActionRandom.Description: WideString;
begin
  Result := WideFormat(stradRandom, [Settings.Mask]);
  case Settings.Method of
    0:  Result := Result + stradRandomNumbers;
    1:  Result := Result + stradRandomTick;
    2:  Result := Result + stradRandomGUID;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenActionRandom.GetTagValue(const ATag: WideString; out AValue: WideString): Boolean;
begin
  if ATag = 'random' then
  begin
    case Settings.Method of
      0:  AValue := Format('%.8d', [Trunc(Random * 100000000.0)]);
      1:  begin AValue := IntToStr(GetTickCount); Sleep(10); end;
      2:  AValue := CreateClassID;
    else
      AValue := '';
    end;
    Result := True;
  end
  else
    Result := inherited GetTagValue(ATag, AValue);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenActionRandom.MakeNewName(const AFileName: WideString; IsFolder: Boolean): WideString;
begin
  inherited MakeNewName(AFileName, IsFolder);
  Result := AdvancedFormat(Settings.Mask, GetTagValue);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionRandom.SaveToFile(BatchNode: TJvSimpleXmlElem);
begin
  with BatchNode.Items.Add('Random') do
  begin
    Properties.Add('Mask', UTF8Encode(Settings.Mask));
    Properties.Add('Method', Settings.Method);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionRandom.LoadFromFile(ActionNode: TJvSimpleXmlElem);
begin
  Settings.Mask := UTF8Decode(ActionNode.Properties.Value('Mask', '%random%%ext%'));
  Settings.Method := ActionNode.Properties.IntValue('Method', 0);
end;

{-------------------------------------------------------------------------------
  TRenActionCase
-------------------------------------------------------------------------------}

function TRenActionCase.Description: WideString;
begin
  Result := stradCase;
  case Settings.CaseType of
    0:
      Result := Result + WideFormat(stradCaseWords, [Settings.AfterChars]);
    1:
      Result := Result + stradCaseFirst;
    2:
      Result := Result + stradCaseUpper;
    3:
      Result := Result + stradCaseLower;
  end;
  if Settings.UseLocale then
    Result := Result + stradCaseLocale;
  if Settings.OnlyExt then
    Result := Result + stradOnlyExt
  else
    if Settings.IncludeExt then
      Result := Result + stradInclExt;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenActionCase.MakeNewName(const AFileName: WideString; IsFolder: Boolean): WideString;
var
  i: Integer;
  function LocalUpperCase(const Source: WideString): WideString;
  begin
    if Settings.UseLocale then
      Result := Tnt_WideUpperCase(Source)
    else
      Result := UpperCase(Source);
  end;
  function LocalLowerCase(const Source: WideString): WideString;
  begin
    if Settings.UseLocale then
      Result := Tnt_WideLowerCase(Source)
    else
      Result := LowerCase(Source);
  end;
begin
  inherited MakeNewName(AFileName, IsFolder);
  if not (IsFolder and not GlobalSettings.FolderExt) then
  begin
    if Settings.OnlyExt then
      Result := WideExtractFileExt(AFileName)
    else
    begin
      if Settings.IncludeExt then
        Result := WideExtractFileName(AFileName)
      else
        Result := WideChangeFileExt(WideExtractFileName(AFileName), '');
    end;
  end
  else
  begin
    Result := WideExtractFileName(AFileName);
    if Settings.OnlyExt then
      Exit;
  end;
  case Settings.CaseType of
    0:
      begin
        Result := LocalLowerCase(Result);
        for i := 1 to Length(Result) do
          if (i = 1) or (Pos(Result[i-1], Settings.AfterChars) > 0) then
            Result[i] := LocalUpperCase(Copy(Result, i, 1))[1];
      end;
    1:
      begin
        Result := LocalLowerCase(Result);
        Result[1] := LocalUpperCase(Copy(Result, 1, 1))[1];
      end;
    2:
      begin
        Result := LocalUpperCase(Result);
      end;
    3:
      begin
        Result := LocalLowerCase(Result);
      end;
  end;
  if not (IsFolder and not GlobalSettings.FolderExt) then
  begin
    if Settings.OnlyExt then
      Result := WideChangeFileExt(WideExtractFileName(AFileName), Result)
    else
      if not Settings.IncludeExt then
        Result := Result + WideExtractFileExt(AFileName);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionCase.SaveToFile(BatchNode: TJvSimpleXmlElem);
begin
  with BatchNode.Items.Add('ChangeCase') do
  begin
    Properties.Add('Option', Settings.CaseType);
    Properties.Add('AfterChars', UTF8Encode(Settings.AfterChars));
    Properties.Add('UseLocale', Settings.UseLocale);
    Properties.Add('IncludeExt', Settings.IncludeExt);
    Properties.Add('OnlyExt', Settings.OnlyExt);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionCase.LoadFromFile(ActionNode: TJvSimpleXmlElem);
begin
  Settings.CaseType := ActionNode.Properties.IntValue('Option', 0);
  Settings.AfterChars := UTF8Decode(ActionNode.Properties.Value('AfterChars', '- .+('));
  Settings.UseLocale := ActionNode.Properties.BoolValue('UseLocale', True);
  Settings.IncludeExt := ActionNode.Properties.BoolValue('IncludeExt', False);
  Settings.OnlyExt := ActionNode.Properties.BoolValue('OnlyExt', False);
end;

{-------------------------------------------------------------------------------
  TRenActionFromList
-------------------------------------------------------------------------------}

function TRenActionFromList.Description: WideString;
begin
  Result := stradFromList;
  if Settings.AppendExt then
    Result := Result + stradFromListExt;
  if Settings.OnlyExt then
    Result := Result + stradFromListOnlyExt;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionFromList.Init;
begin
  FListContents := TTntStringList.Create;
  FListContents.Text := Settings.ListContents;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionFromList.Finish;
begin
  FListContents.Free;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenActionFromList.MakeNewName(const AFileName: WideString; IsFolder: Boolean): WideString;
begin
  inherited MakeNewName(AFileName, IsFolder);
  Result := WideExtractFileName(AFileName);
  if (FListContents <> nil) and (FListContents.Count > 0) then
  begin
    if (FListContents.Strings[0] <> '') then
    begin
      if Settings.OnlyExt then
      begin
        if not (IsFolder and not GlobalSettings.FolderExt) then
          Result := WideChangeFileExt(Result, '.' + FListContents.Strings[0])
        else
          Result := WideExtractFileName(AFileName);
      end
      else
        Result := FListContents.Strings[0];
      if Settings.AppendExt and not (IsFolder and not GlobalSettings.FolderExt) then
        Result := Result + WideExtractFileExt(AFileName);
    end
    else
      with FLastError do
      begin
        Description := WideFormat(strfsNotRenamed, [AFileName, straeFromListEmpty]);
        Status := rsInfo;
      end;
    FListContents.Delete(0);
  end
  else
    with FLastError do
    begin
      Description := WideFormat(strfsNotRenamed, [AFileName, straeFromListEnd]);
      Status := rsInfo;
    end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionFromList.SaveToFile(BatchNode: TJvSimpleXmlElem);
begin
  with BatchNode.Items.Add('FromList') do
  begin
    Properties.Add('AppendExt', Settings.AppendExt);
    Properties.Add('OnlyExt', Settings.OnlyExt);
    Items.Add('listContents').Items.AddCData('', UTF8Encode(Settings.ListContents));
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionFromList.LoadFromFile(ActionNode: TJvSimpleXmlElem);
begin
  Settings.AppendExt := ActionNode.Properties.BoolValue('AppendExt', False);
  Settings.OnlyExt := ActionNode.Properties.BoolValue('OnlyExt', False);
  if (ActionNode.Items.Count > 0) then
    with (ActionNode.Items.Item[0]) do
      if (Items.Count > 0) and (Items.Item[0] is TJvSimpleXmlElemCData) then
        Settings.ListContents := UTF8Decode((Items.Item[0] as TJvSimpleXmlElemCData).Value);
end;

{-------------------------------------------------------------------------------
  TRenActionRegexp
-------------------------------------------------------------------------------}

function TRenActionRegexp.Description: WideString;
begin
  Result := stradRegexp;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenActionRegexp.MakeNewName(const AFileName: WideString; IsFolder: Boolean): WideString;
begin
  inherited MakeNewName(AFileName, IsFolder);
  if Assigned(FRegexp) then
    Result := FRegexp.Replace(WideExtractFileName(AFileName), Settings.Repl, True)
  else
    Result := '<RegExp Error> ' + FCompileError;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionRegexp.Init;
begin
  FCompileError := '';
  FRegexp := TRegExpr.Create;
  FRegexp.ModifierR := False;
  FRegexp.Expression := Settings.Expr;
  try
    FRegexp.Compile;
  except
    on e: Exception do
    begin
      FreeAndNil(FRegexp);
      FCompileError :=  e.Message;
    end;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionRegexp.Finish;
begin
  FRegexp.Free;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionRegexp.LoadFromFile(ActionNode: TJvSimpleXmlElem);
begin
  Settings.Expr := UTF8Decode(ActionNode.Properties.Value('Expr', ''));
  Settings.Repl := UTF8Decode(ActionNode.Properties.Value('Repl', ''));
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionRegexp.SaveToFile(BatchNode: TJvSimpleXmlElem);
begin
  with BatchNode.Items.Add('Regexp') do
  begin
    Properties.Add('Expr', UTF8Encode(Settings.Expr));
    Properties.Add('Repl', UTF8Encode(Settings.Repl));
  end;
end;

{-------------------------------------------------------------------------------
  TRenActionExif
-------------------------------------------------------------------------------}

function TRenActionExif.Description: WideString;
begin
  Result := WideFormat(stradExif, [Settings.Mask]);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenActionExif.GetTagValue(const ATag: WideString; out AValue: WideString): Boolean;
begin
  if FImgData.ExifObj.LookupTagDefn(ATag) <> -1 then // check if it is a valid Exif tag (even if current file does not have it)
  begin
    AValue := FImgData.ExifObj.LookupTagVal(ATag); // returns '' if the tag was not found in current file
    Result := True;
  end
  {
  else
  if CheckChars(ATag, ['0'..'9']) then
  begin
    Result := True;
    AValue := FImgData.ExifObj.ITagArray[StrToInt(ATag)].Desc;
  end
  }
  else
    Result := inherited GetTagValue(ATag, AValue);
  AValue := ValidateFileName(AValue);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenActionExif.MakeNewName(const AFileName: WideString; IsFolder: Boolean): WideString;
begin
  Result := inherited MakeNewName(AFileName, IsFolder);
  if FImgData.ProcessFile(AFileName) and FImgData.HasEXIF then
  begin
    Result := AdvancedFormat(Settings.Mask, GetTagValue, [afoAllowIf,afoAllowQuotes]);
  end
  else
  begin
    with FLastError do
    begin
      Description := WideFormat(strfsNotRenamed, [AFileName, straeExifNoInfo]);
      Status := rsInfo;
    end;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionExif.Init;
begin
  DexifDecode := True;
  FImgData := TImgData.Create;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionExif.Finish;
begin
  FImgData.Free;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionExif.LoadFromFile(ActionNode: TJvSimpleXmlElem);
begin
  Settings.Mask := UTF8Decode(ActionNode.Properties.Value('Mask', ''));
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionExif.SaveToFile(BatchNode: TJvSimpleXmlElem);
begin
  with BatchNode.Items.Add('Exif') do
  begin
    Properties.Add('Mask', UTF8Encode(Settings.Mask));
  end;
end;

{-------------------------------------------------------------------------------
 TRenActionShiftDT
-------------------------------------------------------------------------------}
(*
function TRenActionShiftDT.Description: WideString;
begin
  Result := WideFormat(stradShiftDT, [Settings.DateAdd, Settings.TimeAdd]);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionShiftDT.Init;
begin

end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionShiftDT.Finish;
begin

end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionShiftDT.LoadFromFile(ActionNode: TJvSimpleXmlElem);
begin
  Settings.TimePos := ActionNode.Properties.IntValue('TimePos', 0);
  Settings.TimeAdd := ActionNode.Properties.IntValue('TimeAdd', 0);
  Settings.DatePos := ActionNode.Properties.IntValue('DatePos', 0);
  Settings.DateAdd := ActionNode.Properties.IntValue('DateAdd', 0);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenActionShiftDT.SaveToFile(BatchNode: TJvSimpleXmlElem);
begin
  with BatchNode.Items.Add('ShiftDT') do
  begin
    Properties.Add('TimePos', Settings.TimePos);
    Properties.Add('TimeAdd', Settings.TimeAdd);
    Properties.Add('DatePos', Settings.DatePos);
    Properties.Add('DateAdd', Settings.DateAdd);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TRenActionShiftDT.MakeNewName(const AFileName: WideString; IsFolder: Boolean): WideString;
begin
  inherited MakeNewName(AFileName, IsFolder);

end;
*)
{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

end.
