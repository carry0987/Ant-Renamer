(************************************************************************
 *                                                                      *
 *   (C) 2002-2008 Antoine Potten                                       *
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

unit SettingsBase;

interface

uses
  Classes, SysUtils, JvSimpleXml;

type

  TSettingsBase = class(TObject)
  private
  protected
    FRoot: TJvSimpleXmlElem;
    OptionString: array of string;
    DefaultString: array of string;
    DefaultInteger: array of Integer;
    DefaultBoolean: array of Boolean;
    procedure Init; virtual; abstract;
    procedure Clean; virtual; abstract;
    procedure SetOptionsLength(const Length: Integer);
    function GetOptionStringIndex(const Index: Integer): string;
    procedure SetOptionStringIndex(const Index: Integer; const Value: string);
    function GetOptionIntegerIndex(const Index: Integer): Integer;
    procedure SetOptionIntegerIndex(const Index: Integer; const Value: Integer);
    function GetOptionBooleanIndex(const Index: Integer): Boolean;
    procedure SetOptionBooleanIndex(const Index: Integer; const Value: Boolean);
  public
    constructor Create(const Root: TJvSimpleXmlElem); overload;
    constructor Create(const Parent: TJvSimpleXmlElem; const TagName: string); overload;
    destructor Destroy; override;
    procedure SetRoot(const Root: TJvSimpleXmlElem); overload;
    procedure SetRoot(const Parent: TJvSimpleXmlElem; const TagName: string); overload;
    function GetRoot: TJvSimpleXmlElem;
    procedure RecreateNode;
  end;

  TSettingsCustom = class(TSettingsBase)
  private
  protected
    procedure Init; override;
    procedure Clean; override;
  public
    property Node: TJvSimpleXmlElem read GetRoot;
  end;

  TSettingsForm = class(TSettingsBase)
  private
  protected
    procedure Init; override;
    procedure Clean; override;
  public
    property State: Integer index 0 read GetOptionIntegerIndex write SetOptionIntegerIndex;
    property Width: Integer index 1 read GetOptionIntegerIndex write SetOptionIntegerIndex;
    property Height: Integer index 2 read GetOptionIntegerIndex write SetOptionIntegerIndex;
    property Left: Integer index 3 read GetOptionIntegerIndex write SetOptionIntegerIndex;
    property Top: Integer index 4 read GetOptionIntegerIndex write SetOptionIntegerIndex;
    property OnTop: Boolean index 5 read GetOptionBooleanIndex write SetOptionBooleanIndex;
    procedure SetDefaults(const aState, aWidth, aHeight, aLeft, aTop: Integer; const aOnTop: Boolean);
  end;

  TSettingsFolder = class(TSettingsBase)
  private
  protected
    procedure Init; override;
    procedure Clean; override;
  public
    property Path: string index 0 read GetOptionStringIndex write SetOptionStringIndex;
  end;

  TSettingsList = class(TObject)
  private
    FRoot: TJvSimpleXmlElem;
    FTagName: string;
    FAttName: string;
  protected
  public
    constructor Create(const Root: TJvSimpleXmlElem; const TagName, AttName: string);
    procedure Load(aList: TStrings);
    procedure Save(aList: TStrings);
  end;

  TSettingsCDATA = class(TObject)
  private
    FRoot: TJvSimpleXmlElem;
    FTagName: string;
    function GetValue: string;
    procedure SetValue(const AValue: string);
  protected
  public
    constructor Create(const Root: TJvSimpleXmlElem; const TagName: string);
    property Value: string read GetValue write SetValue;
  end;

  TSettingsCDATAList = class(TSettingsBase)
  private
    FNameNode: string;
    FNameAtt: string;
  protected
    procedure Init; override;
    procedure Clean; override;
  public
    constructor Create(const Root: TJvSimpleXmlElem; const TagName, NameNode, NameAtt: string); reintroduce;
    procedure Names(aList: TStrings);
    function Load(const aName: string; out aValue: string): Boolean;
    procedure Save(const aName, aValue: string);
    procedure Delete(const aName: string);
  end;

  TSettingsMru = class(TSettingsBase)
  private
    FList: TSettingsList;
    FTagNode, FTagAttr: string;
  protected
    procedure Init; override;
    procedure Clean; override;
  public
    constructor Create(const Root: TJvSimpleXmlElem; const TagName, MruNode, MruAtt: string);
    procedure Load(aList: TStrings);
    procedure Save(aList: TStrings);
  end;

  TSettingsFile = class(TObject)
  private
    FFileName: TJvSimpleXmlFileName;
    FMutex: Cardinal;
  protected
    FXML: TJvSimpleXml;
  public
    constructor Create(const AFileName: TJvSimpleXmlFileName; const AMutexName: string);
    procedure LoadFromFile(const AFileName: TJvSimpleXmlFileName = ''); virtual;
    procedure SaveToFile(const AFileName: TJvSimpleXmlFileName = ''); virtual;
    destructor Destroy; override;
  end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

implementation

uses
  Windows;

{-------------------------------------------------------------------------------
  TSettingsFile
-------------------------------------------------------------------------------}

constructor TSettingsFile.Create(const AFileName: TJvSimpleXmlFileName; const AMutexName: string);
begin
  FXML := TJvSimpleXml.Create(nil);
  FFileName := AFileName;
  FMutex := CreateMutex(nil, False, PChar('Global\' + AMutexName));
end;

destructor TSettingsFile.Destroy;
begin
  FXML.Free;
  inherited;
end;

procedure TSettingsFile.LoadFromFile(const AFileName: TJvSimpleXmlFileName = '');
begin
  if AFileName <> '' then
    FFileName := AFileName;
  try
    if FFileName <> '' then
    begin
      if WaitForSingleObject(FMutex, INFINITE) = WAIT_FAILED then
        RaiseLastOSError;
      try
        FXML.LoadFromFile(FFileName)
      finally
        ReleaseMutex(FMutex);
      end;
    end
    else
      raise Exception.Create('');
  except
    FXML.Root.Clear;
    FXML.Prolog.Clear;
  end;
end;

procedure TSettingsFile.SaveToFile(const AFileName: TJvSimpleXmlFileName = '');
begin
  if AFileName <> '' then
    FFileName := AFileName;
  if WaitForSingleObject(FMutex, INFINITE) = WAIT_FAILED then
    RaiseLastOSError;
  try
    FXML.SaveToFile(FFileName);
  finally
    ReleaseMutex(FMutex);
  end;
end;

{-------------------------------------------------------------------------------
  TSettingsBase
-------------------------------------------------------------------------------}

function TSettingsBase.GetOptionStringIndex(const Index: Integer): string;
begin
  Result := FRoot.Properties.Value(OptionString[Index], DefaultString[Index]);
end;

procedure TSettingsBase.SetOptionStringIndex(const Index: Integer; const Value: string);
begin
  FRoot.Properties.Delete(OptionString[Index]);
  FRoot.Properties.Add(OptionString[Index], Value);
end;

function TSettingsBase.GetOptionIntegerIndex(const Index: Integer): Integer;
begin
  Result := FRoot.Properties.IntValue(OptionString[Index], DefaultInteger[Index]);
end;

procedure TSettingsBase.SetOptionIntegerIndex(const Index: Integer; const Value: Integer);
begin
  SetOptionStringIndex(Index, IntToStr(Value));
end;

function TSettingsBase.GetOptionBooleanIndex(const Index: Integer): Boolean;
begin
  Result := FRoot.Properties.BoolValue(OptionString[Index], DefaultBoolean[Index]);
end;

procedure TSettingsBase.SetOptionBooleanIndex(const Index: Integer; const Value: Boolean);
begin
  SetOptionStringIndex(Index, BoolToStr(Value, True));
end;

constructor TSettingsBase.Create(const Root: TJvSimpleXmlElem);
begin
  SetRoot(Root);
end;

constructor TSettingsBase.Create(const Parent: TJvSimpleXmlElem; const TagName: string);
begin
  SetRoot(Parent, TagName);
end;

procedure TSettingsBase.SetRoot(const Root: TJvSimpleXmlElem);
begin
  Clean;
  FRoot := Root;
  Init;
end;

procedure TSettingsBase.SetRoot(const Parent: TJvSimpleXmlElem; const TagName: string);
var
  Root: TJvSimpleXmlElem;
begin
  Root := Parent.Items.ItemNamed[TagName];
  if Root = nil then
    Root := Parent.Items.Add(TagName);
  SetRoot(Root);
end;

function TSettingsBase.GetRoot: TJvSimpleXmlElem;
begin
  Result := FRoot;
end;

destructor TSettingsBase.Destroy;
begin
  Clean;
  inherited;
end;

procedure TSettingsBase.SetOptionsLength(const Length: Integer);
begin
  SetLength(OptionString, Length);
  SetLength(DefaultString, Length);
  SetLength(DefaultInteger, Length);
  SetLength(DefaultBoolean, Length);
end;

{-------------------------------------------------------------------------------
  TSettingsForm
-------------------------------------------------------------------------------}

procedure TSettingsForm.Init;
begin
  SetOptionsLength(6);
  OptionString[0] := 'State';
  OptionString[1] := 'Width';
  OptionString[2] := 'Height';
  OptionString[3] := 'Left';
  OptionString[4] := 'Top';
  OptionString[5] := 'AlwaysOnTop';
end;

procedure TSettingsForm.Clean;
begin
end;

procedure TSettingsForm.SetDefaults(const aState, aWidth, aHeight, aLeft, aTop: Integer; const aOnTop: Boolean);
begin
  DefaultInteger[0] := aState;
  DefaultInteger[1] := aWidth;
  DefaultInteger[2] := aHeight;
  DefaultInteger[3] := aLeft;
  DefaultInteger[4] := aTop;
  DefaultBoolean[5] := aOnTop;
end;

{-------------------------------------------------------------------------------
  TSettingsFolder
-------------------------------------------------------------------------------}

procedure TSettingsFolder.Init;
begin
  SetOptionsLength(1);
  OptionString[0] := 'Path';
  DefaultString[0] := '';
end;

procedure TSettingsFolder.Clean;
begin
end;

{-------------------------------------------------------------------------------
  TSettingsList
-------------------------------------------------------------------------------}

constructor TSettingsList.Create(const Root: TJvSimpleXmlElem; const TagName, AttName: string);
begin
  FRoot := Root;
  FTagName := TagName;
  FAttName := AttName;
end;

procedure TSettingsList.Load(aList: TStrings);
var
  i: Integer;
  s: string;
begin
  aList.BeginUpdate;
  try
    aList.Clear;
    with FRoot.Items do
      for i := 0 to Count-1 do
        if Item[i].Name = FTagName then
        begin
          s := Item[i].Properties.Value(FAttName, '');
          if s <> '' then
            aList.Add(s);
        end;
  finally
    aList.EndUpdate;
  end;
end;

procedure TSettingsList.Save(aList: TStrings);
var
  i: Integer;
begin
  with FRoot.Items do
  begin
    for i := Count-1 downto 0 do
      if Item[i].Name = FTagName then
        Delete(i);
    for i := 0 to aList.Count-1 do
      Add(FTagName).Properties.Add(FAttName, aList.Strings[i]);
  end;
end;

{-------------------------------------------------------------------------------
  TSettingsCDATA
-------------------------------------------------------------------------------}

constructor TSettingsCDATA.Create(const Root: TJvSimpleXmlElem; const TagName: string);
begin
  FRoot := Root;
  FTagName := TagName;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TSettingsCDATA.GetValue: string;
var
  Found: TJvSimpleXmlElem;
begin
  Result := '';
  Found := FRoot.Items.ItemNamed[FTagName];
  if (Found <> nil) and (Found.Items.Count > 0) and (Found.Items.Item[0] is TJvSimpleXmlElemCData) then
    Result := Found.Items.Item[0].Value;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TSettingsCDATA.SetValue(const AValue: string);
begin
  FRoot.Items.Delete(FTagName);
  FRoot.Items.Add(FTagName).Items.AddCData('', AValue);
end;

{-------------------------------------------------------------------------------
  TSettingsCDATAList
-------------------------------------------------------------------------------}

constructor TSettingsCDATAList.Create(const Root: TJvSimpleXmlElem; const TagName, NameNode, NameAtt: string);
begin
  inherited Create(Root, TagName);
  FNameNode := NameNode;
  FNameAtt := NameAtt;
end;

procedure TSettingsCDATAList.Init;
begin
end;

procedure TSettingsCDATAList.Clean;
begin
end;

procedure TSettingsCDATAList.Names(aList: TStrings);
var
  i: Integer;
begin
  aList.BeginUpdate;
  try
    aList.Clear;
    with FRoot.Items do
      for i := 0 to Count-1 do
        if Item[i].Name = FNameNode then
          aList.Add(Item[i].Properties.Value(FNameAtt));
  finally
    aList.EndUpdate;
  end;
end;

function TSettingsCDATAList.Load(const aName: string; out aValue: string): Boolean;
var
  Found: TJvSimpleXmlElem;
  i: Integer;
begin
  Found := nil;
  Result := False;
  with FRoot.Items do
    for i := 0 to Count-1 do
      if (Item[i].Name = FNameNode) and (Item[i].Properties.Value(FNameAtt) = aName) then
      begin
        Found := Item[i];
        Break;
      end;
  if (Found <> nil) and (Found.Items.Count > 0) and (Found.Items.Item[0] is TJvSimpleXmlElemCData) then
  begin
    Result := True;
    aValue := Found.Items.Item[0].Value;
  end;
end;

procedure TSettingsCDATAList.Save(const aName, aValue: string);
var
  Found: TJvSimpleXmlElem;
  i: Integer;
begin
  Found := nil;
  with FRoot.Items do
    for i := 0 to Count-1 do
      if (Item[i].Name = FNameNode) and (Item[i].Properties.Value(FNameAtt) = aName) then
      begin
        Found := Item[i];
        Break;
      end;
  if Found = nil then
  begin
    Found := FRoot.Items.Add(FNameNode);
    Found.Properties.Add(FNameAtt, aName);
  end
  else
    Found.Items.Clear;
  Found.Items.AddCData('', aValue);
end;

procedure TSettingsCDATAList.Delete(const aName: string);
var
  i: Integer;
begin
  with FRoot.Items do
    for i := Count-1 downto 0 do
      if (Item[i].Name = FNameNode) and (Item[i].Properties.Value(FNameAtt) = aName) then
        FRoot.Items.Delete(i);
end;

{-------------------------------------------------------------------------------
  TSettingsCustom
-------------------------------------------------------------------------------}

procedure TSettingsCustom.Clean;
begin
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TSettingsCustom.Init;
begin
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TSettingsBase.RecreateNode;
var
  Parent: TJvSimpleXmlElem;
  TagName: string;
begin
  Parent := FRoot.Parent;
  TagName := FRoot.Name;
  Parent.Items.Delete(TagName);
  FRoot := Parent.Items.Add(TagName);
end;

{-------------------------------------------------------------------------------
  TSettingsMru
-------------------------------------------------------------------------------}

procedure TSettingsMru.Init;
begin
  FList := TSettingsList.Create(FRoot, FTagNode, FTagAttr);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TSettingsMru.Clean;
begin
  FList.Free;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TSettingsMru.Load(aList: TStrings);
begin
  FList.Load(aList);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TSettingsMru.Save(aList: TStrings);
begin
  FList.Save(aList);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

constructor TSettingsMru.Create(const Root: TJvSimpleXmlElem; const TagName, MruNode, MruAtt: string);
begin
  FTagNode := MruNode;
  FTagAttr := MruAtt;
  inherited Create(Root, TagName);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

end.
