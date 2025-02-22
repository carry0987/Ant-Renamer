 (**
 *
 * Copyright 2004, Akretio SPRL.  All Rights Reserved.
 * Evolution of JVCL's TJvSimpleXml Parser
 * (Fork from the JVCL by the author of TJvSimpleXml)
 * ---------------------------------------------------------------------------
 *
 *   This program is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU General Public License
 *   as published by the Free Software Foundation; either version 2
 *   of the License, or (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 * ----------------------------------------------------------------------------
 * Created: December 9, 2004
 * ----------------------------------------------------------------------------
 *)

{$I JVCL.INC}

unit JvSimpleXml;

interface

uses
  SysUtils, Classes, IniFiles, ClassStringListEx{$IFDEF COMPILER6_UP}, Variants{$ENDIF};

type
  {$IFNDEF COMPILER6_UP}
  THandle = Longword;
  {$ENDIF}
  TJvSimpleXml = class;
  TJvSimpleXmlInvalid = class(Exception);
  TJvSimpleXmlElem = class;
  TJvSimpleXmlElems = class;
  TJvSimpleXmlProps = class;
  TJvSimpleXmlElemComment = class;
  TJvSimpleXmlElemClassic = class;
  TJvSimpleXmlElemCData = class;
  TJvSimpleXmlElemText = class;
  TJvOnSimpleXmlParsed = procedure(Sender: TObject; Name: string) of object;
  TJvOnValueParsed = procedure(Sender: TObject; Name, Value: string) of object;
  TJvOnSimpleProgress = procedure(Sender: TObject; const Position, Total: Integer) of object;

  //Those hash stuffs are for future use only
  //Plans are to replace current hash by this mechanism
  TJvHashKind = (hkList, hkDirect);
  PJvHashElem = ^TJvHashElem;
  TJvHashElem = packed record
    Next: PJvHashElem;
    Obj: TObject;
  end;
  PJvHashRecord = ^TJvHashRecord;
  TJvHashList = array [0..25] of PJvHashRecord;
  PJvHashList = ^TJvHashList;
  TJvHashRecord = packed record
    Count: Byte;
    case Kind: TJvHashKind of
      hkList: (List: PJvHashList);
      hkDirect: (FirstElem: PJvHashElem);
  end;

  TJvSimpleXmlProp = class(TObject)
  private
    FName: string;
    FValue: string;
    FParent: TJvSimpleXmlProps;
    FPointer: string;
    FData: Pointer;
    function GetBoolValue: Boolean;
    procedure SetBoolValue(const Value: Boolean);
    procedure SetName(const Value: string);
  protected
    function GetIntValue: Int64;
    procedure SetIntValue(const Value: Int64);
  public
    function SaveToString: string;
    property Parent: TJvSimpleXmlProps read FParent write FParent;
    property Name: string read FName write SetName;
    property Value: string read FValue write FValue;
    property IntValue: Int64 read GetIntValue write SetIntValue;
    property BoolValue: Boolean read GetBoolValue write SetBoolValue;
    property Pointer: string read FPointer write FPointer;

    property Data: Pointer read FData write FData;
  end;

  TJvSimpleXmlProps = class(TObject)
  private
    FProperties: TStringListEx;
    function GetCount: Integer;
    function GetItemNamed(const Name: string): TJvSimpleXmlProp;
  protected
    function GetItem(const Index: Integer): TJvSimpleXmlProp;
    procedure DoItemRename(var Value: TJvSimpleXmlProp; const Name: string);
  public
    destructor Destroy; override;
    function Add(const Name, Value: string): TJvSimpleXmlProp; overload;
    function Add(const Name: string; const Value: Int64): TJvSimpleXmlProp; overload;
    function Add(const Name: string; const Value: Boolean): TJvSimpleXmlProp; overload;
    procedure Clear; virtual;
    procedure Delete(const Index: Integer); overload;
    procedure Delete(const Name: string); overload;
    function Value(const Name: string; Default: string = ''): string;
    function IntValue(const Name: string; Default: Int64 = -1): Int64;
    function BoolValue(const Name: string; Default: Boolean = True): Boolean;
    procedure LoadFromArray(var AData: string; var APosition, ALength: Integer);
    procedure LoadFromStream(const Stream: TStream);
    procedure SaveToStream(const Stream: TStream);
    property Item[const Index: Integer]: TJvSimpleXmlProp read GetItem; default;
    property ItemNamed[const Name: string]: TJvSimpleXmlProp read GetItemNamed;
    property Count: Integer read GetCount;
  end;

  TJvSimpleXmlElemsProlog = class(TObject)
  private
    FElems: TStringListEx;
    function GetCount: Integer;
    function GetItem(const Index: Integer): TJvSimpleXmlElem;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(const Elem: TJvSimpleXmlElem);
    procedure LoadFromArray(var AData: string; var APosition, ALength: Integer; AParent: TJvSimpleXml = nil);
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml = nil);
    procedure SaveToStream(const Stream: TStream; Parent: TJvSimpleXml = nil);
    property Item[const Index: Integer]: TJvSimpleXmlElem read GetItem; default;
    property Count: Integer read GetCount;
  end;

  TJvSimpleXmlElemCompare = function(Elems: TJvSimpleXmlElems; Index1, Index2: Integer): Integer of object;
  TJvSimpleXmlElems = class(TObject)
  private
    FParent: TJvSimpleXmlElem;
    function GetCount: Integer;
    function GetItemNamed(const Name: string): TJvSimpleXmlElem;
  protected
    FElems: TStringListEx;
    FCompare: TJvSimpleXmlElemCompare;
    function GetItem(const Index: Integer): TJvSimpleXmlElem;
    procedure AddChild(const Value: TJvSimpleXmlElem);
    procedure AddChildFirst(const Value: TJvSimpleXmlElem);
    procedure DoItemRename(var Value: TJvSimpleXmlElem; const Name: string);
    procedure CreateElems;
  public
    constructor Create(const AOwner: TJvSimpleXmlElem);
    destructor Destroy; override;
    function Add(const Name: string): TJvSimpleXmlElemClassic; overload;
    function Add(const Name, Value: string): TJvSimpleXmlElemClassic; overload;
    function Add(const Name: string; const Value: Int64): TJvSimpleXmlElemClassic; overload;
    function Add(const Name: string; const Value: Boolean): TJvSimpleXmlElemClassic; overload;
    function Add(const Name: string; const Value: TStream): TJvSimpleXmlElemClassic; overload;
    function Add(Value: TJvSimpleXmlElem): TJvSimpleXmlElem; overload;
    function AddFirst(Value: TJvSimpleXmlElem): TJvSimpleXmlElem; overload;
    function AddFirst(const Name: string): TJvSimpleXmlElemClassic; overload;
    function AddComment(const Name: string; const Value: string): TJvSimpleXmlElemComment;
    function AddCData(const Name: string; const Value: string): TJvSimpleXmlElemCDATA;
    function AddText(const Name: string; const Value: string): TJvSimpleXmlElemText;
    procedure Clear; virtual;
    procedure Delete(const Index: Integer); overload;
    procedure Delete(const Name: string); overload;
    function Value(const Name: string; Default: string = ''): string;
    function IntValue(const Name: string; Default: Int64 = -1): Int64;
    function BoolValue(const Name: string; Default: Boolean = True): Boolean;
    procedure BinaryValue(const Name: string; const Stream: TStream);
    function LoadFromArray(var AData: string; var APosition, ALength: Integer; AParent: TJvSimpleXml = nil): string;
    function LoadFromStream(const Stream: TStream; AParent: TJvSimpleXml = nil): string;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXml = nil);
    procedure Sort;
    procedure CustomSort(AFunction: TJvSimpleXmlElemCompare);
    property Parent: TJvSimpleXmlElem read FParent write FParent;
    property Item[const Index: Integer]: TJvSimpleXmlElem read GetItem; default;
    property ItemNamed[const Name: string]: TJvSimpleXmlElem read GetItemNamed;
    property Count: Integer read GetCount;
  end;

  TJvSimpleXmlElem = class(TObject)
  private
    FName: string;
    FParent: TJvSimpleXmlElem;
    FItems: TJvSimpleXmlElems;
    FProps: TJvSimpleXmlProps;
    FValue: string;
    FPointer: string;
    FData: Pointer;
  protected
    function GetIntValue: Int64;
    function GetBoolValue: Boolean;
    function GetChildsCount: Integer;
    function GetProps: TJvSimpleXmlProps;
    function GetItems: TJvSimpleXmlElems;
    procedure SetBoolValue(const Value: Boolean);
    procedure SetName(const Value: string);
    procedure SetIntValue(const Value: Int64);
  public
    constructor Create(const AOwner: TJvSimpleXmlElem);
    destructor Destroy; override;
    procedure Assign(Value: TJvSimpleXmlElem);
    procedure Clear; virtual;
    function SaveToString: string;
    procedure LoadFromArray(var AData: string; var APosition, ALength: Integer; Parent: TJvSimpleXml = nil); virtual; abstract;
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml = nil); virtual; abstract;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXml = nil); virtual;
      abstract;
    procedure GetBinaryValue(const Stream: TStream);
    property Data: Pointer read FData write FData;
    function GetChildIndex(const AChild: TJvSimpleXmlElem): Integer;
  published
    property Name: string read FName write SetName;
    property Parent: TJvSimpleXmlElem read FParent write FParent;
    property Pointer: string read FPointer write FPointer;
    property ChildsCount: Integer read GetChildsCount;
    property Items: TJvSimpleXmlElems read GetItems;
    property Properties: TJvSimpleXmlProps read GetProps;
    property IntValue: Int64 read GetIntValue write SetIntValue;
    property BoolValue: Boolean read GetBoolValue write SetBoolValue;
    property Value: string read FValue write FValue;
  end;

  TJvSimpleXmlElemComment = class(TJvSimpleXmlElem)
  public
    procedure LoadFromArray(var AData: string; var APosition, ALength: Integer; Parent: TJvSimpleXml = nil); override;
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXml = nil); override;
  end;

  TJvSimpleXmlElemClassic = class(TJvSimpleXmlElem)
  public
    procedure LoadFromArray(var AData: string; var APosition, ALength: Integer; Parent: TJvSimpleXml = nil); override;
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXml = nil); override;
  end;

  TJvSimpleXmlElemCData = class(TJvSimpleXmlElem)
  public
    procedure LoadFromArray(var AData: string; var APosition, ALength: Integer; Parent: TJvSimpleXml = nil); override;
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXml = nil); override;
  end;

  TJvSimpleXmlElemText = class(TJvSimpleXmlElem)
  public
    procedure LoadFromArray(var AData: string; var APosition, ALength: Integer; Parent: TJvSimpleXml = nil); override;
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXml = nil); override;
  end;

  TJvSimpleXmlElemHeader = class(TJvSimpleXmlElem)
  private
    FStandalone: Boolean;
    FEncoding: string;
    FVersion: string;
  public
    constructor Create;
    procedure LoadFromArray(var AData: string; var APosition, ALength: Integer; Parent: TJvSimpleXml = nil); override;
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXml = nil); override;
    property Version: string read FVersion write FVersion;
    property Standalone: Boolean read FStandalone write FStandalone;
    property Encoding: string read FEncoding write FEncoding;
  end;

  TJvSimpleXmlElemDocType = class(TJvSimpleXmlElem)
  public
    procedure LoadFromArray(var AData: string; var APosition, ALength: Integer; Parent: TJvSimpleXml = nil); override;
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXml = nil); override;
  end;

  TJvSimpleXmlElemSheet = class(TJvSimpleXmlElem)
  public
    procedure LoadFromArray(var AData: string; var APosition, ALength: Integer; Parent: TJvSimpleXml = nil); override;
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXml = nil); override;
  end;

  TJvSimpleXml = class(TComponent)
  private
    FFileName: TFileName;
    FRoot: TJvSimpleXmlElemClassic;
    FOnTagParsed: TJvOnSimpleXmlParsed;
    FOnValue: TJvOnValueParsed;
    FOnLoadProg: TJvOnSimpleProgress;
    FOnSaveProg: TJvOnSimpleProgress;
    FProlog: TJvSimpleXmlElemsProlog;
    FSaveCount, FSaveCurrent: Integer;
  protected
    procedure SetFileName(Value: TFileName);
    procedure DoLoadProgress(const APosition, ATotal: Integer);
    procedure DoSaveProgress;
    procedure DoTagParsed(const AName: string);
    procedure DoValueParsed(const AName, AValue: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromString(const Value: string);
    procedure LoadFromFile(const FileName: TFileName);
    procedure LoadFromStream(const Stream: TStream);
    procedure LoadFromResourceName(Instance: THandle; const ResName: string);
    procedure SaveToFile(FileName: TFileName);
    procedure SaveToStream(const Stream: TStream);
    function SaveToString: string;
    property Prolog: TJvSimpleXmlElemsProlog read FProlog write FProlog;
    property Root: TJvSimpleXmlElemClassic read FRoot write FRoot;
  published
    property FileName: TFileName read FFileName write SetFileName;
    property OnSaveProgress: TJvOnSimpleProgress read FOnSaveProg write FOnSaveProg;
    property OnLoadProgress: TJvOnSimpleProgress read FOnLoadProg write FOnLoadProg;
    property OnTagParsed: TJvOnSimpleXmlParsed read FOnTagParsed write FOnTagParsed;
    property OnValueParsed: TJvOnValueParsed read FOnValue write FOnValue;
  end;

{$IFDEF COMPILER6_UP}

  TXmlVariant = class(TInvokeableVariantType)
  public
    procedure Clear(var V: TVarData); override;
    function IsClear(const V: TVarData): Boolean; override;
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean); override;
    procedure CastTo(var Dest: TVarData; const Source: TVarData;
      const AVarType: TVarType); override;

    function DoFunction(var Dest: TVarData; const V: TVarData;
      const Name: string; const Arguments: TVarDataArray): Boolean; override;
    function GetProperty(var Dest: TVarData; const V: TVarData;
      const Name: string): Boolean; override;
    function SetProperty(const V: TVarData; const Name: string;
      const Value: TVarData): Boolean; override;
  end;

  TXmlVarData = packed record
    VType: TVarType;
    Reserved1: Word;
    Reserved2: Word;
    Reserved3: Word;
    Xml: TJvSimpleXmlElem;
    Reserved4: Longint;
  end;

procedure XmlCreateInto(var ADest: Variant; const AXml: TJvSimpleXmlElem);
function XmlCreate(const AXml: TJvSimpleXmlElem): Variant; overload;
function XmlCreate: Variant; overload;
function VarXml: TVarType;

{$ENDIF}

resourcestring
  RS_INVALID_SimpleXml = 'Invalid XML file';
  {$IFNDEF COMPILER6_UP}
  SInvalidBoolean = '''%s'' is not a valid Boolean value';
  {$ENDIF COMPILER6_UP}

implementation

const
  cBufferSize = 8192;

{$IFDEF COMPILER6_UP}
var
  XmlVariant: TXmlVariant = nil;
{$ENDIF}

var
 GSorts: TList;


{$IFNDEF COMPILER6_UP}

var
  TrueBoolStrs: array of string;
  FalseBoolStrs: array of string;

const
  DefaultTrueBoolStr = 'True'; // DO NOT LOCALIZE
  DefaultFalseBoolStr = 'False'; // DO NOT LOCALIZE

{$ENDIF COMPILER6_UP}

{$IFNDEF COMPILER6_UP}

procedure VerifyBoolStrArray;
begin
  if Length(TrueBoolStrs) = 0 then
  begin
    SetLength(TrueBoolStrs, 1);
    TrueBoolStrs[0] := DefaultTrueBoolStr;
  end;
  if Length(FalseBoolStrs) = 0 then
  begin
    SetLength(FalseBoolStrs, 1);
    FalseBoolStrs[0] := DefaultFalseBoolStr;
  end;

end;

function TryStrToFloat(const S: string; out Value: Extended): Boolean;
begin
  Result := TextToFloat(PChar(S), Value, fvExtended);
end;

procedure ConvertErrorFmt(ResString: PResStringRec; const Args: array of const);
begin
  raise EConvertError.CreateResFmt(ResString, Args);
end;

function TryStrToBool(const S: string; out Value: Boolean): Boolean;

  function CompareWith(const AStrings: array of string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := Low(AStrings) to High(AStrings) do
      if AnsiSameText(S, AStrings[I]) then
      begin
        Result := True;
        Break;
      end;
  end;

var
  LResult: Extended;
begin
  Result := TryStrToFloat(S, LResult);
  if Result then
    Value := LResult <> 0
  else
  begin
    VerifyBoolStrArray;
    Result := CompareWith(TrueBoolStrs);
    if Result then
      Value := True
    else
    begin
      Result := CompareWith(FalseBoolStrs);
      if Result then
        Value := False;
    end;
  end;
end;

function StrToBoolDef(const S: string; const Default: Boolean): Boolean;
begin
  if not TryStrToBool(S, Result) then
    Result := Default;
end;

function StrToBool(S: string): Boolean;
begin
  if not TryStrToBool(S, Result) then
    ConvertErrorFmt(@SInvalidBoolean, [S]);
end;

function BoolToStr(B: Boolean; UseBoolStrs: Boolean = False): string;
const
  cSimpleBoolStrs: array [Boolean] of string = ('0', '-1');
begin
  if UseBoolStrs then
  begin
    VerifyBoolStrArray;
    if B then
      Result := TrueBoolStrs[0]
    else
      Result := FalseBoolStrs[0];
  end
  else
    Result := cSimpleBoolStrs[B];
end;

{$ENDIF COMPILER6_UP}

function SimpleXmlEncode(const Value: string): string;
var
  i: Integer;
  lDiff: Boolean;
begin
  //http://www.cs.tut.fi/~jkorpela/latin1/3.html#60
  result := Value;
  lDiff := false;
  for i := 1 to Length(Value) do
    if Value[i] in ['<','>','&','"',''''] then
    begin
      if not lDiff then
      begin
        lDiff := true;
        result := Copy(Value,1,i-1);
      end;
      result := result + '&#' + IntToStr(Ord(Value[i])) + ';';
    end
    else
      if lDiff then
        result := result + Value[i];
end;

function SimpleXmlDecode(const Value: string): string;
var
 i, j, k, l, m, lSize: Integer;
 st, st2: string;

  procedure WriteChar(const AValue: Char);
  begin
    if m + 1 >= lSize then
    begin
      SetLength(result, lSize * 2);
      lSize := lSize * 2;
    end;
    result[m] := AValue;
    inc(m);
  end;
 
begin
  i := 1;
  lSize := Length(Value);
  SetLength(result, lSize);
  m := 1;
  
  while i <=  Length(Value) do
  begin
    if (Value[i] = '&') then
    begin
      j := i;
      for k:=i+1 to Length(Value) do
        if not(Value[k] in ['a'..'z', 'A'..'Z', '0'..'9', '#']) then
        begin
          if Value[k] = ';' then
          begin
            st := LowerCase(Copy(Value, j + 1, (k - j) - 1));
            st2 := st;
            if (st <> '') then
            begin
              i := k;
              if st[1] = '#' then
              begin
                st := Copy(st, 2, MAXINT);
                l := StrToIntDef(st, -1);
                WriteChar(Char(l));
              end
              else if st = 'lt' then
                WriteChar('<')
              else if st = 'gt' then
                WriteChar('>')
              else if st = 'amp' then
                WriteChar('&')
              else if st = 'quot' then
                WriteChar('"')
              else if st = 'apos' then
                WriteChar('''')
              else
                i := j;
            end;
          end;
          Break;
        end;
      if i = j then
        WriteChar('&');  
    end
    else
      WriteChar(Value[i]);
    inc(i);
  end;
  SetLength(result, m-1);
end;

//=== TJvSimpleXml ===========================================================

constructor TJvSimpleXml.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRoot := TJvSimpleXmlElemClassic.Create(nil);
  FProlog := TJvSimpleXmlElemsProlog.Create;
end;

destructor TJvSimpleXml.Destroy;
begin
  FreeAndNil(FRoot);
  FreeAndNil(FProlog);
  inherited Destroy;
end;

procedure TJvSimpleXml.DoLoadProgress(const APosition, ATotal: Integer);
begin
  if Assigned(FOnLoadProg) then
    FOnLoadProg(Self, APosition, ATotal);
end;

procedure TJvSimpleXml.DoSaveProgress;
begin
  if Assigned(FOnSaveProg) then
  begin
    Inc(FSaveCount);
    FOnSaveProg(Self, FSaveCurrent, FSaveCount);
  end;
end;

procedure TJvSimpleXml.DoTagParsed(const AName: string);
begin
  if Assigned(FOnTagParsed) then
    FOnTagParsed(Self, AName);
end;

procedure TJvSimpleXml.DoValueParsed(const AName, AValue: string);
begin
  if Assigned(FOnValue) then
    FOnValue(Self, AName, AValue);
end;

procedure TJvSimpleXml.LoadFromFile(const FileName: TFileName);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Stream.LoadFromFile(FileName);
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJvSimpleXml.LoadFromResourceName(Instance: THandle;
  const ResName: string);
const
  RT_RCDATA = PChar(10);
var
  Stream: TResourceStream;
begin
  Stream := TResourceStream.Create(Instance, ResName, RT_RCDATA);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJvSimpleXml.LoadFromStream(const Stream: TStream);
var
 APosition, ALength: Integer;
 AData: string;
begin
  FRoot.Clear;
  FProlog.Clear;
  SetLength(AData, Stream.Size);
  APosition := 1;
  ALength := Stream.Read(AData[1], Stream.Size);
  if Assigned(FOnLoadProg) then
  begin
    FOnLoadProg(Self, Stream.Position, Stream.Size);
    //Read doctype and so on
    FProlog.LoadFromArray(AData, APosition, ALength, self);
    //Read elements
    FRoot.LoadFromArray(AData, APosition, ALength, self);
    FOnLoadProg(Self, Stream.Position, Stream.Size);
  end
  else
  begin
    if Assigned(FOnTagParsed) or Assigned(FOnValue) then
    begin
      FProlog.LoadFromArray(AData, APosition, ALength, self);
      FRoot.LoadFromArray(AData, APosition, ALength, self);
    end
    else
    begin
      FProlog.LoadFromArray(AData, APosition, ALength);
      FRoot.LoadFromArray(AData, APosition, ALength);
    end;
  end;
end;

procedure TJvSimpleXml.LoadFromString(const Value: string);
var
 APosition, ALength: Integer;
 AData: string;
begin
  APosition := 1;
  ALength := Length(Value);
  AData := Value;
  FProlog.LoadFromArray(AData, APosition, ALength);
  FRoot.LoadFromArray(AData, APosition, ALength);
end;

procedure TJvSimpleXml.SaveToFile(FileName: TFileName);
var
  Stream: TFileStream;
begin
  if FileExists(FileName) then
  begin
    Stream := TFileStream.Create(FileName, fmOpenWrite);
    Stream.Size := 0;
  end
  else
    Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJvSimpleXml.SaveToStream(const Stream: TStream);
var
  lCount: Integer;
begin
  if Assigned(FOnSaveProg) then
  begin
    lCount := Root.ChildsCount + Prolog.Count;
    FSaveCount := lCount;
    FSaveCurrent := 0;
    FOnSaveProg(Self, 0, lCount);
    Prolog.SaveToStream(Stream, Self);
    Root.SaveToStream(Stream, '', Self);
    FOnSaveProg(Self, lCount, lCount);
  end
  else
  begin
    Prolog.SaveToStream(Stream);
    Root.SaveToStream(Stream);
  end;
end;

function TJvSimpleXml.SaveToString: string;
var
  LStream: TStringStream;
begin
  LStream := TStringStream.Create('');
  try
    SaveToStream(LStream);
    Result := LStream.DataString;
  finally
    LStream.Free;
  end;
end;

procedure TJvSimpleXml.SetFileName(Value: TFileName);
begin
  FFileName := Value;
  LoadFromFile(Value);
end;

//=== TJvSimpleXmlElem =======================================================

procedure TJvSimpleXmlElem.Assign(Value: TJvSimpleXmlElem);
var
  Elems: TJvSimpleXmlElem;
  Elem: TJvSimpleXmlElem;
  I: Integer;
begin
  Clear;
  if Value = nil then
    Exit;
  Elems := TJvSimpleXmlElem(Value);
  Name := Elems.Name;
  Self.Value := Elems.Value;
  for I := 0 to Elems.Properties.Count - 1 do
    Properties.Add(Elems.Properties[I].Name, Elems.Properties[I].Value);

  for I := 0 to Elems.Items.Count - 1 do
  begin
    Elem := Items.Add(Elems.Items[I].Name, Elems.Items[I].Value);
    Elem.Assign(TJvSimpleXmlElem(Elems.Items[I]));
  end;
end;

procedure TJvSimpleXmlElem.Clear;
begin
  if FItems <> nil then
    FItems.Clear;
  if FProps <> nil then
    Properties.Clear;
end;

constructor TJvSimpleXmlElem.Create(const AOwner: TJvSimpleXmlElem);
begin
  inherited Create;
  FName := '';
  FParent := TJvSimpleXmlElem(AOwner);
end;

destructor TJvSimpleXmlElem.Destroy;
begin
  Clear;
  if FItems <> nil then
    FItems.Free;
  if FProps <> nil then
    FProps.Free;
  inherited Destroy;
end;

procedure TJvSimpleXmlElem.GetBinaryValue(const Stream: TStream);
var
  I, J: Integer;
  St: string;
  Buf: array [0..cBufferSize-1] of Byte;
begin
  I := 1;
  J := 0;
  while I < Length(Value) do
  begin
    St := '$' + Value[I] + Value[I + 1];
    if J = cBufferSize-1 then //Buffered write to speed up the process a little
    begin
      Stream.Write(Buf, J);
      J := 0;
    end;
    Buf[J] := StrToIntDef(St, 0);
    Inc(J);
    Inc(I, 2);
  end;
  Stream.Write(Buf, J);
end;

function TJvSimpleXmlElem.GetBoolValue: Boolean;
begin
  Result := StrToBoolDef(Value, False);
end;

function TJvSimpleXmlElem.GetChildIndex(
  const AChild: TJvSimpleXmlElem): Integer;
begin
  if FItems = nil then
    Result := -1
  else
    Result := FItems.FElems.IndexOfObject(AChild);
end;

function TJvSimpleXmlElem.GetChildsCount: Integer;
var
  I: Integer;
begin
  Result := 1;
  if FItems <> nil then
    for I := 0 to FItems.Count - 1 do
      Result := Result + FItems[I].ChildsCount;
end;

function TJvSimpleXmlElem.GetIntValue: Int64;
begin
  Result := StrToInt64Def(Value, -1);
end;

function TJvSimpleXmlElem.GetItems: TJvSimpleXmlElems;
begin
  if FItems = nil then
    FItems := TJvSimpleXmlElems.Create(Self);
  Result := FItems;
end;

function TJvSimpleXmlElem.GetProps: TJvSimpleXmlProps;
begin
  if FProps = nil then
    FProps := TJvSimpleXmlProps.Create();
  Result := FProps;
end;

function TJvSimpleXmlElem.SaveToString: string;
var
  lStream: TStringStream;
begin
  lStream := TStringStream.Create('');
  try
    SaveToStream(lStream);
    Result := lStream.DataString;
  finally
    lStream.Free;
  end;
end;

procedure TJvSimpleXmlElem.SetBoolValue(const Value: Boolean);
begin
  FValue := BoolToStr(Value);
end;

procedure TJvSimpleXmlElem.SetIntValue(const Value: Int64);
begin
  FValue := IntToStr(Value);
end;

procedure TJvSimpleXmlElem.SetName(const Value: string);
begin
  if (Value <> FName) and (Value <> '') then
  begin
    if (Parent <> nil) and (FName <> '') then
      Parent.Items.DoItemRename(Self, Value);
    FName := Value;
  end;
end;

//=== TJvSimpleXmlElems ======================================================

function TJvSimpleXmlElems.Add(const Name: string): TJvSimpleXmlElemClassic;
begin
  Result := TJvSimpleXmlElemClassic.Create(Parent);
  Result.FName := Name; //Directly set parent to avoid notification
  AddChild(Result);
end;

function TJvSimpleXmlElems.Add(const Name, Value: string): TJvSimpleXmlElemClassic;
begin
  Result := TJvSimpleXmlElemClassic.Create(Parent);
  Result.Name := Name;
  Result.Value := Value;
  AddChild(Result);
end;

function TJvSimpleXmlElems.Add(const Name: string; const Value: Int64): TJvSimpleXmlElemClassic;
begin
  Result := Add(Name, IntToStr(Value));
end;

function TJvSimpleXmlElems.Add(Value: TJvSimpleXmlElem): TJvSimpleXmlElem;
begin
  if Value <> nil then
    AddChild(Value);
  Result := Value;
end;

function TJvSimpleXmlElems.Add(const Name: string;
  const Value: Boolean): TJvSimpleXmlElemClassic;
begin
  Result := Add(Name, BoolToStr(Value));
end;

function TJvSimpleXmlElems.Add(const Name: string;
  const Value: TStream): TJvSimpleXmlElemClassic;
var
  Stream: TStringStream;
  Buf: array [0..cBufferSize-1] of Byte;
  St: string;
  I, Count: Integer;
begin
  Stream := TStringStream.Create('');
  repeat
    St := '';
    Count := Value.Read(Buf, SizeOf(Buf));
    for I := 0 to Count - 1 do
      St := St + IntToHex(Buf[I], 2);
    Stream.WriteString(St);
  until Count = 0;
  Result := Add(Name, Stream.DataString);
  Stream.Free;
end;

procedure TJvSimpleXmlElems.AddChild(const Value: TJvSimpleXmlElem);
begin
  CreateElems;
  FElems.AddObject(Value.Name, Value);
end;

procedure TJvSimpleXmlElems.AddChildFirst(const Value: TJvSimpleXmlElem);
begin
  CreateElems;
  FElems.InsertObject(0, Value.Name, Value);
end;

function TJvSimpleXmlElems.AddFirst(const Name: string): TJvSimpleXmlElemClassic;
begin
  Result := TJvSimpleXmlElemClassic.Create(Parent);
  Result.FName := Name; //Directly set parent to avoid notification
  AddChildFirst(Result);
end;

function TJvSimpleXmlElems.AddFirst(Value: TJvSimpleXmlElem): TJvSimpleXmlElem;
begin
  if Value <> nil then
    AddChildFirst(Value);
  Result := Value;
end;

function TJvSimpleXmlElems.AddComment(const Name,
  Value: string): TJvSimpleXmlElemComment;
begin
  Result := TJvSimpleXmlElemComment.Create(Parent);
  Result.FName := Name;
  Result.Value := Value;
  AddChild(Result);
end;

function TJvSimpleXmlElems.AddCData(const Name, Value: string): TJvSimpleXmlElemCDATA;
begin
  Result := TJvSimpleXmlElemCDATA.Create(Parent);
  Result.FName := Name;
  Result.Value := Value;
  AddChild(Result);
end;

function TJvSimpleXmlElems.AddText(const Name, Value: string): TJvSimpleXmlElemText;
begin
  Result := TJvSimpleXmlElemText.Create(Parent);
  Result.FName := Name;
  Result.Value := Value;
  AddChild(Result);
end;

procedure TJvSimpleXmlElems.BinaryValue(const Name: string;
  const Stream: TStream);
var
  Elem: TJvSimpleXmlElem;
begin
  Elem := GetItemNamed(Name);
  if Elem <> nil then
    Elem.GetBinaryValue(Stream);
end;

function TJvSimpleXmlElems.BoolValue(const Name: string;
  Default: Boolean): Boolean;
var
  Elem: TJvSimpleXmlElem;
begin
  try
    Elem := GetItemNamed(Name);
    if (Elem = nil) or (Elem.Value = '') then
      Result := Default
    else
      Result := Elem.BoolValue;
  except
    Result := Default;
  end;
end;

procedure TJvSimpleXmlElems.Clear;
var
  I: Integer;
begin
  if FElems = nil then
    Exit;
  for I := 0 to FElems.Count - 1 do
  try
    TJvSimpleXmlElem(FElems.Objects[I]).Clear;
    TJvSimpleXmlElem(FElems.Objects[I]).Free;
  except
  end;
  FElems.Clear;
end;

constructor TJvSimpleXmlElems.Create(const AOwner: TJvSimpleXmlElem);
begin
  FParent := AOwner;
end;

procedure TJvSimpleXmlElems.Delete(const Index: Integer);
begin
  if (FElems <> nil) and (Index >= 0) and (Index < FElems.Count) then
  begin
    TJvSimpleXmlElem(FElems.Objects[Index]).Clear;
    TJvSimpleXmlElem(FElems.Objects[Index]).Free;
    FElems.Delete(Index);
  end;
end;

procedure TJvSimpleXmlElems.CreateElems;
begin
  if FElems = nil then
  begin
    //AddToLog('TJvSimpleXmlElems.CreateElems '); //XXX
    FElems := TStringListEx.Create;
    FElems.CaseSensitive := false;
  end;
end;

procedure TJvSimpleXmlElems.Delete(const Name: string);
begin
  if FElems <> nil then
    Delete(FElems.IndexOf(Name));
end;

destructor TJvSimpleXmlElems.Destroy;
begin
  Clear;
  if FElems <> nil then
    FreeAndNil(FElems);
  inherited Destroy;
end;

procedure TJvSimpleXmlElems.DoItemRename(var Value: TJvSimpleXmlElem;
  const Name: string);
var
  I: Integer;
begin
  I := FElems.IndexOfObject(Value);
  if I <> -1 then
    FElems[I] := Name;
end;

function TJvSimpleXmlElems.GetCount: Integer;
begin
  if FElems = nil then
    Result := 0
  else
    Result := FElems.Count;
end;

function TJvSimpleXmlElems.GetItem(const Index: Integer): TJvSimpleXmlElem;
begin
  if (FElems = nil) or (Index > FElems.Count) then
    Result := nil
  else
    Result := TJvSimpleXmlElem(FElems.Objects[Index]);
end;

function TJvSimpleXmlElems.GetItemNamed(const Name: string): TJvSimpleXmlElem;
var
  I: Integer;
begin
  Result := nil;
  if FElems <> nil then
  begin
    I := FElems.IndexOf(Name);
    if I <> -1 then
      Result := TJvSimpleXmlElem(FElems.Objects[I])
  end;
end;

function TJvSimpleXmlElems.IntValue(const Name: string; Default: Int64): Int64;
var
  Elem: TJvSimpleXmlElem;
begin
  Elem := GetItemNamed(Name);
  if Elem = nil then
    Result := Default
  else
    Result := Elem.IntValue;
end;

function TJvSimpleXmlElems.LoadFromArray(var AData: string; var APosition,
  ALength: Integer; AParent: TJvSimpleXml): string;
var
  lPos: Integer;
  St: string;
  lElem: TJvSimpleXmlElem;
begin
  Result := '';
  St := '';
  lPos := 0;

  while (APosition <= ALength) do
  begin
    if AParent <> nil then
      AParent.DoLoadProgress(APosition, Alength);

    case lPos of
      0: //We are waiting for a tag and thus avoiding spaces
        begin
          case AData[APosition] of
            ' ', #9, #13, #10:
              begin
              end;
            '<':
              begin
                lPos := 1;
                St := AData[APosition];
              end;
          else
            begin
              //This is a text
              lElem := TJvSimpleXmlElemText.Create(Parent);
              lElem.LoadFromArray(AData, APosition, ALength, AParent);
              if FElems = nil then
              begin
                CreateElems;
                FElems.BeginUpdate;
              end;
              FElems.AddObject(lElem.Name, lElem);
            end;
          end;
        end;

      1: //We are trying to determine the kind of the tag
        begin
          lElem := nil;
          case AData[APosition] of
            '/':
              if St = '<' then
              begin
                lPos := 2;
                St := '';
              end
              else
              begin
                lElem := TJvSimpleXmlElemClassic.Create(Parent);
                St := St + AData[APosition];
              end;

            ' ', '>', ':': //This should be a classic tag
              begin
                lElem := TJvSimpleXmlElemClassic.Create(Parent);
                St := St + AData[APosition];
              end;
          else
            begin
              St := St + AData[APosition];
              if St = '<![CDATA[' then
                lElem := TJvSimpleXmlElemCData.Create(Parent)
              else
              if St = '<!--' then
                lElem := TJvSimpleXmlElemComment.Create(Parent);
                //<?
            end;
          end;

          if lElem <> nil then
          begin
            if FElems = nil then
            begin
              CreateElems;
              FElems.BeginUpdate;
            end;
            APosition := APosition - Length(st);
            inc(APosition);
            lElem.LoadFromArray(AData, APosition, ALength, AParent);
            FElems.AddObject(lElem.Name, lElem);
            lPos := 0;
            St := '';
          end;
        end;

      2: //This is an end tag
        if AData[APosition] = '>' then
        begin
          Result := St;
          Break;
        end
        else
          St := St + AData[APosition];
    end;
    inc(APosition);
  end;

  if FElems <> nil then
    FElems.EndUpdate;
end;

//returns name of tag ending
function TJvSimpleXmlElems.LoadFromStream(const Stream: TStream; AParent: TJvSimpleXml): string;
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array [0..cBufferSize-1] of Char;
  St: string;
  lElem: TJvSimpleXmlElem;
begin
  lStreamPos := Stream.Position;
  Result := '';
  St := '';
  lPos := 0;

  repeat
    Count := Stream.Read(lBuf, SizeOf(lBuf));
    if AParent <> nil then
      AParent.DoLoadProgress(Stream.Position, Stream.Size);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        0: //We are waiting for a tag and thus avoiding spaces
          begin
            case lBuf[I] of
              ' ', #9, #13, #10:
                begin
                end;
              '<':
                begin
                  lPos := 1;
                  St := lBuf[I];
                end;
            else
              begin
                  //This is a text
                lElem := TJvSimpleXmlElemText.Create(Parent);
                Stream.Seek(lStreamPos - 1, soFromBeginning);
                lElem.LoadFromStream(Stream);
                lStreamPos := Stream.Position;
                if FElems = nil then
                begin
                  CreateElems;
                  FElems.BeginUpdate;
                end;
                FElems.AddObject(lElem.Name, lElem);
                Break;
              end;
            end;
          end;

        1: //We are trying to determine the kind of the tag
          begin
            lElem := nil;
            case lBuf[I] of
              '/':
                if St = '<' then
                begin
                  lPos := 2;
                  St := '';
                end
                else
                begin
                  lElem := TJvSimpleXmlElemClassic.Create(Parent);
                  St := St + lBuf[I];
                end;

              ' ', '>', ':': //This should be a classic tag
                begin
                  lElem := TJvSimpleXmlElemClassic.Create(Parent);
                  St := St + lBuf[I];
                end;
            else
              begin
                St := St + lBuf[I];
                if St = '<![CDATA[' then
                  lElem := TJvSimpleXmlElemCData.Create(Parent)
                else
                if St = '<!--' then
                  lElem := TJvSimpleXmlElemComment.Create(Parent);
                  //<?
              end;
            end;

            if lElem <> nil then
            begin
              if FElems = nil then
              begin
                CreateElems;
                FElems.BeginUpdate;
              end;
              Stream.Seek(lStreamPos - (Length(St)), soFromBeginning);
              lElem.LoadFromStream(Stream);
              lStreamPos := Stream.Position;
              FElems.AddObject(lElem.Name, lElem);
              St := '';
              lPos := 0;
              Break;
            end;
          end;

        2: //This is an end tag
          if lBuf[I] = '>' then
          begin
            Result := St;
            Count := 0;
            Break;
          end
          else
            St := St + lBuf[I];
      end;
    end;
  until Count = 0;

  Stream.Seek(lStreamPos, soFromBeginning);
  if FElems <> nil then
    FElems.EndUpdate;
end;

procedure TJvSimpleXmlElems.SaveToStream(const Stream: TStream;
  const Level: string; Parent: TJvSimpleXml);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Item[I].SaveToStream(Stream, Level, Parent);
end;

function TJvSimpleXmlElems.Value(const Name: string; Default: string): string;
var
  Elem: TJvSimpleXmlElem;
begin
  Result := '';
  Elem := GetItemNamed(Name);
  if Elem = nil then
    Result := Default
  else
    Result := Elem.Value;
end;

function SortItems(List: TStringList; Index1, Index2: Integer): Integer;
var
  i: Integer;
begin
  result := 0;
  for i:=0 to GSorts.Count-1 do
    if TJvSimpleXmlElems(GSorts[i]).FElems = List then
    begin
      result := TJvSimpleXmlElems(GSorts[i]).FCompare(TJvSimpleXmlElems(GSorts[i]), Index1, Index2);
      Exit;
    end;
end;

procedure TJvSimpleXmlElems.CustomSort(
  AFunction: TJvSimpleXmlElemCompare);
begin
  if FElems<>nil then
  begin
    GSorts.Add(self);
    FCompare := AFunction;
    FElems.CustomSort(SortItems);
    GSorts.Remove(self);
  end;
end;

procedure TJvSimpleXmlElems.Sort;
begin
  if FElems<>nil then
    FElems.Sort;
end;


//=== TJvSimpleXmlProps ======================================================

function TJvSimpleXmlProps.Add(const Name, Value: string): TJvSimpleXmlProp;
var
  Elem: TJvSimpleXmlProp;
begin
  if FProperties = nil then
  begin
    //AddToLog('TJvSimpleXmlProprs.Add'); //XXX
    FProperties := TStringListEx.Create;
    FProperties.CaseSensitive := false;
  end;
  Elem := TJvSimpleXmlProp.Create();
  FProperties.AddObject(Name, Elem);
  Elem.FName := Name; //Avoid notification
  Elem.Value := Value;
  Elem.Parent := Self;
  Result := Elem;
end;

function TJvSimpleXmlProps.Add(const Name: string; const Value: Int64): TJvSimpleXmlProp;
begin
  Result := Add(Name, IntToStr(Value));
end;

function TJvSimpleXmlProps.Add(const Name: string; const Value: Boolean): TJvSimpleXmlProp;
begin
  Result := Add(Name, BoolToStr(Value));
end;

function TJvSimpleXmlProps.BoolValue(const Name: string;
  Default: Boolean): Boolean;
var
  Prop: TJvSimpleXmlProp;
begin
  try
    Prop := GetItemNamed(Name);
    if (Prop = nil) or (Prop.Value = '') then
      Result := Default
    else
      Result := Prop.BoolValue;
  except
    Result := Default;
  end;
end;

procedure TJvSimpleXmlProps.Clear;
var
  I: Integer;
begin
  if FProperties = nil then
    Exit;
  for I := 0 to FProperties.Count - 1 do
  try
    TJvSimpleXmlProp(FProperties.Objects[I]).Free;
  except
  end;
  FProperties.Clear;
end;

procedure TJvSimpleXmlProps.Delete(const Index: Integer);
begin
  if (FProperties <> nil) and (Index >= 0) and (Index < FProperties.Count) then
  begin
    TJvSimpleXmlProp(FProperties.Objects[Index]).Free; 
    FProperties.Delete(Index);
  end;
end;

procedure TJvSimpleXmlProps.Delete(const Name: string);
begin
  if FProperties <> nil then
    Delete(FProperties.IndexOf(Name));
end;

destructor TJvSimpleXmlProps.Destroy;
begin
  Clear;
  if FProperties <> nil then
    FreeAndNil(FProperties);
  inherited Destroy;
end;

procedure TJvSimpleXmlProps.DoItemRename(var Value: TJvSimpleXmlProp;
  const Name: string);
var
  I: Integer;
begin
  I := FProperties.IndexOfObject(Value);
  if I <> -1 then
    FProperties[I] := Name;
end;

function TJvSimpleXmlProps.GetCount: Integer;
begin
  if FProperties = nil then
    Result := 0
  else
    Result := FProperties.Count;
end;

function TJvSimpleXmlProps.GetItem(const Index: Integer): TJvSimpleXmlProp;
begin
  if FProperties <> nil then
    Result := TJvSimpleXmlProp(FProperties.Objects[Index])
  else
    Result := nil;
end;

function TJvSimpleXmlProps.GetItemNamed(const Name: string): TJvSimpleXmlProp;
var
  I: Integer;
begin
  Result := nil;
  if FProperties <> nil then
  begin
    I := FProperties.IndexOf(Name);
    if I <> -1 then
      Result := TJvSimpleXmlProp(FProperties.Objects[I])
  end;
end;

function TJvSimpleXmlProps.IntValue(const Name: string; Default: Int64): Int64;
var
  Prop: TJvSimpleXmlProp;
begin
  Prop := GetItemNamed(Name);
  if Prop = nil then
    Result := Default
  else
    Result := StrToIntDef(Prop.Value, Default);
end;

procedure TJvSimpleXmlProps.LoadFromArray(var AData: string; var APosition,
  ALength: Integer);
var
  lPos, j, lSize, j2, lSize2: Integer;
  lName, lValue, lPointer: string;
  lPropStart: Char;

  procedure WriteChar(const AValue: Char);
  begin
    if j + 1 >= lSize then
    begin
      SetLength(lValue, lSize * 2);
      lSize := lSize * 2;
    end;
    lValue[j] := AValue;
    inc(j);
  end;

  procedure WriteChar2(const AValue: Char);
  begin
    if j2 + 1 >= lSize2 then
    begin
      SetLength(lName, lSize2 * 2);
      lSize2 := lSize2 * 2;
    end;
    lName[j2] := AValue;
    inc(j2);
  end;

begin
  lPointer := '';
  lPropStart := ' ';
  lPos := 0;
  lSize := 2048;
  SetLength(lValue, lSize);
  j := 1;
  lSize2 := 2048;
  SetLength(lName, lSize2);
  j2 := 1;

  if FProperties = nil then
  begin
    //AddToLog('TJvSimpleXmlProps.LoadFromArray '+Copy(AData, APosition, 20)); //XXX
    FProperties := TStringListEx.Create;
    FProperties.CaseSensitive := false;
    FProperties.BeginUpdate;
  end;

  while (APosition <= ALength) do
  begin
    case lPos of
      0: //We are waiting for a property
        begin
          case AData[APosition] of
            ' ', #9, #10, #13:
              begin
              end;
            'a'..'z', 'A'..'Z', '0'..'9', '-', '_':
              begin
                j2 := 1;
                WriteChar2(AData[APosition]);
                lPos := 1;
              end;
            '/', '>', '?':
              begin
                Dec(APosition);
                Break;
              end;
          else
            raise TJvSimpleXmlInvalid.Create('Invalid XML Element: Unexpected character in properties declaration (' +
              AData[APosition] + ' found).');
          end;
        end;

      1: //We are reading a property name
        case AData[APosition] of
          'a'..'z', 'A'..'Z', '0'..'9', '-', '_':
            WriteChar2(AData[APosition]);
          ':':
            begin
              SetLength(lName, j2 - 1);
              lPointer := lName;
              j2 := 1;
              SetLength(lName, lSize2);
            end;
          '=':
            lPos := 2;
        else
          raise TJvSimpleXmlInvalid.Create('Invalid XML Element: Unexpected character in properties declaration (' +
            AData[APosition] + ' found).');
        end;

      2: //We are going to start a property content
        if AData[APosition] in ['''', '"'] then
        begin
          lPropStart := AData[APosition];
          j := 1;
          lPos := 3;
        end
        else
          raise
            TJvSimpleXmlInvalid.Create('Invalid XML Element: Unexpected character in property declaration. Expecting " or '' but ' + AData[APosition] + ' found.');

      3: //We are reading a property
        if AData[APosition] = lPropStart then
        begin
          SetLength(lValue, j-1);
          SetLength(lName, j2-1);
          lValue := SimpleXmlDecode(lValue);
          with Add(lName, lValue) do
            Pointer := lPointer;
          SetLength(lValue, lSize);
          SetLength(lName, lSize2);
          lPos := 0;
        end
        else
          WriteChar(AData[APosition]);
    end;
    inc(APosition);
  end;

  if FProperties <> nil then
    FProperties.EndUpdate;
end;

procedure TJvSimpleXmlProps.LoadFromStream(const Stream: TStream);
//<element Prop="foo" Prop='bar' foo:bar="beuh"/>
//Stop on / or ? or >
var
  I, lStreamPos, Count, lPos, j, lSize, j2, lSize2: Integer;
  lBuf: array [0..cBufferSize-1] of Char;
  lName, lValue, lPointer: string;
  lPropStart: Char;

  procedure WriteChar(const AValue: Char);
  begin
    if j + 1 >= lSize then
    begin
      SetLength(lValue, lSize * 2);
      lSize := lSize * 2;
    end;
    lValue[j] := AValue;
    inc(j);
  end;

  procedure WriteChar2(const AValue: Char);
  begin
    if j2 + 1 >= lSize2 then
    begin
      SetLength(lName, lSize2 * 2);
      lSize2 := lSize2 * 2;
    end;
    lName[j2] := AValue;
    inc(j2);
  end;

begin
  lStreamPos := Stream.Position;
  lPointer := '';
  lPropStart := ' ';
  lPos := 0;
  lSize := 2048;
  SetLength(lValue, lSize);
  j := 1;
  lSize2 := 2048;
  SetLength(lName, lSize2);
  j2 := 1;

  if FProperties = nil then
  begin
    //AddToLog('TJvSimpleXmlElems.LoadFromStream'); //XXX
    FProperties := TStringListEx.Create;
    FProperties.CaseSensitive := false;
    FProperties.BeginUpdate;
  end;

  repeat
    Count := Stream.Read(lBuf, SizeOf(lBuf));
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        0: //We are waiting for a property
          begin
            case lBuf[I] of
              ' ', #9, #10, #13:
                begin
                end;
              'a'..'z', 'A'..'Z', '0'..'9', '-', '_':
                begin
                  j2 := 1;
                  WriteChar2(lBuf[i]);
                  lPos := 1;
                end;
              '/', '>', '?':
                begin
                  Dec(lStreamPos);
                  Count := 0;
                  Break;
                end;
            else
              raise TJvSimpleXmlInvalid.Create('Invalid XML Element: Unexpected character in properties declaration (' +
                lBuf[I] + ' found).');
            end;
          end;

        1: //We are reading a property name
          case lBuf[I] of
            'a'..'z', 'A'..'Z', '0'..'9', '-', '_':
              WriteChar2(lBuf[i]);
            ':':
              begin
                SetLength(lName, j2 - 1);
                lPointer := lName;
                j2 := 1;
                SetLength(lName, lSize2);
              end;
            '=':
              lPos := 2;
          else
            raise TJvSimpleXmlInvalid.Create('Invalid XML Element: Unexpected character in properties declaration (' +
              lBuf[I] + ' found).');
          end;

        2: //We are going to start a property content
          if lBuf[I] in ['''', '"'] then
          begin
            lPropStart := lBuf[I];
            j := 1;
            lPos := 3;
          end
          else
            raise
              TJvSimpleXmlInvalid.Create('Invalid XML Element: Unexpected character in property declaration. Expecting " or '' but ' + lBuf[I] + ' found.');

        3: //We are reading a property
          if lBuf[I] = lPropStart then
          begin
            SetLength(lValue, j-1);
            SetLength(lName, j2-1);
            lValue := SimpleXmlDecode(lValue);
            with Add(lName, lValue) do
              Pointer := lPointer;
            SetLength(lValue, lSize);
            SetLength(lName, lSize2);
            lPos := 0;
          end
          else
            WriteChar(lBuf[i]);
      end;
    end;
  until Count = 0;

  Stream.Seek(lStreamPos, soFromBeginning);
  if FProperties <> nil then
    FProperties.EndUpdate;
end;

procedure TJvSimpleXmlProps.SaveToStream(const Stream: TStream);
var
  St: string;
  I: Integer;
begin
  St := '';
  for I := 0 to Count - 1 do
    St := St + Item[I].SaveToString;
  if St <> '' then
    Stream.Write(St[1], Length(St));
end;

function TJvSimpleXmlProps.Value(const Name: string; Default: string): string;
var
  Prop: TJvSimpleXmlProp;
begin
  Result := '';
  Prop := GetItemNamed(Name);
  if Prop = nil then
    Result := Default
  else
    Result := Prop.Value;
end;

//=== TJvSimpleXmlProp =======================================================

function TJvSimpleXmlProp.GetBoolValue: Boolean;
begin
  Result := StrToBoolDef(Value, False);
end;

function TJvSimpleXmlProp.GetIntValue: Int64;
begin
  Result := StrToInt64Def(Value, -1);
end;

function TJvSimpleXmlProp.SaveToString: string;
begin
  if Pointer <> '' then
    Result := Format(' %s:%s="%s"', [Pointer, Name, SimpleXmlEncode(Value)])
  else
    Result := Format(' %s="%s"', [Name, SimpleXmlEncode(Value)]);
end;

procedure TJvSimpleXmlProp.SetBoolValue(const Value: Boolean);
begin
  FValue := BoolToStr(Value);
end;

procedure TJvSimpleXmlProp.SetIntValue(const Value: Int64);
begin
  FValue := IntToStr(Value);
end;

procedure TJvSimpleXmlProp.SetName(const Value: string);
begin
  if (Value <> FName) and (Value <> '') then
  begin
    if (Parent <> nil) and (FName <> '') then
      Parent.DoItemRename(Self, Value);
    FName := Value;
  end;
end;

//=== TJvSimpleXmlElemClassic ================================================

procedure TJvSimpleXmlElemClassic.LoadFromArray(var AData: string;
  var APosition, ALength: Integer; Parent: TJvSimpleXml);
//<element Prop="foo" Prop='bar'/>
//<element Prop="foo" Prop='bar'>foor<b>beuh</b>bar</element>
//<xml:element Prop="foo" Prop='bar'>foor<b>beuh</b>bar</element>
var
  lPos, j, lSize: Integer;
  St, lName, lValue, lPointer: string;

  procedure WriteChar(const AValue: Char);
  begin
    if j + 1 >= lSize then
    begin
      SetLength(st, lSize * 2);
      lSize := lSize * 2;
    end;
    st[j] := AValue;
    inc(j);
  end;

  procedure WriteString(const AValue: UTF8String);
  var
   i,k: Integer;
  begin
    k := Length(AValue);
    if j + k + 1 >= lSize then
    begin
      lSize := lSize * 2 + k;
      SetLength(St, lSize);
    end;
    for i:=1 to k do
    begin
      St[j] := AValue[i];
      inc(j);
    end;
  end;

begin
  lValue := '';
  lPointer := '';
  lPos := 1;
  lSize := 2048;
  SetLength(st, lSize);
  j := 1;

  while (APosition <= ALength) do
  begin
    if Parent <> nil then
      Parent.DoLoadProgress(APosition, ALength);

    case lPos of
      1:
        if AData[APosition] = '<' then
          lPos := 2
        else
          raise TJvSimpleXmlInvalid.Create('Invalid XML Element: Expected beginning of tag but ' + AData[APosition] +
            ' found.');
      -1:
        if AData[APosition] = '>' then
          Break
        else
          raise TJvSimpleXmlInvalid.Create('Invalid XML Element: Expected end of tag but ' + AData[APosition] + ' found.');
    else
      begin
        if AData[APosition] in [' ', #9, #10, #13] then
        begin
          if lPos = 2 then
            raise TJvSimpleXmlInvalid.Create('Invalid XML Element: Malformed tag found (no valid name)');
          Properties.LoadFromArray(AData, APosition, ALength);
        end
        else
        begin
          case AData[APosition] of
            '>':
              begin
                SetLength(st, j-1);
                lName := St;
                SetLength(st, lSize);
                j := 1;
                //Load elements
                inc(APosition);
                WriteString(Items.LoadFromArray(AData, APosition, ALength, Parent));
                SetLength(st, j-1);
                if lName <> St then
                  raise TJvSimpleXmlInvalid.Create('Invalid XML Element: Erroneous end of tag, expecting </' + lName +
                    '> but </' + St + '> found.');
                SetLength(st, lSize);

                //Set value if only one sub element
                //This might reduce speed, but this is for compatibility issues
                if (Items.Count = 1) and (Items[0] is TJvSimpleXmlElemText) then
                begin
                  lValue := Items[0].Value;
                  Items.Clear;
                end;
                Break;
              end;
            '/':
              begin
                SetLength(st, j-1);
                lName := St;
                SetLength(st, lSize);
                lPos := -1;
              end;
            ':':
              begin
                SetLength(st, j-1);
                lPointer := St;
                SetLength(st, lSize);
                j := 1;
              end;
          else
            begin
              WriteChar(AData[APosition]);
              Inc(lPos);
            end;
          end;
        end;
      end;
    end;
    inc(APosition);
  end;

  Name := lName;
  Value := lValue;
  Pointer := lPointer;

  if Parent <> nil then
  begin
    Parent.DoTagParsed(lName);
    Parent.DoValueParsed(lName, lValue);
  end;
end;

procedure TJvSimpleXmlElemClassic.LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml);
//<element Prop="foo" Prop='bar'/>
//<element Prop="foo" Prop='bar'>foor<b>beuh</b>bar</element>
//<xml:element Prop="foo" Prop='bar'>foor<b>beuh</b>bar</element>
var
  I, lStreamPos, Count, lPos, j, lSize: Integer;
  lBuf: array [0..cBufferSize-1] of Char;
  St, lName, lValue, lPointer: string;

  procedure WriteChar(const AValue: Char);
  begin
    if j + 1 >= lSize then
    begin
      SetLength(st, lSize * 2);
      lSize := lSize * 2;
    end;
    st[j] := AValue;
    inc(j);
  end;

  procedure WriteString(const AValue: UTF8String);
  var
   i,k: Integer;
  begin
    k := Length(AValue);
    if j + k + 1 >= lSize then
    begin
      lSize := lSize * 2 + k;
      SetLength(St, lSize);
    end;
    for i:=1 to k do
    begin
      St[j] := AValue[i];
      inc(j);
    end;
  end;

begin
  lStreamPos := Stream.Position;
  lValue := '';
  lPointer := '';
  lPos := 1;
  lSize := 2048;
  SetLength(st, lSize);
  j := 1;

  repeat
    Count := Stream.Read(lBuf, SizeOf(lBuf));
    if Parent <> nil then
      Parent.DoLoadProgress(Stream.Position, Stream.Size);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        1:
          if lBuf[I] = '<' then
            lPos := 2
          else
            raise TJvSimpleXmlInvalid.Create('Invalid XML Element: Expected beginning of tag but ' + lBuf[I] +
              ' found.');
        -1:
          if lBuf[I] = '>' then
          begin
            Count := 0;
            Break;
          end
          else
            raise TJvSimpleXmlInvalid.Create('Invalid XML Element: Expected end of tag but ' + lBuf[I] + ' found.');
      else
        begin
          if lBuf[I] in [' ', #9, #10, #13] then
          begin
            if lPos = 2 then
              raise TJvSimpleXmlInvalid.Create('Invalid XML Element: Malformed tag found (no valid name)');
            Stream.Seek(lStreamPos, soFromBeginning);
            Properties.LoadFromStream(Stream);
            lStreamPos := Stream.Position;
            Break; //Re read buffer
          end
          else
          begin
            case lBuf[I] of
              '>':
                begin
                  SetLength(st, j-1);
                  lName := St;
                  SetLength(st, lSize);
                  j := 1;
                  //Load elements
                  Stream.Seek(lStreamPos, soFromBeginning);
                  WriteString(Items.LoadFromStream(Stream, Parent));
                  SetLength(st, j-1);
                  if lName <> St then
                    raise TJvSimpleXmlInvalid.Create('Invalid XML Element: Erroneous end of tag, expecting </' + lName +
                      '> but </' + St + '> found.');
                  SetLength(st, lSize);
                  lStreamPos := Stream.Position;

                  //Set value if only one sub element
                  //This might reduce speed, but this is for compatibility issues
                  if (Items.Count = 1) and (Items[0] is TJvSimpleXmlElemText) then
                  begin
                    lValue := Items[0].Value;
                    Items.Clear;
                  end;

                  Count := 0;
                  Break;
                end;
              '/':
                begin
                  SetLength(st, j-1);
                  lName := St;
                  SetLength(st, lSize);
                  lPos := -1;
                end;
              ':':
                begin
                  SetLength(st, j-1);
                  lPointer := St;
                  SetLength(st, lSize);
                  j := 1;
                end;
            else
              begin
                WriteChar(lBuf[I]);
                Inc(lPos);
              end;
            end;
          end;
        end;
      end;
    end;
  until Count = 0;

  Name := lName;
  Value := lValue;
  Pointer := lPointer;

  if Parent <> nil then
  begin
    Parent.DoTagParsed(lName);
    Parent.DoValueParsed(lName, lValue);
  end;

  Stream.Seek(lStreamPos, soFromBeginning);
end;

procedure TJvSimpleXmlElemClassic.SaveToStream(const Stream: TStream; const Level: string; Parent: TJvSimpleXml);
var
  St: string;
begin
  St := Level + '<' + Name;
  Stream.Write(St[1], Length(St));
  Properties.SaveToStream(Stream);

  if Items.Count = 0 then
  begin
    if Value = '' then
      St := '/>' + sLineBreak
    else
      St := '>' + SimpleXmlEncode(Value) + '</' + Name + '>' + sLineBreak;
    Stream.Write(St[1], Length(St));
  end
  else
  begin
    St := '>' + sLineBreak;
    Stream.Write(St[1], Length(St));
    Items.SaveToStream(Stream, Level + ' ', Parent);
    St := Level + '</' + Name + '>' + sLineBreak;
    Stream.Write(St[1], Length(St));
  end;
  if Parent <> nil then
    Parent.DoSaveProgress;
end;

//=== TJvSimpleXmlElemComment ================================================

const
  CS_START_COMMENT = '<!--';
  CS_STOP_COMMENT = '    -->';

procedure TJvSimpleXmlElemComment.LoadFromArray(var AData: string;
  var APosition, ALength: Integer; Parent: TJvSimpleXml);
var
 lPos: Integer;
 st: string;
 lOk: Boolean;
begin
  st := '';
  lPos := 1;
  lOk := False;

  while APosition < ALength do
  begin
    case lPos of
      1..4: //<!--
        if AData[APosition] = CS_START_COMMENT[lPos] then
          Inc(lPos)
        else
          raise TJvSimpleXmlInvalid.Create('Invalid Comment: Expected ' + CS_START_COMMENT[lPos] +
            ' Found ' + AData[APosition]);
      5:
        if AData[APosition] = CS_STOP_COMMENT[lPos] then
          Inc(lPos)
        else
          St := St + AData[APosition];
      6: //-
        if AData[APosition] = CS_STOP_COMMENT[lPos] then
          Inc(lPos)
        else
        begin
          St := St + '-' + AData[APosition];
          Dec(lPos);
        end;
      7: //>
        if AData[APosition] = CS_STOP_COMMENT[lPos] then
        begin
          lOk := True;
          Break; //End while
        end
        else
        begin
          St := St + '--' + AData[APosition];
          Dec(lPos, 2);
        end;
    end;
    inc(APosition);
  end;

  if not lOk then
    raise TJvSimpleXmlInvalid.Create('Invalid Comment: Unexpected end of data');

  Value := st;
  Name := '';

  if Parent <> nil then
    Parent.DoValueParsed('', St);
end;

procedure TJvSimpleXmlElemComment.LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml);
//<!-- declarations for <head> & <body> -->
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array [0..cBufferSize-1] of Char;
  St: string;
  lOk: Boolean;
begin
  lStreamPos := Stream.Position;
  St := '';
  lPos := 1;
  lOk := False;

  repeat
    Count := Stream.Read(lBuf, SizeOf(lBuf));
    if Parent <> nil then
      Parent.DoLoadProgress(Stream.Position, Stream.Size);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        1..4: //<!--
          if lBuf[I] = CS_START_COMMENT[lPos] then
            Inc(lPos)
          else
            raise TJvSimpleXmlInvalid.Create('Invalid Comment: Expected ' + CS_START_COMMENT[lPos] +
              ' Found ' + lBuf[I]);
        5:
          if lBuf[I] = CS_STOP_COMMENT[lPos] then
            Inc(lPos)
          else
            St := St + lBuf[I];
        6: //-
          if lBuf[I] = CS_STOP_COMMENT[lPos] then
            Inc(lPos)
          else
          begin
            St := St + '-' + lBuf[I];
            Dec(lPos);
          end;
        7: //>
          if lBuf[I] = CS_STOP_COMMENT[lPos] then
          begin
            Count := 0; //End repeat
            lOk := True;
            Break; //End if
          end
          else
          begin
            St := St + '--' + lBuf[I];
            Dec(lPos, 2);
          end;
      end;
    end;
  until Count = 0;

  if not lOk then
    raise TJvSimpleXmlInvalid.Create('Invalid Comment: Unexpected end of data');

  Value := St;
  Name := '';

  if Parent <> nil then
    Parent.DoValueParsed('', St);

  Stream.Seek(lStreamPos, soFromBeginning);
end;

procedure TJvSimpleXmlElemComment.SaveToStream(const Stream: TStream; const Level: string; Parent: TJvSimpleXml);
var
  St: string;
begin
  St := Level + '<!--';
  Stream.Write(St[1], Length(St));
  if Value <> '' then
    Stream.Write(Value[1], Length(Value));
  St := '-->' + sLineBreak;
  Stream.Write(St[1], Length(St));
  if Parent <> nil then
    Parent.DoSaveProgress;
end;

//=== TJvSimpleXmlElemCData ==================================================

const
  CS_START_CDATA = '<![CDATA[';
  CS_STOP_CDATA = '         ]]>';

procedure TJvSimpleXmlElemCData.LoadFromArray(var AData: string;
  var APosition, ALength: Integer; Parent: TJvSimpleXml);
var
  lPos: Integer;
  St: string;
  lOk: Boolean;
begin
  St := '';
  lPos := 1;
  lOk := False;

  while (APosition <= ALength) do
  begin
    if Parent <> nil then
      Parent.DoLoadProgress(APosition, ALength);

    case lPos of
      1..9: //<![CDATA[
        if AData[APosition] = CS_START_CDATA[lPos] then
          Inc(lPos)
        else
          raise TJvSimpleXmlInvalid.Create('Invalid CDATA: Expected ' + CS_START_CDATA[lPos] +
            ' Found ' + AData[APosition]);
      10:
        if AData[APosition] = CS_STOP_CDATA[lPos] then
          Inc(lPos)
        else
          St := St + AData[APosition];
      11: //-
        if AData[APosition] = CS_STOP_CDATA[lPos] then
          Inc(lPos)
        else
        begin
          St := St + ']' + AData[APosition];
          Dec(lPos);
        end;
      12: //>
        if AData[APosition] = CS_STOP_CDATA[lPos] then
        begin
          lOk := True;
          Break; //End while
        end
        else
        begin
          St := St + ']]' + AData[APosition];
          Dec(lPos, 2);
        end;
    end;
    inc(APosition);
  end;

  if not lOk then
    raise TJvSimpleXmlInvalid.Create('Invalid CDATA: Unexpected end of data');

  Value := St;
  Name := '';

  if Parent <> nil then
    Parent.DoValueParsed('', St);
end;

procedure TJvSimpleXmlElemCData.LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml);
//<![CDATA[<greeting>Hello, world!</greeting>]]>
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array [0..cBufferSize-1] of Char;
  St: string;
  lOk: Boolean;
begin
  lStreamPos := Stream.Position;
  St := '';
  lPos := 1;
  lOk := False;

  repeat
    Count := Stream.Read(lBuf, SizeOf(lBuf));
    if Parent <> nil then
      Parent.DoLoadProgress(Stream.Position, Stream.Size);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        1..9: //<![CDATA[
          if lBuf[I] = CS_START_CDATA[lPos] then
            Inc(lPos)
          else
            raise TJvSimpleXmlInvalid.Create('Invalid CDATA: Expected ' + CS_START_CDATA[lPos] +
              ' Found ' + lBuf[I]);
        10:
          if lBuf[I] = CS_STOP_CDATA[lPos] then
            Inc(lPos)
          else
            St := St + lBuf[I];
        11: //-
          if lBuf[I] = CS_STOP_CDATA[lPos] then
            Inc(lPos)
          else
          begin
            St := St + ']' + lBuf[I];
            Dec(lPos);
          end;
        12: //>
          if lBuf[I] = CS_STOP_CDATA[lPos] then
          begin
            Count := 0; //End repeat
            lOk := True;
            Break; //End if
          end
          else
          begin
            St := St + ']]' + lBuf[I];
            Dec(lPos, 2);
          end;
      end;
    end;
  until Count = 0;

  if not lOk then
    raise TJvSimpleXmlInvalid.Create('Invalid CDATA: Unexpected end of data');

  Value := St;
  Name := '';

  if Parent <> nil then
    Parent.DoValueParsed('', St);

  Stream.Seek(lStreamPos, soFromBeginning);
end;

procedure TJvSimpleXmlElemCData.SaveToStream(const Stream: TStream; const Level: string; Parent: TJvSimpleXml);
var
  St: string;
begin
  St := Level + '<![CDATA[';
  Stream.Write(St[1], Length(St));
  if Value <> '' then
    Stream.Write(Value[1], Length(Value));
  St := ']]>' + sLineBreak;
  Stream.Write(St[1], Length(St));
  if Parent <> nil then
    Parent.DoSaveProgress;
end;

//=== TJvSimpleXmlElemText ===================================================

procedure TJvSimpleXmlElemText.LoadFromArray(var AData: string;
  var APosition, ALength: Integer; Parent: TJvSimpleXml);
var
  lPos, j, lSize: Integer;
  St: string;

  procedure WriteChar(const AValue: Char);
  begin
    if j + 1 >= lSize then
    begin
      SetLength(St, lSize * 2);
      lSize := lSize * 2;
    end;
    St[j] := AValue;
    inc(j);
  end;

  procedure WriteString(const AValue: UTF8String);
  var
   i,k: Integer;
  begin
    k := Length(AValue);
    if j + k + 1 >= lSize then
    begin
      lSize := lSize * 2 + k;
      SetLength(St, lSize);
    end;
    for i:=1 to k do
    begin
      St[j] := AValue[i];
      inc(j);
    end;
  end;
  
begin
  lSize := 2048;
  SetLength(st, lSize);
  j := 1;
  lPos := 0;

  while (APosition <= ALength) do
  begin
    if Parent <> nil then
      Parent.DoLoadProgress(APosition, ALength);

    case AData[APosition] of
      '<':
        begin
          //Quit text
          Dec(APosition);
          Break;
        end;
      ' ':
        begin
          if lPos = 0 then
            Inc(lPos);
          WriteChar(' ');
        end;
    else
      begin
        lPos := 0;
        WriteChar(AData[APosition]);
      end;
    end;
    inc(APosition);
  end;

  SetLength(st, j-1);
  st := SimpleXmlDecode(St);
  Value := St;
  Name := '';

  if Parent <> nil then
    Parent.DoValueParsed('', St);
end;

procedure TJvSimpleXmlElemText.LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml);
var
  I, lStreamPos, Count, lPos, j, lSize: Integer;
  lBuf: array [0..cBufferSize-1] of Char;
  St: string;

  procedure WriteChar(const AValue: Char);
  begin
    if j + 1 >= lSize then
    begin
      SetLength(St, lSize * 2);
      lSize := lSize * 2;
    end;
    St[j] := AValue;
    inc(j);
  end;

  procedure WriteString(const AValue: UTF8String);
  var
   i,k: Integer;
  begin
    k := Length(AValue);
    if j + k + 1 >= lSize then
    begin
      lSize := lSize * 2 + k;
      SetLength(St, lSize);
    end;
    for i:=1 to k do
    begin
      St[j] := AValue[i];
      inc(j);
    end;
  end;
  
begin
  lStreamPos := Stream.Position;
  lSize := 2048;
  SetLength(st, lSize);
  j := 1;
  lPos := 0;

  repeat
    Count := Stream.Read(lBuf, SizeOf(lBuf));
    if Parent <> nil then
      Parent.DoLoadProgress(Stream.Position, Stream.Size);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lBuf[I] of
        '<':
          begin
            //Quit text
            Dec(lStreamPos);
            Count := 0;
            Break;
          end;
        ' ':
          begin
            if lPos = 0 then
              Inc(lPos);
            WriteChar(' ');
          end;
      else
        begin
          lPos := 0;
          WriteChar(lBuf[i]);
        end;
      end;
    end;
  until Count = 0;

  SetLength(st, j-1);
  st := SimpleXmlDecode(St);
  Value := St;
  Name := '';

  if Parent <> nil then
    Parent.DoValueParsed('', St);

  Stream.Seek(lStreamPos, soFromBeginning);
end;

procedure TJvSimpleXmlElemText.SaveToStream(const Stream: TStream; const Level: string; Parent: TJvSimpleXml);
var
  St: string;
begin
  if Value <> '' then
  begin
    St := Level + SimpleXmlEncode(Value) + sLineBreak;
    Stream.Write(St[1], Length(St));
  end;
  if Parent <> nil then
    Parent.DoSaveProgress;
end;

//=== TJvSimpleXmlElemHeader =================================================

constructor TJvSimpleXmlElemHeader.Create;
begin
  FVersion := '1.0';
  FEncoding := 'iso-8859-1';
  FStandalone := False;
end;

const
  CS_START_HEADER = '<?xml';
  CS_STOP_HEADER = '     ?>';

procedure TJvSimpleXmlElemHeader.LoadFromArray(var AData: string;
  var APosition, ALength: Integer; Parent: TJvSimpleXml);
var
  lPos: Integer;
  lOk: Boolean;
begin
  lPos := 1;
  lOk := False;

  while (APosition <= ALength) do
  begin
    if Parent <> nil then
      Parent.DoLoadProgress(APosition, ALength);

    case lPos of
      1..4: //<?xml
        if AData[APosition] = CS_START_HEADER[lPos] then
          Inc(lPos)
        else
          raise TJvSimpleXmlInvalid.Create('Invalid Header: Expected ' + CS_START_HEADER[lPos] +
            ' Found ' + AData[APosition]);
      5: //L
        if AData[APosition] = CS_START_HEADER[lPos] then
        begin
          inc(APosition);
          Properties.LoadFromArray(AData, APosition, ALength);
          Inc(lPos);

          FVersion := Properties.Value('version');
          FEncoding := Properties.Value('encoding');
          FStandalone := Properties.Value('standalone') = 'yes';

          Properties.Clear;
        end
        else
          raise TJvSimpleXmlInvalid.Create('Invalid Header: Expected ' + CS_START_HEADER[lPos] +
            ' Found ' + AData[APosition]);
      6: //?
        if AData[APosition] = CS_STOP_HEADER[lPos] then
          Inc(lPos)
        else
          raise TJvSimpleXmlInvalid.Create('Invalid Header: Expected ' + CS_STOP_HEADER[lPos] +
            ' Found ' + AData[APosition]);
      7: //>
        if AData[APosition] = CS_STOP_HEADER[lPos] then
        begin
          lOk := True;
          Break; //End while
        end
        else
          raise TJvSimpleXmlInvalid.Create('Invalid Header: Expected ' + CS_STOP_HEADER[lPos] +
            ' Found ' + AData[APosition]);
    end;
    inc(APosition);
  end;

  if not lOk then
    raise TJvSimpleXmlInvalid.Create('Invalid Comment: Unexpected end of data');

  Name := '';
end;

procedure TJvSimpleXmlElemHeader.LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml);
//<?xml version="1.0" encoding="iso-xyzxx" standalone="yes"?>
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array [0..cBufferSize-1] of Char;
  lOk: Boolean;
begin
  lStreamPos := Stream.Position;
  lPos := 1;
  lOk := False;

  repeat
    Count := Stream.Read(lBuf, SizeOf(lBuf));
    if Parent <> nil then
      Parent.DoLoadProgress(Stream.Position, Stream.Size);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        1..4: //<?xml
          if lBuf[I] = CS_START_HEADER[lPos] then
            Inc(lPos)
          else
            raise TJvSimpleXmlInvalid.Create('Invalid Header: Expected ' + CS_START_HEADER[lPos] +
              ' Found ' + lBuf[I]);
        5: //L
          if lBuf[I] = CS_START_HEADER[lPos] then
          begin
            Stream.Seek(lStreamPos, soFromBeginning);
            Properties.LoadFromStream(Stream);
            lStreamPos := Stream.Position;
            Inc(lPos);

            FVersion := Properties.Value('version');
            FEncoding := Properties.Value('encoding');
            FStandalone := Properties.Value('standalone') = 'yes';

            Properties.Clear;

            Break; //Re read buffer
          end
          else
            raise TJvSimpleXmlInvalid.Create('Invalid Header: Expected ' + CS_START_HEADER[lPos] +
              ' Found ' + lBuf[I]);
        6: //?
          if lBuf[I] = CS_STOP_HEADER[lPos] then
            Inc(lPos)
          else
            raise TJvSimpleXmlInvalid.Create('Invalid Header: Expected ' + CS_STOP_HEADER[lPos] +
              ' Found ' + lBuf[I]);
        7: //>
          if lBuf[I] = CS_STOP_HEADER[lPos] then
          begin
            Count := 0; //End repeat
            lOk := True;
            Break; //End if
          end
          else
            raise TJvSimpleXmlInvalid.Create('Invalid Header: Expected ' + CS_STOP_HEADER[lPos] +
              ' Found ' + lBuf[I]);
      end;
    end;
  until Count = 0;

  if not lOk then
    raise TJvSimpleXmlInvalid.Create('Invalid Comment: Unexpected end of data');

  Name := '';

  Stream.Seek(lStreamPos, soFromBeginning);
end;

procedure TJvSimpleXmlElemHeader.SaveToStream(const Stream: TStream;
  const Level: string; Parent: TJvSimpleXml);
var
  St: string;
begin
  St := Level + '<?xml version="' + FVersion + '"';
  if Standalone then
    St := St + ' standalone="yes"';
  if Encoding <> '' then
    St := St + ' encoding="' + Encoding + '"';
  St := St + '?>' + sLineBreak;
  Stream.Write(St[1], Length(St));
  if Parent <> nil then
    Parent.DoSaveProgress;
end;

//=== TJvSimpleXmlElemDocType ================================================

procedure TJvSimpleXmlElemDocType.LoadFromArray(var AData: string;
  var APosition, ALength: Integer; Parent: TJvSimpleXml);
const
  CS_START_DOCTYPE = '<!DOCTYPE';
var
  lPos: Integer;
  lOk: Boolean;
  lChar: Char;
  St: string;
begin
  lPos := 1;
  lOk := False;
  lChar := '>';
  St := '';

  while (APosition <= ALength) do
  begin
    if Parent <> nil then
      Parent.DoLoadProgress(APosition, ALength);

    case lPos of
      1..9: //<!DOCTYPE
        if AData[APosition] = CS_START_DOCTYPE[lPos] then
          Inc(lPos)
        else
          raise TJvSimpleXmlInvalid.Create('Invalid Header: Expected ' + CS_START_DOCTYPE[lPos] +
            ' Found ' + AData[APosition]);
      10: //]> or >
        if lChar = AData[APosition] then
        begin
          if lChar = '>' then
          begin
            lOk := True;
            Break; //End while
          end
          else
          begin
            St := St + AData[APosition];
            lChar := '>';
          end;
        end
        else
        begin
          St := St + AData[APosition];
          if AData[APosition] = '[' then
            lChar := ']';
        end;
    end;
    inc(APosition);
  end;

  if not lOk then
    raise TJvSimpleXmlInvalid.Create('Invalid Comment: Unexpected end of data');

  Name := '';
  Value := Trim(St);

  if Parent <> nil then
    Parent.DoValueParsed('', St);
end;

procedure TJvSimpleXmlElemDocType.LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml);
{
<!DOCTYPE test [
<!ELEMENT test (#PCDATA) >
<!ENTITY % xx '&#37;zz;'>
<!ENTITY % zz '&#60;!ENTITY tricky "error-prone" >' >
%xx;
]>

<!DOCTYPE greeting SYSTEM "hello.dtd">
}
const
  CS_START_DOCTYPE = '<!DOCTYPE';
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array [0..cBufferSize-1] of Char;
  lOk: Boolean;
  lChar: Char;
  St: string;
begin
  lStreamPos := Stream.Position;
  lPos := 1;
  lOk := False;
  lChar := '>';
  St := '';

  repeat
    Count := Stream.Read(lBuf, SizeOf(lBuf));
    if Parent <> nil then
      Parent.DoLoadProgress(Stream.Position, Stream.Size);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        1..9: //<!DOCTYPE
          if lBuf[I] = CS_START_DOCTYPE[lPos] then
            Inc(lPos)
          else
            raise TJvSimpleXmlInvalid.Create('Invalid Header: Expected ' + CS_START_DOCTYPE[lPos] +
              ' Found ' + lBuf[I]);
        10: //]> or >
          if lChar = lBuf[I] then
          begin
            if lChar = '>' then
            begin
              lOk := True;
              Count := 0;
              Break; //This is the end
            end
            else
            begin
              St := St + lBuf[I];
              lChar := '>';
            end;
          end
          else
          begin
            St := St + lBuf[I];
            if lBuf[I] = '[' then
              lChar := ']';
          end;
      end;
    end;
  until Count = 0;

  if not lOk then
    raise TJvSimpleXmlInvalid.Create('Invalid Comment: Unexpected end of data');

  Name := '';
  Value := Trim(St);

  if Parent <> nil then
    Parent.DoValueParsed('', St);

  Stream.Seek(lStreamPos, soFromBeginning);
end;

procedure TJvSimpleXmlElemDocType.SaveToStream(const Stream: TStream;
  const Level: string; Parent: TJvSimpleXml);
var
  St: string;
begin
  St := '<!DOCTYPE ' + Value + '>' + sLineBreak;
  Stream.Write(St[1], Length(St));
  if Parent <> nil then
    Parent.DoSaveProgress;
end;

//=== TJvSimpleXmlElemSheet ==================================================

procedure TJvSimpleXmlElemSheet.LoadFromArray(var AData: string;
  var APosition, ALength: Integer; Parent: TJvSimpleXml);
const
  CS_START_PI = '<?xml-stylesheet';
  CS_STOP_PI = '                ?>';
var
  lPos: Integer;
  lOk: Boolean;
begin
  lPos := 1;
  lOk := False;

  while (APosition <= ALength) do
  begin
    if Parent <> nil then
      Parent.DoLoadProgress(APosition, ALength);

    case lPos of
      1..15: //<?xml-stylesheet
        if AData[APosition] = CS_START_PI[lPos] then
          Inc(lPos)
        else
          raise TJvSimpleXmlInvalid.Create('Invalid Stylesheet: Expected ' + CS_START_PI[lPos] +
            ' Found ' + AData[APosition]);
      16: //L
        if AData[APosition] = CS_START_PI[lPos] then
        begin
          inc(APosition);
          Properties.LoadFromArray(AData, APosition, ALength);
          Inc(lPos);
        end
        else
          raise TJvSimpleXmlInvalid.Create('Invalid Stylesheet: Expected ' + CS_START_PI[lPos] +
            ' Found ' + AData[APosition]);
      17: //?
        if AData[APosition] = CS_STOP_PI[lPos] then
          Inc(lPos)
        else
          raise TJvSimpleXmlInvalid.Create('Invalid Stylesheet: Expected ' + CS_STOP_PI[lPos] +
            ' Found ' + AData[APosition]);
      18: //>
        if AData[APosition] = CS_STOP_PI[lPos] then
        begin
          lOk := True;
          Break; //End while
        end
        else
          raise TJvSimpleXmlInvalid.Create('Invalid Stylesheet: Expected ' + CS_STOP_PI[lPos] +
            ' Found ' + AData[APosition]);
    end;
    inc(APosition);
  end;

  if not lOk then
    raise TJvSimpleXmlInvalid.Create('Invalid Stylesheet: Unexpected end of data');

  Name := '';
end;

procedure TJvSimpleXmlElemSheet.LoadFromStream(const Stream: TStream;
  Parent: TJvSimpleXml);
//<?xml-stylesheet alternate="yes" type="text/xsl" href="sheet.xsl"?>
const
  CS_START_PI = '<?xml-stylesheet';
  CS_STOP_PI = '                ?>';
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array [0..cBufferSize-1] of Char;
  lOk: Boolean;
begin
  lStreamPos := Stream.Position;
  lPos := 1;
  lOk := False;

  repeat
    Count := Stream.Read(lBuf, SizeOf(lBuf));
    if Parent <> nil then
      Parent.DoLoadProgress(Stream.Position, Stream.Size);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        1..15: //<?xml-stylesheet
          if lBuf[I] = CS_START_PI[lPos] then
            Inc(lPos)
          else
            raise TJvSimpleXmlInvalid.Create('Invalid Stylesheet: Expected ' + CS_START_PI[lPos] +
              ' Found ' + lBuf[I]);
        16: //L
          if lBuf[I] = CS_START_PI[lPos] then
          begin
            Stream.Seek(lStreamPos, soFromBeginning);
            Properties.LoadFromStream(Stream);
            lStreamPos := Stream.Position;
            Inc(lPos);
            Break; //Re read buffer
          end
          else
            raise TJvSimpleXmlInvalid.Create('Invalid Stylesheet: Expected ' + CS_START_PI[lPos] +
              ' Found ' + lBuf[I]);
        17: //?
          if lBuf[I] = CS_STOP_PI[lPos] then
            Inc(lPos)
          else
            raise TJvSimpleXmlInvalid.Create('Invalid Stylesheet: Expected ' + CS_STOP_PI[lPos] +
              ' Found ' + lBuf[I]);
        18: //>
          if lBuf[I] = CS_STOP_PI[lPos] then
          begin
            Count := 0; //End repeat
            lOk := True;
            Break; //End if
          end
          else
            raise TJvSimpleXmlInvalid.Create('Invalid Stylesheet: Expected ' + CS_STOP_PI[lPos] +
              ' Found ' + lBuf[I]);
      end;
    end;
  until Count = 0;

  if not lOk then
    raise TJvSimpleXmlInvalid.Create('Invalid Stylesheet: Unexpected end of data');

  Name := '';

  Stream.Seek(lStreamPos, soFromBeginning);
end;

procedure TJvSimpleXmlElemSheet.SaveToStream(const Stream: TStream;
  const Level: string; Parent: TJvSimpleXml);
var
  I: integer;
  St: string;
begin
  St := Level + '<?xml-stylesheet';
  for I := 0 to Properties.GetCount - 1 do
    St := St + Properties.Item[I].SaveToString;
  St := St + '?>' + sLineBreak;
  Stream.Write(St[1], Length(St));
  if Parent <> nil then
    Parent.DoSaveProgress;
end;

//=== TJvSimpleXmlElemsProlog ================================================

procedure TJvSimpleXmlElemsProlog.Add(const Elem: TJvSimpleXmlElem);
begin
  FElems.AddObject(Elem.Name, Elem)
end;

constructor TJvSimpleXmlElemsProlog.Create;
begin
  inherited Create;
  //AddToLog('TJvSimpleXmlProlog.Create'); //XXX
  FElems := TStringListEx.Create;
  FElems.CaseSensitive := false;
end;

destructor TJvSimpleXmlElemsProlog.Destroy;
begin
  Clear;
  FreeAndNil(FElems);
  inherited Destroy;
end;

procedure TJvSimpleXmlElemsProlog.Clear;
begin
  while FElems.Count > 0 do
  begin
    TJvSimpleXmlElem(FElems.Objects[0]).Free;
    FElems.Delete(0);
  end;               
end;

function TJvSimpleXmlElemsProlog.GetCount: Integer;
begin
  Result := FElems.Count;
end;

function TJvSimpleXmlElemsProlog.GetItem(const Index: Integer): TJvSimpleXmlElem;
begin
  Result := TJvSimpleXmlElem(FElems.Objects[Index]);
end;

procedure TJvSimpleXmlElemsProlog.LoadFromArray(var AData: string;
  var APosition, ALength: Integer; AParent: TJvSimpleXml = nil);
var
  lPos: Integer;
  St: string;
  lEnd: Boolean;
  lElem: TJvSimpleXmlElem;
begin
  St := '';
  lPos := 0;

  while (APosition <= ALength) do
  begin
    if AParent <> nil then
      AParent.DoLoadProgress(APosition, ALength);

    case lPos of
      0: //We are waiting for a tag and thus avoiding spaces
        begin
          case AData[APosition] of
            ' ', #9, #13, #10:
              begin
              end;
            '<':
              begin
                lPos := 1;
                St := AData[APosition];
              end;
          else
            raise TJvSimpleXmlInvalid.Create('Invalid Document: Unexpected text in file prolog.');
          end;
        end;
      1: //We are trying to determine the kind of the tag
        begin
          lElem := nil;
          lEnd := False;

          St := St + AData[APosition];
          if St = '<![CDATA[' then
            lEnd := True
          else
          if St = '<!--' then
            lElem := TJvSimpleXmlElemComment.Create(nil)
          else
          if St = '<?xml-stylesheet' then
            lElem := TJvSimpleXmlElemSheet.Create(nil)
          else
          if St = '<?xml ' then
            lElem := TJvSimpleXmlElemHeader.Create
          else
          if St = '<!DOCTYPE' then
            lElem := TJvSimpleXmlElemDoctype.Create(nil)
          else
          if (Length(St) > 1) and not (St[2] in ['!', '?']) then
            lEnd := True;

          if lEnd then
          begin
            APosition := (APosition - Length(St)) + 1;
            Break;
          end
          else if lElem <> nil then
          begin
            APosition := (APosition - Length(st)) + 1;
            lElem.LoadFromArray(AData, APosition, ALength, AParent);
            FElems.AddObject(lElem.Name, lElem);
            St := '';
            lPos := 0;
          end;
        end;
    end;
    inc(APosition);
  end;
end;

procedure TJvSimpleXmlElemsProlog.LoadFromStream(
  const Stream: TStream; Parent: TJvSimpleXml);
{<?xml version="1.0" encoding="UTF-8" ?>
<!-- Test -->
<!DOCTYPE greeting [
  <!ELEMENT greeting (#PCDATA)>
]>
<greeting>Hello, world!</greeting>

<?xml version="1.0"?> <!DOCTYPE greeting SYSTEM "hello.dtd"> <greeting>Hello, world!</greeting>
}
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array [0..cBufferSize-1] of Char;
  St: string;
  lEnd: Boolean;
  lElem: TJvSimpleXmlElem;
begin
  lStreamPos := Stream.Position;
  St := '';
  lPos := 0;

  repeat
    Count := Stream.Read(lBuf, SizeOf(lBuf));
    if Parent <> nil then
      Parent.DoLoadProgress(Stream.Position, Stream.Size);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        0: //We are waiting for a tag and thus avoiding spaces
          begin
            case lBuf[I] of
              ' ', #9, #13, #10:
                begin
                end;
              '<':
                begin
                  lPos := 1;
                  St := lBuf[I];
                end;
            else
              raise TJvSimpleXmlInvalid.Create('Invalid Document: Unexpected text in file prolog.');
            end;
          end;
        1: //We are trying to determine the kind of the tag
          begin
            lElem := nil;
            lEnd := False;

            St := St + lBuf[I];
            if St = '<![CDATA[' then
              lEnd := True
            else
            if St = '<!--' then
              lElem := TJvSimpleXmlElemComment.Create(nil)
            else
            if St = '<?xml-stylesheet' then
              lElem := TJvSimpleXmlElemSheet.Create(nil)
            else
            if St = '<?xml ' then
              lElem := TJvSimpleXmlElemHeader.Create
            else
            if St = '<!DOCTYPE' then
              lElem := TJvSimpleXmlElemDoctype.Create(nil)
            else
            if (Length(St) > 1) and not (St[2] in ['!', '?']) then
              lEnd := True;

            if lEnd then
            begin
              lStreamPos := lStreamPos - Length(St);
              Count := 0;
              Break;
            end
            else
            if lElem <> nil then
            begin
              Stream.Seek(lStreamPos - (Length(St)), soFromBeginning);
              lElem.LoadFromStream(Stream);
              lStreamPos := Stream.Position;
              FElems.AddObject(lElem.Name, lElem);
              St := '';
              lPos := 0;
              Break;
            end;
          end;
      end;
    end;
  until Count = 0;

  Stream.Seek(lStreamPos, soFromBeginning);
end;

procedure TJvSimpleXmlElemsProlog.SaveToStream(const Stream: TStream; Parent: TJvSimpleXml);
var
  I: Integer;
begin
  if Count = 0 then
    FElems.AddObject('', TJvSimpleXmlElemHeader.Create);
  for I := 0 to Count - 1 do
    Item[I].SaveToStream(Stream, '', Parent);
end;

{$IFDEF COMPILER6_UP}

function VarXml: TVarType;
begin
  Result := XmlVariant.VarType;
end;

procedure XmlCreateInto(var ADest: Variant; const AXml: TJvSimpleXmlElem);
begin
  TXmlVarData(ADest).VType := VarXml;
  TXmlVarData(ADest).Xml := AXml;
end;

function XmlCreate(const AXml: TJvSimpleXmlElem): Variant;
begin
  XmlCreateInto(Result, AXml);
end;

function XmlCreate: Variant;
begin
  XmlCreateInto(Result, TJvSimpleXmlElemClassic.Create(nil));
end;

//=== TXmlVariant ============================================================

procedure TXmlVariant.CastTo(var Dest: TVarData; const Source: TVarData;
  const AVarType: TVarType);
begin
  if Source.VType = VarType then
  begin
    case AVarType of
      varOleStr:
        VarDataFromOleStr(Dest, TXmlVarData(Source).Xml.SaveToString);
      varString:
        VarDataFromStr(Dest, TXmlVarData(Source).Xml.SaveToString);
    else
      RaiseCastError;
    end;
  end
  else
    inherited;
end;

procedure TXmlVariant.Clear(var V: TVarData);
begin
  V.VType := varEmpty;
  TXmlVarData(V).Xml := nil;
end;

procedure TXmlVariant.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
    VarDataCopyNoInd(Dest, Source)
  else
    with TXmlVarData(Dest) do
    begin
      VType := VarType;
      Xml := TXmlVarData(Source).Xml;
    end;
end;

function TXmlVariant.DoFunction(var Dest: TVarData; const V: TVarData;
  const Name: string; const Arguments: TVarDataArray): Boolean;
var
  lXml: TJvSimpleXmlElem;
  I, J, K: Integer;
begin
  Result := False;
  if (Length(Arguments) = 1) and (Arguments[0].VType in [vtInteger, vtExtended]) then
    with TXmlVarData(V) do
    begin
      K := Arguments[0].vInteger;
      J := 0;

      if K > 0 then
        for I := 0 to Xml.Items.Count - 1 do
          if UpperCase(Xml.Items[I].Name) = Name then
          begin
            Inc(J);
            if J = K then
              Break;
          end;

      if (J = K) and (J < Xml.Items.Count) then
      begin
        lXml := Xml.Items[J];
        if lXml <> nil then
        begin
          Dest.VType := VarXml;
          TXmlVarData(Dest).Xml := lXml;
          Result := True;
        end
      end;
    end;
end;

function TXmlVariant.GetProperty(var Dest: TVarData; const V: TVarData;
  const Name: string): Boolean;
var
  lXml: TJvSimpleXmlElem;
  lProp: TJvSimpleXmlProp;
begin
  Result := False;
  with TXmlVarData(V) do
  begin
    lXml := Xml.Items.ItemNamed[Name];
    if lXml <> nil then
    begin
      Dest.VType := VarXml;
      TXmlVarData(Dest).Xml := lXml;
      Result := True;
    end
    else
    begin
      lProp := Xml.Properties.ItemNamed[Name];
      if lProp <> nil then
      begin
        VarDataFromOleStr(Dest, lProp.Value);
        Result := True;
      end;
    end;
  end;
end;

function TXmlVariant.IsClear(const V: TVarData): Boolean;
begin
  Result := (TXmlVarData(V).Xml = nil) or
    (TXmlVarData(V).Xml.Items.Count = 0);
end;

function TXmlVariant.SetProperty(const V: TVarData; const Name: string;
  const Value: TVarData): Boolean;
var
  lXml: TJvSimpleXmlElem;
  lProp: TJvSimpleXmlProp;

  function GetStrValue: string;
  begin
    try
      Result := Value.VOleStr;
    except
      Result := '';
    end;
  end;

begin
  Result := False;
  with TXmlVarData(V) do
  begin
    lXml := Xml.Items.ItemNamed[Name];
    if lXml = nil then
    begin
      lProp := Xml.Properties.ItemNamed[Name];
      if lProp <> nil then
      begin
        lProp.Value := GetStrValue;
        Result := True;
      end;
    end
    else
    begin
      lXml.Value := GetStrValue;
      Result := True;
    end;
  end;
end;
{$ENDIF}



initialization
{$IFDEF COMPILER6_UP}
  XmlVariant := TXmlVariant.Create;
{$ENDIF}
  GSorts := TList.Create;
finalization
{$IFDEF COMPILER6_UP}
  FreeAndNil(XmlVariant);
{$ENDIF}
  FreeAndNil(GSorts);
end.

