(************************************************************************
 *                                                                      *
 *   (C) 2002-2014 Antoine Potten, Mickaël Vanneufville                 *
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

unit functions_str;

interface

uses
  Types, SysUtils, Graphics, Windows, Math,
  {$IFDEF ANTUNICODE}
  TntClasses,
  {$ENDIF}
  Classes;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure Split(const Source, Sep: string; var Left, Right: string; const ToLeftIfNotFound: Boolean = False); overload;
procedure Split(const Source, Sep: WideString; var Left, Right: WideString; const ToLeftIfNotFound: Boolean = False); overload;
procedure Split(Source, Sep: string; Target: TStrings); overload;
function TextAfter(const Source: string; const Sep: string): string;
function TextBefore(const Source: string; const Sep: string; ReturnAllIfNotFound: Boolean = False): string;
function LastPos(const SubStr: string; S: string): Integer;
function StrToIntTrunc(Source: string; Def: Integer = 0): Integer;
function StrToFloatTrunc(Source: string; Def: Double = 0): Double;
function CharReplace(const Input: string; OldChar, NewChar: Char): string;
function WideCharReplace(const Input: WideString; OldChar, NewChar: WideChar): WideString;
function InsertLineBreaks(const Source: string): string;
function ReplaceLineBreaks(const Source: string): string;
function Encrypt(Value: string): string;
function Decrypt(Value: string): string;
function IsReallyEmpty(const Text: string): Boolean;
function IndexStr(const AText: string; const AValues: array of string): Integer;
function IndexText(const AText: string; const AValues: array of string): Integer;
function StartsStr(const AText, AStr: string): Boolean; overload;
function StartsStr(const AText, AStr: WideString): Boolean; overload;
function StartsText(const AText, AStr: string): Boolean; overload;
function StartsText(const AText, AStr: WideString): Boolean; overload;
function EndsStr(const AText, AStr: string): Boolean; overload;
function EndsStr(const AText, AStr: WideString): Boolean; overload;
function EndsText(const AText, AStr: string): Boolean; overload;
function EndsText(const AText, AStr: WideString): Boolean; overload;
procedure ArrayToStrings(const Source: array of string; Dest: TStrings);
function CheckChars(const Source: string; const Chars: TSysCharSet): Boolean;
function RemoveLettersNotInList(const Source: string; const List: TSysCharSet): string;
function EncodeFromStream(const AStream: TStream): string;
procedure DecodeToStream(const AStream: TStream; const AString: string);
procedure WriteIntegerToStream(const I: Integer; AStream: TStream);
procedure WriteStringToStream(const S: string; AStream: TStream);
procedure WriteStringToTextStream(const S: string; AStream: TStream);
function ReadIntegerFromStream(AStream: TStream): Integer;
function ReadStringFromStream(AStream: TStream): string;
{$IFDEF ANTUNICODE}
procedure UTF8ListEncode(const ASource: TTntStrings; const ADest: TStrings; const MaxItems: Integer = -1);
procedure UTF8ListDecode(const ASource: TStrings; const ADest: TTntStrings; const MaxItems: Integer = -1);
{$ENDIF}
function LoCase(ch: Char): Char;
function RectToStr(const ARect: TRect): string;
function StrToRect(AStr: string): TRect;
function DelAfterChar(const AStr: string; const AChar: Char): string;
function DelBeforeChar(const AStr: string; const AChar: Char): string;
function CopyExceptLastChars(const AStr: string; const LastCharsToIgnore: Integer): string;
function CharsetToCodePage(const ACharset: TFontCharset): Cardinal;
function CodePageToCharset(const ACodePage: Cardinal): TFontCharset;
function EncodingToCharset(Encoding: string): TFontCharset;
function CharsetToEncoding(Charset: TFontCharset): string;
function CharsetToWideString(const AStr: string; const ACharset: TFontCharset): WideString;
function GetDefaultAnsiCodePage: Cardinal;
function StrToChar(const AStr: string): Char;
function QuotedStrEx(const S: string; const QuoteChar: Char): string;

{Function called in initialization}
function InitDefaultCPInfo : Boolean;

function IsDefaultCPLeadByte(b: Byte): Boolean;

function AnsiCompareEx(const S1, S2: string; IgnoreCase: Boolean = False;
  IgnoreAccents: Boolean = False): Integer;
function AnsiCompareSubEx(const S1, S2: string; IgnoreCase: Boolean = False;
  IgnoreAccents: Boolean = False; Pos1: Integer = 1; Pos2: Integer = 1;
  Length1: Integer = MaxInt; Length2: Integer = MaxInt): Integer;
function AnsiCompareTextEx(const S1, S2: string; IgnoreAccents: Boolean = False): Integer;
function AnsiCompareStrEx(const S1, S2: string; IgnoreAccents: Boolean = False): Integer;
function AnsiSameTextEx(const S1, S2: string; IgnoreAccents: Boolean = False): Boolean;
function AnsiSameStrEx(const S1, S2: string; IgnoreAccents: Boolean = False): Boolean;

function AnsiNatCompare(const S1, S2: string; IgnoreCase: Boolean = False;
  IgnoreAccents: Boolean = False; TwoPasses: Boolean = True): Integer;
function AnsiNatCompareText(const S1, S2: string; IgnoreAccents: Boolean = False): Integer;
function AnsiNatCompareStr(const S1, S2: string; IgnoreAccents: Boolean = False): Integer;
function AnsiNatSameText(const S1, S2: string; IgnoreAccents: Boolean = False): Boolean;
function AnsiNatSameStr(const S1, S2: string; IgnoreAccents: Boolean = False): Boolean;

function AnsiPosEx(const SubStr, S: string; IgnoreCase: Boolean = False;
  IgnoreAccents: Boolean = False; StartPos: Integer = 1): Integer;
function AnsiLastPosEx(const SubStr, S: string; IgnoreCase: Boolean = False;
  IgnoreAccents: Boolean = False; StartPos: Integer = MaxInt): Integer;
function AnsiContainsEx(const S, SubStr: string; IgnoreCase: Boolean = False;
  IgnoreAccents: Boolean = False): Boolean;
function AnsiContainsTextEx(const S, SubStr: string; IgnoreAccents: Boolean = False): Boolean;
function AnsiContainsStrEx(const S, SubStr: string; IgnoreAccents: Boolean = False): Boolean;

function AnsiBestFitUS(const AInput: string): string;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

implementation

uses
  IdCoderMIME, StrUtils;

type
  TCmpChar = array[0..255, 0..255] of Byte;
  PCmpChar = ^TCmpChar;

var
  DefaultCodePage : Cardinal = 1252;
  { Is Multi-Byte Character Sets for Default Code Page }
  IsDefaultCPMBCS : Boolean = False;
  { Is Multi-Byte Character Sets and Double-Byte Character Sets for Default Code Page }
  IsDefaultCPDBCS: Boolean = False;
  { Lead Byte for Double-Byte Character Sets }
  DefaultCPLeadByte: array[0..MAX_LEADBYTES-1] of Byte = (0,0,0,0,0,0,0,0,0,0,0,0);

  CmpCharSensitive: TCmpChar;
  CmpCharNoCaseSensitive: TCmpChar;
  CmpCharNoAccentSensitive: TCmpChar;
  CmpCharNoCaseAccentSensitive: TCmpChar;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure Split(const Source, Sep: string; var Left, Right: string; const ToLeftIfNotFound: Boolean = False);
var
  SepPos: Integer;
begin
  SepPos := Pos(Sep, Source);
  if (SepPos = 0) and (ToLeftIfNotFound) then
  begin
    Left := Source;
    Right := '';
  end
  else
  begin
    Left := Copy(Source, 1, SepPos - 1);
    Right := Copy(Source, SepPos + Length(Sep), MaxInt);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure Split(const Source, Sep: WideString; var Left, Right: WideString; const ToLeftIfNotFound: Boolean = False); overload;
var
  SepPos: Integer;
begin
  SepPos := Pos(Sep, Source);
  if (SepPos = 0) and (ToLeftIfNotFound) then
  begin
    Left := Source;
    Right := '';
  end
  else
  begin
    Left := Copy(Source, 1, SepPos - 1);
    Right := Copy(Source, SepPos + Length(Sep), MaxInt);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure Split(Source, Sep: string; Target: TStrings);
var
  SepPos: Integer;
begin
  Target.BeginUpdate;
  try
    Target.Clear;
    repeat
      SepPos := Pos(Sep, Source);
      if SepPos = 0 then
        Target.Add(Copy(Source, 1, MaxInt))
      else
      begin
        Target.Add(Copy(Source, 1, SepPos - 1));
        Delete(Source, 1, SepPos);
      end;
    until SepPos = 0;
  finally
    Target.EndUpdate;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TextAfter(const Source: string; const Sep: string): string;
var
  Bidon: string;
begin
  Split(Source, Sep, Bidon, Result, True);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TextBefore(const Source: string; const Sep: string; ReturnAllIfNotFound: Boolean = False): string;
var
  FoundPos: Integer;
begin
  Result := '';
  FoundPos := LastPos(Sep, Source);
  if FoundPos = 0 then
  begin
    if ReturnAllIfNotFound then
      Result := Source;
    Exit;
  end;
  Result := Copy(Source, 1, FoundPos - 1);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function LastPos(const SubStr: string; S: string): Integer;
var
  CurPos, PrevPos: Integer;
begin
  PrevPos := 0;
  CurPos := Pos(SubStr, S);
  while CurPos > 0 do
  begin
    if PrevPos = 0 then
      PrevPos := CurPos
    else
      PrevPos := PrevPos + CurPos + Length(SubStr) - 1;
    Delete(S, 1, CurPos + Length(SubStr) - 1);
    CurPos := Pos(SubStr, S);
  end;
  Result := PrevPos;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function StrToIntTrunc(Source: string; Def: Integer = 0): Integer;
var
  p, L: Integer;
begin
  L := Length(Source);
  p := 1;
  while (p <= L) and (Source[p] in ['0'..'9']) do
  begin
    Inc(p);
  end;
  SetLength(Source, p - 1);
  Result := StrToIntDef(Source, Def);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function StrToFloatTrunc(Source: string; Def: Double = 0): Double;
var
  p, L: Integer;
begin
  Source := CharReplace(CharReplace(Source, '.', DecimalSeparator), ',', DecimalSeparator);
  L := Length(Source);
  p := 1;
  while (p <= L) and (Source[p] in ['0'..'9', DecimalSeparator]) do
  begin
    Inc(p);
  end;
  SetLength(Source, p - 1);
  Result := StrToFloatDef(Source, Def);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function CharReplace(const Input: string; OldChar, NewChar: Char): string;
var
  i: Integer;
begin
  Result := Input;
  for i := 1 to Length(Input) do
  begin
    if result[i] = OldChar then
      result[i] := NewChar;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function WideCharReplace(const Input: WideString; OldChar, NewChar: WideChar): WideString;
var
  i: Integer;
begin
  Result := Input;
  for i := 1 to Length(Input) do
  begin
    if result[i] = OldChar then
      result[i] := NewChar;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function InsertLineBreaks(const Source: string): string;
begin
  Result := StringReplace(Source, '|', sLineBreak, [rfReplaceAll]);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function ReplaceLineBreaks(const Source: string): string;
begin
  Result := StringReplace(Source, sLineBreak, '|', [rfReplaceAll]);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function Encrypt(Value: string): string;
var
 i: Integer;
 b: byte;
begin
  result := '';
  for i:=1 to Length(Value) do
    Value[i] := Char(Byte(Value[i]) xor i);
  for i:=1 to Length(Value) do
  begin
    b := Byte(Value[i]) and $0F;
    b := Ord('A')+b;
    result := result+char(b);
    b := (Byte(Value[i]) and $F0) shr 4;
    b := Ord('a')+b;
    result := result+char(b);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function Decrypt(Value: string): string;
var
 i,j: Integer;
 b,c: byte;
begin
  result := '';
  i := 1;
  j := 1;
  while i<Length(Value) do
  begin
    b := Byte(Value[i])-Ord('A');
    c := (Byte(Value[i+1])-Ord('a')) shl 4;
    b := b+c;
    Value[j] := char(b);
    inc(i,2);
    inc(j,1);
  end;
  SetLength(Value,Length(Value) div 2);
  for i:=1 to Length(Value) do
    result := result+Char(Byte(Value[i]) xor i);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function IsReallyEmpty(const Text: string): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 1 to Length(Text) do
    if not (Text[i] in [#13, #10, ' ']) then
    begin
      Result := False;
      Break;
    end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function IndexStr(const AText: string; const AValues: array of string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := Low(AValues) to High(AValues) do
    if AText = AValues[i] then
    begin
      Result := i;
      Break;
    end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function IndexText(const AText: string; const AValues: array of string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := Low(AValues) to High(AValues) do
    if SameText(AText, AValues[i]) then
    begin
      Result := i;
      Break;
    end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function StartsStr(const AText, AStr: string): Boolean;
begin
  Result := Copy(AStr, 1, Length(AText)) = AText;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function StartsStr(const AText, AStr: WideString): Boolean;
begin
  Result := Copy(AStr, 1, Length(AText)) = AText;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function StartsText(const AText, AStr: string): Boolean;
begin
  Result := StartsStr(AnsiUpperCase(AText), AnsiUpperCase(AStr));
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function StartsText(const AText, AStr: WideString): Boolean;
begin
  Result := StartsStr(WideUpperCase(AText), WideUpperCase(AStr));
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function EndsStr(const AText, AStr: string): Boolean; overload;
begin
  Result := Copy(AStr, Length(AStr) - Length(AText) + 1, Length(AText)) = AText;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function EndsStr(const AText, AStr: WideString): Boolean; overload;
begin
  Result := Copy(AStr, Length(AStr) - Length(AText) + 1, Length(AText)) = AText;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function EndsText(const AText, AStr: string): Boolean; overload;
begin
  Result :=  EndsStr(AnsiUpperCase(AText), AnsiUpperCase(AStr));
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function EndsText(const AText, AStr: WideString): Boolean; overload;
begin
  Result :=  EndsStr(WideUpperCase(AText), WideUpperCase(AStr));
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure ArrayToStrings(const Source: array of string; Dest: TStrings);
var
  i: Integer;
begin
  Dest.BeginUpdate;
  try
    Dest.Clear;
    for i := Low(Source) to High(Source) do
    begin
      Dest.Add(Source[i]);
    end;
  finally
    Dest.EndUpdate;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function CheckChars(const Source: string; const Chars: TSysCharSet): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 1 to Length(Source) do
    if not (Source[i] in Chars) then
    begin
      Result := False;
      Break;
    end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function RemoveLettersNotInList(const Source: string; const List: TSysCharSet): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(Source) do
    if Source[i] in List then
      Result := Result + Source[i];
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function EncodeFromStream(const AStream: TStream): string;
begin
  with TIdEncoderMIME.Create(nil) do
    try
      AStream.Seek(0, soFromBeginning);
      Result := Encode(AStream);
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure DecodeToStream(const AStream: TStream; const AString: string);
begin
  with TIdDecoderMIME.Create(nil) do
    try
      AStream.Seek(0, soFromBeginning);
      DecodeToStream(AStream, AString);
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

{$IFDEF ANTUNICODE}
procedure UTF8ListEncode(const ASource: TTntStrings; const ADest: TStrings; const MaxItems: Integer = -1);
var
  i: Integer;
begin
  ADest.BeginUpdate;
  try
    ADest.Clear;
    for i := 0 to ASource.Count-1 do
    begin
      if (MaxItems > -1) and (i = MaxItems) then
        Break;
      ADest.Add(UTF8Encode(ASource.Strings[i]));
    end;
  finally
    ADest.EndUpdate;
  end;
end;
{$ENDIF}

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

{$IFDEF ANTUNICODE}
procedure UTF8ListDecode(const ASource: TStrings; const ADest: TTntStrings; const MaxItems: Integer = -1);
var
  i: Integer;
begin
  ADest.BeginUpdate;
  try
    ADest.Clear;
    for i := 0 to ASource.Count-1 do
    begin
      if (MaxItems > -1) and (i = MaxItems) then
        Break;
      ADest.Add(UTF8Decode(ASource.Strings[i]));
    end;
  finally
    ADest.EndUpdate;
  end;
end;
{$ENDIF}

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function LoCase(ch: Char): Char;
{ ->    AL      Character       }
{ <-    AL      Result          }
asm
        CMP     AL,'A'
        JB      @@exit
        CMP     AL,'Z'
        JA      @@exit
        ADD     AL,'a' - 'A'
@@exit:
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function RectToStr(const ARect: TRect): string;
begin
  Result := Format('%d,%d,%d,%d', [ARect.Left, ARect.Top, ARect.Right, ARect.Bottom]);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function StrToRect(AStr: string): TRect;
  function ExtractNumber(var Source: string): string;
  var
    p: Integer;
  begin
    p := Pos(',', Source);
    if p = 0 then
      p := MaxInt;
    Result := Copy(Source, 1, p-1);
    Delete(Source, 1, p);
  end;
begin
  if AStr = '' then
    raise Exception.Create('Empty string');
  Result.Left := StrToInt(ExtractNumber(AStr));
  Result.Top := StrToInt(ExtractNumber(AStr));
  Result.Right := StrToInt(ExtractNumber(AStr));
  Result.Bottom := StrToInt(ExtractNumber(AStr));
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function DelAfterChar(const AStr: string; const AChar: Char): string;
var
  p: Integer;
begin
  p := Pos(AChar, AStr);
  if p = 0 then
    Result := AStr
  else
    Result := Copy(AStr, 1, p - 1);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function DelBeforeChar(const AStr: string; const AChar: Char): string;
var
  p: Integer;
begin
  p := Pos(AChar, AStr);
  if p = 0 then
    Result := AStr
  else
    Result := Copy(AStr, p + 1, MaxInt); 
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function CopyExceptLastChars(const AStr: string; const LastCharsToIgnore: Integer): string;
begin
  Result := Copy(AStr, 1, Length(AStr) - LastCharsToIgnore);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function CharsetToCodePage(const ACharset: TFontCharset): Cardinal;
begin
  case ACharset of
    ANSI_CHARSET        : Result := 1252;
    DEFAULT_CHARSET     : Result := CP_ACP;
    SYMBOL_CHARSET      : Result := CP_ACP;
    MAC_CHARSET         : Result := CP_MACCP;
    SHIFTJIS_CHARSET    : Result := 932;
    HANGEUL_CHARSET     : Result := 949;
    JOHAB_CHARSET       : Result := 1361;
    GB2312_CHARSET      : Result := 936;
    CHINESEBIG5_CHARSET : Result := 950;
    GREEK_CHARSET       : Result := 1253;
    TURKISH_CHARSET     : Result := 1254;
    VIETNAMESE_CHARSET  : Result := 1258;
    HEBREW_CHARSET      : Result := 1255;
    ARABIC_CHARSET      : Result := 1256;
    BALTIC_CHARSET      : Result := 1257;
    RUSSIAN_CHARSET     : Result := 1251;
    THAI_CHARSET        : Result := 874;
    EASTEUROPE_CHARSET  : Result := 1250;
    OEM_CHARSET         : Result := CP_OEMCP;
  else
    Result := CP_ACP;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function CodePageToCharset(const ACodePage: Cardinal): TFontCharset;
begin
  case ACodePage of
    1252                : Result := ANSI_CHARSET;
    CP_ACP              : Result := DEFAULT_CHARSET;
    CP_MACCP            : Result := MAC_CHARSET;
    932                 : Result := SHIFTJIS_CHARSET;
    949                 : Result := HANGEUL_CHARSET;
    1361                : Result := JOHAB_CHARSET;
    936                 : Result := GB2312_CHARSET;
    950                 : Result := CHINESEBIG5_CHARSET;
    1253                : Result := GREEK_CHARSET;
    1254                : Result := TURKISH_CHARSET;
    1258                : Result := VIETNAMESE_CHARSET;
    1255                : Result := HEBREW_CHARSET;
    1256                : Result := ARABIC_CHARSET;
    1257                : Result := BALTIC_CHARSET;
    1251                : Result := RUSSIAN_CHARSET;
    874                 : Result := THAI_CHARSET;
    1250                : Result := EASTEUROPE_CHARSET;
    CP_OEMCP            : Result := OEM_CHARSET;
  else
    Result := DEFAULT_CHARSET;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function EncodingToCharset(Encoding: string): TFontCharset;
begin
  Encoding := LowerCase(Encoding);
  if (Encoding = 'windows-874') or (Encoding = 'iso-8859-11') then
    Result := THAI_CHARSET
  else
  if (Encoding = 'windows-1250') or (Encoding = 'iso-8859-2') then
    Result := EASTEUROPE_CHARSET
  else
  if (Encoding = 'windows-1251') then
    Result := RUSSIAN_CHARSET
  else
  if (Encoding = 'windows-1252') or (Encoding = 'iso-8859-1') then
    Result := ANSI_CHARSET
  else
  if (Encoding = 'windows-1253') or (Encoding = 'iso-8859-7') then
    Result := GREEK_CHARSET
  else
  if (Encoding = 'windows-1254') or (Encoding = 'iso-8859-9') then
    Result := TURKISH_CHARSET
  else
  if (Encoding = 'windows-1255') or (Encoding = 'iso-8859-8') then
    Result := HEBREW_CHARSET
  else
  if (Encoding = 'windows-1256') or (Encoding = 'iso-8859-9') then
    Result := ARABIC_CHARSET
  else
  if (Encoding = 'windows-1257') or (Encoding = 'iso-8859-13') then
    Result := BALTIC_CHARSET
  else
  if (Encoding = 'windows-1258') then
    Result := VIETNAMESE_CHARSET
  else
  if (Encoding = 'gb2312') or (Encoding = 'euc-cn') then
    Result := GB2312_CHARSET
  else
  if (Encoding = 'big5') then
    Result := CHINESEBIG5_CHARSET
  else
  if (Encoding = 'euc-kr') then
    Result := HANGEUL_CHARSET
  else
  if (Encoding = 'shift_jis') then
    Result := SHIFTJIS_CHARSET
  else
    Result := DEFAULT_CHARSET
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function CharsetToEncoding(Charset: TFontCharset): string;
begin
  case Charset of
    THAI_CHARSET        : Result := 'windows-874';
    EASTEUROPE_CHARSET  : Result := 'windows-1250';
    RUSSIAN_CHARSET     : Result := 'windows-1251';
    ANSI_CHARSET        : Result := 'windows-1252';
    GREEK_CHARSET       : Result := 'windows-1253';
    TURKISH_CHARSET     : Result := 'windows-1254';
    HEBREW_CHARSET      : Result := 'windows-1255';
    ARABIC_CHARSET      : Result := 'windows-1256';
    BALTIC_CHARSET      : Result := 'windows-1257';
    VIETNAMESE_CHARSET  : Result := 'windows-1258';
    GB2312_CHARSET      : Result := 'gb2312';
    CHINESEBIG5_CHARSET : Result := 'big5';
    HANGEUL_CHARSET     : Result := 'euc-kr';
    SHIFTJIS_CHARSET    : Result := 'shift_jis';
  else
    Result := 'iso-8859-1';
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function CharsetToWideString(const AStr: string; const ACharset: TFontCharset): WideString;
var
  w: PWideChar;
  Size: Integer;
begin
  Size := (Length(AStr) + 1) * 2;
  GetMem(w, Size);
  MultiByteToWideChar(CharsetToCodePage(GB2312_CHARSET), MB_USEGLYPHCHARS, PChar(AStr), -1, w, Size);
  Result := w;
  FreeMem(w);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function GetDefaultAnsiCodePage: Cardinal;
var
  BufSize: Integer;
  s: string;
begin
  BufSize := GetLocaleInfo(GetUserDefaultLCID, LOCALE_IDEFAULTANSICODEPAGE, nil, 0);
  SetLength(s, BufSize);
  GetLocaleInfo(GetUserDefaultLCID, LOCALE_IDEFAULTANSICODEPAGE, PChar(s), BufSize);
  SetLength(s, BufSize - 1);
  Result := StrToIntDef(s, 0);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function StrToChar(const AStr: string): Char;
begin
  if AStr = '' then
    Result := #0
  else
  if AStr = '[tab]' then
    Result := #9
  else
  Result := AStr[1];
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function QuotedStrEx(const S: string; const QuoteChar: Char): string;
var
  I: Integer;
begin
  Result := S;
  if QuoteChar = #0 then
    Exit;
  for I := Length(Result) downto 1 do
    if Result[I] = QuoteChar then Insert(QuoteChar, Result, I);
  Result := QuoteChar + Result + QuoteChar;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure WriteIntegerToStream(const I: Integer; AStream: TStream);
begin
  AStream.WriteBuffer(I, SizeOf(Integer));
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure WriteStringToStream(const S: string; AStream: TStream);
begin
  WriteIntegerToStream(Length(S), AStream);
  if Length(S) > 0 then
    AStream.WriteBuffer(S[1], Length(S));
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure WriteStringToTextStream(const S: string; AStream: TStream);
begin
  AStream.WriteBuffer(S[1], Length(S));
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function ReadIntegerFromStream(AStream: TStream): Integer;
begin
  AStream.ReadBuffer(Result, SizeOf(Integer));
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function ReadStringFromStream(AStream: TStream): string;
var
  L: Integer;
begin
  L := ReadIntegerFromStream(AStream);
  if L > 0 then
  begin
    SetLength(Result, L);
    AStream.ReadBuffer(Result[1], L);
  end
  else
    Result := '';
end;

{-------------------------------------------------------------------------------
  Function called in initialization
-------------------------------------------------------------------------------}

function InitDefaultCPInfo : Boolean;
var
  CodePage: Cardinal;
  CPInfo : TCPInfo;
  i, j : Byte;
begin
  CodePage := GetDefaultAnsiCodePage;
  Result := GetCPInfo(CodePage, CPInfo);
  if not Result then
    Exit;
  DefaultCodePage := CodePage;
  IsDefaultCPMBCS := (CPInfo.MaxCharSize > 1);
  IsDefaultCPDBCS := IsDefaultCPMBCS and (CPInfo.LeadByte[0] > 0);
  Move(CPInfo.LeadByte[0], DefaultCPLeadByte[0],
    SizeOf(DefaultCPLeadByte[0]) * Length(DefaultCPLeadByte));

  for i := 0 to 255 do
  begin
    for j := 0 to 255 do
    begin
      if not IsDefaultCPMBCS then
      begin
        CmpCharSensitive[i][j] := CompareString(LOCALE_USER_DEFAULT,
          0, PChar(@i), 1, PChar(@j), 1) - 2;
        CmpCharNoCaseSensitive[i][j] := CompareString(LOCALE_USER_DEFAULT,
          NORM_IGNORECASE, PChar(@i), 1, PChar(@j), 1) - 2;
        CmpCharNoAccentSensitive[i][j] := CompareString(LOCALE_USER_DEFAULT,
          NORM_IGNORENONSPACE, PChar(@i), 1, PChar(@j), 1) - 2;
        CmpCharNoCaseAccentSensitive[i][j] := CompareString(LOCALE_USER_DEFAULT,
          NORM_IGNORECASE or NORM_IGNORENONSPACE, PChar(@i), 1, PChar(@j), 1) - 2;
      end else
      begin
        if IsDefaultCPDBCS and not IsDefaultCPLeadByte(i) and not IsDefaultCPLeadByte(j) then
        begin
          CmpCharSensitive[i][j] := CompareString(LOCALE_USER_DEFAULT,
            0, PChar(@i), 1, PChar(@j), 1) - 2;
          CmpCharNoCaseSensitive[i][j] := CompareString(LOCALE_USER_DEFAULT,
            NORM_IGNORECASE, PChar(@i), 1, PChar(@j), 1) - 2;
          CmpCharNoAccentSensitive[i][j] := CmpCharSensitive[i][j];
          CmpCharNoCaseAccentSensitive[i][j] := CmpCharNoCaseSensitive[i][j];
        end else
        begin
          CmpCharSensitive[i][j] := CompareValue(i, j);
          CmpCharNoCaseSensitive[i][j] := CmpCharSensitive[i][j];
          CmpCharNoAccentSensitive[i][j] := CmpCharSensitive[i][j];
          CmpCharNoCaseAccentSensitive[i][j] := CmpCharSensitive[i][j];
        end;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function IsDefaultCPLeadByte(b: Byte): Boolean;
var
  i: Integer;
begin
  Result := False;
  if not IsDefaultCPDBCS then
    exit;
  i := 0;
  while (DefaultCPLeadByte[i] <> 0) and (DefaultCPLeadByte[i+1] <> 0) do
  begin
    if (b >= DefaultCPLeadByte[i]) and (b <= DefaultCPLeadByte[i+1]) then
    begin
      Result := True;
      break;
    end;
    i := i + 2;
  end;
end;

{-------------------------------------------------------------------------------
  AnsiCompareEx:
  - IgnoreCase = False
    The strings are compared with regard to case
  - IgnoreCase = True
    The strings are compared without regard to case
  - IgnoreAccents = False:
    The strings are compared without regard to accents on first pass
    and if the strings are equal,
    a second pass over the strings is performed to compare accents
  - IgnoreAccents = True: The strings are compared without regard to accents
    (e.g. c < e = é < d)
  - With MBCS (including DBCS), IgnoreAccents option is ignored
-------------------------------------------------------------------------------}
function AnsiCompareEx(const S1, S2: string;
  IgnoreCase: Boolean = False; IgnoreAccents: Boolean = False): Integer;
var
  flag: Integer;
begin
  flag := 0;
  if IgnoreCase then
    flag := flag or NORM_IGNORECASE;
  if IgnoreAccents and not IsDefaultCPMBCS then
    flag := flag or NORM_IGNORENONSPACE;
  Result := CompareString(LOCALE_USER_DEFAULT, flag,
    PChar(S1), Length(S1), PChar(S2), Length(S2)) - 2;
end;

{-------------------------------------------------------------------------------
  AnsiCompareSubEx:
  - IgnoreCase = False
    The strings are compared with regard to case
  - IgnoreCase = True
    The strings are compared without regard to case
  - IgnoreAccents = False
    The strings are compared without regard to accents on first pass
    and if the strings are equal,
    a second pass over the strings is performed to compare accents
  - IgnoreAccents = True: The strings are compared without regard to accents
    (e.g. c < e = é < d)
  - Pos1 = Position in the string S1 where comparison start (1 = firt position)
  - Pos2 = Position in the string S2 where comparison start (1 = firt position)
  - Length1 = Length of string S1 from Pos1 to compare
  - Length2 = Length of string S2 from Pos2 to compare
  - With MBCS (including DBCS), IgnoreAccents option is ignored
-------------------------------------------------------------------------------}
function AnsiCompareSubEx(const S1, S2: string;
  IgnoreCase: Boolean = False; IgnoreAccents: Boolean = False;
  Pos1: Integer = 1; Pos2: Integer = 1;
  Length1: Integer = MaxInt; Length2: Integer = MaxInt): Integer;
var
  flag, len1, len2: Integer;
begin
  flag := 0;
  len1 := Length(S1);
  len2 := Length(S2);
  if IgnoreCase then
    flag := flag or NORM_IGNORECASE;
  if IgnoreAccents and not IsDefaultCPMBCS then
    flag := flag or NORM_IGNORENONSPACE;
  Dec(Pos1);
  Dec(Pos2);
  if Pos1 < 0 then Pos1 := 0
  else if Pos1 > len1 then Pos1 := len1;
  if Pos2 < 0 then Pos2 := 0
  else if Pos2 > len2 then Pos2 := len2;
  if Length1 < 0 then Length1 := 0
  else if Length1 > len1 - Pos1 then Length1 := len1 - Pos1;
  if Length2 < 0 then Length2 := 0
  else if Length2 > len2 - Pos2 + 1 then Length2 := len2 - Pos2;
  Result := CompareString(LOCALE_USER_DEFAULT, flag,
    PChar(S1) + Pos1, Length1, PChar(S2) + Pos2, Length2) - 2;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function AnsiCompareTextEx(const S1, S2: string; IgnoreAccents: Boolean = False): Integer;
begin
  Result := AnsiCompareEx(S1, S2, True, IgnoreAccents);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function AnsiCompareStrEx(const S1, S2: string; IgnoreAccents: Boolean = False): Integer;
begin
  Result := AnsiCompareEx(S1, S2, False, IgnoreAccents);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function AnsiSameTextEx(const S1, S2: string; IgnoreAccents: Boolean = False): Boolean;
begin
  Result := AnsiCompareTextEx(S1, S2, IgnoreAccents) = 0;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function AnsiSameStrEx(const S1, S2: string; IgnoreAccents: Boolean = False): Boolean;
begin
  Result := AnsiCompareStrEx(S1, S2, IgnoreAccents) = 0;
end;

{-------------------------------------------------------------------------------
  NatCompare:
  - IgnoreCase = False
    The strings are compared with regard to case
  - IgnoreCase = True
    The strings are compared without regard to case
  - IgnoreAccents = False:
    - TwoPasses = True: The strings are compared without regard to accents
      on first pass and if the strings are equal, a second pass over the
      strings is performed to compare accents
    - TwoPasses = False: The strings are compared with accents on first pass
      (e.g. c < e < é < d)
  - IgnoreAccents = True: The strings are compared without regard to accents
    (e.g. c < e = é < d)
  - With MBCS (including DBCS), IgnoreAccents option is ignored
    but the natural comparison should works properly
-------------------------------------------------------------------------------}

function AnsiNatCompare(const S1, S2: string; IgnoreCase: Boolean = False;
  IgnoreAccents: Boolean = False; TwoPasses: Boolean = True): Integer;
var
  i1, i2, j1, j2: Integer;
  len1, len2: Integer;

  function ExtractNum(var i: Integer; const txt: string; len: Integer): Int64;
  var
    n: Integer;
  begin
    n := i;
    while (i <= len) and (txt[i] in ['0'..'9']) do
      i := i + 1;
    Result := StrToInt64Def(Copy(txt, n, (i-n)), 0);
  end;
begin
  Result := 0;
  len1 := Length(S1);
  len2 := Length(S2);
  i1 := 1;
  i2 := 1;
  if (len1 > 0) and (len2 > 0) then
  begin
    repeat
      {With DBCS and MBCS, extract of numbers works too because :
        - number chars are always between x30 and x39 in table
        - lead bytes start always after x7F in table
        - second chars after lead bytes start always after x40 in table}
      if ((S1[i1] in ['0'..'9']) and (S2[i2] in ['0'..'9'])) then
        Result := Sign(ExtractNum(i1, s1, len1) - ExtractNum(i2, s2, len2))
      else
      begin
        j1 := i1;
        j2 := i2;
        repeat
          i1 := i1 + 1;
          i2 := i2 + 1;
        until (Min(len1-i1, len2-i2) < 0) or ((S1[i1] in ['0'..'9']) and (S2[i2] in ['0'..'9']));
        Result := AnsiCompareSubEx(S1, S2, IgnoreCase,
          (IgnoreAccents or TwoPasses) and not IsDefaultCPMBCS, j1, j2, i1-j1, i2-j2);
      end;
    until (Result <> 0) or (Min(len1-i1, len2-i2) < 0);
  end;
  if (Result = 0) then
    Result := Sign((len1-i1) - (len2-i2));
  if (Result = 0) and TwoPasses and not IgnoreAccents and not IsDefaultCPMBCS then
    Result := AnsiNatCompare(S1, S2, IgnoreCase, False, False);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function AnsiNatCompareText(const S1, S2: string; IgnoreAccents: Boolean = False): Integer;
begin
  Result := AnsiNatCompare(S1, S2, True, IgnoreAccents);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function AnsiNatCompareStr(const S1, S2: string; IgnoreAccents: Boolean = False): Integer;
begin
  Result := AnsiNatCompare(S1, S2, False, IgnoreAccents);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function AnsiNatSameText(const S1, S2: string; IgnoreAccents: Boolean = False): Boolean;
begin
  Result := AnsiNatCompareText(S1, S2, IgnoreAccents) = 0;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function AnsiNatSameStr(const S1, S2: string; IgnoreAccents: Boolean = False): Boolean;
begin
  Result := AnsiNatCompareStr(S1, S2, IgnoreAccents) = 0;
end;

{-------------------------------------------------------------------------------
  AnsiPosEx:
  - IgnoreCase = False
    The strings are compared with regard to case
  - IgnoreCase = True
    The strings are compared without regard to case
  - IgnoreAccents = False: The strings are compared with accents
    (e.g. c < e < é < d)
  - IgnoreAccents = True: The strings are compared without regard to accents
    (e.g. c < e = é < d)
  - StartPos: Start position in S where we start search from left to right
    (1 by default)
  - With DBCS, IgnoreAccents option is ignored
  - With other MBCS, IgnoreCase and IgnoreAccents options are ignored
-------------------------------------------------------------------------------}

function AnsiPosEx(const SubStr, S: string; IgnoreCase: Boolean = False;
  IgnoreAccents: Boolean = False; StartPos: Integer = 1): Integer;
var
  i1, i2, i2start, cmp: Integer;
  len1, len2: Integer;
  CmpChar: PCmpChar;
  {With DBCS, we keep previous lead byte information for SubStr and S to
   handle the comparison properly}
  PrevLeadByteSubStr: Boolean;
  PrevLeadByteS: Boolean;
  PrevLeadByteSStart: Boolean;
  PStartPos, P: PChar;
begin
  Result := 0;
  len1 := Length(SubStr);
  len2 := Length(S);
  if (len1 = 0) or (len2 = 0) or (len1 > len2) then
    Exit;
  if (StartPos > len2) then
    Exit;
  if (StartPos < 1) then
    StartPos := 1;

  {With MBCS (not DBCS), we call original function AnsiPos}
  if IsDefaultCPMBCS and not IsDefaultCPDBCS then
  begin
    PStartPos := PChar(S) + (StartPos - 1);
    P := AnsiStrPos(PStartPos, PChar(SubStr));
    if P <> nil then
      Result := Integer(P) - Integer(PChar(S)) + 1;
    Exit;
  end;

  i1 := 1;
  i2 := StartPos;
  PrevLeadByteSubStr := False;
  PrevLeadByteS := False;
  { With DBCS, we initialise value of PrevLeadByteS properly }
  if IsDefaultCPDBCS then
  begin
    i2 := 1;
    while i2 < StartPos do
    begin
      PrevLeadByteS := not PrevLeadByteS and IsDefaultCPLeadByte(Ord(S[i2]));
      i2 := i2 + 1;
    end;
  end;
  if IgnoreCase then
    if IgnoreAccents then
      CmpChar := @CmpCharNoCaseAccentSensitive
    else
      CmpChar := @CmpCharNoCaseSensitive
  else
    if IgnoreAccents then
      CmpChar := @CmpCharNoAccentSensitive
    else
      CmpChar := @CmpCharSensitive;
  repeat
    i2start := i2;
    PrevLeadByteSStart := PrevLeadByteS;
    repeat
      if not IsDefaultCPDBCS then
        cmp := CmpChar^[Ord(SubStr[i1])][Ord(S[i2])]
      else
      begin
        if PrevLeadByteSubStr <> PrevLeadByteS then
          cmp := -1
        else if not PrevLeadByteSubStr then
          cmp := CmpChar^[Ord(SubStr[i1])][Ord(S[i2])]
        else
          cmp := CompareValue(Ord(SubStr[i1]), Ord(S[i2]));
        PrevLeadByteSubStr := not PrevLeadByteSubStr and IsDefaultCPLeadByte(Ord(SubStr[i1]));
        PrevLeadByteS := not PrevLeadByteS and IsDefaultCPLeadByte(Ord(S[i2]));
      end;
      i1 := i1 + 1;
      i2 := i2 + 1;
    until (cmp <> 0) or (Min(len1 - i1, len2 - i2) < 0);
    if (cmp <> 0) or (i2 - i2start <> len1) then
    begin
      i1 := 1;
      i2 := i2start + 1;
      if IsDefaultCPDBCS then
      begin
        PrevLeadByteSubStr := False;
        PrevLeadByteS := not PrevLeadByteSStart and IsDefaultCPLeadByte(Ord(S[i2-1]));
      end;
    end else
      Result := i2start;
  until (len2 - i2 + 1 < len1) or (Result <> 0);
end;

{-------------------------------------------------------------------------------
  AnsiLastPosEx:
  - IgnoreCase = False
    The strings are compared with regard to case
  - IgnoreCase = True
    The strings are compared without regard to case
  - IgnoreAccents = False: The strings are compared with accents
    (e.g. c < e < é < d)
  - IgnoreAccents = True: The strings are compared without regard to accents
    (e.g. c < e = é < d)
  - StartPos: Start position in S where we start search from right to left
    (Length(S) by default)
  - With DBCS, IgnoreAccents option is ignored
  - With MBCS, IgnoreCase and IgnoreAccents options are ignored
  Note : String comparison is done in reverse only for Single-Byte Character Sets
-------------------------------------------------------------------------------}

function AnsiLastPosEx(const SubStr, S: string; IgnoreCase: Boolean = False;
  IgnoreAccents: Boolean = False; StartPos: Integer = MaxInt): Integer;
var
  i1, i2, i2start, cmp: Integer;
  len1, len2: Integer;
  CmpChar: PCmpChar;
begin
  Result := 0;
  len1 := Length(SubStr);
  len2 := Length(S);
  if (len1 = 0) or (len2 = 0) or (len1 > len2) then
    Exit;
  if (StartPos < 1) then
    Exit;
  if (StartPos > len2) then
    StartPos := len2;

  {With MBCS, we have to perform string comparison from left to right using AnsiPosEx}
  if IsDefaultCPMBCS then
  begin
    i2 := 0;
    i1 := AnsiPosEx(SubStr, S, IgnoreCase, IgnoreAccents);
    while (i1 <> 0) and (i1 + len1 - 1 <= StartPos) do
    begin
      i2 := i1;
      i1 := AnsiPosEx(SubStr, S, IgnoreCase, IgnoreAccents, i1 + len1);
    end;
    Result := i2;
    Exit;
  end;

  i1 := len1;
  i2 := StartPos;
  if IgnoreCase then
    if IgnoreAccents then
      CmpChar := @CmpCharNoCaseAccentSensitive
    else
      CmpChar := @CmpCharNoCaseSensitive
  else
    if IgnoreAccents then
      CmpChar := @CmpCharNoAccentSensitive
    else
      CmpChar := @CmpCharSensitive;
  repeat
    i2start := i2;
    repeat
      cmp := CmpChar^[Ord(SubStr[i1])][Ord(S[i2])];
      i1 := i1 - 1;
      i2 := i2 - 1;
    until (cmp <> 0) or (Min(i1, i2) < 1);
    if (cmp <> 0) or (i2start - i2 <> len1) then
    begin
      i1 := len1;
      i2 := i2start - 1;
    end else
      Result := i2 + 1;
  until (i2 < len1) or (Result <> 0);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function AnsiContainsEx(const S, SubStr: string; IgnoreCase: Boolean = False;
  IgnoreAccents: Boolean = False): Boolean;
begin
  Result := AnsiPosEx(SubStr, S, IgnoreCase, IgnoreAccents) > 0;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function AnsiContainsTextEx(const S, SubStr: string; IgnoreAccents: Boolean = False): Boolean;
begin
  Result := AnsiContainsEx(S, SubStr, True, IgnoreAccents);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function AnsiContainsStrEx(const S, SubStr: string; IgnoreAccents: Boolean = False): Boolean;
begin
  Result := AnsiContainsEx(S, SubStr, False, IgnoreAccents);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

// Fit string at best to have only us-ascii characters (remove accents, ...)
function AnsiBestFitUS(const AInput: string): string;
const
  CodePage = 20127; //20127 = us-ascii
var
  WS: WideString;
begin
  WS := WideString(AInput);
  SetLength(Result, WideCharToMultiByte(CodePage, 0, PWideChar(WS),
    Length(WS), nil, 0, nil, nil));
  WideCharToMultiByte(CodePage, 0, PWideChar(WS), Length(WS),
    PAnsiChar(Result), Length(Result), nil, nil);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

initialization
  InitDefaultCPInfo;

end.
