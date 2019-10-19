(************************************************************************
 *                                                                      *
 *   (C) 2002-2006 Antoine Potten                                       *
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

unit functions_strformat;

interface

uses
  Classes, SysUtils;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

type
  TAdvancedFormatEvent = function (const Tag: WideString; out Value: WideString): Boolean of object;
  TAdvancedFormatOption = (afoAllowQuotes, afoAllowIf);
  TAdvancedFormatOptions = set of TAdvancedFormatOption;

function AdvancedFormat(const Mask: WideString; OnGetValue: TAdvancedFormatEvent; const Options: TAdvancedFormatOptions = []; const StopAtChar: WideChar = #0): WideString;

implementation

const
  KeyIf = '$if(';
  LIf = Length(KeyIf);

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function FindNextChar(const Mask: WideString; i: Integer; const ToFind: WideChar; const Options: TAdvancedFormatOptions): Integer;
var
  L: Integer;
  inStr: Boolean;
  inCond: Integer;
  {
    si > 0 alors on est dans un set de parenthèses suivant un $if
    compteur au lieu de booleen, car les conditions peuvent être imbriquées
    quand il revient à zéro c'est qu'on est sorti de toutes les conditions
    dans lesquels on est entré
    ça foire si on met des parenthèses dans un if sans les protéger avec des
    quotes, mais bon ça c'est comme ça, c'est une feature
  }
begin
  inStr := False;
  inCond := 0;
  L := Length(Mask);
  while i <= L do
  begin
    if (Mask[i] = ToFind) and not (inStr or (inCond > 0)) then
      Break
    else
    if (Mask[i] = ')') and not (inStr) then
    begin
      Dec(inCond);
      if inCond < 0 then
        inCond := 0;
      Inc(i);
    end
    else
    if (afoAllowIf in Options) and (Mask[i] = '$') and not (inStr) then
    begin
      if (i < L) and (Mask[i+1] = '$') then
        Inc(i, 2)
      else
      if Copy(Mask, i, LIf) = KeyIf then
      begin
        Inc(inCond);
        Inc(i, LIf);
      end;
    end
    else
    if (afoAllowQuotes in Options) and (Mask[i] = '"') then
    begin
      if not ((i < L) and (Mask[i+1] = '"')) then
      begin
        inStr := not inStr;
        Inc(i);
      end
      else
        Inc(i, 2);
    end
    else
      Inc(i);
  end;
  Result := i;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function AdvancedFormat(const Mask: WideString; OnGetValue: TAdvancedFormatEvent; const Options: TAdvancedFormatOptions = []; const StopAtChar: WideChar = #0): WideString;
var
  i, j, L: Integer;
  inStr: Boolean;
  ReplVal: WideString;
begin
  inStr := False;
  i := 1;
  L := Length(Mask);
  Result := '';
  while i <= L do
  begin
    if (Mask[i] = StopAtChar) and not (inStr) then
      Exit
    else
    if (afoAllowQuotes in Options) and (Mask[i] = '"') then
    begin
      if not ((i < L) and (Mask[i+1] = '"')) then
      begin
        inStr := not inStr;
        Inc(i);
      end
      else
      begin
        Result := Result + '"';
        Inc(i, 2);
      end;
    end
    else
    if (afoAllowIf in Options) and (Mask[i] = '$') and not (inStr) then
    begin
      if (i < L) and (Mask[i+1] = '$') then
      begin
        Result := Result + '$';
        Inc(i, 2);
      end
      else
      if Copy(Mask, i, LIf) = KeyIf then
      begin
        Inc(i, LIf);
        if AdvancedFormat(Copy(Mask, i, L), OnGetValue, Options, ',') <> '' then
        begin
          i := FindNextChar(Mask, i, ',', Options) + 1;
          Result := Result + AdvancedFormat(Copy(Mask, i, L), OnGetValue, Options, ',');
          i := FindNextChar(Mask, i, ')', Options) + 1;
        end
        else
        begin
          i := FindNextChar(Mask, i, ',', Options) + 1;
          i := FindNextChar(Mask, i, ',', Options) + 1;
             // si si, c'est normal qu'y ait 2x la même chose
          Result := Result + AdvancedFormat(Copy(Mask, i, L), OnGetValue, Options, ')');
          i := FindNextChar(Mask, i, ')', Options) + 1;
        end;
      end
      else
        Inc(i);
    end
    else
    if (Mask[i] = '%') and (i < L) and not (inStr) then
    begin
      if (Mask[i+1] = '%') then
      begin
        Result := Result + '%';
        Inc(i, 2);
      end
      else
      begin
        j := FindNextChar(Mask, i+1, '%', Options);
        if OnGetValue(Copy(Mask, i+1, j-i-1), ReplVal) then
        begin
          Result := Result + ReplVal;
          i := j + 1;
        end
        else
        begin
          Result := Result + '%';
          Inc(i);
        end;
      end;
    end
    else
    begin
      Result := Result + Mask[i];
      Inc(i);
    end;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

end.
