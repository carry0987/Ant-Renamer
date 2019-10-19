(************************************************************************
 *                                                                      *
 *   Ant Renamer 2.x                                                    *
 *   (C) 2003-2006 Antoine Potten                                       *
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

unit Log;

interface

uses
  SysUtils, Classes,
  TntSystem, TntClasses, TntSysUtils,
  Files;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

type
  TLogNode = TRenError;
  PLogNode = ^TLogNode;

  TLogWriter = class(TObject)
  private
    FFile: TTntFileStream;
  protected
  public
    constructor Create(const AFileName: WideString; const Append: Boolean);
    destructor Destroy; override;
    procedure Add(const ALine: PLogNode);
  end;

implementation

{-------------------------------------------------------------------------------
  TLogWriter 
-------------------------------------------------------------------------------}

constructor TLogWriter.Create(const AFileName: WideString; const Append: Boolean);
begin
  FFile := nil;
  try
    if Append and WideFileExists(AFileName) then
      FFile := TTntFileStream.Create(AFileName, fmOpenWrite)
    else
      FFile := TTntFileStream.Create(AFileName, fmCreate);
    FFile.Seek(0, soFromEnd);
    if FFile.Position = 0 then
      FFile.Write(UTF8_BOM[1], Length(UTF8_BOM));
  except
    FreeAndNil(FFile);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

destructor TLogWriter.Destroy;
begin
  FFile.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TLogWriter.Add(const ALine: PLogNode);
var
  s: string;
begin
  if Assigned(FFile) then
  begin
    case ALine^.Status of
      rsNotRenamed: s := ' ';
      rsOk:         s := 'v';
      rsError:      s := 'x';
    else
      Exit;
    end;
    s := Format('%s%s%s%s', [s, #9, UTF8Encode(ALine^.Description), sLineBreak]);
    FFile.Write(s[1], Length(s));
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

end.
