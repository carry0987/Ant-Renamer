(************************************************************************
 *                                                                      *
 *   (C) 2002-2012 Antoine Potten, Mickaël Vanneufville                 *
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

unit functions_xml;

interface

uses
  Classes, SysUtils, JvSimpleXml;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure WriteXMLHeader(Root: TJvSimpleXmlElem; FileVersion: Integer; const RootName: string; const RefVersion, RefDate: string);
procedure AddNotEmpty(const Props: TJvSimpleXmlProps; const Name, Value: string);
function ReadTag(const Props: TJvSimpleXmlProps; const Name, Default: string; MultiLines: Boolean = False): string;
function XmlReadInt(const ToolbarName, Value: string; const Default: Longint; const ExtraData: Pointer): Longint;
function XmlReadString(const ToolbarName, Value, Default: string; const ExtraData: Pointer): string;
procedure XmlWriteInt(const ToolbarName, Value: string; const Data: Longint; const ExtraData: Pointer);
procedure XmlWriteString(const ToolbarName, Value, Data: string; const ExtraData: Pointer);
function FindItem(const ADoc: TJvSimpleXml; const APath: string): TJvSimpleXmlElem;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

implementation

uses
  functions_str;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure WriteXMLHeader(Root: TJvSimpleXmlElem; FileVersion: Integer; const RootName: string; const RefVersion, RefDate: string);
begin
  Root.Name := RootName;
  with Root.Properties do
  begin
    Clear;
    Add('Format', FileVersion);
    Add('Version', Format('%s (%s)', [RefVersion, RefDate]));
    Add('Date', DateTimeToStr(Now));
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure AddNotEmpty(const Props: TJvSimpleXmlProps; const Name, Value: string);
begin
  if Value <> '' then
    Props.Add(Name, ReplaceLineBreaks(Value));
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function ReadTag(const Props: TJvSimpleXmlProps; const Name, Default: string; MultiLines: Boolean): string;
begin
  if MultiLines then
    Result := InsertLineBreaks(Props.Value(Name, Default))
  else
    Result := Props.Value(Name, Default);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function XmlReadInt(const ToolbarName, Value: string; const Default: Longint; const ExtraData: Pointer): Longint;
var
  XmlItem: TJvSimpleXmlElem;
begin
  XmlItem := TJvSimpleXmlElem(ExtraData).Items.ItemNamed[ToolbarName];
  if XmlItem <> nil then
    Result := XmlItem.Properties.IntValue(Value, Default)
  else
    Result := Default;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function XmlReadString(const ToolbarName, Value, Default: string; const ExtraData: Pointer): string;
var
  XmlItem: TJvSimpleXmlElem;
begin
  XmlItem := TJvSimpleXmlElem(ExtraData).Items.ItemNamed[ToolbarName];
  if XmlItem <> nil then
    Result := XmlItem.Properties.Value(Value, Default)
  else
    Result := Default;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure XmlWriteInt(const ToolbarName, Value: string; const Data: Longint; const ExtraData: Pointer);
var
  XmlItem: TJvSimpleXmlElem;
begin
  XmlItem := TJvSimpleXmlElem(ExtraData).Items.ItemNamed[ToolbarName];
  if XmlItem = nil then
    XmlItem := TJvSimpleXmlElem(ExtraData).Items.Add(ToolbarName);
  XmlItem.Properties.Add(Value, Data);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure XmlWriteString(const ToolbarName, Value, Data: string; const ExtraData: Pointer);
var
  XmlItem: TJvSimpleXmlElem;
begin
  XmlItem := TJvSimpleXmlElem(ExtraData).Items.ItemNamed[ToolbarName];
  if XmlItem = nil then
    XmlItem := TJvSimpleXmlElem(ExtraData).Items.Add(ToolbarName);
  XmlItem.Properties.Add(Value, Data);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function FindItem(const ADoc: TJvSimpleXml; const APath: string): TJvSimpleXmlElem;
var
  lst: TStringList;
  i: Integer;
begin
  Result := nil;
  lst := TStringList.Create;
  try
    lst.Delimiter := '.';
    lst.DelimitedText := APath;
    if lst.Count = 0 then
      Exit;
    if lst[0] = ADoc.Root.Name then
      Result := ADoc.Root
    else
      Exit;
    for i := 1 to lst.Count-1 do
    begin
      Result := Result.Items.ItemNamed[lst[i]];
      if Result = nil then
        Exit;
    end;
  finally
    lst.Free;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

end.
