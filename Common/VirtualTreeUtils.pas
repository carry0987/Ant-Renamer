(************************************************************************
 *                                                                      *
 *   (C) 2006 Antoine Potten                                            *
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

unit VirtualTreeUtils;

interface

uses
  SysUtils, Classes,
  VirtualTrees, JvSimpleXml;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

type
  TGetVTColumnNameEvent = function(const ColIndex: Integer): string;
  TGetVTColumnIndexEvent = function(const ColName: string): Integer;

procedure SaveVTColumns(Header: TObject; XmlNode: TJvSimpleXmlElem; GetColNameProc: TGetVTColumnNameEvent = nil); overload;
procedure SaveVTColumns(Header: TVTHeader; XmlNode: TJvSimpleXmlElem; GetColNameProc: TGetVTColumnNameEvent = nil); overload;
procedure LoadVTColumns(Header: TObject; XmlNode: TJvSimpleXmlElem; GetColIndexProc: TGetVTColumnIndexEvent = nil); overload;
procedure LoadVTColumns(Header: TVTHeader; XmlNode: TJvSimpleXmlElem; GetColIndexProc: TGetVTColumnIndexEvent = nil); overload;

implementation

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure SaveVTColumns(Header: TObject; XmlNode: TJvSimpleXmlElem; GetColNameProc: TGetVTColumnNameEvent = nil);
begin
  if Header is TVTHeader then
    SaveVTColumns(Header as TVTHeader, XmlNode, GetColNameProc)
  else
    raise Exception.Create('Bad object in SaveVTColumns; expected TVTHeader got ' + Header.ClassName);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure SaveVTColumns(Header: TVTHeader; XmlNode: TJvSimpleXmlElem; GetColNameProc: TGetVTColumnNameEvent = nil);
var
  i: Integer;
  NewNode: TJvSimpleXmlElem;
begin
  XmlNode.Items.Clear;
//  XmlNode.Properties.Clear;
  with Header.Columns do
    for i := 0 to Count-1 do
      with Items[i] do
      begin
        if not Assigned(GetColNameProc) then
        begin
          NewNode := XmlNode.Items.Add('Column');
          NewNode.Properties.Add('Nr', i);
        end
        else
          NewNode := XmlNode.Items.Add(GetColNameProc(i));
        NewNode.Properties.Add('Position', Position);
        NewNode.Properties.Add('Width', Width);
        NewNode.Properties.Add('Visible', coVisible in Options);
      end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure LoadVTColumns(Header: TObject; XmlNode: TJvSimpleXmlElem; GetColIndexProc: TGetVTColumnIndexEvent = nil);
begin
  if Header is TVTHeader then
    LoadVTColumns(Header as TVTHeader, XmlNode, GetColIndexProc)
  else
    raise Exception.Create('Bad object in LoadVTColumns; expected TVTHeader got ' + Header.ClassName);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure LoadVTColumns(Header: TVTHeader; XmlNode: TJvSimpleXmlElem; GetColIndexProc: TGetVTColumnIndexEvent = nil);
var
  i, ColNr, Val: Integer;
  CurNode: TJvSimpleXmlElem;
begin
  with Header.Columns do
    for i := 0 to XmlNode.Items.Count-1 do
    begin
      CurNode := XmlNode.Items.Item[i];
      ColNr := -1; // and without this line the compiler says that it may be not initialized... a hint is better than a warning ;)
      if not Assigned(GetColIndexProc) then
      begin
        if CurNode.Name <> 'Column' then
          Continue;
        ColNr := CurNode.Properties.IntValue('Nr', -1);
      end
      else
        ColNr := GetColIndexProc(CurNode.Name);
      if (ColNr < 0) or (ColNr > Count-1) then
        Continue;
      Val := CurNode.Properties.IntValue('Position', -1);
      if (Val >= 0) then
        Items[ColNr].Position := Val;
      Val := CurNode.Properties.IntValue('Width', -1);
      if (Val >= 0) then
        Items[ColNr].Width := Val;
      if CurNode.Properties.BoolValue('Visible', coVisible in Items[ColNr].Options) then
        Items[ColNr].Options := Items[ColNr].Options + [coVisible]
      else
        Items[ColNr].Options := Items[ColNr].Options - [coVisible];
    end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

end.
