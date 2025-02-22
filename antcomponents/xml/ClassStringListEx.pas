 (**
 *
 * Copyright 2004, Akretio SPRL.  All Rights Reserved.
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

unit ClassStringListEx;

{.$I kelare.inc}

interface

uses
  Classes, SysUtils;

type
  PStringTree = ^TStringTree;
  TStringTree = record
    Index: Integer;
    Letter: Char;
    Count: Byte;
    Trees: TList;
  end;

  TStringListEx = class(TStringList)
  protected
    FTree: TStringTree;
    FCount: Integer;
    FUpdates: Integer;
    procedure Changed; override;
    procedure DoClearSort;
    procedure DoMakeSort;
    procedure DoAddItem(const AIndex: Integer; const AValue: string);
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    procedure Clear; override;
    function IndexOf(const S: string): Integer; override;
    function Add(const S: string): Integer; override;
    function AddObject(const S: string; AObject: TObject): Integer; override;
  end;


implementation

{ TStringListEx }

{**************************************************************************}
function TStringListEx.Add(const S: string): Integer;
begin
  result := inherited Add(s);
  if FCount = Count - 1 then
  begin
    DoAddItem(result, S);
    inc(FCount);
  end;
end;
{**************************************************************************}
function TStringListEx.AddObject(const S: string;
  AObject: TObject): Integer;
begin
  result := inherited AddObject(s, AObject);
  if FCount = Count - 1 then
  begin
    DoAddItem(result, S);
    inc(FCount);
  end;
end;
{**************************************************************************}
procedure TStringListEx.AfterConstruction;
begin
  inherited;
  FTree.Trees := TList.Create;
  //{$IFNDEF ISMODULE}AddToLog('Construction '+IntToStr(Integer(self)));{$ENDIF}
end;
{**************************************************************************}
procedure TStringListEx.Changed;
begin
  inherited;
  if self.UpdateCount = 0 then
    inc(FUpdates);
end;
{**************************************************************************}
procedure TStringListEx.Clear;
begin
  inherited;
  DoClearSort;
end;
{**************************************************************************}
destructor TStringListEx.Destroy;
begin
  //{$IFNDEF ISMODULE}AddToLog('Destruction '+IntToStr(Integer(self)));{$ENDIF}
  DoClearSort;
  FTree.Trees.Free;
  inherited;
end;
{**************************************************************************}
procedure TStringListEx.DoAddItem(const AIndex: Integer;
  const AValue: string);
var
 i,j,k: Integer;
 lTree, lTree2: PStringTree;
begin
  lTree := @FTree;
  for i:=1 to Length(AValue) do
  begin
    //Find index
    k := -1;
    for j:=0 to lTree.Count-1 do
      if PStringTree(lTree.Trees[j])^.Letter = AValue[i] then
      begin
        k := j;
        Break;
      end;

    //Create sub array?
    if k = -1 then
    begin
      k := lTree.Count;
      Inc(lTree.Count);
      GetMem(lTree2, SizeOf(TStringTree));
      lTree.Trees.Capacity := lTree.Count;
      lTree.Trees.Add(lTree2);
      //New(lTree^.Trees[Ord(AValue[i])]);
      with lTree2^ do
      begin
        Letter := AValue[i];
        Index := -1;            
        Count := 0;
        Trees := TList.Create;
        Trees.Capacity := 1;
      end;
    end;

    //Get next item
    lTree := lTree.Trees[k];
  end;
  lTree^.Index := AIndex;
end;
{**************************************************************************}
procedure TStringListEx.DoClearSort;
var
 lList: TList;
 i: Integer;
begin
  lList := TList.Create;

  //Get first items
  FTree.Index := -1;
  for i:=0 to FTree.Count-1 do
    lList.Add(FTree.Trees[i]);
  FTree.Trees.Clear;
  FTree.Count := 0;

  while lList.Count > 0 do
  begin
    with PStringTree(lList[0])^ do
      for i:=0 to Count - 1 do
        lList.Add(Trees[i]);
    //Dispose(PStringTree(lList[0]));
    PStringTree(lList[0])^.Trees.Free;
    FreeMem(lList[0], SizeOf(TStringTree));
    lList.Delete(0);
  end;
  FreeAndNil(lList);
  FCount := 0;
end;
{**************************************************************************}
procedure TStringListEx.DoMakeSort;
var
 i: Integer;
begin
  if CaseSensitive then
  begin
    for i := 0 to Count-1 do
      DoAddItem(i, Strings[i])
  end
  else
  begin
    for i := 0 to Count-1 do
      DoAddItem(i, LowerCase(Strings[i]));
  end;
  FCount := Count;
end;
{**************************************************************************}
function TStringListEx.IndexOf(const S: string): Integer;
var
 lTree: PStringTree;
 i,j,k: Integer;
 lSearch: string;
begin
  if (FUpdates > 0) or (FCount <> Count) then
  begin
    DoClearSort;
    DoMakeSort;
    FUpdates := 0;
  end;
  if not CaseSensitive then
    lSearch := LowerCase(S)
  else
    lSearch := S;
  result := -1;
  lTree := @FTree;
  for i:=1 to Length(lSearch) do
  begin
    k := -1;
    for j:=0 to lTree^.Count-1 do
      if PStringTree(lTree^.Trees[j])^.Letter = lSearch[i] then
      begin
        lTree := lTree^.Trees[j];
        k := j;
        Break;
      end;
    if k = -1 then
      Exit;
  end;
  result := lTree^.Index;
end;
{**************************************************************************}

end.
