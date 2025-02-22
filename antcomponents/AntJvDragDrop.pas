{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDragDrop.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2003-04-11

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

2003-04-11 : Antoine Potten : conversion of TJvDragDrop to use unicode filenames
                              under Windows NT (using TTntStringList from
                              'TNT Unicode Controls') if ANTUNICODE is defined.

Known Issues:
-----------------------------------------------------------------------------}

unit AntJvDragDrop;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls,
  {$IFDEF ANTUNICODE}
  TntClasses, TntWideStrUtils, TntSysUtils,
  {$ENDIF}
  ShellApi;

type
  {$IFDEF ANTUNICODE}
  TDropEvent = procedure(Sender: TObject; Pos: TPoint; Value: TTntStringList) of object;
  {$ELSE}
  TDropEvent = procedure(Sender: TObject; Pos: TPoint; Value: TStringList) of object;
  {$ENDIF}


  TJvDragDrop = class(TComponent)
  private
    FAcceptDrag: Boolean;
    { (rb) Don't remember FHandle, it may be changed, for instance by
      setting FormStyle }
    //FHandle: THandle;
    {$IFDEF ANTUNICODE}
    FFiles: TTntStringList;
    {$ELSE}
    FFiles: TStringList;
    {$ENDIF}
    FOnDrop: TDropEvent;
    FIsHooked: Boolean;
    {$IFDEF ANTUNICODE}
    procedure DropFilesW(Handle: HDROP);
    {$ENDIF}
    procedure DropFiles(Handle: HDROP);
    procedure SetAcceptDrag(Value: Boolean);
    function WndProc(var Msg: TMessage): Boolean;
  protected
    procedure HookControl;
    procedure UnHookControl;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF ANTUNICODE}
    property Files: TTntStringList read FFiles;
    {$ELSE}
    property Files: TStringList read FFiles;
    {$ENDIF}
  published
    property AcceptDrag: Boolean read FAcceptDrag write SetAcceptDrag default True;
    property OnDrop: TDropEvent read FOnDrop write FOnDrop;
  end;

implementation

uses
  AntJvWndProcHook;

constructor TJvDragdrop.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAcceptDrag := True;
  {$IFDEF ANTUNICODE}
  FFiles := TTntStringList.Create;
  {$ELSE}
  FFiles := TStringList.Create;
  {$ENDIF}
  FIsHooked := False;
end;

destructor TJvDragdrop.Destroy;
begin
  FFiles.Free;
  { (rb) Actually Owner now = nil, thus the unhooking is already done, in the
    function TJvWndProcHook.Notification }
  UnHookControl;
  inherited Destroy;
end;

procedure TJvDragdrop.SetAcceptDrag(Value: Boolean);
begin
  FAcceptDrag := Value;
  if [csDesigning, csLoading] * ComponentState <> [] then
    Exit;

  if Owner is TWinControl then
  begin
    DragAcceptFiles(TWinControl(Owner).Handle, FAcceptDrag);
    if FAcceptDrag then
      HookControl
    else
      UnHookControl;
  end;
end;

function TJvDragdrop.WndProc(var Msg: TMessage): boolean;
begin
  Result := Msg.Msg = WM_DROPFILES;
  if Result then
  begin
    {$IFDEF ANTUNICODE}
    if (Win32Platform = VER_PLATFORM_WIN32_NT) then
      DropFilesW(HDrop(Msg.wParam))
    else
    {$ENDIF}
      DropFiles(HDrop(Msg.wParam));
  end;
end;

{$IFDEF ANTUNICODE}
procedure TJvDragdrop.DropFilesW(Handle: HDROP);
var
  pszFileWithPath, pszFile: PWideChar;
  iFile, iStrLen, iTempLen: Integer;
  MousePt: TPoint;
  Count: Integer;
  ws: WideString;
begin
  FFiles.Clear;
  iStrLen := 256 * SizeOf(WideChar);
  pszFileWithPath := WStrAlloc(iStrLen);
  pszFile := WStrAlloc(iStrLen);
  Count := DragQueryFileW(Handle, $FFFFFFFF, pszFile, iStrLen);
  iFile := 0;
  while iFile < Count do
  begin
    iTempLen := DragQueryFileW(Handle, iFile, nil, 0) + 2;
    if iTempLen > iStrLen then
    begin
      iStrLen := iTempLen;
      WStrDispose(pszFileWithPath);
      pszFileWithPath := WStrAlloc(iStrLen);
    end;
    DragQueryFileW(Handle, iFile, pszFileWithPath, iStrLen);
    SetString(ws, pszFileWithPath, WStrLen(pszFileWithPath));
    FFiles.Add(ws);
    Inc(iFile);
  end;
  WStrDispose(pszFileWithPath);
  if Assigned(FOnDrop) then
  begin
    DragQueryPoint(Handle, MousePt);
    FOnDrop(Self, MousePt, FFiles);
  end;
  DragFinish(Handle);
end;
{$ENDIF}

procedure TJvDragDrop.DropFiles(Handle: HDROP);
var
  pszFileWithPath, pszFile: PChar;
  iFile, iStrLen, iTempLen: Integer;
  MousePt: TPoint;
  Count: Integer;
begin
  FFiles.Clear;
  iStrLen := 256;
  pszFileWithPath := StrAlloc(iStrLen);
  pszFile := StrAlloc(iStrLen);
  Count := DragQueryFile(Handle, $FFFFFFFF, pszFile, iStrLen);
  iFile := 0;
  while iFile < Count do
  begin
    iTempLen := DragQueryFile(Handle, iFile, nil, 0) + 1;
    if iTempLen > iStrLen then
    begin
      iStrLen := iTempLen;
      StrDispose(pszFileWithPath);
      pszFileWithPath := StrAlloc(iStrLen);
    end;
    DragQueryFile(Handle, iFile, pszFileWithPath, iStrLen);
    FFiles.Add(StrPas(pszFileWithPath));
    Inc(iFile);
  end;
  StrDispose(pszFileWithPath);
  if Assigned(FOnDrop) then
  begin
    DragQueryPoint(Handle, MousePt);
    FOnDrop(Self, MousePt, FFiles);
  end;
  DragFinish(Handle);
end;

procedure TJvDragdrop.Loaded;
begin
  inherited Loaded;
  SetAcceptDrag(FAcceptDrag);
end;

procedure TJvDragdrop.HookControl;
begin
  if FIsHooked then Exit;

  if (Owner is TWinControl) and not (csDesigning in ComponentState) then
    FIsHooked := RegisterWndProcHook(TWinControl(Owner), WndProc, hoBeforeMsg);
end;

procedure TJvDragdrop.UnHookControl;
begin
  if not FIsHooked then Exit;

  FIsHooked := False;

  if (Owner is TWinControl) and not (csDesigning in ComponentState) then
    UnRegisterWndProcHook(TWinControl(Owner), WndProc, hoBeforeMsg);
end;

end.

