{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvLabel.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is S�bastien Buysse [sbuysse@buypin.com]
Portions created by S�bastien Buysse are Copyright (C) 2001 S�bastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

unit AntJvLabel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls;

type
  TAntJvLabel = class(TLabel)
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FHotTrack: Boolean;
    FHotTrackFont: TFont;
    FFontSave: TFont;
    FHintColor: TColor;
    FHintSaved: TColor;
    FOver: Boolean;
    procedure SetHotTrackFont(const Value: TFont);
  protected
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CMDialogChar(var Msg: TCMDialogChar); message CM_DIALOGCHAR;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Action;
    property HotTrack: Boolean read FHotTrack write FHotTrack default False;
    property HotTrackFont: TFont read FHotTrackFont write SetHotTrackFont;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  end;

implementation

constructor TAntJvLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHotTrack := False;
  FHotTrackFont := TFont.Create;
  FFontSave := TFont.Create;
  FHintColor := clInfoBk;
  FOver := False;
end;

destructor TAntJvLabel.Destroy;
begin
  FHotTrackFont.Free;
  FFontSave.Free;
  inherited Destroy;
end;

procedure TAntJvLabel.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

procedure TAntJvLabel.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TAntJvLabel.CMMouseEnter(var Msg: TMessage);
begin
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if not FOver then
  begin
    FHintSaved := Application.HintColor;
    Application.HintColor := FHintColor;
    if HotTrack then
    begin
      FFontSave.Assign(Font);
      Font.Assign(FHotTrackFont);
    end;
    FOver := True;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TAntJvLabel.CMMouseLeave(var Msg: TMessage);
begin
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if FOver then
  begin
    Application.HintColor := FHintSaved;
    if HotTrack then
      Font.Assign(FFontSave);
    FOver := False;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TAntJvLabel.SetHotTrackFont(const Value: TFont);
begin
  FHotTrackFont.Assign(Value);
end;

procedure TAntJvLabel.CMDialogChar(var Msg: TCMDialogChar);
var
  Form: TCustomForm;
begin
  inherited;
  if Msg.Result = 1 then
  begin
    Form := GetParentForm(Self);
    if Assigned(Form) and Assigned(Form.ActiveControl) and not Form.ActiveControl.TabStop then
      PostMessage(Form.Handle, WM_NEXTDLGCTL, 0, 0);
  end;
end;

end.

