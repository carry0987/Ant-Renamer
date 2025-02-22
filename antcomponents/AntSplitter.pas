(*

(c) 2002-2003 Antoine Potten
software@antp.be
htt://www.antp.be/software/

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

*)

unit AntSplitter;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, ExtCtrls;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

type
  TAntSplitter = class(TPanel)
  private
    FControl1: TWinControl;
    FControl2: TWinControl;
    FOldY: Integer;
    FOldControl1Y: Integer;
    FOldControl2Y: Integer;
    procedure PanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure Loaded; override;
  published
    property ControlAtTop: TWinControl read FControl1 write FControl1;
    property ControlAtBottom: TWinControl read FControl2 write FControl2;
  end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

implementation

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

constructor TAntSplitter.Create(AOwner: TComponent);
begin
  inherited;
  Cursor := crVSplit;
  Height := 4;
  Caption := '';
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAntSplitter.Loaded;
begin
  inherited;
  if (FControl1 <> nil) and (FControl2 <> nil) then
  begin
    OnMouseMove := PanelMouseMove;
    OnMouseDown := PanelMouseDown;
    OnMouseUp := PanelMouseUp;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAntSplitter.PanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FOldY := Y;
  FOldControl1Y := FControl1.Top + FControl1.Height;
  FOldControl2Y := FControl2.Top + FControl2.Height;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAntSplitter.PanelMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  Delta: Integer;
begin
  if Shift = [ssLeft] then
  begin
    if FOldY <> -1 then
    begin
      Delta := Y - FOldY;
      if ((FControl2.Top + Delta) < (FOldControl2Y - 21))
        and (FControl1.Height + Delta > 42) then
      begin
        FControl2.Top := FControl2.Top + Delta;
        FControl2.Height := FControl2.Height - Delta;
        Top := Top + Delta;
        FControl1.Height := FControl1.Height + Delta;
      end;
      FOldY := -1;
    end else
      FOldY := Y;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAntSplitter.PanelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  PanelMouseMove(Sender, Shift, X, Y);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

end.
