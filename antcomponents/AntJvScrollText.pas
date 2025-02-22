{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvScrollText.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is S�bastien Buysse [sbuysse@buypin.com]
Portions created by S�bastien Buysse are Copyright (C) 2001 S�bastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com]
                Michael Freislich [mikef@korbi.net]

Last Modified: 2002-10-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

unit AntJvScrollText;

interface

{$I Ant.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls;

type
  TDirection = (drFromLeft, drFromRight, drFromTop, drFromBottom);

  TAntJvImageDrawThread = class(TThread)
  private
    FTag: Integer;
    FDelay: Cardinal;
    FOnDraw: TNotifyEvent;
  protected
    procedure Draw;
    procedure Execute; override;
  public
    property Tag: Integer read FTag write FTag;
    property Delay: Cardinal read FDelay write FDelay;
    property OnDraw: TNotifyEvent read FOnDraw write FOnDraw;
    property Terminated;
  end;

  TAntJvScrollText = class(TCustomControl)
  private
    FMemo: TStaticText;
    FTimerTag: Integer;
    FActive: Boolean;
    FDelay: Cardinal;
    FPixel: Integer;
    FCurrPos: Integer;
    FSelectable: Boolean;
    FScrollDirection: TDirection;
    FScrollSaved: Integer;
    FStrings: TStringList;
    FDeja: Cardinal;
    FScroll: TAntJvImageDrawThread;
    FFont: TFont;
    FStartY: Integer;
    FDown: Boolean;
    procedure SetItems(const Value: TStringList);
    procedure OnScroll(Sender: TObject);
    procedure SetActive(const Value: Boolean);
    procedure SetDelay(const Value: Cardinal);
    procedure SetPixel(const Value: Integer);
    procedure SetScrollDirection(const Value: TDirection);
    procedure CalculateMemo(Sender: TObject);
    function GetAlignment: TAlignment;
    procedure SetAlignment(const Value: TAlignment);
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    procedure FontChanged(Sender: TObject);
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);
    procedure MouseD(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseM(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MouseU(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SetCursor(Value: TCursor);
    function GetCursor: TCursor;
  protected
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // (p3) this should be just Alignment
    property TextAlignment: TAlignment read GetAlignment write SetAlignment;
    property Items: TStringList read FStrings write SetItems;
    property Active: Boolean read FActive write SetActive default False;
    property Delay: Cardinal read FDelay write SetDelay default 50;
    property ScrollPixels: Integer read FPixel write SetPixel default 1;
    property ScrollDirection: TDirection read FScrollDirection write SetScrollDirection default drFromBottom;
    property BackgroundColor: TColor read GetColor write SetColor;
    property Font: TFont read GetFont write SetFont;
    procedure Pause;
    procedure Unpause;
    procedure Reset;
    property Align;
    property ShowHint;
    property ParentShowHint;
    property Height default 150;
    property Width default 200;
    property Cursor: TCursor read GetCursor write SetCursor default crDefault;
  end;

implementation

procedure TAntJvImageDrawThread.Draw;
begin
  if not Terminated and Assigned(FOnDraw) then
    FOnDraw(Self);
end;

procedure TAntJvImageDrawThread.Execute;
begin
  // (rom) secure thread against exceptions
  try
    while not Terminated do
    begin
      Sleep(FDelay);
      Synchronize(Draw);
    end;
  except
  end;
end;

resourcestring
  RC_TestText = 'abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';

constructor TAntJvScrollText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 200;
  Height := 150;
  FActive := False;
  FDelay := 50;
  FPixel := 1;
  FCurrPos := 0;
  FSelectable := True;
  FScrollDirection := drFromBottom;
  FStrings := TStringList.Create;

  FMemo := TStaticText.Create(Self);
  FMemo.Parent := Self;
//  FMemo.SetBounds(2,2,Width-4,Height-4);
  FMemo.Width := Width;
  FMemo.Height := Height;
  FMemo.Borderstyle := sbsNone;
  FMemo.TabStop := False;
  FMemo.Enabled := FSelectable;
  FMemo.AutoSize := False;
  FMemo.OnMouseDown := MouseD;
  FMemo.OnMouseMove := MouseM;
  FMemo.OnMouseUp := MouseU;
  {$IFDEF DELPHI7_UP}
  FMemo.Transparent := False;
  {$ENDIF}

  FFont := TFont.Create;
  FFont := FMemo.Font;
  FFont.OnChange := FontChanged;

  FTimerTag := 0;
  FDown := False;
  FDeja := Application.HintPause;

  FScroll := TAntJvImageDrawThread.Create(True);
  FScroll.FreeOnTerminate := False;
  FScroll.Delay := FDelay;
  FScroll.OnDraw := OnScroll;
end;

destructor TAntJvScrollText.Destroy;
begin
  FScroll.OnDraw := nil;
  FScroll.Terminate;
//  FScroll.WaitFor;
  FreeAndNil(FScroll);
  Application.HintPause := FDeja;
  FStrings.Free;
  FMemo.Free;
  inherited Destroy;
end;

function TAntJvScrollText.GetFont: TFont;
begin
  Result := FMemo.Font;
end;

procedure TAntJvScrollText.SetFont(const Value: TFont);
var
  Al: TAlignment;
begin
  FMemo.Font.Assign(Value);
  CalculateMemo(Self);
  Al := FMemo.Alignment;
  if FMemo.Alignment = taCenter then
    FMemo.Alignment := taLeftJustify
  else
    FMemo.Alignment := taCenter;
  FMemo.Alignment := Al;
end;

procedure TAntJvScrollText.MouseD(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  P: TPoint;
begin
  P.X := X;
  P.Y := Y;
  P := FMemo.ClientToScreen(P);

  if ScrollDirection in [drFromTop, drFromBottom] then
    FStartY := P.Y
  else
    FStartY := P.X;
  FScroll.OnDraw := nil;
  FDown := True;
end;

procedure TAntJvScrollText.MouseM(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  NewY: Integer;
  P: TPoint;
begin
  if FDown then
  begin
    //if NewY>0, going up, NewY<0, going down
    P.X := X;
    P.Y := Y;
    P := FMemo.ClientToScreen(P);
    Y := P.Y;
    X := P.X;

    if ScrollDirection in [drFromTop, drFromBottom] then
    begin
      NewY := FStartY - Y;
      FStartY := Y;
      FCurrPos := FCurrPos - NewY;

      if FCurrPos < -FMemo.Height then
        FCurrPos := Height
      else
      if FCurrPos > Height then
        FCurrPos := -FMemo.Height;

      FMemo.Top := FCurrPos;
    end
    else
    begin
      NewY := FStartY - X;
      FStartY := X;
      FCurrPos := FCurrPos - NewY;

      if FCurrPos < -FMemo.Width then
        FCurrPos := Width
      else
      if FCurrPos > Width then
        FCurrPos := -FMemo.Width;

      FMemo.Left := FCurrPos;
    end;
  end;
end;

procedure TAntJvScrollText.MouseU(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FScroll.OnDraw := OnScroll;
  FDown := False;
end;

procedure TAntJvScrollText.OnScroll(Sender: TObject);
var
  T: Integer;
begin
  //tag=1 pause
  if FTimerTag = 1 then
  begin
    if FScrollSaved <= 0 then
    begin
      SetActive(False);
      FTimerTag := 0;
      Exit;
    end
    else
    begin
      T := FScrollSaved;
      Dec(FScrollSaved);
    end;
  end
  else
  if FTimerTag = 2 then
  begin
    if FScrollSaved >= FPixel then
    begin
      FTimerTag := 0;
      T := FPixel;
    end
    else
    begin
      T := FScrollSaved;
      Inc(FSCrollSaved);
    end;
  end
  else
    T := FPixel;

  //tag=2 unpause
  //FScrollDirection

  case ScrollDirection of
    drFromTop:
      begin
        if FCurrPos > Height then
          FCurrPos := -FMemo.Height
        else
          FCurrPos := FCurrPos + T;
        FMemo.Top := FCurrPos;
      end;
    drFromLeft:
      begin
        if - FCurrPos > FMemo.Width then
          FCurrPos := Width
        else
          FCurrpos := FCurrPos - T;
        FMemo.Left := FCurrPos;
      end;
    drFromRight:
      begin
        if FCurrPos > Width then
          FCurrPos := -Width
        else
          FCurrpos := FCurrPos + T;
        FMemo.Left := FCurrPos;
      end;
    drFromBottom:
      begin
        if - FCurrPos > FMemo.Height then
          FCurrPos := Height
        else
          FCurrPos := FCurrPos - T;
        FMemo.Top := FCurrPos;
      end;
  end;
end;

procedure TAntJvScrollText.Pause;
begin
  if FActive then
  begin
    FScrollSaved := FPixel;
    FTimerTag := 1;
  end;
end;

procedure TAntJvScrollText.SetActive(const Value: Boolean);
begin
  SetItems(FStrings);
  FActive := Value;
  if Value then
    FScroll.Resume
  else
    FScroll.Suspend;
end;

procedure TAntJvScrollText.SetDelay(const Value: Cardinal);
begin
  if Value > FDeja then
    Application.HintPause := FDeja
  else
  if Value > 10 then
    Application.HintPause := Value - 10
  else
    Application.HintPause := Abs(Value - 1);
  FDelay := Value;
  FScroll.Delay := Value;
end;

procedure TAntJvScrollText.SetScrollDirection(const Value: TDirection);
begin
  FScrollDirection := Value;
  FMemo.Left := 0;
  FMemo.Top := 0;
  Reset;
end;

procedure TAntJvScrollText.CalculateMemo(Sender: TObject);
var
  I, J: Integer;
  Ts: TStringList;
begin
  //calculate the Size of the memo (vertically)
  with TCanvas.Create do
  begin
    Handle := GetDC(HWND_DESKTOP);
    Font.Assign(FMemo.Font);
    J := 0;
    Ts := TStringList.Create;
    Ts.Text := FMemo.Caption;
    for I := 0 to Ts.Count - 1 do
    try
      if Ts[I] <> '' then
        J := J + TextHeight(Ts[I]) * ((TextWidth(Ts[I]) div Width) + 1)
      else
        J := J + TextHeight(RC_TestText);
    except
    end;
    if J <= 0 then
      J := Height;
    FMemo.Height := J;
    ReleaseDC(HWND_DESKTOP, Handle);
    Ts.Free;
    Free;
  end;
  if FMemo.Height < Height then
    FMemo.Height := Height;
  Reset;
end;

procedure TAntJvScrollText.SetItems(const Value: TStringList);
begin
  FStrings.Text := Value.Text;
  FMemo.Caption := Value.text;
  CalculateMemo(Self);
end;

function TAntJvScrollText.GetColor: TColor;
begin
  Result := FMemo.Color;
end;

procedure TAntJvScrollText.SetColor(const Value: TColor);
begin
  FMemo.Color := Value;
  Color := Value;
  Invalidate;
end;

procedure TAntJvScrollText.FontChanged(Sender: TObject);
var
  Al: TAlignment;
begin
  CalculateMemo(Self);
  Al := FMemo.Alignment;
  if FMemo.Alignment = taCenter then
    FMemo.Alignment := taLeftJustify
  else
    FMemo.Alignment := taCenter;
  FMemo.Alignment := Al;
end;

procedure TAntJvScrollText.SetPixel(const Value: Integer);
begin
  FPixel := Value;
end;

procedure TAntJvScrollText.Reset;
begin
  case ScrollDirection of
    drFromTop:
      FCurrPos := Height;
    drFromLeft:
      FCurrPos := -Width;
    drFromRight:
      FCurrPos := Width;
    drFrombottom:
      FCurrPos := -FMemo.Height;
  end;
end;

procedure TAntJvScrollText.Unpause;
begin
  if not FActive then
  begin
    FScrollSaved := 0;
    FTimerTag := 2;
    SetActive(True);
  end;
end;

procedure TAntJvScrollText.WMSize(var Msg: TWMSize);
begin
  FMemo.Width := Width;
  if FMemo.Height < Height then
    FMemo.Height := Height;
end;

function TAntJvScrollText.GetAlignment: TAlignment;
begin
  Result := FMemo.Alignment;
end;

procedure TAntJvScrollText.SetAlignment(const Value: TAlignment);
begin
  FMemo.Alignment := Value;
end;

procedure TAntJvScrollText.SetCursor(Value: TCursor);
begin
  inherited;
  FMemo.Cursor := Value;
end;

function TAntJvScrollText.GetCursor: TCursor;
begin
  Result := FMemo.Cursor;
end;

end.

