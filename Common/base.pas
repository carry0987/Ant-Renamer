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

unit base;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, ComCtrls,

  AntAutoHintLabel, AntCorelButton;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

type
  TBaseDlg = class(TForm)
    Bevel1: TBevel;
    btn1: TCorelButton;
    btn2: TCorelButton;
    btn3: TCorelButton;
    AntAutoHintLabel1: TAntAutoHintLabel;
    btn4: TCorelButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  protected
    procedure LoadOptions; virtual;
    procedure SaveOptions; virtual;
    procedure LaunchHelp;
  public
    procedure Translate; virtual;
  end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

implementation

{$R *.dfm}

uses
{$IFDEF ANTTRANSLATOR}
  Global,
{$ENDIF}
  functions_files;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TBaseDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveOptions;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TBaseDlg.FormCreate(Sender: TObject);
begin
  inherited;
  Translate;
  LoadOptions;
  if WindowState = wsNormal then
  begin
    Position := poOwnerFormCenter;
  end;
  Font.Name := Graphics.DefFontData.Name;
  Font.Charset := Graphics.DefFontData.Charset;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TBaseDlg.LoadOptions;
begin
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TBaseDlg.SaveOptions;
begin
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TBaseDlg.Translate;
begin
  {$IFDEF ANTTRANSLATOR}
  Translator.Translate(Self);
  Font.Name := Graphics.DefFontData.Name;
  Font.Charset := Graphics.DefFontData.Charset;
  {$ENDIF}
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TBaseDlg.FormShow(Sender: TObject);
begin
  //
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TBaseDlg.LaunchHelp;
begin
  functions_files.LaunchHelp(Self);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

end.
