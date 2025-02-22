(************************************************************************
 *                                                                      *
 *   Ant Renamer 2.x                                                    *
 *   (C) 2003-2024 Antoine Potten                                       *
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

unit fAbout;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls,

  AntAutoHintLabel, AntJvLinkLabel, AntCorelButton,

  base;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

type
  TAboutForm = class(TBaseDlg)
    Panel1: TPanel;
    Image1: TImage;
    Label7: TLabel;
    lblVersion: TLabel;
    Panel2: TPanel;
    PageControl1: TPageControl;
    tshGeneral: TTabSheet;
    tshCredits: TTabSheet;
    lblGeneral: TAntJvLinkLabel;
    lblCredits: TAntJvLinkLabel;
    procedure lblGeneralLinkClick(Sender: TObject; LinkNumber: Integer;
      LinkText: String);
    procedure FormShow(Sender: TObject);
    procedure lblCreditsClick(Sender: TObject; LinkNumber: Integer;
      LinkText: String);
    procedure FormCreate(Sender: TObject);
    procedure btn1Click(Sender: TObject);
  private
  public
  end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

//var
//  AboutForm: TAboutForm;

implementation

uses
  ShellAPI,

  ConstValues, functions_files;

{$R *.dfm}

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

procedure TAboutForm.FormShow(Sender: TObject);
begin
  inherited;
  PageControl1.ActivePageIndex := 0;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAboutForm.lblGeneralLinkClick(Sender: TObject; LinkNumber: Integer; LinkText: String);
begin
  case LinkNumber of
    0:  PageControl1.ActivePageIndex := 1;
    1:  ShellExecute(0, nil, PChar('mailto:antrenamer-contact@antp.be'), nil, nil, SW_SHOWNORMAL);
    2:  ShellExecute(0, nil, PChar('http://www.antp.be/software/renamer'), nil, nil, SW_SHOWNORMAL);
    3:  ShellExecute(0, nil, PChar('http://www.gnu.org/licenses/gpl.txt'), nil, nil, SW_SHOWNORMAL);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAboutForm.lblCreditsClick(Sender: TObject; LinkNumber: Integer; LinkText: String);
begin
  ShellExecute(0, nil, PChar('http://' + LinkText), nil, nil, SW_SHOWNORMAL);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  inherited;
  lblVersion.Caption := Format('Version %s (%s)', [strVersion, strDate]);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TAboutForm.btn1Click(Sender: TObject);
begin
  functions_files.LaunchHelp(9100);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

end.

