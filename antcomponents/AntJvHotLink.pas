{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvHotLink.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

unit AntJvHotLink;

interface

uses
  SysUtils, Classes, Graphics, Controls, AntJvLabel;

type
  TAntJvHotLink = class(TAntJvLabel)
  private
    FUrl: string;
  public
    procedure Click; override;
    constructor Create(AOwner: TComponent); override;
  published
    property Url: string read FUrl write FUrl;
  end;

implementation

uses
  Windows, ShellApi;

constructor TAntJvHotLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUrl := 'http://localhost/';
  Cursor := crHandPoint;
  HotTrack := True;
  HotTrackFont.Color := clBlue;
  HotTrackFont.Style := [fsUnderline];
end;

procedure TAntJvHotLink.Click;
begin
  inherited Click;
  ShellExecute(0, nil, PChar(Url), nil, nil, SW_SHOWNORMAL);
end;

end.

