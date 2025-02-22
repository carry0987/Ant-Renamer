(*

(c) 2002-2003 Antoine Potten
software@antp.be
htt://www.antp.be/software/

Some components come from JVCL : http://jvcl.sourceforge.net

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

*)

unit AntRegister;

interface

uses
  Classes;

procedure Register;

implementation

uses
  AntCommon,
  AntAutoHintLabel,
  AntButtonEdit,
  AntSpinEdit,
  AntSplitter,
  AntStringList,
  AntJvLabel,
  AntJvHotLink,
  AntJvZoom,
  AntJvLinkLabel,
  AntJvTranslator,
  AntJvSpecialProgress,
  AntJvTrayIcon,
  AntJvScrollText,
  AntJvListView,
  AntJvGroupHeader,
  AntJvSpin,
  AntJvToolEdit,
  AntCorelButton;

{$R AntIcons.RES}

procedure Register;
begin
  RegisterComponents('Ant', [TAntAutoHintLabel]);
  RegisterComponents('Ant', [TAntButtonEdit]);
  RegisterComponents('Ant', [TAntSpinEdit]);
  RegisterComponents('Ant', [TAntSplitter]);
  RegisterComponents('Ant', [TAntStringList]);
  RegisterComponents('Ant', [TAntJvLabel]);
  RegisterComponents('Ant', [TAntJvHotLink]);
  RegisterComponents('Ant', [TAntJvZoom]);
  RegisterComponents('Ant', [TAntJvLinkLabel]);
  RegisterComponents('Ant', [TAntJvTranslatorStrings]);
  RegisterComponents('Ant', [TAntJvSpecialProgress]);
  RegisterComponents('Ant', [TAntJvTrayIcon]);
  RegisterComponents('Ant', [TAntJvScrollText]);
  RegisterComponents('Ant', [TAntJvListView]);
  RegisterComponents('Ant', [TAntJvGroupHeader]);
  RegisterComponents('Ant', [TAntJvSpinEdit]);
  RegisterComponents('Ant', [TAntJvComboEditXP]);
  RegisterComponents('Ant', [TCorelButton]);
end;

end.
