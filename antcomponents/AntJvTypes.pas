{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTypes.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].
Peter Thornqvist
Oliver Giesen
Gustavo Bianconi

Last Modified: 2003-10-10

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I ant.inc}

unit AntJvTypes;

interface

uses
  Windows, Messages, Controls, Forms, Graphics;

type
  TDlgCode = (
    dcWantAllKeys, dcWantArrows, dcWantTab, dcWantChars,
    dcButton,
    dcNative // if dcNative is in the set the native functions are used and DoGetDlgCode is ignored
  );
  TDlgCodes = set of TDlgCode;

  TAntJvClipboardCommand = (caCopy, caCut, caPaste, caUndo);
  TAntJvClipboardCommands = set of TAntJvClipboardCommand;
  // Defines how a property (like a HotTrackFont) follows changes in the component's normal Font

  TAntJvTrackFontOption = (
    hoFollowFont,  // makes HotTrackFont follow changes to the normal Font
    hoPreserveCharSet,  // don't change HotTrackFont.Charset
    hoPreserveColor,    // don't change HotTrackFont.Color
    hoPreserveHeight,   // don't change HotTrackFont.Height (affects Size as well)
    hoPreserveName,     // don't change HotTrackFont.Name
    hoPreservePitch,    // don't change HotTrackFont.Pitch
    hoPreserveStyle);   // don't change HotTrackFont.Style
  TAntJvTrackFontOptions = set of TAntJvTrackFontOption;

  // used in JvSpeedButton, JvArrowButton, JvButton
  TAntJvCMButtonPressed = packed record
    Msg: Cardinal;
    Index: Integer;
    Control: TControl;
    Result: Longint;
  end;

const
  CM_ANTJVBASE = CM_BASE + 80;
  { Command message for JvSpeedbar editor }
  CM_SPEEDBARCHANGED = CM_ANTJVBASE + 0;
  { Command message for TJvSpeedButton }
  CM_ANTJVBUTTONPRESSED = CM_ANTJVBASE + 1;

const
  DefaultTrackFontOptions = [hoFollowFont, hoPreserveColor, hoPreserveStyle];

  { TBitmap.GetTransparentColor from GRAPHICS.PAS use this value }
  PaletteMask = $02000000;

implementation

end.
