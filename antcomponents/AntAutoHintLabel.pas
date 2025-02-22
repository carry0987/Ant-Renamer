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

unit AntAutoHintLabel;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

type
  TAntAutoHintLabel = class(TCustomLabel)
  private
  protected
  public
    function ExecuteAction(Action: TBasicAction): Boolean; override;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Color nodefault;
    property Constraints;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property PopupMenu;
    property Transparent;
    property Layout;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
  end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

implementation

uses
  StdActns;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TAntAutoHintLabel.ExecuteAction(Action: TBasicAction): Boolean;
begin
  if (Enabled) and (Visible) and (Action is THintAction) then
  begin
    Caption := THintAction(Action).Hint;
    Result := True;
  end
  else
    Result := inherited ExecuteAction(Action);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

end.
