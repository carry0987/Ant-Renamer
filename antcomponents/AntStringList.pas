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

unit AntStringList;

interface

uses
  Classes;

type

  TAntStringList = class(TComponent)
  private
    FStrings: TStrings;
    procedure SetStrings(Value: TStrings);
  protected
  public
    constructor Create(Owner: TComponent); override;
    destructor  Destroy; override;
  published
    property Strings: TStrings read FStrings write SetStrings;
  end;

implementation

constructor TAntStringList.Create(Owner: TComponent);
begin
  inherited;
  FStrings := TStringList.Create;
end;

destructor TAntStringList.Destroy;
begin
  FStrings.Free;
  inherited;
end;

procedure TAntStringList.SetStrings(Value: TStrings);
begin
  FStrings.Assign(Value);
end;

end.
