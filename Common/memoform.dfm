inherited MemoWin: TMemoWin
  Left = 505
  Top = 352
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'MemoWin'
  ClientHeight = 221
  ClientWidth = 392
  Constraints.MinHeight = 172
  Constraints.MinWidth = 381
  PixelsPerInch = 96
  TextHeight = 13
  inherited Bevel1: TBevel
    Top = 188
    Width = 386
  end
  inherited AntAutoHintLabel1: TAntAutoHintLabel
    Top = 205
    Width = 392
  end
  inherited btn1: TCorelButton
    Left = 314
    Top = 193
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
    Visible = True
  end
  inherited btn2: TCorelButton
    Left = 236
    Top = 193
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
    Visible = True
  end
  inherited btn3: TCorelButton
    Left = 158
    Top = 193
    TabOrder = 2
  end
  inherited btn4: TCorelButton
    Left = 80
    Top = 193
    TabOrder = 1
  end
  object edtMemo: TMemo
    Left = 3
    Top = 3
    Width = 386
    Height = 182
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'edtMemo')
    ScrollBars = ssBoth
    TabOrder = 0
  end
end
