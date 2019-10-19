inherited InputWin: TInputWin
  Left = 445
  Top = 438
  Caption = ''
  ClientHeight = 97
  ClientWidth = 334
  Constraints.MinHeight = 135
  Constraints.MinWidth = 300
  Font.Name = 'MS Shell Dlg 2'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited Bevel1: TBevel
    Top = 64
    Width = 328
  end
  object lblPrompt: TLabel [1]
    Left = 8
    Top = 6
    Width = 318
    Height = 28
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = False
    Caption = 'lblPrompt'
    Layout = tlCenter
    WordWrap = True
  end
  inherited AntAutoHintLabel1: TAntAutoHintLabel
    Top = 81
    Width = 334
  end
  inherited btn1: TCorelButton
    Left = 256
    Top = 69
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
    Visible = True
  end
  inherited btn2: TCorelButton
    Left = 178
    Top = 69
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
    Visible = True
  end
  inherited btn3: TCorelButton
    Left = 100
    Top = 69
    TabOrder = 2
  end
  inherited btn4: TCorelButton
    Left = 22
    Top = 69
    TabOrder = 1
  end
  object edtInput: TEdit
    Left = 8
    Top = 38
    Width = 318
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 0
    Text = 'edtInput'
  end
end
