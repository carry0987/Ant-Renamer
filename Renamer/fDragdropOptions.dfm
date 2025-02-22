inherited DragdropOptionsForm: TDragdropOptionsForm
  HelpContext = 1210
  BorderStyle = bsSingle
  Caption = 'Drag & Drop Options'
  ClientHeight = 173
  ClientWidth = 380
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited Bevel1: TBevel
    Top = 140
    Width = 374
  end
  inherited AntAutoHintLabel1: TAntAutoHintLabel
    Top = 157
    Width = 380
  end
  object lblOptions: TLabel [2]
    Left = 8
    Top = 8
    Width = 242
    Height = 13
    Caption = 'Options to use to add files that have been dropped:'
  end
  inline Options: TAddFoldersFrame [3]
    Left = 24
    Top = 32
    Width = 347
    Height = 65
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
    inherited cbxRecursive: TTntCheckBox
      Width = 347
    end
    inherited cbxFiles: TTntCheckBox
      Width = 347
    end
    inherited cbxFolders: TTntCheckBox
      Width = 347
    end
  end
  inherited btn1: TCorelButton [4]
    Left = 302
    Top = 145
    Caption = 'Help'
    Visible = True
    OnClick = btn1Click
  end
  inherited btn2: TCorelButton [5]
    Left = 224
    Top = 145
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    Visible = True
  end
  inherited btn3: TCorelButton [6]
    Left = 146
    Top = 145
    Caption = 'OK'
    Default = True
    Visible = True
    OnClick = btn3Click
  end
  inherited btn4: TCorelButton
    Left = 68
    Top = 145
  end
  object cbxNotAgain: TTntCheckBox
    Left = 8
    Top = 112
    Width = 363
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 
      'Do not show this window again (keep selected options for next ti' +
      'mes)'
    TabOrder = 5
  end
end
