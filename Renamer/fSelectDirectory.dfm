inherited SelectDirectoryForm: TSelectDirectoryForm
  Left = 391
  Top = 257
  HelpContext = 1210
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Select folders to add'
  ClientHeight = 416
  ClientWidth = 315
  Constraints.MinHeight = 400
  Constraints.MinWidth = 323
  OldCreateOrder = True
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  inherited Bevel1: TBevel
    Top = 383
    Width = 309
  end
  inherited AntAutoHintLabel1: TAntAutoHintLabel
    Top = 400
    Width = 315
  end
  object Tree: TVirtualStringTree [2]
    Left = 4
    Top = 4
    Width = 307
    Height = 299
    Anchors = [akLeft, akTop, akRight, akBottom]
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Shell Dlg 2'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag]
    Images = imglstIcons
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
    TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.SelectionOptions = [toMultiSelect]
    OnChange = TreeChange
    OnExpanding = TreeExpanding
    OnGetText = TreeGetText
    OnGetImageIndex = TreeGetImageIndex
    Columns = <>
  end
  inherited btn1: TCorelButton
    Left = 237
    Top = 388
    Caption = 'Help'
    TabOrder = 5
    Visible = True
    OnClick = btn1Click
  end
  inherited btn2: TCorelButton
    Left = 159
    Top = 388
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
    Visible = True
  end
  inherited btn3: TCorelButton
    Left = 81
    Top = 388
    Caption = 'OK'
    Default = True
    Enabled = False
    TabOrder = 3
    Visible = True
    OnClick = btn3Click
  end
  inline Options: TAddFoldersFrame [6]
    Left = 4
    Top = 309
    Width = 306
    Height = 67
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 1
    inherited cbxRecursive: TCheckBox
      Width = 306
    end
    inherited cbxFiles: TCheckBox
      Width = 306
    end
    inherited cbxFolders: TCheckBox
      Width = 306
    end
  end
  inherited btn4: TCorelButton
    Left = 3
    Top = 388
    Caption = 'Network...'
    TabOrder = 2
    Visible = True
    OnClick = btn4Click
  end
  object imglstIcons: TImageList
    Left = 232
    Top = 320
  end
end
