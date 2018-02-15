object AddFoldersFrame: TAddFoldersFrame
  Left = 0
  Top = 0
  Width = 225
  Height = 65
  TabOrder = 0
  DesignSize = (
    225
    65)
  object cbxRecursive: TCheckBox
    Left = 0
    Top = 48
    Width = 225
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Apply these two options also to subfolders'
    TabOrder = 2
  end
  object cbxFiles: TCheckBox
    Left = 0
    Top = 0
    Width = 225
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Add files of selected folders'
    TabOrder = 0
    OnClick = ClickCheck
  end
  object cbxFolders: TCheckBox
    Left = 0
    Top = 24
    Width = 225
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Add selected folders'
    TabOrder = 1
    OnClick = ClickCheck
  end
end
