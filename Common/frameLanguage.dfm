object LanguageFrame: TLanguageFrame
  Left = 0
  Top = 0
  Width = 387
  Height = 344
  TabOrder = 0
  object PanelForXpThemeBug: TPanel
    Left = 0
    Top = 0
    Width = 387
    Height = 344
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      387
      344)
    object lblVersionText: TLabel
      Left = 88
      Top = 224
      Width = 34
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'version'
      Transparent = True
    end
    object lblVersion: TLabel
      Left = 0
      Top = 224
      Width = 38
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Version:'
      Transparent = True
    end
    object lblMadeby: TLabel
      Left = 0
      Top = 248
      Width = 44
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Made by:'
      Transparent = True
    end
    object lblComments: TLabel
      Left = 0
      Top = 296
      Width = 52
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Comments:'
      Transparent = True
    end
    object lblMadebyText: TAntJvLinkLabel
      Left = 88
      Top = 248
      Width = 299
      Height = 47
      Caption = 'madeby'
      Text.Strings = (
        'madeby')
      Anchors = [akLeft, akRight, akBottom]
      Transparent = True
      LinkColor = clBlue
      LinkColorClicked = clPurple
      LinkColorHot = clRed
      LinkStyle = [fsUnderline]
      HotLinks = True
      AutoHeight = False
      MarginWidth = 0
      MarginHeight = 0
      OnLinkClick = lblMadebyTextLinkClick
    end
    object lblCommentsText: TAntJvLinkLabel
      Left = 88
      Top = 296
      Width = 299
      Height = 47
      Caption = 'comment'
      Text.Strings = (
        'comment')
      Anchors = [akLeft, akRight, akBottom]
      Transparent = True
      LinkColor = clBlue
      LinkColorClicked = clPurple
      LinkColorHot = clRed
      LinkStyle = [fsUnderline]
      HotLinks = True
      AutoHeight = False
      MarginWidth = 0
      MarginHeight = 0
      OnLinkClick = lblCommentsTextLinkClick
    end
    object lstLanguages: TListView
      Left = 0
      Top = 0
      Width = 387
      Height = 217
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Caption = 'Name'
          Width = 180
        end
        item
          Caption = 'Local name'
          Width = 180
        end>
      ColumnClick = False
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      SmallImages = imgLanguages
      SortType = stText
      TabOrder = 0
      ViewStyle = vsReport
      OnCustomDrawSubItem = lstLanguagesCustomDrawSubItem
      OnSelectItem = lstLanguagesSelectItem
    end
  end
  object imgLanguages: TImageList
    Left = 24
    Top = 40
  end
end
