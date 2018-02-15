inherited OptionsForm: TOptionsForm
  Left = 424
  Top = 298
  HelpContext = 2000
  BorderStyle = bsSingle
  Caption = 'Options'
  ClientHeight = 400
  ClientWidth = 424
  Constraints.MinHeight = 420
  Constraints.MinWidth = 432
  OldCreateOrder = True
  OnActivate = FormActivate
  DesignSize = (
    424
    400)
  PixelsPerInch = 96
  TextHeight = 13
  inherited Bevel1: TBevel
    Top = 367
    Width = 418
  end
  inherited AntAutoHintLabel1: TAntAutoHintLabel
    Top = 384
    Width = 424
  end
  object PageControl1: TPageControl [2]
    Left = 3
    Top = 2
    Width = 418
    Height = 367
    ActivePage = tshDisplay
    Anchors = [akLeft, akTop, akRight, akBottom]
    HotTrack = True
    TabOrder = 4
    OnChange = PageControl1Change
    object tshDisplay: TTabSheet
      Caption = 'Display'
      DesignSize = (
        410
        339)
      object lblForceFont: TLabel
        Left = 36
        Top = 171
        Width = 301
        Height = 13
        Caption = 
          'Note: Arial Unicode MS is strongly recommended if it is availabl' +
          'e'
      end
      object lblDropdownMax: TLabel
        Left = 12
        Top = 216
        Width = 169
        Height = 13
        Caption = 'Number of values to keep in history:'
      end
      object lblIconSet: TLabel
        Left = 12
        Top = 288
        Width = 41
        Height = 13
        Caption = 'Icon set:'
      end
      object lbhLists: TAntJvGroupHeader
        Left = 4
        Top = 4
        Width = 400
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Lists'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        BevelSpace = 4
        Transparent = True
        Layout = lBottom
      end
      object lbhFonts: TAntJvGroupHeader
        Left = 4
        Top = 100
        Width = 400
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Fonts'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        BevelSpace = 4
        Transparent = True
        Layout = lBottom
      end
      object lbhDropdown: TAntJvGroupHeader
        Left = 4
        Top = 196
        Width = 400
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Drop-down fields'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        BevelSpace = 4
        Transparent = True
        Layout = lBottom
      end
      object lbhToolbars: TAntJvGroupHeader
        Left = 4
        Top = 268
        Width = 400
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Toolbars'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        BevelSpace = 4
        Transparent = True
        Layout = lBottom
      end
      object cbxRealTimeUpdate: TCheckBox
        Left = 12
        Top = 24
        Width = 393
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Real-time update when renaming files'
        TabOrder = 0
      end
      object cbxResizeColsFiles: TCheckBox
        Left = 12
        Top = 48
        Width = 394
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Auto-resize columns of Files list'
        TabOrder = 1
      end
      object cbxFilesIcons: TCheckBox
        Left = 12
        Top = 72
        Width = 394
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Show icons in files list, based on file extension'
        TabOrder = 2
      end
      object cbxForceFont: TCheckBox
        Left = 12
        Top = 120
        Width = 394
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 
          'Force the following font for controls that have to display speci' +
          'al characters:'
        TabOrder = 3
        OnClick = cbxForceFontClick
      end
      object cmbForceFont: TComboBox
        Left = 32
        Top = 144
        Width = 373
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        DropDownCount = 32
        ItemHeight = 13
        TabOrder = 4
        Items.Strings = (
          'Default Windows font')
      end
      object cmbIconSet: TComboBox
        Left = 91
        Top = 284
        Width = 314
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 7
        Items.Strings = (
          'Scrows')
      end
      object cbxDropdownComplete: TCheckBox
        Left = 12
        Top = 237
        Width = 394
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Autocomplete with history values while typing'
        TabOrder = 6
      end
      object edtDropdownMax: TAntJvSpinEdit
        Left = 189
        Top = 212
        Width = 41
        Height = 21
        MaxValue = 99.000000000000000000
        Value = 20.000000000000000000
        TabOrder = 5
      end
    end
    object tshAdding: TTabSheet
      Caption = 'Files && folders'
      ImageIndex = 1
      DesignSize = (
        410
        339)
      object lbhDragdrop: TAntJvGroupHeader
        Left = 4
        Top = 4
        Width = 400
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Drag & Drop'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        BevelSpace = 4
        Transparent = True
        Layout = lBottom
      end
      object lbhBehaviour: TAntJvGroupHeader
        Left = 4
        Top = 124
        Width = 400
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = 'List behaviour'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        BevelSpace = 4
        Transparent = True
        Layout = lBottom
      end
      object cbxDragdropNoAsk: TCheckBox
        Left = 12
        Top = 24
        Width = 393
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Do not ask what to do, always use the following options:'
        TabOrder = 0
        OnClick = cbxDragdropNoAskClick
      end
      inline DragdropOptions: TAddFoldersFrame
        Left = 32
        Top = 48
        Width = 369
        Height = 65
        TabOrder = 1
        DesignSize = (
          369
          65)
        inherited cbxRecursive: TCheckBox
          Width = 369
        end
        inherited cbxFiles: TCheckBox
          Width = 369
        end
        inherited cbxFolders: TCheckBox
          Width = 369
        end
      end
      object cbxLaunchFile: TCheckBox
        Left = 12
        Top = 144
        Width = 393
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Launch file on double click'
        TabOrder = 2
      end
    end
    object tshProcessing: TTabSheet
      Caption = 'Processing'
      ImageIndex = 2
      DesignSize = (
        410
        339)
      object lbhRenaming: TAntJvGroupHeader
        Left = 4
        Top = 4
        Width = 400
        Height = 13
        Caption = 'File renaming'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        BevelSpace = 4
        Transparent = True
        Layout = lBottom
      end
      object lbhFoldersRules: TAntJvGroupHeader
        Left = 4
        Top = 76
        Width = 400
        Height = 13
        Caption = 'Folders rules'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        BevelSpace = 4
        Transparent = True
        Layout = lBottom
      end
      object cbxForceDir: TCheckBox
        Left = 12
        Top = 120
        Width = 393
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 
          'Force directories (allows to create folders if "\" are found in ' +
          'names)'
        TabOrder = 3
      end
      object cbxDetectAbsdir: TCheckBox
        Left = 12
        Top = 96
        Width = 393
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 
          'Detect a ":\" or "\\" as an absolute path (allows to move files ' +
          'to anywhere)'
        TabOrder = 2
      end
      object cbxCopy: TCheckBox
        Left = 12
        Top = 24
        Width = 393
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Copy instead of Rename'
        TabOrder = 0
      end
      object cbxGenerLog: TCheckBox
        Left = 12
        Top = 48
        Width = 393
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Generate log'
        TabOrder = 1
      end
      object cbxFolderExt: TCheckBox
        Left = 12
        Top = 144
        Width = 393
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 
          'Consider the last "." in a folder name as extension delimiter li' +
          'ke for files'
        TabOrder = 4
      end
    end
    object tshEvents: TTabSheet
      Caption = 'Events'
      ImageIndex = 4
      DesignSize = (
        410
        339)
      object lbhWhenStarted: TAntJvGroupHeader
        Left = 4
        Top = 4
        Width = 400
        Height = 13
        Caption = 'When renaming starts'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        BevelSpace = 4
        Transparent = True
        Layout = lBottom
      end
      object lbhWhenFinished: TAntJvGroupHeader
        Left = 4
        Top = 76
        Width = 400
        Height = 13
        Caption = 'When renaming ends'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        BevelSpace = 4
        Transparent = True
        Layout = lBottom
      end
      object cbxStartSwitch: TCheckBox
        Left = 12
        Top = 24
        Width = 129
        Height = 17
        Caption = 'Switch to:'
        TabOrder = 0
        OnClick = cbxStartSwitchClick
      end
      object cmbStartSwitch: TComboBox
        Left = 144
        Top = 22
        Width = 261
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 1
        Items.Strings = (
          'Files list'
          'Actions'
          'Log')
      end
      object cbxStartClearLog: TCheckBox
        Left = 12
        Top = 48
        Width = 393
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Clear log'
        TabOrder = 2
      end
      object cbxFinishSwitch: TCheckBox
        Left = 12
        Top = 96
        Width = 129
        Height = 17
        Caption = 'Switch to:'
        TabOrder = 3
        OnClick = cbxFinishSwitchClick
      end
      object cbxClearFilesList: TCheckBox
        Left = 12
        Top = 120
        Width = 393
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Clear files list'
        TabOrder = 4
      end
      object cbxClearBatchList: TCheckBox
        Left = 12
        Top = 144
        Width = 393
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Clear batch list'
        TabOrder = 5
      end
      object cbxSaveLog: TCheckBox
        Left = 12
        Top = 168
        Width = 129
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Save log to file:'
        TabOrder = 6
        OnClick = cbxSaveLogClick
      end
      object cbxSaveLogAppend: TCheckBox
        Left = 144
        Top = 192
        Width = 261
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Append to current file'
        TabOrder = 7
      end
      object edtSaveLog: TTntEdit
        Left = 144
        Top = 166
        Width = 237
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 8
      end
      object btnSaveLog: TCorelButton
        Left = 384
        Top = 166
        Width = 21
        Height = 21
        Hint = 'Browse...'
        Caption = '...'
        TabOrder = 9
        OnClick = btnSaveLogClick
      end
      object cmbFinishSwitch: TComboBox
        Left = 144
        Top = 94
        Width = 261
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 10
        Items.Strings = (
          'Files list'
          'Actions'
          'Log')
      end
    end
    object tshLanguage: TTabSheet
      Caption = 'Language'
      ImageIndex = 3
      DesignSize = (
        410
        339)
      inline LanguageFrame: TLanguageFrame
        Left = 4
        Top = 4
        Width = 402
        Height = 331
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 0
        inherited PanelForXpThemeBug: TPanel
          Width = 402
          Height = 331
          inherited lblVersionText: TLabel
            Top = 202
          end
          inherited lblVersion: TLabel
            Top = 202
          end
          inherited lblMadeby: TLabel
            Top = 226
          end
          inherited lblComments: TLabel
            Top = 274
          end
          inherited lblMadebyText: TAntJvLinkLabel
            Top = 226
            Width = 313
          end
          inherited lblCommentsText: TAntJvLinkLabel
            Top = 274
          end
          inherited lstLanguages: TListView
            OnDblClick = btn3Click
          end
        end
      end
    end
  end
  inherited btn1: TCorelButton
    Left = 346
    Top = 372
    Caption = 'Help'
    Visible = True
    OnClick = btn1Click
  end
  inherited btn2: TCorelButton
    Left = 268
    Top = 372
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    Visible = True
  end
  inherited btn3: TCorelButton
    Left = 190
    Top = 372
    Caption = 'OK'
    Default = True
    Visible = True
    OnClick = btn3Click
  end
  inherited btn4: TCorelButton
    Left = 112
    Top = 372
  end
end
