object MainForm: TMainForm
  Left = 422
  Top = 207
  HelpContext = 1200
  AutoScroll = False
  Caption = 'MainForm'
  ClientHeight = 547
  ClientWidth = 708
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object tbdMain: TSpTBXDock
    Left = 0
    Top = 0
    Width = 708
    Height = 46
    FixAlign = True
    object tbProgram: TSpTBXToolbar
      Left = 127
      Top = 0
      CloseButton = False
      DefaultDock = tbdMain
      DockPos = -2
      HideWhenInactive = False
      Images = imglstHot
      TabOrder = 1
      Caption = 'Program'
      object tbbGo: TSpTBXItem
        Action = ActionActionGo
        DisplayMode = nbdmImageAndText
      end
      object tbbStop: TSpTBXItem
        Action = ActionActionStop
        DisplayMode = nbdmImageAndText
      end
      object tbbUndo: TSpTBXItem
        Action = ActionActionUndo
      end
      object tbsProgram: TSpTBXSeparatorItem
      end
      object tbbAlwaysOnTop: TSpTBXItem
        Action = ActionAlwaysOnTop
      end
      object tbbOptions: TSpTBXItem
        Action = ActionOptions
      end
      object tbsHelp: TSpTBXSeparatorItem
      end
      object tbbHelp: TSpTBXItem
        Action = ActionHelpContents
      end
      object tbbAbout: TSpTBXItem
        Action = ActionHelpAbout
      end
      object tbsExit: TSpTBXSeparatorItem
      end
      object tbbExit: TSpTBXItem
        Action = ActionExit
      end
    end
    object tbTabs: TSpTBXToolbar
      Left = 0
      Top = 0
      CloseButton = False
      DefaultDock = tbdMain
      DockPos = -3
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg'
      Font.Style = []
      HideWhenInactive = False
      Images = imglstHot
      ParentFont = False
      TabOrder = 0
      Caption = 'Pages'
      object tbbTabFiles: TSpTBXItem
        Action = ActionTabFiles
        DisplayMode = nbdmImageAndText
        GroupIndex = 1
        FontSettings.Style = [fsBold]
      end
      object tbbTabActions: TSpTBXItem
        Action = ActionTabActions
        DisplayMode = nbdmImageAndText
        GroupIndex = 1
        FontSettings.Style = [fsBold]
      end
      object tbbLog: TSpTBXItem
        Action = ActionTabLog
        DisplayMode = nbdmImageAndText
        GroupIndex = 1
        FontSettings.Style = [fsBold]
      end
    end
    object tbFiles: TSpTBXToolbar
      Left = 0
      Top = 23
      CloseButton = False
      DefaultDock = tbdMain
      DockRow = 1
      HideWhenInactive = False
      Images = imglstHot
      TabOrder = 2
      Caption = 'Files'
      object tbbFilesAdd: TSpTBXItem
        Action = ActionAddFiles
        DisplayMode = nbdmImageAndText
      end
      object tbbFilesFolder: TSpTBXItem
        Action = ActionAddFolders
        DisplayMode = nbdmImageAndText
      end
      object tbsFilesRemove: TSpTBXSeparatorItem
      end
      object tbbFilesRemoveSel: TSpTBXItem
        Action = ActionRemoveSelected
      end
      object tbbFilesRemoveAll: TSpTBXItem
        Action = ActionRemoveAll
        DisplayMode = nbdmImageAndText
      end
      object tbbFilesRemoveDead: TSpTBXItem
        Action = ActionRemoveDead
      end
      object tbsFilesPreview: TSpTBXSeparatorItem
      end
      object tbbFilesPreview: TSpTBXItem
        Action = ActionPreview
      end
    end
    object tbActions: TSpTBXToolbar
      Left = 235
      Top = 23
      CloseButton = False
      DefaultDock = tbdMain
      DockPos = 2
      DockRow = 1
      HideWhenInactive = False
      Images = imglstHot
      TabOrder = 4
      Caption = 'Actions'
      object tbbAddToBatch: TSpTBXItem
        Action = ActionBatchAddAction
      end
      object tbbBatchRemoveSel: TSpTBXItem
        Action = ActionBatchRemoveSel
      end
      object tbsBatchFile: TSpTBXSeparatorItem
      end
      object tbbBatchRemoveAll: TSpTBXItem
        Action = ActionBatchClear
      end
      object tbbBatchOpen: TSpTBXSubmenuItem
        Action = ActionBatchOpen
        DropdownCombo = True
        OnPopup = tbbBatchOpenPopup
        object tbbBatchNoRecent: TSpTBXItem
          Action = ActionBatchNoRecent
        end
        object mruBatch: TSpTBXMRUListItem
          MaxItems = 5
          OnClick = mruBatchClick
        end
      end
      object tbbBatchSave: TSpTBXItem
        Action = ActionBatchSaveAs
      end
    end
    object tbLog: TSpTBXToolbar
      Left = 534
      Top = 23
      CloseButton = False
      DefaultDock = tbdMain
      DockPos = 3
      DockRow = 1
      HideWhenInactive = False
      Images = imglstHot
      TabOrder = 5
      Caption = 'Log'
      object tbbLogFilter: TSpTBXSubmenuItem
        Action = ActionLogFilter
        Options = [tboDropdownArrow]
        object tbbLogFilterOk: TSpTBXItem
          Action = ActionLogFilterOk
        end
        object tbsLogFilterError: TSpTBXItem
          Action = ActionLogFilterError
        end
        object tbsLogFilterNot: TSpTBXItem
          Action = ActionLogFilterNot
        end
      end
      object tbsLogFilter: TSpTBXSeparatorItem
      end
      object tbbLogClear: TSpTBXItem
        Action = ActionLogClear
      end
      object tbbLogSave: TSpTBXItem
        Action = ActionLogSave
      end
      object tbbLogCopy: TSpTBXItem
        Action = ActionLogCopy
      end
    end
    object tbMove: TSpTBXToolbar
      Left = 93
      Top = 23
      CloseButton = False
      DefaultDock = tbdMain
      DockPos = 1
      DockRow = 1
      HideWhenInactive = False
      Images = imglstHot
      TabOrder = 3
      Caption = 'Move Items'
      object tbbMoveUp: TSpTBXItem
        Action = ActionMoveUp
      end
      object tbbMoveDown: TSpTBXItem
        Action = ActionMoveDown
      end
      object tbbMoveTop: TSpTBXItem
        Action = ActionMoveTop
      end
      object tbbMoveBottom: TSpTBXItem
        Action = ActionMoveBottom
      end
    end
  end
  object pgctrlMain: TPageControl
    Left = 0
    Top = 46
    Width = 708
    Height = 472
    ActivePage = tshFiles
    Align = alClient
    Style = tsButtons
    TabOrder = 1
    TabStop = False
    object tshFiles: TTabSheet
      Caption = 'Files'
      object lstFiles: TVirtualStringTree
        Left = 0
        Top = 0
        Width = 700
        Height = 441
        Hint = 
          '|Files list - Click with right mouse button on header to customi' +
          'ze columns'
        Align = alClient
        DragMode = dmAutomatic
        DragType = dtVCL
        DrawSelectionMode = smBlendedRectangle
        Header.AutoSizeIndex = -1
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'MS Sans Serif'
        Header.Font.Style = []
        Header.Images = imglstFiles
        Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoHotTrack, hoShowSortGlyphs, hoVisible]
        Header.PopupMenu = pmhFiles
        HintAnimation = hatNone
        PopupMenu = pmFiles
        TabOrder = 0
        TreeOptions.AutoOptions = [toAutoScroll, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes]
        TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
        TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
        TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect, toRightClickSelect]
        OnBeforeCellPaint = lstFilesBeforeCellPaint
        OnChange = lstFilesChange
        OnCompareNodes = lstFilesCompareNodes
        OnDblClick = lstFilesDblClick
        OnDragOver = lstFilesDragOver
        OnDragDrop = lstFilesDragDrop
        OnGetText = lstFilesGetText
        OnGetImageIndex = lstFilesGetImageIndex
        OnHeaderClick = lstFilesHeaderClick
        OnKeyDown = lstFilesKeyDown
        OnKeyUp = lstFilesKeyUp
        Columns = <
          item
            Position = 0
            Width = 200
            WideText = 'Folder'
          end
          item
            Position = 1
            Width = 400
            WideText = 'Filename'
          end
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
            Position = 2
            Width = 400
            WideText = 'Full path'
          end
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
            Position = 3
            WideText = 'Ext.'
          end
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
            Position = 4
            Width = 100
            WideText = 'Preview'
          end
          item
            Alignment = taRightJustify
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
            Position = 5
            Width = 70
            WideText = 'Size'
          end
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
            Position = 6
            Width = 130
            WideText = 'Date created'
          end
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
            Position = 7
            Width = 130
            WideText = 'Date modified'
          end>
      end
    end
    object tshAction: TTabSheet
      Caption = 'Action'
      ImageIndex = 1
      object Splitter1: TSplitter
        Left = 150
        Top = 0
        Height = 268
        AutoSnap = False
        ResizeStyle = rsUpdate
      end
      object splBatch: TSplitter
        Left = 0
        Top = 333
        Width = 700
        Height = 3
        Cursor = crVSplit
        Align = alBottom
        AutoSnap = False
        MinSize = 20
        ResizeStyle = rsUpdate
        OnCanResize = splBatchCanResize
      end
      object Panel1: TPanel
        Left = 153
        Top = 0
        Width = 547
        Height = 268
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object pgctrlAction: TPageControl
          Left = 0
          Top = 0
          Width = 547
          Height = 268
          ActivePage = tshEmpty
          Align = alClient
          RaggedRight = True
          Style = tsButtons
          TabOrder = 0
          TabStop = False
          object tshEmpty: TTabSheet
            Caption = 'tshEmpty'
          end
          object tshChangeExt: TTabSheet
            Caption = 'tshChangeExt'
            ImageIndex = 1
            DesignSize = (
              539
              237)
            object lblChangeExt: TLabel
              Left = 8
              Top = 8
              Width = 105
              Height = 13
              Caption = 'Replace extension by:'
            end
            object lblChangeExtNotes: TAntJvLinkLabel
              Left = 8
              Top = 88
              Width = 527
              Height = 39
              Caption = 
                'Notes:<br>'#13#10'- Do not include the dot, just enter the new extensi' +
                'on<br>'#13#10'- If you want to make an incremented extension, use the ' +
                '<LINK>Enumeration</LINK> action'#13#10
              Text.Strings = (
                
                  'Notes:<br>'#13#10'- Do not include the dot, just enter the new extensi' +
                  'on<br>'#13#10'- If you want to make an incremented extension, use the ' +
                  '<LINK>Enumeration</LINK> action'#13#10)
              Anchors = [akLeft, akTop, akRight]
              Transparent = False
              LinkColor = clBlue
              LinkColorClicked = clRed
              LinkColorHot = clPurple
              LinkStyle = [fsUnderline]
              HotLinks = False
              AutoHeight = True
              MarginWidth = 0
              MarginHeight = 0
              OnLinkClick = lblChangeExtNotesLinkClick
            end
            object edtChangeExt: TTntComboBox
              Left = 32
              Top = 32
              Width = 137
              Height = 21
              AutoComplete = False
              DropDownCount = 20
              ItemHeight = 13
              TabOrder = 0
              OnChange = UpdateActionPreview
            end
            object cbxChangeExtNoRepl: TTntCheckBox
              Left = 8
              Top = 64
              Width = 523
              Height = 17
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Do not replace extension, append it to current full name'
              TabOrder = 1
              OnClick = UpdateActionPreview
            end
          end
          object tshStringRepl: TTabSheet
            Caption = 'tshStringRepl'
            ImageIndex = 2
            DesignSize = (
              539
              237)
            object lblStrReplSearch: TLabel
              Left = 8
              Top = 8
              Width = 52
              Height = 13
              Caption = 'Search for:'
            end
            object lblStrReplBy: TLabel
              Left = 8
              Top = 64
              Width = 57
              Height = 13
              Caption = 'Replace by:'
            end
            object cbxStrReplAll: TTntCheckBox
              Left = 8
              Top = 128
              Width = 523
              Height = 17
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Replace all occurences'
              TabOrder = 2
              OnClick = UpdateActionPreview
            end
            object cbxStrReplCase: TTntCheckBox
              Left = 8
              Top = 152
              Width = 523
              Height = 17
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Case sensitive'
              TabOrder = 3
              OnClick = UpdateActionPreview
            end
            object cbxStrReplExt: TTntCheckBox
              Left = 8
              Top = 176
              Width = 523
              Height = 17
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Include extension in search'
              TabOrder = 4
              OnClick = UpdateActionPreview
            end
            object edtStrReplSearch: TTntComboBox
              Left = 32
              Top = 32
              Width = 499
              Height = 21
              AutoComplete = False
              Anchors = [akLeft, akTop, akRight]
              DropDownCount = 20
              ItemHeight = 13
              TabOrder = 0
              OnChange = UpdateActionPreview
            end
            object edtStrReplBy: TTntComboBox
              Left = 32
              Top = 88
              Width = 499
              Height = 21
              AutoComplete = False
              Anchors = [akLeft, akTop, akRight]
              DropDownCount = 20
              ItemHeight = 13
              TabOrder = 1
              OnChange = UpdateActionPreview
            end
            object cbxStrReplOnlyExt: TTntCheckBox
              Left = 8
              Top = 200
              Width = 523
              Height = 17
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Apply to extension instead of name'
              TabOrder = 5
              OnClick = UpdateActionPreview
            end
          end
          object tshMultstrRepl: TTabSheet
            Caption = 'tshMultstrRepl'
            ImageIndex = 12
            DesignSize = (
              539
              237)
            object lblMultstrSearch: TLabel
              Left = 8
              Top = 8
              Width = 52
              Height = 13
              Caption = 'Search for:'
            end
            object lblMultstrReplBy: TLabel
              Left = 151
              Top = 8
              Width = 57
              Height = 13
              Caption = 'Replace by:'
            end
            object lblMultstrReplSet: TLabel
              Left = 328
              Top = 8
              Width = 76
              Height = 13
              Caption = 'Predefined sets:'
            end
            object lblMultstrReplNotes: TAntJvLinkLabel
              Left = 328
              Top = 131
              Width = 203
              Height = 39
              Caption = 
                'Notes:<BR>'#13#10'- Use Ctrl+Plus (+) and Ctrl+Minus (-) to insert or ' +
                'delete a line at current position'#13#10
              Text.Strings = (
                
                  'Notes:<BR>'#13#10'- Use Ctrl+Plus (+) and Ctrl+Minus (-) to insert or ' +
                  'delete a line at current position'#13#10)
              Anchors = [akLeft, akTop, akRight]
              Transparent = False
              LinkColor = clBlue
              LinkColorClicked = clRed
              LinkColorHot = clPurple
              LinkStyle = [fsUnderline]
              HotLinks = False
              AutoHeight = True
              MarginWidth = 0
              MarginHeight = 0
            end
            object edtMultstrRepl: TTntStringGrid
              Left = 8
              Top = 24
              Width = 313
              Height = 209
              Anchors = [akLeft, akTop, akBottom]
              ColCount = 2
              DefaultColWidth = 140
              DefaultRowHeight = 21
              FixedCols = 0
              RowCount = 1
              FixedRows = 0
              Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goEditing, goThumbTracking]
              ScrollBars = ssVertical
              TabOrder = 0
              OnKeyUp = edtMultstrReplKeyUp
              OnSetEditText = edtMultstrReplSetEditText
            end
            object edtMultstrReplSet: TTntComboBox
              Left = 328
              Top = 24
              Width = 203
              Height = 21
              AutoComplete = False
              Anchors = [akLeft, akTop, akRight]
              ItemHeight = 13
              Sorted = True
              TabOrder = 1
              OnClick = edtMultstrReplSetClick
            end
            object btnMultstrReplSave: TCorelButton
              Left = 328
              Top = 50
              Width = 97
              Height = 21
              Action = ActionMultistrReplSave
              TabOrder = 2
            end
            object btnMultstrReplDelete: TCorelButton
              Left = 432
              Top = 50
              Width = 97
              Height = 21
              Action = ActionMultistrReplDelete
              TabOrder = 3
            end
            object btnMultstrReplClear: TCorelButton
              Left = 328
              Top = 209
              Width = 97
              Height = 21
              Action = ActionMultistrReplClear
              Anchors = [akLeft, akBottom]
              TabOrder = 8
            end
            object cbxMultstrReplExt: TTntCheckBox
              Left = 328
              Top = 80
              Width = 203
              Height = 17
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Include extension in search'
              TabOrder = 4
              OnClick = UpdateActionPreview
            end
            object cbxMultstrCase: TTntCheckBox
              Left = 328
              Top = 104
              Width = 203
              Height = 17
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Case sensitive'
              TabOrder = 5
              OnClick = UpdateActionPreview
            end
            object btnMultstrReplPaste: TCorelButton
              Left = 328
              Top = 181
              Width = 97
              Height = 21
              Action = ActionMultistrReplPaste
              Anchors = [akLeft, akBottom]
              TabOrder = 7
            end
            object btnMultstrReplCopy: TCorelButton
              Left = 328
              Top = 153
              Width = 97
              Height = 21
              Action = ActionMultistrReplCopy
              Anchors = [akLeft, akBottom]
              TabOrder = 6
            end
          end
          object tshStringInsert: TTabSheet
            Caption = 'tshStringInsert'
            ImageIndex = 3
            DesignSize = (
              539
              237)
            object lblStrInsStr: TLabel
              Left = 8
              Top = 8
              Width = 70
              Height = 13
              Caption = 'String to insert:'
            end
            object lblStrInsPos: TLabel
              Left = 8
              Top = 64
              Width = 52
              Height = 13
              Caption = 'At position:'
            end
            object lblStrInsNotes: TAntJvLinkLabel
              Left = 8
              Top = 144
              Width = 527
              Height = 91
              Caption = 
                'Notes:<br>'#13#10'- If you want to insert a string after or before ano' +
                'ther one, use the <LINK>String replacement</LINK> action<br>'#13#10'- ' +
                '%name% = Original file name (without extension)<BR>'#13#10'- %ext% = O' +
                'riginal extension (including the dot)<BR>'#13#10'- %folderN% = Folder ' +
                'name, where N is the level (1 = current, 2 = parent, etc.)<BR>'#13#10 +
                '- %% = % character<BR>'#13#10'see <LINK>Help</LINK> for more informati' +
                'on'#13#10
              Text.Strings = (
                
                  'Notes:<br>'#13#10'- If you want to insert a string after or before ano' +
                  'ther one, use the <LINK>String replacement</LINK> action<br>'#13#10'- ' +
                  '%name% = Original file name (without extension)<BR>'#13#10'- %ext% = O' +
                  'riginal extension (including the dot)<BR>'#13#10'- %folderN% = Folder ' +
                  'name, where N is the level (1 = current, 2 = parent, etc.)<BR>'#13#10 +
                  '- %% = % character<BR>'#13#10'see <LINK>Help</LINK> for more informati' +
                  'on'#13#10)
              Anchors = [akLeft, akTop, akRight]
              Transparent = False
              LinkColor = clBlue
              LinkColorClicked = clRed
              LinkColorHot = clPurple
              LinkStyle = [fsUnderline]
              HotLinks = False
              AutoHeight = True
              MarginWidth = 0
              MarginHeight = 0
              OnLinkClick = lblStrInsNotesLinkClick
            end
            object rbtStrInsBegin: TTntRadioButton
              Left = 112
              Top = 90
              Width = 113
              Height = 17
              Caption = 'From begin'
              Checked = True
              TabOrder = 2
              TabStop = True
              OnClick = UpdateActionPreview
            end
            object rbtStrInsEnd: TTntRadioButton
              Left = 232
              Top = 90
              Width = 113
              Height = 17
              Caption = 'From end'
              TabOrder = 3
              OnClick = UpdateActionPreview
            end
            object edtStrInsStr: TTntComboBox
              Left = 32
              Top = 32
              Width = 499
              Height = 21
              AutoComplete = False
              Anchors = [akLeft, akTop, akRight]
              DropDownCount = 20
              ItemHeight = 13
              TabOrder = 0
              OnChange = UpdateActionPreview
            end
            object cbxStrInsExt: TTntCheckBox
              Left = 8
              Top = 120
              Width = 523
              Height = 17
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Apply to extension instead of name'
              TabOrder = 4
            end
            object edtStrInsPos: TAntJvSpinEdit
              Left = 32
              Top = 88
              Width = 65
              Height = 21
              TabOrder = 1
              OnChange = UpdateActionPreview
            end
          end
          object tshMoveString: TTabSheet
            Caption = 'tshMoveString'
            ImageIndex = 12
            DesignSize = (
              539
              237)
            object lblMoveStrFrom: TLabel
              Left = 8
              Top = 8
              Width = 65
              Height = 13
              Caption = 'From position:'
            end
            object lblMoveStrCount: TLabel
              Left = 8
              Top = 64
              Width = 146
              Height = 13
              Caption = 'Number of characters to move:'
            end
            object lblMoveStrTo: TLabel
              Left = 8
              Top = 120
              Width = 55
              Height = 13
              Caption = 'To position:'
            end
            object lblMoveStrNotes: TAntJvLinkLabel
              Left = 8
              Top = 176
              Width = 527
              Height = 65
              Caption = 
                'Notes:<br>'#13#10'- If you want to move a specified string but that is' +
                ' not always at the same position, use the <link>String replaceme' +
                'nt</link> to delete it, and then the <link>String insertion</lin' +
                'k> to insert it at the new place<BR>'#13#10'- The "To position" value ' +
                'does not include the moved characters (i.e. it is like of they h' +
                'ave already been deleted)'#13#10
              Text.Strings = (
                
                  'Notes:<br>'#13#10'- If you want to move a specified string but that is' +
                  ' not always at the same position, use the <link>String replaceme' +
                  'nt</link> to delete it, and then the <link>String insertion</lin' +
                  'k> to insert it at the new place<BR>'#13#10'- The "To position" value ' +
                  'does not include the moved characters (i.e. it is like of they h' +
                  'ave already been deleted)'#13#10)
              Anchors = [akLeft, akTop, akRight]
              Transparent = False
              LinkColor = clBlue
              LinkColorClicked = clRed
              LinkColorHot = clPurple
              LinkStyle = [fsUnderline]
              HotLinks = False
              AutoHeight = True
              MarginWidth = 0
              MarginHeight = 0
              OnLinkClick = lblMoveStrNotesLinkClick
            end
            object pnlMoveStrFrom: TPanel
              Left = 104
              Top = 32
              Width = 371
              Height = 21
              BevelOuter = bvNone
              TabOrder = 1
              object rbtMoveStrFromBegin: TTntRadioButton
                Left = 8
                Top = 2
                Width = 113
                Height = 17
                Caption = 'From begin'
                Checked = True
                TabOrder = 0
                TabStop = True
                OnClick = UpdateActionPreview
              end
              object rbtMoveStrFromEnd: TTntRadioButton
                Left = 128
                Top = 2
                Width = 113
                Height = 17
                Caption = 'From end'
                TabOrder = 1
                OnClick = UpdateActionPreview
              end
            end
            object pnlMoveStrTo: TPanel
              Left = 104
              Top = 144
              Width = 371
              Height = 21
              BevelOuter = bvNone
              TabOrder = 4
              object rbtMoveStrToBegin: TTntRadioButton
                Left = 8
                Top = 2
                Width = 113
                Height = 17
                Caption = 'From begin'
                Checked = True
                TabOrder = 0
                TabStop = True
                OnClick = UpdateActionPreview
              end
              object rbtMoveStrToEnd: TTntRadioButton
                Left = 128
                Top = 2
                Width = 113
                Height = 17
                Caption = 'From end'
                TabOrder = 1
                OnClick = UpdateActionPreview
              end
            end
            object edtMoveStrFrom: TAntJvSpinEdit
              Left = 32
              Top = 32
              Width = 65
              Height = 21
              CheckMinValue = True
              TabOrder = 0
              OnChange = UpdateActionPreview
            end
            object edtMoveStrCount: TAntJvSpinEdit
              Left = 32
              Top = 88
              Width = 65
              Height = 21
              TabOrder = 2
              OnChange = UpdateActionPreview
            end
            object edtMoveStrTo: TAntJvSpinEdit
              Left = 32
              Top = 144
              Width = 65
              Height = 21
              CheckMinValue = True
              TabOrder = 3
              OnChange = UpdateActionPreview
            end
          end
          object tshCharDel: TTabSheet
            Caption = 'tshCharDel'
            ImageIndex = 4
            DesignSize = (
              539
              237)
            object lblCharDel: TLabel
              Left = 8
              Top = 8
              Width = 149
              Height = 13
              Caption = 'Number of characters to delete:'
            end
            object lblCharDelNotes: TAntJvLinkLabel
              Left = 8
              Top = 200
              Width = 527
              Height = 26
              Caption = 
                'Notes:<BR>- To delete a specified string, use the <LINK>String r' +
                'eplacement</LINK> action and replace the string by an empty one'#13 +
                #10
              Text.Strings = (
                
                  'Notes:<BR>- To delete a specified string, use the <LINK>String r' +
                  'eplacement</LINK> action and replace the string by an empty one'#13 +
                  #10)
              Anchors = [akLeft, akTop, akRight]
              Transparent = False
              LinkColor = clBlue
              LinkColorClicked = clRed
              LinkColorHot = clPurple
              LinkStyle = [fsUnderline]
              HotLinks = False
              AutoHeight = True
              MarginWidth = 0
              MarginHeight = 0
              OnLinkClick = lblCharDelNotesLinkClick
            end
            object rbtCharDelPos: TTntRadioButton
              Left = 8
              Top = 64
              Width = 523
              Height = 17
              Anchors = [akLeft, akTop, akRight]
              Caption = 'From position:'
              Checked = True
              TabOrder = 1
              TabStop = True
              OnClick = UpdateActionPreview
            end
            object rbtCharDelStr: TTntRadioButton
              Left = 8
              Top = 120
              Width = 120
              Height = 17
              Caption = 'After the string:'
              TabOrder = 4
              OnClick = UpdateActionPreview
            end
            object pnlCharDelPos: TPanel
              Left = 104
              Top = 88
              Width = 371
              Height = 21
              BevelOuter = bvNone
              TabOrder = 3
              object rbtCharDelBegin: TTntRadioButton
                Left = 8
                Top = 2
                Width = 113
                Height = 17
                Caption = 'From begin'
                Checked = True
                TabOrder = 0
                TabStop = True
                OnClick = UpdateActionPreview
              end
              object rbtCharDelEnd: TTntRadioButton
                Left = 128
                Top = 2
                Width = 113
                Height = 17
                Caption = 'From end'
                TabOrder = 1
                OnClick = UpdateActionPreview
              end
            end
            object edtCharDelStr: TTntComboBox
              Left = 32
              Top = 144
              Width = 499
              Height = 21
              AutoComplete = False
              Anchors = [akLeft, akTop, akRight]
              DropDownCount = 20
              ItemHeight = 13
              TabOrder = 6
              OnChange = UpdateActionPreview
            end
            object cbxCharDelExt: TTntCheckBox
              Left = 8
              Top = 176
              Width = 523
              Height = 17
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Apply to extension instead of name'
              TabOrder = 7
            end
            object rbtCharDelStr2: TTntRadioButton
              Left = 136
              Top = 120
              Width = 120
              Height = 17
              Caption = 'Before the string:'
              TabOrder = 5
              OnClick = UpdateActionPreview
            end
            object edtCharDel: TAntJvSpinEdit
              Left = 32
              Top = 32
              Width = 65
              Height = 21
              TabOrder = 0
              OnChange = UpdateActionPreview
            end
            object edtCharDelPos: TAntJvSpinEdit
              Left = 32
              Top = 88
              Width = 65
              Height = 21
              TabOrder = 2
              OnChange = UpdateActionPreview
            end
          end
          object tshEnum: TTabSheet
            Caption = 'tshEnum'
            ImageIndex = 5
            DesignSize = (
              539
              237)
            object lblEnumMask: TLabel
              Left = 8
              Top = 8
              Width = 29
              Height = 13
              Caption = 'Mask:'
            end
            object lblEnumStart: TLabel
              Left = 8
              Top = 64
              Width = 37
              Height = 13
              Caption = 'Start at:'
            end
            object lblEnumDigits: TLabel
              Left = 128
              Top = 64
              Width = 79
              Height = 13
              Caption = 'Number of digits:'
            end
            object lblEnumIncr: TLabel
              Left = 248
              Top = 64
              Width = 64
              Height = 13
              Caption = 'Increment by:'
            end
            object lblEnumNotes: TAntJvLinkLabel
              Left = 8
              Top = 144
              Width = 527
              Height = 91
              Caption = 
                'Notes:<BR>'#13#10'- %name% = Original file name (without extension)<BR' +
                '>'#13#10'- %ext% = Original extension (including the dot)<BR>'#13#10'- %num%' +
                ' = Number<BR>'#13#10'- %folderN% = Folder name, where N is the level (' +
                '1 = current, 2 = parent, etc.)<BR>'#13#10'- %% = % character<BR>'#13#10'see ' +
                '<LINK>Help</LINK> for more information'#13#10
              Text.Strings = (
                
                  'Notes:<BR>'#13#10'- %name% = Original file name (without extension)<BR' +
                  '>'#13#10'- %ext% = Original extension (including the dot)<BR>'#13#10'- %num%' +
                  ' = Number<BR>'#13#10'- %folderN% = Folder name, where N is the level (' +
                  '1 = current, 2 = parent, etc.)<BR>'#13#10'- %% = % character<BR>'#13#10'see ' +
                  '<LINK>Help</LINK> for more information'#13#10)
              Anchors = [akLeft, akTop, akRight]
              Transparent = False
              LinkColor = clBlue
              LinkColorClicked = clRed
              LinkColorHot = clPurple
              LinkStyle = [fsUnderline]
              HotLinks = False
              AutoHeight = True
              MarginWidth = 0
              MarginHeight = 0
              OnLinkClick = lblEnumNotesLinkClick
            end
            object edtEnum: TTntComboBox
              Left = 32
              Top = 32
              Width = 499
              Height = 21
              AutoComplete = False
              Anchors = [akLeft, akTop, akRight]
              DropDownCount = 20
              ItemHeight = 13
              TabOrder = 0
              OnChange = UpdateActionPreview
            end
            object cbxEnumRestart: TTntCheckBox
              Left = 8
              Top = 120
              Width = 523
              Height = 17
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Go back to start value when changing folder'
              TabOrder = 4
              OnClick = UpdateActionPreview
            end
            object edtEnumStart: TAntJvSpinEdit
              Left = 32
              Top = 88
              Width = 65
              Height = 21
              TabOrder = 1
              OnChange = UpdateActionPreview
            end
            object edtEnumDigits: TAntJvSpinEdit
              Left = 152
              Top = 88
              Width = 65
              Height = 21
              TabOrder = 2
              OnChange = UpdateActionPreview
            end
            object edtEnumIncr: TAntJvSpinEdit
              Left = 272
              Top = 88
              Width = 65
              Height = 21
              TabOrder = 3
              OnChange = UpdateActionPreview
            end
          end
          object tshMP3Tag: TTabSheet
            Caption = 'tshMP3Tag'
            ImageIndex = 6
            DesignSize = (
              539
              237)
            object lblMP3Mask: TLabel
              Left = 8
              Top = 8
              Width = 29
              Height = 13
              Caption = 'Mask:'
            end
            object lblMP3Notes: TAntJvLinkLabel
              Left = 8
              Top = 88
              Width = 527
              Height = 182
              Caption = 
                'Notes:<BR>'#13#10'- %author% = Author<BR>'#13#10'- %title% = Title<BR>'#13#10'- %a' +
                'lbum% = Album<BR>'#13#10'- %year% = Year<BR>'#13#10'- %genre% = Genre<BR>'#13#10'-' +
                ' %comm% = Comment<BR>'#13#10'- %track% = Track number (ID3V1.1)<BR>'#13#10'-' +
                ' %name% = Original filename (without extension)<BR>'#13#10'- %ext% = O' +
                'riginal extension (including dot)<BR>'#13#10'- %folderN% = Folder name' +
                ', where N is the level (1 = current, 2 = parent, etc.)<BR>'#13#10'- %%' +
                ' = % character<BR>'#13#10'- $if(condition,true-value,false-value)<BR>'#13 +
                #10'see <LINK>Help</LINK> for more information'#13#10
              Text.Strings = (
                
                  'Notes:<BR>'#13#10'- %author% = Author<BR>'#13#10'- %title% = Title<BR>'#13#10'- %a' +
                  'lbum% = Album<BR>'#13#10'- %year% = Year<BR>'#13#10'- %genre% = Genre<BR>'#13#10'-' +
                  ' %comm% = Comment<BR>'#13#10'- %track% = Track number (ID3V1.1)<BR>'#13#10'-' +
                  ' %name% = Original filename (without extension)<BR>'#13#10'- %ext% = O' +
                  'riginal extension (including dot)<BR>'#13#10'- %folderN% = Folder name' +
                  ', where N is the level (1 = current, 2 = parent, etc.)<BR>'#13#10'- %%' +
                  ' = % character<BR>'#13#10'- $if(condition,true-value,false-value)<BR>'#13 +
                  #10'see <LINK>Help</LINK> for more information'#13#10)
              Anchors = [akLeft, akTop, akRight]
              Transparent = False
              LinkColor = clBlue
              LinkColorClicked = clRed
              LinkColorHot = clPurple
              LinkStyle = [fsUnderline]
              HotLinks = False
              AutoHeight = True
              MarginWidth = 0
              MarginHeight = 0
              OnLinkClick = lblMP3NotesLinkClick
            end
            object cbxMP3TwoDigit: TTntCheckBox
              Left = 8
              Top = 64
              Width = 523
              Height = 17
              Anchors = [akLeft, akTop, akRight]
              Caption = 
                'Two-digit track number (insert a '#39'0'#39' in front of the number if r' +
                'equired)'
              TabOrder = 1
              OnClick = UpdateActionPreview
            end
            object edtMP3Mask: TTntComboBox
              Left = 32
              Top = 32
              Width = 499
              Height = 21
              AutoComplete = False
              Anchors = [akLeft, akTop, akRight]
              DropDownCount = 20
              ItemHeight = 13
              TabOrder = 0
              OnChange = UpdateActionPreview
            end
          end
          object tshDateTime: TTabSheet
            Caption = 'tshDateTime'
            ImageIndex = 7
            DesignSize = (
              539
              237)
            object lblDTMask: TLabel
              Left = 8
              Top = 8
              Width = 29
              Height = 13
              Caption = 'Mask:'
            end
            object lblDTNotes: TLabel
              Left = 8
              Top = 168
              Width = 31
              Height = 13
              Caption = 'Notes:'
            end
            object lblDTWhich: TLabel
              Left = 8
              Top = 89
              Width = 58
              Height = 13
              Caption = 'Date to use:'
            end
            object lblDTOffset: TLabel
              Left = 8
              Top = 112
              Width = 31
              Height = 13
              Caption = 'Offset:'
            end
            object lblDTOffsetSec: TLabel
              Left = 104
              Top = 138
              Width = 40
              Height = 13
              Caption = 'seconds'
            end
            object cbxDTSuffix: TTntCheckBox
              Left = 8
              Top = 64
              Width = 427
              Height = 17
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Add suffix if more than one file with the same date/time'
              TabOrder = 1
              OnClick = UpdateActionPreview
            end
            object lblDTNotes2: TMemo
              Left = 24
              Top = 184
              Width = 507
              Height = 51
              TabStop = False
              Anchors = [akLeft, akTop, akRight, akBottom]
              BorderStyle = bsNone
              Lines.Strings = (
                'f'#9'Original filename (without extension)'
                'e'#9'Original extension (including dot)'
                'c'#9'Date and time using the standard format in Windows'
                'd'#9'Day without a leading zero (1-31)'
                'dd'#9'Day with a leading zero (01-31)'
                'ddd'#9'Day, short name (Sun-Sat)'
                'dddd'#9'Day, full name (Sunday-Saturday)'
                'ddddd'#9'Date using Windows'#39' short date format'
                'dddddd'#9'Date using Windows'#39' long date format'
                'm'#9'Month without a leading zero (1-12)'
                'mm'#9'Month with a leading zero (01-12)'
                'mmm'#9'Month, short name (Jan-Dec)'
                'mmmm'#9'Month, full name (January-December)'
                'yy'#9'Year, 2-digit number (00-99)'
                'yyyy'#9'Year, 4-digit number (0000-9999)'
                'h'#9'Hour without a leading zero (0-23)'
                'hh'#9'Hour with a leading zero (00-23)'
                'n'#9'Minute without a leading zero (0-59)'
                'nn'#9'Minute with a leading zero (00-59)'
                's'#9'Second without a leading zero (0-59)'
                'ss'#9'Second with a leading zero (00-59)'
                'z'#9'Millisecond without a leading zero (0-999)'
                'zzz'#9'Millisecond with a leading zero (000-999)'
                'am/pm'#9'Uses the 12-hour clock for the preceding h or hh'
                'a/p'#9'Uses the 12-hour clock for the preceding h or hh'
                '"xx"'#9'Characters enclosed in quotes remain unchanged'
                ''
                
                  'If m or mm immediately follows h or hh, minute rather than month' +
                  ' is used')
              ParentColor = True
              ReadOnly = True
              ScrollBars = ssVertical
              TabOrder = 5
              WordWrap = False
            end
            object edtDTMask: TTntComboBox
              Left = 32
              Top = 32
              Width = 499
              Height = 21
              AutoComplete = False
              Anchors = [akLeft, akTop, akRight]
              DropDownCount = 20
              ItemHeight = 13
              TabOrder = 0
              OnChange = UpdateActionPreview
            end
            object rbtDTWhichCreation: TTntRadioButton
              Left = 96
              Top = 88
              Width = 105
              Height = 17
              Caption = 'Creation'
              TabOrder = 2
              OnClick = UpdateActionPreview
            end
            object rbtDTWhichModif: TTntRadioButton
              Left = 208
              Top = 88
              Width = 137
              Height = 17
              Caption = 'Last modification'
              TabOrder = 3
              OnClick = UpdateActionPreview
            end
            object edtDTOffset: TAntJvSpinEdit
              Left = 32
              Top = 136
              Width = 65
              Height = 21
              TabOrder = 4
              OnChange = UpdateActionPreview
            end
          end
          object tshRandom: TTabSheet
            Caption = 'tshRandom'
            ImageIndex = 8
            DesignSize = (
              539
              237)
            object lblRandomMask: TLabel
              Left = 8
              Top = 8
              Width = 29
              Height = 13
              Caption = 'Mask:'
            end
            object lblRandomMethod: TLabel
              Left = 8
              Top = 64
              Width = 39
              Height = 13
              Caption = 'Method:'
            end
            object lblRandomNotes: TAntJvLinkLabel
              Left = 8
              Top = 160
              Width = 527
              Height = 78
              Caption = 
                'Notes:<BR>'#13#10'- %name% = Original file name (without extension)<BR' +
                '>'#13#10'- %ext% = Original extension (including the dot)<BR>'#13#10'- %rand' +
                'om% = Random generated string<BR>'#13#10'- %% = % character<BR>'#13#10'see <' +
                'LINK>Help</LINK> for more information'#13#10
              Text.Strings = (
                
                  'Notes:<BR>'#13#10'- %name% = Original file name (without extension)<BR' +
                  '>'#13#10'- %ext% = Original extension (including the dot)<BR>'#13#10'- %rand' +
                  'om% = Random generated string<BR>'#13#10'- %% = % character<BR>'#13#10'see <' +
                  'LINK>Help</LINK> for more information'#13#10)
              Anchors = [akLeft, akTop, akRight]
              Transparent = False
              LinkColor = clBlue
              LinkColorClicked = clRed
              LinkColorHot = clPurple
              LinkStyle = [fsUnderline]
              HotLinks = False
              AutoHeight = True
              MarginWidth = 0
              MarginHeight = 0
              OnLinkClick = lblRandomNotesLinkClick
            end
            object rbtRandomGUID: TTntRadioButton
              Left = 32
              Top = 136
              Width = 499
              Height = 17
              Anchors = [akLeft, akTop, akRight]
              Caption = 'GUID'
              TabOrder = 3
              OnClick = UpdateActionPreview
            end
            object rbtRandomTick: TTntRadioButton
              Left = 32
              Top = 112
              Width = 499
              Height = 17
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Windows tick count'
              TabOrder = 2
              OnClick = UpdateActionPreview
            end
            object rbtRandomNumber: TTntRadioButton
              Left = 32
              Top = 88
              Width = 499
              Height = 17
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Random number (8 digits)'
              TabOrder = 1
              OnClick = UpdateActionPreview
            end
            object edtRandomMask: TTntComboBox
              Left = 32
              Top = 32
              Width = 499
              Height = 21
              AutoComplete = False
              Anchors = [akLeft, akTop, akRight]
              DropDownCount = 20
              ItemHeight = 13
              TabOrder = 0
              OnChange = UpdateActionPreview
            end
          end
          object tshCase: TTabSheet
            Caption = 'tshCase'
            ImageIndex = 9
            DesignSize = (
              539
              237)
            object rbtCaseWords: TTntRadioButton
              Left = 8
              Top = 8
              Width = 523
              Height = 17
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Upper first letter following any of these characters:'
              TabOrder = 0
              OnClick = UpdateActionPreview
            end
            object rbtCaseFirst: TTntRadioButton
              Left = 8
              Top = 64
              Width = 523
              Height = 17
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Upper the first letter of the filename'
              TabOrder = 2
              OnClick = UpdateActionPreview
            end
            object rbtCaseUpper: TTntRadioButton
              Left = 8
              Top = 88
              Width = 523
              Height = 17
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Upper'
              TabOrder = 3
              OnClick = UpdateActionPreview
            end
            object rbtCaseLower: TTntRadioButton
              Left = 8
              Top = 112
              Width = 523
              Height = 17
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Lower'
              TabOrder = 4
              OnClick = UpdateActionPreview
            end
            object cbxCaseLocale: TTntCheckBox
              Left = 8
              Top = 136
              Width = 523
              Height = 17
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Use Windows'#39' locale settings to change accentuated characters'
              TabOrder = 5
              OnClick = UpdateActionPreview
            end
            object cbxCaseIncludeExt: TTntCheckBox
              Left = 8
              Top = 160
              Width = 523
              Height = 17
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Also apply to extension'
              TabOrder = 6
              OnClick = UpdateActionPreview
            end
            object edtCaseAfter: TTntComboBox
              Left = 32
              Top = 32
              Width = 499
              Height = 21
              AutoComplete = False
              Anchors = [akLeft, akTop, akRight]
              DropDownCount = 20
              ItemHeight = 13
              TabOrder = 1
              OnClick = UpdateActionPreview
            end
            object cbxCaseOnlyExt: TTntCheckBox
              Left = 8
              Top = 184
              Width = 523
              Height = 17
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Apply to extension instead of name'
              TabOrder = 7
              OnClick = UpdateActionPreview
            end
          end
          object tshFromList: TTabSheet
            Caption = 'tshFromList'
            ImageIndex = 10
            DesignSize = (
              539
              237)
            object lblFromListNotes: TAntJvLinkLabel
              Left = 8
              Top = 189
              Width = 527
              Height = 46
              Caption = 
                'Notes:<BR>'#13#10'- Each line from this list will be used as a name fo' +
                'r a file. Items are used sequentially<BR>'#13#10'- If lines are empty ' +
                'or if there are not enough lines, the corresponding files will n' +
                'ot be renamed'#13#10
              Text.Strings = (
                
                  'Notes:<BR>'#13#10'- Each line from this list will be used as a name fo' +
                  'r a file. Items are used sequentially<BR>'#13#10'- If lines are empty ' +
                  'or if there are not enough lines, the corresponding files will n' +
                  'ot be renamed'#13#10)
              Anchors = [akLeft, akRight, akBottom]
              Transparent = False
              LinkColor = clBlue
              LinkColorClicked = clRed
              LinkColorHot = clPurple
              LinkStyle = [fsUnderline]
              HotLinks = False
              AutoHeight = False
              MarginWidth = 0
              MarginHeight = 0
            end
            object edtFromList: TTntMemo
              Left = 0
              Top = 24
              Width = 539
              Height = 113
              Anchors = [akLeft, akTop, akRight, akBottom]
              ScrollBars = ssBoth
              TabOrder = 0
              WordWrap = False
              OnChange = UpdateActionPreview
            end
            object cbxFromListExt: TTntCheckBox
              Left = 8
              Top = 143
              Width = 523
              Height = 17
              Anchors = [akLeft, akRight, akBottom]
              Caption = 'Append original extension of the file'
              TabOrder = 1
              OnClick = UpdateActionPreview
            end
            object btnFromListClear: TCorelButton
              Left = 0
              Top = 0
              Width = 75
              Height = 21
              Action = ActionFromListClear
              TabOrder = 2
            end
            object btnFromListOpen: TCorelButton
              Left = 80
              Top = 0
              Width = 75
              Height = 21
              Action = ActionFromListOpen
              TabOrder = 3
            end
            object btnFromListSave: TCorelButton
              Left = 160
              Top = 0
              Width = 75
              Height = 21
              Action = ActionFromListSave
              TabOrder = 4
            end
            object cbxFromListOnlyExt: TTntCheckBox
              Left = 8
              Top = 167
              Width = 523
              Height = 17
              Anchors = [akLeft, akRight, akBottom]
              Caption = 'Use items as extensions instead of names'
              TabOrder = 5
              OnClick = UpdateActionPreview
            end
          end
          object tshRegexp: TTabSheet
            Caption = 'tshRegexp'
            ImageIndex = 13
            DesignSize = (
              539
              237)
            object lblRegexp: TLabel
              Left = 8
              Top = 8
              Width = 54
              Height = 13
              Caption = 'Expression:'
            end
            object lblRegexpRepl: TLabel
              Left = 8
              Top = 64
              Width = 54
              Height = 13
              Caption = 'New name:'
            end
            object lblRegexpNotes: TAntJvLinkLabel
              Left = 8
              Top = 120
              Width = 527
              Height = 78
              Caption = 
                'Notes:<BR>'#13#10'- \ character can be used to escape special characte' +
                'rs (like $ and others), use a \\ for a simple \ in new name (if ' +
                'you want to target the new name to another folder)<BR>'#13#10'- Use ^ ' +
                'to indicate the beginning of the name and $ for the end of name<' +
                'BR>'#13#10'- The new name must contain $1, $2, etc. to specify where f' +
                'ound characters will come<BR>'#13#10'see <LINK>Help</LINK> for more in' +
                'formation on regular expressions'#13#10
              Text.Strings = (
                
                  'Notes:<BR>'#13#10'- \ character can be used to escape special characte' +
                  'rs (like $ and others), use a \\ for a simple \ in new name (if ' +
                  'you want to target the new name to another folder)<BR>'#13#10'- Use ^ ' +
                  'to indicate the beginning of the name and $ for the end of name<' +
                  'BR>'#13#10'- The new name must contain $1, $2, etc. to specify where f' +
                  'ound characters will come<BR>'#13#10'see <LINK>Help</LINK> for more in' +
                  'formation on regular expressions'#13#10)
              Anchors = [akLeft, akTop, akRight]
              Transparent = False
              LinkColor = clBlue
              LinkColorClicked = clRed
              LinkColorHot = clPurple
              LinkStyle = [fsUnderline]
              HotLinks = False
              AutoHeight = True
              MarginWidth = 0
              MarginHeight = 0
              OnLinkClick = lblRegexpNotesLinkClick
            end
            object edtRegexp: TTntComboBox
              Left = 32
              Top = 32
              Width = 499
              Height = 21
              AutoComplete = False
              Anchors = [akLeft, akTop, akRight]
              DropDownCount = 20
              ItemHeight = 13
              TabOrder = 0
              OnChange = UpdateActionPreview
            end
            object edtRegexpRepl: TTntComboBox
              Left = 32
              Top = 88
              Width = 499
              Height = 21
              AutoComplete = False
              Anchors = [akLeft, akTop, akRight]
              DropDownCount = 20
              ItemHeight = 13
              TabOrder = 1
              OnChange = UpdateActionPreview
            end
          end
          object tshExif: TTabSheet
            Caption = 'tshExif'
            ImageIndex = 14
            DesignSize = (
              539
              237)
            object lblExifMask: TLabel
              Left = 8
              Top = 8
              Width = 29
              Height = 13
              Caption = 'Mask:'
            end
            object lblExifNotes: TAntJvLinkLabel
              Left = 8
              Top = 120
              Width = 527
              Height = 104
              Caption = 
                'Notes:<BR>'#13#10'- %name% = Original file name (without extension)<BR' +
                '>'#13#10'- %ext% = Original extension (including the dot)<BR>'#13#10'- %date' +
                'timeoriginal% or %datetime% = Original date and time of the pict' +
                'ure<BR>'#13#10'- %orientation% = Picture orientation<BR>'#13#10'- %flash% = ' +
                'Flash was on or off<BR>'#13#10'- %% = % character<BR>'#13#10'see <LINK>Help<' +
                '/LINK> for more information and for complete list of EXIF tags'#13#10 +
                #13#10
              Text.Strings = (
                
                  'Notes:<BR>'#13#10'- %name% = Original file name (without extension)<BR' +
                  '>'#13#10'- %ext% = Original extension (including the dot)<BR>'#13#10'- %date' +
                  'timeoriginal% or %datetime% = Original date and time of the pict' +
                  'ure<BR>'#13#10'- %orientation% = Picture orientation<BR>'#13#10'- %flash% = ' +
                  'Flash was on or off<BR>'#13#10'- %% = % character<BR>'#13#10'see <LINK>Help<' +
                  '/LINK> for more information and for complete list of EXIF tags'#13#10 +
                  #13#10)
              Anchors = [akLeft, akTop, akRight]
              Transparent = False
              LinkColor = clBlue
              LinkColorClicked = clRed
              LinkColorHot = clPurple
              LinkStyle = [fsUnderline]
              HotLinks = False
              AutoHeight = True
              MarginWidth = 0
              MarginHeight = 0
              OnLinkClick = lblExifNotesLinkClick
            end
            object lblExifOffset: TLabel
              Left = 8
              Top = 64
              Width = 31
              Height = 13
              Caption = 'Offset:'
            end
            object lblExifOffsetSec: TLabel
              Left = 104
              Top = 90
              Width = 40
              Height = 13
              Caption = 'seconds'
            end
            object edtExifMask: TTntComboBox
              Left = 32
              Top = 32
              Width = 499
              Height = 21
              AutoComplete = False
              Anchors = [akLeft, akTop, akRight]
              DropDownCount = 20
              ItemHeight = 13
              TabOrder = 0
              OnChange = UpdateActionPreview
            end
            object edtExifOffset: TAntJvSpinEdit
              Left = 32
              Top = 88
              Width = 65
              Height = 21
              TabOrder = 1
              OnChange = UpdateActionPreview
            end
          end
        end
      end
      object lstActions: TListBox
        Left = 0
        Top = 0
        Width = 150
        Height = 268
        Hint = '|Actions list'
        Align = alLeft
        ItemHeight = 13
        Items.Strings = (
          'Change extension'
          'String replacement'
          'Multiple string replacement'
          'String insertion'
          'Move string'
          'Characters deletion'
          'Enumeration'
          'Use mp3 tag info'
          'Use date & time'
          'Random names'
          'Change case'
          'Take names from list'
          'Regular expression'
          'Use EXIF info')
        PopupMenu = pmActions
        TabOrder = 0
        OnClick = lstActionsClick
        OnContextPopup = lstActionsContextPopup
      end
      object grpPreview: TGroupBox
        Left = 0
        Top = 268
        Width = 700
        Height = 65
        Align = alBottom
        Caption = '     Preview of selected file '
        TabOrder = 2
        DesignSize = (
          700
          65)
        object lblPreviewOld: TLabel
          Left = 16
          Top = 20
          Width = 66
          Height = 13
          Caption = 'Current name:'
        end
        object lblPreviewNew: TLabel
          Left = 16
          Top = 40
          Width = 90
          Height = 13
          Caption = 'Sample new name:'
        end
        object lblPreviewOldName: TTntLabel
          Left = 120
          Top = 16
          Width = 569
          Height = 22
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = '.'
          ShowAccelChar = False
          Layout = tlCenter
        end
        object lblPreviewNewName: TTntLabel
          Left = 120
          Top = 36
          Width = 569
          Height = 22
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = '.'
          ShowAccelChar = False
          Layout = tlCenter
        end
        object cbxPreview: TSpTBXCheckBox
          Left = 8
          Top = 0
          Width = 14
          Height = 15
          ParentColor = True
          TabOrder = 0
          OnClick = UpdateActionPreview
        end
      end
      object grpBatch: TGroupBox
        Left = 0
        Top = 336
        Width = 700
        Height = 105
        Align = alBottom
        Caption = '     Batch contents (planned actions) '
        TabOrder = 3
        object lstBatch: TVirtualStringTree
          Left = 2
          Top = 15
          Width = 696
          Height = 88
          Hint = '|Batch contents (planned actions)'
          Align = alClient
          DragMode = dmAutomatic
          DragType = dtVCL
          DrawSelectionMode = smBlendedRectangle
          Header.AutoSizeIndex = -1
          Header.Font.Charset = DEFAULT_CHARSET
          Header.Font.Color = clWindowText
          Header.Font.Height = -11
          Header.Font.Name = 'MS Sans Serif'
          Header.Font.Style = []
          Header.Options = [hoColumnResize, hoDrag]
          HintAnimation = hatNone
          PopupMenu = pmBatch
          TabOrder = 1
          TreeOptions.AutoOptions = [toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
          TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
          TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
          TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect, toRightClickSelect]
          OnDragOver = lstFilesDragOver
          OnDragDrop = lstFilesDragDrop
          OnGetText = lstBatchGetText
          OnKeyDown = lstFilesKeyDown
          OnKeyUp = lstFilesKeyUp
          OnResize = lstBatchResize
          Columns = <
            item
              Options = [coEnabled, coParentBidiMode, coParentColor, coShowDropMark, coVisible]
              Position = 0
              Width = 30
            end
            item
              Options = [coEnabled, coParentBidiMode, coParentColor, coShowDropMark, coVisible]
              Position = 1
              Width = 600
            end>
        end
        object cbxBatch: TSpTBXCheckBox
          Left = 8
          Top = 0
          Width = 14
          Height = 15
          ParentColor = True
          TabOrder = 0
          OnClick = cbxBatchClick
        end
      end
    end
    object tshLog: TTabSheet
      Caption = 'Log'
      ImageIndex = 4
      object lstLog: TVirtualStringTree
        Left = 0
        Top = 0
        Width = 700
        Height = 441
        Align = alClient
        DrawSelectionMode = smBlendedRectangle
        Header.AutoSizeIndex = 0
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'MS Shell Dlg 2'
        Header.Font.Style = []
        Header.Options = [hoAutoResize]
        Indent = 0
        PopupMenu = pmLog
        TabOrder = 0
        TabStop = False
        TreeOptions.AutoOptions = [toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
        TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
        TreeOptions.PaintOptions = [toHideFocusRect, toHideSelection, toShowRoot, toThemeAware, toUseBlendedImages]
        TreeOptions.SelectionOptions = [toDisableDrawSelection]
        OnEnter = lstLogEnter
        OnGetText = lstLogGetText
        OnPaintText = lstLogPaintText
        Columns = <
          item
            Position = 0
            Width = 696
          end>
      end
    end
  end
  object tbdBottom: TSpTBXDock
    Left = 0
    Top = 518
    Width = 708
    Height = 9
    Position = dpBottom
  end
  object StatusBar1: TSpTBXStatusBar
    Left = 0
    Top = 527
    Width = 708
    Height = 20
    object StatusBarProgress: TTBControlItem
      Control = ProgressBar1
    end
    object StatusBarSep1: TSpTBXSeparatorItem
    end
    object StatusBarPanel1: TSpTBXLabelItem
      Alignment = taCenter
      MinWidth = 70
    end
    object StatusBarSep2: TSpTBXSeparatorItem
    end
    object StatusBarPanel2: TSpTBXLabelItem
    end
    object ProgressBar1: TAntJvSpecialProgress
      Left = 0
      Top = 0
      Width = 146
      Height = 16
      Color = clBtnFace
      EndColor = clGradientActiveCaption
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clCaptionText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      Position = 70
      Solid = True
      StartColor = clActiveCaption
      TextCentered = True
      TextOption = toPercent
    end
  end
  object ActionList1: TActionList
    Images = imglstHot
    OnExecute = ActionList1Execute
    Left = 48
    Top = 272
    object ActionLogClear: TAction
      Category = 'Log'
      Caption = 'Clear'
      Hint = 'Clear log|Clear log'
      OnExecute = ActionLogClearExecute
    end
    object ActionAddFiles: TAction
      Category = 'Files'
      Caption = 'Add files...'
      Hint = 'Add files|Add files to the list'
      ShortCut = 113
      OnExecute = ActionAddFilesExecute
    end
    object ActionAddFolders: TAction
      Category = 'Files'
      Caption = 'Add folders...'
      Hint = 'Add folders|Add folders or folder contents to the list'
      ShortCut = 114
      OnExecute = ActionAddFoldersExecute
    end
    object ActionMoveUp: TAction
      Category = 'Items'
      Caption = 'Up'
      Hint = 'Move up|Move up selected items'
      ShortCut = 16422
      OnExecute = ActionMoveUpExecute
    end
    object ActionMoveDown: TAction
      Category = 'Items'
      Caption = 'Down'
      Hint = 'Move down|Move down selected items'
      ShortCut = 16424
      OnExecute = ActionMoveDownExecute
    end
    object ActionMoveTop: TAction
      Category = 'Items'
      Caption = 'Top'
      Hint = 'Move top|Move selected items to top of the list'
      ShortCut = 24614
      OnExecute = ActionMoveTopExecute
    end
    object ActionMoveBottom: TAction
      Category = 'Items'
      Caption = 'Bottom'
      Hint = 'Move bottom|Move selected items to bottom of the list'
      ShortCut = 24616
      OnExecute = ActionMoveBottomExecute
    end
    object ActionRemoveSelected: TAction
      Category = 'Files'
      Caption = 'Remove sel.'
      Hint = 'Remove selected files|Remove selected files from the list'
      ShortCut = 117
      OnExecute = ActionRemoveSelectedExecute
    end
    object ActionRemoveAll: TAction
      Category = 'Files'
      Caption = 'Remove all'
      Hint = 'Remove all files|Remove all files from the list'
      ShortCut = 118
      OnExecute = ActionRemoveAllExecute
    end
    object ActionRemoveDead: TAction
      Category = 'Files'
      Caption = 'Remove dead'
      Hint = 'Remove dead files|Remove dead files from the list'
      ShortCut = 119
      OnExecute = ActionRemoveDeadExecute
    end
    object ActionActionGo: TAction
      Category = 'Program'
      Caption = 'Go'
      Hint = 'Go|Start the renaming process'
      ShortCut = 120
      OnExecute = ActionActionGoExecute
    end
    object ActionActionStop: TAction
      Category = 'Program'
      Caption = 'Stop'
      Enabled = False
      Hint = 'Stop|Stop the renaming process'
      ShortCut = 27
      OnExecute = ActionActionStopExecute
    end
    object ActionOptions: TAction
      Category = 'Program'
      Caption = 'Options...'
      Hint = 'Options|Change program options'
      ShortCut = 121
      OnExecute = ActionOptionsExecute
    end
    object ActionActionUndo: TAction
      Category = 'Program'
      Caption = 'Undo'
      Hint = 'Undo|Undo last renaming'
      ShortCut = 123
      OnExecute = ActionActionGoExecute
    end
    object ActionHelpContents: TAction
      Category = 'Program'
      Caption = 'Help'
      Hint = 'Help contents|Open help contents'
      ShortCut = 112
      OnExecute = ActionHelpContentsExecute
    end
    object ActionHelpAbout: TAction
      Category = 'Program'
      Caption = 'About...'
      Hint = 'About|About this program'
      ShortCut = 8304
      OnExecute = ActionHelpAboutExecute
    end
    object ActionExit: TAction
      Category = 'Program'
      Caption = 'Exit'
      Hint = 'Exit|Quit the program'
      ShortCut = 32883
      OnExecute = ActionExitExecute
    end
    object ActionTabFiles: TAction
      Category = 'Tab'
      Caption = 'Files'
      GroupIndex = 1
      Hint = 'Files|Display files'
      ShortCut = 16433
      OnExecute = ActionTabFilesExecute
    end
    object ActionTabActions: TAction
      Tag = 1
      Category = 'Tab'
      Caption = 'Actions'
      GroupIndex = 1
      Hint = 'Actions|Display actions'
      ShortCut = 16434
      OnExecute = ActionTabFilesExecute
    end
    object ActionTabLog: TAction
      Tag = 2
      Category = 'Tab'
      Caption = 'Log'
      GroupIndex = 1
      Hint = 'Log|Display log'
      ShortCut = 16435
      OnExecute = ActionTabFilesExecute
    end
    object ActionBatchAddAction: TAction
      Category = 'Batch'
      Caption = 'Add to batch'
      Hint = 
        'Add action to batch|Add the selected action and its settings to ' +
        'the batch'
      ShortCut = 16497
      OnExecute = ActionBatchAddActionExecute
    end
    object ActionBatchRemoveSel: TAction
      Category = 'Batch'
      Caption = 'Remove sel.'
      Hint = 'Remove selected actions|Remove selected actions from the batch'
      ShortCut = 8309
      OnExecute = ActionBatchRemoveSelExecute
    end
    object ActionBatchOpen: TAction
      Category = 'Batch'
      Caption = 'Open...'
      Hint = 'Open|Open a batch file'
      ShortCut = 16463
      OnExecute = ActionBatchOpenExecute
    end
    object ActionBatchSaveAs: TAction
      Category = 'Batch'
      Caption = 'Save...'
      Hint = 'Save|Save this batch to a file'
      ShortCut = 16467
      OnExecute = ActionBatchSaveAsExecute
    end
    object ActionPreview: TAction
      Category = 'Files'
      Caption = 'Preview'
      Hint = 
        'Refresh preview|Refresh preview column with currently planned ac' +
        'tions'
      ShortCut = 116
      OnExecute = ActionActionGoExecute
    end
    object ActionLogSave: TAction
      Category = 'Log'
      Caption = 'Save...'
      Hint = 'Save log|Save log to a file'
      OnExecute = ActionLogCopyExecute
    end
    object ActionAlwaysOnTop: TAction
      Category = 'Program'
      Caption = 'On top'
      Hint = 'Always on top|Keep window always on top'
      ShortCut = 122
      SecondaryShortCuts.Strings = (
        'Ctrl+T')
      OnExecute = ActionAlwaysOnTopExecute
    end
    object ActionLogCopy: TAction
      Category = 'Log'
      Caption = 'Copy'
      Hint = 'Copy log|Copy log to clipboard'
      OnExecute = ActionLogCopyExecute
    end
    object ActionBatchClear: TAction
      Category = 'Batch'
      Caption = 'Clear'
      Hint = 'Clear batch|Remove all actions from the batch'
      ShortCut = 8310
      OnExecute = ActionBatchClearExecute
    end
    object ActionFromListClear: TAction
      Category = 'ActionItems'
      Caption = 'Clear'
      Hint = '|Clear list'
      OnExecute = ActionFromListClearExecute
    end
    object ActionFromListOpen: TAction
      Category = 'ActionItems'
      Caption = 'Open...'
      Hint = '|Load a list from a file'
      OnExecute = ActionFromListOpenExecute
    end
    object ActionFromListSave: TAction
      Category = 'ActionItems'
      Caption = 'Save...'
      Hint = '|Save the list to a file'
      OnExecute = ActionFromListSaveExecute
    end
    object ActionItemsSelectAll: TAction
      Category = 'Items'
      Caption = 'Select all'
      Hint = 'Select all|Select all items'
      OnExecute = ActionItemsSelectAllExecute
    end
    object ActionItemsSelectNone: TAction
      Category = 'Items'
      Caption = 'Select none'
      Hint = 'Select none|Deselect all items'
      OnExecute = ActionItemsSelectAllExecute
    end
    object ActionItemsSelectInvert: TAction
      Category = 'Items'
      Caption = 'Invert selection'
      Hint = 'Invert selection|Invert items selection'
      OnExecute = ActionItemsSelectInvertExecute
    end
    object ActionBatchNoRecent: TAction
      Category = 'Batch'
      Caption = '(no recent file)'
      Enabled = False
    end
    object ActionItemsUnsort: TAction
      Category = 'Items'
      Caption = 'Unsorted items'
      Hint = '|Removes current sort order'
      OnExecute = ActionItemsUnsortExecute
    end
    object ActionMultistrReplClear: TAction
      Category = 'ActionItems'
      Caption = 'Clear Table'
      Hint = '|Clear the string table'
      OnExecute = ActionMultistrReplClearExecute
    end
    object ActionMultistrReplSave: TAction
      Category = 'ActionItems'
      Caption = 'Save'
      Hint = '|Save current set under the name written above'
      OnExecute = ActionMultistrReplSaveExecute
    end
    object ActionMultistrReplDelete: TAction
      Category = 'ActionItems'
      Caption = 'Delete'
      Hint = '|Delete currently selected set name'
      OnExecute = ActionMultistrReplDeleteExecute
    end
    object ActionLogFilter: TAction
      Category = 'Log'
      Caption = 'Filter'
      Hint = 'Filter log|Filter log entries'
      OnExecute = ActionLogFilterExecute
    end
    object ActionLogFilterOk: TAction
      Category = 'Log'
      AutoCheck = True
      Caption = 'Successes'
      Hint = 
        'Show renamed files|Show entries corresponding to successfully re' +
        'named files'
      OnExecute = ActionLogFilterChange
    end
    object ActionLogFilterError: TAction
      Category = 'Log'
      AutoCheck = True
      Caption = 'Errors'
      Hint = 'Show errors|Show entries corresponding to errors'
      OnExecute = ActionLogFilterChange
    end
    object ActionLogFilterNot: TAction
      Category = 'Log'
      AutoCheck = True
      Caption = 'Not renamed'
      Hint = 
        'Show skipped files|Show entries corresponding to files that were' +
        ' not renamed'
      OnExecute = ActionLogFilterChange
    end
    object ActionMultistrReplCopy: TAction
      Category = 'ActionItems'
      Caption = 'Copy/Export'
      Hint = 
        '|Copy the whole string table to the clipboard as tab-delimited t' +
        'ext for importing in Excel or text editors'
      OnExecute = ActionMultistrReplCopyExecute
    end
    object ActionMultistrReplPaste: TAction
      Category = 'ActionItems'
      Caption = 'Paste/Import'
      Hint = 
        '|Paste the whole string table from the clipboard as tab-delimite' +
        'd text, for importing from Excel or text editors'
      OnExecute = ActionMultistrReplPasteExecute
    end
  end
  object imglstHot: TImageList
    Left = 48
    Top = 312
  end
  object pmFiles: TSpTBXPopupMenu
    Images = imglstHot
    Left = 128
    Top = 120
    object tbmFileRemoveSel: TSpTBXItem
      Action = ActionRemoveSelected
    end
    object tbmFileRemoveAll: TSpTBXItem
      Action = ActionRemoveAll
    end
    object tbmFileSelectAll: TSpTBXItem
      Action = ActionItemsSelectAll
    end
    object tbmFileSelectNone: TSpTBXItem
      Action = ActionItemsSelectNone
    end
    object tbmFileSelectInvert: TSpTBXItem
      Action = ActionItemsSelectInvert
    end
    object tbmFileSep: TSpTBXSeparatorItem
    end
    object tbmFilePreview: TSpTBXItem
      Action = ActionPreview
    end
  end
  object pmBatch: TSpTBXPopupMenu
    Images = imglstHot
    Left = 128
    Top = 216
    object tbmBatchRemoveSel: TSpTBXItem
      Action = ActionBatchRemoveSel
    end
    object tbmBatchRemoveAll: TSpTBXItem
      Action = ActionBatchClear
    end
  end
  object pmLog: TSpTBXPopupMenu
    Images = imglstHot
    Left = 128
    Top = 264
    object tbmLogClear: TSpTBXItem
      Action = ActionLogClear
    end
    object tbmLogCopy: TSpTBXItem
      Action = ActionLogCopy
    end
  end
  object pmActions: TSpTBXPopupMenu
    Images = imglstHot
    Left = 128
    Top = 168
    object tbmBatchAdd: TSpTBXItem
      Action = ActionBatchAddAction
    end
  end
  object pmhFiles: TVTHeaderPopupMenu
    OnPopup = pmhFilesPopup
    Left = 84
    Top = 316
  end
  object imglstFiles: TImageList
    Left = 12
    Top = 308
  end
  object XPManifest1: TXPManifest
    Left = 16
    Top = 272
  end
  object VarMessages: TAntJvTranslatorStrings
    Left = 120
    Top = 320
  end
end
