object BaseDlg: TBaseDlg
  Left = 500
  Top = 447
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'basedlg'
  ClientHeight = 98
  ClientWidth = 353
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    353
    98)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 3
    Top = 65
    Width = 347
    Height = 2
    Anchors = [akLeft, akRight, akBottom]
    Shape = bsBottomLine
  end
  object AntAutoHintLabel1: TAntAutoHintLabel
    Left = 0
    Top = 82
    Width = 353
    Height = 16
    Align = alBottom
    AutoSize = False
    Transparent = True
  end
  object btn1: TCorelButton
    Left = 275
    Top = 70
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'btn1'
    TabOrder = 3
    Visible = False
  end
  object btn2: TCorelButton
    Left = 197
    Top = 70
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'btn2'
    TabOrder = 2
    Visible = False
  end
  object btn3: TCorelButton
    Left = 119
    Top = 70
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'btn3'
    TabOrder = 1
    Visible = False
  end
  object btn4: TCorelButton
    Left = 41
    Top = 70
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'btn4'
    TabOrder = 0
    Visible = False
  end
end
