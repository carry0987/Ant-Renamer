object MessageWin: TMessageWin
  Left = 691
  Top = 390
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'MessageWin'
  ClientHeight = 134
  ClientWidth = 365
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  DesignSize = (
    365
    134)
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 10
    Top = 10
    Width = 32
    Height = 32
    AutoSize = True
  end
  object btn1: TCorelButton
    Left = 8
    Top = 80
    Width = 75
    Height = 25
    Caption = 'btn1'
    TabOrder = 0
  end
  object btn2: TCorelButton
    Left = 16
    Top = 88
    Width = 75
    Height = 25
    Caption = 'btn2'
    TabOrder = 1
  end
  object btn3: TCorelButton
    Left = 24
    Top = 96
    Width = 75
    Height = 25
    Caption = 'btn3'
    TabOrder = 2
  end
  object CheckBox1: TCheckBox
    Left = 5
    Top = 52
    Width = 355
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'CheckBox1'
    TabOrder = 4
    Visible = False
  end
  object btn4: TCorelButton
    Left = 32
    Top = 104
    Width = 75
    Height = 25
    Caption = 'btn4'
    TabOrder = 3
  end
  object Captions: TAntStringList
    Strings.Strings = (
      'Warning'
      'Error'
      'Information'
      'Confirmation'
      '&Yes'
      '&No'
      '&OK'
      '&Cancel'
      '&Abort'
      '&Retry'
      '&Ignore'
      '&All'
      'No to all'
      'Yes to all'
      '&Help')
    Left = 184
    Top = 8
  end
end
