inherited AboutForm: TAboutForm
  Left = 484
  Top = 224
  HelpContext = 9900
  BorderStyle = bsSingle
  Caption = 'About this program'
  ClientHeight = 347
  ClientWidth = 400
  OldCreateOrder = True
  DesignSize = (
    400
    347)
  PixelsPerInch = 96
  TextHeight = 13
  inherited Bevel1: TBevel
    Top = 314
    Width = 394
  end
  inherited AntAutoHintLabel1: TAntAutoHintLabel
    Top = 331
    Width = 400
  end
  inherited btn1: TCorelButton
    Left = 322
    Top = 319
    Caption = 'History'
    Visible = True
    OnClick = btn1Click
  end
  inherited btn2: TCorelButton
    Left = 244
    Top = 319
    Cancel = True
    Caption = 'Close'
    Default = True
    ModalResult = 2
    Visible = True
  end
  object Panel1: TPanel [4]
    Left = 3
    Top = 3
    Width = 394
    Height = 47
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvLowered
    Color = clWindow
    ParentBackground = False
    TabOrder = 4
    DesignSize = (
      394
      47)
    object Image1: TImage
      Left = 8
      Top = 8
      Width = 32
      Height = 32
      AutoSize = True
      Picture.Data = {
        07544269746D6170360C0000424D360C00000000000036000000280000002000
        0000200000000100180000000000000C0000130B0000130B0000000000000000
        0000F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7
        F5D7B7777777F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7
        B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5
        D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7
        777777000000777777777777F5D7B7F5D7B7F5D7B7777777F5D7B7F5D7B7F5D7
        B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5
        D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7777777
        00000066FFFF000000000000777777777777777777777777777777F5D7B7F5D7
        B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5
        D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7777777000000
        66FFFF66FFFF66FFFF66FFFF000000000000777777000000777777777777F5D7
        B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5
        D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B777777700000066FFFF
        66FFFF99CCCC99CCCC6699996699990000000000000000000000007777777777
        77F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5
        D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B777777700000066FFFF99CCCC
        99CCCC66999966999900000000000000FFFF00000000000000FFFF0000007777
        77777777F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5
        D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B777777700000099CCCC99CCCC669999
        66999900000000000000000000FFFF00FFFF00000000000000000000FFFF0000
        00000000777777777777F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5
        D7B7F5D7B7F5D7B7F5D7B7F5D7B777777700000099CCCC669999669999000000
        00000033FFFF00000000000033FFFF00000000000033FFFF33FFFF33FFFF33FF
        FF000000000000000000777777777777F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5
        D7B7F5D7B7F5D7B7F5D7B7777777000000669999669999000000000000000000
        00FFFF00FFFF00000000000000000066FFFF66FFFF66FFFF66FFFF66FFFF66FF
        FF66FFFF00000099CCCC000000000000777777777777F5D7B7F5D7B7F5D7B7F5
        D7B7F5D7B7F5D7B777777700000066999900000000000000FFFF000000000000
        00FFFF00000000000099FFFF99FFFF99FFFF99FFFF99FFFF99FFFF99FFFF99FF
        FF99FFFF99FFFF00000099CCCC99CCCC000000000000777777777777777777F5
        D7B7F5D7B777777700000000000000000000000000FFFF00FFFF000000000000
        00000099FFFF99FFFF99FFFF99FFFF99FFFF99FFFF99FFFF99FFFF99FFFF99FF
        FF99FFFF99FFFF99FFFF00000099CCCC99CCCC66FFFF000000000000777777F5
        D7B777777700000000000000FFFF00000000000000FFFF000000000000CCFFFF
        CCFFFFCCFFFFFFFFFFFFFFFFCCFFFFFFFFFFCCFFFFCCFFFFCCFFFFCCFFFFCCFF
        FFCCFFFFCCFFFFCCFFFFCCFFFF00000099CCCC99CCCC000000777777F5D7B7F5
        D7B700000000000000FFFF00FFFF000000000000000000CCFFFFCCFFFFCCFFFF
        FFFFFF336666336666336666336666336666FFFFFFCCFFFFCCFFFFCCFFFF3366
        66336666000000000000CCFFFFCCFFFF000000000000777777777777F5D7B7F5
        D7B700000000000000FFFF000000000000CCFFFFCCFFFFCCFFFFFFFFFFFFFFFF
        000000000000000000000000000000336666336666FFFFFFCCFFFF3366660000
        00000000CCFFFFCCFFFFCCFFFFCCFFFFCCFFFF000000777777777777777777F5
        D7B7F5D7B7000000000000FFFFFFCCFFFFFFFFFFCCFFFFFFFFFFCCFFFF000000
        000066000066000088000099000088000000336666336666FFFFFF0000003366
        66CCFFFFCCFFFFCCFFFFCCFFFF336666000000000000000000F5D7B7F5D7B7F5
        D7B7F5D7B7F5D7B7000000FFFFFFFFFFFFCCFFFFFFFFFFCCFFFFFFFFFF000000
        000099000088000099000099000099000088000000336666336666CCFFFF0000
        00CCFFFFCCFFFF336666336666CCFFFF000000CCFFFFCCFFFF000000F5D7B7F5
        D7B7F5D7B7F5D7B7F5D7B7000000FFFFFFFFFFFFCCFFFFFFFFFFCCFFFF000000
        0000AA0000AA0000AA000000000000000099000000336666336666FFFFFF0000
        00CCFFFF336666CCFFFFCCFFFFCCFFFF000000000000000000F5D7B7F5D7B7F5
        D7B7F5D7B7F5D7B7F5D7B7F5D7B7000000FFFFFFFFFFFFCCFFFFFFFFFF000000
        0000AA0000DD0000AA0000000000000000660000660000003366663366660000
        00000000CCFFFFCCFFFFCCFFFF000000000000F5D7B7F5D7B7F5D7B7F5D7B7F5
        D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7000000FFFFFFFFFFFFFFFFFF000000
        0000DD9999FF0000DD0000000000000000660000660000003366663366663366
        66000000CCFFFF000000000000000000F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5
        D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7000000FFFFFFFFFFFF000000
        0000DD0000DD0000DD0000CC0000AA0000AA0000660000000000000000000000
        00000000000000336666000000000000777777000000000000F5D7B7F5D7B7F5
        D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7000000FFFFFF000000
        0000000000660000DD0000000000660000660000000000000000000000000000
        00336666336666000000000000777777F5D7B7000000F5D7B7F5D7B7F5D7B7F5
        D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7000000FFFFFF
        0000000000000000000000000000660000000000660000000000AA0000000000
        00000000000000000000777777777777000000000000F5D7B7F5D7B7F5D7B7F5
        D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7000000
        FFFFFF0000000000000000000000000000AA0000000000000000AA0000000000
        66000000000000000000000000777777000000777777777777F5D7B7F5D7B7F5
        D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7000000F5D7B7000000
        0000000000000000000000000000000000000000000000880000880000000000
        00000000000000000000000066000000000000777777777777F5D7B7F5D7B7F5
        D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7000000F5D7B7
        000000000000F5D7B7000000F5D7B7F5D7B70000000000000000000000660000
        CC0000000000000000AA000099000000000000777777F5D7B7F5D7B7F5D7B7F5
        D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7000000
        F5D7B7F5D7B7000000F5D7B7F5D7B7F5D7B70000000000000000660000DD0000
        DD0000DD000000000000000000000000000066000000F5D7B7F5D7B7F5D7B7F5
        D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7000000
        F5D7B7F5D7B7000000F5D7B7F5D7B7F5D7B70000000000000000880000AA9999
        FF0000DD0000DD0000000000000000AA000066000000F5D7B7F5D7B7F5D7B7F5
        D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7000000
        F5D7B7F5D7B7000000F5D7B7F5D7B7F5D7B7000000F5D7B70000000000DD0000
        DD0000DD0000DD0000000000000000AA000088000000F5D7B7F5D7B7F5D7B7F5
        D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7
        F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7000000F5D7B70000000000880000
        DD0000DD0000DD0000DD0000CC0000AA0000AA000000F5D7B7F5D7B7F5D7B7F5
        D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7
        F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7000000F5D7B7F5D7B70000000000
        DD0000DD0000DD0000DD0000AA0000AA000066000000F5D7B7F5D7B7F5D7B7F5
        D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7
        F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7000000F5D7B7F5D7B7F5D7B70000
        00000000000088000066000066000000000000F5D7B7F5D7B7F5D7B7F5D7B7F5
        D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7
        F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7
        B7F5D7B7000000000000000000F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5D7B7F5
        D7B7}
      Transparent = True
    end
    object Label7: TLabel
      Left = 48
      Top = 8
      Width = 342
      Height = 23
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Ant Renamer'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'Arial'
      Font.Style = [fsBold, fsItalic]
      ParentFont = False
      Transparent = True
    end
    object lblVersion: TLabel
      Left = 48
      Top = 28
      Width = 341
      Height = 14
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = '[version]'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
  end
  object Panel2: TPanel [5]
    Left = 3
    Top = 53
    Width = 394
    Height = 263
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 5
    object PageControl1: TPageControl
      Left = 0
      Top = 0
      Width = 394
      Height = 263
      ActivePage = tshCredits
      Align = alClient
      HotTrack = True
      MultiLine = True
      RaggedRight = True
      TabOrder = 0
      object tshGeneral: TTabSheet
        BorderWidth = 5
        Caption = 'General'
        object lblGeneral: TAntJvLinkLabel
          Left = 0
          Top = 0
          Width = 376
          Height = 225
          Caption = 
            '<b>Copyright '#169' 2000-2015 Antoine Potten</b><br>'#13#10'<br>'#13#10'This prog' +
            'ram was made with Borland Delphi 7<br>'#13#10'See <link>Credits</link>' +
            ' page for additionnal information<br>'#13#10'<br>'#13#10'E-mail address: <li' +
            'nk>antrenamer-contact@antp.be</link><br>'#13#10'<br>'#13#10'Web site address' +
            ': <link>www.antp.be/software/renamer/</link><br>'#13#10'<br>'#13#10'<b>Licen' +
            'se:</b><br>'#13#10'This program is free software; you can redistribute' +
            ' it and/or modify it under the terms of the GNU General Public L' +
            'icense as published by the Free Software Foundation; either vers' +
            'ion 2 of the License, or (at your option) any later version.This' +
            ' program is distributed in the hope that it will be useful, but ' +
            'WITHOUT ANY WARRANTY; without even the implied warranty of MERCH' +
            'ANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the <link>G' +
            'NU General Public License</link> for more details.<br>'#13#10
          Text.Strings = (
            
              '<b>Copyright '#169' 2000-2015 Antoine Potten</b><br>'#13#10'<br>'#13#10'This prog' +
              'ram was made with Borland Delphi 7<br>'#13#10'See <link>Credits</link>' +
              ' page for additionnal information<br>'#13#10'<br>'#13#10'E-mail address: <li' +
              'nk>antrenamer-contact@antp.be</link><br>'#13#10'<br>'#13#10'Web site address' +
              ': <link>www.antp.be/software/renamer/</link><br>'#13#10'<br>'#13#10'<b>Licen' +
              'se:</b><br>'#13#10'This program is free software; you can redistribute' +
              ' it and/or modify it under the terms of the GNU General Public L' +
              'icense as published by the Free Software Foundation; either vers' +
              'ion 2 of the License, or (at your option) any later version.This' +
              ' program is distributed in the hope that it will be useful, but ' +
              'WITHOUT ANY WARRANTY; without even the implied warranty of MERCH' +
              'ANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the <link>G' +
              'NU General Public License</link> for more details.<br>'#13#10)
          Transparent = True
          LinkColor = clBlue
          LinkColorClicked = clRed
          LinkColorHot = clPurple
          LinkStyle = [fsUnderline]
          HotLinks = False
          AutoHeight = False
          MarginWidth = 0
          MarginHeight = 0
          OnLinkClick = lblGeneralLinkClick
          Align = alClient
        end
      end
      object tshCredits: TTabSheet
        BorderWidth = 5
        Caption = 'Credits'
        ImageIndex = 2
        object lblCredits: TAntJvLinkLabel
          Left = 0
          Top = 0
          Width = 376
          Height = 225
          Caption = 
            '<b>Thanks to:</b><br>'#13#10'Sebastien Buysse, Gr'#233'gory Zicot, Michel P' +
            'otten, ...and all the people that sent me suggestions and commen' +
            'ts<br>'#13#10'<br>'#13#10'<b>Translations:</b><br>'#13#10'Arek Czak, '#13#10'Asabukuro, ' +
            #13#10'Damir13, '#13#10'Denny, '#13#10'dzmitry[li], '#13#10'elega, '#13#10'GVG, '#13#10'Hiro5, '#13#10'Iv' +
            'o Blaauw, '#13#10'Jim Liu, '#13#10'Karin Andersson, '#13#10'Lin Fan, '#13#10'Roberto Pai' +
            'va, '#13#10'Salvatore Meschini, '#13#10'Sepp Winkler, '#13#10'Seunghoe Yang, '#13#10'Vic' +
            'o//Koby, '#13#10'Volker Schmidt, '#13#10'Wasilis Mandratzis, '#13#10'Zoltan Danhau' +
            'ser'#13#10'<br>'#13#10'<br>'#13#10'<b>Components:</b><br>'#13#10'JVCL (<link>jvcl.delphi' +
            '-jedi.org</link> - a few comps. extracted from there and modifie' +
            'd),<br>'#13#10'Toolbar 2000 (<link>www.jrsoftware.org</link>) & TBX,<b' +
            'r>'#13#10'CorelButton (<link>www.theill.com/delphi.asp</link> - modifi' +
            'ed), <br>'#13#10'VirtualTreeview (<link>www.soft-gems.net</link>), Tnt' +
            ' Unicode Controls, <br>'#13#10'TRegExpr (<link>regexpstudio.com</link>' +
            '), dEXIF'#13#10#13#10
          Text.Strings = (
            
              '<b>Thanks to:</b><br>'#13#10'Sebastien Buysse, Gr'#233'gory Zicot, Michel P' +
              'otten, ...and all the people that sent me suggestions and commen' +
              'ts<br>'#13#10'<br>'#13#10'<b>Translations:</b><br>'#13#10'Arek Czak, '#13#10'Asabukuro, ' +
              #13#10'Damir13, '#13#10'Denny, '#13#10'dzmitry[li], '#13#10'elega, '#13#10'GVG, '#13#10'Hiro5, '#13#10'Iv' +
              'o Blaauw, '#13#10'Jim Liu, '#13#10'Karin Andersson, '#13#10'Lin Fan, '#13#10'Roberto Pai' +
              'va, '#13#10'Salvatore Meschini, '#13#10'Sepp Winkler, '#13#10'Seunghoe Yang, '#13#10'Vic' +
              'o//Koby, '#13#10'Volker Schmidt, '#13#10'Wasilis Mandratzis, '#13#10'Zoltan Danhau' +
              'ser'#13#10'<br>'#13#10'<br>'#13#10'<b>Components:</b><br>'#13#10'JVCL (<link>jvcl.delphi' +
              '-jedi.org</link> - a few comps. extracted from there and modifie' +
              'd),<br>'#13#10'Toolbar 2000 (<link>www.jrsoftware.org</link>) & TBX,<b' +
              'r>'#13#10'CorelButton (<link>www.theill.com/delphi.asp</link> - modifi' +
              'ed), <br>'#13#10'VirtualTreeview (<link>www.soft-gems.net</link>), Tnt' +
              ' Unicode Controls, <br>'#13#10'TRegExpr (<link>regexpstudio.com</link>' +
              '), dEXIF'#13#10#13#10)
          Transparent = True
          LinkColor = clBlue
          LinkColorClicked = clRed
          LinkColorHot = clPurple
          LinkStyle = [fsUnderline]
          HotLinks = False
          AutoHeight = False
          MarginWidth = 0
          MarginHeight = 0
          OnLinkClick = lblCreditsClick
          Align = alClient
        end
      end
    end
  end
  inherited btn3: TCorelButton
    Left = 166
    Top = 319
  end
  inherited btn4: TCorelButton
    Left = 88
    Top = 319
  end
end
