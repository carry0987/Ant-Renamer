(************************************************************************
 *                                                                      *
 *   (C) 2004-2006 Antoine Potten                                       *
 *   http://www.antp.be/software                                        *
 *                                                                      *
 ************************************************************************
 *                                                                      *
 *   This program is free software; you can redistribute it and/or      *
 *   modify it under the terms of the GNU General Public License        *
 *   as published by the Free Software Foundation; either version 2     *
 *   of the License, or (at your option) any later version.             *
 *                                                                      *
 *   This program is distributed in the hope that it will be useful,    *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of     *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *
 *   GNU General Public License for more details.                       *
 *                                                                      *
 ************************************************************************)

unit frameLanguage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, ComCtrls, ImgList, ExtCtrls,

  AntJvLinkLabel, functions_files;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

type

  TLanguageFileType = (lfXML, lfLNG);
  TLanguageInfo = record
    EnglishName: string;
    LocalName: string;
    Version: string;
    Authors: string;
    Comments: string;
    Icon: string;
    Encoding: string;
  end;
  TLanguageFrame = class(TFrame)
    lstLanguages: TListView;
    lblVersion: TLabel;
    lblVersionText: TLabel;
    lblMadeby: TLabel;
    lblComments: TLabel;
    imgLanguages: TImageList;
    PanelForXpThemeBug: TPanel;
    lblMadebyText: TAntJvLinkLabel;
    lblCommentsText: TAntJvLinkLabel;
    procedure lstLanguagesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lblMadebyTextLinkClick(Sender: TObject; LinkNumber: Integer;
      LinkText: String);
    procedure lblCommentsTextLinkClick(Sender: TObject;
      LinkNumber: Integer; LinkText: String);
    procedure lstLanguagesCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      var DefaultDraw: Boolean);
  private
    FLanguagesLoaded: Boolean;
    FDefaultEnglish: TListItem;
    FListItemTopTextMargin: Integer;
    FListItemLeftTextMargin: Integer;
    procedure LoadLNGLanguage(const AFileName: TAntFileName; out ALangInfo: TLanguageInfo);
    procedure LoadXMLLanguage(const AFileName: TAntFileName; out ALangInfo: TLanguageInfo);
    procedure LoadInfos(const FileContents: string; out ALangInfo: TLanguageInfo);
    function GetSelectedLanguageFile: string;
    procedure SetSelectedLanguageFile(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    property LanguagesLoaded: Boolean read FLanguagesLoaded;
    property SelectedLanguageFile: string read GetSelectedLanguageFile write SetSelectedLanguageFile;
    procedure LoadLanguages(AType: TLanguageFileType; const AFolder, AFilter: TAntFileName);
  end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

implementation

{$R *.dfm}

uses
  ShellAPI, IdCoderMIME, IniFiles,

  {$IFDEF ANTUNICODE}
  TntSysUtils, TntClasses,
  {$ENDIF}
  functions_str, functions_sys;

const
  idxEnglishName = -1;
  idxLocalName = 0;
  idxFileName = 1;
  idxVersion = 2;
  idxAuthors = 3;
  idxComments = 4;
  idxEncoding = 5;
  lstLangTotal = 6;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

constructor TLanguageFrame.Create(AOwner: TComponent);
begin
  FLanguagesLoaded := False;
  inherited;
  PanelForXpThemeBug.DoubleBuffered := True;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TLanguageFrame.LoadLanguages(AType: TLanguageFileType; const AFolder, AFilter: TAntFileName);
var
  {$IFDEF ANTUNICODE}
  SearchRecord: TSearchRecW;
  {$ELSE}
  SearchRecord: TSearchRec;
  {$ENDIF}
  RemaindFiles: Integer;
  PicStream: TMemoryStream;
  Decoder: TIdDecoderMIME;
  bmp: TBitmap;
  FileInfo: TLanguageInfo;
  function ImportIcon(const Encoded: string): Integer;
  begin
    Result := -1;
    if Encoded <> '' then
      try
        PicStream.Seek(0, soFromBeginning);
        Decoder.DecodeStream(Encoded, PicStream);
        PicStream.Seek(0, soFromBeginning);
        bmp.LoadFromStream(PicStream);
        Result := imgLanguages.AddMasked(bmp, bmp.Canvas.Pixels[0, 0])
      except
      end;
  end;
begin
  PicStream := TMemoryStream.Create;
  Decoder := TIdDecoderMIME.Create(nil);
  bmp := TBitmap.Create;
  lstLanguages.Items.BeginUpdate;
  try
    lstLanguages.Clear;
    imgLanguages.Clear;
    FDefaultEnglish := lstLanguages.Items.Add;
    with FDefaultEnglish do
    begin
      Caption := 'English (default)';       // english name
      SubItems.Add('English');              // local name
      SubItems.Add('');                     // file name
      SubItems.Add('n/a');                  // version
      SubItems.Add('');                     // authors
      SubItems.Add('English internal default strings.|Applies only at application start.');
      SubItems.Add('');                     // charset
      ImageIndex := ImportIcon('Qk32AAAAAAAAAHYAAAAoAAAAEAAAABAAAAABAAQAAAAAAIAAAAAzCwAAMwsAABAAAAAQAAAAAAAAAFAAAABmAAAAmAAAAK4AAACYMgAA/QAyAP9mMwAAAJgAAAC0AAAA/QBlZf8AmZmZAP//mQD//' + '8sA////AP//////////AAAAAAAAAAAJUSLIjCIVkAHqQ+quNK1gAj6j6q4642ACM+rqrq4zYAzu7uqu7u7gCKqqqqqqqrAIqqqqqqqqsAzu7uqu7u7gAjTq6q6uM2ACPqPqrjrjYAHqQ+quM65gCXZm675mZrAAAAAAAAAAAP//////////');
      Selected := True;
    end;
    {$IFDEF ANTUNICODE}
    WideSetCurrentDir(AFolder);
    RemaindFiles := WideFindFirst(AFilter, 0, SearchRecord);
    {$ELSE}
    SetCurrentDir(AFolder);
    RemaindFiles := FindFirst(AFilter, 0, SearchRecord);
    {$ENDIF}
    try
      while RemaindFiles = 0 do
      begin
        case AType of
          lfXML:  LoadXMLLanguage(SearchRecord.Name, FileInfo);
          lfLNG:  LoadLNGLanguage(SearchRecord.Name, FileInfo);
        end;
        with FileInfo, lstLanguages.Items.Add do
        begin
          Caption := EnglishName;
          with SubItems do
          begin
            Add(LocalName);
            // we suppose that language filenames are in English, so no need of unicode when path is stripped
            {$IFDEF ANTUNICODE}
            Add(WideExtractFileName(SearchRecord.Name));
            {$ELSE}
            Add(ExtractFileName(SearchRecord.Name));
            {$ENDIF}
            Add(Version);
            Add(Authors);
            Add(Comments);
            Add(Encoding);
            ImageIndex := ImportIcon(Icon)
          end;
          {$IFDEF ANTUNICODE}
          RemaindFiles := WideFindNext(SearchRecord);
          {$ELSE}
          RemaindFiles := FindNext(SearchRecord);
          {$ENDIF}
        end;
      end;
    finally
      {$IFDEF ANTUNICODE}
      WideFindClose(SearchRecord);
      {$ELSE}
      FindClose(SearchRecord);
      {$ENDIF}
    end;
  finally
    lstLanguages.Items.EndUpdate;
    PicStream.Free;
    Decoder.Free;
    bmp.Free;
  end;
  FLanguagesLoaded := True;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TLanguageFrame.LoadLNGLanguage(const AFileName: TAntFileName; out ALangInfo: TLanguageInfo);
var
  f: string;
  BegPos, EndPos: Integer;
begin
  try
    {$IFDEF ANTUNICODE}
    with TTntFileStream.Create(AFileName, fmOpenRead) do
    {$ELSE}
    with TFileStream.Create(AFileName, fmOpenRead) do
    {$ENDIF}
      try
        Seek(0, soFromBeginning);
        System.SetLength(f, Size);
        Read(f[1], Size);
      finally
        Free;
      end;
    BegPos := Pos('[FileInformation]', f);
    EndPos := Pos(sLineBreak + sLineBreak, f);
    if (BegPos > 0) and (EndPos > 0) then
      LoadInfos(Copy(f, BegPos + 17, EndPos - BegPos), ALangInfo)
    else
      Abort;
  except
    with ALangInfo do
    begin
      EnglishName := '?';
      LocalName := '';
      Version := '';
      Authors := '';
      Comments := '';
      Icon := '';
    end;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TLanguageFrame.LoadXMLLanguage(const AFileName: TAntFileName; out ALangInfo: TLanguageInfo);
var
  f: string;
  BegPos, EndPos: Integer;
begin
  try
    {$IFDEF ANTUNICODE}
    with TTntFileStream.Create(AFileName, fmOpenRead) do
    {$ELSE}
    with TFileStream.Create(AFileName, fmOpenRead) do
    {$ENDIF}
      try
        Seek(0, soFromBeginning);
        System.SetLength(f, Size);
        Read(f[1], Size);
      finally
        Free;
      end;
    BegPos := Pos('<!--', f);
    EndPos := Pos('-->', f);
    if (BegPos > 0) and (EndPos > 0) then
      LoadInfos(Copy(f, BegPos + 4, EndPos - BegPos - 4), ALangInfo)
    else
      Abort;
    ALangInfo.Encoding := TextBefore(TextAfter(f, '<?xml version="1.0" encoding="'), '"?>');
  except
    with ALangInfo do
    begin
      EnglishName := '?';
      LocalName := '';
      Version := '';
      Authors := '';
      Comments := '';
      Icon := '';
      Encoding := '';
    end;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TLanguageFrame.LoadInfos(const FileContents: string; out ALangInfo: TLanguageInfo);
begin
  with TStringList.Create do
    try
      Text := FileContents;
      with ALangInfo do
      begin
        EnglishName := Values['Name'];
        LocalName := Values['LocalName'];
        Version := Values['Version'];
        Authors := Values['Authors'];
        Comments := Values['Comments'];
        Icon := Values['Icon'];
        Encoding := Values['Encoding'];
      end;
    finally
      Free;
    end
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TLanguageFrame.GetSelectedLanguageFile: string;
begin
  Result := '';
  with lstLanguages do
    if Selected <> nil then
      with Selected do
        if SubItems.Count > idxFileName then
          Result := SubItems[idxFileName];
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TLanguageFrame.SetSelectedLanguageFile(const Value: string);
var
  i: Integer;
begin
  with lstLanguages do
  begin
    HandleNeeded;
    if (Value = '') or (Value = '.lng') then
    begin
      if Items.Count > 0 then
        Selected := FDefaultEnglish;
    end
    else
      for i := 0 to Items.Count-1 do
        with Items[i].SubItems do
          if (Count > idxFileName) and (Strings[idxFileName] = Value) then
          begin
            lstLanguages.Selected := Items[i];
            Break;
          end;

  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TLanguageFrame.lstLanguagesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  sl: TStringList;
  Authors: string;
  i: Integer;
  Charset: TFontCharset;
begin
  if (Item <> nil) and (Selected) and (Item.SubItems.Count >= lstLangTotal) then
  begin
    sl := TStringList.Create;
    try
      lblVersionText.Caption := Item.SubItems[idxVersion];
      sl.Text := InsertLineBreaks(Item.SubItems[idxAuthors]);
      i := 0;
      while i < sl.Count do
      begin
        if (sl[i] <> '') then
        begin
          if (i < sl.Count-1) and (sl[i+1] <> '') then
            Authors := Format('%s%s (<link>%s</link>)<br>', [Authors, sl[i], sl[i+1]])
          else
            Authors := Format('%s%s<br>', [Authors, sl[i]]);
        end;
        Inc(i, 2);
      end;
      Charset := EncodingToCharset(Item.SubItems[idxEncoding]);
      if IsWindowsNT then
        lblMadebyText.Font.Charset := Charset;
      lblMadebyText.Caption := Authors;
      if IsWindowsNT then
        lblCommentsText.Font.Charset := Charset;
      lblCommentsText.Caption := StringReplace(Item.SubItems[idxComments], '|', '<br>', [rfReplaceAll]);
    finally
      sl.Free;
    end;
  end
  else
  begin
    lblVersionText.Caption := '';
    if IsWindowsNT then
      lblMadebyText.Font.Charset := DEFAULT_CHARSET;
    lblMadebyText.Caption := '';
    if IsWindowsNT then
      lblCommentsText.Font.Charset := DEFAULT_CHARSET;
    lblCommentsText.Caption := '';
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TLanguageFrame.lblMadebyTextLinkClick(Sender: TObject; LinkNumber: Integer; LinkText: String);
begin
  ShellExecute(0, nil, PChar('mailto:' + LinkText), nil, nil, SW_SHOWMAXIMIZED);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TLanguageFrame.lblCommentsTextLinkClick(Sender: TObject; LinkNumber: Integer; LinkText: String);
begin
  if not StartsText('http://', LinkText) then
    LinkText := 'http://' + LinkText;
  ShellExecute(0, nil, PChar(LinkText), nil, nil, SW_SHOWMAXIMIZED);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TLanguageFrame.lstLanguagesCustomDrawSubItem(Sender: TCustomListView; Item: TListItem; SubItem: Integer; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  IsUTF8: Boolean;
  Charset: Byte;
  ColWidth: Integer;
  r: TRect;
  s: string;
  ws: WideString;
  w: PWidechar;
begin
  DefaultDraw := True;
  if IsWindowsNT and (Subitem = 1) then
  begin
    s := Item.SubItems[idxLocalName];
    if SameText(Item.SubItems[idxEncoding], 'UTF-8') then
    begin
      IsUTF8 := true;
      Charset := DEFAULT_CHARSET;
    end
    else
    begin
      IsUTF8 := false;
      Charset := EncodingToCharset(Item.SubItems[idxEncoding]);
    end;
    if IsUTF8 or (Charset <> DEFAULT_CHARSET) then
    begin
      DefaultDraw := False;
      if Item = lstLanguages.Selected then
      begin
        if lstLanguages.Focused then
        begin
          Sender.Canvas.Brush.Color := clHighlight;
          Sender.Canvas.Font.Color := clHighlightText;
        end
        else
        begin
          Sender.Canvas.Brush.Color := clBtnFace;
          Sender.Canvas.Font.Color := clBtnText;
        end
      end
      else
      begin
        Sender.Canvas.Brush.Color := clWindow;
        Sender.Canvas.Font.Color := clWindowText;
      end;
      r := Item.DisplayRect(drBounds);
      ColWidth := Sender.Column[0].Width;
      if FListItemTopTextMargin + FListItemLeftTextMargin = 0 then
      begin
        FListItemTopTextMargin := (r.Bottom - r.Top - Sender.Canvas.TextHeight('E')) div 2;
        FListItemLeftTextMargin := Sender.Canvas.TextWidth('  ');
      end;
      Sender.Canvas.FillRect(Rect(r.Left + ColWidth, r.Top, ColWidth + Sender.Column[1].Width, r.Bottom));
      try
        if IsUTF8 then
        begin
          ws := Utf8Decode(s);
          w := PWideChar(ws);
        end
        else
        begin
          GetMem(w, Length(s) * 2 + 1);
          MultiByteToWideChar(CharsetToCodepage(Charset), MB_USEGLYPHCHARS, PChar(s), -1, w, Length(s) * 2 + 1);
        end;
        Windows.TextOutW(Sender.Canvas.Handle,
          r.Left + ColWidth + FListItemLeftTextMargin,
          r.Top + FListItemTopTextMargin,
          w, Length(WideString(w)));
      finally
        if not IsUTF8 then
        begin
          FreeMem(w);
        end;
      end;
    end;
  end
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

end.
