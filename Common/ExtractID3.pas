(************************************************************************
 *                                                                      *
 *   Ant Renamer 2.x                                                    *
 *   (C) 2003-2006 Antoine Potten                                       *
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

unit ExtractID3;

interface

uses
  SysUtils, Classes, Windows,
  functions_files;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

type
  TID3InfoInternal = record
    TAG:     array[0..2] of Char;
    Title:   array[0..29] of Char;
    Author:  array[0..29] of Char;
    Album:   array[0..29] of Char;
    Year:    array[0..3] of Char;
    Comment: array[0..29] of Char;
    Genre:   Byte;
  end;
  TID3Info = record
    Title:   string;
    Author:  string;
    Album:   string;
    Year:    string;
    Comment: string;
    Track:   Byte;
    Genre:   Byte;
  end;

procedure ExtractID3Info(const FileNameW: TAntFileName; var ARec: TID3Info);

var
  strID3CannotOpen: string = 'unable to open file to read ID3 tag';
  strID3NoTag:      string = 'no ID3 tag found in file';
  refTag:           array[0..2] of Char = 'TAG';

var
  ID3Genres: array [0..126] of string = (
     'Blues',
     'Classic Rock',
     'Country',
     'Dance',
     'Disco',
     'Funk',
     'Grunge',
     'Hip-Hop',
     'Jazz',
     'Metal',
     'New Age',
     'Oldies',
     'Other',
     'Pop',
     'R&B',
     'Rap',
     'Reggae',
     'Rock',
     'Techno',
     'Industrial',
     'Alternative',
     'Ska',
     'Death Metal',
     'Pranks',
     'Soundtrack',
     'Euro-Techno',
     'Ambient',
     'Trip-Hop',
     'Vocal',
     'Jazz+Funk',
     'Fusion',
     'Trance',
     'Classical',
     'Instrumental',
     'Acid',
     'House',
     'Game',
     'Sound Clip',
     'Gospel',
     'Noise',
     'AlternRock',
     'Bass',
     'Soul',
     'Punk',
     'Space',
     'Meditative',
     'Instrumental Pop',
     'Instrumental Rock',
     'Ethnic',
     'Gothic',
     'Darkwave',
     'Techno-Industrial',
     'Electronic',
     'Pop-Folk',
     'Eurodance',
     'Dream',
     'Southern Rock',
     'Comedy',
     'Cult',
     'Gangsta',
     'Top 40',
     'Christian Rap',
     'Pop/Funk',
     'Jungle',
     'Native American',
     'Cabaret',
     'New Wave',
     'Psychadelic',
     'Rave',
     'Showtunes',
     'Trailer',
     'Lo-Fi',
     'Tribal',
     'Acid Punk',
     'Acid Jazz',
     'Polka',
     'Retro',
     'Musical',
     'Rock & Roll',
     'Hard Rock',
     'Folk',
     'Folk/Rock',
     'National Folk',
     'Swing',
     'Fast Fusion',
     'Bebob',
     'Latin',
     'Revival',
     'Celtic',
     'Bluegrass',
     'Avantgarde',
     'Gothic Rock',
     'Progressive Rock',
     'Psychedelic Rock',
     'Symphonic Rock',
     'Slow Rock',
     'Big Band',
     'Chorus',
     'Easy Listening',
     'Acoustic',
     'Humour',
     'Speech',
     'Chanson',
     'Opera',
     'Chamber Music',
     'Sonata',
     'Symphony',
     'Booty Bass',
     'Primus',
     'Porn Groove',
     'Satire',
     'Slow Jam',
     'Club',
     'Tango',
     'Samba',
     'Folklore',
     'Ballad',
     'Power Ballad',
     'Rhythmic Soul',
     'Freestyle',
     'Duet',
     'Punk Rock',
     'Drum Solo',
     'Acapella',
     'Euro-House',
     'Dance Hall',
     ''
  );

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

implementation

{$IFDEF ANTUNICODE}
uses
  TntClasses, functions_sys;
{$ENDIF}

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure ExtractID3Info(const FileNameW: TAntFileName; var ARec: TID3Info);
var
  f: THandleStream;
  h: THandle;
  TempRec: TID3InfoInternal;
  FileNameA: string;
begin
  {$IFDEF ANTUNICODE}
  if IsWindowsNT then
    h := CreateFileW(PWideChar(FileNameW), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_ALWAYS, 0, 0)
  else
  {$ENDIF}
  begin
    FileNameA := FileNameW;
    h := CreateFile(PChar(FileNameA), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_ALWAYS, 0, 0);
  end;
  if h = INVALID_HANDLE_VALUE then
    raise Exception.Create(strID3CannotOpen);
  try
    f := THandleStream.Create(h);
    try
      try
        f.Seek(- SizeOf(TID3InfoInternal), soFromEnd);
        f.Read(TempRec, SizeOf(TID3InfoInternal));
      except
        raise Exception.Create(strID3NoTag);
      end;
      if not CompareMem(@TempRec.TAG, @refTag, 3) then
        raise Exception.Create(strID3NoTag);
      SetString(ARec.Title, TempRec.Title, 30);
      if Pos(#0, ARec.Title) > 0 then
        SetLength(ARec.Title, Pos(#0, ARec.Title) - 1);
      SetString(ARec.Author, TempRec.Author, 30);
      if Pos(#0, ARec.Author) > 0 then
        SetLength(ARec.Author, Pos(#0, ARec.Author) - 1);
      SetString(ARec.Album, TempRec.Album, 30);
      if Pos(#0, ARec.Album) > 0 then
        SetLength(ARec.Album, Pos(#0, ARec.Album) - 1);
      SetString(ARec.Year, TempRec.Year, 4);
      SetString(ARec.Comment, TempRec.Comment, 30);
      if Pos(#0, ARec.Comment) > 0 then
        SetLength(ARec.Comment, Pos(#0, ARec.Comment) - 1);
      ARec.Track := Byte(TempRec.Comment[29]);
      ARec.Genre := TempRec.Genre;
    finally
      f.Free;
    end;
  finally
    CloseHandle(h);
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

end.
