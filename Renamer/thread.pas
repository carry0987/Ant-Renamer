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

unit thread;

interface

uses
  Classes, sysUtils,

  Actions, Files;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

type
  TRenThread = class(TThread)
  private
    FLastMessage: WideString;
    FRefreshParams: record
      Sender: TRenFile;
      BatchPosition: Integer;
      FilePosition: Integer;
    end;
    procedure SyncRefresh;
//    procedure DisplayMessage;
    procedure LogError;
  protected
    procedure Execute; override;
  public
    FActionsList: TRenActions;
    FFilesList: TRenFiles;
    FOperation: TRenActionOperation;
  end;

var
  FRenThread: TRenThread = nil;

procedure Refresh(Sender: TRenFile; BatchPosition, FilePosition: Integer);

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

implementation

uses
  fMain, MessageForm, global, Dialogs;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenThread.Execute;
begin
  try
    FRefreshParams.Sender := nil;
    FRefreshParams.BatchPosition := 0;
    FRefreshParams.FilePosition := 0;
    FActionsList.Perform(FFilesList, FOperation);
  except
    on e: Exception do
    begin
      FLastMessage := 'Thread error caught: ' + e.Message;
      Synchronize(LogError);
    end;
  end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}
(*
procedure TRenThread.DisplayMessage;
begin
  MessageWin.Execute(FLastMessage, mtError, [mbOk]);
end;
*)
{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenThread.LogError;
begin
  MainForm.AddToLog(FLastMessage, rsError);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TRenThread.SyncRefresh;
begin
  with FRefreshParams do
    MainForm.Refresh(Sender, BatchPosition, FilePosition);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure Refresh(Sender: TRenFile; BatchPosition, FilePosition: Integer);
begin
  if FRenThread <> nil then
    with FRenThread do
    begin
      FRefreshParams.Sender := Sender;
      if BatchPosition <> -1 then
        FRefreshParams.BatchPosition := BatchPosition;
      if FilePosition <> -1 then
        FRefreshParams.FilePosition := FilePosition;
      Synchronize(SyncRefresh);
    end;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

end.
