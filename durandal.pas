
program Durandal;

{$IFDEF WINDOWS}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
{$IFDEF UNIX}
  CThreads,
{$ENDIF}
  SysUtils, Classes, Math, StrUtils, Chess, Log, History, Parser;

{$I version.inc}

procedure SendToUser(const AText: string; const AFlush: boolean = TRUE);
begin
  ToLog(Concat(TimeToStr(Now), ' <- ', AText));
  WriteLn(output, AText);
  if AFlush then
    Flush(output);
end;

type
  TProcessus = class(TThread)
    protected
      procedure Execute; override;
  end;

var
  LPos: TPosition;
  LMoveTime: cardinal;

procedure TProcessus.Execute;
var
  LTimeElapsed: cardinal;
  LMove: string;
begin
  LTimeElapsed := {$IFDEF FPC}GetTickCount64{$ELSE}GetTickCount{$ENDIF};
  LMove := {$IFDEF RANDOM_MOVER}RandomMove(LPos){$ELSE}BestMove(LPos, LMoveTime){$ENDIF};
  LTimeElapsed := {$IFDEF FPC}GetTickCount64{$ELSE}GetTickCount{$ENDIF} - LTimeElapsed;
  if not Terminated then
  begin
    SendToUser(Format('bestmove %s', [LMove]));
    ToLog(FormatDateTime('"** Time elapsed: "hh:nn:ss:zzz', LTimeElapsed / (1000 * SECSPERDAY)), 1);
  end;
end;

const
  CStartPos = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1';
  CCapablancaStartPos = 'rnabqkbcnr/pppppppppp/10/10/10/10/PPPPPPPPPP/RNABQKBCNR w KQkq - 0 1';
  CGothicStartPos = 'rnbqckabnr/pppppppppp/10/10/10/10/PPPPPPPPPP/RNBQCKABNR w KQkq - 0 1';
  
var
  LFen, LUserCmd, LName, LValue: string;
  LMoves: TStringArray;
  LProcessus: TProcessus;
  LMTime, LWTime, LBTime, LMTG, LWInc, LBInc: integer;
  LIdx: integer;
  LVariant: string;
  
begin
{$IFDEF RANDOM_MOVER}
  Randomize;
{$ENDIF}
  ToLog(Concat(TimeToStr(Now), ' ** ', CAppInfo));
  LVariant := 'chess';
  while not EOF do
  begin
    ReadLn(LUserCmd);
    ToLog(Concat(TimeToStr(Now), ' -> ', LUserCmd));
    if LUserCmd = 'quit' then
      Break
    else
    if LUserCmd = 'uci' then
    begin
      SendToUser(Format('id name %s %s', [CAppName, CVersion]), FALSE);
      SendToUser(Format('id author %s', [CAuthor]), FALSE);
      SendToUser('option name UCI_Chess960 type check default false', FALSE);
      SendToUser('option name UCI_Capablanca type check default false', FALSE);
      SendToUser('option name UCI_Caparandom type check default false', FALSE);
      SendToUser('option name UCI_Gothic type check default false', FALSE);
      SendToUser('option name UCI_Variant type combo default chess var capablanca var caparandom var chess var fischerandom var gothic', FALSE);
      SendToUser('uciok');
    end else
    if LUserCmd = 'isready' then
      SendToUser('readyok')
    else
    if LUserCmd = 'stop' then
      SendToUser('bestmove 0000')
    else
    if IsCmdSetOption(LUserCmd, LName, LValue) then
    begin
      if LName = 'UCI_Chess960'   then LVariant := 'fischerandom' else
      if LName = 'UCI_Capablanca' then LVariant := 'capablanca'   else
      if LName = 'UCI_Caparandom' then LVariant := 'caparandom'   else
      if LName = 'UCI_Gothic'     then LVariant := 'gothic'       else
      if LName = 'UCI_Variant'    then LVariant := LValue;
    end else
    if Copy(LUserCmd, 1, 8) = 'position' then
    begin
      if IsCmdPosStartPos(LUserCmd, LMoves) then
      begin
        if LVariant = 'capablanca' then LFen := CCapablancaStartPos else
        if LVariant = 'gothic'     then LFen := CGothicStartPos     else
        LFen := CStartPos;
        InitPosition(LPos, LFen);
        LHistory.Clear;
      end else
      if IsCmdPosFen(LUserCmd, LFen, LMoves)then
      begin
        InitPosition(LPos, LFen);
        LHistory.Clear;
      end else
        ToLog(Concat(TimeToStr(Now), Format(' ** Unknown command: %s', [LUserCmd])));
      for LIdx := Low(LMoves) to High(LMoves) do
      begin
        DoMove(LPos, LMoves[LIdx]);
        LHistory.Append(LMoves[LIdx]);
      end;
    end else
    if Copy(LUserCmd, 1, 2) = 'go' then
    begin
      if IsCmdGo(LUserCmd, LWTime, LBTime, LWInc, LBinc) then // go wtime 60000 btime 60000 winc 1000 binc 1000
        LMoveTime := IfThen(LPos.color, LBinc, LWInc)
      else
      if IsCmdGo(LUserCmd, LWTime, LBTime, LMTG) then         // go wtime 59559 btime 56064 movestogo 38
        LMoveTime := IfThen(LPos.color, LBTime div LMTG, LWTime div LMTG)
      else
      if IsCmdGo(LUserCmd, LWTime, LBTime) then               // go wtime 600000 btime 600000
        LMoveTime := IfThen(LPos.color, LBTime, LWTime)
      else
      if IsCmdGo(LUserCmd, LMTime) then                       // go movetime 500
        LMoveTime := LMTime
      else
        LMoveTime := 1000;
      LProcessus := TProcessus.Create(TRUE);
      with LProcessus do
      begin
        FreeOnTerminate := TRUE;
        Priority := tpNormal;
        Start;
      end;
    end else
    if LUserCmd = 'show' then
      SendToUser(ShowPosition(LPos))
    else
      ToLog(Concat(TimeToStr(Now), Format(' ** Unknown command: %s', [LUserCmd])));
  end;
end.
