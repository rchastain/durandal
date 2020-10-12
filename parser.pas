
unit Parser;

interface

type
  TStringArray = array of string;

function IsCmdSetOption(const AStr: string; out AName, AValue: string): boolean;
function IsCmdPosStartPos(const AStr: string; var AMoves: TStringArray): boolean;
function IsCmdPosFen(const AStr: string; out AFen: string; var AMoves: TStringArray): boolean;
function IsCmdGo(const AStr: string; out MTime: integer): boolean; overload; // go movetime 500
function IsCmdGo(const AStr: string; out WTime, BTime: integer): boolean; overload; // go wtime 600000 btime 600000
function IsCmdGo(const AStr: string; out WTime, BTime, MTG: integer): boolean; overload; // go wtime 59559 btime 56064 movestogo 38
function IsCmdGo(const AStr: string; out WTime, BTime, WInc, BInc: integer): boolean; overload; // go wtime 60000 btime 60000 winc 1000 binc 1000

implementation

uses
  SysUtils, RegExpr;

var
  LRESetOption,{ LREPosStartPos,} LREMove, LREPosFen: TRegExpr;
  LREGo: array[1..4] of TRegExpr;

function IsCmdSetOption(const AStr: string; out AName, AValue: string): boolean;
begin
  result := LRESetOption.Exec(AStr);
  if result then
  begin
    AName := LRESetOption.Match[1];
    AValue := LRESetOption.Match[2];
  end;
end;

function IsCmdPosStartPos(const AStr: string; var AMoves: TStringArray): boolean;
const
  CInc = 20;
var
  LCount: integer;
begin
  result := Pos('position startpos', AStr) = 1;
  LCount := 0;
  if LREMove.Exec(AStr) then
  repeat
    Inc(LCount);
    if Pred(LCount) > High(AMoves) then
      SetLength(AMoves, Length(AMoves) + CInc);
    AMoves[Pred(LCount)] := LREMove.Match[0];
  until not LREMove.ExecNext;
  SetLength(AMoves, LCount);
end;

function IsCmdPosFen(const AStr: string; out AFen: string; var AMoves: TStringArray): boolean;
const
  CInc = 20;
var
  LCount: integer;
begin
  result := LREPosFen.Exec(AStr);
  if result then
  begin
    AFen := LREPosFen.Match[1];
    LCount := 0;
    if (LREPosFen.Match[2] <> '')
    and LREMove.Exec(LREPosFen.Match[2]) then
    repeat
      Inc(LCount);
      if Pred(LCount) > High(AMoves) then
        SetLength(AMoves, Length(AMoves) + CInc);
      AMoves[Pred(LCount)] := LREMove.Match[0];
    until not LREMove.ExecNext;
    SetLength(AMoves, LCount);
  end;
end;

function IsCmdGo(const AStr: string; out MTime: integer): boolean; // go movetime 500
begin
  result := LREGo[1].Exec(AStr);
  if result then
    MTime := StrToInt(LREGo[1].Match[1]);
end;

function IsCmdGo(const AStr: string; out WTime, BTime: integer): boolean; // go wtime 600000 btime 600000
var
  iw, ib: integer;
begin
  result := LREGo[2].Exec(AStr);
  if result then
  begin
    if LREGo[2].Match[1] = 'w' then
    begin
      iw := 2;
      ib := 4;
    end else
    begin
      iw := 4;
      ib := 2;
    end;
    WTime := StrToInt(LREGo[2].Match[iw]);
    BTime := StrToInt(LREGo[2].Match[ib]);
  end;
end;

function IsCmdGo(const AStr: string; out WTime, BTime, MTG: integer): boolean; // go wtime 59559 btime 56064 movestogo 38
var
  iw, ib: integer;
begin
  result := LREGo[3].Exec(AStr);
  if result then
  begin
    if LREGo[3].Match[1] = 'w' then
    begin
      iw := 2;
      ib := 4;
    end else
    begin
      iw := 4;
      ib := 2;
    end;
    WTime := StrToInt(LREGo[3].Match[iw]);
    BTime := StrToInt(LREGo[3].Match[ib]);
    MTG := StrToInt(LREGo[3].Match[5]);
  end;
end;

function IsCmdGo(const AStr: string; out WTime, BTime, WInc, BInc: integer): boolean; // go wtime 60000 btime 60000 winc 1000 binc 1000
var
  iw, ib, jw, jb: integer;
begin
  result := LREGo[4].Exec(AStr);
  if result then
  begin
    if LREGo[4].Match[1] = 'w' then
    begin
      iw := 2;
      ib := 4;
    end else
    begin
      iw := 4;
      ib := 2;
    end;
    if LREGo[4].Match[5] = 'w' then
    begin
      jw := 6;
      jb := 8;
    end else
    begin
      jw := 8;
      jb := 6;
    end;
    WTime := StrToInt(LREGo[4].Match[iw]);
    BTime := StrToInt(LREGo[4].Match[ib]);
    WInc := StrToInt(LREGo[4].Match[jw]);
    BInc := StrToInt(LREGo[4].Match[jb]);
  end;
end;

initialization
  LRESetOption := TRegExpr.Create('setoption name (.+) value (\w+)');
  LREMove := TRegExpr.Create('[a-j]\d[a-j]\d[rnbqac]?');
  LREPosFen := TRegExpr.Create('position fen ([0-9ABCKNPQRabcknpqr/]+ [wb] [KQkqA-Ja-j\-]+ [a-h36\-]+ \d+ \d+)( moves .+)?');
  LREGo[1] := TRegExpr.Create('go movetime (\d+)');
  LREGo[2] := TRegExpr.Create('go ([wb])time (\d+) ([wb])time (\d+)');
  LREGo[3] := TRegExpr.Create('go ([wb])time (\d+) ([wb])time (\d+) movestogo (\d+)');
  LREGo[4] := TRegExpr.Create('go ([wb])time (\d+) ([wb])time (\d+) ([wb])inc (\d+) ([wb])inc (\d+)');
  
finalization
  LRESetOption.Free;
  LREMove.Free;
  LREPosFen.Free;
  LREGo[1].Free;
  LREGo[2].Free;
  LREGo[3].Free;
  LREGo[4].Free;

end.
