
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
  SysUtils, RegularExpressions, StrUtils;

var
  LRESetOption, LREPosStartPos, LREMove, LREPosFen: TRegEx;
  LREGo: array[1..4] of TRegEx;

function IsCmdSetOption(const AStr: string; out AName, AValue: string): boolean;
var
  LMatch: TMatch;
begin
  LMatch := LRESetOption.Match(AStr);
  result := LMatch.Success;
  if result then
  begin
    AName := LMatch.Groups.Item[1].Value;
    AValue := LMatch.Groups.Item[2].Value;
  end;
end;

function IsCmdPosStartPos(const AStr: string; var AMoves: TStringArray): boolean;
const
  CInc = 20;
var
  LMatch, LMatch2: TMatch;
  LCount: integer;
begin
  LMatch := LREPosStartPos.Match(AStr);
  LCount := 0;
  result := LMatch.Success;
  if result and (LMatch.Groups.Count = 2) then
  begin
    LMatch2 := LREMove.Match(LMatch.Groups.Item[1].Value);
    while LMatch2.Success do
    begin
      Inc(LCount);
      if Pred(LCount) > High(AMoves) then
        SetLength(AMoves, Length(AMoves) + CInc);
      AMoves[Pred(LCount)] := LMatch2.Value;
      LMatch2 := LMatch2.NextMatch;
    end;
  end;
  SetLength(AMoves, LCount);
end;

function IsCmdPosFen(const AStr: string; out AFen: string; var AMoves: TStringArray): boolean;
const
  CInc = 20;
var
  LMatch, LMatch2: TMatch;
  LCount: integer;
begin
  LMatch := LREPosFen.Match(AStr);
  LCount := 0;
  result := LMatch.Success;
  if result then
    AFen := LMatch.Groups.Item[1].Value;
  if result and (LMatch.Groups.Count = 3) then
  begin
    LMatch2 := LREMove.Match(LMatch.Groups.Item[2].Value);
    while LMatch2.Success do
    begin
      Inc(LCount);
      if Pred(LCount) > High(AMoves) then
        SetLength(AMoves, Length(AMoves) + CInc);
      AMoves[Pred(LCount)] := LMatch2.Value;
      LMatch2 := LMatch2.NextMatch;
    end;
  end;
  SetLength(AMoves, LCount);
end;

function IsCmdGo(const AStr: string; out MTime: integer): boolean; // go movetime 500
var
  LMatch: TMatch;
begin
  LMatch := LREGo[1].Match(AStr);
  result := LMatch.Success;
  if result then
  begin
    MTime := StrToInt(LMatch.Groups.Item[1].Value);
  end;
end;

function IsCmdGo(const AStr: string; out WTime, BTime: integer): boolean; // go wtime 600000 btime 600000
var
  LMatch: TMatch;
  iw, ib: integer;
begin
  LMatch := LREGo[2].Match(AStr);
  result := LMatch.Success;
  if result then
  begin
    if LMatch.Groups.Item[1].Value = 'w' then
    begin
      iw := 2;
      ib := 4;
    end else
    begin
      iw := 4;
      ib := 2;
    end;
    WTime := StrToInt(LMatch.Groups.Item[iw].Value);
    BTime := StrToInt(LMatch.Groups.Item[ib].Value);
  end;
end;

function IsCmdGo(const AStr: string; out WTime, BTime, MTG: integer): boolean; // go wtime 59559 btime 56064 movestogo 38
var
  LMatch: TMatch;
  iw, ib: integer;
begin
  LMatch := LREGo[3].Match(AStr);
  result := LMatch.Success;
  if result then
  begin
    if LMatch.Groups.Item[1].Value = 'w' then
    begin
      iw := 2;
      ib := 4;
    end else
    begin
      iw := 4;
      ib := 2;
    end;
    WTime := StrToInt(LMatch.Groups.Item[iw].Value);
    BTime := StrToInt(LMatch.Groups.Item[ib].Value);
    MTG   := StrToInt(LMatch.Groups.Item[5].Value);
  end;
end;

function IsCmdGo(const AStr: string; out WTime, BTime, WInc, BInc: integer): boolean; // go wtime 60000 btime 60000 winc 1000 binc 1000
var
  LMatch: TMatch;
  iw, ib, jw, jb: integer;
begin
  LMatch := LREGo[4].Match(AStr);
  result := LMatch.Success;
  if result then
  begin
    if LMatch.Groups.Item[1].Value = 'w' then
    begin
      iw := 2;
      ib := 4;
    end else
    begin
      iw := 4;
      ib := 2;
    end;
    if LMatch.Groups.Item[5].Value = 'w' then
    begin
      jw := 6;
      jb := 8;
    end else
    begin
      jw := 8;
      jb := 6;
    end;
    WTime := StrToInt(LMatch.Groups.Item[iw].Value);
    BTime := StrToInt(LMatch.Groups.Item[ib].Value);
    WInc  := StrToInt(LMatch.Groups.Item[jw].Value);
    BInc  := StrToInt(LMatch.Groups.Item[jb].Value);
  end;
end;

initialization
  LRESetOption.Create('setoption name (.+) value (\w+)');
  LREPosStartPos.Create('position startpos( moves .+)?');
  LREMove.Create('[a-j]\d[a-j]\d[rnbqac]?');
  LREPosFen.Create('position fen ([0-9ABCKNPQRabcknpqr/]+ [wb] [KQkqA-Ja-j\-]+ [a-h36\-] \d+ \d+)( moves .+)?');
  LREGo[1].Create('go movetime (\d+)');
  LREGo[2].Create('go ([wb])time (\d+) ([wb])time (\d+)');
  LREGo[3].Create('go ([wb])time (\d+) ([wb])time (\d+) movestogo (\d+)');
  LREGo[4].Create('go ([wb])time (\d+) ([wb])time (\d+) ([wb])inc (\d+) ([wb])inc (\d+)');

end.
