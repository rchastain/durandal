
unit Chess;
{$ASSERTIONS ON}

interface

type
  TBoard = array[0..209] of char;
  TCastling = array[0..3] of integer;
  TPosition = record
    board: TBoard;       { Piece placement.          }
    color: boolean;      { Active color.             }
    castling: TCastling; { Castling availability.    }
    enpassant: string;   { En passant target square. }
    halfmove,            { Halfmove clock.           }
    fullmove: integer;   { Fullmove number.          }
  end;
  TPieceType = (
    ptNil,
    ptPawn,
    ptKnight,
    ptBishop,
    ptRook,
    ptArchbishop,
    ptChancelor,
    ptQueen,
    ptKing
  );
  TMove = integer;
  
function ShowPosition(const APos: TPosition): string;
function SquareToStr(const x, y: integer): string; overload;
function SquareToStr(const i: integer): string; overload;
function EncodeMove(const AFrom, ATo: byte; const APromo: TPieceType): TMove;
procedure DecodeMove(const AMove: TMove; out AFrom, ATo: byte; out APromo: TPieceType); overload;
procedure DecodeMove(const AMove: string; out AFrom, ATo: byte; out APromo: TPieceType); overload;
function MoveToStr(const AMove: TMove): string;
procedure InitBoard(var ABoard: TBoard; const AFen: string);
function EncodeCastling(const AFen: string; const ABoard: TBoard): TCastling;
procedure InitPosition(var APos: TPosition; const AFen: string);
procedure GenPieceMoves(var AList: array of TMove; var ACount: integer; const APos: TPosition; const ASqr: integer; const AVect: array of integer; const ARepeat: boolean);
procedure GenPawnMoves(var AList: array of TMove; var ACount: integer; const APos: TPosition; const ASqr: integer; const AVect: array of integer; const ACheckDetectionMode: boolean = FALSE);
procedure GenCastling(var AList: array of TMove; var ACount: integer; const APos: TPosition; const ASqr: integer);
procedure GenMoves(var AList: array of TMove; var ACount: integer; const APos: TPosition; const ACheckDetectionMode: boolean = FALSE);
function DoMove(var APos: TPosition; const AMove: TMove): boolean; overload;
function DoMove(var APos: TPosition; const AMove: string): boolean; overload;
function Eval(const APos: TPosition): integer;
function IsCheck(const APos: TPosition): boolean;
function BestMove(const APos: TPosition; const ATime: cardinal): string;
  
implementation

uses
  SysUtils, Classes, Math, Vectors, Log;

function FindChar(const s1, s2: string): char;
var
  i: integer;
begin
  result := '-';
  for i := 1 to Length(s1) do
    if Pos(s1[i], s2) > 0 then
      Exit(s1[i]);
end;

function ShowPosition(const APos: TPosition): string;
const
  CColorName: array[boolean] of string = ('White', 'Black');
  CWhiteRookFile = '-ABCDEFGHIJ';
  CBlackRookFile = '-abcdefghij';
var
  x, y: integer;
  c: char;
begin
  result := '+  A B C D E F G H I J'#13#10;
  for y := 7 downto 0 do
  begin
    result := Concat(result, Chr(y + Ord('1')), ' ');
    for x := 0 to 9 do
    begin
      c := APos.board[15 * x + y + 35];
      result := Concat(result, ' ', c);
    end;
    result := Concat(result, #13#10);
  end;
  result := Format(
    '%sActive color: %s'#13#10'Castling right: %s%s%s%s'#13#10'En passant: %s',
    [
      result,
      CColorName[APos.color],
      CWhiteRookFile[APos.castling[0] + 2],
      CWhiteRookFile[APos.castling[1] + 2],
      CBlackRookFile[APos.castling[2] + 2],
      CBlackRookFile[APos.castling[3] + 2],
      APos.enpassant
    ]
  );
end;

const
  COutside = '#';
  CEmptySquare = '.';
  CNil = -1;
  AKT = 2;                                 { Alpha side king target square. }
  OKT: array[boolean] of integer = (6, 8); { Omega side king target square. }
  ART = 3;                                 { Alpha side rook target square. }
  ORT: array[boolean] of integer = (5, 7); { Omega side rook target square. }

function SquareToStr(const x, y: integer): string; overload;
begin
  result := Chr(x + Ord('a')) + Chr(y + Ord('1'));
end;

function SquareToStr(const i: integer): string; overload;
begin
  result := SquareToStr((i - 35) div 15, (i - 35) mod 15);
end;

function EncodeMove(const AFrom, ATo: byte; const APromo: TPieceType): TMove;
begin
  result := (AFrom shl 24) or (ATo shl 16) or (Ord(APromo) shl 8);
end;

procedure DecodeMove(const AMove: TMove; out AFrom, ATo: byte; out APromo: TPieceType);
begin
  AFrom  := (AMove and $FF000000) shr 24;
  ATo    := (AMove and $00FF0000) shr 16;
  APromo := TPieceType((AMove and $0000FF00) shr 8);
end;

function CharToPieceType(const c: char): TPieceType;
begin
  case UpCase(c) of
    'A': result := ptArchbishop;
    'B': result := ptBishop;
    'C': result := ptChancelor;
    'K': result := ptKing;
    'N': result := ptKnight;
    'P': result := ptPawn;
    'Q': result := ptQueen;
    'R': result := ptRook;
    else
      result := ptNil;
  end
end;

procedure DecodeMove(const AMove: string; out AFrom, ATo: byte; out APromo: TPieceType);
var
  x, y: integer;
begin
  x := Ord(AMove[1]) - Ord('a');
  y := Ord(AMove[2]) - Ord('1');
  AFrom := 15 * x + y + 35;
  x := Ord(AMove[3]) - Ord('a');
  y := Ord(AMove[4]) - Ord('1');
  ATo := 15 * x + y + 35;
  if Length(AMove) = 5 then
    case UpCase(AMove[5]) of
      'R': APromo := ptRook;
      'N': APromo := ptKnight;
      'A': APromo := ptArchbishop;
      'B': APromo := ptBishop;
      'Q': APromo := ptQueen;
      'C': APromo := ptChancelor;
    end
  else
    APromo := ptNil;
end;

function MoveToStr(const AMove: TMove): string;
var
  LFrom, LTo: byte;
  LType: TPieceType;
begin
  DecodeMove(AMove, LFrom, LTo, LType);
  result := SquareToStr(LFrom) + SquareToStr(LTo);
  if LType <> ptNil then
    case LType of
      ptRook:       result := result + 'r';
      ptKnight:     result := result + 'n';
      ptArchbishop: result := result + 'a';
      ptBishop:     result := result + 'b';
      ptQueen:      result := result + 'q';
      ptChancelor:  result := result + 'c';
    end;
end;

procedure InitBoard(var ABoard: TBoard; const AFen: string);
var
  i, j: integer;
  x, y: integer;
begin
  for i := Low(ABoard) to High(ABoard) do
    ABoard[i] := COutside;
  i := 1;
  x := 0;
  y := 7;
  while (i <= Length(AFen)) and (AFen[i] <> ' ') do
  begin
    case AFen[i] of
      'p', 'r', 'n', 'a', 'b', 'q', 'k', 'c', 'P', 'R', 'N', 'A', 'B', 'Q', 'K', 'C':
        begin
          ABoard[15 * x + y + 35] := AFen[i];
          Inc(x);
        end;
      '/':
        begin
          x := 0;
          Dec(y);
        end;
      '0'..'9':
        if (i < Length(AFen)) and (AFen[i] = '1') and (AFen[Succ(i)] = '0') then
        begin
          for j := 1 to 10 do
          begin
            ABoard[15 * x + y + 35] := CEmptySquare;
            Inc(x);
          end;
          Inc(i);
        end else
          for j := 1 to Ord(AFen[i]) - Ord('0') do
          begin
            ABoard[15 * x + y + 35] := CEmptySquare;
            Inc(x);
          end;
    end;
    Inc(i);
  end;
end;

function EncodeCastling(const AFen: string; const ABoard: TBoard): TCastling;
var
  xk, xr: integer;
  c: char;
begin
  result[0] := CNil;
  result[1] := CNil;
  result[2] := CNil;
  result[3] := CNil;
  if FindChar(AFen, 'ABCDEFGHIJKQ') <> '-' then
  begin
    xk := 0;
	  while (xk <= 9) and (ABoard[15 * xk + 0 + 35] <> 'K') do Inc(xk);
	  Assert(xk <= 9);
    if Pos('K', AFen) > 0 then c := 'K' else c := FindChar(AFen, Copy('ABCDEFGHIJ', xk + 2));
    if c <> '-' then for xr := 9 downto xk do if (ABoard[15 * xr + 0 + 35] = 'R') and ((c = 'K') or (c = Chr(xr + Ord('A')))) then
      result[0] := xr;
    if Pos('Q', AFen) > 0 then c := 'Q' else c := FindChar(AFen, Copy('ABCDEFGHIJ', 1, xk));
    if c <> '-' then for xr := 0 to xk do if (ABoard[15 * xr + 0 + 35] = 'R') and ((c = 'Q') or (c = Chr(xr + Ord('A')))) then
      result[1] := xr;
  end;
  if FindChar(AFen, 'abcdefghijkq') <> '-' then
  begin
    xk := 0;
	  while (xk <= 9) and (ABoard[15 * xk + 7 + 35] <> 'k') do Inc(xk);
	  Assert(xk <= 9);
    if Pos('k', AFen) > 0 then c := 'k' else c := FindChar(AFen, Copy('abcdefghij', xk + 2));
    if c <> '-' then for xr := 9 downto xk do if (ABoard[15 * xr + 7 + 35] = 'r') and ((c = 'k') or (c = Chr(xr + Ord('a')))) then
      result[2] := xr;
    if Pos('q', AFen) > 0 then c := 'q' else c := FindChar(AFen, Copy('abcdefghij', 1, xk));
    if c <> '-' then for xr := 0 to xk do if (ABoard[15 * xr + 7 + 35] = 'r') and ((c = 'q') or (c = Chr(xr + Ord('a')))) then
      result[3] := xr;
  end;
end;

procedure InitPosition(var APos: TPosition; const AFen: string);
var
  ls: TStringList;
begin
  ls := TStringList.Create;
  ls.DelimitedText := AFen;
  InitBoard(APos.board, ls[0]);
  APos.color := ls[1] = 'b';
  APos.castling := EncodeCastling(ls[2], APos.board);
  APos.enpassant := ls[3];
  APos.halfmove := StrToInt(ls[4]);
  APos.fullmove := StrToInt(ls[5]);
  ls.Free;
end;

procedure GenPieceMoves(var AList: array of TMove; var ACount: integer; const APos: TPosition; const ASqr: integer; const AVect: array of integer; const ARepeat: boolean);
var
  i, LSqr: integer;
begin
  for i := Low(AVect) to High(AVect) do
  begin
    LSqr := ASqr;
    while APos.board[LSqr + AVect[i]] <> COutside do
    begin
      Inc(LSqr, AVect[i]);
      if (     APos.color  and CharInSet(APos.board[LSqr], ['a'..'z']))
      or ((not APos.color) and CharInset(APos.board[LSqr], ['A'..'Z'])) then
        Break;
      Inc(ACount);
      AList[Pred(ACount)] := EncodeMove(ASqr, LSqr, ptNil);
      if (not ARepeat) or (APos.board[LSqr] <> CEmptySquare) then
        Break;
    end;
  end;
end;

procedure GenPawnMoves(var AList: array of TMove; var ACount: integer; const APos: TPosition; const ASqr: integer; const AVect: array of integer; const ACheckDetectionMode: boolean);
var
  i, LSqr, y: integer;
  LType: TPieceType;
begin
  if not ACheckDetectionMode then
  begin
    LSqr := ASqr;
    Inc(LSqr, AVect[0]);
    if APos.board[LSqr] = CEmptySquare then
    begin
      Inc(ACount);
      y := (LSqr - 35) mod 15;
      if (y = 0) or (y = 7) then LType := ptQueen else LType := ptNil;
      AList[Pred(ACount)] := EncodeMove(ASqr, LSqr, LType);
      if (AVect[0] =  1) and ((ASqr - 35) mod 15 = 1)
      or (AVect[0] = -1) and ((ASqr - 35) mod 15 = 6) then
      begin
        Inc(LSqr, AVect[0]);
        if APos.board[LSqr] = CEmptySquare then
        begin
          Inc(ACount);
          AList[Pred(ACount)] := EncodeMove(ASqr, LSqr, ptNil);
        end;
      end;
    end;
  end;
  for i := 1 to 2 do
  begin
    LSqr := ASqr;
    Inc(LSqr, AVect[i]);
    if (AVect[0] =  1) and CharInSet(APos.board[LSqr], ['a'..'z'])
    or (AVect[0] = -1) and CharInSet(APos.board[LSqr], ['A'..'Z'])
    or (APos.board[LSqr] = CEmptySquare) and (SquareToStr(LSqr) = APos.enpassant)
    or (APos.board[LSqr] = CEmptySquare) and ACheckDetectionMode then
    begin
      Inc(ACount);
      y := (LSqr - 35) mod 15;
      if (y = 0) or (y = 7) then LType := ptQueen else LType := ptNil;
      AList[Pred(ACount)] := EncodeMove(ASqr, LSqr, LType);
    end;
  end;
end;
  
procedure GenCastling(var AList: array of TMove; var ACount: integer; const APos: TPosition; const ASqr: integer);
var
  x, xk, yk, xr: integer;
  LFirst, LLast: integer;
  LMoveList: array[0..99] of TMove;
  LMoveCount: integer;
  LPos: TPosition;
  LIndex: integer;
  LFrom, LTo: byte;
  LType: TPieceType;
  LAttackedSquares: array[0..9] of boolean;
  LImpossible, LCapablanca: boolean;
begin
  xk := (ASqr - 35) div 15;
  yk := (ASqr - 35) mod 15;
  
  LPos := APos;
  LPos.color := not LPos.color;
  LMoveCount := 0;
  GenMoves(LMoveList, LMoveCount, LPos, TRUE);
  for x := 0 to 9 do
    LAttackedSquares[x] := FALSE;
  for LIndex := 0 to Pred(LMoveCount) do
  begin
    DecodeMove(LMoveList[LIndex], LFrom, LTo, LType);
    if (LTo - 35) mod 15 = yk then
      LAttackedSquares[(LTo - 35) div 15] := TRUE;
  end;
  
  LCapablanca := APos.board[155] <> COutside;
  
  if (not APos.color) and (APos.castling[0] <> CNil) then
  begin
    xr := APos.castling[0];
    Assert(yk = 0);
    Assert(APos.board[15 * xr + yk + 35] = 'R');
    LFirst := Min(xk, ORT[LCapablanca]);
    LLast  := Max(xr, OKT[LCapablanca]);
    LImpossible := FALSE;
    for x := LFirst to LLast do if (x <> xk) and (x <> xr) and (APos.board[15 * x + yk + 35] <> CEmptySquare) then
    begin
      ToLog(Format('Roque impossible case occupée (%d).', [15 * x + yk + 35]));
      LImpossible := TRUE;
      Break;
    end;
    if not LImpossible then for x := xk to OKT[LCapablanca] do if LAttackedSquares[x] then
    begin
      ToLog(Format('Roque impossible case menacée (%d).', [15 * x + yk + 35]));
      LImpossible := TRUE;
      Break;
    end;
    if not LImpossible then
    begin
      Inc(ACount);
      AList[Pred(ACount)] := EncodeMove(ASqr, 15 * xr + yk + 35, ptNil);
    end;
  end;
  
  if (not APos.color) and (APos.castling[1] <> CNil) then
  begin
    xr := APos.castling[1];
    Assert(yk = 0);
    Assert(APos.board[15 * xr + yk + 35] = 'R');
    LFirst := Min(xr, AKT);
    LLast  := Max(xk, ART);
    LImpossible := FALSE;
    for x := LFirst to LLast do if (x <> xk) and (x <> xr) and (APos.board[15 * x + yk + 35] <> CEmptySquare) then
    begin
      ToLog(Format('Roque impossible case occupée (%d).', [15 * x + yk + 35]));
      LImpossible := TRUE;
      Break;
    end;
    if not LImpossible then for x := xk downto AKT do if LAttackedSquares[x] then
    begin
      ToLog(Format('Roque impossible case menacée (%d).', [15 * x + yk + 35]));
      LImpossible := TRUE;
      Break;
    end;
    if not LImpossible then
    begin
      Inc(ACount);
      AList[Pred(ACount)] := EncodeMove(ASqr, 15 * xr + yk + 35, ptNil);
    end;
  end;
  
  if APos.color and (APos.castling[2] <> CNil) then
  begin
    xr := APos.castling[2];
    Assert(yk = 7);
    Assert(APos.board[15 * xr + yk + 35] = 'r');
    LFirst := Min(xk, ORT[LCapablanca]);
    LLast  := Max(xr, OKT[LCapablanca]);
    LImpossible := FALSE;
    for x := LFirst to LLast do if (x <> xk) and (x <> xr) and (APos.board[15 * x + yk + 35] <> CEmptySquare) then
    begin
      ToLog(Format('Roque impossible case occupée (%d).', [15 * x + yk + 35]));
      LImpossible := TRUE;
      Break;
    end;
    if not LImpossible then for x := xk to OKT[LCapablanca] do if LAttackedSquares[x] then
    begin
      ToLog(Format('Roque impossible case menacée (%d).', [15 * x + yk + 35]));
      LImpossible := TRUE;
      Break;
    end;
    if not LImpossible then
    begin
      Inc(ACount);
      AList[Pred(ACount)] := EncodeMove(ASqr, 15 * xr + yk + 35, ptNil);
    end;
  end;
  
  if APos.color and (APos.castling[3] <> CNil) then
  begin
    xr := APos.castling[3];
    Assert(yk = 7);
    Assert(APos.board[15 * xr + yk + 35] = 'r');
    LFirst := Min(xr, AKT);
    LLast  := Max(xk, ART);
    LImpossible := FALSE;
    for x := LFirst to LLast do if (x <> xk) and (x <> xr) and (APos.board[15 * x + yk + 35] <> CEmptySquare) then
    begin
      ToLog(Format('Roque impossible case occupée (%d).', [15 * x + yk + 35]));
      LImpossible := TRUE;
      Break;
    end;
    if not LImpossible then for x := xk downto AKT do if LAttackedSquares[x] then
    begin
      ToLog(Format('Roque impossible case menacée (%d).', [15 * x + yk + 35]));
      LImpossible := TRUE;
      Break;
    end;
    if not LImpossible then
    begin
      Inc(ACount);
      AList[Pred(ACount)] := EncodeMove(ASqr, 15 * xr + yk + 35, ptNil);
    end;
  end;
end;

procedure GenMoves(var AList: array of TMove; var ACount: integer; const APos: TPosition; const ACheckDetectionMode: boolean);
var
  x, y, i: integer;
  c: char;
begin
  for x := 0 to 9 do for y := 0 to 7 do
  begin
    i := 15 * x + y + 35;
    c := APos.board[i];
    if (APos.color and CharInSet(c, ['a'..'z'])) or ((not APos.color) and CharInSet(c, ['A'..'Z'])) then
      case UpCase(c) of
        'P':
          case c of
            'P': GenPawnMoves(AList, ACount, APos, i, CVecWhitePawn, ACheckDetectionMode);
            'p': GenPawnMoves(AList, ACount, APos, i, CVecBlackPawn, ACheckDetectionMode);
          end;
        'R': GenPieceMoves(AList, ACount, APos, i, CVecRook, TRUE);
        'N': GenPieceMoves(AList, ACount, APos, i, CVecKnight, FALSE);
        'A':
          begin
            GenPieceMoves(AList, ACount, APos, i, CVecKnight, FALSE);
            GenPieceMoves(AList, ACount, APos, i, CVecBishop, TRUE);
          end;
        'B': GenPieceMoves(AList, ACount, APos, i, CVecBishop, TRUE);
        'Q':
          begin
            GenPieceMoves(AList, ACount, APos, i, CVecRook, TRUE);
            GenPieceMoves(AList, ACount, APos, i, CVecBishop, TRUE);
          end;
        'K':
          begin
            GenPieceMoves(AList, ACount, APos, i, CVecRook, FALSE);
            GenPieceMoves(AList, ACount, APos, i, CVecBishop, FALSE);
            if not ACheckDetectionMode then
              GenCastling(AList, ACount, APos, i);
          end;
        'C':
          begin
            GenPieceMoves(AList, ACount, APos, i, CVecRook, TRUE);
            GenPieceMoves(AList, ACount, APos, i, CVecKnight, FALSE);
          end;
      end;
  end;
end;
  
function DoMove(var APos: TPosition; const AMove: TMove): boolean;
var
  i, j: byte;
  k: TPieceType;
  x1, y1, x2, y2: integer;
  king, rook: char;
begin
  result := TRUE;
  DecodeMove(AMove, i, j, k);
  x1 := (i - 35) div 15;
  y1 := (i - 35) mod 15;
  x2 := (j - 35) div 15;
  y2 := (j - 35) mod 15;
  
  if APos.board[i] = 'K' then
  begin
    APos.castling[0] := CNil;
    APos.castling[1] := CNil;
  end;
  if APos.board[i] = 'k' then
  begin
    APos.castling[2] := CNil;
    APos.castling[3] := CNil;
  end;
  if (APos.board[i] = 'R') and (y1 = 0) then
  begin
    if x1 = APos.castling[0] then APos.castling[0] := CNil;
    if x1 = APos.castling[1] then APos.castling[1] := CNil;
  end;
  if (APos.board[i] = 'r') and (y1 = 7) then
  begin
    if x1 = APos.castling[2] then APos.castling[2] := CNil;
    if x1 = APos.castling[3] then APos.castling[3] := CNil;
  end;
  if (APos.board[j] = 'R') and (y2 = 0) then
  begin
    if x2 = APos.castling[0] then APos.castling[0] := CNil;
    if x2 = APos.castling[1] then APos.castling[1] := CNil;
  end;
  if (APos.board[j] = 'r') and (y2 = 7) then
  begin
    if x2 = APos.castling[2] then APos.castling[2] := CNil;
    if x2 = APos.castling[3] then APos.castling[3] := CNil;
  end;
  
  if ((APos.board[i] = 'K') and (APos.board[j] = 'R'))
  or ((APos.board[i] = 'k') and (APos.board[j] = 'r')) then
  begin
    king := APos.board[i];
    rook := APos.board[j];
    APos.board[i] := CEmptySquare;
    APos.board[j] := CEmptySquare;
    if j > i then
    begin
      APos.board[15 * OKT[APos.board[155] <> COutside] + y1 + 35] := king;
      APos.board[15 * ORT[APos.board[155] <> COutside] + y1 + 35] := rook;
    end else
    begin
      APos.board[15 * AKT + y1 + 35] := king;
      APos.board[15 * ART + y1 + 35] := rook;
    end;
    Inc(APos.halfmove);
    if APos.color then
      Inc(APos.fullmove);
    APos.color := not APos.color;
	  Exit;
  end;
  
  APos.EnPassant := '-';
  if ((APos.board[i] = 'P') or (APos.board[i] = 'p')) then
  begin
    if (x2 <> x1) and (APos.board[j] = CEmptySquare) then
      APos.board[15 * x2 + y1 + 35] := CEmptySquare;
    if (APos.board[i] = 'P') and (j - i = 2) then APos.EnPassant := SquareToStr(j - 1);
    if (APos.board[i] = 'p') and (i - j = 2) then APos.EnPassant := SquareToStr(j + 1);
  end;
  
  if (APos.board[i] = 'P') or (APos.board[i] = 'p') or (APos.board[j] <> CEmptySquare) then
    APos.halfmove := 0
  else
    Inc(APos.halfmove);
  if APos.color then
    Inc(APos.fullmove);
  
  if k = ptNil then
    APos.board[j] := APos.board[i]
  else
  begin
    case k of
      ptRook:       APos.board[j] := 'r';
      ptKnight:     APos.board[j] := 'n';
      ptArchbishop: APos.board[j] := 'a';
      ptBishop:     APos.board[j] := 'b';
      ptQueen:      APos.board[j] := 'q';
      ptChancelor:  APos.board[j] := 'c';
    end;
    if not APos.color then APos.board[j] := UpCase(APos.board[j]);
  end;
  APos.board[i] := CEmptySquare;
  
  APos.color := not APos.color;
end;

function DoMove(var APos: TPosition; const AMove: string): boolean;
var
  i, j: byte;
  pt: TPieceType;
  m: TMove;
  LMove: string;
begin
  DecodeMove(AMove, i, j, pt);
  if (APos.board[i] = 'K') then
  begin
    LMove := '';
    if AMove = 'e1g1' then LMove := 'e1h1';
	  if AMove = 'e1c1' then LMove := 'e1a1';
    if AMove = 'f1i1' then LMove := 'f1j1';
	  if AMove = 'f1c1' then LMove := 'f1a1';
	  if LMove <> '' then DecodeMove(LMove, i, j, pt);
  end;
  if (APos.board[i] = 'k') then
  begin
    LMove := '';
    if AMove = 'e8g8' then LMove := 'e8h8';
	  if AMove = 'e8c8' then LMove := 'e8a8';
    if AMove = 'f8i8' then LMove := 'f8j8';
	  if AMove = 'f8c8' then LMove := 'f8a8';
	  if LMove <> '' then DecodeMove(LMove, i, j, pt);
  end;
  m := EncodeMove(i, j, pt);
  result := DoMove(APos, m);
end;

function Eval(const APos: TPosition): integer;
var
  x, y: integer;
  c: char;
begin
  result := 0;
  for y := 7 downto 0 do
    for x := 0 to 9 do
    begin
      c := APos.board[15 * x + y + 35];
      case c of
        'p': Dec(result, 100);
        'r': Dec(result, 500);
        'n': Dec(result, 300);
        'a': Dec(result, 600);
        'b': Dec(result, 300);
        'q': Dec(result, 800);
        'k': Dec(result, 10000);
        'c': Dec(result, 800);
        'P': Inc(result, 100);
        'R': Inc(result, 500);
        'N': Inc(result, 300);
        'A': Inc(result, 600);
        'B': Inc(result, 300);
        'Q': Inc(result, 800);
        'K': Inc(result, 10000);
        'C': Inc(result, 800);
      end;
    end;
  if APos.color then
    result := -1 * result;
end;

procedure Sort(var AList: array of TMove; var AEval: array of integer; const ACount: integer);
var
  LSorted: boolean;
  LEval: integer;
  LMove: TMove;
  LIndex: integer;
begin
  LSorted := FALSE;
  while not LSorted do
  begin
    LSorted := TRUE;
    for LIndex := 0 to Pred(ACount) do
      if (LIndex < Pred(ACount)) and (AEval[LIndex] < AEval[Succ(LIndex)]) then
      begin
        LSorted := FALSE;
        LEval := AEval[LIndex];
        LMove := AList[LIndex];
        AEval[LIndex] := AEval[Succ(LIndex)];
        AList[LIndex] := AList[Succ(LIndex)];
        AEval[Succ(LIndex)] := LEval;
        AList[Succ(LIndex)] := LMove;
      end;
  end;
end;

function BestMovesCount(const AEval: array of integer; const ACount: integer): integer;
begin
 result := 1;
 while (result < ACount) and (AEval[result] = AEval[0]) do
   Inc(result);
end;

function ArrayToStr(const AList: array of TMove; const AEval: array of integer; const ACount: integer): string;
var
  LIndex: integer;
begin
  result := '';
  for LIndex := 0 to Pred(ACount) do
    result := Format('%s%s(%d) ', [result, MoveToStr(AList[LIndex]), AEval[LIndex]]);
end;

function IsCheck(const APos: TPosition): boolean;
var
  LPos: TPosition;
  LList: array[0..99] of TMove;
  LCount: integer;
  LIndex: integer;
  i, j: byte;
  pt: TPieceType;
begin
  LPos := APos;
  LPos.color := not LPos.color;
  LCount := 0;
  GenMoves(LList, LCount, LPos, TRUE);
  result := FALSE;
  for LIndex := 0 to Pred(LCount) do
  begin
    DecodeMove(LList[LIndex], i, j, pt);
    if UpCase(LPos.board[j]) = 'K' then
      Exit(TRUE);
  end;
end;

function FirstEval(const APos: TPosition; const AMove: TMove): integer;
var
  LPos: TPosition;
  LList: array[0..99] of TMove;
  LCount: integer;
  LIndex: integer;
  LPos2: TPosition;
  LRes, LResMax: integer;
begin
  LPos := APos;
  DoMove(LPos, AMove);
  LCount := 0;
  GenMoves(LList, LCount, LPos);
  LResMax := Low(integer);
  for LIndex := 0 to Pred(LCount) do
  begin
    LPos2 := LPos;
    DoMove(LPos2, LList[LIndex]);
    LPos2.color := not LPos2.color;
    (*
    if IsCheck(LPos2) then
      LRes := Low(integer)
    else
    *)
      LRes := Eval(LPos2);
    if LRes > LResMax then LResMax := LRes;
  end;
  result := -1 * LResMax;
end;

function SecondEval(const APos: TPosition; const AMove: TMove): integer;
var
  LFrom, LTo: byte;
  LType: TPieceType;
begin
  DecodeMove(AMove, LFrom, LTo, LType);
  result := -1 * Ord(CharToPieceType(APos.board[LFrom]));
end;

function BestMove(const APos: TPosition; const ATime: cardinal): string;
var
  LList: array[0..99] of TMove;
  LEval: array[0..99] of integer;
  LCount: integer;
  LIndex: integer;
begin
  LCount := 0;
  GenMoves(LList, LCount, APos);
  for LIndex := 0 to Pred(LCount) do
    LEval[LIndex] := FirstEval(APos, LList[LIndex]);
  Sort(LList, LEval, LCount);
  ToLog(Concat(#13#10, ShowPosition(APos), #13#10, ArrayToStr(LList, LEval, LCount)), 1);
  
  LCount := BestMovesCount(LEval, LCount);
  for LIndex := 0 to Pred(LCount) do
    LEval[LIndex] := SecondEval(APos, LList[LIndex]);
  Sort(LList, LEval, LCount);
  ToLog(Concat(#13#10, ArrayToStr(LList, LEval, LCount)), 1);
  
  result := MoveToStr(LList[0]);
end;

end.
