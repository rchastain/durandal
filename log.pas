
unit Log;

interface

uses
  SysUtils;

procedure ToLog(const ALine: string; const AFileNum: integer = 0);

implementation

var
  LFile: array[0..1] of text;

procedure CreateFiles;
const
  CDir = 'log';
var
  LDateTime: string;
  LName: array[0..1] of string;
  LFileNum: integer;
begin
  if not DirectoryExists(CDir) then
    if not CreateDir(CDir) then
      raise Exception.Create('Impossible de créer le dossier.');
  LDateTime := FormatDateTime('yyyymmddhhnnss', Now);
  for LFileNum := 0 to 1 do
    LName[LFileNum] := Format('%s-%s-%d.log', [ChangeFileExt(ExtractFileName(ParamStr(0)), ''), LDateTime, LFileNum]);
  for LFileNum := 0 to 1 do
  begin
    Assign(LFile[LFileNum], CDir + '\' + LName[LFileNum]);
    if FileExists(LName[LFileNum]) then
      Append(LFile[LFileNum])
    else
      Rewrite(LFile[LFileNum]);
  end;
end;

procedure FreeFiles;
var
  LFileNum: integer;
begin
  for LFileNum := 0 to 1 do
    Close(LFile[LFileNum]);
end;

procedure ToLog(const ALine: string; const AFileNum: integer);
begin
{$IFDEF DEBUG}
  WriteLn(LFile[AFileNum], ALine);
  Flush(LFile[AFileNum]);
{$ENDIF}
end;

initialization
{$IFDEF DEBUG}
  CreateFiles;
{$ENDIF}

finalization
{$IFDEF DEBUG}
  FreeFiles;
{$ENDIF}

end.
