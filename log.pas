
unit Log;

interface

uses
  SysUtils;

procedure ToLog(const ALine: string; const AFileNum: integer = 0);

implementation

var
  LFile: array[0..1] of text;

procedure CreateFiles;
var
  LDir, LDateTime: string;
  LName: array[0..1] of string;
  LFileNum: integer;
begin
  LDir := ExtractFileDir(ParamStr(0)) + DirectorySeparator + 'log';
  if not DirectoryExists(LDir) then
    if not CreateDir(LDir) then
      raise Exception.Create('Cannot create directory.');
  LDateTime := FormatDateTime('yyyymmddhhnnss', Now);
  for LFileNum := 0 to 1 do
    LName[LFileNum] := Format('%s-%s-%d.log', [ChangeFileExt(ExtractFileName(ParamStr(0)), ''), LDateTime, LFileNum]);
  for LFileNum := 0 to 1 do
  begin
    Assign(LFile[LFileNum], LDir + DirectorySeparator + LName[LFileNum]);
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
