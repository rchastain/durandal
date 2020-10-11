
unit History;

{$MODE Delphi}

interface

uses
  SysUtils, Classes;
  
var
  LHistory: TStringList;

implementation

initialization
  LHistory := TStringList.Create;
  
finalization
  LHistory.Free;
  
end.
