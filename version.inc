
const
  CAppName = 'Durandal';
  CVersion = '0.0.8';
  CAuthor  = 'Roland Chastain';
{$IFDEF FPC}
  CBuild = 'build ' + {$I %DATE%} + ' ' + {$I %TIME%};
  CCompiler = 'Free Pascal ' + {$I %FPCVERSION%} + ' ' + {$I %FPCTARGET%};
  CAppInfo = CAppName + ' ' + CVersion + ' ' + CBuild + ' ' + CCompiler;
{$ELSE}
  CAppInfo = CAppName + ' ' + CVersion;
{$ENDIF}
