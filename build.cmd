@echo off

if not exist bin\ md bin\
if not exist bin\debug\ md bin\debug\
if not exist bin\release\ md bin\release\

call dcc32 -CC -NSsystem -DDEBUG durandal.pas
move durandal.exe bin\debug\durandal32.exe

call dcc32 -CC -NSsystem durandal.pas
move durandal.exe bin\release\durandal32.exe

call dcc32 -CC -NSsystem -DRANDOM_MOVER -DDEBUG durandal.pas
move durandal.exe bin\debug\durandom32.exe

call dcc32 -CC -NSsystem -DRANDOM_MOVER durandal.pas
move durandal.exe bin\release\durandom32.exe
