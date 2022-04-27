@echo off
: execute ``auto FILENAME`` to compile FILENAME.asm
: execute ``auto FILENAME clean`` to clear all output files
: execute ``auto FILENAME run`` to compile & run
: execute ``auto FILENAME debug`` to compile & debug

set file=%1
set arg=%2
if [%arg%]==[clean] goto :delFiles

.\utils\tasm /zi %file%.asm
.\utils\tlink /v %file%.obj
if [%arg%]==[run] goto :run
if [%arg%]==[debug] goto :debug
goto :eof

:run
  %file%.exe
  goto :eof

:debug 
  .\utils\td %file%.exe
  goto :eof

:delFiles
  del %file%.exe
  del %file%.obj
  del %file%.map

:eof