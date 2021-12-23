@echo off
:: execute ``auto.bat`` to automate the compilation process
:: execute ``auto.bat clean`` to clear all output files

set file=compress
if NOT [%1]==[] if %1==clean goto :delFiles

tasm /zi %file%.asm
tlink /v %file%.obj
goto :eof

:delFiles
del %file%.exe
del %file%.obj
del %file%.map

:eof