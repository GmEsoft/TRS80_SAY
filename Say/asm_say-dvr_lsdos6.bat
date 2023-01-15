@echo off
set NAME=SAY
ZMAC --zmac %NAME%.ASM -P0=4 --od . --oo CMD,LST,BDS
if errorlevel 1 goto :eof
move %NAME%.CMD %NAME%.DVR
if errorlevel 1 goto :eof
