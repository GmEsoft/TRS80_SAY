@echo off
set NAME=SAY
ZMAC --zmac %NAME%.ASM -P0=3 --od . --oo CMD,LST,BDS
if errorlevel 1 goto :eof
