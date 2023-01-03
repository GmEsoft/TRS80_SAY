@echo off
set NAME=SAY
ZMAC --zmac %NAME%.ASM -P0=2 --od . --oo CIM,LST,BDS
if errorlevel 1 goto :eof

move %NAME%.CIM %NAME%.COM
if errorlevel 1 goto :eof
