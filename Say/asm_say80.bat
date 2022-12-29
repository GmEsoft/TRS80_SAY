@echo off
set NAME=SAY80
ZMAC --zmac %NAME%.ASM --od . --oo CIM,LST,BDS
if errorlevel 1 goto :end

move %NAME%.CIM %NAME%.COM
if errorlevel 1 goto :end
