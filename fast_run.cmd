@echo off

@if exist ".\GEDKeeper2.exe" goto start

call clean.cmd
set MSBDIR=@%WINDIR%\Microsoft.NET\Framework\v4.0.30319
%MSBDIR%\msbuild.exe projects\GEDKeeper2.mswin.sln /verbosity:quiet /p:Configuration="Debug" /p:Platform="x86" /t:Rebuild /p:TargetFrameworkVersion=v4.0

:start
start .\GEDKeeper2.exe
