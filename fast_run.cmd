@echo off

@if exist ".\bin\GEDKeeper2.exe" goto start

call clean.cmd
set MSBDIR=@%WINDIR%\Microsoft.NET\Framework\v4.0.30319
%MSBDIR%\msbuild.exe projects\GKv2\GEDKeeper2.sln /verbosity:quiet /p:Configuration="Debug" /p:Platform="x86" /t:Rebuild /p:TargetFrameworkVersion=v4.7.1

:start
start .\bin\GEDKeeper2.exe
