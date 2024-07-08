@echo off

@if exist ".\bin\GEDKeeper2.exe" goto start

call clean.cmd

set MSBDIR="C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\MSBuild\Current\Bin"
@if exist %MSBDIR%\msbuild.exe goto build

set MSBDIR="C:\Program Files (x86)\Microsoft Visual Studio\2017\BuildTools\MSBuild\15.0\Bin"
@if exist %MSBDIR%\msbuild.exe goto build

echo Build is not possible!
goto quit

rem Don't build with wf4.7.1 and lang7.3, only for 1-6
rem set MSBDIR="C:\Program Files (x86)\MSBuild\14.0\Bin"
rem Don't build with wf4.7.1 and lang7.3, only for 1-5
rem set MSBDIR=@%WINDIR%\Microsoft.NET\Framework\v4.0.30319

:build
%MSBDIR%\msbuild.exe projects\GKv2\GEDKeeper2.sln /verbosity:quiet /p:Configuration="Debug" /p:Platform="x86" /t:Rebuild /p:TargetFrameworkVersion=v4.7.1

:start
start .\bin\GEDKeeper2.exe

:quit
