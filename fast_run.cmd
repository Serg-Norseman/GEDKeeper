@echo off

@if exist ".\bin\GEDKeeper2.exe" goto start

call clean.cmd

rem set MSBDIR=C:\Program Files (x86)\Microsoft Visual Studio\2017\BuildTools\MSBuild\15.0\Bin
rem set MSBDIR=C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\MSBuild\Current\Bin
rem set MSBDIR=C:\Program Files (x86)\MSBuild\14.0\Bin

set MSBDIR=@%WINDIR%\Microsoft.NET\Framework\v4.0.30319
%MSBDIR%\msbuild.exe projects\GKv2\GEDKeeper2.sln /verbosity:quiet /p:Configuration="Debug" /p:Platform="x86" /t:Rebuild /p:TargetFrameworkVersion=v4.7.1

:start
start .\bin\GEDKeeper2.exe
