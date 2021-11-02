@echo off
set MSBDIR=@%WINDIR%\Microsoft.NET\Framework\v4.0.30319
%MSBDIR%\msbuild ./GKMap.sln /p:Configuration="v4.0-Release" /p:Platform="Any CPU" /p:DefineConstants="SQLite" /p:TargetFrameworkVersion=v4.0 /t:Rebuild
