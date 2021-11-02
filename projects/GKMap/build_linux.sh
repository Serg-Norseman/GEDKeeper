#!/bin/sh

msbuild ./GKMap.sln /p:Configuration="v4.0-Release" /p:Platform="Any CPU" /p:DefineConstants="MONO;SQLite" /p:TargetFrameworkVersion=v4.0 /t:Rebuild
