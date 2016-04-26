@echo off
color 0A
title GEDKeeper Gource Visualization

set host=%computername%
echo Hostname: %host%

if /i %host%==VALHALLA goto zsv_home
if /i %host%==WS goto zsv_ws
goto end

rem gource --output-ppm-stream - | ffmpeg -y -b 3000K -r 60 -f image2pipe -vcodec ppm -i - gource.h264

:zsv_home
C:
cd "C:\Program Files (x86)\Gource"
gource -f --highlight-all-users --multi-sampling d:\virts\shared\gedkeeper\.git
goto end


:zsv_ws
C:
cd "C:\Program Files (x86)\Gource"
gource -1600x900 -f --highlight-all-users --multi-sampling e:\temp\gedkeeper\.git
goto end


:other
goto end


:end


