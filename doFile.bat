REM Copyright (c) 2016-2021 James Cook
@echo off
IF NOT "%EUDIR%"=="" GOTO label
set EUDIR=%ONEDRIVE%\euphoria40
set path=%EUDIR%\bin;%path%
:label
rem First argument is output, the rest are input, processed in order.
eui dofile.ex %1 %*
pause
