:: Running and Testing the dll from Simulink

echo off

C:

:: Run
cd C:\FAST\CertTest
echo on
FAST_Win64.exe Test18.SL.fst

:: Make new folder with correct timestamp
echo off
for /f "tokens=2 delims==" %%a in ('wmic OS Get localdatetime /value') do set "dt=%%a"
set "YYYY=%dt:~0,4%" & set "MM=%dt:~4,2%" & set "DD=%dt:~6,2%"
set "HH=%dt:~8,2%" & set "Min=%dt:~10,2%"

set "stamp=%YYYY%_%MM%_%DD%_%HH%%Min%"
set "dbFolder=%userprofile%\Dropbox\ZomerNerds\Debug\%stamp%"
mkdir %dbFolder%

:: Copy debug files to this folder
copy Test18.SL.out %dbFolder%\Test18.SL.out
copy Test18.SL.sum %dbFolder%\Test18.SL.sum
copy Test18.SL.AD.sum %dbFolder%\Test18.SL.AD.sum
copy Test18.SL.ED.sum %dbFolder%\Test18.SL.ED.sum

echo %dbFolder%