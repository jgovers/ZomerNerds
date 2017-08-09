:: Compiling and Testing Discon.f90

echo off

C:
:: Remove old .dll file
DEL C:\FAST\CertTest\5MW_Baseline\ServoData\DISCON_gwin32.dll

:: Compile
cd C:\FAST\Compiling
echo on
mingw32-make.exe

:: Run
echo off
cd ..\CertTest
echo on
FAST_Win32.exe Test18.fst

:: Make new folder with correct timestamp
echo off
for /f "tokens=2 delims==" %%a in ('wmic OS Get localdatetime /value') do set "dt=%%a"
set "YYYY=%dt:~0,4%" & set "MM=%dt:~4,2%" & set "DD=%dt:~6,2%"
set "HH=%dt:~8,2%" & set "Min=%dt:~10,2%"

set "stamp=%YYYY%_%MM%_%DD%_%HH%%Min%"
set "dbFolder=%userprofile%\Dropbox\ZomerNerds\Debug\%stamp%"
mkdir %dbFolder%

:: Copy debug files to this folder
copy Test18.SrvD.dbg %dbFolder%\Test18.SrvD.dbg
copy Test18.SrvD.dbg2 %dbFolder%\Test18.SrvD.dbg2
copy Test18.outb %dbFolder%\Test18.outb
copy Test18.sum %dbFolder%\Test18.sum
copy Test18.AD.sum %dbFolder%\Test18.AD.sum
copy Test18.ED.sum %dbFolder%\Test18.ED.sum

echo %dbFolder%