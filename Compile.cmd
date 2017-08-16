:: Compiling and Testing Discon.f90

set "FASTdir=C:\FAST"
C:
:: Remove old .dll file
DEL %FASTdir%\CertTest\5MW_Baseline\ServoData\DISCON_gwin32.dll

:: Compile
cd %FASTdir%\Compiling
mingw32-make.exe
pause
