:: Compiling and Testing Discon.f90

C:
:: Remove old .dll file
DEL C:\FAST\CertTest\5MW_Baseline\ServoData\DISCON_gwin32.dll

:: Compile
cd C:\FAST\Compiling
mingw32-make.exe 
pause
