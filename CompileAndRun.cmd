:: Compiling and Testing Discon.f90

C:
:: Remove old .dll file
DEL C:\FAST\CertTest\5MW_Baseline\ServoData\DISCON_gwin32.dll
cd C:\FAST\Compiling
mingw32-make.exe
cd ..\CertTest
FAST_Win32.exe Test18.fst