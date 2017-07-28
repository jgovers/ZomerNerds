:: Compiling and Run testing code in current folder

SET executable=TestCode

:: Delete old executable
DEL %executable%.exe

:: Compile
gfortran *.f90 -o %executable%

:: Run
TestCode

pause
