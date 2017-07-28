PROGRAM TestFile
    USE TestModule
    IMPLICIT NONE

    REAL(4)     :: a,b,output,global
    a = 4
    b = 5
    global = 50

    CALL TestRoutine(a,b,global,output)

    PRINT*, output

END PROGRAM TestFile
