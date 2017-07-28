PROGRAM TestFile
    USE TestModule
    IMPLICIT NONE

    REAL(4)     :: a,b,output
    a = 4
    b = 5

    CALL TestRoutine(a,b,output)

    PRINT*, output

END PROGRAM TestFile
