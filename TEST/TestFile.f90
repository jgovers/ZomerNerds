PROGRAM TestFile

    IMPLICIT NONE

    REAL(4)     :: a,b,c
    a = 4
    b = 5

    CALL TestRoutine(a,b,c)

    PRINT*,c

END PROGRAM TestFile
