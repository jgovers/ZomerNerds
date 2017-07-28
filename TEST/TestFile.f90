PROGRAM TestFile

    IMPLICIT NONE

    REAL(4)     :: a,b,c,d,e,f
    a = 4
    b = 5
    d = 10
    e = 6

    CALL TestRoutine(a,b,c,d,e,f)

    PRINT*,c,f

END PROGRAM TestFile
