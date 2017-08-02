PROGRAM Reader

    IMPLICIT NONE

    REAL(4)                 :: a,b,c
    INTEGER(4),PARAMETER    :: Un = 2
    CHARACTER(10)           :: UserFile = 'DISCON.in'

    OPEN( Un, file=UserFile)
    READ( 2,*) a,b

    c = a+b

    PRINT*,c

END PROGRAM Reader
