PROGRAM Reader

    IMPLICIT NONE

    REAL(4)                 :: a,b,c
    INTEGER(4),PARAMETER    :: Un = 2
    CHARACTER, PARAMETER    :: UserFile = 'DISCON.txt'

    OPEN( Un, file=UserFile)
    READ( Un,*) a,b

    c = a+b

    PRINT*,c

END PROGRAM Reader
