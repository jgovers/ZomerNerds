SUBROUTINE TestRoutine(a,b,c,d,e,f)

    IMPLICIT NONE

    REAL(4),INTENT(INOUT)   :: a,b,c,d,e,f

    CALL TestSubRoutine(a,b,c)
    CALL TestSubRoutine(a,e,f)

    CONTAINS

        SUBROUTINE TestSubRoutine(h,i)
            REAL(4),INTENT(INOUT)   :: h,i

            i = a*h

        END SUBROUTINE TestSubRoutine

END SUBROUTINE TestRoutine
