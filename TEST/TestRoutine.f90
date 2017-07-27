SUBROUTINE TestRoutine(a,b,c)

    IMPLICIT NONE

    REAL(4),INTENT(INOUT)   :: a,b,c

    CALL TestSubRoutine()

    CONTAINS

        SUBROUTINE TestSubRoutine()
            !REAL(4),INTENT(INOUT)   :: a,b,c

            c = a*b

        END SUBROUTINE TestSubRoutine

END SUBROUTINE TestRoutine
