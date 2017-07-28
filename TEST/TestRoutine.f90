SUBROUTINE TestRoutine(a,b,c,d,e,f)

    IMPLICIT NONE

    REAL(4),INTENT(INOUT)   :: a,b,c,d,e,f

    CALL TestSubroutine(a,b,c)
    CALL TestSubroutine(d,e,f)

    CONTAINS

        SUBROUTINE TestSubRoutine(g,h,i)

            IMPLICIT NONE

            REAL(4), INTENT(INOUT)  :: g,h,i

            CALL TestSubSubRoutine()


        END SUBROUTINE TestSubRoutine

        SUBROUTINE TestSubSubRoutine()

            IMPLICIT NONE

!            REAL(4),INTENT(INOUT)   :: g,h,i

            i = g*h

        END SUBROUTINE TestSubSubRoutine

END SUBROUTINE TestRoutine
