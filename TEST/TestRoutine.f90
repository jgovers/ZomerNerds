SUBROUTINE TestRoutine(a,b,c,d,e,f)

    IMPLICIT NONE

    REAL(4),INTENT(INOUT)   :: a,b,c,d,e,f

    CALL TestSubroutine()

    CONTAINS

        SUBROUTINE TestSubRoutine()

            IMPLICIT NONE

            CALL TestSubSubRoutine(a,b,c)
            CALL TestSubSubRoutine(d,e,f)


        END SUBROUTINE TestSubRoutine

        SUBROUTINE TestSubSubRoutine(g,h,i)

            IMPLICIT NONE

            REAL(4),INTENT(INOUT)   :: g,h,i

            i = g*h

        END SUBROUTINE TestSubSubRoutine

END SUBROUTINE TestRoutine
