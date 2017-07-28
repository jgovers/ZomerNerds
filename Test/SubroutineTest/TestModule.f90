MODULE TestModule
    REAL(4) :: global  =13

CONTAINS
    SUBROUTINE TestRoutine(in1, in2, output)

        IMPLICIT NONE

        REAL(4),INTENT(IN)      :: in1, in2
        REAL(4),INTENT(OUT)     :: output
!        REAL(4),INTENT(IN)      :: global

        CALL TestSubroutine(in1, in2, output)

    END SUBROUTINE TestRoutine

    SUBROUTINE TestSubRoutine(in1, in2, output)

        IMPLICIT NONE

        REAL(4),INTENT(IN)      :: in1, in2
        REAL(4),INTENT(OUT)     :: output

        CALL TestSubSubRoutine(in1, in2, output)


    END SUBROUTINE TestSubRoutine

    SUBROUTINE TestSubSubRoutine(in1, in2, output)

        IMPLICIT NONE

        REAL(4),INTENT(IN)      :: in1, in2
        REAL(4),INTENT(OUT)     :: output

        output = in1*in2*global

    END SUBROUTINE TestSubSubRoutine
END MODULE TestModule
