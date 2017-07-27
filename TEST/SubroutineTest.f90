PROGRAM SubTest
    USE SubTestMod
    IMPLICIT NONE
    INTEGER     ::  a,b
    a = 5

    CALL modSub(a,b)
    WRITE(*,*) b


CONTAINS
    SUBROUTINE sub()
        IMPLICIT NONE

        b = a*10
    END SUBROUTINE sub

END PROGRAM
