MODULE SecondSub
    CONTAINS
    SUBROUTINE DISCO(a,b,c)

        IMPLICIT NONE
        REAL(4), INTENT(INOUT)     :: a,b,c

        a = b*20
        b = a
        c = a+b

    END SUBROUTINE DISCO
END MODULE SecondSub
