MODULE SubTestMod
    IMPLICIT NONE
        INTEGER, INTENT(IN)     :: a
        INTEGER, INTENT(OUT)    :: b
CONTAINS
    SUBROUTINE modSub()
        IMPLICIT NONE
        b = a*12
    END SUBROUTINE modSub
END MODULE SubTestMod
