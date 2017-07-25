SUBROUTINE f(x,y,a)

    IMPLICIT NONE
    REAL(4), INTENT(INOUT)  :: x,y,a
    REAL(4)                 :: b,c
    a = 10
    b = 1
    c = 0

    y = x**3 - x + cos(x)

    CALL DISCO(a,b,c)

END SUBROUTINE f
