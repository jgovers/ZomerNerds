SUBROUTINE f(x,y)

    IMPLICIT NONE
    REAL(4), INTENT(IN)     :: x
    REAL(4), INTENT(INOUT)  :: y

    y = x**3 - x + cos(x)

END SUBROUTINE f
