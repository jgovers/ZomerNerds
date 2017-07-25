PROGRAM MainTest
    IMPLICIT NONE
    REAL(4)                 :: x,y
    REAL(4), EXTERNAL       :: f

    x = 3.0_rp
    y = f(x)
    print*,y
END PROGRAM MainTest
