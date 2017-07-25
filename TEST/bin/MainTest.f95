PROGRAM MainTest
    IMPLICIT NONE
    REAL(4)                 :: x,y,a

    x = 3.0
    CALL f(x,y,a)
    print*,y,a
END PROGRAM MainTest
