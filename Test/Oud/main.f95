PROGRAM MainTest
    IMPLICIT NONE
    REAL(4)                 :: x,y

    x = 3.0
    CALL f(x,y)
    print*,y
END PROGRAM MainTest
