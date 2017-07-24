module testmodule
    contains
FUNCTION Substraction(a, b)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: a, b
    Substraction = a-b
END FUNCTION
end module
