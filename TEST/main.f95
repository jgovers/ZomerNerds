! A fortran95 program for G95
! By WQY


program main
  IMPLICIT NONE
  use :: testmodule
  INTEGER :: ans, a, b
    ans = Substraction(1,5)
  write(*,*) ans

contains

INTEGER FUNCTION Addition(a, b)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: a, b
    Addition = a+b
END FUNCTION
end program
