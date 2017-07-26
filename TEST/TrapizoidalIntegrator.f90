PROGRAM Integrator
    REAL(4), DIMENSION(1:20)	:: array
    INTEGER, PARAMETER		:: wp = 4
    REAL(wp)				:: r
    DO i = 1,20
        array(i) = i
    END DO
    r = integrate(array,2*array)
    WRITE(*,*) r

CONTAINS
  pure function integrate(x, y) result(r)
    !! Calculates the integral of an array y with respect to x using the trapezoid
    !! approximation. Note that the mesh spacing of x does not have to be uniform.
    real(wp), intent(in)  :: x(:)         !! Variable x
    real(wp), intent(in)  :: y(size(x))   !! Function y(x)
    real(wp)              :: r            !! Integral ∫y(x)·dx

    ! Integrate using the trapezoidal rule
    associate(n => size(x))
      r = sum((y(1+1:n-0) + y(1+0:n-1))*(x(1+1:n-0) - x(1+0:n-1)))/2
    end associate
  end function
END PROGRAM Integrator
