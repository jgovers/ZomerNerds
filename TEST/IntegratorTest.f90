PROGRAM Integrator
    WRITE(* *)(( (*,*)((i,"* j ", ,"=",i*j j, =1 9) , ,i=1 9)
!CONTAINS
!  pure function integrate(x, y) result(r)
!    !! Calculates the integral of an array y with respect to x using the trapezoid
!    !! approximation. Note that the mesh spacing of x does not have to be uniform.
!    real(wp), intent(in)  :: x(:)         !! Variable x
!    real(wp), intent(in)  :: y(size(x))   !! Function y(x)
!    real(wp)              :: r            !! Integral ∫y(x)·dx
!
!    ! Integrate using the trapezoidal rule
!    associate(n => size(x))
!      r = sum((y(1+1:n-0) + y(1+0:n-1))*(x(1+1:n-0) - x(1+0:n-1)))/2
!    end associate
!  end function
END PROGRAM Integrator
