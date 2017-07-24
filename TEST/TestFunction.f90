function f(x)
    implicit none
    integer,parameter           :: RP = selected_real_kind(15)
    real(kind=rp),intent(in)    :: x
    real(kind=rp)               :: f

    f = x**3 - x + cos(x)
end function f
