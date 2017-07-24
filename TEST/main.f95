program main
    implicit none
    integer,parameter   :: rp = selected_real_kind(15)
    real(kind=rp)        :: x,y
    real(kind=rp),external  :: f

    x = 3.0_rp
    y = f(x)
    print*,y
end program main
