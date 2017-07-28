MODULE FunctionToolbox
    ! This module contains all basic functions

    IMPLICIT NONE

    CONTAINS
!=======================================================================
    	REAL FUNCTION saturate(inputValue,minValue,maxValue)
            ! Saturates inputValue. Makes sure it is not smaller than minValue and not larger than maxValue

            IMPLICIT NONE

            REAL(4), INTENT(IN)		:: inputValue,minValue,maxValue

            saturate = MIN(MAX(inputValue,minValue),maxValue)

        END FUNCTION saturate
!=======================================================================

        REAL FUNCTION PI(input,Kp,Ki,DT,iStatus)
            ! PI controller
            IMPLICIT NONE

            REAL(4), INTENT(IN)		::  input
            REAL(4), INTENT(IN)		::  DT          ! Time step [s]
            REAL(4), INTENT(IN)		::  Kp, Ki      ! Proportional and integral gain
            REAL(4), SAVE			::	integral    ! Keeps track of the integral

            IF ( iStatus == 0 ) integral = 0		! Instantiate the integral on the first call
            integral = integral + Ki*input*DT		! Integrate
            PI = Kp*input + integral 			    ! Calculate output

        END FUNCTION PI

END MODULE FunctionToolbox
