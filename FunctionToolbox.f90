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

            REAL(4), INTENT(IN)		::  input       ! Input signal for the PI controller
            REAL(4), INTENT(IN)		::  DT          ! Time step [s]
            REAL(4), INTENT(IN)		::  Kp, Ki      ! Proportional and integral gain
            INTEGER, INTENT(IN)     ::  iStatus     ! A status flag set by the simulation as follows: 0 if this is the first call, 1 for all subsequent time steps, -1 if this is the final call at the end of the simulation.
            REAL(4), SAVE			::	integral    ! Keeps track of the integral

            IF ( iStatus == 0 ) integral = 0		! Instantiate the integral on the first call
            integral = integral + Ki*input*DT		! Integrate
            PI = Kp*input + integral 			    ! Calculate output

        END FUNCTION PI

END MODULE FunctionToolbox
