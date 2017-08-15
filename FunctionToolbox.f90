! This module contains basic functions
MODULE FunctionToolbox

    IMPLICIT NONE

CONTAINS
	!-------------------------------------------------------------------------------------------------------------------------------
	! Saturates inputValue. Makes sure it is not smaller than minValue and not larger than maxValue
	REAL FUNCTION saturate(inputValue,minValue,maxValue)
	!...............................................................................................................................

		IMPLICIT NONE

		REAL(4), INTENT(IN)		:: inputValue
		REAL(4), INTENT(IN)		:: minValue
		REAL(4), INTENT(IN)		:: maxValue

            IF(minValue > maxValue) THEN    ! If the minimum value is larger than the maximum value give a warning
                WRITE(*,*) "Warning in saturate function: minimum value larger than maximum value"
            ENDIF
                saturate = MIN(MAX(inputValue,minValue),maxValue)

        END FUNCTION saturate
!=======================================================================

        REAL FUNCTION PI(input,Kp,Ki,DT,iStatus,satMin,satMax)
            ! PI controller
            IMPLICIT NONE

            REAL(4), INTENT(IN)		::  input       ! Input signal for the PI controller
            REAL(4), INTENT(IN)		::  DT          ! Time step [s]
            REAL(4), INTENT(IN)		::  Kp, Ki      ! Proportional and integral gain
            REAL(4), INTENT(IN)		::  satMin, satMax ! Input signal for the PI controller
            INTEGER, INTENT(IN)     ::  iStatus     ! A status flag set by the simulation as follows: 0 if this is the first call, 1 for all subsequent time steps, -1 if this is the final call at the end of the simulation.
            REAL(4), SAVE			::	integral    ! Keeps track of the integral

            IF ( iStatus == 0 ) integral = 0		! Instantiate the integral on the first call
            integral = integral + Ki*input*DT		! Integrate
            integral = saturate(integral, satMin, satMax)

            PI = Kp*input + integral 			    ! Calculate output
            PI = saturate(PI, satMin, satMax)
        END FUNCTION PI

END MODULE FunctionToolbox
