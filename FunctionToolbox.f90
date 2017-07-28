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

        REAL FUNCTION PI(input,DT,P,iStatus)
            ! PI controller
            IMPLICIT NONE

            REAL(4), INTENT(IN)		::  input
            REAL(4), INTENT(IN)		::  DT
            REAL(4), INTENT(IN)		::  P
            REAL(4), SAVE			::	integral

            IF ( iStatus == 0 ) integral = 0		! Instantiate the integral on the first call
            integral = integral + input*DT			! Integrate
            PI = P*(input*integral)					! Calculate output

        END FUNCTION PI

END MODULE FunctionToolbox
