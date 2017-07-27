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

END MODULE FunctionToolbox
