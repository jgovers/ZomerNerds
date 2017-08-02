MODULE TryMod
    ! This module contains all the filters

    IMPLICIT NONE

    CONTAINS

!=======================================================================
    	REAL FUNCTION LPFilter( InputSignal, a, b, inst)
            ! Discrete time Low-Pass Filter

            IMPLICIT NONE

            REAL(4), INTENT(IN)     :: InputSignal, a, b
            INTEGER, INTENT(IN)		:: inst
            REAL(4), DIMENSION(:) SAVE           :: InputSignalLast (3), OutputSignalLast (3)

            IF ( iStatus == 0 )  THEN           ! .TRUE. if we're on the first call to the DLL
               OutputSignalLast (inst) = InputSignal   ! Initialization of Output
               InputSignalLast (inst) = InputSignal   ! Initialization of previous Input
            ENDIF

            LPFilter = (DT*CornerFreq*InputSignal + DT*CornerFreq*InputSignalLast(inst) - (DT*CornerFreq-2.0)*OutputSignalLast(inst))/(DT*CornerFreq+2.0)	!Filter output

            InputSignalLast (inst)  = InputSignal		!Save input signal for next time step
            OutputSignalLast (inst) = LPFilter		    !Save input signal for next time step

        END FUNCTION LPFilter
!=======================================================================

END MODULE TryMod
