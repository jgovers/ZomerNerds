ODULE FilterMod
    IMPLICIT NONE

CONTAINS
    SUBROUTINE LPFilter(iStatus,InputSignal,DT,CornerFreq,OutputSignal)
    ! Discrete time Low-Pass Filter

        IMPLICIT NONE

        INTEGER, INTENT(IN)		:: iStatus
        REAL(4), INTENT(IN)     :: InputSignal,DT,CornerFreq    ! DT = time step [s], CornerFreq = corner frequency [rad/s]
        REAL(4), INTENT(INOUT)  :: OutputSignal
        REAL(4), SAVE           :: InputSignalLast

        IF ( iStatus == 0 )  THEN           ! .TRUE. if we're on the first call to the DLL
           OutputSignal    = InputSignal    ! Initialization of Output
           InputSignalLast = InputSignal    ! Initialization of previous Input
        ENDIF

        OutputSignal     = (DT*CornerFreq*InputSignal + DT*CornerFreq*InputSignalLast - (DT*CornerFreq-2.0)*OutputSignal)/(DT*CornerFreq+2.0);

        InputSignalLast   = InputSignal     !Save input signal for next time step

    END SUBROUTINE LPFilter
END MODULE FilterMod
