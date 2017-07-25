SUBROUTINE GenSpeedLPF(iStatus,GenSpeed,VS_DT,CornerFreqF2,GenSpeedF2)

    IMPLICIT NONE

    REAL(4), INTENT(IN)     :: iStatus, GenSpeed, VS_DT, CornerFreqF2
    REAL(4), INTENT(INOUT)  :: GenSpeedF2
    REAL(4), SAVE           :: GenSpeedLast

    IF ( iStatus == 0 )  THEN  ! .TRUE. if we're on the first call to the DLL
       GenSpeedF2     = GenSpeed
       GenSpeedLast   = GenSpeed
    ENDIF

    GenSpeedF2     = (VS_DT*CornerFreqF2*GenSpeed + VS_DT*CornerFreqF2*GenSpeedLast - (VS_DT*CornerFreqF2-2.0)*GenSpeedF2)/(VS_DT*CornerFreqF2+2.0);

    GenSpeedLast   = GenSpeed  !Save input signal for next time step

END SUBROUTINE GenSpeedLPF
