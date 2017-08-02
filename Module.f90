MODULE Filters
    ! This module contains all the filters

    IMPLICIT NONE

    CONTAINS

!=======================================================================
    	REAL FUNCTION LPFilter( InputSignal, DT, CornerFreq, iStatus, inst)
            ! Discrete time Low-Pass Filter

            IMPLICIT NONE

            REAL(4), INTENT(IN)     :: InputSignal,DT,CornerFreq    ! DT = time step [s], CornerFreq = corner frequency [rad/s]
            INTEGER, INTENT(IN)		:: iStatus, inst
            REAL(4), SAVE           :: InputSignalLast (3), OutputSignalLast (3)

            IF ( iStatus == 0 )  THEN           ! .TRUE. if we're on the first call to the DLL
               OutputSignalLast (inst) = InputSignal   ! Initialization of Output
               InputSignalLast (inst) = InputSignal   ! Initialization of previous Input
            ENDIF

            LPFilter = (DT*CornerFreq*InputSignal + DT*CornerFreq*InputSignalLast(inst) - (DT*CornerFreq-2.0)*OutputSignalLast(inst))/(DT*CornerFreq+2.0)	!Filter output

            InputSignalLast (inst)  = InputSignal		!Save input signal for next time step
            OutputSignalLast (inst) = LPFilter		    !Save input signal for next time step

        END FUNCTION LPFilter
!=======================================================================
        REAL FUNCTION SecLPFilter(InputSignal, DT, CornerFreq, Damp, iStatus, inst, instSecLP)
            ! Discrete time second order Low-Pass Filter

            IMPLICIT NONE

            REAL(4), INTENT(IN)     :: InputSignal,DT,Damp,CornerFreq    ! DT = time step [s], CornerFreq = corner frequency [rad/s]
            INTEGER, INTENT(IN)		:: iStatus, inst, instSecLP
            REAL(4), SAVE           :: InputSignalLast1 (3), InputSignalLast2 (3), OutputSignalLast1 (3), OutputSignalLast2 (3)

            IF ( iStatus == 0 )  THEN				! .TRUE. if we're on the first call to the DLL
                OutputSignalLast1(inst)  = InputSignal	! Initialization of Output
                OutputSignalLast2(inst)  = InputSignal
                InputSignalLast1(inst)   = InputSignal
                InputSignalLast2(inst)   = InputSignal   !Initialization of previous Input
            ENDIF

            SecLPFilter         = 1/(4+4*DT*Damp*CornerFreq+DT**2*CornerFreq**2) * &
                ( (8-2*DT**2*CornerFreq**2)*OutputSignalLast1(inst) + (-4+4*DT*Damp*CornerFreq-DT**2*CornerFreq**2)*OutputSignalLast2(inst) + &
                (DT**2*CornerFreq**2)*InputSignal + (2*DT**2*CornerFreq**2)*InputSignalLast1(inst) + (DT**2*CornerFreq**2)*InputSignalLast2(inst) )

            InputSignalLast2(inst)   = InputSignalLast1 (inst)
            InputSignalLast1(inst)   = InputSignal			!Save input signal for next time step
            OutputSignalLast2(inst)  = OutputSignalLast1 (inst)		!Save input signal for next time step
            OutputSignalLast1(inst)  = SecLPFilter

        END FUNCTION SecLPFilter
!=======================================================================
        REAL FUNCTION HPFilter(InputSignal,DT,CornerFreq,iStatus)
            ! Discrete time High-Pass Filter

            IMPLICIT NONE

            REAL(4), INTENT(IN)     :: InputSignal,DT,CornerFreq    ! DT = time step [s], CornerFreq = corner frequency [rad/s]
            INTEGER, INTENT(IN)		:: iStatus
            REAL(4), SAVE           :: InputSignalLast, OutputSignalLast
            REAL(4)                 :: K

            IF ( iStatus == 0 )  THEN				! .TRUE. if we're on the first call to the DLL
                OutputSignalLast    = InputSignal	! Initialization of Output
                InputSignalLast     = InputSignal   ! Initialization of previous Input
            ENDIF

            K = 2.0 / DT

            HPFilter = K/(CornerFreq + K)*InputSignal - K/(CornerFreq + K)*InputSignalLast - (CornerFreq - K)/(CornerFreq + K)*OutputSignalLast	!Filter output

            InputSignalLast   = InputSignal			!Save input signal for next time step
            OutputSignalLast  = HPFilter			!Save input signal for next time step

        END FUNCTION HPFilter
!=======================================================================
        REAL FUNCTION NotchFilter(InputSignal, DT, K, CornerFreq, Damp, iStatus, inst, instNotch)

            ! Discrete time inverted Notch Filter

            IMPLICIT NONE

            REAL(4), INTENT(IN)     :: InputSignal,DT,Damp,CornerFreq,K    ! DT = time step [s], CornerFreq = corner frequency [rad/s]
            INTEGER, INTENT(IN)		:: iStatus, inst, instNotch
            REAL(4), SAVE           :: InputSignalLast1 (3), InputSignalLast2 (3), OutputSignalLast1 (3), OutputSignalLast2 (3)

            IF ( iStatus == 0 )  THEN				! .TRUE. if we're on the first call to the DLL
                OutputSignalLast1(inst)  = InputSignal	! Initialization of Output
                OutputSignalLast2(inst)  = InputSignal
                InputSignalLast1(inst)   = InputSignal
                InputSignalLast2(inst)   = InputSignal   !Initialization of previous Input

            ENDIF

            NotchFilter         = 1.0/(4.0+2.0*DT*Damp*CornerFreq+DT**2.0*CornerFreq**2.0) * &
                ( (8.0-2.0*DT**2.0*CornerFreq**2.0)*OutputSignalLast1(inst) + (-4.0+2.0*DT*Damp*CornerFreq-DT**2.0*CornerFreq**2.0)*OutputSignalLast2(inst) + &
                (2.0*DT*Damp*CornerFreq*K)*InputSignal + (-2.0*DT*Damp*CornerFreq*K)*InputSignalLast2(inst) )

            InputSignalLast2(inst)   = InputSignalLast1(inst)
            InputSignalLast1(inst)   = InputSignal			!Save input signal for next time step
            OutputSignalLast2(inst)  = OutputSignalLast1(inst)		!Save input signal for next time step
            OutputSignalLast1(inst)  = NotchFilter

        END FUNCTION NotchFilter
!=======================================================================

END MODULE Filters
