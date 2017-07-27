MODULE Filters
    CONTAINS
    	REAL FUNCTION LPFilter(InputSignal,DT,CornerFreq,iStatus)
            ! Discrete time Low-Pass Filter

            IMPLICIT NONE

            REAL(4), INTENT(IN)     :: InputSignal,DT,CornerFreq    ! DT = time step [s], CornerFreq = corner frequency [rad/s]
            INTEGER, INTENT(IN)		:: iStatus
            REAL(4), SAVE           :: InputSignalLast, OutputSignalLast

            IF ( iStatus == 0 )  THEN           ! .TRUE. if we're on the first call to the DLL
               OutputSignalLast    = InputSignal    ! Initialization of Output
               InputSignalLast = InputSignal    ! Initialization of previous Input
            ENDIF

            LPFilter     = (DT*CornerFreq*InputSignal + DT*CornerFreq*InputSignalLast - (DT*CornerFreq-2.0)*OutputSignalLast)/(DT*CornerFreq+2.0)	!Filter output

            InputSignalLast   = InputSignal		!Save input signal for next time step
            OutputSignalLast  = LPFilter		!Save input signal for next time step

        END FUNCTION LPFilter

        REAL FUNCTION HPFilter(InputSignal,DT,CornerFreq,iStatus)
        ! Discrete time High-Pass Filter

            IMPLICIT NONE

            REAL(4), INTENT(IN)     :: InputSignal,DT,CornerFreq    ! DT = time step [s], CornerFreq = corner frequency [rad/s]
            INTEGER, INTENT(IN)		:: iStatus
            REAL(4), SAVE           :: InputSignalLast, OutputSignalLast
            REAL(4)                 :: K

            IF ( iStatus == 0 )  THEN				! .TRUE. if we're on the first call to the DLL
                OutputSignalLast    = InputSignal	! Initialization of Output
                InputSignalLast = InputSignal    	! Initialization of previous Input
            ENDIF

            K = 2.0 / DT

            HPFilter = K/(CornerFreq + K)*InputSignal - K/(CornerFreq + K)*InputSignalLast - (CornerFreq - K)/(CornerFreq + K)*OutputSignalLast	!Filter output

            InputSignalLast   = InputSignal			!Save input signal for next time step
            OutputSignalLast  = HPFilter			!Save input signal for next time step

        END FUNCTION HPFilter

        REAL FUNCTION NotchFilter(InputSignal,DT,Damp,CornerFreq,K,iStatus)
        ! Discrete time inverted Notch Filter

            IMPLICIT NONE

            REAL(4), INTENT(IN)     :: InputSignal,DT,Damp,CornerFreq,K    ! DT = time step [s], CornerFreq = corner frequency [rad/s]
            INTEGER, INTENT(IN)		:: iStatus
            REAL(4), SAVE           :: InputSignalLast1,InputSignalLast2,OutputSignalLast1,OutputSignalLast2

            IF ( iStatus == 0 )  THEN				! .TRUE. if we're on the first call to the DLL
                OutputSignalLast1   = InputSignal	! Initialization of Output
                OutputSignalLast2   = InputSignal
                InputSignalLast1    = InputSignal
                InputSignalLast2    = InputSignal   !Initialization of previous Input

            ENDIF

            K = 2.0 / DT

            NotchFilter        = 1/(4+2*DT*Damp*CornerFreq+DT**2*CornerFreq**2) * &
                ( (8-2*DT**2*CornerFreq**2)*OutputSignalLast1 + (-4+2*DT*Damp*CornerFreq-DT**2*CornerFreq**2)*OutputSignalLast2 + &
                (2*DT*Damp*CornerFreq*K)*InputSignal + (-2*DT*Damp*CornerFreq*K)*InputSignalLast2 )

            InputSignalLast2    = InputSignalLast1
            InputSignalLast1    = InputSignal			!Save input signal for next time step
            OutputSignalLast2   = OutputSignalLast1		!Save input signal for next time step
            OutputSignalLast1   = NotchFilter

        END FUNCTION HPFilter

        REAL FUNCTION SecLPFilter(InputSignal,DT,Damp,CornerFreq,K,iStatus)
        ! Discrete time second order Low-Pass Filter

            IMPLICIT NONE

            REAL(4), INTENT(IN)     :: InputSignal,DT,Damp,CornerFreq,K    ! DT = time step [s], CornerFreq = corner frequency [rad/s]
            INTEGER, INTENT(IN)		:: iStatus
            REAL(4), SAVE           :: InputSignalLast1,InputSignalLast2,OutputSignalLast1,OutputSignalLast2

            IF ( iStatus == 0 )  THEN				! .TRUE. if we're on the first call to the DLL
                OutputSignalLast1   = InputSignal	! Initialization of Output
                OutputSignalLast2   = InputSignal
                InputSignalLast1    = InputSignal
                InputSignalLast2    = InputSignal   !Initialization of previous Input

            ENDIF

            K = 2.0 / DT

            SecLPFilter        = 1/(4+4*DT*Damp*CornerFreq+DT**2*CornerFreq**2) * &
                ( (8-2*DT**2*CornerFreq**2)*OutputSignalLast1 + (-4+4*DT*Damp*CornerFreq-DT**2*CornerFreq**2)*OutputSignalLast2 + &
                (DT**2*CornerFreq**2)*InputSignal + (2*DT**2*CornerFreq**2)*InputSignalLast1 + (DT**2*CornerFreq**2)*InputSignalLast2 )

            InputSignalLast2    = InputSignalLast1
            InputSignalLast1    = InputSignal			!Save input signal for next time step
            OutputSignalLast2   = OutputSignalLast1		!Save input signal for next time step
            OutputSignalLast1   = SecLPFilter

        END FUNCTION SecLPFilter

END MODULE Filters
