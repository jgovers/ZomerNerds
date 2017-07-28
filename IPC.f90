SUBROUTINE IPC(rootMOOP, aziAngle, DT, KInter, KNotch, omegaLP, omegaNotch, phi, zetaLP, zetaNotch, iStatus, PitComIPC, PitComIPCF, rootMOOPF)
! The individual pitch control module
	USE Filters

    IMPLICIT NONE

    !Inputs
	REAL(4), INTENT(IN)		:: aziAngle							! Rotor azimuth angle
	REAL(4), INTENT(IN)     :: DT                               ! Time step
	REAL(4), INTENT(IN)     :: KInter                           ! Gain for the integrator
	REAL(4), INTENT(IN)     :: KNotch                           ! Gain for the notch filter
	REAL(4), INTENT(IN)		:: omegaLP          				!phase offset added to the azimuth angle TODO: better description
	REAL(4), INTENT(IN)		:: omegaNotch          				!phase offset added to the azimuth angle TODO: better description
	REAL(4), INTENT(IN)		:: phi          					!phase offset added to the azimuth angle TODO: better description
	REAL(4), INTENT(IN)		:: rootMOOP (3)                 	!root out of plane bending moments of each blade
	REAL(4), INTENT(IN)		:: zetaLP       					!
    REAL(4), INTENT(IN)		:: zetaNotch       					!

	INTEGER, INTENT(IN)     :: iStatus                          ! A status flag set by the simulation as follows: 0 if this is the first call, 1 for all subsequent time steps, -1 if this is the final call at the end of the simulation.

	!Outputs
	REAL(4), INTENT(OUT)    :: PitComIPC  (3)                      ! Pitch angle of each rotor blade
    REAL(4), INTENT(OUT)    :: PitComIPCF (3)                      ! Filtered pitch angle of each rotor blade
    REAL(4), INTENT(OUT)    :: rootMOOPF  (3)                       ! Filtered RootMoop

    !Local variables
    REAL(4), PARAMETER		:: PI = 3.14159265359				!mathematical constant pi

    !Filter rootMOOPs
    rootMOOPF(1) = NotchFilter(rootMOOP(1), DT, KNotch, omegaNotch, zetaNotch, iStatus)
    rootMOOPF(2) = NotchFilter(rootMOOP(2), DT, KNotch, omegaNotch, zetaNotch, iStatus)
    rootMOOPF(3) = NotchFilter(rootMOOP(3), DT, KNotch, omegaNotch, zetaNotch, iStatus)

    CALL IPC_core(rootMOOPF, aziAngle, DT, KInter, phi, iStatus, PitComIPC)

    PitComIPCF(1) = SecLPFilter(PitComIPC(1), DT, omegaLP, zetaLP, iStatus)
    PitComIPCF(2) = SecLPFilter(PitComIPC(2), DT, omegaLP, zetaLP, iStatus)
    PitComIPCF(3) = SecLPFilter(PitComIPC(3), DT, omegaLP, zetaLP, iStatus)

CONTAINS
    SUBROUTINE IPC_core(rootMOOP, aziAngle, DT, KInter, phi, iStatus, PitComIPC)
    ! Does the core IPC work

        IMPLICIT NONE

        !Inputs
        REAL(4), INTENT(IN)		:: rootMOOP (3)	                    !root out of plane bending moments of each blade
        REAL(4), INTENT(IN)		:: aziAngle							!rotor azimuth angle
        REAL(4), INTENT(IN)     :: DT                               !the time step
        REAL(4), INTENT(IN)     :: KInter                           ! Integrator gain
        REAL(4), INTENT(IN)		:: phi						        !phase offset added to the azimuth angle TODO: better description

        INTEGER, INTENT(IN)     :: iStatus                          ! A status flag set by the simulation as follows: 0 if this is the first call, 1 for all subsequent time steps, -1 if this is the final call at the end of the simulation.

        !Outputs
        REAL(4), INTENT(OUT)    :: PitComIPC(3)                       !pitch angle of each rotor blade

        !Local variables
        REAL(4)             	:: axisDirect, axisQuadr			!direct axis and quadrature axis outputted by Coleman transform
        REAL(4), SAVE           :: IntAxisDirect, IntAxisQuadr      !integral of the direct axis and quadrature axis

        !Initialization
        IF(iStatus==0)  THEN
			!Set integrals to be 0 in the first time step
            IntAxisDirect = 0
            IntAxisQuadr = 0
        END IF

        !Body
        CALL ColemanTransform(rootMOOP, aziAngle, axisDirect, axisQuadr)	        !pass rootMOOPs through the Coleman transform

        IntAxisDirect	= IntAxisDirect + DT * KInter * axisDirect		!multiply with gain and take the integral
        IntAxisQuadr	= IntAxisQuadr + DT * KInter * axisQuadr			!multiply with gain and take the integral

        CALL ColemanTransformInverse(axisDirect, axisQuadr, aziAngle, phi, PitComIPC)	!pass signal through the inverse Coleman transform

    END SUBROUTINE IPC_core

    SUBROUTINE ColemanTransform(rootMOOP, aziAngle, axisDirect, axisQuadr)
    !The Coleman or d-q axis transformation transforms the root out of plane bending moments of each turbine blade
    !to a direct axis and a quadrature axis

        IMPLICIT NONE

        REAL(4), INTENT(IN)		:: rootMOOP (3)	                    !root out of plane bending moments of each blade
        REAL(4), INTENT(IN)		:: aziAngle							!rotor azimuth angle

        REAL(4), INTENT(OUT)	:: axisDirect, axisQuadr			!direct axis and quadrature axis outputted by this transform

        REAL(4), PARAMETER		:: phi2 = 2/3*PI					!phase difference to second blade
        REAL(4), PARAMETER		:: phi3 = 4/3*PI					!phase difference to third blade

        axisDirect	= 2/3 * (cos(aziAngle)*rootMOOP(1) + cos(aziAngle+phi2)*rootMOOP(2) + cos(aziAngle+phi3)*rootMOOP(3))
        axisQuadr	= 2/3 * (sin(aziAngle)*rootMOOP(1) + sin(aziAngle+phi2)*rootMOOP(2) + sin(aziAngle+phi3)*rootMOOP(3))

    END SUBROUTINE ColemanTransform

    SUBROUTINE ColemanTransformInverse(axisDirect, axisQuadr, aziAngle, phi, PitComIPC)
    !The inverse Coleman or d-q axis transformation transforms the direct axis and quadrature axis
    !back to root out of plane bending moments of each turbine blade

        IMPLICIT NONE

        REAL(4), INTENT(IN)		:: axisDirect, axisQuadr			!direct axis and quadrature axis
        REAL(4), INTENT(IN)		:: aziAngle 						!rotor azimuth angle
        REAL(4), INTENT(IN)		:: phi								!phase shift added to the azimuth angle TODO: better description
        REAL(4), INTENT(OUT)	:: PitComIPC (3)	                !root out of plane bending moments of each blade

        REAL(4), PARAMETER		:: phi2 = 2/3*PI					! Phase difference to second blade
        REAL(4), PARAMETER		:: phi3 = 4/3*PI					! Phase difference to third blade

        PitComIPC(1) = cos(aziAngle+phi)*axisDirect + sin(aziAngle+phi)*axisQuadr
        PitComIPC(2) = cos(aziAngle+phi+phi2)*axisDirect + sin(aziAngle+phi+phi2)*axisQuadr
        PitComIPC(3) = cos(aziAngle+phi+phi3)*axisDirect + sin(aziAngle+phi+phi3)*axisQuadr

    END SUBROUTINE ColemanTransformInverse

END SUBROUTINE
