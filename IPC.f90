SUBROUTINE IPC(rootMOOP1, rootMOOP2, rootMOOP3, aziAngle, DT, iStatus, phi_1p, phi_2p, gain_1p, gain2_p, pAngle1, pAngle2, pAngle3)
! The individual pitch control module
	USE Filters
    IMPLICIT NONE

    !Inputs
	REAL(4), INTENT(IN)		:: rootMOOP1, rootMOOP2, rootMOOP3	!root out of plane bending moments of each blade
	REAL(4), INTENT(IN)		:: aziAngle							!rotor azimuth angle
	REAL(4), INTENT(IN)		:: phi_1p, phi_2p					!phase offset added to the azimuth angle TODO: better description
	REAL(4), INTENT(IN)     :: gain_1p, gain_2p                 !gain for the IPC block
	REAL(4), INTENT(IN)     :: DT                               !the time step
	INTEGER, INTENT(IN)     :: iStatus                          ! A status flag set by the simulation as follows: 0 if this is the first call, 1 for all subsequent time steps, -1 if this is the final call at the end of the simulation.

	!Outputs
	REAL(4), INTENT(OUT)    :: pAngle1, pAngle2, pAngle3        !pitch angle of each rotor blade

    !Local variables
    REAL(4), PARAMETER		:: PI = 3.14159265359				!mathematical constant pi
    REAL(4)					:: pAngle1_1p, pAngle2_1p, pAngle3_1p	!individual pitch angles for 1p IPC
    REAL(4)					:: pAngle1_2p, pAngle2_2p, pAngle3_2p	!individual pitch angles for 2p IPC

    !Filter rootMOOPs







CONTAINS
    SUBROUTINE IPC_core(rootMOOP1, rootMOOP2, rootMOOP3, aziAngle, DT, iStatus, phi1, gain, pAngle1, pAngle2, pAngle3)
    ! Does the core IPC work
        IMPLICIT NONE

        !Inputs
        REAL(4), INTENT(IN)		:: rootMOOP1, rootMOOP2, rootMOOP3	!root out of plane bending moments of each blade
        REAL(4), INTENT(IN)		:: aziAngle							!rotor azimuth angle
        REAL(4), INTENT(IN)		:: phi1						!phase offset added to the azimuth angle TODO: better description
        REAL(4), INTENT(IN)     :: gain                             !a gain
        REAL(4), INTENT(IN)     :: DT                               !the time step
        INTEGER, INTENT(IN)     :: iStatus                          ! A status flag set by the simulation as follows: 0 if this is the first call, 1 for all subsequent time steps, -1 if this is the final call at the end of the simulation.

        !Outputs
        REAL(4), INTENT(OUT)    :: pAngle1, pAngle2, pAngle3        !pitch angle of each rotor blade

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
        CALL ColemanTransform(rootMOOP1, rootMOOP2, rootMOOP3, aziAngle, axisDirect, axisQuadr)	!pass rootMOOPs through the Coleman transform


        IntAxisDirect	= IntAxisDirect + DT * gain * axisDirect		!multiply with gain and take the integral
        IntAxisQuadr	= IntAxisQuadr + DT * gain * axisQuadr			!multiply with gain and take the integral

        CALL ColemanTransformInverse(axisDirect, axisQuadr, aziAngle, phi1, pAngle1, pAngle2, pAngle3)	!pass signal through the inverse Coleman transform

    END SUBROUTINE IPC_core

    SUBROUTINE ColemanTransform(rootMOOP1, rootMOOP2, rootMOOP3, aziAngle, axisDirect, axisQuadr)
    !The Coleman or d-q axis transformation transforms the root out of plane bending moments of each turbine blade
    !to a direct axis and a quadrature axis

        IMPLICIT NONE

        REAL(4), INTENT(IN)		:: rootMOOP1, rootMOOP2, rootMOOP3	!root out of plane bending moments of each blade
        REAL(4), INTENT(IN)		:: aziAngle							!rotor azimuth angle
        REAL(4), INTENT(OUT)	:: axisDirect, axisQuadr			!direct axis and quadrature axis outputted by this transform
        REAL(4), PARAMETER		:: phi2 = 2/3*PI					!phase difference to second blade
        REAL(4), PARAMETER		:: phi3 = 4/3*PI					!phase difference to third blade

        axisDirect	= 2/3 * (cos(aziAngle)*rootMOOP1 + cos(aziAngle+phi2)*rootMOOP2 + cos(aziAngle+phi3)*rootMOOP3)
        axisQuadr	= 2/3 * (sin(aziAngle)*rootMOOP1 + sin(aziAngle+phi2)*rootMOOP2 + sin(aziAngle+phi3)*rootMOOP3)

    END SUBROUTINE ColemanTransform

    SUBROUTINE ColemanTransformInverse(axisDirect, axisQuadr, aziAngle, phi1, rootMOOP1, rootMOOP2, rootMOOP3)
    !The inverse Coleman or d-q axis transformation transforms the direct axis and quadrature axis
    !back to root out of plane bending moments of each turbine blade

        IMPLICIT NONE

        REAL(4), INTENT(IN)		:: axisDirect, axisQuadr			!direct axis and quadrature axis
        REAL(4), INTENT(IN)		:: aziAngle 						!rotor azimuth angle
        REAL(4), INTENT(IN)		:: phi1								!phase shift added to the azimuth angle TODO: better description
        REAL(4), INTENT(OUT)	:: rootMOOP1, rootMOOP2, rootMOOP3	!root out of plane bending moments of each blade

        REAL(4), PARAMETER		:: phi2 = 2/3*PI					!phase difference to second blade
        REAL(4), PARAMETER		:: phi3 = 4/3*PI					!phase difference to third blade

        rootMOOP1 = cos(aziAngle+phi1)*axisDirect + sin(aziAngle+phi1)*axisQuadr
        rootMOOP2 = cos(aziAngle+phi1+phi2)*axisDirect + sin(aziAngle+phi1+phi2)*axisQuadr
        rootMOOP3 = cos(aziAngle+phi1+phi3)*axisDirect + sin(aziAngle+phi1+phi3)*axisQuadr

    END SUBROUTINE ColemanTransformInverse
END SUBROUTINE
