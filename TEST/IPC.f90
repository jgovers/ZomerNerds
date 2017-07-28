SUBROUTINE IPC()
! The individual pitch control module
    IMPLICIT NONE


CONTAINS
    SUBROUTINE IPC_core()
    ! Core IPC code
        IMPLICIT NONE


    END SUBROUTINE IPC_core

    SUBROUTINE ColmanTransform(rootMOOP1, rootMOOP2, rootMOOP3, aziAngle, axisDirect, axisQuadr)
    !The Colman or d-q axis transformation transforms the root out of plane bending moments of each turbine blade
    !to a direct axis and a quadrature axis

        IMPLICIT NONE

        REAL(4), INTENT(IN)		:: rootMOOP1, rootMOOP2, rootMOOP3	!root out of plane bending moments of each blade
        REAL(4), INTENT(IN)		:: aziAngle							!rotor azimuth angle
        REAL(4), INTENT(OUT)	:: axisDirect, axisQuadr			!direct axis and quadrature axis outputted by this transform
        REAL(4), PARAMETER		:: PI = 3.14159265359				!mathematical constant pi
        REAL(4), PARAMETER		:: phi2 = 2/3*PI					!phase difference to second blade
        REAL(4), PARAMETER		:: phi3 = 4/3*PI					!phase difference to third blade

        axisDirect	= 2/3 * (cos(aziAngle)*rootMOOP1 + cos(aziAngle+phi2)*rootMOOP2 + cos(aziAngle+phi3)*rootMOOP3)
        axisQuadr	= 2/3 * (sin(aziAngle)*rootMOOP1 + sin(aziAngle+phi2)*rootMOOP2 + sin(aziAngle+phi3)*rootMOOP3)

    END SUBROUTINE ColmanTransform

    SUBROUTINE ColmanTransformInverse(axisDirect, axisQuadr, aziAngle, delta1pAngle, rootMOOP1, rootMOOP2, rootMOOP3)
    !The inverse Colman or d-q axis transformation transforms the direct axis and quadrature axis
    !back to root out of plane bending moments of each turbine blade

        IMPLICIT NONE

        REAL(4), INTENT(IN)		:: axisDirect, axisQuadr			!direct axis and quadrature axis
        REAL(4), INTENT(IN)		:: aziAngle 						!rotor azimuth angle
        REAL(4), INTENT(IN)		:: delta1pAngle						!phase shift added to the azimuth angle TODO: better description
        REAL(4), INTENT(OUT)	:: rootMOOP1, rootMOOP2, rootMOOP3	!root out of plane bending moments of each blade

        REAL(4), PARAMETER		:: PI = 3.14159265359				!mathematical constant pi
        REAL(4), PARAMETER		:: phi2 = 2/3*PI					!phase difference to second blade
        REAL(4), PARAMETER		:: phi3 = 4/3*PI					!phase difference to third blade

        rootMOOP1 = cos(aziAngle+delta1pAngle)*axisDirect + sin(aziAngle+delta1pAngle)*axisQuadr
        rootMOOP2 = cos(aziAngle+delta1pAngle+phi2)*axisDirect + sin(aziAngle+delta1pAngle+phi2)*axisQuadr
        rootMOOP3 = cos(aziAngle+delta1pAngle+phi3)*axisDirect + sin(aziAngle+delta1pAngle+phi3)*axisQuadr

    END SUBROUTINE ColmanTransformInverse
END SUBROUTINE
