SUBROUTINE IPC
! The individual pitch control module
    IMPLICIT NONE

    REAL(4), PARAMETER		:: PI = 3.14159265359				!mathematical constant pi



CONTAINS
    SUBROUTINE IPC_core(rootMOOP1, rootMOOP2, rootMOOP3, aziAngle, deltaPAngle, pAngle1, pAngle2, pAngle3)
    ! Core IPC code
        IMPLICIT NONE

        !Inputs
        REAL(4), INTENT(IN)		:: rootMOOP1, rootMOOP2, rootMOOP3	!root out of plane bending moments of each blade
        REAL(4), INTENT(IN)		:: aziAngle							!rotor azimuth angle
        REAL(4), INTENT(IN)		:: deltaPAngle						!phase offset added to the azimuth angle TODO: better description
        REAL(4), INTENT(IN)     :: gain                             !a gain

        !Outputs
        REAL(4), INTENT(OUT)    :: pAngle1, pAngle2, pAngle3        !pitch angle of each rotor blade

        !Internal variables
        REAL(4)             	:: axisDirect, axisQuadr			!direct axis and quadrature axis outputted by Coleman transform

        !Body
        CALL ColmanTransform(rootMOOP1, rootMOOP2, rootMOOP3, aziAngle, axisDirect, axisQuadr)




    END SUBROUTINE IPC_core

    SUBROUTINE ColmanTransform(rootMOOP1, rootMOOP2, rootMOOP3, aziAngle, axisDirect, axisQuadr)
    !The Colman or d-q axis transformation transforms the root out of plane bending moments of each turbine blade
    !to a direct axis and a quadrature axis

        IMPLICIT NONE

        REAL(4), INTENT(IN)		:: rootMOOP1, rootMOOP2, rootMOOP3	!root out of plane bending moments of each blade
        REAL(4), INTENT(IN)		:: aziAngle							!rotor azimuth angle
        REAL(4), INTENT(OUT)	:: axisDirect, axisQuadr			!direct axis and quadrature axis outputted by this transform
        REAL(4), PARAMETER		:: phi2 = 2/3*PI					!phase difference to second blade
        REAL(4), PARAMETER		:: phi3 = 4/3*PI					!phase difference to third blade

        axisDirect	= 2/3 * (cos(aziAngle)*rootMOOP1 + cos(aziAngle+phi2)*rootMOOP2 + cos(aziAngle+phi3)*rootMOOP3)
        axisQuadr	= 2/3 * (sin(aziAngle)*rootMOOP1 + sin(aziAngle+phi2)*rootMOOP2 + sin(aziAngle+phi3)*rootMOOP3)

    END SUBROUTINE ColmanTransform

    SUBROUTINE ColmanTransformInverse(axisDirect, axisQuadr, aziAngle, deltaPAngle, rootMOOP1, rootMOOP2, rootMOOP3)
    !The inverse Colman or d-q axis transformation transforms the direct axis and quadrature axis
    !back to root out of plane bending moments of each turbine blade

        IMPLICIT NONE

        REAL(4), INTENT(IN)		:: axisDirect, axisQuadr			!direct axis and quadrature axis
        REAL(4), INTENT(IN)		:: aziAngle 						!rotor azimuth angle
        REAL(4), INTENT(IN)		:: deltaPAngle						!phase shift added to the azimuth angle TODO: better description
        REAL(4), INTENT(OUT)	:: rootMOOP1, rootMOOP2, rootMOOP3	!root out of plane bending moments of each blade

        REAL(4), PARAMETER		:: phi2 = 2/3*PI					!phase difference to second blade
        REAL(4), PARAMETER		:: phi3 = 4/3*PI					!phase difference to third blade

        rootMOOP1 = cos(aziAngle+deltaPAngle)*axisDirect + sin(aziAngle+deltaPAngle)*axisQuadr
        rootMOOP2 = cos(aziAngle+deltaPAngle+phi2)*axisDirect + sin(aziAngle+deltaPAngle+phi2)*axisQuadr
        rootMOOP3 = cos(aziAngle+deltaPAngle+phi3)*axisDirect + sin(aziAngle+deltaPAngle+phi3)*axisQuadr

    END SUBROUTINE ColmanTransformInverse
END SUBROUTINE
