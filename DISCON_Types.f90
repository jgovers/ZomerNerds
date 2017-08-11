MODULE DISCON_Types
    ! Module containing
    TYPE, PUBLIC        :: PC_type
        REAL(4)         ::MinPit
    END TYPE PC_type
    TYPE, PUBLIC        :: IPC_InputType
        REAL(4)     :: aziAngle							! Rotor azimuth angle
        REAL(4)     :: KInter                           ! Gain for the integrator
        REAL(4)     :: KNotch                           ! Gain for the notch filter
        INTEGER     :: NumBl                            ! Number of turbine blades
        REAL(4)		:: omegaLP
        REAL(4)		:: omegaNotch
        REAL(4)		:: phi
        REAL(4)		:: rootMOOP (3)                 	!root out of plane bending moments of each blade
        REAL(4)		:: zetaLP       					!
        REAL(4)     :: zetaNotch       					!
    END TYPE IPC_InputType

END MODULE DISCON_Types
