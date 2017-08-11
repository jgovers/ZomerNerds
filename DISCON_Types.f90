MODULE DISCON_Types
    ! Module containing
    TYPE, PUBLIC        :: PC_InputType     ! Input type for collective pitch control
        REAL(4)                      :: KI           ! Integral gain for pitch controller at rated pitch (zero), (-).
        REAL(4)                      :: KK           ! Pitch angle where the the derivative of the aerodynamic power w.r.t. pitch has increased by a factor of two relative to the derivative at rated pitch (zero), rad.
        REAL(4)                      :: KP           ! Proportional gain for pitch controller at rated pitch (zero), sec.
        REAL(4)                      :: MaxPit       ! Maximum pitch setting in pitch controller, rad.
        REAL(4)                      :: MaxRat       ! Maximum pitch  rate (in absolute value) in pitch  controller, rad/s.
        REAL(4)                      :: MinPit       ! Minimum pitch setting in pitch controller, rad.
        REAL(4)                      :: RefSpd       ! Desired (reference) HSS speed for pitch controller, rad/s.
        REAL(4)                      :: SetPnt
    END TYPE PC_InputType

    TYPE, PUBLIC        :: IPC_InputType    ! Input type for individual pitch control
        REAL(4)     :: aziAngle							! Rotor azimuth angle
        REAL(4)     :: KInter                           ! Gain for the integrator
        REAL(4)     :: KNotch                           ! Gain for the notch filter
        INTEGER     :: NumBl                            ! Number of turbine blades
        REAL(4)		:: omegaLP                          !
        REAL(4)		:: omegaNotch                       !
        REAL(4)		:: phi                              !
        REAL(4)		:: rootMOOP (3)                 	! Root out of plane bending moments of each blade
        REAL(4)		:: zetaLP       					!
        REAL(4)     :: zetaNotch       					!
    END TYPE IPC_InputType

END MODULE DISCON_Types
