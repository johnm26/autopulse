;+
; NAME:
;      CONST
;
;
; PURPOSE:
;      Define some physical constant in CGS
;
;
; CATEGORY:
;      Definition
;
;
; CALLING SEQUENCE:
;      @const
;
;
; INPUTS:
;      None.
;
;
; OPTIONAL INPUTS:
;      None.
;
;
; KEYWORD PARAMETERS:
;      None.
;
;
; OUTPUTS:
;      systems variables
;
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;      None.
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
;
;
; PROCEDURE:
;
;
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;      A. Beelen, 2003            IAS
;-

DEFSYSV, '!const',1                                          ; is constant already present

DEFSYSV, '!cvac',2.99792458D10                                  ; Velocity of light (in vacuo) (cm s^{-1})
DEFSYSV, '!G', 6.672D-8                                      ; Gravitational constant (cm^3 g^{-1} s^{-2})
DEFSYSV, '!AU',1.4959787D13                                  ; Astronomical Unit of Distance (cm)
DEFSYSV, '!pc',3.0857D18                                     ; Parsec (=AU/sin 1") (cm)
DEFSYSV, '!msun',1.9891D33                                   ; Solar Mass (g)
DEFSYSV, '!lsun',3.85D33                                     ; Solar Luminosity (erg/s)
DEFSYSV, '!mb_sun',4.74                                      ; Absolute bolometric magnitude of the sun (Allen's Astro Quantities)
DEFSYSV, '!h',6.62606896d-27                                 ; Planck's Constant (erg s = cm^2 g s^{-2})
DEFSYSV, '!kb',1.3806504D-16                                 ; Boltzmann constant (erg K^{-1} =  cm^2 g s^{-3} K^{-1})
DEFSYSV, '!mp',1.67262158d-24                                ; Proton mass
DEFSYSV, '!me',9.10938188d-28                                ; Electon mass
;DEFSYSV, '!sigma',5.670400d-5                               ; Stefan Boltzmann constant (erg s^{-1} cm^{-2} K^{-4})
DEFSYSV, '!sigb',2d0*!dpi^5*!kb^4/15d0/!h^3/!cvac^2          ; Stefan Boltzmann const (erg/s/cm^2/K^4)

DEFSYSV, '!erg',1.D-7                                        ; erg unit (J)
DEFSYSV, '!jansky',1.D-23                                    ; janski unit (erg s^{-1} cm^{-2} Hz^{-1})
DEFSYSV, '!pc2cm',3.0857678D18                               ; parsec to centimeter coefficient
DEFSYSV, '!arcsec2Deg',60.^(-2)                              ; arcsec to deg
DEFSYSV, '!arcsec2raDian',60.^(-2)*180.^(-1)*!Dpi            ; arcsec to radian
DEFSYSV, '!arcsec2_to_steraDian',60.^(-4)*180.^(-2)*!Dpi^(2) ; arcsec2 to steradian
DEFSYSV, '!gmsun',1.32712440018d26                           ; G*M_Sun in cm^2/s^2
DEFSYSV, '!rearth', 6.37810d8                                ; radius of Earth in cm
DEFSYSV, '!rmoon', 1737.1d5                                  ; radius of Moon in cm
DEFSYSV, '!mmoon', 7.3477d25                                 ; mass of Moon in cm
DEFSYSV, '!mearth',5.9742d27                                 ; mass of Earth in grams
DEFSYSV, '!rsun',6.955d10                                    ; radius of Sun in cm
DEFSYSV, '!tsun',(!lsun/(4d0*!pi*!rsun^2*!sigb))^.25d0       ; temperature of Sun in cm
DEFSYSV, '!mjupiter',1.8987d30   ; Mass of Jupiter in grams
DEFSYSV, '!rjupiter',7.1492d9    ; Radius of Jupiter in cm
DEFSYSV, '!euler',0.57721566490153286060651209d0    ; Euler constant
DEFSYSV, '!qe',4.8d-10 ; Charge of the electron
