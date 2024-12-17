      SUBROUTINE JPLEP2( FILE, NAME, TIME, LONG, LAT, HEIGHT, OBSV, 
     1                   RA, DEC, DRA, DDEC, DIST, IER )
C
C     Top level routine for SCHED for finding coordinates of 
C     planets and other solar system bodies.
C
C ***  Stub version.  This responds as if the source were not 
C      available.  This can be used for sites that do not need
C      the planets option and that have trouble with the OPEN 
C      statement imbedded in the routines (does not use VLBOPE).
C         RCW  Jun 96
C
C     Coordinates in EPOCH='J2000' will be wanted for the VLBA.  
C     For the VLA, the will be wanted for EPOCH='DATE'.
C
C     The first calls to this routine will be just to determine if
C     a specified source is in the ephemeris and to get nominal
C     coordinates if it is.  IER is used to tell the result.
C
C     Input variables:
         CHARACTER         FILE*(*)     ! Ephemeris file name (<80 chr).
         CHARACTER         NAME*(*)     ! Object name (up to 12 char).
         DOUBLE PRECISION  TIME         ! Modified Julian day (UTC).
         DOUBLE PRECISION  LONG         ! East longitude (radians).
         DOUBLE PRECISION  LAT          ! Geodetic latitude (radians).
         DOUBLE PRECISION  HEIGHT       ! Geodetic height (meters).
         CHARACTER         OBSV*(*)     ! 'VLBA' or 'VLA'
C     Returned data:
         DOUBLE PRECISION  RA           ! RA (J2000) (radians).
         DOUBLE PRECISION  DEC          ! Dec (J2000) (radians).
         DOUBLE PRECISION  DRA          ! RA rate of date (seconds of
C                                       !    time per UT day).
         DOUBLE PRECISION  DDEC         ! Dec rate of date (seconds of
C                                       !    arc per UT day).
         DOUBLE PRECISION  DIST         ! distance to planet (AU)
         INTEGER           IER          ! Error condition (0 => ok).
C ---------------------------------------------------------------------
      INTEGER NPLAN, II
      PARAMETER (NPLAN = 10)
      CHARACTER PLANET(NPLAN)*12
C
      DATA PLANET / 'MERCURY     ', 'VENUS       ', 'MOON        ',
     *              'MARS        ', 'JUPITER     ', 'SATURN      ',
     *              'URANUS      ', 'NEPTUNE     ', 'PLUTO       ',
     *              'SUN         ' /
C ---------------------------------------------------------------------
      IER = 1
      II = 1
      DO WHILE (II .LE. NPLAN .AND. IER .EQ. 1)
         IF (NAME .EQ. PLANET(II)) THEN
            CALL PUTOUT( 'JPLEPH: Planet below requested, but ' //
     1            'ephemeris routines not linked with SCHED.' )
            CALL PUTOUT( NAME )
         END IF
         II = II + 1
      END DO
      RETURN
      END





