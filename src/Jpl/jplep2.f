      SUBROUTINE JPLEP2( FILE, NAME, TIME, LONG, LAT, HEIGHT, OBSV, 
     1                   RA, DEC, DRA, DDEC, DIST, IER )
C
C  !!!NOTE!!!
C     This version of the ephemeris subroutine (JPLEP2) is a copy of
C     the standard subroutine (JPLEPH).  JPLEP2 calls JPLPO2 instead 
C     of JPLPOS.  JPLPO2 only considers the 2 specific cases: VLA and 
C     VLBA.  See notes in JPLPO2.
C  !!!END NOTE!!!
C
C     15 Oct 2001  Added SAVE FIRST, SS to this routine  RCW
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
         CHARACTER         OBSV*(*)     ! Which observatory
C                                       !  ('VLA' or 'VLBA').
C     Returned data:
         DOUBLE PRECISION  RA           ! RA (radians).
         DOUBLE PRECISION  DEC          ! Dec (radians).
         DOUBLE PRECISION  DRA          ! RA rate (seconds of
C                                       !    time per UT day).
         DOUBLE PRECISION  DDEC         ! Dec rate (seconds of
C                                       !    arc per UT day).
         DOUBLE PRECISION  DIST         ! distance to planet (AU)
         INTEGER           IER          ! Error condition (0 => ok).
C                                       !    1 => NAME not supported
C                                       !    2 => TIME not supported
C ---------------------------------------------------------------------
      DOUBLE PRECISION TDTTIM, JTDT, sla_DAT, LEAPS, VALS(400), SS(3)
      INTEGER NPLAN, NP, II, NVS
      PARAMETER (NPLAN = 10)
      LOGICAL FIRST
      CHARACTER PLANET(NPLAN)*12, NAMS(400)*6, EPHFI2*80

      COMMON / FNAME / EPHFI2

      DATA PLANET / 'MERCURY     ', 'VENUS       ', 'MOON        ',
     *              'MARS        ', 'JUPITER     ', 'SATURN      ',
     *              'URANUS      ', 'NEPTUNE     ', 'PLUTO       ',
     *              'SUN         ' /
      DATA FIRST / .TRUE. /
      SAVE        FIRST, SS

      IER = 1
      II = 1
      DO WHILE (II .LE. NPLAN .AND. IER .EQ. 1)
         IF (NAME .EQ. PLANET(II)) THEN
            IER = 0
            NP = II
         END IF
         II = II + 1
      END DO
      IF (IER .EQ. 0) THEN
C find the TDT (given UTC)
         LEAPS = sla_DAT (TIME)
         TDTTIM = TIME + (LEAPS + 32.184D0) / 86400.0D0
         JTDT = TDTTIM + 2400000.5D0
         IF (FIRST) THEN
            FIRST = .FALSE.
            EPHFI2 = FILE
            CALL CONST (NAMS, VALS, SS, NVS)
         END IF
         IF (JTDT .LT. SS(1) .OR. JTDT .GT. SS(2)) THEN
C ephemeris doesn't support that date/time
            IER = 2
         ELSE
            CALL JPLPO2 (TDTTIM, NP, LONG, LAT, HEIGHT, RA, DRA, DEC,
     *                   DDEC, DIST, OBSV)
         END IF
      END IF
      RETURN
      END
