      SUBROUTINE HALIM( ISCN, IREFSTA, HAMIN, HAMAX, THAMIN, THAMAX )
Cf2py intent(in) ISCN, IREFSTA
Cf2py intent(out) HAMIN, HAMAX, THAMIN, THAMAX
C
C     Routine for optimization modes.
C     This routine figures out the hour angle limits for an input scan
C     based on the position of the source, the antenna chosen for 
C     hour angle calculation, the minimum elevation, the minimum number
C     of antennas, and, approximately, the horizon at each antenna.  It
C     also gets the appropriate times corresponding to those limits.
C
C     HAMIN and HAMAX are the reference station hour angle limits that
C        conform to the required number of stations being up.  
C     THAMIN and THAMAX are MJD times that go with those limits.  
C        THAMIN is the last rise before the experiment stop time.  
C        THAMAX is the first set after the start time.
C
      INCLUDE 'sched.inc'
C
      INTEGER           ISCN, ISTA, IREFSTA, NUP
      REAL              HAMIN, HAMAX
      LOGICAL           GOOD, HADGOOD, SEENRISE, SEENSET
      CHARACTER         HORCHK*1
      DOUBLE PRECISION  TTEST, TINT, THAMIN, THAMAX
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'HALIM starting.' )
C
C     Calculate the local hour angle limits for each scan.  
C     The source must be up at the minimum number of stations.
C     A source is up when it is above the minimum elevation for an 
C     optimization is above the horizon, and is above the antenna 
C     hardware limits.
C
C     Do an iterative search in time.  Because of the complexities
C     of checking horizons etc, this is easier than trying to calculate
C     the result analytically.  This routine is only called once per 
C     input scan so some inefficiency can be tolerated.
C
C     Go to the time of transit at the reference antenna first.
C     This is the transit closest to the experiment start time.
C     Then work backward for the rise time and forward for the stop
C     time.  Don't worry about staying in experiment boundaries -
C     we're just finding hour angle limits here.
C
C     Note that what I'm doing might not work too well with an array
C     that is distributed around the Earth with a reference antenna
C     on the fringes.  For such situations the simple concepts of 
C     a rise and set time are not so clear cut.  The reference station
C     should be in the main cluster of antennas.
C
C     In fact, for now I don't want to deal with cases where the 
C     reference antenna would not participate in a valid scan.  That
C     potentially adds layers of complexity that are unnecessary in
C     for the survey applications that this mode is for.
C   
C     This algorithm might also be fooled if a source sets on the
C     array for less than the initial TINT (1 hour).
C
C     Note that throughout, the convention is that times are in days
C     and hour angles are in hours.  Also recall that intervals of
C     hour angle are different from intervals of time by the sidereal
C     rate (SIDR=1.0027...).
C
C
C     First be sure that the source can be seen from the required
C     number of stations.  This is just a declination vs latitude
C     test so there may still be a problem trying to get enough
C     stations on source at once.
C
      NUP = 0
      DO ISTA = 1, NSTA
         IF( LAT(STANUM(ISTA)) - 90.0 * RADDEG  + 
     1        OPMINEL(ISCN) * RADDEG .LE. DECP(SRCNUM(ISCN)) ) THEN
            NUP = NUP + 1
         END IF
      END DO
      IF( NUP .LT. OPMIAN(ISCN) ) THEN
         MSGTXT = 'Fewer than OPMINANT stations can ever see source ' //
     1        SCNSRC(ISCN) // ' above OPMINEL.'
         CALL WLOG( 1, MSGTXT )
         CALL ERRLOG( 'Leave out the source or change OPMINANT ' //
     1        'or OPMINEL' )
      END IF
C
C     ---------------------------
C     Work backward from the stop time to find the last rise time to 
C     within about a minute.
C
      TTEST = STARTJ(1) + OPDUR
      TINT = -1.0D0 / 24.D0
      HADGOOD = .FALSE.
      SEENRISE = .FALSE.
      DO WHILE( ABS( TINT ) .GE. 30.D0 / 86400.D0 )
         NUP = 0 
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) ) THEN
C
               CALL SCHGEO( ISCN, ISTA, TTEST, HA1(ISCN,ISTA), 
     1                   EL1(ISCN,ISTA), AZ1(ISCN,ISTA),
     2                   LST1(ISCN,ISTA), PA1(ISCN,ISTA) )
               UP1(ISCN,ISTA) = HORCHK( STANUM(ISTA), HA1(ISCN,ISTA),
     1                 AZ1(ISCN,ISTA), EL1(ISCN,ISTA), SRCNUM(ISCN) )
C
               IF( UP1(ISCN,ISTA) .EQ. ' ' .AND. 
     1             EL1(ISCN,ISTA) .GT. OPMINEL(ISCN) ) THEN
                  NUP = NUP + 1
               END IF
            END IF
         END DO
C
C        Check if for this time, enough antennas can see the source.
C        If so, this is a "GOOD" time.
C
         GOOD = NUP .GE. OPMIAN(ISCN)
C
C        Keep a record of whether the source was ever up at enough 
C        stations.  Once that is true, keep a record that a rise
C        was seen.
C
         IF( GOOD ) HADGOOD = .TRUE.
         IF( HADGOOD .AND. .NOT. GOOD ) SEENRISE = .TRUE.
C
C        Test if there never is a rise in a day.  Go a bit more than
C        a day in case of edge effects.  If there was some good data,
C        then presume that the source never set.  Set HAMIN and
C        HAMAX accordingly and skip most of rest of routine.  Set
C        the time range to start at the observation start.  If
C        there was no good data, assume that the source was never
C        up at enough stations.  Quit and give user a chance to 
C        adjust matters.
C
         IF( ABS( STARTJ(1) + OPDUR - TTEST ) .GT. 1.2D0 ) THEN
            IF( HADGOOD ) THEN
               HAMIN = -24.0
               HAMAX = 24.0
               THAMIN = STARTJ(1)
               THAMAX = STARTJ(1) + 1.D0
               GO TO 999
            ELSE
               MSGTXT = 'Fewer than OPMINANT stations can ever see '//
     1          'source ' // SCNSRC(ISCN) // ' above OPMINEL at once.'
               CALL WLOG( 1, MSGTXT )
               CALL ERRLOG( 'Leave out the source or change ' //
     1                   'OPMINANT or OPMINEL' )
            END IF
         END IF
C
C        If a rise has been seen, cut TINT in half.  If the source
C        is up for the current scan, make TINT negative.  If down,
C        make it positive.  This should home on the rise time.
C
         IF( SEENRISE ) THEN
            TINT = TINT / 2.D0
            IF( GOOD ) THEN
               TINT = -1.D0 * ABS( TINT )
            ELSE
               TINT = ABS( TINT )
            END IF
         END IF
         TTEST = TTEST + TINT
      END DO
C
C     Now the best rise time estimate should be in TTEST.
C     Get the rise hour angle and transfer the rise time to the output
C     variable.
C
      CALL SCHGEO( ISCN, IREFSTA, TTEST, HA1(ISCN,ISTA), 
     1             EL1(ISCN,ISTA), AZ1(ISCN,ISTA),
     2             LST1(ISCN,ISTA), PA1(ISCN,ISTA) )
      HAMIN = HA1(ISCN,ISTA)
      THAMIN = TTEST
C
C     ---------------------------
C     Now repeat for the set time.
C     ---------------------------
C     Work forward from the start time to find the first set time.
C
      TTEST = STARTJ(1)
      TINT = 1.0D0 / 24.D0
      HADGOOD = .FALSE.
      SEENSET = .FALSE.
      DO WHILE( ABS( TINT ) .GE. 30.D0 / 86400.D0 )
         NUP = 0 
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) ) THEN
C
               CALL SCHGEO( ISCN, ISTA, TTEST, HA1(ISCN,ISTA), 
     1                   EL1(ISCN,ISTA), AZ1(ISCN,ISTA),
     2                   LST1(ISCN,ISTA), PA1(ISCN,ISTA) )
               UP1(ISCN,ISTA) = HORCHK( STANUM(ISTA), HA1(ISCN,ISTA),
     1                 AZ1(ISCN,ISTA), EL1(ISCN,ISTA), SRCNUM(ISCN) )
C
               IF( UP1(ISCN,ISTA) .EQ. ' ' .AND. 
     1             EL1(ISCN,ISTA) .GT. OPMINEL(ISCN) ) THEN
                  NUP = NUP + 1
               END IF
            END IF
         END DO
         GOOD = NUP .GE. OPMIAN(ISCN)
         IF( GOOD ) HADGOOD = .TRUE.
         IF( HADGOOD .AND. .NOT. GOOD ) SEENSET = .TRUE.
C
C        Test if there never is a rise in a day.  We shouldn't get
C        here because of the rise test.  But perhaps the non-complete
C        sampling could do it.  The always up case could then override
C        a HAMIN found by the rise test, but any set source will not
C        be down by much and, since down will usually have to do with
C        the OPMINEL, this won't really matter.
C
         IF( ABS( TTEST - STARTJ(1) ) .GT. 1.2D0 ) THEN
            IF( HADGOOD ) THEN 
               HAMIN = -24.0
               HAMAX = 24.0
               THAMIN = STARTJ(1)
               THAMAX = STARTJ(1) + 1.D0
               GO TO 999
            ELSE
               MSGTXT = 'Fewer than OPMINANT stations can ever see '//
     1           'source ' // SCNSRC(ISCN) // 
     2           ' above OPMINEL at once (set).'
               CALL WLOG( 1, MSGTXT )
               CALL ERRLOG( 'Leave out the source or change ' //
     1                   'OPMINANT or OPMINEL' )
            END IF
         END IF
C
C        If a set has been seen, cut TINT in half.  If the source
C        is up for the current scan, make TINT positive.  If down,
C        make it negative.  This should home on the set time.
C
         IF( SEENSET ) THEN
            TINT = TINT / 2.D0
            IF( GOOD ) THEN
               TINT = ABS( TINT )
            ELSE
               TINT = -1.D0 * ABS( TINT )
            END IF
         END IF
         TTEST = TTEST + TINT
      END DO
C
C     Now the desired set time estimate should be in TTEST
C
      CALL SCHGEO( ISCN, IREFSTA, TTEST, HA1(ISCN,ISTA), 
     1             EL1(ISCN,ISTA), AZ1(ISCN,ISTA),
     2             LST1(ISCN,ISTA), PA1(ISCN,ISTA) )
      HAMAX = HA1(ISCN,ISTA)
      THAMAX = TTEST
C
C     Done.
C
  999 CONTINUE
      RETURN
      END
