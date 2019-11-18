      SUBROUTINE GEOCHK( JSCN, ISCN, STARTB, TGEOEND, OKGEO, USEGEO,
     1                   SEGELEV )
Cf2py intent(in) JSCN, ISCN, STARTB, TGEOEND
C 2 argument which are actually out parameters, but the in is required
C to define their dimension
Cf2py intent(in, out) OKGEO, USEGEO
Cf2py intent(out) SEGELEV
C
C     Make a pass through all the geodetic sources looking at the 
C     midpoint of the block to see if enough stations are up.  It tolerates
C     sources somewhat below the limit to allow for the finite time of
C     the geodetic sequence.  Set OKGEO to true if there are enough
C     stations up for the source.
C
C     A main intent is to pare down the sources being considered.
C     Use a dummy ISCN that should be an empty slot for this test.
C     Set USETIME so that TAPPROX is used.
C
C     Try to provide a list (in USEGEO) that will optimize the 
C     chances of getting a good set.  When there are a lot of 
C     sources, it is very easy  to never see the right combinations.  
C     USEGEO will prioritize the sources to try to use.  OKGEO means a 
C     source can be used at all.
C
C     First scheme tried - give sources with low elevation scans an
C     extra chance of being scheduled for each low antenna.  This didn't
C     work too well but accounts for the name (CHANCE) of the array
C     olding the likely sources in the MAKESEG routine.
C
C     Next scheme tried is to keep a source only if it is one of the 3 
C     lowest (but above 11 deg) for some station or one of the 2 highest.
C     Use LOWS(I,STAS) and HIGHS(I,STAS)
C
C     The latest scheme, as of this writing, uses the priority 1 or 2 
C     sources that are closest to the previous source (whether or not it
C     is in the geodetic sequence).  From the 5 closest, it randomly picks
C     one.  Then repeats that until 4 sources are selected.  After that,
C     it does trial fits for SecZ and picks the source that helps the
C     most (see MAKESEG for details).
C
C     Inputs:
C        JSCN:      Template scan (<ISCN usually)
C        ISCN:      First scan of the sequence.
C        STARTB:    Geodetic sequence start time.
C        TGEOEND:   Geodetic sequence end time.
C     Outputs:
C        OKGEO:     Logical flag for source is usable.
C        USEGEO:    Integer priority for each source.
C
      INCLUDE  'sched.inc'
C
      DOUBLE PRECISION  STARTB, TGEOEND
      DOUBLE PRECISION  SRCSEP, REQSEP, TFREQ
      INTEGER           JSCN, ISCN, USEGEO(*)
      LOGICAL           OKGEO(*)
      REAL              SEGELEV(MAXSTA,MGEO)
C
      DOUBLE PRECISION  TAPPROX, TIMRAD, RAS, DECS, SLA_DSEP
      REAL              ELTOL
      INTEGER           NLOW, NHIGH
      INTEGER           ISTA, ICH, IGEO, LSCN, MSPRT
      INTEGER           NREJECT, LASTISCN(MAXSTA)
      INTEGER           YR, DY, NGOOD
      LOGICAL           OKSTA(MAXSTA), USETIME
      CHARACTER         TFORM*8, CTIME*8
C
C------------------------------------------------------------------
C     Initialize USETIME so MAKESCN will use TAPPROX without 
C     adjustment.  Set TAPPROX to the center of the sequence.
C     
C
      USETIME = .TRUE.
      DO ISTA = 1, NSTA
         LASTISCN(ISTA) = 0
      END DO
      TAPPROX = ( TGEOEND + STARTB ) / 2.D0
      NREJECT = 0
C
      IF( NSTA .GT. 20 ) THEN
         CALL WLOG( 1, 'GEOCHK:  Printing only first 20 stations '
     1        // 'information' )
         MSPRT = 20
      ELSE
         MSPRT = NSTA
      END IF
C
C     Write some messages to the user about the source list to
C     follow.
C
      MSGTXT = '             --------------------- '
      CALL WLOG( 1, MSGTXT )
      MSGTXT = ' '
      CALL TIMEJ( TAPPROX, YR, DY, TIMRAD )
      CTIME = TFORM( TIMRAD, 'T', 0, 2, 2, '::@' )
      WRITE( MSGTXT, '( A, I4, I4, 1X, A )' ) 
     1      'Building geodetic segment centered at ', YR, DY, CTIME
      CALL WLOG( 1, MSGTXT )
      MSGTXT = ' '
      WRITE( MSGTXT, 
     1      '( A, I2, 4X, A, I4, 2X, A, I3, 3X, A, I3 )' )
     2  '   Using GEOPRT= ', GEOPRT,  '  GEOTRIES=', GEOTRIES, 
     3  '  GEOBACK=', GEOBACK, '  GEOSREP=', GEOSREP
      CALL WLOG( 1, MSGTXT )
      MSGTXT = ' '
      WRITE( MSGTXT, '( A, F6.2, A, F6.1, A, F6.2, A, F6.2 )' )
     1  '         GEOSLEW=', GEOSLEW, '  GEOSLOW= ', GEOSLOW,
     3  '  GELOWEL=', GEOLOWEL,'  GEOHIEL=', GEOHIEL
      CALL WLOG( 1, MSGTXT )
      CALL WLOG( 1, 
     1  '         See sched.runlog for details of the build process.' )
C
      IF( GEOPRT .GE. 0 ) THEN
         MSGTXT = ' '
         WRITE( MSGTXT, '( A )' ),'       Note in fit, SecZ < 4 '
     1     // 'treated as 4 to avoid favoring'
     2     // ' extreme low elevations.'
         CALL WLOG( 0, MSGTXT )
         CALL WLOG( 0, 
     1       'Elevations at center for sources considered are: ' )
         MSGTXT = ' '
         WRITE( MSGTXT, '( 18X, A, 20( A3, 2X ) )' ) 'Prio ',
     1        (STCODE(STANUM(ISTA)),ISTA=1,MSPRT)
         CALL WLOG( 0, MSGTXT )
         MSGTXT = ' '
      END IF
C
C     Set the limits and required numbers for finding sources
C     that contribute both high and low elevation data.
C     The ELTOL allows us to look at sources that are below the
C     elevation cutoff in the center of the segment, but not the 
C     edges.  But the usage here is only to set the priorities. 
C     At the time sources are picked, the actual geometry will be
C     picked, so for now, set ELTOL to zero.
C     The default GEOLOWEL = 23 corresponds to SecZ of 2.55 while 
C     the default GEOHIEL of 40 corresponds to 1.55, giving a 
C     spread of 1.  These parameters are user adjustable. Again, 
C     this is used for selecting the initial sources in a segment.
C
      ELTOL = 0.0
C
C     Set up to test the sun distance.  Sources too close to 
C     the sun were selected uncomfortably often during testing of 
C     the geo segment insertion.  I considered moving the call to 
C     GETSUN to before this routine, but it needs TFIRST and
C     TLAST which are calculated in SCHOPT.
C
C     For parallel schedules with the RDBE and old system, I have
C     found that small changes in frequency between the setups
C     can cause a source to be flagged for one and not the other.
C     That can completely change the results of the geodetic 
C     segment optimization.  

      CALL SUNPOS( TAPPROX, RAS, DECS )
      TFREQ = ( SFFREQ(1,SETNUM(JSCN)) / 1000.D0 )
      REQSEP = 60.D0 * TFREQ**(-0.6)
C
C     Loop through the sources getting the geometry and determining
C     which sources are useful for this segment.  Those that can be
C     used at all will have OKGEO set true and have an assigned
C     USEGEO priority.
C
C     For testing against OPMINEL, allow the source to be a bit
C     lower than the limit because the final use may be at
C     a somewhat different time.  ISCN + 1 is arbitrary and is out
C     of the way of other activity for the moment.
C
      DO IGEO = 1, NGEO
         LSCN = ISCN + 1
C
C        Make a scan with the geodetic source IGEO.  This gets all
C        the geometric information.
C
         CALL MAKESCN( LASTISCN, LSCN, JSCN, GEOSRCI(IGEO),
     1        GEOSRC(IGEO), TAPPROX, OPMINEL(JSCN) - ELTOL,
     2        NGOOD, OKSTA, USETIME )
C
C        Save the elevations.
C
         DO ISTA = 1, NSTA
            SEGELEV(ISTA,IGEO) = (EL1(LSCN,ISTA)+EL2(LSCN,ISTA))/2.0
         END DO
C
C        Make sure enough antennas are good for this source.  
C
         OKGEO(IGEO) = NGOOD .GE. OPMIAN(JSCN)
C
C        Check the sun distance.
C
         SRCSEP = SLA_DSEP( RAP(GEOSRCI(IGEO)), DECP(GEOSRCI(IGEO)),
     1         RAS, DECS ) / RADDEG
         OKGEO(IGEO) = OKGEO(IGEO) .AND. SRCSEP .GT. REQSEP 
C
C        Count those turned down.  This will be tested later to 
C        determine if there will be a problem.  Set USEGEO out of
C        range.
C
         IF( .NOT. OKGEO(IGEO) ) THEN
            NREJECT = NREJECT + 1
            USEGEO(IGEO) = 9
         END IF        
C
C        For sources that are usable, assign priorities.
C        Note that we will work from low priority critera to high
C        priority criteria.
C
C        For normal observations (.GE. 6 stations), the USEGEO priority
C        settings are:
C          5:  Meets OKGEO constraints, but otherwise not interesting.
C          4:  One or more station below GEOLOWEL+10 
C          3:  One or more low station.
C          2:  One or more low and three or more high stations or
C                  three or more low el stations.
C          1:  Two or more low and two or more high stations.
C
         IF( OKGEO(IGEO) ) THEN
            USEGEO(IGEO) = 5
            DO ISTA = 1, NSTA
               IF( STASCN(LSCN,ISTA) .AND.
     1            SEGELEV(ISTA,IGEO) .LE. GEOLOWEL + 10.0 ) 
     2            USEGEO(IGEO) = 4
            END DO
            DO ISTA = 1, NSTA
               IF( STASCN(LSCN,ISTA) .AND.
     1            SEGELEV(ISTA,IGEO) .LE. GEOLOWEL  ) 
     2            USEGEO(IGEO) = 3
            END DO
C
C           Now the multi-station criteria.
C
            NLOW = 0
            NHIGH = 0
            DO ISTA = 1, NSTA
               IF( STASCN(LSCN,ISTA) .AND.
     1             SEGELEV(ISTA,IGEO) .GT. OPMINEL(JSCN) - ELTOL .AND. 
     2             SEGELEV(ISTA,IGEO) .LE. GEOLOWEL )
     3             NLOW = NLOW + 1
               IF( STASCN(LSCN,ISTA) .AND.
     1             SEGELEV(ISTA,IGEO) .GE. GEOHIEL )
     2             NHIGH = NHIGH + 1
            END DO
            IF( ( NLOW .GE. 1 .AND. NHIGH .GE. 3 ) .OR. 
     1          NLOW .GE. 3 ) USEGEO(IGEO) = 2
            IF( NLOW .GE. 2 .AND. NHIGH .GE. 2 ) USEGEO(IGEO) = 1
         END IF
C
C        Write a line with information about each source.
C        Don't show elevations when more than 3 deg below OPMINEL
C
         IF( GEOPRT .GE. 0) THEN
            MSGTXT = ' ' 
            WRITE(MSGTXT,'( I5, 2X, A, 1X, I1 )' ) IGEO, 
     1           GEOSRC(IGEO), USEGEO(IGEO)
            ICH = 17
            DO ISTA = 1, NSTA
               ICH = ICH + 5
               IF( SEGELEV(ISTA,IGEO) .GT. OPMINEL(LSCN) - ELTOL ) THEN
                  WRITE( MSGTXT(ICH:ICH+4), '( F5.0)' ) 
     1               SEGELEV(ISTA,IGEO)
               ELSE
                  WRITE( MSGTXT(ICH:ICH+4), '(A5)' ) '  -- '
               END IF
            END DO
            ICH = ICH + 5
            IF( SRCSEP .LE. REQSEP ) THEN
               WRITE( MSGTXT(ICH:ICH+30), '( A, F5.0, A )' )
     1              '  Too near sun:', SRCSEP, ' deg.'
            END IF
            CALL WLOG(0,MSGTXT)
         END IF
C
      END DO
C
C     Prevent an opportunity for an infinite loop.  There is still
C     a small chance if there is a source up at the middle of the
C     segment, but there is nothing at the beginning.
C
      IF( NREJECT .EQ. NGEO ) THEN
         CALL ERRLOG( 'GEOCHK:  None of the sources specified '//
     1        'for a geodetic segment are up at OPMINANT antennas. ' )
      END IF
C
      RETURN
      END
