      SUBROUTINE GEOCHK( JSCN, ISCN, STARTB, TGEOEND, OKGEO, USEGEO,
     1                   SEGELEV )
C
C     Make a pass through all the geodetic sources looking at the 
C     midpoint of the block to see if enough stations are up.  It tolerates
C     sources somewhat below the limit to allow for the finite time of
C     the geodetic sequence.  Set OKGEO to true if there are enough
C     stations up for the source.
C
C     A main intent is to pare down the sources being considered.
C     Use a dummy ISCN that should be an empty slot for this test.
C     Initialize the LASTLSCN values so that TAPPROX is used.
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
      INTEGER           JSCN, ISCN, USEGEO(*)
      LOGICAL           OKGEO(*)
      REAL              SEGELEV(MAXSTA,MGEO)
C
      DOUBLE PRECISION  TAPPROX, TIMRAD
      REAL              ELTOL
      INTEGER           NLOW, NHIGH
      INTEGER           ISTA, ICH, IGEO, LSCN 
      INTEGER           NREJECT, LASTLSCN(MAXSTA)
      INTEGER           YR, DY, NGOOD
      LOGICAL           OKSTA(MAXSTA)
      CHARACTER         TFORM*8, CTIME*8
C
C------------------------------------------------------------------
C     Initialize LASTLSCN so MAKESCN will use TAPPROX without 
C     adjustment.  Set TAPPROX to the center of the sequence.
C
      DO ISTA = 1, NSTA
         LASTLSCN(ISTA) = 0
      END DO
      TAPPROX = ( TGEOEND + STARTB ) / 2.D0
      NREJECT = 0
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
      WRITE( MSGTXT, '( A, I2, A, I4, A, I3, A, F6.2, A, F6.1 )' )
     1  '   Using GEOPRT=', GEOPRT, '  GEOTRIES=', GEOTRIES, 
     2  '  GEOBACK=', GEOBACK, '  GEOSLEW=', GEOSLEW,
     3  '  GEOSLOW=', GEOSLOW
      CALL WLOG( 1, MSGTXT )
      IF( GEOPRT .GE. 0 ) THEN
         CALL WLOG( 1, 
     1       'Elevations at center for sources considered are: ' )
         IF( NSTA .GT. 20 ) CALL WLOG( 1, '(Only first 20 stations)' )
         MSGTXT = ' '
         WRITE( MSGTXT, '( 21X, 20A5 )' ) 
     1        (STCODE(STANUM(ISTA)),ISTA=1,NSTA)
         CALL WLOG( 1, MSGTXT )
         MSGTXT = ' '
      END IF
C
C     Set the limits and required numbers for finding sources
C     that contribute both high and low elevation data.
C     The ELTOL allows us to look at sources that are below the
C     elevation cutoff in the center of the segment, but not the 
C     edges.
C
      GEOLOWEL = 20.0
      GEOHIEL = 40.0
      ELTOL = 3.0
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
         CALL MAKESCN( LASTLSCN, LSCN, JSCN, GEOSRCI(IGEO),
     1        GEOSRC(IGEO), TAPPROX, OPMINEL(JSCN) - 4.0,
     2        NGOOD, OKSTA )
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
C          4:  More than one station below GEOLOWEL+10 
C          3:  More than one low station.
C          2:  More than one low and one high stations.
C          1:  More than two low and two high stations.
C
         IF( OKGEO(IGEO) ) THEN
            USEGEO(IGEO) = 5
            DO ISTA = 1, NSTA
               IF( SEGELEV(ISTA,IGEO) .LE. GEOLOWEL + 10.0 ) 
     1            USEGEO(IGEO) = 4
            END DO
            DO ISTA = 1, NSTA
               IF( SEGELEV(ISTA,IGEO) .LE. GEOLOWEL  ) 
     1            USEGEO(IGEO) = 3
            END DO
C
C           Now the multi-station criteria.
C
            NLOW = 0
            NHIGH = 0
            DO ISTA = 1, NSTA
               IF( SEGELEV(ISTA,IGEO) .GT. OPMINEL(JSCN) - ELTOL .AND. 
     1             SEGELEV(ISTA,IGEO) .LE. GEOLOWEL )
     2             NLOW = NLOW + 1
               IF( SEGELEV(ISTA,IGEO) .GE. GEOHIEL )
     1             NHIGH = NHIGH + 1
            END DO
            IF( NLOW .GE. 1 .AND. NHIGH .GE. 1 ) USEGEO(IGEO) = 2
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
C           write(*,*) 'geochk, writing line of data ', ich, nsta
            CALL WLOG(1,MSGTXT)
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
