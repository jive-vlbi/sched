      SUBROUTINE CHKGEO( JSCN, ISCN, STARTB, TGEOEND, OKGEO, USEGEO,
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
C     USEGEO will flag the sources to try to use.  OKGEO means a 
C     source can be used at all and is a superset of those with 
C     USEGEO true.
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
C     Inputs:
C        JSCN:      Template scan (<ISCN usually)
C        ISCN:      First scan of the sequence.
C        STARTB:    Geodetic sequence start time.
C        TGEOEND:   Geodetic sequence end time.
C     Outputs:
C        OKGEO:     Logical flag for source is usable.
C        USEGEO:    Logical flag for favored source.
C
      INCLUDE  'sched.inc'
C
      DOUBLE PRECISION  STARTB, TGEOEND
      INTEGER           JSCN, ISCN
      LOGICAL           OKGEO(*), USEGEO(*)
      REAL              SEGELEV(MAXSTA,MGEO)
C
      DOUBLE PRECISION  TAPPROX, TIMRAD
      REAL              LOWLIM, HIGHLIM1, HIGHLIM2, ELTOL
      INTEGER           RNLOW, RNHIGH, NLOW, NHIGH1, NHIGH2
      INTEGER           I, ISTA, ICH, IGEO, LSCN 
      INTEGER           NREJECT, LASTLSCN(MAXSTA)
      INTEGER           YR, DY, NGOOD, OKSTA(MAXSTA)
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
      IF( GEOPRT ) THEN
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
C     Note that it may be necessary to use sources that are 
C     low at only one antenna if that antenna is near the edge
C     of the array.  The HIGH2 variables allow a scan to be accepted
C     if it contributes lots of high elevation data.  I think that 
C     could be useful for isolating the clock terms.
C
      LOWLIM = 23.5
      HIGHLIM1 = 40.0
      HIGHLIM2 = 55.0
      RNLOW = 1
      RNHIGH = 1
      IF( NSTA .GE. 6 ) THEN
         RNLOW = 2
         RNHIGH = 2
      END IF
      ELTOL = 3.0
C
C     Loop through the sources getting the geometry and determining
C     which sources are useful for this segment.  Those that can be
C     used at all will have OKGEO set and those chosen as better will
C     have USEGEO set positive.
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
C        determine if there will be a problem.
C
         IF( .NOT. OKGEO(IGEO) ) THEN
            NREJECT = NREJECT + 1
         END IF        
C
C        For sources that are usable, pick out the most useful ones.
C
         IF( OKGEO(IGEO) ) THEN
C
C           Detect the sources that have a good range of elevations.
C           They are the ones of interest.
C
C           Try asking for sources that can really contribute to
C           the solutions by providing both low and high elevation
C           data from different stations.
C
C           There is an issue
C
            NLOW = 0
            NHIGH1 = 0
            NHIGH2 = 0
            DO ISTA = 1, NSTA
               IF( SEGELEV(ISTA,IGEO) .GT. OPMINEL(JSCN) - ELTOL .AND. 
     1             SEGELEV(ISTA,IGEO) .LE. LOWLIM )
     2             NLOW = NLOW + 1
               IF( SEGELEV(ISTA,IGEO) .GE. HIGHLIM1 )
     1             NHIGH1 = NHIGH1 + 1
               IF( SEGELEV(ISTA,IGEO) .GE. HIGHLIM2 )
     1             NHIGH2 = NHIGH2 + 1
            END DO
C
C           Test the number of low and high stations.  Only sources
C           that have USEGEO true will be used in test schedules, at
C           least until they run out.
C
            USEGEO(IGEO) = ( NLOW .GE. RNLOW .AND. NHIGH1 .GE. RNHIGH )
      1          .OR. ( NHIGH2 .GE. NSTA - 2 )
      2          .OR. ( NLOW .GE. 1 .AND. NHIGH1 .GE. NSTA / 2 )
      3          .OR. ( NLOW .GE. NSTA / 3 .AND. NHIGH1 .GE. 1 )
         ELSE
            USEGEO(IGEO) = .FALSE.
         END IF
C
C        Write a line with information about each source.
C        Don't show elevations when more than 3 deg below OPMINEL
C	 	 
         IF( GEOPRT ) THEN
            MSGTXT = ' ' 
            IF( USEGEO(IGEO) ) THEN
               WRITE(MSGTXT,'( I5, 2X, A, 1X, A1 )' ) IGEO, 
     1           GEOSRC(IGEO), '*'
            ELSE
               WRITE(MSGTXT,'( I5, 2X, A )' ) IGEO, GEOSRC(IGEO)
            END IF
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
         CALL ERRLOG( 'MAKEGEO:  None of the sources specified '//
     1        'for a geodetic segment are up at OPMINANT antennas. ' )
      END IF
C
      RETURN
      END
