      SUBROUTINE MAKESEG( JSCN, ISCN, LASTISCN, 
     1                    OKGEO, USEGEO, SEGELEV, STARTB, TGEOEND, 
     2                    LSCN, NTSEG, TSRC, IDUM, SIGMA )
C
C     Routine for SCHED called by GEOMAKE that invents a 
C     series of sources to try as a geodetic sequence.
C
C     This particular version selects sources that improve the
C     quality measure (or something closely related) while minimizing
C     slews.  For the first few sources, it randomly picks from
C     among the ones available with the shortest slews, including
C     knowing about where the antenna is before the segment.
C
C     Inputs:
C        JSCN:      Template scan.  Less than ISCN usually.
C        ISCN:      The first scan of the sequence
C        LASTISCN:  The preceeding scan for each station at the start.
C        OKGEO:     Sources in the geodetic set that have a useful
C                   elevation distribution.
C        SEGELEV    2D array (sta,source) of elevations
C                   Not used with current algorithm.
C        STARTB:    Start time of the sequence.
C        TGEOEND:   End time for a sequence.
C        IDUM:      Seed for the random number generator
C        SIGMA:     Used for the zenith atmosphere errors from GEOQUAL.
C                   Mainly sent here so don't need a separate array
C                   from the one in GEOMAKE.
C 
C     Outputs:
C        LSCN:      The last scan of the sequence
C        NTSEG:     Number of scans in the geodetic sequence.
C        TSRC:      The sequence of geodetic source numbers for the
C                   geodetic sequence.
C
C     The chosen output scans will be deposited in scans ISCN to LSCN.
C
      INCLUDE 'sched.inc'
C
      INTEGER            MSEG10
      PARAMETER          (MSEG10=10*MSEG)
      INTEGER            JSCN, ISCN, LSCN, LASTISCN(*)
      INTEGER            TSRC(*), NTSEG, USEGEO(*), IDUM
      LOGICAL            OKGEO(*)
      REAL               SEGELEV(MAXSTA,MGEO)
      DOUBLE PRECISION   STARTB, TGEOEND, SIGMA(*)
C
      INTEGER            ISTA, IS, ISEG, IGEO, IC
      INTEGER            LASTLSCN(MAXSTA), NNEAR, NRAND, CTSC
      INTEGER            KSCN, MAXPRIO, IISCN, NPRT
      INTEGER            NGOOD, NCHANCE, CHANCE(MSEG10), TEMPCH 
      INTEGER            INSCN(MAXSTA), INSCT, SELECTED(MGEO)
      INTEGER            HIGHSTA, HIGHSTA1, HIGHSTA2, HIGHSTA3
      INTEGER            LHIGHSTA
      INTEGER            TSRC1, TSRC2, TSRC3
      LOGICAL            OKSTA(MAXSTA), LSTAS, TEST
      LOGICAL            STAOK, NEWC, PRDEBUG
      LOGICAL            SSTASCN(MAXSTA), KSTASCN(MGEO,MAXSTA)
      REAL               RAN5, TESTQUAL, PENALTY
      REAL               SLQUAL, BSLQUAL
      REAL               FQUAL, BFQUAL, FQUALSL, BFQUALSL
      REAL               PKSIG
      REAL               SAVSIG(MAXSTA), SAVSIG1(MAXSTA)
      REAL               SAVSIG2(MAXSTA), SAVSIG3(MAXSTA)
      REAL               HIGHSIG, HIGHSIG1, HIGHSIG2, HIGHSIG3
      REAL               LHIGHSIG
      DOUBLE PRECISION   TAPPROX, TESTTIME(MSEG10), TEMPTIME
      DOUBLE PRECISION   SLEWTIME, TSEP
      CHARACTER          LSRCNAME*12
C --------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 1, 'MAKESEG starting' )
      NPRT = MIN( 20, NSTA )
C
C     Initializations
C
      ISEG = 0
      NTSEG = 0
      DO IGEO = 1, NGEO
         SELECTED(IGEO) = 0
      END DO
      DO IS = 1, MSEG
         TSRC(ISEG) = 0
      END DO
      MAXPRIO = 2
      LHIGHSTA = 0
      LHIGHSIG = 0.0
C
C     Move LASTISCN to LASTLSCAN where 
C     it can keep getting remade with each call to this routine.
C
      DO ISTA = 1, NSTA
         LASTLSCN(ISTA) = LASTISCN(ISTA)
      END DO
C
C     Set start time of the first new scan.  This is mainly
C     for the case when the segment is at the start of the
C     experiment and all LASTISCN entries are zero.
C
      STARTJ(ISCN) = STARTB
C
C     Keep track of the number of scans each station is in.
C     Mainly use this to try to avoid too few for the quality
C     measure least square fit.  Initialize the counter here.
C
      DO ISTA = 1, NSTA
        INSCN(ISTA) = 0
      END DO
C
C     Write column headers if going to print the sigmas.
C
      IF( GEOPRT .GE. 1 ) THEN
         WRITE( MSGTXT, '( T40, 20( 2X, A2, 1X) )' ) 
     1           '                      ', 
     2           (STCODE(STANUM(ISTA)),ISTA=1,NPRT)
         CALL WLOG( 1, MSGTXT )
      END IF
C
C     Start selecting scans.  Jump to 100 to get next source.
C
  100 CONTINUE
C
C        Set the scan number.
C
         ISEG = ISEG + 1
         LSCN = ISEG + ISCN - 1
C
         IF( GEOPRT .GE. 2 ) THEN
            WRITE(*,*) ' '
            WRITE(*,*) '----MAKESEG:  Starting to work on source:',
     1                  ISEG
            END IF
C
C        Set the first scan to use for quality measures and 
C        avoiding repeats.  Doing this here avoids haveing to do
C        it multiple times below.
C
         IISCN = MAX( ISCN , LSCN - GEOBACK + 1 ) 
C
C        Set the number of sources to pick before starting to 
C        measure the quality measure.  For these, restrict to
C        better sources using USEGEO and take ones that involve
C        short slews.
C
         NRAND = 4
C
C        ==========  First NRAND sources  ==========
C
         IF( ISEG .LE. NRAND ) THEN
C
C           For the first NRAND sources, just try for short slews but with
C           enough ramdom choices to make the final sequence random.  Try
C           inserting each source with USEGEO LE 2.  Check the stop
C           times (they are sensitive to where the antenna was in the
C           last scan) and select the first 5 possibilities.  Then pick
C           one of those at random.
C
C           Note that the NGOOD test should be passed if USEGEO is 
C           acceptable, but the geo segments are of finite length so
C           the up status can change compared to what GEOCHK used.
C
C           SELECTED is the last scan in which this source
C           was selected.  It is used to help prevent rapid repeats.
C
            NNEAR = 5
            NCHANCE = 0
C
C           To test fixed sequence.
C
            DO IGEO = 1, NGEO
               IF( USEGEO(IGEO) .LE. MAXPRIO  .AND. 
     1             SELECTED(IGEO) .LT. IISCN ) THEN
                  IF( ISEG .EQ. 1 ) THEN
                     TAPPROX = STARTB
                  ELSE
                     TAPPROX = STOPJ(LSCN-1) + 60.D0 * ONESEC
                  END IF
                  IF( GEOPRT .GE. 2 ) WRITE(*,*)
     1               'MAKESEG looking for nearby source. Considering ',
     2               iseg, igeo, ' ', GEOSRC(igeo), ' at ', tapprox
C
C                 Insert the new scan.  GMKSCN is like MAKESCN
C                 but also is capable of removing stations based
C                 on long slews.  It can also make or take a 
C                 predetermined list of stations to include.
C
                  CALL GMKSCN( LASTLSCN, LSCN, JSCN, GEOSRCI(IGEO),
     1                    GEOSRC(IGEO), TAPPROX, OPMINEL(JSCN), 0,
     2                    NGOOD, OKSTA, SSTASCN, 'SET  ' )
C
C                 Save SSTASCN for after source selection is done.
C
                  DO ISTA = 1, NSTA
                     KSTASCN(IGEO,ISTA) = SSTASCN(ISTA)
                  END DO
C
C                 Check that every station has been in enough scans
C                 by now.  That means they have been in half.  Recall
C                 that integer division gives the next lowest integer.
C                 Also require that enough stations be in the scan.
C
                  STAOK = .TRUE.
                  NEWC = .FALSE.
                  DO ISTA = 1, NSTA
                     INSCT = INSCN(ISTA)
                     IF( STASCN(LSCN,ISTA) ) INSCT = INSCT + 1
                     IF( INSCT .LT. ISEG / 2 ) STAOK = .FALSE.
                  END DO
                  IF( STAOK .AND. NGOOD .GE. OPMIAN(JSCN) ) THEN
C
C                    Add to the CHANCE array if we're just starting.
C                    Also for the first source of the experiment, 
C                    just add all of the qualifying sources.
C
                     IF( NCHANCE .LT. NNEAR .OR. LSCN .EQ. SCAN1 ) THEN
                        NCHANCE = NCHANCE + 1
                        CHANCE(NCHANCE) = IGEO
                        TESTTIME(NCHANCE) = STARTJ(LSCN)
                        NEWC = .TRUE.
C
C                    Accumulate the best NNEAR cases.  First replace
C                    the end of the NNEAR long list if the current
C                    one ends earlier.
C
                     ELSE IF( STARTJ(LSCN) .LT. TESTTIME(NNEAR) ) THEN
                        CHANCE(NNEAR) = IGEO
                        TESTTIME(NNEAR) = STARTJ(LSCN)
                        NEWC = .TRUE.
                     END IF
C
                     IF( GEOPRT .GE. 2 ) THEN
                        IF( LSCN .EQ. SCAN1 ) THEN
                           LSRCNAME = 'First scan'
                           TSEP = 0.D0
                        ELSE
                           LSRCNAME = SCNSRC(LSCN-1)
                           TSEP = (STARTJ(LSCN)-STOPJ(LSCN-1))*86400.D0
                        END IF
                        WRITE(*,*) 'MAKESEG getting slew info',
     1                     IGEO, LSCN, ' Last source: ', LSRCNAME, 
     2                     '  Testing: ', GEOSRC(IGEO)
                        DO ISTA = 1, NSTA
                           IF( LSCN .EQ. SCAN1 ) THEN
                              LSTAS = .FALSE.
                           ELSE
                              LSTAS = STASCN(LSCN-1,ISTA)
                           END IF
                           WRITE(*,'(A,2I5,5F7.1,2L2,F7.1)') 
     1                     ' makeseg els, azs, tslew: ', ISTA, 
     2                     LASTLSCN(ISTA), 
     3                     EL2(LASTLSCN(ISTA),ISTA), EL1(LSCN,ISTA),
     4                     AZ2(LASTLSCN(ISTA),ISTA), AZ1(LSCN,ISTA),
     5                     TSLEW(LSCN,ISTA)*86400.D0,
     6                     LSTAS, STASCN(LSCN,ISTA),
     7                     TSEP
                     
                        END DO
                     END IF
C
C                    Now push up anything that was added to where
C                    it belongs.  This is sorting the sources in the
C                    chance array by slew time.  Don't bother for the
C                    first source.
C
                     IF( NEWC .AND. .NOT. LSCN .EQ. SCAN1 ) THEN
                        DO IC = NCHANCE-1, 1, -1
                           IF( TESTTIME(IC+1) .LT. TESTTIME(IC) ) THEN
                              TEMPCH = CHANCE(IC)
                              TEMPTIME = TESTTIME(IC)
                              CHANCE(IC) = CHANCE(IC+1)
                              TESTTIME(IC) = TESTTIME(IC+1)
                              CHANCE(IC+1) = TEMPCH
                              TESTTIME(IC+1) = TEMPTIME
                           END IF
                        END DO
                     END IF
                  END IF
               END IF
            END DO
C
C           Make sure there are some sources.  If not, adjust MAXPRIO
C           With a good list, this should this should not happen.
C
            IF( NCHANCE .EQ. 0 ) THEN
               IF( GEOPRT .GE. 1 ) THEN
                  CALL WLOG( 1, 'MAKESEG: Having to reduce '//
     1               ' standards for initial sources for geodedetic '//
     2               ' sequence.' )
                  CALL WLOG( 1, '         Are you using an '//
     1               ' adequate source list?' )
               END IF
               MAXPRIO = 9
               ISEG = ISEG - 1
               GO TO 100
            END IF               
C
C           Pick one of the top choices randomly.
C
            IF( NCHANCE .GE. 1 ) THEN
               IC = INT( 1 + RAN5(IDUM) * NCHANCE )
               IF( GEOPRT .GE. 2 ) WRITE(*,*) 
     1               'makeseg 7.5 ', IC, CHANCE(IC)
               TSRC(ISEG) = CHANCE(IC)
               DO ISTA = 1, NSTA
                  SSTASCN(ISTA) = KSTASCN(TSRC(ISEG),ISTA)
               END DO
C
            END IF
            IF( GEOPRT .GE. 2 ) WRITE(*,*) 'makeseg 8', TSRC(ISEG)
C
C        ==============  Rest of sources based on fit  =============
C
         ELSE
C
C           For the rest of the sources, pick the one from the whole 
C           list that gives the lowest quality measure (highest 
C           quality), which is a combination of the fit rms's from 
C           GEOQUAL and the slew time.  This will be deterministic 
C           for each set of the first NRAND sources, but those first 
C           NRAND are random which should make the whole sequence 
C           randomly different each time the routine is called.
C
C           Note SELECTED is the last scan in which this source
C           was selected.  It is used to help prevent rapid repeats.
C
            BSLQUAL = 1.E9
            BFQUAL = 1.E9
            BFQUALSL = 1.E9
            TSRC(ISEG) = 0
            TSRC1 = 0
            TSRC2 = 0
            TSRC3 = 0
            DO IGEO = 1, NGEO
               IF( OKGEO(IGEO) .AND. SELECTED(IGEO) .LT. IISCN ) THEN
C
C                 Insert the source as the next scan to get all the
C                 required geometric parameters in a simple way.
C
                  TAPPROX = STOPJ(LSCN-1) + 60.D0 * ONESEC
C
C                 Insert the new scan.  GMKSCN is like MAKESCN
C                 but also is capable of removing stations based
C                 on long slews.  It can also make or take a 
C                 predetermined list of stations to include.
C                 Don't let it skip a scan on the station we really
C                 want to fix (LHIGHSTA)
C
                  CALL GMKSCN( LASTLSCN, LSCN, JSCN, GEOSRCI(IGEO),
     1                    GEOSRC(IGEO), TAPPROX, OPMINEL(JSCN),
     2                    LHIGHSTA, NGOOD, OKSTA, SSTASCN, 'SET  ' )
C
C                 Save SSTASCN as a function of source for use 
C                 after source selection.
C
                  DO ISTA = 1, NSTA
                     KSTASCN(IGEO,ISTA) = SSTASCN(ISTA)
                  END DO
C
C                 Decide if this is usable.
C
C                 First get the numbers needed and protect against
C                 a source choice that leaves some station with too
C                 few scans.  Additionally try to prevent having too
C                 few scans with this station in the GEOBACK look
C                 back set to do a fit.
C
                  STAOK = .TRUE.
                  DO ISTA = 1, NSTA
C
C                    Require that a station be in half of scans.
C
                     INSCT = INSCN(ISTA)
                     IF( STASCN(LSCN,ISTA) ) INSCT = INSCT + 1
                     IF( INSCT .LT. ISEG / 2 ) STAOK = .FALSE.
C
C                    Require a station have 2 scans in the lookback
C                    set.  Only need to worry if the station is not
C                    in this scan.
C
                     IF( .NOT. STASCN(LSCN,ISTA) ) THEN
                        CTSC = 0
                        DO IS = IISCN, LSCN - 1
                           IF( STASCN(IS,ISTA) ) CTSC = CTSC + 1
                        END DO
                        IF( CTSC .LT. 2 ) STAOK = .FALSE.
                     END IF
C
                  END DO
C
                  IF( NGOOD .GE. OPMIAN(JSCN) .AND. STAOK .AND.
     1                STOPJ(LSCN) .LE. TGEOEND ) THEN
C
C                    Run GEOQUAL on the scans we have so far.
C
C                    Set up for initial call or incremental call.
C                    There shouldn't be any changes in any but the
C                    current scan, except on the first call for
C                    this segment.  GEOQUAL can be told not to bother
C                    repeating calculations for scans IISCN to KSCN
C                    by setting KSCN appropriately.  TSRC(ISEG) is 
C                    just a handy number that can be used to detect
C                    the first time GEOQUAL will be run for finding
C                    a source to add for scan LSCN.
C
C                    While at it, set up for a finite lookback
C                    time by using IISCN.  This is to prevent the 
C                    sequence from stagnating after it has a good 
C                    set for all stations.  With the current
C                    version of the algorithm, this might not be
C                    useful
C
C                    Also set the minimum elevation that will be
C                    rewarded in the fit so that not too much
C                    emphasis is placed on scans very close to
C                    the horizon.  The number is after JSCN
C                    in the GEOQUAL call.  This is done only for
C                    the process of selecting sources within the
C                    segment.  Later for choosing a segment, that
C                    minimum elevation will be set to zero.
C
                     IF( TSRC(ISEG) .EQ. 0 ) THEN
                        KSCN = IISCN
                     ELSE
                        KSCN = LSCN
                     END IF
                     PRDEBUG = GEOPRT .GE. 2
C
                     IF( GEOPRT .GE. 2 ) THEN
                        WRITE(*,*) ' '
                        WRITE(*,*) 
     1                   'Makeseg about to test quality ', ISEG, 
     2                   ISCN, IISCN, KSCN, LSCN, JSCN,
     3                   '  geosrc:', IGEO, ' ', GEOSRC(IGEO)
                     END IF
C
                     CALL GEOQUAL( IISCN, KSCN, LSCN, JSCN, 16.0,
     1                    TESTQUAL, PRDEBUG, SIGMA )
C
C                    Construct the quality measures we want to use
C                    here.  Unlike in the final selection, here
C                    we want to take slews into account.  We also 
C                    want to reward improvement for stations that
C                    aren't the worst.  Note that, if there were a
C                    failure to invert the matrix, probably from
C                    inadequate data for some station, the testqual
C                    will be very high and this source will be 
C                    bypassed, unless there are no sources that work.
C                    
C                    Calculate three different quality measures.  Later
C                    choose which one to use.  All are based on the
C                    theoretical sigmas from a fit for the zenith
C                    atmosphere (sec(Z) term) base on the sources
C                    in the segment so far.  The three measures are
C                    1.)  FQUALSL: The sigma of the fit for the station 
C                         that had the highest sigma using the scans for
C                         which sources are already selected (LHIGHSTA)
C                         with an additional penalty added for long slews.
C                    2.)  FQUAL:  Just the sigma of the previous worst 
C                         station without the slew penalty.
C                    3.)  SLQUAL: The rms of all the sigmas across all 
C                         stations plus a slew penalty.
C
C                    Note that LHIGHSTA is the station with the highest
C                    sigma, LHIGHSIG, in the last sequence.  HIGHSTA
C                    is the station with the highest sigma HIGHSIG
C                    in the current sequence - being accumulated for
C                    use in setting up the next sequence.  SAVSIG
C                    is being accumulated for printing.
C
C                    Sigma of station with worst sigma last time.
C
                     IF( LHIGHSTA .GE. 1 ) THEN
                        FQUAL = SIGMA( LHIGHSTA )
                     ELSE
                        FQUAL = 0.0
                     END IF
C
C                    RMS sigma across array.
C
                     SLQUAL = 0.0
                     PKSIG = 0
                     DO ISTA = 1, NSTA
                        SLQUAL = SLQUAL + SIGMA(ISTA)**2
                        PKSIG = MAX( PKSIG, SIGMA(ISTA) )
                     END DO
                     SLQUAL = SQRT( SLQUAL / NSTA )
C
C                    Add in the slew penalty where needed.
C
                     SLEWTIME = STARTJ(LSCN) - STOPJ(LSCN-1)
                     PENALTY = GEOSLEW * SLEWTIME / ( 30.0 * ONESEC)
C       write(*,*) 'makeseg qual', lscn, igeo, slqual, fqual, penalty,
C     1     (sigma(ista),ista=1,nsta)
                     SLQUAL = SLQUAL + PENALTY
                     IF( FQUAL .NE. 0.0 ) THEN
                        FQUALSL = FQUAL + PENALTY
                     ELSE
                        FQUALSL = 0.0
                     END IF
C	   	   
C                    Spew info if requested.
C
                     IF( GEOPRT .GE. 2 ) THEN
                        WRITE(*,'( A, 2I4, A, I4, 3A, 3F7.2, I5, F7.2)')
     1                     'Makeseg quality - LSCN:',
     2                     LSCN, ISEG, ' Source:', IGEO, '  ',
     3                     GEOSRC(IGEO), ' Quality: ', FQUALSL, FQUAL,
     4                     SLQUAL, LHIGHSTA, PKSIG
                     END IF
C
C                    Keep this track of the best source based on
C                    each quality measure.  For each type, keep
C                    track of the worst station.
C
                     IF( FQUALSL .NE. 0.0 .AND. 
     1                   FQUALSL .LT. BFQUALSL ) THEN
                        BFQUALSL = FQUALSL
                        TSRC1 = IGEO
                        HIGHSTA1 = 0
                        HIGHSIG1 = 0.0
                        DO ISTA = 1, NSTA
                           IF( SIGMA(ISTA) .GT. HIGHSIG1 ) THEN
                              HIGHSTA1 = ISTA
                              HIGHSIG1 = SIGMA(ISTA)
                           END IF
                           SAVSIG1(ISTA) = SIGMA(ISTA)
                        END DO
                        IF( GEOPRT .GE. 2 ) 
     1                    WRITE(*, '( 2A, I5, 2A, F8.2, I5, F8.2 )' ) 
     2                    'makeseg: Got new best sigma with ',
     3                    'slew penalty: ', 
     4                    igeo, ' ', geosrc(igeo), fqualsl, highsta1,
     5                    highsig1
                     END IF
C
                     IF( FQUAL .NE. 0.0 .AND. FQUAL .LT. BFQUAL ) THEN
                        BFQUAL = FQUAL
                        TSRC2 = IGEO
                        HIGHSTA2 = 0
                        HIGHSIG2 = 0.0
                        DO ISTA = 1, NSTA
                           IF( SIGMA(ISTA) .GT. HIGHSIG2 ) THEN
                              HIGHSTA2 = ISTA
                              HIGHSIG2 = SIGMA(ISTA)
                           END IF
                           SAVSIG2(ISTA) = SIGMA(ISTA)
                        END DO
                        IF( GEOPRT .GE. 2 )
     1                    WRITE(*, '( 2A, I5, 2A, F8.2, I5, F8.2 )' ) 
     2                    'makesig: Got new best sigma without ',
     3                    'slew penalty: ',
     4                     igeo, ' ', geosrc(igeo), fqual, highsta2, 
     5                     highsig2
                     END IF
C
                     IF( SLQUAL .LT. BSLQUAL ) THEN
                        BSLQUAL = SLQUAL
                        TSRC3 = IGEO
                        HIGHSTA3 = 0
                        HIGHSIG3 = 0.0
                        DO ISTA = 1, NSTA
                           SAVSIG3(ISTA) = SIGMA(ISTA)
                           IF( SIGMA(ISTA) .GT. HIGHSIG3 ) THEN
                              HIGHSTA3 = ISTA
                              HIGHSIG3 = SIGMA(ISTA)
                           END IF
                        END DO
                        IF( GEOPRT .GE. 2 )
     1                    WRITE(*, '( 2A, I5, 2A, F8.2, I5, F8.2 )' ) 
     2                    'makeseg: Got new best RMS sigma with ', 
     3                    'slew penalty: ',
     4                    igeo, ' ', geosrc(igeo), slqual, 
     5                    highsta3, highsig3
                     END IF
                  END IF
               END IF
            END DO
C
C           Choose the next source based on the quality measures.
C           If the best using the rms of the worst station last time
C           with a slew penalty gives a good improvement for that
C           station, use it.  If not, use the one with the best rms
C           without a penalty.  It even that doesn't give much
C           improvement, take the best based on the improvement of
C           the rms errors.  Don't use the schemes based on the
C           previous worst, when that has not been determined.  Use
C           the best RMS then.
C
C           Allow for the last scan+1 when no sources were picked
C           for which all of the TSRCs will be zero.  Also use the
C           RMS one for ISEG = NRAND + 1 where there is no LHIGHSTA
C
C           Use the one that improves the previous worst station,
C           with slew penalty, if it is enough better than the the
C           one without the slew penalty.  Beyond the relative 
C           improvement, require an absolute improvement over the
C           previous case.
C
            IF( LHIGHSTA .NE. 0 ) THEN
               IF( TSRC1 .NE. 0 .AND. 
     1             ( LHIGHSIG - SAVSIG1(LHIGHSTA) ) .GT.  
     2             ( LHIGHSIG - SAVSIG2(LHIGHSTA) ) * 0.7 .AND. 
     3             LHIGHSIG / SAVSIG1(LHIGHSTA) .GT. 1.10 ) THEN
                  TSRC(ISEG) = TSRC1
C
C                 Deal with the information for the next pass.
C
                  HIGHSTA = HIGHSTA1
                  HIGHSIG = HIGHSIG1
                  DO ISTA = 1, NSTA
                     SAVSIG(ISTA) = SAVSIG1(ISTA)
                  END DO
C
C                 Optional print.
C
                  IF( GEOPRT .GE. 1 )
     1              WRITE(*,'( A, I4, A, F8.2, 3(F8.2,''/'',I3) )' )
     2              'makeseg:', 
     3              ISEG, ' Use slew penalty.', 
     4              LHIGHSIG, SAVSIG1(LHIGHSTA), TSRC1,
     5              SAVSIG2(LHIGHSTA), TSRC2, SAVSIG3(LHIGHSTA), TSRC3
C
C              Use the version without the slew penalty if the one
C              with the penalty wasn't so good, and the absolute
C              improvement is good.
C
               ELSE IF( TSRC2 .NE. 0 .AND. 
     1             LHIGHSIG / SAVSIG2(LHIGHSTA) .GT. 1.10  ) THEN
                  TSRC(ISEG) = TSRC2
C
C                 Deal with the information for the next pass.
C
                  HIGHSTA = HIGHSTA2
                  HIGHSIG = HIGHSIG2
                  DO ISTA = 1, NSTA
                     SAVSIG(ISTA) = SAVSIG2(ISTA)
                  END DO
C
C                 Optional print.
C
                  IF( GEOPRT .GE. 1 ) 
     1              WRITE(*,'( A, I4, A, F8.2, 3(F8.2,''/'',I3) )' )
     2              'makeseg:', 
     3              ISEG, ' Use sigma only.  ',
     4              LHIGHSIG, SAVSIG1(LHIGHSTA), TSRC1, 
     5              SAVSIG2(LHIGHSTA), TSRC2, SAVSIG3(LHIGHSTA), TSRC3
               END IF
            END IF
C
C           Use the RMS sigma if needed, as detected by TSRC(ISEG)=0. 
C           This will always be the case for ISEG equal to NRAND+1, 
C           which had LHIGHSTA=0 and skipped the opportunities to set
C           TSRC(ISEG) above.  It will also be the case when the 
C           improvements required above were not met.
C
            IF( TSRC3 .NE. 0 .AND. TSRC(ISEG) .EQ. 0 ) THEN
               TSRC(ISEG) = TSRC3
C
C              Deal with the information for the next pass.
C
               HIGHSTA = HIGHSTA3
               HIGHSIG = HIGHSIG3
               DO ISTA = 1, NSTA
                  SAVSIG(ISTA) = SAVSIG1(ISTA)
               END DO
C
C              Optional print.
C
               IF( GEOPRT .GE. 1 ) 
     1           WRITE(*,'( A, I4, A, F8.2, 3(F8.2,''/'',I3) )' )
     2           'makeseg:', 
     3           ISEG, ' Use RMS sigma.   ',
     4           LHIGHSIG, SAVSIG1(LHIGHSTA), TSRC1, 
     5           SAVSIG3(LHIGHSTA), TSRC2, SAVSIG3(LHIGHSTA), TSRC3
C
            END IF
C
C           Recover the stations to use.
C
            IF( TSRC(ISEG) .NE. 0 ) THEN
               DO ISTA = 1, NSTA
                  SSTASCN(ISTA) = KSTASCN(TSRC(ISEG),ISTA)
               END DO
            END IF
C
C           End of source selection.
C
         END IF
C
C        ===============
C        If a source was found and the stop time was early enough,
C        construct a scan with the source and go back for the next
C        source.
C
         IF( TSRC(ISEG) .GT. 0 ) THEN
C
            IF( GEOPRT .GE. 2 ) THEN
               WRITE(*,*) 'Makeseg got the next source - seg:', 
     1           ISEG, '  Geosrc:', TSRC(ISEG), 
     2           '  ', GEOSRC(TSRC(ISEG)), ' tapprox:', TAPPROX,
     3           ' stascn: ', (sstascn(ista),ista=1,nsta) 
            END IF
C
C           Insert the chosen source in the sequence.  Don't second
C           guess the number of good stations etc.  Use the list
C           of stations previously determined.  Also, do not recheck
C           the stop time.
C
            CALL GMKSCN( LASTLSCN, LSCN, JSCN, GEOSRCI(TSRC(ISEG)),
     1           GEOSRC(TSRC(ISEG)), TAPPROX, OPMINEL(JSCN), 0,
     2           NGOOD, OKSTA, SSTASCN, 'FORCE' )
C
C           Accumulate INSCN(ISTA) and SELECTED and LASTLSCN
C
            DO ISTA = 1, NSTA
               IF( STASCN(LSCN,ISTA) ) THEN
                  INSCN(ISTA) = INSCN(ISTA) + 1
                  LASTLSCN(ISTA) = LSCN
               END IF
            END DO
            SELECTED(TSRC(ISEG)) = LSCN
C
C           If requested, give feedback on the fit quality.
C
            IF( GEOPRT .GE. 1 ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, 2I4, 1X, A )' ) 
     1            'MAKESEG:', ISEG, TSRC(ISEG), GEOSRC(TSRC(ISEG))
               IF( ISEG .GT. NRAND ) THEN
                  WRITE( MSGTXT(26:256), '( A, I3, A, 20F5.1 )' ) 
     1               ' High:', HIGHSTA, '  Sigmas: ', 
     3                (SAVSIG(ISTA),ISTA=1,NPRT)
               END IF
               CALL WLOG( 1, MSGTXT )
            END IF
C
C           Save the identity of the worst station.
C
            LHIGHSTA = HIGHSTA
            LHIGHSIG = HIGHSIG
C
C           Go back for the next source.
C           Reset MAXPRIO in case it had been adjusted when getting
C           the first 4 sources.
C
            MAXPRIO = 2
            GO TO 100
C
         ELSE
            IF( GEOPRT .GE. 2 ) THEN
               WRITE(*,*) 'makeseg:  No source found that fits'
               WRITE(*,*) '          Ending sequence.'
            END IF
C
C           If no source was found, either it is too late, or there
C           are no more available sources.  If so, end the sequence.
C           For output, LSCN should be the last scan.
C
            ISEG = ISEG - 1
            NTSEG = ISEG
            LSCN = ISCN + NTSEG - 1 
         END IF
C
C     Now we are out of the GO TO loop and basically done.
C     There used to be some feed back here, but moved to GEOMAKE.
C
      RETURN
      END
