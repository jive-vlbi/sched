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
      INTEGER            JSCN, ISCN, LASTISCN(*)
      INTEGER            TSRC(*), NTSEG, USEGEO(*)
      LOGICAL            OKGEO(*)
      REAL               SEGELEV(MAXSTA,MGEO)
      DOUBLE PRECISION   STARTB, TGEOEND, SIGMA(*)
C
      INTEGER            ISTA, IS, ISEG, IGEO, IC
      INTEGER            LASTLSCN(MAXSTA), NNEAR, NRAND, CTSC
      INTEGER            IDUM, KSCN, LSCN, MAXPRIO, IISCN
      INTEGER            NGOOD, NCHANCE, CHANCE(MSEG10), TEMPCH 
      INTEGER            INSCN(MAXSTA), INSCT, SELECTED(MGEO)
      LOGICAL            OKSTA(MAXSTA), LSTAS
      LOGICAL            STAOK, NEWC, PRDEBUG
      LOGICAL            SSTASCN(MAXSTA), KSTASCN(MGEO,MAXSTA)
      REAL               RAN5, TESTQUAL, SLQUAL, TSLQUAL, BSLQUAL
      REAL               PKSIG
      DOUBLE PRECISION   TAPPROX, TESTTIME(MSEG10), TEMPTIME
      DOUBLE PRECISION   SLEWTIME, TSEP
      CHARACTER          LSRCNAME
C --------------------------------------------------------------------
      IF( DEBUG .OR. GEOPRT .GE. 1 ) CALL WLOG( 1, 'MAKESEG starting' )
C
C     Initializations
C
      ISEG = 0
      NTSEG = 0
      DO IGEO = 1, NGEO
         SELECTED(IGEO) = 0
      END DO
      MAXPRIO = 2
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
C     Start selecting scans.  Jump to 100 to get next source.
C
  100 CONTINUE
C
C        Set the scan number.
C
         ISEG = ISEG + 1
         LSCN = ISEG + ISCN - 1
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
            DO IGEO = 1, NGEO
               IF( USEGEO(IGEO) .LE. MAXPRIO  .AND. 
     1             SELECTED(IGEO) .LT. IISCN ) THEN
                  IF( ISEG .EQ. 1 ) THEN
                     TAPPROX = STARTB
                  ELSE
                     TAPPROX = STOPJ(LSCN-1) + 60.D0 * ONESEC
                  END IF
                  IF( GEOPRT .GE. 2 ) WRITE(*,*) 'makeseg space'
C
C                 Insert the new scan.  GMKSCN is like MAKESCN
C                 but also is capable of removing stations based
C                 on long slews.  It can also make or take a 
C                 predetermined list of stations to include.
C
                  CALL GMKSCN( LASTLSCN, LSCN, JSCN, GEOSRCI(IGEO),
     1                    GEOSRC(IGEO), TAPPROX, OPMINEL(JSCN),
     2                    NGOOD, OKSTA, SSTASCN, 'SET  ' )
C
C                 Save SSTASCN for after source selection.
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
C
                     IF( NCHANCE .LT. NNEAR ) THEN
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
                        WRITE(*,*) 'MAKESEG 6.65', IGEO, LSCN, 
     1                     ' LAST: ', LSRCNAME, '  TEST: ', GEOSRC(IGEO)
                        DO ISTA = 1, NSTA
                           IF( LSCN .EQ. SCAN1 ) THEN
                              LSTAS = .FALSE.
                           ELSE
                              LSTAS = STASCN(LSCN-1,ISTA)
                           END IF
                           WRITE(*,'(A,2I5,5F7.1,2L2,F7.1)') 
     1                     ' makeseg 6.66 ', ISTA, LASTLSCN(ISTA),
     2                     EL2(LASTLSCN(ISTA),ISTA), EL1(LSCN,ISTA),
     3                     AZ2(LASTLSCN(ISTA),ISTA), AZ1(LSCN,ISTA),
     4                     TSLEW(LSCN,ISTA)*86400.D0,
     5                     LSTAS, STASCN(LSCN,ISTA),
     6                     TSEP
                     
                        END DO
                     END IF
C
C                    Now push up anything that was added to where
C                    if belongs.  This is sorting the sources in the
C                    chance array by slew time.
C
                     IF( NEWC ) THEN
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
               IF( GEOPRT .GE. 1 ) THEN
                  WRITE(*,*) 'makeseg adding source: LSCN:',
     1                LSCN, ISEG, ' Source:', TSRC(ISEG), 
     2                '  ', GEOSRC(TSRC(ISEG))
               END IF
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
            TSRC(ISEG) = 0
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
C
                  CALL GMKSCN( LASTLSCN, LSCN, JSCN, GEOSRCI(IGEO),
     1                    GEOSRC(IGEO), TAPPROX, OPMINEL(JSCN),
     2                    NGOOD, OKSTA, SSTASCN, 'SET  ' )
C
C                 Save SSTASCN for after source selection.
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
C                    this segment.  TSRC(ISEG) is a handy number
C                    that will be zero the first time we get here
C                    for each LSCN, then non-zero later.  Some 
C                    savings could be had on the first call too,
C                    but it's probably not worth it.
C
C                    While at it, set up for a finite lookback
C                    time.  This is to prevent the sequence from
C                    stagnating after it has a good set for all
C                    stations.
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
                     CALL GEOQUAL( IISCN, KSCN, LSCN, JSCN, TESTQUAL,
     1                    PRDEBUG, SIGMA )
C
C                    Construct the quality measure we want to use
C                    here.  Unlike in the final selection, here
C                    we want to take slews into account.  We also 
C                    want to reward improvement for stations that
C                    aren't the worst.  Note that, if there were a
C                    failure to invert the matrix, probably from
C                    inadequate data for some station, the testqual
C                    will be very high and this source will be 
C                    bypassed, unless there are no sources that work.
C                    
C                    For the quality measure, use the RMS deviation
C                    from zero of the atmospheric fit sigmas plus
C                    a linear penalty for slews.  The different
C                    sources don't change the RMS all that much so
C                    it seems that the scale factor multiplying 
C                    the slew time can be small.
C	   	   
                     SLQUAL = 0
                     PKSIG = 0
                     DO ISTA = 1, NSTA
                        SLQUAL = SLQUAL + SIGMA(ISTA)**2
                        PKSIG = MAX( PKSIG, SIGMA(ISTA) )
                     END DO
                     SLQUAL = SQRT( SLQUAL / NSTA )
                     SLEWTIME = STARTJ(LSCN) - STOPJ(LSCN-1)
                     TSLQUAL = SLQUAL + 
     1                     GEOSLEW * SLEWTIME / ( 30.0 * ONESEC)
C	   	   
C                    Keep this source if it is better than the current
C                    best.
C
                     IF( GEOPRT .GE. 2 ) THEN
                        WRITE(*,'( A, 3F10.2, 2X, A, F10.2 )' )
     1                     'Makeseg quality: ', SLQUAL, 
     2                        SLEWTIME / ONESEC, TSLQUAL,
     3                     '  Peak sigma:', PKSIG
                     END IF
C
C
                     IF( TSLQUAL .LT. BSLQUAL ) THEN
                        BSLQUAL = TSLQUAL
                        TSRC(ISEG) = IGEO
C
C
                        IF( GEOPRT .GE. 1 ) THEN
                           WRITE(*,'(A, 2I5, A, I4, 3A, '//
     1                         '3F8.2, A,  F8.2 )' )
     2                        'Makeseg better source - LSCN:',
     3                        LSCN, ISEG, ' Source:', IGEO, '  ',
     4                        GEOSRC(IGEO), ' Qual:', SLQUAL, 
     5                        SLEWTIME / ONESEC, TSLQUAL, 
     6                        '  Peak sigma:', PKSIG
                        END IF
                     END IF
                  END IF
               END IF
            END DO
C
C           Recover the stations to use.
C
            DO ISTA = 1, NSTA
               SSTASCN(ISTA) = KSTASCN(TSRC(ISEG),ISTA)
            END DO
C
         END IF
C
C        ===============
C        If a source was found and the stop time was early enough,
C        add the source to the list and go back for more.
C
         IF( TSRC(ISEG) .GT. 0 ) THEN
C
            IF( GEOPRT .GE. 1 ) THEN
               WRITE(*,*) 'Makeseg got the next source - seg:', 
     1        ISEG, '  Geosrc:', TSRC(ISEG), 
     2        '  ', GEOSRC(TSRC(ISEG)), ' tapprox:', TAPPROX,
     3        ' stascn: ', (sstascn(ista),ista=1,nsta) 
            END IF
C
C           Insert the chosen source in the sequence.  Don't second
C           guess the number of good stations etc.  Use the list
C           of stations previously determined.
C
            CALL GMKSCN( LASTLSCN, LSCN, JSCN, GEOSRCI(TSRC(ISEG)),
     1           GEOSRC(TSRC(ISEG)), TAPPROX, OPMINEL(JSCN),
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
C           Go back for the next source.
C           Reset MAXPRIO in case it had been adjusted when getting
C           the first 4 sources.
C
            IF( GEOPRT .GE. 2 ) THEN
               WRITE(*,*) ' '
               WRITE(*,*) '----Makeseg going for next source:', 
     1                  ISEG + 1
            END IF
C
            MAXPRIO = 2
            GO TO 100
C
         ELSE
            IF( GEOPRT .GE. 1 ) THEN
               WRITE(*,*) 'makeseg:  No source found that fits'
               WRITE(*,*) '          Ending sequence.'
            END IF
C
C           If no source was found, either it is too late, or there
C           are no more available sources.  If so, end the sequence.
C
            ISEG = ISEG - 1
            NTSEG = ISEG
         END IF
C
C     Now we are out of the GO TO loop and basically done.
C     There used to be some feed back here, but moved to GEOMAKE.
C
      RETURN
      END
