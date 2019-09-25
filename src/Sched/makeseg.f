      SUBROUTINE MAKESEG( JSCN, ISCN, LASTISCN, 
     1                    OKGEO, USEGEO, SEGELEV, STARTB, TGEOEND, 
     2                    LSCN, NTSEG, TSRC, IDUM, SIGMA, SELTYPE,
     3                    WSTA )
Cf2py intent(in) JSCN, ISCN, LASTISCN, OKGEO, USEGEO, SEGELEV, STARTB
Cf2py intent(in) TGEOEND, SIGMA
Cf2py intent(out) LSCN, NTSEG, TSRC, SELTYPE, WSTA
Cf2py intent(in, out) IDUM
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
C        SELTYPE:   Selection type used:  SP=sigma+slew penalty, 
C                        S=sigma, no penalty, RMS=rms sigma.
C                        NR=nearby random.
C        WSTA:      Worst station.
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
      CHARACTER          SELTYPE(*)*3, WSTA(*)*3
C
      INTEGER            ISTA, IS, ISEG, IGEO, IC, LSEG, JJSCN 
      INTEGER            LASTLSCN(MAXSTA), NNEAR, NRAND, CTSC
      INTEGER            KSCN, MAXPRIO, IISCN, SSCN1, NPRT, MAXIC
      INTEGER            NGOOD, NCHANCE, CHANCE(MSEG10), TEMPCH 
      INTEGER            INSCN(MAXSTA), INSCT, SELECTED(MGEO)
      INTEGER            HIGHSTA, HIGHSTA1, HIGHSTA2, HIGHSTA3
      INTEGER            LHIGHSTA, NEEDLOW, NEEDHIGH, LHGOAL
      INTEGER            STALOW(MAXSTA), NSTALOW, NNEWLOW
      INTEGER            STAHIGH(MAXSTA), NSTAHIGH, NNEWHIGH
      INTEGER            TSRC1, TSRC2, TSRC3, NSTASCN(MAXSTA), MINNSS
      LOGICAL            OKSTA(MAXSTA), LSTAS
      LOGICAL            STAOK, SRCOK, NEWC, PRDEBUG, DORAND
      LOGICAL            SSTASCN(MAXSTA), KSTASCN(MGEO,MAXSTA)
      REAL               RAN5, TESTQUAL, PENALTY, ELA
      REAL               SLQUAL, BSLQUAL
      REAL               FQUAL, BFQUAL, FQUALSL, BFQUALSL
      REAL               PKSIG
      REAL               SAVSIG(MAXSTA), SAVSIG1(MAXSTA)
      REAL               SAVSIG2(MAXSTA), SAVSIG3(MAXSTA)
      REAL               HIGHSIG, HIGHSIG1, HIGHSIG2, HIGHSIG3
      REAL               LHIGHSIG
      DOUBLE PRECISION   TAPPROX, TESTTIME(MSEG10), TEMPTIME
      DOUBLE PRECISION   SLEWTIME, TSEP, T0
      CHARACTER          LSRCNAME*12, LASTTYPE*5, METHOD*4
C --------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 1, 'MAKESEG starting' )
      NPRT = MIN( 20, NSTA )
C
C     Initializations
C
      ISEG = 0
      LSEG = 0
      NRAND = 0
      DORAND = .TRUE.
      NTSEG = 0
      DO IGEO = 1, NGEO
         SELECTED(IGEO) = 0
      END DO
      DO IS = 1, MSEG
         TSRC(IS) = 0
         SELTYPE(IS) = '   '
      END DO
      LHIGHSTA = 0
      LHIGHSIG = 0.0
      LASTTYPE = ' '
C
C     For the first source, be relaxed about what is chosen.  This
C     was flagged as a test in the 9.4 release, but that release
C     seems to be working, so keep it.  Note that MAXPRIO 5 are 
C     sources that can be observed but don't contribute anything
C     special.  The only higher setting is 9, which are not useful
C     for a variety of reasons.
C
      MAXPRIO = 6
C
C     In the initial source selection, try to get low and high elevations
C     at each station using sources close to previous one to avoid
C     long slews.  LHGOAL is the desired number of sources at each
C     station with low (or high) elevation scans.  Start trying to get
C     one of each (LHGOAL=1).  Later this will be raised to 2.
C
      LHGOAL = 1
C
C     Keep track of the number of scans each station is in.
C     Mainly use this to try to avoid too few for the quality
C     measure least square fit.  Initialize the counter here.
C     Also keep track of the number of low elevation scans
C     a station has, at least for before each station has minimal
C     low and high elevation sources.
C
      DO ISTA = 1, NSTA
         INSCN(ISTA) = 0
         STALOW(ISTA) = 0
         STAHIGH(ISTA) = 0
      END DO
      NSTALOW = 0
      NSTAHIGH = 0
C
C     Copy LASTISCN to LASTLSCN where it can then get modified
C     in calls to other routines during trials of sets of sources
C     without affecting the input values.
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
C     Write column headers if it's going to print the sigmas.
C
      IF( GEOPRT .GE. 1 ) THEN
         WRITE( MSGTXT, '( T40, 20( 2X, A2, 1X) )' ) 
     1           '                      ', 
     2           (STCODE(STANUM(ISTA)),ISTA=1,NPRT)
         CALL WLOG( 0, MSGTXT )
      END IF
C
C     Start selecting scans.  Jump to 100 to get next source.
C
  100 CONTINUE
C
C        Set the scan number.
C
         ISEG = ISEG + 1
         IF( ISEG .GT. MSEG ) THEN
            CALL WLOG( 1, ' ' )
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I5, A )' )
     1        ' Trying to exceed SCHED''s limit of ', MSEG, 
     2        ' scans in an inserted geodetic segment.'
            CALL WLOG( 1, MSGTXT )
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, A )' )
     1        ' Shorten the requested segment or divide it into ',
     2        'multiple segments.'
            CALL ERRLOG( MSGTXT )
         END IF
         LSCN = ISEG + ISCN - 1
C
C        Initialize these or they get remembered to next trial
C        sequence.
C
         HIGHSTA = 0
         HIGHSIG = 0
C
C        Sometimes there might be too little data to fit for all the
C        parameters.  This happens when any antenna is in less than
C        3 scans (actually one can have 2, but ignore that), although 
C        it might be disguised by using all the baselines.  Get the 
C        minimum number of scans so far per station and don't allow
C        the use of the least squares fit until there are 3 per station.
C        MINNSS is the minimum number of scans for a station.
C
C        First count the number of scans each station is in for which
C        the source is up.  Note LSCN is the next scan which for which
C        the source is not yet picked.
C        
         DO ISTA = 1, NSTA
            NSTASCN(ISTA) = 0
            IF( LSCN .GT. ISCN ) THEN
               DO JJSCN = ISCN, LSCN - 1 
                  IF( STASCN(JJSCN,ISTA) .AND. UP1(JJSCN,ISTA) .EQ. ' ' 
     1                .AND. UP2(JJSCN,ISTA) .EQ. ' ' ) THEN
                     NSTASCN(ISTA) = NSTASCN(ISTA) + 1
                  END IF
               END DO
            END IF
         END DO
C
C        Now get the minimum across stations.
C
         MINNSS=999
         DO ISTA = 1, NSTA
            MINNSS = MIN( NSTASCN(ISTA), MINNSS )
         END DO
C
C        Note the start of new source attempt if putting out debug print.
C
         IF( GEOPRT .GE. 2 ) THEN
            CALL WLOG( 0, ' ' )
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I3, A, L1, A, I3, A, I2 )' ) 
     1         '----MAKESEG:  Starting to work on next source: ISEG: ',
     2         ISEG, '   DORAND:', DORAND, '   MINNSS:', MINNSS, 
     3         '   MAXPRIO: ', MAXPRIO
            CALL WLOG( 0, MSGTXT )
            MSGTXT = ' '
         END IF
C
C        
C        Set the first scan to use for quality measures using GEOBACK.
C        That is the "look back" distance if you want the quality measure
C        determined only over the more recent scans.
C        Set a separate first scan for avoiding repeats of individual 
C        sources.  In some cases where there are limited options, a 
C        sequence might benefit from using the same source twice. But they
C        shouldn't be adjacent scans.  Use GEOSREP for this function.
C        Setting these first scans saves repeats below.
C
         IISCN = MAX( ISCN, LSCN - GEOBACK + 1 ) 
         SSCN1 = MAX( ISCN, LSCN - GEOSREP + 1 )
C
C        At first, using psuedo fits to select the best source won't be
C        very productive because there are too few scans.  So start by
C        selecting sources such that there is at least one high and
C        one low elevation scan for each antenna.  After that, use the
C        test fits.  Use NRAND to keep track of the number of sources
C        picked this way.
C
C        Set the number of sources to pick before starting to 
C        measure the quality measure.  For these, restrict to
C        better sources using USEGEO and take ones that involve
C        short slews.
C
C        ====  First sources - get something low el and something high 
C        ====  el for each station 
C
C        Also force this selection scheme if any station has only one scan
C        as it cannot reach the required 3 with one additional scan.
C
         IF( ( ( NSTALOW .LT. NSTA .OR. NSTAHIGH .LT. NSTA ) .AND. 
     1       DORAND ) .OR. MINNSS .LE. 1 ) THEN
            NRAND = NRAND + 1
            METHOD = 'RAND'
C
C           Find all of the sources with USEGEO .LE. MAXPRIO below the limit 
C           that can contribute to the number stations with low or high
C           elevation data.  MAXPRIO starts as 2, but is raised to 3 
C           (> 5, the highest for useful sources) if there are no more
C  RCW  Should that be <5?
C           prio 1 or 2 sources available.
C           Of the sources that qualify, find the closest 5 and choose one
C           at random.  For the first scan of the experiment, choose
C           from all with USEGEO low enough.
C
C           Note that the NGOOD test should be passed if USEGEO is 
C           acceptable, but the geo segments are of finite length so
C           the up status can change compared to what GEOCHK used.
C
C           SELECTED is the last scan in which this source
C           was selected.  It is used to help prevent rapid repeats.
C
C           With STALOW and STAHIGH, keep track of the number of stations 
C           with low or high elevation scans.  Insist that any new 
C           source increase one of those numbers.  Once both reach NSTA,
C           switch to doing test fits.
C
C           This selection process is much faster than the scheme used 
C           below based on a psuedo LSQ fit for SecZ.  Try to use it as
C           much as possible.  That means trying to find 2 low and 2 high
C           sources for each station.
C
            NNEAR = 5
            NCHANCE = 0
C
            DO IGEO = 1, NGEO
               IF( USEGEO(IGEO) .LE. MAXPRIO  .AND. 
     1             SELECTED(IGEO) .LT. SSCN1 ) THEN
                  IF( ISEG .EQ. 1 ) THEN
                     TAPPROX = STARTB
                  ELSE
                     TAPPROX = STOPJ(LSCN-1) + 60.D0 * ONESEC
                  END IF
C
C                 For debug print, mark attempt for this source.
                  IF( GEOPRT .GE. 2 ) THEN
                     MSGTXT = ' ' 
                     WRITE( MSGTXT, 
     1                 '( 2A, I4, A, I4, 3A, I4, A, F13.6 )' )
     2                 'MAKESEG random section - looking for nearby ',
     3                 'source.  Considering:', iseg, ' SRC: ', igeo,
     4                  ' ',  GEOSRC(igeo), '  USEGEO: ', USEGEO(IGEO), 
     5                  ' at ', TAPPROX
                     CALL WLOG( 0, MSGTXT )
                  END IF
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
C                 Count then number of new stations for which this source
C                 adds the first low elevation data.  Do the same for
C                 high elevation data.  Then count the number of such
C                 stations.  Accept sources that add significant numbers
C                 of stations with low and high el scans.  See the code
C                 for what "significant" means.
C
                  NNEWLOW = 0
                  NNEWHIGH = 0
                  NEEDLOW = 0
                  NEEDHIGH = 0
                  DO ISTA = 1, NSTA
                     IF( STALOW(ISTA) .LT. LHGOAL ) 
     1                     NEEDLOW = NEEDLOW + 1
                     IF( STAHIGH(ISTA) .LT. LHGOAL ) 
     1                     NEEDHIGH = NEEDHIGH + 1
                     IF( SSTASCN(ISTA) ) THEN
                        ELA = ( EL1(LSCN,ISTA) + EL2(LSCN,ISTA) ) / 2.0
                        IF( ELA .LE. GEOLOWEL ) THEN
                           IF( STALOW(ISTA) .LT. LHGOAL ) THEN
                              NNEWLOW = NNEWLOW + 1
                           END IF
                        END IF
                        IF( ELA .GE. GEOHIEL ) THEN
                           IF( STAHIGH(ISTA) .LT. LHGOAL ) THEN
                              NNEWHIGH = NNEWHIGH + 1
                           END IF
                        END IF
                     END IF
                  END DO
                  SRCOK = ( NNEWLOW + NNEWHIGH .GT. NSTA / 3 ) .OR. 
     1                    NNEWLOW + NNEWHIGH .GE. 
     2                    MAX( 1, ( NEEDLOW + NEEDHIGH ) / 3 )
C
C                  SRCOK = ( NNEWLOW .GE. 1 .AND. NSTALOW .LT. NSTA ) 
C     1               .OR. ( NNEWHIGH .GE. 1 .AND. NSTAHIGH .LT. NSTA ) 
C
C      Save another set of currently unused debug printout.
C      write(*,*) 'makeseg iseg nrand ', iseg, nrand, igeo, nstalow, 
C     1    nstahigh, nnewlow, nnewhigh, ' ', srcok, needlow, needhigh,
C     2    lhgoal
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
                     IF( INSCT .LT. ISEG / 2 ) THEN
                        STAOK = .FALSE.
                     END IF
                  END DO
                  IF( SRCOK .AND. STAOK .AND. NGOOD .GE. OPMIAN(JSCN) )
     1                 THEN
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
                        MSGTXT = ' '
                        WRITE( MSGTXT, * ) 'MAKESEG getting slew info',
     1                     IGEO, LSCN, ' Last source: ', LSRCNAME, 
     2                     '  Testing: ', GEOSRC(IGEO)
                        CALL WLOG( 0, MSGTXT )
                        DO ISTA = 1, NSTA
                           IF( LSCN .EQ. SCAN1 ) THEN
                              LSTAS = .FALSE.
                           ELSE
                              LSTAS = STASCN(LSCN-1,ISTA)
                           END IF
                           MSGTXT = ' '
                           WRITE( MSGTXT, '(A,2I5,5F7.1,2L2,F7.1)' ) 
     1                       ' makeseg els, azs, tslew: ', ISTA, 
     2                       LASTLSCN(ISTA), 
     3                       EL2(LASTLSCN(ISTA),ISTA), EL1(LSCN,ISTA),
     4                       AZ2(LASTLSCN(ISTA),ISTA), AZ1(LSCN,ISTA),
     5                       TSLEW(LSCN,ISTA)*86400.D0,
     6                       LSTAS, STASCN(LSCN,ISTA), TSEP
                           CALL WLOG( 0, MSGTXT )
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
C           With a good list, this should this should not happen.  But
C           it does sometimes with Mark's original list.  Exit to the
C           fitting part if we get here twice on the same ISEG and there
C           are enough scans for a fit.  If there aren't enough for
C           a fit, increase MAXPRIO again.  If get past all acceptable
C           sources, complain about too few sources to choose from and
C           quit.
C
            IF( NCHANCE .EQ. 0 ) THEN
C
C              Debug print.
C
               IF( GEOPRT .GE. 1 ) THEN
                  MSGTXT =  ' '
                  WRITE( MSGTXT, '( A, I2, A )' )
     1               'MAKESEG: No priority <= ', MAXPRIO, 
     2               ' sources found for random source additions.'
                  CALL WLOG( 0, MSGTXT )
               END IF
C
C              Take action depending on MAXPRIO.
C              Only resort to priority 4 and 5 sources when we won't have
C              enough scans for a LSQ fit when adding one more.  In other
C              words, require each station have at least 2 scans (MINNSS>=2).
C
               IF( MAXPRIO .GE. 3 .AND. MAXPRIO .LT. 5 ) THEN
                  IF( MINNSS .GE. 2 ) THEN
                     DORAND = .FALSE.
                     IF (GEOPRT .GE. 1 ) THEN
                        CALL WLOG( 0, 
     1                  '         Select a source using a SecZ fit.' )
                     END IF
                  ELSE
                     MAXPRIO = MAXPRIO + 1
                     DORAND = .TRUE.
                  END IF
               ELSE IF( MAXPRIO .GE. 5 ) THEN
                  MSGTXT = 'Too few useful sources to choose from to ' 
     1                // 'construct a geodetic segment. '
                  CALL WLOG( 1, MSGTXT )
                  CALL WLOG( 1, '   Possible solutions include: ' )
                  CALL WLOG( 1, '     Add sources ' )
                  MSGTXT =      '     Adjust GEOLOWEL and GEOHIEL '//
     1              'so more sources are considered useful.'
                  CALL WLOG( 1, MSGTXT )
                  MSGTXT =      '     Try different OPMINANT or '//
     1              'OPMINEL'
                  CALL WLOG( 1, MSGTXT )
                  CALL ERRLOG( '    Pick a solution and try again.' )
               ELSE
                  MAXPRIO = MAXPRIO + 1
                  IF( GEOPRT .GE. 1 ) THEN
                     CALL WLOG( 0, 
     1                  '         Allow scans with low stations but '
     2                  // 'no very high elevation stations.' )
                  END IF
               END IF
C
C              Reset counters from this attempt and go back to try again.
C
               ISEG = ISEG - 1
               NRAND = NRAND - 1
               GO TO 100
            END IF               
C
C           Pick one of the top choices randomly.  Don't take one that
C           for which the scan is more than one minute after the first.
C           Ok, the first DO loop is trivial for IC = 1, but keeps the 
C           code simpler.  Note if NCHANCE is zero, we won't get here.

C ***********************  is this causing the infinite loop?


C
            DO IC = 1, NCHANCE
               IF( TESTTIME(IC) .LE. TESTTIME(1) + 60.D0 * ONESEC )
     1             THEN
                  MAXIC = IC
               END IF
            END DO
            IC = INT( 1 + RAN5(IDUM) * MAXIC )
            IF( GEOPRT .GE. 2 ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, 2I4 )' ) 
     1            'MAKESEG picked random source: ', IC, CHANCE(IC)
               CALL WLOG( 0, MSGTXT )
            END IF
            TSRC(ISEG) = CHANCE(IC)
            DO ISTA = 1, NSTA
               SSTASCN(ISTA) = KSTASCN(TSRC(ISEG),ISTA)
            END DO
            SELTYPE(ISEG) = 'NR'
            WSTA(ISEG) = '-- '
C
C           Write some slew time information if requested.
C
            IF( GEOPRT .GE. 2 ) THEN
               T0 = DINT( STOPJ(LSCN) )
               MSGTXT = ' '
               WRITE( MSGTXT,'( A, 2I4, F10.2, 2I4 )' )
     1             'MAKESEQ slew times: ', TSRC(ISEG), IC, 
     2             (STOPJ(LSCN-1)-T0)*24.D0*60.D0, NCHANCE, MAXIC
               CALL WLOG( 0, MSGTXT )
               MSGTXT = ' '
               CALL WLOG( 0, ' Time below is minutes in day ' )
               CALL WLOG( 0, 
     1             '                    Src    Time  Since Last stop' )
               DO IS = 1, NCHANCE
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( 10X, A, I3, I4, A, A12, 2F10.2 )' )
     1             '   ', IS, CHANCE(IS), ' ', GEOSRC(CHANCE(IS)), 
     2             (TESTTIME(IS)-T0)*24.D0*60.D0, 
     3             (TESTTIME(IS)-STOPJ(LSCN-1))*24.D0*60.D0
                  CALL WLOG( 0, MSGTXT )
               END DO
            END IF
C
C        ==============  Rest of sources based on fit  =============
C
         ELSE
            METHOD = 'FIT'
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
               IF( OKGEO(IGEO) .AND. SELECTED(IGEO) .LT. SSCN1 ) THEN
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
                  STAOK = .TRUE.
C
C                 Insist that the last high station (highest Sigma)
C                 be in the scan.
C
                  IF( LHIGHSTA .NE. 0 ) THEN
                     IF( .NOT. SSTASCN(LHIGHSTA) ) STAOK = .FALSE.
                  END IF
C
C                 First get the numbers needed and protect against
C                 a source choice that leaves some station with too
C                 few scans.  Additionally try to prevent having too
C                 few scans with this station in the GEOBACK look
C                 back set to do a fit.
C
                  IF( STAOK ) THEN
                     DO ISTA = 1, NSTA
C
C                       Require that a station be in half of scans.
C
                        INSCT = INSCN(ISTA)
                        IF( STASCN(LSCN,ISTA) ) INSCT = INSCT + 1
                        IF( INSCT .LT. ISEG / 2 ) STAOK = .FALSE.
C
C                       Require a station have 2 scans in the lookback
C                       set.  Only need to worry if the station is not
C                       in this scan.
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
                  END IF
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
C                    in the GEOQUAL call and is set for SecZ=4.  
C
                     IF( TSRC(ISEG) .EQ. 0 ) THEN
                        KSCN = IISCN
                     ELSE
                        KSCN = LSCN
                     END IF
                     PRDEBUG = GEOPRT .GE. 2
C
                     IF( GEOPRT .GE. 2 ) THEN
                        CALL WLOG( 0, ' ' )
                        MSGTXT = ' '
                        WRITE( MSGTXT, * ) 
     1                   'Makeseg about to test quality ', ISEG, 
     2                   ISCN, IISCN, KSCN, LSCN, JSCN,
     3                   '  geosrc:', IGEO, ' ', GEOSRC(IGEO)
                        CALL WLOG( 0, MSGTXT )
                     END IF
C
                     CALL GEOQUAL( IISCN, KSCN, LSCN, JSCN, 14.48,
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
C                    sigma, LHIGHSIG, in the previous scan.  HIGHSTA
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
C
C       write(*,*) 'makeseg qual', lscn, igeo, slqual, fqual, penalty,
C     1     (sigma(ista),ista=1,nsta)
C
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
                        MSGTXT = ' '
                        WRITE( MSGTXT,
     1                     '( A, 2I4, A, I4, 3A, 3F7.2, I5, F7.2)')
     2                     'Makeseg quality - LSCN:',
     3                     LSCN, ISEG, ' Source:', IGEO, '  ',
     4                     GEOSRC(IGEO), ' Quality: ', FQUALSL, FQUAL,
     5                     SLQUAL, LHIGHSTA, PKSIG
                        CALL WLOG( 0, MSGTXT )
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
                        IF( GEOPRT .GE. 2 ) THEN
                           MSGTXT = ' '
                           WRITE( MSGTXT, 
     1                       '( 2A, I5, 2A, F8.2, I5, F8.2 )' ) 
     2                       'makeseg: Got new best sigma with ',
     3                       'slew penalty: ', igeo, ' ', 
     4                       geosrc(igeo), fqualsl, highsta1,
     5                       highsig1
                           CALL WLOG( 0, MSGTXT )
                        END IF
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
                        IF( GEOPRT .GE. 2 ) THEN
                           MSGTXT = ' '
                           WRITE( MSGTXT, 
     1                         '( 2A, I5, 2A, F8.2, I5, F8.2 )' ) 
     2                         'makesig: Got new best sigma without ',
     3                         'slew penalty: ',
     4                         igeo, ' ', geosrc(igeo), fqual, highsta2, 
     5                         highsig2
                           CALL WLOG( 0, MSGTXT )
                        END IF
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
                        IF( GEOPRT .GE. 2 ) THEN
                          MSGTXT = ' '
                          WRITE(*, '( 2A, I5, 2A, F8.2, I5, F8.2 )' ) 
     1                      'makeseg: Got new best RMS sigma with ', 
     2                      'slew penalty: ',
     3                      igeo, ' ', geosrc(igeo), slqual, 
     4                      highsta3, highsig3
                          CALL WLOG( 0, MSGTXT )
                        END IF
                     END IF
                  END IF
               END IF
            END DO
C
C           Choose the next source based on the quality measures.
C           If the best using the rms of the worst station last time
C           with a slew penalty gives a good improvement for that
C           station, use it.  If not, use the one with the best rms
C           without a penalty.  If even that doesn't give much
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
                  SELTYPE(ISEG) = 'SP'
                  WSTA(ISEG) = STCODE(STANUM(HIGHSTA1))
C
C                 Deal with the information for the next pass.
C
                  HIGHSTA = HIGHSTA1
                  HIGHSIG = HIGHSIG1
                  DO ISTA = 1, NSTA
                     SAVSIG(ISTA) = SAVSIG1(ISTA)
                  END DO
                  LASTTYPE = 'SIGP'
C
C                 Optional print.
C
                  IF( GEOPRT .GE. 1 ) THEN
                     MSGTXT = ' '
                     WRITE( MSGTXT, '( A, I4, A, F8.2, A, 3(F8.2,''/'',
     1                  I2, ''/'', I3 ) )' ) 'MAKESEG:', 
     2                  ISEG, ' Choice used slew penalty. Last:', 
     3                  LHIGHSIG, '  Options (sig/sta/src):', 
     4                  SAVSIG1(LHIGHSTA), HIGHSTA1, TSRC1, 
     5                  SAVSIG2(LHIGHSTA), HIGHSTA2, TSRC2, 
     6                  SAVSIG3(LHIGHSTA), HIGHSTA3, TSRC3
                     CALL WLOG( 0, MSGTXT )
                  END IF
C
C              Use the version without the slew penalty if the one
C              with the penalty wasn't so good, and the absolute
C              improvement is good.  Require it to be significantly better
C              to justify the long slew.
C
C              If the RMS was used in the last iteration, the same 
C              station is worst for the RMS, and the sigma is better 
C              with this option, take it without requiring the 
C              especially big improvement.
C
               ELSE IF( TSRC2 .NE. 0 .AND. 
     1             ( ( LHIGHSIG / SAVSIG2(LHIGHSTA) .GT. 1.25 ) .OR.
     2             ( LASTTYPE .EQ. 'RMS' .AND. 
     3               HIGHSTA3 .EQ. LHIGHSTA .AND. 
     3               HIGHSIG3 .GT. HIGHSIG2 ) ) ) THEN
                  TSRC(ISEG) = TSRC2
                  SELTYPE(ISEG) = 'S'
                  WSTA(ISEG) = STCODE(STANUM(HIGHSTA2))
C
C                 Deal with the information for the next pass.
C
                  HIGHSTA = HIGHSTA2
                  HIGHSIG = HIGHSIG2
                  DO ISTA = 1, NSTA
                     SAVSIG(ISTA) = SAVSIG2(ISTA)
                  END DO
                  LASTTYPE = 'SIG'
C
C                 Optional print.
C
                  IF( GEOPRT .GE. 1 ) THEN
                     MSGTXT = ' '
                     WRITE( MSGTXT, '( A, I4, A, F8.2, A, 3(F8.2,''/'',
     1                  I2, ''/'', I3 ) )' ) 'MAKESEG:', 
     2                  ISEG, ' Choice used sigma only.   Last:',
     3                  LHIGHSIG, '  Options (sig/sta/src):', 
     4                  SAVSIG1(LHIGHSTA), HIGHSTA1, TSRC1, 
     5                  SAVSIG2(LHIGHSTA), HIGHSTA2, TSRC2, 
     6                  SAVSIG3(LHIGHSTA), HIGHSTA3, TSRC3
                     CALL WLOG( 0, MSGTXT )
                  END IF
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
               SELTYPE(ISEG) = 'RMS'
               WSTA(ISEG) = STCODE(STANUM(HIGHSTA3))
C
C              Deal with the information for the next pass.
C
               HIGHSTA = HIGHSTA3
               HIGHSIG = HIGHSIG3
               DO ISTA = 1, NSTA
                  SAVSIG(ISTA) = SAVSIG3(ISTA)
               END DO
               LASTTYPE = 'RMS'
C
C              Optional print.
C
               IF( GEOPRT .GE. 1 ) THEN
                  MSGTXT = ' '
                  WRITE(*,'( A, I4, A, F8.2, A, 3(F8.2,''/'',I2,
     1               ''/'', I3 ) )' ) 'MAKESEG:', 
     2              ISEG, ' Choice used RMS sigma.    Last:',
     3              LHIGHSIG, '  Options (sig/sta/src):', 
     4              SAVSIG1(LHIGHSTA), HIGHSTA1, TSRC1, 
     5              SAVSIG2(LHIGHSTA), HIGHSTA2, TSRC2, 
     6              SAVSIG3(LHIGHSTA), HIGHSTA3, TSRC3
                  CALL WLOG( 0, MSGTXT )
               END IF
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
               MSGTXT = ' '
               WRITE( MSGTXT, * ) 'Makeseg got the next source - seg:', 
     1           ISEG, '  Geosrc:', TSRC(ISEG), 
     2           '  ', GEOSRC(TSRC(ISEG)), ' tapprox:', TAPPROX,
     3           ' stascn: ', (sstascn(ista),ista=1,nsta) 
               CALL WLOG( 0, MSGTXT )
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
C           Accumulate INSCN(ISTA), SELECTED, LASTLSCN, STALOW, and
C           STAHIGH.
C
            DO ISTA = 1, NSTA
               IF( STASCN(LSCN,ISTA) ) THEN
                  INSCN(ISTA) = INSCN(ISTA) + 1
                  LASTLSCN(ISTA) = LSCN
                  ELA = ( EL1(LSCN,ISTA) + EL2(LSCN,ISTA) ) / 2.0
                  IF( ELA .LE. GEOLOWEL ) THEN
                     STALOW(ISTA) = STALOW(ISTA) + 1
                  END IF
                  IF( ELA .GE. GEOHIEL ) THEN
                     STAHIGH(ISTA) = STAHIGH(ISTA) + 1
                  END IF
               END IF
            END DO
C
C           Get NSTALOW and NSTAHIGH
C
            NSTALOW = 0
            NSTAHIGH = 0
            DO ISTA = 1, NSTA
               IF( STALOW(ISTA) .GE. LHGOAL ) NSTALOW = NSTALOW + 1
               IF( STAHIGH(ISTA) .GE. LHGOAL ) NSTAHIGH = NSTAHIGH + 1
            END DO
C
C           After there is one high and one low, go back for the
C           second.
C      ***  Deactivated for now by asking LHGOAL to be below 0.
C                  Normally that would be below 1.
C
            IF( NSTALOW .GE. NSTA .AND. NSTAHIGH .GE. NSTA .AND. 
     1          LHGOAL .LE. 0 ) THEN
               LHGOAL = 2
               NSTALOW = 0
               NSTAHIGH = 0
               DO ISTA = 1, NSTA
                  IF( STALOW(ISTA) .GE. LHGOAL ) NSTALOW = NSTALOW + 1
                  IF( STAHIGH(ISTA) .GE. LHGOAL ) NSTAHIGH = NSTAHIGH+1
               END DO
            END IF


            SELECTED(TSRC(ISEG)) = LSCN
C
C           If requested, give feedback on the fit quality.
C
            IF( GEOPRT .GE. 1 ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, 2I4, 1X, A )' ) 
     1          'Makeseg source:',
     2           ISEG, TSRC(ISEG), GEOSRC(TSRC(ISEG))
               IF( METHOD .EQ. 'FIT' ) THEN
                  WRITE( MSGTXT(33:256), '( A, I3, A, 20F5.1 )' ) 
     1               ' High:', HIGHSTA, '  Sigmas: ', 
     3                (SAVSIG(ISTA),ISTA=1,NPRT)
               END IF
               CALL WLOG( 0, MSGTXT )
            END IF
C
C           Save the identity of the worst station.
C
            LHIGHSTA = HIGHSTA
            LHIGHSIG = HIGHSIG
C
C           Reset DORAND to give a chance of filling out low and
C           high el scans.  We might have done a fit iteration because
C           some source couldn't be used because it was not up at a
C           station with too few scans so far.  Also use this opportunity
C           to force going to using psuedo fits for the last few scans
C           of the segment.
C
C           Reset MAXPRIO in case it had been adjusted when getting
C           the last source.
C
            IF( STOPJ(LSCN) .GT. STARTB + 
     1          0.7D0 * ( TGEOEND - STARTB ) ) THEN
               DORAND = .FALSE.
            ELSE
               DORAND = .TRUE.
            END IF
            MAXPRIO = 2
C
C           Go back for the next source.
C
            GO TO 100
C
         ELSE
            IF( GEOPRT .GE. 2 ) THEN
               CALL WLOG( 0, 'makeseg:  No source found that fits' )
               CALL WLOG( 0, '          Ending sequence.' )
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
C     Indention above is for the GO TO 100 loop.
C
      RETURN
      END
