      SUBROUTINE GEOQUAL( SC1, SC1A, SC2, JSCN, MINSEL, TESTQUAL,
     1                    PRDEBUG, ZATMERR )
Cf2py intent(in) SC1, SC1A, SC2, JSCN, MINSEL, PRDEBUG
Cf2py intent(out) TESTQUAL
Cf2py intent(in, out) ZATMERR
C
C     Get the quality measure of scans SC1 to SC2.  This gets the
C     quality measure for this particular choice of sources.  The calling
C     program will then pick the highest set with the highest quality
C     measure.  
C
C     If SC1A is not equal to SC1, it is assumed that the
C     previous call accumulated data for SC1 to SC1A-1 and that only
C     SC1A to SC2 need to be accumulated here.  This is an attempt to
C     reduce the run time in the MAKESEG process.
C
C     Isolate this to a subroutine in case someone wants to brew 
C     their own measure.  If someone has a quality measure that should
C     be low, it can just be multiplied by -1 and I think it should
C     work.
C
C     For the original version of the routine, the measure was the minimum 
C     over stations of the rms SecZ times sqrt(N).  The sqrt(N) 
C     favors sequences with more scans.  Otherwise, we are selecting
C     on the worst RMS spanned SecZ.
C
C     Mark Reid pointed out that the stations should not be done in 
C     isolation.  For example, a scan with all stations high plus
C     a scan with all stations low would have a good spread of secZ
C     but would not produce a good solution because there is nothing
C     tieing the low to the high.  One needs at least one or more
C     scans with some stations high and some low.  
C
C     Because of the above, I have set up a dummy least squares fit 
C     for the atmospheric terms (ignoring clock) and look at the errors 
C     on the derived zenith atmospheric delays.  I then look at the
C     worst such error.   The initial trials allowed one station with
C     very little SecZ range.  This might be because the fit does not
C     include a constant term so even one observation constrains SecZ.
C     Add a clock offset.
C
C     MINSEL allows minimizing the reward for really low elevation
C     scans.  The secZ will not be allowed to go above sec(90-minsel)
C
C     The tests are only done for stations in the template scan (JSCN)
C
      INCLUDE 'sched.inc'
C
      INTEGER               SC1, SC1A, SC2, JSCN, ISCN
      INTEGER               ISTA, JSTA, ICH
      INTEGER               IBAS, NBAS, IERR
      REAL                  TESTQUAL, SECZ
      REAL                  TRIQUAL, RELEV  !  , RAN5
      REAL                  MINSEL, SMINSEL
      LOGICAL               PRDEBUG
C
C     For fit:
C
      INTEGER               MAXDAT, MAXPAR
      PARAMETER             ( MAXDAT = 10000 )
      PARAMETER             ( MAXPAR = MAXSTA*2 )
      INTEGER               SOLVE(MAXPAR), NNBAS(MAXSCN), LSC2
      INTEGER               NSTASCN(MAXSTA), MINNSS
      DOUBLE PRECISION      RESDEL(MAXDAT), DELERR(MAXDAT)
      DOUBLE PRECISION      GUESS(MAXPAR)
      DOUBLE PRECISION      PARTIAL(MAXDAT,MAXPAR)
      DOUBLE PRECISION      PPARTIAL(MAXDAT,MAXPAR)
      DOUBLE PRECISION      ZATM(MAXPAR), ZATMERR(*)
C
      DATA   SOLVE  / MAXPAR * 1 /
      DATA   GUESS  / MAXPAR * 0.D0 /
      DATA   RESDEL / MAXDAT * 0.D0 /
      DATA   DELERR / MAXDAT * 100.D0 /
      DATA   LSC2   / 0 /
C
C     Save some numbers for incremental calls.
C
      SAVE                  NNBAS, PARTIAL, LSC2
C -----------------------------------------------------------------------
C     The least squares fit will be done on dummy data for all 
C     baselines.  I need actual dummy measured values (zero), 
C     sigmas (arbitrary 20ps), and the partial derivatives
C     (secZ for the two stations of a baseline with an appropriate
C     sign).  
C
C     Try setting the sigmas to 100ps to match what Mark did.  July 23
C     2010
C
C     Require that the station be in the template scan and in
C     the individual scans.  Actually, that is inforced when STASCN
C     is set for the individual scans.
C
      IF( NSTA .LE. 1 ) THEN
         CALL ERRLOG( 'GEOQUAL:  Cannot insert DELZN segments for '//
     1           'single station observations.' )
      END IF
C
C     Initializations.  Set NBAS to use the values previously
C     calculated for scans SC1 to SC1A.
C
      IF( SC1 .EQ. SC1A ) THEN
         NBAS = 0
         DO ISCN = SC1, SC2
            NNBAS(ISCN) = 0
         END DO
      ELSE
         NBAS = NNBAS(SC1A-1)
         DO ISCN = SC1A, SC2
            NNBAS(ISCN) = 0
         END DO
      END IF
C
      DO ISTA = 1, 2 * NSTA
         DO IBAS = NBAS+1, MAXDAT
            PARTIAL(IBAS,ISTA) = 0.D0
         END DO
      END DO
C
      SMINSEL = 1.0 / SIN( MINSEL * RADDEG )
      DO ISCN = SC1A, SC2
C
C        Station 1 loop.
C
         DO ISTA = 2, NSTA
            IF( STASCN(ISCN,ISTA) .AND. UP1(ISCN,ISTA) .EQ. ' ' .AND.
     1          UP2(ISCN,ISTA) .EQ. ' ' ) THEN
C
C              Station 2 loop.
C
               DO JSTA = 1, ISTA - 1
                  IF( STASCN(ISCN,JSTA) .AND. UP1(ISCN,JSTA) .EQ. ' '
     1                .AND. UP2(ISCN,JSTA) .EQ. ' ' ) THEN
C
C                    Get the baseline data for the fit.
C
                     NBAS = NBAS + 1
C
                     IF( NBAS .GT. MAXDAT ) THEN
                        CALL ERRLOG( 'GEOQUAL: Too many scan/'//
     1                    'baselines in making geodetic segment.' )
                     END IF
C
C                    Accumulate the highest number baseline for
C                    each scan for use in incremental calls.
C
                     NNBAS(ISCN) = NBAS
C
C                    SecZ partial for first station.
C                    Don't reward excessively low elevation scans.
C                    Try a max secz for the fit corresponding to
C                    MINSEL.  Make a parameter so that it can be
C                    used differently in MAKESEG and in the final
C                    selection.
C
                     RELEV = EL1(ISCN,ISTA)
                     SECZ = MIN( 1.0 / SIN( RELEV * RADDEG ), SMINSEL )
                     PARTIAL(NBAS,ISTA) = SECZ
C
C                    SecZ partial for second station (is negative of secZ).
C
                     RELEV = EL1(ISCN,JSTA)
                     SECZ = MIN( 1.0 / SIN( RELEV * RADDEG ), SMINSEL )
                     PARTIAL(NBAS,JSTA) = -1.0 * SECZ
C
C                    Add the partials for the clock terms.  Put those 
C                    in NSTA+1 to NSTA*2-1.  Do not fit for station 
C                    NSTA (one has to be fixed) and making it the last
C                    simplifies some bookkeeping.
C
                     IF( ISTA .NE. NSTA) THEN
                        PARTIAL(NBAS,ISTA+NSTA) = 1.0
                     END IF
                     PARTIAL(NBAS,JSTA+NSTA) = -1.0
C
                  END IF
               END DO
            END IF
         END DO
      END DO
C
C     Move the partials for all baselines to an array to pass
C     to LSQFIT.  LSQFIT modifies them, so this step is needed
C     when trying to save computations above by not redoing
C     the ones for scans SC1 to SC1A-1.  
C
      DO ISTA = 1, NSTA*2-1
         DO IBAS = 1, NBAS
            PPARTIAL(IBAS,ISTA) = PARTIAL(IBAS,ISTA)
         END DO
      END DO
C
C     Do the fit.  Note that the first half of the ZATM and ZATMERR 
C     arrays will be the items apparent from the names for all
C     stations.  The second half of the arrays, minus one, will be
C     for the clocks at the stations.  One clock is held fixed.
C     Jump throughsome hoops to prevent spewing excessive error
C     messages.
C
      IF( LSC2 .NE. SC2 ) THEN
         IERR = GEOPRT
      ELSE
         IF( IERR .GE. 1 ) IERR = 0
      END IF
C
C     Sometimes there might be too little data to fit for all the
C     parameters.  In fact, we are counting baselines, but many are
C     actually redundant in terms of giving useful information.  So
C     don't do the fit unless each station participates in 3 scans.
C     That is one more than strictly needed, but should be ok.
C
      DO ISTA = 1, NSTA
         NSTASCN(ISTA) = 0
         DO ISCN = SC1, SC2
            IF( STASCN(ISCN,ISTA) .AND. UP1(ISCN,ISTA) .EQ. ' ' .AND.
     1          UP2(ISCN,ISTA) .EQ. ' ' ) THEN
               NSTASCN(ISTA) = NSTASCN(ISTA) + 1
            END IF
         END DO
      END DO
      MINNSS=999
      DO ISTA = 1, NSTA
         MINNSS = MIN( NSTASCN(ISTA), MINNSS )
      END DO
C
C     If there aren't enough delays to each station, skip the fit.  
C     Set IERR = 1 so the rest of the routine thinks the fit failed.
C     Tell the user if appropriate.
C
      IF( MINNSS .LT. 3 ) THEN
         IERR = 1
         IF( GEOPRT .GE. 1 ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, 5I5 )' ) 
     1         'GEOQUAL skipping fit - inadequate data ', 
     2         NSTA, NBAS, SC1, SC1A, SC2
            CALL WLOG( 0, MSGTXT )
            MSGTXT = ' '
         END IF
      ELSE
         CALL LSQFIT( 0, .FALSE., .FALSE., MAXDAT, NSTA*2-1, NBAS,
     1         STANAME, GUESS, SOLVE, RESDEL, PPARTIAL, DELERR,
     2         ZATM, ZATMERR, IERR )
C
C        Initialize the quality measure.  Use zero since the final
C        derived quality measure will be the highest across stations.
C
         TESTQUAL = 0.0
      END IF
C
C     Deal with failure to invert the matrix.  Cause the scan to be 
C     skipped by setting a very high quality measure.  Also write
C     out a bit of information in the hopes of figureing out the
C     cause some day.
C
      IF( IERR .NE. 0 ) THEN
         TESTQUAL = 1.E6
         IF( GEOPRT .GE. 2 ) THEN
            DO ISCN = SC1, SC2
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, I5, A, 20L2  )' ) ' Scan: ', ISCN, 
     1          '  Stations in scan: ', (STASCN(ISCN,ISTA),ISTA=1,NSTA)
               CALL WLOG( 0, MSGTXT )
            END DO
         END IF
         IF( GEOPRT .GE. 1 ) THEN
            IF( SC2 .NE. LSC2 ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( 3A, 2I5, 3A )' ) 
     1            ' GEOQUAL: Fit error when testing ', SCNSRC(SC2), 
     2            ' to scans:', SC1, SC2, ' Skip source this scan.' 
               CALL WLOG( 0, MSGTXT )
               CALL WLOG( 0, '        This message can usually be'//
     1            ' ignored safely when inserting geodetic segments.' )
               LSC2 = SC2
            ELSE
               CALL WLOG( 0, ' GEOQUAL: Similar error testing '//
     1             SCNSRC(SC2) )
            END IF
C              
         END IF
C         
C         WRITE(*,*) 'GEOQUAL: Partials matrix: '
C         DO J = 1, NBAS
C            WRITE(*,'( I5, 40F8.3 )' ) J, 
C     1             ( PARTIAL(J,I), I=1, NSTA*2-1 )
C         END DO
      END IF
C
C     Set TESTQUAL to the highest (worst) ZATMERR seen across
C     stations.  Later the set with the lowest TESTQUAL will
C     be chosen.
C
      IF( PRDEBUG ) THEN
         MSGTXT = ' '
         WRITE( MSGTXT, '( A )' )
     1        'GEOQUAL:   Sta  Zatm RMS      El by scan. '
         CALL WLOG( 0, MSGTXT )
      END IF
      DO ISTA = 1, NSTA
         IF( STASCN(JSCN,ISTA) ) THEN
C
C           Pick the quality measure.  Separated to TRIQUAL 
C           in case I want to try varients.
C
            TRIQUAL =  ZATMERR(ISTA)
C
            TESTQUAL = MAX( TESTQUAL, TRIQUAL )
C
C           Some printout if the programmer is trying to watch
C           the process in detail.  MKGDEBUG is hard wired in 
C           GEOMAKE.
C
            IF( PRDEBUG ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, 2X, A3, F8.3 )' ) 
     1               'GEOQUAL:  ', STCODE(STANUM(ISTA)), 
     2               ZATMERR(ISTA)
               ICH = 26
               DO ISCN = SC1, SC2
                  IF( ICH .GT. 250 ) THEN
                     CALL WLOG( 1, MSGTXT ) 
                     MSGTXT = ' '
                     ICH = 26
                  END IF
                  IF( STASCN(ISCN,ISTA) ) THEN
                     WRITE( MSGTXT(ICH:ICH+5), '( F6.1 )' ) 
     1                       EL1(ISCN,ISTA)
                  ELSE
                     WRITE( MSGTXT(ICH:ICH+5), '( A )' ) '  --- '
                  END IF
                  ICH = ICH + 6
               END DO
               CALL WLOG( 0, MSGTXT )
               MSGTXT = ' '
            END IF
         END IF
      END DO
C
C
      RETURN
      END
