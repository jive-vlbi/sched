      SUBROUTINE GEOQUAL( SC1, SC2, JSCN, TESTQUAL, MKGDEBUG, IDUM,
     1    ZATMERR   )
C
C     Get the quality measure of scans SC1 to SC2.  This gets the
C     quality measure for this particular choice of soruces.  The calling
C     program will then pick the highest set with the highest quality
C     measure.
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
C     The tests are only done for stations in the template scan (JSCN)
C
      INCLUDE 'sched.inc'
C
      INTEGER               SC1, SC2, JSCN, ISCN, ISTA, JSTA, ICH, i,j
      INTEGER               IBAS, NBAS, IPAR, IDUM, IERR
      REAL                  TESTQUAL, SECZ
      REAL                  TRIQUAL, RELEV, RAN5
      LOGICAL               MKGDEBUG
C
C     For fit:
C
      INTEGER               MAXDAT, MAXPAR
      PARAMETER             ( MAXDAT = 10000 )
      PARAMETER             ( MAXPAR = MAXSTA*2 )
      INTEGER               SOLVE(MAXPAR)
      DOUBLE PRECISION      RESDEL(MAXDAT), DELERR(MAXDAT)
      DOUBLE PRECISION      GUESS(MAXPAR)
      DOUBLE PRECISION      PARTIAL(MAXDAT,MAXPAR)
      DOUBLE PRECISION      ZATM(MAXPAR), ZATMERR(*)
      
C -----------------------------------------------------------------------
C     The least squares fit will be done on dummy data for all 
C     baselines.  I need actual dummy measured values (zero), 
C     sigmas (arbitrary 20ps), and the partial derivatives
C     (secZ for the two stations of a baseline with an appropriate
C     sign).  
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
C     Initializations.
C
      NBAS = 0
      DO ISTA = 1, MAXPAR
         RESDEL(ISTA) = 0.0D0
         DELERR(ISTA) = 0.0D0
         DO IBAS = 1, MAXDAT
            PARTIAL(IBAS,ISTA) = 0.D0
         END DO
      END DO
      DO ISCN = SC1, SC2
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
                     IF( NBAS .GT. MAXDAT ) THEN
                        CALL ERRLOG( 'GEOQUAL: Too many scan/'//
     1                    'baselines in making geodetic segment.' )
                     END IF
                     RESDEL(NBAS) = 0.D0 ! 2.D-4 * RAN5( IDUM )
                     DELERR(NBAS) = 20.D0
C	   	   
C                    Partial for first station (is secZ).
C	   	   
                     RELEV = EL1(ISCN,ISTA)
                     SECZ = 1.0 / SIN( RELEV * RADDEG )
                     PARTIAL(NBAS,ISTA) = SECZ
C	   	   
C                    Partial for second station (is negative of secZ).
C	   	   
                     RELEV = EL1(ISCN,JSTA)
                     SECZ = 1.0 / SIN( RELEV * RADDEG )
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
      DO IPAR = 1, NSTA*2-1
         SOLVE(IPAR) = 1
         GUESS(IPAR) = 0.D0
      END DO
C
C     Do the fit.  Note that ZATM and ZATMERR will run through
C     the stations giving what they appear to be, then go through
C     all but the last station again with clock offsets.
C
      CALL LSQFIT( 0, .FALSE., .FALSE., MAXDAT, NSTA*2-1, NBAS,
     1      STANAME, GUESS, SOLVE, RESDEL, PARTIAL, DELERR,
     2      ZATM, ZATMERR, IERR )
C
C     Initialize the quality measure.  Use zero since the final
C     derived quality measure will be the highest across stations.
C
      TESTQUAL = 0.0
C
C     Deal with failure to invert the matrix.  Cause the scan to be 
C     skipped by setting a very high quality measure.  Also write
C     out a bit of information in the hopes of figureing out the
C     cause some day.
C
      IF( IERR .NE. 0 ) THEN
         TESTQUAL = 1.E6
         DO ISCN = SC1, SC2
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I5, A, 20L2  )' ) ' Scan: ', ISCN, 
     1          '  Stations in scan: ', (STASCN(ISCN,ISTA),ISTA=1,NSTA)
            CALL WLOG( 1, MSGTXT )
         END DO
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
C     Set it to zero if the station was not used.
C     This prevents this set from being used, which is the 
C     desired outcome if a station is missed completely.
C
C
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
            IF( MKGDEBUG ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, I5, 2F8.3 )' ) 
     1               'GEOQUAL:  ', ISTA, ZATM(ISTA), 
     2               ZATMERR(ISTA)
               ICH = 35
               DO ISCN = SC1, SC2
                  IF( STASCN(ISCN,ISTA) ) THEN
                     WRITE( MSGTXT(ICH:ICH+5), '( F6.1 )' ) 
     1                       EL1(ISCN,ISTA)
                  ELSE
                     WRITE( MSGTXT(ICH:ICH+5), '( A )' ) '  --- '
                  END IF
                  ICH = ICH + 6
               END DO
               CALL WLOG( 1, MSGTXT )
               MSGTXT = ' '
            END IF
         END IF
      END DO
C
C
      RETURN
      END
