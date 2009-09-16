      SUBROUTINE CHKVDIFX
C
C     Routine for SCHED called by DEFSET that checks that various
C     parameters are consistent with correlation on the VLBA DiFX
C     software correlator.
C
      INCLUDE     'sched.inc'
      INCLUDE     'schset.inc'
C
      INTEGER    KS, OVERSAMP, NFFTS, NPFFTS
      REAL       TAVG, RNFFTS
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'CHKVDIFX: starting.' )
C
C
      IF( ( CORREL(1:7) .EQ. 'VLBADIFX' ) .AND. .NOT. NOTAPE ) THEN
C
C        Give some feedback on the average time request.
C        If "EXACT" is not specified as the second argument to 
C        CORAVG, the correlator will adjust the average time request
C        to be an even number of short term accumulations which in
C        turn are an even number of FFT intervals.  SCHED does not
C        know the algorithm for breaking down short and long term
C        integrations, so it cannot predict exactly what is going
C        to happen.  If "EXACT" is given, the correlator will bin
C        the scan and each integration will include whatever short
C        term integrations whose center time falls in the bin.  Thus
C        there is a jitter in the actual mean time of the data, although
C        this jitter won't be reflected in the data times.  Tell 
C        the user what I can about his/her situation.
C

C OLD COMMENTS:

C        Deal with the averaging time request.  There are two modes.
C        If the user specifies "CORAVG=2.0,exact", he/she will get bins 
C        that are exactly 2.0000 seconds long and have time tags in
C        the middle of the 2 second bin.  There may be a variable 
C        number of FFTs in each bin.  If the "exact" is left off,
C        then the average interval is adjusted by SCHED to the nearest 
C        even multiple of the FFT length.
C            See email from me to Walter at 1:22 on Sept. 4.2009.
C
C        Loop over setups to check all of them.
C        Note that we don't yet have a concept of making spectra that
C        cover more spectrum than the baseband filter.  So calculate
C        the oversampling factor based on the bandwidth and sample rate.
C        We could just use the bandwidth in the calculation, but I want
C        to leave open having the FFT wider than the baseband.
C      
         DO KS = 1, NSET
            OVERSAMP = SAMPRATE(KS) / ( 2.0 * BBFILT(1,KS) )
            RNFFTS =  CORAVG * SAMPRATE(KS) / ( CORCHAN * OVERSAMP )
            NFFTS = NINT( RNFFTS )
            TAVG = NFFTS * CORCHAN * OVERSAMP / SAMPRATE(KS)
            IF( ABS( TAVG - CORAVG ) .GT. 0.00001 ) THEN
               IF( RNFFTS .GT. REAL( NFFTS ) ) THEN
                  NPFFTS = NFFTS
               ELSE
                  NPFFTS = NFFTS - 1
               END IF
C
               MSGTXT = ' '
               CALL WRTMSG( 1, 'CHKVDIFX', 'CORAVGsetting' )
               WRITE( MSGTXT, '( 2A )' )  'Setup file: ', SETNAME(KS)
               CALL WLOG( 1, MSGTXT )
C
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, F10.6 )' )  
     1            'Requested integration time (CORAVG): ', CORAVG
               CALL WLOG( 1, MSGTXT )
C
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, F10.6 )' )  
     1            'Closest time for even number of FFTs: ', TAVG
               CALL WLOG( 1, MSGTXT )
C
               IF( CAEXACT ) THEN
                  CALL WLOG( 1, 
     1               'EXACT specified - pass CORAVG unmodified.' )
               ELSE
                  CALL WLOG( 1, 'Will set CORAVG to TAVG' )
     1               
                  CORAVG = TAVG
               END IF
C
C*** have an input and used CORAVG.  eg CORAVGIN for the input.
CCheck if other using routines come earlier or later, and pass CORAVGIN
Cto CORAVG if not DiFX.
C
            END IF
         END DO
C
      END IF
C
      RETURN
      END
