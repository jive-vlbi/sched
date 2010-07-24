      SUBROUTINE CORSOC
C
C     Subroutine for SCHED, called by CORLST, that deals with correlator
C     constraints specific to the Socorro VLBA hardware correlator.  This
C     is now out of date because that correlator is shut down as of late
C     December 2009.  Changed the meaning of CORREL=SOCORRO or VLBA to
C     be DIFX and added FXCORR to mean the hardware correlator, just in
C     case someone wants it.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER           LSTA, LSCN, NSANT, MOSTCH
      REAL              FIRRATE, MAXFIRR, RCORCH
      LOGICAL           WARN
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'CORSOC starting.' )
      WARN = .FALSE.
C
C     This routine is only for the SOCORRO VLBA correlator
C
C      IF( CORREL(1:7) .EQ. 'SOCORRO' .OR. 
C     1   ( CORREL(1:4) .EQ. 'VLBA' .AND. LEN1( CORREL ) .EQ. 4 ) ) THEN
      IF( CORREL(1:6) .EQ. 'FXCORR' ) THEN
C
C        Do some initializations.
C
         MAXFIRR = 0.0
         MOSTCH = 0
C
C        Loop through scans.
C
         DO LSCN = SCAN1, SCANL
C
C           Get number of stations in the scan.
C
            NSANT = 0
            DO LSTA = 1, NSTA
               IF( STASCN(LSCN,LSTA) ) THEN
                  NSANT = NSANT + 1            
               END IF
            END DO
            IF( NSANT .GT. 0 .AND. .NOT. NOREC(LSCN) ) THEN
C
C              Deal with a special internal limitation in the 
C              SOCORRO hardware correlator.
C
C              Get the rate that channels are transferred through
C              the FIR filter in the correlator.  This cannot be
C              more than 44236-2048=41188 spectral channels per
C              0.131 sec tic.  The correlator always transfers
C              2048 spectral channels divided by the spectral
C              averaging, regardless of how many are kept.
C              Usually, the correlator does 512 point FFTs producing
C              256 point spectra, which are then averaged to 16 
C              or 32 for continuum projects.  
C              Use a real version of CORCHAN to avoid getting zero
C              out of one of the ratios while it is still integer.
C
               RCORCH = CORCHAN
               FIRRATE = ( 0.131 / CORAVG ) * 
     1                   ( NSANT * ( NSANT + 1.0 ) / 2.0 ) *
     2                   2048.0 * ( RCORCH / MAX( 256.0, RCORCH ) )
     3                   * FSPEED(SETNUM(LSCN))
               MOSTCH = MAX( MSCHN(SETNUM(LSCN)), MOSTCH )
C
               MAXFIRR = MAX( MAXFIRR, FIRRATE )
            END IF
         END DO

C
C        Warn if data rate is too high.  The limit was changed from
C        500 Mbytes/sec to 1000 Mbytes/sec on May 9, 2003.
C
         IF( MAXDR .GT. 1000000. .AND. .NOT. TWOHEAD ) THEN
            WRITE( ISUM, '( A, /, A, A / A )' )
     1           ' **** WARNING ****',
     2           '    Projected correlator output data rate ',
     3           'exceeds VLBA correlator ',
     4           '    limit of 1000 kbytes per second. '
C            WRITE( ISUM, '( A, A )' ) 
C     1           '                    ',
C     2           'Higher rates may be ok.  Contact Socorro for info.'
            CALL WLOG( 1, 'CORSOC:  **** WARNING ****' )
            CALL WLOG( 1, '    Correlator output '//
     1         'data rate limit exceeded.  See summary file.' )
            WARN = .TRUE.
         ELSE IF( MAXDR .GT. 2.0E6 .AND. TWOHEAD ) THEN
            WRITE( ISUM, '( A, A, A / T22, A )' )
     1           ' **** WARNING ****',
     2           'Projected correlator output data rate ',
     3           'exceeds VLBA correlator ',
     4           'limit of 1000 kbytes per second per pass. '
C            WRITE( ISUM, '( A, A )' ) 
C     1           '                    ',
C     2           'Higher rates may be ok.  Contact Socorro for info.'
            WRITE( ISUM, '( T22, A, A )' )
     1           'Assuming 2 pass processing for this wide band ',
     2           'observation.'
            CALL WLOG( 1, 'CORSOC: **** WARNING **** ' )
            CALL WLOG( 1, '    Correlator output '//
     1           'data rate limit exceeded.  See summary.' )
            WARN = .TRUE.
         END IF
C
C        Warn of the FIR transfer rate limit if it is exceeded for
C        the SOCORRO correlator.
C
         IF( MAXFIRR .GT. 41188 ) THEN
            WRITE( ISUM, '( 1X, A, A / T22, A, F8.0 )' )
     1      ' ******** WARNING:  You have exceeded the FIR transfer ',
     2      'rate limit of 41188 ',
     3      'channels per tic.  Your projected rate is ', MAXFIRR
            IF( CORAVG .NE. CORAV2 .AND. CORAV2 .NE. 0.0 ) THEN
               WRITE( ISUM, '( T22, A, A )' )
     1             'This projection does not take into account your ',
     2             'secondary averaging time.'
            END IF
            IF( MOSTCH .GT. 1 ) THEN
               WRITE( ISUM, '( T22, A, A )' ) 
     1                   'This cannot be fixed by processing ',
     2                   'IF channels separately.'
            END IF
            WRITE( ISUM, '( T22, A, A )' )
     1         'Please contact someone in Socorro if you don''t ',
     2         'understand this.'
            WARN = .TRUE.
         END IF
         IF( WARN .AND. TWOHEAD ) THEN
            WRITE( ISUM, '( T5, 2A, /, T5, 2A )' )
     1                    'If you are doing two pass processing ',
     2                    '(eg 512 Mbps), ',
     3                    'the output bit rate and FIR rate limits ',
     4                    'are twice the above values.'
         END IF
      END IF

      RETURN
      END



