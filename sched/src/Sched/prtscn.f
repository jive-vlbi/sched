      SUBROUTINE PRTSCN( ISCN )
C
C     Print identifying information about a scan.  This will mainly
C     be used to help identify a problem scan when an error occurs.
C
      INCLUDE           'sched.inc'
C
      INTEGER           ISCN, ISTA, LEN1, LC, NC
      DOUBLE PRECISION  START, STOP
      INTEGER           YEAR1, YEAR2, DAY1, DAY2
      CHARACTER         TFORM*15, TSTOP*8, TSTART*8
C  ------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 1, 'PRTSCN starting.' )
C
C     Get the start and stop times.
C
      CALL TIMEJ( STARTJ(ISCN), YEAR1, DAY1, START )
      CALL TIMEJ( STOPJ(ISCN), YEAR2, DAY2, STOP )
      TSTART = TFORM( START, 'T', 0, 2, 2, '::@' )
      TSTOP  = TFORM( STOP, 'T', 0, 2, 2, '::@' )
C
      CALL WLOG( 0, '  ' )
      MSGTXT = '  '
      WRITE( MSGTXT, '( A, I5 )' )
     1    '  Scan number:  ', ISCN
      CALL WLOG( 0, MSGTXT )
C
      MSGTXT = '  '
      WRITE( MSGTXT, '( A, I4, A, I3.3, A, A )' )
     1    '  Start time:    ', YEAR1, '-', DAY1, '-', TSTART
      CALL WLOG( 0, MSGTXT )
C
      MSGTXT = '  '
      WRITE( MSGTXT, '( A, I4, A, I3.3, A, A )' )
     1    '  Stop time:     ', YEAR2, '-', DAY2, '-', TSTOP
      CALL WLOG( 0, MSGTXT )
C
      MSGTXT = '  '
      WRITE( MSGTXT, '( A, A )' )
     1    '  Source:        ', SCNSRC(ISCN)(1:LEN1(SCNSRC(ISCN)))
      CALL WLOG( 0, MSGTXT )
C
      MSGTXT = ' '
      WRITE( MSGTXT, '( A )' ) '  Station codes: '
      LC = 17
      DO ISTA = 1, NSTA 
         IF( STASCN(ISCN,ISTA) ) THEN
            NC = LEN1( STCODE(STANUM(ISTA)) )
            IF( LC .NE. 17 ) THEN
               WRITE( MSGTXT(LC+1:LC+1), '( A )' ) ','
               LC = LC + 1
            END IF
            WRITE( MSGTXT(LC+1:LC+NC), '( A )' ) 
     1          STCODE(STANUM(ISTA))(1:NC)
            LC = LC + NC
         END IF
      END DO
      CALL WLOG( 0, MSGTXT )
C
      MSGTXT = ' '
      IF( SETNUM(ISCN) .GT. 0 ) THEN
         WRITE( MSGTXT, '( A, A )' ) '  Setup file:    ',
     1      SETFILE(SETNUM(ISCN))(1:LEN1(SETFILE(SETNUM(ISCN))))
      ELSE
         WRITE( MSGTXT, '( A, A )' ) '  Setup file number ', 
     1      SETNUM(ISCN)
      END IF
      CALL WLOG( 0, MSGTXT )
C
      CALL WLOG( 1, 'PRTSCN: Scan information was '//
     1           'written to the log file.' )
      RETURN
      END

