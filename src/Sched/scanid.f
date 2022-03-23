      SUBROUTINE SCANID( MODE, ISCN, ISTA )
C
C     Routine for SCHED, to be called mainly when warnings or errors
C     are being issued.  It writes identification information about
C     the scan.  If ISTA is 0, the station identification will be left
C     off.
C
C     MODE is the write mode for messages (routine WLOG) and is 0 or 1.
C     0 writes to the log file only.  1 writes to the logfile and to
C     the terminal.
C
      INCLUDE        'sched.inc'
C
      INTEGER           ISCN, ISTA, YEAR, DAY1, DAY2, LEN1, ICH, MODE
      CHARACTER         TFORM*8, CSTART*8, CSTOP*8
      DOUBLE PRECISION  START, STOP
C -------------------------------------------------------------------      
      IF( DEBUG ) CALL WLOG( 0, 'SCANID starting' )
C
C     Get the scan times.
C
      CALL TIMEJ( STARTJ(ISCN), YEAR, DAY1, START )
      CALL TIMEJ( STOPJ(ISCN), YEAR, DAY2, STOP )
      CSTART = TFORM( START, 'T', 0, 2, 2, '::@' )
      CSTOP = TFORM( STOP, 'T', 0, 2, 2, '::@' )
C
      WRITE( MSGTXT, '( A, I5, I5, A, A, A, I3, A, A, 2X, A )' )
     1   ' Scan', ISCN, DAY1, '/', CSTART, ' - ', DAY2, '/', CSTOP,
     2   SCNSRC(ISCN)
C
C     Add station if ISTA not zero.
C
      IF( ISTA .NE. 0 ) THEN
         ICH = LEN1( MSGTXT )
         WRITE( MSGTXT(ICH+2:ICH+20), '( A, A )' )
     1           '  Station ', STANAME(ISTA)
      END IF
C
C     Now write the result.
C
      ICH = LEN1( MSGTXT )
      CALL WLOG( MODE, MSGTXT(1:ICH) )
C
      RETURN
      END
