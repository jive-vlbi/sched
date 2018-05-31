      SUBROUTINE CHKSPD( KS, ERRS )
Cf2py intent(in, out) ERRS
C
C     Routine for SCHED called by CHKSET that checks the 
C     bit rate per track and the tape speeds for VLBA, Mark III, and
C     Mark IV recorder systems.  The speed checks are obsolete
C     and are removed.
C
      INCLUDE     'sched.inc'
      INCLUDE     'schset.inc'
C
      INTEGER     KS, BITRATE
      LOGICAL     ERRS, OK
C -------------------------------------------------------------------
C     Only make these checks for Mark III/IV or VLBA formats.
C
      IF( FORMAT(KS) .NE. 'MARKIII' .AND. 
     1    FORMAT(KS)(1:4) .NE. 'VLBA' .AND. 
     2    FORMAT(KS)(1:4) .NE. 'MKIV' ) 
     3      RETURN
C
C     Check the requested bit rate per track.  All of these systems
C     can handle 2, 4, and 8 Mbps per track.  The MarkIV formatters
C     can also do 16 Mbps per track.  A VLBA4 DAR is mostly a VLBA
C     DAR but includes a MarkIV formatter, so it can also do 16 Mbps
C     per track.
C
      BITRATE = TBPS(KS) + 0.001
      OK = BITRATE .EQ. 2 .OR. BITRATE .EQ. 4 .OR. BITRATE .EQ. 8
      OK = OK .OR. 
     1    ( BITRATE .EQ. 16 .AND. DAR(ISETSTA(KS)) .EQ. 'MKIV' ) .OR.
     4    ( BITRATE .EQ. 16 .AND. DAR(ISETSTA(KS)) .EQ. 'VLBA4' )
      IF( .NOT. OK ) THEN
         CALL WLOG( 1, 'CHKSPD: SAMPRATE and FORMAT(KS) do not ' //
     1      'combine to give valid bit rate on tape.' )
         MSGTXT = ' '
         WRITE( MSGTXT, '( A, F10.3, 2A, A, I5, 4A )' ) 
     1         'CHKSPD: SAMPRATE=', SAMPRATE(KS), '   FORMAT=',
     2         FORMAT(KS), '   BITRATE=', BITRATE, '  DAR at ',
     3         STATION(ISETSTA(KS)), ' is ', DAR(ISETSTA(KS))
         CALL WLOG( 1, MSGTXT )
         ERRS = .TRUE.
      END IF
C
C     The checking of the tape speeds was here and was removed
C     on Jan. 7, 2011.  RCW.
C
      RETURN
      END


