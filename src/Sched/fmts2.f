      SUBROUTINE FMTS2( KS, OK )
C
C     Routine for SCHED called by SETFMT that sets the format 
C     for S2 systems for setup group KS.  It will attempt to 
C     set FORMAT, SPEEDUP, FANOUT, and TAPEMODE.  OK will 
C     return .TRUE. if it succeeds.  It will return .FALSE. 
C     if not, which for S2 should not happen.
C
      INCLUDE        'sched.inc'
      INCLUDE        'schset.inc'
C
      INTEGER        KS, TMODE, LEN1
      REAL           TBPR, RECBPS
      LOGICAL        OK
C
C     Fix the bit rate for a recorder.
C 
      PARAMETER      (RECBPS=16.0)
C ---------------------------------------------------------------------
C   
C     FORMAT is already set or we wouldn't be in this routine.
C     I think the format is totally determined by the total bit rate.
C
      IF( FANOUT(KS) .NE. 0.0 ) GO TO 999
C
C     TAPEMODE is the number of passes through the N recorders
C     at RECBPS Mbps per recorder.
C
      TBPR = NCHAN(KS) * SAMPRATE(KS) * BITS(1,KS)
      TMODE = NINT( RECBPS * STNDRIV(ISETSTA(KS)) / TBPR )
      IF( TAPEMODE(KS) .GT. 0 ) THEN
         IF( TAPEMODE(KS) .NE. TMODE ) THEN
            CALL WLOG( 1, 
     1        'FMTS2: FORMAT S2 TPMODE does not seem appropriate' )
            MSGTXT = ' ' 
            WRITE( MSGTXT, '( A, I5, A, I5 )' ) 
     1         '        User supplied TPMODE =', TAPEMODE(KS), 
     2         ' - appropriate value for format = ', TMODE
            CALL WLOG( 1, MSGTXT )
            CALL WLOG( 1, '      Setup file: ' //
     1         SETNAME(KS)(1:LEN1(SETNAME(KS))) )
         END IF
      ELSE
         TAPEMODE(KS) = TMODE
      END IF
C 
C     I don't really know what fanout and speedup should be.  But
C     assume that FANOUT is sample rate/16
C 
      FANOUT(KS) = SAMPRATE(KS) / RECBPS
      SPEEDUP(KS) = 1.0
C
  999 CONTINUE
      OK = .TRUE.
      RETURN
      END




