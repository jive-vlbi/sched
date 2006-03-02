      SUBROUTINE CHKSREC( KS, ERRS )
C
C     Routine for SCHED called by CHKSET that checks items related
C     to the S2 RECORDERS's.  This probably needs lots of work.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER    KS
      INTEGER    NS2USD
      CHARACTER  IS2USD(16)*4, S2MODE*7
      LOGICAL    ERRS, VALID
C ----------------------------------------------------------------------
C     Check the format request.
C
      IF( FORMAT(KS) .NE. 'S2' ) THEN
         MSGTXT = ' '
         WRITE( MSGTXT, '( A, A)' )
     1       'CHKSREC: Invalid FORMAT for S2 recording: ',
     2       FORMAT(KS)
         CALL WLOG( 1, MSGTXT )
         ERRS = .TRUE.
      END IF
C
C     Check tape speeds. These are very specific
C
      IF( SPEEDH(KS) .NE. 4.2 .OR. SPEEDL(KS) .NE. 6.3 ) THEN
         MSGTXT = ' '
         WRITE( MSGTXT, '( A, 2F6.2 )' )
     1       'CHKSREC: Incorrect tape speeds for S2 recorder:', 
     2       SPEEDH(KS), SPEEDL(KS)
         CALL WLOG( 1, MSGTXT )
         ERRS = .TRUE.
      END IF
C
C     Check the tpmode is valid to S2, simply call mode determination
C
      CALL VXS2MD( KS, S2MODE, VALID, NS2USD, IS2USD, .FALSE. )
      IF( .NOT. VALID ) THEN
         MSGTXT = ' '
         WRITE( MSGTXT, '( A )' ) 'CHKSREC: Unable to '//
     1        'determine consistent S2 mode'
         CALL WLOG( 1, MSGTXT )
         ERRS = .TRUE.
      END IF
C
      RETURN
      END




