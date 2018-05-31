      SUBROUTINE CHKWIDAR( KS, ERRS )
Cf2py intent(in, out) ERRS
C
C     Routine for SCHED called by CHKSET that checks items related
C     to the use of the WIDAR correlator to format Mark5 disks.
C
C     This is basically just a stub so far.
C
C     Started May 09, 2012.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER           KS !  , ICH
      LOGICAL           ERRS
C      LOGICAL           DDWARN, SBWARN
C      CHARACTER         USEDIFC(2)*2
C      DATA              DDWARN, SBWARN / .TRUE., .TRUE. /
C      SAVE              DDWARN, SBWARN
C ------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'CHKWIDAR: Starting' )
      ERRS = .FALSE.
      RETURN
      END
