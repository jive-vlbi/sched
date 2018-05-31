      SUBROUTINE CHKCDAS( KS, ERRS )
Cf2py intent(in, out) ERRS
C
C     Routine called by CHKSET to check the setup for a CDAS 
C     (Chinese Data Aquisition System) station.
C
C     For now, this is mostly a dummy.  Checks will be added 
C     as they are understood.
C
      INCLUDE   'sched.inc'
      INCLUDE   'schset.inc'
C
      INTEGER    KS
      LOGICAL    ERRS
C ----------------------------------------------------------------
      MSGTXT = ' '
      WRITE( MSGTXT, '( A, I4, A )' )
     1    'CHKCDAS: Setup ', KS, ' is for a CDAS station.'
      CALL WLOG( 1, '          SCHED does not yet do much checking '//
     1    'of CDAS parameters.' )
      CALL WLOG( 1, '          Be sure your setup parameters are '//
     1    'correct for this station!' )
C
      RETURN
      END
