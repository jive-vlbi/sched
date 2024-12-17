      SUBROUTINE CHKIF( KS, RESULT, LOSUM, BBCBW )
C
C     Subroutine to check that frequencies are within the IF's.
C     Before this routine was available, only setup frequencies 
C     were checked.  Doppler and user set frequencies were not
C     checked.
C
C     This routine depends on the variables FIFMIN and FIFMAX
C     being set for each IF and setup.
C
C     This should only be called when DOPPLER or FREQ or BW are
C     used.  In those cases, SCHED prevents inconsistent channelization.
C     Otherwise there is potential confusion with channelization.
C
      INCLUDE      'sched.inc'
      INCLUDE      'schset.inc'
C
      INTEGER            KS, ICH
      DOUBLE PRECISION   BWTOT, OVERLAP, OVERCH
      CHARACTER          RESULT*(*)
      DOUBLE PRECISION   BBCBW(MCHAN)
      DOUBLE PRECISION   LOSUM(MCHAN)
C ---------------------------------------------------------------------
C     Check for how much frequency overlap exists with the IFs.
C     This used to be done checking using FREQ and BW rather than
C     LOSUM and BBCBW, but ran into problems when one or the other
C     of those two was zero for the scan.
C
      BWTOT = 0.D0
      OVERLAP = 0.D0
      DO ICH = 1, NCHAN(KS)
         IF( NETSIDE(ICH,KS) .EQ. 'U' ) THEN 
            OVERCH = MIN( LOSUM(ICH) + BBCBW(ICH), 
     1                     FIFMAX(ICH,KS) ) - 
     2                MAX( LOSUM(ICH), FIFMIN(ICH,KS) )
            OVERCH = MAX( OVERCH, 0.0D0 )
         ELSE
            OVERCH = MIN( LOSUM(ICH), FIFMAX(ICH,KS) ) - 
     1                MAX( LOSUM(ICH) - BBCBW(ICH), 
     2                     FIFMIN(ICH,KS) )
            OVERCH = MAX( OVERCH, 0.0D0 )
         END IF
         OVERLAP = OVERLAP + OVERCH
         BWTOT = BWTOT + BBCBW(ICH)
      END DO
C
C     Check how the overlap did.  Protect against bit rounding.
C
      IF( OVERLAP .GE. BWTOT * 0.99999D0 ) THEN
         RESULT = 'OK'
      ELSE
         WRITE( RESULT, '( A, F8.2, A, F8.2, A )' )
     1     ' Only ', OVERLAP, ' MHz of ', BWTOT, 
     2     ' MHz in scheduled channels is within IFs.'
      END IF
C
      RETURN
      END




