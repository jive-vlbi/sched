      SUBROUTINE MAXBAS( XYMAX )
C
C     Routine for SCHED plotting to determine the maximum baseline
C     length for plotting.  Make it 5% longer than the longest baseline.
C
      INCLUDE  'sched.inc'
C
      INTEGER           ISTA, KSTA
      DOUBLE PRECISION  BMAX, BLEN
      REAL              XYMAX
C ----------------------------------------------------------------------
C     Loop over baselines finding the longest.
C
      BMAX = 0.D0
      DO ISTA = 1, NSTA - 1
         DO KSTA = ISTA + 1, NSTA
            BLEN = ( XPOS(STANUM(ISTA)) - XPOS(STANUM(KSTA)) )**2 +
     1             ( YPOS(STANUM(ISTA)) - YPOS(STANUM(KSTA)) )**2 +
     2             ( ZPOS(STANUM(ISTA)) - ZPOS(STANUM(KSTA)) )**2
            BLEN = SQRT( BLEN )
            BMAX = MAX( BMAX, BLEN )
         END DO
      END DO 
C
C     Add 5 percent and convert to km.
C
      XYMAX = 1.05D0 * BMAX / 1.D3
C
      RETURN
      END
