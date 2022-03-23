      SUBROUTINE MTXYEW( KSTA, UP, SAZ, SEL, TX, TY )
C
C     Routine for SCHED that checks if the source is up at an
C     XYEW mount antenna.  It also returns the current position 
C     regardless of where the antenna is requested to point.
C     Adapted from mtxyns.f
C
C     An example of a XYEW antenna is Hobart.
C
      INCLUDE 'sched.inc'
C
      INTEGER      KSTA, IBOX, LOWER, UPPER
      REAL         SAZ, SEL, SX, SY, TX, TY, DIST, RX, RY, DX, DY
      CHARACTER    UP*1
C ---------------------------------------------------------------
C     Complain if limits not given.
C
      IF( NAXLIM(KSTA) .LT. 1 .OR. NAXLIM(KSTA) .GT. 3 ) THEN
         CALL ERRLOG( 'MTXYEW: Need between 1 and 3 sets of axis '//
     1     'limits for '//STATION(KSTA) )
      END IF
C
C     Get the requested X, Y coordinates.
C
      CALL AZELXYEW( SAZ, SEL, SX, SY, RADDEG )
C
C     Find the closest place that the antenna can get to the requested
C     position in terms of the sum of the X and Y offsets.  The offsets
C     will be zero if the source is up.
C
      DIST = 1000.
      DO IBOX = 1, NAXLIM(KSTA)
         LOWER = 2 * IBOX - 1
         UPPER = 2 * IBOX
         RX = MAX( SX, AX1LIM(LOWER,KSTA) )
         RX = MIN( RX, AX1LIM(UPPER,KSTA) )
         RY = MAX( SY, AX2LIM(LOWER,KSTA) )
         RY = MIN( RY, AX2LIM(UPPER,KSTA) )
         DX = ABS( RX - SX )
         DY = ABS( RY - SY )
         IF( DX + DY .LT. DIST ) THEN
            TX = RX
            TY = RY
            DIST = DX + DY
         END IF
      END DO
C
C     Check if up.
C
      IF( TX .EQ. SX .AND. TY .EQ. SY ) THEN
         UP = ' '
      ELSE
         UP = 'D'
      END IF
C
      RETURN
      END

