      SUBROUTINE AZELXYNS( AZ, EL, X, Y, RADDEG )
C
C     Routine for sched that converts Az and El to X and Y for
C     checking limits and slews at XYNS mount antennas.  Such
C     an antenna has a fixed N-S axel.
C
C   ***********  I don't really know if this is right.
C      See also AZELXYEW
C
C     Az is clockwise from north looking down.
C     El is El
C     X is clockwise from up looking north (positive to east).
C     Y is clockwise from up looking west when x=0 (positive to north).
C     The fixed axis is the NS axis.
C
      REAL              AZ, EL, X, Y, ELADJ
      DOUBLE PRECISION  AZR,  ELR, XR, YR, DIAG, HT, EST, STH
      DOUBLE PRECISION  RADDEG
C -------------------------------------------------------------------
C     There is a dead zone just off the end of the axis in the
C     north and south direction at the horizon.  To protect against
C     it, do not allow the elevation to be right at the horizon.
C     The change is small and, since no observations should be done
C     in the position anyway, should be benign.
C
      ELADJ = EL
      IF( EL .LT. 0.02 .AND. EL .GE. 0.0 ) ELADJ = 0.02
      IF( EL .GT. -0.02 .AND. EL .LT. 0.0 ) ELADJ = -0.02
C
C     Get the radian equivalents of the angles.
C
      AZR = AZ * RADDEG
      ELR = ELADJ * RADDEG
C
C     We need to use ATAN2 to be sure of getting below horizon stuff
C     right.  This just helps sense out-of-limit conditions.
C
      HT = SIN( ELR )
      STH = COS( ELR ) * COS( AZR )
      EST = COS( ELR ) * SIN( AZR )
      DIAG = HT**2.D0 + EST**2.D0
C
C     Protect against round off problems.
C
      IF( DIAG .LT. 0.D0 ) THEN
         DIAG = 0.D0
      ELSE
         DIAG = SQRT( DIAG )
      END IF
C
C     Now X and Y.
C
      XR = ATAN2( EST, HT )
      YR = ATAN2( STH, DIAG )
      X = XR / RADDEG
      Y = YR / RADDEG
C
      RETURN
      END

