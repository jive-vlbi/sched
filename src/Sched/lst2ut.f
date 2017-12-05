      SUBROUTINE LST2UT( LONG, SIDDAY, LSTTIM, YEAR, DAY, UT )
C
C     A routine to calculate YEAR, DAY, and UT from Modified Local 
C     Sidereal Day (The day number on VLA schedules) and LSTtim for
C     a station at longitude LONG.  It uses the equations of SLA_GMST
C     but goes backwards from that routine using an iterative 
C     proceedure.  LSTtim and UT should be in fractional days.
C
      INTEGER            SIDDAY, YEAR, DAY, ITER
Cf2py intent(out) YEAR
Cf2py intent(out) DAY
      DOUBLE PRECISION   LONG, LSTTIM, UT, GMST, GMSTD, JULTIM
Cf2py intent(out) UT
      DOUBLE PRECISION   TU, GMSTDN, JULNEW, TWOPI, S2R
      PARAMETER          (TWOPI=6.283185307179586476925287D0)
      PARAMETER          (S2R=0.7272205216643039849D-4)
C ----------------------------------------------------------------------
C     Get first guess of Julian time.  This just uses a low order
C     version of the equation from SLA_GMST.  The sidereal day offset
C     (6713) is the difference between the number of julian days and
C     sidereal days from the beginning of the julian calandar to the
C     year 2000.  Actually I got the number by requiring agreement 
C     of the results with the VLA schedule.
C       R. C. Walker.
C
      GMST = LSTTIM * TWOPI + LONG
      GMSTD = ( SIDDAY - 6713) * TWOPI + GMST 
      JULTIM = ( GMSTD - 24110.54841D0 * S2R ) / TWOPI
      ITER = 0
C
C     Iterate to the julian time using the equation for sidereal time
C     from SLA_GMST.  There must be a direct equation but I don't have
C     it.  The Julian century of 36525 days exactly is what is wanted
C     for the time axis, not something closer to reality (36524....)
C
100      CONTINUE
         TU = (JULTIM - 51544.5D0) / 36525D0
         GMSTDN = JULTIM * TWOPI + ( 24110.54841D0 + (8640184.812866D0
     1       + ( 0.093104D0 - 6.2D-6 * TU ) * TU ) * TU ) * S2R 
         JULNEW = JULTIM + (GMSTD-GMSTDN)/TWOPI
         IF( ABS(JULNEW-JULTIM) .LT. 1.D-6 ) THEN
            JULTIM = JULNEW
            GO TO 200
         ELSE
            JULTIM = JULNEW
            ITER = ITER+1
            GO TO 100
         END IF
200   CONTINUE
C
C     Get year, day of year, and UT (rad).
C
      CALL TIMEJ( JULTIM, YEAR, DAY, UT )
C
      UT = UT / TWOPI
C
      RETURN
      END
