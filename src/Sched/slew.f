      SUBROUTINE SLEW( ISCN, LSCN, ISTA )
C
C     This routine calculates the slew distances and time in
C     getting from scan LSCN to the current scan ISCN.  The
C     geometry parameters for the end of scan LSCN and the 
C     beginning of scan ISCN should have been filled with calls
C     to SCHGEO.  Also, WRAP should have been called to set where
C     in the azimuths are in the wrap for ALTAZ antennas for both scans.
C
C     This routine is called from OPTGEO, SCNGEO, SUVOPT, and PLOTSTA.
C
C     Units are degrees for Az, El, X, and Y.  Ha is in hours.  Dec
C     is radians on input, but the DEC4 parameters are degrees.
C     Slew time is in fractional days at the end.
C
C     Note that valid mount types are checked in HORCHK.
C
      INCLUDE 'sched.inc'
C
      INTEGER            ISCN, LSCN, ISTA, KSTA
      REAL               LASTAX1, LASTAX2, CURAX1, CURAX2, LASTAZ
      DOUBLE PRECISION   AX1SLEW, AX2SLEW, DMINSU 
      REAL               TACC1, TACC2, DACC1, DACC2, TSLW1, TSLW2
      REAL               RATE1, RATE2, AX1ADU, AX2ADU
      REAL               LDEC4, DEC4
C --------------------------------------------------------------
      IF( DEBUG .AND. ISCN .LE. 3 ) CALL WLOG( 0, 'SLEW: Starting.' )
      KSTA = STANUM(ISTA)
C
C     Do the calculation for an actual move.
C
C     First get the previous position. 
C     
      IF( LSCN .EQ. 0 ) THEN
C     
C        First scan.  Some nominal position. 
C     
         IF( MOUNT(KSTA) .EQ. 'ALTAZ' ) THEN
            LASTAZ = ( AX1LIM(2,KSTA) - AX1LIM(1,KSTA) ) / 2.0
         ELSE
            LASTAZ = 180.0
         END IF
C     
         LDEC4 = LAT(KSTA) / RADDEG
C     
C        Changed SHA argument (3ed to last) from LDEC4 to 0.0, which
C        seems to make more sense, on 26oct2010.  But the routines
C        that call SLEW either don't do so when LSCN is zero, or don't
C        use the results so it probably doesn't matter.
C     
         CALL ANTPOS( KSTA, ' ', LDEC4,
     1           LASTAZ, 90.0, 0.0, LASTAX1, LASTAX2 )
C     
      ELSE
C     
C        Get position at end of scan LSCN.  Note wrap position in Az
C        should have been established already by routine WRAP.
C     
         LDEC4 = DECP(SRCNUM(LSCN)) / RADDEG
         CALL ANTPOS( KSTA, UP2(LSCN,ISTA), LDEC4,
     1           AZ2(LSCN,ISTA), EL2(LSCN,ISTA), HA2(LSCN,ISTA),
     2           LASTAX1, LASTAX2 )
C     
      END IF
C     
C     Get the current antenna position.
C     
      DEC4 = DECP(SRCNUM(ISCN)) / RADDEG
      CALL ANTPOS( KSTA, UP1(ISCN,ISTA), DEC4,
     1        AZ1(ISCN,ISTA), EL1(ISCN,ISTA), HA1(ISCN,ISTA),
     2        CURAX1, CURAX2 )
C     
C     Get the slew time in minutes.  All rates assumed to be deg/min.
C     Acceleration is assumed to be in deg/s/s.
C     Consider the two cases of when the antenna reaches full speed
C     and when it has to start slowing before reaching full speed.
C     TACC and DACC are the total time and distance spent both
C     accelerating and decelerating.
C     Note that the equation for DACC does not show two factors of
C     2 that cancel, one for both acceleration and deceleration, and
C     the other from D=0.5at^2.
C     Likewise, the equation for TSLEW in the case of not reaching
C     full speed is really T = 2sqrt(2(D/2)/a).
C     TACC1 and TACC2 are in seconds.
C     RATE1 and RATE2 are deg/sec  (trying to use seconds for all).
C     
C     The above was for the original implementation of acceleration.
C     It assumed acceleration and deceleration were the same.  But some
C     antennas, including the DSN, have different rates.
C     
C     First the slew distance.
C     
      AX1SLEW = ABS( CURAX1 - LASTAX1 )
      AX2SLEW = ABS( CURAX2 - LASTAX2 )
C     
C     Then how long does it take and how far does it go to get to
C     full speed.
C     
      RATE1 = AX1RATE(KSTA) / 60.0
      RATE2 = AX2RATE(KSTA) / 60.0
      AX1ADU = 1.0 / AX1ACC(1,KSTA) + 1.0 / AX1ACC(2,KSTA)
      AX2ADU = 1.0 / AX2ACC(1,KSTA) + 1.0 / AX2ACC(2,KSTA)
      TACC1 = RATE1 * AX1ADU
      TACC2 = RATE2 * AX2ADU
      DACC1 = RATE1**2 * AX1ADU * 0.5
      DACC2 = RATE2**2 * AX2ADU * 0.5
C     
C     Then get the actual time, with the method depending on whether
C     full speed was reached.
C     
      IF( AX1SLEW .LT. DACC1 ) THEN
         TSLW1 = SQRT( 2.0 * AX1SLEW * AX1ADU )
      ELSE
         TSLW1 = TACC1 + ( AX1SLEW - DACC1 ) / RATE1
      END IF
      IF( AX2SLEW .LT. DACC2 ) THEN
         TSLW2 = SQRT( 2.0 * AX2SLEW * AX2ADU )
      ELSE
         TSLW2 = TACC2 + ( AX2SLEW - DACC2 ) / RATE2
      END IF
C     
C     Use the largest.
C     
      TSLEW(ISCN,ISTA) = MAX( TSLW1, TSLW2 )
C
C     When there is no source change, there can be confusion.
C     Presumably the slew time is zero, assuming tracking continues.  
C     But when there is a gap between scans, the sky rotates a bit, 
C     and this routine used to come up with a non-zero slew time 
C     (second or two, typically, maybe more if there are GAPs scheduled).
C     That can be pretty confusing in the printouts.  So, set the 
C     actual slew time, before TSETTLE etc, to zero in this case.  But
C     don't always set it to zero.  If a wrap is needed, a long slew
C     may be required and would be reflected in the above calculations.
C     So only set TSLEW to zero if it is already small - implying no wrap.
C
      IF( SRCNUM(LSCN) .EQ. SRCNUM(ISCN) .AND.
     1     TSLEW(ISCN,ISTA) .LT. 20.D0 * ONESEC ) THEN
         TSLEW(ISCN,ISTA) = 0.D0
      END IF
C
C   
C     Add the settling time for the station.  
C
      TSLEW(ISCN,ISTA) = TSLEW(ISCN,ISTA) + TSETTLE(KSTA)
C
C     Make sure that the "slew" time does not drop below the minimum
C     setup time.
C
      DMINSU = MINSETUP(KSTA)
      TSLEW(ISCN,ISTA) = MAX( TSLEW(ISCN,ISTA), DMINSU )
C
C     Convert TSLEW from seconds to fractional days.
C
      TSLEW(ISCN,ISTA) = TSLEW(ISCN,ISTA) / ( 1440.0D0 * 60.D0 )
C
      RETURN
      END
