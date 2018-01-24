      SUBROUTINE SIDTIM( JULTIM, LONG, TWOPI, SIDDAY, LST, LSTDAY )
C
C     Routine for SCHED for calculating LST (or GMST).
C
C     Input: 
C        JULTIM      Julian time (eg, STARTJ or STOPJ in SCHED).
C        LONG        Longitude.  Give station longitude for LST. 
C                                Give 0.D0 for GMST.
C                    In SCHED, use LONG(STANUM(ISTA)).
C        TWOPI       ~2.D0 * 3.14159....  (SCHED keeps it in common).
C
C     Output:
C        SIDDAY      LST (GMST) day number as required for VLA 
C                    schedules.  Zero point set to match VLA schedules.
C        LST         The local sidereal time in radians (modulo 2pi).
C        LSTDAY      The LST day plus fractional day with the zero
C                    point the same as for SIDDAY.  This may be 
C                    useful for plotting.
C
C     The code closely follows SLA_GMST, but retains the full day
C     numbers.
C
      DOUBLE PRECISION  JULTIM, LONG, TWOPI, LST, LSTDAY
      INTEGER           SIDDAY
Cf2py intent(out) SIDDAY, LST, LSTDAY
      DOUBLE PRECISION  TU, S2R, GMSTD
C --------------------------------------------------------------------
C      call wlog( 0, 'sidtim starting' )
C
      S2R = TWOPI / 86400.D0
C     
C     Julian centuries from fundamental epoch J2000 to this UT
C     Note that a Julian century is defined as 36525 days rather than
C     the more realistic 365.242198781.  The Julian century is what
C     is used in the time axis for these calculations, as I understand
C     it.
C     
      TU = (JULTIM-51544.5D0)/36525.D0
C     
C     GMST at this UT.
C     
      GMSTD = JULTIM * TWOPI + ( 24110.54841D0 + (8640184.812866D0
     1    + ( 0.093104D0 - 6.2D-6 * TU ) * TU ) * TU ) * S2R 
C
C     Get the local sidereal time including a day.  I'm not sure what
C     the meaning of the zero is at this point.
C
      LSTDAY =  6713.D0 + ( GMSTD - LONG ) / TWOPI
C
C     Get the local sidereal day number as for VLA schedules.
C     Is this a well defined quantity?  Anyway, it matches the 
C     sidereal day numbers needed VLA schedules.  The constants
C     involved were determined by matching a specific schedule 
C     from OBSERV.
C
      SIDDAY = DINT( LSTDAY )
C
C     Get the LST modulo 24 hours (still radians).
C
      LST  = DMOD( LSTDAY, 1.D0 ) * TWOPI
C
      RETURN
      END

