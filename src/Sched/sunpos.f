      SUBROUTINE SUNPOS( JTIME, RAS, DECS )
Cf2py intent(in) JTIME
Cf2py intent(out) RAS, DECS
C
C     Routine to get the approximate position of the Sun at Julian 
C     time TIME using the SLALIB routines.
C
C     Inputs:   JTIME    Modified Julian Day, including fraction. 
C     Outputs:  RAS      RA of sun in radians.
C               DECS     Dec of sun in radians.
C
C     Called in SCHED by GETSUN.  Also in plot stuff??
C
      INTEGER           YEAR, DAY
      REAL              RSUN(6), FRACTDAY
      DOUBLE PRECISION  JTIME, RAS, DECS, RADDAY, TWOPI
      PARAMETER         (TWOPI=3.141592653589793238D0*2.0D0)
C --------------------------------------------------------------------
C     Convert TIME to the units wanted by SLA_EARTH
C
      CALL TIMEJ( JTIME, YEAR, DAY, RADDAY )
      FRACTDAY = RADDAY / TWOPI
      CALL SLA_EARTH( YEAR, DAY, FRACTDAY, RSUN )
      RAS = ATAN2( -RSUN(2), -RSUN(1) )
      DECS = (-1.)*ATAN2( RSUN(3), SQRT( RSUN(1)**2 + RSUN(2)**2 ) )
C
      RETURN
      END
