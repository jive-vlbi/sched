      SUBROUTINE TIMEJ( JTIME, YEAR, DAY, TIME )
Cf2py intent(in) JTIME
Cf2py intent(out) TIME
Cf2py intent(out) YEAR
Cf2py intent(out) DAY
C
C     Routine for SCHED that takes the Modified Julian Date and returns
C     the year, day of year, and time of day.  JTIME is in fractional
C     days.  TIME is in radians.
C
      DOUBLE PRECISION   JTIME, TIME, RADDAY, FD
      PARAMETER          (RADDAY = 2.D0 * 3.141592653589793238D0)
      INTEGER            YEAR, DAY, JM, JD, NY, J
      CHARACTER*60       MSGTXT
      CHARACTER*10       TFORM, CTIME
C --------------------------------------------------------------------
      CALL SLA_DJCL( JTIME, YEAR, JM, JD, FD, J )
      IF( J.NE.0 )  CALL ERRLOG( ' Time out of range for SLA_DJCL' )
      TIME = FD * RADDAY
      CALL SLA_CALYD( YEAR, JM, JD, NY, DAY, J )
      IF( J.NE.0 ) THEN
         CTIME = TFORM( TIME, 'T', 0, 2, 2, '::@' )
         WRITE( MSGTXT, '( A, A, 3I4, A )' ) ' Bad time in TIMEJ -',
     1       ' y,m,d,h: ', YEAR, JM, JD, CTIME
         CALL ERRLOG( MSGTXT )
      END IF
C
      RETURN
      END
