      SUBROUTINE SCHDAY( JTIME, VLBAD, HUMAND, DOY, TIME )
C
C     Routine to take Julian time (STARTJ or STOPJ)
C     and convert to format needed for DATE parameter for VLBA and 
C     to a human readable format of the form:  Tue, 1988 Mar 12
C     Also returns the day of year and the fraction of the day.  The
C     fraction needs to be converted to radians before most uses.
C
      DOUBLE PRECISION       JTIME, TIME 
      CHARACTER    VLBAD*9, HUMAND*16, MNAME*3, DNAME*3
      INTEGER      KY, KM, KD, J, DOY, JY, JD
C -----------------------------------------------------------------------
      CALL SLA_DJCL( JTIME, KY, KM, KD, TIME, J )
      IF( J .EQ. -1 ) CALL ERRLOG( 'Unacceptable date in SCHDAY' )
C
      CALL TDATECW(KY,KM,KD,JD,MNAME,DNAME)
C
      WRITE( VLBAD, '(I4.4,A3,I2.2)' ) KY, MNAME, KD
C
      WRITE( HUMAND, '(A3,'', '',I4,1X,A3,1X,I2.2 )' ) DNAME, 
     1               KY, MNAME, KD
C
      CALL SLA_CALYD( KY, KM, KD, JY, DOY, J )
      IF( J .GE. 1 ) CALL ERRLOG( 'Date problem in SCHDAY' )
C
      RETURN
      END
