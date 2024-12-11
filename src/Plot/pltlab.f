      SUBROUTINE PLTLAB( TYPE, DAY0, OFFSET, LABEL )
C
C     Routine for SCHED called by PLOTDEF to set X axis label for
C     time types
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
      DOUBLE PRECISION  DDAY, TIME, DAY0
      CHARACTER         TYPE*(*), LABEL*(*), DNAME*3, MNAME*3, COF*3
      INTEGER           OFFSET, YEAR, DAY, JD, MONTH, LS, LEN1
      INTEGER           TOF, I, J
C ----------------------------------------------------------------------
C
C     Set the UT Date
C
      TOF = OFFSET
      IF( TYPE .EQ. 'LST' ) TOF = 0
C
      DDAY = DINT( DAY0 + ( 3600.D0 * TOF / 86400.D0 ) ) 
      CALL TIMEJ( DDAY, YEAR, DAY, TIME )
      MONTH = 1
      CALL TDATECW( YEAR, MONTH, DAY, JD, MNAME, DNAME )
C
C     Set the Label
C
      IF( TYPE .EQ. 'UT' ) THEN
         WRITE( LABEL, '( A, A3, I4, 1X, A3, I5 )' )
     1         'UT starting on ',  DNAME, DAY, MNAME, YEAR
C
      ELSE IF( TYPE .EQ. 'LT' ) THEN
         CALL PGNUMB( OFFSET, 0, 1, COF, I )
         LABEL = 'Local Time ( UT +'
         J = 18
         IF ( POFVAL(1) .LT. 0 ) J = 17
         LABEL(J:) = COF(1:I)//' hr )'
         J = J + I
         WRITE( LABEL(J:), '( A, A3, I4, 1X, A3, I5 )' )
     1         ' hr ) starting on ',  DNAME, DAY, MNAME, YEAR
C
      ELSE IF( TYPE .EQ. 'GST' ) THEN
         IF( PXYSUN ) THEN
            WRITE( LABEL, '( A, A3, I4, 1X, A3, I5, A )' )
     1         'GST ( starting on UT ',  DNAME, DAY, MNAME, YEAR, ' )'
         ELSE
            LABEL = 'GST'
         END IF
C
      ELSE IF( TYPE .EQ. 'LST' ) THEN
         IF( OFFSET .NE. 0 ) THEN
            LS = LEN1( STANAME(OFFSET) )
            LABEL = 'LST - '//STANAME(OFFSET)(1:LS)
         ELSE
            LABEL = 'LST = GST'
         END IF
         IF( PXYSUN ) THEN
            LS = LEN1( LABEL ) + 1
            WRITE( LABEL(LS:), '( A, A3, I4, 1X, A3, I5, A )' )
     1         ' ( starting on UT ',  DNAME, DAY, MNAME, YEAR, ' )'
         END IF
C
      ELSE IF( TYPE .EQ. 'RADEC' ) THEN
         LS = LEN1( LABEL ) + 1
         WRITE( LABEL(LS:), '( A, A3, I4, 1X, A3, I5, A )' )
     1         ' ( starting on UT ', DNAME, DAY, MNAME, YEAR, ' )'
C
      END IF
C
      RETURN
      END
