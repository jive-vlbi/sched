      SUBROUTINE PLMKCO( COOR, COSTR, TYPE, SP ) 
C
C     Routine for plot that make the coordinate in character format
C     converting from coordinate in seconds.
C
      CHARACTER        COSTR*(*), SVAL*10, SIGN
      INTEGER          TYPE, VAL, NC, SP
      REAL             COOR, COSEC
C ----------------------------------------------------------------------
C
C     Set the Sign of coordinate
C
      
      SIGN = ' ' 
      IF( COOR .LT. 0 ) SIGN = '-'
      COSTR = SIGN
      COSEC = ABS( COOR )
      SP = 2
C
C     Set the Hours or Degrees
C
      VAL = INT( COSEC / 3600.D0 )
      CALL PGNUMB( VAL, 0, 1, SVAL, NC ) 
      COSTR(SP:) = SVAL(1:NC)
      SP = SP + NC
      IF( TYPE .EQ. 0 ) THEN
         COSTR(SP:SP) = 'h'
      ELSE 
         COSTR(SP:SP) = 'd'
      END IF
      SP = SP + 1
C
C     Set the Minutes and Seconds
C
      COSEC = MOD( COSEC, 3600.0 )
      IF( COSEC .GT. 0.0 ) THEN
C
C        Minutes
C
         VAL = INT( COSEC / 60.D0 )
         CALL PGNUMB( VAL, 0, 1, SVAL, NC ) 
         COSTR(SP:) = SVAL(1:NC)
         SP = SP + NC
         COSTR(SP:SP) = 'm'
         SP = SP + 1
C
C        Seconds
C
         VAL = INT( MOD( COSEC, 60.0 ) * 10.0 )
         CALL PGNUMB( VAL, -1, 1, SVAL, NC ) 
         NC = INDEX( SVAL, '.' ) + 1
         IF( NC .EQ. 1 ) THEN
            SVAL(3:4) = '.0'
            NC = 4
         END IF
         COSTR(SP:) = SVAL(1:NC)
         SP = SP + NC
         COSTR(SP:SP) = 's'
      ELSE
         COSTR(SP:) = '00m00.0s'
         SP = SP + 5
      END IF
C
      RETURN
      END
