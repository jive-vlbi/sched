      SUBROUTINE PLMKRD( R, RADSTR ) 
C
C     Routine for plot that make the circle radii in character format
C     converting from radius in seconds.
C
      CHARACTER        RADSTR*(*), SVAL*20
      INTEGER          VAL, NC, SP
      REAL             R, RAD
C ----------------------------------------------------------------------
C
      RAD = R / 3600.0
      RADSTR = 'Circle radii: '
      SP = 15
C
C     Inner radius
C
      VAL = INT( RAD / 10.0 * 10000 )
      CALL PGNUMB( VAL, -4, 1, SVAL, NC )
C      IF( NC .GT. 8 ) NC = 8
      RADSTR(SP:) = SVAL(1:NC)//' deg; '
      SP = SP + NC + 6
C
C     Middle radius
C
      VAL = INT( RAD / 2.0 * 10000 )
      CALL PGNUMB( VAL, -4, 1, SVAL, NC ) 
C      IF( NC .GT. 8 ) NC = 8
      RADSTR(SP:) = SVAL(1:NC)//' deg; '
      SP = SP + NC + 6
C
C     External radius
C
      VAL = INT( RAD * 10000 )
      CALL PGNUMB( VAL, -4, 1, SVAL, NC ) 
C      IF( NC .GT. 8 ) NC = 8
      RADSTR(SP:) = SVAL(1:NC)//' deg; '
C
      RETURN
      END
