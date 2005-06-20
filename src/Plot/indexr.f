      INTEGER FUNCTION INDEXR( STRING, CHAR )
C
C     The Function return the last occourence of SUBSTR
C     in STRING, 0 otherwise
C
      CHARACTER*(*)     STRING, CHAR
      INTEGER           I, STLEN
C ----------------------------------------------------------------------
C
      STLEN  = LEN( STRING )
      INDEXR = 0
C
      DO 10 I = 1, STLEN
         IF( STRING(I:I) .EQ. CHAR ) INDEXR = I
 10   CONTINUE
C
      RETURN
      END
