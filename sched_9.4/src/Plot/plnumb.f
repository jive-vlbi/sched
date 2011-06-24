      SUBROUTINE PLNUMB ( VAL, STRVAL )
C
C     Routine that converts a integer number into a decimal character
C     representation for a value greater than 10**8
C
      CHARACTER     STRVAL*14, TMPSTR*14
      INTEGER       VAL
      INTEGER       I, J
C ----------------------------------------------------------------------
C
      STRVAL = ' '
      TMPSTR = ' '
C
      WRITE( TMPSTR, 10 ) VAL
10    FORMAT( I14 )
C
      J = 1
      DO 20 I = 1, 14
         IF( TMPSTR(I:I) .NE. ' ' ) THEN
            STRVAL(J:J) = TMPSTR(I:I)
            J = J + 1
         END IF 
20    END DO
C
      RETURN
      END
