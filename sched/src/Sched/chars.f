      SUBROUTINE CHARS( STRING, I1, I2, J1, J2 )
C
C     In STRING(I1:I2), determine the first (J1) and last (J2)
C     non-blank character.
C
      CHARACTER   STRING*(*)
      INTEGER     I1, I2, J1, J2, I, J
C ------------------------------------------------------------------
      J1 = 0
      DO I = I1, I2
         J = I2 + I1 - I
         IF( STRING(J:J) .NE. ' ' ) J1 = J
         IF( STRING(I:I) .NE. ' ' ) J2 = I
      END DO
C
      RETURN
      END
