      SUBROUTINE SHIFT( TEXT, ML, L1, LN, NS )
C
C     Shift lines L1 to LN in TEXT by NS lines.  Vacated lines will
C     be left blank.  Be careful about overwrites.
C
C     LN will be assumed to be the total number of lines used in TEXT
C     and will be adjusted accordingly by NS regardless of whether 
C     any lines were actually moved.
C
C     If L1 is less then LN, nothing will be moved, but LN will be
C     adjusted (presumably a line was meant to be vacated or added)
C
      INTEGER       ML
      CHARACTER     TEXT(ML)*120
      INTEGER       L1, LN, NS, JL
C ---------------------------------------------------------------------
C
      IF( LN + NS .GT. ML .OR. L1 + NS .LT. 1 ) THEN
C
C        Shift request will put lines outside the array.
C
         WRITE(*,*) 'SHIFT: Invalid shift request.', ML, L1, LN, NS
         STOP
      ELSE IF( LN .GE. L1 ) THEN
         IF( NS .GT. 0 ) THEN
C
C           Shift lines forward and clear vacated ones at start.
C
            DO JL = LN, L1, -1
               TEXT(JL+NS) = TEXT(JL)
            END DO
            DO JL = L1, L1 + NS - 1
               TEXT(JL) = ' '
            END DO
         ELSE IF( NS .LT. 0 ) THEN
C
C           Shift lines backwards and clear ones at end.
C
            DO JL = L1, LN
               TEXT(JL+NS) = TEXT(JL)
            END DO
            DO JL = LN + NS + 1, LN
               TEXT(JL) = ' '
            END DO
         END IF
      END IF
C
C     Adjust the range of used lines, assuming LN is the last.
C     This is done even if there were no lines to shift on the
C     assumption that, after this call, the length of the file
C     should have changed - for example, to make room for some
C     new lines.
C
      LN = LN + NS
C
      RETURN
      END


