      SUBROUTINE SCRAMBLE( ARRAY, N, IDUM )
C
C     Scramble the order of ARRAY to element N.
C     Use an element swapping method.
C     Don't worry about occasionally swapping an element
C     with itself.
C
C     Inputs:
C         ARRAY   An integer array
C         N       Number of elements to scramble.
C         IDUM    Seed for the random number generator.
C
      INTEGER      ARRAY(*), N, IDUM, ILOOP
      INTEGER      I1, I2, TEMP
      REAL         RAN5
C -----------------------------------------------------------
      DO ILOOP = 1, 3 * N
         I1 = INT( RAN5( IDUM ) * N + 1 )
         I2 = INT( RAN5( IDUM ) * N + 1 )
         TEMP = ARRAY(I1)
         ARRAY(I1) = ARRAY(I2)
         ARRAY(I2) = TEMP
      END DO
C
      RETURN
      END
