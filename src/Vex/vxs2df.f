      SUBROUTINE VXS2DF( NDATCH, MBITSC, BSAMPL, NGROUP, 
     1    S2MODE, VALID, NS2USD, IS2USD )
C 
C     Routine specific for the VEX extension of SCHED. 
C     Figures out S2 mode name and checks validity
C     This routine sets default mode names, according
C     to a simple naming convention. It assumes the
C     first NDATCH * Bits inputs are used and calls
C     these "in##"
C     By H.J. van Langevelde, JIVE, 250996
C     If NGROUP is set then checked for consitency
C     else set on return.
C 
C 
      INTEGER NDATCH, BSAMPL, NGROUP, NS2USD
      REAL MBITSC
      CHARACTER S2MODE*7, IS2USD(16)*4
      LOGICAL VALID
C
      REAL TOTAL
      INTEGER NTMP, LPOS, I
      INTEGER LEN1
C ----------------------------------------------------------------------
C
      VALID = .TRUE.
      S2MODE = ' '
C
C     S2 user channels must be 1, 2, 4, 8, 16
C
      TOTAL = NDATCH * BSAMPL
      IF( TOTAL .NE. 1.0 .AND.
     1    TOTAL .NE. 2.0 .AND.
     1    TOTAL .NE. 4.0 .AND.
     1    TOTAL .NE. 8.0 .AND.
     1    TOTAL .NE. 16.0 ) VALID = .FALSE.
C
C     total rate must be 16, 32, 48, 64, 80, 96, 112, 128
C
      TOTAL = NDATCH * MBITSC * BSAMPL
      TOTAL = TOTAL / 16.0 
      IF( TOTAL .NE. 1.0 .AND.
     1    TOTAL .NE. 2.0 .AND.
     1    TOTAL .NE. 3.0 .AND.
     1    TOTAL .NE. 4.0 .AND.
     1    TOTAL .NE. 5.0 .AND.
     1    TOTAL .NE. 6.0 .AND.
     1    TOTAL .NE. 7.0 .AND.
     1    TOTAL .NE. 8.0  ) VALID = .FALSE.
C
C     Figure NGROUP
C
      IF( VALID ) THEN
         TOTAL = NDATCH * MBITSC * BSAMPL
         NTMP = INT( 128. / TOTAL )
         IF( NGROUP .NE. 0 ) THEN
            IF( NGROUP .NE. NTMP ) VALID = .FALSE.
         ELSE
            NGROUP = NTMP
         END IF
      END IF
C
C     Give it a name
C
      IF( VALID ) THEN
         LPOS = LEN1(S2MODE)+1
         IF( MBITSC .LT. 10 ) THEN
            WRITE( S2MODE(LPOS:LPOS), '( I1 )' ) INT( MBITSC )
         ELSE
            WRITE( S2MODE(LPOS:LPOS+1), '( I2 )' ) INT( MBITSC )
         END IF
         LPOS = LEN1(S2MODE)+1
         WRITE( S2MODE(LPOS:LPOS), '( A1 )' ) 'x'
         LPOS = LEN1(S2MODE)+1
         IF( (NDATCH * BSAMPL) .LT. 10 ) THEN
            WRITE( S2MODE(LPOS:LPOS), '( I1 )' ) NDATCH * BSAMPL
         ELSE
            WRITE( S2MODE(LPOS:LPOS+1), '( I2 )' ) NDATCH * BSAMPL
         END IF
         LPOS = LEN1(S2MODE)+1
         WRITE( S2MODE(LPOS:LPOS), '( A1 )' ) '-'
         LPOS = LEN1(S2MODE)+1
         WRITE( S2MODE(LPOS:LPOS), '( I1 )' ) BSAMPL
      END IF
C
C     Number of used channels is 
C
      NS2USD = NDATCH * BSAMPL
C
C     Nothing much important in S2USD write simple cable names
C
      DO I = 1, NS2USD
         WRITE(IS2USD(I), '( A2, I2.2 )' ) 'in', I-1
      END DO
C
      RETURN
      END
