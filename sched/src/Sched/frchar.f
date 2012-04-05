      SUBROUTINE FRCHAR( FRIN, NPER, NFC1, NFCN, FRTXT )
C
C     Utility formatting routine that takes in a double precision
C     number FRIN and creats a text string (FRTXT) containing that 
C     number with the decimal point in position NPER (input) with
C     the first number in position NFC1 (output) and the last non-zero 
C     number in position NFCN (output).
C
C     The primary intended use is to deal with displaying frequencies
C     with more digits than we are used to displaying (.01 MHz) but
C     only when they are needed.
C
C     Do enough digits that the calling routine can sense non-integer
C     values (at least in most cases) if needed.
C
      DOUBLE PRECISION   FRIN
      INTEGER            NPER, NFC1, NFCN, IC
      CHARACTER          FRTXT*16, FMTF*30
C -------------------------------------------------------------------
C
C     Derive the format and write the number to the character string.
C
      IF( NPER .GT. 6 ) THEN
         WRITE( FMTF, '( A6, I1, A2 )' ) 
     1         '( F16.', 16 - NPER, ' )'
      ELSE
         WRITE( FMTF, '( A6, I2, A2 )' ) 
     1         '( F16.', 16 - NPER, ' )'
      END IF
C
C     Write the output string.
C
      WRITE( FRTXT, FMTF ) FRIN
C
C     Get the first digit position and the last non-zero digit position
C     for digits beyond the decimal.  At the decimal if all zero.
C     Set up to print 2 digits (the usual in the past) regardless.
C 
      NFC1 = NPER
      NFCN = NPER + 2
      DO IC = NPER, 1, -1
         IF( FRTXT(IC:IC) .NE. ' ' ) NFC1 = IC
      END DO
      DO IC = NPER+3, 16
         IF( FRTXT(IC:IC) .NE. '0' ) NFCN = IC
      END DO
C
      RETURN
      END
