      SUBROUTINE QBANDFREQ(FREQTOTAL)
C
C     Routine for SCHED called by MAKEFREQ that creates the frequency 
C     settings for the 7mm band. 
C
C
      INCLUDE 'schfreq.inc'
C
C     Temporary values
C
      DOUBLE PRECISION  TMP_FLO1(MFREQ)
      INTEGER           TMP_PRIO(MFREQ)
      INTEGER           ISET, J, S2, S3, I
      DOUBLE PRECISION  TMP_LO
      INTEGER           FREQTOTAL
      INTEGER           TMP_RF1(MFREQ), TMP_RF2(MFREQ)
      INTEGER           TMP_FREQTOTAL, TMP_FREQTOTAL2
      CHARACTER         B_LETTER
      DOUBLE PRECISION  DIFF
      CHARACTER         TSYN*10, TLO*10
      DOUBLE PRECISION  SYN2(21), SYN3(12)
      DATA  SYN2 / 7.1, 7.2, 7.3, 7.4, 7.5, 7.6, 7.7, 7.8, 7.9, 8.0, 
     1             8.1, 9.0, 9.1, 9.2, 9.3, 9.4, 9.5, 9.6, 9.7, 9.8, 
     2             9.9 /
      DATA  SYN3 / 9.9, 10.1, 10.4, 10.6, 10.9, 11.1, 11.4, 11.6, 
     1             11.9, 12.1, 12.4, 12.6 /
C
C     Initialize variables
      TMP_LO = 37000.0
      TMP_FREQTOTAL = FREQTOTAL + 1
C
C     Calculate LO for 7mm Upper band 
C
      DO ISET = TMP_FREQTOTAL, MFREQ
         IF( TMP_LO  .LE. 45400.0 ) THEN
             TMP_FLO1(ISET) = TMP_LO
             FREQTOTAL = FREQTOTAL + 1
         END IF
         TMP_LO = TMP_LO + 100.0
      END DO
C
C     Calculate RF1 and RF2 values for 7mm Upper band
C
      CALL GETRFX(TMP_FLO1, TMP_FREQTOTAL, FREQTOTAL, TMP_RF1, 
     1            TMP_RF2, 512, 1024)
C
C     Calculate priority for Upper band
C
      DO ISET = TMP_FREQTOTAL, FREQTOTAL
          TMP_LO = TMP_RF1(ISET) - 512
          IF( TMP_LO .LT. 39200 .OR. TMP_LO .GE. 44600 ) THEN
              TMP_PRIO(ISET) = 1
          ELSE
              TMP_PRIO(ISET) = 0
          END IF
      END DO
C
C     Calculate LO for 7mm lower band 
C
      TMP_FREQTOTAL2 = FREQTOTAL + 1
      TMP_LO = 39700
      DO ISET = TMP_FREQTOTAL2, MFREQ
         IF( TMP_LO  .LE. 46800.0 ) THEN
             TMP_FLO1(ISET) = TMP_LO
             FREQTOTAL = FREQTOTAL + 1
         END IF
         TMP_LO = TMP_LO + 100.0
      END DO
C
C     Calculate RF1 and RF2 values for 7mm lower band
C
      CALL GETRFX(TMP_FLO1, TMP_FREQTOTAL2, FREQTOTAL, TMP_RF1, 
     1            TMP_RF2, -1024, -512)
C
C     Calculate priority for lower band
C
      DO ISET = TMP_FREQTOTAL2, FREQTOTAL
          IF( TMP_RF1(ISET) .EQ. 41676) THEN
              TMP_PRIO(ISET) = 4
          ELSE
              TMP_PRIO(ISET) = 2
          END IF
      END DO
C
C     Check 7mm rf1 and rf2 for limits;
c     Create name, priority, 'station'
C
      I = TMP_FREQTOTAL - 1
      DO ISET = TMP_FREQTOTAL, FREQTOTAL
C         First iterate through syn1 values
          DO S2 = 1, 21
C             Iterate through syn3 values
              DO S3 = 1, 12
C                 (syn3 *3) + syn2 should be the same as lo1/1000
                  WRITE(TSYN, '(F10.3)') (SYN3(S3)*3) + SYN2(S2)
                  WRITE(TLO, '(F10.3)') TMP_FLO1(ISET)/1000 
                  IF( TSYN .EQ. TLO ) THEN
                      I = I + 1
C                     Correct priority index
                      PRIO(I) = TMP_PRIO(ISET)
C                     Create name
                      IF ( TMP_RF1(ISET) .GT. TMP_FLO1(ISET) ) THEN
                          B_LETTER = 'U'
                      ELSE
                          B_LETTER = 'L'
                      END IF
C                      FSYN(2,ISET) = TMP_FLO1(ISET)/1000.0
                      FSYN(2,I) = SYN2(S2)
                      FSYN(3,I) = SYN3(S3)
                      FRNOTE(I) = ''
                      WRITE( FRNAME(I), '( A, F3.1, A, F4.1, A )' ) 
     1                     "v7mm_", FSYN(2,I), '-', FSYN(3,I),
     2                      B_LETTER
C
C                     50 cm filter.
C
                      FLCP50CM(ISET) = 'NARROW'
                      FRCP50CM(ISET) = 'NARROW'
C                     Set stations
                      DO J = 1, MFSTA
                          IF( J .EQ. 1 ) THEN
                              WRITE( FSTNAM(J,I), '(A8)') 'VLBA    '
                          ELSE
                              WRITE( FSTNAM(J,I), '(A8)') ''
                          END IF
                      END DO
C                     Set ifnam
                      DO J = 1, MFIF
                          IF( J .EQ. 1) THEN
                              WRITE( FIFNAM(J,I), '(A1)' ) 'A'
                          ELSE IF( J .EQ. 2) THEN
                              WRITE( FIFNAM(J,I), '(A1)' ) 'C'
                          ELSE
                              WRITE( FIFNAM(J,I), '(A2)' ) ''
                          END IF
                      END DO
C                     Set fnif, fe, pol, rf1, rf2, and lo
                      FNIF(ISET) = 2
                      DO J = 1, FNIF(ISET)
                          IF( J .EQ. 1) THEN
                              WRITE( FPOL(J,I), '(A3)' ) 'RCP'
                          ELSE
                              WRITE( FPOL(J,I), '(A3)' ) 'LCP'
                          END IF
                          WRITE( FFE(J,I), '(A3)' ) '7mm'
                          FRF1(J,I) = TMP_RF1(ISET)
                          FRF2(J,I) = TMP_RF2(ISET)
                          FLO1(J,I) = TMP_FLO1(ISET)
                      END DO
                  END IF
              END DO
          END DO
      END DO
      FREQTOTAL = I
C
      END