      SUBROUTINE WBANDFREQ(FREQTOTAL)
C
C     Routine for SCHED called by MAKEFREQ that creates the frequency 
C     settings for the 3mm band. 
C
C
      INCLUDE 'schfreq.inc'
C
C     Temporary values
C
      DOUBLE PRECISION  TMP_FLO1(MFREQ)
      INTEGER           ISET, J, S1, S3, I
      DOUBLE PRECISION  TMP_LO
      INTEGER           FREQTOTAL
      INTEGER           TMP_RF1(MFREQ), TMP_RF2(MFREQ)
      INTEGER           TMP_FREQTOTAL
      CHARACTER         B_LETTER
      DOUBLE PRECISION  DIFF
      CHARACTER         TSYN*10, TLO*10
      DOUBLE PRECISION  SYN1(33), SYN3(12)
      DATA  SYN1 / 11.4, 11.5, 11.6, 11.7, 11.8, 11.9, 12.0, 12.1, 
     1             12.2, 12.3, 12.4, 12.5, 12.6, 12.7, 12.8, 12.9, 
     2             13.0, 13.1, 13.2, 13.3, 13.4, 13.5, 13.6, 13.7, 
     3             13.8, 13.9, 14.0, 14.1, 14.2, 14.3, 14.4, 14.5, 
     4             14.6 /
      DATA  SYN3 / 10.9, 11.1, 11.4, 11.6, 11.9, 12.1, 12.4, 12.6,
     1             12.9, 13.1, 13.4, 13.6 /
C
C     Initialize variables      
      TMP_LO = 79200.0
      TMP_FREQTOTAL = FREQTOTAL + 1
C
C     Calculate LO for 3mm Upper band 
C
      DO ISET = TMP_FREQTOTAL, MFREQ
         IF( TMP_LO  .LE. 95200.0 ) THEN
             TMP_FLO1(ISET) = TMP_LO
             FREQTOTAL = FREQTOTAL + 1
         END IF
         TMP_LO = TMP_LO + 100.0
      END DO
C
C     Calculate RF1 and RF2 values for 3mm Upper band
C
      CALL GETRFX(TMP_FLO1, TMP_FREQTOTAL, FREQTOTAL, TMP_RF1, 
     1            TMP_RF2, 512, 1024)
C
C     Check 3mm rf1 and rf2 for limits;
c     Fill out the rest of the values used for frequency settings
C
      I = TMP_FREQTOTAL - 1
      DO ISET = TMP_FREQTOTAL, FREQTOTAL
C         First iterate through syn1 values
          DO S1 = 1, 33
C             Iterate through syn3 values
              DO S3 = 1, 12
C                 (syn3*6) + syn1 should be the same as lo1/1000
                  WRITE(TSYN, '(F10.3)') (SYN3(S3)*6) + SYN1(S1)
                  WRITE(TLO, '(F10.3)') TMP_FLO1(ISET)/1000 
                  IF( TSYN .EQ. TLO ) THEN
                      I = I + 1
C                     Calculate priority
                      PRIO(I) = 0
C                     Create name
                      IF ( TMP_RF1(ISET) .GT. TMP_FLO1(ISET) ) THEN
                          B_LETTER = 'U'
                      ELSE
                          B_LETTER = 'L'
                      END IF
C                      FSYN(2,ISET) = TMP_FLO1(ISET)/1000.0
                      FSYN(1,I) = SYN1(S1)
                      FSYN(3,I) = SYN3(S3)
                      FRNOTE(I) = ''
                      WRITE( FRNAME(I), '( A, F4.1, A, F4.1, A )' ) 
     1                     "v3mm_", FSYN(1,I), '-', FSYN(3,I),
     2                      B_LETTER
C
C                     50 cm filter.
C
                      FLCP50CM(I) = 'NARROW'
                      FRCP50CM(I) = 'NARROW'
C
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
                              WRITE( FIFNAM(J,I), '(A1)' ) 'B'
                          ELSE IF( J .EQ. 2) THEN
                              WRITE( FIFNAM(J,I), '(A1)' ) 'D'
                          ELSE
                              WRITE( FIFNAM(J,I), '(A2)' ) ''
                          END IF
                      END DO
C                     Set fnif, fe, pol, rf1, rf2, and lo
                      FNIF(I) = 2
                      DO J = 1, FNIF(I)
                          IF( J .EQ. 1) THEN
                              WRITE( FPOL(J,I), '(A3)' ) 'RCP'
                          ELSE
                              WRITE( FPOL(J,I), '(A3)' ) 'LCP'
                          END IF
                          WRITE( FFE(J,I), '(A3)' ) '3mm'
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