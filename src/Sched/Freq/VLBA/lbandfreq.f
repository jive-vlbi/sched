      SUBROUTINE LBANDFREQ(FREQTOTAL)
C
C     Routine for SCHED called by MAKEFREQ that creates the frequency 
C     settings for the 20cm band. 
C
C
      INCLUDE 'schfreq.inc'
C
C     Temporary values
C
      DOUBLE PRECISION  TMP_FLO1(MFREQ)
      INTEGER           ISET, J
      DOUBLE PRECISION  TMP_LO
      INTEGER           FREQTOTAL, TMP_FREQTOTAL
      INTEGER           TMP_RF1(MFREQ), TMP_RF2(MFREQ)
C
C     Initialize variables
      TMP_LO = 2000.0
      TMP_FREQTOTAL = FREQTOTAL + 1
C      FREQTOTAL = 0
C
C     Calculate LO for 20cm
C
      DO ISET = TMP_FREQTOTAL, MFREQ
         IF( TMP_LO  .LE. 2600.0 ) THEN
             TMP_FLO1(ISET) = TMP_LO
             FREQTOTAL = FREQTOTAL + 1
         END IF
         TMP_LO = TMP_LO + 100.0
      END DO
C
C     Calculate RF1 and RF2 values for 20cm
C
      CALL GETRFX(TMP_FLO1, TMP_FREQTOTAL, FREQTOTAL, TMP_RF1, 
     1            TMP_RF2, -1024, -512)
C
C     Check 20cm rf1 and rf2 for limits;
c     Fill out the rest of the values used for frequency settings
C
      DO ISET = TMP_FREQTOTAL, FREQTOTAL
C         Limits for rf1 and rf2
          IF ( TMP_RF1(ISET) .LT. 1180 ) THEN
              TMP_RF1(ISET) = 1180
          END IF
          IF ( TMP_RF2(ISET) .GT. 1850 ) THEN
              TMP_RF2(ISET) = 1850
          END IF
C         Set priority
          IF( ISET .EQ. 1 ) THEN
             PRIO(ISET) = 1
          ELSE
             PRIO(ISET) = 0
          END IF
C         Create name
          FSYN(2,ISET) = TMP_FLO1(ISET)/1000.0
          WRITE( FRNAME(ISET), '(A, F3.1, A)' ) "v20cm_", 
     1       FSYN(2,ISET), "L"
          FRNOTE(ISET) = ''
C         Set station names
          DO J = 1, MFSTA
              IF( J .EQ. 1 ) THEN
                  WRITE( FSTNAM(J,ISET), '(A8)' ) 'VLBA    '
              ELSE
                  WRITE( FSTNAM(J,ISET), '(A8)' ) ''
              END IF
          END DO
C         Set ifnam
          DO J = 1, MFIF
              IF( J .EQ. 1) THEN
                  WRITE( FIFNAM(J,ISET), '(A1)' ) 'A'
              ELSE IF( J .EQ. 2) THEN
                  WRITE( FIFNAM(J,ISET), '(A1)' ) 'C'
              ELSE
                  WRITE( FIFNAM(J,ISET), '(A2)' ) ''
              END IF
          END DO
C         Set fnif, fe, pol, rf1, rf2, and lo
          FNIF(ISET) = 2
          DO J = 1, FNIF(ISET)
              IF( J .EQ. 1) THEN
                  WRITE( FPOL(J,ISET), '(A3)' ) 'RCP'
              ELSE
                  WRITE( FPOL(J,ISET), '(A3)' ) 'LCP'
              END IF
              WRITE( FFE(J,ISET), '(A4)' ) '20cm'
              FRF1(J,ISET) = TMP_RF1(ISET)
              FRF2(J,ISET) = TMP_RF2(ISET)
              FLO1(J,ISET) = TMP_FLO1(ISET)
          END DO
C
C        For some reason the original setup of frequencies have 50cm for all
C        50 cm filter.
C
         FLCP50CM(ISET) = 'NARROW'
         FRCP50CM(ISET) = 'NARROW'
C
      END DO
C
      END