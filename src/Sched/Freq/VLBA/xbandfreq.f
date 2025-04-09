      SUBROUTINE XBANDFREQ(FREQTOTAL)
C
C
C     Routine for SCHED called by MAKEFREQ that creates the frequency 
C     settings for the 4cm band. 
C
C
      INCLUDE 'schfreq.inc'
C
C     Temporary values
C
      DOUBLE PRECISION  TMP_FLO1(MFREQ)
      INTEGER           ISET, J
      DOUBLE PRECISION  TMP_LO
      INTEGER           FREQTOTAL
      INTEGER           TMP_RF1(MFREQ), TMP_RF2(MFREQ)
      INTEGER           TMP_FREQTOTAL
      INTEGER           TMP_FREQTOTAL2
      CHARACTER         B_LETTER
C
C     Initialize variables
      TMP_LO = 7100.0
      TMP_FREQTOTAL = FREQTOTAL + 1
C
C     Calculate LO for 4 cm Upper band 
C
      DO ISET = TMP_FREQTOTAL, MFREQ
         IF( TMP_LO  .LE. 7900.0 ) THEN
             TMP_FLO1(ISET) = TMP_LO
             FREQTOTAL = FREQTOTAL + 1
         END IF
         TMP_LO = TMP_LO + 100.0
      END DO
C
C     Calculate RF1 and RF2 values for 4cm Upper band
C
      CALL GETRFX(TMP_FLO1, TMP_FREQTOTAL, FREQTOTAL, TMP_RF1, 
     1            TMP_RF2, 512, 1024)

C
C     Calculate priority for upper band
C
      DO ISET = TMP_FREQTOTAL, FREQTOTAL
          IF( TMP_RF1(ISET) .LT. 7700 .OR. 
     1                       TMP_RF1(ISET) .GT. 8900 ) THEN
              PRIO(ISET) = 1
          ELSE IF ( TMP_FLO1(ISET) == 7900 ) THEN
              PRIO(ISET) = 2
          ELSE
              PRIO(ISET) = 0
          END IF
      END DO
C
C     Calculate LO for 4 cm Lower band 
C
      TMP_FREQTOTAL2 = FREQTOTAL + 1
      TMP_LO = 9000
      DO ISET = TMP_FREQTOTAL2, MFREQ
         IF( TMP_LO  .LE. 9600.0 ) THEN
             TMP_FLO1(ISET) = TMP_LO
             FREQTOTAL = FREQTOTAL + 1
         END IF
         TMP_LO = TMP_LO + 100.0
      END DO
C
C     Calculate RF1 and RF2 values for 4cm Lower band
C
      CALL GETRFX(TMP_FLO1, TMP_FREQTOTAL2, FREQTOTAL, TMP_RF1, 
     1            TMP_RF2, -1024, -512)

C
C     Calculate priority for Lower band
C
      DO ISET = TMP_FREQTOTAL2, FREQTOTAL
          IF( TMP_RF1(ISET) .LT. 8100 .OR. 
     1                       TMP_RF1(ISET) .GT. 9050 ) THEN
              PRIO(ISET) = 1
          ELSE
              PRIO(ISET) = 0
          END IF
      END DO
C
C     Check 4cm rf1 and rf2 for limits;
c     Fill out the rest of the values used for frequency settings
C
      DO ISET = TMP_FREQTOTAL, FREQTOTAL
C         Limits for rf1 and rf2
          IF ( TMP_RF1(ISET) .LT. 7700 ) THEN
              TMP_RF1(ISET) = 7700
          END IF
          IF ( TMP_RF2(ISET) .GT. 9050 ) THEN
              TMP_RF2(ISET) = 9050
          END IF
C         Create name
          IF ( TMP_RF1(ISET) .GT. TMP_FLO1(ISET) ) THEN
              B_LETTER = 'U'
          ELSE
              B_LETTER = 'L'
          END IF
          FSYN(1,ISET) = TMP_FLO1(ISET)/1000.0
          WRITE( FRNAME(ISET), '(A, F3.1, A)' ) "v4cm_", 
     1       FSYN(1,ISET), B_LETTER
C         Set station names
          DO J = 1, MFSTA
              IF( J .EQ. 1 ) THEN
                  WRITE( FSTNAM(J,ISET), '(A8)') 'VLBA    '
              ELSE
                  WRITE( FSTNAM(J,ISET), '(A8)') ''
              END IF
          END DO
C         Set ifnam
          DO J = 1, MFIF
              IF( J .EQ. 1) THEN
                  WRITE( FIFNAM(J,ISET), '(A1)' ) 'B'
              ELSE IF( J .EQ. 2) THEN
                  WRITE( FIFNAM(J,ISET), '(A1)' ) 'D'
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
              WRITE( FFE(J,ISET), '(A3)' ) '4cm'
              FRF1(J,ISET) = TMP_RF1(ISET)
              FRF2(J,ISET) = TMP_RF2(ISET)
              FLO1(J,ISET) = TMP_FLO1(ISET)
          END DO
          FRNOTE(ISET) = ''
C
C        50 cm filter.
C
         FLCP50CM(ISET) = 'NARROW'
         FRCP50CM(ISET) = 'NARROW'          
      END DO
C
      END