      SUBROUTINE KUBANDFREQ(FREQTOTAL)
C
C     Routine for SCHED called by MAKEFREQ that creates the frequency 
C     settings for the 2cm band. 
C
C
      INCLUDE 'schfreq.inc'
C
C     Temporary values
C
      DOUBLE PRECISION  TMP_FLO1(MFREQ)
      INTEGER           ISET, J, I
      DOUBLE PRECISION  TMP_LO
      INTEGER           FREQTOTAL
      INTEGER           TMP_RF1(MFREQ), TMP_RF2(MFREQ)
      INTEGER           TMP_FREQTOTAL
      DOUBLE PRECISION  TMP_CH1RF1(MFREQ), TMP_CH1RF2(MFREQ)
C
C     New lists to merge
C
      DOUBLE PRECISION  NEW_FLO1(MFREQ)
      INTEGER           NEW_RF1(MFREQ), NEW_RF2(MFREQ)
      INTEGER           TMP_PRIO
      CHARACTER         F_NAME(MFREQ)*3
C
C     Initialize variables
      TMP_LO = 11100.0
      TMP_FREQTOTAL = FREQTOTAL + 1
C
C     Calculate LO for 2cm
C
      DO ISET = TMP_FREQTOTAL, MFREQ
         IF( TMP_LO  .LE. 14900.0 ) THEN
             TMP_FLO1(ISET) = TMP_LO
             FREQTOTAL = FREQTOTAL + 1
         END IF
         TMP_LO = TMP_LO + 100.0
      END DO
C
C     Calculate RF1 and RF2 values for 2cm
C
      CALL GETRFX(TMP_FLO1, TMP_FREQTOTAL, FREQTOTAL, TMP_RF1, 
     1            TMP_RF2, 512, 1024)
C
C     Calculate filter values
C
      I = TMP_FREQTOTAL
      DO ISET = TMP_FREQTOTAL, FREQTOTAL
          TMP_PRIO = 0
c
C         Filter 1
C         Limit of filter for LO grabbed from original catalog file
C         Filter ranges from 11800 to 13000
C
          IF( TMP_FLO1(ISET) .GE. 11100 .AND. 
     1        TMP_FLO1(ISET) .LE. 12100 ) THEN
C             Copy existing values to new list
              NEW_FLO1(I) = TMP_FLO1(ISET)
              NEW_RF1(I) = TMP_RF1(ISET)
              NEW_RF2(I) = TMP_RF2(ISET)
C             Add new values for ch1rf
              TMP_CH1RF1(I) = 11000
              TMP_CH1RF2(I) = 12900
              FRNOTE(I) = 'First 2cm filter'
              F_NAME(I) = '1F'
C             Limit RF1
              IF( TMP_RF1(ISET) .LT. 11800 ) THEN
                  NEW_RF1(I) = 11800
                  TMP_PRIO = 1
              END IF
C             Repeat setting at the limit between each filter
              IF(TMP_FLO1(ISET) .EQ. 12100 .AND. 
     1              TMP_RF2(ISET) .GT. 13000) THEN
                  NEW_FLO1(I) = TMP_FLO1(ISET)
                  NEW_RF1(I) = TMP_RF1(ISET)
                  NEW_RF2(I) = 13000
                  TMP_PRIO = 1
                  TMP_CH1RF1(I) = 11000
                  TMP_CH1RF2(I) = 12900
                  FRNOTE(I) = 'First 2cm filter'
                  PRIO(I) = TMP_PRIO
                  I = I + 1
              ELSE IF ( TMP_RF2(ISET) .GT. 13000 ) THEN
                  NEW_RF2(I) = 13000
                  TMP_PRIO = 1
              END IF
          END IF
C
C         Filter 2
C         Filter ranges from 12800 to 14000
C
          IF( TMP_FLO1(ISET) .GE. 12100 .AND. 
     1        TMP_FLO1(ISET) .LE. 13100 ) THEN
C         Copy existing values to new list
              NEW_FLO1(I) = TMP_FLO1(ISET)
              NEW_RF1(I) = TMP_RF1(ISET)
              NEW_RF2(I) = TMP_RF2(ISET)
C             Add new values for ch1rf
              TMP_CH1RF1(I) = 12900
              TMP_CH1RF2(I) = 13900
              FRNOTE(I) = 'Second 2cm filter'
              F_NAME(I) = '2F'
C             Limit RF1
              IF( TMP_RF1(ISET) .LT. 12800 ) THEN
                  NEW_RF1(I) = 12800
                  TMP_PRIO = 1
              END IF
C             Repeat setting at the limit between each filter
              IF(TMP_FLO1(ISET) .EQ. 13100 .AND. 
     1              TMP_RF2(ISET) .GT. 14000) THEN
                  NEW_FLO1(I) = TMP_FLO1(ISET)
                  NEW_RF1(I) = TMP_RF1(ISET)
                  NEW_RF2(I) = 14000
                  TMP_PRIO = 1
                  TMP_CH1RF1(I) = 12900
                  TMP_CH1RF2(I) = 13900
                  FRNOTE(I) = 'Second 2cm filter'
                  PRIO(I) = TMP_PRIO
                  I = I + 1
              ELSE IF ( TMP_RF2(ISET) .GT. 14000 ) THEN
                  NEW_RF2(I) = 14000
                  TMP_PRIO = 1
              END IF
          END IF
C
C         Filter 3
C         Filter ranges from 13800 to 15000
C
          IF( TMP_FLO1(ISET) .GE. 13100 .AND. 
     1        TMP_FLO1(ISET) .LE. 14100 ) THEN
C         Copy existing values to new list
              NEW_FLO1(I) = TMP_FLO1(ISET)
              NEW_RF1(I) = TMP_RF1(ISET)
              NEW_RF2(I) = TMP_RF2(ISET)
C             Add new values for ch1rf
              TMP_CH1RF1(I) = 13900
              TMP_CH1RF2(I) = 14900
              FRNOTE(I) = 'Third 2cm filter'
              F_NAME(I) = '3F'
C             Limit RF1
              IF( TMP_RF1(ISET) .LT. 13800 ) THEN
                  NEW_RF1(I) = 13800
                  TMP_PRIO = 1
              END IF
C             Repeat setting at the limit between each filter
              IF(TMP_FLO1(ISET) .EQ. 14100 .AND. 
     1              TMP_RF2(ISET) .GT. 15000) THEN
c                 I = I + 1
                  NEW_FLO1(I) = TMP_FLO1(ISET)
                  NEW_RF1(I) = TMP_RF1(ISET)
                  NEW_RF2(I) = 15000
                  TMP_PRIO = 1
                  TMP_CH1RF1(I) = 13900
                  TMP_CH1RF2(I) = 14900
                  FRNOTE(I) = 'Third 2cm filter'
                  PRIO(I) = TMP_PRIO
                  I = I + 1
              ELSE IF ( TMP_RF2(ISET) .GT. 15000 ) THEN
                  NEW_RF2(I) = 15000
                  TMP_PRIO = 1
              END IF
          END IF
C
C         Filter 4
C         Filter ranges from 14700 to 15900
C
          IF( TMP_FLO1(ISET) .GE. 14100) THEN
C         Copy existing values to new list
              NEW_FLO1(I) = TMP_FLO1(ISET)
              NEW_RF1(I) = TMP_RF1(ISET)
              NEW_RF2(I) = TMP_RF2(ISET)
C             Add new values for ch1rf
              TMP_CH1RF1(I) = 14900
              TMP_CH1RF2(I) = 15900
              FRNOTE(I) = 'Fourth 2cm filter'
              F_NAME(I) = '4F'
C             Limit RF1
              IF( TMP_RF1(ISET) .LT. 14700 ) THEN
                  NEW_RF1(I) = 14700
                  TMP_PRIO = 1
              ELSE IF ( TMP_RF2(ISET) .GT. 15900 ) THEN
                  TMP_PRIO = 1
              END IF
          END IF
          PRIO(I) = TMP_PRIO
          IF( ISET .NE. FREQTOTAL ) THEN
              I = I + 1
          END IF
      END DO
C     
c     Fill out the rest of the values used for frequency settings
C
      DO ISET = TMP_FREQTOTAL, I
C         Create name
          FSYN(1,ISET) = NEW_FLO1(ISET)/1000.0
          WRITE( FRNAME(ISET), '(A, F4.1, A, A)' ) "v2cm_", 
     1       FSYN(1,ISET), "U_", F_NAME(ISET)
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
              WRITE( FFE(J,ISET), '(A3)' ) '2cm'
              FRF1(J,ISET) = NEW_RF1(ISET)
              FRF2(J,ISET) = NEW_RF2(ISET)
              FLO1(J,ISET) = NEW_FLO1(ISET)
              FCH1RF1(J,ISET) = TMP_CH1RF1(ISET)
              FCH1RF2(J,ISET) = TMP_CH1RF2(ISET)
          END DO
C
C        50 cm filter.
C
         FLCP50CM(ISET) = 'NARROW'
         FRCP50CM(ISET) = 'NARROW'
C
      END DO
C
      FREQTOTAL = I
C
      END