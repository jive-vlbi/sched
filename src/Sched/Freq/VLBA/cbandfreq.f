      SUBROUTINE CBANDFREQ(FREQTOTAL)
C 
C     Routine for SCHED called by MAKEFREQ that creates the frequency 
C     settings for the 6cm band.
C     TO-DO: Fix priority
C
      INCLUDE 'schfreq.inc'
C
C     Temporary values
C
      DOUBLE PRECISION  TMP_FLO1(MFREQ)
      INTEGER           ISET, J, JSET, I
      DOUBLE PRECISION  TMP_LO
      INTEGER           FREQTOTAL
      INTEGER           TMP_RF1(MFREQ), TMP_RF2(MFREQ)
      INTEGER           TMP_PRIO(MFREQ)
      INTEGER           TMP_FREQTOTAL
      INTEGER           TMP_FREQTOTAL2
      CHARACTER         B_LETTER, B2
      INTEGER           IFAC, IFBD, JF, KF, ESIZE, IPRIO
C
C     Sorting variables
C
      INTEGER           SINDEX(MFREQ), SRT(MFREQ)
C
C     Initialize variables
      TMP_LO = 3400.0
      TMP_FREQTOTAL = FREQTOTAL + 1
      IPRIO = TMP_FREQTOTAL
C
C     Calculate values for the first 6 cm Upper band 
C
C     LO Value
C  
      DO ISET = TMP_FREQTOTAL, MFREQ
         IF( TMP_LO  .LE. 4400.0 ) THEN
             TMP_FLO1(ISET) = TMP_LO
             FREQTOTAL = FREQTOTAL + 1
         END IF
         TMP_LO = TMP_LO + 100.0
      END DO
C
C     Calculate RF1 and RF2 values for 6cm Upper band
C
      CALL GETRFX(TMP_FLO1, TMP_FREQTOTAL, FREQTOTAL, TMP_RF1, 
     1            TMP_RF2, 512, 1024)
C
C     Check RF1 for limits
C
      DO ISET = TMP_FREQTOTAL, FREQTOTAL
          IF( TMP_FLO1(ISET) .GE. 4400 ) THEN
              TMP_RF1(ISET) = 5050
          END IF
      END DO
C
C     Calculate values for the second 6cm Upper band 
C
C     LO Value
C  
      TMP_FREQTOTAL2 = FREQTOTAL + 1
      TMP_LO = 5100
      DO ISET = TMP_FREQTOTAL2, MFREQ
         IF( TMP_LO  .LE. 6100.0 ) THEN
             TMP_FLO1(ISET) = TMP_LO
             FREQTOTAL = FREQTOTAL + 1
         END IF
         TMP_LO = TMP_LO + 100.0
      END DO
C
C     Calculate RF1 and RF2 values for second 6cm Upper band
C
      CALL GETRFX(TMP_FLO1, TMP_FREQTOTAL2, FREQTOTAL, TMP_RF1, 
     1            TMP_RF2, 512, 1024)
C
C     Check RF1 for limits
C
      DO ISET = TMP_FREQTOTAL2, FREQTOTAL
          IF( TMP_FLO1(ISET) .GE. 6100 ) THEN
              TMP_RF1(ISET) = 6800
          END IF
      END DO
C
C     Calculate priority
C    
      DO ISET = IPRIO, FREQTOTAL
          IF( TMP_RF1(ISET) .GE. 3912 .AND. 
     1                      TMP_RF1(ISET) .LT. 4112 ) THEN
              TMP_PRIO(ISET) = 1
          ELSE IF( TMP_RF1(ISET) .LT. 3912 .OR. 
     1        (TMP_RF1(ISET) .GE. 5050 .AND. TMP_RF1(ISET) .LE. 5600) 
     2                .OR. (TMP_RF1(ISET) .GE. 6800) ) THEN
              TMP_PRIO(ISET) = 2
          ELSE
              TMP_PRIO(ISET) = 0    
          END IF
      END DO
C
C
C     Calculate values for the first 6 cm Lower band 
C
C     LO Value
C  
      TMP_FREQTOTAL2 = FREQTOTAL + 1
      IPRIO = TMP_FREQTOTAL2
      TMP_LO = 5400
      DO ISET = TMP_FREQTOTAL2, MFREQ
         IF( TMP_LO  .LE. 6400.0 ) THEN
             TMP_FLO1(ISET) = TMP_LO
             FREQTOTAL = FREQTOTAL + 1
         END IF
         TMP_LO = TMP_LO + 100.0
      END DO
C
C     Calculate RF1 and RF2 values for the first 6cm Lower band
C
      CALL GETRFX(TMP_FLO1, TMP_FREQTOTAL2, FREQTOTAL, TMP_RF1, 
     1            TMP_RF2, -1024, -512)
C
C     Check RF2 for limits
C
      DO ISET = TMP_FREQTOTAL2, FREQTOTAL
          IF( TMP_FLO1(ISET) .LE. 5400 ) THEN
              TMP_RF2(ISET) = 4750
          END IF
      END DO
C
C     Calculate values for the second 6cm second Lower band 
C
C     LO Value
C  
      TMP_FREQTOTAL2 = FREQTOTAL + 1
      TMP_LO = 7400
      DO ISET = TMP_FREQTOTAL2, MFREQ
         IF( TMP_LO  .LE. 8600.0 ) THEN
             TMP_FLO1(ISET) = TMP_LO
             FREQTOTAL = FREQTOTAL + 1
         END IF
         TMP_LO = TMP_LO + 100.0
      END DO
C
C     Calculate RF1 and RF2 values for second 6cm Lower band
C
      CALL GETRFX(TMP_FLO1, TMP_FREQTOTAL2, FREQTOTAL, TMP_RF1, 
     1            TMP_RF2, -1024, -512)
C
C     Check RF2 for limits
C
      DO ISET = TMP_FREQTOTAL2, FREQTOTAL
          IF( TMP_FLO1(ISET) .LE. 7400 ) THEN
              TMP_RF2(ISET) = 6700
          END IF
          IF( TMP_FLO1(ISET) .LE. 8600 ) THEN
              TMP_RF2(ISET) = 7900
          END IF
      END DO
C
C     Calculate priority
C    
      DO ISET = IPRIO, FREQTOTAL
          IF( TMP_RF1(ISET) .LE. 4376 .OR. 
     1        (TMP_RF1(ISET) .GE. 5576 .AND. TMP_RF1(ISET) .LE. 6376)) 
     2                THEN
              TMP_PRIO(ISET) = 2
          ELSE IF( (TMP_RF1(ISET) .GT. 4376 .AND. 
     1                      TMP_RF1(ISET) .LE. 4750) .OR. 
     2                       TMP_RF1(ISET) .GE. 7576 ) THEN
              TMP_PRIO(ISET) = 1
          ELSE
              TMP_PRIO(ISET) = 0    
          END IF 
      END DO      
C
C
C     Sort by RF1
C
      DO ISET = TMP_FREQTOTAL, FREQTOTAL
          SRT(ISET) = TMP_RF1(ISET)
          SINDEX(ISET) = ISET
      END DO
C
      CALL SORTRFX(TMP_FREQTOTAL, FREQTOTAL, SRT, SINDEX)
C
C     Check 6cm rf1 and rf2 for limits;
c     Fill out the rest of the values used for frequency settings
C
      DO JSET = TMP_FREQTOTAL, FREQTOTAL
          ISET = SINDEX(JSET)
C         Create name
          IF ( TMP_RF1(ISET) .GT. TMP_FLO1(ISET) ) THEN
              B_LETTER = 'U'
          ELSE
              B_LETTER = 'L'
          END IF
          FSYN(2,JSET) = TMP_FLO1(ISET)/1000.0
          FRNOTE(JSET) = ''
          WRITE( FRNAME(JSET), '(A, F3.1, A)' ) "v6cm_", 
     1       FSYN(2,JSET), B_LETTER
C         Set priority
          PRIO(JSET) = TMP_PRIO(ISET)
C         Set stations
          DO J = 1, MFSTA
              IF( J .EQ. 1 ) THEN
                  WRITE( FSTNAM(J,JSET), '(A8)') 'VLBA    '
              ELSE
                  WRITE( FSTNAM(J,JSET), '(A8)') ''
              END IF
          END DO
C         Set ifnam
          DO J = 1, MFIF
              IF( J .EQ. 1) THEN
                  WRITE( FIFNAM(J,JSET), '(A1)' ) 'A'
              ELSE IF( J .EQ. 2) THEN
                  WRITE( FIFNAM(J,JSET), '(A1)' ) 'C'
              ELSE
                  WRITE( FIFNAM(J,JSET), '(A2)' ) ''
              END IF
          END DO
C         Set fnif, fe, pol, rf1, rf2, and lo
          FNIF(ISET) = 2
          DO J = 1, FNIF(ISET)
              IF( J .EQ. 1) THEN
                  WRITE( FPOL(J,JSET), '(A3)' ) 'RCP'
              ELSE
                  WRITE( FPOL(J,JSET), '(A3)' ) 'LCP'
              END IF
              WRITE( FFE(J,JSET), '(A3)' ) '6cm'
              FRF1(J,JSET) = TMP_RF1(ISET)
              FRF2(J,JSET) = TMP_RF2(ISET)
              FLO1(J,JSET) = TMP_FLO1(ISET)
          END DO
C          
C                 50 cm filter.
C
                  FLCP50CM(ISET) = 'NARROW'
                  FRCP50CM(ISET) = 'NARROW'
C                  
      END DO
C
C
C     Calculate size of new variables for a combination by  loops;
C     creating a factorial function would require an extra loop
C
      ESIZE = 0
      DO IFAC =  TMP_FREQTOTAL, FREQTOTAL
          DO IFBD = TMP_FREQTOTAL, FREQTOTAL
              IF( TMP_RF1(IFBD) .GT. TMP_RF1(IFAC) ) THEN
                  ESIZE = ESIZE + 1   
              END IF
          END DO
      END DO
C
C
C     Write the 4 IF options using AC and BX 
C
C
      I = FREQTOTAL
      DO IFAC = TMP_FREQTOTAL, FREQTOTAL
          DO IFBD = TMP_FREQTOTAL, FREQTOTAL
              JF = SINDEX(IFAC)
              KF = SINDEX(IFBD)
              IF( TMP_RF1(KF) .GT. TMP_RF1(JF) ) THEN
C                  END IF
                  I = I + 1 
C                 Create name
                  IF ( TMP_RF1(JF) .GT. TMP_FLO1(JF) ) THEN
                      B_LETTER = 'U'
                  ELSE
                      B_LETTER = 'L'
                  END IF
                  IF ( TMP_RF1(KF) .GT. TMP_FLO1(KF) ) THEN
                      B2 = 'U'
                  ELSE
                      B2 = 'L'
                  END IF
                  FSYN(2,I) = TMP_FLO1(JF)/1000.0
                  FSYN(1,I) = TMP_FLO1(KF)/1000.0
                  FRNOTE(I) = ''
C          
C                 50 cm filter.
C
                  FLCP50CM(I) = 'NARROW'
                  FRCP50CM(I) = 'NARROW'
C        
                  WRITE( FRNAME(I), '(A, F3.1, A, A, F3.1, A)' ) 
     1               "v6cm_", FSYN(2,I), B_LETTER, '-',
     2               FSYN(1,I), B2
C                 Set Priority
                  PRIO(I) = TMP_PRIO(JF) + TMP_PRIO(KF) + 1
C                 Set stations
                  DO J = 1, MFSTA
                      IF( J .EQ. 1 ) THEN
                          WRITE( FSTNAM(J,I), '(A8)') 'VLBA    '
                      ELSE
                          WRITE( FSTNAM(J,I), '(A8)') ''
                      END IF
                  END DO
C                 Set ifnam
                  DO J = 1, MFIF
                      IF( J .EQ. 1) THEN
                          WRITE( FIFNAM(J,I), '(A1)' ) 'A'
                      ELSE IF( J .EQ. 2) THEN
                          WRITE( FIFNAM(J,I), '(A1)' ) 'C'
                      ELSE IF( J .EQ. 3) THEN
                          WRITE( FIFNAM(J,I), '(A1)' ) 'B'
                      ELSE IF( J .EQ. 4) THEN
                          WRITE( FIFNAM(J,I), '(A1)' ) 'D'
                      ELSE
                          WRITE( FIFNAM(J,I), '(A2)' ) ''
                      END IF
                  END DO
C                 Set fnif, fe, pol, rf1, rf2, and lo
                  FNIF(I) = 4
                  DO J = 1, FNIF(I)
                      IF( J .EQ. 1 .OR. J .EQ. 3) THEN
                          WRITE( FPOL(J,I), '(A3)' ) 'RCP'
                      ELSE
                          WRITE( FPOL(J,I), '(A3)' ) 'LCP'
                      END IF
                      WRITE( FFE(J,I), '(A3)' ) '6cm'
                      IF( J .EQ. 1 .OR. J .EQ. 2) THEN
                          FRF1(J,I) = TMP_RF1(JF)
                          FRF2(J,I) = TMP_RF2(JF)
                          FLO1(J,I) = TMP_FLO1(JF)
                      ELSE
                          FRF1(J,I) = TMP_RF1(KF)
                          FRF2(J,I) = TMP_RF2(KF)
                          FLO1(J,I) = TMP_FLO1(KF)
                      END IF
                  END DO          
              END IF
          END DO
      END DO
      FREQTOTAL = I
C
      END