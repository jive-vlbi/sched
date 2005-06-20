      SUBROUTINE SBHOURS( ISRC, TSCAN, TBASE )
C
C     Routine for sched, called by srclst, that gets the number 
C     of hours in scans and in baselines for each source.
C
      INCLUDE     'sched.inc'
C
      INTEGER     ISCN, ISTA, JSTA, ISRC
      DOUBLE PRECISION   TBASE, TSCAN, TBSTRT
C -----------------------------------------------------------------
      TSCAN = 0.D0
      TBASE = 0.D0
C
C     Loop over scans.
C
      DO ISCN = SCAN1, SCANL
C
C        Select recording scans on the current source.
C
         IF( SRCNUM(ISCN) .EQ. ISRC .AND. .NOT. NOREC(ISCN) ) THEN
C
C           Add the scan time.
C
            TSCAN = TSCAN + ( STOPJ(ISCN) - STARTJ(ISCN) )
C
C           Get baseline time using double loop over stations.
C           Require source be up at both ends of scan.
C
            IF( NSTA .GT. 1 ) THEN
               DO ISTA = 1, NSTA -1
                  IF( STASCN(ISCN,ISTA) .AND. 
     1                UP1(ISCN,ISTA) .EQ. ' ' .AND.
     2                UP2(ISCN,ISTA) .EQ. ' ' ) THEN
                     DO JSTA = ISTA + 1, NSTA
                        IF( STASCN(ISCN,JSTA) .AND.
     1                    UP1(ISCN,JSTA) .EQ. ' ' .AND.
     2                    UP2(ISCN,JSTA) .EQ. ' ' .AND.
     3                    TONSRC(ISCN,ISTA).LT.STOPJ(ISCN) .AND.
     4                    TONSRC(ISCN,JSTA).LT.STOPJ(ISCN)
     5                         ) THEN
C
C                          Get start time of data.
C
                           TBSTRT = MAX( STARTJ(ISCN), 
     1                                  TONSRC(ISCN,ISTA) )
                           TBSTRT = MAX( TBSTRT,
     1                                  TONSRC(ISCN,JSTA) )
C
C                          Get baseline hours.
C
                           TBASE = TBASE + STOPJ(ISCN) - TBSTRT
C
                        END IF
                     END DO
                  END IF
               END DO
            END IF                  
         END IF
      END DO
C
      RETURN
      END
