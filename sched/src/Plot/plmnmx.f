      SUBROUTINE PLMNMX( XYAXIS, XYMIN, XYMAX, MODE )
C
C     Routine for SCHED that plots sky coverage according to requests
C     from user gathered by plotter.f
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
      INTEGER           MODE, ISCN, ISTA, KSET, LSRC, KSRC
      REAL              PT1, PT2, XYMIN, XYMAX
      CHARACTER         XYAXIS*(*)
C -------------------------------------------------------------------
C
      LSRC = 0
      KSRC = 0
      IF( PSFBCK .EQ. 0 ) THEN
         KSET = 1
      ELSE
         KSET = PSFBCK
      END IF
C
C     Make min/max if selected
C
      IF( MODE .EQ. 1 ) THEN
         IF( XYAXIS .EQ. 'Sec' ) THEN
            XYMIN = PXSLIM(8,2)
            XYMAX = PXSLIM(8,1)
         END IF 
C
C        Loop over stations.
C
         DO ISTA = 1, NSTA
           IF( PSTBCK(ISTA,1) .EQ. 1 ) THEN 
C
C           Loop over scans
C
            DO ISCN = SCAN1, SCANL
               IF( ( ( SETNUM(ISCN) .EQ. KSET .OR. KSET .EQ. 0 ) .AND.
     1               ( SRCNUM(ISCN) .EQ. LSRC .OR. KSRC .EQ. 0 ) ) .AND.
     3                 PSOBCK(SRLSTN(SRCNUM(ISCN))) .EQ. 1 ) THEN
                  IF( STASCN(ISCN,ISTA) .AND. 
     1                UP1(ISCN,ISTA) .EQ. ' ' .AND.
     2                UP2(ISCN,ISTA) .EQ. ' ' ) THEN

C
C                    Get min and max value of the points to plot.
C
                     IF( XYAXIS .EQ. 'Sec' ) THEN
                        PT1 = 1.0/SIN( EL1(ISCN,ISTA) * RADDEG )
                        PT2 = 1.0/SIN( EL2(ISCN,ISTA) * RADDEG )
                     END IF
C
                     IF( PT1 .GT. XYMAX ) XYMAX = PT1
                     IF( PT1 .LT. XYMIN ) XYMIN = PT1
                     IF( PT2 .GT. XYMAX ) XYMAX = PT2
                     IF( PT2 .LT. XYMIN ) XYMIN = PT2
C
                  END IF
               END IF
            END DO
          END IF
         END DO
      END IF
C
C     Add offset for clear plot
C
      IF( XYAXIS .EQ. 'Sec' ) THEN
         IF( MODE .EQ. 0 .AND. XYMIN .LT. 1.0 ) XYMIN = 1.0
         IF( XYMAX .LE. 2.0 ) THEN
            XYMIN = XYMIN - 0.1
            XYMAX = XYMAX + 0.1
         ELSE IF( XYMAX .LE. 6.0 ) THEN
              XYMIN = XYMIN - 0.2
              XYMAX = XYMAX + 0.2
         ELSE IF( XYMAX .LE. 15.0 ) THEN
                XYMIN = XYMIN - 0.5
                XYMAX = XYMAX + 0.5
         ELSE
                  XYMIN = XYMIN - 1.0
                  XYMAX = XYMAX + 1.0
         END IF
      END IF 
      RETURN
      END
