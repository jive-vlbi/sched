      SUBROUTINE PLSUN( CHSEL, MODE )
C
C     Routine for sched that plot the Sun
C     MODE: 1 = Draw Sun Controls
C           2 = Set Elevation Sign
C           3 = Set Elevation Exponent
C           4 = Set Elevation Value
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
      CHARACTER        CHSEL, CH, STRING*14
      INTEGER          DVAL
      INTEGER          MODE, REV, J, N
      LOGICAL          PBTR
      REAL             XL, XR, YT, YB, CSIZ
C ----------------------------------------------------------------------
C
C     Set Font, Radio Buttons Policy and Label offset
C
      CSIZ  = PPNDIM(3)
      PBTR  = .TRUE.
      XL    = 0.02
C     
C     Draw Sun Controls
C
      IF( MODE .EQ. 1 ) THEN
C
C        Draw The Selection Box
C
         REV = 0
         IF ( PXYSUN ) REV = 1
         CALL PLBUTS( PBTR, PSUBOX(1), PSUBOX(2), PSUBOX(3), PSUBOX(4),
     1                XL, CSIZ, 'Plot SUN', PPNCOL(7), REV)
C
C        Draw Elevation Selection Boxes if the Sun is selected and
C        Plot type not equal RD
C
         IF( POPTYP(POPBCK) .NE. 'RD' ) THEN
            IF( PXYSUN ) THEN
C
C              Label
C
               XL = PPSDIM(1) + 0.25
               YT = PPSDIM(3) - 0.025
               CALL PGSCH( CSIZ )
               CALL PGSCI( PPNCOL(7) )
               CALL PGPTXT(XL, YT, 0.0, 0.0, 'at min EL' )
C
C              Elevation Sign
C
               CH = '+'
               IF( PSUNEL .LT. 0.0 ) CH = '-'
               CALL PLSTXT( PSUSGN(1), PSUSGN(2), PSUSGN(3),
     1                      PSUSGN(4), CH, 1, .TRUE. )
C
C              Elevation Value
C
               N = ABS( PSUNEL )
               CALL PGNUMB( N, 0, 1, STRING, J )
               CALL PLSTXT( PSUTXT(1), PSUTXT(2), PSUTXT(3),
     1                      PSUTXT(4), STRING, 1, .FALSE. )
C
C              Elevation Value Controls
C
               CALL PLBUTC( PSUDEF(1), PSUDEF(2), PSUDEF(3),
     1                      PSUDEF(4), 'X', 0, 0 )
               CALL PLBUTC( PSUBXM(1), PSUBXM(2), PSUBXM(3),
     1                      PSUBXM(4), ' ', PSUEXP, 1 )
C
C           Else Clear Elevation Area
C
            ELSE
               XL = PPSDIM(1) + 0.25
               XR = PPSDIM(2)
               YB = PPSDIM(3) - 0.035
               YT = YB + 0.075
               CALL PGSCI( PPNCOL(1) )
               CALL PGRECT( XL, XR, YB, YT )
            END IF
         END IF
C
C    Set Sun Elevation Sign
C
      ELSE IF( MODE .EQ. 2 ) THEN
         IF( PSUNEL .LT. 0.0 ) THEN
             CH = '+'
             PSUNEL = ABS( PSUNEL )
         ELSE
             CH = '-'
             PSUNEL = PSUNEL * (-1.0)
         END IF
         CALL PLSTXT( PSUSGN(1), PSUSGN(2), PSUSGN(3),
     1                PSUSGN(4), CH, 1, .TRUE. )
C
C    Set Sun Elevation Exponent
C
      ELSE IF( MODE .EQ. 3 ) THEN
          IF( CHSEL .EQ. 'A' .OR. CHSEL .EQ. '+' ) THEN
             PSUEXP = PSUEXP + 1
             IF( PSUEXP .GT. 4) PSUEXP = 0
          ELSE
             PSUEXP = PSUEXP - 1
             IF( PSUEXP .LT. 0) PSUEXP = 4
          END IF
          CALL PLBUTC( PSUBXM(1), PSUBXM(2), PSUBXM(3),
     1                 PSUBXM(4), ' ', PSUEXP, 1 )
C
C    Set Sun Elevation Value
C
      ELSE IF( MODE .EQ. 4 ) THEN
          N = 1
          IF( PSUNEL .LT. 0.0 ) N = -1
          J = PSUNEL
          DVAL = PSUNEL
          CALL PLVLCK( PSUTXT(1), PSUTXT(2), PSUTXT(3), PSUTXT(4),
     1              PSUBXM(1), PSUBXM(2), PSUBXM(3), PSUBXM(4),
     2              DVAL, 0, 0, 90, PSUEXP, PXYEXP, N, 0, 1,
     3              CHSEL, STRING )
          PSUNEL = J
C
      END IF
C
      RETURN
      END

