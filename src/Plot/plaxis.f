      SUBROUTINE PLAXIS( MODE )
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
C     Display AXIS area
C
C     To add or modify the options in the AXIS Panel, check also
C     the INCLUDE file 'plot.inc' and the soubroutine 'plinit.f'
C
      INTEGER       I, J, REV, MODE
      LOGICAL       PBTR
      REAL          XL, CSIZ, XT, YT
C ----------------------------------------------------------------------
C
C     Set Font, Radio Buttons Policy and Label offset
C
      CSIZ  = PPNDIM(3)
      PBTR  = .TRUE.
      XL    = 0.02
C
C     Save PGPLOT calling attribute
C
      CALL PGSAVE
C
      CALL PGSLW( 5 )
      CALL PGSCF( 1 )
      CALL PGSCH( PPNDIM(3) )
C
C     Type of Plot
C
      IF( MODE .EQ. 1 ) THEN
C
C        Titles
C
         XT = PPSDIM(1)
         YT = PPSDIM(4)
         CALL PGSCI( PPNCOL(7) )
         CALL PGPTXT(XT, YT, 0.0, 0.0,
     1            'Select the Type of Plot to Display' )
C
C        Radio Box
C
         DO 10 I=1,POPBXM
            REV = 0
            IF ( I .EQ. POPBCK) REV = 1
            CALL PLBUTS( PBTR, POPBOX(1,I), POPBOX(2,I), POPBOX(3,I),
     1                   POPBOX(4,I), XL, CSIZ, POPVAL(I), 0, REV)
 10      CONTINUE
C
      END IF
C
C     Clear Input area
C
      YT = PXYTXT(4,1) + 0.09
      CALL PGSFS( 1 )
      CALL PGSCI( 0 )
      CALL PGRECT( PPSDIM(1), PPSDIM(2),
     1             ( PPSDIM(3) - 0.03 ), YT )
C
C     Axis Type Text Area and Select box
C
      IF( POPTYP(POPBCK) .EQ. 'BM' ) THEN
C
C        Beam
C
         CALL PLAXBM
C
      ELSE
C
C        Titles
C
         CALL PGSCI( PPNCOL(7) )
         YT = PXYTXT(4,1) + 0.04
         XT = PPSDIM(1)
         CALL PGPTXT(XT, YT, 0.0, 0.0,
     1               'Select Plot Axis Types and Scales' )
C
C        X Axis Min/Max
C
         YT = PXLTXT(4,1) + 0.04
         CALL PGPTXT(XT, YT, 0.0, 0.0,
     1               'Set X Axis Min/Max Values' )
C
C        Y Axis Bottom/Top
C
         YT = PYBTXT(4,1) + 0.04
         CALL PGPTXT(XT, YT, 0.0, 0.0,
     1               'Set Y Axis Bottom/Top Values' )
C
         IF( MODE .EQ. 1 ) THEN
C
C           XY Axis Options
C
            CALL PGSCI( PPNCOL(2) )
            YT = PXYTXT(3,1) + 0.025
            DO 30 I=1,2
               IF( I .EQ. 1 ) THEN
                  XT = PPSDIM(1)
                  CALL PGPTXT(XT, YT, 0.0, 0.0, 'X' )
               ELSE
                  XT = PXYTXT(1,2) - 0.05
                  CALL PGPTXT(XT, YT, 0.0, 0.0, 'Y' )
               ENDIF
               J = (I * 2) - 1
               CALL PLSTXT( PXYTXT(1,I), PXYTXT(2,I), PXYTXT(3,I),
     1                      PXYTXT(4,I), PXYTYP(PXYBCK(I)), 0, .TRUE. )
C
               CALL PLSBAR( PXYSBR(1,I), PXYSBR(2,I), PXYSBR(3,I),
     1                      PXYSBR(4,I), 1, 1, PXYBXM(1,J) )
30          CONTINUE
C
C           Plot Wavelength Scale
C
            CALL PLWLSC( ' ', 0, 1 )
C
C           Plot Time Offset if requested else clean area
C
            CALL PLTMOF( ' ', 0 )
C
C           X Axis Coordinate Objects
C
            CALL PLAXST( 'X' )
C
C           Y Axis Coordinate Objects
C
            CALL PLAXST( 'Y' )
C
C           Sun and Sky Scale Box
C
            IF( POPTYP(POPBCK) .EQ. 'RD' .OR.
     1          POPTYP(POPBCK) .EQ. 'AL' ) THEN
                CALL PLSUN( ' ', 1 )
            END IF
C
            IF( POPTYP(POPBCK) .EQ. 'UP' .OR.
     1          POPTYP(POPBCK) .EQ. 'AL' ) THEN
                CALL PLSUN( ' ', 1 )
            END IF
C
C           Lock sign and value in UV Plot
C
            IF( POPTYP(POPBCK) .EQ. 'UV' ) THEN
               REV = 0
               IF ( PLOSGN ) REV = 1
               CALL PLBUTS( PBTR, PLOBOX(1,1), PLOBOX(2,1),
     1                      PLOBOX(3,1), PLOBOX(4,1), XL, CSIZ,
     2                      'Lock Sign', 0, REV)
               REV = 0
               IF ( PLOVAL ) REV = 1
               CALL PLBUTS( PBTR, PLOBOX(1,2), PLOBOX(2,2),
     1                      PLOBOX(3,2), PLOBOX(4,2), XL, CSIZ,
     2                      'Lock Value', 0, REV)
            END IF
C
         END IF
      END IF
C
C     Restore PGPLOT calling attributes
C
      CALL PGUNSA
C
      RETURN
      END
