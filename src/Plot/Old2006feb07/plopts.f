      SUBROUTINE PLOPTS
C
      INCLUDE 'plot.inc'
C
C     Display OPTIONS area
C
C     To add or modify the options in the OPTIONS Panel, check also
C     the INCLUDE file 'plot.inc' and the soubroutine 'plinit.f'
C
      INTEGER       I, J
      REAL          XT, YT
C ----------------------------------------------------------------------
C
C     Save PGPLOT calling attribute
C
      CALL PGSAVE
C
C     Clear Area if Needed
C
      IF( PRWARN ) THEN
         CALL PGSFS( 1 )
         CALL PGSCI( 0 )
         CALL PGRECT( PPSDIM(1), PPSDIM(2), PPSDIM(3), PPSDIM(4) )
      END IF
C
C     Titles
C
      CALL PGSLW( 5 )
      CALL PGSCF( 1 )
      CALL PGSCH( PPNDIM(3) )
      CALL PGSCI( PPNCOL(7) )
C
C     Line Width
C
      XT = PPSDIM(1)
      YT = PPSDIM(4)
      CALL PGPTXT(XT, YT, 0.0, 0.0,
     1            'Select the line width' )
C
C     Axis and Label Width
C
      XT = PPSDIM(1)
      YT = PAWTXT(4,1) + 0.05
      CALL PGPTXT(XT, YT, 0.0, 0.0,
     1            'Select the Axis and Labels Width' )
C
C     Background Colors
C
      XT = PPSDIM(1)
      YT = PBGTXT(4,1) + 0.05
      CALL PGPTXT(XT, YT, 0.0, 0.0,
     1            'Select the background colors' )
C
C     Catalog Symbols and Colors
C
      XT = PPSDIM(1)
      YT = PCTTXT(4,1) + 0.05
      CALL PGPTXT(XT, YT, 0.0, 0.0,
     1            'Select the catalog symbols/colors' )
C
C     Line Width Text Area and Select box
C
      CALL PGSCI( PPNCOL(2) )
      YT = PLWTXT(3,1) + 0.025
      XT = PPSDIM(1)
      CALL PGPTXT(XT, YT, 0.0, 0.0, 'Display' )
      XT = PLWTXT(1,2) - 0.15
      CALL PGPTXT(XT, YT, 0.0, 0.0, 'Printer' )
      DO 10 I=1,2
         J = (I * 2) - 1
         CALL PLSTXT( PLWTXT(1,I), PLWTXT(2,I), PLWTXT(3,I),
     1                PLWTXT(4,I), ' ', 0, .TRUE. )
C
         CALL PLSBAR( PLWSBR(1,I), PLWSBR(2,I), PLWSBR(3,I),
     1                PLWSBR(4,I), 1, 1, PLWBXM(1,J) )
10    CONTINUE
C
C     Set Initial Line Width
C
      CALL PLOPLW( 0 )
C
C     Axis and Labels Width Text Area and Select box
C
      CALL PGSCI( PPNCOL(2) )
      YT = PAWTXT(3,1) + 0.025
      XT = PPSDIM(1)
      CALL PGPTXT(XT, YT, 0.0, 0.0, 'Display' )
      XT = PAWTXT(1,2) - 0.15
      CALL PGPTXT(XT, YT, 0.0, 0.0, 'Printer' )
      DO 15 I=1,2
         J = (I * 2) - 1
         CALL PLSTXT( PAWTXT(1,I), PAWTXT(2,I), PAWTXT(3,I),
     1                PAWTXT(4,I), ' ', 0, .TRUE. )
C
         CALL PLSBAR( PAWSBR(1,I), PAWSBR(2,I), PAWSBR(3,I),
     1                PAWSBR(4,I), 1, 1, PAWBXM(1,J) )
15    CONTINUE
C
C     Set Initial Axis and Labels Width
C
      CALL PLOPAW( 0 )
C
C     Background Color Area and Select box
C
      CALL PLSTXT( PBGTXT(1,1), PBGTXT(2,1), PBGTXT(3,1),
     1             PBGTXT(4,1), PBGLAB(PBGCK), 0, .FALSE. )
      CALL PLSCOL( PBGTXT(1,2), PBGTXT(2,2), PBGTXT(3,2),
     1             PBGTXT(4,2), PBGCK, PLYBG(PBGCK) )
      DO 20 I=1,2
         J = (I * 2) - 1
         CALL PLSBAR( PBGSBR(1,I), PBGSBR(2,I), PBGSBR(3,I),
     1                PBGSBR(4,I), 1, 1, PBGBXM(1,J) )
20    CONTINUE
C
C     Catalog Names, Symbols and Colors Area
C
      CALL PLSTXT( PCTTXT(1,1), PCTTXT(2,1), PCTTXT(3,1),
     1             PCTTXT(4,1), PCTLAB(PCTCK), 0, .FALSE. )
      CALL PLSSYM( PCTTXT(1,2), PCTTXT(2,2), PCTTXT(3,2), PCTTXT(4,2) )
      DO 30 I=1,3
         J = (I * 2) - 1
         CALL PLSBAR( PCTSBR(1,I), PCTSBR(2,I), PCTSBR(3,I),
     1                PCTSBR(4,I), 1, 1, PCTBXM(1,J) )
30    CONTINUE
C
C     Actions Buttons
C
      DO 40 I=1,3
          CALL PLBUTA( PLYBUT(1,I), PLYBUT(2,I), PLYBUT(3,I),
     1                 PLYBUT(4,I), PLYLAB(I), PPNCOL(1) )
 40   CONTINUE
C
C     Restore PGPLOT calling attributes
C
      CALL PGUNSA
C
      RETURN
      END
