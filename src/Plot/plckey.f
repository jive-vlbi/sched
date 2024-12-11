      SUBROUTINE PLCKEY(SCREEN)
C
C     Routine for SCHED that plots catalog source key in RD plot
C
      INCLUDE 'plot.inc'
C
      CHARACTER         WARN*80
      INTEGER           CCOL, CSYM, LABSIZ, LINSIZ
      LOGICAL           SCREEN
      REAL              XPT, YPT
C -------------------------------------------------------------------
C
C     Set Symbols and Labels Size 
C
      IF( SCREEN ) THEN
         LINSIZ = PLYLW(1)
         LABSIZ = PLYAW(1)
      ELSE
         LINSIZ = PLYLW(2)
         LABSIZ = PLYAW(2)
      END IF
C
C     Reset viewport to plot symbols outside graphics area
C
      CALL PGSVP( 0.0, 1.0, 0.0, 1.0 )
      CALL PGSWIN( 0.0, 1.0, 0.0, 1.0 )
C
C     Set the common X axis position and the Yaxis start
C
      XPT = PZMWIN(2) + 0.03
      YPT = 0.25
C
C     Plot symbol and label for sources in schedule
C
      CSYM = PLYCT(2,2)
      CCOL = PLYCT(2,1)
      CALL PGSCI( CCOL )
      CALL PGSCH( 1.0 )
      CALL PGSLW( LINSIZ )
      CALL PGPT( 1, XPT, YPT, CSYM )
      CALL PGSCH( 0.6 )
      CALL PGSLW( LABSIZ )
      CALL PGPTXT( XPT, YPT, 90.0, 0.0, '  SCHED' )
C
C     Plot symbol and label for sources in VLA Catalog
C
      YPT = YPT + ( 0.88 / 6.5 )
      CSYM = PLYCT(3,2)
      CCOL = PLYCT(3,1)
      CALL PGSCI( CCOL )
      CALL PGSCH( 1.0 )
      CALL PGSLW( LINSIZ )
      CALL PGPT( 1, XPT, YPT, CSYM )
      CALL PGSCH( 0.6 )
      CALL PGSLW( LABSIZ )
      CALL PGPTXT( XPT, YPT, 90.0, 0.0, '  VLA' )
C
C     Plot symbol and label for sources in USNO Catalog
C
      YPT = YPT + ( 0.88 / 6.5 )
      CSYM = PLYCT(4,2)
      CCOL = PLYCT(4,1)
      CALL PGSCI( CCOL )
      CALL PGSCH( 1.0 )
      CALL PGSLW( LINSIZ )
      CALL PGPT( 1, XPT, YPT, CSYM )
      CALL PGSCH( 0.6 )
      CALL PGSLW( LABSIZ )
      CALL PGPTXT( XPT, YPT, 90.0, 0.0, '  USNO' )
C
C     Plot symbol and label for sources in MERLIN Catalog
C
      YPT = YPT + ( 0.88 / 6.5 )
      CSYM = PLYCT(5,2)
      CCOL = PLYCT(5,1)
      CALL PGSCI( CCOL )
      CALL PGSCH( 1.0 )
      CALL PGSLW( LINSIZ )
      CALL PGPT( 1, XPT, YPT, CSYM )
      CALL PGSCH( 0.6 )
      CALL PGSLW( LABSIZ )
      CALL PGPTXT( XPT, YPT, 90.0, 0.0, '  JVAS' )
C
C     Plot symbol and label for sources in VLBA Catalog
C
      YPT = YPT + ( 0.88 / 6.5 )
      CSYM = PLYCT(6,2)
      CCOL = PLYCT(6,1)
      CALL PGSCI( CCOL )
      CALL PGSCH( 1.0 )
      CALL PGSLW( LINSIZ )
      CALL PGPT( 1, XPT, YPT, CSYM )
      CALL PGSCH( 0.6 )
      CALL PGSLW( LABSIZ )
      CALL PGPTXT( XPT, YPT, 90.0, 0.0, '  VLBA' )
C
C     Plot Warning only to screen ( comment the IF condition to see
C     warning on paper also )
C
      IF( SCREEN ) THEN
         WARN = 'WARNING: Not all catalog sources are good calibrators'
         XPT = 0.59
         YPT = 0.1
         CALL PGSCI( 1 )
         CALL PGSCH( 0.8 )
         CALL PGPTXT( XPT, YPT, 0.0, 0.0, WARN )
      END IF
C
      RETURN
      END
