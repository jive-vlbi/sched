      SUBROUTINE PLBUTC( X1, X2, Y1, Y2, CH, NUM, MODE )
C
C     Routine for sched that plot a 3D Button with single character
C     inside.
C     MODE 0 = Plot CH
C          1 = Plot NUM
C
      INCLUDE 'plot.inc'
C
      CHARACTER     CH*(*), SCH
      REAL          X1, Y1, X2, Y2
      REAL          XL, YL
      INTEGER       NUM, MODE, LIGHT, DARK
C ----------------------------------------------------------------------
C
C     Set Color schemes 
C
      LIGHT = PPNCOL(3)
      DARK  = PPNCOL(4)
C
C     Set character to plot
C
      IF( MODE .EQ. 0 ) THEN
         SCH = CH(1:1)
      ELSE IF( MODE .EQ. 1 ) THEN
         SCH = CHAR( ( NUM + 48 ) )
      END IF
C
C     Save PGPLOT calling attribute
C
      CALL PGSAVE
C
C     Set line style (full) and size to plot 3D shadow
C
      CALL PGSLS( 1 )
      CALL PGSLW( 2 )
C
C     Draw a 3D Button light (white) borders
C
      CALL PGSCI( 0 )
      CALL PGSFS( 1 )
      CALL PGRECT( X1, X2, Y1, Y2 )
      CALL PGSCI( LIGHT )
      CALL PGSFS( 2 )
      CALL PGMOVE( X1, Y1 )
      CALL PGDRAW( X1, Y2 )
      CALL PGDRAW( X2, Y2 )
C
C     Draw a 3D Button dark (black) borders
C
      CALL PGSCI( DARK )
      CALL PGDRAW( X2, Y1 )
      CALL PGDRAW( X1, Y1 )
C
C     Write the character
C
      CALL PGSCF( 1 )
      CALL PGSCH( 1.0 )
      XL = X1 + 0.01
      YL = Y1 + 0.01
      CALL PGPTXT( XL, YL, 0.0, 0.0, SCH )
C
C     Restore PGPLOT calling attributes
C
      CALL PGUNSA
C
      RETURN
      END
