      SUBROUTINE PLBUTG( X1, X2, Y1, Y2, LABEL, LLEN, COLORS, MODE )
C
C     Routine for sched that plot a General 3D button with label inside.
C
C     MODE:     0 - Normal Button
C               1 - Button Pressed
C               2 - Button Activated
C               3 - Button Disabled
C
C     COLORS(): 1 - Normal Background Button
C               2 - Normal Foreground Button
C               3 - Normal Light Border 
C               4 - Normal Dark Border 
C               5 - Activated Background Button
C               6 - Activated Foreground Button
C               7 - Activated Light Border 
C               8 - Activated Dark Border 
C               9 - Disabled Foreground Button 
C
      CHARACTER*(*) LABEL
      REAL          X1, X2, Y1, Y2, XSL, YSL, XS, YS, CH
      INTEGER       LLEN, COLORS(*), MODE, LIGHT, DARK, LCOL, BCOL
C ----------------------------------------------------------------------
C
C     Set Color schemes: 
C
      IF( MODE .EQ. 0 ) THEN
         BCOL  = COLORS(1)
         LCOL  = COLORS(2)
         LIGHT = COLORS(3)
         DARK  = COLORS(4)
      ELSE IF( MODE .EQ. 1 ) THEN
         BCOL  = COLORS(1)
         LCOL  = COLORS(2)
         LIGHT = COLORS(4)
         DARK  = COLORS(3)
      ELSE IF( MODE .EQ. 2 ) THEN
         BCOL  = COLORS(5)
         LCOL  = COLORS(6)
         LIGHT = COLORS(8)
         DARK  = COLORS(7)
      ELSE
         BCOL  = COLORS(1)
         LCOL  = COLORS(9)
         LIGHT = COLORS(3)
         DARK  = COLORS(4)
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
      CALL PGSCI( BCOL )
      CALL PGRECT( X1, X2, Y1, Y2 )
      CALL PGSCI( LIGHT )
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
C     Write a label inside the button whit the specified panel color
C
      CALL PGSLW( 5 )
      CALL PGSCF( 1 )
      CALL PGSCI( LCOL )
C
C     Set Character Height
C
      CH = ( 40.0 / ( 1.0 / ( Y2 - Y1 ) ) ) * 0.5
      CALL PGSCH( CH )
C
C     Set String boundary
C
      CALL PGLEN( 0, LABEL(1:LLEN), XSL, YSL)
      XS = X1 + ( ( X2 - X1 - XSL ) / 2.0 )
      CALL PGQCS( 0, XSL, YSL )
      YS = Y1 + ( ( Y2 - Y1 - YSL ) / 1.5 )
C
C     Plot Button Label
C
      CALL PGPTXT(XS, YS, 0.0, 0.0, LABEL)
C
C     Restore PGPLOT calling attributes
C
      CALL PGUNSA
C
      RETURN
      END
