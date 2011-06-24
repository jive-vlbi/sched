      SUBROUTINE PLBUTA( X1, X2, Y1, Y2, LABEL, REV )
C
C     Routine for sched that plot a 3D button with label inside.
C
      INCLUDE 'plot.inc'
C
      CHARACTER*(*) LABEL
      REAL          X1, X2, Y1, Y2, XL, YL
      INTEGER       REV, LIGHT, DARK, LCOL, BCOL
C ----------------------------------------------------------------------
C
C     Set Color schemes: 
C     If REV = 0, LIGHT is light and DARK is dark
C     If REV = 1, LIGHT is dark  and DARK is light
C
      BCOL  = PPNCOL(1)
      IF( REV .EQ. 0 ) THEN
         LIGHT = PPNCOL(3)
         DARK  = PPNCOL(4)
      ELSE
         LIGHT = PPNCOL(4)
         DARK  = PPNCOL(3)
         IF( REV .GT. 0 ) BCOL = REV
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
C     or with the fill foreground color if REV > 0
C
      LCOL = PPNCOL(2)
      IF( REV .GT. 0) LCOL = PPNCOL(6)
C
C     String Bounding Box
C
      CALL PGSLW( 5 )
      CALL PGSCF( 1 )
      CALL PGSCH( PPNDIM(3) )
C      CALL PGLEN( 0, LABEL, XL, YL)
      XL = X1 + 0.015
      YL = Y1 + 0.025
C
      CALL PGSCI( LCOL )
      CALL PGPTXT(XL, YL, 0.0, 0.0, LABEL)
C
C     Restore PGPLOT calling attributes
C
      CALL PGUNSA
C
      RETURN
      END
