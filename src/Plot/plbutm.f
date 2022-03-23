      SUBROUTINE PLBUTM( X1, X2, Y1, Y2, MODE )
C
C     Routine for sched that plot a 3D Button with scroll arrow
C     MODE 1 = UP
C          2 = DOWN
C          3 = LEFT
C          4 = RIGHT
C
      INCLUDE 'plot.inc'
C
      REAL          X1, Y1, X2, Y2
      REAL          XA(3), YA(3)
      INTEGER       LIGHT, DARK, MODE
C ----------------------------------------------------------------------
C
C     Set Color schemes 
C
      LIGHT = PPNCOL(3)
      DARK  = PPNCOL(4)
C
C     Set polygon vertices
C
      IF( MODE .EQ. 1 ) THEN
         XA(1) = X1 + 0.005
         YA(1) = Y1 + 0.005
         XA(2) = X1 + ((X2 - X1) / 2)
         YA(2) = Y2 - 0.005
         XA(3) = X2 - 0.005
         YA(3) = YA(1)
      ELSE IF( MODE .EQ. 2 ) THEN
         XA(1) = X1 + 0.005
         YA(1) = Y2 - 0.007
         XA(2) = X1 + ((X2 - X1) / 2)
         YA(2) = Y1 + 0.005
         XA(3) = X2 - 0.005
         YA(3) = YA(1)
      ELSE IF( MODE .EQ. 3 ) THEN
         XA(1) = X2 - 0.005
         YA(1) = Y2 - 0.005
         XA(2) = X1 + 0.005
         YA(2) = Y1 + ((Y2 - Y1) / 2)
         XA(3) = XA(1)
         YA(3) = Y1 + 0.005
      ELSE IF( MODE .EQ. 4 ) THEN
         XA(1) = X1 + 0.005
         YA(1) = Y2 - 0.005
         XA(2) = X2 - 0.005
         YA(2) = Y1 + ((Y2 - Y1) / 2)
         XA(3) = XA(1)
         YA(3) = Y1 + 0.005
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
C     Write the scroll arrow
C
      CALL PGSFS( 1 )
      CALL PGPOLY( 3, XA, YA )
C
C     Restore PGPLOT calling attributes
C
      CALL PGUNSA
C
      RETURN
      END
