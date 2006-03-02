      SUBROUTINE PLSBAR( X1, X2, Y1, Y2, MODE, COL, SBUT )
C
C     Routine for sched that plot a 3D Scroll Bar.
C     MODE 1 = Vertical
C          2 = Horizzontal
C
      INTEGER       MODE, COL, ROT
      REAL          X1, X2, Y1, Y2
      REAL          SBUT(4,2), XYSIZ
C ----------------------------------------------------------------------
C
C     Initialize the Scroll buttons dimensions
C
      IF( MODE .EQ. 1) THEN
         ROT = 1
         XYSIZ = X2 - X1
         SBUT(1,1) = X1
         SBUT(2,1) = X2
         SBUT(3,1) = Y2 - XYSIZ
         SBUT(4,1) = Y2
         SBUT(1,2) = X1
         SBUT(2,2) = X2
         SBUT(3,2) = Y1
         SBUT(4,2) = Y1 + XYSIZ
      ELSE
         ROT = 3
         XYSIZ = Y2 - Y1
         SBUT(1,1) = X1
         SBUT(2,1) = X1 + XYSIZ
         SBUT(3,1) = Y1
         SBUT(4,1) = Y2
         SBUT(1,2) = X2 - XYSIZ
         SBUT(2,2) = X2
         SBUT(3,2) = Y1
         SBUT(4,2) = Y2
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
C     Draw a Bar with black border
C
      CALL PGSCI( COL )
      CALL PGRECT( X1, X2, Y1, Y2 )
      CALL PGSCI( 15 )
      CALL PGSFS( 2 )
      CALL PGRECT( X1, X2, Y1, Y2 )
C
C     Draw the scroll buttons in the bar
C
      CALL PLBUTM( SBUT(1,1), SBUT(2,1), SBUT(3,1),
     1             SBUT(4,1), ROT )
      CALL PLBUTM( SBUT(1,2), SBUT(2,2), SBUT(3,2),
     1             SBUT(4,2), ROT+1 )
C
C     Restore PGPLOT calling attributes
C
      CALL PGUNSA
C
      RETURN
      END
