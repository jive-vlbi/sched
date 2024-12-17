      SUBROUTINE PLSCOL( X1, X2, Y1, Y2, IND, COL )
C
C     Routine for sched that plot a select color area
C     If MODE: 1 replot the area
C              0 plot only the color box
C
      INTEGER       COL, IND, CIND
      REAL          X1, X2, Y1, Y2, RGB
C ----------------------------------------------------------------------
C
C     Check range and set RGB value ( 16 gray color ) and Color Index
C
      IF( COL .GT. 16 ) COL = 0
      IF( COL .LT. 0  ) COL = 16
      RGB = 0.0625 * COL
      CIND = 15 + IND
      CALL PGSCR( CIND, RGB, RGB, RGB )
C
C     Save PGPLOT calling attribute
C
      CALL PGSAVE
C
C        Draw the Area with Border 
C
      CALL PGSLS( 1 )
      CALL PGSLW( 2 )
      CALL PGSCI( CIND )
      CALL PGRECT( X1, X2, Y1, Y2 )
      CALL PGSCI( 4 )
      CALL PGSFS( 2 )
      CALL PGRECT( X1, X2, Y1, Y2 )
C
C     Restore PGPLOT calling attributes
C
      CALL PGUNSA
C
      RETURN
      END
