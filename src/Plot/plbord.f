      SUBROUTINE PLBORD( X1, X2, Y1, Y2, BCOL, FILL )
C
C     Routine for sched that plot a 3D border
C     BCOL = Background Color of the X, Y window area
C     FILL = Clear area if greater than 0
C
      INCLUDE  'plot.inc'
C
      INTEGER       BCOL, FILL
      REAL          X1, X2, Y1, Y2
C ----------------------------------------------------------------------
C
C     Save PGPLOT calling attribute
C

      CALL PGSAVE
C
C     Clear area
C
      IF( FILL .GT. 0 ) THEN
         CALL PGSFS( 1 )
         CALL PGSCI( BCOL )
         CALL PGRECT( X1, X2, Y1, Y2 )
      END IF
C
C     Draw a border to the viewport
C
      CALL PGSLW( 6 )
      CALL PGSLS( 1 )
      CALL PGSCI( PPNCOL(3) )
      CALL PGSFS( 2 )
      CALL PGRECT( X1, X2, Y1, Y2 )
      CALL PGSLW( 1 )
      CALL PGSCI( PPNCOL(4) )
      CALL PGRECT( X1, X2, Y1, Y2 )
C
C     Restore PGPLOT calling attributes
C
      CALL PGUNSA
C
      RETURN
      END
