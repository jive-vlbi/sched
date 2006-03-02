      SUBROUTINE PLSSYM( X1, X2, Y1, Y2 )
C
      INCLUDE 'plot.inc'
C
C     Routine for sched that plot a symbol and color into a 
C     selection areas.
C
      REAL          X1, X2, Y1, Y2, XT, YT
C ----------------------------------------------------------------------
C
C     Save PGPLOT calling attribute
C
      CALL PGSAVE
C
C        Draw the Area with Border 
C
      CALL PGSLS( 1 )
      CALL PGSLW( 2 )
      CALL PGSCI( 16 )
      CALL PGRECT( X1, X2, Y1, Y2 )
      CALL PGSCI( 4 )
      CALL PGSFS( 2 )
      CALL PGRECT( X1, X2, Y1, Y2 )
C
C     Draw symbol in a selected color into area
C
      XT = X1 + 0.05
      YT = Y1 + 0.035
      CALL PGSCI( PLYCT(PCTCK,1) )
      CALL PGSCH( 2.2 )
      CALL PGSLW( PLYLW(1) )
      CALL PGPT( 1, XT, YT, PLYCT(PCTCK,2) )
C
C     Restore PGPLOT calling attributes
C
      CALL PGUNSA
C
      RETURN
      END
