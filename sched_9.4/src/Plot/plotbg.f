      SUBROUTINE PLOTBG( MODE, COL )
C
      INCLUDE 'plot.inc'
C
C     Routine for SCHED that set the palette for the Plot Window.
C
C
      INTEGER           MODE, COL
      REAL              RGB
C -------------------------------------------------------------------
C
C     Swap Black and Gray colors if requested.
C
C
      IF( MODE .EQ. 1 ) THEN
         CALL PGSCR( 0,  0.7, 0.7, 0.7 )
         CALL PGSCR( 15, 0.0, 0.0, 0.0 )
      END IF
C
C     Select the color for the background: Screen, B/W Printer and
C     Color Printer
C
      RGB    = 0.0625 * PLYBG(1)
      CALL PGSCR( 16, RGB, RGB, RGB )
      RGB    = 0.0625 * PLYBG(2)
      CALL PGSCR( 17, RGB, RGB, RGB )
      RGB    = 0.0625 * PLYBG(3)
      CALL PGSCR( 18, RGB, RGB, RGB )
C
C     Color the Plot Window background if requested
C
      IF( COL .GT. 0 ) THEN
         CALL PGSVP( 0.0, 1.0, 0.0, 1.0 )
         CALL PGSWIN( 0.0, 1.0, 0.0, 1.0 )
         CALL PGSCI( COL )
         CALL PGRECT( 0.0, 1.0, 0.0, 1.0 )
      END IF
C
      RETURN
      END
