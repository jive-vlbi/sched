      SUBROUTINE PLOTOFS( DAY0, XAXIS, XMIN, XMAX, YMIN, YMAX )
C
C     Routine for SCHED called by PLOTXY and PLOTUP that plot
C     the Local Time axis on Top of the window.
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
      CHARACTER         LABEL*60, XAXIS*(*)
      REAL              XMIN, XMAX, YMIN, YMAX, XOMIN, XOMAX
      DOUBLE PRECISION  DAY0
C ----------------------------------------------------------------------
C
C     Save PGPLOT calling attribute
C
      CALL PGSAVE

C
C     UT Time Offset
C
      IF( POFVAL(1) .NE. 0 .AND. XAXIS .EQ. 'UT' ) THEN
C
C        Set the new values for Local Time
C
         XOMIN = XMIN + ( 3600.D0 * POFVAL(1) )
         XOMAX = XMAX + ( 3600.D0 * POFVAL(1) )
C
C        Set the Local Time Label
C
         CALL PLTLAB( 'LT', DAY0, POFVAL(1), LABEL )
C
C        If the minor limits is less than 0 add 24 hours
C        to avoid negative time
C
         IF( XOMIN .LT. 0 ) then 
            XOMIN = XOMIN + 86400.D0
            XOMAX = XOMAX + 86400.D0
         END IF
C
C        Change windows limits
C
         CALL PGSWIN( XOMIN, XOMAX, YMIN, YMAX )
         CALL PGTBOX( 'BCMTSYZXH', 0.0, 0, 'BCTS', 0.0, 0 )
C
C        Write the Label on Top
C
         CALL PGSCI( 2 )
         CALL PGSCH( 1.1 )
         CALL PGMTXT( 'T', 1.7, 0.5, 0.5, LABEL)
C
C        Restore The regular axis window and limits
C
         CALL PGSWIN( XMIN, XMAX, YMIN, YMAX )
      END IF

C
      CALL PGUNSA
      RETURN
      END
