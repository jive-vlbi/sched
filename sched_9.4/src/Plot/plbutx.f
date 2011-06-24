      SUBROUTINE PLBUTX( X1, X2, Y1, Y2, XL,
     1                   CSIZ, LABEL, CL, REV )
C
C     Routine for sched that plot a Text Select Button with
C     a label outside.
C
      INCLUDE 'plot.inc'
C
      CHARACTER*(*) LABEL
      CHARACTER*80  STRING
      REAL          X1, Y1, X2, Y2, XL, YL
      REAL          CSIZ, XOF, XM, YM
      INTEGER       REV, CL, LCOL
C ----------------------------------------------------------------------
C
C     Save PGPLOT calling attribute
C
      CALL PGSAVE
C
C     Set line style (full) and size to plot button borders
C
      CALL PGSLS( 1 )
      CALL PGSLW( 2 )
C
C     Draw a Button 
C
      CALL PGSCI( 1 )
      CALL PGRECT( X1, X2, Y1, Y2 )
      CALL PGSCI( 15 )
      CALL PGSFS( 2 )
      CALL PGRECT( X1, X2, Y1, Y2 )
C
C     Draw a selected mark inside the button 
C
      IF( REV .EQ. 1 ) THEN
         CALL PGSLW( 6 )
         XM = X1 + 0.008
         YM = Y1 + ((Y2 - Y1) / 2)
         CALL PGMOVE( XM, YM )
         YM = Y1 + 0.008
         XM = X1 + ((X2 - X1) / 2)
         CALL PGDRAW( XM, YM )
         XM = X2 - 0.008
         YM = Y2 - 0.008
         CALL PGDRAW( XM, YM )
      END IF
C
C     Write a label outside the button whit the specified color
C     and X offset
C
      IF( LABEL .NE. ' ' ) THEN
         STRING = LABEL
         CALL PGSLW( 2 )
         IF( CL .EQ. 0 ) THEN
            LCOL = PPNCOL(2)
         ELSE
            LCOL = CL
         END IF
C
         XOF = XL + X2
         YL  = Y1 + 0.008
C
         CALL PGSCF( 1 )
         CALL PGSCH( CSIZ )
         CALL PGSCI( LCOL )
         CALL PGPTXT(XOF, YL, 0.0, 0.0, LABEL)
      END IF
C
C     Restore PGPLOT calling attributes
C
      CALL PGUNSA
C
      RETURN
      END
