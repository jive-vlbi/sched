      SUBROUTINE PLSTXS( X1, X2, Y1, Y2, LABEL, POS, SIGN, SYMBOL )
C
C     Routine for sched that plot a 3D Text Area.
C
      INCLUDE 'plot.inc'
C
      CHARACTER*(*) LABEL
      LOGICAL       SIGN
      INTEGER       POS, LP, SYMBOL
      REAL          X1, X2, Y1, Y2, XL, YL
      REAL          XB(4), YB(4)
C ----------------------------------------------------------------------
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
      CALL PGSCI( 1 )
      CALL PGRECT( X1, X2, Y1, Y2 )
      CALL PGSCI( 15 )
      CALL PGSFS( 2 )
      CALL PGRECT( X1, X2, Y1, Y2 )
C
C     Write a text inside the area
C     Right justified if POS equal 1
C     and without sign if SIGN equal false
C
      CALL PGSLW( 5 )
      CALL PGSCF( 1 )
      CALL PGSCH( PPNDIM(3) )
      YL = Y1 + 0.025
      XL = X1
C
      IF( LABEL .NE. ' ' ) THEN
         LP = 1
         IF( .NOT. SIGN .AND. LABEL(1:1) .EQ. '-' ) LP = 2
C
         IF( POS .EQ. 1) THEN
            CALL PGQTXT( X1, Y1, 0.0, 0.0, LABEL(LP:), XB, YB )
            XL = X2 - 0.01 - ( XB(4) - XB(1) ) 
         ELSE 
            XL = X1 + 0.015
         END IF
         CALL PGPTXT(XL, YL, 0.0, 0.0, LABEL(LP:))
      END IF
C
C     Plot symbol
C
      IF( SYMBOL .GT. 0 ) THEN
         YL = YL + 0.01
         XL = XL + 0.04
         CALL PGPT1 (XL, YL, SYMBOL)
      END IF
C
C     Restore PGPLOT calling attributes
C
      CALL PGUNSA
C
      RETURN
      END
