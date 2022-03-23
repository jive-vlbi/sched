      SUBROUTINE PLBUTS( PBTR, X1, X2, Y1, Y2, XL,
     1                   CSIZ, LABEL, CL, REV )
C
C     Routine for sched that plot a 3D Select Button with
C     the dimension of character height and with a label outside.
C
      INCLUDE 'plot.inc'
C
      CHARACTER*(*) LABEL
      REAL          X1, Y1, X2, Y2, XL, YL
      REAL          CSIZ, XOF
      INTEGER       REV, LIGHT, DARK, LCOL, CL, CLBOX, LSZ
      LOGICAL       PBTR
C ----------------------------------------------------------------------
C
C     Set Color schemes and line style/size: 
C     REV = 0, LIGHT is light and DARK is dark
C     REV = 1, LIGHT is dark  and DARK is light
C     PBTR = .TRUE. Color for Radio else Color for Select 
C
      IF( REV .EQ. 0 ) THEN
         LIGHT = PPNCOL(3)
         DARK  = PPNCOL(4)
         CLBOX = PPNCOL(1)
         LSZ   = 2
      ELSE
         LIGHT = PPNCOL(4)
         DARK  = PPNCOL(3)
         LSZ   = 3
         IF( PBTR ) THEN
            CLBOX = PPNCOL(8)
         ELSE
            CLBOX = PPNCOL(7)
         END IF
      END IF
C
C     Save PGPLOT calling attribute
C
      CALL PGSAVE
C
C     Set line style (full) and size to plot 3D shadow
C
      CALL PGSLS( 1 )
      CALL PGSLW( LSZ )
C
C     Draw a 3D Button light (white) borders
C
      CALL PGSCI( CLBOX )
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
C     Write a label outside the button whit the specified color
C     and X offset
C
      IF( LABEL .NE. ' ' ) THEN
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
