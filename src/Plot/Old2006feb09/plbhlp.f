      SUBROUTINE PLBHLP( ID, SID, MODE, LCOL, LSIZ )
C
C     Routine for sched that plot an help string in the graphic
C     window
C
      CHARACTER     LABEL*120
      INTEGER       ID, SID, MODE, LCOL
      REAL          XL, YL, LSIZ
C ----------------------------------------------------------------------
C
C     Select the Help String
C
      IF( ID .EQ. 1 ) THEN
C
C        Zoom Help
C
         IF( SID .EQ. -1 ) THEN
            LABEL = 'Press the left button to Zoom IN or '//
     1              'the middle button to CENTER'
         ELSE IF( SID .EQ. 1 ) THEN
            LABEL = 'Press the right button to Zoom OUT or '//
     1              'the middle button to CENTER'
         ELSE
            LABEL = 'Press the left button to Zoom IN or '//
     1              'the right button to Zoom OUT or '//
     2              'the middle button to CENTER'
         END IF
C
      ELSE IF( ID .EQ. 2 ) THEN
C
C        Window Help
C
         LABEL = 'Click in the Top Left and Bottom Right '//
     1           'corners of the coordinate window'
C
      ELSE IF( ID .EQ. 3 ) THEN
C
C        Calibrators Help
C
         IF( SID .EQ. 1 ) THEN
            LABEL = 'Click on the source to display calibrators' 
         ELSE
            LABEL = 'Press RADEC Button to return to the RD Plot'
         END IF
C
      ELSE
C
C        Default Help
C
         LABEL = 'Press the DONE button to return to '//
     1           'the main panel'
C
      END IF
C
C     Save PGPLOT calling attribute
C
      CALL PGSAVE
C
C     If MODE = 1 clear help area
C
      IF( MODE .EQ. 1 ) THEN
         YL = 0.01 + ( LSIZ * ( 1.0 / 40.0 ) )
         CALL PGSCI( 0 )
         CALL PGRECT( 0.0, 1.0, 0.00, YL )
      END IF
C
C     Write a help text at the bottom of graphic window
C
      IF( LABEL .NE. ' ' ) THEN
         CALL PGSLW( 5 )
         CALL PGSCF( 1 )
         CALL PGSCH( LSIZ )
         CALL PGSCI( LCOL )
C
         XL = 0.0
         YL = 0.01
C
         CALL PGPTXT(XL, YL, 0.0, 0.0, LABEL )
C
      END IF
C
C     Restore PGPLOT calling attributes
C
      CALL PGUNSA
C
      RETURN
      END
