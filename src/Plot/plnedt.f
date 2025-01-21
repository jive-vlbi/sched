      SUBROUTINE PLNEDT( X1, X2, Y1, Y2, OCH, VAL )
C
C     Routine for sched that edit a number in a 3D Slider Area.
C
      INCLUDE 'plot.inc'
C
      CHARACTER     OCH, CH, STRING*6
      INTEGER       I, J, IER, ICH
      INTEGER       VAL, WFL, PCH
      INTEGER       JUNK, PGCURS
      REAL          X1, X2, Y1, Y2, XX, YY
      REAL          XB(4), YB(4), XL(2), YL(2), YLL
C ----------------------------------------------------------------------
C
C     Save PGPLOT calling attribute
C
      CALL PGSAVE
C
C     Set some string pointers and save original value
C
      PCH    = 1
      STRING = OCH
C
C     Calculate the string bounding box and the ofset from
C     the Text Area Y Border
C
      CALL PGSLW( 5 )
      CALL PGSCF( 1 )
      CALL PGSCH( PPNDIM(3) )
      CALL PGQTXT( X1, Y1, 0.0, 0.0, STRING(1:PCH), XB, YB )
      XL(1) = X2 - 0.01 - ( XB(4) - XB(1) )
      XL(2) = XL(1) + ( XB(4) - XB(1) )
      YL(1) = Y1 + 0.018 
      YLL   = Y1 + 0.025
      YL(2) = YL(1) + ( YB(2) - YB(1) )
C
C     Clear slider box and plot the first number
C     ( White in a Blu Box )
C
      CALL PLSTXT( X1, X2, Y1, Y2, ' ', 0, .FALSE. )
      CALL PGSCI( PPNCOL(5) )
      CALL PGRECT( XL(1), XL(2), YL(1), YL(2) )
      CALL PGSCI( PPNCOL(6) )
      CALL PGPTXT(XL(1), YLL, 0.0, 0.0, STRING(1:PCH) )
      XX = X2
      YY = Y1 - 0.05
C
C     Text Input Loop
C     Exit on ERROR ( 0 ) or ESC ( 27 )
C
 200  XX  = X2
      YY  = Y1 - 0.05
      WFL = 0
      JUNK = PGCURS(XX, YY, CH)
C
      ICH = ICHAR(CH)
C
C     Exit on ERROR ( 0 ) or ESC ( 27 )
C
      IF( ICH .EQ. 0 .OR. ICH .EQ. 27 ) THEN
         IER = 1
         GOTO 500
C
C     Return new string if CR ( 10 ) or CR ( 13 )
C
      ELSE IF( ICH .EQ. 10 .OR. ICH .EQ. 13 ) THEN
         IER = 0
         GOTO 500
C
C     Delete left character ( Backspace/CTRL H/Del )
C
      ELSE IF( ICH .EQ. 8 .OR. ICH .EQ. 127 ) THEN
         IF( PCH .GT. 0 ) THEN
            WFL  = 1
            STRING(PCH:PCH) = ' '
            PCH  = PCH - 1
         ENDIF
C
C     Add a legal character if room
C
      ELSE IF( ( ICH .GE. 48 .AND. ICH .LE. 57  ) .AND.
     1           PCH .LT. 6 ) THEN
C
        WFL  = 1
        PCH  = PCH + 1
        STRING(PCH:PCH) = CH
C
      END IF
C
C        Check if the width of new string is
C        less than the text area width 
C
      IF( WFL .EQ. 1 ) THEN
C        CALL PGQTXT( XL, YL, 0.0, 0.0, STRING(1:PCH), XB, YB )
         CALL PGQTXT( XL(1), YL(1), 0.0, 0.0, STRING(1:PCH), XB, YB )
         IF( ( X2 - X1 ) .LE. ( XB(4) - XB(1) ) ) THEN
            WFL = 0
            PCH = PCH - 1
            STRING(PCH:PCH) = ' '
         ENDIF
      ENDIF
C
C     Write New String
C
      IF( WFL .EQ. 1 ) THEN
C
C        Rewrite string
C
         CALL PGQTXT( X1, Y1, 0.0, 0.0, STRING(1:PCH), XB, YB )
         XL(1) = X2 - 0.01 - ( XB(4) - XB(1) )
         XL(2) = XL(1) + ( XB(4) - XB(1) )
         CALL PLSTXT( X1, X2, Y1, Y2, ' ', 0, .FALSE. )
         CALL PGSCI( PPNCOL(5) )
         CALL PGRECT( XL(1), XL(2), YL(1), YL(2) )
         CALL PGSCI( PPNCOL(6) )
         CALL PGPTXT(XL(1), YLL, 0.0, 0.0, STRING(1:PCH) )
C
      END IF
C
C     Loop over input
C
      GOTO 200
C
C     Exit on error or ESC typed IER = 1
C     Return The new String      IER = 0
C
 500  CONTINUE
      IF( IER .EQ. 0 ) THEN
         VAL = 0
         J   = 0
         DO 510 I = PCH, 1, -1
            VAL = VAL + ( ( ICHAR( STRING(I:I) ) - 48 ) * 10**J )
            J   = J + 1
 510     CONTINUE 
      END IF
C
C     Restore PGPLOT calling attributes
C
      CALL PGUNSA
C
      RETURN
      END
