      SUBROUTINE PLEDIT( X1, X2, Y1, Y2, STRING )
C
C     Routine for sched that edit a string in a 3D Text Area.
C
      INCLUDE 'plot.inc'
C
      CHARACTER*(*) STRING
      CHARACTER     OSTRIN*80, BUFFER*80, CH
      INTEGER       I, J, K, LNT, LNR, IER, ICH
      INTEGER       WFL, IFL, PCH, CPOS, OPCH, OCPOS
      INTEGER       JUNK, PGCURS, LEN1, CCOL
      REAL          X1, X2, Y1, Y2, XL, YL, XX, YY
      REAL          XB(4), YB(4), XB1(4), YB1(4)
      REAL          XC1, XC2, YC1, YC2, XCO
C ----------------------------------------------------------------------
C
C     Save PGPLOT calling attribute
C
      CALL PGSAVE
C
C     Set some string pointers and save original string
C
      LNT    = LEN( STRING )
      LNR    = LEN1( STRING )
      PCH    = LNR
      CPOS   = LNR
      OPCH   = LNR
      OCPOS  = LNR
      OSTRIN = STRING
      BUFFER = STRING

C
C     Calculate the string bounding box and the ofset from
C     the Text Area Y Border
C
      CALL PGSLW( 5 )
      CALL PGSCF( 1 )
      CALL PGSCH( PPNDIM(3) )
      YL = Y1 + 0.025 
      XL = X1 + 0.015
C
C     Calculate the cursor bounding box: if the string less than
C     max lenght after last character else, at last character.
C
      CALL PGQTXT( XL, YL, 0.0, 0.0, STRING, XB, YB )
      CALL PGQTXT( XL, YL, 0.0, 0.0, STRING(1:LNR-1), XB1, YB1 )
      YC1 = YB(1)
      YC2 = YB(2)
      XCO = XB(3) - XB1(3)
      IF( LNR .LE. LNT ) THEN
         XC1 = XB(3)
         XC2 = XC1 + XCO
      ELSE
         XC1 = XB1(3)
         XC2 = XB(3)
      END IF
C
C     Set the Hardware cursor position
C
      XX = XC1 + ( (XC2 - XC1 ) / 2.0 )      
      YY = YC1 - 0.05
C
C     Redraw the text area with a red border and
C     rewrite the string
C
      CALL PGSLS( 1 )
      CALL PGSLW( 2 )
      CALL PGSCI( 1 )
      CALL PGRECT( X1, X2, Y1, Y2 )
      CALL PGSCI( PPNCOL(8) )
      CALL PGSFS( 2 )
      CALL PGRECT( X1, X2, Y1, Y2 )
      CALL PGSCI( PPNCOL(2) )
      CALL PGPTXT(XL, YL, 0.0, 0.0, STRING)
C
C     Draw the initial text cursor
C
      CCOL = PPNCOL(7)
      CALL PGSCI( CCOL )
      CALL PGRECT( XC1, XC2, YC1, YC2 )
C
C     Text Input Loop
C     Exit on ERROR ( 0 ) or ESC ( 27 )
C
      WFL = 0
      IFL = 0
 200  JUNK = PGCURS(XX, YY, CH)
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
C     Turn insert ON/OFF ( CTRL I )
C
      ELSE IF( ICH .EQ. 9 ) THEN
         WFL = 1
         IF( IFL .EQ. 0 ) THEN
            IFL = 1
            CCOL = PPNCOL(8)
         ELSE
            IFL = 0
            CCOL = PPNCOL(7)
         END IF
C
C     Move cursor to HOME  (  CTRL A ) 
C
      ELSE IF( ICH .EQ. 1 ) THEN
         WFL = 1
         CPOS = 0
C
C     Move cursor to END (  CTRL E ) 
C
      ELSE IF( ICH .EQ. 5 ) THEN
            WFL = 1
            CPOS = PCH
C
C     Move cursor left (  CTRL B ) 
C
      ELSE IF( ICH .EQ. 2 ) THEN
         IF( CPOS .GT. 0 ) THEN
            WFL = 1
            CPOS = CPOS - 1
         END IF
C
C     Move cursor Right ( CTRL F ) 
C
      ELSE IF( ICH .EQ. 6 ) THEN
         IF( CPOS .LT. PCH ) THEN
            WFL = 1
            CPOS = CPOS + 1
         END IF
C
C     Delete left character ( Backspace/CTRL H/Del )
C
      ELSE IF( ICH .EQ. 8 .OR. ICH .EQ. 127 ) THEN
         IF( PCH .GT. 0 .AND. CPOS .EQ. PCH ) THEN
            WFL  = 1
            BUFFER(PCH:PCH) = ' '
            PCH  = PCH - 1
            CPOS = CPOS - 1
         ELSE IF( CPOS .GT. 0 ) THEN
            WFL = 1
            DO 10 I = CPOS, PCH 
                  K = I + 1
                  BUFFER(I:I) = BUFFER(K:K)
 10         CONTINUE
            PCH = PCH - 1
            CPOS = CPOS - 1
         ENDIF
C
C     Delete character under the cursor ( CTRL D )
C
      ELSE IF( ICH .EQ. 4 .AND. CPOS .LT. PCH) THEN
            WFL  = 1
            J = CPOS + 1
            DO 12 I = J, PCH 
               K = I + 1
               BUFFER(I:I) = BUFFER(K:K)
 12         CONTINUE
            BUFFER(I:I) = ' '
C
C     Delete all characters after cursor CTRL K
C
      ELSE IF( ICH .EQ. 11 ) THEN
            K = CPOS + 2
            IF( CPOS .LE. PCH ) THEN
               WFL  = 1
               DO 14 I = K, PCH 
                  BUFFER(I:I) = ' '
 14            CONTINUE
               PCH = K - 1
            END IF
C
C     Delete  entire string CTRL U
C
      ELSE IF( ICH .EQ. 21 ) THEN
         IF( PCH .GT. 0 ) THEN
            WFL    = 1
            BUFFER = ' '
            PCH    = 0
            CPOS   = 0
            IFL    = 0
         ENDIF
C
C     Add a legal character if room or overwrite
C
      ELSE IF( ( ICH .GE. 45 .AND. ICH .LE. 57  ) .OR.
     1         ( ICH .GE. 65 .AND. ICH .LE. 90  ) .OR.
     2         ( ICH .GE. 97 .AND. ICH .LE. 122 ) .OR.
     3           ICH .EQ. 95 .OR.  ICH .EQ. 126 ) THEN
C
        WFL  = 1
        OPCH  = PCH
        OCPOS = CPOS
C           
C       Insert character at the end
C
        IF( CPOS .EQ. PCH .AND. PCH .LT. LNT ) THEN
           PCH  = PCH + 1
           CPOS = CPOS + 1
           BUFFER(PCH:PCH) = CH
C           
C           Overwrite character at the cursor position and
C           do not move cursor
C
            ELSE IF( CPOS .NE. PCH .AND. IFL .EQ. 1 ) THEN
               J = CPOS + 1
               BUFFER(J:J) = CH
C           
C           Insert character at the cursor position
C
            ELSE IF( CPOS .NE. PCH .AND. PCH .LT. LNT ) THEN
               PCH = PCH + 1
               J = CPOS + 1
               DO 20 I = PCH, J, -1 
                  K = I - 1
                  BUFFER(I:I) = BUFFER(K:K)
 20            CONTINUE
               CPOS = CPOS + 1
               BUFFER(CPOS:CPOS) = CH
C               
        END IF
C
      END IF
C
C        Check if the width of new string and cursor is
C        less than the text area width 
C
      IF( WFL .EQ. 1 ) THEN
         CALL PGQTXT( XL, YL, 0.0, 0.0, BUFFER, XB, YB )
         IF( X2 .LE. (XB(3) + XCO) ) THEN
            WFL    = 0
            PCH    = OPCH
            CPOS   = OCPOS 
            BUFFER = STRING
         ENDIF
      ENDIF
C
C     Rewrite New String or cursor at new position
C
      IF( WFL .EQ. 1 ) THEN
C
C        Delete Cursor and string rewritting in the
C        background color.
C
         CALL PGSCI( 1 )
         CALL PGRECT( XC1, XC2, YC1, YC2 )
         CALL PGPTXT(XL, YL, 0.0, 0.0, STRING)
C
C        Rewrite string
C
         CALL PGSCI( PPNCOL(2) )
         CALL PGPTXT(XL, YL, 0.0, 0.0, BUFFER)
C
C        Rewrite cursor
C
         IF( CPOS .GT. 0 ) THEN
            CALL PGQTXT( XL, YL, 0.0, 0.0, BUFFER(1:CPOS), XB, YB )
            IF( CPOS .GT. 1 ) THEN
               CALL PGQTXT( XL, YL, 0.0, 0.0, BUFFER(1:CPOS-1),
     1                      XB1, YB1 )
               IF( CPOS .LE. LNT ) THEN
                 XC1 = XB(3)
                 XC2 = XC1 + XCO
               ELSE
                 XC1 = XB1(3)
                 XC2 = XB(3)
               END IF
            ELSE
               XC1 = XB(3)
               XC2 = XC1 + XCO
            END IF
         ELSE
            XC1 = X1 + 0.015
            XC2 = XC1 + XCO
         END IF
C
         CALL PGSCI( CCOL )
         CALL PGRECT( XC1, XC2, YC1, YC2 )
C
C        update string and flags
C
         STRING = BUFFER
         WFL    = 0
         OPCH   = PCH
         OCPOS  = CPOS
         XX = XC1 + ( (XC2 - XC1 ) / 2.0 )      
         YY = YC1 - 0.05
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
      IF( IER .EQ. 1 ) STRING = OSTRIN
C
C     Restore PGPLOT calling attributes
C
      CALL PGUNSA
C
      RETURN
      END
