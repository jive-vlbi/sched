      SUBROUTINE PLMENU( ID )
C
      INCLUDE 'plot.inc'
C
C     Decorate the CONTROL Panel and get the mouse input that is
C     returned in ID
C
C     To add or modify the options in the MENU Panel, check also the
C     INCLUDE file 'plot.inc' and the soubroutine 'plinit.f'
C
      INTEGER      ID, I, JUNK, REV, PGCURS
      REAL         XX, YY
      CHARACTER    CH
C
      DATA XX/0.5/, YY/0.5/
C ----------------------------------------------------------------------
C
C     Save PGPLOT calling attribute
C
      CALL PGSAVE
C
C     Title
C
      CALL PLBORD( 0.0, 0.20, 0.80, 1.0, PPNCOL(5), 1 )
      CALL PGSCI( PPNCOL(6) )
      CALL PGSCH( 1.8 )
      CALL PGSLW( 5 )
      CALL PGPTXT(0.025, 0.885, 0.0, 0.0, 'SCHED')
C
C     Display Menu buttons.
C
      DO 10 I=1,PMNBTM
         REV = PPNCOL(1)
         IF( PMNBCK(1) .EQ. I .AND. PMNACT(I) .EQ. 1 ) REV = PPNCOL(5)
         CALL PLBUTA( PMNBUT(1,I), PMNBUT(2,I), PMNBUT(3,I),
     1                PMNBUT(4,I), PMNVAL(I), REV )
 10   CONTINUE
C
C     Restore PGPLOT calling attributes
C
      CALL PGUNSA
C
C     Display the active options area.
C     If the active area is different from selected area
C     clear the area
C
 100  IF( PMNBCK(1) .NE. PMNBCK(2) ) THEN
          PMNBCK(2) = PMNBCK(1)
          CALL PLBORD( 0.25, 1.0, 0.0, 1.0, 0, 1 ) 
      END IF
C
C     FILES area
C
      IF( PMNBCK(1) .EQ. 3 ) THEN
         CALL PLFILE
C
C     AXIS area
C
      ELSE IF( PMNBCK(1) .EQ. 4 ) THEN
         CALL PLAXIS( 1 )
C
C     STATION/SOURCE area
C
      ELSE IF( PMNBCK(1) .EQ. 5 ) THEN
         CALL PLSTAS
C
C     OPTIONS area
C
      ELSE IF( PMNBCK(1) .EQ. 6 ) THEN
         CALL PLOPTS
      END IF
C
C     Menu input.
C
 200  JUNK = PGCURS(XX, YY, CH)
      IF (ICHAR(CH).EQ.0) GOTO 500
C
C Find which Menu button and highlight it
C
      DO 20 I=1,PMNBTM
         IF( PMNBUT(1,I).LE.XX .AND. PMNBUT(2,I).GE.XX .AND.
     1       PMNBUT(3,I).LE.YY .AND. PMNBUT(4,I).GE.YY ) THEN
            IF( PMNBCK(1) .EQ. I ) THEN
               GOTO 200
            END IF
C
            IF( PMNACT(I) .EQ. 1 ) THEN
               CALL PLBUTA( PMNBUT(1,PMNBCK(2)), PMNBUT(2,PMNBCK(2)),
     1                      PMNBUT(3,PMNBCK(2)), PMNBUT(4,PMNBCK(2)),
     2                      PMNVAL(PMNBCK(2)), PPNCOL(1) )
               CALL PLBUTA( PMNBUT(1,I), PMNBUT(2,I), PMNBUT(3,I),
     1                      PMNBUT(4,I), PMNVAL(I), PPNCOL(5) )
               PMNBCK(1) = I
               GOTO 100
            END IF
            CALL PLBUTA( PMNBUT(1,I), PMNBUT(2,I), PMNBUT(3,I),
     1                   PMNBUT(4,I), PMNVAL(I), -1 )
            ID = I 
            RETURN
         END IF
 20   CONTINUE
C
C     Area Input
C
      CALL PLSINP( XX, YY, CH, ID )
      IF( ID .GT. 0) RETURN
C
C     Loop
C
      GOTO 200
C
C     Return if Error
C
 500  ID = 0
      RETURN
      END
