      SUBROUTINE PLOTRD( SCREEN, COLOR, RXMIN, RXMAX, RYMIN, RYMAX )
C
      INCLUDE 'plot.inc'
C
C     Routine for SCHED that plots source positions in RA - Dec.
C
C
      CHARACTER         CH
      LOGICAL           SCREEN, COLOR, PLSSUN
      INTEGER           COLORS(9), I, JUNK, REV, PGCURS
      INTEGER           ID, LLEN, LEN1, PW, HID, HSID
      REAL              XW(2), YW(2), XX, YY
      DOUBLE PRECISION  RXMIN, RXMAX, RYMIN, RYMAX
      DOUBLE PRECISION  OXMIN, OXMAX, OYMIN, OYMAX
      DOUBLE PRECISION  CXMIN, CXMAX, CYMIN, CYMAX
C
      DATA XX/0.5/, YY/0.5/, PW/0/
C -------------------------------------------------------------------
C      write(*,*) 'plotrd starting.'
C
C     Save original limits for Reset operation
C
      OXMIN = RXMIN
      OXMAX = RXMAX
      OYMIN = RYMIN
      OYMAX = RYMAX
C
C     Set colors scheme to draw control buttons
C
      COLORS(1) = 15
      COLORS(2) = 0
      COLORS(3) = 1
      COLORS(4) = 0
      COLORS(5) = 11
      COLORS(6) = 1
      COLORS(7) = 1
      COLORS(8) = 0
      COLORS(9) = 14
C
C     Set the label for the switch buttons
C
      PLSSUN = PXYSUN
      DO 10 I=1,PRDBTN
         IF( PRDVAL(1,I)(1:3) .EQ. 'SUN' .AND. PXYSUN ) THEN
            PRDLAB(I) = 2
         ELSE
            PRDLAB(I) = 1
         END IF
 10   CONTINUE
C
      IF( POPBCK .NE. 5 ) CALL PGPAGE
      CALL PGSAVE
C
C     Set Window coordinate
C
      CALL PGSVP( 0.0, 1.0, 0.0, 1.0 )
      CALL PGSWIN( 0.0, 1.0, 0.0, 1.0 )
C
C     If not in display mode print identification and
C     use zoomed limits if presents
C
      IF( .NOT. SCREEN ) THEN
         IF( PRZOOM .OR. PRDWIN .OR. PRDCAL ) THEN
            RXMIN = PZMVAL(1)
            RXMAX = PZMVAL(2)
            RYMIN = PZMVAL(3)
            RYMAX = PZMVAL(4)
         END IF
      ELSE
            PRZOOM = .FALSE.
            PRDWIN = .FALSE.
            PRDCAL = .FALSE.
            PRDCAT = .FALSE.
            PRDNAM = .FALSE.
            PRDAXI = .TRUE.
            PRZOFX = .FALSE.
            PRDBCK = 0
            PCALCK = 0
            HID = 0
            HSID = 0
      END IF
C
C     Display Control buttons.
C
100   IF( POPBCK .NE. 5 ) CALL PGPAGE
      IF( .NOT. SCREEN ) CALL PGIDEN
C
      IF( SCREEN .AND. POPBCK .NE. 5 ) THEN
         DO 110 I=1,PRDBTN
            REV = 0
            IF( PRDBCK .EQ. I .AND. PRDACT(I) .EQ. 1 ) REV = 2
            IF( PRDCAL .AND. PRDDIS(I) .EQ. 1 ) REV = 3
            LLEN = LEN1( PRDVAL(PRDLAB(I),I) )
            CALL PLBUTG( PRDBUT(1,I), PRDBUT(2,I), PRDBUT(3,I),
     1                   PRDBUT(4,I), PRDVAL(PRDLAB(I),I), LLEN,
     2                   COLORS, REV )
 110     CONTINUE
C
C        Display the button help at the bottom of window
C
         CALL PLBHLP( HID, HSID, 0, 1, 1.0 )
C
      END IF
C
C     Display Selected Viewport
C
      IF( PRDCAL ) THEN
C
C        Set RADEC Button
C
         IF( PCALCK .EQ. 0 .AND. SCREEN .AND. POPBCK .NE. 5 ) THEN
            PRDLAB(PRDBCK) = 2
            REV = 0
            LLEN = LEN1( PRDVAL(PRDLAB(PRDBCK),PRDBCK) )
            CALL PLBUTG( PRDBUT(1,PRDBCK), PRDBUT(2,PRDBCK),
     1                   PRDBUT(3,PRDBCK), PRDBUT(4,PRDBCK),
     2                   PRDVAL(PRDLAB(PRDBCK),PRDBCK), LLEN,
     3                   COLORS, REV )
            CALL PLBHLP( HID, HSID, 1, 1, 1.0 )
            PRDBCK = 0
         END IF
C
         CALL PLCALB( SCREEN, COLOR, RXMIN, RXMAX, RYMIN, RYMAX )
C
      ELSE
C
         CALL PLRDPL( SCREEN, COLOR, RXMIN, RXMAX, RYMIN, RYMAX )
C
      END IF
C
      IF( .NOT. SCREEN .OR. POPBCK .EQ. 5 ) RETURN
C
C     Reset Window coordinate, Button State and loop over Control
C
      CALL PGSVP( 0.0, 1.0, 0.0, 1.0 )
      CALL PGSWIN( 0.0, 1.0, 0.0, 1.0 )
C
 200  JUNK = PGCURS( XX, YY, CH )
C
      ID = 0
      DO 210 I=1,PRDBTN
         IF( PRDBUT(1,I).LE.XX .AND. PRDBUT(2,I).GE.XX .AND.
     1       PRDBUT(3,I).LE.YY .AND. PRDBUT(4,I).GE.YY ) THEN
C
C           If button is disabled do nothing
C
            IF( PRDCAL .AND. PRDDIS(I) .EQ. 1 ) GOTO 210
C
C           Reset the old active button
C
            IF( PRDBCK .NE. I .AND. PRDACT(I) .EQ. 1 ) THEN
               REV = 0
               IF( PRDBCK .NE. 0 ) THEN
                  LLEN = LEN1( PRDVAL(PRDLAB(PRDBCK),PRDBCK) )
                  CALL PLBUTG( PRDBUT(1,PRDBCK), PRDBUT(2,PRDBCK),
     1                         PRDBUT(3,PRDBCK), PRDBUT(4,PRDBCK),
     2                         PRDVAL(PRDLAB(PRDBCK),PRDBCK), LLEN,
     3                         COLORS, REV )
               END IF
               PRDBCK = I
            ENDIF
C
C           Set new active or pressed button
C
            IF( PRDCAL .AND. PRDDIS(I) .EQ. 1 ) THEN
               REV = 3
            ELSE IF( PRDACT(I) .EQ. 1 ) THEN
               REV = 2
            ELSE
                 REV = 1
            END IF
            ID = I
            LLEN = LEN1( PRDVAL(PRDLAB(ID),ID) )
            CALL PLBUTG( PRDBUT(1,ID), PRDBUT(2,ID), PRDBUT(3,ID),
     1                   PRDBUT(4,ID), PRDVAL(PRDLAB(ID),ID), LLEN,
     2                   COLORS, REV )
         END IF
 210  CONTINUE
C
C     ZOOM Button
C
      IF( ID .EQ. 1 ) THEN
         PRZOFX = .FALSE.
         HID = ID
         HSID = PZOOCK
         CALL PLBHLP( HID, HSID, 1, 1, 1.0 )
C
C     WINDOW or ZOOM FIXED Button
C
C      ELSE IF( ID .EQ. 2 .AND. .NOT. PRDCAL ) THEN
      ELSE IF( ID .EQ. 2 ) THEN
         HID = ID
         HSID = 0
         IF( PRDCAL ) THEN
            HID = 1
            HSID = PZOOCK
            PRZOFX = .TRUE.
         END IF
         CALL PLBHLP( HID, HSID, 1, 1, 1.0 )
C
C     CALIB / RADEC Button
C
      ELSE IF( ID .EQ. 3 ) THEN
         IF( PRDCAL ) THEN
            PRDLAB(PRDBCK) = 1
            PRDLAB(4) = 1
            PRDLAB(2) = 1
            PRDBCK = 0
            PRDCAL = .FALSE.
            PRDAXI = .TRUE.
            PRZOFX = .FALSE.
            PCALCK = 0
            PW = 0
            HID = 0
            HSID = 0
            RXMIN = CXMIN
            RXMAX = CXMAX
            RYMIN = CYMIN
            RYMAX = CYMAX
            GOTO 100
         ELSE
            HID = ID
            HSID = 1
            CALL PLBHLP( HID, HSID, 1, 1, 1.0 )
         END IF
C
C     CATALOG or AXIS Button
C
C      ELSE IF( ID .EQ. 4 .AND. .NOT. PRDCAL ) THEN
      ELSE IF( ID .EQ. 4 ) THEN
         IF( .NOT. PRDCAL ) THEN
            IF( PRDCAT ) THEN
               PRDCAT = .FALSE.
               PRDLAB(ID) = 1
            ELSE
               PRDCAT = .TRUE.
               PRDLAB(ID) = 2
            END IF
         ELSE
            IF( PRDAXI ) THEN
               PRDAXI = .FALSE.
               PRDLAB(ID) = 4
            ELSE
               PRDAXI = .TRUE.
               PRDLAB(ID) = 3
            END IF
         END IF
         GOTO 100
C
C     CATALOG LABEL ON / OFF Button
C
      ELSE IF( ID .EQ. 5 ) THEN
         IF( PRDNAM ) THEN
            PRDNAM = .FALSE.
            PRDLAB(ID) = 1
         ELSE
            PRDNAM = .TRUE.
            PRDLAB(ID) = 2
         END IF
         GOTO 100
C
C     SUN ON / OFF Button
C
      ELSE IF( ID .EQ. 6 ) THEN
         IF( PXYSUN ) THEN
            PXYSUN = .FALSE.
            PRDLAB(ID) = 1
         ELSE
            PXYSUN = .TRUE.
            PRDLAB(ID) = 2
         END IF
         GOTO 100
C
C     RESET Button
C
      ELSE IF( ID .EQ. 7 ) THEN
         RXMIN = OXMIN
         RXMAX = OXMAX
         RYMIN = OYMIN
         RYMAX = OYMAX
C
         PRZOOM = .FALSE.
         PRDCAL = .FALSE.
         PRDWIN = .FALSE.
         PRDCAT = .FALSE.
         PRDNAM = .FALSE.
         PRZOFX = .FALSE.
         PRDAXI = .TRUE.
         PW = 0
         PRDBCK = 0
         PZOOCK = -1
         PCALCK = 0
         HID = 0
         HSID = 0
C
         PXYSUN = PLSSUN
         DO 310 I=1,PRDBTN
            IF( PRDVAL(1,I)(1:3) .EQ. 'SUN' .AND. PXYSUN ) THEN
               PRDLAB(I) = 2
            ELSE
               PRDLAB(I) = 1
            END IF
 310     CONTINUE
C
         GOTO 100
C
C     DONE Button
C
      ELSE IF( ID .EQ. 8 ) THEN
         GOTO 900
C
C     ZOOM WINDOW CALIB operation
C
      ELSE IF( PZMWIN(1).LE.XX .AND. PZMWIN(2).GE.XX .AND.
     1         PZMWIN(3).LE.YY .AND. PZMWIN(4).GE.YY ) THEN
C
C        ZOOM Coordinate
C
         IF( PRDBCK .EQ. 1 .OR. PRZOFX ) THEN
            CALL PLZOOM( XX, YY, CH, RXMIN, RXMAX, RYMIN, RYMAX )
            PRZOOM = .TRUE.
            PRDWIN = .FALSE.
            PW = 0
            HSID = PZOOCK
            GOTO 100
C
C        WINDOW Coordinate
C
         ELSE IF( PRDBCK .EQ. 2 ) THEN
            PW = PW + 1
            XW(PW) = XX
            YW(PW) = YY
            IF( PW .EQ. 2 ) THEN
               CALL PLRDWN( XW, YW, RXMIN, RXMAX, RYMIN, RYMAX )
               PRZOOM = .FALSE.
               PRDWIN = .TRUE.
               PW = 0
               GOTO 100
            END IF
            GOTO 200
C
C        CALIB Coordinate
C
         ELSE IF( PRDBCK .EQ. 3 ) THEN
            IF( PCALCK .EQ. 0 ) THEN
               CXMIN = RXMIN
               CXMAX = RXMAX
               CYMIN = RYMIN
               CYMAX = RYMAX
               PRZOOM = .FALSE.
               PRDCAL = .TRUE.
               PRDWIN = .FALSE.
               PRDAXI = .TRUE.
               PRZOFX = .FALSE.
               PW = 0
               PCALXY(1) = XX
               PCALXY(2) = YY
               PRDLAB(4) = 3
               PRDLAB(2) = 2
               HID = 3
               HSID = 2
               GOTO 100
            ELSE
               GOTO 200
            END IF
C
         END IF
C
      END IF
C
C     Input Loop
C
      GOTO 200
C
C     Return
C
 900  CONTINUE
C
C     Reset Button to normal state
C
      IF( ID .GT. 0 .AND. PRDACT(ID) .EQ. 0 ) THEN
         REV = 0
         LLEN = LEN1( PRDVAL(PRDLAB(ID),ID) )
         CALL PLBUTG( PRDBUT(1,ID), PRDBUT(2,ID), PRDBUT(3,ID),
     1                PRDBUT(4,ID), PRDVAL(PRDLAB(ID),ID), LLEN,
     2                COLORS, REV )
      END IF
C
C     If zoomed or windowed or calib save limit values
C
      IF( PRZOOM .OR. PRDWIN .OR. PRDCAL ) THEN
         PZMVAL(1) = RXMIN
         PZMVAL(2) = RXMAX
         PZMVAL(3) = RYMIN
         PZMVAL(4) = RYMAX
      END IF
C
      CALL PGUNSA
      RETURN
      END
