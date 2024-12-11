      SUBROUTINE PLCATA( SCREEN, COLOR, XMIN, XMAX, YMIN, YMAX, MODE )
C
C     Routine for SCHED that plots source positions in RA - Dec.
C     and calibrators
C
      INCLUDE 'sched.inc'
      INCLUDE 'srlist.inc'
      INCLUDE 'plot.inc'
C
      INTEGER           MODE, ISRC, SL, LEN1, CCOL, CSYM
      INTEGER           LABSIZ, LINSIZ
      LOGICAL           SCREEN, COLOR, VALID
      REAL              XPT, YPT, R, DECM, RAM
      REAL              XMIN, XMAX, YMIN, YMAX
      REAL              XB(4), YB(4)
C -------------------------------------------------------------------
C
C     Color the background.
C
      IF( COLOR ) THEN
         IF( SCREEN ) THEN
            LINSIZ = PLYLW(1)
            LABSIZ = PLYAW(1)
         ELSE
            LINSIZ = PLYLW(2)
            LABSIZ = PLYAW(2)
         END IF
      END IF
C
C     Radius for Calibrators check
C
      IF( MODE .EQ. 1 ) THEN
         R = ( YMAX - YMIN ) / 2.0
         DECM = ( ( ( YMIN + YMAX ) / 3600.0 ) / 2.0 ) * RADDEG
         RAM  = ( ( XMIN + XMAX ) / 2.0 ) * 15.0 * COS( DECM )
      END IF
C
C     Loop over all sources in the catalog.
C
      DO 100 ISRC = 1, SRLN
C
C        Do not plot Sched Sources
C
         IF( SRLUSED(ISRC) ) GOTO 100
C
C
C        Select color based on the calcode
C
         IF( SRLCALC(ISRC) .EQ. 'Y' ) THEN
            CCOL = PLYCT(3,1)
            CSYM = PLYCT(3,2)
         ELSE IF( SRLCALC(ISRC) .EQ. 'N' ) THEN
            CCOL = PLYCT(4,1)
            CSYM = PLYCT(4,2)
         ELSE IF( SRLCALC(ISRC) .EQ. 'M' ) THEN
            CCOL = PLYCT(5,1)
            CSYM = PLYCT(5,2)
         ELSE IF( SRLCALC(ISRC) .EQ. 'V' ) THEN
            CCOL = PLYCT(6,1)
            CSYM = PLYCT(6,2)
         ELSE
            CCOL = 2
            CSYM = 2
         END IF
         CALL PGSCI( CCOL )
C
C        Draw data.
C
         XPT = SRLRA(ISRC) * 3600.D0 / RADHR
         YPT = SRLDEC(ISRC) * 3600.D0 / RADDEG
         IF( XPT .GE. XMIN .AND. XPT .LE. XMAX .AND. 
     1       YPT .GE. YMIN .AND. YPT .LE. YMAX ) THEN
C
C           Calibrators check
C
            IF( MODE .EQ. 1 ) THEN
               CALL PLCKRD( XPT, YPT, R, DECM, RAM, VALID )
               IF( .NOT. VALID ) GO TO 100
            END IF
C   
            CALL PGSCH( 1.0 )
            CALL PGSLW( LINSIZ )
            CALL PGPT( 1, XPT, YPT, CSYM )
C
C           Write label only if selected
C
            IF( PRDNAM ) THEN
               CALL PGSCH( 0.6 )
C
C              Check if the bounding box string is less than max
C              X axis. If not plot the source name before the
C              source mark.
C
               CALL PGSLW( LABSIZ )
               SL = LEN1( SRLNAME(ISRC) )
               CALL PGQTXT( XPT, YPT, 0.0, 0.0, 
     1                      'X'//SRLNAME(ISRC)(1:SL), XB, YB )
               YPT = YB(1) - ( YB(2) - YB(1) )
C               IF( XB(4) .GE. XMAX ) THEN
               IF( XB(4) .LE. XMIN ) THEN
                  XPT = XPT - ( XB(4) - XB(1) )
                  CALL PGTEXT( XPT, YPT, SRLNAME(ISRC)(1:SL) )
               ELSE
                  CALL PGTEXT( XPT, YPT, '  '//SRLNAME(ISRC) )
               END IF
            END IF
         END IF
C
 100   CONTINUE
C
C      CALL PGEBUF
C
      RETURN
      END
