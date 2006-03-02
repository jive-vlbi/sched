      SUBROUTINE PLAXST( AXIS )
C
C     Routine for sched that plot an axis value object
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
      CHARACTER*(*)    AXIS
      CHARACTER        STRING*6, CH, ANUM*2, SNUM*3, MESSAG*80
      INTEGER          AP, EP, K, I, J, N
      REAL             XL, XR, YT, YB
C ----------------------------------------------------------------------
C     
C     Set Pointers to Axis Type
C
      IF( AXIS .EQ. 'X' ) THEN
         AP = 1
         EP = 1
      ELSE
         AP = 2
         EP = 3
      END IF
C
C     Set type pointer of the value matrix
C
      K = PXYBCK(AP)
C
C     Save PGPLOT calling attribute
C
      CALL PGSAVE
C
C     Clear a Coordinate Area
C
      XL = PPSDIM(1)
      XR = PXSBXM(2,1) + 0.01
C
      IF( AP .EQ. 1 ) THEN
         YB = PXLTXT(4,1) + 0.005
         YT = PXRTXT(3,1) - 0.005
      ELSE
         YB = PYBTXT(4,1) + 0.005
         YT = PYTTXT(3,1) - 0.005
      END IF
      CALL PGSCI( PPNCOL(1) )
      CALL PGRECT( XL, XR, YB, YT )
C
      CALL PGSLW( 5 )
      CALL PGSCF( 1 )
      CALL PGSCH( PPNDIM(3) )
      CALL PGSCI( PPNCOL(2) )
C
C     XY Axis Text Area Labels, Values and Buttons 
C
      IF( AP .EQ. 1 ) THEN
C
C        X Axis Labels
C
         YT = PXLTXT(3,1) + 0.025
         CALL PGPTXT( XL, YT, 0.0, 0.0, PXSTYP(PXYBCK(1)) )
         YT = PXRTXT(3,1) + 0.025
         CALL PGPTXT( XL, YT, 0.0, 0.0, PXSTYP(PXYBCK(1)) )
C
C        X Axis Value
C
         IF( PXYBIG ) THEN
C
            CALL PGNUMB( PXSVAL(K,1,1), 0, 1, STRING, J )
            CALL PLSTXT( PXLTXT(1,1), PXLTXT(2,3), PXLTXT(3,1),
     1                   PXLTXT(4,1), STRING, 1, .FALSE. )
            CALL PGNUMB( PXSVAL(K,2,1), 0, 1, STRING, J )
            CALL PLSTXT( PXRTXT(1,1), PXRTXT(2,3), PXRTXT(3,1),
     1                   PXRTXT(4,1), STRING, 1, .FALSE. )
C
         ELSE
C
            DO 50 I=1,3
               CALL PGNUMB( PXSVAL(K,1,I), 0, 1, STRING, J )
               CALL PLSTXT( PXLTXT(1,I), PXLTXT(2,I), PXLTXT(3,I),
     1                      PXLTXT(4,I), STRING, 1, .FALSE. )
               CALL PGNUMB( PXSVAL(K,2,I), 0, 1, STRING, J )
               CALL PLSTXT( PXRTXT(1,I), PXRTXT(2,I), PXRTXT(3,I),
     1                      PXRTXT(4,I), STRING, 1, .FALSE. )
 50         CONTINUE
C
         END IF
C
C        X Axis Buttons
C
         DO 60 I=1,2
            CALL PLBUTC( PXSDEF(1,I), PXSDEF(2,I), PXSDEF(3,I),
     1                   PXSDEF(4,I), 'X', 0, 0 )
            CALL PLBUTC( PXSBXM(1,I), PXSBXM(2,I), PXSBXM(3,I),
     1                   PXSBXM(4,I), ' ', PXSEXP(I), 1 )
 60      CONTINUE
C
C        X Axis Days
C
         IF( PXSTYP(PXYBCK(AP))(1:1) .EQ. 'd' ) THEN
            DO 65 I=1,2
               CH = CHAR( PADAYS(K,I) + 48 )
               CALL PLSTXT( PSGTXT(1,I), PSGTXT(2,I), PSGTXT(3,I),
     1                      PSGTXT(4,I), CH, 1, .FALSE. )
 65         CONTINUE
         END IF
C
      ELSE
C
C        If plot type equal UPTIME ( Y Axis fixed to "Ant" )
C        Write messages and return
C
         IF( PXYTYP(PXYBCK(AP)) .EQ. 'Ant' ) THEN
C
            PYANTN = .TRUE.
            YT = PYBTXT(3,1)
            CALL PGNUMB( PSTNUM, 0, 1, ANUM, N )
            CALL PGNUMB( PSONUM, 0, 1, SNUM, N )
            MESSAG = ANUM//' Antennas and  '//SNUM//' Sources'
            CALL PGPTXT( XL, YT, 0.0, 0.0, MESSAG )
            YT = YT - 0.05
            MESSAG = 'selected in SOURCES panel.'
            CALL PGPTXT( XL, YT, 0.0, 0.0, MESSAG )
            GO TO 500
C
         ELSE
            PYANTN = .FALSE.
         END IF
         
C
C        Y Axis Labels
C
         YT = PYBTXT(3,1) + 0.025
         CALL PGPTXT( XL, YT, 0.0, 0.0, PXSTYP(PXYBCK(2)) )
         YT = PYTTXT(3,1) + 0.025
         CALL PGPTXT( XL, YT, 0.0, 0.0, PXSTYP(PXYBCK(2)) )
C
C        Y Axis Values
C
         IF( PXYBIG ) THEN
C
            CALL PGNUMB( PXSVAL(K,3,1), 0, 1, STRING, J )
            CALL PLSTXT( PYBTXT(1,1), PYBTXT(2,3), PYBTXT(3,1),
     1                   PYBTXT(4,1), STRING, 1, .FALSE. )
            CALL PGNUMB( PXSVAL(K,4,1), 0, 1, STRING, J )
            CALL PLSTXT( PYTTXT(1,1), PYTTXT(2,3), PYTTXT(3,1),
     1                   PYTTXT(4,1), STRING, 1, .FALSE. )
C
         ELSE
C
            DO 70 I=1,3
               CALL PGNUMB( PXSVAL(K,3,I), 0, 1, STRING, J )
               CALL PLSTXT( PYBTXT(1,I), PYBTXT(2,I), PYBTXT(3,I),
     1                      PYBTXT(4,I), STRING, 1, .FALSE. )
               CALL PGNUMB( PXSVAL(K,4,I), 0, 1, STRING, J )
               CALL PLSTXT( PYTTXT(1,I), PYTTXT(2,I), PYTTXT(3,I),
     1                      PYTTXT(4,I), STRING, 1, .FALSE. )
 70         CONTINUE
C
         END IF
C
C        Y Axis Buttons
C
         DO 80 I=3,4
            CALL PLBUTC( PXSDEF(1,I), PXSDEF(2,I), PXSDEF(3,I),
     1                   PXSDEF(4,I), 'X', 0, 0 )
            CALL PLBUTC( PXSBXM(1,I), PXSBXM(2,I), PXSBXM(3,I),
     1                   PXSBXM(4,I), ' ', PXSEXP(I), 1 )
 80      CONTINUE
C
      END IF
C
C     XY Axis Sign
C
      IF( PXSTYP(PXYBCK(AP))(1:1) .EQ. 's' ) THEN
         PASIGN(AP) = .TRUE.
         DO 90 I=EP, EP+1
            CH = '+'
            IF( PXSSGN(K,I) .EQ. -1 ) CH = '-'
            CALL PLSTXT( PSGTXT(1,I), PSGTXT(2,I), PSGTXT(3,I),
     1                   PSGTXT(4,I), CH, 1, PASIGN(AP) )
 90      CONTINUE
      ELSE
         PASIGN(AP) = .FALSE.
      ENDIF
C
C     Restore PGPLOT calling attributes
C
 500  CONTINUE
      CALL PGUNSA
C
      RETURN
      END
