      SUBROUTINE PLAXSY()
C
C     Routine for sched that plot the Y axis value object
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
      CHARACTER         STRING*14, ANUM*2, SNUM*3, MESSAG*80
      INTEGER           DNUM
      INTEGER           AP, EP, K, I, J, N
      REAL              XL, XR, YT, YB
C ----------------------------------------------------------------------
C     
C     Set Pointers to Axis Type
C
      AP = 2
      EP = 3
      K = PXYBCK(AP)
      IF( PXYTYP(K) .EQ. 'Wv' .OR. PXYTYP(K) .EQ. 'Km' ) THEN
         PXYBIG = .TRUE.
      ELSE
         PXYBIG = .FALSE.
      END IF
C
C     Clear a Coordinate Area
C
      XL = PPSDIM(1)   - 0.01
      XR = PXSBXM(2,1) + 0.01
      YB = PYBTXT(4,1) + 0.005
      YT = PYTTXT(3,1) - 0.005
      CALL PGSCI( PPNCOL(1) )
      CALL PGRECT( XL, XR, YB, YT )
C
      CALL PGSLW( 5 )
      CALL PGSCF( 1 )
      CALL PGSCH( PPNDIM(3) )
      CALL PGSCI( PPNCOL(2) )
C
C     If plot type equal UPTIME ( Y Axis fixed to "Ant" )
C     Write messages and return
C
      IF( PXYTYP(K) .EQ. 'Ant' ) THEN
C
         PYANTN = .TRUE.
         YT = PYBTXT(3,1)
         DNUM = PSTNUM
         CALL PGNUMB( DNUM, 0, 1, ANUM, N )
         DNUM = PSONUM
         CALL PGNUMB( DNUM, 0, 1, SNUM, N )
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
C     Y Axis Labels
C
      YT = PYBTXT(3,1) + 0.025
      CALL PGPTXT( XL, YT, 0.0, 0.0, PXSTYP(K) )
      YT = PYTTXT(3,1) + 0.025
      CALL PGPTXT( XL, YT, 0.0, 0.0, PXSTYP(K) )
C
C     Y Axis Values
C
      IF( PXYBIG ) THEN
C
         IF( PXSVAL(K,3,1) .GT. (10**8) ) THEN
            CALL PLNUMB( PXSVAL(K,3,1), STRING )
         ELSE
            CALL PGNUMB( PXSVAL(K,3,1), 0, 1, STRING, J )
         END IF
         CALL PLSTXT( PYBTXT(1,1), PYBTXT(2,3), PYBTXT(3,1),
     1                PYBTXT(4,1), STRING, 1, .FALSE. )
C
         IF( PXSVAL(K,4,1) .GT. (10**8) ) THEN
            CALL PLNUMB( PXSVAL(K,4,1), STRING )
         ELSE
            CALL PGNUMB( PXSVAL(K,4,1), 0, 1, STRING, J )
         END IF
         CALL PLSTXT( PYTTXT(1,1), PYTTXT(2,3), PYTTXT(3,1),
     1                PYTTXT(4,1), STRING, 1, .FALSE. )
C
      ELSE IF( PXYONE ) THEN
C
         CALL PGNUMB( PXSVAL(K,3,1), 0, 1, STRING, J )
         CALL PLSTXT( PYBTXT(1,3), PYBTXT(2,3), PYBTXT(3,1),
     1                PYBTXT(4,1), STRING, 1, .FALSE. )
         CALL PGNUMB( PXSVAL(K,4,1), 0, 1, STRING, J )
         CALL PLSTXT( PYTTXT(1,3), PYTTXT(2,3), PYTTXT(3,1),
     1                PYTTXT(4,1), STRING, 1, .FALSE. )
C
      ELSE
C
         DO 70 I=1,3
            CALL PGNUMB( PXSVAL(K,3,I), 0, 1, STRING, J )
            CALL PLSTXT( PYBTXT(1,I), PYBTXT(2,I), PYBTXT(3,I),
     1                   PYBTXT(4,I), STRING, 1, .FALSE. )
            CALL PGNUMB( PXSVAL(K,4,I), 0, 1, STRING, J )
            CALL PLSTXT( PYTTXT(1,I), PYTTXT(2,I), PYTTXT(3,I),
     1                   PYTTXT(4,I), STRING, 1, .FALSE. )
 70      CONTINUE
C
      END IF
C
C     Y Axis Buttons
C
      DO 80 I=3,4
         CALL PLBUTC( PXSDEF(1,I), PXSDEF(2,I), PXSDEF(3,I),
     1                PXSDEF(4,I), 'X', 0, 0 )
         CALL PLBUTC( PXSBXM(1,I), PXSBXM(2,I), PXSBXM(3,I),
     1                PXSBXM(4,I), ' ', PXSEXP(I), 1 )
 80   CONTINUE
C
C     Y Axis Sign
C
      CALL PLAXSS( AP, EP )
C
 500  CONTINUE
      RETURN
      END
