      SUBROUTINE PLAXSX()
C
C     Routine for sched that plot the X axis value object
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
      CHARACTER         STRING*14, CH
      INTEGER           AP, EP, K, I, J
      REAL              XL, XR, YT, YB
C ----------------------------------------------------------------------
C     
C     Set Pointers to Axis Type
C
      AP = 1
      EP = 1
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
      YB = PXLTXT(4,1) + 0.005
      YT = PXRTXT(3,1) - 0.005
      CALL PGSCI( PPNCOL(1) )
      CALL PGRECT( XL, XR, YB, YT )
C
      CALL PGSLW( 5 )
      CALL PGSCF( 1 )
      CALL PGSCH( PPNDIM(3) )
      CALL PGSCI( PPNCOL(2) )
C
C     X Axis Labels
C
      YT = PXLTXT(3,1) + 0.025
      CALL PGPTXT( XL, YT, 0.0, 0.0, PXSTYP(K) )
      YT = PXRTXT(3,1) + 0.025
      CALL PGPTXT( XL, YT, 0.0, 0.0, PXSTYP(K) )
C
C     X Axis Value
C
      IF( PXYBIG ) THEN
C
         IF( PXSVAL(K,1,1) .GT. (10**8) ) THEN
            CALL PLNUMB( PXSVAL(K,1,1), STRING )
         ELSE
            CALL PGNUMB( PXSVAL(K,1,1), 0, 1, STRING, J )
         END IF
C
         CALL PLSTXT( PXLTXT(1,1), PXLTXT(2,3), PXLTXT(3,1),
     1                PXLTXT(4,1), STRING, 1, .FALSE. )
C
         IF( PXSVAL(K,2,1) .GT. (10**8) ) THEN
            CALL PLNUMB( PXSVAL(K,2,1), STRING )
         ELSE
            CALL PGNUMB( PXSVAL(K,2,1), 0, 1, STRING, J )
         END IF
         CALL PLSTXT( PXRTXT(1,1), PXRTXT(2,3), PXRTXT(3,1),
     1                   PXRTXT(4,1), STRING, 1, .FALSE. )
C
      ELSE IF( PXYONE ) THEN
C
         CALL PGNUMB( PXSVAL(K,1,1), 0, 1, STRING, J )
         CALL PLSTXT( PXLTXT(1,3), PXLTXT(2,3), PXLTXT(3,1),
     1                PXLTXT(4,1), STRING, 1, .FALSE. )
         CALL PGNUMB( PXSVAL(K,2,1), 0, 1, STRING, J )
         CALL PLSTXT( PXRTXT(1,3), PXRTXT(2,3), PXRTXT(3,1),
     1                PXRTXT(4,1), STRING, 1, .FALSE. )
C
      ELSE
C
         DO 50 I=1,3
            CALL PGNUMB( PXSVAL(K,1,I), 0, 1, STRING, J )
            CALL PLSTXT( PXLTXT(1,I), PXLTXT(2,I), PXLTXT(3,I),
     1                   PXLTXT(4,I), STRING, 1, .FALSE. )
            CALL PGNUMB( PXSVAL(K,2,I), 0, 1, STRING, J )
            CALL PLSTXT( PXRTXT(1,I), PXRTXT(2,I), PXRTXT(3,I),
     1                   PXRTXT(4,I), STRING, 1, .FALSE. )
 50      CONTINUE
C
      END IF
C
C     X Axis Buttons
C
      DO 60 I=1,2
         CALL PLBUTC( PXSDEF(1,I), PXSDEF(2,I), PXSDEF(3,I),
     1                PXSDEF(4,I), 'X', 0, 0 )
         CALL PLBUTC( PXSBXM(1,I), PXSBXM(2,I), PXSBXM(3,I),
     1                PXSBXM(4,I), ' ', PXSEXP(I), 1 )
 60   CONTINUE
C
C     X Axis Days
C
      IF( PXSTYP(K)(1:1) .EQ. 'd' ) THEN
         DO 65 I=1,2
            CH = CHAR( PADAYS(K,I) + 48 )
            CALL PLSTXT( PSGTXT(1,I), PSGTXT(2,I), PSGTXT(3,I),
     1                   PSGTXT(4,I), CH, 1, .FALSE. )
 65      CONTINUE
      END IF
C
C     XY Axis Sign
C
      CALL PLAXSS( AP, EP )
C
      RETURN
      END
