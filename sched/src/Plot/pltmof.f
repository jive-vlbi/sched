      SUBROUTINE PLTMOF( CH, MODE )
C
C     Routine for sched that plot a Text Area
C     with value for time offset
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
      CHARACTER        CH, STRING*4, STRIN1*9
      INTEGER          MODE, TYPE, OTYPE, I, J
      DOUBLE PRECISION ANLONG
      SAVE             OTYPE
      DATA             OTYPE / 0 /
C ----------------------------------------------------------------------
C
C     Save PGPLOT calling attribute
C
      CALL PGSAVE
C
C     Set Type of Offset
C
      TYPE = POFPOI(PXYBCK(1))
C
C     If type was changed clear offset area
C
      IF( TYPE .NE. OTYPE .AND. PXYTYP(PXYBCK(1)) .NE. 'Wv' ) THEN
          OTYPE = TYPE
          CALL PGSCI( PPNCOL(1) )
          CALL PGRECT( PXYTXT(1,4), PXYTXT(2,4),
     1                 PXYTXT(3,4), PXYTXT(4,4) )
      END IF
C
C     Plot/Check Time Offset if requested else clean area
C     First Check for UT Time Offset
C
      IF( TYPE .EQ. 1 ) THEN
C
C        If mode equal 1 first update value
C
         IF( MODE .EQ. 1 ) THEN
C
C           Update value
C
            IF( CH .EQ. 'A' ) THEN
               POFVAL(1) = POFVAL(1) +  1
            ELSE IF( CH .EQ. 'D' .OR. CH .EQ. 'X' ) THEN
               POFVAL(1) = POFVAL(1) - 1
            END IF
C
C           Check value
C
            IF( POFVAL(1) .LT. -12 .OR. 
     1          POFVAL(1) .GT. 12 ) POFVAL(1) =  0
C
         END IF
C
C        Plot Text Area and value
C
         CALL PGNUMB( POFVAL(1), 0, 1, STRING, I )
         CALL PLSTXT( PXYTXT(1,3), PXYTXT(2,3), PXYTXT(3,3),
     1                PXYTXT(4,3), STRING, 1, .TRUE. )
C
C     Check for LST Time Offset
C
      ELSE IF( TYPE .EQ. 2 ) THEN
C
C        If mode equal 1 first update Antenna value
C
         IF( MODE .EQ. 1 ) THEN
C
C           Increment value and check limit
C
            IF( CH .EQ. 'A' ) THEN
               POFVAL(2) = POFVAL(2) +  1
            ELSE IF( CH .EQ. 'D' .OR. CH .EQ. 'X' ) THEN
               POFVAL(2) = POFVAL(2) - 1
            END IF
C
C           Check value
C
            IF( POFVAL(2) .LT. 0 ) POFVAL(2)  = NSTA
            IF( POFVAL(2) .GT. NSTA ) POFVAL(2) = 0
C
C           Set New Time
C
            IF( POFVAL(2) .EQ. 0 ) THEN
               ANLONG = 0.D0
            ELSE
               ANLONG = LONG(STANUM(POFVAL(2)))
            END IF
            CALL PLTIME( 'LST', ANLONG, TFIRST, TEND,
     1                   TWOPI, RADHR, PLSVAL, PANDAY, PADAYS )
C
C           Update X Axis Value
C
            DO 20 I=1,2
               DO 10 J=1,3
                  PXSVAL(PXYBCK(1),I,J) = PLSVAL(I,J)
 10            CONTINUE
 20         CONTINUE
            CALL PLAXST( 'X' )
         END IF
C
C        Plot Text Area and value
C
         IF( POFVAL(2) .GT. 0 ) THEN
            STRIN1 = STANAME(POFVAL(2))
         ELSE
            STRIN1 = 'Greenwich'
         END IF
         CALL PLSTXT( PXYTXT(1,4), PXYTXT(2,4), PXYTXT(3,4),
     1                PXYTXT(4,4), STRIN1, 1, .TRUE. )
C
C     No Time Offset
C
      ELSE
         IF( MODE .EQ. 1 ) THEN
            CALL PLTIME( 'GST', 0.D0, TFIRST, TEND,
     1                   TWOPI, RADHR, PSGVAL, PANDAY, PADAYS )
C
C           Update X Axis Value
C
            CALL PLAXST( 'X' )
         END IF
C
      END IF
C
C     Restore PGPLOT calling attributes
C
      CALL PGUNSA
C
      RETURN
      END
