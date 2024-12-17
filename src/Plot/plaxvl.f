      SUBROUTINE PLAXVL( XAXIS, YAXIS, KX, KY, XMIN, XMAX, YMIN, YMAX )
C
C     Routine for plot that make the axis value
C
C
      INCLUDE 'plot.inc'
C
      CHARACTER         XAXIS*(*), YAXIS*(*)
      INTEGER           KX, KY
      DOUBLE PRECISION  XMIN, XMAX, YMIN, YMAX
C ----------------------------------------------------------------------
C
C     Set X axis value
C
      IF( XAXIS .EQ. 'GST' .OR. XAXIS .EQ. 'UT' .OR. 
     1    XAXIS .EQ. 'LST' .OR. XAXIS .EQ. 'RA' ) THEN
C
         XMIN = ( 3600.D0 * PXSVAL(KX,1,1) ) +
     1          (   60.D0 * PXSVAL(KX,1,2) ) + PXSVAL(KX,1,3)
         XMAX = ( 3600.D0 * PXSVAL(KX,2,1) ) + 
     1          (   60.D0 * PXSVAL(KX,2,2) ) + PXSVAL(KX,2,3)
C
C        Check Axis Limits
C
         IF( XAXIS .NE. 'RA' ) THEN
            CALL PLTMCK( XAXIS, XMIN, XMAX )
         END IF
C
      ELSE IF( XAXIS .EQ. 'Km' .OR. XAXIS .EQ. 'Wv' ) THEN
         XMIN = PXSVAL(KX,1,1) * PXSSGN(KX,1)
         XMAX = PXSVAL(KX,2,1) * PXSSGN(KX,2)
C
      ELSE
         XMIN = ( PXSVAL(KX,1,1) + 
     1          ( ( ( 60.D0 * PXSVAL(KX,1,2) ) + PXSVAL(KX,1,3) ) * 
     2                10.D0**(-4) ) ) * PXSSGN(KX,1)
         XMAX = ( PXSVAL(KX,2,1) +
     1          ( ( ( 60.D0 * PXSVAL(KX,2,2) ) + PXSVAL(KX,2,3) ) *
     2                10.D0**(-4) ) ) * PXSSGN(KX,2)
C
      ENDIF
C
C     Set the Y Axis Value
C
      IF( YAXIS .EQ. 'DEC' ) THEN
         YMIN = ( ( 3600.D0 * PXSVAL(KY,3,1) ) +
     1            ( 60.D0 * PXSVAL(KY,3,2) ) + PXSVAL(KY,3,3) ) *
     2          PXSSGN(KY,3)
         YMAX = ( ( 3600.D0 * PXSVAL(KY,4,1) ) +
     1            ( 60.D0 * PXSVAL(KY,4,2) ) + PXSVAL(KY,4,3) ) *
     2          PXSSGN(KY,4)
C
      ELSE IF( YAXIS .EQ. 'Km' .OR. YAXIS .EQ. 'Wv' ) THEN
         YMIN = PXSVAL(KY,3,1) * PXSSGN(KY,3)
         YMAX = PXSVAL(KY,4,1) * PXSSGN(KY,4)
C
      ELSE IF( YAXIS .EQ. 'Ant' ) THEN
         YMIN = 1.D0
         YMAX = ( 1.D0 * PSONUM ) + 1.D0
C
      ELSE
         YMIN = ( PXSVAL(KY,3,1) +
     1          ( ( ( 60.D0 * PXSVAL(KY,3,2) ) + PXSVAL(KY,3,3) ) *
     2                10.D0**(-4) ) ) * PXSSGN(KY,3)
         YMAX = ( PXSVAL(KY,4,1) +
     1          ( ( ( 60.D0 * PXSVAL(KY,4,2) ) + PXSVAL(KY,4,3) ) *
     2                10.D0**(-4) ) ) * PXSSGN(KY,4)
      END IF
C
      RETURN
      END
