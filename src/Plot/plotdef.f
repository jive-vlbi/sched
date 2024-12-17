      SUBROUTINE PLOTDEF( IER, WHICH, TYPE, RMIN, RMAX, DAY0, 
     1                    LABEL, MIN, MAX )
C
C     Routine for SCHED called by PLOTXY to set axis limits and
C     labels.  Also checks the axis requests.  RMIN and RMAX are
C     inputs.  MIN and MAX are outputs.
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
      DOUBLE PRECISION  RMIN, RMAX, DAY0, TBORDER
      CHARACTER         WHICH*1, TYPE*8, LABEL*(*)
      REAL              MIN, MAX
      INTEGER           IER, I
C ----------------------------------------------------------------------
      IER = 0
C
C     Go through the allowed types.
C
      IF( TYPE .EQ. 'UT' .AND. WHICH .EQ. 'X' ) THEN
         TBORDER = ( TEND - TFIRST ) / 20.D0
         IF( RMIN .EQ. UNSET ) THEN
            MIN = ( TFIRST - DAY0 - TBORDER ) * 86400.D0
         ELSE
            MIN = RMIN
         END IF
         IF( RMAX .EQ. UNSET ) THEN
            MAX = ( TEND - DAY0 + TBORDER ) * 86400.D0
         ELSE
            MAX = RMAX
         END IF
         CALL PLTLAB( TYPE, DAY0, 0, LABEL )
C
      ELSE IF( TYPE .EQ. 'GST' .AND. WHICH .EQ. 'X' ) THEN
         IF( RMIN .EQ. UNSET ) THEN
            MIN = 0.D0
         ELSE
            MIN = RMIN
         END IF
         IF( RMAX .EQ. UNSET ) THEN
            MAX = 86400.0
         ELSE
            MAX = RMAX
         END IF
         CALL PLTLAB( TYPE, DAY0, 0, LABEL )
C
      ELSE IF( TYPE .EQ. 'LST' .AND. WHICH .EQ. 'X' ) THEN
         MIN = RMIN
         MAX = RMAX
         CALL PLTLAB( TYPE, DAY0, POFVAL(2), LABEL )
C
      ELSE IF( TYPE .EQ. 'AZ' ) THEN
         IF( RMIN .EQ. UNSET ) THEN
            MIN = 0.D0
         ELSE
            MIN = RMIN
         END IF
         IF( RMAX .EQ. UNSET ) THEN
            MAX = 360.0
         ELSE
            MAX = RMAX
         END IF
         LABEL = 'AZ (Deg)'
C
      ELSE IF( TYPE .EQ. 'EL' ) THEN
         IF( RMIN .EQ. UNSET ) THEN
            MIN = 0.D0
         ELSE
            MIN = RMIN
         END IF
         IF( RMAX .EQ. UNSET ) THEN
            MAX = 90.0
         ELSE
            MAX = RMAX
         END IF
         LABEL = 'EL (Deg)'
C
      ELSE IF( TYPE .EQ. 'PA' ) THEN
         IF( RMIN .EQ. UNSET ) THEN
            MIN = -180.0
         ELSE
            MIN = RMIN
         END IF
         IF( RMAX .EQ. UNSET ) THEN
            MAX = 180.0
         ELSE
            MAX = RMAX
         END IF
         LABEL = 'Paralactic Angle (Deg)'
C
      ELSE IF( TYPE .EQ. 'HA' ) THEN
         IF( RMIN .EQ. UNSET ) THEN
            MIN = -12.0
         ELSE
            MIN = RMIN
         END IF
         IF( RMAX .EQ. UNSET ) THEN
            MAX = 12.0
         ELSE
            MAX = RMAX
         END IF
         LABEL = 'Hour Angle (Hours)'
C
      ELSE IF( TYPE .EQ. 'RA' ) THEN
         IF( RMIN .EQ. UNSET ) THEN
            MIN = 0.0
         ELSE
            MIN = RMIN
         END IF
         IF( RMAX .EQ. UNSET ) THEN
            MAX = 24.0 * 3600.0
         ELSE
            MAX = RMAX
         END IF
         LABEL = 'RA (Hours)'
C
      ELSE IF( TYPE .EQ. 'DEC' ) THEN
         IF( RMIN .EQ. UNSET ) THEN
            MIN = (-90.0) * 3600.0
         ELSE
            MIN = RMIN
         END IF
         IF( RMAX .EQ. UNSET ) THEN
            MAX = 90.0 * 3600.0
         ELSE
            MAX = RMAX
         END IF
         LABEL = 'Dec (Deg)'
C
      ELSE IF( TYPE .EQ. 'Ant' ) THEN
         MIN = RMIN
         MAX = RMAX
         LABEL = ' '
C
      ELSE IF( TYPE .EQ. 'Sec' ) THEN
         IF( PXYSEC ) THEN
            I = 1
         ELSE
            I = 0
            MIN = RMIN
            MAX = RMAX
         END IF
         CALL PLMNMX( TYPE, MIN, MAX, I )
         LABEL = 'Sec(z)'
C
      ELSE
         CALL PUTOUT( 'PLOTDEF: Invalid request for ' // WHICH //
     1         ' axis type: ' // TYPE )
         IER = 1
      END IF
C
      RETURN
      END
