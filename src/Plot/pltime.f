      SUBROUTINE PLTIME( XAXIS, LONG, TFIRST, TEND, TWOPI, 
     1                   RADHR, VALUE, PANDAY, PADAYS )
C
C     Routine for plot that make the integer array of HH MM SS
C     values of min and max UT and LST axis, converting from the
C     start and stop Julian day.
C
      CHARACTER        XAXIS*(*)
      INTEGER          VALUE(2,3), PANDAY(3), INIDAY, ENDDAY
      INTEGER          PADAYS(3,3), K
      DOUBLE PRECISION LONG, TFIRST, TEND, TWOPI, RADHR
      DOUBLE PRECISION LST, LSTDAY, HTIM
C ----------------------------------------------------------------------
C
C     UT/GST/LST Array Minor limits
C
      IF( XAXIS .EQ. 'UT' ) THEN
         INIDAY    = DINT( TFIRST )
         PANDAY(1) = INIDAY
         HTIM      = ( TFIRST - INIDAY ) * 86400.D0
      ELSE
         CALL SIDTIM( TFIRST, LONG, TWOPI, INIDAY, LST, LSTDAY )
         IF( XAXIS .EQ. 'GST' ) THEN
            PANDAY(2) = INIDAY
         ELSE
            PANDAY(3) = INIDAY
         END IF

         HTIM   = LST * 3600.D0 / RADHR
      END IF
C
C     Min Hours
C
      VALUE(1,1) = DINT( HTIM / 3600.D0 )
C
C     Min Minutes and Seconds
C
      HTIM = DMOD( HTIM, 3600.D0 )
      IF( HTIM .GT. 0.D0 ) THEN
         VALUE(1,2) = DINT( HTIM / 60.D0 )
         VALUE(1,3) = DINT( DMOD( HTIM, 60.D0 ) )
      END IF
C
C     UT/GST/LST Array Maximum limits
C
      IF( XAXIS .EQ. 'UT' ) THEN
         ENDDAY = DINT( TEND )
         HTIM = ( TEND - INIDAY ) * 86400.D0
         K = 1
      ELSE
         CALL SIDTIM( TEND, LONG, TWOPI, ENDDAY, LST, LSTDAY )
         HTIM = LST * 3600.D0 / RADHR
         IF( XAXIS .EQ. 'GST' ) THEN
            K = 2
         ELSE
            K = 3
         END IF
      END IF
C
C     Set Day Offset
C
      PADAYS(K,1) = 0
      PADAYS(K,2) = ENDDAY - INIDAY
      PADAYS(K,3) = PADAYS(K,2)
C
C     Max Hours Modulo 24
C
      VALUE(2,1) = DINT( HTIM / 3600.D0 )
      IF( PADAYS(K,3) .GT. 0 .AND. VALUE(2,1) .GE. 24 ) THEN
         VALUE(2,1) = VALUE(2,1) - ( PADAYS(K,3) * 24 )
      END IF
C
C     Max Minutes and Seconds
C
      HTIM = DMOD( HTIM, 3600.D0 )
      IF( HTIM .GT. 0.D0 ) THEN
         VALUE(2,2) = DINT( HTIM / 60.D0 )
         VALUE(2,3) = DINT( DMOD( HTIM, 60.D0 ) )
      END IF
C
      RETURN
      END
