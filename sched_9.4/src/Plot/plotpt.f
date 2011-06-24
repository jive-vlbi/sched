      SUBROUTINE PLOTPT( ISCN, ISTA, AXIS, PT, NPT, DAY0 )
C
C     Routine for SCHED called by PLOTXY that gets the numbers to plot
C     based on the axis type.  It gets one number for each end of
C     the scan.  For GST, the time axis can fold over, in which case,
C     there will be 4 elements of PT, two pairs which are the 
C     two sets with the different 1 day ambiguities.  A similar
C     thing is done with angle axes like azimuth.
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
      INTEGER           ISCN, ISTA, NPT, SIDDAY, INIDAY
      CHARACTER         AXIS*8
      REAL              PT(4)
      DOUBLE PRECISION  DAY0, DLST, LSTDAY, ANLONG
C  -------------------------------------------------------------------
C     Assume that there will be two points unless proven otherwise.
C
      NPT = 2
C
C     Go through the possible plot axes.
C
      IF( AXIS .EQ. 'UT' ) THEN
         PT(1) = ( STARTJ(ISCN) - DAY0 ) * 86400.D0
         PT(2) = ( STOPJ(ISCN) - DAY0 ) * 86400.D0
C
      ELSE IF( AXIS .EQ. 'LST' .OR. AXIS .EQ. 'GST' ) THEN
         IF( AXIS .EQ. 'GST' ) THEN
            ANLONG = 0.D0
            INIDAY = PANDAY(2)
         ELSE
            INIDAY = PANDAY(3)
            IF( POFVAL(2) .EQ. 0 ) THEN
               ANLONG = 0.D0
            ELSE
               ANLONG = LONG( STANUM( POFVAL(2) ) )
            END IF
         END IF
         CALL SIDTIM( STARTJ(ISCN), ANLONG, TWOPI, SIDDAY,
     1                DLST, LSTDAY )
         PT(1) = ( LSTDAY - INIDAY ) * 86400.D0
C
         CALL SIDTIM( STOPJ(ISCN), ANLONG, TWOPI, SIDDAY,
     1                DLST, LSTDAY )
         PT(2) = ( LSTDAY - INIDAY ) * 86400.D0
C
         IF( PT(2) .LT. PT(1) ) THEN
            PT(3) = PT(1)
            PT(1) = PT(1) - 86400.0
            PT(4) = PT(2) + 86400.0
            NPT = 4
         END IF
C
      ELSE IF( AXIS .EQ. 'AZ' ) THEN
         PT(1) = MOD( AZ1(ISCN,ISTA), 360.0 )
         PT(2) = MOD( AZ2(ISCN,ISTA), 360.0 )
         IF( PT(1) .LT. 0.0 ) PT(1) = PT(1) + 360.0
         IF( PT(2) .LT. 0.0 ) PT(2) = PT(2) + 360.0
         IF( ABS( PT(1) - PT(2) ) .GT. 270.0 ) THEN
            IF( PT(1) .LT. 180.0 ) THEN
               PT(3) = PT(1) + 360.0 
            ELSE
               PT(3) = PT(1)
               PT(1) = PT(1) - 360.0
            END IF
            IF( PT(2) .LT. 180.0 ) THEN
               PT(4) = PT(2) + 360.0 
            ELSE
               PT(4) = PT(2)
               PT(2) = PT(2) - 360.0
            END IF
            NPT = 4
         END IF
C
      ELSE IF( AXIS .EQ. 'EL' ) THEN
         PT(1) = EL1(ISCN,ISTA)
         PT(2) = EL2(ISCN,ISTA)
C
      ELSE IF( AXIS .EQ. 'Sec' ) THEN
         PT(1) = 1.0/SIN( EL1(ISCN,ISTA) * RADDEG )
         PT(2) = 1.0/SIN( EL2(ISCN,ISTA) * RADDEG )
C
      ELSE IF( AXIS .EQ. 'HA' ) THEN
         PT(1) = HA1(ISCN,ISTA)
         PT(2) = HA2(ISCN,ISTA)
         IF( ABS( PT(1) - PT(2) ) .GT. 12.0 ) THEN
            IF( PT(1) .LT. 0.0 ) THEN
               PT(3) = PT(1) + 24.0 
            ELSE
               PT(3) = PT(1)
               PT(1) = PT(1) - 24.0
            END IF
            IF( PT(2) .LT. 0.0 ) THEN
               PT(4) = PT(2) + 24.0 
            ELSE
               PT(4) = PT(2)
               PT(2) = PT(2) - 24.0
            END IF
            NPT = 4
         END IF
C
      ELSE IF( AXIS .EQ. 'PA' ) THEN
         PT(1) = PA1(ISCN,ISTA)
         PT(2) = PA2(ISCN,ISTA)
         IF( ABS( PT(1) - PT(2) ) .GT. 180.0 ) THEN
            IF( PT(1) .LT. 0 ) THEN
               PT(3) = PT(1) + 360.0 
            ELSE
               PT(3) = PT(1)
               PT(1) = PT(1) - 360.0
            END IF
            IF( PT(2) .LT. 0 ) THEN
               PT(2) = PT(1) + 360.0 
            ELSE
               PT(4) = PT(2)
               PT(2) = PT(2) - 360.0
            END IF
            NPT = 4
         END IF
C
      ELSE IF( AXIS .EQ. 'Ant' ) THEN
         PT(1) = ISTA
         PT(2) = PT(1)
C
      ELSE
         CALL PUTOUT( 'PLOTPT: Programming problem with axis type.' //
     1           '  Please report. ' // AXIS )
      END IF
C
      RETURN
      END

