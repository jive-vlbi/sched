      SUBROUTINE SCHGEO( ISCN, ISTA, JTIME, HA, EL, AZ, LSTTIM, PA )
C
C     Subroutine to calculate HA, EL, AZ, and LST from
C     Modified Julian time (JTIME), RA, DEC, LONG, and LAT.
C     Also gets paralactic angle for altaz antenna (PA)
C
C     The PA equations for XY mounts are from Sovers, Fanselow, and 
C     Jacobs.  The equat version agrees with that reference too.
C
      INCLUDE 'sched.inc' 
C
      INTEGER          ISCN, ISTA, KSTA
      REAL             HA, ZA, EL, AZ, PA
      DOUBLE PRECISION JTIME, LSTTIM
      DOUBLE PRECISION GAST, CD, SD, SL, CL, CH, SH, SLA_GMST
C --------------------------------------------------------------------
      IF( ISCN .EQ. 0 ) CALL ERRLOG( 'SCHGEO called for scan 0.' //
     1    '  Programming problem.' )
      IF( ISTA .EQ. 0 ) CALL ERRLOG( 'SCHGEO called for station 0.' //
     1    '  Programming problem.' )
      GAST = SLA_GMST( JTIME )
      HA = MOD( GAST - RAP(SRCNUM(ISCN)) - LONG(STANUM(ISTA)), TWOPI )
      LSTTIM = MOD( GAST - LONG(STANUM(ISTA)), TWOPI )
      IF( LSTTIM. LT. 0.D0 ) LSTTIM = LSTTIM + TWOPI
      CD = COS(DECP(SRCNUM(ISCN)))
      SD = SIN(DECP(SRCNUM(ISCN)))
      CL = COS(LAT(STANUM(ISTA)))
      SL = SIN(LAT(STANUM(ISTA)))
      CH = COS(HA)
      SH = SIN(HA)
      HA = HA / RADHR
      IF( HA.LT.-12.0 ) HA = HA + 24.0
      IF( HA.GT. 12.0 ) HA = HA - 24.0
      ZA = ACOS(CD*CL*CH + SD*SL) / RADDEG
      EL = 90.0 - ZA
      AZ = ATAN2(SD*CL - CD*SL*CH, CD*SH) / RADDEG
      AZ = AMOD( AZ - 90.0, 360.0 )
      IF( AZ.LT.0. ) AZ = AZ + 360.0
C
C     Get paralactic angle depending on mount type.
C
      KSTA = STANUM(ISTA)
      IF( MOUNT(KSTA) .EQ. 'ALTAZ' ) THEN
         PA = ATAN2( CL*SH, SL*CD - CL*SD*CH ) / RADDEG
      ELSE IF( MOUNT(KSTA) .EQ. 'EQUAT' ) THEN
         PA = 0.0
      ELSE IF( MOUNT(KSTA) .EQ. 'XYNS' ) THEN
         PA = ATAN2( -1.*SL*SH, CL*CD + SL*SD*CH ) / RADDEG
      ELSE IF( MOUNT(KSTA) .EQ. 'XYEW' ) THEN
         PA = ATAN2( CH, SD*SH ) / RADDEG
      ELSE
         WRITE( MSGTXT, '( A, A, A, A )' ) 
     1      'SCHGEO: Unknown mount type ', MOUNT(KSTA), ' at ',
     2      STATION(KSTA)
         CALL ERRLOG( MSGTXT )
      END IF
C
      RETURN
      END
