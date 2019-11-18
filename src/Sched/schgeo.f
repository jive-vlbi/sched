      SUBROUTINE SCHGEO( ISCN, ISTA, JTIME, HA, EL, AZ, LSTTIM, PA )
Cf2py intent(in) ISCN, ISTA, JTIME
Cf2py intent(out) HA, EL, AZ, LSTTIM, PA
C
C     Subroutine to calculate HA, EL, AZ, and LST from
C     Modified Julian time (JTIME), RA, DEC, LONG, and LAT.
C     Also gets paralactic angle for altaz antenna (PA)
C
C     The PA equations for XY mounts are from Sovers, Fanselow, and 
C     Jacobs.  The equat version agrees with that reference too.
C
C     Note that some approximations are made that make this routine
C     inappropriate for accurate work.  First SLA_GMST delivers the
C     Greenwich mean sidereal time when what is really wanted is the
C     Greenwich apparent sidereal time.  The difference is typically
C     under a second.  Refraction is not taken into account which 
C     might affect when something actually goes behind a mountain.
C
C     Oct. 9, 2010.  For planets and satellites, get the current 
C     scan/station position.  RCW.
C
C     This routine does not modify any of the scan data from the
C     COMMONs in the include file, so it can be used to calculate
C     hypothetical cases without corrupting scan calculations that
C     were made before.  WRAP will do that.
C
      INCLUDE 'sched.inc' 
C
      INTEGER          ISCN, ISRC, ISTA, KSTA
      REAL             HA, ZA, EL, AZ, PA
      REAL             PDRA, PDDEC
      DOUBLE PRECISION JTIME, LSTTIM
      DOUBLE PRECISION GAST, CD, SD, SL, CL, CH, SH, SLA_GMST
      DOUBLE PRECISION PRA, PDEC, PPMTIME, DATERA, DATEDEC
C --------------------------------------------------------------------
      IF( ISCN .EQ. 0 ) CALL ERRLOG( 'SCHGEO called for scan 0.' //
     1    '  Programming problem.' )
      IF( ISTA .EQ. 0 ) CALL ERRLOG( 'SCHGEO called for station 0.' //
     1    '  Programming problem.' )
C
C     Get the source position to use.  For moving sources, this can 
C     be station and scan dependent.  Note that SRCLOC returns
C     J2000 coordinates where the geometry calculation needs to be
C     based on coordinates of date.  Use the same precession as in
C     SCHPRE which is for the first stop time.
C     Bypass these calls if the source is not moving because they are
C     a bit slow, causing problems for some optimization routines.
C
      ISRC = SRCNUM(ISCN)
      IF( PLANET(ISRC) .OR. SATEL(ISRC) .OR. 
     1      DRA(ISRC) .NE. 0.0 .OR. DDEC(ISRC) .NE. 0.0 ) THEN
         CALL SRCLOC( ISCN, ISTA, PRA, PDEC, PPMTIME, PDRA, PDDEC )
         CALL SLA_MAP( PRA, PDEC, 0.D0, 0.D0, 0.D0, 0.D0, 2000.D0, 
     1              STOPJ(1), DATERA, DATEDEC )
      ELSE
         DATERA = RAP(ISRC)
         DATEDEC = DECP(ISRC)
      END IF
C
C     Now get the geometry.
C
      GAST = SLA_GMST( JTIME )
      HA = MOD(  GAST - DATERA - LONG(STANUM(ISTA)), TWOPI )
      LSTTIM = MOD( GAST - LONG(STANUM(ISTA)), TWOPI )
      IF( LSTTIM. LT. 0.D0 ) LSTTIM = LSTTIM + TWOPI
      CD = COS(DATEDEC)
      SD = SIN(DATEDEC)
      CL = COS(LAT(STANUM(ISTA)))
      SL = SIN(LAT(STANUM(ISTA)))
      CH = COS(HA)
      SH = SIN(HA)
      HA = HA / RADHR
      IF( HA.LE.-12.0 ) HA = HA + 24.0
      IF( HA.GT. 12.0 ) HA = HA - 24.0
      ZA = ACOS(CD*CL*CH + SD*SL) / RADDEG
      EL = 90.0 - ZA
      AZ = ATAN2(SD*CL - CD*SL*CH, CD*SH) / RADDEG
      AZ = AMOD( AZ - 90.0, 360.0 )
      IF( AZ .LT. 0.0 ) AZ = AZ + 360.0
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
