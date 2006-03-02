      SUBROUTINE PLSGEO( ISTA, RAS, DECS, JTIME, 
     1                   HA, EL, AZ, LSTTIM, PA )
C
C     Subroutine to calculate HA, EL, AZ, and LST from
C     Modified Julian time (JTIME), RA, DEC, LONG, and LAT.
C     Also gets paralactic angle for altaz antenna (PA)
C
      INCLUDE 'sched.inc' 
C
      INTEGER          ISTA
      REAL             HA, ZA, EL, AZ, PA
      DOUBLE PRECISION JTIME, LSTTIM, RAS, DECS
      DOUBLE PRECISION GAST, CD, SD, SL, CL, CH, SH, SLA_GMST
C --------------------------------------------------------------------
      GAST = SLA_GMST( JTIME )
      HA = MOD( GAST - RAS - LONG(STANUM(ISTA)), TWOPI )
      LSTTIM = MOD( GAST - LONG(STANUM(ISTA)), TWOPI )
      IF( LSTTIM.LT.0.0 ) LSTTIM = LSTTIM + TWOPI
      CD = COS(DECS)
      SD = SIN(DECS)
      SL = SIN(LAT(STANUM(ISTA)))
      CL = COS(LAT(STANUM(ISTA)))
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
      PA = ATAN2( CL*SH, SL*CD-CL*SD*CH ) / RADDEG
C
      RETURN
      END
