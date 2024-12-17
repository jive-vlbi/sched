      SUBROUTINE PVOBS (PHI, ELONG, HEIGHT, GAST, LITTLG)
c
c calculate the position of the observer on the surface of the Earth,
c in an Earth-fixed, geocentric, right-handed coordinate system with 
c the xy-plane the Earth's equator, the xz-plane the Greenwich 
c meridian, and the z-axis pointed toward the north terrestrial pole.  
c taken from section 3.353 of the Explanatory Supplement to the 
c Astronomical Almanac.
c
c  Inputs:
c   PHI    = observer's geodetic latitude (radians)
c   ELONG  = observer's East longitude (radians)
c   HEIGHT = observer's height above geoid (meters)
c   GAST   = Greenwich Apparent Sidereal Time (radians)
c
c  Outputs:
c   LITTLG = the geocentric position and velocity vector of
c            the observer (AU, AU/day)
c
c  Calls: sla_DMXM, sla_DMXV
c
C  B.J.Butler    NRAO/VLA   summer 1996

      IMPLICIT NONE

      DOUBLE PRECISION PHI, ELONG, HEIGHT, GAST, LITTLG(6)
      DOUBLE PRECISION FLAT, EQRAD, OMEGA, AU
      PARAMETER (FLAT = 1.0D0 / 298.257D0)
      PARAMETER (EQRAD = 6.37814D6)
      PARAMETER (OMEGA = 7.2921151467D-5)
      PARAMETER (AU = 1.49597870D11)
      DOUBLE PRECISION CPHI, SPHI, CVAL, SVAL, TVAL, XP, YP
      DOUBLE PRECISION LITTLR(3), R1(3,3), R2(3,3), R3(3,3),
     *                 IMTX1(3,3), IMTX2(3,3)
      INTEGER II

c 3.351 Location and Universal Time of the Observation
c step b
      CPHI = DCOS (PHI)
      SPHI = DSIN (PHI)
      TVAL = (1 - FLAT) * (1 - FLAT)
      CVAL = 1.0D0 / DSQRT (CPHI * CPHI + TVAL * SPHI * SPHI)
      SVAL = CVAL * TVAL
      LITTLR(1) = (EQRAD * CVAL + HEIGHT) * CPHI * DCOS (ELONG)
      LITTLR(2) = (EQRAD * CVAL + HEIGHT) * CPHI * DSIN (ELONG)
      LITTLR(3) = (EQRAD * SVAL + HEIGHT) * SPHI
c 3.353 Geocentric Position and Velocity Vectors of the Observer
c step e
c   here, if you have access to the coordinates of the Celestial
c   Ephemeris Pole (CEP), XP and YP, put them in.  i haven't put
c   in a routine to calculate them yet, although it appears that
c   it could be done (see the web site at:
c   file://maia.usno.navy.mil/standards/iersch5.t2)
c   the supplement states that neglecting these terms affects the
c   topocentric place of the moon by several milliarcsec, with a
c   smaller effect, inversely proportional to distance, for other
c   bodies.  neglecting these terms is in effect setting R1 and
c   R2 to unit matrices...
c   set up the XP matrix (R2):
      XP = 0.0D0
      R2(1,1) = DCOS (XP)
      R2(1,2) = 0.0D0
      R2(1,3) = -DSIN (XP)
      R2(2,1) = 0.0D0
      R2(2,2) = 1.0D0
      R2(2,3) = 0.0D0
      R2(3,1) = DSIN (XP)
      R2(3,2) = 0.0D0
      R2(3,3) = DCOS (XP)
c   set up the YP matrix (R1):
      YP = 0.0D0
      R1(1,1) = 1.0D0
      R1(1,2) = 0.0D0
      R1(1,3) = 0.0D0
      R1(2,1) = 0.0D0
      R1(2,2) = DCOS (YP)
      R1(2,3) = DSIN (YP)
      R1(3,1) = 0.0D0
      R1(3,2) = -DSIN (YP)
      R1(3,3) = DCOS (YP)
c   set up the GAST matrix (R3) [remember, rotating by -GAST]:
      R3(1,1) = DCOS (GAST)
      R3(1,2) = -DSIN (GAST)
      R3(1,3) = 0.0D0
      R3(2,1) = DSIN (GAST)
      R3(2,2) = DCOS (GAST)
      R3(2,3) = 0.0D0
      R3(3,1) = 0.0D0
      R3(3,2) = 0.0D0
      R3(3,3) = 1.0D0
c   multiply R3 and R1, and store in an intermediate matrix:
      CALL sla_DMXM (R3, R1, IMTX1)
c   multiply that intermediate matrix by R2, and store in another
c   intermediate matrix:
      CALL sla_DMXM (IMTX1, R2, IMTX2)
c   multiply this by the observers position vector:
      CALL sla_DMXV (IMTX2, LITTLR, LITTLG)
c   now, set up the rotation vector (R3 again...)
      R3(1,1) = -DSIN (GAST)
      R3(1,2) = -DCOS (GAST)
      R3(1,3) = 0.0D0
      R3(2,1) = DCOS (GAST)
      R3(2,2) = -DSIN (GAST)
      R3(2,3) = 0.0D0
      R3(3,1) = 0.0D0
      R3(3,2) = 0.0D0
      R3(3,3) = 0.0D0
c   multiply R3 and R1, and store in an intermediate matrix:
      CALL sla_DMXM (R3, R1, IMTX1)
c   multiply that intermediate matrix by R2, and store in another
c   intermediate matrix:
      CALL sla_DMXM (IMTX1, R2, IMTX2)
c   multiply this by the observers velocity vector:
      CALL sla_DMXV (IMTX2, LITTLR, LITTLG(4))
c   multiply by earth rotation rate:
      DO II = 4, 6
         LITTLG(II) = LITTLG(II) * OMEGA
      END DO
c step f
      DO II = 1, 3
         LITTLG(II) = LITTLG(II) / AU
      END DO
      DO II = 4, 6
         LITTLG(II) = LITTLG(II) * 86400.0D0 / AU
      END DO
      RETURN
      END
