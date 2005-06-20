      SUBROUTINE JPLPO2 (DATEP, NP, ELONG, PHI, HEIGHT, RA, RARAT,
     *                   DEC, DECRAT, DIST, OBSV)
C
C  Exact geocentric or topocentric apparent RA, RA rate, Dec, Dec rate,
C  and distance of a planet, in either J2000.0 or position of date.  
C  Based on section 3.3 of the Explanatory Supplement to the 
C  Astronomical Almanac.  Specifically, this subroutine performs the 
C  steps in equation 3.31-1 for positions of date, and 3.41-1 for 
C  positions of J2000.  For topocentric positions, the correction 
C  described in section 3.35 is performed.  The 4 different 
C  combinations of J2000/date/geocentric/topocentric correspond to
C  the following designations and sections in the Explanatory Suppl:
C    date+geocentric = 'Apparent-Place'; section 3.31
C    date+topocentric = 'Topocentric-Place'; section 3.35
C    J2000+geocentric = 'Virtual Place'; section 3.41
C    J2000+topocentric = 'Local Place'; section 3.42
C  This routine calls the very accurate JPL ephemeris routines.  The 
C  most current ephemeris at JPL is DE403 (as of 6/25/96), which is the 
C  one used by this routine.  Errors are very small, on the order of a 
C  few milliarcseconds (without rigorous testing, though...).
C
C  !!!NOTE!!!
C     This version of the ephemeris subroutine (JPLPO2) is a modified 
C     version of the standard subroutine (JPLPOS).  JPLPO2 is set up 
C     specifically to supply positions which, when used by either the 
C     VLA or the VLBA, will result in correct pointings for the planets.
C     For the VLA, desired coordinates are geocentric, of date 
C     (Apparent-Place).  The corrections applied by the on-line system
C     are the diurnal parallax and diurnal aberration.  So, 
C     gravitational deflection, annual aberration, precession, and 
C     nutation need to be included in the supplied geocentric positions.
C     For the VLBA, desired coordinates are topocentric, of J2000.0 
C     (Local Place).  The corrections applied by the on-line system are 
C     the gravitational deflection, the annual aberration, the 
C     precession, and the nutation.  So, the diurnal aberration needs 
C     to be included in the supplied positions.  Note that the way 
C     these supplied positions (with some included corrections) are 
C     processed by the on-line systems will cause some inaccuracy in 
C     the pointed positions, since the order in which these things are 
C     done will make a difference.  This inaccuracy should be small, 
C     though.
C  !!!END NOTE!!!
C
C  Inputs:
C   DATEP   = MJD of observation (JD - 2400000.5), in the TDT timescale
C   NP      = planet for which positions and rates are desired: 
C                1 = Mercury
C                2 = Venus
C                3 = Moon
C                4 = Mars
C                5 = Jupiter
C                6 = Saturn
C                7 = Uranus
C                8 = Neptune
C                9 = Pluto
C                else = Sun
C   ELONG   = observer's East longitude (rad)
C   PHI     = observer's geodetic latitude (rad)
C   HEIGHT  = observer's height above geoid (meters)
C   OBSV    = which observatory: 'VLA' or 'VLBA'
C
C  Outputs:
C   RA      = RA, for equinox and equator of J2000.0 or of date (rad)
C   DEC     = Dec, for equinox and equator of J2000.0 or of date (rad)
C   RARAT   = RA rate, for J2000.0 or date (seconds of time per UT day)
C   DECRAT  = Dec rate, for J2000.0 or date (seconds of arc per UT day)
C   DIST    = distance, for the desired date (AU)
C                               
C  Calls: sla_EPJ, sla_GMST, sla_EQEQX, sla_PRECL, sla_NUT, sla_DMXV,
C         sla_DCC2S, sla_DRANRM, PVOBS, PLEPH, FINDT, TRANSP
C
C    All sla_XXX routines are from the slalib library, PLEPH and related
C    routines are from the JPL software, PVOBS, FINDT and TRANSP are 
C    my own routines.
C
C  B J Butler    NRAO/VLA   summer 1996, modified summer 1997

      IMPLICIT NONE

c passed parameters:
      DOUBLE PRECISION DATEP, ELONG, PHI, HEIGHT, RA, DEC, DIST, 
     *                 RARAT, DECRAT
      INTEGER NP
      CHARACTER OBSV*(*)
c constants:
      DOUBLE PRECISION PI, CC, MU
      PARAMETER (PI = 3.14159265358979323846D0)
      PARAMETER (CC = 173.144633D0)
      PARAMETER (MU = 2.9591220788654D-4)
c time stuff:
      DOUBLE PRECISION JEDP, DATE, JED, TPRIME, MM, SS, TT, UT1, GMST,
     *                 GAST, DELTAT, FINDT
c position and velocity vectors:
      DOUBLE PRECISION EB(6), SB(6), EH(6), UB(6), UU(6), QQ(6), 
     *                 LITTLU(6), LITTLQ(6), LITTLE(6), VEC1(3), 
     *                 VEC2(3), PP(3), VV(3), U1(6), U2(6), U3(6), 
     *                 U4(6), LITTLG(6), BIGG(6), BIGG2(6)
c other stuff:
      DOUBLE PRECISION LITTIM, LITTIP, DU, DE, DQ, G1, G2, QDOTE, UDOTQ,
     *                 EDOTU, BETA, F1, F2, ARGV,
     *                 RMATP(3,3), RMATN(3,3)
      LOGICAL ISSUN, ISVLA
c slalib functions:
      DOUBLE PRECISION sla_GMST, sla_EQEQX, sla_EPJ, sla_DRANRM
      INTEGER CONVRT(0:9), OTHBOD, IP, II

      DATA CONVRT / 11, 1, 2, 10, 4, 5, 6, 7, 8, 9 /

c 3.311 Relevant Time Arguments
c step a
      JEDP = DATEP + 2400000.5D0
c step b
      TPRIME = (JEDP - 2451545.0D0) / 36525.0D0
c step c
      MM = (357.528D0 + 35999.050D0 * TPRIME) * 2.0D0 * PI / 360.0D0
c step d
      SS = 0.001658D0 * DSIN (MM + 0.1671D0 * DSIN (MM))
      JED = JEDP + SS / 86400.0D0
      DATE = JED - 2400000.5D0
      TT = (JED - 2451545.0D0) / 36525.0D0
      ISVLA = OBSV .EQ. 'VLA'
      IF (.NOT. ISVLA) THEN
c this is VLBA, so do topocentric part...
c step a of topocentric part
c   first, find deltaT, the difference between TDT and UT1
         DELTAT = FINDT (sla_EPJ (DATEP))
         UT1 = DATEP - DELTAT / 86400.0D0
c step d of topocentric part
         GMST = sla_GMST (UT1)
         GAST = GMST + sla_EQEQX (DATEP)
c        LST = GAST + ELONG
c steps b, e & f of topocentric part
         CALL PVOBS (PHI, ELONG, HEIGHT, GAST, LITTLG)
c step g of topocentric part
c   Nutation:
         CALL sla_NUT (DATE, RMATN)
c   transpose...  
         CALL TRANSP (RMATN)
c   apply:
         CALL sla_DMXV (RMATN, LITTLG, BIGG)
         CALL sla_DMXV (RMATN, LITTLG(4), BIGG(4))
c   Precession:
         CALL sla_PRECL (2000.0D0, sla_EPJ (DATE), RMATP)
c   transpose...  
         CALL TRANSP (RMATP)
c   apply:
         CALL sla_DMXV (RMATP, BIGG, BIGG2)
         CALL sla_DMXV (RMATP, BIGG(4), BIGG2(4))
c step h of topocentric part
c   barycentric position and velocity of the earth
         CALL PLEPH (JED, 3, 12, EB)
         DO II = 1, 6
            EB(II) = EB(II) + BIGG2(II)
         END DO
      ELSE
c step e
c 3.312 Ephemeris Data for the Earth and Sun
c step f
c   barycentric position and velocity of the earth
         CALL PLEPH (JED, 3, 12, EB)
      END IF
c   barycentric position and velocity of the sun
      CALL PLEPH (JED, 11, 12, SB)
c   form the heliocentric position of the earth
      DO II = 1, 3
         EH(II) = EB(II) - SB(II)
      END DO
c step g
c   first, classify the other body, and convert it to JPL's notation...
      IP = NP
      IF (IP .LT. 0 .OR. IP .GT. 9) IP = 0
      OTHBOD = CONVRT(IP)
      ISSUN = OTHBOD .EQ. 11
c   barycentric position and velocity of the desired body
      CALL PLEPH (JED, OTHBOD, 12, UB)
c 3.314 Geometric Distance Between Earth and Planet
c step h
c   first, distance
      DIST = DSQRT ((UB(1) - EB(1)) ** 2.0D0 +
     *              (UB(2) - EB(2)) ** 2.0D0 +
     *              (UB(3) - EB(3)) ** 2.0D0)
c   then, light-time
      LITTIP = DIST / CC
c 3.315 Geocentric Position of Planet, Accounting for Light-Time
c precalculate this one:
      DE = DSQRT (EH(1) ** 2.0D0 + EH(2) ** 2.0D0 + EH(3) ** 2.0D0)
      LITTIM = 0.0D0
      DO WHILE (DABS (LITTIM - LITTIP) .GE. 1.0D-8)
         LITTIM = LITTIP
c step i
c   barycentric position and velocity of the desired body
         CALL PLEPH (JED-LITTIP, OTHBOD, 12, UB)
c   barycentric position and velocity of the sun
         IF (.NOT. ISSUN) CALL PLEPH (JED-LITTIP, 11, 12, SB)
c step j
         DO II = 1, 3
c   geocentric position of the planet
            UU(II) = UB(II) - EB(II)
c   heliocentric position of the planet
            IF (.NOT. ISSUN) QQ(II) = UB(II) - SB(II)
         END DO
c step k
c   first, magnitudes of vectors
         DU = DSQRT (UU(1) ** 2.0D0 + UU(2) ** 2.0D0 + UU(3) ** 2.0D0)
         IF (.NOT. ISSUN) THEN
            DQ = DSQRT (QQ(1)**2.0D0 + QQ(2)**2.0D0 + QQ(3)**2.0D0)
c   the second term is due to the relativistic delay caused by the
c   Sun's gravitational field, so we only calculate it (and the
c   necessary other quantities) if we're not looking at the Sun itself.
            LITTIP = (DU + (2.0D0 * MU / CC ** 2.0D0) *
     *                DLOG ((DE + DU + DQ) / (DE - DU + DQ))) / CC
         ELSE
            LITTIP = DU / CC
         END IF
      END DO
c step l
c   barycentric position and velocity of the desired body
      CALL PLEPH (JED-LITTIP, OTHBOD, 12, UB)
c   barycentric position and velocity of the sun
      CALL PLEPH (JED-LITTIP, 11, 12, SB)
      DO II = 1, 6
c   geocentric position of the planet
         UU(II) = UB(II) - EB(II)
c   heliocentric position of the planet
         QQ(II) = UB(II) - SB(II)
      END DO
c if this is the sun itself, or if this is VLBA, then don't do
c the gravitational deflection calculation
      IF (ISSUN .OR. .NOT. ISVLA) THEN
         DO II = 1, 3
            U1(II) = UU(II)
         END DO
      ELSE 
c 3.316 Relativistic Deflection of Light
c step m
         DU = DSQRT (UU(1) ** 2.0D0 + UU(2) ** 2.0D0 + UU(3) ** 2.0D0)
         DQ = DSQRT (QQ(1) ** 2.0D0 + QQ(2) ** 2.0D0 + QQ(3) ** 2.0D0)
         DO II = 1, 3
            LITTLU(II) = UU(II) / DU
            LITTLQ(II) = QQ(II) / DQ
            LITTLE(II) = EH(II) / DE
         END DO
         G1 = 2 * MU / (CC * CC * DE)
         QDOTE = LITTLQ(1) * LITTLE(1) + LITTLQ(2) * LITTLE(2) +
     *           LITTLQ(3) * LITTLE(3) 
         G2 = 1.0D0 + QDOTE
c step n
         UDOTQ = LITTLU(1) * LITTLQ(1) + LITTLU(2) * LITTLQ(2) +
     *           LITTLU(3) * LITTLQ(3) 
         EDOTU = LITTLE(1) * LITTLU(1) + LITTLE(2) * LITTLU(2) +
     *           LITTLE(3) * LITTLU(3) 
         DO II = 1, 3
            VEC1(II) = UDOTQ * LITTLE(II)
            VEC2(II) = EDOTU * LITTLQ(II)
         END DO
         DO II = 1, 3
            U1(II) = DU * (LITTLU(II) + (G1/G2) * (VEC1(II) - VEC2(II)))
         END DO
      END IF
c 3.317 Aberration of Light
c step o
      DU = DSQRT (U1(1) ** 2.0D0 + U1(2) ** 2.0D0 + U1(3) ** 2.0D0)
      DO II = 1, 3
         PP(II) = U1(II) / DU
         IF (ISVLA) THEN
            VV(II) = EB(II+3) / CC
         ELSE
c here is the complication of wanting the diurnal aberration part, 
c but not the annual part for VLBA positions.  BIGG2 contains only the 
c diurnal rotation part of the entire relative velocity, so just use it.
            VV(II) = BIGG2(II+3) / CC
         END IF
      END DO
      ARGV = VV(1) * VV(1) + VV(2) * VV(2) + VV(3) * VV(3)
      BETA = DSQRT (1.0D0 - ARGV)
      F1 = PP(1) * VV(1) + PP(2) * VV(2) + PP(3) * VV(3) 
      F2 = 1.0D0 + F1 / (1 + BETA)
c step p
      DO II = 1, 3
         U2(II) = (BETA * U1(II) + F2 * DU * VV(II)) / (1.0D0 + F1)
      END DO
c XXX don't know for sure if this is right, but it seems so...  XXX
      DO II = 4, 6
         U2(II) = UU(II)
      END DO
c 3.318 Precession, & 3.319 Nutation 
      IF (ISVLA) THEN
c    step q
         CALL sla_PRECL (2000.0D0, sla_EPJ (DATE), RMATP)
c    step s
         CALL sla_NUT (DATE, RMATN)
c    step r
         CALL sla_DMXV (RMATP, U2, U3)
         CALL sla_DMXV (RMATP, U2(4), U3(4))
c    step t
         CALL sla_DMXV (RMATN, U3, U4)
         CALL sla_DMXV (RMATN, U3(4), U4(4))
      ELSE
         DO II = 1, 6
            U4(II) = U2(II)
         END DO
      END IF
c    step u
      CALL sla_DCC2S (U4, RA, DEC)
      RA = sla_DRANRM (RA)
      DIST = DSQRT (U4(1) ** 2.0D0 + U4(2) ** 2.0D0 + U4(3) ** 2.0D0)
c rates...
      RARAT = (U4(1) * U4(5) - U4(2) * U4(4)) /
     *        (U4(1) ** 2.0D0 + U4(2) ** 2.0D0)
c   convert to seconds of time per UT day:
      RARAT = 4.32D4 * RARAT / PI
      DECRAT = ((U4(6) * (U4(1) ** 2.0D0 + U4(2) ** 2.0D0)) -
     *          (U4(3) * (U4(1) * U4(4) + U4(2) * U4(5)))) /
     *         ((U4(1) * U4(1) + U4(2) * U4(2) + U4(3) * U4(3)) *
     *          DSQRT (U4(1) * U4(1) + U4(2) * U4(2)))
c   convert to seconds of arc per UT day:
      DECRAT = 6.48D5 * DECRAT / PI
      RETURN
      END
