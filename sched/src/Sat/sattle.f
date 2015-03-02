      SUBROUTINE SATTLE( ISAT, ISCN, ISTA, INSTRU, SRA, SDEC, 
     1                  SDRA, SDDEC, T0, DISTAU, GEOVEL )
C
C     This is a version of SATEP that was modified by Walter Brisken
C     to read TLE files.  It is being interfaced to SCHED Dec. 4, 2014
C     by Craig Walker with John Benson watching.
C
C     The satellite ephemeris file in this case should contain exactly
C     two lines -- the two lines of the TLE.  For example:
C
C 1 27663U 03005A   14336.10735096 -.00000073  00000-0  00000+0 0  8911
C 2 27663  56.6910  20.9990 0077466  11.1259 344.3610  2.00560145 86762
C
C     -Walter 20141204
C
C     Original comments from SATEP as of the split:
C
C     Subroutine for SCHED that gets satellite positions from
C     spice files using the NAIF software from JPL.  This is
C     adapted from John Benson's testsched.f sample program.
C     The geocentric velocity is also returned to help with
C     Doppler setting.
C
C     In the initial incarnation, the position is as seen from
C     the Earth center.  The parallax effect should be added
C     eventually.
C
C     March 2007:  I think parallax was added long ago.
C
C     The spice subroutines will die without passing back error
C     messages if there is a problem.  Hence there is no point
C     in passing an error indication to this routines calling 
C     routine.
C
C     The VLBA on-line system does not take into account diurnal
C     aberration so formally this program should.  The planet
C     position program does.  But don't worry about that here.
C     The maximum effect is about 0.7 arcsecond.  That will not
C     contribute significantly to the pointing errors (which is
C     probably why it is not in the on-line system).  I don't
C     actually know it is not in the on-line system, but comments
C     in jplpo2.f say so.
C
C     The original version did not attempt to adjust for station motion.
C     The code for doing the adjustment was provided by Bill Junor,
C     Oct. 28, 2009.  Some rewriting, mostly to put in Bill's debug
C     statements, was done Feb. 01, 2010. RCW.
C
C
C     Initial version 2004 March 18.  R. C. Walker
C
      INCLUDE             'sched.inc'
C
C     Call arguments.  Most obvious.  T0 is reference time for 
C     rates and is taken to be the same as the scan start time.
C     ISAT is the satellite number (as per SATINI).
C     INSTRU is the instrument, probably 'VLBA'.  Not used for now.
C     Don't worry about the GFORTRAN complaint about being unused.
C     ISTA is the station number.  If 0, do geocentric.  If not,
C     do parallax correction for this station.
C
C     Returned arguments are:
C       SRA, SDEC are J2000 in radians.
C       SDRA is RA rate in RA coordinate time seconds per day.
C       SDDEC is Dec rate in arcseconds per day.
C       T0 is distance in AU.
C
C
      DOUBLE PRECISION    SRA, SDEC, SDRA, SDDEC, T0, DISTAU
      INTEGER             ISAT, ISCN, ISTA
      CHARACTER           INSTRU*(*)
C
C     SPICELIB Functions
C
      INTEGER               STRLEN
      PARAMETER           ( STRLEN =  32 )
C
C     Variables
C
      INTEGER               KSTA, PASS
      CHARACTER*(1)         FORMAT
      CHARACTER*20          TFORM, RAC, DECC
      CHARACTER*(STRLEN)    UTC
      CHARACTER*(STRLEN)    UTCBEG
      
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ETBEG
      DOUBLE PRECISION      STATE(6)
      DOUBLE PRECISION      X, Y, Z, DX, DY, DZ
      DOUBLE PRECISION      R, DIST, CONST, GEOVEL
      DOUBLE PRECISION      MDRA, MDDEC
      DOUBLE PRECISION      GMST, SLA_GMST, XT, YT, ZT
      INTEGER               PREC
      LOGICAL               SPDEBUG, KERLOAD(MAXSAT)
      CHARACTER             RESULT*255
C
C     For the station dependent rate calculation:
C
      DOUBLE PRECISION  RHOE, SXDOT, SYDOT, SZDOT, OMEGAE
      DOUBLE PRECISION  XS, YS, ZS
      DOUBLE PRECISION LSTIME, STHA, SRCEL, SRCAZ, STLATD, STLOND
C
C     Variables for handling TLE files - one set per station.
C
C     Geophysical constants for TLE propagation
C
      DOUBLE PRECISION GEO(8)
C
C     Orbital elements for TLEs
C
      DOUBLE PRECISION TLE(10,MAXSAT)
C
C     Epoch of the TLE data
C
      DOUBLE PRECISION TLEEPO(MAXSAT)
C
C     Variables for the frame conversion for TLEs.
C
      DOUBLE PRECISION PRECM(6,6), INVPRC(6,6), TMPSTA(6)
C
C     Text containing TLEs
C  
      CHARACTER TLETXT(2)*130
C
C     Variables for the TLE handling.
C
C     GEO(1) = J2 gravitational harmonic for earth
C     GEO(2) = J3 gravitational harmonic for earth
C     GEO(3) = J4 gravitational harmonic for earth
C     GEO(4) = KE: Square root of the GM for earth where GM 
C               is expressed in earth radii cubed per minutes squared.
C     GEO(5) = QO: Low altitude bound for atmospheric model in km.
C     GEO(6) = SO: High altitude bound for atmospheric model in km.
C     GEO(7) = RE: Equatorial radius of the earth in km.
C     GEO(8) = AE: Distance units/earth radius (normally 1)
C
      DATA  GEO(1) / 1.082616D-3 /
      DATA  GEO(2) / -2.53881D-6 /
      DATA  GEO(3) / -1.65597D-6 /
      DATA  GEO(4) / 7.43669161D-2 /
      DATA  GEO(5) / 120.0D0 /
      DATA  GEO(6) / 78.0D0 /
      DATA  GEO(7) / 6378.135D0 /
      DATA  GEO(8) / 1.0D0 /
C
      PARAMETER  (OMEGAE=2.D0*PI*SIDR/86400.D0)
C
      DATA             KERLOAD / MAXSAT*.TRUE. /
      SAVE             KERLOAD, TLEEPO, TLE
C -------------------------------------------------------------------- 
C     Set whether or not to print various debugging information.
C
      SPDEBUG = .FALSE.
C
C
C     Normal first debug line.
C
      IF( DEBUG .OR. SPDEBUG ) THEN
         MSGTXT = ' '
         CALL WLOG( 1, ' ' )
         WRITE( MSGTXT, '( A, 3I5, F14.5)' )
     1      '=++++++ SATTLE starting ++++++ ', ISAT, ISCN, ISTA, 
     2      STARTJ(ISCN)
         CALL WLOG( 1, MSGTXT )
      END IF
C
C     The calculations are done for the start time of scan ISCN.
C     The time is needed in an ascii format.  Recall that SCHED
C     times are MJD.
C
      T0 = STARTJ(ISCN)
      WRITE( UTCBEG, '( F20.9, A )' ) T0 + 2400000.5D0, ' JD'
C
C     Load the spice kernels or TLE files if not already done.
C
      IF( KERLOAD(ISAT) ) THEN
         IF( SPDEBUG ) THEN
            WRITE(*,*) 'SATTLE kernel files for satellite ', ISAT,
     1         KERFILE(ISAT), TLEFILE(ISAT)
         END IF
C
C        First load the leapseconds file into the kernel pool, so
C        we can convert the UTC time strings to ephemeris seconds
C        past J2000.
C
         CALL FURNSH ( KERFILE(ISAT) )
C
C        Get the TLE data.
C
         CALL VLBOPE( ITLE, TLEFILE(ISAT), 'TEXT', 'OLD', RESULT )
         READ(UNIT=ITLE,FMT='(A)') TLETXT(1)
         READ(UNIT=ITLE,FMT='(A)') TLETXT(2)
         CLOSE(ITLE)
C
C        Convert TLE text to element data
C
         CALL GETELM( 1990, TLETXT, TLEEPO(ISAT), TLE(1,ISAT) )
C
         KERLOAD(ISAT) = .FALSE.
C
      END IF
C
C     Convert the UTC time strings into DOUBLE PRECISION ETs.
C     Note that STR2ET and UTC2ET give the exact same result.
C
      CALL STR2ET ( UTCBEG, ETBEG )
C
C     Compute and print the state of the target body
C     as seen by the observer.  The output time will be in calendar
C     format, rounded to the nearest seconds.
C
      ET  =   ETBEG
C
C     Get initial position assuming infinite speed of light.
C     Then iterate twice to get the speed corrected time.
C
C     Another note (see below) from Jon Giorgini - Feb. 20, 2015:
C     Different routines are needed for high and low orbit cases.
C     The dividing line is at a period of 225 minutes.  Below that,
C     use EV2LIN and above use DPSPCE.  The period is in ELEMS(9)
C     from GETELMS and is in rad/minute.
C
      PASS = 1
      DO WHILE ( PASS .LE. 3 )
         IF( PASS .GE. 2 ) THEN
C
C           Correct for light travel time from object after the
C           first pass.
C
            X = STATE(1)
            Y = STATE(2)
            Z = STATE(3)
            DIST = SQRT( X*X + Y*Y + Z*Z )
            ET = ET - DIST/299792.458
         END IF
C
         IF( TLE(9,ISAT) .GE. ( TWOPI / 225.D0 ) ) THEN
            CALL EV2LIN(ET, GEO, TLE(1,ISAT), STATE)
         ELSE
            CALL DPSPCE(ET, GEO, TLE(1,ISAT), STATE)
         END IF
C
         PASS = PASS + 1
      END DO
C
C     Note from Jon D. Giorgini at JPL via Walter on Feb. 17, 2015:
C     STATE() returned by EV2LIN (basically, NORAD's SGP4 model
C     with some weighted averaging between epochs) is in the 
C     Earth-Centered Inertial (ECI) reference frame, and relative 
C     to the true equator and mean equinox" (TEME) of the epoch of 
C     the elements.
C
C     From SPKEZR (used for the bsp files), the STATE output is in 
C     the inertial J2000 system requested by the calling argument.
C
C     The following sequence was suggested by Giorgini to convert
C     the EV2LIN results to the same frame as the SPKEZR results.
C
C     Get rotation matrix from TEME @ET (sec past J2000 epoch) to J2000
C     PRECM is 6x6, goes from J2000 -> TEME

C
      CALL ZZTEME( ET, PRECM )
C
C     Invert state transformation matrix to go from TEME -> J2000
C   
      CALL INVSTM( PRECM, INVPRC )
C
C     Do transformation of state from EV2LIN's TEME to J2000
C
      CALL MXVG( INVPRC, STATE, 6, 6, TMPSTA )
C
C     Copy (overwrite) original TEME state vector store from EV2LIN
C
      CALL MOVED( TMPSTA, 6, STATE )
C
C
C     Some debugging printout.
C
      IF( SPDEBUG ) THEN
C
C        Convert the ET (ephemeris time) into a UTC time string
C        for displaying on the screen.
C
         FORMAT = 'C'
         PREC   =  0
         CALL ET2UTC ( ET, FORMAT, PREC, UTC )
C
C        Display the results of the state calculation.
C
         IF( SPDEBUG ) THEN
            WRITE (*,'(A,2X,A20,2X,F15.3,2X,F15.3,2X,F15.3,/
     +          18X, A, F15.5,2X,F15.5,2X,F15.5)') 'STATE:',
     +          UTC, STATE(1), STATE(2), STATE(3),
     +          'RATES:  ', STATE(4), STATE(5), STATE(6)
         END IF
C
      END IF
C
C     Construct SRA, Dec, and Distance
C     First get the geocentric position of the satellite.
C
      X = STATE(1)
      Y = STATE(2)
      Z = STATE(3)
      R = SQRT( X*X + Y*Y )
      DIST = SQRT( X*X + Y*Y + Z*Z )
      DISTAU = DIST / 149.6D6
C
      IF( ISTA .NE. 0 ) THEN
C
C        This routine can be called to get a geocentric position
C        or a station specific position.  For geocentric, use
C        ISTA = 0.  Here we do the station adjustment if required.
C        XYZ above are in km.  Telescope positions are in m.
C        The station coordinates are known in the Earth frame.  Need
C        to put them in the sky frame using the sidereal time.
C        Note that SCHED does not know UT1-UTC so this has to be 
C        an approximation.
C        
         KSTA = STANUM(ISTA)
         GMST = SLA_GMST( STARTJ(ISCN) )
         XT = XPOS(KSTA) * COS( GMST ) - YPOS(KSTA) * SIN( GMST )
         YT = XPOS(KSTA) * SIN( GMST ) + YPOS(KSTA) * COS( GMST )
         ZT = ZPOS(KSTA)
C        
C        Get the difference vector to take into account the parallax.
C        Note that there is no adjustment of the rates so this is
C        not exact.  Also issues of precession etc. are not being taken
C        into account.
C        
         X = X - XT / 1000.D0
         Y = Y - YT / 1000.D0
         Z = Z - ZT / 1000.D0
         R = SQRT( X*X + Y*Y )
         DIST = SQRT( X*X + Y*Y + Z*Z )
         DISTAU = DIST / 149.6D6
C        
C        Comparison with the planets section of SCHED shows agreement 
C        of the parallax to within about an arc second.  I'm not sure
C        what level of accuracy to expect from this calculation, but
C        that does seem adequate for pointing.  But the derived 
C        positions should not be used for correlation.  It is possible
C        that the difference is diurnal aberration which is not 
C        added here but is added in the planet software.
C
      END IF
C
C     Convert to RA and Dec.
C
      IF (R.EQ.0D0) THEN
         SRA = 0D0
      ELSE
         SRA = ATAN2(Y,X)
      END IF
      IF( SRA .LT. 0.D0 ) SRA = SRA + TWOPI
      IF (Z.EQ.0D0) THEN
         SDEC = 0D0
      ELSE
         SDEC = ATAN2(Z,R)
      END IF
C
C     Complain if the distance is clearly wrong.
C
      IF( DIST .EQ. 0.D0 ) THEN
         CALL ERRLOG( 'SATTLE: Zero distance to satellite?' )
      END IF
C
C     Write the results in various formats if in debug mode.
C
      IF( SPDEBUG ) THEN
         WRITE(*,*) ' Distance km, au ', DIST, DISTAU
         WRITE(*,*) ' Max parallax (arcsec): ', 
     1            6000.D0 * 3600.D0 / ( DIST * RADDEG )
         WRITE(*,*) ' RA, DEC radians:   ', SRA, '   ', SDEC
         WRITE(*,*) ' RA, DEC hr, deg:   ', 
     1              SRA / RADHR, '   ', SDEC / RADDEG
         RAC = TFORM( SRA, 'T', 0, 2, 10, 'hms' )
         DECC = TFORM( SDEC, ' ', 1, 2, 10, 'd''"' )
         WRITE(*,*) ' RA, DEC hr, deg:   ', RAC, '   ', DECC
C
C        Add some debugging stuff from Bill Junor's version.
C        None of the geometry calculations here are used in the
C        calculations that follow for source positions etc.
C
C        Get the latitude, longitude of the station.
C
         IF( ISTA .NE. 0 ) THEN
            STLATD = LAT(KSTA) 
            STLOND = LONG(KSTA)
C
            WRITE(*,'( A, 2F10.4 )' )  '  STLATD, STLOND:', 
     1             STLATD, STLOND
C
C           Calculate the Local Sidereal Time. 
C           Remember W is negative longitude.
C
            LSTIME = GMST - LONG(KSTA)
C
C           Calculate the source Hour Angle.
C
            STHA = LSTIME - SRA
C
            WRITE(*,'( A, 2F10.4 )' )  '  LSTIME, STHA:', LSTIME, STHA
C
C           Calculate the elevation and azimuth of the source.
C
            SRCEL = ASIN( SIN(SDEC) * SIN(LAT(KSTA)) 
     1                + COS(SDEC) * COS(LAT(KSTA)) * COS(STHA) )

            SRCAZ = ACOS( SIN(SDEC) / (COS(SRCEL) * COS(LAT(KSTA)))
     1                - TAN(SRCEL) * TAN(LAT(KSTA)) ) 
C
C           Write some results if in debug mode.
C
            WRITE(*,'( A, I4 )' ) 'Station # ', KSTA
            WRITE(*,'( A, 2F10.4 )' )  '  STLAT, STLONG:', 
     1                                 LAT(KSTA), LONG(KSTA)
            WRITE(*,'( A, 3F14.4 )' )  
     1                     '  ET, SRCAZ (deg), SRCEL (deg):  ', 
     2                        ET, SRCAZ/RADDEG, SRCEL/RADDEG
C
         END IF
      END IF
C
C     Get the rates in the right format.  
C
C     Original did not attempt to adjust for station motion.
C     The code for doing the adjustment was provided by Bill Junor,
C     Oct. 28, 2009
C
      DX = STATE(4)
      DY = STATE(5)
      DZ = STATE(6)
C
C     Beginning of code for station dependent rates
C
C     Do an explicit correction for the station motion. See
C     "Explanatory Supplement to the Astronomical Almanac" 2006, P.K.
C     Seidelmann (ed.), University Science Books, p.133.
C     [Equation 3.254-4 is what we need.]
C     *** Do the correction ONLY if there is a station!
C
      IF (ISTA .NE. 0) THEN
C
C        Take care with units. These are in meters.
C
         XS = XPOS(KSTA)
         YS = YPOS(KSTA)
         ZS = ZPOS(KSTA)
C
C        But DX, DY, DZ are in km/s.
C
         RHOE = SQRT(XS*XS + YS*YS + ZS*ZS) / 1000.0D0
C
C        *** Care with sense of longitude! LA is retrieved from 
C        catalog as 1.854 radians but it is 106.24560 degrees W.
C
         SXDOT = -1.0D0 * OMEGAE *
     1           RHOE * COS(LAT(KSTA)) * SIN(GMST - LONG(KSTA))
C
         SYDOT =          OMEGAE *
     1           RHOE * COS(LAT(KSTA)) * COS(GMST - LONG(KSTA))
C
         SZDOT = 0.0D0
C
C        Make the correction. Get the sense correct.
C        For geosynch satellite this is the correct sense.
C
         DX = DX - SXDOT
         DY = DY - SYDOT
         DZ = DZ - SZDOT
C
      ENDIF
C
C     A good reference for this is "Explanatory Supplement to the
C     Astronomical Almanac" 2006, P.K. Seidelmann (ed.), University
C     Science Books, p.335. [Equations 6.15-8 are what we need.]
C
      MDRA = -1.0 * DX * SIN( SRA ) + DY * COS( SRA )
      MDDEC = DZ * COS( SDEC ) - 
     1     ( DX * COS( SRA ) + DY * SIN( SRA ) ) * SIN( SDEC )
C
C     VLBA uses changes in coordinate value (time or arc sec) per day.
C     CONST converts from km/s to arcsec/day
C
      IF( SPDEBUG ) THEN
         WRITE(*,'( A, F20.2 )' ) ' Dist: ', DIST
      END IF
      CONST = 86400.D0 * 3600.D0 / ( DIST * RADDEG )
      SDDEC = MDDEC * CONST
      IF( COS( SDEC ) .NE. 0.0 ) THEN
         SDRA =  MDRA * CONST / ( COS( SDEC ) * 15.D0 )
      ELSE
         SDRA = 0.D0
      END IF
C
C     Get the geocentric velocity for the station catalog so that
C     Doppler can be used.  Only do if it is not already there so
C     that the value from the original SATGOT routine call is used.
C
      GEOVEL = ( X * DX + Y * DY + Z * DZ ) / DIST
      IF( SPDEBUG ) THEN
         WRITE(*,'( A, F20.2 )' ) ' Dist2: ', DIST
      END IF
C
C     Write some results if in debug mode.
C
      IF( SPDEBUG ) THEN
         WRITE(*,'( A, 3F15.4 )' ) '  ET, DRA, DDEC:', ET, MDRA, MDDEC
         WRITE(*,'( A, 3F15.4 )' ) '  ET, SDRA, SDDEC:', 
     1                                                  ET, SDRA, SDDEC
      END IF
C
      RETURN
      END
