C$Procedure RECPGR ( Rectangular to planetographic )
 
      SUBROUTINE RECPGR ( BODY, RECTAN, RE, F, LON, LAT, ALT )
 
C$ Abstract
C
C     Convert rectangular coordinates to planetographic coordinates.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     KERNEL
C     NAIF_IDS
C     PCK
C
C$ Keywords
C
C     CONVERSION
C     COORDINATES
C     GEOMETRY
C     MATH
C
C$ Declarations
 
      IMPLICIT NONE

      INCLUDE               'zzctr.inc'

      CHARACTER*(*)         BODY
      DOUBLE PRECISION      RECTAN ( 3 )
      DOUBLE PRECISION      RE
      DOUBLE PRECISION      F
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      ALT
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     BODY       I   Body with which coordinate system is associated.
C     RECTAN     I   Rectangular coordinates of a point.
C     RE         I   Equatorial radius of the reference spheroid.
C     F          I   Flattening coefficient.
C     LON        O   Planetographic longitude of the point (radians).
C     LAT        O   Planetographic latitude of the point (radians).
C     ALT        O   Altitude of the point above reference spheroid.
C
C$ Detailed_Input
C
C     BODY       Name of the body with which the planetographic
C                coordinate system is associated.
C
C                BODY is used by this routine to look up from the
C                kernel pool the prime meridian rate coefficient giving
C                the body's spin sense.  See the Files and Particulars
C                header sections below for details.
C
C
C     RECTAN     The rectangular coordinates of a point.  Units
C                are arbitrary, except that the input RE must be
C                expressed in the same units.
C
C
C     RE         Equatorial radius of a reference spheroid.  This
C                spheroid is a volume of revolution:  its horizontal
C                cross sections are circular.  The shape of the
C                spheroid is defined by an equatorial radius RE and a
C                polar radius RP.  Units of RE must match those of
C                RECTAN.
C
C
C     F          Flattening coefficient = 
C
C                   (RE-RP) / RE
C
C                where RP is the polar radius of the spheroid, and the
C                units of RP match those of RE.
C
C$ Detailed_Output
C
C     LON        Planetographic longitude of the input point.  This is
C                the angle between the prime meridian and the meridian
C                containing RECTAN.  For bodies having prograde (aka
C                direct) rotation, the direction of increasing
C                longitude is positive west:  from the +X axis of the
C                rectangular coordinate system toward the -Y axis.
C                For bodies having retrograde rotation, the direction
C                of increasing longitude is positive east:  from the +X
C                axis toward the +Y axis.
C
C                The earth, moon, and sun are exceptions:
C                planetographic longitude is measured positive east for
C                these bodies.
C
C                The default interpretation of longitude by this
C                and the other planetographic coordinate conversion
C                routines can be overridden; see the discussion in
C                Particulars below for details.
C
C                LON is output in radians.  The nominal range of LON is
C                given by:
C
C                   0  <  LON  <  2*pi
C                      -       
C
C                However, round-off error could cause LON to equal 2*pi.
C
C
C     LAT        Planetographic latitude of the input point.  For a
C                point P on the reference spheroid, this is the angle
C                between the XY plane and the outward normal vector at
C                P. For a point P not on the reference spheroid, the
C                planetographic latitude is that of the closest point
C                to P on the spheroid.
C
C                LAT is output in radians. The range of LAT is given
C                by: 
C                
C                   -pi/2  <  LAT  <  pi/2
C                          -       -
C 
C
C     ALT        Altitude of point above the reference spheroid.
C
C                The units associated with ALT are those associated
C                with the input RECTAN and RE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the body name BODY cannot be mapped to a NAIF ID code,
C        and if BODY is not a string representation of an integer,
C        the error SPICE(IDCODENOTFOUND) will be signaled.
C  
C     2) If the kernel variable  
C
C           BODY<ID code>_PGR_POSITIVE_LON
C
C        is present in the kernel pool but has a value other than one
C        of
C
C            'EAST'
C            'WEST'
C
C        the error SPICE(INVALIDOPTION) will be signaled.  Case
C        and blanks are ignored when these values are interpreted.
C
C     3) If polynomial coefficients for the prime meridian of BODY
C        are not available in the kernel pool, and if the kernel
C        variable BODY<ID code>_PGR_POSITIVE_LON is not present in
C        the kernel pool, the error SPICE(MISSINGDATA) will be signaled.
C        
C     4) If the equatorial radius is non-positive, the error
C        SPICE(VALUEOUTOFRANGE) is signaled.
C
C     5) If the flattening coefficient is greater than or equal to one,
C        the error SPICE(VALUEOUTOFRANGE) is signaled.
C
C     6) For points inside the reference ellipsoid, the nearest point
C        on the ellipsoid to RECTAN may not be unique, so latitude may
C        not be well-defined.
C
C$ Files
C
C     This routine expects a kernel variable giving BODY's prime
C     meridian angle as a function of time to be available in the
C     kernel pool.  Normally this item is provided by loading a PCK
C     file.  The required kernel variable is named 
C
C        BODY<body ID>_PM 
C
C     where <body ID> represents a string containing the NAIF integer 
C     ID code for BODY.  For example, if BODY is 'JUPITER', then 
C     the name of the kernel variable containing the prime meridian 
C     angle coefficients is 
C
C        BODY599_PM
C
C     The optional kernel variable 
C     
C        BODY<body ID>_PGR_POSITIVE_LON
C
C     also is normally defined via loading a text kernel. When this
C     variable is present in the kernel pool, the prime meridian
C     coefficients for BODY are not required by this routine. See the
C     Particulars section below for details.
C
C$ Particulars
C
C     Given the body-fixed rectangular coordinates of a point, this
C     routine returns the planetographic coordinates of the point. The
C     body-fixed rectangular frame is that having the X-axis pass
C     through the 0 degree latitude 0 degree longitude direction, the
C     Z-axis pass through the 90 degree latitude direction, and the
C     Y-axis equal to the cross product of the unit Z-axis and X-axis
C     vectors.
C
C     The planetographic definition of latitude is identical to the
C     planetodetic (also called "geodetic" in SPICE documentation)
C     definition. In the planetographic coordinate system, latitude is
C     defined using a reference spheroid.  The spheroid is
C     characterized by an equatorial radius and a polar radius. For a
C     point P on the spheroid, latitude is defined as the angle between
C     the X-Y plane and the outward surface normal at P.  For a point P
C     off the spheroid, latitude is defined as the latitude of the
C     nearest point to P on the spheroid.  Note if P is an interior
C     point, for example, if P is at the center of the spheroid, there
C     may not be a unique nearest point to P.
C
C     In the planetographic coordinate system, longitude is defined
C     using the spin sense of the body.  Longitude is positive to the
C     west if the spin is prograde and positive to the east if the spin
C     is retrograde.  The spin sense is given by the sign of the first
C     degree term of the time-dependent polynomial for the body's prime
C     meridian Euler angle "W":  the spin is retrograde if this term is
C     negative and prograde otherwise.  For the sun, planets, most
C     natural satellites, and selected asteroids, the polynomial
C     expression for W may be found in a SPICE PCK kernel.
C
C     The earth, moon, and sun are exceptions: planetographic longitude
C     is measured positive east for these bodies.
C
C     If you wish to override the default sense of positive longitude
C     for a particular body, you can do so by defining the kernel
C     variable
C
C        BODY<body ID>_PGR_POSITIVE_LON
C
C     where <body ID> represents the NAIF ID code of the body. This
C     variable may be assigned either of the values
C
C        'WEST'
C        'EAST'
C
C     For example, you can have this routine treat the longitude
C     of the earth as increasing to the west using the kernel
C     variable assignment
C
C        BODY399_PGR_POSITIVE_LON = 'WEST'
C        
C     Normally such assignments are made by placing them in a text
C     kernel and loading that kernel via FURNSH.
C
C     The definition of this kernel variable controls the behavior of
C     the SPICELIB planetographic routines
C
C        PGRREC
C        RECPGR
C        DPGRDR
C        DRDPGR
C
C     It does not affect the other SPICELIB coordinate conversion
C     routines.
C
C$ Examples
C
C
C     Numerical results shown for this example may differ between
C     platforms as the results depend on the SPICE kernels used as
C     input and the machine specific arithmetic implementation.
C
C
C     1) Find the planetographic coordinates of the point having Mars
C        rectangular coordinates:
C
C           X (km) =      0.0
C           Y (km) =  -2620.678914818178
C           Z (km) =   2592.408908856967 
C
C        (These input values have been chosen to create "simple" output
C        values.)
C
C
C                 PROGRAM EX1
C                 IMPLICIT NONE
C           C
C           C     SPICELIB functions
C           C
C                 DOUBLE PRECISION      RPD
C           C
C           C     Local variables
C           C
C                 DOUBLE PRECISION      ALT
C                 DOUBLE PRECISION      F
C                 DOUBLE PRECISION      LAT
C                 DOUBLE PRECISION      LON
C                 DOUBLE PRECISION      RADII  ( 3 )
C                 DOUBLE PRECISION      RE
C                 DOUBLE PRECISION      RECTAN ( 3 )
C                 DOUBLE PRECISION      RP
C
C                 INTEGER               N
C           C
C           C     Load a PCK file containing a triaxial
C           C     ellipsoidal shape model and orientation
C           C     data for Mars.
C           C     
C                 CALL FURNSH ( 'pck00008.tpc' )
C             
C           C
C           C     Look up the radii for Mars.  Although we
C           C     omit it here, we could first call BADKPV
C           C     to make sure the variable BODY499_RADII
C           C     has three elements and numeric data type.
C           C     If the variable is not present in the kernel
C           C     pool, BODVRD will signal an error.
C           C
C                 CALL BODVRD ( 'MARS', 'RADII', 3, N, RADII )
C
C           C
C           C     Compute flattening coefficient.
C           C
C                 RE  =  RADII(1)
C                 RP  =  RADII(3)
C                 F   =  ( RE - RP ) / RE
C           
C           C
C           C     Do the conversion. 
C           C
C                 RECTAN(1) =      0.D0
C                 RECTAN(2) =  -2620.678914818178D0
C                 RECTAN(3) =   2592.408908856967D0
C
C                 CALL RECPGR ( 'MARS', RECTAN, RE, F, LON, LAT, ALT )
C
C                 WRITE (*,*) ' '
C                 WRITE (*,*) 'Rectangular coordinates:'
C                 WRITE (*,*) ' '
C                 WRITE (*,*) '  X (km)                 = ', RECTAN(1)
C                 WRITE (*,*) '  Y (km)                 = ', RECTAN(2)
C                 WRITE (*,*) '  Z (km)                 = ', RECTAN(3)
C                 WRITE (*,*) ' '
C                 WRITE (*,*) 'Ellipsoid shape parameters: '
C                 WRITE (*,*) ' '
C                 WRITE (*,*) '  Equatorial radius (km) = ', RE
C                 WRITE (*,*) '  Polar radius      (km) = ', RP
C                 WRITE (*,*) '  Flattening coefficient = ', F
C                 WRITE (*,*) ' '
C                 WRITE (*,*) 'Planetographic coordinates:'
C                 WRITE (*,*) ' '
C                 WRITE (*,*) '  Longitude (deg)        = ', LON / RPD()
C                 WRITE (*,*) '  Latitude  (deg)        = ', LAT / RPD()
C                 WRITE (*,*) '  Altitude  (km)         = ', ALT
C                 WRITE (*,*) ' '
C
C                 END
C
C
C        Output from this program should be similar to the following
C        (rounding and formatting will differ across platforms):
C
C           Rectangular coordinates:
C
C             X (km)                 =   0.
C             Y (km)                 =  -2620.67891
C             Z (km)                 =   2592.40891
C
C           Ellipsoid shape parameters:
C
C             Equatorial radius (km) =   3396.19
C             Polar radius      (km) =   3376.2
C             Flattening coefficient =   0.00588600756
C
C           Planetographic coordinates:
C
C             Longitude (deg)        =   90.
C             Latitude  (deg)        =   45.
C             Altitude  (km)         =   300.
C
C
C
C     2) Below is a table showing a variety of rectangular coordinates
C        and the corresponding Mars planetographic coordinates.  The
C        values are computed using the reference spheroid having radii
C
C           Equatorial radius:    3397
C           Polar radius:         3375
C
C        Note:  the values shown above may not be current or suitable
C               for your application.
C
C
C        Corresponding rectangular and planetographic coordinates are
C        listed to three decimal places.
C
C    RECTAN(1)    RECTAN(2)   RECTAN(3)    LON        LAT         ALT
C    ------------------------------------------------------------------
C     3397.000      0.000      0.000       0.000      0.000       0.000 
C    -3397.000      0.000      0.000     180.000      0.000       0.000 
C    -3407.000      0.000      0.000     180.000      0.000      10.000 
C    -3387.000      0.000      0.000     180.000      0.000     -10.000 
C        0.000  -3397.000      0.000      90.000      0.000       0.000 
C        0.000   3397.000      0.000     270.000      0.000       0.000 
C        0.000      0.000   3375.000       0.000     90.000       0.000 
C        0.000      0.000  -3375.000       0.000    -90.000       0.000 
C        0.000      0.000      0.000       0.000     90.000   -3375.000
C
C
C
C     3)  Below we show the analogous relationships for the earth,
C         using the reference ellipsoid radii
C
C            Equatorial radius:    6378.140
C            Polar radius:         6356.750
C
C         Note the change in longitudes for points on the +/- Y axis
C         for the earth vs the Mars values.
C
C
C    RECTAN(1)    RECTAN(2)   RECTAN(3)    LON        LAT         ALT
C    ----------------------------------  -------------------------------
C     6378.140      0.000      0.000       0.000      0.000       0.000 
C    -6378.140      0.000      0.000     180.000      0.000       0.000 
C    -6388.140      0.000      0.000     180.000      0.000      10.000 
C    -6368.140      0.000      0.000     180.000      0.000     -10.000 
C        0.000  -6378.140      0.000     270.000      0.000       0.000 
C        0.000   6378.140      0.000      90.000      0.000       0.000 
C        0.000      0.000   6356.750       0.000     90.000       0.000 
C        0.000      0.000  -6356.750       0.000    -90.000       0.000 
C        0.000      0.000      0.000       0.000     90.000   -6356.750
C
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     C.H. Acton      (JPL)
C     N.J. Bachman    (JPL)
C     H.A. Neilan     (JPL)
C     B.V. Semenov    (JPL)
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 21-SEP-2013 (BVS)
C
C        Updated to save the input body name and ZZBODTRN state
C        counter and to do name-ID conversion only if the counter
C        has changed.
C
C        Updated to call LJUCRS instead of CMPRSS/UCASE. 
C
C-    SPICELIB Version 1.0.1, 23-JAN-2008 (EDW)
C
C        Corrected typo in LAT range description, from:
C
C                   -pi/2  <  LAT  <  pi
C                          -       -
C
C        to:
C
C                   -pi/2  <  LAT  <  pi/2
C                          -       -
C
C-    SPICELIB Version 1.0.0, 26-DEC-2004 (CHA) (NJB) (HAN) (BVS) (WLT)
C
C-&
 
C$ Index_Entries
C
C     convert rectangular to planetographic coordinates
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      BRCKTD
      DOUBLE PRECISION      TWOPI

      INTEGER               PLNSNS

      LOGICAL               RETURN
 
C
C     Local parameters
C
      CHARACTER*(*)         PMTMP
      PARAMETER           ( PMTMP  = 'BODY#_PM' )

      CHARACTER*(*)         OVRTMP
      PARAMETER           ( OVRTMP = 'BODY#_PGR_POSITIVE_LON' )


      INTEGER               EARTH
      PARAMETER           ( EARTH  = 399 )

      INTEGER               KVNMLN
      PARAMETER           ( KVNMLN = 32  )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80  )

      INTEGER               MOON
      PARAMETER           ( MOON   = 301 )

      INTEGER               SENSLN
      PARAMETER           ( SENSLN = 4 )

      INTEGER               SUN
      PARAMETER           ( SUN    = 10  )

C
C     Saved body name length.
C
      INTEGER               MAXL
      PARAMETER           ( MAXL  = 36 )


C
C     Local variables
C 
      CHARACTER*(LNSIZE)    KVALUE
      CHARACTER*(KVNMLN)    PMKVAR
      CHARACTER*(SENSLN)    PGRLON
      
      INTEGER               BODYID
      INTEGER               N
      INTEGER               SENSE

      LOGICAL               FOUND

C
C     Saved name/ID item declarations.
C
      INTEGER               SVCTR1 ( CTRSIZ )
      CHARACTER*(MAXL)      SVBODY
      INTEGER               SVBDID
      LOGICAL               SVFND1

      LOGICAL               FIRST

C
C     Saved name/ID items.
C
      SAVE                  SVCTR1
      SAVE                  SVBODY
      SAVE                  SVBDID
      SAVE                  SVFND1

      SAVE                  FIRST

C
C     Initial values.
C
      DATA                  FIRST   / .TRUE. /


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'RECPGR' )
 
C
C     Initialization.
C
      IF ( FIRST ) THEN

C
C        Initialize counter.
C
         CALL ZZCTRUIN( SVCTR1 )

         FIRST = .FALSE.

      END IF
 
C
C     Convert the body name to an ID code.
C 
      CALL ZZBODS2C ( SVCTR1, SVBODY, SVBDID, SVFND1,
     .                BODY, BODYID, FOUND    )
       
      IF ( .NOT. FOUND ) THEN
 
         CALL SETMSG ( 'The value of the input argument BODY is #, '
     .   //            'this is not a recognized name of an '
     .   //            'ephemeris object. The cause of this '
     .   //            'problem may be that you need an updated '
     .   //            'version of the SPICE Toolkit. '           )
         CALL ERRCH  ( '#', BODY                                  )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'RECPGR'                                   )
         RETURN
 
      END IF

C
C     The equatorial radius must be positive. If not, signal an error
C     and check out.
C
      IF ( RE .LE. 0.0D0 ) THEN

         CALL SETMSG ( 'Equatorial radius was #.' )
         CALL ERRDP  ( '#', RE                    )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'   )
         CALL CHKOUT ( 'RECPGR'                   )
         RETURN

      END IF
 
C
C     If the flattening coefficient is greater than 1, the polar radius
C     is negative. If F is equal to 1, the polar radius is zero. Either
C     case is a problem, so signal an error and check out.
C
      IF ( F .GE. 1.D0 ) THEN

         CALL SETMSG ( 'Flattening coefficient was #.'  )
         CALL ERRDP  ( '#', F                           )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'         )
         CALL CHKOUT ( 'RECPGR'                         )
         RETURN

      END IF

C
C     Look up the longitude sense override variable from the
C     kernel pool.
C
      CALL REPMI  ( OVRTMP, '#',     BODYID, PMKVAR )
      CALL GCPOOL ( PMKVAR, 1, 1, N, KVALUE, FOUND  )

      IF ( FOUND ) THEN
C
C        Make sure we recognize the value of PGRLON.
C
         CALL LJUCRS ( 0, KVALUE, PGRLON )

         IF ( PGRLON .EQ. 'EAST' ) THEN

            SENSE =  1

         ELSE IF ( PGRLON .EQ. 'WEST' ) THEN

            SENSE = -1

         ELSE
            
            CALL SETMSG ( 'Kernel variable # may have the values ' //
     .                    'EAST or WEST.  Actual value was #.'    )
            CALL ERRCH  ( '#', PMKVAR                             )
            CALL ERRCH  ( '#', KVALUE                             )
            CALL SIGERR ( 'SPICE(INVALIDOPTION)'                  )
            CALL CHKOUT ( 'RECPGR'                                )
            RETURN

         END IF


      ELSE
C
C        Look up the spin sense of the body's prime meridian.
C
         SENSE = PLNSNS ( BODYID )

C
C        If the required prime meridian rate was not available, 
C        PLNSNS returns the code 0.  Here we consider this situation
C        to be an error.
C   
         IF ( SENSE .EQ. 0 ) THEN

            CALL REPMI  ( PMTMP, '#', BODYID, PMKVAR )

            CALL SETMSG ( 'Prime meridian rate coefficient defined ' //
     .                    'by kernel variable # is required but '    //
     .                    'not available for body #. '               )
            CALL ERRCH  ( '#', PMKVAR                                )
            CALL ERRCH  ( '#', BODY                                  )
            CALL SIGERR ( 'SPICE(MISSINGDATA)'                       )
            CALL CHKOUT ( 'RECPGR'                                   )
            RETURN

         END IF

C
C        Handle the special cases:  earth, moon, and sun.
C
         IF (      ( BODYID .EQ. EARTH ) 
     .        .OR. ( BODYID .EQ. MOON  ) 
     .        .OR. ( BODYID .EQ. SUN   )  ) THEN
            
            SENSE = 1

         END IF

      END IF 

C
C     At this point, SENSE is set to +/- 1.
C
C     Convert the input coordinates first to geodetic coordinates.
C
      CALL RECGEO ( RECTAN, RE, F, LON, LAT, ALT )


C     Adjust the longitude according to the sense of the body's
C     spin, or according to the override value if one is provided.
C
      LON = SENSE * LON

C
C     Convert the longitude from the range (-pi, pi] to [0, 2*pi),
C     the latter being the range of planetographic longitude.
C
      IF ( LON .LT. 0.D0 ) THEN

         LON = LON + TWOPI()

      END IF

C
C     Make sure round-off error doesn't take LON out of range.
C
      LON = BRCKTD (  LON,   0.D0,  TWOPI()  )

      CALL CHKOUT ( 'RECPGR' )
      RETURN
      END
