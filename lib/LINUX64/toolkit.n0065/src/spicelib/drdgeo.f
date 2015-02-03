C$Procedure      DRDGEO ( Derivative of rectangular w.r.t. geodetic )
  
      SUBROUTINE DRDGEO ( LONG, LAT, ALT, RE, F, JACOBI )
      IMPLICIT NONE
 
C$ Abstract
C
C     This routine computes the Jacobian of the transformation from
C     geodetic to rectangular coordinates.
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
C     None.
C
C$ Keywords
C
C     COORDINATES
C     DERIVATIVES
C     MATRIX
C
C$ Declarations
 
      DOUBLE PRECISION      LONG
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      RE
      DOUBLE PRECISION      F
      DOUBLE PRECISION      JACOBI ( 3, 3 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     LONG       I   Geodetic longitude of point (radians).
C     LAT        I   Geodetic latitude of point (radians).
C     ALT        I   Altitude of point above the reference spheroid.
C     RE         I   Equatorial radius of the reference spheroid.
C     F          I   Flattening coefficient.
C     JACOBI     O   Matrix of partial derivatives.
C
C$ Detailed_Input
C
C     LONG       Geodetic longitude of point (radians).
C
C     LAT        Geodetic latitude  of point (radians).
C
C     ALT        Altitude of point above the reference spheroid.
C
C     RE         Equatorial radius of the reference spheroid.
C
C     F          Flattening coefficient = (RE-RP) / RE,  where RP is
C                the polar radius of the spheroid.  (More importantly
C                RP = RE*(1-F).)
C
C$ Detailed_Output
C
C     JACOBI     is the matrix of partial derivatives of the conversion
C                between geodetic and rectangular coordinates.  It
C                has the form
C
C                   .-                              -.
C                   |  DX/DLONG   DX/DLAT  DX/DALT   |
C                   |  DY/DLONG   DY/DLAT  DY/DALT   |
C                   |  DZ/DLONG   DZ/DLAT  DZ/DALT   |
C                   `-                              -'
C
C                evaluated at the input values of LONG, LAT and ALT.
C
C                The formulae for computing X, Y, and Z from
C                geodetic coordinates are given below.
C
C                   X = [ALT +          RE/G(LAT,F)]*COS(LONG)*COS(LAT)
C                   Y = [ALT +          RE/G(LAT,F)]*SIN(LONG)*COS(LAT)
C                   Z = [ALT + RE*(1-F)**2/G(LAT,F)]*          SIN(LAT)
C
C                where
C
C                   RE is the polar radius of the reference spheroid.
C
C                   F  is the flattening factor (the polar radius is
C                      obtained by multiplying the equatorial radius by
C                      1-F).
C
C                   G( LAT, F ) is given by
C
C                      sqrt ( cos(lat)**2 + (1-f)**2 * sin(lat)**2 )
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the flattening coefficient is greater than or equal to
C        one, the error SPICE(VALUEOUTOFRANGE) is signaled.
C
C     2) If the equatorial radius is non-positive, the error
C        SPICE(BADRADIUS) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     It is often convenient to describe the motion of an object in
C     the geodetic coordinate system.  However, when performing
C     vector computations its hard to beat rectangular coordinates.
C
C     To transform states given with respect to geodetic coordinates
C     to states with respect to rectangular coordinates, one makes use
C     of the Jacobian of the transformation between the two systems.
C
C     Given a state in geodetic coordinates
C
C          ( long, lat, alt, dlong, dlat, dalt )
C
C     the velocity in rectangular coordinates is given by the matrix
C     equation:
C
C                    t          |                                   t
C        (dx, dy, dz)   = JACOBI|              * (dlong, dlat, dalt)
C                               |(long,lat,alt)
C
C
C     This routine computes the matrix 
C
C              |
C        JACOBI|
C              |(long,lat,alt)
C
C$ Examples
C
C     Suppose that one has a model that gives radius, longitude and
C     latitude as a function of time (long(t), lat(t), alt(t) ) for
C     which the derivatives ( dlong/dt, dlat/dt, dalt/dt ) are
C     computable.
C
C     To find the velocity of the object in bodyfixed rectangular
C     coordinates, one simply multiplies the Jacobian of the
C     transformation from geodetic to rectangular coordinates,
C     evaluated at (long(t), lat(t), alt(t) ), by the vector of 
C     derivatives of the geodetic coordinates.
C
C     In code this looks like:
C
C        C
C        C     Load the derivatives of long, lat, and alt into the
C        C     geodetic velocity vector GEOV.
C        C
C              GEOV(1) = DLONG_DT ( T )
C              GEOV(2) = DLAT_DT  ( T )
C              GEOV(3) = DALT_DT  ( T )
C
C        C
C        C     Determine the Jacobian of the transformation from
C        C     geodetic to rectangular coordinates at the geodetic 
C        C     coordinates of time T.
C        C
C              CALL DRDGEO ( LONG(T), LAT(T), ALT(T), RE, F, JACOBI )
C
C        C
C        C     Multiply the Jacobian on the right by the geodetic
C        C     velocity to obtain the rectangular velocity RECV.
C        C
C              CALL MXV ( JACOBI, GEOV, RECV )
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
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 20-JUL-2001 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Jacobian of rectangular w.r.t. geodetic coordinates
C
C-&
 
C$ Revisions
C
C     None.
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL          RETURN
 
C
C     Local parameters
C
      INTEGER               DX
      PARAMETER           ( DX     = 1 )
 
      INTEGER               DY
      PARAMETER           ( DY     = 2 )
 
      INTEGER               DZ
      PARAMETER           ( DZ     = 3 )
 
      INTEGER               DLON
      PARAMETER           ( DLON   = 1 )
 
      INTEGER               DLAT
      PARAMETER           ( DLAT   = 2 )
 
      INTEGER               DALT
      PARAMETER           ( DALT   = 3 )
 
C
C     Local variables
C
      DOUBLE PRECISION      CLAT
      DOUBLE PRECISION      CLON
      DOUBLE PRECISION      DGDLAT
      DOUBLE PRECISION      G
      DOUBLE PRECISION      SLAT
      DOUBLE PRECISION      SLON

      DOUBLE PRECISION      FLAT
      DOUBLE PRECISION      FLAT2
      DOUBLE PRECISION      G2
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DRDGEO' )
      END IF
 
C
C     If the flattening coefficient is greater than one, the polar
C     radius computed below is negative. If it's equal to one, the
C     polar radius is zero. Either case is a problem, so signal an
C     error and check out.
C
      IF ( F .GE. 1.0D0 ) THEN

          CALL SETMSG ( 'Flattening coefficient was *.'  )
          CALL ERRDP  ( '*', F                           )
          CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'         )
          CALL CHKOUT ( 'DRDGEO'                         )
          RETURN

      END IF
 
      IF ( RE .LE. 0.0D0 ) THEN

          CALL SETMSG ( 'Equatorial Radius <= 0.0D0. RE = *' )
          CALL ERRDP  ( '*', RE                              )
          CALL SIGERR ( 'SPICE(BADRADIUS)'                   )
          CALL CHKOUT ( 'DRDGEO'                             )
          RETURN

      END IF
  
C
C     For the record, here is a derivation of the formulae for the
C     values of x, y and z as a function of longitude, latitude and
C     altitude.
C
C     First, let's take the case where the longitude is 0. Moreover,
C     lets assume that the length of the equatorial axis is a and
C     that the polar axis is b:
C
C        a = re
C        b = re * (1-f)
C
C     For any point on the spheroid where y is zero we know that there
C     is a unique q in the range (-Pi, Pi] such that
C
C        x = a cos(q) and z = b sin(q).
C
C     The normal to the surface at such a point is given by
C
C           cos(q)     sin(q)
C        ( ------- ,  ------- )
C             a          b
C
C     The unit vector in the same direction is
C
C                 b cos(q)                         a sin(q)
C        ( --------------------------  ,  -------------------------- )
C             ______________________         ______________________
C            / 2   2        2   2           / 2   2        2   2
C          \/ b cos (q)  + a sin (q)      \/ b cos (q)  + a sin (q)
C
C
C     The first component of this term is by definition equal to the
C     cosine of the geodetic latitude, thus
C
C                                ______________________
C                               / 2   2        2   2
C        b cos(q) = cos(lat)  \/ b cos (q)  + a sin (q)
C
C
C     This can be transformed to the equation
C
C                                ______________________________
C                               /   2    2     2        2
C        b cos(q) = cos(lat)  \/ ( b  - a  )cos (q)  + a
C
C
C     Squaring both sides and rearranging terms gives:
C
C         2   2         2         2   2     2        2    2
C        b cos (q) + cos (lat) ( a - b ) cos (q) =  a  cos (lat)
C
C     Thus
C                           2    2
C           2              a  cos (lat)
C        cos (q)  =  --------------------------
C                     2    2         2   2
C                    b  sin (lat) + a cos (lat)
C
C
C
C                             cos (lat)
C                 =  ------------------------------
C                       _____________________________
C                      /      2    2           2
C                    \/  (b/a)  sin (lat) + cos (lat)
C
C
C
C                             cos (lat)
C                 =  ---------------------------------
C                       _____________________________
C                      /      2    2           2
C                    \/  (1-f)  sin (lat) + cos (lat)
C
C
C
C     From this one can also conclude that
C
C
C                           (1-f) sin (lat)
C        sin(q)   =  ----------------------------------
C                        _____________________________
C                       /      2    2           2
C                     \/  (1-f)  sin (lat) + cos (lat)
C
C
C
C     Thus the point on the surface of the spheroid is given by
C
C                            re * cos (lat)
C        x_0      =  ---------------------------------
C                        _____________________________
C                       /      2    2           2
C                     \/  (1-f)  sin (lat) + cos (lat)
C
C
C
C                                  2
C                        re * (1-f) sin (lat)
C        z_0      =  ----------------------------------
C                        _____________________________
C                       /      2    2           2
C                     \/  (1-f)  sin (lat) + cos (lat)
C
C
C     Thus given a point with the same latitude but a non-zero
C     longitude, one can conclude that
C
C                         re * cos (long) *cos (lat)
C        x_0      =  ---------------------------------
C                        _____________________________
C                       /      2    2           2
C                     \/  (1-f)  sin (lat) + cos (lat)
C
C
C
C                         re * sin (long) cos (lat)
C        y_0      =  ---------------------------------
C                        _____________________________
C                       /      2    2           2
C                     \/  (1-f)  sin (lat) + cos (lat)
C
C
C                                    2
C                          re * (1-f) sin (lat)
C        z_0      =  ----------------------------------
C                        _____________________________
C                       /      2    2           2
C                     \/  (1-f)  sin (lat) + cos (lat)
C
C
C     The unit normal, n, at this point is simply
C
C        ( cos(long)cos(lat),  sin(long)cos(lat),  sin(lat) )
C
C
C     Thus for a point at altitude alt, we simply add the vector
C
C        alt*n
C
C     to the vector ( x_0, y_0, z_0 ).  Hence we have
C
C        x = [ alt +          re/g(lat,f) ] * cos(long) * cos(lat)
C        y = [ alt +          re/g(lat,f) ] * sin(long) * cos(lat)
C        z = [ alt + re*(1-f)**2/g(lat,f) ] *             sin(lat)
C
C
C     We're going to need the sine and cosine of LAT and LONG many 
C     times.  We'll just compute them once.
C
      CLAT              = DCOS ( LAT  )
      CLON              = DCOS ( LONG )
      SLAT              = DSIN ( LAT  )
      SLON              = DSIN ( LONG )
 
C
C     Referring to the G given in the header we have...
C
      FLAT              = 1.D0 - F
      FLAT2             = FLAT * FLAT

      G                 = DSQRT( CLAT*CLAT   +   FLAT2 * SLAT*SLAT )
      G2                = G * G
      DGDLAT            = ( -1.0D0  +  FLAT2 ) * SLAT * CLAT / G

C
C     Now simply take the partial derivatives of the x,y,z w.r.t.
C     long,lat, alt.
C
      JACOBI (DX,DLON)  =  - ( ALT + RE/G ) * SLON * CLAT
      JACOBI (DY,DLON)  =    ( ALT + RE/G ) * CLON * CLAT
      JACOBI (DZ,DLON)  =    0.0D0
 
 
      JACOBI (DX,DLAT)  =    (      -  RE*DGDLAT / G2 ) * CLON * CLAT
     .                     - ( ALT  +  RE        / G  ) * CLON * SLAT
 
      JACOBI (DY,DLAT)  =    (      -  RE*DGDLAT / G2 ) * SLON * CLAT
     .                     - ( ALT  +  RE        / G  ) * SLON * SLAT
 
      JACOBI (DZ,DLAT)  =    (      -  FLAT2*RE*DGDLAT / G2 ) * SLAT
     .                     + ( ALT  +  FLAT2*RE        / G  ) * CLAT
 
  
      JACOBI (DX,DALT)  =  CLON * CLAT
      JACOBI (DY,DALT)  =  SLON * CLAT
      JACOBI (DZ,DALT)  =         SLAT
 

      CALL CHKOUT ( 'DRDGEO' )
      RETURN
      END
