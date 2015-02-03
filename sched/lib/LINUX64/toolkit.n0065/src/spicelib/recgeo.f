C$Procedure      RECGEO ( Rectangular to geodetic )
 
      SUBROUTINE RECGEO ( RECTAN, RE, F,  LONG, LAT, ALT )
 
C$ Abstract
C
C     Convert from rectangular coordinates to geodetic coordinates.
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
C      CONVERSION,  COORDINATES
C
C$ Declarations
 
      DOUBLE PRECISION   RECTAN ( 3 )
      DOUBLE PRECISION   RE
      DOUBLE PRECISION   F
      DOUBLE PRECISION   LONG
      DOUBLE PRECISION   LAT
      DOUBLE PRECISION   ALT
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     RECTAN     I   Rectangular coordinates of a point.
C     RE         I   Equatorial radius of the reference spheroid.
C     F          I   Flattening coefficient.
C     LONG       O   Geodetic longitude of the point (radians).
C     LAT        O   Geodetic latitude  of the point (radians).
C     ALT        O   Altitude of the point above reference spheroid.
C
C$ Detailed_Input
C
C     RECTAN     The rectangular coordinates of a point.  
C
C     RE         Equatorial radius of a reference spheroid.  This
C                spheroid is a volume of revolution:  its horizontal
C                cross sections are circular.  The shape of the
C                spheroid is defined by an equatorial radius RE and
C                a polar radius RP.
C
C     F          Flattening coefficient = (RE-RP) / RE,  where RP is
C                the polar radius of the spheroid.
C
C$ Detailed_Output
C
C     LONG       Geodetic longitude of the input point.  This is the
C                angle between the prime meridian and the meridian
C                containing RECTAN.  The direction of increasing
C                longitude is from the +X axis towards the +Y axis.
C
C                LONG is output in radians.  The range of LONG is
C                [-pi, pi]. 
C
C     LAT        Geodetic latitude of the input point.  For a point P
C                on the reference spheroid, this is the angle between
C                the XY plane and the outward normal vector at P.
C                For a point P not on the reference spheroid, the
C                geodetic latitude is that of the closest point to P on
C                the spheroid. 
C
C                LAT is output in radians.  The range of LAT is
C                [-pi/2, pi/2].
C 
C
C     ALT        Altitude of point above the reference spheroid.
C
C                The units associated with ALT are those associated
C                with the input RECTAN.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the equatorial radius is non-positive, the error
C        SPICE(VALUEOUTOFRANGE) is signaled.
C
C     2) If the flattening coefficient is greater than or equal to
C        one, the error SPICE(VALUEOUTOFRANGE) is signaled.
C
C     3) For points inside the reference ellipsoid, the nearest
C        point on the ellipsoid to RECTAN may not be unique, so
C        latitude may not be well-defined. 
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Given the body-fixed rectangular coordinates of a point, and the
C     constants describing the reference spheroid,  this routine
C     returns the geodetic coordinates of the point.  The body-fixed
C     rectangular frame is that having the x-axis pass through the
C     0 degree latitude 0 degree longitude point.  The y-axis passes
C     through the 0 degree latitude 90 degree longitude.  The z-axis
C     passes through the 90 degree latitude point.  For some bodies
C     this coordinate system may not be a right-handed coordinate
C     system.
C
C$ Examples
C
C     This routine can be used to convert body fixed rectangular
C     coordinates (such as the Satellite Tracking and Data Network
C     of 1973) to geodetic coordinates such as those used by the
C     United States Geological Survey topographic maps.
C
C     The code would look something like this
C
C     C
C     C     Shift the STDN-73 coordinates to line up with the center
C     C     of the Clark66 reference system.
C     C
C           CALL VSUB ( STDNX, OFFSET, X )
C
C     C
C     C     Using the equatorial radius of the Clark66 spheroid
C     C     (CLARKR = 6378.2064 km) and the Clark 66 flattening
C     C     factor (CLARKF = 1.0D0 / 294.9787D0 ) convert to
C     C     geodetic coordinates of the North American Datum of 1927.
C     C
C           CALL RECGEO ( X, CLARKR, CLARKF, LONG, LAT, ALT )
C
C
C
C     Below are two tables.
C
C     Listed in the first table (under X(1), X(2) and X(3)) are a
C     number of points whose rectangular coordinates are
C     taken from the set {-1, 0, 1}.
C
C     The results of the code fragment
C
C          CALL RECGEO ( X, CLARKR, CLARKF, LONG, LAT, ALT )
C
C          Use the SPICELIB routine CONVRT to convert the angular
C          quantities to degrees
C
C          CALL CONVRT ( LAT,  'RADIANS', 'DEGREES', LAT  )
C          CALL CONVRT ( LONG, 'RADIANS', 'DEGREES', LONG )
C
C     are listed to 4 decimal places in the second parallel table under
C     LONG (longitude), LAT (latitude), and ALT (altitude).
C
C
C       X(1)       X(2)     X(3)         LONG      LAT        ALT
C       --------------------------       ----------------------------
C       0.0000     0.0000   0.0000       0.0000    90.0000   -6356.5838
C       1.0000     0.0000   0.0000       0.0000     0.0000   -6377.2063
C       0.0000     1.0000   0.0000      90.0000     0.0000   -6377.2063
C       0.0000     0.0000   1.0000       0.0000    90.0000   -6355.5838
C      -1.0000     0.0000   0.0000     180.0000     0.0000   -6377.2063
C       0.0000    -1.0000   0.0000     -90.0000     0.0000   -6377.2063
C       0.0000     0.0000  -1.0000       0.0000   -90.0000   -6355.5838
C       1.0000     1.0000   0.0000      45.0000     0.0000   -6376.7921
C       1.0000     0.0000   1.0000       0.0000    88.7070   -6355.5725
C       0.0000     1.0000   1.0000      90.0000    88.7070   -6355.5725
C       1.0000     1.0000   1.0000      45.0000    88.1713   -6355.5612
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     See FUNDAMENTALS OF ASTRODYNAMICS, Bate, Mueller, White
C     published by Dover for a description of geodetic coordinates.
C
C$ Author_and_Institution
C
C     C.H. Acton      (JPL)
C     N.J. Bachman    (JPL)
C     H.A. Neilan     (JPL)
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.3, 02-JUL-2007 (NJB)
C
C        In Examples section of header, description of right-hand
C        table was updated to use correct names of columns. Term
C        "bodyfixed" is now hyphenated.
C
C-    SPICELIB Version 1.0.2, 30-JUL-2003 (NJB) (CHA)
C
C        Various header changes were made to improve clarity.  Some
C        minor header corrections were made.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
C
C-&
 
C$ Index_Entries
C
C     rectangular to geodetic
C
C-&
 
 
 
 
C$ Revisions
C
C-    Beta Version 3.0.1, 9-JUN-1989 (HAN)
C
C        Error handling was added to detect and equatorial radius
C        whose value is less than or equal to zero.
C
C-    Beta Version 2.0.0, 21-DEC-1988 (HAN)
C
C        Error handling to detect invalid flattening coefficients
C        was added. Because the flattening coefficient is used to
C        compute the length of an axis, it must be checked so that
C        the length is greater than zero.
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
 
C
C     Local variables
C
 
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION C
      DOUBLE PRECISION BASE   (3)
      DOUBLE PRECISION NORMAL (3)
      DOUBLE PRECISION RADIUS
 
 
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'RECGEO' )
      END IF
 
 
C
C     The equatorial radius must be positive. If not, signal an error
C     and check out.
C
      IF ( RE. LE. 0.0D0 ) THEN
          CALL SETMSG ( 'Equatorial radius was *.' )
          CALL ERRDP  ( '*', RE                    )
          CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'   )
          CALL CHKOUT ( 'RECGEO'                   )
          RETURN
      END IF
 
 
C
C     If the flattening coefficient is greater than one, the length
C     of the 'C' axis computed below is negative. If it's equal to one,
C     the length of the axis is zero. Either case is a problem, so
C     signal an error and check out.
C
 
      IF ( F .GE. 1 ) THEN
          CALL SETMSG ( 'Flattening coefficient was *.'  )
          CALL ERRDP  ( '*', F                           )
          CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'         )
          CALL CHKOUT ( 'RECGEO'                         )
          RETURN
      END IF
 
 
C
C     Determine the lengths of the axes of the reference ellipsoid.
C
      A = RE
      B = RE
      C = RE - F*RE
 
C
C     Find the point on the reference spheroid closes to the input point
C
      CALL NEARPT ( RECTAN, A, B, C, BASE,   ALT    )
 
C
C     From this closest point determine the surface normal
C
      CALL SURFNM (         A, B, C, BASE,   NORMAL )
 
C
C     Using the surface normal, determine the latitude and longitude
C     of the input point.
C
      CALL RECLAT ( NORMAL, RADIUS,  LONG, LAT )
 
 
 
      CALL CHKOUT ( 'RECGEO' )
      RETURN
      END
