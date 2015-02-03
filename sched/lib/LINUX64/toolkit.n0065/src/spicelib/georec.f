C$Procedure      GEOREC ( Geodetic to rectangular coordinates )
 
      SUBROUTINE GEOREC ( LONG, LAT, ALT,  RE, F,  RECTAN )
 
C$ Abstract
C
C     Convert geodetic coordinates to rectangular coordinates.
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
C     CONVERSION,  COORDINATES
C
C$ Declarations
 
      DOUBLE PRECISION   LONG
      DOUBLE PRECISION   LAT
      DOUBLE PRECISION   ALT
      DOUBLE PRECISION   RE
      DOUBLE PRECISION   F
      DOUBLE PRECISION   RECTAN ( 3 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     LONG       I   Geodetic longitude of point (radians).
C     LAT        I   Geodetic latitude  of point (radians).
C     ALT        I   Altitude of point above the reference spheroid.
C     RE         I   Equatorial radius of the reference spheroid.
C     F          I   Flattening coefficient.
C     RECTAN     O   Rectangular coordinates of point.
C
C$ Detailed_Input
C
C     LONG       Geodetic longitude of the input point.  This is the
C                angle between the prime meridian and the meridian
C                containing RECTAN.  The direction of increasing
C                longitude is from the +X axis towards the +Y axis.
C
C                Longitude is measured in radians.  On input, the 
C                range of longitude is unrestricted. 
C
C     LAT        Geodetic latitude of the input point.  For a point P
C                on the reference spheroid, this is the angle between
C                the XY plane and the outward normal vector at P.
C                For a point P not on the reference spheroid, the
C                geodetic latitude is that of the closest point to P on
C                the spheroid. 
C
C                Latitude is measured in radians.  On input, the 
C                range of latitude is unrestricted.
C 
C     ALT        Altitude of point above the reference spheroid.
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
C     RECTAN     The rectangular coordinates of a point.  
C
C                The units associated with RECTAN are those associated
C                with the input ALT.
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
C     2) If the equatorial radius is less than or equal to zero,
C        the error SPICE(VALUEOUTOFRANGE) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Given the geodetic coordinates of a point, and the constants
C     describing the reference spheroid,  this routine returns the
C     bodyfixed rectangular coordinates of the point.  The bodyfixed
C     rectangular frame is that having the x-axis pass through the
C     0 degree latitude 0 degree longitude point.  The y-axis passes
C     through the 0 degree latitude 90 degree longitude.  The z-axis
C     passes through the 90 degree latitude point.  For some bodies
C     this coordinate system may not be a right-handed coordinate
C     system.
C
C$ Examples
C
C     This routine can be used to convert body fixed geodetic
C     coordinates (such as the used for United States Geological
C     Survey topographic maps) to bodyfixed rectangular coordinates
C     such as the Satellite Tracking and Data Network of 1973.
C
C     The code would look something like this
C
C        C
C        C     Using the equatorial radius of the Clark66 spheroid
C        C     (CLARKR = 6378.2064 km) and the Clark 66 flattening
C        C     factor (CLARKF = 1.0D0 / 294.9787D0 ) convert to
C        C     body fixed rectangular coordinates.
C        C
C              CALL GEOREC ( LONG, LAT, ALT, CLARKR, CLARKF, X )
C
C        C
C        C     Add the North American Datum of 1927 to STDN 73 center
C        C     offset
C        C
C              CALL VADD   ( X, OFFSET, STDNX )
C
C
C     Below are two tables.
C
C     Listed in the first table (under LONG, LAT, and ALT ) are
C     geodetic coordinate triples that approximately represent points
C     whose rectangular coordinates are taken from the set {-1, 0, 1}.
C     (Angular quantities are given in degrees.)
C
C     The result of the code fragment
C
C        C
C        C     Use the SPICELIB routine CONVRT to convert the angular
C        C     quantities to degrees.
C        C
C              CALL CONVRT ( LAT,  'DEGREES', 'RADIANS', LAT  )
C              CALL CONVRT ( LONG, 'DEGREES', 'RADIANS', LONG )
C
C              CALL GEOREC ( LONG, LAT, ALT, CLARKR, CLARKF, X )
C
C
C     are listed in the second parallel table under X(1), X(2) and X(3).
C
C
C       LONG      LAT        ALT            X(1)       X(2)     X(3)
C       ------------------------------      --------------------------
C       0.0000    90.0000   -6356.5838      0.0000     0.0000   0.0000
C       0.0000     0.0000   -6377.2063      1.0000     0.0000   0.0000
C      90.0000     0.0000   -6377.2063      0.0000     1.0000   0.0000
C       0.0000    90.0000   -6355.5838      0.0000     0.0000   1.0000
C     180.0000     0.0000   -6377.2063     -1.0000     0.0000   0.0000
C     -90.0000     0.0000   -6377.2063      0.0000    -1.0000   0.0000
C       0.0000   -90.0000   -6355.5838      0.0000     0.0000  -1.0000
C      45.0000     0.0000   -6376.7921      1.0000     1.0000   0.0000
C       0.0000    88.7070   -6355.5725      1.0000     0.0000   1.0000
C      90.0000    88.7070   -6355.5725      0.0000     1.0000   1.0000
C      45.0000    88.1713   -6355.5612      1.0000     1.0000   1.0000
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     C.H. Acton      (JPL)
C     N.J. Bachman    (JPL)
C     H.A. Neilan     (JPL)
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     See FUNDAMENTALS OF ASTRODYNAMICS, Bate, Mueller, White
C     published by Dover for a description of geodetic coordinates.
C
C$ Version
C
C-    SPICELIB Version 1.0.2, 29-JUL-2003 (NJB) (CHA)
C
C        Various header changes were made to improve clarity.  Some
C        minor header corrections were made.
C
C-   SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C       Comment section for permuted index source lines was added
C       following the header.
C
C-   SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
C
C-&
 
C$ Index_Entries
C
C     geodetic to rectangular coordinates
C
C-&
 
 
 
C$ Revisions
C
C-    Beta Version 3.0.0, 9-JUN-1989  (HAN)
C
C        Error handling added to detect equatorial radius out of
C        range. If the equatorial radius is less than or equal to
C        zero, an error is signaled.
C
C-    Beta Version 2.0.0, 21-DEC-1988 (HAN)
C
C        Error handling to detect invalid flattening coefficients
C        was added. Because the flattening coefficient is used to
C        compute the polar radius, it must be checked so that the
C        polar radius greater than zero.
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL          RETURN
 
 
C
C     Local variables
C
      DOUBLE PRECISION HEIGHT
 
      DOUBLE PRECISION RP
      DOUBLE PRECISION CLMBDA
      DOUBLE PRECISION SLMBDA
      DOUBLE PRECISION CPHI
      DOUBLE PRECISION SPHI
      DOUBLE PRECISION BIG
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION SCALE
 
      DOUBLE PRECISION BASE   (3)
      DOUBLE PRECISION NORMAL (3)
 
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'GEOREC' )
      END IF
 
 
 
C
C     The equatorial radius must be greater than zero.
C
      IF ( RE .LE. 0.0D0 ) THEN
          CALL SETMSG ( 'Equatorial radius was *.' )
          CALL ERRDP  ( '*', RE                    )
          CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'   )
          CALL CHKOUT ( 'GEOREC'                   )
          RETURN
      END IF
 
C
C     If the flattening coefficient is greater than one, the polar
C     radius computed below is negative. If it's equal to one, the
C     polar radius is zero. Either case is a problem, so signal an
C     error and check out.
C
      IF ( F .GE. 1 ) THEN
          CALL SETMSG ( 'Flattening coefficient was *.'  )
          CALL ERRDP  ( '*', F                           )
          CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'         )
          CALL CHKOUT ( 'GEOREC'                         )
          RETURN
      END IF
 
 
C
C     Move the altitude to a temporary variable.
C
      HEIGHT = ALT
 
 
C
C     Compute the polar radius of the spheroid.
C
      RP     = RE - (F * RE)
 
C
C     Compute a scale factor needed for finding the rectangular
C     coordinates of a point with altitude 0 but the same geodetic
C     latitude and longitude as the input point.
C
      CPHI   = DCOS(LAT)
      SPHI   = DSIN(LAT)
      CLMBDA = DCOS(LONG)
      SLMBDA = DSIN(LONG)
 
      BIG    = MAX ( DABS(RE*CPHI), DABS(RP*SPHI) )
 
      X      = RE*CPHI / BIG
      Y      = RP*SPHI / BIG
 
      SCALE  = 1.0D0   / (BIG * DSQRT(X*X + Y*Y) )
 
C
C     Compute the rectangular coordinates of the point with zero
C     altitude.
C
      BASE(1) = SCALE * RE * RE * CLMBDA * CPHI
      BASE(2) = SCALE * RE * RE * SLMBDA * CPHI
      BASE(3) = SCALE * RP * RP * SPHI
 
C
C     Fetch the normal to the ellipsoid at this point.
C
      CALL SURFNM ( RE, RE, RP, BASE, NORMAL )
 
C
C     Move along the normal to the input point.
C
      CALL VLCOM ( 1.0D0, BASE, HEIGHT, NORMAL, RECTAN )
 
 
 
      CALL CHKOUT ( 'GEOREC' )
      RETURN
      END
