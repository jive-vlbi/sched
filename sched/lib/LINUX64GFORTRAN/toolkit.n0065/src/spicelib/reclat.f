C$Procedure      RECLAT ( Rectangular to latitudinal coordinates )
 
      SUBROUTINE RECLAT ( RECTAN, RADIUS, LONG, LAT )
 
C$ Abstract
C
C     Convert from rectangular coordinates to latitudinal coordinates.
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
 
      DOUBLE PRECISION   RECTAN ( 3 )
      DOUBLE PRECISION   RADIUS
      DOUBLE PRECISION   LONG
      DOUBLE PRECISION   LAT
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     RECTAN     I   Rectangular coordinates of the point.
C     RADIUS     O   Distance of a point from the origin.
C     LONG       O   Longitude of point in radians. 
C     LAT        O   Latitude of point in radians. 
C
C$ Detailed_Input
C
C     RECTAN     The rectangular coordinates of a point.
C
C$ Detailed_Output
C
C     RADIUS     Distance of a point from the origin.
C
C                The units associated with RADIUS are those
C                associated with the input RECTAN.
C
C     LONG       Longitude of the input point.  This is the angle 
C                between the prime meridian and the meridian
C                containing the point.  The direction of increasing
C                longitude is from the +X axis towards the +Y axis.
C 
C                LONG is output in radians.  The range of LONG is 
C                [ -pi, pi].
C
C
C     LAT        Latitude of the input point.  This is the angle from
C                the XY plane of the ray from the origin through the
C                point.
C
C                LAT is output in radians.  The range of LAT is 
C                [-pi/2, pi/2].
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C     This routine returns the latitudinal coordinates of a point
C     whose position is input in rectangular coordinates.
C
C     Latitudinal coordinates are defined by a distance from a central
C     reference point, an angle from a reference meridian, and an angle
C     above the equator of a sphere centered at the central reference
C     point.
C
C$ Exceptions
C
C     Error free.
C
C     1) If the X and Y components of RECTAN are both zero, the
C        longitude is set to zero.
C
C     2) If RECTAN is the zero vector, longitude and latitude are
C        both set to zero.
C
C$ Files
C
C     None.
C
C$ Examples
C
C     Below are two tables.
C
C     Listed in the first table (under X(1), X(2) and X(3) ) are a
C     number of points whose rectangular coordinates are
C     taken from the set {-1, 0, 1}.
C
C     The results of the code fragment
C
C              CALL RECLAT ( X, R, LONG, LAT )
C        C
C        C     Use the SPICELIB routine CONVRT to convert the angular
C        C     quantities to degrees
C        C
C              CALL CONVRT ( LAT,  'RADIANS', 'DEGREES', LAT  )
C              CALL CONVRT ( LONG, 'RADIANS', 'DEGREES', LONG )
C
C     are listed to 4 decimal places in the second parallel table under
C     R (radius), LONG (longitude), and  LAT (latitude).
C
C
C       X(1)       X(2)     X(3)        R         LONG      LAT
C       --------------------------      --------------------------
C       0.0000     0.0000   0.0000      0.0000    0.0000    0.0000
C       1.0000     0.0000   0.0000      1.0000    0.0000    0.0000
C       0.0000     1.0000   0.0000      1.0000   90.0000    0.0000
C       0.0000     0.0000   1.0000      1.0000    0.0000   90.0000
C      -1.0000     0.0000   0.0000      1.0000  180.0000    0.0000
C       0.0000    -1.0000   0.0000      1.0000  -90.0000    0.0000
C       0.0000     0.0000  -1.0000      1.0000    0.0000  -90.0000
C       1.0000     1.0000   0.0000      1.4142   45.0000    0.0000
C       1.0000     0.0000   1.0000      1.4142    0.0000   45.0000
C       0.0000     1.0000   1.0000      1.4142   90.0000   45.0000
C       1.0000     1.0000   1.0000      1.7320   45.0000   35.2643
C
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     C.H. Acton      (JPL)
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
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
C     rectangular to latitudinal coordinates
C
C-&
 
 
C$ Revisions
C
C-     Beta Version 1.0.1, 1-Feb-1989 (WLT)
C
C      Example section of header upgraded.
C
C-&
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION Z
      DOUBLE PRECISION BIG
 
C
C     Store rectangular coordinates in temporary variables
C
      BIG = MAX ( DABS(RECTAN(1)), DABS(RECTAN(2)), DABS(RECTAN(3)) )
 
      IF ( BIG .GT. 0 ) THEN
 
         X      = RECTAN(1) / BIG
         Y      = RECTAN(2) / BIG
         Z      = RECTAN(3) / BIG
 
         RADIUS = BIG * DSQRT (X*X + Y*Y + Z*Z)
 
         LAT    = DATAN2 ( Z, DSQRT(X*X + Y*Y) )
 
         X      = RECTAN(1)
         Y      = RECTAN(2)
 
         IF (X.EQ.0.D0 .AND. Y.EQ.0.D0) THEN
            LONG = 0.D0
         ELSE
            LONG = DATAN2 (Y,X)
         END IF
 
      ELSE
         RADIUS = 0.0D0
         LAT    = 0.D0
         LONG   = 0.D0
      END IF
 
      RETURN
      END
