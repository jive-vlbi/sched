C$Procedure      LATREC ( Latitudinal to rectangular coordinates )
 
      SUBROUTINE LATREC ( RADIUS, LONG, LAT, RECTAN )
 
C$ Abstract
C
C     Convert from latitudinal coordinates to rectangular coordinates.
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
 
      DOUBLE PRECISION   RADIUS
      DOUBLE PRECISION   LONG
      DOUBLE PRECISION   LAT
      DOUBLE PRECISION   RECTAN ( 3 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     RADIUS     I   Distance of a point from the origin.
C     LONG       I   Longitude of point in radians. 
C     LAT        I   Latitude of point in radians. 
C     RECTAN     O   Rectangular coordinates of the point.
C
C$ Detailed_Input
C
C     RADIUS     Distance of a point from the origin.
C
C     LONG       Longitude of the input point.  This is the angle 
C                between the prime meridian and the meridian
C                containing the point.  The direction of increasing
C                longitude is from the +X axis towards the +Y axis.
C 
C                Longitude is measured in radians.  On input, the 
C                range of longitude is unrestricted.
C
C     LAT        Latitude of the input point.  This is the angle from
C                the XY plane of the ray from the origin through the
C                point.
C
C                Latitude is measured in radians. On input, the range
C                of latitude is unrestricted.
C
C$ Detailed_Output
C
C     RECTAN     The rectangular coordinates of the input point.
C                RECTAN is a 3-vector.
C
C                The units associated with RECTAN are those
C                associated with the input RADIUS.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine returns the rectangular coordinates of a point
C     whose position is input in latitudinal coordinates.
C
C     Latitudinal coordinates are defined by a distance from a central
C     reference point, an angle from a reference meridian, and an angle
C     above the equator of a sphere centered at the central reference
C     point.
C
C$ Examples
C
C     Below are two tables.
C
C     Listed in the first table (under R, LONG and LAT) are
C     latitudinal coordinate triples that approximately represent
C     points whose rectangular coordinates are taken from the set
C     {-1, 0, 1}.  (Angular quantities are given in degrees.)
C
C     The results of the code fragment
C
C          C
C          C     Use the SPICELIB routine CONVRT to convert the angular
C          C     quantities to radians
C          C
C                CALL CONVRT ( LAT,  'DEGREES', 'RADIANS', LAT  )
C                CALL CONVRT ( LONG, 'DEGREES', 'RADIANS', LONG )
C
C                CALL LATREC ( R, LONG, LAT, X )
C
C
C     are listed in the second parallel table under X(1), X(2) and X(3).
C
C
C       R         LONG       LAT           X(1)       X(2)     X(3)
C       --------------------------         --------------------------
C       0.0000    0.0000    0.0000         0.0000     0.0000   0.0000
C       1.0000    0.0000    0.0000         1.0000     0.0000   0.0000
C       1.0000   90.0000    0.0000         0.0000     1.0000   0.0000
C       1.0000    0.0000   90.0000         0.0000     0.0000   1.0000
C       1.0000  180.0000    0.0000        -1.0000     0.0000   0.0000
C       1.0000  -90.0000    0.0000         0.0000    -1.0000   0.0000
C       1.0000    0.0000  -90.0000         0.0000     0.0000  -1.0000
C       1.4142   45.0000    0.0000         1.0000     1.0000   0.0000
C       1.4142    0.0000   45.0000         1.0000     0.0000   1.0000
C       1.4142   90.0000   45.0000         0.0000     1.0000   1.0000
C       1.7320   45.0000   35.2643         1.0000     1.0000   1.0000
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
C-    SPICELIB Version 1.0.2, 29-JUL-2003 (NJB) (CHA)
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
C     latitudinal to rectangular coordinates
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
 
C
C     Convert to rectangular coordinates, storing the results in
C     temporary variables.
C
      X = RADIUS * DCOS(LONG) * DCOS(LAT)
      Y = RADIUS * DSIN(LONG) * DCOS(LAT)
      Z = RADIUS * DSIN(LAT)
 
C
C  Move the results to the output variables.
C
      RECTAN(1) = X
      RECTAN(2) = Y
      RECTAN(3) = Z

      RETURN
      END
