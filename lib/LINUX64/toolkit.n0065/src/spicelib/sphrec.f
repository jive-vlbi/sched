C$Procedure      SPHREC ( Spherical to rectangular coordinates )
 
      SUBROUTINE SPHREC ( R, COLAT, LONG, RECTAN  )
 
C$ Abstract
C
C     Convert from spherical coordinates to rectangular coordinates.
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
 
      DOUBLE PRECISION   R
      DOUBLE PRECISION   COLAT
      DOUBLE PRECISION   LONG
      DOUBLE PRECISION   RECTAN ( 3 )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      R          I   Distance of a point from the origin.
C      COLAT      I   Angle of the point from the positive Z-axis.
C      LONG       I   Angle of the point from the XZ plane in radians.
C      RECTAN     O   Rectangular coordinates of the point.
C
C$ Detailed_Input
C
C      R          Distance of the point from the origin.
C
C      COLAT      Angle between the point and the positive z-axis.
C
C      LONG       Angle of the projection of the point to the XY
C                 plane from the positive X-axis.  The positive
C                 Y-axis is at longitude PI/2 radians.
C
C$ Detailed_Output
C
C      RECTAN     The rectangular coordinates of a point.
C
C$ Parameters
C
C      None.
C
C$ Particulars
C
C      This routine returns the rectangular coordinates of a point
C      whose position is input in spherical coordinates.
C
C      Spherical coordinates are defined by a distance from a central
C      reference point, an angle from a reference meridian, and an angle
C      from the z-axis.  The co-latitude of the positive Z-axis is
C      zero.  The longitude of the posive Y-axis is PI/2 radians.
C
C$ Examples
C
C     Below are two tables.
C
C     Listed in the first table (under R, COLAT and LONG ) are
C     spherical coordinate triples that approximately represent points
C     whose rectangular coordinates are taken from the set {-1, 0, 1}.
C     (Angular quantities are given in degrees.)
C
C     The result of the code fragment
C
C          Use the SPICELIB routine CONVRT to convert the angular
C          quantities to radians
C
C          CALL CONVRT ( COLAT, 'DEGREES', 'RADIANS', LAT  )
C          CALL CONVRT ( LONG,  'DEGREES', 'RADIANS', LONG )
C
C          CALL SPHREC ( R, COLAT, LONG, X )
C
C
C     are listed in the second parallel table under X(1), X(2) and X(3).
C
C       R          COLAT      LONG           X(1)       X(2)     X(3)
C       ----------------------------         --------------------------
C       0.0000     0.0000     0.0000         0.0000     0.0000   0.0000
C       1.0000    90.0000     0.0000         1.0000     0.0000   0.0000
C       1.0000    90.0000    90.0000         0.0000     1.0000   0.0000
C       1.0000     0.0000     0.0000         0.0000     0.0000   1.0000
C       1.0000    90.0000   180.0000        -1.0000     0.0000   0.0000
C       1.0000    90.0000   -90.0000         0.0000    -1.0000   0.0000
C       1.0000   180.0000     0.0000         0.0000     0.0000  -1.0000
C       1.4142    90.0000    45.0000         1.0000     1.0000   0.0000
C       1.4142    45.0000     0.0000         1.0000     0.0000   1.0000
C       1.4142    45.0000    90.0000         0.0000     1.0000   1.0000
C       1.7320    54.7356    45.0000         1.0000     1.0000   1.0000
C
C
C$ Restrictions
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C      None.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-    SPICELIB Version 1.0.3, 24-SEP-1997 (WLT)
C
C        The BRIEF I/O section was corrected so that it
C        correctly reflects the inputs and outputs.
C
C-    SPICELIB Version 1.0.2, 12-JUL-1995 (WLT)
C
C        The header documentation was corrected so that longitude
C        now is correctly described as the angle from the
C        XZ plane instead of XY.
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
C     spherical to rectangular coordinates
C
C-&
 
 
 
C$ Revisions
C
C-     Beta Version 1.0.1, 1-Feb-1989 (WLT)
C
C      Example section of header upgraded.
C
C-&
 
C
C     Local Variables
C
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION Z
 
C
C     Convert to rectangular coordinates, storing in the results in
C     temporary variables
C
      X = R * DCOS(LONG) * DSIN(COLAT)
      Y = R * DSIN(LONG) * DSIN(COLAT)
      Z = R * DCOS(COLAT)
 
C
C     Move the results to the output variables
C
      RECTAN(1) = X
      RECTAN(2) = Y
      RECTAN(3) = Z
 
      RETURN
      END
