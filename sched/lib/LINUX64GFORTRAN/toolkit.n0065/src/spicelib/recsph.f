C$Procedure      RECSPH ( Rectangular to spherical coordinates )
 
      SUBROUTINE RECSPH ( RECTAN, R, COLAT, LONG  )
 
C$ Abstract
C
C     Convert from rectangular coordinates to spherical coordinates.
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
      DOUBLE PRECISION   R
      DOUBLE PRECISION   COLAT
      DOUBLE PRECISION   LONG
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      RECTAN     I   Rectangular coordinates of a point.
C      R          O   Distance of the point from the origin.
C      COLAT      O   Angle of the point from the positive Z-axis.
C      LONG       O   Longitude of the point radians.
C
C$ Detailed_Input
C
C      RECTAN     The rectangular coordinates of a point.
C
C$ Detailed_Output
C
C      R          Distance of the point from the origin.
C
C      COLAT      Angle between the point and the positive z-axis.
C
C      LONG       Longitude of the point in radians.  This is the angle
C                 between the positive X-axis and the orthogonal
C                 projection of the point onto the XY plane.  LONG
C                 increases in the counterclockwise sense about the
C                 positive Z-axis.  The range of LONG is:
C
C                    -pi < LONG <= pi
C
C$ Parameters
C
C      None.
C
C$ Particulars
C
C      This routine returns the spherical coordinates of a point
C      whose position is input in rectangular coordinates.
C
C      Spherical coordinates are defined by a distance from a central
C      reference point, an angle from a reference meridian, and an angle
C      from the z-axis.
C
C$ Examples
C
C     Below are two tables.
C
C     Listed in the first table (under X(1), X(2) and X(3) ) are a
C     number of points whose rectangular coordinates are
C     taken from the set {-1, 0, 1}.
C
C     The result of the code fragment
C
C          CALL RECSPH ( X, R, COLAT, LONG )
C
C          Use the SPICELIB routine CONVRT to convert the angular
C          quantities to degrees
C
C          CALL CONVRT ( COLAT, 'RADIANS', 'DEGREES', COLAT )
C          CALL CONVRT ( LONG,  'RADIANS', 'DEGREES', LONG  )
C
C     are listed to 4 decimal places in the second parallel table under
C     R (radius), COLAT (co-latitude), and  LONG (longitude).
C
C       X(1)       X(2)     X(3)        R         COLAT       LONG
C       --------------------------      ----------------------------
C       0.0000     0.0000   0.0000      0.0000     0.0000     0.0000
C       1.0000     0.0000   0.0000      1.0000    90.0000     0.0000
C       0.0000     1.0000   0.0000      1.0000    90.0000    90.0000
C       0.0000     0.0000   1.0000      1.0000     0.0000     0.0000
C      -1.0000     0.0000   0.0000      1.0000    90.0000   180.0000
C       0.0000    -1.0000   0.0000      1.0000    90.0000   -90.0000
C       0.0000     0.0000  -1.0000      1.0000   180.0000     0.0000
C       1.0000     1.0000   0.0000      1.4142    90.0000    45.0000
C       1.0000     0.0000   1.0000      1.4142    45.0000     0.0000
C       0.0000     1.0000   1.0000      1.4142    45.0000    90.0000
C       1.0000     1.0000   1.0000      1.7320    54.7356    45.0000
C
C$ Restrictions
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
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.2, 07-JAN-2002 (NJB)
C
C        Fixed description of LONG in Brief_I/O and Detailed_I/O
C        header sections.
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
C     rectangular to spherical coordinates
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
 
         R      = BIG * DSQRT (X*X + Y*Y + Z*Z)
 
         COLAT  = DATAN2 ( DSQRT(X*X + Y*Y), Z )
 
         X      = RECTAN(1)
         Y      = RECTAN(2)
 
         IF (X.EQ.0.0D0 .AND. Y.EQ.0.0D0) THEN
            LONG  = 0.0D0
         ELSE
            LONG  = DATAN2 (Y,X)
         END IF
 
      ELSE
         R     = 0.0D0
         COLAT = 0.0D0
         LONG  = 0.0D0
      END IF
 
      RETURN
      END
