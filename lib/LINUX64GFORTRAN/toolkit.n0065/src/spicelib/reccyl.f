C$Procedure      RECCYL ( Rectangular to cylindrical coordinates )
 
      SUBROUTINE RECCYL (RECTAN, R, LONG, Z )
 
C$ Abstract
C
C      Convert from rectangular to cylindrical coordinates.
C
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
C      CONVERSION, COORDINATES
C
C$ Declarations
 
      DOUBLE PRECISION RECTAN(3)
      DOUBLE PRECISION R
      DOUBLE PRECISION LONG
      DOUBLE PRECISION Z
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  -------------------------------------------------
C      RECTAN     I   Rectangular coordinates of a point.
C      R          O   Distance of the point from Z axis.
C      LONG       O   Angle (radians) of the point from XZ plane
C      Z          O   Height of the point above XY plane.
C
C$ Detailed_Input
C
C      RECTAN     Rectangular coordinates of the point of interest.
C
C$ Detailed_Output
C
C      R          Distance of the point of interest from Z axis.
C
C      LONG       Cylindrical angle (in radians) of the point of
C                 interest from XZ plane.
C
C      Z          Height of the point above XY plane.
C
C$ Parameters
C
C      None.
C
C$ Particulars
C
C      This routine transforms the coordinates of a point from
C      rectangular to cylindrical coordinates.
C
C$ Examples
C
C     Below are two tables.
C
C     Listed in the first table (under X(1), X(2) and X(3) ) are a
C     number of points whose rectangular coordinates coorindates are
C     taken from the set {-1, 0, 1}.
C
C     The result of the code fragment
C
C          CALL RECCYL ( X, R, LONG, Z )
C
C          Use the SPICELIB routine CONVRT to convert the angular
C          quantities to degrees
C
C          CALL CONVRT ( LONG, 'RADIANS', 'DEGREES', LONG )
C
C     are listed to 4 decimal places in the second parallel table under
C     R (radius), LONG (longitude), and  Z (same as rectangular Z
C     coordinate).
C
C
C       X(1)       X(2)     X(3)        R         LONG     Z
C       --------------------------      -------------------------
C       0.0000     0.0000   0.0000      0.0000    0.0000   0.0000
C       1.0000     0.0000   0.0000      1.0000    0.0000   0.0000
C       0.0000     1.0000   0.0000      1.0000   90.0000   0.0000
C       0.0000     0.0000   1.0000      0.0000    0.0000   1.0000
C      -1.0000     0.0000   0.0000      1.0000  180.0000   0.0000
C       0.0000    -1.0000   0.0000      1.0000  270.0000   0.0000
C       0.0000     0.0000  -1.0000      0.0000    0.0000  -1.0000
C       1.0000     1.0000   0.0000      1.4142   45.0000   0.0000
C       1.0000     0.0000   1.0000      1.0000    0.0000   1.0000
C       0.0000     1.0000   1.0000      1.0000   90.0000   1.0000
C       1.0000     1.0000   1.0000      1.4142   45.0000   1.0000
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
C-    SPICELIB Version 1.0.2, 22-AUG-2001 (EDW)
C
C        Corrected ENDIF to END IF. Obsolete Revisions section
C        deleted.
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
C     rectangular to cylindrical coordinates
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION TWOPI
 
C
C     Local Variables
C
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION BIG
 
C
C     Use temporary variables for computing R.
C
      BIG = MAX( DABS(RECTAN(1)), DABS(RECTAN(2)) )
 
C
C     Convert to cylindrical coordinates
C
      Z = RECTAN(3  )
 
      IF ( BIG .EQ. 0 ) THEN
         R      = 0.D0
         LONG   = 0.D0
      ELSE
         X      = RECTAN(1) / BIG
         Y      = RECTAN(2) / BIG
 
         R      = BIG * DSQRT (X*X + Y*Y)
 
         LONG   = DATAN2 (Y,X)
      END IF
 
      IF ( LONG .LT. 0.D0) THEN
         LONG = LONG + TWOPI()
      END IF
 
      RETURN
      END
