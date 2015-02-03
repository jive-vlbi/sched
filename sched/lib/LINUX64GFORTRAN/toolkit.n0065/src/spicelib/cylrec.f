C$Procedure      CYLREC ( Cylindrical to rectangular )
 
      SUBROUTINE CYLREC ( R, LONG, Z,  RECTAN )
 
C$ Abstract
C
C      Convert from cylindrical to rectangular coordinates.
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
 
      DOUBLE PRECISION    R
      DOUBLE PRECISION    LONG
      DOUBLE PRECISION    Z
      DOUBLE PRECISION    RECTAN ( 3 )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  -------------------------------------------------
C      R          I   Distance of a point from Z axis.
C      LONG       I   Angle (radians) of a point from XZ plane
C      Z          I   Height of a point above XY plane.
C      RECTAN     O   Rectangular coordinates of the point.
C
C$ Detailed_Input
C
C      R          Distance of the point of interest from Z axis.
C
C      LONG       Cylindrical angle (in radians) of the point of
C                 interest from XZ plane.
C
C      Z          Height of the point above XY plane.
C
C$ Detailed_Output
C
C      RECTAN     Rectangular coordinates of the point of interest.
C
C$ Parameters
C
C      None.
C
C$ Particulars
C
C      This routine transforms the coordinates of a point from
C      cylindrical to rectangular coordinates.
C
C$ Examples
C
C     Below are two tables.
C
C     Listed in the first table (under R, LONG and Z ) are
C     cylindrical coordinate triples that approximately represent
C     points whose rectangular coordinates are taken from the set
C     {-1, 0, 1}.  (Angular quantities are given in degrees.)
C
C     The result of the code fragment
C
C          Use the SPICELIB routine CONVRT to convert the angular
C          quantities to radians
C
C          CALL CONVRT ( LONG, 'DEGREES', 'RADIANS', LONG )
C
C          CALL CYLREC ( R, LONG, Z, X )
C
C
C     are listed in the second parallel table under X(1), X(2) and X(3).
C
C
C       R         LONG     Z            X(1)       X(2)     X(3)
C       -------------------------       --------------------------
C       0.0000    0.0000   0.0000       0.0000     0.0000   0.0000
C       1.0000    0.0000   0.0000       1.0000     0.0000   0.0000
C       1.0000   90.0000   0.0000       0.0000     1.0000   0.0000
C       0.0000    0.0000   1.0000       0.0000     0.0000   1.0000
C       1.0000  180.0000   0.0000      -1.0000     0.0000   0.0000
C       1.0000  270.0000   0.0000       0.0000    -1.0000   0.0000
C       0.0000    0.0000  -1.0000       0.0000     0.0000  -1.0000
C       1.4142   45.0000   0.0000       1.0000     1.0000   0.0000
C       1.0000    0.0000   1.0000       1.0000     0.0000   1.0000
C       1.0000   90.0000   1.0000       0.0000     1.0000   1.0000
C       1.4142   45.0000   1.0000       1.0000     1.0000   1.0000
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
C     cylindrical to rectangular
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
C     Local variables
C
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
 
C
C     Convert to rectangular coordinates, storing the results in
C     temporary variables.
C
      X         = R * DCOS(LONG)
      Y         = R * DSIN(LONG)
 
C
C     Move the results to the output variables.
C
      RECTAN(1) = X
      RECTAN(2) = Y
      RECTAN(3) = Z
 
      RETURN
      END
