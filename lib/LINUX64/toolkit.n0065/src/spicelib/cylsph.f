C$Procedure      CYLSPH ( Cylindrical to spherical )
 
      SUBROUTINE CYLSPH ( R, LONGC, Z,  RADIUS, COLAT, LONG )
 
C$ Abstract
C
C      Convert from cylindrical to spherical coordinates.
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
      DOUBLE PRECISION    LONGC
      DOUBLE PRECISION    Z
      DOUBLE PRECISION    RADIUS
      DOUBLE PRECISION    COLAT
      DOUBLE PRECISION    LONG
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  -------------------------------------------------
C      R          I   Distance of point from Z axis.
C      LONGC      I   Angle (radians) of point from XZ plane.
C      Z          I   Height of point above XY plane.
C      RADIUS     O   Distance of point from origin.
C      COLAT      O   Polar angle (co-latitude in radians) of point.
C      LONG       O   Azimuthal angle (longitude) of point (radians).
C
C$ Detailed_Input
C
C      R          Distance of the point of interest from Z axis.
C
C      LONGC      Cylindrical angle (radians) of the point from the
C                 XZ plane.
C
C      Z          Height of the point above XY plane.
C
C$ Detailed_Output
C
C      RADIUS     Distance of the point from origin.
C
C      COLAT      Polar angle (co-latitude in radians) of the point.
C
C      LONG       Azimuthal angle (longitude) of the point (radians).
C
C$ Parameters
C
C      None.
C
C$ Particulars
C
C      This returns the spherical coordinates of a point whose position
C      is input through cylindrical coordinates.
C
C$ Examples
C
C
C      Below are two tables:  The first is a set of input values
C      the second is the result of the following sequence of
C      calls to Spicelib routines.  Note all input and output angular
C      quantities are in degrees.
C
C         CALL CONVRT ( LONGC, 'DEGREES', 'RADIANS', LONGC       )
C
C         CALL CYLSPH ( R,      LONGC, Z,  RADIUS,   COLAT, LONG )
C
C         CALL CONVRT ( LONG,  'RADIANS', 'DEGREES', LONG        )
C         CALL CONVRT ( LAT,   'RADIANS', 'DEGREES', LAT         )
C
C
C
C      Inputs:                         Results:
C
C      R        LONGC    Z             RADIUS   LONG     COLAT
C      ------   ------   ------        ------   ------   ------
C      1.0000     0       0            1.0000     0       90.00
C      1.0000    90.00    0            1.0000    90.00    90.00
C      1.0000   180.00    1.000        1.4142   180.00    45.00
C      1.0000   180.00   -1.000        1.4142   180.00   135.00
C      0.0000   180.00    1.000        1.0000   180.00     0.00
C      0.0000    33.00    0            0.0000    33.00     0.00
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
C     cylindrical to spherical
C
C-&
 
C
C     Local variables
C
      DOUBLE PRECISION BIG
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
 
      DOUBLE PRECISION RH
      DOUBLE PRECISION TH
 
C
C  Convert to spherical, storing in temporary variables
C
      BIG = MAX( DABS(R), DABS(Z) )
 
      IF ( BIG .EQ. 0.0 D0 ) THEN
 
         TH = 0.0 D0
         RH = 0.0 D0
 
      ELSE
 
         X  = R/BIG
         Y  = Z/BIG
 
         RH = BIG * DSQRT( X*X + Y*Y )
         TH = DATAN2 (R,Z)
 
      END IF
 
C
C     Move the results to output variables
C
      LONG   = LONGC
      RADIUS = RH
      COLAT  = TH
 
      RETURN
      END
