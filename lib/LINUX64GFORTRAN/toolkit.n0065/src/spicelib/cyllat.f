C$Procedure      CYLLAT ( Cylindrical to latitudinal )
 
      SUBROUTINE CYLLAT ( R, LONGC, Z,  RADIUS, LONG, LAT )
 
C$ Abstract
C
C  Convert from cylindrical to latitudinal coordinates.
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
      DOUBLE PRECISION    LONG
      DOUBLE PRECISION    LAT
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      R          I   Distance of point from Z axis.
C      LONGC      I   Cylindrical angle of point from XZ plane(radians).
C      Z          I   Height of point above XY plane.
C      RADIUS     O   Distance of point from origin.
C      LONG       O   Longitude of point (radians).
C      LAT        O   Latitude of point (radians).
C
C$ Detailed_Input
C
C      R          Distance of the input point from Z axis.
C
C      LONGC      Cylindrical angle of the point from XZ plane(radians).
C
C      Z          Height of the point above XY plane.
C
C$ Detailed_Output
C
C      RADIUS     Distance of the input point from origin.
C
C      LONG       Longitude (i.e. angle from the XZ plane) of the input
C                 point.
C
C      LAT        Latitude (i.e. angle above the XY plane) of the input
C                 point (radians).
C
C$ Parameters
C
C      None.
C
C$ Particulars
C
C      This routine converts coordinates given in cylindrical
C      coordinates to coordinates in latitudinal coordinates.
C
C      Latitudinal coordinates are the same coordinates as use for
C      the earth.  Latitude refers to angle above the equator, longitude
C      to angle east from a meridian, and radius to the distance from
C      an origin.
C
C$ Examples
C
C      Below are two tables:  The first is a set of input values
C      the second is the result of the following sequence of
C      calls to Spicelib routines.  Note all input and output angular
C      quantities are in degrees.
C
C         CALL CONVRT ( LONGC, 'DEGREES', 'RADIANS', LONGC     )
C
C         CALL CYLLAT ( R,      LONGC, Z,  RADIUS,   LONG, LAT )
C
C         CALL CONVRT ( LONG,  'RADIANS', 'DEGREES', LONG      )
C         CALL CONVRT ( LAT,   'RADIANS', 'DEGREES', LAT       )
C
C
C
C      Inputs:                         Results:
C
C      R        LONGC    Z             RADIUS   LONG     LAT
C      ------   ------   ------        ------   ------   ------
C      1.0000     0       0            1.0000     0        0
C      1.0000    90.00    0            1.0000    90.00     0
C      1.0000   180.00    1.000        1.4142   180.00    45.00
C      1.0000   180.00   -1.000        1.4142   180.00   -45.00
C      0.0000   180.00    1.000        1.0000   180.00    90.00
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
C        Corrected ENDIF to END IF.
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
C     cylindrical to latitudinal
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
      DOUBLE PRECISION BIG
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
 
      DOUBLE PRECISION RHO
      DOUBLE PRECISION LATTUD
 
C
C     Convert the input cylindrical coordinates to latitudinal
C     coordinates, storing in temporary variables.
C
      BIG = MAX( DABS(R), DABS(Z) )
 
      IF ( BIG .GT. 0 ) THEN
         X   = R/BIG
         Y   = Z/BIG
         RHO = BIG * DSQRT( X*X + Y*Y )
      ELSE
         RHO = 0.0D0
      END IF
 
 
      IF (RHO.EQ.0.D0) THEN
         LATTUD = 0.D0
      ELSE
         LATTUD = DATAN2 (Z,R)
      END IF
C
C  Move results to output variables
C
      LONG   = LONGC
      RADIUS = RHO
      LAT    = LATTUD
C
      RETURN
      END
