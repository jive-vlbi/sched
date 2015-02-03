C$Procedure      SPHLAT ( Spherical to latitudinal coordinates )
 
      SUBROUTINE SPHLAT ( R, COLAT, LONGS, RADIUS, LONG, LAT )
 
C$ Abstract
C
C     Convert from spherical coordinates to latitudinal coordinates.
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
 
      DOUBLE PRECISION R
      DOUBLE PRECISION COLAT
      DOUBLE PRECISION LONGS
      DOUBLE PRECISION RADIUS
      DOUBLE PRECISION LONG
      DOUBLE PRECISION LAT
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      R          I   Distance of the point from the origin.
C      COLAT      I   Angle of the point from positive Z axis (radians).
C      LONGS      I   Angle of the point from the XZ plane (radians).
C      RADIUS     O   Distance of a point from the origin
C      LONG       O   Angle of the point from the XZ plane in radians
C      LAT        O   Angle of the point from the XY plane in radians
C
C$ Detailed_Input
C
C      R          Distance of the point from the origin.
C
C      COLAT      Angle between the vector from the origin to the point
C                 and the positive Z axis in radians.
C
C      LONGS      Angle of the point from the XZ plane (radians).
C
C$ Detailed_Output
C
C      RADIUS     Distance of a point from the origin
C
C      LONG       Angle of the point from the XZ plane in radians
C
C      LAT        Angle of the point from the XY plane in radians
C
C$ Parameters
C
C      None.
C
C$ Particulars
C
C      This routine returns the latitudinal coordinates of a point
C      whose position is input in spherical coordinates.
C
C      Latitudinal coordinates are defined by a distance from a central
C      reference point, an angle from a reference meridian, and an angle
C      above the equator of a sphere centered at the central reference
C      point.
C
C      Spherical coordinates are defined by a distance from a central
C      reference point, an angle from a reference meridian, and an angle
C      from the z-axis.
C
C$ Examples
C
C
C      Latitude is obtained by subtracting co-latitude from HALFPI()
C      Radius and longitude mean the same thing in both latitudinal
C      and spherical coordinates.  The table below lists LAT and
C      corresponding COLAT in terms of degrees.
C
C             LAT            COLAT
C            ------         ------
C              0             90
C             20             70
C             45             45
C            -30            120
C             90              0
C            -45            135
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
C     spherical to latitudinal coordinates
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
C     SPICELIB functions
C
      DOUBLE PRECISION HALFPI
 
C
C     Local Variables
C
      DOUBLE PRECISION RR
      DOUBLE PRECISION LATTUD
 
C
C     Convert to latitudinal coordinates, storing the results in
C     temporary variables
C
      RR     = R
      LATTUD = HALFPI() - COLAT
 
C
C     Move the results to the output variables.
C
      LONG   = LONGS
      RADIUS = RR
      LAT    = LATTUD
 
      RETURN
      END
