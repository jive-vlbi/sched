C$Procedure      LATCYL ( Latitudinal to cylindrical coordinates )
 
      SUBROUTINE LATCYL ( RADIUS, LONG, LAT, R, LONGC, Z )
 
C$ Abstract
C
C     Convert from latitudinal coordinates to cylindrical coordinates.
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
 
      DOUBLE PRECISION   RADIUS
      DOUBLE PRECISION   LONG
      DOUBLE PRECISION   LAT
      DOUBLE PRECISION   R
      DOUBLE PRECISION   LONGC
      DOUBLE PRECISION   Z
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      RADIUS     I   Distance of a point from the origin.
C      LONG       I   Angle of the point from the XZ plane in radians.
C      LAT        I   Angle of the point from the XY plane in radians.
C      R          O   Distance of the point from the Z axis.
C      LONGC      O   Angle of the point from the XZ plane in radians.
C      Z          O   Height of the point above the XY plane.
C
C$ Detailed_Input
C
C      RADIUS     Distance of a point from the origin.
C
C      LONG       Angle of the point from the XZ plane in radians.
C
C      LAT        Angle of the point from the XY plane in radians.
C
C$ Detailed_Output
C
C      R          Distance of the point from the Z axis.
C
C      LONGC      Angle of the point from the XZ plane in radians.
C
C      Z          Height of the point above the XY plane.
C
C$ Parameters
C
C      None.
C
C$ Particulars
C
C      This routine returns the cylindrical coordinates of a point
C      whose position is input in latitudinal coordinates.
C
C      Latitudinal coordinates are defined by a distance from a central
C      reference point, an angle from a reference meridian, and an angle
C      above the equator of a sphere centered at the central reference
C      point.
C
C$ Examples
C
C     Other than the obvious conversion between coordinate systems
C     this routine could be used to obtain the axial projection
C     from a sphere to a cylinder about the z-axis that contains
C     the equator of the sphere.  The following code fragment
C     illustrates this idea.
C
C           CALL LATCYL ( RADIUS, LONG, LAT, R, LONG, Z )
C           R = RADIUS
C
C     R, LONG, and Z now contain the coordinates of the projected
C     point. Such a projection is valuable because it preserves the
C     areas between regions on the sphere and their projections to the
C     cylinder.
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
C     latitudinal to cylindrical coordinates
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
      DOUBLE PRECISION RH
      DOUBLE PRECISION ZZ
 
C
C     Convert to cylindrical, storing in temporary variables
C
      RH = RADIUS * DCOS(LAT)
      ZZ = RADIUS * DSIN(LAT)
 
C
C     Move the results to output variables.
C
      LONGC = LONG
      R      = RH
      Z      = ZZ
C
      RETURN
      END
