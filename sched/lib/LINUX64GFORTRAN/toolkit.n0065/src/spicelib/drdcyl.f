C$Procedure DRDCYL (Derivative of rectangular w.r.t. cylindrical)

      SUBROUTINE DRDCYL ( R, LONG, Z, JACOBI )
      IMPLICIT NONE

C$ Abstract
C
C     This routine computes the Jacobian of the transformation from
C     cylindrical to rectangular coordinates.
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
C     COORDINATES
C     DERIVATIVES
C     MATRIX
C
C$ Declarations

      DOUBLE PRECISION      R
      DOUBLE PRECISION      LONG
      DOUBLE PRECISION      Z
      DOUBLE PRECISION      JACOBI ( 3, 3 )

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     R          I   Distance of a point from the origin.
C     LONG       I   Angle of the point from the XZ plane in radians.
C     Z          I   Height of the point above the XY plane.
C     JACOBI     O   Matrix of partial derivatives.
C
C$ Detailed_Input
C
C     R          Distance of the point of interest from Z axis.
C
C     LONG       Cylindrical angle (in radians) of the point of
C                interest from the XZ plane.  The angle increases in
C                the counterclockwise sense about the +Z axis.
C
C     Z          Height of the point above XY plane.
C
C$ Detailed_Output
C
C     JACOBI     is the matrix of partial derivatives of the conversion
C                between cylindrical and rectangular coordinates.  It
C                has the form
C
C                   .-                                  -.
C                   |  dx/dr     dx/dlong       dx/dz    |
C                   |                                    |
C                   |  dy/dr     dy/dlong       dy/dz    |
C                   |                                    |
C                   |  dz/dr     dz/dlong       dz/dz    |
C                   `-                                  -'
C
C                evaluated at the input values of R, LONG and Z.
C                Here x,y, and z are given by the familiar formulae
C
C                   x = r*cos(long)
C                   y = r*sin(long)
C                   z = z
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
C     It is often convenient to describe the motion of an object in
C     the cylindrical coordinate system.  However, when performing
C     vector computations its hard to beat rectangular coordinates.
C
C     To transform states given with respect to cylindrical coordinates
C     to states with respect to rectangular coordinates, one uses
C     the Jacobian of the transformation between the two systems.
C
C     Given a state in cylindrical coordinates
C
C        ( r, long, z, dr, dlong, dz )
C
C     the velocity in rectangular coordinates is given by the matrix
C     equation:
C                    t          |                           t
C        (dx, dy, dz)   = JACOBI|          * (dr, dlong, dz)
C                               |(r,long,z)
C
C     This routine computes the matrix
C
C              |
C        JACOBI|
C              |(r,long,z)
C
C$ Examples
C
C     Suppose that one has a model that gives radius, longitude and
C     height as a function of time (r(t), long(t), z(t)) for
C     which the derivatives ( dr/dt, dlong/dt, dz/dt ) are computable.
C
C     To find the corresponing velocity in bodyfixed rectangular
C     coordinates, one simply multiplies the Jacobian of the
C     transformation from cylindrical to rectangular coordinates
C     (evaluated at r(t), long(t), z(t) ) by the vector of derivatives
C     of the cylindrical coordinates.
C
C     In code this looks like:
C
C        C
C        C     Load the derivatives of r, long, and z into the
C        C     cylindrical velocity vector SPHV.
C        C
C              CYLV(1) = DR_DT    ( T )
C              CYLV(2) = DLONG_DT ( T )
C              CYLV(3) = DZ_DT    ( T )
C
C        C
C        C     Determine the Jacobian of the transformation from
C        C     cylindrical to rectangular coordinates at the
C        C     given cylindrical coordinates at time T.
C        C
C              CALL DRDCYL ( R(T), LONG(T), Z(T), JACOBI )
C
C        C
C        C     Multiply the Jacobian on the left by the cylindrical
C        C     velocity to obtain the rectangular velocity RECV.
C        C
C              CALL MXV ( JACOBI, CYLV, RECV )
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 12-NOV-2013 (EDW)
C
C        Trivial edit to header, deleted trailing whitespace
C        on lines.
C
C-    SPICELIB Version 1.0.0, 19-JUL-2001 (WLT) (IMU)
C
C-&

C$ Index_Entries
C
C     Jacobian of rectangular w.r.t. cylindrical coordinates
C
C-&

C$ Revisions
C
C     None.
C
C-&

C
C     Local parameters
C
      INTEGER               DX
      PARAMETER           ( DX     = 1 )

      INTEGER               DY
      PARAMETER           ( DY     = 2 )

      INTEGER               DZ
      PARAMETER           ( DZ     = 3 )

      INTEGER               DR
      PARAMETER           ( DR     = 1 )

      INTEGER               DLON
      PARAMETER           ( DLON   = 2 )



      JACOBI (DX,DR)     =  DCOS( LONG )
      JACOBI (DY,DR)     =  DSIN( LONG )
      JACOBI (DZ,DR)     =  0.0D0

      JACOBI (DX,DLON)   = -DSIN( LONG ) * R
      JACOBI (DY,DLON)   =  DCOS( LONG ) * R
      JACOBI (DZ,DLON)   =  0.0D0

      JACOBI (DX,DZ)     =  0.0D0
      JACOBI (DY,DZ)     =  0.0D0
      JACOBI (DZ,DZ)     =  1.0D0

      RETURN
      END
