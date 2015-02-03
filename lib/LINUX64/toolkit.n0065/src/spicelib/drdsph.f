C$Procedure      DRDSPH ( Derivative of rectangular w.r.t. spherical )
 
      SUBROUTINE DRDSPH ( R, COLAT, LONG, JACOBI )
      IMPLICIT NONE
 
C$ Abstract
C
C     This routine computes the Jacobian of the transformation from
C     spherical to rectangular coordinates.
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
      DOUBLE PRECISION      COLAT
      DOUBLE PRECISION      LONG
      DOUBLE PRECISION      JACOBI ( 3, 3 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     R          I   Distance of a point from the origin.
C     COLAT      I   Angle of the point from the positive Z-axis.
C     LONG       I   Angle of the point from the XY plane.
C     JACOBI     O   Matrix of partial derivatives.
C
C$ Detailed_Input
C
C     R          Distance of a point from the origin.
C
C     COLAT      Angle between the point and the positive z-axis, in
C                radians.
C
C     LONG       Angle of the point from the XZ plane in radians.
C                The angle increases in the counterclockwise sense
C                about the +Z axis.
C
C$ Detailed_Output
C
C     JACOBI     is the matrix of partial derivatives of the conversion
C                between spherical and rectangular coordinates, 
C                evaluated at the input coordinates.  This matrix has 
C                the form
C
C                    .-                                   -.
C                    |  DX/DR     DX/DCOLAT     DX/DLONG   |
C                    |                                     |
C                    |  DY/DR     DY/DCOLAT     DY/DLONG   |
C                    |                                     |
C                    |  DZ/DR     DZ/DCOLAT     DZ/DLONG   |
C                    `-                                   -'
C
C               evaluated at the input values of R, LONG and LAT.
C               Here X, Y, and Z are given by the familiar formulae
C
C                   X = R*COS(LONG)*SIN(COLAT)
C                   Y = R*SIN(LONG)*SIN(COLAT)
C                   Z = R*COS(COLAT)
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
C     the spherical coordinate system.  However, when performing
C     vector computations its hard to beat rectangular coordinates.
C
C     To transform states given with respect to spherical coordinates
C     to states with respect to rectangular coordinates, one makes use
C     of the Jacobian of the transformation between the two systems.
C
C     Given a state in spherical coordinates
C
C          ( r, colat, long, dr, dcolat, dlong )
C
C     the velocity in rectangular coordinates is given by the matrix
C     equation:
C                    t          |                                    t
C        (dx, dy, dz)   = JACOBI|              * (dr, dcolat, dlong )
C                               |(r,colat,long)
C
C     This routine computes the matrix 
C
C              |
C        JACOBI|
C              |(r,colat,long)
C
C$ Examples
C
C     Suppose that one has a model that gives the radius, colatitude 
C     and longitude as a function of time (r(t), colat(t), long(t)), 
C     for which the derivatives ( dr/dt, dcolat/dt, dlong/dt ) are
C     computable.
C
C     To find the velocity of the object in bodyfixed rectangular
C     coordinates, one simply multiplies the Jacobian of the
C     transformation from spherical to rectangular coordinates 
C     (evaluated at r(t), colat(t), long(t) ) by the vector of 
C     derivatives of the spherical coordinates.
C
C     In code this looks like:
C
C        C
C        C     Load the derivatives of r, colat, and long into the
C        C     spherical velocity vector SPHV.
C        C
C              SPHV(1) = DR_DT     ( T )
C              SPHV(2) = DCOLAT_DT ( T )
C              SPHV(3) = DLONG_DT  ( T )
C
C        C
C        C     Determine the Jacobian of the transformation from
C        C     spherical to rectangular coordinates at the given 
C        C     spherical coordinates at time T.
C        C
C              CALL DRDSPH ( R(T), COLAT(T), LONG(T), JACOBI )
C
C        C
C        C     Multiply the Jacobian on the left times the spherical
C        C     velocity to obtain the rectangular velocity RECV.
C        C
C              CALL MXV ( JACOBI, SPHV, RECV )
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
C-    SPICELIB Version 1.0.0, 20-JUL-2001 (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     Jacobian of rectangular w.r.t. spherical coordinates
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
 
      INTEGER               DCOLAT
      PARAMETER           ( DCOLAT = 2 )
 
      INTEGER               DLON
      PARAMETER           ( DLON   = 3 )
 
 
C
C     Local variables
C 
      DOUBLE PRECISION      CCOLAT
      DOUBLE PRECISION      CLONG
      DOUBLE PRECISION      SCOLAT
      DOUBLE PRECISION      SLONG
 


      CCOLAT = DCOS( COLAT )
      SCOLAT = DSIN( COLAT )
 
      CLONG  = DCOS( LONG  )
      SLONG  = DSIN( LONG  )
 
 
      JACOBI (DX,DR)     =      CLONG * SCOLAT
      JACOBI (DY,DR)     =      SLONG * SCOLAT
      JACOBI (DZ,DR)     =              CCOLAT
 
      JACOBI (DX,DCOLAT) =  R * CLONG * CCOLAT
      JACOBI (DY,DCOLAT) =  R * SLONG * CCOLAT
      JACOBI (DZ,DCOLAT) = -R *         SCOLAT
 
      JACOBI (DX,DLON)   = -R * SLONG * SCOLAT
      JACOBI (DY,DLON)   =  R * CLONG * SCOLAT
      JACOBI (DZ,DLON)   =  0.0D0
 
      RETURN
      END
