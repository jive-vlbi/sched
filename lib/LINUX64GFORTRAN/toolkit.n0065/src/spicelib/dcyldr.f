C$Procedure DCYLDR (Derivative of cylindrical w.r.t. rectangular )
 
      SUBROUTINE DCYLDR ( X, Y, Z, JACOBI )
      IMPLICIT NONE
 
C$ Abstract
C
C     This routine computes the Jacobian of the transformation from
C     rectangular to cylindrical coordinates.
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
 
      DOUBLE PRECISION      X
      DOUBLE PRECISION      Y
      DOUBLE PRECISION      Z
      DOUBLE PRECISION      JACOBI ( 3, 3 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     X          I   X-coordinate of point.
C     Y          I   Y-coordinate of point.
C     Z          I   Z-coordinate of point.
C     JACOBI     O   Matrix of partial derivatives.
C
C$ Detailed_Input
C
C     X,
C     Y,
C     Z          are the rectangular coordinates of the point at
C                which the Jacobian of the map from rectangular
C                to cylindrical coordinates is desired.
C
C$ Detailed_Output
C
C     JACOBI     is the matrix of partial derivatives of the conversion
C                between rectangular and cylindrical coordinates.  It
C                has the form
C
C                   .-                               -.
C                   |  dr   /dx   dr   /dy  dr   /dz  |
C                   |  dlong/dx   dlong/dy  dlong/dz  |
C                   |  dz   /dx   dz   /dy  dz   /dz  |
C                   `-                               -'
C
C                evaluated at the input values of X, Y, and Z.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the input point is on the Z-axis (X and Y = 0), the
C        Jacobian is undefined.  The error SPICE(POINTONZAXIS)
C        will be signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     When performing vector calculations with velocities it is
C     usually most convenient to work in rectangular coordinates.
C     However, once the vector manipulations have been performed,
C     it is often desirable to convert the rectangular representations
C     into cylindrical coordinates to gain insights about phenomena
C     in this coordinate frame.
C
C     To transform rectangular velocities to derivatives of coordinates
C     in a cylindrical system, one uses the Jacobian of the 
C     transformation between the two systems.
C
C     Given a state in rectangular coordinates
C
C        ( x, y, z, dx, dy, dz )
C
C     the velocity in cylindrical coordinates is given by the matrix 
C     equation:
C
C                       t          |                     t
C        (dr, dlong, dz)   = JACOBI|       * (dx, dy, dz)
C                                  |(x,y,z)
C
C     This routine computes the matrix 
C
C              |
C        JACOBI|
C              |(x,y,z)
C
C$ Examples
C
C     Suppose one is given the bodyfixed rectangular state of an object
C     (x(t), y(t), z(t), dx(t), dy(t), dz(t)) as a function of time t.
C
C     To find the derivatives of the coordinates of the object in
C     bodyfixed cylindrical coordinates, one simply multiplies the
C     Jacobian of the transformation from rectangular to cylindrical
C     coordinates (evaluated at x(t), y(t), z(t)) by the rectangular 
C     velocity vector of the object at time t.
C
C     In code this looks like:
C
C        C
C        C     Load the rectangular velocity vector vector RECV.
C        C
C              RECV(1) = DX_DT ( T )
C              RECV(2) = DY_DT ( T )
C              RECV(3) = DZ_DT ( T )
C
C        C
C        C     Determine the Jacobian of the transformation from
C        C     rectangular to cylindrical coordinates at the 
C        C     given rectangular coordinates at time T.
C        C
C              CALL DCYLDR ( X(T), Y(T), Z(T), JACOBI )
C
C        C
C        C     Multiply the Jacobian on the right by the rectangular
C        C     velocity to obtain the cylindrical coordinate derivatives
C        C     CYLV.
C        C
C              CALL MXV ( JACOBI, RECV, CYLV )
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
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 19-JUL-2001 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Jacobian of cylindrical w.r.t. rectangular coordinates
C
C-&
 
C$ Revisions
C
C     None.
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL          RETURN
 
C
C     Local variables
C
      DOUBLE PRECISION      RECTAN  ( 3 )
      DOUBLE PRECISION      R
      DOUBLE PRECISION      LONG
      DOUBLE PRECISION      ZZ
      DOUBLE PRECISION      INJACB  ( 3, 3 ) 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DCYLDR' )
      END IF
 
C
C     There is a singularity of the Jacobian for points on the z-axis.
C
      IF ( ( X .EQ. 0.D0 ) .AND. ( Y .EQ. 0.D0 ) ) THEN

         CALL SETMSG ( 'The Jacobian of the transformation from '     //
     .                 'rectangular to cylindrical coordinates '      //
     .                 'is not defined for points on the z-axis.'      )
         CALL SIGERR ( 'SPICE(POINTONZAXIS)'                           )
         CALL CHKOUT ( 'DCYLDR'                                        )
         RETURN

      END IF
 
C
C     We will get the Jacobian of rectangular to cylindrical by
C     implicit differentiation.
C
C     First move the X,Y and Z coordinates into a vector.
C
      CALL VPACK  ( X, Y, Z, RECTAN )
 
C
C     Convert from rectangular to cylindrical coordinates.
C
      CALL RECCYL ( RECTAN, R, LONG, ZZ )
 
C
C     Get the Jacobian from cylindrical to rectangular coordinates at
C     R, LONG, Z.
C
      CALL DRDCYL ( R,  LONG, ZZ, INJACB )
 
C
C     Now invert INJACB to get the Jacobian from rectangular to
C     cylindrical coordinates.
C
      CALL INVORT ( INJACB, JACOBI )
 
      CALL CHKOUT ( 'DCYLDR' ) 
      RETURN
      END
