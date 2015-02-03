C$Procedure      DGEODR ( Derivative of geodetic w.r.t. rectangular )
 
 
      SUBROUTINE DGEODR ( X, Y, Z, RE, F, JACOBI )
      IMPLICIT NONE
 
C$ Abstract
C
C     This routine computes the Jacobian of the transformation from
C     rectangular to geodetic coordinates.
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
      DOUBLE PRECISION      RE
      DOUBLE PRECISION      F
      DOUBLE PRECISION      JACOBI ( 3, 3 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     X          I   X-coordinate of point.
C     Y          I   Y-coordinate of point.
C     Z          I   Z-coordinate of point.
C     RE         I   Equatorial radius of the reference spheroid.
C     F          I   Flattening coefficient.
C     JACOBI     O   Matrix of partial derivatives.
C
C$ Detailed_Input
C
C     X,
C     Y,
C     Z          are the rectangular coordinates of the point at
C                which the Jacobian of the map from rectangular
C                to geodetic coordinates is desired.
C
C     RE         Equatorial radius of the reference spheroid.
C
C     F          Flattening coefficient = (RE-RP) / RE,  where RP is
C                the polar radius of the spheroid.  (More importantly
C                RP = RE*(1-F).)
C
C$ Detailed_Output
C
C     JACOBI     is the matrix of partial derivatives of the conversion
C                between rectangular and geodetic coordinates.  It
C                has the form
C
C                    .-                               -.
C                    |  DLONG/DX   DLONG/DY  DLONG/DZ  |
C                    |  DLAT/DX    DLAT/DY   DLAT/DZ   |
C                    |  DALT/DX    DALT/DY   DALT/DZ   |
C                    `-                               -'
C
C               evaluated at the input values of X, Y, and Z.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the input point is on the z-axis (X and Y = 0), the
C        Jacobian is undefined.  The error SPICE(POINTONZAXIS)
C        will be signaled.
C
C     2) If the flattening coefficient is greater than or equal to
C        one, the error SPICE(VALUEOUTOFRANGE) is signaled.
C
C     3) If the equatorial radius is not positive, the error
C        SPICE(BADRADIUS) is signaled.
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
C     into geodetic coordinates to gain insights about phenomena
C     in this coordinate frame.
C
C     To transform rectangular velocities to derivatives of coordinates
C     in a geodetic system, one uses the Jacobian of the transformation
C     between the two systems.
C
C     Given a state in rectangular coordinates
C
C        ( x, y, z, dx, dy, dz )
C
C     the velocity in geodetic coordinates is given by the matrix 
C     equation:
C                          t          |                     t
C        (dlon, dlat, dalt)   = JACOBI|       * (dx, dy, dz)
C                                     |(x,y,z)
C
C     This routine computes the matrix 
C
C              |
C        JACOBI|
C              |(x, y, z)
C
C$ Examples
C
C     Suppose one is given the bodyfixed rectangular state of an object
C     (x(t), y(t), z(t), dx(t), dy(t), dz(t)) as a function of time t.
C
C     To find the derivatives of the coordinates of the object in
C     bodyfixed geodetic coordinates, one simply multiplies the
C     Jacobian of the transformation from rectangular to geodetic
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
C        C     rectangular to geodetic coordinates at the rectangular
C        C     coordinates at time T.
C        C
C              CALL DGEODR ( X(T), Y(T), Z(T), RE, F, JACOBI )
C
C        C
C        C     Multiply the Jacobian on the right by the rectangular
C        C     velocity to obtain the geodetic coordinate derivatives 
C        C     GEOV.
C        C
C              CALL MXV ( JACOBI, RECV, GEOV )
C
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
C-    SPICELIB Version 1.0.0, 20-JUL-2001 (WLT)
C
C-&
 
 
C$ Index_Entries
C
C     Jacobian of geodetic  w.r.t. rectangular coordinates
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
      LOGICAL               RETURN
 
C
C     Local variables
C
      DOUBLE PRECISION      RECTAN  ( 3 )
      DOUBLE PRECISION      LONG
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      INJACB  ( 3, 3 )
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DGEODR' )
      END IF
 
C
C     If the flattening coefficient is greater than one, the polar
C     radius computed below is negative. If it's equal to one, the
C     polar radius is zero. Either case is a problem, so signal an
C     error and check out.
C
      IF ( F .GE. 1.0D0 ) THEN

          CALL SETMSG ( 'Flattening coefficient was *.'  )
          CALL ERRDP  ( '*', F                           )
          CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'         )
          CALL CHKOUT ( 'DGEODR'                         )
          RETURN

      END IF
 
      IF ( RE .LE. 0.0D0 ) THEN

          CALL SETMSG ( 'Equatorial Radius <= 0.0D0. RE = *' )
          CALL ERRDP  ( '*', RE                              )
          CALL SIGERR ( 'SPICE(BADRADIUS)'                   )
          CALL CHKOUT ( 'DGEODR'                             )
          RETURN

      END IF
 
C
C     There is a singularity of the Jacobian for points on the z-axis.
C
      IF ( ( X .EQ. 0 ) .AND. ( Y .EQ. 0 ) ) THEN

         CALL SETMSG ( 'The Jacobian of the transformation from '     //
     .                 'rectangular to geodetic coordinates '         //
     .                 'is not defined for points on the z-axis.'      )
         CALL SIGERR ( 'SPICE(POINTONZAXIS)'                           )
         CALL CHKOUT ( 'DGEODR'                                        )
         RETURN

      END IF
 
C
C     We will get the Jacobian of rectangular to geodetic by
C     implicit differentiation.
C
C     First move the X,Y and Z coordinates into a vector.
C
      CALL VPACK  ( X, Y, Z, RECTAN )
 
C
C     Convert from rectangular to geodetic coordinates.
C
      CALL RECGEO ( RECTAN, RE, F, LONG, LAT, ALT  )
 
C
C     Get the Jacobian of the transformation from geodetic to 
C     rectangular coordinates at LONG, LAT, ALT.
C
      CALL DRDGEO ( LONG,  LAT, ALT, RE, F, INJACB )
 
C
C     Now invert INJACB to get the Jacobian of the transformation
C     from rectangular to geodetic coordinates.
C
      CALL INVORT ( INJACB, JACOBI )
 
      CALL CHKOUT ( 'DGEODR' )
      RETURN
      END
