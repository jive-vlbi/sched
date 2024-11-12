C$Procedure    DLATDR ( Derivative of latitudinal w.r.t. rectangular )
 
      SUBROUTINE DLATDR ( X, Y, Z, JACOBI )
      IMPLICIT NONE
 
C$ Abstract
C
C     This routine computes the Jacobian of the transformation from
C     rectangular to latitudinal coordinates.
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
C                to latitudinal coordinates is desired.
C
C$ Detailed_Output
C
C     JACOBI     is the matrix of partial derivatives of the conversion
C                between rectangular and latitudinal coordinates.  It
C                has the form
C
C                    .-                              -.
C                    |  dr/dx     dr/dy     dr/dz     |
C                    |  dlong/dx  dlong/dy  dlong/dz  |
C                    |  dlat/dx   dlat/dy   dlat/dz   |
C                    `-                              -'
C
C               evaluated at the input values of X, Y, and Z.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the input point is on the z-axis ( X and Y = 0 ), the
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
C     However, once the vector manipulations have been performed
C     it is often desirable to convert the rectangular representations
C     into latitudinal coordinates to gain insights about phenomena
C     in this coordinate frame.
C
C     To transform rectangular velocities to derivatives of coordinates
C     in a latitudinal system, one uses the Jacobian of the
C     transformation between the two systems.
C
C     Given a state in rectangular coordinates
C
C          ( x, y, z, dx, dy, dz )
C
C     the corresponding latitudinal coordinate derivatives are given by
C     the matrix equation:
C
C                         t          |                     t
C        (dr, dlong, dlat)   = JACOBI|        * (dx, dy, dz)
C                                    |(x,y,z)
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
C     ( x(t), y(t), z(t), dx(t), dy(t), dz(t) ) as a function of time t.
C
C     To find the derivatives of the coordinates of the object in
C     bodyfixed latitudinal coordinates, one simply multiplies the
C     Jacobian of the transformation from rectangular to latitudinal
C     (evaluated at x(t), y(t), z(t) ) by the rectangular velocity
C     vector of the object at time t.
C
C     In code this looks like:
C
C        C
C        C     Load the rectangular velocity vector vector RECV.
C        C
C              RECV(1) = DX_DT ( T )
C              RECV(3) = DY_DT ( T )
C              RECV(2) = DZ_DT ( T )
C
C        C
C        C     Determine the Jacobian of the transformation from
C        C     rectangular to latitudinal at the rectangular 
C        C     coordinates at time T.
C        C
C              CALL DLATDR ( X(T), Y(T), Z(T), JACOBI )
C
C        C
C        C     Multiply the Jacobian on the right by the rectangular
C        C     velocity to obtain the latitudinal coordinate 
C        C     derivatives LATV.
C        C
C              CALL MXV ( JACOBI, RECV, LATV )
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
C-    SPICELIB Version 1.0.0, 16-JUL-2001 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Jacobian of latitudinal w.r.t. rectangular coordinates
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
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      INJACB  ( 3, 3 ) 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DLATDR' )
      END IF
 
C
C     There is a singularity of the Jacobian for points on the z-axis.
C
      IF ( ( X .EQ. 0 ) .AND. ( Y .EQ. 0 ) ) THEN

         CALL SETMSG ( 'The Jacobian of the transformation from '     //
     .                 'rectangular to latitudinal coordinates '      //
     .                 'is not defined for points on the z-axis.'      )
         CALL SIGERR ( 'SPICE(POINTONZAXIS)'                           )
         CALL CHKOUT ( 'DLATDR'                                        )
         RETURN

      END IF
 
C
C     We will get the Jacobian of the transformation from rectangular 
C     to latitudinal coordinates by implicit differentiation.
C
C     First move the X,Y and Z coordinates into a vector.
C
      CALL VPACK  ( X, Y, Z, RECTAN )
 
C
C     Convert from rectangular to latitudinal coordinates.
C
      CALL RECLAT ( RECTAN, R, LONG, LAT  )
 
C
C     Get the Jacobian of the transformation from latitudinal to 
C     rectangular coordinates at R, LONG, LAT.
C
      CALL DRDLAT ( R, LONG, LAT, INJACB )
 
C
C     Now invert INJACB to get the Jacobian of the transformation from 
C     rectangular to latitudinal coordinates.
C
      CALL INVORT ( INJACB, JACOBI )
 
      CALL CHKOUT ( 'DLATDR' )
      RETURN
      END
