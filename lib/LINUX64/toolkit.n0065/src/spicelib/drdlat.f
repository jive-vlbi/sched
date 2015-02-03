C$Procedure DRDLAT ( Derivative of rectangular w.r.t. latitudinal )
 
      SUBROUTINE DRDLAT ( R, LONG, LAT, JACOBI )
      IMPLICIT NONE
 
C$ Abstract
C
C     Compute the Jacobian of the transformation from latitudinal to
C     rectangular coordinates.
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
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      JACOBI ( 3, 3 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     RADIUS     I   Distance of a point from the origin.
C     LONG       I   Angle of the point from the XZ plane in radians.
C     LAT        I   Angle of the point from the XY plane in radians.
C     JACOBI     O   Matrix of partial derivatives.
C
C$ Detailed_Input
C
C      RADIUS     Distance of a point from the origin.
C
C      LONG       Angle of the point from the XZ plane in radians.
C                 The angle increases in the counterclockwise sense
C                 about the +Z axis.
C
C      LAT        Angle of the point from the XY plane in radians.
C                 The angle increases in the direction of the +Z axis.
C
C$ Detailed_Output
C
C     JACOBI     is the matrix of partial derivatives of the conversion
C                between latitudinal and rectangular coordinates. It has
C                the form
C
C                    .-                                 -.
C                    |  DX/DR     DX/DLONG     DX/DLAT   |
C                    |                                   |
C                    |  DY/DR     DY/DLONG     DY/DLAT   |
C                    |                                   |
C                    |  DZ/DR     DZ/DLONG     DZ/DLAT   |
C                    `-                                 -'
C
C               evaluated at the input values of R, LONG and LAT.
C               Here X, Y, and Z are given by the familiar formulae
C
C                   X = R * COS(LONG) * COS(LAT)
C                   Y = R * SIN(LONG) * COS(LAT)
C                   Z = R *             SIN(LAT)
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
C     It is often convenient to describe the motion of an object
C     in latitudinal coordinates. It is also convenient to manipulate
C     vectors associated with the object in rectangular coordinates.
C
C     The transformation of a latitudinal state into an equivalent
C     rectangular state makes use of the Jacobian of the
C     transformation between the two systems.
C
C     Given a state in latitudinal coordinates,
C
C          ( r, long, lat, dr, dlong, dlat )
C
C     the velocity in rectangular coordinates is given by the matrix
C     equation
C                    t          |                                t
C        (dx, dy, dz)   = JACOBI|             * (dr, dlong, dlat)
C                               |(r,long,lat)
C                                            
C     This routine computes the matrix 
C
C              |
C        JACOBI|
C              |(r,long,lat)
C
C$ Examples
C
C     Suppose you have a model that gives radius, longitude, and
C     latitude as functions of time (r(t), long(t), lat(t)), and
C     that the derivatives (dr/dt, dlong/dt, dlat/dt) are computable.
C     To find the velocity of the object in rectangular coordinates,
C     multiply the Jacobian of the transformation from latitudinal
C     to rectangular (evaluated at r(t), long(t), lat(t)) by the
C     vector of derivatives of the latitudinal coordinates.
C
C     This is illustrated by the following code fragment.
C
C        C
C        C     Load the derivatives of r, long and lat into the
C        C     latitudinal velocity vector LATV.
C        C
C              LATV(1) = DR_DT    ( T )
C              LATV(2) = DLONG_DT ( T )
C              LATV(3) = DLAT_DT  ( T )
C
C        C
C        C     Determine the Jacobian of the transformation from
C        C     latitudinal to rectangular coordinates, using the 
C        C     latitudinal coordinates at time T.
C        C
C              CALL DRDLAT ( R(T), LONG(T), LAT(T), JACOBI )
C
C        C
C        C     Multiply the Jacobian by the latitudinal velocity to
C        C     obtain the rectangular velocity RECV.
C        C
C              CALL MXV ( JACOBI, LATV, RECV )
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
C     Jacobian of rectangular w.r.t. latitudinal coordinates
C
C-&
 
 
C$ Revisions
C
C     None.
C
C-&
 
 
C
C     Local variables
C
      INTEGER               DX
      PARAMETER           ( DX   = 1 )
 
      INTEGER               DY
      PARAMETER           ( DY   = 2 )
 
      INTEGER               DZ
      PARAMETER           ( DZ   = 3 )
 
      INTEGER               DR
      PARAMETER           ( DR   = 1 )
 
      INTEGER               DLON
      PARAMETER           ( DLON = 2 )
 
      INTEGER               DLAT
      PARAMETER           ( DLAT = 3 )
 
 
 
C
C     Construct the matrix directly.
C
      JACOBI (DX,DR)   =      DCOS( LONG ) * DCOS( LAT )
      JACOBI (DY,DR)   =      DSIN( LONG ) * DCOS( LAT )
      JACOBI (DZ,DR)   =                     DSIN( LAT )
 
      JACOBI (DX,DLON) = -R * DSIN( LONG ) * DCOS( LAT )
      JACOBI (DY,DLON) =  R * DCOS( LONG ) * DCOS( LAT )
      JACOBI (DZ,DLON) =  0.0D0
 
      JACOBI (DX,DLAT) = -R * DCOS( LONG ) * DSIN( LAT )
      JACOBI (DY,DLAT) = -R * DSIN( LONG ) * DSIN( LAT )
      JACOBI (DZ,DLAT) =  R *                DCOS( LAT )
 
      RETURN
      END
