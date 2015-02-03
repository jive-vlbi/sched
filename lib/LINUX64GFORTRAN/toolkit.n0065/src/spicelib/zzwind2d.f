C$Procedure  ZZWIND2D ( Find winding number of polygon about point )

      INTEGER FUNCTION ZZWIND2D ( N, VERTCS, POINT )
      IMPLICIT NONE
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Find the winding number of a planar polygon about a specified
C     point in 2-dimensional space.
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
C     PLANES
C
C$ Keywords
C
C     GEOMETRY
C     MATH
C     PLANE
C
C$ Declarations

      INTEGER               N
      DOUBLE PRECISION      VERTCS ( 2, N )
      DOUBLE PRECISION      POINT  ( 2 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     N          I   Number of vertices of polygon.
C     VERTCS     I   Vertices of polygon.
C     POINT      I   Point in PLANE.
C
C     The function returns the winding number of the input polygon
C     about the input point.
C
C$ Detailed_Input
C
C     N,
C     VERTCS         are, respectively, the number vertices defining
C                    the polygon and the vertices themselves.  Each
C                    pair of consecutive vectors in the array VERTCS
C                    defines an edge of the polygon.
C
C$ Detailed_Output
C
C     The function returns the winding number of the input polygon
C     about the input point. The winding number measures the "net"
C     number of times the polygon wraps around POINT:  this is 
C     the number of times the polygon wraps around POINT in the
C     counterclockwise sense minus the number of times the polygon
C     wraps around POINT in the clockwise sense.
C
C     The possible values and meanings of the winding number are:
C
C        ZZWIND2D > 0:  The polygon winds about POINT a total
C                       of ZZWIND2D times in the counterclockwise
C                       direction.
C
C                       POINT is inside the polygon.
C
C
C        ZZWIND2D < 0:  The polygon winds about POINT a total
C                       of ZZWIND2D times in the clockwise
C                       direction.
C
C                       POINT is inside the polygon.
C
C
C        ZZWIND2D = 0:  The number of times the polygon wraps around
C                       POINT in the counterclockwise sense is equal
C                       to the number of times the polygon wraps around
C                       POINT in the clockwise sense.  
C       
C                       POINT is outside the polygon.    
C
C$ Parameters
C 
C     None.
C
C$ Exceptions
C
C     1)  If the number of boundary vectors N is not at least 3,
C         or if the number exceeds MAXFOV, the error
C         SPICE(INVALIDCOUNT) will be signaled.
C
C     2)  The input point and vertices are expected to lie in
C         the input plane.  To avoid problems introduced by
C         round-off errors, all of these vectors are projected
C         orthogonally onto the plane before the winding number
C         is computed.  If the input point or vertices are "far"
C         from the input plane, no error will be signaled.
C
C     3)  If the input plane as a zero normal vector, the error
C         SPICE(ZEROVECTOR) will be signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Find the winding number of a 2-D polygon about a specified
C     point.
C
C     This routine supports determination of whether an ellipsoidal
C     body is in the field of view of a remote-sensing instrument
C     with a field of view having polygonal cross section.
C
C     The winding number is actually defined for closed, piecewise
C     differentiable curves in the complex plane. If z(t), t in 
C     [0, 2*Pi], is a parameterization of such a curve, then if the
C     symbol I is used to represent the integration operator, z0 is the
C     complex point of interest, and w is the winding number, we have
C
C                1     
C        w =  -------  *  I  ( d ( log(z-z0) ) )
C             2*Pi*i     z(t)
C
C
C                1     
C          =  -------  *  I  ( ( 1 / (z-z0) ) dz )
C             2*Pi*i     z(t)
C
C
C     Because of Cauchy's theorem, we can transform the problem,
C     without loss of generality (leaving out *many* steps here), to
C     one for which the curve has the simple form
C
C                        i n*(t-t0)
C        z(t) =  z0 + r e
C
C     for some real values r, n, and t0.  So 
C
C     
C             1     
C      w = -------  *  I  ( 1 / (z-z0) )
C          2*Pi*i     z(t)
C
C  
C             1      t=2*pi        i n*(t-t0)           i n*(t-t0)
C        = ------- *   I   ( (1/r e         ) * ( r i n e          )dt )
C          2*Pi*i     t=0
C
C  
C             1     t=2*pi     
C        = ------- *  I (   i n dt )
C          2*Pi*i    t=0
C
C             1
C        = ------  *  ( 2 * Pi * i * n )
C          2*Pi*i
C
C     
C        =    n
C
C
C     Given the simplified form of z(t) we've chosen, it's now clear
C     that n is the winding number.
C
C     In the simple case of a polygonal curve, the integral can be
C     computed for a corresponding polygon whose vertices have been
C     scaled to have equal magnitude; the integral can be expressed as
C     the telescoping sum
C
C         N 
C        ___
C        \
C        /    ( argument of vertex(i+1) - argument of vertex(i) )
C        ---
C        i=1
C
C     where vertex N+1 is considered have length identical to that of
C     vertex 1 and argument differing from that of vertex 1 by w*2*pi.
C
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     [1] `Calculus and Analytic Geometry', Thomas and Finney.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 08-JUL-2008 (NJB)
C
C-&
 
C$ Index_Entries
C
C     find winding number of polygon about point
C   
C-&

C
C     SPICELIB functions
C
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      VDOTG
      DOUBLE PRECISION      VSEPG

      LOGICAL               RETURN

C
C     Local variables
C
      DOUBLE PRECISION      ATOTAL
      DOUBLE PRECISION      RNEXT  ( 2 )
      DOUBLE PRECISION      RPERP  ( 2 )
      DOUBLE PRECISION      RVEC   ( 2 )
      DOUBLE PRECISION      SEP

      INTEGER               I
      INTEGER               J

C
C     Initialize the function return value.
C
      ZZWIND2D = 0

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZWIND2D' )

C
C     Check the number of sides of the polygon.
C      
      IF ( N .LT. 3 ) THEN

         CALL SETMSG ( 'Polygon must have at least 3 sides; N = #.' )
         CALL ERRINT ( '#',  N                                      )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'                      )
         CALL CHKOUT ( 'ZZWIND2D'                                     )
         RETURN

      END IF

C
C     The total "wrap angle" starts at zero.
C
      ATOTAL  = 0.D0

      CALL VSUBG ( VERTCS(1,1), POINT, 2, RVEC )

      DO I = 2, N+1

         IF ( I .LE. N ) THEN
            J = I
         ELSE
            J = 1
         END IF

C
C        Find the angular separation of RVEC and the next vector
C        RNEXT.
C
         CALL VSUBG ( VERTCS(1,J), POINT, 2, RNEXT )

         SEP  =  VSEPG ( RNEXT, RVEC, 2 )
         
C
C        Create a normal vector to RVEC by rotating RVEC pi/2 radians
C        counterclockwise.  We'll use this vector RPERP to determine
C        whether the next point is reached by clockwise or
C        counterclockwise rotation from RVEC.  
C        
         RPERP(1) = -RVEC(2)
         RPERP(2) =  RVEC(1)


         IF (  VDOTG( RNEXT, RPERP, 2 )  .GE.  0.D0  ) THEN
C
C           RNEXT is reached by counterclockwise rotation from
C           RVEC.  Note that in the case of zero rotation, the
C           sign doesn't matter because the contribution is zero.
C
            ATOTAL = ATOTAL + SEP
         ELSE
            ATOTAL = ATOTAL - SEP
         END IF
    
C
C        Update RVEC.
C
         CALL MOVED ( RNEXT, 2, RVEC )

      END DO

C
C     The above sum is 2 * pi * <the number of times the polygon
C     wraps around P>.  Let ZZWIND2D be the wrap count.
C
      ZZWIND2D = NINT ( ATOTAL / TWOPI() )

      CALL CHKOUT ( 'ZZWIND2D' )
      RETURN
      END 
