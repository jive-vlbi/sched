C$Procedure      ZZASRYEL ( Angular separation of ray and ellipse )
 
      SUBROUTINE ZZASRYEL ( EXTREM, ELLIPS, VERTEX, DIR, ANGLE, EXTPT )
      IMPLICIT NONE
  
C$ Abstract
C
C     Find the minimum or maximum angular separation between a
C     specified ray and ellipse.
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
C     ELLIPSES
C
C$ Keywords
C
C     ELLIPSE
C     ELLIPSOID
C     GEOMETRY
C     MATH
C
C$ Declarations
 
      INTEGER               UBEL
      PARAMETER           ( UBEL   = 9 )
      
      INTEGER               UBPL
      PARAMETER           ( UBPL   = 4 )

      CHARACTER*(*)         EXTREM
      DOUBLE PRECISION      ELLIPS ( UBEL )
      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      DIR    ( 3 )
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      EXTPT  ( 3 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     UBEL       P   Upper bound of SPICELIB ellipse.
C     UBPL       P   Upper bound of SPICELIB plane.
C     EXTREM     I   Type of extremum to find.
C     ELLIPS     I   SPICE ellipse.
C     VERTEX,
C     DIR        I   Vertex and direction vector of ray.
C     ANGLE      O   Angular separation of ray and ellipse (radians).
C     EXTPT      O   Point on ellipse where extremum is achieved.
C
C$ Detailed_Input
C
C     EXTREM         is a string indicating the type of extremum to
C                    find.  Values are 'MIN' and 'MAX'.  Blanks and
C                    case are not significant.  Only the first three
C                    non-blank characters of EXTREM are significant.
C
C
C     ELLIPS         is a SPICELIB ellipse data structure. ELLIPS must
C                    have non-zero semi-axis lengths.
C
C
C     VERTEX,
C     DIR            are the vertex and direction vector of a ray in
C                    three-dimensional space.
C
C$ Detailed_Output
C
C     ANGLE          is the specified extremum of angular separation of
C                    the input ray and the ellipse.  This is the
C                    minimum or maximum angular separation of the ray
C                    and any line segment extending from the ray's
C                    vertex to a point on the surface of the ellipse.
C                    Units are radians.
C
C                    If the input ray actually intersects the plane
C                    region bounded by the ellipse, ANGLE is set to a
C                    non-positive value whose magnitude is the minimum
C                    or maximum angular separation of the input ray and
C                    the ellipse.
C
C
C     EXTPT          is the point on the ellipse where the specified
C                    extreme value of the angular separation is
C                    achieved.  If there are multiple points where the
C                    extremum is achieved, any such point may be
C                    selected.
C
C$ Parameters
C
C     UBEL           is the upper bound of a SPICELIB ellipse data
C                    structure.
C
C     UBPL           is the upper bound of a SPICELIB plane data
C                    structure.
C
C$ Exceptions
C
C     1)  If the length of any semi-axis of the ellipse is
C         non-positive, the error SPICE(INVALIDAXISLENGTH) is
C         signaled.  ANGLE and EXTPT are not modified.
C
C     2)  If VERTEX lies in the plane of the ellipse, the error
C         SPICE(DEGENERATECASE) is signaled.  ANGLE and EXTPT are not
C         modified.
C
C     3)  If DIR is the zero vector, the error SPICE(ZEROVECTOR) is
C         signaled.  ANGLE and EXTPT are not modified.
C
C     4)  If EXTREM contains an unrecognized value, the error 
C         SPICE(NOTSUPPORTED) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Definition
C     ==========
C
C     The minimum or maximum angular separation of a ray and ellipse is
C     the minimum or maximum, taken over all points on the ellipse, of
C     the angular separation of the ray and the vector from the ray's
C     vertex to a point on the ellipse.  
C
C     Uniqueness or multiplicity of minima
C     ====================================
C  
C     Let's presume that the ray does not intersect the plane region
C     bounded by the ellipse.  If the ray's vertex does not lie in the
C     plane of the ellipse, the uniqueness of the minimizing point can
C     be verified by observing that the right circular cone of minimum
C     angular extent whose axis is the ray, and that is tangent to the
C     ellipse, will be tangent at the minimizing point and no other.
C     If the ray's vertex does lie in the plane of the ellipse, there
C     can be multiple tangency points.
C
C     If the ray intersects the plane region bounded by the ellipse,
C     there may be multiple absolute minima of the angular separation.
C     Consider the case where the ellipse is a circular cross section
C     of a right circular cone, and the ray is the cone's axis:  there
C     is an infinite set of solutions, since the minimum angular
C     separation is achieved at every point on the circle.
C
C
C     Uniqueness or multiplicity of maxima
C     ====================================
C  
C     Let's presume that the ray does not intersect the plane region
C     bounded by the ellipse.  If the ray's vertex does not lie in the
C     plane of the ellipse, one observes that the right circular cone
C     of maximum angular extent whose axis is the ray, and that is
C     tangent to the ellipse, can still easily be tangent to the
C     ellipse at multiple points (consider an ellipse whose shape is
C     "almost" a line segment). The ray's vertex need not lie in the
C     plane of the ellipse for multiple tangency points to exist.
C
C     If the ray intersects the plane region bounded by the ellipse,
C     there may be multiple absolute maxima of the angular separation.
C
C
C     Extremum of angular separation versus distance
C     ==============================================
C
C     Note the point on the ellipse having minimum angular separation
C     from a ray is NOT necessarily the closest point on the ellipse to
C     the ray. You can verify this by considering the case of an
C     extremely eccentric ellipse and a ray that passes above it.   The
C     diagram below illustrates this situation.  The series of three
C     asterisks rising from left to right represents the ray; the other
C     asterisks represent the ellipse. The point `c' is the closest
C     point on the ellipse to the ray; the point `m' has the minimum
C     angular separation from the ray.
C
C     The analoguous distinction applies to maximum angular separation
C     and maximum distance:  compare the points labeled 'M' and 'F'
C     in the diagram below.
C
C
C
C                                                            *
C                                                          (ray)
C                                  *                        
C    ray's vertex                (ray)
C        *
C
C
C  closest ellipse  ---->   c * * * * * * * * m   <-- point of minimum
C  point to the ray       *                     *        angular
C                           M * * * * * * * * F          separation
C
C                           ^                 ^
C     point of maximum angular                farthest ellipse
C     separation                              point from ray
C
C
C
C
C     Applications
C     ============
C
C     This subroutine can be used to:
C
C        -  measure the angular separation of
C           an instrument boresight from a body's limb
C
C        -  test for visibility of an ellipsoidal body within an
C           circular field of view (or, with more work, an elliptical 
C           field of view)
C
C        -  test for occultation of one ellipsoidal body by another
C
C        -  support tests for intersection of an ellipsoidal body with
C           an umbral or penumbral shadow cast by another ellipsoidal
C           body blocking an ellipsoidal light source.
C
C$ Examples
C
C     1)  An example that can be readily checked by hand computation.
C
C            Let
C
C               A = 1
C               B = 1
C               C = 1
C
C               V = ( 2,   0,  0       )
C               D = ( -1,  0,  SQRT(3) )
C
C            The limb of the sphere as seen from the ray's vertex will
C            be the circle centered at ( .5, 0, 0 ), parallel to the
C            y-z plane,  with radius SQRT(3)/2.  The ray lies in the
C            x-z plane and passes over the ellipse, so the limb point
C            of minimum angular separation should be the highest point
C            on the limb.  This would be the point
C
C               ( .5, 0, SQRT(3)/2 ).
C
C            The tangent segment extending from the ray's vertex to the
C            point of mimimum angular separation makes an angle of
C            30 degrees with the x-axis, and the ray makes angle of 60
C            degrees with the x-axis, so the angular separation of the
C            ray and the limb should be 30 degrees.
C
C            For a ray have the same vertex but pointing in the -x
C            direction, the minimum point can be anywhere on the limb,
C            but the angle should be -30 degrees.
C
C            If the vertex is raised slightly (that is, the z-component
C            is increased slightly) and the ray points in the -x
C            direction, the mimimum point should be at the top of the
C            limb, and the angle should be a negative value with
C            magnitude slightly less than 30 degrees.
C
C            The program below should verify these results.
C
C
C               PROGRAM MINANG
C               IMPLICIT NONE
C
C               INTEGER               UBEL
C               PARAMETER           ( UBEL   = 9 )
C
C               DOUBLE PRECISION      DPR
C
C               DOUBLE PRECISION      V(3)
C               DOUBLE PRECISION      D(3)
C               DOUBLE PRECISION      A
C               DOUBLE PRECISION      B
C               DOUBLE PRECISION      C
C               DOUBLE PRECISION      ANGLE
C               DOUBLE PRECISION      LIMB  ( UBEL )
C               DOUBLE PRECISION      EXTPT ( 3 )
C
C               V(1) =  2.D0
C               V(2) =  0.D0
C               V(3) =  0.D0
C
C               D(1) = -1.D0
C               D(2) =  0.D0
C               D(3) =  SQRT( 3.D0 )
C
C               A    =  1.D0
C               B    =  1.D0
C               C    =  1.D0
C
C               CALL EDLIMB   ( A, B, C, V, LIMB )
C
C               CALL ZZASRYEL ( 'MIN', LIMB, V, D, ANGLE, EXTPT )
C
C               PRINT *, ' '
C               PRINT *, 'Angle is'
C               PRINT *, DPR() * ANGLE
C               PRINT *, 'Point of mimimum separation is'
C               PRINT *, EXTPT
C
C         C
C         C     Now take the ray along the x-axis,
C         C     pointing in the -x direction.
C         C
C               D(1) = -1.D0
C               D(2) =  0.D0
C               D(3) =  0.D0
C
C               CALL ZZASRYEL ( 'MIN', LIMB, V, D, ANGLE, EXTPT )
C
C               PRINT *, ' '
C               PRINT *, 'Angle is'
C               PRINT *, DPR() * ANGLE
C               PRINT *, 'Point of mimimum separation is'
C               PRINT *, EXTPT
C
C         C
C         C     Raise the vertex a bit and repeat.
C         C
C               V(1) =  2.D0
C               V(2) =  0.D0
C               V(3) =  1.D-6
C
C               CALL ZZASRYEL ( 'MIN', LIMB, V, D, ANGLE, EXTPT )
C
C               PRINT *, ' '
C               PRINT *, 'Angle is'
C               PRINT *, DPR() * ANGLE
C               PRINT *, 'Point of mimimum separation is'
C               PRINT *, EXTPT
C
C               END
C
C
C$ Restrictions
C
C     1) Under some unusual geometric conditions, the search used
C        in this algorithm may find a relative extremum which is not
C        an absolute extremum.  This can occur if there are two local
C        extrema of separation (both minima or both maxima)
C        located less than (2*pi/20) apart in the parameter domain for
C        the ellipse's limb, where the limb is parameterized as
C
C           CENTER + cos(theta)*SMAJOR  + sin(theta)*SMINOR, 
C
C           0 <= theta <= 2*pi
C
C        and 
C
C           CENTER is the center of the limb
C           SMAJOR is a semi-major axis vector of the limb
C           SMINOR is a semi-minor axis vector of the limb
C
C        The search can also fail to find an absolute extremum in cases
C        where there are two extrema (both minima or both maxima) that
C        are distant but very close to equal in terms of angular
C        separation from the input ray.
C
C
C     2) The point at which the minimum or maximum angular separation
C        occurs is determined to single precision.  Specifically, the
C        angular parameter THETA defining the location relative to the
C        semi-axes is determined at the single precision level.
C 
C    
C$ Literature_References
C
C     [1]  "Numerical Recipes -- The Art of Scientific Computing" by
C           William H. Press, Brian P. Flannery, Saul A. Teukolsky,
C           Willam T. Vetterling.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.1, 28-FEB-2008 (BVS) 
C
C        Corrected the contents of the Required_Reading section.
C
C-    SPICELIB Version 1.1.0, 14-NOV-2006 (NJB)
C
C        The parameter NPT has been replaced by two different
C        parameters:  one for the exterior minimum case and one for the
C        complementary cases. This change was made to improve accuracy.
C
C-    SPICELIB Version 1.0.0, 07-SEP-2005 (NJB)
C
C-&
 
C$ Index_Entries
C
C     angular separation of ray and ellipse
C
C-&
 
 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      TOUCHD
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP
      
      LOGICAL               FAILED
      LOGICAL               RETURN
      LOGICAL               VZERO
 
C
C     Local parameters
C

C
C     Tolerance used for loop convergence.  This tolerance applies
C     to the angular parameter used to specify points on the ellipse.
C
      DOUBLE PRECISION      TOL
      PARAMETER           ( TOL   = 1.D-9 )

C
C     Number of steps used to search the ellipse for region containing
C     the point of extreme angular separation.  We use two different
C     values:  one for the outer minimum case, which is mathematically
C     well behaved, and one for the other cases.
C
      INTEGER               SMLNPT
      PARAMETER           ( SMLNPT =  320 )

      INTEGER               BIGNPT
      PARAMETER           ( BIGNPT =  400 )
 
C
C     Maximum number of loop iterations allowed for extremum search.
C
      INTEGER               MAXITR
      PARAMETER           ( MAXITR = 100 )

C
C     Code returned in INRYPL indicating ray lies in plane.
C
      INTEGER               INF
      PARAMETER           ( INF    = -1 )

C
C     String length for extremum specifier.
C
      INTEGER               EXTLEN
      PARAMETER           ( EXTLEN = 3 )

C
C     Local variables
C
      CHARACTER*(EXTLEN)    EXTTYP

      DOUBLE PRECISION      A
      DOUBLE PRECISION      ACOMP
      DOUBLE PRECISION      ASIGN
      DOUBLE PRECISION      B
      DOUBLE PRECISION      BCOMP
      DOUBLE PRECISION      BTWEEN
      DOUBLE PRECISION      BTWPRX
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DIFF   ( 3 )
      DOUBLE PRECISION      EPLANE ( UBPL )
      DOUBLE PRECISION      EXTPRX
      DOUBLE PRECISION      GR
      DOUBLE PRECISION      LEVEL 
      DOUBLE PRECISION      LOWER
      DOUBLE PRECISION      LPT    ( 3 )
      DOUBLE PRECISION      NEWPT
      DOUBLE PRECISION      P2
      DOUBLE PRECISION      PROXY
      DOUBLE PRECISION      SMAJOR ( 3 )
      DOUBLE PRECISION      SMINOR ( 3 )
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      UDIFF  ( 3 )
      DOUBLE PRECISION      UDIR   ( 3 )
      DOUBLE PRECISION      UPPER
      DOUBLE PRECISION      V2     ( 3 )
      DOUBLE PRECISION      VPRJ   ( 3 )
      DOUBLE PRECISION      XOFF   ( 3 )
      DOUBLE PRECISION      XPT    ( 3 )

      INTEGER               I
      INTEGER               EXTIDX
      INTEGER               NITR
      INTEGER               NPT
      INTEGER               NXPTS
 
      LOGICAL               DOMIN
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZASRYEL' )
      END IF

C
C     Decide whether we're looking for a minimum or maximum.
C
      CALL CMPRSS ( ' ', 0, EXTREM, EXTTYP )
      CALL LJUST  (         EXTTYP, EXTTYP )

      IF ( EXTTYP .EQ. 'MIN' ) THEN

         DOMIN = .TRUE.

      ELSE IF ( EXTTYP .EQ. 'MAX' ) THEN

         DOMIN = .FALSE.

      ELSE

         CALL SETMSG ( 'Extremum specifier # was not recognized.' )
         CALL ERRCH  ( '#',  EXTREM                               )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                      )
         CALL CHKOUT ( 'ZZASRYEL'                                 )
         RETURN

      END IF
 
C
C     Get the center and semi-axes of the ellipse.
C
      CALL EL2CGV ( ELLIPS, CENTER, SMAJOR, SMINOR )

C 
C     The ellipse semi-axes must have positive length.
C 
      A   =   VNORM(SMAJOR)
      B   =   VNORM(SMINOR)

      IF (  VZERO(SMAJOR) .OR. VZERO(SMINOR)  )   THEN
 
         CALL SETMSG ( 'Semi-axis lengths:  A = #, B = #.' )
         CALL ERRDP  ( '#', A                              )
         CALL ERRDP  ( '#', B                              )
         CALL SIGERR ( 'SPICE(INVALIDAXISLENGTH)'          )
         CALL CHKOUT ( 'ZZASRYEL'                          )
         RETURN
 
      END IF

C
C     Find the plane of the ellipse.
C
      CALL PSV2PL ( CENTER, SMAJOR, SMINOR, EPLANE )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZASRYEL' )
         RETURN
      END IF
 
C
C     The ray's direction vector must be non-zero.
C
      IF ( VZERO(DIR) ) THEN
 
         CALL SETMSG ( 'Ray''s direction vector must be non-zero.' )
         CALL SIGERR ( 'SPICE(ZEROVECTOR)'                         )
         CALL CHKOUT ( 'ZZASRYEL'                                  )
         RETURN
 
      END IF

C
C     The ray's vertex must not lie in the plane of the ellipse.
C     The orthogonal projection of the point onto the plane should
C     yield a distinct vector.
C
      CALL VPRJP ( VERTEX, EPLANE, VPRJ )

      IF ( VDIST(VERTEX, VPRJ) .EQ. 0.D0  )  THEN
  
         CALL SETMSG ( 'Viewing point is in the plane of the ellipse.' )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'                         )
         CALL CHKOUT ( 'ZZASRYEL'                                    )
         RETURN
  
      END IF
 
C
C     See whether the ray intersects the plane region bounded by the
C     ellipse.  If it does, set the limb angle sign to -1.  Otherwise
C     the sign is +1.
C
C     First, find the intersection of the ray and plane.
C
      CALL INRYPL ( VERTEX, DIR, EPLANE, NXPTS, XPT )
      
      IF ( NXPTS .EQ. INF )  THEN
C
C        We don't expect to hit this case since we've already tested
C        for the vertex lying in the ellipse plane.  However, 
C        variations in round-off error make this case possible though
C        unlikely.
C
         CALL SETMSG ( 'Ray lies in the plane of the ellipse.' )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'                 )
         CALL CHKOUT ( 'ZZASRYEL'                              )
         RETURN
  
      END IF

C
C     Give NPT an initial value.
C
      NPT = BIGNPT

      IF ( NXPTS .EQ. 0 ) THEN
C
C        The ray does not intersect the plane.
C
         ASIGN = 1.D0

      ELSE
C
C        The ray intersects the plane.  We must determine if the
C        ray intersects the region bounded by the ellipse.
C
C        Find the coordinates of the intersection point in a frame
C        aligned with the axes of the ellipse and centered at
C        the ellipse's center.
C
         CALL VSUB ( XPT, CENTER, XOFF )

         ACOMP = VDOT ( XOFF, SMAJOR ) / A
         BCOMP = VDOT ( XOFF, SMINOR ) / B

C
C        Now find the "level curve parameter" LEVEL for the offset of
C        the intersection point from the ellipse's center.
C
         LEVEL =  ACOMP**2 / A**2   +   BCOMP**2 / B**2

         IF ( LEVEL .LE. 1.D0 ) THEN
C
C           The ray-plane intersection is on the ellipse or inside the
C           plane region bounded by the ellipse.
C
            ASIGN = -1.D0

         ELSE

            ASIGN =  1.D0

            IF ( DOMIN ) THEN
C
C              We have the exterior minimum case:  the ray doesn't
C              penetrate the plane region bounded by the ellipse,
C              and we're looking for an absolute minimum of angular
C              separation.  We can use a fairly small number of test
C              points on the limb and still find the location of
C              minimum angular separation.
C
               NPT = SMLNPT

            END IF

         END IF

      END IF
C
C     ASIGN has been set.
C
 
C
C     The limb is the set of points
C
C        CENTER   +   cos(theta) SMAJOR   +   sin(theta) SMINOR
C
C     where theta is in the interval (-pi, pi].
C
C     We want to find the value of `theta' for which the angular
C     separation of ray and ellipse is minimized (or maximized).  To
C     improve efficiency, instead of working with angular separation,
C     we'll find the extremum of a proxy function:  the distance
C     between the unit ray direction vector and the unit vector in the
C     direction from the ray's vertex to a selected point on the
C     ellipse.  This function doesn't require an arcsine evaluation,
C     and its extrema occur at the same locations as the extrema of the
C     angular separation.
C     
C     We'll compute the proxy value for the angular separation of the
C     ray and limb at NPT different points on the limb, where the
C     points are generated by taking equally spaced values of theta.
C     We'll find the extremum of the proxy function on this set of
C     points, and then search for the absolute extremum.
C
C     To make our computations more efficient, we'll subtract off
C     the ellipse's center from the vertex position to obtain a
C     translated ellipse centered at the origin.  
C
      CALL VSUB ( VERTEX, CENTER, V2 )

      IF ( DOMIN ) THEN
         EXTPRX = 2.D0
      ELSE
         EXTPRX = 0.D0
      END IF

      EXTIDX = 0

      P2     = TWOPI()
      DELTA  = P2 / NPT

      CALL VHAT ( DIR, UDIR )


      DO I = 0, NPT-1
 
         THETA  =  I * DELTA  

         CALL VLCOM3 (  -1.D0,        V2,
     .                  COS(THETA),   SMAJOR,
     .                  SIN(THETA),   SMINOR,   DIFF  )
         
         CALL VHAT ( DIFF, UDIFF )

         PROXY = VDIST ( UDIFF, UDIR )         

         IF ( DOMIN ) THEN

            IF ( PROXY .LT. EXTPRX ) THEN

               EXTIDX  =  I
               EXTPRX  =  PROXY

            END IF

         ELSE

            IF ( PROXY .GT. EXTPRX ) THEN

               EXTIDX  =  I
               EXTPRX  =  PROXY

            END IF

         END IF
 
      END DO      
 
C
C     The extreme value of the proxy function is EXTPRX, and was
C     obtained at the test point indexed by EXTIDX.  We find the values
C     of the proxy function at the neighboring points and perform a
C     `golden section' search.
C
C     In the following section of code,
C
C        LOWER          is the lower bound of the interval in which
C                       the extremum is bracketed.
C
C        UPPER          is the upper bound of the interval in which
C                       the extremum is bracketed.
C
C        BTWEEN         is a point between LOWER and UPPER.  The proxy
C                       function value corresponding to the angle
C                       BTWEEN is less than the proxy function value
C                       corresponding to LOWER and UPPER.
C
C        NEWPT          is a point between LOWER and UPPER such that
C                                                                  ___
C                          BTWEEN - LOWER                  3  -  \/ 5
C                          --------------    =   GR   =    ------------
C                          UPPER  - LOWER                        2
C
C
      GR     =  (  3.D0 -  SQRT(5.D0)  )  /  2.D0
 
      LOWER  =  ( P2 / NPT )  *  ( EXTIDX - 1 ) 
      UPPER  =  ( P2 / NPT )  *  ( EXTIDX + 1 ) 
 
C
C     We're going to move LOWER and UPPER closer together at each
C     iteration of the following loop, thus trapping the extremum. The
C     invariant condition that we will maintain is that the proxy value
C     corresponding to the angle BTWEEN is less (or more) than the proxy
C     value for the limb points corresponding to LOWER and UPPER.
C
C     The loop terminates when the offset by which we adjust LOWER or
C     UPPER is smaller than our tolerance value. This offset is no
C     larger than the difference between LOWER and BTWEEN.
C
      BTWEEN  =  ( P2 / NPT )  *  EXTIDX
 
C
C     We'll give the names LOWPRX and UPRPRX to the proxy function
C     values at the limb points corresponding to LOWER and UPPER,
C     respectively. We don't actually have to evaluate these values,
C     however. They are useful for understanding the minimization
C     algorithm we'll use, but are not actually used in the code.
C
C     We already know that the proxy function value corresponding to
C     BTWEEN is EXTPRX; this was computed above.
C
      BTWPRX  =  EXTPRX
 
C
C     Before starting our loop, we're going to shift all of our angles
C     by 2*pi, so that they're bounded away from zero.
C
      LOWER   =  LOWER  + P2
      UPPER   =  UPPER  + P2
      BTWEEN  =  BTWEEN + P2
 
      NITR    = 0
      PROXY   = 3.D0

      DO WHILE (        (  NITR                .LE. MAXITR ) 
     .           .AND.  (  TOUCHD(UPPER-LOWER) .GT. TOL    )  )
C
C        At this point, the following order relations hold:
C
C           LOWER  <    BTWEEN    <   UPPER
C                  -              -
C
C           BTWPRX <  MIN ( LOWPRX, UPRPRX )
C                  -
C
C        Compute NEWPT.  This point is always located at the fraction
C        GR of the way into the larger of the intervals
C        [ LOWER, BTWEEN ] and [ BTWEEN, UPPER ].
C
C
         IF (  ( BTWEEN - LOWER )  .GT. ( UPPER - BTWEEN )  ) THEN
 
            NEWPT = LOWER   +  GR * ( BTWEEN - LOWER  )
         ELSE
            NEWPT = BTWEEN  +  GR * ( UPPER  - BTWEEN )
         END IF
 
C
C        We are going to shorten our interval by changing LOWER to
C        NEWPT or UPPER to BTWEEN, and if necessary, BTWEEN to NEWPT,
C        while maintaining the order relations of UPPER, LOWER, and
C        BTWEEN, and also the order relations of UPRPRX, LOWPRX, and
C        BTWPRX.  To do this, we need the proxy function value at
C        NEWPT.
C
         CALL VLCOM3 (  -1.D0,        V2,
     .                  COS(NEWPT),   SMAJOR,
     .                  SIN(NEWPT),   SMINOR,   DIFF  )
  
         CALL VHAT ( DIFF, UDIFF )

         PROXY = VDIST ( UDIFF, UDIR )

C
C        Swap NEWPT and BTWEEN if necessary, to ensure that
C
C           NEWPT  <  BTWEEN.
C                  _
C
         IF ( NEWPT .GT. BTWEEN ) THEN
 
            CALL SWAPD ( BTWEEN, NEWPT )
            CALL SWAPD ( BTWPRX, PROXY )
 
         END IF
 
  
         IF ( DOMIN ) THEN

            IF ( PROXY .GT. BTWPRX ) THEN

               LOWER  = NEWPT
            ELSE
               UPPER  = BTWEEN
               BTWEEN = NEWPT
               BTWPRX = PROXY
            END IF

         ELSE

            IF ( PROXY .LT. BTWPRX ) THEN

               LOWER  = NEWPT
            ELSE
               UPPER  = BTWEEN
               BTWEEN = NEWPT
               BTWPRX = PROXY
            END IF

         END IF
 
         NITR = NITR + 1

      END DO

C
C     At this point, LPT is a good estimate of the limb point at which
C     the extremum of the angular separation from the ray occurs.
C
      CALL VADD ( DIFF, V2, LPT )

C
C     Add the center back to LPT to find EXTPT on the original ellipse.
C
      CALL VADD ( CENTER, LPT, EXTPT )

C 
C     Set the angular separation at EXTPT.
C
      ANGLE  =  VSEP( DIFF,  UDIR ) * ASIGN
 

      CALL CHKOUT ( 'ZZASRYEL' )
      RETURN
      END
