C$Procedure      ZZELVUPY ( Is ellipse in polygonal field of view? )

      SUBROUTINE ZZELVUPY ( ELLIPS, VERTEX, AXIS, N, BOUNDS, FOUND )
      IMPLICIT NONE
 
C$ Abstract
C
C     Determine whether a specified ellipse intersects the pyramid
C     defined by a polygonal field of view.
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
C     PLANES
C
C$ Keywords
C
C     ELLIPSE
C     GEOMETRY
C     MATH
C     PLANE
C
C$ Declarations
 
      INCLUDE 'errhnd.inc'

      INTEGER               UBEL
      PARAMETER           ( UBEL = 9 )

      INTEGER               UBPL
      PARAMETER           ( UBPL = 4 )

      INTEGER               MAXFOV
      PARAMETER           ( MAXFOV = 10000 )

      DOUBLE PRECISION      ELLIPS ( UBEL )
      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      AXIS   ( 3 )
      INTEGER               N
      DOUBLE PRECISION      BOUNDS ( 3, N )
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ELLIPS     I   A SPICELIB ellipse.
C     VERTEX     I   Vertex of a pyramid.
C     AXIS       I   Axis of a pyramid.
C     N          I   Number of boundary vectors of the pyramid.
C     BOUNDS     I   Boundary vectors of the pyramid.
C     FOUND      O   Flag indicating whether intersection was found.
C     UBEL       P   Upper bound of SPICELIB ellipse array.
C     UBPL       P   Upper bound of SPICELIB plane array.
C     MAXFOV     P   Maximum number of boundary vectors.
C
C$ Detailed_Input
C
C     ELLIPS         is a SPICELIB ellipse having non-zero semi-axes.
C
C     VERTEX         is the single point of intersection of the vectors
C                    defining the edges of a pyramid.  The vectors
C                    emanate from this point.  The pyramid represents
C                    the spatial region viewed by a polygonal field of
C                    view (FOV).
C
C     AXIS           is a vector emanating from VERTEX that lies inside
C                    the pyramid defined by VERTEX, N, and BOUNDS.
C                    AXIS represents the boresight direction of the FOV.
C
C     N,
C     BOUNDS         are, respectively, the number of boundary vectors
C                    defining the pyramid and the boundary vectors
C                    themselves.  Each pair of consecutive vectors in
C                    the array BOUNDS, together with VERTEX, defines a
C                    face of the pyramid.
C
C                    Each boundary vector must have angular separation
C                    of less than pi/2 radians from AXIS. 
C
C                    For any plane that doesn't contain VERTEX and that
C                    intersects AXIS at right angles, the intersections
C                    of the boundary vectors with that plane are the
C                    vertices of a polygon.  The polygon need not be
C                    convex, but it must be non-self-intersecting.
C
C
C$ Detailed_Output
C
C     FOUND          is set to .TRUE. if the pyramid and ellipse
C                    intersect; otherwise FOUND is .FALSE.
C
C$ Parameters
C
C     UBEL           is the array upper bound for SPICELIB ellipses.
C 
C     UBPL           is the array upper bound for SPICELIB planes.
C
C     MAXFOV         is the maximum number of boundary vectors that
C                    may be supplied in the input array argument
C                    BOUNDS.
C
C$ Exceptions
C
C     If an error is found, the output argument FOUND will be set to
C     .FALSE.
C
C
C     1)  If either of the semi-axes of the input ellipse is the 
C         zero vector, the error SPICE(ZEROVECTOR) will be signaled.
C
C     2)  If the norm of the input ellipse's semi-minor axis is 
C         zero after division by the maximum of the norms of the 
C         semi-major axis, the ellipse's center, and the vertex of
C         the pyramid, the error SPICE(DEGENERATECASE) will be 
C         signaled.
C
C     3)  If the vertex of the pyramid lies in the plane containing
C         the ellipse, at most the edge of the ellipse can be "seen"
C         from the vertex.  This case is not considered to be an
C         error.
C
C     4)  If the number of boundary vectors N is not at least 3,
C         or if the number exceeds MAXFOV, the error
C         SPICE(INVALIDCOUNT) will be signaled.
C
C     5)  If any boundary vector is the zero vector, the error
C         SPICE(ZEROVECTOR) will be signaled.
C
C     6)  If the axis is the zero vector, the error SPICE(ZEROVECTOR) 
C         will be signaled.
C
C     7)  If any boundary vector has angular separation of at least
C         pi/2 radians from AXIS, the error SPICE(INVALIDFOV)
C         will be signaled.
C
C     8)  If any boundary vector has angular separation of zero
C         radians from one of its neighbors, the error SPICE(INVALIDFOV)
C         will be signaled.
C
C     9)  No test is done to ensure that the input boundary vectors
C         define a non-self-intersecting polygon via their intersection
C         with a plane normal to AXIS.  If the boundary vectors don't
C         meet this condition, the results of this routine are
C         unreliable.
C
C     10) The pyramidal field of view and the input ellipse must not
C         differ too radically in scale, or great loss of precision
C         will result, making the results of this routine unreliable.
C         For example, if the ratio of the norm of the semi-minor axis
C         of the ellipse to the distance from VERTEX to the center of
C         the ellipse is less than double precision epsilon on the host
C         system, a meaningful result can't be computed.
C
C         This routine does not attempt to judge the minimum
C         acceptable level of accuracy.
C         
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is useful for determining whether an ellipsoidal
C     body is in the field of view of a remote-sensing instrument
C     with a field of view having polygonal cross section.
C     
C$ Examples
C
C     Test an ellipse for intersection with a square field 
C     of view.
C
C
C           PROGRAM EX1
C           IMPLICIT NONE
C
C           INTEGER               MAXN
C           PARAMETER           ( MAXN   = 4 )
C
C           INTEGER               UBEL
C           PARAMETER           ( UBEL   = 9 )
C
C           DOUBLE PRECISION      AXIS   ( 3 )
C           DOUBLE PRECISION      CENTER ( 3 )
C           DOUBLE PRECISION      ELLIPS ( UBEL )
C           DOUBLE PRECISION      FOV    ( 3, MAXN )
C           DOUBLE PRECISION      SMAJOR ( 3 )
C           DOUBLE PRECISION      SMINOR ( 3 )
C           DOUBLE PRECISION      VERTEX ( 3 )
C
C           INTEGER               N
C
C           LOGICAL               FOUND
C
C     C
C     C     The FOV (field of view) "looks" in the -x direction:
C     C     the axis of the FOV is parallel to the x axis.
C     C     The FOV intersects the plane of the ellipse in a
C     C     square having height and width 4 units.  The edges
C     C     of the square are parallel to the y and z axes.
C     C
C           N = 4
C
C           CALL VPACK ( -1.D0,  -1.D0, -1.D0,  FOV(1,1) )
C           CALL VPACK ( -1.D0,   1.D0, -1.D0,  FOV(1,2) )
C           CALL VPACK ( -1.D0,   1.D0,  1.D0,  FOV(1,3) )
C           CALL VPACK ( -1.D0,  -1.D0,  1.D0,  FOV(1,4) )
C
C           CALL VPACK ( -1.D0,   0.D0,  0.D0,  AXIS     )
C           CALL VPACK (  1.D0,   0.D0,  0.D0,  VERTEX   )
C
C     C
C     C     The ellipse is oriented with the major axis
C     C     vertical and is parallel to the x-z plane.  The ellipse
C     C     lies in the plane defined by x = -1.  The ellipse
C     C     ever-so-slightly overlaps the bottom edge of the FOV.
C     C
C           CALL VPACK (  0.D0,   0.D0,   1.D0,           SMAJOR )
C           CALL VPACK (  0.D0,   5.D-1,  0.D0,           SMINOR )
C           CALL VPACK ( -1.D0,   0.D0,  -3.D0 + 1.D-12,  CENTER )
C
C     C
C     C     Create a SPICELIB ellipse from the center and semi-axes.
C     C
C           CALL CGV2EL ( CENTER, SMAJOR, SMINOR, ELLIPS )
C
C     C
C     C     Test for intersection.  We expect an intersection to be
C     C     found.
C     C
C           CALL ZZELVUPY ( ELLIPS, VERTEX, AXIS, N, FOV, FOUND )
C
C           WRITE (*,*) 'Case 1: FOUND = ', FOUND
C
C     C
C     C     Shift the ellipse center to move the ellipse outside of
C     C     the FOV, then repeat the test.  We expect FOUND to be
C     C     .FALSE.
C     C
C           CALL VPACK ( -1.D0,   0.D0,  -3.D0 - 1.D-12,  CENTER )
C
C           CALL CGV2EL ( CENTER, SMAJOR, SMINOR, ELLIPS )
C
C           CALL ZZELVUPY ( ELLIPS, VERTEX, AXIS, N, FOV, FOUND )
C
C           WRITE (*,*) 'Case 2: FOUND = ', FOUND
C
C           END
C      
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
C     N.J. Bachman    (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 28-FEB-2008 (BVS) 
C
C        Corrected the contents of the Required_Reading section.
C
C-    SPICELIB Version 1.0.0, 10-AUG-2005 (NJB)
C
C-&
 
C$ Index_Entries
C
C     test whether pyramid intersects ellipse
C     test whether ellipse is in pyramidal field of view
C   
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP

      INTEGER               ZZWIND

      LOGICAL               RETURN
      LOGICAL               VZERO

C
C     Local parameters
C

C
C     Local variables
C
      CHARACTER*(LMSGLN)    ERRMSG

      DOUBLE PRECISION      A
      DOUBLE PRECISION      APEX   ( 3 )
      DOUBLE PRECISION      ASEP
      DOUBLE PRECISION      B
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      CONSEP
      DOUBLE PRECISION      CP     ( 3 )
      DOUBLE PRECISION      CTRVEC ( 3 )
      DOUBLE PRECISION      D
      DOUBLE PRECISION      EASIZE
      DOUBLE PRECISION      EBSCTR ( 3 )
      DOUBLE PRECISION      EDGE1  ( 3 )
      DOUBLE PRECISION      EDGE2  ( 3 )
      DOUBLE PRECISION      ELLSCL ( UBEL )
      DOUBLE PRECISION      EPLANE ( UBPL )
      DOUBLE PRECISION      FOVPLN ( UBPL )
      DOUBLE PRECISION      GV1    ( 3 )
      DOUBLE PRECISION      GV2    ( 3 )
      DOUBLE PRECISION      HAFEDG
      DOUBLE PRECISION      HAFSEC
      DOUBLE PRECISION      OFFSET ( 3 )
      DOUBLE PRECISION      ORIGIN ( 3 )
      DOUBLE PRECISION      PASIZE
      DOUBLE PRECISION      PLANE  ( UBPL )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SMAJOR ( 3 )
      DOUBLE PRECISION      SMINOR ( 3 )
      DOUBLE PRECISION      VBSCTR ( 3 )
      DOUBLE PRECISION      VTEMP  ( 3 )
      DOUBLE PRECISION      VXPT1  ( 3 )
      DOUBLE PRECISION      VXPT2  ( 3 )
      DOUBLE PRECISION      WORK   ( 3, MAXFOV )
      DOUBLE PRECISION      X
      DOUBLE PRECISION      XPT    ( 3 )
      DOUBLE PRECISION      XPT1   ( 3 )
      DOUBLE PRECISION      XPT2   ( 3 )
      DOUBLE PRECISION      Y

      INTEGER               I
      INTEGER               J
      INTEGER               NXPTS
      

C
C     Saved variables
C     
      SAVE                 ORIGIN
      SAVE                 WORK

C
C     Initial values
C     
      DATA                 ORIGIN /  3 * 0.D0 /


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZELVUPY' )

C
C     We start out by checking the inputs. 
C
C     The next step will be to look for an intersection of the ellipse
C     and pyramid.  There are three intersection cases:
C
C        1) The ellipse is completely contained in the pyramid.
C
C        2) The ellipse "contains" the field of view in the sense
C           that the intersection of the pyramid and the plane of the
C           ellipse is contained in the region bounded by the ellipse.
C
C        3) One or more sides of the pyramid intersect the ellipse.
C
C     There is also a non-intersection case:  this is when cones
C     bounding the ellipse and pyramid and having their apexes in
C     common with that of the pyramid intersect only in that common
C     apex.  Before test (1), we perform this non-intersection test,
C     since it can be done quickly.
C
C     No intersection has been found so far.  Set the default value
C     of the FOUND flag here so it won't have to be set in every error
C     checking block below.
C
      FOUND = .FALSE.

C
C     Validate the ellipse.  First find the center and the semi-axes
C     of the ellipse.
C
      CALL EL2CGV ( ELLIPS, CENTER, GV1,    GV2    )
      CALL SAELGV ( GV1,    GV2,    SMAJOR, SMINOR )
      
C
C     Check the semi-axis lengths.
C
C     If the semi-major axis is the zero vector, we'd expect
C     the semi-minor axis to be the zero vector as well.  But
C     round-off error could conceivably violate this assumption.
C
      IF ( VZERO(SMAJOR) .OR. VZERO(SMINOR) ) THEN

         CALL SETMSG ( 'Input ellipse has semi-major axis '   //
     .                 'length # and semi-minor axis length ' //
     .                 '#.  Both vectors are required to be ' //
     .                 'non-zero.'                            )
         CALL ERRDP  ( '#', VNORM(SMAJOR)                     )
         CALL ERRDP  ( '#', VNORM(SMINOR)                     )
         CALL SIGERR ( 'SPICE(ZEROVECTOR)'                    )
         CALL CHKOUT ( 'ZZELVUPY'                             )
         RETURN

      END IF

C
C     Scale the vectors defining the ellipse and the vertex of the
C     pyramid so that the largest of these vectors has unit length.
C
      SCALE = 1.D0 / MAX ( VNORM(CENTER), VNORM(SMAJOR), VNORM(VERTEX) )

      DO I = 1, 3
         
         CENTER(I) = SCALE * CENTER(I)
         SMAJOR(I) = SCALE * SMAJOR(I)
         SMINOR(I) = SCALE * SMINOR(I)
         APEX  (I) = SCALE * VERTEX(I)
         
      END DO

C
C     Create a scaled ellipse.  We'll perform the FOV side-ellipse
C     intersection computations using this ellipse.
C
      CALL CGV2EL ( CENTER, SMAJOR, SMINOR, ELLSCL )

C
C     After scaling, make sure the semi-axes have sufficient length to
C     prevent numerical problems.  Let A and B be the scaled semi-axis
C     lengths of the ellipse.
C
      A = VNORM ( SMAJOR )
      B = VNORM ( SMINOR )

      IF ( B .EQ. 0.D0 ) THEN

         CALL SETMSG ( 'Scaled ellipse''s semi-minor axis length = 0.' )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'                         )
         CALL CHKOUT ( 'ZZELVUPY'                                      )
         RETURN  

      END IF
         
C
C     Validate the input pyramid.
C
C     The axis must not be the zero vector.
C
      IF (  VZERO(AXIS)  ) THEN

         CALL SETMSG ( 'The pyramid''s axis the zero vector.' )
         CALL SIGERR ( 'SPICE(ZEROVECTOR)'                    )
         CALL CHKOUT ( 'ZZELVUPY'                             )
         RETURN

      END IF

C
C     There must be at least three boundary vectors.
C
      IF ( N .LT. 3 ) THEN

         CALL SETMSG ( 'The number of boundary vectors was #; ' //
     .                 'this number must be at least 3.'        )
         CALL ERRINT ( '#',  N                                  )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                    )
         CALL CHKOUT ( 'ZZELVUPY'                               )
         RETURN

      END IF

C
C     There must be no more than MAXFOV boundary vectors.
C
      IF ( N .GT. MAXFOV ) THEN

         CALL SETMSG ( 'The number of boundary vectors was #; ' //
     .                 'this number must not exceed #.'         )
         CALL ERRINT ( '#',  N                                  )
         CALL ERRINT ( '#',  MAXFOV                             )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                    )
         CALL CHKOUT ( 'ZZELVUPY'                               )
         RETURN

      END IF

C
C     We must initialize certain variables before continuing with
C     the checks.
C
C     Let CTRVEC be the vector from the apex to the center of the
C     ellipse.  This vector will be used in several places later;
C     it's convenient to compute it here.
C
      CALL VSUB ( CENTER, APEX, CTRVEC )

C
C     Compute PASIZE:  an upper bound on the angular radius of a 
C     circular cone whose axis is the input central axis.  While
C     we're at it, check the angular separation of the boundary
C     vectors from the central axis and from each other.
C
      PASIZE = 0.D0

      DO I = 1, N
C
C        Each boundary vector must have angular separation from the
C        axis of less than pi/2 radians.  Keep track of the maximum
C        angular separation PASIZE as we go.  We'll use this variable
C        later in a non-intersection test.
C      
         ASEP = VSEP ( AXIS, BOUNDS(1,I) )

         IF ( ASEP .GE. PI()/2  ) THEN

            CALL SETMSG ( 'The angular separation of boundary '   //
     .                    'vector # from the axis is #. This '    //
     .                    'number must less than pi/2.'           )
            CALL ERRINT ( '#',  I                                 )
            CALL ERRDP  ( '#',  ASEP                              )
            CALL SIGERR ( 'SPICE(INVALIDFOV)'                     )
            CALL CHKOUT ( 'ZZELVUPY'                              )
            RETURN

         END IF

         PASIZE = MAX ( PASIZE, ASEP )

C
C        Each boundary vector must have non-zero angular separation
C        from its neighbors.
C
         IF ( I .LT. N ) THEN
            J  = I + 1
         ELSE
            J  = 1
         END IF
   
         CALL UCRSS ( BOUNDS(1,I), BOUNDS(1,J), CP )

         IF ( VZERO(CP) ) THEN
C
C           The cross product may be zero because one of the
C           boundary vectors is zero.  Check this first.
C
            IF (  VZERO( BOUNDS(1,J) ) .OR. VZERO( BOUNDS(1,I) ) ) THEN

               ERRMSG = 'The # boundary vector is the zero vector.'
               
               IF (  VZERO( BOUNDS(1,I) )  ) THEN
                  J = I
               END IF
                                 
               CALL REPMOT ( ERRMSG, '#', J, 'L', ERRMSG )
               CALL SETMSG ( ERRMSG                      )
               CALL SIGERR ( 'SPICE(ZEROVECTOR)'         )

            ELSE

               CALL SETMSG ( 'The angular separation of boundary '   //
     .                       'vector # from vector # is 0.'          //
     .                       'This number must be positive.'         )
               CALL ERRINT ( '#',  I                                 )
               CALL ERRINT ( '#',  J                                 )
               CALL SIGERR ( 'SPICE(INVALIDFOV)'                     )

            END IF

            CALL CHKOUT ( 'ZZELVUPY' )
            RETURN

         END IF
 
      END DO

C
C     That's it for the error checks.  We'll now answer the question
C     this routine is meant to answer:  does the ellipse or the region
C     it bounds intersect the pyramid?
C
C     We'll start out with a simple check to rule out intersection
C     when the ellipse and pyramid are contained in disjoint right
C     circular cones with a common apex.
C
C     Find the angular radius (that is, one-half of the angular extent)
C     of a bounding cone of the ellipse as seen from the apex.  The
C     cone circumscribes a sphere of radius A centered at the ellipse's
C     center, where A is the length of the semi-major axis.  Note that
C     the cone does not in general circumscribe the ellipse itself.
C
C     The test can be performed only if the apex of the FOV is outside
C     of the sphere of radius A centered at the ellipse center.
C
      D  =  VDIST ( CENTER, APEX )

      IF ( A .LT. D ) THEN

         EASIZE  =  ASIN ( A / D )

C
C        The variable PASIZE already contains the angular radius of a
C        bounding cone of the pyramid as seen from the pyramid's apex.
C        The angular radius is the maximum of the angular separations
C        of each pyramid edge from the pyramid's axis. Check whether
C        the bounding cones of ellipse and pyramid are disjoint. Recall
C        CTRVEC is the vector from the apex to the center of the
C        ellipse.  If the angular separation of CTRVEC and AXIS exceeds
C        the sum of the angular radii of the ellipse's and pyramid's
C        bounding cones, there can be no intersection.
C
         CONSEP = VSEP ( CTRVEC, AXIS )  -  ( EASIZE + PASIZE )

         IF ( CONSEP .GT. 0.D0 ) THEN

            CALL CHKOUT ( 'ZZELVUPY' )
            RETURN
         END IF

      END IF

C
C     At this point, we have to take a more detailed look at the
C     possible intersection of ellipse and pyramid.  First check
C     whether the center of the ellipse is contained in the pyramid.
C     If the ellipse is completely contained in the pyramid, this
C     check will yield a positive result.
C
C     The center of the ellipse is inside the pyramid if a plane
C     containing this point and normal to the axis vector
C     chops the pyramid in a polygon that has non-zero winding 
C     number about the center. 
C
C     The center of the ellipse must lie in the correct half-space
C     for this test to be applicable.
C
      IF (  VDOT( AXIS, CTRVEC ) .GT. 0.D0 ) THEN
C
C        Construct the plane and find the polygon.
C     
         CALL NVP2PL ( AXIS, CTRVEC, FOVPLN )

C
C        Create the planar FOV boundary using the intersections
C        of the FOV boundary vectors with FOVPLN.
C
         DO I = 1, N

            CALL INRYPL ( ORIGIN,  BOUNDS(1,I),  FOVPLN, 
     .                    NXPTS,   WORK  (1,I)           )

C
C           We expect to have a single point of intersection for each
C           boundary vector.
C
            IF ( NXPTS .NE. 1 ) THEN

               CALL SETMSG ( 'NXPTS = # for boundary vector #/FOV ' //
     .                       'plane intersection.'                  )
               CALL ERRINT ( '#',  NXPTS                            )
               CALL ERRINT ( '#',  I                                )
               CALL SIGERR ( 'SPICE(BUG)'                           )
               CALL CHKOUT ( 'ZZELVUPY'                             )
               RETURN

            END IF

         END DO

C
C        Now WORK contains the polygon representing the intersection of
C        the pyramid with the plane FOVPLN. If the winding number of
C        the polygon about the ellipse center is non-zero, we conclude
C        the center is in the pyramid.
C
         IF (   ZZWIND( FOVPLN, N, WORK, CTRVEC )  .NE.  0 ) THEN
C
C           The center of the ellipse is inside the pyramid.  We're
C           done.
C
            FOUND = .TRUE.

            CALL CHKOUT ( 'ZZELVUPY' )
            RETURN

         END IF

      END IF

C
C     Check whether the ray defined by APEX and the first boundary
C     vector of the pyramid (the "boundary ray") intersects the plane
C     region bounded by the ellipse.  If the intersection of the
C     pyramid and the plane of the ellipse is completely contained in
C     the region bounded by the ellipse, this check will yield a
C     positive result.
C
C     First find the intersection of the boundary ray and the plane
C     containing the ellipse; represent this plane using the SPICELIB
C     plane EPLANE.

C     We don't check FAILED() here because the spanning vectors
C     are orthogonal, and because PSV2PL (via a call to UCRSS)
C     does scaling to prevent underflow.
C
      CALL PSV2PL ( CENTER,  SMAJOR,       SMINOR,  EPLANE )

      CALL INRYPL ( APEX,    BOUNDS(1,1),  EPLANE,  NXPTS,  XPT )

C
C     The routine INRYPL can return the NXPTS values 1, 0, or INF---a
C     code indicating an infinite number of intersection points of ray
C     and plane.  If the value is 1, the boundary ray may intersect
C     the region bounded by the ellipse.
C
      IF ( NXPTS .EQ. 1 ) THEN
C
C        The boundary ray intersects the plane of the ellipse in a
C        single point. Decide whether this point is inside the ellipse.
C        To test for containment, find the "coordinates" of the
C        center-to-point vector relative to the two-dimensional basis
C        formed by the semi-axes of the ellipse.  Call this
C        center-to-point vector OFFSET.  Recall A and B are the
C        semi-axis lengths of the ellipse. Let X and Y be the
C        coordinates of OFFSET in the two-dimensional reference frame
C        whose basis consists of normalized versions of SMAJOR and
C        SMINOR.
C
C        Note that we could have the special case in which the vertex
C        of the pyramid lies in the plane of the ellipse, in which case
C        the FOV "sees" the ellipse edge-on.  However, since NXPTS is
C        not INF, the boundary vector does not lie in the plane of the
C        ellipse.  So in this special case, APEX would be in the region
C        bounded by the ellipse.
C
         CALL VSUB ( XPT, CENTER, OFFSET )

         X  =  VDOT  ( OFFSET, SMAJOR ) / A
         Y  =  VDOT  ( OFFSET, SMINOR ) / B
 
         IF (  ( (X/A)**2.D0  +  (Y/B)**2.D0 ) .LE.  1.D0  ) THEN
C
C           The boundary-vector-plane intercept lies in the
C           topologically closed region bounded by the ellipse.
C
            FOUND = .TRUE.

            CALL CHKOUT ( 'ZZELVUPY' )
            RETURN

         END IF

      END IF

C
C     Check whether one of the pyramid's sides intersects the ellipse.
C     For each side, we first test whether the plane containing that
C     side intersects the ellipse.  If it does, the intersection is
C     a (possibly degenerate) line segment with endpoints on the 
C     ellipse.  The triangle (or segment) defined by the pyramid's 
C     apex and this segment (point) is then checked for intersection
C     with the currently considered side of the pyramid.
C
      I  =  1
      
      DO WHILE (  ( I .LE. N ) .AND. ( .NOT. FOUND )  )
C
C        Create a SPICELIB plane containing the Ith side of the 
C        pyramid.
C
         IF ( I .LT. N ) THEN
            J  = I + 1
         ELSE
            J  = 1
         END IF

C
C        Although PSV2PL can signal an error if the spanning
C        vectors are linearly dependent, it won't do so here
C        because we've already ensured the cross product of 
C        these vectors is non-zero.  
C       
         CALL PSV2PL ( APEX, BOUNDS(1,I), BOUNDS(1,J), PLANE )
C
C        Find the intersection of the plane and the ellipse.
C        
         CALL INELPL ( ELLSCL, PLANE, NXPTS, XPT1, XPT2 )

C
C        If the ellipse-plane intersection is non-empty, test it to see
C        whether it has non-empty intersection with the current side of
C        the pyramid.
C
         IF ( NXPTS .GT. 0 ) THEN
C
C           Let EDGE1 and EDGE2 be the unit length boundary vectors
C           forming the edges of the currently considered side of the
C           pyramid.
C
            CALL VHAT ( BOUNDS(1,I), EDGE1 )
            CALL VHAT ( BOUNDS(1,J), EDGE2 )

C
C           Let EBSCTR ("pyramid edge bisector") be a bisector of the
C           sector bounded by EDGE1 and EDGE2.
C
            CALL VLCOM ( 0.5D0, EDGE1, 0.5D0, EDGE2, EBSCTR )

C
C           Let HAFEDG be half of the angular measure of this sector.
C            
            HAFEDG = VSEP ( EDGE1, EDGE2 ) / 2.D0

C
C           Let VXPT1 and VXPT2 be the unit vectors pointing from the
C           pyramid's apex to the points of intersection of the ellipse
C           and the plane containing the currently considered side of
C           the pyramid.
C
            CALL VSUB ( XPT1,  APEX,  VTEMP )
            CALL VHAT ( VTEMP,        VXPT1 )

            CALL VSUB ( XPT2,  APEX,  VTEMP )
            CALL VHAT ( VTEMP,        VXPT2 )

C
C           At this point we'll introduce a bit of terminology. We're
C           going to work with plane regions defined by pairs of
C           vectors with a common endpoint.  We'll abuse standard
C           terminology a bit and call the region bounded by such a
C           vector pair a "sector."  Strictly speaking, sectors refer
C           only to subsets of a disc.
C
C           When it's convenient, we'll also identify "sectors" with
C           regions of the unit circle.  This will make it possible
C           to talk about intersections of sectors in terms of 
C           intersections of the associated arcs on the unit circle.
C           By the "endpoints" of a sector we mean the endpoints
C           of the arc associated with the sector on the unit circle.
C
C           Let VBSCTR ("VXPT bisector") be a bisector of the sector
C           bounded by VXPT1 and VXPT2.
C
            CALL VLCOM ( 0.5D0, VXPT1, 0.5D0, VXPT2, VBSCTR )

C
C           Let HAFSEC be half of the angular measure of the sector
C           bounded by VXPT1 and VXPT2.
C            
            HAFSEC = VSEP ( VXPT1, VXPT2 ) / 2.D0

C
C           EDGE1, EDGE2, VXPT1, and VXPT2 are four co-planar vectors
C           emanating from APEX.  We want to find out whether the
C           sector bounded by EDGE1 and EDGE2 intersects the sector
C           bounded by VXPT1 and VXPT2.  If there's an intersection, at
C           least one endpoint of one sector is contained in the other
C           sector.
C
C           Because of potential round-off problems when the sectors
C           are nearly coincident, we perform the precautionary check
C           (case 3) on the angle bisector of the sector defined by
C           VXPT1 and VXPT2.
C
C           If the sector defined by VXPT1 and VXPT2 has no endpoint
C           contained in the other sector, it's possible that the
C           former sector contains the latter.  In that case the
C           angular bisector of the latter sector is contained in the
C           former (case 4).
C
C           We test a vector's containment in a sector by comparing the
C           vector's angular separation from the sector's angle
C           bisector to one-half of the angular measure of the sector.
C
C              Case 1:  VXPT1 lies between EDGE1 and EDGE2.
C              Case 2:  VXPT2 lies between EDGE1 and EDGE2.
C              Case 3:  VBSCTR lies between EDGE1 and EDGE2.
C              Case 4:  EBSCTR lies between VXPT1 and VXPT2.
C
            IF      ( VSEP(VXPT1,  EBSCTR) .LE. HAFEDG ) THEN

               FOUND = .TRUE.

            ELSE IF ( VSEP(VXPT2,  EBSCTR) .LE. HAFEDG ) THEN

               FOUND = .TRUE.

            ELSE IF ( VSEP(VBSCTR, EBSCTR) .LE. HAFEDG ) THEN

               FOUND = .TRUE.

            ELSE IF ( VSEP(EBSCTR, VBSCTR) .LE. HAFSEC ) THEN

               FOUND = .TRUE.

            END IF

            IF ( FOUND ) THEN
C
C              We've found an intersection.  We're done.
C               
               CALL CHKOUT ( 'ZZELVUPY' )
               RETURN

            END IF

         END IF

C
C        If no intersection was found, look at the next side of the
C        pyramid.
C
         I = I + 1

      END DO

C
C     If we got this far, the ellipse is not in view.  FOUND has
C     already been set to .FALSE.
C

      CALL CHKOUT ( 'ZZELVUPY' )
      RETURN
      END


