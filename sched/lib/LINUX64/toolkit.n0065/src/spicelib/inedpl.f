C$Procedure      INEDPL ( Intersection of ellipsoid and plane )
 
      SUBROUTINE INEDPL ( A, B, C, PLANE, ELLIPS, FOUND )
 
C$ Abstract
C
C     Find the intersection of a triaxial ellipsoid and a plane.
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
C     ELLIPSOID
C     GEOMETRY
C     MATH
C
C$ Declarations
 
      INTEGER               UBEL
      PARAMETER           ( UBEL = 9 )
 
      INTEGER               UBPL
      PARAMETER           ( UBPL = 4 )
 
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      PLANE  ( UBPL )
      DOUBLE PRECISION      ELLIPS ( UBEL )
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     A          I   Length of ellipsoid semi-axis lying on the x-axis.
C     B          I   Length of ellipsoid semi-axis lying on the y-axis.
C     C          I   Length of ellipsoid semi-axis lying on the z-axis.
C     PLANE      I   Plane that intersects ellipsoid.
C     ELLIPS     O   Intersection ellipse, when FOUND is .TRUE.
C     FOUND      O   Flag indicating whether ellipse was found.
C
C$ Detailed_Input
C
C     A,
C     B,
C     C              are the lengths of the semi-axes of a triaxial
C                    ellipsoid.  The ellipsoid is centered at the
C                    origin and oriented so that its axes lie on the
C                    x, y and z axes.  A, B, and C are the lengths of
C                    the semi-axes that point in the x, y, and z
C                    directions respectively.
C
C     PLANE          is a SPICELIB plane.
C
C$ Detailed_Output
C
C     ELLIPS         is the SPICELIB ellipse formed by the intersection
C                    of the input plane and ellipsoid.  ELLIPS will
C                    represent a single point if the ellipsoid and
C                    plane are tangent.
C
C                    If the intersection of the ellipsoid and plane is
C                    empty, ELLIPS is not modified.
C
C
C     FOUND          is .TRUE. if and only if the intersection of the
C                    ellipsoid and plane is non-empty.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If any of the lengths of the semi-axes of the input ellipsoid
C         are non-positive, the error SPICE(DEGENERATECASE) is
C         signaled.  ELLIPS is not modified.  FOUND is set to .FALSE.
C
C     2)  If the input plane in invalid, in other words, if the input
C         plane as the zero vector as its normal vector, the error
C         SPICE(INVALIDPLANE) is signaled. ELLIPS is not modified.
C         FOUND is set to .FALSE.
C
C     3)  If the input plane and ellipsoid are very nearly tangent,
C         roundoff error may cause this routine to give unreliable
C         results.
C
C     4)  If the input plane and ellipsoid are precisely tangent, the
C         intersection is a single point.  In this case, the output
C         ellipse is degenerate, but FOUND will still have the value
C         .TRUE.  You must decide whether this output makes sense for
C         your application.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     An ellipsoid and a plane can intersect in an ellipse, a single
C     point, or the empty set.
C
C$ Examples
C
C     1)  Suppose we wish to find the limb of a body, as observed from
C         location LOC in body-fixed coordinates.  The SPICELIB routine
C         EDLIMB solves this problem.  Here's how INEDPL is used in
C         that solution.
C
C         We assume LOC is outside of the body. The body is modelled as
C         a triaxial ellipsoid with semi-axes of length A, B, and C.
C         The notation
C
C            < X, Y >
C
C         indicates the inner product of the vectors X and Y.
C
C         The limb lies on the plane defined by
C
C            < X,  N >  =  1,
C
C         where the vector N is defined as
C
C            ( LOC(1) / A**2,   LOC(2) / B**2,   LOC(3) / C**2 ).
C
C         The assignments
C
C            N(1) = LOC(1) / A**2
C            N(2) = LOC(2) / B**2
C            N(3) = LOC(3) / C**2
C
C         and the calls
C
C            CALL NVC2PL ( N,  1.0D0,  PLANE )
C
C            CALL INEDPL ( A,  B,  C,  PLANE,  LIMB,  FOUND )
C
C            CALL EL2CGV ( LIMB, CENTER, SMAJOR, SMINOR )
C
C         will return the center and semi-axes of the limb.
C
C
C         How do we know that  < X, N > = 1  for all X on the limb?
C         This is because all limb points X satisfy
C
C            < LOC - X, SURFNM(X) >  =  0,
C
C         where SURFNM(X) is a surface normal at X.  SURFNM(X) is
C         parallel to the vector
C
C            V = (  X(1) / A**2,   X(2) / B**2,   X(3) / C**2  )
C
C         so we have
C
C            < LOC - X, V >  =  0,
C
C            < LOC, V >      =  < X, V >  =  1  (from the original
C                                                ellipsoid
C                                                equation);
C         and finally
C
C            < X,   N >      =  1,
C
C         where the vector N is defined as
C
C            (  LOC(1) / A**2,    LOC(2) / B**2,   LOC(3) / C**2  ).
C
C
C     2)  Suppose we wish to find the terminator of a body.  We can
C         make a fair approximation to the location of the terminator
C         by finding the limb of the body as seen from the vertex of
C         the umbra; then the problem is essentially the same as in
C         example 1.  Let VERTEX be this location.  We make the
C         assignments
C
C            P(1) =   VERTEX(1) / A**2
C            P(2) =   VERTEX(2) / B**2
C            P(3) =   VERTEX(3) / C**2
C
C         and then make the calls
C
C            CALL NVC2PL ( P,  1.0D0,  PLANE )
C
C            CALL INEDPL ( A,  B,  C,  PLANE,  TERM,  FOUND )
C
C         The SPICELIB ellipse TERM represents the terminator of the
C         body.
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
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 16-NOV-2005 (NJB)
C
C        Bug fix:  error detection for case of invalid input plane was
C        added.
C
C        Updated to remove non-standard use of duplicate arguments
C        in VSCL calls.
C
C-    SPICELIB Version 1.1.0, 11-JUL-1995 (KRG)
C
C        Removed potential numerical precision problems that could be 
C        caused by using a REAL constant in a double precision 
C        computation. The value 1.0 was repaced with the value 1.0D0 in 
C        the following three lines:
C
C           DSTORT(1) = 1.0 / A
C           DSTORT(2) = 1.0 / B
C           DSTORT(3) = 1.0 / C
C
C        Also changed was a numeric constant from 1.D0 to the 
C        equivalent, but more aesthetically pleasing 1.0D0.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 02-NOV-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     intersection of ellipsoid and plane
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.2.0, 16-NOV-2005 (NJB)
C
C        Bug fix:  error detection for case of invalid input plane was
C        added.
C
C        Updated to remove non-standard use of duplicate arguments
C        in VSCL calls.
C
C-    SPICELIB Version 1.1.0, 11-JUL-1995 (KRG)
C
C        Removed potential numerical precision problems that could be 
C        caused by using a REAL constant in a double precision 
C        computation. The value 1.0 was repaced with the value 1.0D0 in 
C        the following three lines:
C
C           DSTORT(1) = 1.0 / A
C           DSTORT(2) = 1.0 / B
C           DSTORT(3) = 1.0 / C
C
C        Also changed was a numeric constant from 1.D0 to the 
C        equivalent, but more aesthetically pleasing 1.0D0.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      BRCKTD
      DOUBLE PRECISION      VNORM
 
      LOGICAL               RETURN
      LOGICAL               VZERO
 
C
C     Local variables
C
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      CONST
      DOUBLE PRECISION      DPLANE ( UBPL )
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      DSTORT ( 3 )
      DOUBLE PRECISION      INVDST ( 3 )
      DOUBLE PRECISION      MAXRAD
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      POINT  ( 3 )
      DOUBLE PRECISION      RCIRCL
      DOUBLE PRECISION      SPAN1  ( 3 )
      DOUBLE PRECISION      SPAN2  ( 3 )
      DOUBLE PRECISION      VEC1   ( 3 )
      DOUBLE PRECISION      VEC2   ( 3 )
 
      INTEGER               I
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'INEDPL' )
      END IF
 
C
C     We don't want to worry about flat ellipsoids:
C
      IF (         ( A .LE. 0.0D0 )
     .      .OR.   ( B .LE. 0.0D0 )
     .      .OR.   ( C .LE. 0.0D0 )   )   THEN
 
         FOUND = .FALSE.
 
         CALL SETMSG ( 'Semi-axes: A = #,  B = #,  C = #.'  )
         CALL ERRDP  ( '#', A                               )
         CALL ERRDP  ( '#', B                               )
         CALL ERRDP  ( '#', C                               )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'              )
         CALL CHKOUT ( 'INEDPL'                             )
         RETURN
 
      END IF

C
C     Check input plane for zero normal vector.
C
      CALL PL2NVC ( PLANE, NORMAL, CONST )

      IF ( VZERO(NORMAL) ) THEN

         CALL SETMSG ( 'Normal vector of the input ' //
     .                 'PLANE is the zero vector.'   )
         CALL SIGERR ( 'SPICE(INVALIDPLANE)'         )
         CALL CHKOUT ( 'INEDPL'                      )
         RETURN

      END IF

C
C     This algorithm is partitioned into a series of steps:
C
C
C     1)  Identify a linear transformation that maps the input
C         ellipsoid to the unit sphere.  We'll call this mapping the
C         `distortion' mapping.  Apply the distortion mapping to both
C         the input plane and ellipsoid.  The image of the plane under
C         this transformation will be a plane.
C
C     2)  Find the intersection of the transformed plane and the unit
C         sphere.
C
C     3)  Apply the inverse of the distortion mapping to the
C         intersection ellipse to find the undistorted intersection
C         ellipse.
C
 
 
C
C     Step 1:
C
C     Find the image of the ellipsoid and plane under the distortion
C     matrix.  Since the image of the ellipsoid is the unit sphere,
C     only the plane transformation requires any work.
C
C     If the input plane is too far from the origin to possibly
C     intersect the ellipsoid, return now.  This can save us
C     some numerical problems when we scale the plane and ellipsoid.
C
C     The point returned by PL2PSV is the closest point in PLANE
C     to the origin, so its norm gives the distance of the plane
C     from the origin.
C
      CALL PL2PSV  ( PLANE, POINT, SPAN1, SPAN2 )
 
      MAXRAD = MAX ( DABS(A), DABS(B), DABS(C) )
 
      IF ( VNORM (POINT) .GT. MAXRAD ) THEN
 
         FOUND = .FALSE.
         CALL CHKOUT ( 'INEDPL' )
         RETURN
 
      END IF
 
C
C     The distortion matrix and its inverse are
C
C        +-               -+        +-               -+
C        |  1/A   0    0   |        |   A    0    0   |
C        |   0   1/B   0   |,       |   0    B    0   |.
C        |   0    0   1/C  |        |   0    0    C   |
C        +-               -+        +-               -+
C
C     We declare them with length three, since we are going to make
C     use of the diagonal elements only.
C
      DSTORT(1) = 1.0D0 / A
      DSTORT(2) = 1.0D0 / B
      DSTORT(3) = 1.0D0 / C
 
      INVDST(1) =  A
      INVDST(2) =  B
      INVDST(3) =  C
 
C
C     Apply the distortion mapping to the input plane.  Applying
C     the distortion mapping to a point and two spanning vectors that
C     define the input plane yields a point and two spanning vectors
C     that define the distorted plane.
C
      DO I = 1, 3
 
         POINT(I)  =  DSTORT(I)  *  POINT(I)
         SPAN1(I)  =  DSTORT(I)  *  SPAN1(I)
         SPAN2(I)  =  DSTORT(I)  *  SPAN2(I)
 
      END DO
 
      CALL PSV2PL ( POINT,   SPAN1,  SPAN2,  DPLANE )
 
C
C     Step 2:
C
C     Find the intersection of the distorted plane and unit sphere.
C
 
C
C     The intersection of the distorted plane and the unit sphere
C     may be a circle, a point, or the empty set.  The distance of the
C     plane from the origin determines which type of intersection we
C     have.  If we represent the distorted plane by a unit normal
C     vector and constant, the size of the constant gives us the
C     distance of the plane from the origin.  If the distance is greater
C     than 1, the intersection of plane and unit sphere is empty. If
C     the distance is equal to 1, we have the tangency case.
C
C     The routine PL2PSV always gives us an output point that is the
C     closest point to the origin in the input plane.  This point is
C     the center of the intersection circle.  The spanning vectors
C     returned by PL2PSV, after we scale them by the radius of the
C     intersection circle, become an orthogonal pair of vectors that
C     extend from the center of the circle to the circle itself.  So,
C     the center and these scaled vectors define the intersection
C     circle.
C
      CALL PL2PSV  ( DPLANE, CENTER, VEC1, VEC2 )
 
      DIST = VNORM ( CENTER )
 
      IF ( DIST .GT. 1.0D0 ) THEN
 
         FOUND = .FALSE.
         CALL CHKOUT ( 'INEDPL' )
         RETURN
 
      END IF
 
C
C     Scale the generating vectors by the radius of the intersection
C     circle.
C
      RCIRCL =  DSQRT (  BRCKTD ( 1.0D0 - DIST**2,  0.0D0,  1.0D0  )  )
 
      CALL VSCLIP ( RCIRCL, VEC1 )
      CALL VSCLIP ( RCIRCL, VEC2 )
 
C
C     Step 3:
C
C     Apply the inverse distortion to the intersection circle to find
C     the actual intersection ellipse.
C
      DO I = 1, 3
 
         CENTER(I)  =  INVDST(I)  *  CENTER(I)
         VEC1(I)    =  INVDST(I)  *  VEC1(I)
         VEC2(I)    =  INVDST(I)  *  VEC2(I)
 
      END DO
C
C     Make an ellipse from the center and generating vectors.
C
      CALL CGV2EL ( CENTER, VEC1, VEC2, ELLIPS )
 
      FOUND  = .TRUE.
 
      CALL CHKOUT ( 'INEDPL' )
      RETURN
      END
