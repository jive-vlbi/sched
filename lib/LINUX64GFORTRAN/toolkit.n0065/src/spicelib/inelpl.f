C$Procedure      INELPL ( Intersection of ellipse and plane )
 
      SUBROUTINE INELPL ( ELLIPS, PLANE, NXPTS, XPT1, XPT2 )
 
C$ Abstract
C
C     Find the intersection of an ellipse and a plane.
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
C
C$ Declarations
 
      INTEGER               UBEL
      PARAMETER           ( UBEL = 9 )
 
      INTEGER               UBPL
      PARAMETER           ( UBPL = 4 )
 
      DOUBLE PRECISION      ELLIPS ( UBEL )
      DOUBLE PRECISION      PLANE  ( UBPL )
      INTEGER               NXPTS
      DOUBLE PRECISION      XPT1   ( 3 )
      DOUBLE PRECISION      XPT2   ( 3 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ELLIPS     I   A SPICELIB ellipse.
C     PLANE      I   A SPICELIB plane.
C     NXPTS      O   Number of intersection points of plane and ellipse.
C     XPT1,
C     XPT2       O   Intersection points.
C
C$ Detailed_Input
C
C     ELLIPS         is a SPICELIB ellipse. The ellipse is allowed to 
C                    be degenerate: one or both semi-axes may have
C                    zero length.
C
C     PLANE          is a SPICELIB plane.
C
C$ Detailed_Output
C
C     NXPTS          is the number of points of intersection of the
C                    geometric plane and ellipse represented by PLANE
C                    and ELLIPS. NXPTS may take the values 0, 1, 2 or
C                    -1.  The value -1 indicates that the ellipse
C                    consists of more than one point and lies in the
C                    plane, so the number of intersection points is
C                    infinite.
C
C                    When the ellipse consists of a single point and
C                    lies in the plane, NXPTS is set to 1.
C
C     XPT1,
C     XPT2           are the points of intersection of the input plane
C                    and ellipse. If there is only one intersection
C                    point, both XPT1 and XPT2 contain that point. If
C                    the number of intersection points is zero or
C                    infinite, the contents of XPT1 and XPT2 are
C                    undefined.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  The input plane must be a SPICE plane: the normal vector must
C         be non-zero and the constant must be non-negative.
C         If the input plane is invalid, the error SPICE(INVALIDPLANE)
C         will be signaled.
C
C     2)  If the input ellipse has non-orthogonal axes, the error 
C         SPICE(INVALIDELLIPSE) will be signaled.
C
C     3)  The input ellipse is allowed to be a line segment or a point;
C         these cases are not considered to be errors. If the ellipse
C         consists of a single point and lies in the plane, the number
C         of intersection points is set to 1 (rather than -1) and
C         the output arguments XPT1 and XPT2 are assigned the value
C         of the ellipse's center.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine computes the intersection set of a non-degenerate 
C     plane with a possibly degenerate ellipse. The ellipse is allowed
C     to consist of a line segment or a point.
C     
C     A plane may intersect an ellipse in 0, 1, 2, or infinitely many
C     points. For there to be an infinite set of intersection points,
C     the ellipse must lie in the plane and consist of more than one
C     point.
C
C$ Examples
C
C     1)  If we want to find the angle of some ray above the limb of an
C         ellipsoid, where the angle is measured in a plane containing
C         the ray and a `down' vector, we can follow the procedure
C         given below.  We assume the ray does not intersect the
C         ellipsoid.  The result we seek is called ANGLE, imaginatively
C         enough.
C
C         We assume that all vectors are given in body-fixed
C         coordinates.
C
C            C
C            C     Find the limb of the ellipsoid as seen from the
C            C     point OBSERV.  Here A, B, and C are the lengths of
C            C     the semi-axes of the ellipsoid.
C            C
C                  CALL EDLIMB ( A, B, C, OBSERV, LIMB )
C
C            C
C            C     The ray direction vector is RAYDIR, so the ray is the
C            C     set of points
C            C
C            C        OBSERV  +  t * RAYDIR
C            C
C            C     where t is any non-negative real number.
C            C
C            C     The `down' vector is just -OBSERV.  The vectors
C            C     OBSERV and RAYDIR are spanning vectors for the plane
C            C     we're interested in.  We can use PSV2PL to represent
C            C     this plane by a SPICELIB plane.
C            C
C                  CALL PSV2PL ( OBSERV, OBSERV, RAYDIR, PLANE )
C
C            C
C            C     Find the intersection of the plane defined by OBSERV
C            C     and RAYDIR with the limb.
C            C
C                  CALL INELPL ( LIMB, PLANE, NXPTS, XPT1, XPT2 )
C
C            C
C            C     We always expect two intersection points, if DOWN
C            C     is valid.
C            C
C                  IF ( NXPTS .LT. 2 ) THEN
C
C                     [ do something about the error ]
C
C                  ENDIF
C
C            C
C            C     Form the vectors from OBSERV to the intersection
C            C     points.  Find the angular separation between the
C            C     boresight ray and each vector from OBSERV to the
C            C     intersection points.
C            C
C                  CALL VSUB   ( XPT1, OBSERV, VEC1 )
C                  CALL VSUB   ( XPT2, OBSERV, VEC2 )
C
C                  SEP1 = VSEP ( VEC1, RAYDIR )
C                  SEP2 = VSEP ( VEC2, RAYDIR )
C
C            C
C            C     The angular separation we're after is the minimum of
C            C     the two separations we've computed.
C            C
C                  ANGLE = MIN ( SEP1, SEP2 )
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
C-    SPICELIB Version 3.0.0, 07-OCT-2011 (NJB)
C
C        Relaxed ellipse semi-axes orthogonality test limit
C        SEPLIM from 1.D-12 TO 1.D-9 radians. The angular
C        separation of the axes of the input ellipse must not
C        differ from pi/2 radians by more than this limit.
C
C-    SPICELIB Version 2.0.0, 14-JAN-2008 (NJB)
C
C        Bug fix: the routine's specification and behavior have been
C        updated so the routine now returns a meaningful result for the
C        case of an ellipse consisting of a single point.
C
C        Bug fix: in the degenerate case where the input ellipse is a
C        line segment of positive length, and this segment intersects
C        the plane, the number of intersection points is set to 1
C        rather than 2.
C
C        Invalid input planes and ellipses are now diagnosed.
C
C-    SPICELIB Version 1.2.0, 25-AUG-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VSUB call.
C
C-    SPICELIB Version 1.1.0, 24-MAR-1992 (NJB) (WLT)
C
C        Output arguments XPT1, XPT2 are now correctly declared
C        with length 3.  Comment section for permuted index source
C        lines was added following the header.
C
C-    SPICELIB Version 1.0.0, 02-NOV-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     intersection of ellipse and plane
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 14-JAN-2008 (NJB)
C
C        Bug fix: the routine's specification and behavior have been
C        updated so the routine now returns a meaningful result for the
C        case of an ellipse consisting of a single point. In this case,
C        if an intersection is found, the number of intersection points
C        is set to 1 and both intersection arguments are set equal to
C        the ellipse's center.
C
C        Bug fix: in the degenerate case where the input ellipse is a
C        line segment of positive length, and this segment intersects
C        the plane, the number of intersection points is set to 1
C        rather than 2.
C
C        Invalid input planes and ellipses are now diagnosed.
C        Error handling code has been added to trap errors that had
C        been erroneously passed off to lower level routines for
C        diagnosis.
C
C-    SPICELIB Version 1.2.0, 25-AUG-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VSUB call.
C
C-    SPICELIB Version 1.1.0, 24-MAR-1992 (NJB) (WLT)
C
C        Output arguments XPT1, XPT2 are now correctly declared
C        with length 3.  They formerly were declared as scalars.
C        The correction will not affect the behavior of the routine
C        in programs that already declared the correponding arguments
C        correctly.
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-&
 
C
C     SPICELIB functions
C     
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORMG
      DOUBLE PRECISION      VSEP

      LOGICAL               RETURN
      LOGICAL               VZERO
      LOGICAL               VZEROG
       
C
C     Local parameters
C     
      DOUBLE PRECISION      SEPLIM
      PARAMETER           ( SEPLIM = 1.D-9 )

C
C     Local variables
C
      DOUBLE PRECISION      ALPHA
      DOUBLE PRECISION      ANGLE1
      DOUBLE PRECISION      ANGLE2
      DOUBLE PRECISION      BETA
      DOUBLE PRECISION      CENTER (    3 )
      DOUBLE PRECISION      CONST
      DOUBLE PRECISION      INPCON
      DOUBLE PRECISION      NORMAL (    3 )
      DOUBLE PRECISION      POINT  (    3 )
      DOUBLE PRECISION      SEP
      DOUBLE PRECISION      SMAJOR (    3 )
      DOUBLE PRECISION      SMINOR (    3 )
      DOUBLE PRECISION      TMPVEC (    3 )
      DOUBLE PRECISION      TRANS  ( UBPL )
      DOUBLE PRECISION      V      (    2 )
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'INELPL' )

C
C     Check the input plane.
C
      CALL PL2NVC ( PLANE, NORMAL, INPCON )

      IF ( VZERO(NORMAL) ) THEN

         CALL SETMSG ( 'Input SPICE plane has zero normal vector.' )
         CALL SIGERR ( 'SPICE(INVALIDPLANE)'                       )
         CALL CHKOUT ( 'INELPL'                                    )
         RETURN

      ELSE IF ( INPCON .LT. 0.D0 ) THEN

         CALL SETMSG ( 'Input SPICE plane has non-positive '
     .   //            'constant #. Properly constructed '
     .   //            'SPICE planes always have non-negative '
     .   //            'constants.'                                )
         CALL ERRDP  ( '#',  INPCON                                )
         CALL SIGERR ( 'SPICE(INVALIDPLANE)'                       )
         CALL CHKOUT ( 'INELPL'                                    )
         RETURN

      END IF

C
C     Get the components of the input ellipse; check for 
C     invalid semi-axes. The semi-axes may have zero length
C     but they must always be orthogonal. We require this 
C     check only if both semi-axes have non-zero length.
C
      CALL EL2CGV ( ELLIPS, CENTER, SMAJOR, SMINOR )

      IF ( .NOT. VZERO(SMINOR) ) THEN

         SEP = VSEP( SMAJOR, SMINOR )

         IF (  ABS( SEP - HALFPI() )  .GT.  SEPLIM  ) THEN

            CALL SETMSG ( 'Input SPICE ellipse has non-orthogonal '
     .      //            'semi-axes: (#,#,#) and (#,#,#). Angular '
     .      //            'separation of these vectors is # radians. '
     .      //            'Properly constructed SPICE ellipses '
     .      //            'always have orthogonal semi-axes.'         )
            CALL ERRDP  ( '#',  SMAJOR(1)                             )
            CALL ERRDP  ( '#',  SMAJOR(2)                             )
            CALL ERRDP  ( '#',  SMAJOR(3)                             )
            CALL ERRDP  ( '#',  SMINOR(1)                             )
            CALL ERRDP  ( '#',  SMINOR(2)                             )
            CALL ERRDP  ( '#',  SMINOR(3)                             )
            CALL ERRDP  ( '#',  SEP                                   )
            CALL SIGERR ( 'SPICE(INVALIDELLIPSE)'                     )
            CALL CHKOUT ( 'INELPL'                                    )
            RETURN

         END IF

      END IF
      
C
C     If the input ellipse is a single point, decide now
C     whether the ellipse lies in the plane.
C
      IF ( VZERO(SMAJOR) ) THEN
C
C        The ellipse is a single point. If the ellipse's center
C        lies in the plane, the whole ellipse is the one
C        intersection point. Check the inner product of the
C        center and the plane's normal vector.
C
         IF (  VDOT( CENTER, NORMAL )  .EQ.  INPCON  ) THEN
C
C           The center does in fact lie in the plane.
C
            NXPTS = 1
            
            CALL VEQU ( CENTER, XPT1 )
            CALL VEQU ( CENTER, XPT2 )
 
         ELSE
C
C           There's no intersection: the intersection arguments
C           are left undefined in this case.
C
            NXPTS = 0

         END IF

C
C        Return now; this simplifies the logic to follow.
C         
         CALL CHKOUT ( 'INELPL' )
         RETURN

      END IF

C
C     At this point the ellipse may still be degenerate: it can be a 
C     line segment. We'll need to compute the intersection point or
C     points if we have a positive, finite intersection set.
C
C     The first thing we want to do is translate the plane and the
C     ellipse so as to center the ellipse at the origin.  To translate
C     the plane, just get a point and normal vector, and translate
C     the point.  Find the plane constant of the translated plane.
C 
      CALL PL2NVP ( PLANE,   NORMAL,  TMPVEC )
      CALL VSUB   ( TMPVEC,  CENTER,  POINT  )
      CALL NVP2PL ( NORMAL,  POINT,   TRANS  )
 
      CALL PL2NVC ( TRANS,   NORMAL,  CONST  )
 
C
C     Ok, we can get to work.  The locus of the ellipse is
C
C        cos(theta) SMAJOR  +  sin(theta) SMINOR,
C
C     and any point X of the ellipse that intersects the input plane
C     satisfies
C
C        < X, NORMAL >  =  CONST.
C
C     Substituting our expression for points on the ellipse into the
C     second equation, we arrive at
C
C           cos(theta) < SMAJOR, NORMAL >
C        +  sin(theta) < SMINOR, NORMAL >   =  CONST.        (1)
C
C     This equation merits a little analysis. First, if NORMAL
C     is orthogonal to SMAJOR and SMINOR, the plane and ellipse must
C     be parallel. Also, the left side of the equation is zero in
C     this case. If CONST is non-zero, there are no solutions:
C     the ellipse and plane are parallel but do not intersect. If
C     CONST is zero, the ellipse lies in the plane: all values of
C     theta are solutions. Let's get this case out of the way
C     right now, shall we?
C
      V(1) = VDOT ( SMAJOR, NORMAL )
      V(2) = VDOT ( SMINOR, NORMAL )
 
C
C     Test whether the plane and ellipse are parallel:
C
      IF (  VZEROG( V, 2 )  ) THEN
C
C        The ellipse lies in the plane if and only if CONST is zero.
C        In any case, we don't modify XPT1 or XPT2.
C
         IF ( CONST .EQ. 0.D0 ) THEN
            NXPTS = -1
         ELSE
            NXPTS = 0
         END IF
 
         CALL CHKOUT ( 'INELPL' )
         RETURN
 
      END IF
 
C
C     Now if NORMAL is not orthogonal to both SMAJOR and SMINOR,
C     the vector
C
C        V = (  < SMAJOR, NORMAL >,  < SMINOR, NORMAL >  )
C
C     is non-zero.  We can re-write (1) as
C
C        < U, V >  =  CONST,
C
C     where
C
C        U = ( cos(theta), sin(theta) ).
C
C     If alpha is the angle between U and V, we have
C
C        < U, V >  =  || U ||  *  || V ||  *  cos(alpha),
C
C     so
C
C        || V ||  *  cos(alpha)  =  CONST.                   (2)
C
C     CONST is positive, since PL2NVC returns the distance
C     of between its input plane and the origin as the output
C     plane constant.
C
C     Equation (2) has solutions if and only if
C
C        || V ||  >    CONST.                                (3)
C                 -
C
C     Let's return right now if there are no solutions.
C
      IF (  VNORMG( V, 2 )  .LT.  CONST  )   THEN
 
         NXPTS = 0
         CALL CHKOUT ( 'INELPL' )
         RETURN
 
      END IF
 
C
C     Since (3) above is satisfied, the plane and ellipse intersect.
C     We can find alpha using the formula
C
C        alpha  =  +  arccos (  CONST  /  || V ||  )
C
C     Since alpha is the angular separation between U and V, we
C     can find U once we have the angular position of V; let's
C     call that beta.  The angular position of U (which we called
C     theta earlier) will be
C
C        theta   =   beta  +  alpha.
C                          -
C
C     The values of theta are the angles we seek.
C
 
      ALPHA   =  ACOS  (  CONST  /  VNORMG ( V, 2 )  )
 
      BETA    =  ATAN2 ( V(2), V(1) )
 
      ANGLE1  =  BETA - ALPHA
      ANGLE2  =  BETA + ALPHA
 
C
C     Determine the number of intersection points. We have a special
C     case if the semi-minor axis has length zero: in that case BETA is
C     zero or Pi, and although ANGLE1 and ANGLE2 may differ, the
C     cosines of these angles are identical. Since in this case
C     the solutions corresponding to ANGLE1 and ANGLE2 have the
C     form
C
C        CENTER + cos(ANGLE1)*SMAJOR
C        CENTER + cos(ANGLE2)*SMAJOR
C
C     the solutions are identical.
C
C
      IF ( VZERO(SMINOR) ) THEN

         NXPTS = 1

      ELSE

         IF ( ANGLE1 .EQ. ANGLE2 ) THEN
C
C           This case occurs when ALPHA is zero.
C
            NXPTS = 1
         ELSE
            NXPTS = 2
         END IF

      END IF

C
C     Compute the intersection points.
C
      CALL VLCOM3 ( 1.0D0,         CENTER,
     .              DCOS(ANGLE1),  SMAJOR,
     .              DSIN(ANGLE1),  SMINOR,    XPT1 )
 
      CALL VLCOM3 ( 1.0D0,         CENTER,
     .              DCOS(ANGLE2),  SMAJOR,
     .              DSIN(ANGLE2),  SMINOR,    XPT2 )
 
 
      CALL CHKOUT ( 'INELPL' )
      RETURN
      END
