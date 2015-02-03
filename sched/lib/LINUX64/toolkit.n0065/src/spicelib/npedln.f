C$Procedure    NPEDLN ( Nearest point on ellipsoid to line )
 
      SUBROUTINE NPEDLN ( A, B, C, LINEPT, LINEDR, PNEAR, DIST )
 
C$ Abstract
C
C     Find nearest point on a triaxial ellipsoid to a specified line,
C     and the distance from the ellipsoid to the line.
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
      DOUBLE PRECISION      LINEPT ( 3 )
      DOUBLE PRECISION      LINEDR ( 3 )
      DOUBLE PRECISION      PNEAR  ( 3 )
      DOUBLE PRECISION      DIST
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     A          I   Length of ellipsoid's semi-axis in the x direction
C     B          I   Length of ellipsoid's semi-axis in the y direction
C     C          I   Length of ellipsoid's semi-axis in the z direction
C     LINEPT     I   Point on line
C     LINEDR     I   Direction vector of line
C     PNEAR      O   Nearest point on ellipsoid to line
C     DIST       O   Distance of ellipsoid from line
C     UBEL       P   Upper bound of array containing SPICELIB ellipse.
C     UBPL       P   Upper bound of array containing SPICELIB plane.
C
C$ Detailed_Input
C
C     A,
C     B,
C     C              are the lengths of the semi-axes of a triaxial
C                    ellipsoid which is centered at the origin and
C                    oriented so that its axes lie on the x-, y- and
C                    z- coordinate axes.  A, B, and C are the lengths of
C                    the semi-axes that point in the x, y, and z
C                    directions respectively.
C
C     LINEPT
C     LINEDR         are, respectively, a point and a direction vector
C                    that define a line.  The line is the set of vectors
C
C                       LINEPT   +   t * LINEDR
C
C                    where t is any real number.
C
C$ Detailed_Output
C
C     PNEAR          is the point on the ellipsoid that is closest to
C                    the line, if the line doesn't intersect the
C                    ellipsoid.
C
C                    If the line intersects the ellipsoid, PNEAR will
C                    be a point of intersection.  If LINEPT is outside
C                    of the ellipsoid, PNEAR will be the closest point
C                    of intersection.  If LINEPT is inside the
C                    ellipsoid, PNEAR will not necessarily be the
C                    closest point of intersection.
C
C
C     DIST           is the distance of the line from the ellipsoid.
C                    This is the minimum distance between any point on
C                    the line and any point on the ellipsoid.
C
C                    If the line intersects the ellipsoid, DIST is zero.
C
C$ Parameters
C
C     UBEL           is the upper bound of the array used to contain
C                    a SPICELIB ellipse.  See the ELLIPSES Required
C                    Reading for details.
C
C     UBPL           is the upper bound of the array used to contain
C                    a SPICELIB plane.  See the PLANES Required Reading
C                    for details.
C
C$ Exceptions
C
C     If this routine detects an error, the output arguments NEARP and
C     DIST are not modified.
C
C     1)  If the length of any semi-axis of the ellipsoid is
C         non-positive, the error SPICE(INVALIDAXISLENGTH) is signalled.
C
C     2)  If the line's direction vector is the zero vector, the error
C         SPICE(ZEROVECTOR) is signalled.
C
C     3)  If the length of any semi-axis of the ellipsoid is zero after
C         the semi-axis lengths are scaled by the reciprocal of the
C         magnitude of the longest semi-axis and then squared, the error
C         SPICE(DEGENERATECASE) is signalled.
C
C     4)  If the input ellipsoid is extremely flat or needle-shaped
C         and has its shortest axis close to perpendicular to the input
C         line, numerical problems could cause this routine's algorithm
C         to fail, in which case the error SPICE(DEGENERATECASE) is
C         signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     For any ellipsoid and line, if the line does not intersect the
C     ellipsoid, there is a unique point on the ellipsoid that is
C     closest to the line.  Therefore, the distance DIST between
C     ellipsoid and line is well-defined.  The unique line segment of
C     length DIST that connects the line and ellipsoid is normal to
C     both of these objects at its endpoints.
C
C     If the line intersects the ellipsoid, the distance between the
C     line and ellipsoid is zero.
C
C$ Examples
C
C     1)   We can find the distance between an instrument optic axis ray
C          and the surface of a body modelled as a tri-axial ellipsoid
C          using this routine.  If the instrument position and pointing
C          unit vector in body-fixed coordinates are
C
C             LINEPT = ( 1.0D6,  2.0D6,  3.0D6 )
C
C          and
C
C             LINEDR = ( -4.472091234D-1
C                        -8.944182469D-1,
C                        -4.472091234D-3  )
C
C          and the body semi-axes lengths are
C
C             A = 7.0D5
C             B = 7.0D5
C             C = 6.0D5,
C
C          then the call to NPEDLN
C
C             CALL NPEDLN ( A,      B,      C,
C            .              LINEPT, LINEDR,
C            .              PNEAR,  DIST        )
C
C          yields a value for PNEAR, the nearest point on the body to
C          the optic axis ray, of
C
C
C             (  -1.6333110792340931E+03,
C                -3.2666222157820771E+03,
C                 5.9999183350006724E+05  )
C
C          and a value for DIST, the distance to the ray, of
C
C             2.3899679338299707E+06
C
C          (These results were obtained on a PC-Linux system under g77.)
C
C          In some cases, it may not be clear that the closest point
C          on the line containing an instrument boresight ray is on
C          the boresight ray itself; the point may lie on the ray
C          having the same vertex as the boresight ray and pointing in
C          the opposite direction.  To rule out this possibility, we
C          can make the following test:
C
C             C
C             C     Find the difference vector between the closest point
C             C     on the ellpsoid to the line containing the boresight
C             C     ray and the boresight ray's vertex.  Find the
C             C     angular separation between this difference vector
C             C     and the boresight ray.  If the angular separation
C             C     does not exceed pi/2, we have the nominal geometry.
C             C     Otherwise, we have an error.
C             C
C                   CALL  VSUB ( PNEAR,  LINEPT,  DIFF )
C                   SEP = VSEP ( DIFF,   LINEDR        )
C
C                   IF (  SEP .LE. HALFPI()  ) THEN
C
C                      [ perform normal processing ]
C
C                   ELSE
C
C                      [ handle error case ]
C
C                   END IF
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
C-    SPICELIB Version 1.3.0, 15-NOV-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VSCL calls.  Changed exponents to DOUBLE PRECISION type
C        in the test for underflow of squared, scaled axis lengths.
C
C-    SPICELIB Version 1.2.1, 06-DEC-2002 (NJB)
C
C        Outputs shown in header example have been corrected to 
C        be consistent with those produced by this routine.
C
C-    SPICELIB Version 1.2.0, 25-NOV-1992 (NJB)
C
C        Bug fix:  in the intercept case, PNEAR is now properly
C        re-scaled prior to output.  Also, an error in the $Examples
C        section was corrected.
C
C-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.1.0, 04-DEC-1990 (NJB)
C
C        Error message and description changed for non-positive
C        axis length error.
C
C-    SPICELIB Version 1.0.0, 02-NOV-1990 (NJB)
C
C-&
 
 
 
C$ Index_Entries
C
C     distance between line and ellipsoid
C     distance between line of sight and body
C     nearest point on ellipsoid to line
C
C-&
 
 
 
C$ Revisions
C
C-    SPICELIB Version 1.3.0, 15-NOV-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VSCL calls.  Changed exponents to DOUBLE PRECISION type
C        in the test for underflow of squared, scaled axis lengths.
C
C-    SPICELIB Version 1.2.0, 25-NOV-1992 (NJB)
C
C        Bug fix:  in the intercept case, PNEAR is now properly
C        re-scaled prior to output.  Formerly, it was returned without
C        having been re-scaled.
C
C        Also, an error in the $Examples section was corrected:  the
C        line
C
C           CALL  VSUB ( LINEPT,  PNEAR,  DIFF )
C
C        was replaced by
C
C           CALL  VSUB ( PNEAR,  LINEPT,  DIFF )
C
C        The in-line comments were re-arranged slightly, and the claim
C        that the inverse orthogonal projection of PRJNPT is guaranteed
C        to exist was removed. (The check for this exception was already
C        being done.)
C
C
C-    SPICELIB Version 1.1.0, 04-DEC-1990 (NJB)
C
C        Error message and description changed for non-positive
C        axis length error.  The former message and description did
C        not match, and the description was incorrect:  it described
C        `zero-length', rather than `non-positive' axes as invalid.
C
C-&
 
 
C
C     SPICELIB functions
C     
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local variables
C
      DOUBLE PRECISION      CANDPL ( UBPL )
      DOUBLE PRECISION      CAND   ( UBEL )
      DOUBLE PRECISION      OPPDIR (    3 )
      DOUBLE PRECISION      PRJPL  ( UBPL )
      DOUBLE PRECISION      MAG
      DOUBLE PRECISION      NORMAL (    3 )
      DOUBLE PRECISION      PRJEL  ( UBEL )
      DOUBLE PRECISION      PRJPT  (    3 )
      DOUBLE PRECISION      PRJNPT (    3 )
      DOUBLE PRECISION      PT     ( 3, 2 )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SCLA
      DOUBLE PRECISION      SCLB
      DOUBLE PRECISION      SCLC
      DOUBLE PRECISION      SCLPT  (    3 )
      DOUBLE PRECISION      UDIR   (    3 )
 
      INTEGER               I
 
      LOGICAL               FOUND  (    2 )
      LOGICAL               IFOUND
      LOGICAL               XFOUND
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NPEDLN' )
      END IF
 
C
C     The algorithm used in this routine has two parts.  The first
C     part handles the case where the input line and ellipsoid
C     intersect.  Our procedure is simple in that case; we just
C     call SURFPT twice, passing it first one ray determined by the
C     input line, then a ray pointing in the opposite direction.
C     The second part of the algorithm handles the case where SURFPT
C     doesn't find an intersection.
C
C     Finding the nearest point on the ellipsoid to the line, when the
C     two do not intersect, is a matter of following four steps:
C
C     1)  Find the points on the ellipsoid where the surface normal
C         is normal to the line's direction.  This set of points is
C         an ellipse centered at the origin.  The point we seek MUST
C         lie on this `candidate' ellipse.
C
C     2)  Project the candidate ellipse onto a plane that is normal
C         to the line's direction.  This projection preserves
C         distance from the line; the nearest point to the line on
C         this new ellipse is the projection of the nearest point to
C         the line on the candidate ellipse, and these two points are
C         exactly the same distance from the line.  If computed using
C         infinite-precision arithmetic, this projection would be
C         guaranteed to be non-degenerate as long as the input
C         ellipsoid were non-degenerate.  This can be verified by
C         taking the inner product of the scaled normal to the candidate
C         ellipse plane and the line's unitized direction vector
C         (these vectors are called NORMAL and UDIR in the code below);
C         the inner product is strictly greater than 1 if the ellipsoid
C         is non-degenerate.
C
C     3)  The nearest point on the line to the projected ellipse will
C         be contained in the plane onto which the projection is done;
C         we find this point and then find the nearest point to it on
C         the projected ellipse.  The distance between these two points
C         is the distance between the line and the ellipsoid.
C
C     4)  Finally, we find the point on the candidate ellipse that was
C         projected to the nearest point to the line on the projected
C         ellipse that was found in step 3.  This is the nearest point
C         on the ellipsoid to the line.
C
C
C
C
C                      Glossary of Geometric Variables
C
C
C            A,
C            B,
C            C           Input ellipsoid's semi-axis lengths.
C
C            POINT       Point of intersection of line and ellipsoid
C                        if the intersection is non-empty.
C
C            CANDPL      Plane containing candidate ellipse.
C
C            NORMAL      Normal vector to the candidate plane CANDPL.
C
C            CAND        Candidate ellipse.
C
C            LINEPT,
C            LINEDR,     Point and direction vector on input line.
C
C            UDIR        Unitized line direction vector.
C
C            PRJPL       Projection plane; the candidate ellipse is
C                        projected onto this plane to yield PRJEL.
C
C            PRJEL       Projection of the candidate ellipse CAND onto
C                        the projection plane PRJEL.
C
C            PRJPT       Projection of line point.
C
C            PRJNPT      Nearest point on projected ellipse to
C                        projection of line point.
C
C            PNEAR       Nearest point on ellipsoid to line.
C
C
 
C
C     We need a valid normal vector.
C
      CALL UNORM ( LINEDR, UDIR, MAG )
 
      IF ( MAG .EQ. 0 ) THEN
 
         CALL SETMSG( 'Line direction vector is the zero vector. ' )
         CALL SIGERR( 'SPICE(ZEROVECTOR)'                          )
         CALL CHKOUT( 'NPEDLN'                                     )
         RETURN
 
C
C     The ellipsoid's semi-axes must have positive length.
C
      ELSE IF (         ( A .LE. 0.D0 )
     .           .OR.   ( B .LE. 0.D0 )
     .           .OR.   ( C .LE. 0.D0 )   )   THEN
 
         CALL SETMSG ( 'Semi-axes: A = #,  B = #,  C = #.'  )
         CALL ERRDP  ( '#', A                               )
         CALL ERRDP  ( '#', B                               )
         CALL ERRDP  ( '#', C                               )
         CALL SIGERR ( 'SPICE(INVALIDAXISLENGTH)'           )
         CALL CHKOUT ( 'NPEDLN'                             )
         RETURN
 
      END IF
 
C
C     Scale the semi-axes lengths for better numerical behavior.
C     If squaring any one of the scaled lengths causes it to
C     underflow to zero, we have an error.  Otherwise, scale the
C     point on the input line too.
C
      SCALE  =  MAX ( DABS(A), DABS(B), DABS(C) )
 
      SCLA   =  A / SCALE
      SCLB   =  B / SCALE
      SCLC   =  C / SCALE
 
      IF (       ( SCLA**2.D0  .EQ.  0.D0 )
     .     .OR.  ( SCLB**2.D0  .EQ.  0.D0 )
     .     .OR.  ( SCLC**2.D0  .EQ.  0.D0 )   )   THEN
 
         CALL SETMSG ( 'Semi-axis too small:  A = #, B = #, C = #. ' )
         CALL ERRDP  ( '#', A                                        )
         CALL ERRDP  ( '#', B                                        )
         CALL ERRDP  ( '#', C                                        )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'                       )
         CALL CHKOUT ( 'NPEDLN'                                      )
         RETURN
 
      END IF
 
C
C     Scale LINEPT.  Because SCALE might be a very small number, 
C     we avoid computing 1/SCALE; that's why we don't call VSCL here.
C
      SCLPT(1)  =  LINEPT(1) / SCALE
      SCLPT(2)  =  LINEPT(2) / SCALE
      SCLPT(3)  =  LINEPT(3) / SCALE
 
C
C     Hand off the intersection case to SURFPT.  SURFPT determines
C     whether rays intersect a body, so we treat the line as a pair
C     of rays.
C
      CALL VMINUS ( UDIR, OPPDIR )
 
      CALL SURFPT ( SCLPT, UDIR,   SCLA, SCLB, SCLC, PT(1,1), FOUND(1) )
      CALL SURFPT ( SCLPT, OPPDIR, SCLA, SCLB, SCLC, PT(1,2), FOUND(2) )
 
      DO I = 1, 2
 
         IF ( FOUND(I) ) THEN
 
            DIST  =  0.0D0
 
            CALL VSCL   ( SCALE,    PT(1,I),  PNEAR )
            CALL CHKOUT ( 'NPEDLN'                  )
            RETURN
 
         END IF
 
      END DO
 
C
C     Getting here means the line doesn't intersect the ellipsoid.
C
C     Find the candidate ellipse CAND.  NORMAL is a normal vector to
C     the plane containing the candidate ellipse.   Mathematically the
C     ellipse must exist, since it's the intersection of an ellipsoid
C     centered at the origin and a plane containing the origin.  Only
C     numerical problems can prevent the intersection from being found.
C
C
      NORMAL(1)  =  UDIR(1) / SCLA**2
      NORMAL(2)  =  UDIR(2) / SCLB**2
      NORMAL(3)  =  UDIR(3) / SCLC**2
 
      CALL NVC2PL ( NORMAL, 0.D0, CANDPL )
 
      CALL INEDPL ( SCLA, SCLB, SCLC, CANDPL, CAND, XFOUND )
 
      IF ( .NOT. XFOUND ) THEN
         CALL SETMSG ( 'Candidate ellipse could not be found.'  )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'                  )
         CALL CHKOUT ( 'NPEDLN'                                 )
         RETURN
      END IF
 
C
C     Project the candidate ellipse onto a plane orthogonal to the
C     line.  We'll call the plane PRJPL and the projected ellipse PRJEL.
C
      CALL NVC2PL ( UDIR,  0.D0,   PRJPL )
      CALL PJELPL ( CAND,  PRJPL,  PRJEL )
 
C
C     Find the point on the line lying in the projection plane, and
C     then find the near point PRJNPT on the projected ellipse.  Here
C     PRJPT is the point on the line lying in the projection plane.
C     The distance between PRJPT and PRJNPT is DIST.
C
C
      CALL VPRJP  ( SCLPT,   PRJPL,  PRJPT         )
      CALL NPELPT ( PRJPT,   PRJEL,  PRJNPT,  DIST )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'NPEDLN' )
         RETURN
      END IF
 
C
C     Find the near point PNEAR on the ellipsoid by taking the inverse
C     orthogonal projection of PRJNPT; this is the point on the
C     candidate ellipse that projects to PRJNPT.  Note that the
C     output DIST was computed in step 3 and needs only to be re-scaled.
C
C     The inverse projection of PNEAR ought to exist, but may not
C     be calculable due to numerical problems (this can only happen
C     when the input ellipsoid is extremely flat or needle-shaped).
C
      CALL VPRJPI ( PRJNPT, PRJPL, CANDPL, PNEAR, IFOUND )
 
      IF ( .NOT. IFOUND ) THEN
         CALL SETMSG ( 'Inverse projection could not be found.'  )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'                   )
         CALL CHKOUT ( 'NPEDLN'                                  )
         RETURN
      END IF
 
C
C     Undo the scaling.
C
      CALL VSCLIP ( SCALE, PNEAR )
 
      DIST = SCALE * DIST
 
 
      CALL CHKOUT ( 'NPEDLN' )
      RETURN
      END
