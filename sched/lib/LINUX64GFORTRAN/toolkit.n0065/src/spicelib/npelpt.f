C$Procedure   NPELPT  ( Nearest point on ellipse to point )
 
      SUBROUTINE NPELPT ( POINT, ELLIPS, PNEAR, DIST )
 
C$ Abstract
C
C     Find the nearest point on an ellipse to a specified point, both
C     in three-dimensional space, and find the distance between the
C     ellipse and the point.
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
C     CONIC
C     ELLIPSE
C     GEOMETRY
C     MATH
C
C$ Declarations
 
      INTEGER               UBEL
      PARAMETER           ( UBEL    =   9 )
 
      DOUBLE PRECISION      ELLIPS ( UBEL )
      DOUBLE PRECISION      POINT  (    3 )
      DOUBLE PRECISION      PNEAR  (    3 )
      DOUBLE PRECISION      DIST
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     POINT      I   Point whose distance to an ellipse is to be found.
C     ELLIPS     I   A SPICELIB ellipse.
C     PNEAR      O   Nearest point on ellipse to input point.
C     DIST       O   Distance of input point to ellipse.
C
C$ Detailed_Input
C
C     ELLIPS         is a SPICELIB ellipse that represents an ellipse
C                    in three-dimensional space.
C
C     POINT          is a point in 3-dimensional space.
C
C$ Detailed_Output
C
C     PNEAR          is the nearest point on ELLIPS to POINT.
C
C     DIST           is the distance between POINT and PNEAR.  This is
C                    the distance between POINT and the ellipse.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  Invalid ellipses will be diagnosed by routines called by
C         this routine.
C
C     2)  Ellipses having one or both semi-axis lengths equal to zero
C         are turned away at the door; the error SPICE(DEGENERATECASE)
C         is signalled.
C
C     3)  If the geometric ellipse represented by ELLIPS does not
C         have a unique point nearest to the input point, any point
C         at which the minimum distance is attained may be returned
C         in PNEAR.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Given an ellipse and a point in 3-dimensional space, if the
C     orthogonal projection of the point onto the plane of the ellipse
C     is on or outside of the ellipse, then there is a unique point on
C     the ellipse closest to the original point.  This routine finds
C     that nearest point on the ellipse.  If the projection falls inside
C     the ellipse, there may be multiple points on the ellipse that are
C     at the minimum distance from the original point.  In this case,
C     one such closest point will be returned.
C
C     This routine returns a distance, rather than an altitude, in
C     contrast to the SPICELIB routine NEARPT.  Because our ellipse is
C     situated in 3-space and not 2-space, the input point is not
C     `inside' or `outside' the ellipse, so the notion of altitude does
C     not apply to the problem solved by this routine.  In the case of
C     NEARPT, the input point is on, inside, or outside the ellipsoid,
C     so it makes sense to speak of its altitude.
C
C$ Examples
C
C     1)  For planetary rings that can be modelled as flat disks with
C         elliptical outer boundaries, the distance of a point in
C         space from a ring's outer boundary can be computed using this
C         routine.  Suppose CENTER, SMAJOR, and SMINOR are the center,
C         semi-major axis, and semi-minor axis of the ring's boundary.
C         Suppose also that SCPOS is the position of a spacecraft.
C         SCPOS, CENTER, SMAJOR, and SMINOR must all be expressed in
C         the same coordinate system.  We can find the distance from
C         the spacecraft to the ring using the code fragment
C
C            C
C            C     Make a SPICELIB ellipse representing the ring,
C            C     then use NPELPT to find the distance between
C            C     the spacecraft position and RING.
C            C
C                  CALL CGV2EL ( CENTER, SMAJOR, SMINOR, RING )
C                  CALL NPELPT ( SCPOS,  RING,   PNEAR,  DIST )
C
C
C     2)  The problem of finding the distance of a line from a tri-axial
C         ellipsoid can be reduced to the problem of finding the
C         distance between the same line and an ellipse; this problem in
C         turn can be reduced to the problem of finding the distance
C         between an ellipse and a point.  The routine NPEDLN carries
C         out this process and uses NPELPT to find the ellipse-to-point
C         distance.
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
C-    SPICELIB Version 1.2.0, 02-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VADD, VSCL, MTXV and MXV calls.
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
C     nearest point on ellipse to point
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.2.0, 02-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VADD, VSCL, MTXV and MXV calls.
C
C-& 

 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VDIST
 
      LOGICAL               RETURN
 
C
C     Local variables
C
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      MAJLEN
      DOUBLE PRECISION      MINLEN
      DOUBLE PRECISION      ROTATE ( 3, 3 )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SMAJOR ( 3 )
      DOUBLE PRECISION      SMINOR ( 3 )
      DOUBLE PRECISION      TEMPV  ( 3 )
      DOUBLE PRECISION      TMPPNT ( 3 )
      DOUBLE PRECISION      PRJPNT ( 3 )
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NPELPT' )
      END IF
 
C
C     Here's an overview of our solution:
C
C        Let ELPL be the plane containing the ELLIPS, and let PRJ be
C        the orthogonal projection of the POINT onto ELPL.  Let X be
C        any point in the plane ELPL.  According to the Pythagorean
C        Theorem,
C
C                           2                       2                  2
C           || POINT - X ||    =   || POINT - PRJ ||   +  || PRJ - X ||.
C
C        Then if we can find a point X on ELLIPS that minimizes the
C        rightmost term, that point X is the closest point on the
C        ellipse to POINT.
C
C        So, we find the projection PRJ, and then solve the problem of
C        finding the closest point on ELLIPS to PRJ.  To solve this
C        problem, we find a triaxial ellipsoid whose intersection with
C        the plane ELPL is precisely ELLIPS, and two of whose axes lie
C        in the plane ELPL.  The closest point on ELLIPS to PRJ is also
C        the closest point on the ellipsoid to ELLIPS.  But we have the
C        SPICELIB routine NEARPT on hand to find the closest point on an
C        ellipsoid to a specified point, so we've reduced our problem to
C        a solved problem.
C
C        There is a subtle point to worry about here:  if PRJ is outside
C        of ELLIPS (PRJ is in the same plane as ELLIPS, so `outside'
C        does make sense here), then the closest point on ELLIPS to PRJ
C        coincides with the closest point on the ellipsoid to PRJ,
C        regardless of how we choose the z-semi-axis length of the
C        ellipsoid.  But the correspondence may be lost if PRJ is inside
C        the ellipse, if we don't choose the z-semi-axis length
C        correctly.
C
C        Though it takes some thought to verify this (and we won't prove
C        it here), making the z-semi-axis of the ellipsoid longer than
C        the other two semi-axes is sufficient to maintain the
C        coincidence of the closest point on the ellipsoid to PRJPNT and
C        the closest point on the ellipse to PRJPNT.
C
 
C
C     Find the ellipse's center and semi-axes.
C
      CALL EL2CGV ( ELLIPS, CENTER, SMAJOR, SMINOR )
 
C
C     Find the lengths of the semi-axes, and scale the vectors to try
C     to prevent arithmetic unpleasantness.  Degenerate ellipses are
C     turned away at the door.
C
      MINLEN = VNORM (SMINOR)
      MAJLEN = VNORM (SMAJOR)
 
      IF (   MIN ( MAJLEN, MINLEN )  .EQ.  0.D0  ) THEN
 
         CALL SETMSG ( 'Semi-axis lengths: # #. ' )
         CALL ERRDP  ( '#', MAJLEN                )
         CALL ERRDP  ( '#', MINLEN                )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'    )
         CALL CHKOUT ( 'NPELPT'                   )
         RETURN
 
      END IF
 
 
      SCALE = 1.D0 / MAJLEN
 
      CALL VSCLIP ( SCALE, SMAJOR )
      CALL VSCLIP ( SCALE, SMINOR )
 
C
C     Translate ellipse and point so that the ellipse is centered at
C     the origin.  Scale the point's coordinates to maintain the
C     correct relative position to the scaled ellipse.
C 
      CALL VSUB   ( POINT, CENTER, TMPPNT )
      CALL VSCLIP ( SCALE,         TMPPNT )

C
C     We want to reduce the problem to a two-dimensional one.  We'll
C     work in a coordinate system whose x- and y- axes are aligned with
C     the semi-major and semi-minor axes of the input ellipse.  The
C     z-axis is picked to give us a right-handed system.  We find the
C     matrix that transforms coordinates to our new system using TWOVEC.
C
      CALL TWOVEC ( SMAJOR, 1, SMINOR, 2, ROTATE )
 
C
C     Apply the coordinate transformation to our scaled input point.
C
      CALL MXV  ( ROTATE, TMPPNT, TEMPV  )
      CALL VEQU ( TEMPV,          TMPPNT )
 
C
C     We must find the distance between the orthogonal projection of
C     TMPPNT onto the x-y plane and the ellipse.  The projection is
C     just
C
C        ( TMPPNT(1), TMPPNT(2), 0 );
C
C     we'll call this projection PRJPNT.
C
C
      CALL VPACK ( TMPPNT(1),  TMPPNT(2),  0.D0,  PRJPNT )
 
C
C     Now we're ready to find the distance between and a triaxial
C     ellipsoid whose intersection with the x-y plane is the ellipse
C     and whose third semi-axis lies on the z-axis.
C
C     Because we've scaled the ellipse's axes so as to give the longer
C     axis length 1, a length of 2.D0 suffices for the ellipsoid's
C     z-semi-axis.
C
 
C
C     Find the nearest point to PRJPNT on the ellipoid, PNEAR.
C
      CALL NEARPT ( PRJPNT, 1.D0, MINLEN/MAJLEN, 2.D0, PNEAR, DIST )
 
C
C     Scale the near point coordinates back to the original scale.
C
      CALL VSCLIP ( MAJLEN, PNEAR )
 
C
C     Apply the required inverse rotation and translation to PNEAR.
C     Compute the point-to-ellipse distance.
C
      CALL MTXV ( ROTATE, PNEAR,  TEMPV )
      CALL VADD ( TEMPV,  CENTER, PNEAR )
 
      DIST = VDIST ( PNEAR, POINT )
 
 
      CALL CHKOUT  ( 'NPELPT' )
      RETURN
      END
