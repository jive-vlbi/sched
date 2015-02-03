C$Procedure      SURFPT ( Surface point on an ellipsoid )
 
      SUBROUTINE SURFPT ( POSITN, U, A, B, C, POINT, FOUND )

      IMPLICIT NONE
 
C$ Abstract
C
C     Determine the intersection of a line-of-sight vector with the
C     surface of an ellipsoid.
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
C     ELLIPSOID,  GEOMETRY
C
C$ Declarations
 
      DOUBLE PRECISION    POSITN ( 3 )
      DOUBLE PRECISION    U      ( 3 )
      DOUBLE PRECISION    A
      DOUBLE PRECISION    B
      DOUBLE PRECISION    C
      DOUBLE PRECISION    POINT  ( 3 )
      LOGICAL             FOUND
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     POSITN     I   Position of the observer in body-fixed frame.
C     U          I   Vector from the observer in some direction.
C     A          I   Length of ellipsoid semi-axis along the x-axis.
C     B          I   Length of ellipsoid semi-axis along the y-axis.
C     C          I   Length of ellipsoid semi-axis along the z-axis.
C     POINT      O   Point on the ellipsoid pointed to by U.
C     FOUND      O   Flag indicating if U points at the ellipsoid.
C
C$ Detailed_Input
C
C     POSITN     3-vector giving the position of an observer with
C                respect to the center of an ellipsoid. The vector is
C                expressed in a body-fixed reference frame. The
C                semi-axes of the ellipsoid are aligned with the x, y,
C                and z-axes of the body-fixed frame.
C
C     U          Pointing vector emanating from the observer.
C
C     A          Length of the semi-axis of the ellipsoid that is
C                parallel to the x-axis of the body-fixed reference
C                frame.
C
C     B          Length of the semi-axis of the ellipsoid that is
C                parallel to the y-axis of the body-fixed reference
C                frame.
C
C     C          Length of the semi-axis of the ellipsoid that is
C                parallel to the z-axis of the body-fixed reference
C                frame.
C
C$ Detailed_Output
C
C     POINT      If the ray with direction vector U emanating from
C                POSITN intersects the ellipsoid, POINT will be
C                returned with the body-fixed coordinates of the point
C                where the ray first meets the ellipsoid.  Otherwise,
C                POINT will be returned as (0, 0, 0).
C
C     FOUND      A logical flag indicating whether or not the ray from
C                POSITN with direction U actually intersects the
C                ellipsoid.  If the ray does intersect the ellipsoid,
C                FOUND will be returned as .TRUE. If the ray misses the
C                ellipsoid, FOUND will be returned as .FALSE.
C
C$ Parameters
C
C      None.
C
C$ Particulars
C
C      This routine assumes that an ellipsoid having semi-axes of
C      length A, B and C is given.  Moreover, it is assumed that these
C      axes are parallel to the x-, y-, and z-axes of a reference frame
C      whose origin is the geometric center of the ellipsoid---this is
C      called the body-fixed reference frame. 
C
C$ Examples
C
C      A typical use of SURFPT would be to obtain the planetocentric
C      coordinates of the point at which the optic axis of a
C      spacecraft-mounted instrument intersects the surface of a target
C      body, given the following items.
C
C         1) The epoch (ET) of observation, and the inertial
C            pointing (VPNT) of the instrument at this epoch.
C
C         2) The apparent position (VTARG) of the center of the
C            target body as seen from the spacecraft at the epoch
C            of observation, and the one-way light time (TAU)
C            from the target to the spacecraft.
C
C      In order to find the point of intersection, the following
C      items are also needed.
C
C         3) The transformation (TIBF) from inertial
C            to body-fixed coordinates at epoch ET-TAU.
C
C         4) The radii (R) of the tri-axial ellipsoid
C            used to model the target body.
C
C      These may be obtained from the kernel pool via calls to PXFORM
C      and BODVRD or BODVCD respectively.
C
C      The position of the observer is just the negative of the
C      spacecraft-target vector, VTARG, computed using the VMINUS
C      module. (Note that this is NOT the same as the apparent position
C      of the spacecraft as seen from the target!) Both vectors must be
C      specified in the body-fixed reference frame. The point of
C      intersection is found as follows:
C
C          CALL VMINUS ( VTARG, VPOS )
C          CALL MXV    ( TIBF,  VPOS,  VPOS )
C          CALL MXV    ( TIBF,  VPNT,  VPNT )
C
C          CALL SURFPT ( VPOS, VPNT, R(1), R(2), R(3), VSURF, FOUND )
C
C      Note that VSURF may or may not be a point of intersection,
C      depending on whether FOUND is .TRUE. or .FALSE. Note also that
C      VSURF is a vector from the center to the surface of the
C      target, in body-fixed coordinates, which may be converted
C      directly to planetocentric latitude, longitude, and radius:
C
C          CALL RECLAT ( VSURF, RADIUS, LONG, LAT )
C
C      To get the inertial vector from the spacecraft to the
C      surface point, you must subtract VPOS from VSURF, and rotate
C      the resulting vector back to inertial coordinates:
C
C          CALL VSUB ( VSURF, VPOS,  VSURF )
C          CALL MTXV ( TIBF,  VSURF, VSURF )
C
C
C$ Restrictions
C
C     None.
C
C$ Exceptions
C
C     1) If the input vector is the zero vector, the error
C        SPICE(ZEROVECTOR) is signaled.
C
C     2) If any of the body's axes is zero, the error
C        SPICE(BADAXISLENGTH) is signaled.
C
C$ Files
C
C     None.
C
C$ Author_and_Institution
C
C     C.H. Acton     (JPL)
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.3.0, 03-APR-2006 (NJB)
C
C        Bug fix:  intercept point is now always set to the
C        ray's vertex when the vertex is on the ellipsoid's 
C        surface.  This routine now uses discovery check-in.
C
C-    SPICELIB Version 1.2.2, 24-OCT-2005 (NJB)
C
C        Updated header to refer to BODVRD and BODVCD instead of
C        BODVAR.
C
C-    SPICELIB Version 1.2.1, 27-JUL-2003 (NJB) (CHA)
C
C        Various header corrections were made.  The example program
C        was upgraded to use real kernels, and the program's output is
C        shown.
C
C-    SPICELIB Version 1.2.0, 28-NOV-2002 (NJB)
C
C        Re-implemented intercept computation to reduce loss of
C        precision.
C
C        Changed SAVE statement to save only the error message.
C        Previously all local variables were saved.
C
C-    SPICELIB Version 1.1.0, 07-AUG-1996 (WLT)
C
C        Added a SAVE statement so that the error message will
C        not be lost between separate invocations of the routine.
C         
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
C
C-&
 
C$ Index_Entries
C
C     line of sight intercept with body
C     point of intersection between ray and ellipsoid
C     surface point of intersection of ray and ellipsoid
C
C-&
 
C$ Revisions
C
C-     SPICELIB Version 1.2.0, 28-NOV-2002 (NJB)
C
C         Re-implemented intercept computation to reduce loss of
C         precision.  New algorithm maps input ellipsoid to unit
C         sphere, finds closest point on input ray to the origin,
C         then finds the offset from this point to the surface.
C
C-     Beta Version 2.0.0, 9-JAN-1988 (WLT)
C
C      Short error message 'SPICE(ZEROAXISLENGTH)' changed to
C      'SPICE(BADAXISLENGTH)'
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM

      LOGICAL               RETURN
      LOGICAL               VZERO
 
C
C     Local parameters
C
      INTEGER               MSGDIM
      PARAMETER           ( MSGDIM = 7 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 32 )

C
C     Local variables
C
      CHARACTER*(MSGLEN)    MSSG  ( MSGDIM )

      DOUBLE PRECISION      P     ( 3 )
      DOUBLE PRECISION      PMAG
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SIGN
      DOUBLE PRECISION      UX    ( 3 )
      DOUBLE PRECISION      X     ( 3 )
      DOUBLE PRECISION      Y     ( 3 )
      DOUBLE PRECISION      YMAG
      DOUBLE PRECISION      YPROJ ( 3 )

      INTEGER               BAD

C
C     Saved variables
C
      SAVE                  MSSG
 
C
C     Initial values
C
      DATA                  MSSG  / 'Axis A was nonpositive.',
     .                              'Axis B was nonpositive.',
     .                              'Axes A and B were nonpositive.',
     .                              'Axis C was nonpositive.',
     .                              'Axes A and C were nonpositive.',
     .                              'Axes B and C were nonpositive.',
     .                              'All three axes were nonpositive.' /
 
 
C
C     Use discovery check-in.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
C
C     Check the input vector to see if its the zero vector. If it is
C     signal an error and return.
C
      IF  ( VZERO(U) )  THEN

         CALL CHKIN  ( 'SURFPT'                                       )
         CALL SETMSG ( 'SURFPT: The input vector is the zero vector.' )
         CALL SIGERR ( 'SPICE(ZEROVECTOR)'                            )
         CALL CHKOUT ( 'SURFPT'                                       )
         RETURN

      END IF
 
C
C     Check the axis to make sure that none of them is less than or
C     equal to zero. If one is, signal an error and return.
C
      BAD = 0
 
      IF ( A .LE. 0 )
     .   BAD = BAD + 1
 
      IF ( B .LE. 0 )
     .   BAD = BAD + 2
 
      IF ( C .LE. 0 )
     .   BAD = BAD + 4
 
 
      IF ( BAD .GT. 0 ) THEN

         CALL CHKIN  ( 'SURFPT'                           )
         CALL SETMSG ( MSSG (BAD)                         //
     .                 ' ? '                              )
         CALL ERRCH  ( ' ? ', 'The A,B, and C axes were ' //
     .                        '#, #, and # respectively.' )
 
         CALL ERRDP  ( '#', A )
         CALL ERRDP  ( '#', B )
         CALL ERRDP  ( '#', C )
 
         CALL SIGERR ( 'SPICE(BADAXISLENGTH)' )
         CALL CHKOUT ( 'SURFPT' )
         RETURN

      END IF
  
C
C     We're done with the error checks.  Set the outputs to the 
C     appropriate values for the "no intersection" case.
C
      FOUND = .FALSE.

      CALL CLEARD ( 3, POINT )

C
C     Apply a linear transformation to the point, direction vector,
C     and ellipsoid to transform the problem to one having the unit
C     sphere as the target ellipsoid.  (The transformation of the
C     ellipsoid is implicit.)
C
      X(1)  = U(1) / A
      X(2)  = U(2) / B
      X(3)  = U(3) / C
 
      Y(1)  = POSITN(1) / A
      Y(2)  = POSITN(2) / B
      Y(3)  = POSITN(3) / C

 
C
C     Find the component P of Y (the ray's vertex) orthogonal to X
C     (the ray's direction).
C
      CALL VPERP ( Y, X, P )

C
C     Find the component of Y parallel to X.
C
      CALL VSUB ( Y, P, YPROJ )

C
C     Find the magnitudes of Y and P.
C
      YMAG = VNORM(Y)
      PMAG = VNORM(P)

C
C     Get a unitized copy of X.
C
      CALL VHAT ( X, UX )

C
C     Now determine whether there's an intersection.  Consider
C     the case where Y is outside the sphere first.
C
      IF ( YMAG .GT. 1 ) THEN
C
C        If P is outside of the sphere, there can be no intersection.
C
         IF ( PMAG .GT. 1.D0 ) THEN
            RETURN
         END IF         

C
C        If X points in the same direction as YPROJ, then the ray
C        is pointing away from the sphere, and there is no
C        intersection.
C
         IF (  VDOT(YPROJ, X) .GT. 0.D0 ) THEN
            RETURN
         END IF

C
C        At this point we know there's an intersection.
C
         IF ( PMAG .EQ. 1.D0 ) THEN
C
C           The vector P we've found is the singleton point of 
C           intersection.  All we have to do is transform P by
C           applying the inverse of our original linear transformation.
C
            POINT(1) = P(1) * A
            POINT(2) = P(2) * B
            POINT(3) = P(3) * C

            FOUND    = .TRUE.
            RETURN
 
         END IF         

C
C        At this point we know there's a non-trivial intersection.
C 
C        Set the sign of the coefficient of UX (a unitized copy
C        of X) that will be used to compute the intercept point.
C        In this case the coefficient of UX has negative sign because 
C        the vector we're adding to P points toward Y. 
C
         SIGN = -1.D0


      ELSE IF ( YMAG .EQ. 1 ) THEN
C
C        The ray's vertex is on the surface of the ellipsoid.
C        The vertex is the first point of intersection.
C
         CALL VEQU ( POSITN, POINT )
         
         FOUND = .TRUE.
         RETURN
         
      ELSE
C
C        Y is inside the sphere, so there's definitely an intersection.
C        In this case, the intercept is obtained by adding a positive
C        multiple of UX to P.
C 
         SIGN =  1.D0

      END IF

C
C
C     We have a small amount of work to do:  we'll find the multiple
C     of X that when added to P yields the desired intercept point.
C 
C     The magnitude of the half-chord connecting P and the surface 
C     is just 
C             ____________
C           \/ 1 - PMAG**2
C
C
      SCALE = SQRT (  MAX ( 0.D0, 1-PMAG*PMAG )  )

C
C     Find the intercept point on the unit sphere.
C
      CALL VLCOM (  1.D0,  P,  SIGN * SCALE,  UX,  POINT  )

C
C     Undo our linear transformation.
C        
      POINT(1) = POINT(1) * A
      POINT(2) = POINT(2) * B
      POINT(3) = POINT(3) * C

      FOUND    = .TRUE.

      RETURN
      END
