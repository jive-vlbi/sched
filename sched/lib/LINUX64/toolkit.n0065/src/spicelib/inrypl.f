C$Procedure      INRYPL ( Intersection of ray and plane )
 
      SUBROUTINE INRYPL ( VERTEX, DIR, PLANE, NXPTS, XPT )
 
C$ Abstract
C
C     Find the intersection of a ray and a plane.
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
C
C$ Declarations
 
      INTEGER               UBPL
      PARAMETER           ( UBPL    =   4 )
 
      DOUBLE PRECISION      VERTEX (    3 )
      DOUBLE PRECISION      DIR    (    3 )
      DOUBLE PRECISION      PLANE  ( UBPL )
      INTEGER               NXPTS
      DOUBLE PRECISION      XPT    (    3 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     VERTEX,
C     DIR        I   Vertex and direction vector of ray.
C     PLANE      I   A SPICELIB plane.
C     NXPTS      O   Number of intersection points of ray and plane.
C     XPT        O   Intersection point, if NXPTS = 1.
C
C$ Detailed_Input
C
C     VERTEX,
C     DIR            are a point and direction vector that define a
C                    ray in three-dimensional space.
C
C     PLANE          is a SPICELIB plane.
C
C$ Detailed_Output
C
C     NXPTS          is the number of points of intersection of the
C                    input ray and plane.  Values and meanings of
C                    NXPTS are:
C
C                       0     No intersection.
C
C                       1     One point of intersection.  Note that
C                             this case may occur when the ray's
C                             vertex is in the plane.
C
C                      -1     An infinite number of points of
C                             intersection; the ray lies in the plane.
C
C
C     XPT            is the point of intersection of the input ray
C                    and plane, when there is exactly one point of
C                    intersection.  Otherwise, XPT is the zero vector.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the ray's direction vector is the zero vector, the error
C         SPICE(ZEROVECTOR) is signaled.  NXPTS and XPT are not
C         modified.
C
C
C     2)  If the ray's vertex is further than DPMAX() / 3 from the
C         origin, the error SPICE(VECTORTOOBIG) is signaled.  NXPTS
C         and XPT are not modified.
C
C
C     3)  If the input plane is s further than DPMAX() / 3 from the
C         origin, the error SPICE(VECTORTOOBIG) is signaled.  NXPTS
C         and XPT are not modified.
C
C
C     4)  The input plane should be created by one of the SPICELIB
C         routines
C
C            NVC2PL
C            NVP2PL
C            PSV2PL
C
C         Invalid input planes will cause unpredictable results.
C
C
C     5)  In the interest of good numerical behavior, in the case
C         where the ray's vertex is not in the plane, this routine
C         considers that an intersection of the ray and plane occurs
C         only if the distance between the ray's vertex and the
C         intersection point is less than DPMAX() / 3.
C
C         If VERTEX is not in the plane and this condition is not
C         met, then NXPTS is set to 0 and XPT is set to the zero
C         vector.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The intersection of a ray and plane in three-dimensional space
C     can be a the empty set, a single point, or the ray itself.
C
C$ Examples
C
C     1)  Find the camera projection of the center of an extended
C         body.  For simplicity, we assume:
C
C            -- The camera has no distortion;  the image of a point
C               is determined by the intersection of the focal plane
C               and the line determined by the point and the camera's
C               focal point.
C
C            -- The camera's pointing matrix (C-matrix) is available
C               in a C-kernel.
C
C
C            C
C            C     Load Leapseconds and SCLK kernels to support time
C            C     conversion.
C            C
C                  CALL FURNSH ( 'LEAP.KER' )
C                  CALL FURNSH ( 'SCLK.KER' )
C
C            C
C            C     Load an SPK file containing ephemeris data for
C            C     observer (a spacecraft, whose NAIF integer code
C            C     is SC) and target at the UTC epoch of observation.
C            C
C                  CALL FURNSH ( 'SPK.BSP' )
C
C            C
C            C     Load a C-kernel containing camera pointing for
C            C     the UTC epoch of observation.
C            C
C                  CALL FURNSH ( 'CK.BC' )
C
C            C
C            C     Find the ephemeris time (barycentric dynamical time)
C            C     and encoded spacecraft clock times corresponding to
C            C     the UTC epoch of observation.
C            C
C                  CALL UTC2ET ( UTC, ET          )
C                  CALL SCE2C  ( SC,  ET,  SCLKDP )
C
C            C
C            C     Encode the pointing lookup tolerance.
C            C
C                  CALL SCTIKS ( SC, TOLCH,  TOLDP  )
C
C            C
C            C     Find the observer-target vector at the observation
C            C     epoch.  In this example, we'll use a light-time
C            C     corrected state vector.
C            C
C                  CALL SPKEZ ( TARGET,  ET,  'J2000',  'LT',  SC,
C                 .             STATE,   LT                        )
C
C            C
C            C     Look up camera pointing.
C            C
C                  CALL CKGP  ( CAMERA, SCLKDP, TOLDP, 'J2000', CMAT,
C                 .             CLKOUT, FOUND                        )
C            
C                  IF ( .NOT. FOUND ) THEN
C
C                     [Handle this case...]
C 
C                  END IF
C
C            C
C            C     Negate the spacecraft-to-target body vector and
C            C     convert it to camera coordinates.
C            C
C                  CALL VMINUS ( STATE, DIR       )
C                  CALL MXV    ( CMAT,  DIR,  DIR )
C
C            C
C            C     If FL is the camera's focal length, the effective
C            C     focal point is
C            C
C            C        FL * ( 0, 0, 1 )
C            C
C                  CALL VSCL ( FL, ZVEC, FOCUS )
C
C            C
C            C     The camera's focal plane contains the origin in
C            C     camera coordinates, and the z-vector is orthogonal
C            C     to the plane.  Make a SPICELIB plane representing
C            C     the focal plane.
C            C
C                  CALL NVC2PL ( ZVEC, 0.D0, FPLANE )
C
C            C
C            C     The image of the target body's center in the focal
C            C     plane is defined by the intersection with the focal
C            C     plane of the ray whose vertex is the focal point and
C            C     whose direction is DIR.
C            C
C                  CALL INRYPL ( FOCUS, DIR, FPLANE, NXPTS, IMAGE )
C
C                  IF ( NXPTS .EQ. 1 ) THEN
C            C
C            C        The body center does project to the focal plane.
C            C        Check whether the image is actually in the
C            C        camera's field of view...
C            C
C                               .
C                               .
C                               .
C                  ELSE
C
C            C
C            C        The body center does not map to the focal plane.
C            C        Handle this case...
C            C
C                               .
C                               .
C                               .
C                  END IF
C
C
C
C     2)  Find the Saturn ring plane intercept of a spacecraft-mounted
C         instrument's boresight vector.  We want the find the point
C         in the ring plane that will be observed by an instrument
C         with a give boresight direction at a specified time.  We
C         must account for light time and stellar aberration in order
C         to find this point.  The intercept point will be expressed
C         in Saturn body-fixed coordinates.
C          
C         In this example, we assume
C
C            -- The ring plane is equatorial.
C
C            -- Light travels in a straight line.
C
C            -- The light time correction for the ring plane intercept
C               can be obtained by performing three light-time
C               correction iterations.  If this assumption does not
C               lead to a sufficiently accurate result, additional
C               iterations can be performed.
C
C            -- A Newtonian approximation of stellar aberration
C               suffices.
C
C            -- The boresight vector is given in J2000 coordinates.
C
C            -- The observation epoch is ET ephemeris seconds past
C               J2000.
C
C            -- The boresight vector, spacecraft and planetary
C               ephemerides, and ring plane orientation are all known
C               with sufficient accuracy for the application.
C
C            -- All necessary kernels are loaded by the caller of
C               this example routine.
C
C
C            SUBROUTINE RING_XPT ( SC, ET, BORVEC, SBFXPT, FOUND )
C            IMPLICIT NONE
C
C            CHARACTER*(*)         SC
C            DOUBLE PRECISION      ET
C            DOUBLE PRECISION      BORVEC ( 3 )
C            DOUBLE PRECISION      SBFXPT ( 3 )
C            LOGICAL               FOUND
C
C      C
C      C     SPICELIB functions
C      C
C            DOUBLE PRECISION      CLIGHT
C            DOUBLE PRECISION      VDIST
C
C      C
C      C     Local parameters
C      C
C            INTEGER               UBPL
C            PARAMETER           ( UBPL = 4 )
C
C            INTEGER               SATURN
C            PARAMETER           ( SATURN = 699 )
C
C      C
C      C     Local variables
C      C
C            DOUBLE PRECISION      BORV2  ( 3 )
C            DOUBLE PRECISION      CORVEC ( 3 )
C            DOUBLE PRECISION      LT
C            DOUBLE PRECISION      PLANE  ( UBPL )
C            DOUBLE PRECISION      SATSSB ( 6 )
C            DOUBLE PRECISION      SCPOS  ( 3 )
C            DOUBLE PRECISION      SCSSB  ( 6 )
C            DOUBLE PRECISION      STATE  ( 6 )
C            DOUBLE PRECISION      STCORR ( 3 )
C            DOUBLE PRECISION      TAU
C            DOUBLE PRECISION      TPMI   ( 3,  3 )
C            DOUBLE PRECISION      XPT    ( 3 )
C            DOUBLE PRECISION      ZVEC   ( 3 )
C
C            INTEGER               I
C            INTEGER               NXPTS
C            INTEGER               SCID
C
C            LOGICAL               FND
C
C      C
C      C     First step:  account for stellar aberration.  Since the
C      C     instrument pointing is given, we need to find the intercept
C      C     point such that, when the stellar aberration correction is
C      C     applied to the vector from the spacecraft to that point,
C      C     the resulting vector is parallel to BORVEC.  An easy 
C      C     solution is to apply the inverse of the normal stellar 
C      C     aberration correction to BORVEC, and then solve the 
C      C     intercept problem with this corrected boresight vector.
C      C
C      C     Find the position of the observer relative
C      C     to the solar system barycenter at ET.
C      C
C            CALL BODN2C ( SC, SCID, FND )
C
C            IF ( .NOT. FND ) THEN
C
C               CALL SETMSG ( 'ID code for body # was not found.' )
C               CALL ERRCH  ( '#',  SC                            )
C               CALL SIGERR ( 'SPICE(NOTRANSLATION'               )
C               RETURN
C
C            END IF
C
C            CALL SPKSSB ( SCID, ET, 'J2000', SCSSB )
C
C      C
C      C     We now wish to find the vector CORVEC that, when 
C      C     corrected for stellar aberration, yields BORVEC.  
C      C     A good first approximation is obtained by applying 
C      C     the stellar aberration correction for transmission 
C      C     to BORVEC.
C      C
C            CALL STLABX ( BORVEC, SCSSB(4), CORVEC )
C
C      C
C      C     The inverse of the stellar aberration correction
C      C     applicable to CORVEC should be a very good estimate of
C      C     the correction we need to apply to BORVEC.  Apply
C      C     this correction to BORVEC to obtain an improved estimate
C      C     of CORVEC.
C      C
C            CALL STELAB ( CORVEC, SCSSB(4), BORV2  )
C            CALL VSUB   ( BORV2,  CORVEC,   STCORR )
C            CALL VSUB   ( BORVEC, STCORR,   CORVEC )
C
C      C
C      C     Because the ring plane intercept may be quite far from
C      C     Saturn's center, we cannot assume light time from the 
C      C     intercept to the observer is well approximated by 
C      C     light time from Saturn's center to the observer.  
C      C     We compute the light time explicitly using an iterative 
C      C     approach.
C      C
C      C     We can however use the light time from Saturn's center to
C      C     the observer to obtain a first estimate of the actual light
C      C     time.
C      C
C            CALL SPKEZR ( 'SATURN', ET, 'J2000', 'LT', SC,
C           .               STATE,   LT                       )
C            TAU = LT
C
C      C
C      C     Find the ring plane intercept and calculate the
C      C     light time from it to the spacecraft.
C      C     Perform three iterations.
C      C
C            I     = 1
C            FOUND = .TRUE.
C
C            DO WHILE (  ( I .LE. 3 )  .AND.  ( FOUND )  )
C      C
C      C        Find the position of Saturn relative
C      C        to the solar system barycenter at ET-TAU.
C      C
C               CALL SPKSSB ( SATURN, ET-TAU, 'J2000', SATSSB )
C
C      C
C      C        Find the Saturn-to-observer vector defined by these
C      C        two position vectors.
C      C
C               CALL VSUB ( SCSSB, SATSSB, SCPOS )
C
C      C
C      C        Look up Saturn's pole at ET-TAU; this is the third
C      C        column of the matrix that transforms Saturn body-fixed
C      C        coordinates to J2000 coordinates.
C      C
C               CALL PXFORM ( 'IAU_SATURN', 'J2000', ET-TAU, TPMI )
C
C               CALL MOVED  ( TPMI(1,3), 3, ZVEC )
C
C      C
C      C        Make a SPICELIB plane representing the ring plane.
C      C        We're treating Saturn's center as the origin, so
C      C        the plane constant is 0.
C      C
C               CALL NVC2PL ( ZVEC, 0.D0, PLANE )
C
C      C
C      C        Find the intersection of the ring plane and the
C      C        ray having vertex SCPOS and direction vector
C      C        CORVEC.
C      C
C               CALL INRYPL ( SCPOS, CORVEC, PLANE, NXPTS, XPT )
C
C      C
C      C        If the number of intersection points is 1,
C      C        find the next light time estimate.
C      C
C               IF ( NXPTS .EQ. 1 ) THEN
C      C
C      C           Find the light time (zero-order) from the
C      C           intercept point to the spacecraft.
C      C
C                  TAU  =  VDIST ( SCPOS, XPT )  /  CLIGHT()
C                  I    =  I + 1
C
C               ELSE
C
C                  FOUND = .FALSE.
C
C               END IF
C
C            END DO
C
C      C
C      C     At this point, if FOUND is .TRUE., we iterated
C      C     3 times, and XPT is our estimate of the
C      C     position of the ring plane intercept point
C      C     relative to Saturn in the J2000 frame.  This is the
C      C     point observed by an instrument pointed in direction
C      C     BORVEC at ET at mounted on the spacecraft SC.
C      C
C      C     If FOUND is .FALSE., the boresight ray does not
C      C     intersect the ring plane.
C      C
C      C     As a final step, transform XPT to Saturn body-fixed
C      C     coordinates.
C      C
C            IF ( FOUND ) THEN
C
C               CALL MTXV ( TPMI, XPT, SBFXPT )
C
C            END IF
C
C            END
C
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
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.1, 07-FEB-2008 (BVS)
C
C        Fixed a few typos in the header.
C
C-    SPICELIB Version 1.1.0, 02-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VSCL call.
C
C-    SPICELIB Version 1.0.3, 12-DEC-2002 (NJB)
C
C        Header fix:  ring plane intercept algorithm was corrected. 
C        Now light time is computed accurately, and stellar aberration 
C        is accounted for.  Example was turned into a complete
C        subroutine.
C
C-    SPICELIB Version 1.0.2, 09-MAR-1999 (NJB)
C
C        Reference to SCE2T replaced by reference to SCE2C.  An 
C        occurrence of ENDIF was replaced by END IF.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 01-APR-1991 (NJB) (WLT)
C
C-&
 
C$ Index_Entries
C
C     intersection of ray and plane
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 02-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VSCL call.
C
C-& 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM
 
      LOGICAL               RETURN
      LOGICAL               SMSGND
      LOGICAL               VZERO
 
C
C     Local parameters
C
      DOUBLE PRECISION      MARGIN
      PARAMETER           ( MARGIN = 3.D0 )
 
C
C     Local variables
C
      DOUBLE PRECISION      CONST
      DOUBLE PRECISION      PRJDIF
      DOUBLE PRECISION      PRJDIR
      DOUBLE PRECISION      PRJVN
      DOUBLE PRECISION      MSCALE
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SCLCON
      DOUBLE PRECISION      SCLVTX ( 3 )
      DOUBLE PRECISION      TOOBIG
      DOUBLE PRECISION      UDIR   ( 3 )
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'INRYPL' )
      END IF
 
C
C     We'll give the name TOOBIG to the bound DPMAX() / MARGIN.
C     If we let VTXPRJ be the orthogonal projection of VERTEX onto
C     PLANE, and let DIFF be the vector VTXPRJ - VERTEX, then
C     we know that
C
C        ||  DIFF  ||    <     2 * TOOBIG
C
C     Check the distance of the ray's vertex from the origin.
C
      TOOBIG = DPMAX() / MARGIN
 
      IF (  VNORM (VERTEX)  .GE.  TOOBIG  )  THEN
 
         CALL SETMSG ( 'Ray''s vertex is too far from the origin.' )
         CALL SIGERR ( 'SPICE(VECTORTOOBIG)'                       )
         CALL CHKOUT ( 'INRYPL'                                    )
         RETURN
 
      END IF
 
C
C     Check the distance of the plane from the origin.  (The returned
C     plane constant IS this distance.)
C
      CALL PL2NVC ( PLANE, NORMAL, CONST )
 
      IF (  CONST  .GE.  TOOBIG  )  THEN
 
         CALL SETMSG ( 'Plane is too far from the origin.' )
         CALL SIGERR ( 'SPICE(VECTORTOOBIG)'               )
         CALL CHKOUT ( 'INRYPL'                            )
         RETURN
 
      END IF
 
C
C     Check the ray's direction vector.
C
      CALL VHAT ( DIR, UDIR )
 
      IF ( VZERO (UDIR) ) THEN
 
         CALL SETMSG ( 'Ray''s direction vector is the zero vector.' )
         CALL SIGERR ( 'SPICE(ZEROVECTOR)'                           )
         CALL CHKOUT ( 'INRYPL'                                      )
         RETURN
 
      END IF
 
C
C     That takes care of the error cases.  Now scale the input vertex
C     and plane to improve numerical behavior.
C
      MSCALE = MAX ( CONST, VNORM(VERTEX) )
 
      IF ( MSCALE .NE. 0.D0 ) THEN
 
         CALL VSCL ( 1.D0 / MSCALE, VERTEX, SCLVTX )
         SCLCON  =  CONST / MSCALE
 
      ELSE
 
         CALL VEQU ( VERTEX, SCLVTX )
         SCLCON   =  CONST
 
      END IF
 
      IF ( MSCALE .GT. 1.D0 ) THEN
         TOOBIG = TOOBIG / MSCALE
      END IF
 
 
C     Find the projection (coefficient) of the ray's vertex along the
C     plane's normal direction.
C
      PRJVN = VDOT ( SCLVTX, NORMAL )
 
C
C     If this projection is the plane constant, the ray's vertex lies in
C     the plane.  We have one intersection or an infinite number of
C     intersections.  It all depends on whether the ray actually lies
C     in the plane.
C
C     The absolute value of PRJDIF is the distance of the ray's vertex
C     from the plane.
C
      PRJDIF  =  SCLCON - PRJVN
 
      IF ( PRJDIF .EQ. 0.D0 ) THEN
C
C        XPT is the original, unscaled vertex.
C
         CALL VEQU ( VERTEX, XPT )
 
         IF (  VDOT ( NORMAL, UDIR )  .EQ.  0.D0  )  THEN
C
C           The ray's in the plane.
C
            NXPTS  =  -1
         ELSE
            NXPTS  =   1
         END IF
 
         CALL CHKOUT ( 'INRYPL' )
         RETURN
 
      END IF
 
C
C     Ok, the ray's vertex is not in the plane.  The ray may still be
C     parallel to or may point away from the plane.  If the ray does
C     point towards the plane, mathematicians would say that the
C     ray does intersect the plane, but the computer may disagree.
C
C     For this routine to find an intersection, both of the following
C     conditions must be met:
C
C        -- The ray must point toward the plane; this happens when
C           PRJDIF has the same sign as < UDIR, NORMAL >.
C
C        -- The vector difference XPT - SCLVTX must not overflow.
C
C      Qualitatively, the case of interest looks something like the
C      picture below:
C
C
C                      * SCLVTX
C                      |\
C                      | \   <-- UDIR
C                      |  \
C    length of this    |   \|
C      segment is      |   -*
C                      |
C     | PRJDIF |   --> | ___________________________
C                      |/                          /
C                      |       *                  /   <-- PLANE
C                     /|        XPT              /
C                    / ^                        /
C                   /  | NORMAL                /
C                  /   | .                    /
C                 /    |/|                   /
C                / .---| /                  /
C               /  |   |/                  /
C              /   `---*                  /
C             /          Projection of SCLVTX onto the plane
C            /                          /
C           /                          /
C          ----------------------------
C
C
C
 
C
C     Find the projection of the direction vector along the plane's
C     normal vector.
C
      PRJDIR  =  VDOT ( UDIR, NORMAL )
 
C
C     We're done if the ray doesn't point toward the plane.  PRJDIF
C     has already been found to be non-zero at this point; PRJDIR is
C     zero if the ray and plane are parallel.  The SPICELIB routine
C     SMSGND will return a value of .FALSE. if PRJDIR is zero.
C
      IF (  .NOT. SMSGND (PRJDIR, PRJDIF)  )  THEN
C
C        The ray is parallel to or points away from the plane.
C
         NXPTS = 0
         CALL CLEARD ( 3, XPT )
 
         CALL CHKOUT ( 'INRYPL' )
         RETURN
 
      END IF
 
C
C     The difference XPT - SCLVTX is the hypotenuse of a right triangle
C     formed by SCLVTX, XPT, and the orthogonal projection of SCLVTX
C     onto the plane.  We'll obtain the hypotenuse by scaling UDIR.
C     We must make sure that this hypotenuse does not overflow.  The
C     scale factor has magnitude
C
C         | PRJDIF |
C       --------------
C         | PRJDIR |
C
C     and UDIR is a unit vector, so as long as
C
C         | PRJDIF |   <   | PRJDIR |  *  TOOBIG
C
C     the hypotenuse is no longer than TOOBIG.  The product can be
C     computed safely since PRJDIR has magnitude 1 or less.
C
 
      IF (  ABS(PRJDIF)  .GE.  ABS(PRJDIR) * TOOBIG  ) THEN
C
C        If the hypotenuse is too long, we say that no intersection
C        exists.
C
         NXPTS = 0
         CALL CLEARD ( 3, XPT )
 
         CALL CHKOUT ( 'INRYPL' )
         RETURN
 
      END IF
 
C
C     We conclude that it's safe to compute XPT.  Scale UDIR and add
C     the result to SCLVTX.  The addition is safe because both addends
C     have magnitude no larger than TOOBIG.  The vector thus obtained
C     is the intersection point.
C
      NXPTS    =   1
      SCALE    =   ABS (PRJDIF)  /   ABS (PRJDIR)
 
      CALL VLCOM ( 1.0D0, SCLVTX, SCALE, UDIR, XPT )
 
C
C     Re-scale XPT.  This is safe, since TOOBIG has already been
C     scaled to allow for any growth of XPT at this step.
C
      CALL VSCLIP ( MSCALE, XPT )

      CALL CHKOUT ( 'INRYPL' )
      RETURN
      END
