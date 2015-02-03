C$Procedure      ZZOCCED ( Occultation of ellipsoidal bodies )
 
      INTEGER FUNCTION ZZOCCED ( VIEWPT, 
     .                           CENTR1, SEMAX1, CENTR2, SEMAX2 )
 
C$ Abstract
C
C     Indicate whether one triaxial ellipsoid is occulted by another as
C     seen from a specified viewing location.
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
C     SPK
C     TIME
C
C$ Keywords
C
C     GEOMETRY
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'zzocced.inc'

      INTEGER               UBEL
      PARAMETER           ( UBEL   = 9 )

      INTEGER               UBPL
      PARAMETER           ( UBPL   = 4 )
 
      DOUBLE PRECISION      VIEWPT ( 3 )
      DOUBLE PRECISION      CENTR1 ( 3 )
      DOUBLE PRECISION      SEMAX1 ( 3, 3 )
      DOUBLE PRECISION      CENTR2 ( 3 )
      DOUBLE PRECISION      SEMAX2 ( 3, 3 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     UBEL       P   Upper bound of SPICELIB ellipse data structure.
C     UBPL       P   Upper bound of SPICELIB plane data structure.
C     VIEWPT     I   Observation location.
C     CENTR1     I   Center of first ellipsoid.
C     SEMAX1     I   Semi-major axis matrix for first ellipsoid.
C     CENTR2     I   Center of second ellipsoid.
C     SEMAX2     I   Semi-major axis matrix for second ellipsoid.
C
C     The function returns an integer code indicating the geometric
C     relationship of the three bodies.  Negative codes indicate that
C     the first target is partially or fully occulted by the second;
C     positive codes indicate that the second target is partially
C     or fully occulted by the first; a value of zero indicates no
C     occultation.
C
C     See Detailed_Output for the list of codes and meanings.
C
C$ Detailed_Input
C
C     VIEWPT         is a point from which a possible occultation of
C                    one ellipsoidal "target" body by another is
C                    observed. VIEWPT must be external to both target
C                    bodies.
C
C     CENTR1         is the center of the first ellipsoidal target
C                    body.
C
C     SEMAX1         is a 3x3 matrix whose columns are semi-axis
C                    vectors of the first ellipsoid.  The columns of
C                    SEMAX1 must form a right-handed, orthogonal basis:
C                    the columns are mutually orthogonal, and the third
C                    column points in the direction of the cross
C                    product of the first and second.  In other words,
C                    if the columns were scaled to unit length, the
C                    matrix would be orthogonal.
C
C                    The lengths of the column vectors are the lengths
C                    of the ellipsoid's semi-axes.  It is not necessary
C                    that the longest semi-axis appear in the first
C                    column.
C
C                    An example:  if the first ellipsoid is described
C                    by the equation
C
C                         2       2       2
C                        x       y       z   
C                       ---  +  ---  +  ---   =   1
C                         2       2       2
C                        a       b       c
C
C                    then a corresponding semi-axis matrix would
C                    be
C
C                       +-          -+
C                       |  a   0   0 |
C                       |  0   b   0 |
C                       |  0   0   c |
C                       +-          -+
C
C                    A second example of a valid semi-axis matrix is
C
C                       +-          -+
C                       |  0  -a   0 |
C                       |  0   0  -b |
C                       |  c   0   0 |
C                       +-          -+
C
C
C     CENTR2         is the center of the second ellipsoidal target
C                    body.
C
C
C     SEMAX2         is a semi-axis matrix for the second ellipsoidal
C                    target body.  See the description of SEMAX1 for
C                    details.
C                    
C
C$ Detailed_Output
C
C     The function returns an integer code indicating the geometric
C     relationship of the three bodies.   
C
C     Codes and meanings are:
C
C        TOTAL1                Total occultation of first target by 
C                              second.
C
C        ANNLR1                Annular occultation of first target by 
C                              second.  The second target does not
C                              block the limb of the first.
C
C        PARTL1                Partial occultation of first target by
C                              second target.
C
C        NOOCC                 No occultation or transit:  both objects
C                              are completely visible to the observer.
C
C        PARTL2                Partial occultation of second target by
C                              first target.
C
C        ANNLR2                Annular occultation of second target by 
C                              first.
C
C        TOTAL2                Total occultation of second target by 
C                              first.
C
C$ Parameters
C
C     UBEL           Upper bound of SPICELIB ellipse data structure.
C
C     UBPL           Upper bound of SPICELIB plane data structure.
C
C$ Exceptions
C
C     1)  If the observer is inside either target ellipsoid, the error
C         SPICE(NOTDISJOINT) is signaled.
C
C     2)  If this routine determines that the target bodies intersect,
C         the error SPICE(NOTDISJOINT) is signaled.  
C
C     3)  If any of the semi-axis lengths of either ellipsoid is
C         non-positive, the error SPICE(BADAXISLENGTH) is signaled.
C
C     4)  If either semi-axis matrix does not have a right-handed,
C         mutually orthogonal set of columns, the error 
C         SPICE(NOTAROTATION) will be signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     For many purposes, modeling extended bodies as tri-axial
C     ellipsoids is adequate for determining whether one body is
C     occulted by another as seen from a specified observer.
C
C     This routine may be used as a tool to support more higher-level
C     occultation tests involving ephemeris objects.
C     
C$ Examples
C
C     1)  View a total occultation of one ellipsoid by another
C         as seen from a viewing location on the +x axis.
C
C                 PROGRAM EX1
C                 IMPLICIT NONE
C
C           C
C           C     SPICELIB functions
C           C
C                 INTEGER ZZOCCED
C
C           C
C           C     Local variables
C           C
C                 DOUBLE PRECISION      CENTR1 ( 3 )
C                 DOUBLE PRECISION      CENTR2 ( 3 )
C                 DOUBLE PRECISION      SEMAX1 ( 3, 3 )
C                 DOUBLE PRECISION      SEMAX2 ( 3, 3 )
C                 DOUBLE PRECISION      VIEWPT ( 3 )
C
C                 INTEGER               CODE
C
C           C
C           C     Initial values
C           C
C                 DATA VIEWPT /    2.D1, 0.D0, 0.D0 /
C
C                 DATA CENTR1 /    1.D1, 0.D0, 0.D0 /
C             
C                 DATA SEMAX1 /    1.D0, 0.D0, 0.D0,
C                .                 0.D0, 5.D0, 0.D0,
C                .                 0.D0, 0.D0, 1.D1 /
C
C                 DATA CENTR2 /   -1.D1, 0.D0, 0.D0 /
C
C                 DATA SEMAX2 /    2.D0, 0.D0, 0.D0,
C                .                 0.D0, 1.D1, 0.D0,
C                .                 0.D0, 0.D0, 2.D1 /
C
C           C
C           C     Find the occultation state and write out the
C           C     occultation code.  We don't place the ZZOCCED
C           C     call directly in the WRITE statement because
C           C     ZZOCCED can signal errors; an error signaled in
C           C     an I/O statement would cause recursive I/O.
C           C 
C                 CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, 
C                .                         CENTR2, SEMAX2 )
C
C                 WRITE (*,*), 'CODE = ', CODE
C                 END
C
C     We expect that the smaller ellipsoid, listed first in the call to
C     ZZOCCED, completely occults the larger, so the return code should
C     be 3.
C
C
C$ Restrictions
C
C     1) The test done by this routine for intersection of target bodies
C        may return a false negative result.  The test is based on
C        finding an intersection of spheres inscribed in each target
C        body.
C
C        Correct application code should never exercise this test.
C
C     2) This routine relies on ZZASRYEL to determine the minimum and
C        maximum angular separation of a specified ray and ellipse. In
C        some unusual cases in which multiple extreme values are very
C        close, or in which the extrema occur at points very close
C        together on the ellipse, ZZASRYEL may locate the incorrect
C        extremum.  This can result in erroneous classification of a
C        partial occultation as a total occultation or annular transit.
C          
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
C-    SPICELIB Version 1.1.0, 17-MAR-2006 (NJB)
C
C        Bug fixes:  
C
C           - Test for intersection of viewpoint with targets
C             was corrected.  Previous test did not properly account
C             for target orientation.
C
C           - Computation of maximum bounding cones of targets
C             failed when viewing point was inside either maximum 
C             bounding sphere.  The algorithm now has a separate
C             branch to handle this situation.
C
C           - Computation of minimum bounding cone for target was
C             incorrect for the computation done after transformation
C             of the targets.  This computation has been corrected.
C
C-    SPICELIB Version 1.0.0, 17-AUG-2005 (NJB)
C
C-&
 
C$ Index_Entries
C
C     occultation test using ellipsoidal bodies
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      DASINE
      DOUBLE PRECISION      DET
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP

      LOGICAL               FAILED
      LOGICAL               ISROT
      LOGICAL               RETURN
      
C
C     Local parameters
C
C
C     Tolerance value for determinant of a rotation matrix.  The
C     determinant must differ from 1 by no more than DTOL.  
C
      DOUBLE PRECISION      DTOL
      PARAMETER           ( DTOL  = 1.D-12 )

C
C     Tolerance value for norms of columns of a rotation matrix.  The
C     norms must differ from 1 by no more than NTOL.  
C
      DOUBLE PRECISION      NTOL
      PARAMETER           ( NTOL  = 1.D-14 )

C
C     Tolerance value for argument of arcsine.  The argument should
C     have absolute value no greater than 1 + ATOL.
C
      DOUBLE PRECISION      ATOL
      PARAMETER           ( ATOL  = 1.D-12 )

C
C     Local variables
C
      DOUBLE PRECISION      ANGCMP
      DOUBLE PRECISION      BIGCTR ( 3 )
      DOUBLE PRECISION      BIGR
      DOUBLE PRECISION      CTRS   ( 3, 2 )
      DOUBLE PRECISION      DIST   ( 2 )
      DOUBLE PRECISION      INVRAY ( 3 )
      DOUBLE PRECISION      LEVEL
      DOUBLE PRECISION      LIMB   ( UBEL )
      DOUBLE PRECISION      LMBCTR ( 3 )
      DOUBLE PRECISION      LMBMAJ ( 3 )
      DOUBLE PRECISION      LMBMIN ( 3 )
      DOUBLE PRECISION      LPLANE ( UBPL )
      DOUBLE PRECISION      LNORML ( 3 )
      DOUBLE PRECISION      MAJLEN
      DOUBLE PRECISION      MAXANG ( 2 )
      DOUBLE PRECISION      MAXPT  ( 3 )
      DOUBLE PRECISION      MAXRAD ( 2 )
      DOUBLE PRECISION      MAXSEP
      DOUBLE PRECISION      MINANG ( 2 )
      DOUBLE PRECISION      MINLEN
      DOUBLE PRECISION      MINPT  ( 3 )
      DOUBLE PRECISION      MINRAD ( 2 )
      DOUBLE PRECISION      MINSEP
      DOUBLE PRECISION      MINVEC ( 3 )
      DOUBLE PRECISION      R      ( 3, 2 )
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      RMAT   ( 3, 3, 2 )
      DOUBLE PRECISION      SCLMAT ( 3, 3 )
      DOUBLE PRECISION      SCLROT ( 3, 3 )
      DOUBLE PRECISION      SMLCTR ( 3 )
      DOUBLE PRECISION      SMLDIR ( 3 )
      DOUBLE PRECISION      SMLMAJ ( 3 )
      DOUBLE PRECISION      SMLMAT ( 3, 3 )
      DOUBLE PRECISION      SMLMIN ( 3 )
      DOUBLE PRECISION      SMLVU  ( 3 )
      DOUBLE PRECISION      T12POS ( 3 )
      DOUBLE PRECISION      T1OPOS ( 3 )
      DOUBLE PRECISION      T2SEP
      DOUBLE PRECISION      TILT
      DOUBLE PRECISION      TMPCTR ( 3 )
      DOUBLE PRECISION      TMPMAJ ( 3 )
      DOUBLE PRECISION      TMPMIN ( 3 )
      DOUBLE PRECISION      TPOS   ( 3, 2 )
      DOUBLE PRECISION      TRGSEP
      DOUBLE PRECISION      TTDIST
      DOUBLE PRECISION      UASIZE
      DOUBLE PRECISION      UBDIST
      DOUBLE PRECISION      VIEW   ( 3 )
      DOUBLE PRECISION      VPH
      DOUBLE PRECISION      VPPROJ ( 3 )
      DOUBLE PRECISION      XASEP
      DOUBLE PRECISION      XDIST  ( 2 )
      DOUBLE PRECISION      XLIMB  ( UBEL )
      DOUBLE PRECISION      XR     ( 3, 3 )
      DOUBLE PRECISION      XSMLVU ( 3 )
      DOUBLE PRECISION      XTPOS  ( 3, 2 )
      DOUBLE PRECISION      XVIEW  ( 3 )
      DOUBLE PRECISION      XVWTRG ( 3 )

      INTEGER               BIGIDX
      INTEGER               FRTIDX
      INTEGER               I
      INTEGER               S
      INTEGER               SMLIDX
      LOGICAL               SFRONT
      
C
C     Overview
C     =======================================================
C
C     This routine starts out by initializing variables and
C     performing some error checks on the inputs.
C
C     The routine proceeds to classify the type occultation,
C     starting with simple approximation techniques, and if those
C     fail, following with more computationally expensive techniques.
C
C     All of the classifications have two elements:
C
C        - Determining the type of overlap: total occultation
C          or annular transit, partial occultation, or no 
C          occultation.
C
C        - Determining which object is in front of the other
C          if an overlap exists.
C
C     For each classification, this routine sets the return code to
C     indicate the above attributes of the occultation geometry.
C
C     The first classification step is performed using "bounding
C     cones." For each ellipsoid, we define a "minimum bounding cone"
C     and a "maximum bounding cone."  A minimum bounding cone for an
C     ellipsoid has the viewing point as its vertex and is tangent to
C     the sphere whose radius is the ellipsoid's minimum semi-axis
C     length and whose center coincides with the ellipsoid's center.
C
C     A maximum bounding cone is defined analogously, with the sphere
C     having radius equal to the ellipsoid's maximum semi-axis length.
C
C     Since all of the bounding cones intersect in the viewing point,
C     it's inaccurate to speak of the cones as "not intersecting."
C     However, it's very convenient to ignore this intersection, so
C     we'll consider a pair of cones to intersect or "overlap" only if
C     they intersect in more than just their common vertex.
C
C     The conditions that can be determined by the initial bounding
C     cone tests are as follows:
C
C        1) The maximum bounding cones are disjoint.  This implies
C           there is no occultation.
C
C        2) The maximum bounding cone of one ellipsoid is contained
C           in the minimum bounding cone of the other.  This implies
C           there is a total occultation or annular transit.
C
C        3) The minimum bounding cones of the ellipsoids overlap, 
C           and neither of these cones is contained in the maximum
C           bounding cone of the other ellipsoid.  This implies there
C           is a partial occultation.
C
C     If the occultation cannot be classified by the above tests, the
C     next step is to change the problem into an equivalent one in
C     which one of the ellipsoids is a sphere.  This new problem can be
C     attacked by considering the angular separation between the ray
C     from the viewing point to the center of the sphere and the limb
C     of the other ellipsoid.
C
C     To obtain this simplified geometric configuration, we apply to
C     all participating objects a non-singular linear transformation.
C     This transformation maps one of the ellipsoids to the unit sphere.
C     The viewing point, the center of the ellipsoid mapped to the 
C     unit sphere, and the center and generating vectors of the limb
C     of the other ellipsoid are all subjected to this transformation.
C     The result is a collection of objects that yield the same
C     occultation state as the original set.  (The reader may want
C     to verify that limbs of ellipsoids map to limbs under this
C     transformation.)
C
C     The conditions that can be identified immediately using the
C     transformed objects are:
C     
C        4)  The minimum angular separation between the ray from the
C            viewing point to the center of the unit sphere ("the ray"
C            henceforth) and the limb of the other ellipsoid is greater
C            than the angular radius (one half of the apparent angular
C            size as seen from the viewing point) of the unit sphere.
C            This implies there is no occultation.
C
C        5)  The minimum angular separation between the ray and the
C            limb of the other ellipsoid is negative (meaning the ray
C            penetrates the plane region bounded by the limb) and has
C            magnitude greater than the angular radius of the unit
C            sphere.  This implies the unit sphere is in total
C            occultation or in annular transit across the other 
C            ellipsoid.
C
C     If both of the above tests fail, there is an occultation, but
C     it remains to be classified.  We do know at this point that the
C     unit sphere extends beyond the other ellipsoid, but we don't 
C     know whether the other ellipsoid also extends beyond the unit
C     sphere.  If it does, we have a partial occultation; if it
C     doesn't, the other ellipsoid is totally occulted by the unit
C     sphere or is in annular transit across it.
C
C     At this point, we perform a second set of bounding cone tests.
C     The reason this may be useful is that the linear transformation
C     we've performed gives rise to a new set of bounding cones whose
C     containment relationships *are not* necessarily the same as those
C     of the original ellipsoids.  The conditions that can be 
C     identified at this point by the bounding cone tests are:
C
C        6) The bounding cone of the unit sphere (the minimum and
C           maximum bounding cones are coincident) contains the maximum
C           bounding cone of the other ellipsoid.  This implies the
C           latter ellipsoid is in total occultation or annular 
C           transit.
C
C        7) The bounding cone of the unit sphere does not contain
C           the minimum bounding cone of the other ellipsoid.  This
C           implies there is a partial occultation.
C 
C     If these tests fail, the final step is to find the maximum
C     angular separation of the ray and the limb of the other
C     ellipsoid.  This separation is signed, with a negative sign
C     indicating that the ray penetrates the plane region bounded by
C     the limb.  The conditions we can determine using this information
C     are:
C
C        8) The maximum *magnitude* of the angular separation of the
C           limb and the ray is less than or equal to the angular size
C           of the unit sphere. This implies the other ellipsoid is in
C           total occultation or annular transit across the unit sphere.
C
C        9) The maximum *magnitude* of the angular separation of the
C           limb and the ray is greater than the angular size
C           of the unit sphere. This implies there is a partial 
C           occultation.
C
C
C
C
C     Executable code
C     =======================================================
C
C     Set an initial function value.
C
      ZZOCCED = NOOCC

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
      
      CALL CHKIN ( 'ZZOCCED' )

C
C     Extract the radii of the targets from the semi-axis vectors.
C     At the same time, create rotation matrices that map vectors
C     from the principal axis frames of the targets to the base frame.
C
      DO I = 1, 3

         CALL UNORM ( SEMAX1(1,I), RMAT(1,I,1), R(I,1) )
         CALL UNORM ( SEMAX2(1,I), RMAT(1,I,2), R(I,2) )

      END DO

C
C     Find the minimum and maximum radii of both targets.
C
      DO I = 1, 2
         MINRAD(I) = MIN ( R(1,I), R(2,I), R(3,I) )
         MAXRAD(I) = MAX ( R(1,I), R(2,I), R(3,I) )
      END DO 

C
C     Make sure the input target radii are positive.  We'll actually do
C     a more stringent test later, but we must prevent divide-by-zero
C     errors at this point.
C
      IF (  ( MINRAD(1) .LE. 0.D0 ) .OR. ( MINRAD(2) .LE. 0.D0 )  ) THEN
 
         CALL SETMSG ( 'Minimum radii of bodies 1 and 2 are #, #. ' //
     .                 'Target radii must be positive.'             )
         CALL ERRDP  ( '#',  MINRAD(1)                              )
         CALL ERRDP  ( '#',  MINRAD(2)                              )
         CALL SIGERR ( 'SPICE(BADAXISLENGTH)'                       )
         CALL CHKOUT ( 'ZZOCCED'                                    )
         RETURN

      END IF

C
C     Compute view point-to-target vectors and ranges for both 
C     target bodies.
C
      CALL VEQU ( CENTR1, CTRS(1,1) )
      CALL VEQU ( CENTR2, CTRS(1,2) )

      DO I = 1, 2

         CALL VSUB ( CTRS(1,I), VIEWPT, TPOS(1,I) )

         DIST(I) = VNORM( TPOS(1,I) )


         IF ( DIST(I) .EQ. 0.D0 ) THEN

            CALL SETMSG ( 'Center of object # coincides with ' //
     .                    'the viewing point.'                 )
            CALL ERRINT ( '#',  I                              )
            CALL SIGERR ( 'SPICE(NOTDISJOINT)'                 )
            CALL CHKOUT ( 'ZZOCCED'                            )
            RETURN

         END IF

      END DO

C
C     Now check the semi-axis matrices.  We'll create new matrices
C     from these inputs by scaling the columns of each to unit length.
C     the resulting matrices are supposed to be rotations.
C
      DO I = 1, 2

         IF (  .NOT.  ISROT ( RMAT(1,1,I), NTOL, DTOL )  ) THEN

            CALL SETMSG ( 'Matrix derived by unitizing columns '  //
     .                    'of semi-axis matrix SEMAX# is not a '  //
     .                    'rotation matrix.  The determinant of ' //
     .                    'this matrix is #.'                     )
            CALL ERRINT ( '#',  I                                 )
            CALL ERRDP  ( '#',  DET ( RMAT(1,1,I) )               )
            CALL SIGERR ( 'SPICE(NOTAROTATION)'                   )
            CALL CHKOUT ( 'ZZOCCED'                               )
            RETURN
            
         END IF

      END DO

C
C     Find the position of the second target relative to the first.
C
      CALL VSUB ( TPOS(1,2), TPOS(1,1), T12POS )

      TTDIST = VNORM ( T12POS )

C
C     Make sure the targets are non-intersecting.
C
      IF (  TTDIST  .LE.  ( MINRAD(1) + MINRAD(2) )  ) THEN
         
         CALL SETMSG ( 'Targets must be non-intersecting, but  '     //
     .                 'spheres inscribed in the targets intersect.' )
         CALL SIGERR ( 'SPICE(NOTDISJOINT)'                          )
         CALL CHKOUT ( 'ZZOCCED'                                     )
         RETURN

      END IF

C
C     Make sure that the viewing point is outside of both target
C     ellipsoids.
C
      DO I = 1, 2         
C
C        Transform the Ith target position into the frame of the 
C        Ith target.
C
         CALL MTXV ( RMAT(1,1,I), TPOS(1,I), XTPOS(1,I) )

C
C        The viewpoint position is the negative of the target position.
C        Since we're squaring the terms involving the target position,
C        we omit the minus signs.
C
         LEVEL =    ( XTPOS(1,I) / R(1,I) )**2
     .           +  ( XTPOS(2,I) / R(2,I) )**2
     .           +  ( XTPOS(3,I) / R(3,I) )**2

         IF ( LEVEL .LT. 1.D0 ) THEN
   
            CALL SETMSG ( 'Viewpoint is inside target #; level ' //
     .                    'surface parameter = #.'               )
            CALL ERRINT ( '#',  I                                )
            CALL ERRDP  ( '#',  LEVEL                            )
            CALL SIGERR ( 'SPICE(NOTDISJOINT)'                   )
            CALL CHKOUT ( 'ZZOCCED'                              )
            RETURN

         END IF

      END DO

C
C     Find the minimum and maximum angular radii of both targets.  Note
C     that the distances used as denominators are guaranteed to be
C     positive at this point.
C
      DO I = 1, 2

         MINANG(I) = DASINE ( MINRAD(I)/DIST(I), ATOL )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZOCCED' )
            RETURN
         END IF

C
C        The situation is a bit more complicated for the maximum
C        bounding sphere, because the observer can be outside both
C        ellipsoids but inside one or both maximum bounding spheres.
C        We handle that special case separately.
C
         IF ( DIST(I) .GE. MAXRAD(I) ) THEN
C
C           The viewing point is outside the sphere; we use the sphere
C           to define the maximum angular radius.
C
            MAXANG(I) = DASINE ( MAXRAD(I)/DIST(I), ATOL )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZOCCED' )
               RETURN
            END IF

         ELSE
C
C           The viewing point is outside the Ith ellipsoid but inside
C           the nominal bounding sphere.  We can't use the sphere to
C           define the maximum bounding cone.  Instead, we bound the
C           angular radius of the ellipsoid as follows:
C
C              1) Find the limb of the ellipsoid as seen from the
C                 viewing point, and construct the limb plane.
C
C              2) Find the orthogonal projection of the viewing point
C                 onto the limb plane; call this project VPPROJ.  The
C                 height of the viewing point above VPPROJ is VPH.
C
C              3) Create an upper bound UBDIST on the maximum distance
C                 between VPPROJ and any limb point.  Here's where we
C                 use a crude but safe estimate:  let UBDIST be the
C                 sum of the distance between VPPROJ and the center of
C                 the limb and the semi-major axis length of the limb.
C                 The triangle inequality shows this is a valid upper
C                 bound.
C            
C              4) The viewing point and the circle of radius UBDIST
C                 centered at VPPROJ define a right circular cone 
C                 that contains the limb:  this is our choice of
C                 the maximum bounding cone.  The arctangent of 
C                 UBDIST/VPH is the angular radius of this cone.
C                 
C
C           The vector XTPOS(*,I) contains the position of the Ith
C           target relative to the viewing point, represented in the
C           principal axis frame of the Ith target. Let XVWTRG contain
C           the inverse of this vector, which is the observer position
C           relative to the center of the Ith target, in the principal
C           axis frame of the Ith target.
C
            CALL VMINUS ( XTPOS(1,I), XVWTRG )

            CALL EDLIMB ( R(1,I), R(2,I), R(3,I), XVWTRG, LIMB )

C
C           Extract the limb's center and semi-axis vectors.
C
            CALL EL2CGV ( LIMB, LMBCTR, LMBMAJ, LMBMIN )

C
C           Create the limb plane.
C           
            CALL PSV2PL ( LMBCTR, LMBMAJ, LMBMIN, LPLANE )

C
C           Project the viewing point onto the limb plane.  Find
C           the height of the viewing point relative to this plane.
C           
            CALL VPRJP ( XVWTRG, LPLANE, VPPROJ )

            VPH = VDIST ( XVWTRG, VPPROJ )

C
C           Find an upper bound on the distance of any limb point from
C           VPPROJ.
C           
            UBDIST = VDIST ( VPPROJ, LMBCTR )  +  VNORM ( LMBMAJ )

C
C           Find the angular size of the maximum bounding cone.  We
C           use the 2-argument arctangent to avoid divide-by-zero
C           problems.  The worst that can happen is that VPH is 
C           zero, which gives us a degenerate cone of angular radius
C           pi/2.
C
            MAXANG(I) = ATAN2 ( UBDIST, VPH )

         END IF
C
C        At this point MAXANG(I) and MINANG(I) are both set for the
C        Ith ellipsoid.
C
      END DO



C
C     Find the angular separation of the centers of the targets
C     seen by the observer.
C
      TRGSEP = VSEP ( TPOS(1,1), TPOS(1,2) )

C
C     If bounding cones defined by the maximum radii don't intersect,
C     we're done.
C
      IF (  TRGSEP  .GT. ( MAXANG(1) + MAXANG(2) )  ) THEN

         ZZOCCED = NOOCC

         CALL CHKOUT ( 'ZZOCCED' )
         RETURN

      END IF

C
C     Use the maximum angular sizes to determine which ellipsoid
C     appears to the observer to be "biggest."  This is merely a
C     heuristic:  the orientation of the ellipsoids may cause the order
C     of the apparent angular sizes to be the opposite. The idea,
C     however, is that for "reasonable" cases, we'll correctly identify
C     the ellipsoid of larger angular size. This choice is made to
C     improve efficiency.
C    
      IF (  MAXANG(1)  .GT.  MAXANG(2)  ) THEN
         BIGIDX  =  1
      ELSE
         BIGIDX  =  2
      END IF

C
C     The other index is SMLIDX.
C
      SMLIDX = 3 - BIGIDX

C
C     We're ready to see whether an occultation condition exists.
C     We can efficiently handle some cases by working with bounding
C     cones defined by the viewing point, the centers of the targets,
C     and spheres centered at the targets having radii equal to the
C     minimum and maximum radii of the targets.
C
C     If the two minimum bounding cones have non-trivial intersection
C     (of course they always intersect at their common vertex), we're
C     guaranteed some sort of occultation.  Check for this case.
C
      IF (  ( MINANG(1) + MINANG(2) )  .GT.  TRGSEP  ) THEN
C
C        The minimum bounding cones do overlap. Determine which target
C        is "in front" of the other.  We do this determining which
C        minimum sphere is in front of the other.
C
C        We'll do the test by examining the angle between the vectors
C        from the first target to the observer and the from the first
C        target to the second.  If that angle is less than the
C        complement of the angular radius of the first target, then the
C        minimum sphere of the second target is in transit across the
C        first. Otherwise the minimum sphere of the second target is at
C        least partially occulted by the first.
C
C        Let T1OPOS be the vector from the first target to the observer.
C
         CALL VMINUS ( TPOS(1,1), T1OPOS )

C
C        ANGCMP is the angle between a vector from the first target's
C        center to its limb and the plane containing the center and
C        orthogonal to the vector from the first target's center to the
C        observer.
C
         ANGCMP = HALFPI() - MINANG(1)

C
C        T2SEP is the angle between the vector from the first target's
C        center to the observer and the vector from the first target
C        to the second target.
C
         T2SEP  =  VSEP ( T1OPOS, T12POS )


         IF ( T2SEP .LT. ANGCMP ) THEN
C
C           The second target is "in front" of the first.
C
            FRTIDX =  2

C
C           Set the sign of the return code.
C
            S      = -1

         ELSE

            FRTIDX =  1
            S      =  1
 
         END IF

C
C        Now classify the occultation.  If the minimum sphere
C        of the front target has angular size greater than the maximum
C        angular size of the rear target plus the angular separation
C        of the target centers, the occultation is total.
C
         DO I = 1, 2
C
C           (The subscript 3-I used below is 2 if I is 1 and vice
C           versa.)
C
            IF (  MINANG(I)  .GE.  ( MAXANG(3-I) + TRGSEP )  ) THEN
C
C              If target I is in front, it totally occults the other
C              target.  Otherwise, the other target is in annular
C              transit across target I.
C              
               IF ( FRTIDX .EQ. I ) THEN
                  ZZOCCED = S * TOTAL2
               ELSE
                  ZZOCCED = S * ANNLR2
               END IF

C
C              We've found the occultation type, so we're done.
C
               CALL CHKOUT ( 'ZZOCCED' )
               RETURN

            END IF

         END DO

C
C        If the angular size of the minimum sphere of *each* target
C        plus the angular separation of the centers exceeds the
C        maximum angular size of the other target, the occultation
C        is partial.  In other words, overlap is guaranteed, but it
C        is also guaranteed that neither target is totally blocked 
C        by the other.
C
         IF (       (  ( MINANG(1) + TRGSEP ) .GT. MAXANG(2)  )
     .        .AND. (  ( MINANG(2) + TRGSEP ) .GT. MAXANG(1)  )   ) THEN
C
C           The occultation code is +/- PARTL2, depending on whether
C           the first target is in front.
C
            ZZOCCED = S * PARTL2

            CALL CHKOUT ( 'ZZOCCED' )
            RETURN

         END IF
C
C        If we get to this point, we were unable to classify the 
C        occultation using bounding cones alone.
C
      END IF

C
C     This is the end of the case of overlapping minimum bounding
C     cones.  
C
C     We're going apply a linear transformation to the viewing point
C     and both targets so as to convert the larger of the targets into
C     a sphere. We'll then find the angular separation from the other
C     target of the ray from viewing point to the center of the sphere.
C     In practice, we must transform the viewing point, the target 
C     centers, and the limb of the ellipsoid that doesn't get mapped
C     to the unit sphere.
C     
C     Note that this transformation *does not* preserve angular
C     separation, but it preserves set containment relationships.
C     In particular, the limbs of the targets map to limbs under
C     this transformation, since the limbs are the intersection sets
C     of the targets and tangent rays emanating from the viewing point.
C
C     First step:  find the limb of the smaller ellipsoid as
C     seen from the viewing point.  We need to map the viewing point
C     into the principal axis frame of the smaller ellipsoid first.
C     Let SMLMAT be the rotation matrix that effects this mapping.
C
      CALL XPOSE ( RMAT(1,1,SMLIDX),  SMLMAT )

C
C     Apply SMLMAT to the vector from the center of the smaller
C     ellipsoid to the viewing point.
C
      CALL VSUB ( VIEWPT, CTRS(1,SMLIDX), SMLVU )

      CALL MXV  ( SMLMAT, SMLVU,          VIEW  )

C
C     Find the limb of the smaller ellipsoid as seen from VIEW.
C
      CALL EDLIMB ( R(1,SMLIDX), R(2,SMLIDX), R(3,SMLIDX), VIEW, LIMB )

C
C     Unpack the limb and map it from the principal axis frame of the
C     small ellipsoid back into the original frame.
C
      CALL EL2CGV ( LIMB, TMPCTR, TMPMAJ, TMPMIN )

      CALL MTXV   ( SMLMAT, TMPCTR, SMLCTR )
      CALL MTXV   ( SMLMAT, TMPMAJ, SMLMAJ )
      CALL MTXV   ( SMLMAT, TMPMIN, SMLMIN )

C
C     At this point SMLCTR is the position of the center of the limb
C     relative to the center of the small ellipsoid.  We want to express
C     this center relative to the origin; we use the vector SMLCTR for
C     this.
C
      CALL VADD ( CTRS(1,SMLIDX), SMLCTR, TMPCTR )
      CALL VEQU ( TMPCTR,                 SMLCTR )

C
C     Create the transformation matrix that maps the larger ellipsoid
C     to the unit sphere.
C
C     First compute the scale matrix SCLMAT that scales vector
C     components by the reciprocals of the respective semi-axis
C     lengths of the large ellipsoid.  
C
      CALL CLEARD ( 9, SCLMAT )

      SCLMAT(1,1) = 1.D0 / R(1,BIGIDX)
      SCLMAT(2,2) = 1.D0 / R(2,BIGIDX)
      SCLMAT(3,3) = 1.D0 / R(3,BIGIDX)

C
C     Compose the row-scaling matrix SCLMAT with the frame
C     transformation required to map vectors to the principal axis
C     frame of this ellipsoid.  The result is the transformation
C     that maps the larger ellipsoid to the unit sphere.
C
C     We use one matrix SCLROT to perform these composed operations.
C
      CALL XPOSE ( RMAT(1,1,BIGIDX),  XR          )
      CALL MXM   ( SCLMAT,            XR,  SCLROT )

C
C     Transform the viewing point, the large ellipsoid's center vector,
C     and vectors defining the limb of the smaller ellipsoid using the
C     mapping that converts the larger ellipsoid to the unit sphere.
C
C     Map the viewing point to XVIEW.
C
      CALL MXV ( SCLROT, VIEWPT, XVIEW  )

C
C     Map the center of the large ellipsoid to BIGCTR.
C
      CALL MXV ( SCLROT, CTRS(1,BIGIDX), BIGCTR )

C
C     Map the limb vectors of the smaller ellipsoid. 
C
      CALL MXV  ( SCLROT, SMLCTR, TMPCTR )
      CALL VEQU ( TMPCTR,         SMLCTR )

      CALL MXV  ( SCLROT, SMLMAJ, TMPMAJ )
      CALL MXV  ( SCLROT, SMLMIN, TMPMIN )

C
C     Find the semi-axes of the transformed limb of the smaller
C     ellipsoid. Pack these vectors into the transformed limb data
C     structure XLIMB.
C
      CALL SAELGV ( TMPMAJ, TMPMIN, SMLMAJ, SMLMIN )
      CALL CGV2EL ( SMLCTR, SMLMAJ, SMLMIN, XLIMB  )

C
C     Find the direction vector of the ray from the viewing point
C     to the transformed center of the large ellipsoid.
C
      CALL VSUB ( BIGCTR, XVIEW, RAYDIR )

C
C     Find the angular separation of the ray and the transformed
C     limb of the small ellipsoid.  The output MINPT is the limb
C     point at which the minimum angular separation is attained.
C
      CALL ZZASRYEL ( 'MIN', XLIMB, XVIEW, RAYDIR, MINSEP, MINPT )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZOCCED' )
         RETURN
      END IF

C
C     Find the angular radius of the unit sphere centered at
C     BIGCTR, as seen from XVIEW.
C
      BIGR = VNORM ( RAYDIR )

C
C     Although previous error checks should ensure that BIGR is
C     greater than or equal to 1, we'll use a safe arcsine
C     computation.
C     
      UASIZE = DASINE ( 1.D0/BIGR, ATOL )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZOCCED' )
         RETURN
      END IF

C
C     At this point, UASIZE is the angular size of the unit sphere
C     representing the transformed larger ellipsoid.  MINSEP is the
C     angular separation of the ray from the viewing point to the
C     center of the unit sphere and the transformed limb of the
C     smaller ellipsoid.
C
      IF ( MINSEP .GT. UASIZE ) THEN
C
C        There's no overlap.
C
         ZZOCCED = NOOCC
              
         CALL CHKOUT ( 'ZZOCCED' )
         RETURN

      END IF

C
C     There's an overlap; now we must classify it. We know the limb
C     point MINPT at which the minimum angular separation occurs lies
C     in front of or behind the unit sphere, since the angular
C     separation at this point is less than or equal to UASIZE.
C
C     Find the vector from the center of the sphere to MINPT.
C
      CALL VSUB ( MINPT, BIGCTR, MINVEC )

C
C     Get the inverse of the ray's direction vector.
C     
      CALL VMINUS ( RAYDIR, INVRAY )

C
C     Now we can apply the criterion from the spherical occultation
C     algorithm to determine whether MINPT is in front of or behind
C     the sphere.  We'll use the logical flag SFRONT to indicate the
C     relative position of MINPT.
C
C     Set the sign S used later to set the return code as well.
C
      IF (  VSEP(MINVEC, INVRAY)  .LE.  ( HALFPI() - UASIZE )  ) THEN
C
C        MINPT is in front.
C
         SFRONT = .TRUE.
      ELSE
         SFRONT = .FALSE.
      END IF


      IF (      (          SFRONT   .AND.  ( SMLIDX .EQ. 1 )  )
     .     .OR. (  ( .NOT. SFRONT ) .AND.  ( SMLIDX .EQ. 2 )  )  ) THEN
C
C        The first target is in front.
C
         S  =  1
      ELSE
         S  = -1
      END IF


      IF ( MINSEP .LE. -UASIZE ) THEN
C
C        Arriving here implies that the "smaller" ellipsoid actually
C        appears larger than the other.  Recall that our determination
C        of which ellipsoid had larger apparent extent was fallible.
C        This situation is not an error condition.
C
C        The ray intersects the interior of the plane region bounded by
C        the limb of the "smaller" ellipsoid, and the unit sphere is
C        either totally occulted by the smaller ellipsoid or is in
C        annular transit across it.  
C
         IF ( SFRONT ) THEN
C
C           The point of minimum angular separation on the limb of the
C           smaller ellipsoid is in front: we have a total occultation
C           of the larger ellipsoid.
C
            ZZOCCED =  S * TOTAL2

         ELSE
C
C           We have an annular transit of the larger ellipsoid 
C           across the smaller one.
C
            ZZOCCED =  S * ANNLR2

         END IF


      ELSE
C
C        We know that some type of occultation exists. We know the
C        unit sphere is *neither* totally occulted by the other
C        ellipsoid nor in annular transit across it. It's possible that
C        the other ellipsoid is totally occulted by the unit sphere or
C        is in annular transit across it; otherwise we have a partial
C        occultation.
C
C        We try two quick classification tests first: 
C        
C           1) We see whether the maximum bounding cone of the small
C              ellipsoid is contained in the cone defined by the
C              viewing point and unit sphere.
C        
C           2) We see whether the minimum bounding cone of the small
C              ellipsoid extends beyond the cone defined by the
C              viewing point and unit sphere.
C       
C        Note that we need to re-compute the bounding cones for the
C        small ellipsoid since we've applied a linear transformation
C        to it.
C 
C        Note also that these tests are not duplicates of the tests
C        performed earlier, since now the bounding cones of the 
C        ellipsoids have been changed by the transformation applied
C        to both.
C
C        The linear transformation applied to the small ellipsoid does
C        not preserve distances, so we must re-compute the distance
C        from the viewing point to the center of the small ellipsoid.
C        
         CALL VSUB ( XVIEW, SMLCTR, XSMLVU )

         XDIST(SMLIDX) = VNORM ( XSMLVU )

C
C        Compute angular radii of bounding cones for the transformed
C        limb of the small ellipsoid.  First, capture the semi-axis
C        lengths of the limb.
C
         MAJLEN = VNORM( SMLMAJ )
         MINLEN = VNORM( SMLMIN )

         IF ( XDIST(SMLIDX) .GE. MAJLEN ) THEN
C
C           The viewing point is outside a sphere of radius MAJLEN
C           centered at the limb's center.  We use this sphere to
C           to define the maximum angular radius.  Note that this
C           sphere may have larger angular extent than the small
C           ellipsoid, but it's guaranteed to block the small 
C           ellipsoid.
C
            MAXANG(SMLIDX) = DASINE ( MAJLEN/XDIST(SMLIDX), ATOL )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZOCCED' )
               RETURN
            END IF

         ELSE
C
C           We create a maximum bounding cone using the same technique
C           we used above for the original, untransformed targets.  In
C           this case we already have the components of the limb of
C           the transformed, small target.
C
C           Create the limb plane.
C           
            CALL PSV2PL ( SMLCTR, SMLMAJ, SMLMIN, LPLANE )

C
C           Project the viewing point onto the limb plane.  Find
C           the height of the viewing point relative to this plane.
C           
            CALL VPRJP ( XVIEW, LPLANE, VPPROJ )

            VPH = VDIST ( XVIEW, VPPROJ )

C
C           Find an upper bound on the distance of any limb point from
C           VPPROJ.
C           
            UBDIST = VDIST ( VPPROJ, SMLCTR )  +  MAJLEN

C
C           Find the angular size of the maximum bounding cone.  We
C           use the 2-argument arctangent to avoid divide-by-zero
C           problems.  The worst that can happen is that VPH is 
C           zero, which gives us a degenerate cone of angular radius
C           pi/2.
C
            MAXANG(SMLIDX) = ATAN2 ( UBDIST, VPH )

         END IF

C
C        Now find the minimum bounding cone.  The situation is slightly
C        complicated by the fact that we have the limb of the
C        transformed, small ellipsoid rather than the ellipsoid itself.
C        We don't want to use ZZASRYEL here because that routine is
C        slow:  we don't want to call it if a quick test will do. So we
C        use a somewhat crude estimate that guarantees that all rays
C        contained in the small bounding cone intersect the small
C        ellipsoid.  The approach is as follows:
C
C           1) Determine the angle between the normal to the limb plane
C              pointing towards XVIEW and the viewing point-limb center
C              vector. Call this angle TILT.
C              
C           2) For a circle having radius equal to the semi-minor axis
C              length of the limb, inscribed in the limb, and coplanar
C              with the limb, the minimum angular radius of any point
C              on the circle, as seen from XVIEW, is associated with
C              the point farthest from XVIEW.  The angular separation
C              of the vector from the limb center to this point and the
C              vector from XVIEW to the limb center is pi/2 + TILT.
C              Find the angular radius associated with that point.
C
C        Start out by constructing a normal to the limb plane.
C
         CALL UCRSS ( SMLMAJ, SMLMIN, LNORML )

C
C        Choose a value of TILT not exceeding pi/2.
C
         TILT = VSEP ( LNORML, XSMLVU )

         IF ( TILT .GT. HALFPI() ) THEN
            
            TILT = PI() - TILT

         END IF

C
C        Now we have a right triangle whose base is the distance from
C        XVIEW to the limb's center plus sin(TILT)*MINLEN, and whose
C        height is cos(TILT)*MINLEN.
C
C        Find the angle associated with the corner of the triangle 
C        associated with the viewing point.  This is the angular
C        radius of our minimum bounding cone.
C                 
         MINANG(SMLIDX)  = ATAN2 ( COS(TILT)*MINLEN, 
     .                             SIN(TILT)*MINLEN + XDIST(SMLIDX) )

C
C        Compute angular separation of the transformed centers.
C 
         CALL VSUB ( SMLCTR, XVIEW, SMLDIR )

         XASEP  =  VSEP ( RAYDIR, SMLDIR )

C
C        Test for inclusion of the maximum bounding cone of the small
C        ellipsoid in the circumscribing cone of the sphere.
C
         IF (  ( XASEP + MAXANG(SMLIDX) )  .LE.  UASIZE  ) THEN
C
C           The small ellipsoid is either in total occultation or
C           in annular transit across the sphere.
C
            IF ( SFRONT ) THEN
C
C              MINPT is in front of the sphere. We have an annular
C              transit of the small ellipsoid across the small one.
C
               ZZOCCED =  S * ANNLR2

            ELSE
C
C              MINPT is behind the sphere.  We have a total
C              occultation of the small ellipsoid.
C
               ZZOCCED =  S * TOTAL2

            END IF

            CALL CHKOUT ( 'ZZOCCED' )
            RETURN

         END IF

C
C        Test for non-containment of the minimum bounding cone of the
C        small ellipsoid by the circumscribing cone of the sphere.
C
         IF (  ( XASEP + MINANG(SMLIDX) )  .GT.  UASIZE  ) THEN
C
C           The small ellipsoid is either in partial occultation or
C           in partial transit across the sphere.
C
            ZZOCCED = S * PARTL2

            CALL CHKOUT ( 'ZZOCCED' )
            RETURN

         END IF

C
C        Arriving at this point means we've been unable to classify
C        the occultation or transit.  We're going to need to compute
C        the maximum angular separation of the limb from the ray
C        emanating from the viewing point and passing through the
C        center of the sphere.
C
         CALL ZZASRYEL ( 'MAX', XLIMB, XVIEW, RAYDIR, MAXSEP, MAXPT )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZOCCED' )
            RETURN
         END IF


         IF ( ABS(MAXSEP) .LE. UASIZE ) THEN
C
C           Whether the ray from the viewing point to the center
C           of the unit sphere does nor does not penetrate the plane
C           region bounded by the limb of the smaller ellipse, no
C           point on that limb has greater angular separation than
C           UASIZE from the ray.          
C
C           The small ellipsoid is either in total occultation or
C           in annular transit across the sphere.
C
            IF ( SFRONT ) THEN
C
C              MINPT is in front of the sphere. We have an annular
C              transit of the small ellipsoid across the smaller.
C
               ZZOCCED =  S * ANNLR2

            ELSE
C
C              MINPT is behind the sphere.  We have a total
C              occultation of the small ellipsoid.
C
               ZZOCCED =  S * TOTAL2

            END IF


         ELSE
C
C           Whether the ray from the viewing point to the center
C           of the unit sphere does nor does not penetrate the plane
C           region bounded by the limb of the smaller ellipse, some
C           point on that limb has greater angular separation than
C           UASIZE from the ray.          
C
C           The small ellipsoid is either in partial occultation or
C           in partial transit across the sphere.
C
            ZZOCCED = S * PARTL2

         END IF
C
C        We've classified the occultation in the case where the
C        maximum angular separation of the ray and limb had to be
C        computed.
C
C        This is the end of the code for the case where there is
C        overlap, but the unit sphere is *neither* totally occulted by
C        the other ellipsoid nor in annular transit across it.
C        
      END IF
C
C     ZZOCCED has been set.
C

      CALL CHKOUT ( 'ZZOCCED' )
      RETURN
      END

