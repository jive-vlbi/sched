C$Procedure   ZZHULLAX ( Pyramidal FOV convex hull to FOV axis )
 
      SUBROUTINE ZZHULLAX ( INST, N, BOUNDS, AXIS )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Identify a face of the convex hull of an instrument's 
C     polygonal FOV, and use this face to generate an axis of the
C     FOV.
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
C     CK
C     FRAMES
C     GF
C     IK
C     KERNEL
C
C$ Keywords
C
C     FOV
C     GEOMETRY
C     INSTRUMENT
C
C$ Declarations
 
      IMPLICIT NONE

      DOUBLE PRECISION      MARGIN
      PARAMETER           ( MARGIN = 1.D-12 )

      CHARACTER*(*)         INST
      INTEGER               N
      DOUBLE PRECISION      BOUNDS ( 3, N )
      DOUBLE PRECISION      AXIS   ( 3 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     MARGIN     P   Minimum complement of FOV cone angle.
C     INST       I   Instrument name.
C     N          I   Number of FOV boundary vectors.
C     BOUNDS     I   FOV boundary vectors.
C     AXIS       O   Instrument FOV axis vector.
C
C$ Detailed_Input
C
C     INST       is the name of an instrument with which the field of
C                view (FOV) of interest is associated. This name is
C                used only to generate long error messages.
C
C     N          is the number of boundary vectors in the array
C                BOUNDS.
C 
C     BOUNDS     is an array of N vectors emanating from a common
C                vertex and defining the edges of a pyramidal region in
C                three-dimensional space: this the region within the
C                FOV of the instrument designated by INST. The Ith
C                vector of BOUNDS resides in elements (1:3,I) of this
C                array. 
C
C                The vectors contained in BOUNDS are called the 
C                "boundary vectors" of the FOV.
C
C                The boundary vectors  must satisfy the constraints:
C
C                   1)  The boundary vectors  must be contained within
C                       a right circular cone of angular radius less
C                       than than (pi/2) - MARGIN radians; in other
C                       words, there must be a vector A such that all
C                       boundary vectors have angular separation from
C                       A of less than (pi/2)-MARGIN radians.
C
C                   2)  There must be a pair of vectors U, V in BOUNDS
C                       such that all other boundary vectors lie in
C                       the same half space bounded by the plane
C                       containing U and V. Furthermore, all other
C                       boundary vectors must have orthogonal
C                       projections onto a plane normal to this plane
C                       such that the projections have angular
C                       separation of at least 2*MARGIN radians from
C                       the plane spanned by U and V.
C
C                Given the first constraint above, there is plane PL
C                such that each of the set of rays extending the
C                boundary vectors intersects PL. (In fact, there is an
C                infinite set of such planes.) The boundary vectors
C                must be ordered so that the set of line segments
C                connecting the intercept on PL of the ray extending
C                the Ith vector to that of the (I+1)st, with the Nth
C                intercept connected to the first, form a polygon (the
C                "FOV polygon") constituting the intersection of the
C                FOV pyramid with PL. This polygon may wrap in either
C                the positive or negative sense about a ray emanating
C                from the FOV vertex and passing through the plane
C                region bounded by the FOV polygon.                
C
C                The FOV polygon need not be convex; it may be
C                self-intersecting as well.
C
C                No pair of consecutive vectors in BOUNDS may be 
C                linearly dependent.
C
C                The boundary vectors need not have unit length.
C
C
C$ Detailed_Output
C
C     AXIS       is a unit vector normal to a plane containing the
C                FOV polygon. All boundary vectors have angular
C                separation from AXIS of not more than 
C
C                   ( pi/2 ) - MARGIN
C
C                radians.
C
C                This routine signals an error if it cannot find
C                a satisfactory value of AXIS.
C
C$ Parameters
C
C     MARGIN     is a small positive number used to constrain the
C                orientation of the boundary vectors. See the two
C                constraints described in the Detailed_Input section
C                above for specifics.
C 
C$ Exceptions
C
C     1)  In the input vector count N is not at least 3, the error
C         SPICE(INVALIDCOUNT) is signaled.
C
C     2)  If any pair of consecutive boundary vectors has cross
C         product zero, the error SPICE(DEGENERATECASE) is signaled.
C         For this test, the first vector is considered the successor
C         of the Nth.
C 
C     3)  If this routine can't find a face of the convex hull of
C         the set of boundary vectors such that this face satisfies
C         constraint (2) of the Detailed_Input section above, the
C         error SPICE(FACENOTFOUND) is signaled.
C
C     4)  If any boundary vectors have longitude too close to 0
C         or too close to pi radians in the face frame (see discussion
C         of the search algorithm's steps 3 and 4 in Particulars
C         below), the respective errors SPICE(NOTSUPPORTED) or
C         SPICE(FOVTOOWIDE) are signaled.
C         
C     5)  If any boundary vectors have angular separation of more than
C         (pi/2)-MARGIN radians from the candidate FOV axis, the
C         error SPICE(FOVTOOWIDE) is signaled.
C
C$ Files
C
C     The boundary vectors input to this routine are typically 
C     obtained from an IK file.
C
C$ Particulars
C
C     Normally implementation is not discussed in SPICE headers, but we
C     make an exception here because this routine's implementation and
C     specification are deeply intertwined.
C
C     This routine produces an "axis" for a polygonal FOV using the
C     following approach:
C
C        1)  Test pairs of consecutive FOV boundary vectors to see
C            whether there's a pair such that the plane region bounded
C            by these vectors is
C
C            a)  part of the convex hull of the set of boundary vectors
C
C            b)  such that all other boundary vectors have angular
C                separation of at least MARGIN from the plane
C                containing these vectors
C
C            This search has O(N**2) run time dependency on N.
C
C            If this test produces a candidate face of the convex hull,
C            proceed to step 3.
C
C
C        2)  If step (1) fails, repeat the search for a candidate
C            convex hull face, but this time search over every pair of
C            distinct boundary vectors.
C
C            This search has O(N**3) run time dependency on N.
C
C            If this search fails, signal an error.
C
C
C        3)  Produce a set of basis vectors for a reference frame,
C            which we'll call the "face frame," using as the +X axis
C            the angle bisector of the vectors bounding the candidate
C            face, the +Y axis the inward normal vector to this face,
C            and the +Z axis completing a right-handed basis.
C
C
C        4)  Transform each boundary vector, other than the two vectors
C            defining the selected convex hull face, to the face frame
C            and compute the vector's longitude in that frame. Find the
C            maximum and minimum longitudes of the vectors in the face
C            frame.
C
C            If any vector's longitude is less than 2*MARGIN or greater
C            than pi - 2*MARGIN radians, signal an error.
C
C
C        5)  Let DELTA be the difference between pi and the maximum
C            longitude found in step (4). Rotate the +Y axis (which
C            points in the inward normal direction relative to the
C            selected face) by -DELTA/2 radians about the +Z axis of
C            the face frame. This rotation aligns the +Y axis with the
C            central longitude of the set of boundary vectors. The
C            resulting vector is our candidate FOV axis.
C
C
C        6)  Check the angular separation of the candidate FOV axis
C            against each boundary vector. If any vector has angular
C            separation of more than (pi/2)-MARGIN radians from the
C            axis, signal an error.
C
C
C     Note that there are reasonable FOVs that cannot be handled by the
C     algorithm described here. For example, any FOV whose cross
C     section is a regular convex polygon can be made unusable by
C     adding boundary vectors aligned with the angle bisectors of each
C     face of the pyramid defined by the FOV's boundary vectors. The
C     resulting set of boundary vectors has no face in its convex hull
C     such that all other boundary vectors have positive angular
C     separation from that face.
C
C     Because of this limitation, this algorithm should be used only
C     after a simple FOV axis-finding approach, such as using as the
C     FOV axis the average of the boundary vectors, has been tried
C     unsuccessfully.
C
C     Note that it's easy to construct FOVs where the average of the
C     boundary vectors doesn't yield a viable axis: a FOV of angular
C     width nearly equal to pi radians, with a sufficiently large
C     number of boundary vectors on one side and few boundary vectors
C     on the other, is one such example. This routine can find an
C     axis for many such intractable FOVs---that's why this routine
C     should be called after the simple approach fails.
C
C$ Examples
C
C     See SPICELIB private routine ZZFOVAXI.
C
C$ Restrictions
C
C     1) This is a SPICE private routine. User applications should not
C        call this routine.
C
C     2) There are "reasonable" polygonal FOVs that cannot be handled
C        by this routine. See the discussion in Particulars above.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    SPICELIB 1.0.0, 05-MAR-2009 (NJB)
C
C-&
 
C$ Index_Entries
C
C     Create axis vector for polygonal FOV
C
C-&
 


C
C     SPICELIB functions
C
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      VSEP

      LOGICAL               RETURN
      LOGICAL               VZERO

C
C     Local parameters
C

C
C     Local variables
C      
      DOUBLE PRECISION      CP     ( 3 )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      LAT 
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      MAXLON
      DOUBLE PRECISION      MINLON
      DOUBLE PRECISION      R
      DOUBLE PRECISION      RAY1   ( 3 )
      DOUBLE PRECISION      RAY2   ( 3 )
      DOUBLE PRECISION      SEP
      DOUBLE PRECISION      TRANS  ( 3, 3 )
      DOUBLE PRECISION      V      ( 3 )
      DOUBLE PRECISION      XVEC   ( 3 )
      DOUBLE PRECISION      YVEC   ( 3 )
      DOUBLE PRECISION      ZVEC   ( 3 )

      INTEGER               I
      INTEGER               M
      INTEGER               MAXIX
      INTEGER               MINIX
      INTEGER               NEXT
      INTEGER               XIDX
      
      LOGICAL               FOUND
      LOGICAL               OK
      LOGICAL               PASS1

      
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZHULLAX' )

C
C     Nothing found yet.
C
      FOUND = .FALSE.
      XIDX  =  0

C
C     We must have at least 3 boundary vectors.
C
      IF ( N .LT. 3 ) THEN
         
         CALL SETMSG ( 'Polygonal FOV requires at least 3 '
     .   //            'boundary vectors but number '
     .   //            'supplied for # was #.'             )
         CALL ERRCH  ( '#', INST                           )
         CALL ERRINT ( '#', N                              )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'               )
         CALL CHKOUT ( 'ZZHULLAX'                          )
         RETURN

      END IF

C
C     Find an exterior face of the pyramid defined by the
C     input boundary vectors. Since most polygonal FOVs will have 
C     an exterior face bounded by two consecutive rays, we'll
C     try pairs of consecutive rays first. If this fails, we'll
C     try each pair of rays.
C
      I = 1
      
      DO WHILE ( ( I .LE. N ) .AND. ( .NOT. FOUND ) ) 
C
C        Set the index of the next ray. When we get to the
C        last boundary vector, the next ray is the first.
C
         IF ( I .EQ. N ) THEN
            NEXT = 1
         ELSE
            NEXT = I + 1
         END IF

C
C        Find the cross product of the first ray with the 
C        second. Depending on the ordering of the boundary
C        vectors, this could be an inward or outward normal,
C        in the case the current face is exterior.
C
         CALL VCRSS ( BOUNDS(1,I), BOUNDS(1,NEXT), CP )

C
C        We insist on consecutive boundary vectors being
C        linearly independent.
C
         IF ( VZERO(CP) ) THEN

            CALL SETMSG ( 'Polygonal FOV must have linearly '
     .      //            'independent consecutive boundary '
     .      //            'but vectors at indices # and # '
     .      //            'have cross product equal to the '
     .      //            'zero vector. Instrument is #.'     )
            CALL ERRINT ( '#', I                              )
            CALL ERRINT ( '#', NEXT                           )
            CALL ERRCH  ( '#', INST                           )
            CALL SIGERR ( 'SPICE(DEGENERATECASE)'             )
            CALL CHKOUT ( 'ZZHULLAX'                          )
            RETURN

         END IF

C
C        See whether the other boundary vectors have angular
C        separation of at least MARGIN from the plane containing
C        the current face.
C
         PASS1 =  .TRUE.
         OK    = .TRUE.
         M     =  1

         DO WHILE (  ( M .LE. N ) .AND. OK  )
C
C           Find the angular separation of CP and the Mth vector if the
C           latter is not an edge of the current face.
C            
            IF (  ( M .NE. I ) .AND. ( M .NE. NEXT )  ) THEN
               
               SEP = VSEP(  CP,  BOUNDS(1,M)  )

               IF ( PASS1 ) THEN
C
C                 Adjust CP if necessary so that it points
C                 toward the interior of the pyramid.
C                 
                  IF ( SEP .GT. HALFPI() ) THEN
C
C                    Invert the cross product vector and adjust SEP
C                    accordingly. Within this "M" loop, all other
C                    angular separations will be computed using the new
C                    value of CP.
C
                     CALL VSCLIP ( -1.D0, CP )

                     SEP = PI() - SEP

                  END IF

                  PASS1 = .FALSE.

               END IF

               OK  = SEP .LT. ( HALFPI() - MARGIN )

            END IF

            IF ( OK ) THEN
C
C              Consider the next boundary vector.
C
               M = M + 1

            END IF

         END DO

C
C        We've tested each boundary vector against the current face, or
C        else the loop terminated early because a vector with
C        insufficient angular separation from the plane containing the
C        face was found.
C
         IF ( OK ) THEN
C
C           The current face is exterior. It's bounded by rays I and
C           NEXT.
C
            XIDX  = I
            FOUND = .TRUE.

         ELSE
C
C           Look at the next face of the pyramid.
C         
            I = I + 1

         END IF

      END DO

C
C     If we didn't find an exterior face, we'll have to look at each
C     face bounded by a pair of rays, even if those rays are not
C     adjacent. (This can be a very slow process is N is large.)
C
      IF ( .NOT. FOUND ) THEN

         I = 1
      
         DO WHILE ( ( I .LE. N ) .AND. ( .NOT. FOUND ) ) 
C
C           Consider all ray pairs (I,NEXT) where NEXT > I.
C
            NEXT = I + 1

            DO WHILE (  ( NEXT .LE. N ) .AND. ( .NOT. FOUND )  ) 
C
C              Find the cross product of the first ray with the second.
C              If the current face is exterior, CP could be an inward
C              or outward normal, depending on the ordering of the
C              boundary vectors.
C
               CALL VCRSS ( BOUNDS(1,I), BOUNDS(1,NEXT), CP )

C
C              It's allowable for non-consecutive boundary vectors to
C              be linearly dependent, but if we have such a pair,
C              it doesn't define an exterior face.
C
               IF ( .NOT. VZERO(CP) ) THEN
C
C                 The rays having direction vectors indexed I and NEXT
C                 define a semi-infinite sector of a plane that might
C                 be of interest.
C
C                 Check whether all of the boundary vectors that are
C                 not edges of the current face have angular separation
C                 of at least MARGIN from the plane containing the
C                 current face.
C         
                  PASS1 = .TRUE.
                  OK    = .TRUE.
                  M     =  1

                  DO WHILE (  ( M .LE. N ) .AND. OK  )
C
C                    Find the angular separation of CP and the Mth
C                    vector if the latter is not an edge of the current
C                    face.
C            
                     IF (  ( M .NE. I ) .AND. ( M .NE. NEXT )  ) THEN

                        SEP = VSEP(  CP,  BOUNDS(1,M)  )

                        IF ( PASS1 ) THEN
C
C                          Adjust CP if necessary so that it points
C                          toward the interior of the pyramid.
C                 
                           IF ( SEP .GT. HALFPI() ) THEN
C
C                             Invert the cross product vector and
C                             adjust SEP accordingly. Within this "M"
C                             loop, all other angular separations will
C                             be computed using the new value of CP.
C

                              CALL VSCLIP ( -1.D0, CP )

                              SEP = PI() - SEP

                           END IF

                           PASS1 = .FALSE.

                        END IF

                        OK  = SEP .LT. ( HALFPI() - MARGIN )

                     END IF

                     IF ( OK ) THEN
C
C                       Consider the next boundary vector.
C
                        M = M + 1

                     END IF

                  END DO

C
C                 We've tested each boundary vector against the current
C                 face, or else the loop terminated early because a
C                 vector with insufficient angular separation from the
C                 plane containing the face was found.
C
                  IF ( OK ) THEN
C
C                    The current face is exterior. It's bounded by rays
C                    I and NEXT.

                     XIDX  = I
                     FOUND = .TRUE.
        
                  END IF
C 
C                 End of angular separation test block.
C
               END IF
C
C              End of non-zero cross product block.
C

               IF ( .NOT. FOUND ) THEN
C
C                 Look at the face bounded by the rays
C                 at indices I and NEXT+1.
C         
                  NEXT = NEXT + 1

               END IF

            END DO
C
C           End of NEXT loop.
C
            IF ( .NOT. FOUND ) THEN
C
C              Look at the face bounded by the pairs of rays
C              including the ray at index I+1.
C         
               I = I + 1

            END IF

         END DO
C
C        End of I loop.
C
      END IF
C
C     End of search for exterior face using each pair of rays.
C
C     If we still haven't found an exterior face, we can't continue.
C
      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'Unable to find face of convex hull '
     .   //            'of FOV of instrument #.'            )
         CALL ERRCH  ( '#', INST                            )
         CALL SIGERR ( 'SPICE(FACENOTFOUND)'                )
         CALL CHKOUT ( 'ZZHULLAX'                           )
         RETURN

      END IF

C
C     Arrival at this point means that the rays at indices
C     XIDX and NEXT define a plane such that all boundary 
C     vectors lie in a half-space bounded by that plane.
C
C     We're now going to define a set of orthonormal basis vectors:
C
C        +X  points along the angle bisector of the bounding vectors
C            of the exterior face.
C
C        +Y  points along CP.
C
C        +Z  is the cross product of +X and +Y.
C
C     We'll call the reference frame having these basis vectors
C     the "face frame."
C
C
      CALL VHAT ( BOUNDS(1,I),    RAY1 )
      CALL VHAT ( BOUNDS(1,NEXT), RAY2 )

      CALL VLCOM  ( 0.5D0, RAY1, 0.5D0, RAY2, XVEC )

      CALL VHATIP ( XVEC )
      CALL VHAT   ( CP,   YVEC )
      CALL UCRSS  ( XVEC, YVEC, ZVEC )

C
C     Create a transformation matrix to map the input boundary
C     vectors into the face frame.
C
      DO I = 1, 3

         TRANS(1,I) = XVEC(I)
         TRANS(2,I) = YVEC(I)
         TRANS(3,I) = ZVEC(I)
         
      END DO
C
C     Now we're going to compute the longitude of each boundary in the
C     face frame. The vectors with indices XIDX and NEXT are excluded.
C     We expect all longitudes to be between MARGIN and pi - MARGIN.
C
      MINLON =  PI()
      MAXLON =  0.D0
      MINIX  =  1
      MAXIX  =  1

      DO I = 1, N

         IF ( ( I .NE. XIDX ) .AND. ( I .NE. NEXT )  ) THEN
C
C           The current vector is not a boundary of our edge,
C           so find its longitude.
C
            CALL MXV ( TRANS, BOUNDS(1,I), V )

            CALL RECLAT ( V, R, LON, LAT )
C
C           Update the longitude bounds.
C
            IF ( LON .LT. MINLON ) THEN
               MINIX  = I
               MINLON = LON
            END IF

            IF ( LON .GT. MAXLON ) THEN
               MAXIX  = I
               MAXLON = LON
            END IF

         END IF

      END DO

C
C     If the longitude bounds are not as expected, don't try 
C     to continue.
C
      IF (  MINLON  .LT.  2*MARGIN  ) THEN

         CALL SETMSG ( 'Minimum boundary vector longitude in '
     .   //            'exterior face frame is # radians. '
     .   //            'Minimum occurs at index #. This FOV '
     .   //            'does not conform to the requirements '
     .   //            'of this routine. Instrument is #.'          )
         CALL ERRDP  ( '#', MINLON                                  )
         CALL ERRINT ( '#', MINIX                                   )
         CALL ERRCH  ( '#', INST                                    )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                        )
         CALL CHKOUT ( 'ZZHULLAX'                                   )
         RETURN

      ELSE IF ( MAXLON  .GT.  PI()-2*MARGIN  ) THEN

         CALL SETMSG ( 'Maximum boundary vector longitude in '
     .   //            'exterior face frame is # radians. '
     .   //            'Maximum occurs at index #. This FOV '
     .   //            'does not conform to the requirements '
     .   //            'of this routine. Instrument is #.'          )
         CALL ERRDP  ( '#', MAXLON                                  ) 
         CALL ERRINT ( '#', MAXIX                                   )
         CALL ERRCH  ( '#', INST                                    )
         CALL SIGERR ( 'SPICE(FOVTOOWIDE)'                          )
         CALL CHKOUT ( 'ZZHULLAX'                                   )
         RETURN

      END IF

C
C     Let delta represent the amount we can rotate the exterior
C     face clockwise about +Z without contacting another boundary
C     vector.
C     
      DELTA = PI() - MAXLON

C
C     Rotate +Y by -DELTA/2 about +Z. The result is our candidate
C     FOV axis. Make the axis vector unit length.
C
      CALL VROTV  ( YVEC, ZVEC, -DELTA/2, AXIS )      
      CALL VHATIP ( AXIS )

C
C     If we have a viable result, ALL boundary vectors have 
C     angular separation less than HALFPI-MARGIN from AXIS.
C
      DO I = 1, N

         SEP = VSEP( BOUNDS(1,I), AXIS )

         IF (  SEP  .GT. ( HALFPI() - MARGIN )  ) THEN
      
            CALL SETMSG ( 'Boundary vector at index # has '
     .      //            'angular separation of # radians '
     .      //            'from candidate FOV axis. This FOV '
     .      //            'does not conform to the requirements '
     .      //            'of this routine. Instrument is #.'    )
            CALL ERRINT ( '#', I                                 )
            CALL ERRDP  ( '#', SEP                               ) 
            CALL ERRCH  ( '#', INST                              )
            CALL SIGERR ( 'SPICE(FOVTOOWIDE)'                    )
            CALL CHKOUT ( 'ZZHULLAX'                             )
            RETURN

         END IF

      END DO

      CALL CHKOUT ( 'ZZHULLAX' )
      RETURN
      END
