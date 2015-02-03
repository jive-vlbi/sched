C$Procedure   ZZFOVAXI ( Generate an axis vector for polygonal FOV )
 
      SUBROUTINE ZZFOVAXI ( INST, N, BOUNDS, AXIS )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Generate an axis of an instrument's polygonal FOV such that all
C     of the FOV's boundary vectors have angular separation of strictly
C     less than pi/2 radians from this axis.
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
C                The boundary vectors must satisfy the constraints:
C
C                   1)  The boundary vectors must be contained within
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
C     This routine first computes the average of the unitized input
C     boundary vectors; if this vector satisfies the angular separation
C     constraint (1) in Detailed_Input, a unit length copy of this
C     vector is returned as the FOV axis.
C
C     If the procedure above fails, an algorithm based on selection
C     of a suitable face of the boundary vector's convex hull is tried.
C     See the routine ZZHULLAX for details.
C     
C     If the second approach fails, an error is signaled.
C
C     Note that it's easy to construct FOVs where the average of the
C     boundary vectors doesn't yield a viable axis: a FOV of angular
C     width nearly equal to pi radians, with a sufficiently large
C     number of boundary vectors on one side and few boundary vectors
C     on the other, is one such example. This routine can find an
C     axis for many such intractable FOVs---that's why ZZHULLAX
C     is called after the simple approach fails.
C
C$ Examples
C
C     See SPICELIB private routine ZZGFFVIN.
C
C$ Restrictions
C
C     1) This is a SPICE private routine. User applications should not
C        call this routine.
C
C     2) There may "reasonable" polygonal FOVs that cannot be handled
C        by this routine. See the discussions in Detailed_Input,
C        Exceptions, and Particulars above for restrictions on the
C        input set of FOV boundary vectors.
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
C-    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB)
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      VSEP
 
      LOGICAL               FAILED
      LOGICAL               RETURN
      LOGICAL               VZERO

C
C     Local parameters
C

C
C     Local variables
C      
      DOUBLE PRECISION      CP     ( 3 )
      DOUBLE PRECISION      LIMIT
      DOUBLE PRECISION      SEP
      DOUBLE PRECISION      V      ( 3 )
      DOUBLE PRECISION      UVEC   ( 3 )

      INTEGER               I
      INTEGER               NEXT
      
      LOGICAL               OK


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZFOVAXI' )

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
         CALL CHKOUT ( 'ZZFOVAXI'                          )
         RETURN

      END IF


C
C     Check for linearly dependent consecutive boundary vectors.
C
      DO I = 1, N
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
C        in the case the current face is is exterior.
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
            CALL CHKOUT ( 'ZZFOVAXI'                          )
            RETURN

         END IF

      END DO

C
C     First try the average of the FOV unit boundary vectors as
C     a candidate axis. In many cases, this simple approach
C     does the trick.
C
      CALL CLEARD ( 3, AXIS )

      DO I = 1, N

         CALL VHAT ( BOUNDS(1,I), UVEC    )
         CALL VADD ( UVEC,        AXIS, V )
         CALL VEQU ( V,           AXIS    )

      END DO

      CALL VSCLIP ( 1.D0/N, AXIS )

C
C     If each boundary vector has sufficiently small
C     angular separation from AXIS, we're done.
C
      LIMIT = HALFPI() - MARGIN

      OK    = .TRUE.
      I     = 1

      DO WHILE (  ( I .LE. N ) .AND.  OK  ) 

         SEP = VSEP( BOUNDS(1,I), AXIS )

         IF ( SEP .GT.  LIMIT  ) THEN
            OK = .FALSE.
         ELSE
            I  =  I + 1
         END IF

      END DO


      IF ( .NOT. OK ) THEN
C
C        See whether we can find an axis using a
C        method based on finding a face of the convex 
C        hull of the FOV. ZZHULLAX signals an error
C        if it doesn't succeed.
C
         CALL ZZHULLAX ( INST, N, BOUNDS, AXIS )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZFOVAXI' )
            RETURN
         END IF

      END IF

C
C     At this point AXIS is valid. Make the axis vector unit length.
C
      CALL VHATIP ( AXIS )

      CALL CHKOUT ( 'ZZFOVAXI' )
      RETURN
      END
