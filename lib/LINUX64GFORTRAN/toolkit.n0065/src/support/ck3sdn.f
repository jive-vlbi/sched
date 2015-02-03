C$Procedure  CK3SDN ( Down sample type 3 CK data prepared for writing )
 
      SUBROUTINE CK3SDN ( SDNTOL, AVFLAG,
     .                   NREC, SCLKDP, QUATS, AVVS, 
     .                   NINTS, STARTS, DPARR, INTARR )
 
C$ Abstract
C
C     Down sample type 3 CK data prepared for writing.
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
C     DAF
C     ROTATIONS
C     SCLK
C
C$ Keywords
C
C     POINTING
C     UTILITY
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      SDNTOL
      LOGICAL               AVFLAG
      INTEGER               NREC
      DOUBLE PRECISION      SCLKDP (      * )
      DOUBLE PRECISION      QUATS  ( 0:3, * )
      DOUBLE PRECISION      AVVS   (   3, * )
      INTEGER               NINTS
      DOUBLE PRECISION      STARTS (      * )
      DOUBLE PRECISION      DPARR  (      * )
      INTEGER               INTARR (      * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     SDNTOL     I   Tolerance used for sampling down.
C     AVFLAG     I   True if angular velocity data is set.
C     NREC      I/O  Number of pointing records.
C     SCLKDP    I/O  Encoded SCLK times.
C     QUATS     I/O  Quaternions representing instrument pointing.
C     AVVS      I/O  Angular velocity vectors.
C     NINTS      I   Number of intervals.
C     STARTS     I   Encoded SCLK interval start times.
C     DPARR      I   Double precision work array.
C     INTARR     I   Integer work array.
C
C$ Detailed_Input
C
C     SDNTOL     is the angular tolerance, in radians, to be used to 
C                down sample the input CK type 3 pointing data.
C                SDNTOL must be a non-negative number.
C
C     AVFLAG     is a logical flag indicating whether or not 
C                the angular velocity data should be processed.
C
C     NREC       is the number of pointing instances in the input 
C                buffer.
C
C     SCLKDP     are the encoded spacecraft clock times associated with
C                each pointing instance. These times must be strictly
C                increasing.
C
C     QUATS      is the quaternion buffer.
C
C     AVVS       is the angular velocity vector buffer.
C
C                If AVFLAG is FALSE then this array is ignored by the
C                routine; however it still must be supplied as part of
C                the calling sequence.
C
C     NINTS      is the number of intervals that the pointing instances
C                are partitioned into.
C
C     STARTS     are the start times of each of the interpolation
C                intervals. These times must be strictly increasing
C                and must coincide with times for which the input
C                quaternion buffer contains pointing.
C
C     DPARR      is a double precision work array.
C
C     INTARR     is an integer work array.
C
C$ Detailed_Output
C
C     NREC       is the number of pointing instances in the buffer
C                after down sampling.
C
C     SCLKDP     is the encoded spacecraft clock time buffer after 
C                down sampling.
C
C     QUATS      is the quaternion buffer after down sampling.
C
C     AVVS       is the angular velocity vector buffer after down
C                sampling.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C      1) If the number of pointing records is not greater than zero, 
C         the error SPICE(INVALIDNUMBEROFRECORDS) is signaled.
C
C      2) If the number of interval starts is not greater than zero, 
C         the error SPICE(INVALIDNUMBEROFINTERVALS) is signaled.
C
C      3) If the number of interval starts is not is not less than 
C         or equal to the number of records, the error 
C         SPICE(BUFFERSIZESMISMATCH) is signaled.
C
C      4) If the first interval start time is not the same as the 
C         first record time, the error SPICE(FIRSTRECORDMISMATCH) 
C         is signaled.
C
C      5) If the down sampling tolerance is not a non-negative number, 
C         the error SPICE(BADDOWNSAMPLINGTOL) is signaled.
C
C      6) If record times buffer does not contain any of the times 
C         from interval start times buffers, the error 
C         SPICE(INTERVALSTARTNOTFOUND) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine eliminates from the input quaternion and angular
C     rate buffers all data points for which type 3 CK interpolation
C     between bounding points that are not eliminated would produce
C     result that is within specified tolerance of the input attitude.
C     The elimination, referred to in these comments as "down sampling",
C     is done within each individual interpolation interval (as
C     specified in the input interval starts buffer), with intervals
C     boundaries unchanged.
C
C$ Examples
C
C     Normally this routine would be called immediately before the
C     CKW03 is called and be supplied with the input time, quaternion,
C     angular rate, and interval start buffers that were fully and
C     properly prepared for the CKW03 input, like this:
C
C         CALL CK3SDN ( SDNTOL, ARFLAG,
C        .              NREC, SCLKDP, QUATS, AVVS, NINTS, STARTS, 
C        .              DPARR, INTARR )
C               
C         CALL CKW03  ( HANDLE, SCLKDP(1), SCLKDP(NREC), 
C        .              INSTID, FRMNAM, ARFLAG, SEGID,  
C        .              NREC, SCLKDP, QUATS, AVVS, NINTS, STARTS )
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
C     N.J. Bachman    (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    Beta Version 1.2.0, 04-JUN-2012 (BVS)
C
C        BUG FIX: changed the end-point selection algorithm to not
C        consider end-point quaternions that are close to 180 degrees
C        apart to prevent cases in which the quaternion (QMINI) and
C        matrix (LINROT_M-like) interpolation algorithms produce
C        rotations in the opposite directions due to numerics.
C
C-    Beta Version 1.1.0, 19-SEP-2005 (BVS)(FST)
C
C        Incorporated Scott's shrinking window search algorithm to
C        speed up down sampling.
C
C-    Beta Version 1.0.0, 29-JUL-2005 (BVS)(NJB)
C
C-&
 
C$ Index_Entries
C
C     sample down ck type_3 pointing data prepared for writing
C
C-&

C
C     SPICELIB functions.
C
      DOUBLE PRECISION      BRCKTD
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      VDOT

C
C     Local parameters.
C
C     Tolerance for 180 separation check, in radians. 
C
      DOUBLE PRECISION      ANGTOL
      PARAMETER           ( ANGTOL = 0.001D0 )
 
C
C     Local variables.
C
      DOUBLE PRECISION      FRAC
      DOUBLE PRECISION      QINTRP ( 0 : 3 )
      DOUBLE PRECISION      QKEEPF ( 0 : 3 )
      DOUBLE PRECISION      QKEEPL ( 0 : 3 )
      DOUBLE PRECISION      QLNEG  ( 0 : 3 )
      DOUBLE PRECISION      QLINPT ( 0 : 3 )
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      DIST2
      DOUBLE PRECISION      DIST2A
      DOUBLE PRECISION      DIST2B
      DOUBLE PRECISION      DPOS
      DOUBLE PRECISION      DNEG
      DOUBLE PRECISION      COSVAL

      INTEGER               I
      INTEGER               J
      INTEGER               INTNRF
      INTEGER               INTCRF
      INTEGER               INTCRL
      INTEGER               KEEPF
      INTEGER               KEEPL
      INTEGER               NDROPD

      INTEGER               LEFT
      INTEGER               RIGHT

      LOGICAL               SKIPIT
      LOGICAL               FITOK

C
C     SPICELIB functions.
C
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      VDISTG
      INTEGER               BSRCHD
      LOGICAL               RETURN

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CK3SDN' )
      END IF

C
C     Let's do some sanity checks that needed to make sure that future
C     loops and comparisons don't blow up. First, verify that the
C     number pointing records is greater that zero.
C
      IF ( NREC .LE. 0 ) THEN
         CALL SETMSG ( 'The number of pointing records must be ' //
     .                 'greater than zero. It was #.'             )
         CALL ERRINT ( '#', NREC                                  )
         CALL SIGERR ( 'SPICE(INVALIDNUMBEROFRECORDS)'            )
         CALL CHKOUT ( 'CK3SDN'                                   )
         RETURN
      END IF

C
C     Then, verify that the number intervals is greater that zero.
C
      IF ( NINTS .LE. 0 ) THEN
         CALL SETMSG ( 'The number of interval starts must be '  //
     .                 'greater than zero. It was #.'             )
         CALL ERRINT ( '#', NINTS                                 )
         CALL SIGERR ( 'SPICE(INVALIDNUMBEROFINTERVALS)'          )
         CALL CHKOUT ( 'CK3SDN'                                   )
         RETURN
      END IF

C
C     Then, verify that the number intervals is less than or equal to
C     the number of records.
C
      IF ( NINTS .GT. NREC ) THEN
         CALL SETMSG ( 'The number of interval starts, #, '      //
     .                 'is not less than or equal to the '       //
     .                 'number of records, #.'                    )
         CALL ERRINT ( '#', NINTS                                 )
         CALL ERRINT ( '#', NREC                                  )
         CALL SIGERR ( 'SPICE(BUFFERSIZESMISMATCH)'               )
         CALL CHKOUT ( 'CK3SDN'                                   )
         RETURN
      END IF

C
C     Then verify that the first time in the intervals array is the same
C     as the first time in the records array.
C     
      IF ( SCLKDP(1) .NE. STARTS(1) ) THEN
         CALL SETMSG ( 'The first interval start time, #, '      //
     .                 'is not the same as the first record '    //
     .                 'time, #.'                                 )
         CALL ERRDP  ( '#', SCLKDP(1)                             )
         CALL ERRDP  ( '#', STARTS(1)                             )
         CALL SIGERR ( 'SPICE(FIRSTRECORDMISMATCH)'               )
         CALL CHKOUT ( 'CK3SDN'                                   )
         RETURN         
      END IF

C
C     Finally verify that input down sampling tolerance is not positive
C     number.
C     
      IF ( SDNTOL .LT. 0.D0 ) THEN
         CALL SETMSG ( 'The down sampling tolerance must '        //
     .                 'be a non-negative number. It was #.'      )
         CALL ERRDP  ( '#', SDNTOL                                )
         CALL SIGERR ( 'SPICE(BADDOWNSAMPLINGTOL)'                )
         CALL CHKOUT ( 'CK3SDN'                                   )
         RETURN         
      END IF

C
C     This variable will hold to the index of the pointing record that
C     matches the start of the next interval. For the first interval
C     it is set to one.
C
      INTNRF = 1

C
C     We will count the number of points that were dropped.
C
      NDROPD = 0

C
C     Loop through interpolation intervals.
C
      DO I = 1, NINTS

C
C        Assign the index of the pointing record that matches the 
C        begin time of this interval.
C
         INTCRF = INTNRF

C
C        Find the index of the pointing record that ends this interval.
C        If this the last interval, it is the last pointing record in
C        pointing buffer.
C
         IF ( I .EQ. NINTS ) THEN
            INTCRL = NREC
         ELSE
            
C           
C           This is not the last interval. To get its end time we need
C           to find the pointing record that matches the start of the
C           next interval and pick the record before it.
C
C           First we find index of the pointing record that corresponds 
C           to the start of the next interval.
C           
            INTNRF = BSRCHD ( STARTS( I + 1 ), NREC - INTCRF + 1, 
     .                        SCLKDP( INTCRF ) )


            IF ( INTNRF .NE. 0 ) THEN

C
C              Found index must be adjusted to be relative to the
C              beginning of the buffer. Currently it is relative to the
C              start of the current interval.
C
               INTNRF = INTNRF + INTCRF - 1
C
C              The index of the last record belonging to this interval
C              in the found index minus 1.
C
               INTCRL = INTNRF - 1

            ELSE

C
C              We did not find such record. The input buffer must have
C              been formed improperly for this to happen. Signal an
C              error.
C
               CALL SETMSG ( 'Cannot find pointing record with '  //
     .                       'time that matches the start time '  //
     .                       '# (encoded SCLK ticks) of the '     //
     .                       'interpolation interval number #.'    )
               CALL ERRDP  ( '#', STARTS( I + 1 )                  )
               CALL ERRINT ( '#', I + 1                            )
               CALL SIGERR ( 'SPICE(INTERVALSTARTNOTFOUND)'        )
               CALL CHKOUT ( 'CK3SDN'                              )
               RETURN
               
            END IF
            
         END IF

C
C        Let's look at the indexes of the pointing records
C        corresponding to the begin and end of this interval. If they
C        are the same (meaning it's a singleton interval) or if they
C        are next to each other (meaning that the whole set of
C        interval's pointing data is comprised of only its begin
C        and end points) there is no down sampling to do.
C
         SKIPIT = ( INTCRF .EQ. INTCRL     ) .OR. 
     .            ( INTCRF .EQ. INTCRL - 1 ) 

C
C        Set initial values for a binary search.
C
         KEEPF = INTCRF
         LEFT  = INTCRF
         RIGHT = INTCRL

         DO WHILE ( .NOT. SKIPIT .AND. KEEPF .LT. INTCRL )

C
C           Set the right endpoint of the interval by dividing the
C           binary search region in half.
C
            KEEPL = ( LEFT + RIGHT ) / 2

C
C           Unitize bracketing quaternions as QMINI seems to be 
C           very sensitive to that. :)
C
            CALL VHATG ( QUATS( 0, KEEPF ), 4, QKEEPF )
            CALL VHATG ( QUATS( 0, KEEPL ), 4, QKEEPL )

C
C           Pick the closer of the right quaternion or its negative to
C           QKEEPF for input into QMINI to ensure that QMINI does
C           interpolation in the "shortest arc" direction.
C
            CALL VMINUG( QKEEPL, 4, QLNEG )

            DPOS = VDISTG( QKEEPL, QKEEPF, 4 )
            DNEG = VDISTG( QLNEG,  QKEEPF, 4 )

            IF ( DNEG .LT. DPOS ) THEN
               CALL MOVED( QLNEG,  4, QLINPT )
            ELSE
               CALL MOVED( QKEEPL, 4, QLINPT )
            END IF

C
C           If the currently picked window ends are not 180 degrees
C           apart, check all records between them to see if
C           interpolated pointing is within tolerance of the actual
C           pointing. If the currently picked window ends are close to
C           180 degrees apart, don't consider them as a possibility.
C
            COSVAL = QKEEPF(0)*QLINPT(0) + VDOT(QKEEPF(1),QLINPT(1))

            ANGLE = DACOS ( BRCKTD ( COSVAL, -1.D0, 1.D0 ) ) * 2.D0

            FITOK = ABS ( PI() - ANGLE ) .GT. ANGTOL

            J = KEEPF + 1

            DO WHILE ( J .LE. ( KEEPL - 1 ) .AND. FITOK )

C
C              Compute interpolation fraction for this pointing record.
C
               IF ( SCLKDP( KEEPL ) - SCLKDP( KEEPF ) .NE. 0.D0 ) THEN
                  FRAC = ( SCLKDP( J )     - SCLKDP( KEEPF ) ) / 
     .                   ( SCLKDP( KEEPL ) - SCLKDP( KEEPF ) )
               ELSE                  
                  CALL SIGERR ( 'SPICE(CK3SDNBUG)'                 )
                  CALL CHKOUT ( 'CK3SDN'                           )
                  RETURN
               END IF

C
C              Call Nat's fast quaternion interpolation routine to 
C              compute interpolated rotation for this point.
C
               CALL QMINI ( QKEEPF, QLINPT, FRAC, QINTRP )

C
C              Find the squared distance between the interpolated 
C              and input quaternions.
C
               DIST2A = (QUATS(0,J)-QINTRP(0)) * (QUATS(0,J)-QINTRP(0))
     .                + (QUATS(1,J)-QINTRP(1)) * (QUATS(1,J)-QINTRP(1))
     .                + (QUATS(2,J)-QINTRP(2)) * (QUATS(2,J)-QINTRP(2))
     .                + (QUATS(3,J)-QINTRP(3)) * (QUATS(3,J)-QINTRP(3))

               DIST2B = (QUATS(0,J)+QINTRP(0)) * (QUATS(0,J)+QINTRP(0))
     .                + (QUATS(1,J)+QINTRP(1)) * (QUATS(1,J)+QINTRP(1))
     .                + (QUATS(2,J)+QINTRP(2)) * (QUATS(2,J)+QINTRP(2))
     .                + (QUATS(3,J)+QINTRP(3)) * (QUATS(3,J)+QINTRP(3))

               DIST2 = MIN( DIST2A, DIST2B )

C
C              The rotation angle theta is related to the distance by
C              the formula
C
C                 || Q1 - Q2 ||     =  2 * | sin(theta/4) |
C
               ANGLE = 4.D0 * ASIN( MIN( SQRT(DIST2)/2.D0, 1.D0 ) )

C
C              Compare the angle with specified threshold.
C
               FITOK = FITOK .AND. ( ABS( ANGLE ) .LE. SDNTOL )

C
C              Increment index to move to the next record.
C
               J = J + 1

            END DO

C
C           Was the fit OK?
C
            IF ( FITOK ) THEN

C
C              Fit was OK. Check if left and right are equal; if so we
C              found the point that were were looking for.
C              
               IF ( LEFT .EQ. RIGHT ) THEN

C
C                 Mark all records between fist and last with DPMAX.
C
                  DO J = KEEPF+1, KEEPL-1
                     SCLKDP(J) = DPMAX()
                     NDROPD = NDROPD + 1
                  END DO

C
C                 Set first point for the next search to be equal to
C                 the to the found point.
C
                  KEEPF = KEEPL
                  
C
C                 Reset window boundaries for binary search.
C
                  LEFT  = KEEPL
                  RIGHT = INTCRL                  

               ELSE

C
C                 Left and right sides haven't converged yet; shift
C                 left side of the binary search window forward.
C
                  LEFT = KEEPL + 1

               END IF
               
            ELSE

C
C              No fit; shift right side of the binary search window
C              backwards.
C
               RIGHT = KEEPL - 1

C
C              If right side went "over" the left side, set left side
C              to be equal to the right side.
C
               IF ( RIGHT .LT. LEFT ) THEN
                  LEFT = RIGHT
               END IF

            END IF

         END DO

      END DO

C
C     At this point all records that are to be removed, if any, have
C     been "tagged" with DPMAX in the times buffer. We need to re-sort
C     the buffers to push these records to the bottom and re-set the
C     number of records to indicate that only the top portion should be
C     used.
C
      IF ( NDROPD .NE. 0 ) THEN

C
C        Since SCLKs were the ones "marked" by DPMAX, we will use them
C        to get the order vector.
C
         CALL ORDERD ( SCLKDP, NREC, INTARR )

C
C        Now, with the order vector in hand, sort the SCLKs ...
C
         CALL REORDD ( INTARR, NREC, SCLKDP )

C
C        ... then sort quaternions (element by element) ...
C
         DO I = 0, 3
            DO J = 1, NREC
               DPARR( J ) = QUATS ( I, J )
            END DO
            CALL REORDD ( INTARR, NREC, DPARR )
            DO J = 1, NREC
               QUATS ( I, J ) = DPARR( J )
            END DO
         END DO

C
C        ... and, finally, if requested, sort AVs (also element by
C        element) ...
C
         IF ( AVFLAG ) THEN
            DO I = 1, 3
               DO J = 1, NREC
                  DPARR( J ) = AVVS ( I, J )
               END DO
               CALL REORDD ( INTARR, NREC, DPARR )
               DO J = 1, NREC
                  AVVS ( I, J ) = DPARR( J )
               END DO
            END DO
         END IF

C
C        Reset the number of points.
C
         NREC = NREC - NDROPD

      END IF
         
C
C     All done. Check out.
C
      CALL CHKOUT ( 'CK3SDN' )

      RETURN

      END
