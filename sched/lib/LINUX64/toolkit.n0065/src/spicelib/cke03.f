C$Procedure CKE03  ( C-kernel, evaluate pointing record, data type 3 )
 
      SUBROUTINE CKE03 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
 
C$ Abstract
C
C   Evaluate a pointing record returned by CKR03 from a CK type 3
C   segment. Return the C-matrix and angular velocity vector associated
C   with the time CLKOUT.
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
C   CK
C     ROTATION
C
C$ Keywords
C
C   POINTING
C
C$ Declarations
 
      LOGICAL               NEEDAV
      DOUBLE PRECISION      RECORD ( *     )
      DOUBLE PRECISION      CMAT   ( 3, 3  )
      DOUBLE PRECISION      AV     ( 3     )
      DOUBLE PRECISION      CLKOUT
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NEEDAV     I   True if angular velocity is requested.
C     RECORD     I   Data type 3 pointing record.
C     CMAT       O   C-matrix.
C     AV         O   Angular velocity vector.
C     CLKOUT     O   SCLK associated with C-matrix.
C
C$ Detailed_Input
C
C     NEEDAV     is true if angular velocity is requested.
C
C     RECORD     is a set of double precision numbers returned by CKR03
C                that contain sufficient information from a type 3 CK
C                segment to evaluate the C-matrix and the angular
C                velocity vector at a particular time.  Depending on
C                the contents of RECORD, this routine will either
C                interpolate between two pointing instances that
C                bracket a request time, or it will simply return the
C                pointing given by a single pointing instance.
C
C                When pointing at the request time can be determined
C                by linearly interpolating between the two pointing
C                instances that bracket that time, the bracketing
C                pointing instances are returned in RECORD as follows:
C
C                   RECORD( 1  ) = Left bracketing SCLK time.
C
C                   RECORD( 2  ) = lq0  \
C                   RECORD( 3  ) = lq1   \    Left bracketing
C                   RECORD( 4  ) = lq2   /      quaternion.
C                   RECORD( 5  ) = lq3  /
C
C                   RECORD( 6  ) = lav1 \     Left bracketing
C                   RECORD( 7  ) = lav2  |    angular velocity
C                   RECORD( 8  ) = lav3 /       ( optional )
C
C                   RECORD( 9  ) = Right bracketing SCLK time.
C
C                   RECORD( 10 ) = rq0  \
C                   RECORD( 11 ) = rq1   \    Right bracketing
C                   RECORD( 12 ) = rq2   /       quaternion.
C                   RECORD( 13 ) = rq3  /
C
C                   RECORD( 14 ) = rav1 \     Right bracketing
C                   RECORD( 15 ) = rav2  |    angular velocity
C                   RECORD( 16 ) = rav3 /       ( optional )
C
C                   RECORD( 17 ) = pointing request time
C
C                The quantities lq0 - lq3 and rq0 - rq3 are the
C                components of the quaternions that represent the
C                C-matrices associated with the times that bracket
C                the requested time.
C
C                The quantities lav1, lav2, lav3 and rav1, rav2, rav3
C                are the components of the angular velocity vectors at
C                the respective bracketing times. The components of the
C                angular velocity vectors are specified relative to the
C                inertial reference frame of the segment.
C
C                When the routine is to simply return the pointing
C                given by a particular pointing instance, then the
C                values of that pointing instance are returned in both
C                parts of RECORD ( i.e. RECORD(1-9) and RECORD(10-16) ).
C
C$ Detailed_Output
C
C     CMAT       is a rotation matrix that transforms the components
C                of a vector expressed in the inertial frame given in
C                the segment to components expressed in the instrument
C                fixed frame at the returned time.
C
C                Thus, if a vector v has components x, y, z in the
C                inertial frame, then v has components x', y', z' in the
C                instrument fixed frame where:
C
C                     [ x' ]     [          ] [ x ]
C                     | y' |  =  |   CMAT   | | y |
C                     [ z' ]     [          ] [ z ]
C
C                If the x', y', z' components are known, use the
C                transpose of the C-matrix to determine x, y, z as
C                follows.
C
C                     [ x ]      [          ]T    [ x' ]
C                     | y |  =   |   CMAT   |     | y' |
C                     [ z ]      [          ]     [ z' ]
C                              (Transpose of CMAT)
C
C     AV         is the angular velocity vector of the instrument fixed
C                frame defined by CMAT.  The angular velocity is
C                returned only if NEEDAV is true.
C
C                The direction of the angular velocity vector gives
C                the right-handed axis about which the instrument fixed
C                reference frame is rotating. The magnitude of AV is
C                the magnitude of the instantaneous velocity of the
C                rotation, in radians per second.
C
C                The angular velocity vector is returned in component
C                form
C
C                         AV = [ AV1  , AV2  , AV3  ]
C
C                which is in terms of the inertial coordinate frame
C                specified in the segment descriptor.
C
C     CLKOUT     is the encoded SCLK associated with the returned
C                C-matrix and angular velocity vector.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) No explicit checking is done to determine whether RECORD is
C        valid.  However, routines in the call tree of this routine
C        may signal errors if inputs are invalid or otherwise
C        in appropriate.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     If the array RECORD contains pointing instances that bracket the
C     request time then CKE03 will linearly interpolate between those
C     two values to obtain pointing at the request time.  If the
C     pointing instances in RECORD are for the same time, then this
C     routine will simply unpack the record and convert the quaternion
C     to a C-matrix.
C
C     The linear interpolation performed by this routine is defined
C     as follows:
C
C     1)  Let t be the time for which pointing is requested and
C         let CMAT1 and CMAT2 be C-matrices associated with times
C         t1 and t2 where:
C
C                t1 < t2,  and  t1 <= t,  and  t <= t2.
C
C     2)  Assume that the spacecraft frame rotates about a fixed
C         axis at a constant angular rate from time t1 to time t2.
C         The angle and rotation axis can be obtained from the
C         rotation matrix ROT12 where:
C
C                            T                       T
C                       CMAT2   =  ROT12    *   CMAT1
C
C            or
C                                       T
C                       ROT12   =  CMAT2    *   CMAT1
C
C
C                       ROT12   ==> ( ANGLE, AXIS )
C
C
C     3)  To obtain pointing at time t, rotate the spacecraft frame
C         about the vector AXIS from its orientation at time t1 by the
C         angle THETA where:
C
C                                            ( t  - t1 )
C                       THETA  =  ANGLE  *   -----------
C                                            ( t2 - t1 )
C
C     4)  Thus if ROT1t is the matrix that rotates vectors by the
C         angle THETA about the vector AXIS, then the output C-matrix
C         is given by:
C
C                           T                     T
C                       CMAT  =  ROT1t   *   CMAT1
C
C                                                 T
C                       CMAT  =  CMAT1   *   ROT1t
C
C
C     5)  The angular velocity is treated independently of the
C         C-matrix. If it is requested, then the AV at time t is
C         the weighted average of the angular velocity vectors at
C         the times t1 and t2:
C
C                          ( t  - t1 )
C                    W  =  -----------
C                          ( t2 - t1 )
C
C
C                    AV  = ( 1 - W ) * AV1   +   W * AV2
C
C$ Examples
C
C     The CKRnn routines are usually used in tandem with the CKEnn
C     routines, which evaluate the record returned by CKRnn to give
C     the pointing information and output time.
C
C     The following code fragment searches through all of the segments
C     in a file applicable to the Mars Observer spacecraft bus that
C     are of data type 3, for a particular spacecraft clock time.
C     It then evaluates the pointing for that epoch and prints the
C     result.
C
C           CHARACTER*(20)        SCLKCH
C           CHARACTER*(20)        SCTIME
C           CHARACTER*(40)        IDENT
C
C           INTEGER               I
C           INTEGER               SC
C           INTEGER               INST
C           INTEGER               HANDLE
C           INTEGER               DTYPE
C           INTEGER               ICD      (    6 )
C
C           DOUBLE PRECISION      SCLKDP
C           DOUBLE PRECISION      TOL
C           DOUBLE PRECISION      CLKOUT
C           DOUBLE PRECISION      DESCR    (    5 )
C           DOUBLE PRECISION      DCD      (    2 )
C           DOUBLE PRECISION      RECORD   (   17 )
C           DOUBLE PRECISION      CMAT     ( 3, 3 )
C           DOUBLE PRECISION      AV       (    3 )
C
C           LOGICAL               NEEDAV
C           LOGICAL               FND
C           LOGICAL               SFND
C
C
C           SC     = -94
C           INST   = -94000
C           DTYPE  =  3
C           NEEDAV = .FALSE.
C
C     C
C     C     Load the MO SCLK kernel and the C-kernel.
C     C
C           CALL FURNSH ( 'MO_SCLK.TSC'       )
C           CALL DAFOPR ( 'MO_CK.BC',  HANDLE )
C     C
C     C     Get the spacecraft clock time. Then encode it for use
C     C     in the C-kernel.
C     C
C           WRITE (*,*) 'Enter spacecraft clock time string:'
C           READ (*,FMT='(A)') SCLKCH
C
C           CALL SCENCD ( SC, SCLKCH, SCLKDP )
C     C
C     C     Use a tolerance of 2 seconds ( half of the nominal
C     C     separation between MO pointing instances ).
C     C
C           CALL SCTIKS ( SC, '0000000002:000', TOL )
C
C     C
C     C     Search from the beginning of the CK file through all
C     C     of the segments.
C     C
C           CALL DAFBFS ( HANDLE )
C           CALL DAFFNA ( SFND   )
C
C           FND    = .FALSE.
C
C           DO WHILE ( ( SFND ) .AND. ( .NOT. FND ) )
C
C     C
C     C        Get the segment identifier and descriptor.
C     C
C
C              CALL DAFGN ( IDENT                 )
C              CALL DAFGS ( DESCR                 )
C     C
C     C        Unpack the segment descriptor into its integer and
C     C        double precision components.
C     C
C              CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
C
C     C
C     C        Determine if this segment should be processed.
C     C
C              IF ( ( INST          .EQ. ICD( 1 ) ) .AND.
C          .        ( SCLKDP + TOL  .GE. DCD( 1 ) ) .AND.
C          .        ( SCLKDP - TOL  .LE. DCD( 2 ) ) .AND.
C          .        ( DTYPE         .EQ. ICD( 3 ) )      ) THEN
C
C
C                 CALL CKR03 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
C          .                   RECORD, FND )
C
C                 IF ( FND ) THEN
C
C                    CALL CKE03 (NEEDAV,RECORD,CMAT,AV,CLKOUT)
C
C                    CALL SCDECD ( SC, CLKOUT, SCTIME )
C
C                    WRITE (*,*)
C                    WRITE (*,*) 'Segment identifier: ', IDENT
C                    WRITE (*,*)
C                    WRITE (*,*) 'Pointing returned for time: ',
C          .                      SCTIME
C                    WRITE (*,*)
C                    WRITE (*,*) 'C-matrix:'
C                    WRITE (*,*)
C                    WRITE (*,*) ( CMAT(1,I), I = 1, 3 )
C                    WRITE (*,*) ( CMAT(2,I), I = 1, 3 )
C                    WRITE (*,*) ( CMAT(3,I), I = 1, 3 )
C                    WRITE (*,*)
C
C                 END IF
C
C              END IF
C
C              CALL DAFFNA ( SFND )
C
C           END DO
C
C$ Restrictions
C
C     1) No explicit checking is done on the input RECORD.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     J.M. Lynch     (JPL)
C     F.S. Turner    (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.1, 22-AUG-2006 (EDW)
C
C        Replaced references to LDPOOL with references 
C        to FURNSH.
C
C-    SPICELIB Version 2.0.0, 13-JUN-2002 (FST)
C
C        This routine now participates in error handling properly.
C
C-    SPICELIB Version 1.0.0, 25-NOV-1992 (JML)
C
C-&
 
C$ Index_Entries
C
C     evaluate ck type_3 pointing data record
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 13-JUN-2002 (FST)
C
C        Calls to CHKIN and CHKOUT in the standard SPICE error
C        handling style were added.  Versions prior to 2.0.0
C        were error free, however changes to RAXISA from error
C        free to error signaling forced this update.
C
C        Additionally, FAILED is now checked after the call to
C        RAXISA.  This prevents garbage from being placed into
C        the output arguments.
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
 
      DOUBLE PRECISION      T
      DOUBLE PRECISION      T1
      DOUBLE PRECISION      T2
      DOUBLE PRECISION      Q1    (    4 )
      DOUBLE PRECISION      Q2    (    4 )
      DOUBLE PRECISION      CMAT1 ( 3, 3 )
      DOUBLE PRECISION      CMAT2 ( 3, 3 )
      DOUBLE PRECISION      AV1   (    3 )
      DOUBLE PRECISION      AV2   (    3 )
      DOUBLE PRECISION      AXIS  (    3 )
      DOUBLE PRECISION      ROT   ( 3, 3 )
      DOUBLE PRECISION      DELTA ( 3, 3 )
      DOUBLE PRECISION      FRAC
      DOUBLE PRECISION      ANGLE
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKE03' )
      END IF
 
C
C     Unpack the record, for easier reading.
C
      T    = RECORD(17)
      T1   = RECORD(1 )
      T2   = RECORD(9 )
 
      CALL MOVED ( RECORD(2),  4, Q1   )
      CALL MOVED ( RECORD(6),  3, AV1  )
      CALL MOVED ( RECORD(10), 4, Q2   )
      CALL MOVED ( RECORD(14), 3, AV2  )
 
C
C     If T1 and T2 are the same then no interpolation or extrapolation
C     is performed.  Simply convert the quaternion to a C-matrix and
C     return.
C
      IF ( T1 .EQ. T2 ) THEN
 
         CALL Q2M ( Q1, CMAT )
 
         CLKOUT = T1
 
         IF ( NEEDAV ) THEN
            CALL VEQU ( AV1, AV )
         END IF
 
         CALL CHKOUT ( 'CKE03' )
 
         RETURN
 
      END IF
 
C
C     Interpolate between the two pointing instances to obtain pointing
C     at the request time.
C
 
C
C     Calculate what fraction of the interval the request time
C     represents.
C
      FRAC = ( T - T1 ) / ( T2 - T1 )
 
C
C     Convert the left and right quaternions to C-matrices.
C
      CALL Q2M ( Q1, CMAT1 )
      CALL Q2M ( Q2, CMAT2 )
 
C
C     Find the matrix that rotates the spacecraft instrument frame from
C     the orientation specified by CMAT1 to that specified by CMAT2.
C     Then find the axis and angle of that rotation matrix.
C
C             T                      T
C        CMAT2   =    ROT    *  CMAT1
C
C                          T
C        ROT     =    CMAT2  *  CMAT1
C
 
      CALL MTXM   ( CMAT2, CMAT1, ROT   )
 
      CALL RAXISA ( ROT,   AXIS,  ANGLE )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'CKE03' )
         RETURN
      END IF
 
C
C     Calculate the matrix that rotates vectors about the vector AXIS
C     by the angle ANGLE * FRAC.
C
      CALL AXISAR ( AXIS,  ANGLE * FRAC, DELTA )
 
C
C     The interpolated pointing at the request time is given by CMAT
C     where:
C
C              T                    T
C          CMAT   =  DELTA  *  CMAT1
C
C     and
C                                   T
C          CMAT   =  CMAT1  *  DELTA
C
 
      CALL MXMT   ( CMAT1, DELTA, CMAT )
 
C
C     Set CLKOUT equal to the time that pointing is being returned.
C
      CLKOUT = T
 
C
C     If angular velocity is requested then take a weighted average
C     of the angular velocities at the left and right endpoints.
C
 
      IF ( NEEDAV ) THEN
 
         CALL VLCOM ( 1.D0 - FRAC, AV1, FRAC, AV2, AV )
 
      END IF
 
      CALL CHKOUT ( 'CKE03' )
 
      RETURN
      END
