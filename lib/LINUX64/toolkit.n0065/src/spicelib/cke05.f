C$Procedure      CKE05 ( C-Kernel, evaluate, type 5 )
 
      SUBROUTINE CKE05 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
 
C$ Abstract
C
C     Evaluate a single data record from a type 5 CK segment.
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
C
C$ Keywords
C
C     POINTING
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'ck05.inc'
      INCLUDE 'ckparam.inc'
      
 
      LOGICAL               NEEDAV
      DOUBLE PRECISION      RECORD   ( * )
      DOUBLE PRECISION      CMAT     ( 3, 3 )
      DOUBLE PRECISION      AV       ( 3 )
      DOUBLE PRECISION      CLKOUT

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NEEDAV     I   True if angular velocity is requested.
C     RECORD    I-O  Data type 5 record.
C     CMAT       O   C-matrix.
C     AV         O   Angular velocity vector.
C     CLKOUT     O   SCLK associated with C-matrix.
C
C$ Detailed_Input
C
C     NEEDAV      is true if angular velocity is requested.
C
C     RECORD      is a record from a type 5 CK segment which, when
C                 evaluated at the epoch contained in its first
C                 element, will give the attitude and angular velocity
C                 of a spacecraft structure or instrument relative to a
C                 base reference frame.
C
C                 The structure of the record is as follows:
C
C                    +----------------------+
C                    | evaluation epoch     |
C                    +----------------------+
C                    | subtype code         |
C                    +----------------------+
C                    | number of packets (n)|
C                    +----------------------+
C                    | nominal SCLK rate    |
C                    +----------------------+
C                    | packet 1             |
C                    +----------------------+
C                    | packet 2             |
C                    +----------------------+
C                             .
C                             .
C                             .
C                    +----------------------+
C                    | packet n             |
C                    +----------------------+
C                    | epochs 1--n          |
C                    +----------------------+
C
C                See the CK Required Reading or the include file
C                ck05.inc for details on CK type 5 packet contents.
C
C
C$ Detailed_Output
C
C     RECORD     has been modified due to its use as a workspace array.
C                The contents are undefined.
C
C
C     CMAT       is a rotation matrix that transforms the components
C                of a vector expressed in the base frame given in
C                the segment to components expressed in the instrument
C                fixed frame at the returned time.
C
C                Thus, if a vector v has components x, y, z in the
C                base frame, then v has components x', y', z' in the
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
C                which is in terms of the base coordinate frame
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
C     1)  If the input record contains an unrecognized subtype code,
C         the error SPICE(NOTSUPPORTED) is signaled. 
C
C     2)  If the record subtype is one for which quaternion derivatives
C         are stored (subtypes 0 and 2), and if the Ith quaternion in
C         the input record is farther than its negative from the (I-1)st
C         quaternion in the record, the error SPICE(BADQUATSIGN) is
C         signaled.
C
C         For subtypes 1 and 3, this condition is not considered an
C         error: the closer to the preceding quaternion of the two
C         quaternion representations is used for interpolation.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The exact format and structure of CK type 5 (MEX/Rosetta Attitude
C     file interpolation) CK segments is described in the CK Required
C     Reading.
C
C$ Examples
C
C     The CKEnn routines are almost always used in conjunction with
C     the corresponding CKRnn routines, which read the records from
C     CK files.
C
C     The following code fragment searches through all of the segments
C     in a file applicable to the Mars Express spacecraft bus that
C     are of data type 5, for a particular spacecraft clock time.
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
C           SC     = -41
C           INST   = -41000
C           DTYPE  =  5
C           NEEDAV = .FALSE.
C
C     C
C     C     Load the MEX SCLK kernel and the C-kernel.
C     C
C           CALL FURNSH ( 'MEX_SCLK.TSC'       )
C           CALL DAFOPR ( 'MEX_CK.BC',  HANDLE )
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
C     C     separation between MEX pointing instances ).
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
C              CALL DAFGN ( IDENT )
C              CALL DAFGS ( DESCR )
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
C                 CALL CKR05 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
C          .                   RECORD, FND )
C
C                 IF ( FND ) THEN
C
C                    CALL CKE05 (NEEDAV,RECORD,CMAT,AV,CLKOUT)
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
C     1)  This routine assumes that the input record is valid.  Any
C         checking of the input data is assumed to have been performed
C         when the source CK file was created.
C
C     2)  This routine assumes that the input data are suitable for the
C         interpolation method indicated by the subtype code in the
C         input record.  Since the mapping of rotations to quaternions
C         is multiple-valued, this routine assumes that whichever sign
C         minimizes the Euclidean distance between one quaternion and
C         the next is the correct sign.
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
C-    SPICELIB Version 3.0.0, 06-FEB-2014 (NJB)
C
C        Bug fix and functional change: quaternion sign adjustment
C        is no longer performed for the Hermite subtypes (0 and 2).
C        If a sign adjustment is needed for quaternions belonging to
C        a record of Hermite subtype, an error is signaled. Sign
C        adjustment is still performed for the Lagrange subtypes.
C
C        Corrected in-line comments concerning change of AV units.
C
C-    SPICELIB Version 2.0.0, 20-NOV-2006 (NJB)
C
C        Bug fix:  this routine now assumes that angular velocity
C        and quaternion derivative values stored in the input 
C        record have units of radians/second.
C
C        Bug fix:  this routine no longer attempts to determine
C        the correct sign of quaternion derivatives.  The caller
C        must supply quaternion derivatives that are suitable
C        for interpolation.
C
C-    SPICELIB Version 1.3.0, 23-OCT-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments in
C        XPOSEG and VSCL calls.  Replaced header reference to LDPOOL
C        with reference to FURNSH.
C
C-    SPICELIB Version 1.2.0, 14-FEB-2003 (NJB)
C
C        Bug fix:  angular velocity computation was modified to
C        match that used in the corresponding algorithm employed
C        by the MEX/Rosetta attitude file reader.  The quaternion
C        derivative used to derive angular velocity now is the
C        derivative of the *unit* quaternion.
C
C-    SPICELIB Version 1.1.0, 06-SEP-2002 (NJB)
C
C-&


C$ Index_Entries
C
C     evaluate type_5 ck segment
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.3.0, 23-OCT-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments in
C        XPOSEG and VSCL calls.  Replaced header reference to LDPOOL
C        with reference to FURNSH.
C
C-    SPICELIB Version 1.2.0, 14-FEB-2003 (NJB)
C
C        Bug fix:  angular velocity computation was modified to
C        match that used in the corresponding algorithm employed
C        by the MEX/Rosetta attitude file reader.  The quaternion
C        derivative used to derive angular velocity now is the
C        derivative of the *unit* quaternion.
C
C        Letting Q(t) be the quaternion derived by polynomial
C        interpolation, and letting UQ(t) be Q(t)/||Q(t)||,
C        the quaternion derivative d(UQ)/dt is now used.
C
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      LGRINT
      DOUBLE PRECISION      VDISTG
      DOUBLE PRECISION      VDOTG
      DOUBLE PRECISION      VNORMG

      LOGICAL               RETURN
 
C
C     Local parameters
C

C
C     Index of evaluation epoch in record:
C
      INTEGER               EPCIDX
      PARAMETER           ( EPCIDX = 1 )

C
C     Index of subtype code in record:
C
      INTEGER               SBTIDX
      PARAMETER           ( SBTIDX = 2 )

C
C     Index of packet count in record:
C
      INTEGER               CNTIDX
      PARAMETER           ( CNTIDX = 3 )

C
C     Index at which packets start; packet base:
C
      INTEGER               PKTIDX
      PARAMETER           ( PKTIDX = 5 )

      INTEGER               PKTBAS
      PARAMETER           ( PKTBAS = PKTIDX - 1 )

C
C     Local variables
C
      DOUBLE PRECISION      DQ     ( 0 : 3 )
      DOUBLE PRECISION      DS     ( 0 : 3 )
      DOUBLE PRECISION      LOCREC ( CKMRSZ )
      DOUBLE PRECISION      MAGS
      DOUBLE PRECISION      Q      ( 0 : 3 )
      DOUBLE PRECISION      QNEG   ( 0 : 3 )
      DOUBLE PRECISION      RADTRM ( 0 : 3 )
      DOUBLE PRECISION      RATE
      DOUBLE PRECISION      SCLDDQ ( 0 : 3 )
      DOUBLE PRECISION      SCLKDP
      DOUBLE PRECISION      STATE  ( 8 )
      DOUBLE PRECISION      VBUFF  ( 6 )
      DOUBLE PRECISION      WORK   ( CKMRSZ * 2,  2 )

      INTEGER               FROM
      INTEGER               I
      INTEGER               J
      INTEGER               N
      INTEGER               NEWPTR
      INTEGER               PACKSZ
      INTEGER               PRVPTR
      INTEGER               SUBTYP
      INTEGER               TO
      INTEGER               UB
      INTEGER               XSTART
      INTEGER               YSTART

      
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
      
      CALL CHKIN ( 'CKE05' )
 
C
C     Capture the subtype from the record and set the packet size
C     accordingly.
C
      SUBTYP =  NINT( RECORD(SBTIDX) )


      IF ( SUBTYP .EQ. C05TP0 ) THEN

         PACKSZ = C05PS0

      ELSE IF ( SUBTYP .EQ. C05TP1 ) THEN

         PACKSZ = C05PS1

      ELSE IF ( SUBTYP .EQ. C05TP2 ) THEN

         PACKSZ = C05PS2

      ELSE IF ( SUBTYP .EQ. C05TP3 ) THEN

         PACKSZ = C05PS3

      ELSE
         
         CALL SETMSG ( 'Unexpected CK type 5 subtype # found in ' //
     .                 'type 5 segment.'                          )
         CALL ERRINT ( '#',  SUBTYP                               )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                      )
         CALL CHKOUT ( 'CKE05'                                    )
         RETURN
      
      END IF

C
C     Get the packet count and epoch.
C
      N       =  NINT( RECORD(CNTIDX) )
      SCLKDP  =        RECORD(EPCIDX)

C
C     Get the nominal clock rate.
C
      RATE = RECORD(4)

C
C     Adjust quaternion "signs" as necessary to minimize distance
C     between successive quaternions. This adjustment is performed
C     only for subtypes that don't store quaternion derivatives
C     (these are the Lagrange subtypes).
C
      IF (  ( SUBTYP .EQ. C05TP1 ) .OR. ( SUBTYP .EQ. C05TP3 )  ) THEN
C
C        For these subtypes, only the quaternions themselves need be
C        adjusted.  
C
C        PRVPTR is the index of the "previous" quaternion---the one to
C        which the successor and its negative will be compared.
C
         PRVPTR = PKTIDX

         DO I = 2, N
C
C           NEWPTR points to the quaternion ahead of the one
C           pointed to by PRVPTR.
C
            NEWPTR = PKTIDX + PACKSZ*(I-1)

            CALL VMINUG ( RECORD(NEWPTR), 4,  QNEG )

C
C           Replace the Ith quaternion with QNEG if QNEG is closer
C           than the current quaternion to the previous quaternion.
C
            IF (     VDISTG( RECORD(PRVPTR), QNEG,           4 ) 
     .          .LT. VDISTG( RECORD(PRVPTR), RECORD(NEWPTR), 4 ) ) THEN

               CALL MOVED ( QNEG, 4, RECORD( NEWPTR ) )

            END IF

            PRVPTR = NEWPTR
            
         END DO


      ELSE
C
C        For the Hermite types, if the quaternions need to be adjusted,
C        we have an error condition.
C
C        PRVPTR is the index of the "previous" quaternion---the one to
C        which the successor and its negative will be compared.
C
         PRVPTR = PKTIDX

         DO I = 2, N
C
C           NEWPTR points to the quaternion ahead of the one
C           pointed to by PRVPTR. 
C
            NEWPTR = PKTIDX + PACKSZ*(I-1)

            CALL VMINUG ( RECORD(NEWPTR), 4,  QNEG )
C
C           Replace the Ith quaternion with QNEG if QNEG is closer
C           than the current quaternion to the previous quaternion.
C
            IF (     VDISTG( RECORD(PRVPTR), QNEG,           4 ) 
     .          .LT. VDISTG( RECORD(PRVPTR), RECORD(NEWPTR), 4 ) ) THEN

               CALL SETMSG ( 'Quaternion sign error: quaternion at '
     .         //            'index # in the input record is farther '
     .         //            'than its negative from the preceding '
     .         //            'quaternion in the record. Quaternion '
     .         //            'is (#, #, #, #); predecessor is ' 
     .         //            '(#, #, #, #). This makes the quaternion '
     .         //            'sequence unsuitable for Hermite '
     .         //            'interpolation. The quaternions, and '
     .         //            'if applicable, their derivatives, '
     .         //            'must be adjusted before they are '
     .         //            'passed to this routine.'                 )
               CALL ERRINT ( '#', I                                    )
               CALL ERRDP  ( '#', RECORD(NEWPTR  )                     )
               CALL ERRDP  ( '#', RECORD(NEWPTR+1)                     )
               CALL ERRDP  ( '#', RECORD(NEWPTR+2)                     )
               CALL ERRDP  ( '#', RECORD(NEWPTR+3)                     )
               CALL ERRDP  ( '#', RECORD(PRVPTR  )                     )
               CALL ERRDP  ( '#', RECORD(PRVPTR+1)                     )
               CALL ERRDP  ( '#', RECORD(PRVPTR+2)                     )
               CALL ERRDP  ( '#', RECORD(PRVPTR+3)                     )
               CALL SIGERR ( 'SPICE(BADQUATSIGN)'                      )
               CALL CHKOUT ( 'CKE05'                                   )
               RETURN

            END IF

         END DO

      END IF



      IF ( SUBTYP .EQ. C05TP1 ) THEN
C
C        We perform Lagrange interpolation on each quaternion 
C        component, and obtain quaternion derivatives from the
C        interpolating polynomials.  The quaternion and derivative
C        gives us angular velocity.
C
C        We'll transpose the pointing information in the input record so
C        that contiguous pieces of it can be shoved directly into the
C        interpolation routine LGRINT.  We allow LGRINT to overwrite
C        the state values in the input record, since this saves local
C        storage and does no harm.  (See the header of LGRINT for a
C        description of its work space usage.)
C
         N   =  NINT( RECORD(CNTIDX) )

         CALL XPSGIP ( PACKSZ, N, RECORD(PKTIDX) ) 

C
C        We interpolate each state component in turn.
C
         XSTART   =   PKTIDX   +  N * PACKSZ
 
         DO I = 1, PACKSZ
 
            YSTART    =   PKTIDX  +  N * (I-1)
 

            CALL LGRIND ( N,
     .                    RECORD(XSTART),
     .                    RECORD(YSTART),
     .                    WORK,
     .                    SCLKDP,
     .                    STATE(I),
     .                    STATE(I+4)  )
     
         END DO  

C
C        The output quaternion is a unitized version of the 
C        interpolated state.
C
         MAGS = VNORMG( STATE, 4 )


         IF ( MAGS .EQ. 0.D0 ) THEN

            CALL SETMSG ( 'Quaternion magnitude at SCLK # was zero.' )
            CALL ERRDP  ( '#',  SCLKDP                               )
            CALL SIGERR ( 'SPICE(DIVIDEBYZERO)'                      )
            CALL CHKOUT ( 'CKE05'                                    )
            RETURN

         END IF

         CALL VSCLG ( 1.D0/MAGS, STATE, 4, Q )


         IF ( NEEDAV ) THEN
C
C           Find the time derivative of the unit quaternion:
C           Letting S represent the quaternion portion of STATE, we
C           have
C
C              Q = S/||S||
C
C
C           Then letting < , > denote the 4-dimensional inner product
C           operator, we have
C
C
C                         d(S)/dt      < Q, d(S)/dt >         
C              d(Q)/dt =  -------  -   -------------- * Q
C                          ||S||            ||S||        
C
C
            CALL MOVED ( STATE(5), 4, DS )

            CALL VSCLG ( 1.D0          / MAGS,  DS, 4,  SCLDDQ )
            CALL VSCLG ( VDOTG(Q,DS,4) / MAGS,  Q,  4,  RADTRM )

            CALL VSUBG ( SCLDDQ, RADTRM, 4, DQ )
C
C           Derive angular velocity from Q and dQ/dt:
C
            CALL QDQ2AV ( Q, DQ, AV )

C
C           Scale the AV from radians/tick to radians/second.
C                
            CALL VSCLIP ( 1.D0/RATE, AV )

         END IF

C
C        Q and if required AV have been assigned.
C

      
      ELSE IF ( SUBTYP .EQ. C05TP3 ) THEN
C
C        This is the easiest case:  we perform Lagrange interpolation
C        on each quaternion or angular velocity component.
C
C        We'll transpose the pointing information in the input record so
C        that contiguous pieces of it can be shoved directly into the
C        interpolation routine LGRINT.  We allow LGRINT to overwrite
C        the state values in the input record, since this saves local
C        storage and does no harm.  (See the header of LGRINT for a
C        description of its work space usage.)
C
         N   =  NINT( RECORD(CNTIDX) )
 
         CALL XPSGIP ( PACKSZ, N, RECORD(PKTIDX) ) 

C
C        We interpolate each state component in turn.
C
         XSTART  =  PKTIDX   +  N * PACKSZ
 
         IF ( NEEDAV ) THEN
            UB  =  PACKSZ
         ELSE
            UB  =  4
         END IF

         DO I = 1, UB
 
            YSTART    =   PKTIDX  +  N * (I-1)
 
            STATE(I)  =   LGRINT ( N,
     .                             RECORD(XSTART),
     .                             RECORD(YSTART),
     .                             LOCREC,
     .                             SCLKDP          )
         END DO  

C
C        The output quaternion is a unitized version of the
C        interpolated state.
C
         CALL VHATG ( STATE, 4, Q )


         IF ( NEEDAV ) THEN
C
C           The angular velocity already is in units of radians/second.
C
            CALL VEQU ( STATE(5), AV )

         END IF

C
C        Q and if required AV have been assigned.
C

      ELSE
C
C        We have a Hermite-style subtype.  Whether it's subtype 0
C        or 2, we perform Hermite interpolation on the quaternions.
C
C        We interpolate each quaternion component in turn.  Attitude and
C        angular velocity are interpolated separately.
C
         XSTART   =   PKTIDX  +  PACKSZ * N 
   
         DO I = 1, 4
 
            DO J = 1, N
C
C              For the Jth input packet, copy the Ith position and
C              velocity components into the local record buffer RECORD.
C
C              In order to perform Hermite interpolation, the
C              quaternions and quaternion derivatives must have a
C              common time scale. So prior to interpolation, we scale
C              the units of the quaternion derivatives from radians/sec
C              to radians/tick.
C
               FROM         = PKTBAS + PACKSZ*(J-1) + I
               TO           =              2 * J    - 1
            
               LOCREC(TO  ) = RECORD ( FROM     )
               LOCREC(TO+1) = RECORD ( FROM + 4 ) * RATE

            END DO

C
C           Interpolate the Ith quaternion and quaternion derivative
C           components.
C        
            CALL HRMINT ( N, 
     .                    RECORD(XSTART),
     .                    LOCREC,
     .                    SCLKDP,           
     .                    WORK,
     .                    STATE(I  ),
     .                    STATE(I+4)      ) 

         END DO

C
C        The output quaternion is a unitized version of the 
C        interpolated state.
C
         MAGS = VNORMG( STATE, 4 )

         IF ( MAGS .EQ. 0.D0 ) THEN

            CALL SETMSG ( 'Quaternion magnitude at SCLK # was zero.' )
            CALL ERRDP  ( '#',  SCLKDP                               )
            CALL SIGERR ( 'SPICE(DIVIDEBYZERO)'                      )
            CALL CHKOUT ( 'CKE05'                                    )
            RETURN

         END IF

         CALL VSCLG ( 1.D0/MAGS, STATE, 4, Q )


         IF ( NEEDAV ) THEN

            IF ( SUBTYP .EQ. C05TP0 ) THEN
C
C              Find the time derivative of the unit quaternion:
C              Letting S represent the quaternion portion of STATE, we
C              have
C
C                 Q = S/||S||
C
C
C              Then letting < , > denote the 4-dimensional inner product
C              operator, we have
C
C
C                            d(S)/dt      < Q, d(S)/dt >         
C                 d(Q)/dt =  -------  -   -------------- * Q
C                             ||S||            ||S||        
C
C
               CALL MOVED ( STATE(5), 4, DS )

               CALL VSCLG ( 1.D0          / MAGS,  DS, 4,  SCLDDQ )
               CALL VSCLG ( VDOTG(Q,DS,4) / MAGS,  Q,  4,  RADTRM )

               CALL VSUBG ( SCLDDQ, RADTRM, 4, DQ )
C
C              Derive angular velocity from Q and dQ/dt:
C               
               CALL QDQ2AV ( Q, DQ, AV )

C
C              Scale the AV from radians/tick to radians/second.
C
               CALL VSCLIP ( 1.D0/RATE, AV )

            ELSE  
C
C              This is subtype 2; we perform Hermite interpolation on
C              the angular velocity and its derivative.
C
C              Now interpolate angular velocity, using separate angular
C              velocity data and angular acceleration.
C
               DO I = 1, 3
  
                  DO J = 1, N
C
C                    For the Jth input packet, copy the Ith position
C                    and velocity components into the local record
C                    buffer LOCREC.  Note that, as with quaternion
C                    derivatives, we must scale angular acceleration
C                    from radians/sec**2 to radians/(sec*tick) before
C                    interpolating.
C
                     FROM         = PKTBAS + PACKSZ*(J-1) +  8  +  I
                     TO           =              2 * J    -  1
            
                     LOCREC(TO  ) = RECORD ( FROM     )
                     LOCREC(TO+1) = RECORD ( FROM + 3 ) * RATE
            
                  END DO
 
C
C                 Interpolate the Ith angular velocity and angular
C                 acceleration components of the attitude. We'll
C                 capture the result in a temporary buffer, then
C                 transfer the velocity to the output argument AV.
C
                  CALL HRMINT ( N, 
     .                          RECORD(XSTART),
     .                          LOCREC,
     .                          SCLKDP,           
     .                          WORK,
     .                          VBUFF(I  ),
     .                          VBUFF(I+3)     ) 
               END DO
 
C
C              Fill in the angular velocity in the output angular
C              velocity vector using the results of interpolating
C              velocity and acceleration.
C
C              The angular velocity is already in units of
C              radians/second.
C
               CALL VEQU ( VBUFF, AV )

            END IF
C
C           We've handled the type 0 and type 2 cases.
C              
         END IF
C
C        We've computed the angular velocity AV for the Hermite
C        subtypes, if a.v. was requested.
C
      END IF
C
C     We've handled all four subtypes.
C


C
C     Produce a C-matrix from the interpolated quaternion. Set CLKOUT.
C
      CALL Q2M ( Q, CMAT ) 
      
      CLKOUT = RECORD(EPCIDX)


      CALL CHKOUT ( 'CKE05' )
      RETURN
      END
 
