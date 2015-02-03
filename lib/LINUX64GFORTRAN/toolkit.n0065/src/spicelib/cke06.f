C$Procedure      CKE06 ( C-Kernel, evaluate, type 6 )
 
      SUBROUTINE CKE06 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
 
C$ Abstract
C
C     Evaluate a single data record from a type 6 CK segment.
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

      INCLUDE 'ck06.inc'
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
C     RECORD    I-O  Data type 6 record.
C     CMAT       O   C-matrix.
C     AV         O   Angular velocity vector.
C     CLKOUT     O   SCLK associated with C-matrix.
C
C$ Detailed_Input
C
C     NEEDAV      is true if angular velocity is requested.
C
C     RECORD      is a record from a type 6 CK segment which, when
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
C                ck06.inc for details on CK type 6 packet contents.
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
C         the error is diagnosed by a routine in the call tree of this
C         routine.
C
C     2)  If the record subtype is one for which quaternion derivatives
C         are stored (subtypes 0 and 2), and if the Ith quaternion in
C         the input record is farther than its negative from the (I-1)st
C         quaternion in the record, an error is signaled by a routine
C         in the call tree of this routine.
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
C     The exact format and structure of CK type 6 (MEX/Rosetta Attitude
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
C     are of data type 6, for a particular spacecraft clock time.
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
C           DTYPE  =  6
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
C                 CALL CKR06 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
C          .                   RECORD, FND )
C
C                 IF ( FND ) THEN
C
C                    CALL CKE06 (NEEDAV,RECORD,CMAT,AV,CLKOUT)
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
C     1)  This routine performs minimal error checking. The input data
C         are assumed to have been checked when the source CK file was
C         created.
C
C     2)  With the exception of the check described in item 2 of 
C         the Exceptions section above, the input data are assumed to
C         be suitable for the interpolation method specified by the
C         input record's subtype and packet count (which implies an
C         interpolating polynomial degree).
C              
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 14-MAR-2014 (NJB) (BVS)
C
C-&


C$ Index_Entries
C
C     evaluate type_6 ck_segment
C
C-&
 
C$ Revisions
C
C     None.
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN
       
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
      
      CALL CHKIN ( 'CKE06' )
 
C
C     Given that our nominally type 6 input record is actually a
C     valid type 5 record, we let the type 5 evaluator do the
C     work.
C
      CALL CKE05 ( NEEDAV, RECORD, CMAT, AV, CLKOUT ) 

      CALL CHKOUT ( 'CKE06' )
      RETURN
      END
 
