C$Procedure      CKE04 ( C-kernel, evaluate pointing record, type 4 )

      SUBROUTINE CKE04 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )

C$ Abstract
C
C     Evaluate a pointing record returned by CKR04 from a CK type 4
C     segment. Return the C-matrix and angular velocity vector 
C     associated with the time CLKOUT.
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

      INCLUDE               'ckparam.inc'

      LOGICAL               NEEDAV
      DOUBLE PRECISION      RECORD ( * )
      DOUBLE PRECISION      CMAT   ( 3, 3 )
      DOUBLE PRECISION      AV     ( 3 )
      DOUBLE PRECISION      CLKOUT

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NEEDAV     I   True if angular velocity is requested.
C     RECORD     I   Data type 4 pointing record.
C     CMAT       O   C-matrix.
C     AV         O   Angular velocity vector.
C     CLKOUT     O   SCLK associated with C-matrix.
C
C$ Detailed_Input
C
C     NEEDAV     is true if angular velocity is requested.
C
C     RECORD     is a set of double precision numbers returned by 
C                CKR04. RECORD must have the following structure:
C
C                ---------------------------------------------------
C                |    Encoded onboard time which is the closest    |
C                |  to SCLKDP and belongs to one of approximation  |
C                |                   intervals                     |
C                ---------------------------------------------------
C                |       encoded SCLK time of the midpoint of      |
C                |             interpolation interval              |
C                ---------------------------------------------------
C                |          radii of interpolation interval        |
C                |    expressed as double precision SCLK ticks     |
C                ---------------------------------------------------
C                |         Number of coefficients for q0           |
C                ---------------------------------------------------
C                |         Number of coefficients for q1           |
C                ---------------------------------------------------
C                |         Number of coefficients for q2           |
C                ---------------------------------------------------
C                |         Number of coefficients for q3           |
C                ---------------------------------------------------
C                |         Number of coefficients for AV1          |
C                ---------------------------------------------------
C                |         Number of coefficients for AV2          |
C                ---------------------------------------------------
C                |         Number of coefficients for AV3          |
C                ---------------------------------------------------
C                |               q0 Cheby coefficients             |
C                ---------------------------------------------------
C                |               q1 Cheby coefficients             |
C                ---------------------------------------------------
C                |               q2 Cheby coefficients             |
C                ---------------------------------------------------
C                |               q3 Cheby coefficients             |
C                ---------------------------------------------------
C                |         AV1 Cheby coefficients (optional)       |
C                ---------------------------------------------------
C                |         AV2 Cheby coefficients (optional)       |
C                ---------------------------------------------------
C                |         AV3 Cheby coefficients (optional)       |
C                ---------------------------------------------------
C
C$ Detailed_Output
C
C     CMAT       is a rotation matrix that transforms the components
C                of a vector expressed in the inertial frame given in
C                the segment to components expressed in the instrument
C                fixed frame at the returned time.
C
C                Thus, if a vector v has components x, y, z in the
C                inertial frame, then v has components x', y', z' in
C                the instrument fixed frame where:
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
C     See 'ckparam.inc'.
C
C$ Exceptions
C
C     Error free.
C
C     No checking is done to determine whether RECORD is valid.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     For a detailed description of the structure of a type 4 pointing
C     segment, see the CK Required Reading file.
C
C     The work done by CKE04 is to calculate quaternion and angular
C     velocity components using Chebyshev polynomial approximation
C     parameters. The second step of evaluation is to convert the
C     pointing portion of the record from quaternion form to C-matrix
C     form.
C
C     The angular velocity vector will only be returned if it has been
C     requested. In other words, if NEEDAV is true, the routine will
C     expect the angular velocity component of the record to be 
C     present.
C
C$ Examples
C
C     The CKRnn routines are usually used in tandem with the CKEnn
C     routines, which evaluate the record returned by CKRnn to give
C     the pointing information and output time.
C
C     The following code fragment searches through all of the segments
C     in a file applicable to the Mars Global Surveyor spacecraft bus
C     that are of data type 4, for a particular spacecraft clock time.
C     It then evaluates the pointing for that epoch and prints the
C     result.
C
C     C
C     C     CK parameters include file.
C     C
C           INCLUDE               'ckparam.inc'
C     C
C     C     Declarations
C     C
C           CHARACTER*(20)        SCLKCH
C           CHARACTER*(20)        SCTIME
C           CHARACTER*(40)        IDENT
C
C           DOUBLE PRECISION      AV     ( 3 )
C           DOUBLE PRECISION      CLKOUT
C           DOUBLE PRECISION      CMAT   ( 3, 3 )
C           DOUBLE PRECISION      DCD    ( 2 )
C           DOUBLE PRECISION      DESCR  ( 5 )
C           DOUBLE PRECISION      RECORD ( CK4RSZ )
C           DOUBLE PRECISION      SCLKDP
C           DOUBLE PRECISION      TOL
C
C           INTEGER               HANDLE
C           INTEGER               I
C           INTEGER               ICD    ( 6 )
C           INTEGER               INST
C           INTEGER               SC
C
C           LOGICAL               FND
C           LOGICAL               NEEDAV
C           LOGICAL               SFND
C     C
C     C     Initial values.
C     C
C           SC     = -94
C           INST   = -94000
C           NEEDAV = .FALSE.
C     C
C     C     Load the MGS SCLK kernel and the C-kernel.
C     C
C           CALL FURNSH( 'MGS_SCLK.TSC' )
C           CALL DAFOPR( 'MGS_CK4.BC', HANDLE )
C     C
C     C     Get the spacecraft clock time. Then encode it for use
C     C     in the C-kernel.
C     C
C           CALL PROMPT( 'Enter SCLK string: ', SCLKCH )
C           CALL SCENCD( SC, SCLKCH, SCLKDP )
C     C
C     C     Use a tolerance of 2 seconds (half of the nominal
C     C     separation between MGS pointing instances ).
C     C
C           CALL SCTIKS ( SC, '0000000002:000', TOL )
C     C
C     C     Search from the beginning of the CK file through all
C     C     of the segments.
C     C
C           CALL DAFBFS( HANDLE )
C           CALL DAFFNA( SFND   )
C
C           FND = .FALSE.
C
C           DO WHILE ( ( SFND ) .AND. ( .NOT. FND ) )
C     C
C     C        Get the segment identifier and descriptor.
C     C
C              CALL DAFGN( IDENT )
C              CALL DAFGS( DESCR )
C     C
C     C        Unpack the segment descriptor into its integer and
C     C        double precision components.
C     C
C              CALL DAFUS( DESCR, 2, 6, DCD, ICD )
C     C
C     C        Determine if this segment should be processed.
C     C
C              IF ( ( INST          .EQ. ICD( 1 ) ) .AND.
C          .        ( SCLKDP + TOL  .GE. DCD( 1 ) ) .AND.
C          .        ( SCLKDP - TOL  .LE. DCD( 2 ) ) .AND.
C          .        ( CK4DTP        .EQ. ICD( 3 ) )      ) THEN
C     C
C     C           Find CK 4 record covering requested time.
C     C
C                 CALL CKR04( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
C          .                  RECORD, FND )
C
C                 IF ( FND ) THEN
C     C
C     C              Compute pointing using found CK 4 record.
C     C
C                    CALL CKE04( NEEDAV, RECORD, CMAT, AV, CLKOUT)
C
C                    CALL SCDECD( SC, CLKOUT, SCTIME )
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
C     1) No checking is done on the input RECORD.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     Y.K. Zaiko     (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.2, 18-APR-2014 (BVS)
C
C        Minor header edits.
C
C-    SPICELIB Version 1.0.1, 22-AUG-2006 (EDW)
C
C        Replaced references to LDPOOL with references 
C        to FURNSH.
C
C-    SPICELIB Version 1.0.0, 05-MAY-1999 (YKZ) (BVS)
C
C-&

C$ Index_Entries
C
C     evaluate CK type_4 pointing data record
C
C-&

C
C     Local variables
C
      DOUBLE PRECISION        Q      ( 4 )
      DOUBLE PRECISION        QOUT   ( 4 )
      
      INTEGER                 BASADD
      INTEGER                 I
      INTEGER                 IDEG   ( QAVSIZ )
      
C
C     Initial values.
C
      AV( 1 ) = 0.D0
      AV( 2 ) = 0.D0
      AV( 3 ) = 0.D0

C
C     Read numbers of polynomial coefficients from input record to 
C     local integer array.
C
      DO I = 1, QAVSIZ      
         IDEG( I ) = INT( RECORD( 3 + I ) )
      END DO

C
C     Evaluate polynomial function for quaternion components at time
C     RECORD( 1 ).
C
      BASADD = CK4SFT + 1
           
      DO I = 1, QSIZ
         
         CALL CHBVAL( RECORD( BASADD ), IDEG( I ) - 1, RECORD( 2 ), 
     .                                         RECORD( 1 ), Q( I ) )
         BASADD = BASADD + IDEG ( I )
      
      END DO

C
C     Normalize quaternion.
C
      CALL VHATG ( Q, QSIZ, QOUT )

C
C     Convert the quaternion to a C-matrix.
C
      CALL Q2M ( QOUT, CMAT )

      CLKOUT = RECORD( 1 )

C
C     Check if angular velocities have to be evaluated, then
C     evaluate them.
C
      IF ( NEEDAV ) THEN

         DO I = QSIZ + 1, QAVSIZ

            CALL CHBVAL ( RECORD( BASADD ), IDEG( I ) - 1, RECORD( 2 ),
     .                                    RECORD( 1 ), AV( I - QSIZ ) )
            BASADD = BASADD + IDEG( I )
         
         END DO

      END IF

C
C     All done.
C
      RETURN
      
      END
