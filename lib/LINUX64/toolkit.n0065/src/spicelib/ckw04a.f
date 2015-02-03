C$Procedure      CKW04A ( CK type 04: Add data to a segment )
 
      SUBROUTINE CKW04A ( HANDLE, NPKTS, PKTSIZ, PKTDAT, SCLKDP )
      IMPLICIT NONE
 
C$ Abstract
C
C     Add data to a type 4 CK segment currently being written to
C     the file associated with HANDLE. See also CKW04B and CKW04E.
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
C
C$ Keywords
C
C     POINTING
C
C$ Declarations
 
      INCLUDE               'ckparam.inc'
 
      INTEGER               HANDLE
      INTEGER               NPKTS
      INTEGER               PKTSIZ( * )
      DOUBLE PRECISION      PKTDAT( * )
      DOUBLE PRECISION      SCLKDP( * )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   The handle of an DAF file opened for writing.
C     NPKTS      I   Number of data packets to write to a segment.
C     PKTSIZ     I   The numbers of values in the data packets
C     PKTDAT     I   The data packets.
C     SCLKDP     I   The SCLK times associated with the data packets.
C
C$ Detailed_Input
C
C     HANDLE     is the file handle of a CK file in which a CK type 4
C                segment is currently being written.
C
C     NPKTS      is the number of data packets to write to a segment.
C
C     PKTSIZ     is the number of values in all data packets.
C
C     PKTDAT     is the data packets. The data packets in this array
C                must be organized as described in the $ Particulars
C                section of the header.
C
C     SCLKDP     contains the initial SCLK times corresponding to the
C                Chebyshev coefficients in PKTSIZ. The I'th time is
C                start time of the I'th packet coverage interval.
C                The times must form a strictly increasing sequence.
C
C$ Detailed_Output
C
C     None.      Data is stored in a segment in the DAF file
C                associated with HANDLE.
C
C$ Parameters
C
C     See 'ckparam.inc'.
C
C$ Exceptions
C
C     1) If the number of coefficient sets and epochs is not positive,
C        the error SPICE(INVALIDARGUMENT) will be signalled.
C
C     2) If size of any input packet is greater that maximum allowed
C        type 4 CK record size minus one, the error
C        SPICE(INVALIDARGUMENT) will be signalled.
C
C$ Files
C
C     See HANDLE in the $ Detailed_Input section.
C
C$ Particulars
C
C     This routine adds data to a type 4 CK segment that is currently
C     being written to the associated with HANDLE. The segment must
C     have been started by a call to the routine CKW04B, the routine
C     which begins a type 4 CK segment.
C
C     This routine is one of a set of three routines for creating and
C     adding data to type 4 CK segments. These routines are:
C
C        CKW04B: Begin a type 4 CK segment. This routine must be
C                called before any data may be added to a type 4
C                segment.
C
C        CKW04A: Add data to a type 4 CK segment. This routine may be
C                called any number of times after a call to CKW04B to
C                add type 4 records to the CK segment that was
C                started.
C
C        CKW04E: End a type 4 CK segment. This routine is called to
C                make the type 4 segment a permanent addition to the
C                DAF file. Once this routine is called, no further type
C                4 records may be added to the segment. A new segment
C                must be started.
C
C     A type 4 CK segment consists of coefficient sets for variable
C     order Chebyshev polynomials over consecutive time intervals of a
C     variable length. The gaps between intervals are allowed. The
C     Chebyshev polynomials represent individual SPICE-style quaternion
C     components q0, q1, q2 and q3 and individual angular velocities
C     AV1, AV2 and AV3 if they are included with the data.
C
C     See the discussion of quaternion styles below.
C
C     The pointing data supplied to the type 4 CK writer (CKW04A)
C     is packed into an array as a sequence of records,
C
C        ----------------------------------------------------
C        | Record 1 | Record 2 | .. | Record N-1 | Record N |
C        ----------------------------------------------------
C
C     with each record in data packets has the following format.
C
C        ----------------------------------------------------
C        | The midpoint of the approximation interval       |
C        ----------------------------------------------------
C        | The radius of the approximation interval         |
C        ----------------------------------------------------
C        | Number of coefficients for q0                    |
C        ----------------------------------------------------
C        | Number of coefficients for q1                    |
C        ----------------------------------------------------
C        | Number of coefficients for q2                    |
C        ----------------------------------------------------
C        | Number of coefficients for q3                    |
C        ----------------------------------------------------
C        | Number of coefficients for AV1                   |
C        ----------------------------------------------------
C        | Number of coefficients for AV2                   |
C        ----------------------------------------------------
C        | Number of coefficients for AV3                   |
C        ----------------------------------------------------
C        | q0 Cheby coefficients                            |
C        ----------------------------------------------------
C        | q1 Cheby coefficients                            |
C        ----------------------------------------------------
C        | q2 Cheby coefficients                            |
C        ----------------------------------------------------
C        | q3 Cheby coefficients                            |
C        ----------------------------------------------------
C        | AV1 Cheby coefficients (optional)                |
C        ----------------------------------------------------
C        | AV2 Cheby coefficients (optional)                |
C        ----------------------------------------------------
C        | AV3 Cheby coefficients (optional)                |
C        ----------------------------------------------------
C
C
C
C     Quaternion Styles
C     -----------------
C
C     There are different "styles" of quaternions used in 
C     science and engineering applications. Quaternion styles
C     are characterized by 
C
C        - The order of quaternion elements
C
C        - The quaternion multiplication formula
C
C        - The convention for associating quaternions
C          with rotation matrices
C
C     Two of the commonly used styles are
C
C        - "SPICE"
C
C           > Invented by Sir William Rowan Hamilton
C           > Frequently used in mathematics and physics textbooks
C
C        - "Engineering"
C
C           > Widely used in aerospace engineering applications
C
C
C     SPICELIB subroutine interfaces ALWAYS use SPICE quaternions.
C     Quaternions of any other style must be converted to SPICE
C     quaternions before they are passed to SPICELIB routines.
C     
C
C     Relationship between SPICE and Engineering Quaternions
C     ------------------------------------------------------
C
C     Let M be a rotation matrix such that for any vector V, 
C
C        M*V
C
C     is the result of rotating V by theta radians in the 
C     counterclockwise direction about unit rotation axis vector A.
C     Then the SPICE quaternions representing M are
C
C        (+/-) (  cos(theta/2), 
C                 sin(theta/2) A(1),  
C                 sin(theta/2) A(2),  
C                 sin(theta/2) A(3)  ) 
C
C     while the engineering quaternions representing M are 
C
C        (+/-) ( -sin(theta/2) A(1),  
C                -sin(theta/2) A(2),  
C                -sin(theta/2) A(3),
C                 cos(theta/2)       )
C
C     For both styles of quaternions, if a quaternion q represents
C     a rotation matrix M, then -q represents M as well.
C
C     Given an engineering quaternion
C
C        QENG   = ( q0,  q1,  q2,  q3 )
C
C     the equivalent SPICE quaternion is
C
C        QSPICE = ( q3, -q0, -q1, -q2 )
C
C
C     Associating SPICE Quaternions with Rotation Matrices
C     ----------------------------------------------------
C
C     Let FROM and TO be two right-handed reference frames, for
C     example, an inertial frame and a spacecraft-fixed frame. Let the
C     symbols
C
C        V    ,   V
C         FROM     TO
C
C     denote, respectively, an arbitrary vector expressed relative to
C     the FROM and TO frames. Let M denote the transformation matrix
C     that transforms vectors from frame FROM to frame TO; then
C
C        V   =  M * V
C         TO         FROM
C
C     where the expression on the right hand side represents left
C     multiplication of the vector by the matrix.
C
C     Then if the unit-length SPICE quaternion q represents M, where
C
C        q = (q0, q1, q2, q3)
C
C     the elements of M are derived from the elements of q as follows:
C
C          +-                                                         -+
C          |           2    2                                          |
C          | 1 - 2*( q2 + q3 )   2*(q1*q2 - q0*q3)   2*(q1*q3 + q0*q2) |
C          |                                                           |
C          |                                                           |
C          |                               2    2                      |
C      M = | 2*(q1*q2 + q0*q3)   1 - 2*( q1 + q3 )   2*(q2*q3 - q0*q1) |
C          |                                                           |
C          |                                                           |
C          |                                                   2    2  |
C          | 2*(q1*q3 - q0*q2)   2*(q2*q3 + q0*q1)   1 - 2*( q1 + q2 ) |
C          |                                                           |
C          +-                                                         -+
C
C     Note that substituting the elements of -q for those of q in the
C     right hand side leaves each element of M unchanged; this shows
C     that if a quaternion q represents a matrix M, then so does the
C     quaternion -q.
C
C     To map the rotation matrix M to a unit quaternion, we start by
C     decomposing the rotation matrix as a sum of symmetric
C     and skew-symmetric parts:
C
C                                        2
C        M = [ I  +  (1-cos(theta)) OMEGA  ] + [ sin(theta) OMEGA ]
C
C                     symmetric                   skew-symmetric
C
C
C     OMEGA is a skew-symmetric matrix of the form
C
C                   +-             -+
C                   |  0   -n3   n2 |
C                   |               |
C         OMEGA  =  |  n3   0   -n1 |
C                   |               |
C                   | -n2   n1   0  |
C                   +-             -+
C
C     The vector N of matrix entries (n1, n2, n3) is the rotation axis
C     of M and theta is M's rotation angle.  Note that N and theta
C     are not unique.
C
C     Let
C
C        C = cos(theta/2)
C        S = sin(theta/2)
C
C     Then the unit quaternions Q corresponding to M are
C
C        Q = +/- ( C, S*n1, S*n2, S*n3 )
C
C     The mappings between quaternions and the corresponding rotations
C     are carried out by the SPICELIB routines
C
C        Q2M {quaternion to matrix}
C        M2Q {matrix to quaternion}
C
C     M2Q always returns a quaternion with scalar part greater than
C     or equal to zero.
C
C
C     SPICE Quaternion Multiplication Formula
C     ---------------------------------------
C
C     Given a SPICE quaternion 
C
C        Q = ( q0, q1, q2, q3 )
C
C     corresponding to rotation axis A and angle theta as above, we can
C     represent Q using "scalar + vector" notation as follows:
C
C        s =   q0           = cos(theta/2)
C
C        v = ( q1, q2, q3 ) = sin(theta/2) * A
C
C        Q = s + v
C
C     Let Q1 and Q2 be SPICE quaternions with respective scalar
C     and vector parts s1, s2 and v1, v2:
C 
C        Q1 = s1 + v1
C        Q2 = s2 + v2
C
C     We represent the dot product of v1 and v2 by
C
C        <v1, v2>
C
C     and the cross product of v1 and v2 by
C
C        v1 x v2
C
C     Then the SPICE quaternion product is
C
C        Q1*Q2 = s1*s2 - <v1,v2>  + s1*v2 + s2*v1 + (v1 x v2)       
C
C     If Q1 and Q2 represent the rotation matrices M1 and M2 
C     respectively, then the quaternion product
C
C        Q1*Q2
C
C     represents the matrix product
C
C        M1*M2
C
C
C$ Examples
C
C     Assume that we have:
C
C        HANDLE   is the handle of an CK file opened with write
C                 access.
C
C        SEGID    is a character string of no more than 40 characters
C                 which provides a pedigree for the data in the CK
C                 segment we will create.
C
C        INST     is the SPICE ID code for the instrument whose
C                 pointing data is to be placed into the file.
C
C        AVFLAG   angular rates flag.
C
C        REFFRM   is the name of the SPICE reference frame for the
C                 pointing data.
C
C        BEGTIM   is the starting encoded SCLK time for which the
C                 segment is valid.
C
C        ENDTIM   is the ending encoded SCLK time for which the segment
C                 is valid.
C
C        N        is the number of type 4 records that we want to
C                 put into a segment in an CK file.
C
C        NPKTS    is integer array which contains the lengths of
C                 variable size data packets
C
C        RECRDS   contains N type 4 records packaged for the CK
C                 file.
C
C        SCSTRT   contains the initial encoded SC time for each of
C                 the records contained in RECRDS, where
C
C                    SCSTRT(I) < SCSTRT(I+1), I = 1, N-1
C
C                    SCSTRT(1) <= FIRST, SCSTRT(N) < LAST
C
C     Then the following code fragment demonstrates how to create
C     a type 4 CK segment if all of the data for the segment is
C     available at one time.
C
C     C
C     C     Begin the segment.
C     C
C           CALL CKW04B ( HANDLE, BEGTIM, INST, REF, AVFLAG, SEGID )
C     C
C     C     Add the data to the segment all at once.
C     C
C           CALL CKW04A ( HANDLE, N, NPKTS, RECRDS, SCSTRT )
C     C
C     C     End the segment, making the segment a permanent
C     C     addition to the CK file.
C     C
C           CALL CKW04E ( HANDLE, ENDTIM )
C
C$ Restrictions
C
C     1) The type 4 CK segment to which the data is added must have
C        been started by the routine CKW04B, the routine which begins
C        a type 4 CK segment.
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
C-    SPICELIB Version 1.1.2, 18-APR-2014 (BVS)
C
C        Minor header edits.
C
C-    SPICELIB Version 1.1.1, 26-FEB-2008 (NJB)
C
C        Updated header; added information about SPICE 
C        quaternion conventions.
C
C-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
C
C        Removed DAFHLU call; replaced ERRFNM call with ERRHAN.
C        Added IMPLICIT NONE.
C
C-    SPICELIB Version 1.0.0, 05-MAY-1999 (YKZ) (BVS)
C
C-&
 
C$ Index_Entries
C
C     add data to a type_4 ck segment
C
C-&
 
C
C     Spicelib functions.
C
      LOGICAL               RETURN
 
C
C     Local parameters.
C
 
C
C     The number of elements by which coefficients in each packet
C     have to be shifted to the left after numbers of coefficients
C     were packed into a single integer.
C
      INTEGER               SHFTAD
      PARAMETER           ( SHFTAD = 6 )
 
C
C     Local Variables.
C
      INTEGER               K
      INTEGER               KK
      INTEGER               NUMCFT ( QAVSIZ )
      INTEGER               DISPLM
      INTEGER               DISPM
 
C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKW04A' )
      END IF
 
C
C     First, check if the number of coefficient sets and epochs
C     is positive and whether each packet is smaller than the
C     maximum size of a record that CKPFS can handle.
C
      DO K = 1, NPKTS
 
         IF ( PKTSIZ( K ) .LE. 0 ) THEN
 
            CALL SETMSG ( 'The number of coefficient sets and '      //
     .                    'epochs in the # data packet (record) '    //
     .                    'to be added to the DAF segment '          //
     .                    'in the file ''#'' was not positive. '     //
     .                    'Its value was: #.'                        )
            CALL ERRINT ( '#', K                                     )
            CALL ERRHAN ( '#', HANDLE                                )
            CALL ERRINT ( '#', PKTSIZ( K )                           )
            CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'                   )
            CALL CHKOUT ( 'CKW04A'                                   )
            RETURN
 
         END IF
 
C
C        We do .GE. comparison because a type 4 CK record passed
C        inside CKPFS will have one more element -- time at which
C        the pointing will be evaluated.
C
         IF ( PKTSIZ( K ) .GE. CK4RSZ ) THEN
 
            CALL SETMSG ( 'The total size of the # data packet '     //
     .                    '(record) to be added to the DAF segment ' //
     .                    'in the file ''#'' is greater than the '   //
     .                    'maximum allowed type 4 record size #. '   //
     .                    'Its value was: #.'                        )
            CALL ERRINT ( '#', K                                     )
            CALL ERRHAN ( '#', HANDLE                                )
            CALL ERRINT ( '#', CK4RSZ - 1                            )
            CALL ERRINT ( '#', PKTSIZ( K )                           )
            CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'                   )
            CALL CHKOUT ( 'CKW04A'                                   )
            RETURN
 
         END IF
 
 
      END DO
 
      DISPLM = 0
      DISPM = 0
C
C     The cycle below encodes groups of numbers of coefficients in
C     data packets to single double precision numbers and shift
C     data in packets to the left to decrease the data packet
C     lengths.
C
      DO K = 1, NPKTS
 
C
C        Encode integer numbers of coefficients for each component
C        to single double precision variable
C
         DO KK = 1, QAVSIZ
            NUMCFT( KK ) = INT ( PKTDAT( KK + 2 + DISPLM ) )
         END DO
 
         CALL ZZCK4I2D ( NUMCFT, QAVSIZ, CK4PCD, PKTDAT( 3 + DISPM ) )
 
C
C        Shift coefficients sets to the left to overwrite numbers of
C        packets
C
         DO KK = 4, PKTSIZ( K )
            PKTDAT( KK + DISPM ) = PKTDAT( KK + SHFTAD + DISPLM )
         END DO
 
C
C        Shift middle value and radii of interval
C
         PKTDAT( 1 + DISPM ) = PKTDAT( 1 + DISPLM )
         PKTDAT( 2 + DISPM ) = PKTDAT( 2 + DISPLM )
 
         DISPLM = DISPLM + PKTSIZ ( K )
 
C
C        Length of each data packet became less for 6 elements because
C        of encoding of 7 double precision numbers, which are the
C        numbers of polynomial coefficients, to one double precision
C        number
C
         PKTSIZ( K ) = PKTSIZ( K ) - SHFTAD
         DISPM = DISPM + PKTSIZ ( K )
 
      END DO
 
C
C     Add the data.
C
      CALL SGWVPK ( HANDLE, NPKTS, PKTSIZ, PKTDAT, NPKTS, SCLKDP )
 
C
C     No need to check FAILED() here, since all we do is check out.
C     Leave it up to the caller.
C
      CALL CHKOUT ( 'CKW04A' )
 
      RETURN
 
      END
