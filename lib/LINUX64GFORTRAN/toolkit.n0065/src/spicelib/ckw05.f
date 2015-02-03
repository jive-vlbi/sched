C$Procedure      CKW05 ( Write CK segment, type 5 )
 
      SUBROUTINE CKW05 ( HANDLE,  SUBTYP,  DEGREE,  BEGTIM,  ENDTIM, 
     .                   INST,    REF,     AVFLAG,  SEGID,   N,  
     .                   SCLKDP,  PACKTS,  RATE,    NINTS,   STARTS  )
  
C$ Abstract
C
C     Write a type 5 segment to a CK file.
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
C     NAIF_IDS
C     ROTATION
C     TIME
C
C$ Keywords
C
C     POINTING
C     FILES
C
C$ Declarations
 
      IMPLICIT NONE 

      INCLUDE 'ck05.inc'
      
      INTEGER               HANDLE
      INTEGER               SUBTYP
      INTEGER               DEGREE
      DOUBLE PRECISION      BEGTIM
      DOUBLE PRECISION      ENDTIM
      INTEGER               INST
      CHARACTER*(*)         REF
      LOGICAL               AVFLAG
      CHARACTER*(*)         SEGID
      INTEGER               N
      DOUBLE PRECISION      SCLKDP ( * )
      DOUBLE PRECISION      PACKTS ( * )
      DOUBLE PRECISION      RATE
      INTEGER               NINTS
      DOUBLE PRECISION      STARTS ( * )

      INTEGER               MAXDEG
      PARAMETER           ( MAXDEG = 23 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of an CK file open for writing.
C     SUBTYP     I   CK type 5 subtype code.
C     DEGREE     I   Degree of interpolating polynomials.
C     BEGTIM     I   Start time of interval covered by segment.
C     ENDTIM     I   End time of interval covered by segment.
C     INST       I   NAIF code for a s/c instrument or structure.
C     REF        I   Reference frame name.
C     AVFLAG     I   True if the segment will contain angular velocity.
C     SEGID      I   Segment identifier.
C     N          I   Number of packets.
C     SCLKDP     I   Encoded SCLK times.
C     PACKTS     I   Array of packets.
C     RATE       I   Nominal SCLK rate in seconds per tick.
C     NINTS      I   Number of intervals.
C     STARTS     I   Encoded SCLK interval start times.
C     MAXDEG     P   Maximum allowed degree of interpolating polynomial.
C
C$ Detailed_Input
C
C     HANDLE         is the file handle of a CK file that has been
C                    opened for writing.
C
C     SUBTYP         is an integer code indicating the subtype of the
C                    the segment to be created.  
C
C     DEGREE         is the degree of the polynomials used to
C                    interpolate the quaternions contained in the input
C                    packets.  All components of the quaternions are
C                    interpolated by polynomials of fixed degree.
C
C     BEGTIM,
C     ENDTIM         are the beginning and ending encoded SCLK times
C                    for which the segment provides pointing
C                    information. BEGTIM must be less than or equal to
C                    ENDTIM, and at least one data packet must have a
C                    time tag T such that
C
C                       BEGTIM  <  T  <  ENDTIM
C                               -     -
C
C     INST           is the NAIF integer code for the instrument or
C                    structure for which a segment is to be created.
C
C     REF            is the NAIF name for a reference frame relative to
C                    which the pointing information for INST is 
C                    specified.
C
C     AVFLAG         is a logical flag which indicates whether or not
C                    the segment will contain angular velocity.
C
C     SEGID          is the segment identifier.  A CK segment
C                    identifier may contain up to 40 characters.
C
C     N              is the number of packets in the input packet
C                    array.
C
C     SCLKDP         are the encoded spacecraft clock times associated
C                    with each pointing instance. These times must be
C                    strictly increasing.
C
C     PACKTS         contains a time-ordered array of data packets
C                    representing the orientation of INST relative to
C                    the frame REF. Each packet contains a SPICE-style
C                    quaternion and optionally, depending on the
C                    segment subtype, attitude derivative data, from
C                    which a C-matrix and an angular velocity vector
C                    may be derived.
C
C                    See the discussion of quaternion styles in
C                    Particulars below.
C
C                    The C-matrix represented by the Ith data packet is
C                    a rotation matrix that transforms the components
C                    of a vector expressed in the base frame specified
C                    by REF to components expressed in the instrument
C                    fixed frame at the time SCLKDP(I).
C
C                    Thus, if a vector V has components x, y, z in the
C                    base frame, then V has components x', y', z'
C                    in the instrument fixed frame where:
C
C                       [ x' ]     [          ] [ x ]
C                       | y' |  =  |   CMAT   | | y |
C                       [ z' ]     [          ] [ z ]
C
C
C                    The attitude derivative information in PACKTS(I)
C                    gives the angular velocity of the instrument fixed
C                    frame at time SCLKDP(I) with respect to the
C                    reference frame specified by REF.
C
C                    The direction of an angular velocity vector gives
C                    the right-handed axis about which the instrument
C                    fixed reference frame is rotating. The magnitude
C                    of the vector is the magnitude of the
C                    instantaneous velocity of the rotation, in radians
C                    per second.
C
C                    Packet contents and the corresponding
C                    interpolation methods depend on the segment
C                    subtype, and are as follows:
C
C                       Subtype 0:  Hermite interpolation, 8-element
C                                   packets. Quaternion and quaternion
C                                   derivatives only, no angular
C                                   velocity vector provided.
C                                   Quaternion elements are listed
C                                   first, followed by derivatives.
C                                   Angular velocity is derived from
C                                   the quaternions and quaternion
C                                   derivatives.
C
C                       Subtype 1:  Lagrange interpolation, 4-element
C                                   packets. Quaternion only. Angular
C                                   velocity is derived by
C                                   differentiating the interpolating
C                                   polynomials.
C
C                       Subtype 2:  Hermite interpolation, 14-element
C                                   packets.  Quaternion and angular
C                                   angular velocity vector, as well as
C                                   derivatives of each, are provided.
C                                   The quaternion comes first, then
C                                   quaternion derivatives, then
C                                   angular velocity and its
C                                   derivatives.
C
C                       Subtype 3:  Lagrange interpolation, 7-element
C                                   packets. Quaternion and angular
C                                   velocity vector provided.  The 
C                                   quaternion comes first.
C
C                    Angular velocity is always specified relative to
C                    the base frame.    
C
C     RATE           is the nominal rate of the spacecraft clock 
C                    associated with INST.  Units are seconds per
C                    tick.  RATE is used to scale angular velocity
C                    to radians/second.
C
C     NINTS          is the number of intervals that the pointing
C                    instances are partitioned into.
C
C     STARTS         are the start times of each of the interpolation
C                    intervals. These times must be strictly increasing
C                    and must coincide with times for which the segment
C                    contains pointing.
C
C$ Detailed_Output
C
C     None.  See $Particulars for a description of the effect of this
C     routine.
C
C$ Parameters
C
C     MAXDEG         is the maximum allowed degree of the interpolating
C                    polynomial.  If the value of MAXDEG is increased,
C                    the SPICELIB routine CKPFS must be changed
C                    accordingly.  In particular, the size of the
C                    record passed to CKRnn and CKEnn must be
C                    increased, and comments describing the record size
C                    must be changed.
C
C$ Exceptions
C
C     If any of the following exceptions occur, this routine will return
C     without creating a new segment.
C
C     1)  If HANDLE is not the handle of a C-kernel opened for writing
C         the error will be diagnosed by routines called by this
C         routine.
C
C     2)  If the last non-blank character of SEGID occurs past index 40,
C         the error SPICE(SEGIDTOOLONG) is signaled.
C
C     3)  If SEGID contains any nonprintable characters, the error
C         SPICE(NONPRINTABLECHARS) is signaled.
C
C     4)  If the first encoded SCLK time is negative then the error
C         SPICE(INVALIDSCLKTIME) is signaled. If any subsequent times
C         are negative the error will be detected in exception (5).
C
C     5)  If the encoded SCLK times are not strictly increasing,
C         the error SPICE(TIMESOUTOFORDER) is signaled.
C
C     6)  If the name of the reference frame is not one of those
C         supported by the routine FRAMEX, the error
C         SPICE(INVALIDREFFRAME) is signaled.
C
C     7)  If the number of packets N is not at least 1, the error 
C         SPICE(TOOFEWPACKETS) will be signaled.
C
C     8)  If NINTS, the number of interpolation intervals, is less than
C         or equal to 0, the error SPICE(INVALIDNUMINTS) is signaled.
C
C     9)  If the encoded SCLK interval start times are not strictly
C         increasing, the error SPICE(TIMESOUTOFORDER) is signaled.
C
C    10)  If an interval start time does not coincide with a time for
C         which there is an actual pointing instance in the segment,
C         then the error SPICE(INVALIDSTARTTIME) is signaled.
C
C    11)  This routine assumes that the rotation between adjacent
C         quaternions that are stored in the same interval has a
C         rotation angle of THETA radians, where
C
C            0  <  THETA  <  pi.
C               _
C
C         The routines that evaluate the data in the segment produced
C         by this routine cannot distinguish between rotations of THETA
C         radians, where THETA is in the interval [0, pi), and
C         rotations of
C
C            THETA   +   2 * k * pi
C
C         radians, where k is any integer.  These "large" rotations will
C         yield invalid results when interpolated.  You must ensure that
C         the data stored in the segment will not be subject to this
C         sort of ambiguity.
C
C    12)  If any quaternion has magnitude zero, the error
C         SPICE(ZEROQUATERNION) is signaled.
C
C    13)  If the interpolation window size implied by DEGREE is not
C         even, the error SPICE(INVALIDDEGREE) is signaled.  The window
C         size is DEGREE+1 for Lagrange subtypes and is (DEGREE+1)/2
C         for Hermite subtypes.
C
C    14)  If an unrecognized subtype code is supplied, the error 
C         SPICE(NOTSUPPORTED) is signaled.
C
C    15)  If DEGREE is not at least 1 or is greater than MAXDEG, the
C         error SPICE(INVALIDDEGREE) is signaled.
C
C    16)  If the segment descriptor bounds are out of order, the
C         error SPICE(BADDESCRTIMES) is signaled.
C
C    17)  If there is no element of SCLKDP that lies between BEGTIM and
C         ENDTIM inclusive, the error SPICE(EMPTYSEGMENT) is signaled.
C
C    18)  If RATE is zero, the error SPICE(INVALIDVALUE) is signaled.
C
C
C$ Files
C
C     A new type 5 CK segment is written to the CK file attached
C     to HANDLE.
C
C$ Particulars
C
C     This routine writes a CK type 5 data segment to the open CK
C     file according to the format described in the type 5 section of
C     the CK Required Reading. The CK file must have been opened with
C     write access.
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
C     Suppose that you have data packets and are prepared to produce
C     a segment of type 5 in a CK file.
C
C     The following code fragment could be used to add the new segment
C     to a previously opened CK file attached to HANDLE. The file must
C     have been opened with write access.
C
C        C
C        C     Create a segment identifier.
C        C
C              SEGID = 'MY_SAMPLE_CK_TYPE_5_SEGMENT'
C
C        C
C        C     Write the segment.
C        C
C              CALL CKW05 ( HANDLE, SUBTYP, DEGREE, BEGTIM, ENDTIM,
C             .             INST,   REF,    AVFLAG, SEGID,  N,
C             .             SCLKDP, PACKTS, RATE,   NINTS,  STARTS )
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
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C     K.R. Gehringer  (JPL)
C     J.M. Lynch      (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.0.0, 27-JAN-2014 (NJB)
C
C        Increased MAXDEG to 23 for compatibility with CK type 6.
C
C-    SPICELIB Version 2.0.0, 08-FEB-2010 (NJB)
C
C        The check for non-unit quaternions has been replaced
C        with a check for zero-length quaternions.
C
C-    SPICELIB Version 1.1.0, 26-FEB-2008 (NJB)
C
C        Updated header; added information about SPICE 
C        quaternion conventions.
C
C        Minor typo in a long error message was corrected.
C
C-    SPICELIB Version 1.0.1, 07-JAN-2005 (NJB)  
C
C        Description in Detailed_Input header section of 
C        constraints on BEGTIM and ENDTIM was corrected.
C
C-    SPICELIB Version 1.0.0, 30-AUG-2002 (NJB) (KRG) (JML) (WLT)
C
C-&
 
C$ Index_Entries
C
C     write ck type_5 data segment
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 08-FEB-2010 (NJB)
C
C        The check for non-unit quaternions has been replaced
C        with a check for zero-length quaternions.
C
C        This change was made to accommodate CK generation,
C        via the non-SPICE utility MEX2KER, for European missions. 
C        
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               VZEROG

      INTEGER               BSRCHD
      INTEGER               LASTNB
      INTEGER               LSTLTD
      
      LOGICAL               FAILED
      LOGICAL               ODD
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN  =  40 )
 
      INTEGER               FPRINT
      PARAMETER           ( FPRINT  =  32 )
 
      INTEGER               LPRINT
      PARAMETER           ( LPRINT  = 126 )
 
      INTEGER               ND
      PARAMETER           ( ND      =   2 )
 
      INTEGER               NI
      PARAMETER           ( NI      =   6 )

      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ  =   5 )
      
      INTEGER               TYPIDX
      PARAMETER           ( TYPIDX  =   4 )
      
      INTEGER               DTYPE
      PARAMETER           ( DTYPE   =   5 )

      INTEGER               DIRSIZ
      PARAMETER           ( DIRSIZ  = 100 )
 
      INTEGER               STATSZ
      PARAMETER           ( STATSZ  =   6 )

C
C     Packet structure parameters
C
      INTEGER               QIDX
      PARAMETER           ( QIDX   = 1 )

C
C     Local variables
C
      DOUBLE PRECISION      DC     ( ND )
      DOUBLE PRECISION      DESCR  ( DSCSIZ )
 
      INTEGER               ADDR
      INTEGER               CHRCOD
      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               PACKSZ
      INTEGER               REFCOD
      INTEGER               WINSIZ
      
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKW05' )
      END IF

C
C     Make sure that the number of packets is positive.
C
      IF ( N .LT. 1 ) THEN
 
         CALL SETMSG ( 'At least 1 packet is required for CK type 5. '//
     .                 'Number of packets supplied:  #'                )
         CALL ERRINT ( '#', N                                          )
         CALL SIGERR ( 'SPICE(TOOFEWPACKETS)'                          )
         CALL CHKOUT ( 'CKW05'                                         )
         RETURN
 
      END IF

C
C     Make sure that there is a positive number of interpolation
C     intervals.
C
      IF ( NINTS .LE. 0 ) THEN
 
         CALL SETMSG ( '# is an invalid number of interpolation ' //
     .                 'intervals for type 5.'                     )
         CALL ERRINT ( '#', NINTS                                  )
         CALL SIGERR ( 'SPICE(INVALIDNUMINTS)'                     )
         CALL CHKOUT ( 'CKW05'                                     )
         RETURN
 
      END IF


C
C     Get the NAIF integer code for the reference frame.
C
      CALL NAMFRM ( REF, REFCOD )
 
      IF ( REFCOD .EQ. 0 ) THEN
 
         CALL SETMSG ( 'The reference frame # is not supported.'   )
         CALL ERRCH  ( '#', REF                                    )
         CALL SIGERR ( 'SPICE(INVALIDREFFRAME)'                    )
         CALL CHKOUT ( 'CKW05'                                     )
         RETURN
 
      END IF
 
C
C     Check to see if the segment identifier is too long.
C
      IF ( LASTNB(SEGID) .GT. SIDLEN ) THEN
 
         CALL SETMSG ( 'Segment identifier contains more than ' //
     .                 '40 characters.'                          )
         CALL SIGERR ( 'SPICE(SEGIDTOOLONG)'                     )
         CALL CHKOUT ( 'CKW05'                                   )
         RETURN
 
      END IF
 
C
C     Now check that all the characters in the segment identifier
C     can be printed.
C
      DO I = 1, LASTNB(SEGID)
 
         CHRCOD = ICHAR( SEGID(I:I) )
 
         IF ( ( CHRCOD .LT. FPRINT ) .OR. ( CHRCOD .GT. LPRINT ) ) THEN
 
            CALL SETMSG ( 'The segment identifier contains '  //
     .                    'nonprintable characters'               )
            CALL SIGERR ( 'SPICE(NONPRINTABLECHARS)'              )
            CALL CHKOUT ( 'CKW05'                                 )
            RETURN
 
         END IF
 
      END DO

C
C     Now check that the encoded SCLK times are positive and strictly
C     increasing.
C
C     Check that the first time is nonnegative.
C
      IF ( SCLKDP(1) .LT. 0.D0 ) THEN
 
         CALL SETMSG ( 'The first SCLKDP time: # is negative.' )
         CALL ERRDP  ( '#', SCLKDP(1)                          )
         CALL SIGERR ( 'SPICE(INVALIDSCLKTIME)'                )
         CALL CHKOUT ( 'CKW05'                                 )
         RETURN
 
      END IF
 
C
C     Now check that the times are ordered properly.
C
      DO I = 2, N
 
         IF ( SCLKDP(I) .LE. SCLKDP(I-1) )  THEN
 
            CALL SETMSG ( 'The SCLKDP times are not strictly '      //
     .                    'increasing. SCLKDP(#) = # and SCLKDP(#) '//
     .                    '= #.'                                )
            CALL ERRINT ( '#', I                                )
            CALL ERRDP  ( '#', SCLKDP(I)                        )
            CALL ERRINT ( '#', I-1                              )
            CALL ERRDP  ( '#', SCLKDP(I-1)                      )
            CALL SIGERR ( 'SPICE(TIMESOUTOFORDER)'              )
            CALL CHKOUT ( 'CKW05'                               )
            RETURN
 
         END IF
 
      END DO
 
C
C     Now check that the interval start times are ordered properly.
C
      DO I = 2, NINTS
 
         IF ( STARTS(I) .LE. STARTS(I-1) )  THEN
 
            CALL SETMSG ( 'The interval start times are not strictly '//
     .                    'increasing. STARTS(#) = # and STARTS(#) '  //
     .                    '= #.'                                )
            CALL ERRINT ( '#', I                                )
            CALL ERRDP  ( '#', STARTS(I)                        )
            CALL ERRINT ( '#', I-1                              )
            CALL ERRDP  ( '#', STARTS(I-1)                      )
            CALL SIGERR ( 'SPICE(TIMESOUTOFORDER)'              )
            CALL CHKOUT ( 'CKW05'                               )
            RETURN
 
         END IF
 
      END DO
 
 
C
C     Now make sure that all of the interval start times coincide with
C     one of the times associated with the actual pointing.
C 
      DO I = 1, NINTS
C
C        We know the SCLKDP array is ordered, so a binary search is
C        ok.
C 
         IF (  BSRCHD ( STARTS(I), N, SCLKDP )  .EQ.  0  ) THEN
 
            CALL SETMSG ( 'Interval start time number # is invalid. '//
     .                    'STARTS(#) = *'                             )
            CALL ERRINT ( '#', I                                      )
            CALL ERRINT ( '#', I                                      )
            CALL ERRDP  ( '*', STARTS(I)                              )
            CALL SIGERR ( 'SPICE(INVALIDSTARTTIME)'                   )
            CALL CHKOUT ( 'CKW05'                                     )
            RETURN
 
         END IF
 
      END DO
 

 
C
C     Set the window, packet size and angular velocity flag, all of 
C     which are functions of the subtype.
C
      IF (  SUBTYP .EQ. C05TP0 ) THEN

         WINSIZ = ( DEGREE + 1 ) / 2
         PACKSZ = C05PS0

      ELSE IF ( SUBTYP .EQ. C05TP1  ) THEN

         WINSIZ = DEGREE + 1
         PACKSZ = C05PS1

      ELSE IF (  SUBTYP .EQ. C05TP2 ) THEN

         WINSIZ = ( DEGREE + 1 ) / 2
         PACKSZ = C05PS2

      ELSE IF ( SUBTYP .EQ. C05TP3  ) THEN

         WINSIZ = DEGREE + 1
         PACKSZ = C05PS3

      ELSE

         CALL SETMSG ( 'CK type 5 subtype <#> is not supported.' )
         CALL ERRINT ( '#', SUBTYP                               )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                     )
         CALL CHKOUT ( 'CKW05'                                   )
         RETURN

      END IF

C
C     Make sure that the quaternions are non-zero. This is just
C     a check for uninitialized data.
C
      DO I = 1, N 
C
C        We have to address the quaternion explicitly, since the shape
C        of the packet array is not known at compile time.
C
         ADDR = PACKSZ * (I-1)  +  QIDX

         IF (  VZEROG( PACKTS(ADDR), 4 )  ) THEN
 
            CALL SETMSG ( 'The quaternion at index # has magnitude '
     .      //            'zero.'                                    )
            CALL ERRINT ( '#', I                                     )
            CALL SIGERR ( 'SPICE(ZEROQUATERNION)'                    )
            CALL CHKOUT ( 'CKW05'                                    )
            RETURN
 
         END IF
 
      END DO

C
C     Make sure that the degree of the interpolating polynomials is
C     in range.
C
      IF (  ( DEGREE .LT. 1 ) .OR. ( DEGREE .GT. MAXDEG )  )  THEN
 
         CALL SETMSG ( 'The interpolating polynomials have degree #; '//
     .                 'the valid degree range is [1, #]'              )
         CALL ERRINT ( '#', DEGREE                                     )
         CALL ERRINT ( '#', MAXDEG                                     )
         CALL SIGERR ( 'SPICE(INVALIDDEGREE)'                          )
         CALL CHKOUT ( 'CKW05'                                         )
         RETURN
 
      END IF
 
C
C     Make sure that the window size is even.  If not, the input
C     DEGREE is incompatible with the subtype.
C
      IF (  ODD(WINSIZ)  )  THEN
 
         CALL SETMSG ( 'The interpolating polynomials have degree #; '//
     .                 'for CK type 5, the degree must be '           //
     .                 'equivalent to 3 mod 4 for Hermite '           //
     .                 'interpolation and odd for for Lagrange '      //
     .                 'interpolation.'                                )
         CALL ERRINT ( '#', DEGREE                                     )
         CALL SIGERR ( 'SPICE(INVALIDDEGREE)'                          )
         CALL CHKOUT ( 'CKW05'                                         )
         RETURN
 
      END IF


C
C     If we made it this far, we're ready to start writing the segment.
C
C     Create the segment descriptor.
C
C     Assign values to the integer components of the segment descriptor.
C
      IC ( 1 ) = INST
      IC ( 2 ) = REFCOD
      IC ( 3 ) = DTYPE
 
      IF ( AVFLAG ) THEN
         IC ( 4 ) = 1
      ELSE
         IC ( 4 ) = 0
      END IF
 
      DC( 1 ) = BEGTIM
      DC( 2 ) = ENDTIM

C
C     Make sure the descriptor times are in increasing order.
C
      IF ( ENDTIM .LT. BEGTIM ) THEN
 
         CALL SETMSG ( 'Descriptor bounds are non-increasing: #:#' )
         CALL ERRDP  ( '#', BEGTIM                                 )
         CALL ERRDP  ( '#', ENDTIM                                 )
         CALL SIGERR ( 'SPICE(BADDESCRTIMES)'                      )
         CALL CHKOUT ( 'CKW05'                                     )
         RETURN
 
      END IF

C
C     Make sure that at least one time tag lies between BEGTIM and
C     ENDTIM.  The first time tag not less than BEGTIM must exist
C     and must be less than or equal to ENDTIM.
C
      I = LSTLTD ( BEGTIM, N, SCLKDP )


      IF ( I .EQ. N ) THEN

         CALL SETMSG ( 'All time tags are less than segment start ' //
     .                 'time #.'                                  )
         CALL ERRDP  ( '#',  BEGTIM                               )
         CALL SIGERR ( 'SPICE(EMPTYSEGMENT)'                      )
         CALL CHKOUT ( 'CKW05'                                    )
         RETURN
         
      ELSE IF (  SCLKDP(I+1)  .GT.  ENDTIM  ) THEN

         CALL SETMSG ( 'No time tags lie between the segment start ' //
     .                 'time # and segment end time #'              )
         CALL ERRDP  ( '#',  BEGTIM                                 )
         CALL ERRDP  ( '#',  ENDTIM                                 )
         CALL SIGERR ( 'SPICE(EMPTYSEGMENT)'                        )
         CALL CHKOUT ( 'CKW05'                                      )
         RETURN
 
      END IF

C
C     The clock rate must be non-zero.
C
      IF ( RATE .EQ. 0.D0 ) THEN

         CALL SETMSG ( 'The SCLK rate RATE was zero.'  )
         CALL SIGERR ( 'SPICE(INVALIDVALUE)'           )
         CALL CHKOUT ( 'CKW05'                         )
         RETURN
         
      END IF

C
C     Now pack the segment descriptor.
C
      CALL DAFPS ( ND, NI, DC, IC, DESCR )
 
C
C     Begin a new segment.
C
      CALL DAFBNA ( HANDLE, DESCR, SEGID )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'CKW05' )
         RETURN
      END IF
      
      
C
C     The type 5 segment structure is eloquently described by this
C     diagram from the CK Required Reading:
C
C        +-----------------------+
C        | Packet 1              |
C        +-----------------------+
C        | Packet 2              |
C        +-----------------------+
C                    .
C                    .
C                    .
C        +-----------------------+
C        | Packet N              |
C        +-----------------------+
C        | Epoch 1               |
C        +-----------------------+
C        | Epoch 2               |
C        +-----------------------+
C                    .
C                    .
C                    .
C        +----------------------------+
C        | Epoch N                    |
C        +----------------------------+
C        | Epoch 100                  | (First directory)
C        +----------------------------+
C                    .
C                    .
C                    .
C        +----------------------------+
C        | Epoch ((N-1)/100)*100      | (Last directory)
C        +----------------------------+
C        | Start time 1               |
C        +----------------------------+
C        | Start time 2               |
C        +----------------------------+
C                    .
C                    .
C                    .
C        +----------------------------+
C        | Start time M               |
C        +----------------------------+
C        | Start time 100             | (First interval start
C        +----------------------------+  time directory)
C                    .
C                    .
C                    .
C        +----------------------------+
C        | Start time ((M-1)/100)*100 | (Last interval start 
C        +----------------------------+  time directory)
C        | Seconds per tick           |  
C        +----------------------------+
C        | Subtype code               |
C        +----------------------------+
C        | Window size                |
C        +----------------------------+
C        | Number of interp intervals |
C        +----------------------------+
C        | Number of packets          |
C        +----------------------------+
C
C
      CALL DAFADA ( PACKTS,  N*PACKSZ )
      CALL DAFADA ( SCLKDP,  N        )
 
      DO I = 1,   (N-1) / DIRSIZ

         CALL DAFADA (  SCLKDP( DIRSIZ*I ),  1  )

      END DO
 
C
C     Now add the interval start times.
C
      CALL DAFADA ( STARTS, NINTS )
 
C
C     And the directory of interval start times.  The directory of
C     start times will simply be every (DIRSIZ)th start time.
C 
      DO I = 1, (NINTS -1) / DIRSIZ
 
         CALL DAFADA ( STARTS( DIRSIZ*I ), 1 )

      END DO

C
C     Add the SCLK rate, segment subtype, window size, interval
C     count, and packet count.
C
      CALL DAFADA ( RATE,            1 )
      CALL DAFADA ( DBLE( SUBTYP ),  1 )
      CALL DAFADA ( DBLE( WINSIZ ),  1 )
      CALL DAFADA ( DBLE( NINTS  ),  1 )
      CALL DAFADA ( DBLE( N      ),  1 )
 
C
C     As long as nothing went wrong, end the segment.
C
      IF ( .NOT. FAILED() ) THEN
         CALL DAFENA
      END IF
 
 
      CALL CHKOUT ( 'CKW05' )
      RETURN
      END
