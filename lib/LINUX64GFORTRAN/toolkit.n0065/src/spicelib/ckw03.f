C$Procedure  CKW03 ( C-Kernel, write segment to C-kernel, data type 3 )
 
      SUBROUTINE CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG,
     .                   SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS,
     .                   STARTS                                      )
 
C$ Abstract
C
C     Add a type 3 segment to a C-kernel.
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
C     ROTATION
C     SCLK
C
C$ Keywords
C
C     POINTING
C     UTILITY
C
C$ Declarations
 
      IMPLICIT NONE

      INTEGER               HANDLE
      DOUBLE PRECISION      BEGTIM
      DOUBLE PRECISION      ENDTIM
      INTEGER               INST
      CHARACTER*(*)         REF
      LOGICAL               AVFLAG
      CHARACTER*(*)         SEGID
      INTEGER               NREC
      DOUBLE PRECISION      SCLKDP (      * )
      DOUBLE PRECISION      QUATS  ( 0:3, * )
      DOUBLE PRECISION      AVVS   (   3, * )
      INTEGER               NINTS
      DOUBLE PRECISION      STARTS (      * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of an open CK file.
C     BEGTIM     I   Beginning encoded SCLK of the segment.
C     ENDTIM     I   Ending encoded SCLK of the segment.
C     INST       I   NAIF instrument ID code.
C     REF        I   Reference frame of the segment.
C     AVFLAG     I   True if the segment will contain angular velocity.
C     SEGID      I   Segment identifier.
C     NREC       I   Number of pointing records.
C     SCLKDP     I   Encoded SCLK times.
C     QUATS      I   SPICE quaternions representing instrument pointing.
C     AVVS       I   Angular velocity vectors.
C     NINTS      I   Number of intervals.
C     STARTS     I   Encoded SCLK interval start times.
C
C$ Detailed_Input
C
C     HANDLE     is the handle of the CK file to which the segment will
C                be written. The file must have been opened with write
C                access.
C
C     BEGTIM,    are the beginning and ending encoded SCLK times for
C     ENDTIM     which the segment provides pointing information.
C                BEGTIM must be less than or equal to the SCLK time
C                associated with the first pointing instance in the
C                segment, and ENDTIM must be greater than or equal to
C                the time associated with the last pointing instance
C                in the segment.
C
C     INST       is the NAIF integer ID code for the instrument that
C                this segment will contain pointing information for.
C
C     REF        is a character string which specifies the inertial
C                reference frame of the segment.
C
C                The rotation matrices represented by the quaternions
C                that are to be written to the segment transform the
C                components of vectors from the inertial reference frame
C                specified by REF to components in the instrument fixed
C                frame. Also, the components of the angular velocity
C                vectors to be written to the segment should be given
C                with respect to REF.
C
C                REF should be the name of one of the frames supported
C                by the SPICELIB routine FRAMEX.
C
C     AVFLAG     is a logical flag which indicates whether or not the
C                segment will contain angular velocity.
C
C     SEGID      is the segment identifier. A CK segment identifier may
C                contain up to 40 printable characters and spaces.
C
C     NREC       is the number of pointing instances in the segment.
C
C     SCLKDP     are the encoded spacecraft clock times associated with
C                each pointing instance. These times must be strictly
C                increasing.
C
C     QUATS      is an array of SPICE-style quaternions representing
C                a sequence of C-matrices. See the discussion of
C                quaternion styles in Particulars below.
C
C                The C-matrix represented by the Ith quaternion in
C                QUATS is a rotation matrix that transforms the
C                components of a vector expressed in the inertial
C                frame specified by REF to components expressed in
C                the instrument fixed frame at the time SCLKDP(I).
C
C                Thus, if a vector V has components x, y, z in the
C                inertial frame, then V has components x', y', z' in
C                the instrument fixed frame where:
C
C                     [ x' ]     [          ] [ x ]
C                     | y' |  =  |   CMAT   | | y |
C                     [ z' ]     [          ] [ z ]
C
C     AVVS       are the angular velocity vectors ( optional ).
C
C                The Ith vector in AVVS gives the angular velocity of
C                the instrument fixed frame at time SCLKDP(I). The
C                components of the angular velocity vectors should
C                be given with respect to the inertial reference frame
C                specified by REF.
C
C                The direction of an angular velocity vector gives
C                the right-handed axis about which the instrument fixed
C                reference frame is rotating. The magnitude of the
C                vector is the magnitude of the instantaneous velocity
C                of the rotation, in radians per second.
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
C                and must coincide with times for which the segment
C                contains pointing.
C
C$ Detailed_Output
C
C     None.  See Files section.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If HANDLE is not the handle of a C-kernel opened for writing
C         the error will be diagnosed by routines called by this
C         routine.
C
C     2)  If SEGID is more than 40 characters long, the error
C         SPICE(SEGIDTOOLONG) is signaled.
C
C     3)  If SEGID contains any non-printable characters, the error
C         SPICE(NONPRINTABLECHARS) is signaled.
C
C     4)  If the first encoded SCLK time is negative then the error
C         SPICE(INVALIDSCLKTIME) is signaled. If any subsequent times
C         are negative the error will be detected in exception (5).
C
C     5)  If the encoded SCLK times are not strictly increasing,
C         the error SPICE(TIMESOUTOFORDER) is signaled.
C
C     6)  If BEGTIM is greater than SCLKDP(1) or ENDTIM is less than
C         SCLKDP(NREC), the error SPICE(INVALIDDESCRTIME) is
C         signaled.
C
C     7)  If the name of the reference frame is not one of those
C         supported by the routine FRAMEX, the error
C         SPICE(INVALIDREFFRAME) is signaled.
C
C     8)  If NREC, the number of pointing records, is less than or
C         equal to 0, the error SPICE(INVALIDNUMREC) is signaled.
C
C     9)  If NINTS, the number of interpolation intervals, is less than
C         or equal to 0, the error SPICE(INVALIDNUMINT) is signaled.
C
C    10)  If the encoded SCLK interval start times are not strictly
C         increasing, the error SPICE(TIMESOUTOFORDER) is signaled.
C
C    11)  If an interval start time does not coincide with a time for
C         which there is an actual pointing instance in the segment,
C         then the error SPICE(INVALIDSTARTTIME) is signaled.
C
C    12)  This routine assumes that the rotation between adjacent
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
C         radians, where k is any integer.  These `large' rotations will
C         yield invalid results when interpolated.  You must ensure that
C         the data stored in the segment will not be subject to this
C         sort of ambiguity.
C
C    13)  If any quaternion has magnitude zero, the error
C         SPICE(ZEROQUATERNION) is signaled.
C
C    14)  If the start time of the first interval and the time of the
C         first pointing instance are not the same, the error
C         SPICE(TIMESDONTMATCH) is signaled.
C
C$ Files
C
C     This routine adds a type 3 segment to a C-kernel. The C-kernel
C     may be either a new one or an existing one opened for writing.
C
C$ Particulars
C
C     For a detailed description of a type 3 CK segment please see the
C     CK Required Reading.
C
C     This routine relieves the user from performing the repetitive
C     calls to the DAF routines necessary to construct a CK segment. 
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
C  C
C  C     This example code fragment writes a type 3 C-kernel segment
C  C     for the Mars Observer spacecraft bus to a previously opened CK
C  C     file attached to HANDLE.
C  C
C
C  C
C  C     Assume arrays of quaternions, angular velocities, and the
C  C     associated SCLK times are produced elsewhere.  The software
C  C     that calls CKW03 must then decide how to partition these
C  C     pointing instances into intervals over which linear
C  C     interpolation between adjacent points is valid.
C  C
C        .
C        .
C        .
C
C  C
C  C     The subroutine CKW03 needs the following items for the
C  C     segment descriptor:
C  C
C  C        1) SCLK limits of the segment.
C  C        2) Instrument code.
C  C        3) Reference frame.
C  C        4) The angular velocity flag.
C  C
C        BEGTIM = SCLK (    1 )
C        ENDTIM = SCLK ( NREC )
C
C        INST   = -94000
C        REF    = 'J2000'
C        AVFLAG = .TRUE.
C
C        SEGID  = 'MO SPACECRAFT BUS - DATA TYPE 3'
C
C  C
C  C     Write the segment.
C  C
C        CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG,
C       .             SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS,
C       .             STARTS                                       )
C
C$ Restrictions
C
C     1) The creator of the segment is given the responsibility for
C        determining whether it is reasonable to interpolate between
C        two given pointing values.
C
C    2)  This routine assumes that the rotation between adjacent
C        quaternions that are stored in the same interval has a
C        rotation angle of THETA radians, where
C
C            0  <  THETA  <  pi.
C               _
C
C        The routines that evaluate the data in the segment produced
C        by this routine cannot distinguish between rotations of THETA
C        radians, where THETA is in the interval [0, pi), and
C        rotations of
C
C            THETA   +   2 * k * pi
C
C        radians, where k is any integer.  These `large' rotations will
C        yield invalid results when interpolated.  You must ensure that
C        the data stored in the segment will not be subject to this
C        sort of ambiguity.
C
C     3) All pointing instances in the segment must belong to one and
C        only one of the intervals.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C     K.R. Gehringer  (JPL)
C     J.M. Lynch      (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.0.0, 01-JUN-2010 (NJB)
C
C        The check for non-unit quaternions has been replaced
C        with a check for zero-length quaternions.
C
C-    SPICELIB Version 2.3.0, 26-FEB-2008 (NJB)
C
C        Updated header; added information about SPICE 
C        quaternion conventions.
C
C        Minor typo in a long error message was corrected.
C
C-    SPICELIB Version 2.2.0, 26-SEP-2005 (BVS)
C
C        Added check to ensure that the start time of the first 
C        interval is the same as the time of the first pointing 
C        instance.
C
C-    SPICELIB Version 2.1.0, 22-FEB-1999 (WLT)
C
C        Added check to make sure that all quaternions are unit
C        length to single precision.
C
C-    SPICELIB Version 2.0.0, 28-DEC-1993 (WLT)
C
C        The routine was upgraded to support non-inertial reference
C        frames.
C
C-    SPICELIB Version 1.1.1, 05-SEP-1993 (KRG)
C
C        Removed all references to a specific method of opening the CK
C        file in the $ Brief_I/O, $ Detailed_Input, $ Exceptions,
C        $ Files, and $ Examples sections of the header. It is assumed
C        that a person using this routine has some knowledge of the DAF
C        system and the methods for obtaining file handles.
C
C-    SPICELIB Version 1.0.0, 25-NOV-1992 (JML)
C
C-&
 
C$ Index_Entries
C
C     write ck type_3 pointing data segment
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 2.2.0, 26-SEP-2005 (BVS)
C
C        Added check to ensure that the start time of the first 
C        interval is the same as the time of the first pointing 
C        instance.
C
C-    SPICELIB Version 2.1.0, 22-FEB-1999 (WLT)
C
C        Added check to make sure that all quaternions are unit
C        length to single precision.
C
C-    SPICELIB Version 1.1.1, 05-SEP-1993 (KRG)
C
C        Removed all references to a specific method of opening the CK
C        file in the $ Brief_I/O, $ Detailed_Input, $ Exceptions,
C        $ Files, and $ Examples sections of the header. It is assumed
C        that a person using this routine has some knowledge of the DAF
C        system and the methods for obtaining file handles.
C
C-    SPICELIB Version 1.0.0, 25-NOV-1992 (JML)
C
C-&
 
 
C
C     SPICELIB functions
C
      INTEGER               LASTNB

      LOGICAL               RETURN
      LOGICAL               FAILED
      LOGICAL               VZEROG
 
C
C     Local parameters
C
C     SIDLEN   is the maximum number of characters allowed in a CK
C              segment identifier.
C
C     NDC      is the size of a packed CK segment descriptor.
C
C     ND       is the number of double precision components in a CK
C              segment descriptor.
C
C     NI       is the number of integer components in a CK segment
C              descriptor.
C
C     DTYPE    is the data type of the segment that this routine
C              operates on.
C
C     FPRINT   is the integer value of the first printable ASCII
C              character.
C
C     LPRINT   is the integer value of the last printable ASCII
C              character.
C
C
      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN  =  40 )
 
      INTEGER               NDC
      PARAMETER           ( NDC     =   5 )
 
      INTEGER               ND
      PARAMETER           ( ND      =   2 )
 
      INTEGER               NI
      PARAMETER           ( NI      =   6 )
 
      INTEGER               DTYPE
      PARAMETER           ( DTYPE   =   3 )
 
      INTEGER               FPRINT
      PARAMETER           ( FPRINT  =  32 )
 
      INTEGER               LPRINT
      PARAMETER           ( LPRINT  = 126 )
 
 
C
C     Local variables
C
      DOUBLE PRECISION      DCD    ( ND    )
      DOUBLE PRECISION      DESCR  ( NDC   )
 
 
      INTEGER               I
      INTEGER               ICD    ( NI    )
      INTEGER               INDEX
      INTEGER               NRDIR
      INTEGER               NIDIR
      INTEGER               REFCOD
      INTEGER               VALUE
 
      LOGICAL               MATCH
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'CKW03' )
 
C
C     The first thing that we will do is create the segment descriptor.
C
C     The structure of the segment descriptor is as follows.
C
C           DCD( 1 ) and DCD( 2 ) -- SCLK limits of the segment.
C           ICD( 1 )              -- Instrument code.
C           ICD( 2 )              -- Reference frame ID.
C           ICD( 3 )              -- Data type of the segment.
C           ICD( 4 )              -- Angular rates flag.
C           ICD( 5 )              -- Beginning address of segment.
C           ICD( 6 )              -- Ending address of segment.
C
 
 
C
C     Make sure that there is a positive number of pointing records.
C
      IF ( NREC .LE. 0 ) THEN
 
         CALL SETMSG ( '# is an invalid number of pointing ' //
     .                 'instances for type 3.'                     )
         CALL ERRINT ( '#', NREC                                   )
         CALL SIGERR ( 'SPICE(INVALIDNUMREC)'                      )
         CALL CHKOUT ( 'CKW03'                                     )
         RETURN
 
      END IF
 
C
C     Make sure that there is a positive number of interpolation
C     intervals.
C
      IF ( NINTS .LE. 0 ) THEN
 
         CALL SETMSG ( '# is an invalid number of interpolation ' //
     .                 'intervals for type 3.'                     )
         CALL ERRINT ( '#', NINTS                                  )
         CALL SIGERR ( 'SPICE(INVALIDNUMINT)'                      )
         CALL CHKOUT ( 'CKW03'                                     )
         RETURN
 
      END IF
 
C
C     Check that the SCLK bounds on the segment are reasonable.
C
      IF ( BEGTIM .GT. SCLKDP(1) ) THEN
 
         CALL SETMSG ( 'The segment begin time is greater than the ' //
     .                 'time associated with the first pointing '    //
     .                 'instance in the segment. DCD(1) = # and '    //
     .                 'SCLKDP(1) = # '                               )
 
         CALL ERRDP  ( '#', BEGTIM                                    )
         CALL ERRDP  ( '#', SCLKDP(1)                                 )
         CALL SIGERR ( 'SPICE(INVALIDDESCRTIME)'                      )
         CALL CHKOUT ( 'CKW03'                                        )
         RETURN
 
      END IF
 
      IF ( ENDTIM .LT. SCLKDP(NREC) ) THEN
 
         CALL SETMSG ( 'The segment end time is less than the time '  //
     .                 'associated with the last pointing instance '  //
     .                 'in the segment. DCD(2) = # and SCLKDP(#) = #'  )
 
         CALL ERRDP  ( '#', ENDTIM               )
         CALL ERRINT ( '#', NREC                 )
         CALL ERRDP  ( '#', SCLKDP(NREC)         )
         CALL SIGERR ( 'SPICE(INVALIDDESCRTIME)' )
         CALL CHKOUT ( 'CKW03'                   )
         RETURN
 
      END IF
 
      DCD( 1 ) = BEGTIM
      DCD( 2 ) = ENDTIM
 
 
C
C     Get the NAIF integer code for the reference frame.
C
      CALL NAMFRM ( REF, REFCOD )
 
      IF ( REFCOD .EQ. 0 ) THEN
 
         CALL SETMSG ( 'The reference frame # is not supported.'   )
         CALL ERRCH  ( '#', REF                                    )
         CALL SIGERR ( 'SPICE(INVALIDREFFRAME)'                    )
         CALL CHKOUT ( 'CKW03'                                     )
         RETURN
 
      END IF
 
C
C     Assign values to the integer components of the segment descriptor.
C
      ICD ( 1 ) = INST
      ICD ( 2 ) = REFCOD
      ICD ( 3 ) = DTYPE
 
      IF ( AVFLAG ) THEN
         ICD ( 4 ) = 1
      ELSE
         ICD ( 4 ) = 0
      END IF
 
C
C     Now pack the segment descriptor.
C
      CALL DAFPS ( ND, NI, DCD, ICD, DESCR )
 
C
C     Check that all the characters in the segid can be printed.
C
      DO I = 1, LASTNB(SEGID)
 
         VALUE = ICHAR( SEGID(I:I) )
 
         IF ( ( VALUE .LT. FPRINT ) .OR. ( VALUE .GT. LPRINT ) ) THEN
 
            CALL SETMSG ( 'The segment identifier contains '  //
     .                    'nonprintable characters'               )
            CALL SIGERR ( 'SPICE(NONPRINTABLECHARS)'              )
            CALL CHKOUT ( 'CKW03'                                 )
            RETURN
 
         END IF
 
      END DO
 
C
C     Also check to see if the segment identifier is too long.
C
      IF ( LASTNB(SEGID) .GT. SIDLEN ) THEN
 
         CALL SETMSG ( 'Segment identifier contains more than ' //
     .                 '40 characters.'                          )
         CALL SIGERR ( 'SPICE(SEGIDTOOLONG)'                     )
         CALL CHKOUT ( 'CKW03'                                   )
         RETURN
 
      END IF
 
 
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
         CALL CHKOUT ( 'CKW03'                                 )
         RETURN
 
      END IF
 
C
C     Now check that the times are ordered properly.
C
      DO I = 2, NREC
 
         IF ( SCLKDP(I) .LE. SCLKDP(I-1) )  THEN
 
            CALL SETMSG ( 'The SCLKDP times are not strictly '      //
     .                    'increasing. SCLKDP(#) = # and SCLKDP(#) '//
     .                    '= #.'                                )
            CALL ERRINT ( '#', I                                )
            CALL ERRDP  ( '#', SCLKDP(I)                        )
            CALL ERRINT ( '#', I-1                              )
            CALL ERRDP  ( '#', SCLKDP(I-1)                      )
            CALL SIGERR ( 'SPICE(TIMESOUTOFORDER)'              )
            CALL CHKOUT ( 'CKW03'                               )
            RETURN
 
         END IF
 
      END DO
 
C
C     Now check that the start time of the first interval is the
C     same as the time of the first pointing instance.
C
      IF ( SCLKDP(1) .NE. STARTS(1) ) THEN

         CALL SETMSG ( 'The start time of the first interval # ' //
     .                 'and the time of the first pointing '     //
     .                 'instance # are not the same.'          )
         CALL ERRDP  ( '#', STARTS(1)                          )
         CALL ERRDP  ( '#', SCLKDP(1)                          )
         CALL SIGERR ( 'SPICE(TIMESDONTMATCH)'                 )
         CALL CHKOUT ( 'CKW03'                                 )
         RETURN
 
      END IF


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
            CALL CHKOUT ( 'CKW03'                               )
            RETURN
 
         END IF
 
      END DO
 
 
C
C     Now make sure that all of the interval start times coincide with
C     one of the times associated with the actual pointing.
C
 
      INDEX = 0
 
      DO I = 1, NINTS
 
         MATCH = .FALSE.
 
         DO WHILE ( ( .NOT. MATCH ) .AND. ( INDEX .LT. NREC ) )
 
            INDEX = INDEX + 1
 
            MATCH = ( STARTS(I) .EQ. SCLKDP(INDEX) )
 
         END DO
 
         IF ( .NOT. MATCH ) THEN
 
            CALL SETMSG ( 'Interval start time number # is invalid. '//
     .                    'STARTS(#) = *'                             )
            CALL ERRINT ( '#', I                                      )
            CALL ERRINT ( '#', I                                      )
            CALL ERRDP  ( '*', STARTS(I)                              )
            CALL SIGERR ( 'SPICE(INVALIDSTARTTIME)'                   )
            CALL CHKOUT ( 'CKW03'                                     )
            RETURN
 
         END IF
 
      END DO

C
C     Make sure that the quaternions are non-zero. This is just
C     a check for uninitialized data.
C
      DO I = 1, NREC

         IF (  VZEROG( QUATS(0,I), 4 )  ) THEN
 
            CALL SETMSG ( 'The quaternion at index # has magnitude '
     .      //            'zero.'                                    )
            CALL ERRINT ( '#', I                                     )
            CALL SIGERR ( 'SPICE(ZEROQUATERNION)'                    )
            CALL CHKOUT ( 'CKW03'                                    )
            RETURN
 
         END IF
 
      END DO
 
C
C     No more checks, begin writing the segment.
C
 
      CALL DAFBNA ( HANDLE, DESCR, SEGID )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'CKW03' )
         RETURN
       END IF
 
 
C
C     Now add the quaternions and optionally, the angular velocity
C     vectors.
C
      IF ( AVFLAG )  THEN
 
         DO I = 1, NREC
            CALL DAFADA ( QUATS(0,I), 4 )
            CALL DAFADA ( AVVS (1,I), 3 )
         END DO
 
      ELSE
 
         CALL DAFADA ( QUATS, 4*NREC )
 
      END IF
 
 
C
C     Add the SCLK times.
C
      CALL DAFADA ( SCLKDP, NREC )
 
C
C     The time tag directory.  The Ith element is defined to be the
C     (I*100)th SCLK time.
C
      NRDIR  =  ( NREC - 1 ) / 100
 
      INDEX = 100
 
      DO I = 1, NRDIR
 
         CALL DAFADA ( SCLKDP(INDEX), 1 )
 
         INDEX    =    INDEX + 100
 
      END DO
 
 
 
C
C     Now add the interval start times.
C
      CALL DAFADA ( STARTS, NINTS )
 
C
C     And the directory of interval start times.  The directory of
C     start times will simply be every 100th start time.
C
      NIDIR  =   ( NINTS - 1 )  / 100
 
      INDEX = 100
 
      DO I = 1, NIDIR
 
         CALL DAFADA ( STARTS(INDEX), 1 )
 
         INDEX    =    INDEX + 100
 
      END DO
 
 
C
C     Finally, the number of intervals and records.
C
      CALL DAFADA ( DBLE(NINTS), 1 )
      CALL DAFADA ( DBLE(NREC),  1 )
 
C
C     End the segment.
C
      CALL DAFENA
 
      CALL CHKOUT ( 'CKW03' )
      RETURN
      END
