C$Procedure  CKW02 ( C-Kernel, write segment to C-kernel, data type 2 )
 
      SUBROUTINE CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .                   NREC,   START,  STOP,   QUATS, AVVS, RATES )
 
C$ Abstract
C
C     Write a type 2 segment to a C-kernel.
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
      CHARACTER*(*)         SEGID
      INTEGER               NREC
      DOUBLE PRECISION      START  (      * )
      DOUBLE PRECISION      STOP   (      * )
      DOUBLE PRECISION      QUATS  ( 0:3, * )
      DOUBLE PRECISION      AVVS   (   3, * )
      DOUBLE PRECISION      RATES  (      * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of an open CK file.
C     BEGTIM     I   The beginning encoded SCLK of the segment.
C     ENDTIM     I   The ending encoded SCLK of the segment.
C     INST       I   The NAIF instrument ID code.
C     REF        I   The reference frame of the segment.
C     SEGID      I   Segment identifier.
C     NREC       I   Number of pointing records.
C     START      I   Encoded SCLK interval start times.
C     STOP       I   Encoded SCLK interval stop times.
C     QUATS      I   SPICE quaternions representing instrument pointing.
C     AVVS       I   Angular velocity vectors.
C     RATES      I   Number of seconds per tick for each interval.
C
C$ Detailed_Input
C
C     HANDLE     is the handle of the CK file to which the segment will
C                be written. The file must have been opened with write
C                access.
C
C     BEGTIM     is the beginning encoded SCLK time of the segment. This
C                value should be less than or equal to the first START
C                time in the segment.
C
C     ENDTIM     is the encoded SCLK time at which the segment ends.
C                This value should be greater than or equal to the last
C                STOP time in the segment.
C
C     INST       is the NAIF integer ID code for the instrument.
C
C     REF        is a character string that specifies the
C                reference frame of the segment. This should be one of
C                the frames supported by the SPICELIB routine NAMFRM
C                which is an entry point to FRAMEX.
C
C     SEGID      is the segment identifier.  A CK segment identifier may
C                contain up to 40 characters.
C
C     NREC       is the number of pointing intervals that will be
C                written to the segment.
C
C     START      are the start times of each interval in encoded
C                spacecraft clock. These times must be strictly
C                increasing.
C
C     STOP       are the stop times of each interval in encoded
C                spacecraft clock. These times must be greater than
C                the START times that they correspond to but less
C                than or equal to the START time of the next interval.
C
C     QUATS      is an array of SPICE-style quaternions representing
C                the C-matrices associated with the start times of each
C                interval. See the discussion of quaternion styles in
C                Particulars below.
C
C     AVVS       are the angular velocity vectors for each interval.
C
C     RATES      are the number of seconds per encoded spacecraft clock
C                tick for each interval.
C
C                In most applications this value will be the same for
C                each interval within a segment.  For example, when
C                constructing a predict C-kernel for Mars Observer, the
C                rate would be 1/256 for each interval since this is
C                the smallest time unit expressible by the MO clock. The
C                nominal seconds per tick rates for Galileo and Voyager
C                are 1/120 and 0.06 respectively.
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
C         SPICE(SEGIDTOOLONG) is signalled.
C
C     3)  If SEGID contains any nonprintable characters, the error
C         SPICE(NONPRINTABLECHARS) is signalled.
C
C     4)  If the first START time is negative, the error
C         SPICE(INVALIDSCLKTIME) is signalled. If any of the subsequent
C         START times are negative the error SPICE(TIMESOUTOFORDER)
C         will be signalled.
C
C     5)  If any of the STOP times are negative, the error
C         SPICE(DEGENERATEINTERVAL) is signalled.
C
C     6)  If the STOP time of any of the intervals is less than or equal
C         to the START time, the error SPICE(DEGENERATEINTERVAL) is
C         signalled.
C
C     7)  If the START times are not strictly increasing, the
C         error SPICE(TIMESOUTOFORDER) is signalled.
C
C     8)  If the STOP time of one interval is greater than the START
C         time of the next interval, the error SPICE(BADSTOPTIME)
C         is signalled.
C
C     9)  If BEGTIM is greater than START(1) or ENDTIM is less than
C         STOP(NREC), the error SPICE(INVALIDDESCRTIME) is
C         signalled.
C
C    10)  If the name of the reference frame is not one of those
C         supported by the routine NAMFRM, the error
C         SPICE(INVALIDREFFRAME) is signalled.
C
C    11)  If NREC, the number of pointing records, is less than or
C         equal to 0, the error SPICE(INVALIDNUMRECS) is signalled.
C
C    12)  If the squared length of any quaternion differes from 1
C         by more than 1.0D-2, the error SPICE(NONUNITQUATERNION) is
C         signalled.
C
C$ Files
C
C     This routine adds a type 2 segment to a C-kernel.  The C-kernel
C     may be either a new one or an existing one opened for writing.
C
C$ Particulars
C
C     For a detailed description of a type 2 CK segment please see the
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
C  C     This example writes a predict type 2 C-kernel segment for
C  C     the Mars Observer spacecraft bus to a previously opened CK file
C  C     attached to HANDLE.
C
C  C
C  C     Assume arrays of quaternions, angular velocities, and interval
C  C     start and stop times are produced elsewhere.
C  C
C        .
C        .
C        .
C
C  C
C  C     The nominal number of seconds in a tick for MO is 1/256
C  C
C        SECTIK = 1.D0 / 256.D0
C
C        DO I = 1, NREC
C           RATE(I) = SECTIK
C        END DO
C
C  C
C  C     The subroutine CKW02 needs the following components of the
C  C     segment descriptor:
C  C
C  C        1) SCLK limits of the segment.
C  C        2) Instrument code.
C  C        3) Reference frame.
C
C        BEGTIM = START (    1 )
C        ENDTIM = STOP  ( NREC )
C
C        INST  = -94000
C        REF   = 'J2000'
C
C        SEGID = 'MO PREDICT SEG TYPE 2'
C
C  C
C  C     Write the segment.
C  C
C        CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST, REF, SEGID,
C       .             NREC,   START,  STOP,   QUAT, AVV, RATES  )
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
C     K.R. Gehringer  (JPL)
C     J.M. Lynch      (JPL)
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.0.0, 01-JUN-2010 (NJB)
C
C        The check for non-unit quaternions has been replaced
C        with a check for zero-length quaternions.
C
C-    SPICELIB Version 2.2.0, 26-FEB-2008 (NJB)
C
C        Updated header; added information about SPICE 
C        quaternion conventions.
C
C        Minor typo in a long error message was corrected.
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
C-    SPICELIB Version 1.1.0, 25-NOV-1992 (JML)
C
C        1) If the number of pointing records is not positive an error
C           is now signalled.
C
C        2) FAILED is checked after the call to DAFBNA.
C
C        3) The variables HLDBEG and HLDEND were removed from the loop
C           where the interval start and stop times are tested.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 30-AUG-1991 (JML)
C
C-&
 
C$ Index_Entries
C
C     write ck type_2 pointing data segment
C
C-&
 
C$ Revisions
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
C-    SPICELIB Version 1.1.0, 25-NOV-1992 (JML)
C
C        1) If the number of pointing records is not positive an error
C           is now signalled.
C
C        2) FAILED is checked after the call to DAFBNA.
C
C        3) The variables HLDBEG and HLDEND were removed from the loop
C           where the interval start and stop times are tested.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 30-AUG-1991 (JML)
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
      PARAMETER           ( DTYPE   =   2 )
 
      INTEGER               FPRINT
      PARAMETER           ( FPRINT  =  32 )
 
      INTEGER               LPRINT
      PARAMETER           ( LPRINT  = 126 )
 
 
C
C     Local variables
C
      DOUBLE PRECISION      DCD    ( ND    )
      DOUBLE PRECISION      DIRENT
      DOUBLE PRECISION      DESCR  ( NDC   ) 
 
      INTEGER               I
      INTEGER               ICD    ( NI    )
      INTEGER               INDEX
      INTEGER               NDIR
      INTEGER               REFCOD
      INTEGER               VALUE
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKW02' )
      END IF
 
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
C           ICD( 5 )              -- Beginning address of the segment.
C           ICD( 6 )              -- Ending address of the segment.
C
 
C
C     Make sure that there is a positive number of pointing records.
C
      IF ( NREC .LE. 0 ) THEN
 
         CALL SETMSG ( '# is an invalid number of pointing ' //
     .                 'instances for type 2.'                     )
         CALL ERRINT ( '#', NREC                                   )
         CALL SIGERR ( 'SPICE(INVALIDNUMREC)'                      )
         CALL CHKOUT ( 'CKW02'                                     )
         RETURN
 
      END IF
 
 
C
C     Check that the SCLK bounds on the segment are reasonable.
C
      IF ( BEGTIM .GT. START(1) ) THEN
 
         CALL SETMSG ( 'The first d.p. component of the descriptor ' //
     .                 'is invalid.  DCD(1) = # and START(1) = # '    )
         CALL ERRDP  ( '#', BEGTIM               )
         CALL ERRDP  ( '#', START(1)             )
         CALL SIGERR ( 'SPICE(INVALIDDESCRTIME)' )
         CALL CHKOUT ( 'CKW02' )
         RETURN
 
      END IF
 
      IF ( ENDTIM .LT. STOP(NREC) ) THEN
 
         CALL SETMSG ( 'The second d.p. component of the descriptor ' //
     .                 'is invalid.  DCD(2) = # and STOP(NREC) = # '   )
         CALL ERRDP  ( '#', ENDTIM               )
         CALL ERRDP  ( '#', STOP(NREC)           )
         CALL SIGERR ( 'SPICE(INVALIDDESCRTIME)' )
         CALL CHKOUT ( 'CKW02' )
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
         CALL CHKOUT ( 'CKW02'                                     )
         RETURN
 
      END IF
 
C
C     Assign values to the integer components of the segment descriptor.
C     By definition data type two must have angular velocity.
C
      ICD ( 1 ) = INST
      ICD ( 2 ) = REFCOD
      ICD ( 3 ) = DTYPE
      ICD ( 4 ) = 1
 
 
C
C     Now pack the segment descriptor.
C
      CALL DAFPS ( ND, NI, DCD, ICD, DESCR )
 
C
C     Now check that all the characters in the segid can be printed.
C
      DO I = 1, LASTNB(SEGID)
 
         VALUE = ICHAR( SEGID(I:I) )
 
         IF ( ( VALUE .LT. FPRINT ) .OR. ( VALUE .GT. LPRINT ) ) THEN
 
            CALL SETMSG ( 'The segment identifier contains '  //
     .                    'nonprintable characters'               )
            CALL SIGERR ( 'SPICE(NONPRINTABLECHARS)'              )
            CALL CHKOUT ( 'CKW02'                                 )
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
         CALL CHKOUT ( 'CKW02'                                   )
         RETURN
 
      END IF
 
C
C     Now check that the START and STOP times on the intervals
C     make sense. Three checks will be performed on each interval:
C
C        1)  Check that the STOP time is greater than the START time.
C
C        2)  Check that the START times are strictly increasing.
C
C        3)  Check that the START time is greater than or equal to the
C            STOP time from the previous interval.
C
C     For the first interval also make sure that the START time is
C     nonnegative.
C
      IF ( START(1) .LT. 0.D0 ) THEN
 
         CALL SETMSG ( 'The first START time: # is negative.'  )
         CALL ERRDP  ( '#', START(1)                           )
         CALL SIGERR ( 'SPICE(INVALIDSCLKTIME)'                )
         CALL CHKOUT ( 'CKW02'                                 )
         RETURN
 
      END IF
 
      IF ( STOP(1) .LE. START(1) ) THEN
 
         CALL SETMSG ( 'The STOP time is less than or equal to the '  //
     .                 'START time for interval number 1. START time '//
     .                 'is # and STOP time is #.'                      )
         CALL ERRDP  ( '#', START(1)               )
         CALL ERRDP  ( '#', STOP (1)               )
         CALL SIGERR ( 'SPICE(DEGENERATEINTERVAL)' )
         CALL CHKOUT ( 'CKW02'                     )
         RETURN
 
      END IF
 
 
      DO I = 2, NREC
 
         IF ( STOP(I) .LE. START(I) ) THEN
 
            CALL SETMSG ( 'The STOP time is less than or equal to '  //
     .                    'the START time for interval number #. '   //
     .                    'START time is # and STOP time is #.'       )
            CALL ERRINT ( '#', I                                      )
            CALL ERRDP  ( '#', START(I)                               )
            CALL ERRDP  ( '#', STOP (I)                               )
            CALL SIGERR ( 'SPICE(DEGENERATEINTERVAL)'                 )
            CALL CHKOUT ( 'CKW02'                                     )
            RETURN
 
         END IF
 
         IF ( START(I) .LE. START(I-1) )  THEN
 
            CALL SETMSG ( 'The START times are not strictly '      //
     .                    'increasing.  START(#) = # and START(#) '//
     .                    '= #.'                                )
            CALL ERRINT ( '#', I                                )
            CALL ERRDP  ( '#', START(I)                         )
            CALL ERRINT ( '#', I-1                              )
            CALL ERRDP  ( '#', START(I-1)                       )
            CALL SIGERR ( 'SPICE(TIMESOUTOFORDER)'              )
            CALL CHKOUT ( 'CKW02'                               )
            RETURN
 
         END IF
 
         IF ( STOP(I-1) .GT. START(I) )  THEN
 
            CALL SETMSG ( 'The STOP time for interval # is greater ' //
     .                    'than the following START time. STOP(#) = '//
     .                    '# and START(#) = #.'                       )
            CALL ERRINT ( '#', I-1                                    )
            CALL ERRINT ( '#', I-1                                    )
            CALL ERRDP  ( '#', STOP(I-1)                              )
            CALL ERRINT ( '#', I                                      )
            CALL ERRDP  ( '#', START(I)                               )
            CALL SIGERR ( 'SPICE(BADSTOPTIME)'                        )
            CALL CHKOUT ( 'CKW02'                                     )
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
            CALL CHKOUT ( 'CKW02'                                    )
            RETURN
 
         END IF
 
      END DO 
 
C
C     No more checks, begin writing the segment.
C
 
      CALL DAFBNA ( HANDLE, DESCR, SEGID )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'CKW02' )
         RETURN
      END IF
 
 
C
C     Now add the quaternions, angular velocity vectors, and time
C     conversion factors for each interval.
C
      DO I = 1, NREC
         CALL DAFADA ( QUATS(0,I), 4 )
         CALL DAFADA ( AVVS (1,I), 3 )
         CALL DAFADA ( RATES(I),   1 )
      END DO
 
C
C     The SCLK start times.
C
      CALL DAFADA ( START, NREC )
C
C     The SCLK stop times.
C
      CALL DAFADA ( STOP, NREC )
 
C
C     The time tag directory.  The Ith element is defined to be the
C     average of the (I*100)th STOP time and the (I*100+1)th START time.
C
      NDIR  =  ( NREC - 1 ) / 100
 
      INDEX = 100
 
      DO I = 1, NDIR
         DIRENT   =  ( STOP(INDEX) + START(INDEX+1) ) / 2.D0
         CALL DAFADA ( DIRENT, 1 )
         INDEX    =    INDEX + 100
      END DO
 
C
C     End the segment.
C
      CALL DAFENA
 
      CALL CHKOUT ( 'CKW02' )
      RETURN
      END
