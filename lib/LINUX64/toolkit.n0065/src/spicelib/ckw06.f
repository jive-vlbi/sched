C$Procedure      CKW06 ( CK, Write segment, type 6 )
 
      SUBROUTINE CKW06 ( HANDLE,  INST,    REF,     AVFLAG,
     .                   FIRST,   LAST,    SEGID,   NMINI,
     .                   NPKTS,   SUBTPS,  DEGRES,  PACKTS, 
     .                   RATES,   SCLKDP,  IVLBDS,  SELLST )

C$ Abstract
C
C     Write a type 6 segment to a CK file.
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
C     NAIF_IDS
C     SCLK
C     SPC
C     TIME
C
C$ Keywords
C
C     ATTITUDE
C     FILES
C     POINTING
C
C$ Declarations
 
      IMPLICIT NONE 

      INCLUDE 'ck06.inc'
      INCLUDE 'ckparam.inc'

      
      INTEGER               HANDLE
      INTEGER               INST
      CHARACTER*(*)         REF
      LOGICAL               AVFLAG
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      CHARACTER*(*)         SEGID
      INTEGER               NMINI
      INTEGER               NPKTS  ( * )
      INTEGER               SUBTPS ( * )
      INTEGER               DEGRES ( * )
      DOUBLE PRECISION      PACKTS ( * )
      DOUBLE PRECISION      RATES  ( * )
      DOUBLE PRECISION      SCLKDP ( * )
      DOUBLE PRECISION      IVLBDS ( * )
      LOGICAL               SELLST
      
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of a CK file open for writing.
C     INST       I   NAIF instrument ID code.
C     REF        I   Reference frame name.
C     AVFLAG     I   True if the segment will contain angular velocity.
C     FIRST      I   Start time of interval covered by segment.
C     LAST       I   End time of interval covered by segment.
C     SEGID      I   Segment identifier.
C     NMINI      I   Number of mini-segments.
C     NPKTS      I   Array of packet counts of mini-segments.
C     SUBTPS     I   Array of segment subtypes of mini-segments.
C     DEGRES     I   Array of polynomial degrees of mini-segments.
C     PACKTS     I   Array of data packets of mini-segments.
C     RATES      I   Nominal SCLK rates in seconds per tick.
C     SCLKDP     I   Array of epochs of mini-segments.
C     IVLBDS     I   Mini-segment interval bounds.
C     SELLST     I   Interval selection flag.
C     MAXDEG     P   Maximum allowed degree of interpolating polynomial.
C
C$ Detailed_Input
C
C     HANDLE         is the handle of a CK file that has been opened
C                    for writing.
C
C
C     INST           is a NAIF integer code associated with an
C                    instrument or spacecraft structure whose
C                    orientation is described by the segment to be
C                    created. INST is treated by the SPICE frame
C                    subsystem as a CK frame class ID (see the
C                    Frames Required Reading for details).
C
C
C     AVFLAG         is a logical flag which indicates whether or not
C                    the segment will contain angular velocity.
C
C
C     REF            is the NAIF name for a reference frame relative to
C                    which the pointing (attitude) information for INST
C                    is specified.
C
C     FIRST,
C     LAST           are, respectively, the bounds of the time interval
C                    over which the segment defines the attitude of
C                    INST. FIRST and LAST are encoded SCLK times.
C
C                    FIRST must be greater than or equal to the first
C                    mini-segment interval start time; LAST must be
C                    less than or equal to the last mini-segment 
C                    interval stop time. See the description of IVLBDS
C                    below.
C
C
C     SEGID          is the segment identifier. A CK segment
C                    identifier may contain up to 40 characters.
C
C
C     NMINI          is the number of mini-segments comprised by 
C                    the input data. Each mini-segment contains data
C                    that could be stored in a type 5 segment. 
C                    The parameters and data of a mini-segment are:
C            
C                       - a packet count
C                       - a type 6 subtype
C                       - an interpolating polynomial degree
C                       - a nominal SCLK rate in seconds/tick
C                       - a sequence of type 6 data packets
C                       - a sequence of packet epochs
C                    
C                    These inputs are described below.
C
C
C     NPKTS          is an array of packet counts. The Ith element of
C                    NPKTS is the packet count of the Ith mini-segment.
C                   
C                    NPKTS has dimension NMINI.
C
C
C     SUBTPS         is an array of type 6 subtypes. The Ith element of
C                    SUBTPS is the subtype of the packets associated
C                    with the Ith mini-segment.
C
C                    SUBTPS has dimension NMINI.
C
C
C     DEGRES         is an array of interpolating polynomial degrees.
C                    The Ith element of DEGRES is the polynomial degree
C                    of the packets associated with the Ith
C                    mini-segment.
C
C                    For subtypes 0 and 2, interpolation degrees must be
C                    equivalent to 3 mod 4, that is, they must be in
C                    the set
C
C                       { 3, 7, 11, ..., MAXDEG }
C
C                    For subtypes 1 and 3, interpolation degrees must
C                    be odd and must be in the range 1:MAXDEG.
C
C                    DEGRES has dimension NMINI.
C
C
C     PACKTS         is an array of data packets representing the
C                    orientation of INST relative to the frame REF. The
C                    packets for a given mini-segment are stored
C                    contiguously in increasing time order. The order
C                    of the sets of packets for different mini-segments
C                    is the same as the order of their corresponding
C                    mini-segment intervals.
C
C                    Each packet contains a SPICE-style quaternion and
C                    optionally, depending on the segment subtype,
C                    attitude derivative data, from which a C-matrix
C                    and an angular velocity vector may be derived.
C
C                    See the discussion of quaternion styles in
C                    Particulars below.
C
C                    The C-matrix CMAT represented by the Ith data
C                    packet is a rotation matrix that transforms the
C                    components of a vector expressed in the base frame
C                    specified by REF to components expressed in the
C                    instrument fixed frame at the time SCLKDP(I).
C
C                    Thus, if a vector V has components x, y, z in the
C                    base frame, then V has components x', y', z'
C                    in the instrument fixed frame where:
C
C                       [ x' ]     [          ] [ x ]
C                       | y' |  =  |   CMAT   | | y |
C                       [ z' ]     [          ] [ z ]
C                    
C                    Attitude derivative information either explicitly
C                    contained in, or else derived from, PACKTS(I)
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
C                    Units of angular velocity and of quaternion
C                    derivatives are radians/second and 1/second
C                    respectively.
C
C                    For the Hermite subtypes (0 and 2), quaternion
C                    representations must be selected so that, for
C                    consecutive quaternions Q(I) and Q(I+1) in a
C                    mini-segment, the distance between Q and Q(I+1) is
C                    less than the distance between Q and -Q(I+1). The
C                    Lagrange subtypes do not have this restriction.
C                    
C
C     RATES          is an array of nominal rates of the spacecraft
C                    clock associated with INST. The Ith element of
C                    rates is the clock rate for the packets associated
C                    with the Ith mini-segment. Units are seconds per
C                    tick. Spacecraft clock rates are used to scale
C                    angular velocity to radians/second.
C
C
C     SCLKDP         is an array containing epochs for all input
C                    mini-segments. The epochs have a one-to-one
C                    relationship with the packets in the input 
C                    packet array. The epochs are encoded SCLK times.
C
C                    The epochs for a given mini-segment are stored
C                    contiguously in increasing order. The order of the
C                    sets of epochs for different mini-segments is the
C                    same as the order of their corresponding
C                    mini-segment intervals.
C
C                    For each mini-segment, "padding" is allowed: the
C                    sequence of epochs for that mini-segment may start
C                    before the corresponding mini-segment interval
C                    start time and end after the corresponding
C                    mini-segment interval stop time. Padding is used
C                    to control behavior of interpolating polynomials
C                    near mini-segment interval boundaries.
C
C                    Due to possible use of padding, the elements of
C                    SCLKDP, taken as a whole, might not be in
C                    increasing order.
C
C
C     IVLBDS         is an array of mini-segment interval boundary
C                    times. This array is a strictly increasing list of
C                    the mini-segment interval start times, to which
C                    the end time for the last interval is appended.
C                    The interval bounds are encoded SCLK times.
C
C                    The Ith mini-segment interval is the time
C                    coverage interval of the Ith mini-segment (see the
C                    description of NPKTS above).
C
C                    For each mini-segment, the corresponding
C                    mini-segment interval's start time is greater
C                    than or equal to the mini-segment's first epoch.
C                    The interval's stop time may exceed the
C                    mini-segment's last epoch, allowing a single
C                    coverage gap to exist between a mini-segment's
C                    last epoch and its interval stop time.
C
C                    The "interpolation interval" of the ith
C                    mini-segment is contained in the ith mini-segment
C                    interval: the interpolation interval extends from
C                    IVLBDS(I) to the minimum of IVLBDS(I+1) and the
C                    last epoch of the mini-segment.
C
C                    For each mini-segment interval other than the
C                    last, the interval's coverage stop time coincides
C                    with the coverage start time of the next interval.
C                 
C                    IVLBDS has dimension NMINI+1.
C
C
C     SELLST         is a logical flag indicating to the CK type 6
C                    segment reader CKR06 how to select the
C                    mini-segment interval when a request time
C                    coincides with a time boundary shared by two
C                    mini-segment intervals. When SELLST ("select
C                    last") is .TRUE., the later interval is selected;
C                    otherwise the earlier interval is selected.
C   
C$ Detailed_Output
C
C     None.  See $Particulars for a description of the effect of this
C     routine.
C
C$ Parameters
C
C     MAXDEG         is the maximum allowed degree of the interpolating
C                    polynomial. 
C
C                    See the INCLUDE file ck06.inc for the value of
C                    MAXDEG.
C
C$ Exceptions
C
C     If any of the following exceptions occur, this routine will return
C     without creating a new segment.
C
C
C     1)  If FIRST is greater than LAST then the error
C         SPICE(BADDESCRTIMES) will be signaled.
C
C     2)  If REF is not a recognized name, the error
C         SPICE(INVALIDREFFRAME) is signaled.
C
C     3)  If the last non-blank character of SEGID occurs past index
C         40, the error SPICE(SEGIDTOOLONG) is signaled.
C
C     4)  If SEGID contains any nonprintable characters, the error
C         SPICE(NONPRINTABLECHARS) is signaled.
C
C     5)  If NMINI is not at least 1, the error SPICE(INVALIDCOUNT)
C         is signaled.
C         
C     6)  If the elements of the array IVLBDS are not in strictly
C         increasing order, the error SPICE(BOUNDSOUTOFORDER) will be
C         signaled.
C
C     7)  If the first interval start time IVLBDS(1) is greater than
C         FIRST, or if the last interval end time IVLBDS(N+1) is less
C         than LAST, the error SPICE(COVERAGEGAP) will be signaled.
C
C     8)  If any packet count in the array NPKTS is not at least 2, the
C         error SPICE(TOOFEWPACKETS) will be signaled.
C
C     9)  If any subtype code in the array SUBTPS is not recognized,
C         the error SPICE(INVALIDSUBTYPE) will be signaled.
C
C    10)  If any interpolation degree in the array DEGRES is not at
C         least 1 or is greater than MAXDEG, the error
C         SPICE(INVALIDDEGREE) is signaled.
C
C    11)  If the window size implied by any element of the array DEGRES
C         is odd, the error SPICE(BADWINDOWSIZE) is signaled.
C
C    12)  If the elements of the array SCLKDP corresponding
C         to a given mini-segment are not in strictly
C         increasing order, the error SPICE(TIMESOUTOFORDER) will be
C         signaled. 
C
C    13)  If the first epoch of a mini-segment exceeds the start time
C         of the associated mini-segment interval, or if the last
C         epoch of a mini-segment is less than the interval start
C         time, the error SPICE(BOUNDSDISAGREE) is signaled. However,
C         the last epoch of a mini-segment may be less than the end
C         time of the corresponding mini-segment interval.
C  
C    14)  If any quaternion has magnitude zero, the error
C         SPICE(ZEROQUATERNION) is signaled.
C
C    15)  Any error that occurs while writing the output segment will
C         be diagnosed by routines in the call tree of this routine.
C
C    16)  This routine assumes that the rotation between adjacent
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
C         yield invalid results when interpolated. The segment creator
C         must ensure that the data stored in the segment will not be
C         subject to this sort of ambiguity.
C
C    17)  For the Hermite subtypes (0 and 2), quaternion
C         representations must be selected so that, for consecutive
C         quaternions Q(I) and Q(I+1) in a mini-segment, the distance
C         between Q and Q(I+1) is less than the distance between Q and
C         -Q(I+1).
C
C         If a pair of quaternions violating this condition is found
C         in the input array PACKTS, the error SPICE(BADQUATSIGN) will
C         be signaled.
C
C$ Files
C
C     A new type 6 CK segment is written to the CK file attached
C     to HANDLE.
C
C$ Particulars
C
C     This routine writes a CK type 6 data segment to the open CK
C     file according to the format described in the type 6 section of
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
C     Suppose that you have states and are prepared to produce
C     a segment of type 6 in a CK file.
C
C     The following code fragment could be used to add the new segment
C     to a previously opened CK file attached to HANDLE. The file must
C     have been opened with write access.
C
C        C
C        C     Create a segment identifier.
C        C
C              SEGID = 'MY_SAMPLE_CK_TYPE_6_SEGMENT'
C
C        C
C        C     Write the segment.
C        C
C              CALL CKW06 ( HANDLE,  INST,    REF,     AVFLAG,
C             .             FIRST,   LAST,    SEGID,   NMINI,  
C             .             NPKTS,   SUBTPS,  DEGRES,  PACKTS,
C             .             RATES,   SCLKDP,  IVLBDS,  SELLST  )
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
C     write spk type_6 ephemeris data segment
C
C-&
 
 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      VDISTG
      INTEGER               LASTNB
      
      LOGICAL               EVEN
      LOGICAL               FAILED
      LOGICAL               ODD
      LOGICAL               RETURN
      LOGICAL               VZEROG
 
C
C     Local parameters
C
      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN =  40 )
 
      INTEGER               FPRINT
      PARAMETER           ( FPRINT =  32 )
 
      INTEGER               LPRINT
      PARAMETER           ( LPRINT = 126 )

      INTEGER               ND
      PARAMETER           ( ND     =   2 )
 
      INTEGER               NI
      PARAMETER           ( NI     =   6 )
      
      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ =   5 )
           
      INTEGER               DTYPE
      PARAMETER           ( DTYPE  =  6 )

      INTEGER               DIRSIZ
      PARAMETER           ( DIRSIZ = 100 )

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
      DOUBLE PRECISION      PREVQ  ( 0 : 3 )
      DOUBLE PRECISION      Q      ( 0 : 3 )
      DOUBLE PRECISION      QNEG   ( 0 : 3 )
 
      INTEGER               ADDR
      INTEGER               BEPIX
      INTEGER               CHRCOD
      INTEGER               EEPIX
      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               ISEL
      INTEGER               J
      INTEGER               K
      INTEGER               MINISZ
      INTEGER               NDIR
      INTEGER               PKTBEG
      INTEGER               PKTDSZ
      INTEGER               PKTEND
      INTEGER               PKTSIZ
      INTEGER               PKTSZS ( 0 : C06NST-1 )
      INTEGER               REFCOD
      INTEGER               SEGBEG
      INTEGER               SEGEND
      INTEGER               SUBTYP
      INTEGER               WINSIZ

C
C     Saved values
C
      SAVE                  PKTSZS

C
C     Initial values
C
      DATA                  PKTSZS  /  C06PS0, C06PS1, C06PS2, C06PS3 /

     
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'CKW06' )
          
C
C     Start with a parameter compatibility check.
C
      IF ( CKMRSZ .LT. MAXRSZ ) THEN

         CALL SETMSG ( 'CK type 6 record size is #, but '
     .   //            'CKPFS record size is #.'          
     .   //            'is #.'                            )
         CALL ERRINT ( '#',  MAXRSZ                       )
         CALL ERRINT ( '#',  CKMRSZ                       )
         CALL SIGERR ( 'SPICE(BUG)'                       )
         CALL CHKOUT ( 'CKW06'                            )
         RETURN

      END IF

C
C     Make sure the segment descriptor bounds are 
C     correctly ordered.
C
      IF ( LAST .LT. FIRST ) THEN

         CALL SETMSG ( 'Segment start time is #; stop time is #; '
     .   //            'bounds must be in nondecreasing order.'    )
         CALL ERRDP  ( '#', FIRST                                  )   
         CALL ERRDP  ( '#', LAST                                   )   
         CALL SIGERR ( 'SPICE(BADDESCRTIMES)'                      )
         CALL CHKOUT ( 'CKW06'                                     )
         RETURN

      END IF

C
C     Get the NAIF integer code for the reference frame.
C
      CALL NAMFRM ( REF, REFCOD )
 
      IF ( REFCOD .EQ. 0 ) THEN
 
         CALL SETMSG ( 'The reference frame # is not supported.' )
         CALL ERRCH  ( '#', REF                                  )
         CALL SIGERR ( 'SPICE(INVALIDREFFRAME)'                  )
         CALL CHKOUT ( 'CKW06'                                   )
         RETURN
 
      END IF
 
C
C     Check to see if the segment identifier is too long.
C
      IF ( LASTNB(SEGID) .GT. SIDLEN ) THEN
 
         CALL SETMSG ( 'Segment identifier contains more than ' 
     .   //            '40 characters.'                        )
         CALL SIGERR ( 'SPICE(SEGIDTOOLONG)'                   )
         CALL CHKOUT ( 'CKW06'                                 )
         RETURN
 
      END IF
 
C
C     Now check that all the characters in the segment identifier
C     can be printed.
C
      DO I = 1, LASTNB(SEGID)
 
         CHRCOD = ICHAR( SEGID(I:I) )
 
         IF ( ( CHRCOD .LT. FPRINT ) .OR. ( CHRCOD .GT. LPRINT ) ) THEN
 
            CALL SETMSG ( 'The segment identifier contains '  
     .      //            'nonprintable characters'         )
            CALL SIGERR ( 'SPICE(NONPRINTABLECHARS)'        )
            CALL CHKOUT ( 'CKW06'                           )
            RETURN
 
         END IF
 
      END DO

C
C     The mini-segment count must be positive.
C
      IF ( NMINI .LT. 1 ) THEN
 
         CALL SETMSG ( 'Mini-segment count was #; ' 
     .   //            'this count must be positive.' )
         CALL ERRINT ( '#', NMINI                     )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'          )
         CALL CHKOUT ( 'CKW06'                        )
         RETURN
 
      END IF

C
C     Make sure the interval bounds form a strictly 
C     increasing sequence. 
C
C     Note that there are NMINI+1 bounds.
C
      DO I = 1, NMINI

         IF ( IVLBDS(I) .GE. IVLBDS(I+1) ) THEN

            CALL SETMSG ( 'Mini-segment interval '
     .      //            'bounds at indices # and # ' 
     .      //            'are # and # respectively. The '
     .      //            'difference is #. The bounds are '
     .      //            'required to be strictly increasing.' )
            CALL ERRINT ( '#', I                                )
            CALL ERRINT ( '#', I+1                              )
            CALL ERRDP  ( '#', IVLBDS(I)                        )
            CALL ERRDP  ( '#', IVLBDS(I+1)                      )
            CALL ERRDP  ( '#', IVLBDS(I+1) - IVLBDS(I)          )
            CALL SIGERR ( 'SPICE(BOUNDSOUTOFORDER)'             )
            CALL CHKOUT ( 'CKW06'                               )
            RETURN

         END IF

      END DO

C
C     Make sure the time span of the descriptor doesn't extend
C     beyond the span of the interval bounds. 
C     
      IF (      ( FIRST .LT. IVLBDS(1        ) )
     .     .OR. ( LAST  .GT. IVLBDS(NMINI+1) ) ) THEN

         CALL SETMSG ( 'First mini-segment interval '
     .   //            'start time is #; segment start time '
     .   //            'is #; segment stop time is #; '
     .   //            'last mini-segment interval stop '
     .   //            'time is #. This sequence of times '
     .   //            'is required to be non-decreasing: ' 
     .   //            'segment coverage must be contained '
     .   //            'within the union of the mini-segment '
     .   //            'intervals.'                            )
         CALL ERRDP  ( '#', IVLBDS(1)                          )
         CALL ERRDP  ( '#', FIRST                              )
         CALL ERRDP  ( '#', LAST                               )
         CALL ERRDP  ( '#', IVLBDS(NMINI+1)                    )
         CALL SIGERR ( 'SPICE(COVERAGEGAP)'                    )
         CALL CHKOUT ( 'CKW06'                                 )
         RETURN        

      END IF

C
C     Check the input data before writing to the file. 
C
C     This order of operations entails some redundant 
C     calculations, but it allows for rapid error 
C     detection.
C     
C     Initialize the mini-segment packet array indices,
C     and those of the mini-segment epoch array as well.
C
      PKTBEG = 0
      PKTEND = 0

      BEPIX  = 0
      EEPIX  = 0

      DO I = 1, NMINI
C
C        First, just make sure the packet count for the current
C        mini-segment is at least two. This check reduces our chances
C        of a subscript range violation.
C
C        Check the number of packets.
C
         IF ( NPKTS(I) .LT. 2 ) THEN
 
            CALL SETMSG ( 'At least 2 packets are required '
     .      //            'for CK type 6. Number of packets '
     .      //            'supplied was # in mini-segment at index #.' )
            CALL ERRINT ( '#', NPKTS(I)                                )
            CALL ERRINT ( '#', I                                       )
            CALL SIGERR ( 'SPICE(TOOFEWPACKETS)'                       )
            CALL CHKOUT ( 'CKW06'                                      )
            RETURN
 
         END IF

C
C        Set the packet size, which is a function of the subtype. Also
C        set the window size. First check the subtype, which will be
C        used as an array index.
C
         SUBTYP = SUBTPS(I)

         IF (  ( SUBTYP .LT. 0 ) .OR. ( SUBTYP .GT. C06NST-1 ) ) THEN
            
            CALL SETMSG ( 'Unexpected CK type 6 subtype # ' 
     .      //            'found in mini-segment #.'       )
            CALL ERRINT ( '#',  SUBTYP                     )
            CALL ERRINT ( '#',  I                          )
            CALL SIGERR ( 'SPICE(INVALIDSUBTYPE)'          )
            CALL CHKOUT ( 'CKW06'                          )
            RETURN

         END IF

         PKTSIZ = PKTSZS ( SUBTYP )

         IF ( ODD(SUBTYP) ) THEN

            WINSIZ =   DEGRES(I) + 1
         ELSE
            WINSIZ = ( DEGRES(I) + 1 ) / 2
         END IF
 
C
C        Update the packet range pointers for this mini-segment.
C
         PKTBEG = PKTEND + 1
         PKTEND = PKTBEG - 1  +  NPKTS(I)*PKTSIZ

C
C        Make sure that the degree of the interpolating polynomials is
C        in range.
C
         IF (      ( DEGRES(I) .LT. 1      ) 
     .        .OR. ( DEGRES(I) .GT. MAXDEG )  )  THEN
 
            CALL SETMSG ( 'The interpolating polynomials of '
     .      //            'mini-segment # have degree #; the '
     .      //            'valid degree range is [1, #]'      )
            CALL ERRINT ( '#', I                              )
            CALL ERRINT ( '#', DEGRES(I)                      )
            CALL ERRINT ( '#', MAXDEG                         )
            CALL SIGERR ( 'SPICE(INVALIDDEGREE)'              )
            CALL CHKOUT ( 'CKW06'                             )
            RETURN
 
         END IF
 
C
C        Make sure that the window size is even. 
C
         IF (  ODD( WINSIZ )  )  THEN
 
            CALL SETMSG ( 'The interpolating polynomials of '
     .      //            'mini-segment # have window size # '
     .      //            'and degree # for CK type 6. '
     .      //            'The mini-segment subtype is #. '
     .      //            'The degree must be equivalent to 3 '       
     .      //            'mod 4 for subtypes 0 or 2 (Hermite '
     .      //            'interpolation) and odd for subtypes '
     .      //            '1 or 3 (Lagrange interpolation).'     )
            CALL ERRINT ( '#', I                                 )
            CALL ERRINT ( '#', WINSIZ                            )
            CALL ERRINT ( '#', DEGRES(I)                         )
            CALL ERRINT ( '#', SUBTPS(I)                         )
            CALL SIGERR ( 'SPICE(BADWINDOWSIZE)'                 )
            CALL CHKOUT ( 'CKW06'                                )
            RETURN
 
         END IF

C
C        Make sure the epochs of the Ith mini-segment form a 
C        strictly increasing sequence.
C
C        To start out, determine the indices of the epoch sequence
C        of the Ith mini-segment. We'll call the begin and end
C        epoch indices BEPIX and EEPIX respectively.
C
         BEPIX  =  EEPIX + 1
         EEPIX  =  BEPIX - 1 +  NPKTS(I)
  

         DO J = 1, NPKTS(I) - 1 
             
            K = BEPIX + J - 1

            IF ( SCLKDP(K) .GE. SCLKDP(K+1) ) THEN
 
               CALL SETMSG ( 'In mini-segment #, epoch # having '
     .         //            'mini-segment-relative index # and '
     .         //            'array-relative index # is greater '
     .         //            'than or equal to its successor #.' )
               CALL ERRINT ( '#',  I                             )
               CALL ERRDP  ( '#',  SCLKDP(K  )                   )
               CALL ERRINT ( '#',  J                             )
               CALL ERRINT ( '#',  K                             )
               CALL ERRDP  ( '#',  SCLKDP(K+1)                   )
               CALL SIGERR ( 'SPICE(TIMESOUTOFORDER)'            )
               CALL CHKOUT ( 'CKW06'                             )
               RETURN

            END IF
 
         END DO

C
C        Make sure that the span of the input epochs of the Ith
C        mini-segment includes the start of the Ith mini-segment
C        interval. Note that the stop time need not be covered, since
C        gaps are allowed at the right ends of mini-segment intervals.
C
         IF (  SCLKDP(BEPIX) .GT. IVLBDS(I) ) THEN
 
            CALL SETMSG ( 'Mini-segment interval # start time # '
     .      //            'precedes mini-segment''s first epoch #.' )
            CALL ERRINT ( '#',  I                                   )
            CALL ERRDP  ( '#',  IVLBDS(I)                           )
            CALL ERRDP  ( '#',  SCLKDP(BEPIX)                       )
            CALL SIGERR ( 'SPICE(BOUNDSDISAGREE)'                   )
            CALL CHKOUT ( 'CKW06'                                   )
            RETURN
 
         ELSE IF (  SCLKDP(EEPIX) .LT. IVLBDS(I) ) THEN
 
            CALL SETMSG ( 'Mini-segment interval # start time # '
     .      //            'follows mini-segment''s last epoch #.'   )
            CALL ERRINT ( '#',  I                                   )
            CALL ERRDP  ( '#',  IVLBDS(I)                           )
            CALL ERRDP  ( '#',  SCLKDP(EEPIX)                       )
            CALL SIGERR ( 'SPICE(BOUNDSDISAGREE)'                   )
            CALL CHKOUT ( 'CKW06'                                   )
            RETURN

         END IF

C
C        Make sure that the quaternions are non-zero. This is just a
C        check for uninitialized data.
C
C        For the Hermite subtypes, make sure quaternions are suitable
C        for interpolation.
C
         DO J = 1, NPKTS(I)              
C
C           We have to address the quaternion explicitly, since the
C           shape of the packet array is not known at compile time.
C
            ADDR = PKTBEG  +  ( PKTSIZ*(J-1) )  -  1  +  QIDX

            IF (  VZEROG( PACKTS(ADDR), 4 )  ) THEN
                  
               CALL SETMSG ( 'The quaternion in packet # within '
     .         //            'mini-segment # has magnitude zero.' )
               CALL ERRINT ( '#', J                               )
               CALL ERRINT ( '#', I                               )
               CALL SIGERR ( 'SPICE(ZEROQUATERNION)'              )
               CALL CHKOUT ( 'CKW06'                              )
               RETURN
 
            END IF

C
C           For the Hermite subtypes, each quaternion must be closer
C           than its negative to its predecessor in the quaternion
C           sequence.
C           
            
            IF (  ( J .GT. 1 )  .AND.  EVEN(SUBTYP)  ) THEN
C
C              Compare the distance between the current quaternion
C              and its predecessor vs the distance between the 
C              negative of the current quaternion and its predecessor.
C                             
               CALL MOVED  ( PACKTS(ADDR),        4, Q     )
               CALL MOVED  ( PACKTS(ADDR-PKTSIZ), 4, PREVQ )
               CALL VMINUG ( Q, 4, QNEG )

               IF (      VDISTG(PREVQ, QNEG, 4 ) 
     .              .LT. VDISTG(PREVQ, Q,    4 )  ) THEN

                  CALL SETMSG ( 'The quaternion in packet # within '
     .            //            'mini-segment # is farther than its '
     .            //            'negative from its predecessor at '
     .            //            'index #. This makes the quaternion '
     .            //            'sequence unsuitable for Hermite '
     .            //            'interpolation. The quaternions, and '
     .            //            'if applicable, their derivatives, '
     .            //            'must be adjusted before they are '
     .            //            'passed to this routine.'              )
                  CALL ERRINT ( '#', J                                 )
                  CALL ERRINT ( '#', I                                 )
                  CALL ERRINT ( '#', J-1                               )
                  CALL SIGERR ( 'SPICE(BADQUATSIGN)'                   )
                  CALL CHKOUT ( 'CKW06'                                )
                  RETURN

               END IF

            END IF
 
         END DO

      END DO

C     
C     If we made it this far, we're ready to start writing the segment.
C
C     The type 6 segment structure is eloquently described by this
C     diagram from the CK Required Reading:
C
C        +---------------------------------------+
C        | Mini-segment 1                        |
C        +---------------------------------------+
C              .
C              .
C              .
C        +---------------------------------------+
C        | Mini-segment N                        |
C        +---------------------------------------+
C        | Mini-segment interval 1 start time    |
C        +---------------------------------------+
C              .
C              .
C              .
C        +---------------------------------------+
C        | Mini-segment interval N start time    |
C        +---------------------------------------+
C        | Mini-segment interval N stop time     |
C        +---------------------------------------+
C        | Mini-seg. interval start time 100     | (First interval 
C        +---------------------------------------+  directory)
C              .
C              .
C              .
C        +---------------------------------------+
C        | Mini-seg. ival. start time (N/100)*100| (Last interval 
C        +---------------------------------------+  directory)
C        | Mini-segment 1 start pointer          |
C        +---------------------------------------+
C              .
C              .
C              .
C        +---------------------------------------+
C        | Mini-segment N start pointer          |
C        +---------------------------------------+
C        | Mini-segment N stop pointer + 1       |
C        +---------------------------------------+
C        | Boundary choice flag                  |
C        +---------------------------------------+
C        | Number of intervals                   |
C        +---------------------------------------+
C
C     CK type 6 mini-segments have the following structure:
C
C        +-----------------------+
C        | Packet 1              |
C        +-----------------------+
C                    .
C                    .
C                    .
C        +-----------------------+
C        | Packet M              |
C        +-----------------------+
C        | Epoch 1               |
C        +-----------------------+
C                    .
C                    .
C                    .
C        +-----------------------+
C        | Epoch M               |
C        +-----------------------+
C        | Epoch 100             | (First time tag directory)
C        +-----------------------+
C                    .
C                    .
C                    .
C        +-----------------------+
C        | Epoch ((M-1)/100)*100 | (Last time tag directory)
C        +-----------------------+
C        | Clock rate (sec/tick) |
C        +-----------------------+
C        | Subtype code          |
C        +-----------------------+
C        | Window size           |
C        +-----------------------+
C        | Number of packets     |
C        +-----------------------+
C
C     Note that the set of parameters at the end of a mini-segment does
C     not contain an mini-segment interval count. This is because,
C     unlike a CK type 5 segment, a CK type 6 segment can contain at
C     most one gap. If present, the gap is located at the end of
C     mini-segment's mini-segment interval.
C
C     Create the segment descriptor. We don't use CKPDS because
C     that routine doesn't allow creation of a singleton segment.
C
      IC(1) = INST
      IC(2) = REFCOD
      IC(3) = DTYPE

      IF ( AVFLAG ) THEN
         IC(4) = 1
      ELSE
         IC(4) = 0
      END IF

      DC(1) = FIRST
      DC(2) = LAST

      CALL DAFPS  ( ND, NI, DC, IC, DESCR )
 
C
C     Begin a new segment.
C
      CALL DAFBNA ( HANDLE, DESCR, SEGID )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'CKW06' )
         RETURN
      END IF

C
C     Re-initialize the mini-segment packet array indices,
C     and those of the mini-segment epoch array as well.
C
      PKTBEG = 0
      PKTEND = 0

      BEPIX  = 0
      EEPIX  = 0

C
C     Write data for each mini-segment to the file.
C
      DO I = 1, NMINI
C
C        Set the packet size, which is a function of the subtype.
C 
         SUBTYP = SUBTPS ( I )

         PKTSIZ = PKTSZS ( SUBTYP )

         IF ( ODD(SUBTYP) ) THEN

            WINSIZ =   DEGRES(I) + 1
         ELSE
            WINSIZ = ( DEGRES(I) + 1 ) / 2
         END IF

C
C        Now that we have the packet size, we can compute
C        mini-segment packet index range. We'll let PKTDSZ
C        be the total count of packet data entries for this
C        mini-segment.
C
         PKTDSZ = NPKTS(I) * PKTSIZ

         PKTBEG = PKTEND  +  1
         PKTEND = PKTBEG  -  1  +  PKTDSZ

C
C        At this point, we're read to start writing the 
C        current mini-segment to the file. Start with the
C        packet data.
C
         CALL DAFADA ( PACKTS(PKTBEG), PKTDSZ )

C
C        Write the epochs for this mini-segment.
C
         BEPIX = EEPIX + 1
         EEPIX = BEPIX - 1 + NPKTS(I)

         CALL DAFADA ( SCLKDP(BEPIX), NPKTS(I) )

C
C        Compute the number of epoch directories for the 
C        current mini-segment.
C
         NDIR = ( NPKTS(I) - 1 ) / DIRSIZ

C        
C        Write the epoch directories to the segment.
C
         DO J = 1, NDIR

            K = BEPIX  -  1  +  ( J * DIRSIZ )

            CALL DAFADA ( SCLKDP(K), 1 )

         END DO

C
C        Write the mini-segment's SCLK rate, subtype, window size, and
C        packet count to the segment.
C
         CALL DAFADA ( RATES(I),           1 )
         CALL DAFADA ( DBLE( SUBTPS(I) ),  1 )
         CALL DAFADA ( DBLE( WINSIZ    ),  1 )
         CALL DAFADA ( DBLE( NPKTS(I)  ),  1 )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'CKW06' )
            RETURN
         END IF

      END DO
 
C
C     We've finished writing the mini-segments.
C
C     Next write the mini-segment interval bounds.
C
      CALL DAFADA ( IVLBDS, NMINI+1 )
 
C
C     Create and write directories for the interval
C     bounds.
C
C     The directory count is the interval bound count
C     (N+1), minus 1, divided by the directory size.
C
      NDIR = NMINI / DIRSIZ

      DO I = 1, NDIR
         CALL DAFADA (  IVLBDS( DIRSIZ*I ),  1  )
      END DO
 
C
C     Now we compute and write the start/stop pointers
C     for each mini-segment.
C
C     The pointers are relative to the DAF address 
C     preceding the segment. For example, a pointer
C     to the first DAF address in the segment has
C     value 1.
C
      SEGEND = 0

      DO I = 1, NMINI
C
C        Set the packet size, which is a function of the subtype. Also
C        set the window size. First check the subtype, which will be
C        used as an array index.
C
         PKTSIZ = PKTSZS ( SUBTPS(I) )

C
C        In order to compute the end pointer of the current
C        mini-segment, we must compute the size, in terms
C        of DAF addresses, of this mini-segment. The formula
C        for the size is
C
C            size =     n_packets * packet_size 
C                    +  n_epochs
C                    +  n_epoch_directories
C                    +  4
C
C                 =     n_packets * ( packet_size + 1 )
C                    +  ( n_packets - 1 ) / DIRSIZ 
C                    +  4
C
         MINISZ =       NPKTS(I) * ( PKTSIZ + 1 )
     .              + ( NPKTS(I) - 1 ) / DIRSIZ
     .              +   4


         SEGBEG = SEGEND + 1
         SEGEND = SEGBEG + MINISZ - 1

C
C        Write the mini-segment begin pointer. 
C
C        After the loop terminates, the final end pointer, incremented
C        by 1, will be written.
C        
         CALL DAFADA ( DBLE(SEGBEG), 1 )

      END DO

C
C     Write the last mini-segment end pointer, incremented by one.
C     SEGEND was computed on the last iteration of the above loop.
C        
      CALL DAFADA ( DBLE(SEGEND+1), 1 )

C
C     Write out the interval selection flag. The input
C     boolean value is represented by a numeric constant.
C
      IF ( SELLST ) THEN
         
         ISEL = ITRUE
      ELSE
         ISEL = IFALSE
      END IF

      CALL DAFADA ( DBLE( ISEL ),  1 )
 
C
C     Write the mini-segment/mini-segment interval count.
C
      CALL DAFADA ( DBLE(NMINI), 1 )

C
C     End the segment.
C
      CALL DAFENA
 
      CALL CHKOUT ( 'CKW06' )
      RETURN
      END
