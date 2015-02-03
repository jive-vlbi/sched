C$Procedure PCKW20 ( PCK, write segment, type 20 )

      SUBROUTINE PCKW20 (  HANDLE,  CLSSID,  FRAME,   FIRST,   
     .                     LAST,    SEGID,   INTLEN,  N,
     .                     POLYDG,  CDATA,   ASCALE,  TSCALE,
     .                     INITJD,  INITFR                   )

C$ Abstract
C
C     Write a type 20 segment to a PCK file.
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
C     DAF
C     NAIF_IDS
C     TIME
C     PCK
C     ROTATION
C
C$ Keywords
C
C     ORIENTATION
C
C$ Declarations
    
      IMPLICIT NONE
      INCLUDE 'spk20.inc'

      INTEGER               HANDLE
      INTEGER               CLSSID
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      CHARACTER*(*)         SEGID
      DOUBLE PRECISION      INTLEN
      INTEGER               N
      INTEGER               POLYDG
      DOUBLE PRECISION      CDATA  ( * )
      DOUBLE PRECISION      ASCALE
      DOUBLE PRECISION      TSCALE
      DOUBLE PRECISION      INITJD
      DOUBLE PRECISION      INITFR

C$ Brief_I/O
C
C   Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of PCK file open for writing.
C     CLSSID     I   NAIF PCK frame class ID.
C     FRAME      I   Reference frame name.
C     FIRST      I   Start time of interval covered by segment.
C     LAST       I   End time of interval covered by segment.
C     SEGID      I   Segment identifier.
C     INTLEN     I   Length of time covered by logical record (days).
C     N          I   Number of logical records in segment.
C     POLYDG     I   Chebyshev polynomial degree.
C     CDATA      I   Array of Chebyshev coefficients and angles.
C     ASCALE     I   Angular scale of data.
C     TSCALE     I   Time scale of data.
C     INITJD     I   Integer part of begin time (TDB Julian date) of
C                    first record.
C     INITFR     I   Fractional part of begin time (TDB Julian date) of
C                    first record.
C     MAXDEG     P   Maximum allowed degree of Chebyshev expansions.
C     TOLSCL     P   Tolerance scale factor for coverage bound checking.
C
C$ Detailed_Input
C
C     HANDLE         is the DAF handle of a PCK file to which a type 20
C                    segment is to be added. The PCK file must be open
C                    for writing.
C
C     CLSSID         is the integer NAIF PCK frame class ID code of the
C                    reference frame whose orientation relative to its
C                    base frame is described by the segment to be
C                    created. See the Frames Required Reading for
C                    details.
C                     
C     FRAME          is the NAIF name for a reference frame relative to
C                    which the orientation information for CLSSID is
C                    specified. This frame is called the "base frame."
C
C     FIRST, 
C     LAST           are the start and stop times of the time interval
C                    over which the segment defines the orientation of
C                    the reference frame identified by CLSSID.
C
C     SEGID          is a segment identifier. A PCK segment identifier
C                    may contain up to 40 characters.
C
C     INTLEN         is the length of time, in TDB Julian days, covered
C                    by each set of Chebyshev polynomial coefficients
C                    (each logical record).
C
C     N              is the number of logical records to be stored in
C                    the segment. There is one logical record for each
C                    time period. Each logical record contains three
C                    sets of Chebyshev coefficients---one for each
C                    coordinate---and three position vector components.
C
C     POLYDG         is the degree of each set of Chebyshev
C                    polynomials, i.e. the number of Chebyshev
C                    coefficients per angle minus one. POLYDG must be
C                    less than or equal to the parameter MAXDEG.
C
C     CDATA          is an array containing sets of Chebyshev
C                    polynomial coefficients and angles to be placed in
C                    the new segment of the PCK file. The Chebyshev
C                    coefficients represent Euler angle rates; the
C                    angles are values of the Euler angles at each
C                    interval midpoint. The angular and time units of
C                    the data are defined by the inputs ASCALE and
C                    TSCALE, which are described below.
C
C                    The Euler angles represent the orientation of the
C                    reference frame designated by CLSSID relative to
C                    its base frame. The angles, which are numbered
C                    according to their ordinal position in the logical
C                    records, define a transformation matrix R as
C                    follows:
C
C                       R = [ A*ANGLE_3 ]  [ A*ANGLE_2 ]  [ A*ANGLE_1 ]
C                                        3              1              3
C
C                    where A is the angular scale ASCALE. Here the
C                    notation
C
C                       [ THETA ]
C                                i
C
C                    denotes a reference frame rotation of THETA
C                    radians in the right-hand sense about the ith
C                    coordinate axis. See the Rotation Required Reading
C                    for further discussion of this notation.
C
C                    The matrix R transforms vectors expressed in the
C                    base frame to vectors expressed in the frame
C                    associated with CLSSID by left multiplication:
C
C                       V       = R * V
C                        CLSSID        FRAME
C
C                    In cases where the frame designated by CLSSID
C                    (which we'll abbreviate as "the CLSSID frame") is
C                    a body-fixed, right-handed frame with its +Z axis
C                    aligned with a body's north pole, the orientation
C                    angles are related to right ascension (RA) and
C                    declination (DEC) of the CLSSID frame's north
C                    pole, and prime meridian orientation (W), by the
C                    equations
C
C                       ANGLE_1 * ASCALE = RA   + pi/2 radians
C                       ANGLE_2 * ASCALE = pi/2 - DEC  radians
C                       ANGLE_3 * ASCALE = W           radians      
C
C                    The coefficients and angles are stored in CDATA in
C                    order as follows:
C
C                       the (POLYDG + 1) coefficients for the rate of
C                       the first angle of the first logical record,
C                       followed by the value of the first angle at the
C                       first interval midpoint.
C
C                       the coefficients for the rate of the second
C                       angle of the first logical record, followed by
C                       the value of the second angle at the first
C                       interval midpoint.
C
C                       the coefficients for the rate of the third
C                       angle of the first logical record, followed by
C                       the value of the third angle at the first
C                       interval midpoint.
C
C                       the (degree + 1) coefficients for the rate of
C                       the first angle of the second logical record,
C                       followed by the value of the first angle at the
C                       second interval midpoint.
C
C                       and so on.
C
C                    The logical data records are stored contiguously:
C                
C                       +----------+
C                       | Record 1 | 
C                       +----------+
C                       | Record 2 | 
C                       +----------+
C                           ...
C                       +----------+
C                       | Record N | 
C                       +----------+
C
C                    The contents of an individual record are:
C                                        
C                       +--------------------------------------+
C                       | Coeff set for ANGLE_1 rate           |
C                       +--------------------------------------+
C                       | ANGLE_1                              |
C                       +--------------------------------------+
C                       | Coeff set for ANGLE_2 rate           |
C                       +--------------------------------------+
C                       | ANGLE_2                              |
C                       +--------------------------------------+
C                       | Coeff set for ANGLE_3 rate           |
C                       +--------------------------------------+
C                       | ANGLE_3                              |
C                       +--------------------------------------+
C
C                   Each coefficient set has the structure:
C
C                       +--------------------------------------+
C                       | Coefficient of T_0                   |
C                       +--------------------------------------+
C                       | Coefficient of T_1                   |
C                       +--------------------------------------+
C                                         ...
C                       +--------------------------------------+
C                       | Coefficient of T_POLYDG              |
C                       +--------------------------------------+
C
C                    Where T_n represents the Chebyshev polynomial
C                    of the first kind of degree n.
C
C     ASCALE, 
C     TSCALE         are, respectively, the angular scale of the input
C                    angle and angular rate data in radians, and the
C                    time scale of the input rate data in TDB
C                    seconds.
C
C                    For example, if the input angular data have units
C                    of degrees, ASCALE should be set to the number of
C                    radians in one degree. If the input rate data have
C                    time units of Julian days, then TSCALE should be
C                    set to the number of seconds per Julian day
C                    (86400).
C
C
C     INITJD         is the integer part of the Julian ephemeris date
C                    of initial epoch of the first record. INITJD may
C                    be less than, equal to, or greater than the
C                    initial epoch.
C
C     INITFR         is the fractional part of the Julian ephemeris
C                    date of initial epoch of the first record. INITFR
C                    has units of Julian days. INITFR has magnitude
C                    strictly less than 1 day. The sum
C
C                       INITJD + INITFR 
C
C                    equals the Julian ephemeris date of the initial
C                    epoch of the first record.
C
C
C$ Detailed_Output
C
C     None. This routine writes data to a PCK file.
C
C$ Parameters
C
C     MAXDEG         is the maximum allowed degree of the input
C                    Chebyshev expansions. MAXDEG is declared in the
C                    Fortran INCLUDE file pck20.inc.
C
C     TOLSCL         is a tolerance scale factor (also called a 
C                    "relative tolerance") used for time coverage
C                    bound checking. TOLSCL is unitless. TOLSCL
C                    produces a tolerance value via the formula
C
C                       TOL = TOLSCL * MAX( ABS(FIRST), ABS(LAST) ) 
C
C                    where FIRST and LAST are the coverage time bounds
C                    of a type 20 segment, expressed as seconds past 
C                    J2000 TDB.
C
C                    The resulting parameter TOL is used as a tolerance
C                    for comparing the input segment descriptor time 
C                    bounds to the first and last epoch covered by the
C                    sequence of time intervals defined by the inputs
C                    to PCKW20:
C
C                       INITJD
C                       INITFR
C                       INTLEN
C                       N
C                        
C                    TOLSCL is declared in the Fortran INCLUDE file
C                    pck20.inc.
C
C                    See the Exceptions section below for a description
C                    of the error check using this tolerance.
C
C$ Exceptions
C
C     1)  If the number of sets of coefficients is not positive
C         SPICE(INVALIDCOUNT) is signaled.
C
C     2)  If the interval length is not positive, SPICE(INTLENNOTPOS)
C         is signaled.
C
C     3)  If the name of the reference frame is not recognized,
C         SPICE(INVALIDREFFRAME) is signaled.
C
C     4)  If segment stop time is not greater then the begin time,
C         SPICE(BADDESCRTIMES) is signaled.
C
C     5)  If the start time of the first record exceeds the descriptor
C         begin time by more than a computed tolerance, or if the end
C         time of the last record precedes the descriptor end time by
C         more than a computed tolerance, the error SPICE(COVERAGEGAP)
C         is signaled. See the Parameters section above for a
C         description of the tolerance.
C
C     6)  If the input degree POLYDG is less than 0 or greater than
C         MAXDEG, the error SPICE(INVALIDDEGREE) is signaled.
C
C     7)  If the last non-blank character of SEGID occurs past index
C         40, or if SEGID contains any nonprintable characters, the
C         error will be diagnosed by a routine in the call tree of this
C         routine.
C
C     8)  If either the angle or time scale is non-positive, the
C         error SPICE(NONPOSITIVESCALE) will be signaled.
C
C$ Files
C
C     A new type 20 PCK segment is written to the PCK file attached
C     to HANDLE.
C
C$ Particulars
C
C     This routine writes a PCK type 20 data segment to the designated
C     PCK file, according to the format described in the PCK Required
C     Reading.
C
C     Each segment can contain data for only one reference frame
C     and base frame. The Chebyshev polynomial degree and length
C     of time covered by each logical record are also fixed. However,
C     an arbitrary number of logical records of Chebyshev polynomial
C     coefficients can be written in each segment.  Minimizing the
C     number of segments in a PCK file will help optimize how the
C     SPICE system accesses the file.
C
C$ Examples
C
C     Suppose that you have in an array CDATA sets of Chebyshev
C     polynomial coefficients and angles representing the orientation
C     of the moon, relative to the J2000 reference frame, and you want
C     to put these into a type 20 segment in an existing PCK file. The
C     following code could be used to add one new type 20 segment. To
C     add multiple segments, put the call to PCKW20 in a loop.
C
C     C
C     C      First open the PCK file and get a handle for it.
C     C
C            CALL DAFOPW ( PCKNAM, HANDLE )
C
C     C
C     C      Create a segment identifier.
C     C
C            SEGID = 'MY_SAMPLE_PCK_TYPE_20_SEGMENT'
C
C     C
C     C      Note that the interval length INTLEN has units
C     C      of Julian days. The start time of the first record
C     C      is expressed using two inputs: integer and fractional
C     C      portions of the Julian ephemeris date of the start 
C     C      time.
C     C
C     C      The PCK frame class ID code is stored in the
C     C      variable CLSSID. This ID must be associated in 
C     C      with a PCK frame; usually such an association is
C     C      made via a frame kernel.
C     C
C     C      Write the segment. 
C     C
C            CALL PCKW20 ( HANDLE, CLSSID, 'J2000', FIRST,
C          .               LAST,   SEGID,  INTLEN,  N,
C          .               POLYDG, CDATA,  ASCALE,  TSCALE
C          .               INITJD, INITFR                  )
C
C     C
C     C      Close the file.
C     C
C            CALL DAFCLS ( HANDLE )
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
C     N.J. Bachman (JPL)
C     K.S. Zukor   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 17-JAN-2014 (NJB) (KSZ)
C
C-&


C$ Index_Entries
C
C     write pck type_20 data segment
C
C-&

C
C     SPICELIB functions
C
      DOUBLE PRECISION      J2000
      DOUBLE PRECISION      SPD

      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local Parameters
C

C
C     DTYPE is the PCK data type.
C
      INTEGER               DTYPE
      PARAMETER           ( DTYPE   =   20 )

C
C     ND is the number of double precision components in a PCK
C     segment descriptor. PCK uses ND = 2.
C
      INTEGER               ND
      PARAMETER           ( ND      =   2 )

C
C     NI is the number of integer components in a PCK segment
C     descriptor. PCK uses NI = 5.
C
      INTEGER               NI
      PARAMETER           ( NI      =   5 )

C
C     NS is the size of a packed PCK segment descriptor.
C
      INTEGER               NS
      PARAMETER           ( NS      =   5 )

C
C     SIDLEN is the maximum number of characters allowed in an
C     PCK segment identifier.
C
      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN  =  40 )

C
C     Local variables
C
      CHARACTER*(SIDLEN)    ETSTR
      CHARACTER*(SIDLEN)    NETSTR

      DOUBLE PRECISION      BTIME
      DOUBLE PRECISION      DCD   ( ND )
      DOUBLE PRECISION      DESCR ( NS )
      DOUBLE PRECISION      LTIME
      DOUBLE PRECISION      NUMREC
      DOUBLE PRECISION      TOL

      INTEGER               ICD   ( NI )
      INTEGER               NINREC
      INTEGER               REFCOD

C
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'PCKW20' )

C
C     The number of sets of coefficients must be positive.
C
      IF ( N .LE. 0 ) THEN

         CALL SETMSG ( 'The number of sets of coordinate '   
     .   //            'coefficients is not positive. N = #.' )
         CALL ERRINT ( '#', N                                 )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                  )
         CALL CHKOUT ( 'PCKW20'                               )
         RETURN

      END IF

C
C     Make sure that the degree of the interpolating polynomials is
C     in range.
C
      IF (  ( POLYDG .LT. 0 ) .OR. ( POLYDG .GT. MAXDEG )  )  THEN
 
         CALL SETMSG ( 'The interpolating polynomials have degree #; '
     .   //            'the valid degree range is [0, #].'             )
         CALL ERRINT ( '#', POLYDG                                     )
         CALL ERRINT ( '#', MAXDEG                                     )
         CALL SIGERR ( 'SPICE(INVALIDDEGREE)'                          )
         CALL CHKOUT ( 'PCKW20'                                        )
         RETURN
 
      END IF

C
C     The interval length must be positive.
C
      IF ( INTLEN .LE. 0 ) THEN

         CALL SETMSG ( 'The interval length is not positive.'
     .   //            'N = #'                                )
         CALL ERRDP  ( '#', INTLEN                            )
         CALL SIGERR ( 'SPICE(INTLENNOTPOS)'                  )
         CALL CHKOUT ( 'PCKW20'                               )
         RETURN

      END IF

C
C     Get the NAIF integer code for the reference frame.
C
      CALL NAMFRM ( FRAME, REFCOD )

      IF ( REFCOD .EQ. 0 ) THEN

         CALL SETMSG ( 'The reference frame # is not supported.' )
         CALL ERRCH  ( '#', FRAME                                )
         CALL SIGERR ( 'SPICE(INVALIDREFFRAME)'                  )
         CALL CHKOUT ( 'PCKW20'                                  )
         RETURN

      END IF

C
C     The segment stop time must be greater than the begin time.
C
      IF ( FIRST .GE. LAST ) THEN

         CALL SETMSG ( 'The segment start time: # (# TDB) is ' 
     .   //            'not less than the segment end time: '
     .   //            '(# TDB).'                               )
         CALL ETCAL  ( FIRST, ETSTR                             )
         CALL ERRCH  ( '#',   ETSTR                             )
         CALL ERRDP  ( '#',   FIRST                             )
         CALL ETCAL  ( LAST,  NETSTR                            )
         CALL ERRCH  ( '#',   NETSTR                            )
         CALL ERRDP  ( '#',   LAST                              )
         CALL SIGERR ( 'SPICE(BADDESCRTIMES)'                   )
         CALL CHKOUT ( 'PCKW20'                                 )
         RETURN

      END IF

C
C     The angle and time scales must be positive.
C
      IF ( ASCALE .LE. 0.D0 ) THEN

         CALL SETMSG ( 'The angle scale is not positive.'  //
     .                 'ASCALE = #'                           )
         CALL ERRDP  ( '#', ASCALE                            )
         CALL SIGERR ( 'SPICE(NONPOSITIVESCALE)'              )
         CALL CHKOUT ( 'PCKW20'                               )
         RETURN

      END IF

      IF ( TSCALE .LE. 0.D0 ) THEN

         CALL SETMSG ( 'The time scale is not positive.'     //
     .                 'TSCALE = #'                           )
         CALL ERRDP  ( '#', TSCALE                            )
         CALL SIGERR ( 'SPICE(NONPOSITIVESCALE)'              )
         CALL CHKOUT ( 'PCKW20'                               )
         RETURN

      END IF

C
C     The begin time of the first record must be less than or equal
C     to the begin time of the segment. Convert the two-part input
C     epoch to seconds past J2000 for the purpose of this check.
C
      BTIME = SPD()  *  (  ( INITJD - J2000() ) + INITFR  )

      LTIME = BTIME  +  ( N * INTLEN * SPD() )

C
C     Compute the tolerance to use for descriptor time bound checks.
C
      TOL = TOLSCL * MAX( ABS(BTIME), ABS(LTIME) )  

      IF ( FIRST .LT. BTIME - TOL ) THEN

         CALL SETMSG ( 'The segment descriptor start time # is too ' 
     .   //            'much less than the beginning time of the ' 
     .   //            'segment data # (in seconds past J2000: #). '
     .   //            'The difference is # seconds; the '
     .   //            'tolerance is # seconds.'                     )
         CALL ETCAL  ( FIRST, ETSTR                                  )
         CALL ERRCH  ( '#',   ETSTR                                  )
         CALL ETCAL  ( BTIME, ETSTR                                  )
         CALL ERRCH  ( '#',   ETSTR                                  )
         CALL ERRDP  ( '#',   FIRST                                  )
         CALL ERRDP  ( '#',   BTIME-FIRST                            )
         CALL ERRDP  ( '#',   TOL                                    )
         CALL SIGERR ( 'SPICE(COVERAGEGAP)'                          )
         CALL CHKOUT ( 'PCKW20'                                      )
         RETURN

      END IF

C
C     The end time of the final record must be greater than or
C     equal to the end time of the segment.
C
      IF ( LAST .GT. LTIME + TOL ) THEN

         CALL SETMSG ( 'The segment descriptor end time # is too ' 
     .   //            'much greater than the end time of the segment ' 
     .   //            'data # (in seconds past J2000: #). The '
     .   //            'difference is # seconds; the tolerance is # '
     .   //            'seconds.'                                    )
         CALL ETCAL  ( LAST,  ETSTR                                  )
         CALL ERRCH  ( '#',   ETSTR                                  )
         CALL ETCAL  ( LTIME, ETSTR                                  )
         CALL ERRCH  ( '#',   ETSTR                                  )
         CALL ERRDP  ( '#',   LAST                                   )
         CALL ERRDP  ( '#',   LAST-LTIME                             )
         CALL ERRDP  ( '#',   TOL                                    )
         CALL SIGERR ( 'SPICE(COVERAGEGAP)'                          )
         CALL CHKOUT ( 'PCKW20'                                      )
         RETURN

      END IF

C
C     Now check the validity of the segment identifier.
C
      CALL CHCKID ( 'PCK segment identifier', SIDLEN, SEGID )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'PCKW20' )
         RETURN
      END IF

C
C     Store the start and end times to be associated
C     with this segment.
C
      DCD(1) = FIRST
      DCD(2) = LAST

C
C     Create the integer portion of the descriptor.
C
      ICD(1) = CLSSID
      ICD(2) = REFCOD
      ICD(3) = DTYPE

C
C     Pack the segment descriptor.
C
      CALL DAFPS ( ND, NI, DCD, ICD, DESCR )

C
C     Begin a new segment of PCK type 20 form:
C
C        Record 1
C        Record 2
C        ...
C        Record N
C        ASCALE     ( angular scale in radians )
C        TSCALE     ( time scale in seconds )
C        INITJD     ( integer part of initial epoch of first record, 
C                     expressed as a TDB Julian date )
C        INITFR     ( fractional part of initial epoch, in units of
C                     TDB Julian days )
C        INTLEN     ( length of interval covered by each record, in
C                     units of TDB Julian days )
C        RSIZE      ( number of data elements in each record )
C        N          ( number of records in segment )
C
C     Each record will have the form:
C
C        ANGLE_1 coefficients
C        ANGLE_1 angle at interval midpoint
C        ANGLE_2 coefficients
C        ANGLE_2 angle at interval midpoint
C        ANGLE_3 coefficients
C        ANGLE_3 angle at interval midpoint
C
C
      CALL DAFBNA ( HANDLE, DESCR, SEGID )

C
C     Calculate the number of entries in a record.
C
      NINREC = ( POLYDG + 2 ) * 3

C
C     Fill segment with N records of data.
C
      CALL DAFADA ( CDATA, N * NINREC )

C
C     Store the angle and time scales.
C
      CALL DAFADA ( ASCALE, 1 )
      CALL DAFADA ( TSCALE, 1 )

C
C     Store the integer and fractional parts of the initial epoch of
C     the first record.
C
      CALL DAFADA ( INITJD, 1 )
      CALL DAFADA ( INITFR, 1 )

C
C     Store the length of interval covered by each record.
C
      CALL DAFADA ( INTLEN, 1 )

C
C     Store the size of each record (total number of array elements).
C     Note that this size is smaller by 2 than the size of a type 2
C     record of the same degree, since the record coverage midpoint
C     and radius are not stored.
C
      CALL DAFADA ( DBLE( NINREC ), 1 )

C
C     Store the number of records contained in the segment.
C
      NUMREC = N
      CALL DAFADA ( NUMREC, 1 )

C
C     End this segment.
C
      CALL DAFENA

      CALL CHKOUT ( 'PCKW20' )
      RETURN
      END

