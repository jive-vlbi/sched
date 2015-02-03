C$Procedure SPKW20 ( SPK, write segment, type 20 )

      SUBROUTINE SPKW20 (  HANDLE,  BODY,    CENTER,  FRAME,
     .                     FIRST,   LAST,    SEGID,   INTLEN,
     .                     N,       POLYDG,  CDATA,   DSCALE,
     .                     TSCALE,  INITJD,  INITFR          )

C$ Abstract
C
C     Write a type 20 segment to an SPK file.
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
C     SPK
C
C$ Keywords
C
C     EPHEMERIS
C
C$ Declarations
    
      IMPLICIT NONE
      INCLUDE 'spk20.inc'

      INTEGER               HANDLE
      INTEGER               BODY
      INTEGER               CENTER
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      CHARACTER*(*)         SEGID
      DOUBLE PRECISION      INTLEN
      INTEGER               N
      INTEGER               POLYDG
      DOUBLE PRECISION      CDATA  ( * )
      DOUBLE PRECISION      DSCALE
      DOUBLE PRECISION      TSCALE
      DOUBLE PRECISION      INITJD
      DOUBLE PRECISION      INITFR

C$ Brief_I/O
C
C   Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of SPK file open for writing.
C     BODY       I   NAIF code for ephemeris object.
C     CENTER     I   NAIF code for the center of motion of the body.
C     FRAME      I   Reference frame name.
C     FIRST      I   Start time of interval covered by segment.
C     LAST       I   End time of interval covered by segment.
C     SEGID      I   Segment identifier.
C     INTLEN     I   Length of time covered by logical record (days).
C     N          I   Number of logical records in segment.
C     POLYDG     I   Chebyshev polynomial degree.
C     CDATA      I   Array of Chebyshev coefficients and positions.
C     DSCALE     I   Distance scale of data.
C     TSCALE     I   Time scale of data.
C     INITJD     I   Integer part of begin time (TDB Julian date) of
C                    first record.
C     INITFR     I   Fractional part of begin time (TDB Julian date) of
C                    first record.
C     MAXDEG     P   Maximum allowed degree of Chebyshev expansions.
C     TOLSCL     P   Tolerance scale for coverage bound checking.
C
C$ Detailed_Input
C
C     HANDLE         is the DAF handle of an SPK file to which a type 20
C                    segment is to be added.  The SPK file must be open
C                    for writing.
C
C     BODY           is the NAIF integer code for an ephemeris object
C                    whose state relative to another body is described
C                    by the segment to be created.
C
C     CENTER         is the NAIF integer code for the center of motion
C                    of the object identified by BODY.
C
C     FRAME          is the NAIF name for a reference frame relative to
C                    which the state information for BODY is specified.
C
C     FIRST,
C     LAST           are the start and stop times of the time interval
C                    over which the segment defines the state of the
C                    object identified by BODY.
C
C     SEGID          is a segment identifier. An SPK segment identifier
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
C                    coefficients per coordinate minus one. POLYDG must
C                    be less than or equal to the parameter MAXDEG.
C
C     CDATA          is an array containing all the sets of Chebyshev
C                    polynomial coefficients and position components to
C                    be placed in the new segment of the SPK file.
C                    There are three sets of coefficients and position
C                    components for each time interval covered by the
C                    segment.
C
C                    The coefficients and position components are
C                    stored in CDATA in order as follows:
C
C                       the (POLYDG + 1) coefficients for the first
C                       coordinate of the first logical record,
C                       followed by the X component of position at the
C                       first interval midpoint. The first coefficient
C                       is that of the constant term of the expansion.
C
C                       the coefficients for the second coordinate,
C                       followed by the Y component of position at the
C                       first interval midpoint.
C
C                       the coefficients for the third coordinate,
C                       followed by the Z component of position at the
C                       first interval midpoint.
C
C                       the coefficients for the first coordinate for
C                       the second logical record, followed by the X
C                       component of position at the second interval
C                       midpoint.
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
C                       | Coeff set for X velocity component   |
C                       +--------------------------------------+
C                       | X position component                 |
C                       +--------------------------------------+
C                       | Coeff set for Y velocity component   |
C                       +--------------------------------------+
C                       | Y position component                 |
C                       +--------------------------------------+
C                       | Coeff set for Z velocity component   |
C                       +--------------------------------------+
C                       | Z position component                 |
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
C
C     DSCALE, 
C     TSCALE         are, respectively, the distance scale of the input
C                    position and velocity data in km, and the time
C                    scale of the input velocity data in TDB seconds.
C
C                    For example, if the input distance data have units
C                    of astronomical units (AU), DSCALE should be set
C                    to the number of km in one AU. If the input
C                    velocity data have time units of Julian days, then
C                    TSCALE should be set to the number of seconds per
C                    Julian day (86400).
C
C
C     INITJD         is the integer part of the Julian ephemeris date
C                    of initial epoch of the first record. INITJD may
C                    be less than, equal to, or greater than the
C                    initial epoch.
C
C     INITFR         is the fractional part of the Julian ephemeris date
C                    of initial epoch of the first record. INITFR has
C                    units of Julian days. INITFR has magnitude
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
C     None. This routine writes data to an SPK file.
C
C$ Parameters
C
C     The parameters described in this section are declared in the 
C     Fortran INCLUDE file spk20.inc
C
C
C     MAXDEG         is the maximum allowed degree of the input
C                    Chebyshev expansions.
C
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
C
C                       INITJD
C                       INITFR
C                       INTLEN
C                       N
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
C     4)  If segment stop time is not greater than or equal to 
C         the begin time, SPICE(BADDESCRTIMES) is signaled.
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
C     8)  If either the distance or time scale is non-positive, the
C         error SPICE(NONPOSITIVESCALE) will be signaled.
C
C$ Files
C
C     A new type 20 SPK segment is written to the SPK file attached
C     to HANDLE.
C
C$ Particulars
C
C     This routine writes an SPK type 20 data segment to the designated
C     SPK file, according to the format described in the SPK Required
C     Reading.
C
C     Each segment can contain data for only one target, central body,
C     and reference frame. The Chebyshev polynomial degree and length
C     of time covered by each logical record are also fixed. However,
C     an arbitrary number of logical records of Chebyshev polynomial
C     coefficients can be written in each segment.  Minimizing the
C     number of segments in an SPK file will help optimize how the
C     SPICE system accesses the file.
C
C$ Examples
C
C     Suppose that you have in an array CDATA sets of Chebyshev
C     polynomial coefficients and position vectors representing the
C     state of the moon (NAIF ID = 301), relative to the Earth-moon
C     barycenter (NAIF ID = 3), in the J2000 reference frame, and you
C     want to put these into a type 20 segment in an existing SPK file.
C     The following code could be used to add one new type 20 segment.
C     To add multiple segments, put the call to SPKW20 in a loop.
C
C     C
C     C      First open the SPK file and get a handle for it.
C     C
C            CALL DAFOPW ( SPKNAM, HANDLE )
C
C     C
C     C      Create a segment identifier.
C     C
C            SEGID = 'MY_SAMPLE_SPK_TYPE_20_SEGMENT'
C
C     C
C     C      Note that the interval length INTLEN has units
C     C      of Julian days. The start time of the first record
C     C      is expressed using two inputs: integer and fractional
C     C      portions of the Julian ephemeris date of the start 
C     C      time.
C     C
C     C      Write the segment. 
C     C
C            CALL SPKW20 ( HANDLE, 301,    3,      'J2000',
C          .               FIRST,  LAST,   SEGID,  INTLEN,
C          .               N,      POLYDG, CDATA,  DSCALE,
C          .               TSCALE, INITJD, INITFR           )
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
C-    SPICELIB Version 1.0.0, 17-JAN-2017 (NJB) (KSZ)
C
C-&


C$ Index_Entries
C
C     write spk type_20 data segment
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
C     DTYPE is the SPK data type.
C
      INTEGER               DTYPE
      PARAMETER           ( DTYPE   =   20 )

C
C     ND is the number of double precision components in an SPK
C     segment descriptor. SPK uses ND = 2.
C
      INTEGER               ND
      PARAMETER           ( ND      =   2 )

C
C     NI is the number of integer components in an SPK segment
C     descriptor. SPK uses NI = 6.
C
      INTEGER               NI
      PARAMETER           ( NI      =   6 )

C
C     NS is the size of a packed SPK segment descriptor.
C
      INTEGER               NS
      PARAMETER           ( NS      =   5 )

C
C     SIDLEN is the maximum number of characters allowed in an
C     SPK segment identifier.
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

      CALL CHKIN ( 'SPKW20' )

C
C     The number of sets of coefficients must be positive.
C
      IF ( N .LE. 0 ) THEN

         CALL SETMSG ( 'The number of sets of coordinate '    
     .   //            'coefficients is not positive. N = # ' )
         CALL ERRINT ( '#', N                                 )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                  )
         CALL CHKOUT ( 'SPKW20'                               )
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
         CALL CHKOUT ( 'SPKW20'                                        )
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
         CALL CHKOUT ( 'SPKW20'                               )
         RETURN

      END IF

C
C     Get the NAIF integer code for the reference frame.
C
      CALL NAMFRM ( FRAME, REFCOD )

      IF ( REFCOD .EQ. 0 ) THEN

         CALL SETMSG ( 'The reference frame # is not supported.'   )
         CALL ERRCH  ( '#', FRAME                                  )
         CALL SIGERR ( 'SPICE(INVALIDREFFRAME)'                    )
         CALL CHKOUT ( 'SPKW20'                                    )
         RETURN

      END IF

C
C     The segment stop time must be greater than or equal to the begin
C     time.
C
      IF ( FIRST .GT. LAST ) THEN

         CALL SETMSG ( 'The segment start time: # (# TDB) is ' 
     .   //            'greater than the segment end time: '
     .   //            '(# TDB).'                               )
         CALL ETCAL  ( FIRST, ETSTR                             )
         CALL ERRCH  ( '#',   ETSTR                             )
         CALL ERRDP  ( '#',   FIRST                             )
         CALL ETCAL  ( LAST,  NETSTR                            )
         CALL ERRCH  ( '#',   NETSTR                            )
         CALL ERRDP  ( '#',   LAST                              )
         CALL SIGERR ( 'SPICE(BADDESCRTIMES)'                   )
         CALL CHKOUT ( 'SPKW20'                                 )
         RETURN

      END IF

C
C     The distance and time scales must be positive.
C
      IF ( DSCALE .LE. 0.D0 ) THEN

         CALL SETMSG ( 'The distance scale is not positive.'  
     .   //            'DSCALE = #'                           )
         CALL ERRDP  ( '#', DSCALE                            )
         CALL SIGERR ( 'SPICE(NONPOSITIVESCALE)'              )
         CALL CHKOUT ( 'SPKW20'                               )
         RETURN

      END IF

      IF ( TSCALE .LE. 0.D0 ) THEN

         CALL SETMSG ( 'The time scale is not positive.'      
     .   //            'TSCALE = #'                      )
         CALL ERRDP  ( '#', TSCALE                       )
         CALL SIGERR ( 'SPICE(NONPOSITIVESCALE)'         )
         CALL CHKOUT ( 'SPKW20'                          )
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
         CALL CHKOUT ( 'SPKW20'                                      )
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
         CALL CHKOUT ( 'SPKW20'                                      )
         RETURN

      END IF

C
C     Now check the validity of the segment identifier.
C
      CALL CHCKID ( 'SPK segment identifier', SIDLEN, SEGID )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKW20' )
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
      ICD(1) = BODY
      ICD(2) = CENTER
      ICD(3) = REFCOD
      ICD(4) = DTYPE

C
C     Pack the segment descriptor.
C
      CALL DAFPS ( ND, NI, DCD, ICD, DESCR )

C
C     Begin a new segment of SPK type 20 form:
C
C        Record 1
C        Record 2
C        ...
C        Record N
C        DSCALE     ( distance scale in km )
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
C        X coefficients
C        X position component at interval midpoint
C        Y coefficients
C        Y position component at interval midpoint
C        Z coefficients
C        Z position component at interval midpoint
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
C     Store the distance and time scales.
C
      CALL DAFADA ( DSCALE, 1 )
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

      CALL CHKOUT ( 'SPKW20' )
      RETURN
      END

