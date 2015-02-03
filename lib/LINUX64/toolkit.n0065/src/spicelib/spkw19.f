C$Procedure      SPKW19 ( Write SPK segment, type 19 )
 
      SUBROUTINE SPKW19 ( HANDLE,  BODY,    CENTER,  FRAME,    
     .                    FIRST,   LAST,    SEGID,   NINTVL,
     .                    NPKTS,   SUBTPS,  DEGRES,  PACKTS, 
     .                    EPOCHS,  IVLBDS,  SELLST          )

C$ Abstract
C
C     Write a type 19 segment to an SPK file.
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
C     SPC
C     SPK
C     TIME
C
C$ Keywords
C
C     EPHEMERIS
C     FILES
C
C$ Declarations
 
      IMPLICIT NONE 

      INCLUDE 'spk19.inc'
      INCLUDE 'spkrec.inc'

      
      INTEGER               HANDLE
      INTEGER               BODY
      INTEGER               CENTER
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      CHARACTER*(*)         SEGID
      INTEGER               NINTVL
      INTEGER               NPKTS  ( * )
      INTEGER               SUBTPS ( * )
      INTEGER               DEGRES ( * )
      DOUBLE PRECISION      PACKTS ( * )
      DOUBLE PRECISION      EPOCHS ( * )
      DOUBLE PRECISION      IVLBDS ( * )
      LOGICAL               SELLST
      
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of an SPK file open for writing.
C     BODY       I   NAIF ID code for an ephemeris object.
C     CENTER     I   NAIF ID code for center of motion of BODY.
C     FRAME      I   Reference frame name.
C     FIRST      I   Start time of interval covered by segment.
C     LAST       I   End time of interval covered by segment.
C     SEGID      I   Segment identifier.
C     NINTVL     I   Number of mini-segments and interpolation 
C                    intervals.
C     NPKTS      I   Array of packet counts of mini-segments.
C     SUBTPS     I   Array of segment subtypes of mini-segments.
C     DEGRES     I   Array of polynomial degrees of mini-segments.
C     PACKTS     I   Array of data packets of mini-segments.
C     EPOCHS     I   Array of epochs of mini-segments.
C     IVLBDS     I   Interpolation interval bounds.
C     SELLST     I   Interval selection flag.
C     MAXDEG     P   Maximum allowed degree of interpolating polynomial.
C
C$ Detailed_Input
C
C     HANDLE         is the handle of an SPK file that has been opened
C                    for writing.
C
C
C     BODY           is the NAIF integer code for an ephemeris object
C                    whose state relative to another body is described
C                    by the segment to be created.
C
C
C     CENTER         is the NAIF integer code for the center of motion
C                    of the object identified by BODY.
C
C
C     FRAME          is the NAIF name for a reference frame
C                    relative to which the state information for BODY
C                    is specified.
C
C     FIRST,
C     LAST           are, respectively, the bounds of the time interval
C                    over which the segment defines the state of BODY.
C
C                    FIRST must be greater than or equal to the first
C                    interpolation interval start time; LAST must be
C                    less than or equal to the last interpolation 
C                    interval stop time. See the description of IVLBDS
C                    below.
C
C
C     SEGID          is the segment identifier. An SPK segment
C                    identifier may contain up to 40 characters.
C
C
C     NINTVL         is the number of interpolation intervals
C                    associated with the input data. The interpolation
C                    intervals are associated with data sets referred
C                    to as "mini-segments."
C
C                    The input data comprising each mini-segment are:
C            
C                       - a packet count
C                       - a type 19 subtype
C                       - an interpolating polynomial degree
C                       - a sequence of type 19 data packets
C                       - a sequence of packet epochs
C                    
C                    These inputs are described below.
C
C
C     NPKTS          is an array of packet counts. The Ith element of
C                    NPKTS is the packet count of the Ith interpolation
C                    interval/mini-segment.
C
C                    NPKTS has dimension NINTVL.
C
C
C     SUBTPS         is an array of type 19 subtypes. The Ith element
C                    of SUBTPS is the subtype of the packets associated
C                    with the Ith interpolation interval/mini-segment.
C
C                    SUBTPS has dimension NINTVL.
C
C
C     DEGRES         is an array of interpolating polynomial degrees.
C                    The Ith element of DEGRES is the polynomial degree
C                    of the packets associated with the Ith
C                    interpolation interval/mini-segment.
C
C                    For subtype 0, interpolation degrees must be
C                    equivalent to 3 mod 4, that is, they must be in
C                    the set
C
C                       { 3, 7, 11, ..., MAXDEG }
C
C                    For subtype 1, interpolation degrees must be odd
C                    and must be in the range 1:MAXDEG.
C
C                    DEGRES has dimension NINTVL.
C
C
C     PACKTS         is an array containing data packets for all input
C                    mini-segments. The packets for a given
C                    mini-segment are stored contiguously in increasing
C                    time order. The order of the sets of packets for
C                    different mini-segments is the same as the order
C                    of their corresponding interpolation intervals.
C
C                    Each packet represents geometric states of BODY
C                    relative to CENTER, specified relative to FRAME.
C                    The packet structure depends on the segment
C                    subtype as follows:
C
C                       Type 0 (indicated by code S19TP0):
C
C                           x,  y,  z,  dx/dt,  dy/dt,  dz/dt,
C                           vx, vy, vz, dvx/dt, dvy/dt, dvz/dt
C                    
C                       where x, y, z represent Cartesian position
C                       components and  vx, vy, vz represent Cartesian
C                       velocity components.  Note well:  vx, vy, and
C                       vz *are not necessarily equal* to the time
C                       derivatives of x, y, and z. This packet 
C                       structure mimics that of the Rosetta/MEX orbit
C                       file.
C                      
C                       Type 1 (indicated by code S19TP1):
C
C                           x,  y,  z,  dx/dt,  dy/dt,  dz/dt
C                    
C                       where x, y, z represent Cartesian position
C                       components and  vx, vy, vz represent Cartesian
C                       velocity components.
C
C                    Position units are kilometers, velocity units 
C                    are kilometers per second, and acceleration units
C                    are kilometers per second per second.
C
C
C     EPOCHS         is an array containing epochs for all input
C                    mini-segments. Each epoch is expressed as seconds
C                    past J2000 TDB. The epochs have a one-to-one
C                    relationship with the packets in the input packet
C                    array.
C
C                    The epochs for a given mini-segment are stored
C                    contiguously in increasing order. The order of the
C                    sets of epochs for different mini-segments is the
C                    same as the order of their corresponding
C                    interpolation intervals.
C
C                    For each mini-segment, "padding" is allowed: the
C                    sequence of epochs for that mini-segment may start
C                    before the corresponding interpolation interval
C                    start time and end after the corresponding
C                    interpolation interval stop time. Padding is used
C                    to control behavior of interpolating polynomials
C                    near interpolation interval boundaries.
C
C                    Due to possible use of padding, the elements of
C                    EPOCHS, taken as a whole, may not be in increasing
C                    order.
C
C
C     IVLBDS         is an array of interpolation interval boundary
C                    times. This array is an ordered list of the
C                    interpolation interval start times, to which the
C                    the end time for the last interval is appended.
C
C                    The Ith interpolation interval is the time
C                    coverage interval of the Ith mini-segment (see the
C                    description of NPKTS above).
C
C                    For each mini-segment, the corresponding
C                    interpolation interval's start time is greater
C                    than or equal to the mini-segment's first epoch,
C                    and the interval's stop time is less than or equal
C                    to the mini-segment's last epoch.
C
C                    For each interpolation interval other than the
C                    last, the interval's coverage stop time coincides
C                    with the coverage start time of the next interval.
C                    There are no coverage gaps, and coverage overlap
C                    for adjacent intervals consists of a single epoch.
C                 
C                    IVLBDS has dimension NINTVL+1.
C
C
C     SELLST         is a logical flag indicating to the SPK type 19
C                    segment reader SPKR19 how to select the
C                    interpolation interval when a request time
C                    coincides with a time boundary shared by two
C                    interpolation intervals. When SELLST ("select
C                    last") is .TRUE., the later interval is selected;
C                    otherwise the earlier interval is selected.
C   
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
C                    See the INCLUDE file spk19.inc for the value of
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
C     2)  If FRAME is not a recognized name, the error
C         SPICE(INVALIDREFFRAME) is signaled.
C
C     3)  If the last non-blank character of SEGID occurs past index
C         40, the error SPICE(SEGIDTOOLONG) is signaled.
C
C     4)  If SEGID contains any nonprintable characters, the error
C         SPICE(NONPRINTABLECHARS) is signaled.
C
C     5)  If NINTVL is not at least 1, the error SPICE(INVALIDCOUNT)
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
C    10)  If any interpolation degree in the array DEGRES
C         is not at least 1 or is greater than MAXDEG, the
C         error SPICE(INVALIDDEGREE) is signaled.
C
C    11)  If the window size implied by any element of the array DEGRES
C         is odd, the error SPICE(BADWINDOWSIZE) is signaled.
C
C    12)  If the elements of the array EPOCHS corresponding to a given
C         mini-segment are not in strictly increasing order, the error
C         SPICE(TIMESOUTOFORDER) will be signaled.
C
C    13)  If the first epoch of a mini-segment exceeds the start
C         time of the associated interpolation interval, or if the
C         last epoch of the mini-segment precedes the end time of the
C         interpolation interval, the error SPICE(BOUNDSDISAGREE)
C         is signaled.
C
C    14)  Any error that occurs while writing the output segment will
C         be diagnosed by routines in the call tree of this routine.
C
C$ Files
C
C     A new type 19 SPK segment is written to the SPK file attached
C     to HANDLE.
C
C$ Particulars
C
C     This routine writes an SPK type 19 data segment to the open SPK
C     file according to the format described in the type 19 section of
C     the SPK Required Reading. The SPK file must have been opened with
C     write access.
C
C$ Examples
C
C     Suppose that you have states and are prepared to produce
C     a segment of type 19 in an SPK file.
C
C     The following code fragment could be used to add the new segment
C     to a previously opened SPK file attached to HANDLE. The file must
C     have been opened with write access.
C
C        C
C        C     Create a segment identifier.
C        C
C                  SEGID = 'MY_SAMPLE_SPK_TYPE_19_SEGMENT'
C
C        C
C        C     Write the segment.
C        C
C              CALL SPKW19 ( HANDLE,  BODY,    CENTER,  FRAME,    
C             .              FIRST,   LAST,    SEGID,   NINTVL,  
C             .              NPKTS,   SUBTPS,  DEGRES,  PACKTS,
C             .              EPOCHS,  IVLBDS,  SELLST           )
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
C-    SPICELIB Version 1.0.0, 05-FEB-2014 (NJB) (BVS)
C
C-&
 
C$ Index_Entries
C
C     write spk type_19 ephemeris data segment
C
C-&
 
 
 
C
C     SPICELIB functions
C
      INTEGER               LASTNB
      
      LOGICAL               FAILED
      LOGICAL               ODD
      LOGICAL               RETURN
 
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
      PARAMETER           ( DTYPE  =  19 )

      INTEGER               DIRSIZ
      PARAMETER           ( DIRSIZ = 100 )
        
C
C     Local variables
C
      DOUBLE PRECISION      DC     ( ND )
      DOUBLE PRECISION      DESCR  ( DSCSIZ )
 
 
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
      INTEGER               PKTSZS ( 0 : S19NST-1 )
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
      DATA                  PKTSZS  /  S19PS0, S19PS1 /
    
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SPKW19' )
         
C
C     Start with a parameter compatibility check.
C
      IF ( MAXREC .LT. MAXRSZ ) THEN

         CALL SETMSG ( 'SPK type 19 record size may be as '
     .   //            'large as #, but SPKPVN record size '
     .   //            'is #.'                              )
         CALL ERRINT ( '#',  MAXRSZ                         )
         CALL ERRINT ( '#',  MAXREC                         )
         CALL SIGERR ( 'SPICE(BUG0)'                        )
         CALL CHKOUT ( 'SPKW19'                             )
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
         CALL CHKOUT ( 'SPKW19'                                    )
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
         CALL CHKOUT ( 'SPKW19'                                  )
         RETURN
 
      END IF
 
C
C     Check to see if the segment identifier is too long.
C
      IF ( LASTNB(SEGID) .GT. SIDLEN ) THEN
 
         CALL SETMSG ( 'Segment identifier contains more than ' 
     .   //            '40 characters.'                        )
         CALL SIGERR ( 'SPICE(SEGIDTOOLONG)'                   )
         CALL CHKOUT ( 'SPKW19'                                )
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
            CALL CHKOUT ( 'SPKW19'                          )
            RETURN
 
         END IF
 
      END DO

C
C     The mini-segment/interval count must be positive.
C
      IF ( NINTVL .LT. 1 ) THEN
 
         CALL SETMSG ( 'Mini-segment/interval count was #; ' 
     .   //            'this count must be positive.'       )
         CALL ERRINT ( '#', NINTVL                          )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                )
         CALL CHKOUT ( 'SPKW19'                             )
         RETURN
 
      END IF

C
C     Make sure the interval bounds form a strictly 
C     increasing sequence. 
C
C     Note that there are NINTVL+1 bounds.
C
      DO I = 1, NINTVL

         IF ( IVLBDS(I) .GE. IVLBDS(I+1) ) THEN

            CALL SETMSG ( 'Interval bounds at indices # and # ' 
     .      //            'are # and # respectively. The '
     .      //            'difference is #. The bounds are '
     .      //            'required to be strictly increasing.' )
            CALL ERRINT ( '#', I                                )
            CALL ERRINT ( '#', I+1                              )
            CALL ERRDP  ( '#', IVLBDS(I)                        )
            CALL ERRDP  ( '#', IVLBDS(I+1)                      )
            CALL ERRDP  ( '#', IVLBDS(I+1) - IVLBDS(I)          )
            CALL SIGERR ( 'SPICE(BOUNDSOUTOFORDER)'             )
            CALL CHKOUT ( 'SPKW19'                              )
            RETURN

         END IF

      END DO

C
C     Make sure the time span of the descriptor doesn't extend
C     beyond the span of the interval bounds. 
C     
      IF (      ( FIRST .LT. IVLBDS(1       ) )
     .     .OR. ( LAST  .GT. IVLBDS(NINTVL+1) ) ) THEN

         CALL SETMSG ( 'First interval start time is #; '
     .   //            'segment start time is #; segment '
     .   //            'stop time is #; last interval stop '
     .   //            'time is #. This sequence of times '
     .   //            'is required to be non-decreasing: ' 
     .   //            'segment coverage must be contained '
     .   //            'within the union of the interpolation '
     .   //            'intervals.'                            )
         CALL ERRDP  ( '#', IVLBDS(1)                          )
         CALL ERRDP  ( '#', FIRST                              )
         CALL ERRDP  ( '#', LAST                               )
         CALL ERRDP  ( '#', IVLBDS(NINTVL+1)                   )
         CALL SIGERR ( 'SPICE(COVERAGEGAP)'                    )
         CALL CHKOUT ( 'SPKW19'                                )
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

      DO I = 1, NINTVL
C
C        First, just make sure the packet count for the current
C        mini-segment is at least two. This check reduces our chances
C        of a subscript range violation.
C
C        Check the number of packets.
C
         IF ( NPKTS(I) .LT. 2 ) THEN
 
            CALL SETMSG ( 'At least 2 packets are required '
     .      //            'for SPK type 19. Number of packets '
     .      //            'supplied was # in mini-segment at index #.' )
            CALL ERRINT ( '#', NPKTS(I)                                )
            CALL ERRINT ( '#', I                                       )
            CALL SIGERR ( 'SPICE(TOOFEWPACKETS)'                       )
            CALL CHKOUT ( 'SPKW19'                                     )
            RETURN
 
         END IF

C
C        Set the packet size, which is a function of the subtype. Also
C        set the window size. First check the subtype, which will be
C        used as an array index.
C
         SUBTYP = SUBTPS(I)

         IF (  ( SUBTYP .LT. 0 ) .OR. ( SUBTYP .GT. S19NST-1 ) ) THEN
         
            CALL SETMSG ( 'Unexpected SPK type 19 subtype # '
     .      //            'found in mini-segment #.'          )
            CALL ERRINT ( '#',  SUBTYP                        )
            CALL ERRINT ( '#',  I                             )
            CALL SIGERR ( 'SPICE(INVALIDSUBTYPE)'             )
            CALL CHKOUT ( 'SPKW19'                            )
            RETURN
      
         END IF

         PKTSIZ = PKTSZS ( SUBTYP )

         IF ( ODD(SUBTYP) ) THEN

            WINSIZ =   DEGRES(I) + 1
         ELSE
            WINSIZ = ( DEGRES(I) + 1 ) / 2
         END IF

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
            CALL CHKOUT ( 'SPKW19'                            )
            RETURN
 
         END IF
 
C
C        Make sure that the window size is even. 
C
         IF (   ODD( WINSIZ )   )  THEN
 
            CALL SETMSG ( 'The interpolating polynomials of '
     .      //            'mini-segment # have window size # '
     .      //            'and degree # for SPK type 19. '
     .      //            'The mini-segment subtype is #. '
     .      //            'The degree must be equivalent to 3 '       
     .      //            'mod 4 for subtype 0 (Hermite '
     .      //            'interpolation) and be odd for subtype '
     .      //            '1 (Lagrange interpolation).'          )
            CALL ERRINT ( '#', I                                 )
            CALL ERRINT ( '#', WINSIZ                            )
            CALL ERRINT ( '#', DEGRES(I)                         )
            CALL ERRINT ( '#', SUBTPS(I)                         )
            CALL SIGERR ( 'SPICE(BADWINDOWSIZE)'                 )
            CALL CHKOUT ( 'SPKW19'                               )
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

            IF ( EPOCHS(K) .GE. EPOCHS(K+1) ) THEN
 
               CALL SETMSG ( 'In mini-segment #, epoch # having '
     .         //            'index # in array EPOCHS and index # '
     .         //            'in the mini-segment is greater than '
     .         //            'or equal to its successor #.'         )
               CALL ERRINT ( '#',  I                                )
               CALL ERRDP  ( '#',  EPOCHS(K  )                      )
               CALL ERRINT ( '#',  K                                )
               CALL ERRINT ( '#',  J                                )
               CALL ERRDP  ( '#',  EPOCHS(K+1)                      )
               CALL SIGERR ( 'SPICE(TIMESOUTOFORDER)'               )
               CALL CHKOUT ( 'SPKW19'                               )
               RETURN

            END IF
 
         END DO

C
C        Make sure that the span of the input epochs of the Ith
C        mini-segment includes the Ith interpolation interval.
C
         IF (  EPOCHS(BEPIX) .GT. IVLBDS(I) ) THEN
 
            CALL SETMSG ( 'Interpolation interval # start time # '
     .      //            'precedes mini-segment''s first epoch #.' )
            CALL ERRINT ( '#',  I                                   )
            CALL ERRDP  ( '#',  IVLBDS(I)                           )
            CALL ERRDP  ( '#',  EPOCHS(BEPIX)                       )
            CALL SIGERR ( 'SPICE(BOUNDSDISAGREE)'                   )
            CALL CHKOUT ( 'SPKW19'                                  )
            RETURN
 
         ELSE IF (  EPOCHS(EEPIX) .LT. IVLBDS(I+1) ) THEN
 
            CALL SETMSG ( 'Interpolation interval # end time # '
     .      //            'exceeds mini-segment''s last epoch #.'   )
            CALL ERRINT ( '#',  I                                   )
            CALL ERRDP  ( '#',  IVLBDS(I+1)                         )
            CALL ERRDP  ( '#',  EPOCHS(EEPIX)                       )
            CALL SIGERR ( 'SPICE(BOUNDSDISAGREE)'                   )
            CALL CHKOUT ( 'SPKW19'                                  )
            RETURN

         END IF

      END DO

C     
C     If we made it this far, we're ready to start writing the segment.
C
C     The type 19 segment structure is eloquently described by this
C     diagram from the SPK Required Reading:
C
C        +--------------------------------+
C        | Interval 1 mini-segment        |
C        +--------------------------------+
C              .
C              .
C              .
C        +--------------------------------+
C        | Interval N mini-segment        |
C        +--------------------------------+
C        | Interval 1 start time          |
C        +--------------------------------+
C              .
C              .
C              .
C        +--------------------------------+
C        | Interval N start time          |
C        +--------------------------------+
C        | Interval N stop time           |
C        +--------------------------------+
C        | Interval start 100             | (First interval directory)
C        +--------------------------------+
C              .
C              .
C              .
C        +--------------------------------+
C        | Interval start (N/100)*100     | (Last interval directory)
C        +--------------------------------+
C        | Interval 1 start pointer       |
C        +--------------------------------+
C              .
C              .
C              .
C        +--------------------------------+
C        | Interval N start pointer       |
C        +--------------------------------+
C        | Interval N stop pointer + 1    |
C        +--------------------------------+
C        | Boundary choice flag           |
C        +--------------------------------+
C        | Number of intervals            |
C        +--------------------------------+
C
C
C     SPK type 19 mini-segments have the following structure:
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
C        | Subtype code          |
C        +-----------------------+
C        | Window size           |
C        +-----------------------+
C        | Number of packets     |
C        +-----------------------+
C
C
C     Create the segment descriptor. We don't use SPKPDS because
C     that routine doesn't allow creation of a singleton segment.
C
      IC(1) = BODY
      IC(2) = CENTER
      IC(3) = REFCOD
      IC(4) = DTYPE

      DC(1) = FIRST
      DC(2) = LAST

      CALL DAFPS  ( ND, NI, DC, IC, DESCR )
 
C
C     Begin a new segment.
C
      CALL DAFBNA ( HANDLE, DESCR, SEGID )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKW19' )
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
      DO I = 1, NINTVL
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
         PKTDSZ =  NPKTS(I) * PKTSIZ

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

         CALL DAFADA ( EPOCHS(BEPIX), NPKTS(I) )

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

            CALL DAFADA ( EPOCHS(K), 1 )

         END DO

C
C        Write the mini-segment's subtype, window size, and packet
C        count to the segment.
C
         CALL DAFADA ( DBLE( SUBTPS(I) ),  1 )
         CALL DAFADA ( DBLE( WINSIZ    ),  1 )
         CALL DAFADA ( DBLE( NPKTS(I)  ),  1 )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SPKW19' )
            RETURN
         END IF

      END DO
 
C
C     We've finished writing the mini-segments.
C
C     Next write the interpolation interval bounds.
C
      CALL DAFADA ( IVLBDS, NINTVL+1 )
 
C
C     Create and write directories for the interval
C     bounds.
C
C     The directory count is the interval bound count
C     (N+1), minus 1, divided by the directory size.
C
      NDIR = NINTVL / DIRSIZ

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

      DO I = 1, NINTVL
C
C        Set the packet size, which is a function of the subtype.
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
C                    +  3
C
C                 =     n_packets * ( packet_size + 1 )
C                    +  ( n_packets - 1 ) / DIRSIZ 
C                    +  3
C
         MINISZ =       NPKTS(I) * ( PKTSIZ + 1 )
     .              + ( NPKTS(I) - 1 ) / DIRSIZ
     .              +   3


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
C     Write the mini-segment/interpolation interval count.
C
      CALL DAFADA ( DBLE(NINTVL), 1 )

C
C     End the segment.
C
      CALL DAFENA
 
      CALL CHKOUT ( 'SPKW19' )
      RETURN
      END
