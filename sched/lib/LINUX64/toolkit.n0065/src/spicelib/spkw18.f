C$Procedure      SPKW18 ( Write SPK segment, type 18 )
 
      SUBROUTINE SPKW18 (  HANDLE,  SUBTYP,  BODY,    CENTER,  
     .                     FRAME,   FIRST,   LAST,    SEGID,   
     .                     DEGREE,  N,       PACKTS,  EPOCHS  )
 
C$ Abstract
C
C     Write a type 18 segment to an SPK file.
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

      INCLUDE 'spk18.inc'
      
      INTEGER               HANDLE
      INTEGER               SUBTYP
      INTEGER               BODY
      INTEGER               CENTER
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      CHARACTER*(*)         SEGID
      INTEGER               DEGREE
      INTEGER               N
      DOUBLE PRECISION      PACKTS ( * )
      DOUBLE PRECISION      EPOCHS ( * )
 
      INTEGER               MAXDEG
      PARAMETER           ( MAXDEG = 15 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of an SPK file open for writing.
C     SUBTYP     I   SPK type 18 subtype code.
C     BODY       I   NAIF code for an ephemeris object.
C     CENTER     I   NAIF code for center of motion of BODY.
C     FRAME      I   Reference frame name.
C     FIRST      I   Start time of interval covered by segment.
C     LAST       I   End time of interval covered by segment.
C     SEGID      I   Segment identifier.
C     DEGREE     I   Degree of interpolating polynomials.
C     N          I   Number of packets.
C     PACKTS     I   Array of packets.
C     EPOCHS     I   Array of epochs corresponding to packets.
C     MAXDEG     P   Maximum allowed degree of interpolating polynomial.
C
C$ Detailed_Input
C
C     HANDLE         is the file handle of an SPK file that has been
C                    opened for writing.
C
C     SUBTYP         is an integer code indicating the subtype of the
C                    the segment to be created.  
C
C     BODY           is the NAIF integer code for an ephemeris object
C                    whose state relative to another body is described
C                    by the segment to be created.
C
C     CENTER         is the NAIF integer code for the center of motion
C                    of the object identified by BODY.
C
C     FRAME          is the NAIF name for a reference frame
C                    relative to which the state information for BODY
C                    is specified.
C
C     FIRST,
C     LAST           are, respectively, the start and stop times of
C                    the time interval over which the segment defines
C                    the state of BODY.
C
C     SEGID          is the segment identifier.  An SPK segment
C                    identifier may contain up to 40 characters.
C
C     DEGREE         is the nominal degree of the polynomials used to
C                    interpolate the states contained in the input
C                    packets.  All components of the state vectors are 
C                    interpolated by polynomials of the specified
C                    degree, except near the segment boundaries, 
C                    or if the total number of states in the segment
C                    is too few to allow interpolation using the
C                    specified degree.
C
C                    If the actual interpolation degree is reduced,
C                    the highest degree feasible degree valid for 
C                    the interpolation type is used.
C
C     N              is the number of packets in the input packet
C                    array.
C
C     PACKTS         contains a time-ordered array of data packets
C                    representing geometric states of BODY relative 
C                    to CENTER, specified relative to FRAME.  The 
C                    packet structure depends on the segment subtype
C                    as follows:
C
C                       Type 0 (indicated by code S18TP0):
C
C                           x,  y,  z,  dx/dt,  dy/dt,  dz/dt,
C                           vx, vy, vz, dvx/dt, dvy/dt, dvz/dt
C                    
C                       where x, y, z represent Cartesian position
C                       components and  vx, vy, vz represent Cartesian
C                       velocity components.  Note well:  vx, vy, and
C                       vz *are not necessarily equal* to the time
C                       derivatives of x, y, and z.  This packet 
C                       structure mimics that of the Rosetta/MEX orbit
C                       file from which the data are taken.
C                      
C                       Type 1 (indicated by code S18TP1):
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
C     EPOCHS         is an array of epochs corresponding to the members
C                    of the packets array.  The epochs are specified as
C                    seconds past J2000, TDB.
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
C                    the SPICELIB routine SPKPV must be changed
C                    accordingly.  In particular, the size of the
C                    record passed to SPKRnn and SPKEnn must be
C                    increased, and comments describing the record size
C                    must be changed.
C
C$ Exceptions
C
C     If any of the following exceptions occur, this routine will return
C     without creating a new segment.
C
C     1) If FRAME is not a recognized name, the error
C        SPICE(INVALIDREFFRAME) is signaled.
C
C     2) If the last non-blank character of SEGID occurs past index 40,
C        the error SPICE(SEGIDTOOLONG) is signaled.
C
C     3) If SEGID contains any nonprintable characters, the error
C        SPICE(NONPRINTABLECHARS) is signaled.
C
C     4) If DEGREE is not at least 1 or is greater than MAXDEG, the
C        error SPICE(INVALIDDEGREE) is signaled.
C
C     5) If the window size implied by DEGREE is odd, the error 
C        SPICE(INVALIDDEGREE) is signaled.
C
C     6) If the number of packets N is not at least 2, 
C        the error SPICE(TOOFEWSTATES) will be signaled.
C
C     7) If FIRST is greater than or equal to LAST then the error
C        SPICE(BADDESCRTIMES) will be signaled.
C
C     8) If the elements of the array EPOCHS are not in strictly
C        increasing order, the error SPICE(TIMESOUTOFORDER) will be
C        signaled.
C
C     9) If the first epoch EPOCHS(1) is greater than FIRST, the error
C        SPICE(BADDESCRTIMES) will be signaled.
C
C    10) If the last epoch EPOCHS(N) is less than LAST, the error
C        SPICE(BADDESCRTIMES) will be signaled.
C
C    11) If the subtype code is not recognized, the error
C        SPICE(INVALIDVALUE) will be signaled.
C
C
C$ Files
C
C     A new type 18 SPK segment is written to the SPK file attached
C     to HANDLE.
C
C$ Particulars
C
C     This routine writes an SPK type 18 data segment to the open SPK
C     file according to the format described in the type 18 section of
C     the SPK Required Reading. The SPK file must have been opened with
C     write access.
C
C$ Examples
C
C     Suppose that you have states and are prepared to produce
C     a segment of type 18 in an SPK file.
C
C     The following code fragment could be used to add the new segment
C     to a previously opened SPK file attached to HANDLE. The file must
C     have been opened with write access.
C
C        C
C        C     Create a segment identifier.
C        C
C                  SEGID = 'MY_SAMPLE_SPK_TYPE_18_SEGMENT'
C
C        C
C        C     Write the segment.
C        C
C              CALL SPKW18 (  HANDLE,  BODY,    CENTER,  FRAME,
C             .               FIRST,   LAST,    SEGID,   DEGREE,
C             .               N,       STATES,  EPOCHS          )
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
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 21-DEC-2012 (NJB)
C
C        Increased the minimum packet count from 1 to 2.
C
C-    SPICELIB Version 1.0.1, 29-APR-2003 (NJB)
C
C        Description of error condition arising from invalid window
C        size was corrected.
C
C-    SPICELIB Version 1.0.0, 13-MAY-2002 (NJB)
C
C-&
 
C$ Index_Entries
C
C     write spk type_18 ephemeris data segment
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
      PARAMETER           ( DTYPE   =  18 )

      INTEGER               DIRSIZ
      PARAMETER           ( DIRSIZ  = 100 )
 
      INTEGER               STATSZ
      PARAMETER           ( STATSZ  =   6 )
       
C
C     Local variables
C
      DOUBLE PRECISION      DC     ( ND )
      DOUBLE PRECISION      DESCR  ( DSCSIZ )
      DOUBLE PRECISION      MAXTIM
 
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
         CALL CHKIN ( 'SPKW18' )
      END IF

C
C     Set the packet size, which is a function of the subtype.
C
      IF ( SUBTYP .EQ. S18TP0 ) THEN

         PACKSZ = S18PS0

      ELSE IF ( SUBTYP .EQ. S18TP1 ) THEN

         PACKSZ = S18PS1

      ELSE
         
         CALL SETMSG ( 'Unexpected SPK type 18 subtype requested: #' )
         CALL ERRINT ( '#',  SUBTYP                                  )
         CALL SIGERR ( 'SPICE(INVALIDVALUE)'                         )
         CALL CHKOUT ( 'SPKW18'                                      )
         RETURN
      
      END IF

      
C
C     Set the window size corresponding to the input degree.  This
C     size will be used in various places below.
C
      IF ( SUBTYP .EQ. S18TP0 ) THEN

         WINSIZ  =  ( DEGREE + 1 ) / 2
   
      ELSE IF ( SUBTYP .EQ. S18TP1 ) THEN

         WINSIZ  =  DEGREE + 1

      ELSE

         CALL SETMSG ( 'This point should not be reached. Getting ' //
     .                 'here may indicate that the code needs to '  //
     .                 'updated to handle new subtypes.'            )
         CALL SIGERR ( 'SPICE(BUG)'                                 )
         CALL CHKOUT ( 'SPKW18'                                     )
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
         CALL CHKOUT ( 'SPKW18'                                    )
         RETURN
 
      END IF
 
C
C     Check to see if the segment identifier is too long.
C
      IF ( LASTNB(SEGID) .GT. SIDLEN ) THEN
 
         CALL SETMSG ( 'Segment identifier contains more than ' //
     .                 '40 characters.'                          )
         CALL SIGERR ( 'SPICE(SEGIDTOOLONG)'                     )
         CALL CHKOUT ( 'SPKW18'                                  )
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
            CALL CHKOUT ( 'SPKW18'                                )
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
         CALL CHKOUT ( 'SPKW18'                                        )
         RETURN
 
      END IF
 
C
C     Make sure that the window size is even.  If not, the input
C     DEGREE is incompatible with the subtype.
C
      IF (  ODD(WINSIZ)  )  THEN
 
         CALL SETMSG ( 'The interpolating polynomials have degree #; '//
     .                 'for SPK type 18, the degree must be '         //
     .                 'equivalent to 3 mod 4 for Hermite '           //
     .                 'interpolation and odd for for Lagrange '      //
     .                 'interpolation.'                                )
         CALL ERRINT ( '#', DEGREE                                     )
         CALL SIGERR ( 'SPICE(INVALIDDEGREE)'                          )
         CALL CHKOUT ( 'SPKW18'                                        )
         RETURN
 
      END IF
 
C
C     Make sure that the number of packets is sufficient to define a
C     polynomial whose degree is DEGREE.
C
      IF ( N .LT. 2 ) THEN
 
         CALL SETMSG ( 'At least 2 packets are required for SPK type '//
     .                 '18.  Number of packets supplied:  #'           )
         CALL ERRINT ( '#', N                                          )
         CALL SIGERR ( 'SPICE(TOOFEWSTATES)'                           )
         CALL CHKOUT ( 'SPKW18'                                        )
         RETURN
 
      END IF
 
C
C     The segment stop time should be greater than or equal to
C     the begin time.
C
      IF ( FIRST .GT. LAST ) THEN
 
         CALL SETMSG ( 'The segment start time: # is greater then ' //
     .                 'the segment end time: #'                   )
         CALL ERRDP  ( '#', FIRST                                  )
         CALL ERRDP  ( '#', LAST                                   )
         CALL SIGERR ( 'SPICE(BADDESCRTIMES)'                      )
         CALL CHKOUT ( 'SPKW18'                                    )
         RETURN
 
      END IF
 
C
C     Make sure the epochs form a strictly increasing sequence.
C
      MAXTIM = EPOCHS(1)
 
      DO I = 2, N
 
         IF ( EPOCHS(I) .LE. MAXTIM ) THEN
 
            CALL SETMSG ( 'EPOCH # having index # is not greater '    //
     .                    'than its predecessor #.'                   )
            CALL ERRDP  ( '#',  EPOCHS(I)                             )
            CALL ERRINT ( '#',  I                                     )
            CALL ERRDP  ( '#',  EPOCHS(I-1)                           )
            CALL SIGERR ( 'SPICE(TIMESOUTOFORDER)'                    )
            CALL CHKOUT ( 'SPKW18'                                    )
            RETURN
         ELSE
            MAXTIM = EPOCHS(I)
         END IF
 
      END DO
 
C
C     Make sure that the span of the input epochs includes the interval
C     defined by the segment descriptor.
C
      IF (  EPOCHS(1) .GT. FIRST ) THEN
 
         CALL SETMSG ( 'Segment start time # precedes first epoch #.' )
         CALL ERRDP  ( '#',  FIRST                                    )
         CALL ERRDP  ( '#',  EPOCHS(1)                                )
         CALL SIGERR ( 'SPICE(BADDESCRTIMES)'                         )
         CALL CHKOUT ( 'SPKW18'                                       )
         RETURN
 
      ELSE IF (  EPOCHS(N) .LT. LAST ) THEN
 
         CALL SETMSG ( 'Segment end time # follows last epoch #.'     )
         CALL ERRDP  ( '#',  LAST                                     )
         CALL ERRDP  ( '#',  EPOCHS(N)                                )
         CALL SIGERR ( 'SPICE(BADDESCRTIMES)'                         )
         CALL CHKOUT ( 'SPKW18'                                       )
         RETURN
 
      END IF
 


C
C     If we made it this far, we're ready to start writing the segment.
C
C
C     Create the segment descriptor.  We don't use SPKPDS because
C     that routine doesn't allow creation of a singleton segment.
C
      IC(1) = BODY
      IC(2) = CENTER

      CALL NAMFRM ( FRAME, IC(3) )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKW18' )
         RETURN
      END IF

      IC(4) = DTYPE

      DC(1) = FIRST
      DC(2) = LAST

      CALL DAFPS  ( ND, NI, DC, IC, DESCR )
 
C
C     Begin a new segment.
C
      CALL DAFBNA ( HANDLE, DESCR, SEGID )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKW18' )
         RETURN
      END IF
      
      
C
C     The type 18 segment structure is eloquently described by this
C     diagram from the SPK Required Reading:
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
C        +-----------------------+
C        | Epoch N               |
C        +-----------------------+
C        | Epoch 100             | (First directory)
C        +-----------------------+
C                    .
C                    .
C                    .
C        +-----------------------+
C        | Epoch ((N-1)/100)*100 | (Last directory)
C        +-----------------------+
C        | Subtype code          |
C        +-----------------------+
C        | Window size           |
C        +-----------------------+
C        | Number of packets     |
C        +-----------------------+
C
C 
      CALL DAFADA ( PACKTS,  N*PACKSZ )
      CALL DAFADA ( EPOCHS,  N        )
 
      DO I = 1,   (N-1) / DIRSIZ
         CALL DAFADA (  EPOCHS( DIRSIZ*I ),  1  )
      END DO
 
      CALL DAFADA ( DBLE( SUBTYP ),  1 )
      CALL DAFADA ( DBLE( WINSIZ ),  1 )
      CALL DAFADA ( DBLE( N      ),  1 )
 
C
C     As long as nothing went wrong, end the segment.
C
      IF ( .NOT. FAILED() ) THEN
         CALL DAFENA
      END IF
 
      CALL CHKOUT ( 'SPKW18' )
      RETURN
      END
