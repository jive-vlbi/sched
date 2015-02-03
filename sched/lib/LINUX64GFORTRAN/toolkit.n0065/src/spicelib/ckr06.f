C$Procedure CKR06 ( C-kernel, read record from segment, type 6 )
 
      SUBROUTINE CKR06 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
     .                   RECORD, FOUND                       )
      IMPLICIT NONE
 
C$ Abstract
C
C     Read a single CK data record from a segment of type 6
C     (ESOC/DDID Piecewise Interpolation).
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

      INCLUDE 'ckparam.inc'
      INCLUDE 'ck06.inc'
 
      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR    ( 5 )
      DOUBLE PRECISION      SCLKDP
      DOUBLE PRECISION      TOL
      LOGICAL               NEEDAV
      DOUBLE PRECISION      RECORD   ( * )
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     DESCR      I   Segment descriptor.
C     SCLKDP     I   Pointing request time.
C     TOL        I   Lookup tolerance.
C     NEEDAV     I   Angular velocity flag.
C     RECORD     O   Data record.
C     FOUND      O   Flag indicating whether record was found.
C
C$ Detailed_Input
C
C     HANDLE,
C     DESCR       are the file handle and segment descriptor for a CK
C                 segment of type 6.
C
C     SCLKDP      is an encoded spacecraft clock time indicating the
C                 epoch for which pointing is desired.
C
C     TOL         is a time tolerance, measured in the same units as
C                 encoded spacecraft clock.
C
C                 When SCLKDP falls between the start time of one of
C                 the mini-segment intervals and the last time tag of
C                 that interval, the tolerance has no effect because
C                 pointing will be returned at the request time.
C
C                 However, if the request time falls within a coverage
C                 gap in one of the intervals, or outside of any
C                 interval, then the tolerance is used to determine if
C                 pointing should be returned at the closest epoch for
C                 which pointing is available. This epoch is either an
C                 interval's start time or the smaller of an interval's
C                 end time and its last time tag.
C
C
C     NEEDAV      is true if angular velocity is requested. If the
C                 input segment descriptor indicates angular velocity
C                 is absent, the error SPICE(NOAVDATA) is signaled.
C
C                 Note: although all subtypes of type 6 records either
C                 contain or compute angular velocity, a CK creator may
C                 choose to indicate that the provided angular velocity
C                 data are not valid; this can be done by setting the
C                 segment descriptor angular velocity flag to .FALSE.
C
C
C$ Detailed_Output
C
C     RECORD      is a set of data from the specified segment which,
C                 when evaluated at epoch SCLKDP, will give the 
C                 attitude and angular velocity of some body, relative
C                 to the reference frame indicated by DESCR.
C
C                 The structure of the record is as follows:
C
C                    +----------------------+
C                    | evaluation epoch     |
C                    +----------------------+
C                    | subtype code         |
C                    +----------------------+
C                    | number of packets (n)|
C                    +----------------------+
C                    | nominal SCLK rate    |
C                    +----------------------+
C                    | packet 1             |
C                    +----------------------+
C                    | packet 2             |
C                    +----------------------+
C                                .
C                                .
C                                .
C                    +----------------------+
C                    | packet n             |
C                    +----------------------+
C                    | epochs 1--n          |
C                    +----------------------+
C
C                 The packet size is a function of the subtype code.
C                 All packets in a record have the same size.
C
C
C     FOUND       is a logical flag indicating whether data were found.
C                 If NEEDAV is .FALSE., data will be found if the
C                 request time is within TOL ticks of a time for which
C                 the segment provides data. If NEEDAV is .TRUE., the
C                 segment's angular velocity flag must also be set in
C                 order for data to be found.
C                                               
C                 A type 6 segment provides data for times that are
C                 between its descriptor time bounds and that are
C                 within the coverage region of a mini-segment
C                 interval. The coverage region of a mini-segment
C                 interval extends from its start time to the lesser of
C                 its stop time and its last time tag.
C                 
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine roughly follows the pattern established in the
C     lower-numbered CK data type readers of not explicitly performing
C     error diagnoses. In particular, the C-kernel from which data are
C     read is assumed to be valid in most respects. The few exceptions
C     that are handled here are listed below.
C
C     1)  If the input HANDLE does not designate a loaded CK file, the
C         error will be diagnosed by routines called by this routine.
C
C     2)  If the segment specified by DESCR is not of data type 6, the
C         error SPICE(WRONGCKTYPE) is signaled.
C
C     3)  If the input SCLK value is not within TOL ticks of a time 
C         for which the segment provides data, FOUND is set to .FALSE.
C         and the output record is undefined.
C
C     4)  If the window size is non-positive or greater than the
C         maximum allowed value, the error SPICE(INVALIDVALUE) is
C         signaled.
C
C     5)  If the window size is not compatible with the segment
C         subtype, the error SPICE(INVALIDVALUE) is signaled.
C
C     6)  If the segment subtype is not recognized, the error
C         SPICE(INVALIDSUBTYPE) is signaled.
C
C     7)  If the tolerance is negative, the error SPICE(NEGATIVETOL) is
C         signaled.
C
C     8)  If an error occurs while trying to read data from a C-kernel,
C         the error is diagnosed by routines in the call tree of this 
C         routine.
C
C     9)  If the input segment descriptor indicates that angular
C         velocity data are not present, and if the input flag NEEDAV
C         is set to .TRUE., then the error SPICE(NOAVDATA) is signaled.
C
C$ Files
C
C     See argument HANDLE.
C
C$ Particulars
C
C     See the CK Required Reading file for a description of the
C     structure of a data type 6 segment.
C
C$ Examples
C
C     The data returned by the CKRnn routine is in its rawest form,
C     taken directly from the segment.  As such, it will be meaningless
C     to a user unless he/she understands the structure of the data type
C     completely.  Given that understanding, however, the CKRxx
C     routines might be used to "dump" and check segment data for a
C     particular epoch.
C
C
C     C
C     C     Get a segment applicable to a specified body and epoch.
C     C
C     C     CALL CKBSS ( INST,   SCLKDP, TOL,   NEEDAV )
C           CALL CKSNS ( HANDLE, DESCR,  SEGID, SFND   )
C
C           IF ( .NOT. SFND ) THEN
C              [Handle case of pointing not being found]
C           END IF
C
C     C
C     C     Look at parts of the descriptor.
C     C
C           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
C           CENTER = ICD( 2 )
C           REF    = ICD( 3 )
C           TYPE   = ICD( 4 )
C
C           IF ( TYPE .EQ. 6 ) THEN
C
C              CALL CKR06 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
C          .                RECORD, FOUND                       )
C
C              IF ( .NOT. FOUND ) THEN
C                 [Handle case of pointing not being found]
C              END IF
C
C              [Look at the RECORD data]
C                  .
C                  .
C                  .
C           END IF
C
C$ Restrictions
C
C     1)  Correctness of the C-kernel read by this routine is 
C         assumed.
C
C     2)  Correctness of inputs must be ensured by the caller of
C         this routine.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 14-MAR-2014 (NJB) (BVS)
C
C-&
 
C$ Index_Entries
C
C     read record from type_6 ck segment
C
C-&
 
C$ Revisions
C
C     None.
C
C-&

C
C     SPICELIB functions
C
      DOUBLE PRECISION      BRCKTD

      INTEGER               LSTLED
      INTEGER               LSTLTD
 
      LOGICAL               FAILED
      LOGICAL               ODD
      LOGICAL               RETURN
      LOGICAL               TOUCHL
 
C
C     Local parameters
C
      INTEGER               ND
      PARAMETER           ( ND     = 2 )
 
      INTEGER               NI
      PARAMETER           ( NI     = 6 )
  
      INTEGER               DIRSIZ
      PARAMETER           ( DIRSIZ = 100 )
 
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = DIRSIZ + 1 )

      INTEGER               CTRLSZ
      PARAMETER           ( CTRLSZ = 4 )

      INTEGER               NSGPAR
      PARAMETER           ( NSGPAR = 2 )
 

C
C     Maximum window sizes, based on subtypes:
C
      INTEGER               C06MW0
      PARAMETER           ( C06MW0 =  ( MAXDEG + 1 )/2  )

      INTEGER               C06MW1
      PARAMETER           ( C06MW1 =  ( MAXDEG + 1 )    )

      INTEGER               C06MW2
      PARAMETER           ( C06MW2 =  ( MAXDEG + 1 )/2  )

      INTEGER               C06MW3
      PARAMETER           ( C06MW3 =  ( MAXDEG + 1 )    )

C
C     Local variables
C
      DOUBLE PRECISION      BUFFER ( BUFSIZ )
      DOUBLE PRECISION      CONTRL ( CTRLSZ )
      DOUBLE PRECISION      DC     ( ND )
      DOUBLE PRECISION      GAP
      DOUBLE PRECISION      LSTEPC
      DOUBLE PRECISION      MINTIM ( 2 )
      DOUBLE PRECISION      RATE
      DOUBLE PRECISION      SVBTIM
      DOUBLE PRECISION      SVETIM
      DOUBLE PRECISION      SVRATE
      DOUBLE PRECISION      T
      
      INTEGER               BADDR
      INTEGER               BEGIDX
      INTEGER               BUFBAS
      INTEGER               DIRBAS
      INTEGER               EADDR
      INTEGER               ENDIDX
      INTEGER               FIRST
      INTEGER               GROUP
      INTEGER               HIGH
      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               ISEL
      INTEGER               IVBAS
      INTEGER               IVBIX
      INTEGER               IVEIX
      INTEGER               LAST
      INTEGER               LOW
      INTEGER               LSIZE
      INTEGER               MAXWND
      INTEGER               MINIB
      INTEGER               MINIE
      INTEGER               MINIIX
      INTEGER               MXWNSZ ( 0 : C06NST-1 )
      INTEGER               N
      INTEGER               NDIR
      INTEGER               NPKDIR
      INTEGER               NPKT
      INTEGER               NRCPKT
      INTEGER               NREAD
      INTEGER               PKDBAS
      INTEGER               PKTSIZ
      INTEGER               PKTSZS ( 0 : C06NST-1 )
      INTEGER               REMAIN
      INTEGER               RSIZE
      INTEGER               SUBTYP
      INTEGER               SVBEG
      INTEGER               SVHAN
      INTEGER               SVMIIX
      INTEGER               SVMINB
      INTEGER               SVN
      INTEGER               SVNPKT
      INTEGER               SVPKDB
      INTEGER               SVPKND
      INTEGER               SVPKSZ
      INTEGER               SVSTYP
      INTEGER               SVWNSZ
      INTEGER               TIMBAS
      INTEGER               TYPE
      INTEGER               WNDSIZ

      LOGICAL               AVFLAG
      LOGICAL               IVLSEL
      LOGICAL               LVAL
      LOGICAL               PASS1
      LOGICAL               PRVFND
      LOGICAL               SAMIVL
      LOGICAL               SAMSEG
      LOGICAL               SVFND
      LOGICAL               SVLAST

C
C     Saved variables
C
      SAVE                  MXWNSZ 
      SAVE                  PASS1
      SAVE                  PKTSZS
      SAVE                  SVBEG
      SAVE                  SVBTIM
      SAVE                  SVETIM
      SAVE                  SVFND
      SAVE                  SVHAN
      SAVE                  SVLAST
      SAVE                  SVMIIX
      SAVE                  SVMINB
      SAVE                  SVN
      SAVE                  SVNPKT
      SAVE                  SVPKDB
      SAVE                  SVPKND
      SAVE                  SVPKSZ
      SAVE                  SVRATE
      SAVE                  SVSTYP
      SAVE                  SVWNSZ

C
C     Initial values
C
      DATA                  MXWNSZ  /  C06MW0, C06MW1, C06MW2, C06MW3 /
      DATA                  PASS1   /  .TRUE.  /
      DATA                  PKTSZS  /  C06PS0, C06PS1, C06PS2, C06PS3 /
      DATA                  SVBEG   /  -1      /
      DATA                  SVBTIM  /   0.D0   /
      DATA                  SVETIM  /  -1.D0   /
      DATA                  SVFND   /  .FALSE. /
      DATA                  SVHAN   /   0      /
      DATA                  SVLAST  /  .FALSE. /
      DATA                  SVMIIX  /  -1      /
      DATA                  SVMINB  /  -1      /
      DATA                  SVN     /  -1      /
      DATA                  SVNPKT  /  -1      /
      DATA                  SVPKDB  /  -1      /
      DATA                  SVPKND  /  -1      /
      DATA                  SVPKSZ  /  -1      /
      DATA                  SVRATE  /  -1.D0   /
      DATA                  SVSTYP  /  -1      /
      DATA                  SVWNSZ  /  -1      /
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
      
      CALL CHKIN ( 'CKR06' )

C
C     Start with a parameter compatibility check on the first
C     pass.
C     
      IF ( PASS1 ) THEN

         IF ( CKMRSZ .LT. MAXRSZ ) THEN

            CALL SETMSG ( 'CK type 6 record size may be as '
     .      //            'large as #, but CKPFS record size '
     .      //            '(defined in ckparam.inc) is #.'     )
            CALL ERRINT ( '#',  MAXRSZ                         )
            CALL ERRINT ( '#',  CKMRSZ                         )
            CALL SIGERR ( 'SPICE(BUG)'                         )

         END IF
C
C        Indicate the first pass was completed.
C        
         PASS1 = .FALSE.

      END IF

C
C     No pointing found so far.
C
      FOUND  = .FALSE.

C
C     Let PRVFND indicate the last value of FOUND we returned. PRVFND
C     allows us to reset SVFND to .FALSE. at the start of this routine,
C     so we don't have to do this prior to every return (of which there
C     are more than 35).
C
      PRVFND = SVFND

C
C     Set the saved value of FOUND so as to reflect the value
C     of FOUND we'll return next.
C
      SVFND  = .FALSE.

C
C     "Touch" the input argument NEEDAV to suppress compiler warnings.
C
      LVAL = TOUCHL ( NEEDAV )
      LVAL = TOUCHL ( LVAL   )

C
C     Unpack the segment descriptor, and get the start and end addresses
C     of the segment.
C
      CALL DAFUS ( DESCR, 2, 6, DC, IC )
 
      TYPE   = IC( 3 )
      AVFLAG = IC( 4 ) .EQ. 1
      BADDR  = IC( 5 )
      EADDR  = IC( 6 )

C
C     Check whether angular velocity data are requested but not
C     available.
C
      IF (  NEEDAV  .AND. ( .NOT. AVFLAG ) ) THEN

         CALL SETMSG ( 'Segment descriptor indicates angular velocity '
     .   //            'data are not available, but such data were '
     .   //            'requested.'                                    )
         CALL SIGERR ( 'SPICE(NOAVDATA)'                               )
         CALL CHKOUT ( 'CKR06'                                         )
         RETURN         
         
      END IF

C
C     Check the tolerance value.
C
      IF ( TOL .LT. 0.D0 ) THEN

         CALL SETMSG( 'Tolerance must be non-negative but ' 
     .   //           'was actually *.'                      )
         CALL ERRDP ( '*',  TOL                              )
         CALL SIGERR( 'SPICE(NEGATIVETOL)'                   )
         CALL CHKOUT( 'CKR06'                                )
         RETURN         

      END IF
 
C
C     Check the request time and tolerance against the bounds in 
C     the segment descriptor.
C
      IF (       ( (SCLKDP + TOL) .LT. DC(1) )  
     .     .OR.  ( (SCLKDP - TOL) .GT. DC(2) )  )  THEN
C
C        The request time is too far outside the segment's coverage
C        interval for any pointing to satisfy the request.
C
         CALL CHKOUT ( 'CKR06' )
         RETURN
 
      END IF

C
C     Set the request time to use for searching.
C
      T = BRCKTD ( SCLKDP, DC(1), DC(2) )

C
C     From this point onward, we assume the segment was constructed
C     correctly.  
C
C
C     Terminology: below, the phrase "base address of 'X'" refers to
C     the DAF address immediately preceding X. Base addresses simplify
C     mapping DAF array (here "array" means an array stored in
C     consecutive DAF addresses, not "segment") indices to DAF
C     addresses, since the DAF address of the Ith array element is
C     obtained by adding I to the DAF array's base address.
C     
C     Key variables: 
C
C        Name      Meaning
C        ----      -------
C        BADDR     Segment begin DAF address.
C
C        DIRBAS    Base address of mini-segment interval directory.
C
C        EADDR     Segment end DAF address.
C
C        FIRST     Index (mini-segment-relative) of first time tag in
C                  sequence transferred to the output record.
C
C        HIGH      Index (mini-segment-relative) of time tag following
C                  the tag at index LOW (see description below).
C
C        IVBIX     Index in the mini-segment interval bounds array of
C                  the start time of the applicable interval.
C
C        IVLBAS    Base address of mini-segment interval time bounds.
C
C        IVLSEL    Interval selection flag: this routine selects the
C                  last applicable interval if true; otherwise it
C                  selects the first applicable interval.
C
C        LAST      Index (mini-segment-relative) of last time tag in
C                  sequence transferred to the output record.
C
C        LOW       Index (mini-segment-relative) of last time tag less
C                  than the request time, or of the first time tag if
C                  this tag equals the request time.
C
C        MINIB,
C        MINIE     Mini-segment begin and end DAF addresses. These
C                  addresses are absolute, not segment-relative.
C
C        MINIIX    Mini-segment/mini-segment interval index.
C
C        N         Count of mini-segments.
C
C        NDIR      Number of mini-segment interval time bounds
C                  directories.
C
C        NPKDIR    Number of packet directory entries for current
C                  mini-segment.
C
C        NPKT      Packet count for current mini-segment.
C
C        NRCPKT    Output record packet count. Note that this count,
C                  due to reduction of order at mini-segment
C                  boundaries, may be smaller than the window size
C                  stored in the current mini-segment.
C
C        PKDBAS    Base address of packet directory for current
C                  mini-segment.
C
C        PKTSIZ    Size of packets of current mini-segment.
C
C        SUBTYP    Subtype of current mini-segment.
C
C        TIMBAS    Base address of time tags of current mini-segment.
C
C        WNDSIZ    Interpolation window size of current mini-segment.
C
C
C     Re-used variables: the variables shown in the list below
C     are used as short-duration variables, much like loop index 
C     variables; they are re-used as needed.
C
C        BUFBAS
C        BUFFER
C        GROUP
C        NREAD
C        REMAIN
C
C
C     Decide whether we're looking at the same segment we saw on the
C     previous call, and whether the mini-segment interval used on
C     that call is still applicable.
C
C     Re-use of data from a previous call requires that the saved
C     data were set on a successful call. Note that PRVFND can not
C     be true on the first pass.
C
      SAMSEG  =       ( HANDLE .EQ.  SVHAN ) 
     .          .AND. ( BADDR  .EQ.  SVBEG )
     .          .AND.   PRVFND             
            
C
C     Give SAMIVL an initial value. If we do have the
C     same interval, update SAMIVL to indicate this.
C
      SAMIVL = .FALSE.

      IF ( SAMSEG ) THEN
C
C        We now assume that all data saved from the last
C        read of this segment are valid.
C        
         IF ( SVLAST ) THEN
C
C           We pick the last interval containing T. For all intervals
C           but the last, T must be less than the interval end time.
C           For the last interval, T may equal the end time.
C
C           Note that we don't bother to test for the special case
C           where the interval is not the last, there's a gap at the
C           end of the interval and T equals the last epoch of the
C           interval. In this rare case, we do not reuse the old
C           interval data, even though it would be possible to 
C           add code to do so.
C
            IF ( SVMIIX .LT. SVN ) THEN

               SAMIVL =       ( T .GE. SVBTIM )
     .                  .AND. ( T .LT. SVETIM )
            ELSE

               SAMIVL =         T .GE. SVBTIM
     .                  .AND. ( T .LE. SVETIM )
            END IF

         ELSE
C
C           We pick the first interval containing T. For all intervals
C           but the first, T must be greater than the interval start
C           time. For the first interval, T may equal the start time.
C
            IF ( SVMIIX .GT. 1 ) THEN

               SAMIVL =       ( T  .GT. SVBTIM )
     .                  .AND. ( T  .LE. SVETIM )

            ELSE

               SAMIVL =       ( T  .GE. SVBTIM )
     .                  .AND. ( T  .LE. SVETIM )
            END IF

         END IF

      END IF


      IF ( SAMSEG .AND. SAMIVL ) THEN
C
C        We're looking at the same segment as last time, and the
C        mini-segment interval we looked up last time is applicable
C        for the input time T. 
C
C        Simply restore the segment and interval parameters we
C        saved from the previous lookup.
C
C        We don't need to restore the segment start DAF address
C        BADDR, since we've already extracted it from DESCR.
C
C        Restore
C
C           - The mini-segment's packet directory count
C           - The mini-segment's packet directory base address
C
         NPKDIR = SVPKND
         PKDBAS = SVPKDB

C
C        Restore
C 
C           - The mini-segment/interval count
C           - The mini-segment/interval index
C           - The mini-segment/interval start pointer
C        
         N      = SVN
         MINIIX = SVMIIX
         MINIB  = SVMINB
       
C
C        Restore
C 
C           - The mini-segment subtype
C           - The mini-segment packet size
C           - The mini-segment packet count
C           - The mini-segment interpolation window size
C           - The mini-segment clock rate
C
         SUBTYP = SVSTYP
         PKTSIZ = SVPKSZ
         NPKT   = SVNPKT
         WNDSIZ = SVWNSZ
         RATE   = SVRATE

      ELSE
C
C        The segment and interval information for the current segment
C        must be looked up.
C
C        Perform checks on this segment.
C
C        Make sure that this really is a type 06 data segment.
C
         IF ( TYPE .NE. 6 ) THEN

            CALL SETMSG( 'You are attempting to locate type * ' 
     .      //           'data in a type 6 data segment.'       )
            CALL ERRINT( '*',  TYPE                             )
            CALL SIGERR( 'SPICE(WRONGCKTYPE)'                   )
            CALL CHKOUT( 'CKR06'                                )
            RETURN

         END IF

C
C        Locate the mini-segment interval that contains the request
C        time. If the request time lies a common boundary of two
C        intervals, the choice of interval is determined by the
C        interval selection flag.
C
C        Before getting started, we need to determine which interval to
C        use if the request time lies on a boundary between two
C        intervals. The segment's interval selection flag tells us how
C        to resolve this.
C
         CALL DAFGDA ( HANDLE, EADDR-1, EADDR, CONTRL )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'CKR06' )
            RETURN
         END IF

         ISEL   = NINT ( CONTRL(1) )
         N      = NINT ( CONTRL(2) )

         IVLSEL = ISEL .EQ. ITRUE

C
C        Determine the number of interval directory entries in the
C        segment. Note that for most CK types, this computation is
C        performed by computing
C
C           ( N - 1 ) / DIRSIZ
C
C        where N is the segment's epoch count. 
C
C        However the set of items in this case is a sequence
C        of N start times followed by a final stop time, so
C        the epoch count is
C     
C           N + 1
C
C        and the numerator in the ratio above is incremented by 1.
C
         NDIR = N / DIRSIZ

C
C        Note that the directory placement scheme always leaves
C        a non-empty group of epochs following the last directory
C        entry. 
C
C        Let DIRBAS be the base address of the interval directory.
C        We'll compute DIRBAS whether or not the interval directory
C        is non-empty.
C
C        If the interval directory is non-empty, it spans the address
C        range
C
C           DIRBAS+1 : DIRBAS+NDIR
C
C        We compute DIRBAS by starting at the end of the segment
C        and skipping over the control area, the mini-segment
C        start/stop pointers, and the interval directory itself.
C
         DIRBAS = EADDR - NSGPAR - ( N + 1 ) - NDIR

C
C        The way we search the directory depends on the treatment
C        of request times that lie on interval boundaries.
C
         IF ( IVLSEL ) THEN
C           
C           We must pick the latest interval containing the request
C           time.
C
C           The stop time of the interval we seek is the first interval
C           boundary strictly greater than T, unless T is the stop time
C           of the final interval.
C
C           We want to find the group of interval boundaries containing
C           the stop time of the interval containing T. There are
C           NDIR+1 such groups; all but the last have a directory entry
C           that coincides with the final epoch of the group. We'll use
C           the variable GROUP as the group index.
C
C           If there is an interval directory, search it to determine
C           the group of interval times to search next.
C
            IF ( NDIR .EQ. 0 ) THEN
C
C              There's no question about which group of epochs to
C              search.
C       
               GROUP = 1

            ELSE
C
C              The index of the group we seek is the index of the first
C              directory entry that is greater than T, if such an entry
C              exists. If there's no such entry, the group we seek is
C              the final one.
C
C              Find the last directory entry less than or equal to
C              the request time. The directory entry after that one,
C              if such exists, is the one to pick.
C           
               NREAD  = MIN ( NDIR, BUFSIZ )
               BUFBAS = DIRBAS
C
C              Fetch the current batch of directory entries.
C
               CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, BUFFER )

               IF ( FAILED() ) THEN
                  CALL CHKOUT( 'CKR06' )
                  RETURN
               END IF

               REMAIN = NDIR - NREAD
C
C              The variable NREAD always contains a positive value at
C              this point, so we can use it as an array index.
C
               DO WHILE (       ( REMAIN        .GT. 0 ) 
     .                    .AND. ( BUFFER(NREAD) .LE. T )  )

                  BUFBAS = BUFBAS + NREAD
                  NREAD  = MIN ( REMAIN, BUFSIZ )
C
C                 Fetch the current batch of directory entries.
C
                  CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, BUFFER )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT( 'CKR06' )
                     RETURN
                  END IF

                  REMAIN = REMAIN - NREAD
                              
               END DO            
C
C              Count the directory entries that are less than or equal
C              to T. The number we skipped over before the final loop
C              iteration is BUFBAS-DIRBAS; the number of buffered
C              entries we're skipping is the number of entries that are
C              less than or equal to T. The index of the group of
C              epochs containing T exceeds the skipped directory count
C              by 1.
C
               GROUP =    BUFBAS - DIRBAS
     .                 +  LSTLED ( T, NREAD, BUFFER )
     .                 +  1            
C
C              GROUP is in the range 1 : NDIR+1.
C
            END IF

C
C           Let IVBAS be the base address of the sequence of interval
C           time bounds.
C
            IVBAS  = DIRBAS - ( N + 1 )
C
C           Now find the index of the last interval boundary less than
C           or equal to T. We'll need to read the current group of
C           epochs first, so compute the base of the range of addresses
C           containing this group.

            BUFBAS = IVBAS + ( ( GROUP - 1 ) * DIRSIZ )
C
C           Compute the number of epochs to read. Note that all groups
C           of epochs except the last have DIRSIZ elements.
C 
            REMAIN = ( N + 1 ) - ( ( GROUP - 1 ) * DIRSIZ )
C
C           Note that REMAIN is always non-zero, since there's always
C           at least one epoch that exceeds the last directory entry.
C
            NREAD  = MIN ( DIRSIZ, REMAIN )

            CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, BUFFER )

            IF ( FAILED() ) THEN
               CALL CHKOUT( 'CKR06' )
               RETURN
            END IF
            
C
C           Find the index of the first epoch greater than T; this is
C           the number of epochs that are less than or equal to T, plus
C           1. The case where T matches the final epoch must be handled
C           here, since in this case no epoch exceeds T.
C
            IVEIX =    BUFBAS - IVBAS
     .              +  LSTLED ( T, NREAD, BUFFER )
     .              +  1

            IVEIX = MIN ( IVEIX,  N+1 )

C
C           Backstop test:
C
            IF ( IVEIX .LT. 2 ) THEN

               CALL SETMSG ( 'IVEIX = #.' )
               CALL ERRINT ( '#', IVEIX   )
               CALL SIGERR ( 'SPICE(BUG)' )
               CALL CHKOUT ( 'CKR06'      )
               RETURN

            END IF

C
C           The epoch at index IVEIX is the end time of the
C           mini-segment interval we'll use. The index of
C           the interval itself is IVEIX - 1. 
C
            MINIIX = IVEIX - 1


         ELSE
C
C           IVLSEL is .FALSE., meaning we must pick the first interval
C           containing the request time.

C           The start time of the interval we seek is the last interval
C           boundary strictly less than T, unless T is the start time
C           of the first interval. The stop time of this interval is 
C           the first boundary greater than or equal to T.
C
C           We want to find the group of interval boundaries containing
C           the stop time of the interval containing T. There are
C           NDIR+1 such groups; all but the last have a directory entry
C           that coincides with the final epoch of the group. We'll use
C           the variable GROUP as the group index.
C
C           If there is an interval directory, search it to determine
C           the group of interval times to search next.
C
            IF ( NDIR .EQ. 0 ) THEN
C
C              There's no question about which group of epochs to
C              search.
C   
               GROUP = 1

            ELSE
C
C              Find the last directory entry strictly less than the
C              request time. The directory entry after that one, if
C              such exists, is the one to pick.
C           
               NREAD  = MIN ( NDIR, BUFSIZ )
               BUFBAS = DIRBAS
               REMAIN = NDIR - NREAD
C
C              Fetch the current batch of directory entries.
C
               CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, BUFFER )

               IF ( FAILED() ) THEN
                  CALL CHKOUT( 'CKR06' )
                  RETURN
               END IF
C
C              The variable NREAD always contains a positive value at
C              this point, so we can use it as an array index.
C
               DO WHILE (      ( REMAIN        .GT. 0 ) 
     .                   .AND. ( BUFFER(NREAD) .LT. T )  )

                  BUFBAS = BUFBAS + NREAD
                  NREAD  = MIN ( REMAIN, BUFSIZ )
C
C                 Fetch the current batch of directory entries.
C
                  CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, BUFFER )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT( 'CKR06' )
                     RETURN
                  END IF

                  REMAIN = REMAIN - NREAD

               END DO
C
C              Count the directory entries that are less than T. The
C              number we skipped over before the final loop iteration
C              is BUFBAS-DIRBAS; the number of buffered entries we're
C              skipping is the number of entries that are less than T.
C              The index of the group of epochs containing T exceeds
C              the skipped directory count by 1.
C
               GROUP =    BUFBAS - DIRBAS
     .                 +  LSTLTD ( T, NREAD, BUFFER )
     .                 +  1
C
C              GROUP is in the range 1 : NDIR+1.
C               
            END IF

C
C           Let IVBAS be the base address of the sequence of interval
C           time bounds.
C
            IVBAS  = DIRBAS - ( N + 1 )
          
C
C           Now find the index of the last interval boundary epoch less
C           than T. We'll need to read the current group of epochs
C           first, so compute the base of the range of addresses
C           containing this group.
C
            BUFBAS = IVBAS + ( ( GROUP - 1 ) * DIRSIZ )

C
C           Compute the number of epochs to read. Note that all groups
C           of epochs except the last have DIRSIZ elements.
C 
            REMAIN = ( N + 1 ) - ( ( GROUP - 1 ) * DIRSIZ )
C
C           Note that REMAIN is always non-zero, since there's always
C           at least one epoch that exceeds the last directory entry.
C
            NREAD  = MIN ( DIRSIZ, REMAIN )

            CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, BUFFER )

            IF ( FAILED() ) THEN
               CALL CHKOUT( 'CKR06' )
               RETURN
            END IF
            
C
C           Find the index of the last interval boundary less than T.
C           The case where T matches the first boundary must be handled
C           here, since in this case no boundary precedes T.
C
            IVBIX =    BUFBAS - IVBAS
     .              +  LSTLTD ( T, NREAD, BUFFER )

            IVBIX = MAX ( IVBIX,  1 )

C
C           Backstop test:
C
            IF ( IVBIX .GT. N ) THEN

               CALL SETMSG ( 'IVBIX = #.' )
               CALL ERRINT ( '#', IVBIX   )
               CALL SIGERR ( 'SPICE(BUG)' )
               CALL CHKOUT ( 'CKR06'      )
               RETURN

            END IF

C
C           The epoch at index IVBIX is the begin time of the
C           mini-segment interval we'll use. 
C
            MINIIX = IVBIX

         END IF
C
C        This is the end of the IF block that handles mini-segment
C        selection for the two possible values of IVLSEL.
C
C        If the mini-segment we just found has a gap, and if TOL is
C        positive, it's possible that the mini-segment we want actually
C        is the successor of the one at index MINIIX. We'll check this
C        by finding the last epoch of the mini-segment we just
C        identified.
C
C        Look up the begin and end pointers of the mini-segment at index
C        MINIIX. For the first N-1 mini-segments, the "end pointer"
C        of one mini-segment is the "begin" pointer of the next. 
C
         BUFBAS = EADDR - NSGPAR - ( N + 1 ) + ( MINIIX - 1 ) 

         CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+2, BUFFER )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'CKR06' )
            RETURN
         END IF

         MINIB = NINT ( BUFFER(1) ) + BADDR - 1
C
C        Note that the end of the current mini-segment
C        precedes the start of the next mini-segment by
C        one address.
C
         MINIE = NINT ( BUFFER(2) ) + BADDR - 2

C
C        Look up the control area of the mini-segment.
C
         CALL DAFGDA ( HANDLE, MINIE-CTRLSZ+1, MINIE, CONTRL )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'CKR06' )
            RETURN
         END IF

C
C        Fetch the control area parameters for the mini-segment.
C
         RATE    =         CONTRL(1)
         SUBTYP  =  NINT ( CONTRL(2) )
         WNDSIZ  =  NINT ( CONTRL(3) )
         NPKT    =  NINT ( CONTRL(4) )

C
C        Compute the directory count for the mini-segment.
C
         NPKDIR  =  ( NPKT - 1 ) / DIRSIZ 

C
C        The last epoch of the mini-segment precedes the epoch
C        directories and the control area. Look up this epoch.
C
         BUFBAS  =  MINIE - CTRLSZ - NPKDIR - 1

         CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+1, LSTEPC )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'CKR06' )
            RETURN
         END IF

C
C        Determine whether the request time is in a gap.
C
         IF ( T .GT. LSTEPC ) THEN
C
C           Yep, T lies in a gap. But we may still be able to 
C           find data for this request, if the lookup tolerance
C           is positive.
C
            IF ( TOL .EQ. 0.D0 ) THEN
C
C              We're out of luck. We can't find pointing for this
C              request. FOUND is already .FALSE., so just return.
C
               CALL CHKOUT ( 'CKR06' )
               RETURN
               
            ELSE
C
C              Determine the distance of T from the nearest epochs.
C
C              Look up the time bounds of the mini-segment at index
C              MINIIX.
C
               CALL DAFGDA ( HANDLE, IVBAS+MINIIX, 
     .                               IVBAS+MINIIX+1, MINTIM )
      
               IF ( FAILED() ) THEN               
                  CALL CHKOUT ( 'CKR06' )
                  RETURN
               END IF

C
C              See whether T is close enough to a stored epoch for
C              us to find pointing. If not, return now.
C
               IF (      (  ( T         - LSTEPC )  .GT.  TOL  )
     .             .AND. (  ( MINTIM(2) - T      )  .GT.  TOL  )  ) THEN
C
C                 We can't find pointing for T. FOUND is already
C                 .FALSE., so just return.
C
                  CALL CHKOUT ( 'CKR06' )
                  RETURN

               END IF

C
C              Continue to look for pointing usable for time T.
C
               IF ( MINIIX .EQ. N ) THEN
C
C                 We're looking at the final mini-segment. If
C                 T is close enough to LSTEPC, we can find 
C                 pointing.
C
                  IF (  ( T - LSTEPC )  .LE.  TOL  ) THEN
C
C                    We're going to carry on using the current
C                    mini-segment. We'll update T to be the last epoch
C                    of this mini-segment.
C
                     T = LSTEPC

                  ELSE
C
C                    T is too far from LSTEPC. We're done. FOUND is
C                    already .FALSE., so just return.
C
                     CALL CHKOUT ( 'CKR06' )
                     RETURN

                  END IF

               ELSE
C
C                 There's a successor to the current interval. Determine
C                 which interval contains an epoch closest to T.
C
C                 Compute the size of the gap at the right end of the
C                 interior of the current interval.
C
                  GAP  =  MINTIM(2) - LSTEPC

                  IF (  ( T - LSTEPC )  .LE.  ( GAP/2 )  ) THEN
C
C                    T is closer to LSTEPC than the start time of the
C                    next interval. We're going to carry on using the
C                    current mini-segment. We'll update T to be the
C                    last epoch of this mini-segment.
C
                     T = LSTEPC

                  ELSE
C
C                    T is closer to the start time of the next interval
C                    than to LSTEPC. than the start time of the next
C                    interval. We're going to use the next
C                    mini-segment. 
C
                     MINIIX  =  MINIIX + 1

C
C                    Update the mini-segment parameters we already
C                    found, since these have been superseded.
C
C                    The mini-segment pointers:
C
                     BUFBAS = EADDR - NSGPAR - (N + 1) + ( MINIIX - 1 )

                     CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+2, BUFFER )

                     IF ( FAILED() ) THEN
                        CALL CHKOUT ( 'CKR06' )
                        RETURN
                     END IF

                     MINIB = NINT ( BUFFER(1) ) + BADDR - 1
C
C                    Note that the end of the current mini-segment
C                    precedes the start of the next mini-segment by one
C                    address.
C
                     MINIE = NINT ( BUFFER(2) ) + BADDR - 2

C
C                    Look up the control area of the mini-segment.
C
                     CALL DAFGDA ( HANDLE, MINIE-CTRLSZ+1, 
     .                                     MINIE,          CONTRL )

                     IF ( FAILED() ) THEN
                        CALL CHKOUT ( 'CKR06' )
                        RETURN
                     END IF

C
C                    Fetch the control area parameters for the
C                    mini-segment.
C
                     RATE    =  NINT ( CONTRL(1) )
                     SUBTYP  =  NINT ( CONTRL(2) )
                     WNDSIZ  =  NINT ( CONTRL(3) )
                     NPKT    =  NINT ( CONTRL(4) )
C
C                    Since we have new mini-segment parameters, we need
C                    to check them. We'll defer these checks until
C                    later, so we can perform one set of checks,
C                    regardless of which logic path we followed to
C                    select a mini-segment.
C
C                    Compute the directory count for the mini-segment.
C
                     NPKDIR  =  ( NPKT - 1 ) / DIRSIZ 
C
C                    We're going to set T to the start time of the
C                    current mini-segment interval, which is the stop
C                    time of the previous one.
C
                     T = MINTIM(2)
C
C                    We still need to look up the last epoch of the
C                    current mini-segment. We'll use this when we save
C                    the time bounds of the mini-segment.
C
                     BUFBAS = MINIE - CTRLSZ - NPKDIR - 1

                     CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+1, LSTEPC )

                     IF ( FAILED() ) THEN               
                        CALL CHKOUT ( 'CKR06' )
                        RETURN
                     END IF

                  END IF
 
               END IF
C
C              At this point T is set. If we had to update the
C              mini-segment index and its parameters, we did so.
C
            END IF
C
C           We've handled the case where T lies in a gap and the
C           tolerance is non-zero.
          
         END IF
C
C        This is the end of the block that handles the case where T lies
C        in a gap. At this point, the following items are set:
C
C           T
C           MINIIX
C           MINIB
C           MINIE
C           SUBTYP
C           WNDSIZ
C           NPKT
C           NPKDIR
C           RATE
C
C        Look up the time bounds of the mini-segment at index MINIIX.
C        These bounds are used quite a bit farther on, when we save
C        them for future use.
C
         CALL DAFGDA ( HANDLE, IVBAS+MINIIX, IVBAS+MINIIX+1, MINTIM )
      
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'CKR06' )
            RETURN
         END IF

C
C        From this point onward, we'll work with the mini-segment
C        that occupies the address range MINIB : MINIE.
C
C        Set the packet size, which is a function of the subtype.
C        Also set the maximum window size. First check the subtype,
C        which will be used as an array index.
C
         IF (  ( SUBTYP .LT. 0 ) .OR. ( SUBTYP .GE. C06NST ) ) THEN
            
            CALL SETMSG ( 'Unexpected CK type 6 subtype # found in ' 
     .      //            'type 06 segment within mini-segment #.'     )
            CALL ERRINT ( '#',  SUBTYP                                 )
            CALL ERRINT ( '#',  MINIIX                                 )
            CALL SIGERR ( 'SPICE(INVALIDSUBTYPE)'                      )
            CALL CHKOUT ( 'CKR06'                                      )
            RETURN

         END IF

         PKTSIZ = PKTSZS ( SUBTYP )
         MAXWND = MXWNSZ ( SUBTYP )

C
C        Check the window size.
C
         IF ( ( WNDSIZ .LT. 2 ) .OR. ( WNDSIZ .GT. MAXWND ) ) THEN

            CALL SETMSG ( 'Window size in type 6 segment was #; must ' 
     .      //            'be in the range 2:# for subtype #. '
     .      //            'Mini-segment index is #.'                   )
            CALL ERRINT ( '#',  WNDSIZ                                 )
            CALL ERRINT ( '#',  MAXWND                                 )
            CALL ERRINT ( '#',  SUBTYP                                 )
            CALL ERRINT ( '#',  MINIIX                                 )
            CALL SIGERR ( 'SPICE(INVALIDVALUE)'                        )
            CALL CHKOUT ( 'CKR06'                                      )
            RETURN

         END IF

         IF ( ODD(WNDSIZ) ) THEN

            CALL SETMSG ( 'Window size in type 06 segment was #; ' 
     .      //            'must be even for subtype #. ' 
     .      //            'Mini-segment index is #.'               )
            CALL ERRINT ( '#',  WNDSIZ                             )
            CALL ERRINT ( '#',  SUBTYP                             )
            CALL ERRINT ( '#',  MINIIX                             )
            CALL SIGERR ( 'SPICE(INVALIDVALUE)'                    )
            CALL CHKOUT ( 'CKR06'                                  )
            RETURN

         END IF
 
C
C        Compute the base address of the sequence of packet
C        directory entries for the current mini-segment/interval.
C        
         PKDBAS  =  MINIB - 1 + ( NPKT * ( PKTSIZ + 1 ) )
 
C
C        The test below is done for safety. No SPICE errors
C        should ever be detected at this point.
C        
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'CKR06' )
            RETURN
         END IF
      
C
C        If we made it this far, we did so without a SPICE error. We
C        have valid segment parameters which can be saved for the next
C        call.
C
C        Save 
C
C           - The DAF handle
C           - The segment begin DAF address
C           - The segment's "select last/first interval" flag
C
         SVHAN  = HANDLE
         SVBEG  = BADDR
         SVLAST = IVLSEL

C
C        Save the time bounds of the applicable mini-segment/interval.
C        We don't want to indicate data availability within a gap, since
C        the re-use logic assumes data availability.
C
         SVBTIM =       MINTIM(1)
         SVETIM = MIN ( MINTIM(2), LSTEPC )

C
C        Save
C
C           - The mini-segment/interval directory count
C           - The mini-segment/interval directory base address
C
         SVPKND = NPKDIR
         SVPKDB = PKDBAS

C
C        Save
C 
C           - The mini-segment/interval count
C           - The mini-segment/interval index
C           - The mini-segment/interval start pointer
C        
         SVN    = N
         SVMIIX = MINIIX
         SVMINB = MINIB
         
C
C        Save
C 
C           - The mini-segment subtype
C           - The mini-segment packet size
C           - The mini-segment packet count
C           - The mini-segment window size
C           - The mini-segment clock rate
C
         SVSTYP = SUBTYP
         SVPKSZ = PKTSIZ
         SVNPKT = NPKT
         SVWNSZ = WNDSIZ
         SVRATE = RATE

      END IF

C
C     We're ready to construct the output record. The first step is to
C     identify the indices of the packets and epochs corresponding to
C     the request.
C
C     We'll now select the set of packets that define the interpolating
C     polynomials.   We'll start out by finding the first directory
C     entry that is greater than or equal to the request epoch.  We'll
C     use the variable GROUP to indicate the set of epochs to search
C     within, once we've found the right directory entry.
C
      IF ( NPKDIR .EQ. 0 ) THEN
C
C        There's no mystery about which group of epochs to search.
C
         GROUP  = 1
 
      ELSE
C
C        There's at least one directory entry. Find the first directory
C        entry whose time is greater than or equal to the request time,
C        if there is such an entry.  We'll search linearly through the
C        directory entries, reading up to DIRSIZ of them at a time.
C        Having found the correct set of directory entries, we'll
C        perform a binary search within that set for the desired entry.
C
         BUFBAS  =  PKDBAS
         NREAD   =  MIN ( NPKDIR, DIRSIZ )
         REMAIN  =  NPKDIR - NREAD
 
         CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, BUFFER )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'CKR06' )
            RETURN
         END IF
 
         DO WHILE (       ( BUFFER(NREAD) .LT. T )
     .              .AND. ( REMAIN        .GT. 0 )  )
 
            BUFBAS  =  BUFBAS + NREAD
            NREAD   =  MIN ( REMAIN, DIRSIZ )
            REMAIN  =  REMAIN - NREAD
C
C           Note:  NREAD is always > 0 here.
C
            CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, BUFFER )
 
            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'CKR06' )
               RETURN
            END IF

         END DO
 
C
C        At this point, BUFBAS - PKDBAS is the number of directory
C        entries preceding the one contained in BUFFER(1).
C
         GROUP  =     ( BUFBAS - PKDBAS )
     .              +   LSTLTD ( T, NREAD, BUFFER )
     .              +   1
      END IF
 
C
C     GROUP now indicates the set of epochs in which to search for the
C     request epoch.  If GROUP is 1, the request time lies within the
C     inclusive time interval bounded by the first and last epochs of
C     the first group.  Otherwise, the request time lies in the time
C     interval bounded by the last element of the preceding group and
C     the last element of the current group.
C
C     We'll use the variable names BEGIDX and ENDIDX to refer to
C     the indices, relative to the set of time tags, of the first
C     and last time tags in the set we're going to look up.
C
      IF ( GROUP .EQ. 1 ) THEN
 
         BEGIDX  =  1
         ENDIDX  =  MIN ( NPKT, DIRSIZ )
 
      ELSE
C
C        If the group index is greater than 1, we'll include the last
C        time tag of the previous group in the set of time tags we look
C        up.  That way, the request time is bracketed by the time tag
C        set we look up.
C
         BEGIDX  =      (  GROUP  - 1 ) * DIRSIZ
         ENDIDX  =  MIN (  BEGIDX + DIRSIZ,  NPKT  )
 
      END IF
 
 
      TIMBAS  =  PKDBAS - NPKT

      CALL DAFGDA ( HANDLE, TIMBAS+BEGIDX, TIMBAS+ENDIDX, BUFFER )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'CKR06' )
         RETURN
      END IF
 
C
C     Find two adjacent epochs bounding the request epoch.  The request
C     time cannot be greater than all of epochs in the group, and it
C     cannot precede the first element of the group.
C
      I  =  LSTLTD (  T,  ENDIDX-BEGIDX+1,  BUFFER  )
 
C
C     The variables LOW and high are the indices of a pair of time
C     tags that bracket the request time.
C
      IF ( I .EQ. 0 ) THEN
         LOW  =  1
      ELSE
         LOW  =  BEGIDX + I - 1
      END IF
 
      HIGH  =  LOW  +  1
 
C
C     Now select the set of packets used for interpolation. Note
C     that the window size is known to be even.  
C
C     For CK type 6 we allow the window size to shrink when the window
C     must be truncated due to proximity to an interval boundary.
C
C     The nominal bracketing epochs we've found are the (WNDSIZ/2)nd
C     and (WNDSIZ/2 + 1)st of the interpolating set.  If the
C     request time is too close to one end of the coverage interval,
C     we reduce the window size, after which one endpoint of the 
C     window will coincide with an endpoint of the coverage interval.
C
C     Let LSIZE be the size of the "left half" of the window: the size
C     set of window epochs to the left of the request time. We want
C     this size to be WNDSIZ/2, but if not enough packets are
C     available, the set ranges from index 1 to index LOW.
C

      LSIZE =  MIN ( WNDSIZ/2, LOW ) 

C
C     RSIZE is defined analogously for the right half of the window.
C
      RSIZE =  MIN (  WNDSIZ/2,  ( NPKT - HIGH + 1 )  )

C
C     The actual window size is simply the sum of LSIZE and RSIZE.
C
      NRCPKT =  LSIZE + RSIZE

C
C     FIRST and LAST are the endpoints of the range of indices of
C     time tags (and packets) we'll collect in the output record.
C
      FIRST =  LOW    -  LSIZE   +  1

      LAST  =  FIRST  +  NRCPKT  -  1

C
C     We're ready to construct the output record.
C
C     Put the subtype and window size into the output record.
C     The fourth element is the nominal SCLK rate.
C
      RECORD(1) =  T
      RECORD(2) =  SUBTYP
      RECORD(3) =  NRCPKT
      RECORD(4) =  RATE

C
C     Read the packets.
C
      CALL DAFGDA ( HANDLE,
     .              MINIB + (FIRST-1)*PKTSIZ,
     .              MINIB +      LAST*PKTSIZ  -  1,
     .              RECORD( CTRLSZ + 1 )           )
C
C     Finally, add the epochs to the output record.
C     Read the sequence of time tags.
C
      BUFBAS  =  ( MINIB - 1 )  +  ( NPKT * PKTSIZ )  +  ( FIRST - 1 )

      CALL DAFGDA ( HANDLE, 
     .              BUFBAS+1, 
     .              BUFBAS+NRCPKT, 
     .              RECORD( CTRLSZ + NRCPKT*PKTSIZ + 1 )  )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'CKR06' )
         RETURN
      END IF
C
C     Indicate pointing was found.
C
      FOUND = .TRUE.
      SVFND = .TRUE.

      CALL CHKOUT ( 'CKR06' )
      RETURN
      END









