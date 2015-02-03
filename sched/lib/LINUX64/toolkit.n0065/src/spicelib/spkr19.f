C$Procedure  SPKR19 ( SPK, read record from segment, type 19 )
 
      SUBROUTINE SPKR19 ( HANDLE, DESCR, ET, RECORD )
      IMPLICIT NONE
 
C$ Abstract
C
C     Read a single SPK data record from a segment of type 19
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
C     SPK
C
C$ Keywords
C
C     EPHEMERIS
C
C$ Declarations

      INCLUDE 'spk19.inc'
      INCLUDE 'spkrec.inc'

      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR    ( 5 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RECORD   ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     DESCR      I   Segment descriptor.
C     ET         I   Target epoch.
C     RECORD     O   Data record.
C
C$ Detailed_Input
C
C     HANDLE,
C     DESCR       are the file handle and segment descriptor for an SPK
C                 segment of type 19. The SPK file designated by HANDLE
C                 must be open for read access.
C
C     ET          is an epoch for which a data record from a specific
C                 segment is required. ET is expressed as seconds past
C                 J2000 TDB.
C
C$ Detailed_Output
C
C     RECORD      is an array of data from the specified segment which,
C                 when evaluated at epoch ET, will give the state
C                 (position and velocity) of the target body identified
C                 by the input segment descriptor. The descriptor
C                 specifies the center of motion and reference frame of
C                 the state.
C
C                 The structure of the record is as follows:
C
C                    +----------------------+
C                    | subtype code         |
C                    +----------------------+
C                    | number of packets (n)|
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
C                 The packet size is a function of the type 19 subtype.
C                 All packets in a record have the same size.
C
C$ Parameters
C
C     See the Fortran INCLUDE file spk19.inc.
C
C$ Exceptions
C
C     1) If the input HANDLE does not designate a loaded SPK file, the
C        error will be diagnosed by routines called by this routine.
C
C     2) If the segment specified by DESCR is not of data type 19,
C        the error 'SPICE(WRONGSPKTYPE)' is signaled.
C
C     3) If the input ET value is not within the range specified
C        in the segment descriptor, the error SPICE(TIMEOUTOFBOUNDS)
C        is signaled.
C
C     4) If the window size is non-positive or greater than the 
C        maximum allowed value, the error SPICE(INVALIDVALUE) is
C        signaled.
C
C     5) If the window size is not compatible with the segment
C        subtype, the error SPICE(INVALIDVALUE) is signaled.
C
C     6) If the segment subtype is not recognized, the error 
C        SPICE(NOTSUPPORTED) is signaled.
C
C$ Files
C
C     See argument HANDLE.
C
C$ Particulars
C
C     SPICE user applications normally will have no need to call this
C     routine directly. For further information, see the headers of the
C     SPICE SPK APIs
C
C        SPKEZR
C        SPKPOS
C
C     the SPK Required Reading file spk.req, and the SPICE SPK
C     tutorial.
C
C     See the SPK Required Reading file for a description of the
C     structure of a data type 19 segment.
C
C$ Examples
C
C     The data returned by the SPKRnn routine is in its rawest form,
C     taken directly from the segment.  As such, it will be meaningless
C     to a user unless he/she understands the structure of the data type
C     completely.  Given that understanding, however, the SPKRxx
C     routines might be used to "dump" and check segment data for a
C     particular epoch.
C
C
C     C
C     C     Get a segment applicable to a specified body and epoch.
C     C
C           CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
C
C     C
C     C     Look at parts of the descriptor.
C     C
C           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
C           CENTER = ICD( 2 )
C           REF    = ICD( 3 )
C           TYPE   = ICD( 4 )
C
C           IF ( TYPE .EQ. 19 ) THEN
C              CALL SPKR19 ( HANDLE, DESCR, ET, RECORD )
C                  .
C                  .  Look at the RECORD data.
C                  .
C           END IF
C
C$ Restrictions
C
C     1)  Correctness of inputs must be ensured by the caller of
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
C     read record from type_19 spk segment
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
      INTEGER               LSTLED
      INTEGER               LSTLTD
 
      LOGICAL               ODD
      LOGICAL               FAILED
      LOGICAL               RETURN
 
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
      PARAMETER           ( CTRLSZ = 3 )

C
C     Maximum window sizes, based on subtypes:
C
      INTEGER               S19MW0
      PARAMETER           ( S19MW0 =  ( MAXDEG + 1 )/2  )

      INTEGER               S19MW1
      PARAMETER           ( S19MW1 =  ( MAXDEG + 1 )    )

C
C     Local variables
C
      DOUBLE PRECISION      BUFFER ( BUFSIZ )
      DOUBLE PRECISION      CONTRL ( CTRLSZ )
      DOUBLE PRECISION      DC     ( ND )
      DOUBLE PRECISION      MINTIM ( ND )
      DOUBLE PRECISION      SVBTIM
      DOUBLE PRECISION      SVETIM
      
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
      INTEGER               MXWNSZ ( 0 : S19NST-1 )
      INTEGER               N
      INTEGER               NDIR
      INTEGER               NPKDIR
      INTEGER               NPKT
      INTEGER               NRCPKT
      INTEGER               NREAD
      INTEGER               PKDBAS
      INTEGER               PKTSIZ
      INTEGER               PKTSZS ( 0 : S19NST-1 )
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

      LOGICAL               IVLSEL
      LOGICAL               PASS1
      LOGICAL               PRVOK
      LOGICAL               SAMIVL
      LOGICAL               SAMSEG
      LOGICAL               SVLAST
      LOGICAL               SVOK

C
C     Saved variables
C
      SAVE                  MXWNSZ
      SAVE                  PASS1
      SAVE                  PKTSZS
      SAVE                  SVBEG
      SAVE                  SVBTIM
      SAVE                  SVETIM
      SAVE                  SVHAN
      SAVE                  SVLAST
      SAVE                  SVMIIX
      SAVE                  SVMINB
      SAVE                  SVN
      SAVE                  SVNPKT
      SAVE                  SVOK
      SAVE                  SVPKDB
      SAVE                  SVPKND
      SAVE                  SVPKSZ
      SAVE                  SVSTYP
      SAVE                  SVWNSZ


C
C     Initial values
C
      DATA                  MXWNSZ  /  S19MW0, S19MW1 /      
      DATA                  PASS1   /  .TRUE.  /
      DATA                  PKTSZS  /  S19PS0, S19PS1 /
      DATA                  SVBEG   /  -1      /
      DATA                  SVBTIM  /   0.D0   /
      DATA                  SVETIM  /  -1.D0   /
      DATA                  SVHAN   /   0      /
      DATA                  SVLAST  /  .FALSE. /
      DATA                  SVMIIX  /  -1      /
      DATA                  SVMINB  /  -1      /
      DATA                  SVN     /  -1      /
      DATA                  SVNPKT  /  -1      /
      DATA                  SVOK    /  .FALSE. /
      DATA                  SVPKDB  /  -1      /
      DATA                  SVPKND  /  -1      /
      DATA                  SVPKSZ  /  -1      /
      DATA                  SVSTYP  /  -1      /
      DATA                  SVWNSZ  /  -1      /



      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SPKR19' )     

C
C     Before any error checks are done, copy the status from
C     the previous call. Set the saved status variable to
C     .FALSE. here so it will be .FALSE. on exit unless this
C     call is successful.
C
      PRVOK = SVOK
      SVOK  = .FALSE.

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
C        DIRBAS    Base address of interpolation interval directory.
C
C        EADDR     Segment end DAF address.
C
C        FIRST     Index (mini-segment-relative) of first time tag in
C                  sequence transferred to to output record.
C
C        HIGH      Index (mini-segment-relative) of time tag following
C                  the tag at index LOW (see description below).
C
C        IVBIX     Index in the interpolation interval bounds array of
C                  the start time of the applicable interval.
C
C        IVLBAS    Base address of interpolation interval time bounds.
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
C        MINIIX    Interpolation interval/mini-segment index.
C
C        N         Count of interpolation intervals/mini-segments.
C
C        NDIR      Number of interpolation interval time bounds
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
C     Start with a parameter compatibility check on the first
C     pass.
C     
      IF ( PASS1 ) THEN

         IF ( MAXREC .LT. MAXRSZ ) THEN

            CALL SETMSG ( 'SPK type 19 record size may be as '
     .      //            'large as #, but SPKPVN record size '
     .      //            '(defined in spkrec.inc) is #.'      )
            CALL ERRINT ( '#',  MAXRSZ                         )
            CALL ERRINT ( '#',  MAXREC                         )
            CALL SIGERR ( 'SPICE(BUG)'                         )

         END IF

C
C        Indicate the first pass was completed.
C        
         PASS1 = .FALSE.

      END IF

C
C     Unpack the segment descriptor, and get the start and end
C     addresses of the segment.
C
      CALL DAFUS ( DESCR, 2, 6, DC, IC )
 
      TYPE  = IC( 4 )
      BADDR = IC( 5 )
      EADDR = IC( 6 )

 
C
C     Check the request time against the bounds in the segment
C     descriptor.
C
      IF (  ( ET .LT. DC(1) )  .OR.  ( ET .GT. DC(2) )  )  THEN

         CALL SETMSG( 'Request time # is outside of descriptor '   
     .   //           'bounds # : #.'                           )
         CALL ERRDP ( '#',  ET                                  )
         CALL ERRDP ( '#',  DC(1)                               )
         CALL ERRDP ( '#',  DC(2)                               )
         CALL SIGERR( 'SPICE(TIMEOUTOFBOUNDS)'                  )
         CALL CHKOUT( 'SPKR19'                                  )
         RETURN
 
      END IF

C
C     Decide whether we're looking at the same segment we saw on the
C     previous call, and whether the interpolation interval used on
C     that call is still applicable.
C
C     Re-use of data from a previous call requires that the saved
C     data were set on a successful call.
C
      SAMSEG  =       ( HANDLE .EQ.  SVHAN ) 
     .          .AND. ( BADDR  .EQ.  SVBEG )
     .          .AND.                PRVOK

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
C           We pick the last interval containing ET. For
C           all intervals but the last, ET must be
C           less than the interval end time.
C
            IF ( SVMIIX .LT. SVN ) THEN

               SAMIVL =       ( ET .GE. SVBTIM )
     .                  .AND. ( ET .LT. SVETIM )
            ELSE

               SAMIVL =         ET .GE. SVBTIM
     .                  .AND. ( ET .LE. SVETIM )

            END IF

         ELSE
C
C           We pick the first interval containing ET. For
C           all intervals but the first, ET must be
C           greater than the interval start time.
C
            IF ( SVMIIX .GT. 1 ) THEN

               SAMIVL =       ( ET  .GT. SVBTIM )
     .                  .AND. ( ET  .LE. SVETIM )

            ELSE

               SAMIVL =         ET  .GE. SVBTIM 
     .                  .AND. ( ET  .LE. SVETIM )

            END IF

         END IF

      END IF


      IF ( SAMSEG .AND. SAMIVL ) THEN
C
C        We're looking at the same segment as last time, and the
C        interpolation interval we looked up last time is applicable
C        for the input time ET. 
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
C           - The mini-segment window size
C
         SUBTYP = SVSTYP
         PKTSIZ = SVPKSZ
         NPKT   = SVNPKT
         WNDSIZ = SVWNSZ

      ELSE
C
C        The segment and interval information for the current segment
C        must be looked up.
C
C        Perform checks on this segment.
C
C        Make sure that this really is a type 19 data segment.
C
         IF ( TYPE .NE. 19 ) THEN

            CALL SETMSG( 'You are attempting to locate type * ' 
     .      //           'data in a type 19 data segment.'      )
            CALL ERRINT( '*',  TYPE                             )
            CALL SIGERR( 'SPICE(WRONGSPKTYPE)'                  )
            CALL CHKOUT( 'SPKR19'                               )
            RETURN
         END IF

C
C        Locate the interpolation interval that contains the request
C        time.
C
C        Before getting started, we need to determine which interval to
C        use if the request time lies on a boundary between two
C        intervals. The segment's interval selection flag tells us how
C        to resolve this.
C
         CALL DAFGDA ( HANDLE, EADDR-1, EADDR, CONTRL )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SPKR19' )
            RETURN
         END IF

         ISEL   = NINT ( CONTRL(1) )
         N      = NINT ( CONTRL(2) )

         IVLSEL = ISEL .EQ. ITRUE

C
C        Determine the number of interval directory entries in the
C        segment. Note that for most SPK types, this computation is
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
C        a non-empty set of epochs following the last directory
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
         DIRBAS = EADDR - 2 - ( N + 1 ) - NDIR

C
C        The way we search the directory depends on the treatment
C        of request times that lie on interval boundaries.
C
         IF ( IVLSEL ) THEN
C
C           If there is an interval directory, search it to determine
C           the group of interval times to search next.
C
            IF ( NDIR .GT. 0 ) THEN
C
C              Find the last directory entry *less than or equal to*
C              the request time. The directory entry *after* that one,
C              if such exists, is the one to pick.
C           
               NREAD  = MIN ( NDIR, BUFSIZ )
               BUFBAS = DIRBAS

C
C              Fetch the current batch of directory entries.
C
               CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, BUFFER )

               IF ( FAILED() ) THEN
                  CALL CHKOUT( 'SPKR19' )
                  RETURN
               END IF

               REMAIN = NDIR - NREAD

C
C              The variable NREAD always contains a positive value at
C              this point, so we can use it as an array index.
C
               DO WHILE (       ( REMAIN        .GT. 0  ) 
     .                    .AND. ( BUFFER(NREAD) .LE. ET )  )

                  BUFBAS = BUFBAS + NREAD
                  NREAD  = MIN ( REMAIN, BUFSIZ )
C
C                 Fetch the current batch of directory entries.
C
                  CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, BUFFER )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT( 'SPKR19' )
                     RETURN
                  END IF

                  REMAIN = REMAIN - NREAD
                              
               END DO
            
C
C              Count the directory entries that are less than or equal
C              to ET. The number we skipped over before the final loop
C              iteration is BUFBAS-DIRBAS. The index of the group of
C              epochs containing ET exceeds the skipped directory count
C              by 1.
C
               GROUP =    BUFBAS - DIRBAS
     .                 +  LSTLED ( ET, NREAD, BUFFER )
     .                 +  1
            
            ELSE
C
C              There's no question about which group of epochs to
C              search.
C       
               GROUP = 1

            END IF

C
C           Let IVBAS be the base address of the sequence of interval
C           time bounds.
C
            IVBAS  = DIRBAS - ( N + 1 )

C
C           Now find the index of the last interval boundary less than
C           or equal to ET. We'll need to read the current group of
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
               CALL CHKOUT( 'SPKR19' )
               RETURN
            END IF
            
C
C           Find the index of the first epoch greater than ET. The case
C           where ET matches the final epoch must be handled here,
C           since in this case no epoch exceeds ET.
C
            IVEIX =    BUFBAS - IVBAS
     .              +  LSTLED ( ET, NREAD, BUFFER )
     .              +  1

            IVEIX = MIN ( IVEIX,  N+1 )

C
C           Backstop test:
C
            IF ( IVEIX .LT. 2 ) THEN

               CALL SETMSG ( 'IVEIX = #.' )
               CALL ERRINT ( '#', IVEIX   )
               CALL SIGERR ( 'SPICE(BUG)' )
               CALL CHKOUT ( 'SPKR19'     )
               RETURN

            END IF

C
C           The epoch at index IVEIX is the end time of the
C           interpolation interval we'll use. The index of
C           the interval itself is IVEIX - 1. 
C
            MINIIX = IVEIX - 1


         ELSE
C
C           IVLSEL is .FALSE., meaning we must pick the first interval
C           containing the request time.
C
C           If there is an interval directory, search it to determine
C           the group of interval times to search next.
C
            IF ( NDIR .GT. 0 ) THEN
C
C              Find the last directory entry *less than* the request
C              time. The directory entry *after* that one, if such
C              exists, is the one to pick.
C           
               NREAD  = MIN ( NDIR, BUFSIZ )
               BUFBAS = DIRBAS
               REMAIN = NDIR - NREAD

C
C              Fetch the current batch of directory entries.
C
               CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, BUFFER )

               IF ( FAILED() ) THEN
                  CALL CHKOUT( 'SPKR19' )
                  RETURN
               END IF

C
C              The variable NREAD always contains a positive value at
C              this point, so we can use it as an array index.
C
                DO WHILE (      ( REMAIN        .GT. 0  ) 
     .                    .AND. ( BUFFER(NREAD) .LT. ET )  )

                  BUFBAS = BUFBAS + NREAD
                  NREAD  = MIN ( REMAIN, BUFSIZ )
C
C                 Fetch the current batch of directory entries.
C
                  CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, BUFFER )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT( 'SPKR19' )
                     RETURN
                  END IF

                  REMAIN = REMAIN - NREAD

               END DO

C
C              Count the directory entries that are less than ET. The
C              number we skipped over before the final loop iteration
C              is BUFBAS-DIRBAS. The index of the group of epochs
C              containing ET exceeds the skipped directory count by 1.
C          
               GROUP =    BUFBAS - DIRBAS
     .                 +  LSTLTD ( ET, NREAD, BUFFER )
     .                 +  1
 
            ELSE
C
C              There's no question about which group of epochs to
C              search.
C   
               GROUP = 1

            END IF

C
C           Let IVBAS be the base address of the sequence of interval
C           time bounds.
C
            IVBAS  = DIRBAS - ( N + 1 )
          
C
C           Now find the index of the last interval epoch less than ET.
C           We'll need to read the current group of epochs first, so
C           compute the base of the range of addresses containing this
C           group.

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
               CALL CHKOUT( 'SPKR19' )
               RETURN
            END IF
            
C
C           Find the index of the last epoch less than ET. The case
C           where ET matches the first epoch must be handled here,
C           since in this case no epoch precedes ET.
C
            IVBIX =    BUFBAS - IVBAS
     .              +  LSTLTD ( ET, NREAD, BUFFER )

            IVBIX = MAX ( IVBIX,  1 )

C
C           Backstop test:
C
            IF ( IVBIX .GT. N ) THEN

               CALL SETMSG ( 'IVBIX = #.' )
               CALL ERRINT ( '#', IVBIX   )
               CALL SIGERR ( 'SPICE(BUG)' )
               CALL CHKOUT ( 'SPKR19'     )
               RETURN

            END IF

C
C           The epoch at index IVBIX is the begin time of the
C           interpolation interval we'll use. The index of the interval
C           itself is also IVBIX.
C
            MINIIX = IVBIX

         END IF
C
C        This is the end of the IF block that handles mini-segment
C        selection for the two possible values of IVLSEL.
C
C        Look up the begin and end pointers of the mini-segment at index
C        MINIIX. For the first N-1 mini-segments, the "end pointer"
C        of one mini-segment is the "begin" pointer of the next. 
C
         BUFBAS = EADDR - 2 - ( N + 1 ) + ( MINIIX - 1 ) 

         CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+2, BUFFER )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SPKR19' )
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
C        Look up the time bounds of the mini-segment at index MINIIX.
C        These bounds are used quite a bit farther on, when we save
C        them for future use.
C
         CALL DAFGDA ( HANDLE, IVBAS+MINIIX, IVBAS+MINIIX+1, MINTIM )
      
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SPKR19' )
            RETURN
         END IF

C
C        From this point onward, we'll work with the mini-segment
C        that occupies the address range MINIB : MINIE.
C
C        Look up the control area of the mini-segment.
C
         CALL DAFGDA ( HANDLE, MINIE-CTRLSZ+1, MINIE, CONTRL )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SPKR19' )
            RETURN
         END IF

C
C        Fetch the control area parameters for the mini-segment.
C
         SUBTYP  =  NINT ( CONTRL(1) )
         WNDSIZ  =  NINT ( CONTRL(2) )
         NPKT    =  NINT ( CONTRL(3) )

         IF (  ( SUBTYP .LT. 0 ) .OR. ( SUBTYP .GE. S19NST ) ) THEN
            
            CALL SETMSG ( 'Unexpected SPK type 19 subtype # found in ' 
     .      //            'type 19 segment within mini-segment #.'     )
            CALL ERRINT ( '#',  SUBTYP                                 )
            CALL ERRINT ( '#',  MINIIX                                 )
            CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                        )
            CALL CHKOUT ( 'SPKR19'                                     )
            RETURN

         END IF

         PKTSIZ = PKTSZS ( SUBTYP )
         MAXWND = MXWNSZ ( SUBTYP )

C
C        Check the window size.
C
         IF ( ( WNDSIZ .LT. 2 ) .OR. ( WNDSIZ .GT. MAXWND ) ) THEN

            CALL SETMSG ( 'Window size in type 19 segment was #; must ' 
     .      //            'be in the range 2:# for subtype #. '
     .      //            'Mini-segment index is #.'                   )
            CALL ERRINT ( '#',  WNDSIZ                                 )
            CALL ERRINT ( '#',  MAXWND                                 )
            CALL ERRINT ( '#',  SUBTYP                                 )
            CALL ERRINT ( '#',  MINIIX                                 )
            CALL SIGERR ( 'SPICE(INVALIDVALUE)'                        )
            CALL CHKOUT ( 'SPKR19'                                     )
            RETURN

         END IF

         IF ( ODD(WNDSIZ) ) THEN

            CALL SETMSG ( 'Window size in type 19 segment was #; ' 
     .      //            'must be even for subtype #. ' 
     .      //            'Mini-segment index is #.'               )
            CALL ERRINT ( '#',  WNDSIZ                             )
            CALL ERRINT ( '#',  SUBTYP                             )
            CALL ERRINT ( '#',  MINIIX                             )
            CALL SIGERR ( 'SPICE(INVALIDVALUE)'                    )
            CALL CHKOUT ( 'SPKR19'                                 )
            RETURN

         END IF

C
C        Compute the number of packet directory entries for
C        the current mini-segment/interval.
C
         NPKDIR  =  (NPKT-1) / DIRSIZ
C
C        Compute the base address of the sequence of packet
C        directory entries for the current mini-segment/interval.
C        
         PKDBAS  =  MINIB - 1 + ( NPKT * ( PKTSIZ + 1 ) )
 
C
C        The test below is done for safety. No SPICE error s
C        should ever be detected at this point.
C        
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SPKR19' )
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
C
         SVBTIM = MINTIM(1)
         SVETIM = MINTIM(2)

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
C
         SVSTYP = SUBTYP
         SVPKSZ = PKTSIZ
         SVNPKT = NPKT
         SVWNSZ = WNDSIZ

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
            CALL CHKOUT ( 'SPKR19' )
            RETURN
         END IF
 
         DO WHILE (       ( BUFFER(NREAD) .LT. ET )
     .              .AND. ( REMAIN        .GT. 0  )   )
 
            BUFBAS  =  BUFBAS + NREAD
            NREAD   =  MIN ( REMAIN, DIRSIZ )
            REMAIN  =  REMAIN - NREAD
C
C           Note:  NREAD is always > 0 here.
C
            CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, BUFFER )
 
            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'SPKR19' )
               RETURN
            END IF

         END DO
 
C
C        At this point, BUFBAS - PKDBAS is the number of directory
C        entries preceding the one contained in BUFFER(1).
C
         GROUP  =     ( BUFBAS - PKDBAS )
     .              +   LSTLTD ( ET, NREAD, BUFFER )
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
         CALL CHKOUT ( 'SPKR19' )
         RETURN
      END IF
 
C
C     Find two adjacent epochs bounding the request epoch.  The request
C     time cannot be greater than all of epochs in the group, and it
C     cannot precede the first element of the group.
C
      I  =  LSTLTD (  ET,  ENDIDX-BEGIDX+1,  BUFFER  )
 
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
C     Now select the set of packets used for interpolation.  Note
C     that the window size is known to be even.  
C
C     Unlike SPK types 8, 9, 12, and 13, for type 19 we allow the
C     window size to shrink when the window must be truncated due to
C     proximity to an interval boundary.
C
C     The nominal bracketing epochs we've found are the (WNDSIZ/2)nd
C     and (WNDSIZ/2 + 1)st of the interpolating set.  If the
C     request time is too close to one end of the coverage interval,
C     we reduce the window size, after which one endpoint of the 
C     window will coincide with an endpoint of the coverage interval.
C
C     Let LSIZE be the size of the "left half" of the window:  the
C     size set of window epochs to the left of the request time.
C     We want this size to be WNDSIZ/2, but if not enough states are
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
C
      RECORD(1) =  SUBTYP

      RECORD(2) =  NRCPKT
 
C
C     Read the packets.
C
      CALL DAFGDA ( HANDLE,
     .              MINIB + (FIRST-1)*PKTSIZ,
     .              MINIB +      LAST*PKTSIZ  -  1,
     .              RECORD(3)                       )
 
C
C     Finally, add the epochs to the output record.
C     Read the sequence of time tags.
C
      BUFBAS  =  ( MINIB - 1 )  +  ( NPKT * PKTSIZ )  +  ( FIRST - 1 )

      CALL DAFGDA ( HANDLE, 
     .              BUFBAS+1, 
     .              BUFBAS+NRCPKT, 
     .              RECORD( 3 + NRCPKT*PKTSIZ )  )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKR19' )
         RETURN
      END IF

C
C     The call was successful. Record this fact so that saved
C     interval data are available for re-use.
C
      SVOK = .TRUE.

      CALL CHKOUT ( 'SPKR19' )
      RETURN
      END
