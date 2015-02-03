C$Procedure      CKR05 ( Read CK record from segment, type 05 )
 
      SUBROUTINE CKR05 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
     .                   RECORD, FOUND                       )
      IMPLICIT NONE
 
C$ Abstract
C
C     Read a single CK data record from a segment of type 05
C     (MEX/Rosetta Attitude file interpolation).
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

      INCLUDE 'ck05.inc'
      INCLUDE 'ckparam.inc'
 
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
C     DESCR       are the file handle and segment descriptor for
C                 a CK segment of type 05.
C
C     SCLKDP      is an encoded spacecraft clock time indicating
C                 the epoch for which pointing is desired.
C
C     TOL        is a time tolerance, measured in the same units as
C                encoded spacecraft clock.
C
C                When SCLKDP falls within the bounds of one of the
C                interpolation intervals then the tolerance has no
C                effect because pointing will be returned at the
C                request time.
C
C                However, if the request time is not in one of the
C                intervals, then the tolerance is used to determine
C                if pointing at one of the interval endpoints should
C                be returned.
C
C     NEEDAV     is true if angular velocity is requested.
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
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine follows the pattern established in the lower-numbered
C     CK data type readers of not explicitly performing error
C     diagnoses.  Exceptions are listed below nonetheless.
C
C     1) If the input HANDLE does not designate a loaded CK file, the
C        error will be diagnosed by routines called by this routine.
C
C     2) If the segment specified by DESCR is not of data type 05,
C        the error 'SPICE(WRONGCKTYPE)' is signaled.
C
C     3) If the input SCLK value is not within the range specified
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
C     7) If the tolerance is negative, the error SPICE(VALUEOUTOFRANGE)
C        is signaled.
C
C$ Files
C
C     See argument HANDLE.
C
C$ Particulars
C
C     See the CK Required Reading file for a description of the
C     structure of a data type 05 segment.
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
C           IF ( TYPE .EQ. 05 ) THEN
C
C              CALL CKR05 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
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
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 27-JAN-2014 (NJB)
C
C        Increased MAXDEG to 23 for compatibility with CK type 6.
C
C-    SPICELIB Version 1.1.0, 06-SEP-2002 (NJB)
C
C-&
 
C$ Index_Entries
C
C     read record from type_5 ck segment
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
      DOUBLE PRECISION      DPMAX

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
 
      INTEGER               PBUFSZ
      PARAMETER           ( PBUFSZ = DIRSIZ + 1 )

      INTEGER               CTRLSZ
      PARAMETER           ( CTRLSZ = 5 )

      INTEGER               PARSIZ
      PARAMETER           ( PARSIZ = 4 )

      INTEGER               SBUFSZ
      PARAMETER           ( SBUFSZ = DIRSIZ + 3 )

C
C     Maximum polynomial degree:
C     
      INTEGER               MAXDEG
      PARAMETER           ( MAXDEG = 23 )

C
C     Local variables
C
      DOUBLE PRECISION      CONTRL ( CTRLSZ )
      DOUBLE PRECISION      DC     ( ND )
      DOUBLE PRECISION      HEPOCH
      DOUBLE PRECISION      LEPOCH
      DOUBLE PRECISION      NSTART
      DOUBLE PRECISION      NNSTRT
      DOUBLE PRECISION      PBUFFR ( PBUFSZ )
      DOUBLE PRECISION      PREVN
      DOUBLE PRECISION      PREVNN
      DOUBLE PRECISION      PREVS
      DOUBLE PRECISION      RATE
      DOUBLE PRECISION      SBUFFR ( SBUFSZ )
      DOUBLE PRECISION      START
      DOUBLE PRECISION      T
 
      INTEGER               PBEGIX
      INTEGER               BEGIN
      INTEGER               BUFBAS
      INTEGER               DIRBAS
      INTEGER               END
      INTEGER               PENDIX
      INTEGER               FIRST
      INTEGER               HIGH
      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               J
      INTEGER               LAST
      INTEGER               LBEG
      INTEGER               LEND
      INTEGER               LHAND
      INTEGER               LSIZE
      INTEGER               LOW
      INTEGER               MAXWND
      INTEGER               N
      INTEGER               NIDIR
      INTEGER               NPDIR
      INTEGER               NINTS
      INTEGER               NPREAD
      INTEGER               NSRCH
      INTEGER               NSREAD
      INTEGER               PACKSZ
      INTEGER               PGROUP
      INTEGER               REMAIN
      INTEGER               RSIZE
      INTEGER               SBEGIX
      INTEGER               SENDIX
      INTEGER               SGROUP
      INTEGER               SUBTYP
      INTEGER               TIMBAS
      INTEGER               TYPE
      INTEGER               WNDSIZ
      INTEGER               WSTART
 
C
C     Saved variables
C
      SAVE                  PREVS
      SAVE                  PREVN
      SAVE                  PREVNN
      SAVE                  LHAND
      SAVE                  LBEG
      SAVE                  LEND
 
C
C     Initial values
C
      DATA    LBEG    / -1    / 
      DATA    LEND    / -1    /
      DATA    LHAND   / 0     / 
      DATA    PREVN   / -1.D0 / 
      DATA    PREVNN  / -1.D0 / 
      DATA    PREVS   / -1.D0 /
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
      
      CALL CHKIN ( 'CKR05' )

C
C     No pointing found so far.
C
      FOUND = .FALSE.

C
C     Unpack the segment descriptor, and get the start and end addresses
C     of the segment.
C
      CALL DAFUS ( DESCR, 2, 6, DC, IC )
 
      TYPE  = IC( 3 )
      BEGIN = IC( 5 )
      END   = IC( 6 )
 
C
C     Make sure that this really is a type 05 data segment.
C
      IF ( TYPE .NE. 5 ) THEN
         CALL SETMSG( 'You are attempting to locate type * ' //
     .                'data in a type 5 data segment.'       )
         CALL ERRINT( '*',  TYPE                             )
         CALL SIGERR( 'SPICE(WRONGCKTYPE)'                   )
         CALL CHKOUT( 'CKR05'                                )
         RETURN
      END IF

C
C     Check the tolerance value.
C
      IF ( TOL .LT. 0.D0 ) THEN
         CALL SETMSG( 'Tolerance must be non-negative but '  //
     .                'was actually *.'                      )
         CALL ERRDP ( '*',  TOL                              )
         CALL SIGERR( 'SPICE(VALUEOUTOFRANGE)'               )
         CALL CHKOUT( 'CKR05'                                )
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
         CALL CHKOUT ( 'CKR05' )
         RETURN
 
      END IF

C
C     Set the request time to use for searching.
C
      T = BRCKTD ( SCLKDP, DC(1), DC(2) )

C
C     From this point onward, we assume the segment was constructed
C     correctly.  In particular, we assume:
C
C        1)  The segment descriptor's time bounds are in order and are
C            distinct.
C
C        2)  The epochs in the segment are in strictly increasing
C            order.
C
C
C        3)  The interpolation interval start times in the segment are 
C            in strictly increasing order.
C
C
C        4)  The degree of the interpolating polynomial specified by
C            the segment is at least 1 and is no larger than MAXDEG.
C
C
      CALL DAFGDA ( HANDLE, END-CTRLSZ+1, END, CONTRL )
 
C
C     Check the FAILED flag just in case HANDLE is not attached to
C     any DAF file and the error action is not set to ABORT.  We
C     do this only after the first call to DAFGDA, as in CKR03.
C
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'CKR05' )
         RETURN
      END IF

      RATE    =         CONTRL(1) 
      SUBTYP  =  NINT ( CONTRL(2) )
      WNDSIZ  =  NINT ( CONTRL(3) )
      NINTS   =  NINT ( CONTRL(4) )
      N       =  NINT ( CONTRL(5) )

C
C     Set the packet size, which is a function of the subtype.
C
      IF ( SUBTYP .EQ. C05TP0 ) THEN

         PACKSZ = C05PS0

      ELSE IF ( SUBTYP .EQ. C05TP1 ) THEN

         PACKSZ = C05PS1

      ELSE IF ( SUBTYP .EQ. C05TP2 ) THEN

         PACKSZ = C05PS2

      ELSE IF ( SUBTYP .EQ. C05TP3 ) THEN

         PACKSZ = C05PS3

      ELSE
         
         CALL SETMSG ( 'Unexpected CK type 5 subtype # found in ' //
     .                 'type 5 segment.'                          )
         CALL ERRINT ( '#',  SUBTYP                               )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                      )
         CALL CHKOUT ( 'CKR05'                                    )
         RETURN
      
      END IF

C
C     Check the window size.
C
      IF ( WNDSIZ .LE. 0 ) THEN

         CALL SETMSG ( 'Window size in type 05 segment was #; must ' //
     .                 'be positive.'                                )
         CALL ERRINT ( '#',  WNDSIZ                                  )
         CALL SIGERR ( 'SPICE(INVALIDVALUE)'                         )
         CALL CHKOUT ( 'CKR05'                                       )
         RETURN

      END IF


      IF (      ( SUBTYP .EQ. C05TP0 )
     .     .OR. ( SUBTYP .EQ. C05TP2 )  ) THEN
C
C        These are the Hermite subtypes.
C         
         MAXWND  =  (MAXDEG+1)/2

         IF ( WNDSIZ .GT. MAXWND ) THEN

            CALL SETMSG ( 'Window size in type 05 segment was #; max '//
     .                    'allowed value is # for subtypes 0 and 2 '  //
     .                    '(Hermite, 8 or 14-element packets).'       )
            CALL ERRINT ( '#',  WNDSIZ                                )
            CALL ERRINT ( '#',  MAXWND                                )
            CALL SIGERR ( 'SPICE(INVALIDVALUE)'                       )
            CALL CHKOUT ( 'CKR05'                                     )
            RETURN

         END IF

         IF ( ODD(WNDSIZ) ) THEN

            CALL SETMSG ( 'Window size in type 05 segment was #; '    //
     .                    'must be even for subtypes 0 and 2 '        //
     .                    '(Hermite, 8 or 14-element packets).'       )
            CALL ERRINT ( '#',  WNDSIZ                                )
            CALL SIGERR ( 'SPICE(INVALIDVALUE)'                       )
            CALL CHKOUT ( 'CKR05'                                     )
            RETURN

         END IF

      
      ELSE IF (      ( SUBTYP .EQ. C05TP1 )
     .          .OR. ( SUBTYP .EQ. C05TP3 )  ) THEN
C
C        These are the Lagrange subtypes.
C         
         MAXWND  =  MAXDEG + 1

         IF ( WNDSIZ .GT. MAXWND ) THEN

            CALL SETMSG ( 'Window size in type 05 segment was #; max '//
     .                    'allowed value is # for subtypes 1 and 3 '  //
     .                    '(Lagrange, 4 or 7-element packets).'       )
            CALL ERRINT ( '#',  WNDSIZ                                )
            CALL ERRINT ( '#',  MAXWND                                )
            CALL SIGERR ( 'SPICE(INVALIDVALUE)'                       )
            CALL CHKOUT ( 'CKR05'                                     )
            RETURN

         END IF

         IF ( ODD(WNDSIZ) ) THEN

            CALL SETMSG ( 'Window size in type 05 segment was #; '    //
     .                    'must be even for subtypes 1 and 3 '        //
     .                    '(Lagrange, 4 or 7-element packets).'       )
            CALL ERRINT ( '#',  WNDSIZ                                )
            CALL SIGERR ( 'SPICE(INVALIDVALUE)'                       )
            CALL CHKOUT ( 'CKR05'                                     )
            RETURN

         END IF

      ELSE

         CALL SETMSG ( 'This point should not be reached. Getting ' //
     .                 'here may indicate that the code needs to '  //
     .                 'updated to handle the new subtype #'        )
         CALL ERRINT ( '#', SUBTYP                                  )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                        )
         CALL CHKOUT ( 'CKR05'                                      )
         RETURN

      END IF

C
C     We now need to select the pointing values to interpolate
C     in order to satisfy the pointing request.  The first step
C     is to use the pointing directories (if any) to locate a set of 
C     epochs bracketing the request time.  Note that the request
C     time might not be bracketed:  it could precede the first
C     epoch or follow the last epoch.  
C
C     We'll use the variable PGROUP to refer to the set of epochs
C     to search.  The first group consists of the epochs prior to
C     and including the first pointing directory entry.  The last
C     group consists of the epochs following the last pointing 
C     directory entry.  Other groups consist of epochs following
C     one pointing directory entry up to and including the next
C     pointing directory entry.
C

      NPDIR   =  (N-1) / DIRSIZ
      DIRBAS  =  BEGIN +  N*PACKSZ  +  N  -  1 
 
      IF ( NPDIR .EQ. 0 ) THEN
C
C        There's no mystery about which group of epochs to search.
C
         PGROUP  = 1
 
      ELSE
C
C        There's at least one directory.  Find the first directory
C        whose time is greater than or equal to the request time, if
C        there is such a directory.  We'll search linearly through the
C        directory entries, reading up to DIRSIZ of them at a time.
C        Having found the correct set of directory entries, we'll
C        perform a binary search within that set for the desired entry.
C
         BUFBAS  =  DIRBAS
         NPREAD  =  MIN ( NPDIR, DIRSIZ )
 
         CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NPREAD, PBUFFR )

         REMAIN  =  NPDIR - NPREAD

 
         DO WHILE (       ( PBUFFR(NPREAD) .LT. T )
     .              .AND. ( REMAIN         .GT. 0 )   )
 
            BUFBAS  =  BUFBAS + NPREAD
            NPREAD  =  MIN ( REMAIN, DIRSIZ )
C
C           Note:  NPREAD is always > 0 here.
C
            CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NPREAD, PBUFFR )
 
            REMAIN  =  REMAIN - NPREAD

         END DO
 
C
C        At this point, BUFBAS - DIRBAS is the number of directory
C        entries preceding the one contained in PBUFFR(1).
C
C        PGROUP is one more than the number of directories we've 
C        passed by.
C
         PGROUP  =     ( BUFBAS - DIRBAS )
     .              +   LSTLTD ( T, NPREAD, PBUFFR )
     .              +   1
 
      END IF

C
C     PGROUP now indicates the set of epochs in which to search for the
C     request epoch.  The following cases can occur:
C
C        PGROUP = 1
C        ==========
C
C           NPDIR = 0
C           --------
C           The request time may precede the first time tag
C           of the segment, exceed the last time tag, or lie
C           in the closed interval bounded by these time tags.
C      
C           NPDIR >= 1
C           ---------
C           The request time may precede the first time tag
C           of the group but does not exceed the last epoch
C           of the group.
C
C
C        1 < PGROUP <= NPDIR
C        ===================
C
C           The request time follows the last time of the
C           previous group and is less than or equal to
C           the pointing directory entry at index PGROUP.
C      
C        1 < PGROUP = NPDIR + 1
C        ======================
C
C           The request time follows the last time of the
C           last pointing directory entry.  The request time
C           may exceed the last time tag.
C
C
C     Now we'll look up the time tags in the group of epochs
C     we've identified.
C
C     We'll use the variable names PBEGIX and PENDIX to refer to
C     the indices, relative to the set of time tags, of the first
C     and last time tags in the set we're going to look up.
C
      IF ( PGROUP .EQ. 1 ) THEN
 
         PBEGIX  =  1
         PENDIX  =  MIN ( N, DIRSIZ )
 
      ELSE
C
C        If the group index is greater than 1, we'll include the last
C        time tag of the previous group in the set of time tags we look
C        up.  That way, the request time is strictly bracketed on the
C        low side by the time tag set we look up.
C
         PBEGIX  =      (  PGROUP  - 1 ) * DIRSIZ
         PENDIX  =  MIN (  PBEGIX + DIRSIZ,  N  )
 
      END IF
 
      TIMBAS  =  DIRBAS - N

      CALL DAFGDA ( HANDLE, TIMBAS+PBEGIX, TIMBAS+PENDIX, PBUFFR )

      NPREAD  =  PENDIX - PBEGIX + 1

C
C     At this point, we'll deal with the cases where T lies outside
C     of the range of epochs we've buffered.
C
      IF ( T .LT. PBUFFR(1) ) THEN
C
C        This can happen only if PGROUP = 1 and T precedes all epochs.
C        If the input request time is too far from PBUFFR(1) on
C        the low side, we're done. 
C         
         IF ( SCLKDP + TOL .LT. PBUFFR(1) ) THEN

            CALL CHKOUT ( 'CKR05' )
            RETURN

         END IF

C
C        Bracket T to move it within the range of buffered epochs.
C
         T = PBUFFR(1)


      ELSE IF ( T .GT. PBUFFR(NPREAD) ) THEN
C
C        This can happen only if T follows all epochs.
C         
         IF ( SCLKDP - TOL .GT. PBUFFR(NPREAD) ) THEN

            CALL CHKOUT ( 'CKR05' )
            RETURN

         END IF

C
C        Bracket T to move it within the range of buffered epochs.
C
         T = PBUFFR(NPREAD)

      END IF

C
C     At this point,
C
C        | T - SCLKDP |  <=  TOL
C                      
C     Also, one of the following is true:
C
C        T is the first time of the segment
C 
C        T is the last time of the segment
C 
C        T equals SCLKDP
C
C
C
C     Find two adjacent time tags bounding the request epoch.  The 
C     request time cannot be greater than all of time tags in the
C     group, and it cannot precede the first element of the group.
C
      I  =  LSTLTD (  T,  NPREAD,  PBUFFR  )
 
C
C     The variables LOW and HIGH are the indices of a pair of time
C     tags that bracket the request time.  Remember that NPREAD could
C     be equal to 1, in which case we would have LOW = HIGH.
C
      IF ( I .EQ. 0 ) THEN
C
C        This can happen only if PGROUP = 1 and T = PBUFFR(1).
C
         LOW    =  1
         LEPOCH =  PBUFFR(1)

         IF ( N .EQ. 1 ) THEN
            HIGH = 1
         ELSE
            HIGH = 2
         END IF

         HEPOCH = PBUFFR(HIGH)

      ELSE

         LOW    =  PBEGIX + I - 1
         LEPOCH =  PBUFFR(I)

         HIGH   =  LOW + 1
         HEPOCH =  PBUFFR(I+1)

      END IF
      
C
C     We now need to find the interpolation interval containing
C     T, if any.  We may be able to use the interpolation 
C     interval found on the previous call to this routine.  If
C     this is the first call or if the previous interval is not
C     applicable, we'll search for the interval.
C
C     First check if the request time falls in the same interval as
C     it did last time.  We need to make sure that we are dealing
C     with the same segment as well as the same time range.
C
C
C        PREVS      is the start time of the interval that satisfied
C                   the previous request for pointing.
C
C        PREVN      is the start time of the interval that followed
C                   the interval specified above.
C
C        PREVNN     is the start time of the interval that followed
C                   the interval starting at PREVN.
C
C        LHAND      is the handle of the file that PREVS and PREVN
C                   were found in.
C
C        LBEG,      are the beginning and ending addresses of the
C        LEND       segment in the file LHAND that PREVS and PREVN
C                   were found in.
C
      IF (  ( HANDLE .EQ. LHAND ) .AND.
     .      ( BEGIN  .EQ. LBEG  ) .AND.
     .      ( END    .EQ. LEND  ) .AND.
     .      ( T      .GE. PREVS ) .AND.
     .      ( T      .LT. PREVN )      ) THEN
 

         START  = PREVS
         NSTART = PREVN
         NNSTRT = PREVNN
 
      ELSE
C
C        Search for the interpolation interval.
C 
         NIDIR   =  (NINTS-1) / DIRSIZ
         DIRBAS  =   END - CTRLSZ - NIDIR
 
         IF ( NIDIR .EQ. 0 ) THEN
C
C           There's no mystery about which group of epochs to search.
C
            SGROUP  = 1
 
         ELSE
C
C           There's at least one directory.  Find the first directory
C           whose time is greater than or equal to the request time, if
C           there is such a directory.  We'll search linearly through
C           the directory entries, reading up to DIRSIZ of them at a
C           time. Having found the correct set of directory entries,
C           we'll perform a binary search within that set for the
C           desired entry.
C
            BUFBAS  =  DIRBAS
            NSREAD  =  MIN ( NIDIR, DIRSIZ )
            REMAIN  =  NIDIR - NSREAD

            CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NSREAD, SBUFFR )
 
            DO WHILE (       ( SBUFFR(NSREAD) .LT. T )
     .                 .AND. ( REMAIN         .GT. 0 )   )
 
               BUFBAS  =  BUFBAS + NSREAD
               NSREAD  =  MIN ( REMAIN, DIRSIZ )
               REMAIN  =  REMAIN - NSREAD
C
C              Note:  NSREAD is always > 0 here.
C
               CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NSREAD, SBUFFR )
 
            END DO
 
C
C           At this point, BUFBAS - DIRBAS is the number of directory
C           entries preceding the one contained in SBUFFR(1).
C
C           SGROUP is one more than the number of directories we've 
C           passed by.
C
            SGROUP  =     ( BUFBAS - DIRBAS )
     .                 +   LSTLTD ( T, NSREAD, SBUFFR )
     .                 +   1
 
         END IF

C
C        SGROUP now indicates the set of interval start times in which
C        to search for the request epoch.
C
C        Now we'll look up the time tags in the group of epochs we've
C        identified.
C
C        We'll use the variable names SBEGIX and SENDIX to refer to the
C        indices, relative to the set of start times, of the first and
C        last start times in the set we're going to look up.
C
         IF ( SGROUP .EQ. 1 ) THEN

            SBEGIX  =  1
            SENDIX  =  MIN ( NINTS, DIRSIZ + 2 )

         ELSE
C
C           Look up the start times for the group of interest. Also
C           buffer last start time from the previous group. Also, it
C           turns out to be useful to pick up two extra start
C           times---the first two start times of the next group---if
C           they exist.
C          
            SBEGIX  =      (  SGROUP  - 1 ) * DIRSIZ

            SENDIX  =  MIN (  SBEGIX + DIRSIZ + 2,  NINTS  )

         END IF

         TIMBAS  =  DIRBAS - NINTS

         CALL DAFGDA ( HANDLE, TIMBAS+SBEGIX, TIMBAS+SENDIX, SBUFFR )

         NSREAD  =  SENDIX - SBEGIX + 1

C
C        Find the last interval start time less than or equal to the
C        request time.  We know T is greater than or equal to the
C        first start time, so I will be > 0.  
C
         NSRCH  =  MIN ( DIRSIZ + 1, NSREAD )
 
         I      =  LSTLED (  T,  NSRCH, SBUFFR  )

         START  =  SBUFFR(I)

C
C        Let NSTART ("next start") be the start time that follows
C        START, if START is not the last start time.  If NSTART
C        has a successor, let NNSTRT be that start time.
C        
         IF ( I .LT. NSREAD ) THEN

            NSTART = SBUFFR(I+1)            

            IF ( I+1 .LT. NSREAD ) THEN

               NNSTRT = SBUFFR(I+2)
            ELSE
               NNSTRT = DPMAX()
            END IF

         ELSE

            NSTART = DPMAX()
            NNSTRT = DPMAX()

         END IF

      END IF

C
C     If T does not lie within the interpolation interval starting
C     at time START, we'll determine whether T is closer to this
C     interval or the next.  If the distance between T and the
C     closer interval is less than or equal to TOL, we'll map T
C     to the closer endpoint of the closer interval.  Otherwise,
C     we return without finding pointing.
C 
      IF ( HEPOCH .EQ. NSTART ) THEN
C
C        The first time tag greater than or equal to T is the start
C        time of the next interpolation interval.  
C
C        The request time lies between interpolation intervals.
C        LEPOCH is the last time tag of the first interval; HEPOCH
C        is the first time tag of the next interval.
C
         IF (  ABS( T - LEPOCH ) .LE. ABS( HEPOCH - T )  ) THEN
C
C           T is closer to the first interval...
C
            IF (  ABS( T - LEPOCH )  .GT.  TOL  ) THEN
C
C              ...But T is too far from the interval.
C
               CALL CHKOUT ( 'CKR05' )
               RETURN

            END IF

C
C           Map T to the right endpoint of the preceding interval.
C
            T      = LEPOCH
            HIGH   = LOW
            HEPOCH = LEPOCH

         ELSE
C
C           T is closer to the second interval...
C
            IF (  ABS( HEPOCH - T )  .GT.  TOL  ) THEN
C
C              ...But T is too far from the interval.
C
               CALL CHKOUT ( 'CKR05' )
               RETURN

            END IF

C
C           Map T to the left endpoint of the next interval.
C
            T      = HEPOCH
            LOW    = HIGH
            LEPOCH = HEPOCH

C
C           Since we're going to be picking time tags from the next
C           interval, we'll need to adjust START and NSTART.
C 
            START  = NSTART
            NSTART = NNSTRT

         END IF

      END IF

C
C     We now have 
C
C        LEPOCH < T <  HEPOCH
C                -   -
C
C     where LEPOCH and HEPOCH are the time tags at indices
C     LOW and HIGH, respectively.
C
C     Now select the set of packets used for interpolation.  Note
C     that the window size is known to be even.  
C
C     Unlike CK types 8, 9, 12, and 13, for type 05 we adjust
C     the window size to keep the request time within the central
C     interval of the window.
C
C     The nominal bracketing epochs we've found are the (WNDSIZ/2)nd
C     and (WNDSIZ/2 + 1)st of the interpolating set.  If the request
C     time is too close to one end of the interpolation interval, we
C     reduce the window size, after which one endpoint of the window
C     will coincide with an endpoint of the interpolation interval.
C
C     We start out by looking up the set of time tags we'd use
C     if there were no gaps in the coverage.  We then trim our
C     time tag set to ensure all tags are in the interpolation
C     interval.  It's possible that the interpolation window will
C     collapse to a single point as a result of this last step.
C
C     Let LSIZE be the size of the "left half" of the window:  the
C     size of the set of window epochs to the left of the request time.
C     We want this size to be WNDSIZ/2, but if not enough states are
C     available, the set ranges from index 1 to index LOW.
C
      LSIZE =  MIN ( WNDSIZ/2, LOW ) 

C
C     RSIZE is defined analogously for the right half of the window.
C
      RSIZE =  MIN (  WNDSIZ/2,  ( N - HIGH + 1 )  )

C
C     The window size is simply the sum of LSIZE and RSIZE.
C
      WNDSIZ =  LSIZE + RSIZE

C
C     FIRST and LAST are the endpoints of the range of indices of
C     time tags (and packets) we'll collect in the output record.
C
      FIRST =  LOW    -  LSIZE   +  1

      LAST  =  FIRST  +  WNDSIZ  -  1
 
C
C     Buffer the epochs.
C
      WSTART  =  BEGIN  +  N * PACKSZ  +  FIRST  -  1

      CALL DAFGDA (  HANDLE, WSTART, WSTART+WNDSIZ-1, PBUFFR )

C
C     Discard any epochs less than START or greater than or equal
C     to NSTART.  The set of epochs we want ranges from indices
C     I+1 to J.  This range is non-empty unless START and NSTART
C     are both DPMAX().
C
      I = LSTLTD ( START,  WNDSIZ, PBUFFR )
      J = LSTLTD ( NSTART, WNDSIZ, PBUFFR )


      IF ( I .EQ. J ) THEN
C
C        Fuggedaboudit.
C
         CALL CHKOUT ( 'CKR05' )
         RETURN

      END IF

C
C     Update FIRST, LAST, and WNDSIZ.
C
      WNDSIZ = J     - I
      FIRST  = FIRST + I      
      LAST   = FIRST + WNDSIZ - 1

C
C     Put the subtype into the output record.  The size of the group 
C     of packets is derived from the subtype, so we need not include
C     the size.
C
      RECORD(1) =  T
      RECORD(2) =  SUBTYP
      RECORD(3) =  WNDSIZ
      RECORD(4) =  RATE 

C
C     Read the packets.
C
      CALL DAFGDA ( HANDLE,
     .              BEGIN + (FIRST-1)*PACKSZ,
     .              BEGIN +      LAST*PACKSZ  -  1,
     .              RECORD(PARSIZ+1)                )
 
C
C     Finally, add the epochs to the output record.
C
      CALL MOVED ( PBUFFR(I+1), 
     .             J-I,  
     .             RECORD( PARSIZ + 1 + WNDSIZ*PACKSZ )  )


C
C     Save the information about the interval and segment.
C
      LHAND  = HANDLE
      LBEG   = BEGIN
      LEND   = END
      PREVS  = START
      PREVN  = NSTART
      PREVNN = NNSTRT

C
C     Indicate pointing was found.
C
      FOUND = .TRUE.

      CALL CHKOUT ( 'CKR05' )
      RETURN
      END









