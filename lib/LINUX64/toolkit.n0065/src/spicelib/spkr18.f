C$Procedure      SPKR18 ( Read SPK record from segment, type 18 )
 
      SUBROUTINE SPKR18 ( HANDLE, DESCR, ET, RECORD )
      IMPLICIT NONE
 
C$ Abstract
C
C     Read a single SPK data record from a segment of type 18
C     (MEX/Rosetta Orbit file interpolation).
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

      INCLUDE 'spk18.inc'
 
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
C     DESCR       are the file handle and segment descriptor for
C                 a SPK segment of type 18.
C
C     ET          is a target epoch, for which a data record from
C                 a specific segment is required.
C
C$ Detailed_Output
C
C     RECORD      is a set of data from the specified segment which,
C                 when evaluated at epoch ET, will give the state
C                 (position and velocity) of some body, relative
C                 to some center, in some reference frame.
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
C                 The packet size is a function of the subtype code.
C                 All packets in a record have the same size.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the input HANDLE does not designate a loaded SPK file, the
C        error will be diagnosed by routines called by this routine.
C
C     2) If the segment specified by DESCR is not of data type 18,
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
C     7) If the input segment contains fewer than 2 packets, the 
C        error SPICE(TOOFEWSTATES) is signaled.
C
C$ Files
C
C     See argument HANDLE.
C
C$ Particulars
C
C     See the SPK Required Reading file for a description of the
C     structure of a data type 18 segment.
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
C           IF ( TYPE .EQ. 18 ) THEN
C              CALL SPKR18 ( HANDLE, DESCR, ET, RECORD )
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
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 21-DEC-2012 (NJB)
C
C        An error check was added for segment packet counts
C        less than 2.
C
C        An in-line comment regarding deducibility of record size from
C        segment subtype was removed. The comment now says the actual
C        count of packets in the output record is inserted into the
C        record.
C
C-    SPICELIB Version 1.0.0, 04-SEP-2002 (NJB)
C
C-&
 
C$ Index_Entries
C
C     read record from type_18 spk segment
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 21-DEC-2012 (NJB)
C
C        An error check was added for segment packet counts
C        less than 2.
C     
C-&
 
 
C
C     SPICELIB functions
C
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
C     Maximum polynomial degree:
C     
      INTEGER               MAXDEG
      PARAMETER           ( MAXDEG = 15 )

C
C     Local variables
C
      DOUBLE PRECISION      BUFFER ( BUFSIZ )
      DOUBLE PRECISION      CONTRL ( CTRLSZ )
      DOUBLE PRECISION      DC     ( ND )
 
      INTEGER               BEGIDX
      INTEGER               BEGIN
      INTEGER               BUFBAS
      INTEGER               DIRBAS
      INTEGER               END
      INTEGER               ENDIDX
      INTEGER               FIRST
      INTEGER               GROUP
      INTEGER               HIGH
      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               LAST
      INTEGER               LSIZE
      INTEGER               LOW
      INTEGER               MAXWND
      INTEGER               N
      INTEGER               NDIR
      INTEGER               NREAD
      INTEGER               PACKSZ
      INTEGER               REMAIN
      INTEGER               RSIZE
      INTEGER               START
      INTEGER               SUBTYP
      INTEGER               TIMBAS
      INTEGER               TYPE
      INTEGER               WNDSIZ
 

      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SPKR18' )
 
C
C     Unpack the segment descriptor, and get the start and end addresses
C     of the segment.
C
      CALL DAFUS ( DESCR, 2, 6, DC, IC )
 
      TYPE  = IC( 4 )
      BEGIN = IC( 5 )
      END   = IC( 6 )
 
C
C     Make sure that this really is a type 18 data segment.
C
      IF ( TYPE .NE. 18 ) THEN
         CALL SETMSG( 'You are attempting to locate type * ' //
     .                'data in a type 18 data segment.'      )
         CALL ERRINT( '*',  TYPE                             )
         CALL SIGERR( 'SPICE(WRONGSPKTYPE)'                  )
         CALL CHKOUT( 'SPKR18'                               )
         RETURN
      END IF
 
C
C     Check the request time against the bounds in the segment
C     descriptor.
C
      IF (  ( ET .LT. DC(1) )  .OR.  ( ET .GT. DC(2) )  )  THEN
         CALL SETMSG( 'Request time # is outside of descriptor '   //
     .                'bounds # : #.'                               )
         CALL ERRDP ( '#',  ET                                      )
         CALL ERRDP ( '#',  DC(1)                                   )
         CALL ERRDP ( '#',  DC(2)                                   )
         CALL SIGERR( 'SPICE(TIMEOUTOFBOUNDS)'                      )
         CALL CHKOUT( 'SPKR18'                                      )
         RETURN
 
      END IF

C     We'll need the last two items before we can determine which
C     packets make up our output record.
C
      CALL DAFGDA ( HANDLE, END-CTRLSZ+1, END, CONTRL )

C
C     Check the FAILED flag just in case HANDLE is not attached to
C     any DAF file and the error action is not set to ABORT. You need
C     need to do this only after the first call to DAFGDA.
C
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKR18' )
         RETURN
      END IF
 
      SUBTYP  =  NINT ( CONTRL(1) )
      WNDSIZ  =  NINT ( CONTRL(2) )
      N       =  NINT ( CONTRL(3) )

      IF ( N .LT. 2 )  THEN

         CALL SETMSG( 'Packet count # is less than the minimum '   //
     .                'valid value, which is 2.'                   )
         CALL ERRINT( '#',  N                                      )
         CALL SIGERR( 'SPICE(TOOFEWSTATES)'                        )
         CALL CHKOUT( 'SPKR18'                                     )
         RETURN
 
      END IF

C
C     From this point onward, we assume the segment was constructed
C     correctly.  In particular, we assume:
C
C        1)  The first and last epochs in the segment define a time
C            interval that contains the interval defined by the segment
C            descriptor's time bounds.
C
C        2)  The segment descriptor's time bounds are in order and are
C            distinct.
C
C        3)  The epochs in the segment are in strictly increasing
C            order.
C
C        4)  The degree of the interpolating polynomial specified by
C            the segment is at least 1 and is no larger than
C
C               MAXDEG       
C


C
C     Set the packet size, which is a function of the subtype.
C
      IF ( SUBTYP .EQ. S18TP0 ) THEN

         PACKSZ = S18PS0

      ELSE IF ( SUBTYP .EQ. S18TP1 ) THEN

         PACKSZ = S18PS1

      ELSE
         
         CALL SETMSG ( 'Unexpected SPK type 18 subtype # found in ' //
     .                 'type 18 segment.'                           )
         CALL ERRINT ( '#',  SUBTYP                                 )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                        )
         CALL CHKOUT ( 'SPKR18'                                     )
         RETURN
      
      END IF

C
C     Check the window size.
C
      IF ( WNDSIZ .LE. 0 ) THEN

         CALL SETMSG ( 'Window size in type 18 segment was #; must ' //
     .                 'be positive.'                                )
         CALL ERRINT ( '#',  SUBTYP                                  )
         CALL SIGERR ( 'SPICE(INVALIDVALUE)'                         )
         CALL CHKOUT ( 'SPKR18'                                      )
         RETURN

      END IF


      IF ( SUBTYP .EQ. S18TP0 ) THEN
         
         MAXWND  =  (MAXDEG+1)/2

         IF ( WNDSIZ .GT. MAXWND ) THEN

            CALL SETMSG ( 'Window size in type 18 segment was #; max '//
     .                    'allowed value is # for subtype 0 (Hermite,'//
     .                    ' 12-element packets).'                     )
            CALL ERRINT ( '#',  WNDSIZ                                )
            CALL ERRINT ( '#',  MAXWND                                )
            CALL SIGERR ( 'SPICE(INVALIDVALUE)'                       )
            CALL CHKOUT ( 'SPKR18'                                    )
            RETURN

         END IF

         IF ( ODD(WNDSIZ) ) THEN

            CALL SETMSG ( 'Window size in type 18 segment was #; '    //
     .                    'must be even for subtype 0 (Hermite,'      //
     .                    ' 12-element packets).'                     )
            CALL ERRINT ( '#',  WNDSIZ                                )
            CALL SIGERR ( 'SPICE(INVALIDVALUE)'                       )
            CALL CHKOUT ( 'SPKR18'                                    )
            RETURN

         END IF

      
      ELSE IF ( SUBTYP .EQ. S18TP1 ) THEN

         MAXWND  =  MAXDEG + 1

         IF ( WNDSIZ .GT. MAXWND ) THEN

            CALL SETMSG ( 'Window size in type 18 segment was #; max '//
     .                    'allowed value is # for subtype 1 '         //
     .                    '(Lagrange, 6-element packets).'            )
            CALL ERRINT ( '#',  WNDSIZ                                )
            CALL ERRINT ( '#',  MAXWND                                )
            CALL SIGERR ( 'SPICE(INVALIDVALUE)'                       )
            CALL CHKOUT ( 'SPKR18'                                    )
            RETURN

         END IF

         IF ( ODD(WNDSIZ) ) THEN

            CALL SETMSG ( 'Window size in type 18 segment was #; '//
     .                    'must be even for subtype 1 '           //
     .                    '(Lagrange, 6-element packets).'        )
            CALL ERRINT ( '#',  WNDSIZ                            )
            CALL SIGERR ( 'SPICE(INVALIDVALUE)'                   )
            CALL CHKOUT ( 'SPKR18'                                )
            RETURN

         END IF

      ELSE

         CALL SETMSG ( 'This point should not be reached. Getting ' //
     .                 'here may indicate that the code needs to '  //
     .                 'updated to handle new subtypes.'            )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                        )
         CALL CHKOUT ( 'SPKR18'                                     )
         RETURN

      END IF

C
C     We'll now select the set of packets that define the interpolating
C     polynomials.   We'll start out by finding the first directory
C     entry that is greater than or equal to the request epoch.  We'll
C     use the variable GROUP to indicate the set of epochs to search
C     within, once we've found the right directory entry.
C
      NDIR    =  (N-1) / DIRSIZ
      DIRBAS  =  END  -  NDIR  -  CTRLSZ
 
      IF ( NDIR .EQ. 0 ) THEN
C
C        There's no mystery about which group of epochs to search.
C
         GROUP  = 1
 
      ELSE
C
C        There's at least one directory.  Find the first directory
C        whose time is greater than or equal to the request time, if
C        there is such a directory.  We'll search linearly through the
C        directory entries, reading up to BUFSIZ of them at a time.
C        Having found the correct set of directory entries, we'll
C        perform a binary search within that set for the desired entry.
C
         BUFBAS  =  DIRBAS
         NREAD   =  MIN ( NDIR, DIRSIZ )
         REMAIN  =  NDIR - NREAD
 
         CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, BUFFER )
 
         DO WHILE (       ( BUFFER(NREAD) .LT. ET )
     .              .AND. ( REMAIN        .GT. 0  )   )
 
            BUFBAS  =  BUFBAS + NREAD
            NREAD   =  MIN ( REMAIN, DIRSIZ )
            REMAIN  =  REMAIN - NREAD
C
C           Note:  NREAD is always > 0 here.
C
            CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, BUFFER )
 
         END DO
 
C
C        At this point, BUFBAS - DIRBAS is the number of directory
C        entries preceding the one contained in BUFFER(1).
C
         GROUP  =     ( BUFBAS - DIRBAS )
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
         ENDIDX  =  MIN ( N, DIRSIZ )
 
      ELSE
C
C        If the group index is greater than 1, we'll include the last
C        time tag of the previous group in the set of time tags we look
C        up.  That way, the request time is bracketed by the time tag
C        set we look up.
C
         BEGIDX  =      (  GROUP  - 1 ) * DIRSIZ
         ENDIDX  =  MIN (  BEGIDX + DIRSIZ,  N  )
 
      END IF
 
 
      TIMBAS  =  DIRBAS - N
 
      CALL DAFGDA ( HANDLE, TIMBAS+BEGIDX, TIMBAS+ENDIDX, BUFFER )
 
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
C     Unlike SPK types 8, 9, 12, and 13, for type 18 we adjust
C     the window size to keep the request time within the central
C     interval of the window.
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
C     Put the subtype and actual window size, which is the number of
C     packets in the record, into the output record.
C
      RECORD(1) =  SUBTYP

      RECORD(2) =  WNDSIZ
 
C
C     Read the packets.
C
      CALL DAFGDA ( HANDLE,
     .              BEGIN + (FIRST-1)*PACKSZ,
     .              BEGIN +      LAST*PACKSZ  -  1,
     .              RECORD(3)                       )
 
C
C     Finally, add the epochs to the output record.
C
      START  =  BEGIN   +  N * PACKSZ  +  FIRST - 2
 
      CALL DAFGDA (  HANDLE,
     .               START + 1,
     .               START + WNDSIZ,
     .               RECORD( 3 + WNDSIZ*PACKSZ )   )

      CALL CHKOUT ( 'SPKR18' )
      RETURN
      END
