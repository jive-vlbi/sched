C$Procedure      SPKR09 ( Read SPK record from segment, type 9 )
 
      SUBROUTINE SPKR09 ( HANDLE, DESCR, ET, RECORD )
      IMPLICIT NONE
 
C$ Abstract
C
C     Read a single SPK data record from a segment of type 9
C     (Unequally spaced discrete states, interpolated by Lagrange
C     polynomials).
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
C                 a SPK segment of type 9.
C
C     ET          is a target epoch, for which a data record from
C                 a specific segment is required.
C
C$ Detailed_Output
C
C     RECORD      is a set of data from the specified segment which,
C                 when evaluated at epoch ET, will give the state
C                 (position and velocity) of some body, relative
C                 to some center, in some inertial reference frame.
C
C                 The structure of the record is as follows:
C
C                    +----------------------+
C                    | number of states (n) |
C                    +----------------------+
C                    | state 1 (6 elts.)    |
C                    +----------------------+
C                    | state 2 (6 elts.)    |
C                    +----------------------+
C                                .
C                                .
C                                .
C                    +----------------------+
C                    | state n (6 elts.)    |
C                    +----------------------+
C                    | epochs 1--n          |
C                    +----------------------+
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine follows the pattern established in the lower-numbered
C     SPK data type readers of not explicitly performing error
C     diagnoses.  Exceptions are listed below nonetheless.
C
C     1)  If the input HANDLE does not designate a loaded SPK file, the
C         error will be diagnosed by routines called by this routine.
C
C     2) If the segment specified by DESCR is not of data types 9 or 13,
C        the error 'SPICE(WRONGSPKTYPE)' is signalled.
C
C     3)  If the input ET value is not within the range specified
C         in the segment descriptor, the error SPICE(TIMEOUTOFBOUNDS)
C         is signalled.
C
C$ Files
C
C     See argument HANDLE.
C
C$ Particulars
C
C     See the SPK Required Reading file for a description of the
C     structure of a data type 9 segment.
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
C           IF ( TYPE .EQ. 9 ) THEN
C              CALL SPKR09 ( HANDLE, DESCR, ET, RECORD )
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
C-    SPICELIB Version 2.1.0, 07-SEP-2001 (EDW)
C
C        Replaced DAFRDA call with DAFGDA.
C        Added IMPLICIT NONE.
C
C-    SPICELIB Version 2.0.0, 06-NOV-1999 (NJB)
C
C        Data type check was relaxed to enable reading type 13
C        segments.
C
C-    SPICELIB Version 1.0.1, 24-OCT-1994 (NJB)
C
C        In-line comment concerning transpose of state data was
C        removed.
C
C-    SPICELIB Version 1.0.0,  14-AUG-1993 (NJB)
C
C-&
 
C$ Index_Entries
C
C     read record from type_9 spk segment
C
C-&
 
C$ Revisions
C
C-&
 
 
 
C
C     SPICELIB functions
C
      INTEGER               LSTLTD
 
      LOGICAL               ODD
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               ND
      PARAMETER           ( ND     = 2 )
 
      INTEGER               NI
      PARAMETER           ( NI     = 6 )
 
      INTEGER               STATSZ
      PARAMETER           ( STATSZ = 6 )
 
      INTEGER               DIRSIZ
      PARAMETER           ( DIRSIZ = 100 )
 
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = DIRSIZ + 1 )
 
 
C
C     Local variables
C
      DOUBLE PRECISION      BUFFER ( BUFSIZ )
      DOUBLE PRECISION      CONTRL ( 2  )
      DOUBLE PRECISION      DC     ( ND )
 
      INTEGER               BEGIDX
      INTEGER               BEGIN
      INTEGER               BUFBAS
      INTEGER               DEGREE
      INTEGER               DIRBAS
      INTEGER               END
      INTEGER               ENDIDX
      INTEGER               FIRST
      INTEGER               GROUP
      INTEGER               HIGH
      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               LAST
      INTEGER               LOW
      INTEGER               N
      INTEGER               NDIR
      INTEGER               NEAR
      INTEGER               NREAD
      INTEGER               REMAIN
      INTEGER               START
      INTEGER               TIMBAS
      INTEGER               TYPE
      INTEGER               WNDSIZ
 
 
C
C     Use discovery check-in.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
C
C     Unpack the segment descriptor, and get the start and end addresses
C     of the segment.
C
      CALL DAFUS ( DESCR, 2, 6, DC, IC )
 
      TYPE  = IC( 4 )
      BEGIN = IC( 5 )
      END   = IC( 6 )
 
C
C     Make sure that this really is a type 9 or type 13 data segment.
C
      IF (  ( TYPE .NE. 09 ) .AND. ( TYPE .NE. 13 )  ) THEN
         CALL CHKIN ( 'SPKR09'                                        )
         CALL SETMSG( 'You are attempting to locate type 9 or type ' //
     .                '13 data in a type # data segment.'             )
         CALL ERRINT( '#',  TYPE                                      )
         CALL SIGERR( 'SPICE(WRONGSPKTYPE)'                           )
         CALL CHKOUT( 'SPKR09'                                        )
         RETURN
      END IF
 
C
C     Check the request time against the bounds in the segment
C     descriptor.
C
      IF (  ( ET .LT. DC(1) )  .OR.  ( ET .GT. DC(2) )  )  THEN
         CALL CHKIN ( 'SPKR09'                                      )
         CALL SETMSG( 'Request time # is outside of descriptor '   //
     .                'bounds # : #.'                               )
         CALL ERRDP ( '#',  ET                                      )
         CALL ERRDP ( '#',  DC(1)                                   )
         CALL ERRDP ( '#',  DC(2)                                   )
         CALL SIGERR( 'SPICE(TIMEOUTOFBOUNDS)'                      )
         CALL CHKOUT( 'SPKR09'                                      )
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
C               ( L - 1 ) / 7         [integer division]
C
C            where L is the declared length of the argument RECORD.
C
C        5)  There are at least as many epochs in the segment as the
C            the number of points required to define an interpolating
C            polynomial of the specified degree.
C
C
C     We'll need the last two items before we can determine which
C     states make up our output record.
C
C
      CALL DAFGDA ( HANDLE, END-1, END, CONTRL )
 
      DEGREE  =  NINT ( CONTRL(1) )
      N       =  NINT ( CONTRL(2) )
 
      WNDSIZ  =  DEGREE + 1
 
C
C     We'll now select the set of states that define the interpolating
C     polynomials.   We'll start out by finding the first directory
C     entry that is greater than or equal to the request epoch.  We'll
C     use the variable GROUP to indicate the set of epochs to search
C     within, once we've found the right directory entry.
C
      NDIR    =  (N-1) / DIRSIZ
      DIRBAS  =  END  -  NDIR  -  2
 
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
         NREAD   =  MIN ( NDIR, BUFSIZ )
         REMAIN  =  NDIR - NREAD
 
         CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, BUFFER )
 
         DO WHILE (       ( BUFFER(NREAD) .LT. ET )
     .              .AND. ( REMAIN        .GT. 0  )   )
 
            BUFBAS  =  BUFBAS + NREAD
            NREAD   =  MIN ( REMAIN, BUFSIZ )
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
      I     =  LSTLTD (  ET,  ENDIDX-BEGIDX+1,  BUFFER  )
 
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
C     Now select the set of states used for interpolation.
C
      IF ( ODD(WNDSIZ) ) THEN
C
C        Find the index of the state whose epoch is closest to the
C        input epoch.  The index I is in the range [0, DIRSIZ],
C        since ENDIDX - BEGIDX never exceeds DIRSIZ, and ET is
C        never larger than the (ENDIDX-BEGIDX+1)th element of the
C        buffer.
C
         IF ( I .EQ. 0 ) THEN
C
C           This can happen only if the request time matches the
C           first time tag of the segment.
C
            NEAR = LOW
 
         ELSE IF (       DABS(  ET  -  BUFFER(I)    )
     .             .LT.  DABS(  ET  -  BUFFER(I+1)  )    )  THEN
 
            NEAR = LOW
 
         ELSE
            NEAR = HIGH
         END IF
 
C
C        The epochs whose index is NEAR is the (WNDSIZ/2 + 1)th
C        of the interpolating set, unless the request time is too close
C        to the end of the coverage interval, in which case one endpoint
C        of the window will coincide with an endpoint of the coverage
C        interval.
C
         FIRST =  MIN (  MAX ( NEAR - DEGREE/2,  1),   N-DEGREE  )
 
         LAST  =  FIRST  +  DEGREE
 
      ELSE
C
C        The group size is even.
C
C        The bracketing epochs we've found are the (WNDSIZ/2)th
C        and (WNDSIZ/2 + 1)th of the interpolating set, unless the
C        request time is too close to the end of the coverage interval,
C        in which case one endpoint of the window will coincide with
C        an endpoint of the coverage interval.
C
         FIRST =  MIN (  MAX ( LOW - DEGREE/2,  1 ),   N-DEGREE  )
 
         LAST  =  FIRST  +  DEGREE
 
      END IF
 
C
C     Put the size of the group of states into the output record.
C
      RECORD(1) =  WNDSIZ
 
C
C     Read the states.
C
      CALL DAFGDA ( HANDLE,
     .              BEGIN + (FIRST-1)*STATSZ,
     .              BEGIN +      LAST*STATSZ  -  1,
     .              RECORD(2)                       )
 
C
C     Finally, add the epochs to the output record.
C
      START  =  BEGIN   +  N * STATSZ  +  FIRST - 2
 
      CALL DAFGDA (  HANDLE,
     .               START + 1,
     .               START + WNDSIZ,
     .               RECORD( 2 + WNDSIZ*STATSZ )   )
 
      RETURN
      END
