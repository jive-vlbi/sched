C$Procedure  SPKR05 ( Read SPK record from segment, type 5 )
 
      SUBROUTINE SPKR05 ( HANDLE, DESCR, ET, RECORD )
      IMPLICIT NONE
 
C$ Abstract
C
C     Read a single SPK data record from a segment of type 5
C     ( two body propagation between discrete state vectors ).
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
C                 the type 05 SPK segment to be read.
C
C     ET          is a target epoch, specified as ephemeris seconds past
C                 J2000, for which a data record from the segment is
C                 required.
C
C$ Detailed_Output
C
C     RECORD      is a logical record from the specified segment which,
C                 when evaluated at epoch ET, will give the state
C                 (position and velocity) of some body, relative
C                 to some center, in some inertial reference frame.
C
C                 The structure of RECORD is:
C
C                     RECORD(1)
C                        .            state of the body at epoch 1.
C                        .
C                        .
C                     RECORD(6)
C
C                     RECORD(7)
C                        .
C                        .            state of the body at epoch 2.
C                        .
C                     RECORD(12)
C                     RECORD(13)      epoch 1 in seconds past 2000.
C                     RECORD(14)      epoch 2 in seconds past 2000.
C                     RECORD(15)      GM for the center of motion.
C
C
C                 Epoch 1 and epoch 2 are the times in the segment that
C                 bracket ET.  If ET is less than the first time in the
C                 segment then both epochs 1 and 2 are equal to the
C                 first time.  And if ET is greater than the last time
C                 then, epochs 1 and 2 are set equal to this last time.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the segment specified by DESCR is not of data type 05,
C        the error 'SPICE(WRONGSPKTYPE)' is signalled.
C
C     2) No error is signalled if ET is outside the time bounds of
C        the segment. The output RECORD will contain epochs and the
C        associated states which satisfy the rules stated above.
C
C$ Files
C
C     See argument HANDLE.
C
C$ Particulars
C
C     This routine reads the segment specified by DESCR from the SPK
C     file attached to HANDLE to locate the two epochs in the segment
C     that bracket the input ET. It then returns a logical record which
C     contains these times and their associated states, and also the
C     mass of the center of motion. The routine makes explicit use of
C     the structure of the type 05 data segment to locate this data.
C
C     See the section of the SPK Required Reading on data type 05 for
C     a description of the structure of a type 05 segment.
C
C$ Examples
C
C     The data returned by the SPKRnn routine is in its rawest form,
C     taken directly from the segment.  As such, it will be meaningless
C     to a user unless he/she understands the structure of the data type
C     completely.  Given that understanding, however, the SPKRnn
C     routines might be used to "dump" and check segment data for a
C     particular epoch.
C
C
C     C
C     C     Get a segment applicable to a specified body and epoch.
C     C
C           CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
C
C           IF ( FOUND ) THEN
C
C     C
C     C        Look at parts of the descriptor.
C     C
C              CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
C              CENTER = ICD( 2 )
C              REF    = ICD( 3 )
C              TYPE   = ICD( 4 )
C
C              IF ( TYPE .EQ. 05 ) THEN
C
C                 CALL SPKR05 ( HANDLE, DESCR, ET, RECORD )
C                     .
C                     .  Look at the RECORD data.
C                     .
C              END IF
C
C           END IF
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
C     J.M. Lynch      (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
C
C        Replaced DAFRDA call with DAFGDA.
C        Added IMPLICIT NONE.
C
C-    SPICELIB Version 1.0.0, 01-APR-1992 (JML) (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     read record from type_5 spk segment
C
C-&
 
 
 
C
C     Local parameters
C
      INTEGER               SIZE
      PARAMETER           ( SIZE    = 100 )
 
      INTEGER               DIRSIZ
      PARAMETER           ( DIRSIZ  = SIZE )
 
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ  = SIZE )
 
      INTEGER               STASIZ
      PARAMETER           ( STASIZ =    6 )
 
      INTEGER               ND
      PARAMETER           ( ND =        2 )
 
      INTEGER               NI
      PARAMETER           ( NI =        6 )
 
C
C     SPICELIB functions
C
      INTEGER               LSTLTD
      LOGICAL               RETURN
 
C
C     Local variables
C
      DOUBLE PRECISION      DC       (   ND )
      DOUBLE PRECISION      DATA     ( SIZE )
 
      INTEGER               IC       (   NI )
      INTEGER               BEGIN
      INTEGER               END
      INTEGER               TYPE
      INTEGER               NREC
      INTEGER               NDIR
      INTEGER               GROUP
      INTEGER               DIRLOC
      INTEGER               REMAIN
      INTEGER               SKIP
      INTEGER               GRPADD
      INTEGER               ADDRSS
      INTEGER               I
      INTEGER               N
 
      LOGICAL               FND
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKR05' )
      END IF
 
C
C     Unpack the segment descriptor.
C
      CALL DAFUS ( DESCR, ND, NI, DC, IC )
 
      TYPE  = IC(4)
      BEGIN = IC(5)
      END   = IC(6)
 
C
C     Make sure that this really is a type 5 data segment.
C
      IF ( TYPE .NE. 05 ) THEN
         CALL SETMSG( 'You are attempting to locate type 5 '      //
     .                'data in a type # data segment.'              )
         CALL ERRINT( '#',  TYPE                                    )
         CALL SIGERR( 'SPICE(WRONGSPKTYPE)'                         )
         CALL CHKOUT( 'SPKR05'                                      )
         RETURN
      END IF
 
C
C     Get the number of records in the segment. While we're at it,
C     get the GM of the central body (it's adjacent to NREC)
C     since we'll need it anyway. Put it where it belongs, at the
C     end of the output record.
C
      CALL DAFGDA ( HANDLE, END-1, END, DATA )
 
      NREC       = NINT ( DATA(2) )
      RECORD(15) =        DATA(1)
 
C
C     From the number of records, we can compute the number of
C     directory epochs.
C
      NDIR = NREC / DIRSIZ
 
C
C     The directory epochs narrow down the search to a group of DIRSIZ
C     or fewer records. Because the Ith directory epoch is the I*100th
C     epoch, the Ith group will contain epochs ((I-1)*100 + 1) through
C     (I*100).  For example:
C                            group   first epoch #   last epoch #
C                            -----   -------------   ------------
C                              1               1          100
C                              2             101          200
C                              .               .            .
C                              .               .            .
C                             10             901         1000
C                              .               .            .
C                              .               .            .
C                              N     (N-1)*100+1        N*100
 
      IF ( NDIR .EQ. 0 ) THEN
C
C        There is only one group if there are no directory epochs.
C
         GROUP = 1
 
      ELSE
C
C        Compute the location of the first directory epoch.  From the
C        beginning of the segment, we need to go through all of the
C        NREC states and epochs.
C
         DIRLOC = BEGIN + ( STASIZ + 1 ) * NREC
 
C
C        Determine which group of DIRSIZ times to search, by finding
C        the last directory epoch that is less than ET.
C
         FND    = .FALSE.
         REMAIN =  NDIR
         GROUP  =  0
 
         DO WHILE ( .NOT. FND )
C
C           Read in as many as BUFSIZ directory epochs at a time
C           for comparison.
C
            N = MIN( REMAIN, BUFSIZ )
 
            CALL DAFGDA ( HANDLE, DIRLOC, DIRLOC + N - 1, DATA )
 
            REMAIN = REMAIN - N
 
C
C           Determine the last directory element in DATA that's less
C           than ET.
C
C           If we reach the end of the directories, and still haven't
C           found one bigger than the epoch, the group is the last group
C           in the segment.
C
C           Otherwise keep looking.
C
C
            I = LSTLTD( ET, N, DATA )
 
            IF ( I .LT. N ) THEN
 
               GROUP =  GROUP + I + 1
               FND   = .TRUE.
 
            ELSE IF ( REMAIN .EQ. 0 ) THEN
 
               GROUP =  NDIR + 1
               FND   = .TRUE.
 
            ELSE
 
               DIRLOC = DIRLOC + N
               GROUP  = GROUP  + N
 
            END IF
 
         END DO
 
      END IF
 
C
C     Now we know which group of DIRSIZ (or less) epochs to look at.
C     Out of the NREC epochs, the number that we should skip over
C     to get to the proper group is DIRSIZ * ( GROUP - 1 ).
C
      SKIP = DIRSIZ * ( GROUP - 1 )
 
C
C     From this we can compute the index into the segment of the group
C     of times we want.  From the beginning, we need to pass through
C     STASIZ * NREC state numbers to get to the first epoch. Then we
C     skip over the number just computed above.
C
      GRPADD = BEGIN + NREC * ( STASIZ ) + SKIP
 
C
C     The number of epochs that we have to look at may be less than
C     DIRSIZ.  However many there are, go ahead and read them into the
C     buffer.
C
C     If there are no times in the last group then the time that we
C     are looking for is the same as the last directory epoch.
C     We should not try to read in this instance.
C
      N = MIN ( DIRSIZ, NREC - SKIP )
 
      IF ( N .NE. 0 ) THEN
 
         CALL DAFGDA ( HANDLE, GRPADD, GRPADD + N - 1, DATA )
C
C        Find the index of the largest time in the group that is less
C        than the input time.
C
         I = LSTLTD ( ET, N, DATA )
 
      ELSE
C
C        If we are here it means that ET is greater then the last
C        time in the segment and there are no elements in the last
C        group.  This can occur when the number of epochs is a multiple
C        DIRSIZ.
C
C        By setting N equal to I we can handle this case in the
C        same branch as when there are elements in the last group.
C        This is because the DATA array still contains the directory
C        epochs and I is pointing at the last element which is also the
C        last time in the segment.
C
         N = I
 
      END IF
 
C
C     At this point N is the number of epochs in this GROUP which is
C     also the size of the array DATA which contains the epochs. I is
C     the index of the largest time in DATA which is less than ET.
C
C     We need to take different actions depending on whether ET is less
C     than the first time or greater than the last one in the GROUP.
C
 
      IF ( I .EQ. 0 ) THEN
 
         IF ( GROUP .EQ. 1 ) THEN
 
C
C           ET is less than or equal to the first time in the segment.
C           Return the state at the first time twice.
C
            RECORD(13) = DATA(1)
            RECORD(14) = DATA(1)
 
            CALL DAFGDA ( HANDLE, BEGIN, BEGIN + 5, DATA )
 
            CALL MOVED  ( DATA, 6, RECORD(1) )
            CALL MOVED  ( DATA, 6, RECORD(7) )
 
            CALL CHKOUT( 'SPKR05' )
            RETURN
 
         ELSE
C
C           ET is less than or equal to the first time in this group
C           but not the first time in the segment. Get the last time
C           from the preceding group. The states for this case will
C           be read outside of the IF block.
C
            CALL DAFGDA ( HANDLE, GRPADD - 1, GRPADD, DATA )
 
            RECORD(13) = DATA(1)
            RECORD(14) = DATA(2)
 
         END IF
 
      ELSE IF ( I .EQ. N ) THEN
 
         IF ( GROUP .EQ. (NDIR + 1) ) THEN
C
C           ET is greater than all of the times in the segment. Return
C           the state for the last time twice.
C
            RECORD(13) = DATA(N)
            RECORD(14) = DATA(N)
 
            ADDRSS = BEGIN + ( NREC - 1 ) * STASIZ
 
            CALL DAFGDA ( HANDLE, ADDRSS, ADDRSS + 5, DATA )
 
            CALL MOVED ( DATA, 6, RECORD(1) )
            CALL MOVED ( DATA, 6, RECORD(7) )
 
            CALL CHKOUT( 'SPKR05' )
            RETURN
 
         ELSE
C
C           ET is greater than the last time in this group but this is
C           not the last time in the segment.  Need the first time from
C           the following group. The states for this case will be read
C           outside of the IF block.
C
            CALL DAFGDA ( HANDLE, GRPADD + N - 1, GRPADD + N, DATA )
 
            RECORD(13) = DATA(1)
            RECORD(14) = DATA(2)
 
         END IF
 
      ELSE
C
C        There are two times in the group that bracket ET. The states
C        for this case will be read outside of the IF block.
C
         RECORD(13) = DATA( I )
         RECORD(14) = DATA(I+1)
 
      END IF
 
C
C     Read the consecutive states for the two epochs found above.
C     ET is greater than the (SKIP + I)th time but less than or
C     equal to the time (SKIP + I + 1).
C
 
      ADDRSS = BEGIN + ( SKIP + I - 1 ) * STASIZ
 
      CALL DAFGDA ( HANDLE, ADDRSS, ADDRSS + 11, DATA )
 
      CALL MOVED  ( DATA, 12, RECORD(1) )
 
      CALL CHKOUT( 'SPKR05' )
      RETURN
      END
