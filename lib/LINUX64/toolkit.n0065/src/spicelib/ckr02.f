C$Procedure      CKR02 ( C-kernel, read pointing record, data type 2 )
 
      SUBROUTINE CKR02 ( HANDLE, DESCR, SCLKDP, TOL, RECORD, FOUND )
      IMPLICIT NONE
 
C$ Abstract
C
C     Read a pointing record from a CK segment, data type 2.
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
C     DAF
C
C$ Keywords
C
C     POINTING
C
C$ Declarations
 
      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR  ( * )
      DOUBLE PRECISION      SCLKDP
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      RECORD ( * )
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     DESCR      I   Segment descriptor.
C     SCLKDP     I   Spacecraft clock time.
C     TOL        I   Time tolerance
C     RECORD     O   Pointing data record.
C     FOUND      O   True when data is found.
C
C$ Detailed_Input
C
C     HANDLE     is the integer handle of the CK file containing the
C                segment.
C
C     DESCR      is the descriptor of the segment.
C
C     SCLKDP     is the encoded spacecraft clock time for which
C                pointing is being requested.
C
C     TOL        is a time tolerance, measured in the same units as
C                encoded spacecraft clock.
C
C                When SCLKDP falls within the bounds of one of the
C                intervals then the tolerance has no effect. However,
C                if the request time is not in one of the intervals
C                then the tolerance is used to determine if pointing
C                at one of the interval endpoints should be returned.
C
C$ Detailed_Output
C
C     RECORD     is the pointing record.  Contents are as follows:
C
C                   RECORD( 1  ) = Start time of interval.
C                   RECORD( 2  ) = Time for which pointing was found.
C                   RECORD( 3  ) = Seconds per tick rate.
C
C                   RECORD( 4  ) = q0
C                   RECORD( 5  ) = q1
C                   RECORD( 6  ) = q2
C                   RECORD( 7  ) = q3
C
C                   RECORD( 8  ) = av1
C                   RECORD( 9  ) = av2
C                   RECORD( 10 ) = av3
C
C                The quantities q0 - q3 are the components of the
C                quaternion that represents the C-matrix associated with
C                the start time of the interval. The quantities av1,
C                av2, and av3 represent the angular velocity vector of
C                the interval. The components of the angular velocity
C                vector are specified relative to the inertial reference
C                frame of the segment.
C
C     FOUND      is true if a record was found to satisfy the pointing
C                request.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the specified handle does not belong to any file that is
C         currently known to be open, an error is diagnosed by a
C         routine that this routine calls.
C
C     2)  If DESCR is not a valid, packed descriptor of a segment in
C         the CK file specified by HANDLE, the results of this routine
C         are unpredictable.
C
C     3)  If the segment is not of data type 2, as specified in the
C         third integer component of the segment descriptor, then
C         the error SPICE(WRONGDATATYPE) is signalled.
C
C$ Files
C
C     The file containing the segment is specified by its handle, and
C     should be opened for read, either by CKLPF or DAFOPR.
C
C$ Particulars
C
C     See the CK Required Reading file for a detailed description of
C     the structure of a type 2 pointing segment.
C
C     This routine searches a type 2 segment and determines if the
C     request for pointing can be satisfied by the segment.  If so,
C     then it returns information in the array RECORD that CKE02 uses
C     to evaluate the pointing at the time for which pointing was found.
C
C     When the time for which pointing was requested falls within one
C     of the intervals then the returned time is the same as the
C     requested time. However, when the request time is not within any
C     of the intervals then the returned time will be the interval
C     endpoint closest to the request time, provided that endpoint is
C     within the tolerance specified by the user.
C
C
C$ Examples
C
C     The CKRnn routines are usually used in tandem with the CKEnn
C     routines, which evaluate the record returned by CKRnn to give
C     the pointing information and output time.
C
C     The following code fragment searches through a file (attached to
C     HANDLE) for all segments applicable to the Voyager 2 wide angle
C     camera, for a particular spacecraft clock time, that are of data
C     types 1 or 2. It then evaluates the pointing for that epoch and
C     prints the result.
C
C
C           SC     = -32
C           INST   = -32002
C     C
C     C     Load the Voyager 2 spacecraft clock kernel and the C-kernel.
C     C
C           CALL FURNSH ( 'VGR_SCLK.TSC'        )
C           CALL DAFOPR ( 'VGR2_CK.BC',  HANDLE )
C     C
C     C     Get the spacecraft clock time. Must encode it for use
C     C     in the C-kernel.
C     C
C           WRITE (*,*) 'Enter spacecraft clock time string:'
C           READ (*,FMT='(A)') SCLKCH
C           CALL SCENCD ( SC, SCLKCH, SCLKDP )
C
C     C
C     C     Search from the beginning through all segments.
C     C
C           CALL DAFBFS ( HANDLE )
C           CALL DAFFNA ( SFND   )
C
C           DO WHILE ( SFND )
C
C              CALL DAFGN ( IDENT                 )
C              CALL DAFGS ( DESCR                 )
C              CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
C
C              IF ( INST          .EQ. ICD( 1 )  .AND.
C          .        SCLKDP + TOL  .GE. DCD( 1 )  .AND.
C          .        SCLKDP - TOL  .LE. DCD( 2 ) ) THEN
C
C                 DTYPE = ICD ( 3 )
C
C                 IF ( DTYPE .EQ. 1 ) THEN
C
C                    CALL CKR01 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
C          .                      RECORD, FOUND                       )
C
C                    IF ( FOUND ) THEN
C                       CALL CKE01 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
C                    END IF
C
C                 ELSE  IF ( DTYPE .EQ. 2 ) THEN
C
C                    CALL CKR02 ( HANDLE, DESCR, SCLKDP, TOL,
C          .                      RECORD, FOUND )
C
C                    IF ( FOUND ) THEN
C                       CALL CKE02 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
C                    END IF
C
C                 END IF
C
C                 IF ( FOUND ) THEN
C
C                    WRITE (*,*) 'Segment descriptor and identifier:'
C                    WRITE (*,*) DCD, ICD
C                    WRITE (*,*) IDENT
C
C                    WRITE (*,*) 'C-matrix:'
C                    WRITE (*,*) CMAT
C
C                 END IF
C
C              END IF
C
C              CALL DAFFNA ( SFND )
C
C           END DO
C
C$ Restrictions
C
C     1) The file containing the segment should be opened for read,
C        either by CKLPF or DAFOPR.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     J.M. Lynch     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.1, 22-AUG-2006 (EDW)
C
C        Replaced references to LDPOOL with references 
C        to FURNSH.
C
C-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
C
C        Replaced DAFRDA call with DAFGDA.
C        Added IMPLICIT NONE.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 30-AUG-1991 (JML)
C
C-&
 
C$ Index_Entries
C
C     read ck type_2 pointing data record
C
C-&
 
 
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      INTEGER               LSTLED
 
C
C     Local parameters
C
C        DIRSIZ     is the directory size.
C
C        NDC        is the number of double precision components in an
C                   unpacked C-kernel segment descriptor.
C
C        NIC        is the number of integer components in an unpacked
C                   C-kernel segment descriptor.
C
C        PSIZ       is the number of double precision numbers making up
C                   the record containing the quaternion, angular
C                   velocity vector, and seconds per tick rate.
C
C        DTYPE      is the data type of the segment that this routine
C                   operates on.
C
C
      INTEGER               DIRSIZ
      PARAMETER           ( DIRSIZ = 100 )
 
      INTEGER               NDC
      PARAMETER           ( NDC    = 2 )
 
      INTEGER               NIC
      PARAMETER           ( NIC    = 6 )
 
      INTEGER               PSIZ
      PARAMETER           ( PSIZ   = 8 )
 
      INTEGER               DTYPE
      PARAMETER           ( DTYPE =  2 )
 
C
C     Local variables
C
      INTEGER               ICD    ( NIC )
      INTEGER               BEG
      INTEGER               END
      INTEGER               NREC
      INTEGER               NDIR
      INTEGER               GROUP
      INTEGER               DIRLOC
      INTEGER               REMAIN
      INTEGER               SKIP
      INTEGER               GRPNDX
      INTEGER               INDEX
      INTEGER               STPLOC
      INTEGER               ARRSIZ
      INTEGER               I
      INTEGER               N
 
      DOUBLE PRECISION      DCD    ( NDC    )
      DOUBLE PRECISION      BUFFER ( DIRSIZ )
      DOUBLE PRECISION      PREC   ( PSIZ   )
      DOUBLE PRECISION      START
      DOUBLE PRECISION      CLKOUT
      DOUBLE PRECISION      STOPI
      DOUBLE PRECISION      DIFF1
      DOUBLE PRECISION      DIFF2
 
      LOGICAL               FND
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKR02' )
      END IF
 
C
C     To minimize the number of file reads performed during the search,
C     a buffer of 100 double precision numbers is used to read the SCLK
C     times from the C-kernel.  If there are 10,001 or fewer pointing
C     records, at most four reads will be needed to satisfy the request:
C     one to read in 100 or fewer directory times, one to read 100 or
C     fewer interval start times, one to read from the stop times, and
C     then, after the appropriate record has been located, one to read
C     the pointing record.
C
C     One more read would be required for every other group of 10,000
C     records in the segment.
C
 
 
C
C     Start off with FOUND equal to false.
C
      FOUND = .FALSE.
C
C     We need to look at a few of the descriptor components.
C
C     The unpacked descriptor contains the following information
C     about the segment:
C
C        DCD(1)  Initial encoded SCLK
C        DCD(2)  Final encoded SCLK
C        ICD(1)  Instrument
C        ICD(2)  Inertial reference frame
C        ICD(3)  Data type
C        ICD(4)  Angular velocity flag
C        ICD(5)  Initial address of segment data
C        ICD(6)  Final address of segment data
C
 
      CALL DAFUS ( DESCR, NDC, NIC, DCD, ICD )
 
C
C     Check to make sure that the segment is type 2.
C
      IF ( ICD( 3 ) .NE. DTYPE ) THEN
 
         CALL SETMSG ( 'The segment is not a type 2 segment.  ' //
     .                 'Type is #'                                 )
         CALL ERRINT ( '#', ICD(3)                                 )
         CALL SIGERR ( 'SPICE(WRONGDATATYPE)'                      )
         CALL CHKOUT ( 'CKR02'                                     )
         RETURN
 
      END IF
 
C
C     The beginning and ending addresses of the segment are in the
C     descriptor.
C
      BEG = ICD( 5 )
      END = ICD( 6 )
 
C
C     Get the number of records in this segment, and from that determine
C     the number of directory epochs.
C
C
C        Based on the structure of a type 2 segment, the size of a
C        segment with N pointing intervals is given as follows:
C
C           ARRSIZ  =  PSIZ * N  +  2 * N  +  ( N-1 ) / 100       (1)
C
C        In the above equation PSIZ is eight and integer arithmetic is
C        used.  This equation is equivalent to:
C
C
C           100 * ARRSIZ  =  1000 * N  + ( N-1 ) * 100            (2)
C                                        -------
C                                          100
C
C        If we can eliminate the integer division then, since all of
C        the other values represent whole numbers, we can solve the
C        equation for N in terms of ARRSIZ by using double precision
C        arithmetic and then rounding the result to the nearest integer.
C
C        This next equation uses double precision arithmetic and is
C        equivalent to (2):
C
C           100 * ARRSIZ  = 1000 * N + ( N-1 ) - ( N-1 ) MOD 100  (3)
C
C        Which means:
C
C           100 * ARRSIZ + 1     ( N-1 ) MOD 100
C           ----------------  +  ---------------   =   N          (4)
C                1001                 1001
C
C         Since the second term on the left side of (4) is always less
C         than 0.1, the first term will always round to the correct
C         value of N.
C
      ARRSIZ = END - BEG + 1
 
      NREC   = NINT (  ( 100.D0 * (DBLE(ARRSIZ)) + 1.D0 ) / 1001.D0  )
 
      NDIR   = ( NREC - 1 ) / DIRSIZ
 
C
C     The directory epochs narrow down the search to a group of DIRSIZ
C     or fewer records.
C
C     There is only one group if there are no directory epochs.
C
      IF ( NDIR .EQ. 0 ) THEN
         GROUP = 1
 
      ELSE
C
C        Compute the location of the first directory epoch.  From the
C        beginning of the segment, we need to go through all of the
C        pointing numbers (PSIZ*NREC of them), then through all of
C        the SCLK start and stop times (2*NREC more) to get to the
C        first SCLK directory.
C
         DIRLOC = BEG + ( PSIZ + 2 ) * NREC
 
C
C        Locate the last directory epoch less than or equal to SCLKDP.
C
C        Read in as many as DIRSIZ directory epochs at a time for
C        comparison.
C
         FND    = .FALSE.
         REMAIN =  NDIR
         GROUP  =  0
 
         DO WHILE ( .NOT. FND )
C
C           The number of records to read in the buffer.
C
            N = MIN( REMAIN, DIRSIZ )
 
            CALL DAFGDA ( HANDLE, DIRLOC, DIRLOC + N - 1, BUFFER )
 
            REMAIN = REMAIN - N
 
C
C           Determine the last directory element in BUFFER that's less
C           than or equal to SCLKDP.
C
C           If we reach the end of the directories, and still haven't
C           found one bigger than the epoch, the group is the last group
C           in the segment.
C
C           Otherwise keep looking.
C
            I = LSTLED( SCLKDP, N, BUFFER )
 
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
C     Now we know which group of DIRSIZ (or less) times to look at.
C     Out of the NREC START times, the number that we should skip over
C     to get to the proper group is DIRSIZ*( GROUP - 1 ).
C
      SKIP = DIRSIZ * ( GROUP - 1 )
 
C
C     From this we can compute the index into the segment of the group
C     of times we want.  From the beginning, we need to pass through
C     PSIZ*NREC pointing numbers to get to the first START time.
C     Then we skip over the number just computed above.
C
      GRPNDX = BEG + NREC * ( PSIZ ) + SKIP
 
C
C     The number of times that we have to look at may be less than
C     DIRSIZ.  However many there are, go ahead and read them into the
C     buffer.
C
      N = MIN( DIRSIZ, NREC - SKIP )
 
      CALL DAFGDA ( HANDLE, GRPNDX, GRPNDX + N - 1, BUFFER )
 
C
C     Find the largest time in the group less than or equal to the input
C     time.
C
      I = LSTLED ( SCLKDP, N, BUFFER )
 
C
C     If the request time does not fall into one of the intervals, then
C     there are several cases in which this routine can return an
C     endpoint of an interval.
C
C        1)  If I = 0 then the request time falls before the first START
C            time in the group.  Because of the way that the directory
C            is constructed we already know that the preceding STOP
C            time is not the right one so all we have to check is if
C            SCLKDP + TOL is greater than or equal to the first START
C            time of the group.
C
C        2)  If I = N and the request time is not in the Nth interval
C            then we know that the request time is after the last STOP
C            time in the group.  Because of the way that the directory
C            is constructed we already know that the following START
C            time is not the right one so all we have to check is if
C            SCLKDP - TOL is less than or equal to the last STOP time
C            of the group.
C
C        3)  Finally, if I is between 1 and N-1 and the request time
C            does not fall in any of the intervals then we need to
C            return the closer of STOP(I) or START(I+1) if it is
C            within TOL of SCLKDP.
C
C
C     If SCLKDP is less than the first time in BUFFER then check to see
C     if we want the first START time in the group.
C
      IF ( I .EQ. 0 ) THEN
 
         IF ( ( SCLKDP + TOL ) .GE. BUFFER(1) ) THEN
 
            FOUND  = .TRUE.
            START  =  BUFFER(1)
            CLKOUT =  BUFFER(1)
            INDEX  =  1
 
         ELSE
 
            CALL CHKOUT ( 'CKR02' )
            RETURN
 
         END IF
 
      ELSE
 
C
C        I is not equal to zero. Determine if the request time falls
C        within the Ith interval.
C
         STPLOC = BEG + NREC*( PSIZ + 1 ) + SKIP + I - 1
 
         CALL DAFGDA ( HANDLE, STPLOC, STPLOC, STOPI )
 
         IF ( SCLKDP .LE. STOPI )  THEN
 
            FOUND  = .TRUE.
            START  =  BUFFER(I)
            CLKOUT =  SCLKDP
            INDEX  =  I
 
         ELSE
 
C
C           The request time does not fall within the interval. Check
C           to see if the Ith STOP time or the (I+1)th START time
C           satisfy the request.
C
C           If I = N then we need to consider only the STOP time
C           because of the way that the directory is constructed.
C
            IF ( I .EQ. N ) THEN
 
               IF ( ( SCLKDP - TOL ) .LE. STOPI ) THEN
 
                  FOUND  = .TRUE.
                  START  =  BUFFER(I)
                  CLKOUT =  STOPI
                  INDEX  =  I
 
               ELSE
 
                  CALL CHKOUT ( 'CKR02' )
                  RETURN
 
               END IF
 
            ELSE
 
C
C              Find which time SCLKDP is closest to and then see if
C              it is within the tolerance.
C
               DIFF1 = SCLKDP      - STOPI
               DIFF2 = BUFFER(I+1) - SCLKDP
 
               IF ( MIN ( DIFF1, DIFF2 ) .LE. TOL ) THEN
 
                  FOUND = .TRUE.
C
C                 Notice that if the request time is equidistant from
C                 the STOP and START time the START time will be chosen.
C
                  IF ( DIFF2 .LE. DIFF1 ) THEN
 
                     START  = BUFFER(I+1)
                     CLKOUT = BUFFER(I+1)
                     INDEX  = I+1
 
                  ELSE
 
                     START  = BUFFER(I)
                     CLKOUT = STOPI
                     INDEX  = I
 
                  END IF
 
               ELSE
 
                  CALL CHKOUT ( 'CKR02' )
                  RETURN
 
               END IF
 
            END IF
 
         END IF
 
      END IF
C
C
C     Now we know the exact record that we want and can begin
C     constructing the output record.
C
C     RECORD( 1 ) holds the interval start time.
C     RECORD( 2 ) holds the time for which pointing was found (CLKOUT).
C
      RECORD( 1 ) =  START
      RECORD( 2 ) =  CLKOUT
 
C
C     We need the pointing record out of GROUP indexed by INDEX.
C     This group of size DIRSIZ is SKIP records into the beginning
C     of the segment. And each record is PSIZ big.
C
      N = BEG + PSIZ * ( SKIP + INDEX - 1 )
 
      CALL DAFGDA ( HANDLE, N, N + PSIZ - 1, PREC )
 
      RECORD ( 3 ) = PREC ( PSIZ )
 
      CALL VEQUG ( PREC, 7, RECORD( 4 ) )
 
 
C
C     That is all.
C
      CALL CHKOUT ( 'CKR02' )
      RETURN
      END
