C$Procedure      CKR03 ( C-kernel, read pointing record, data type 3 )
 
      SUBROUTINE CKR03 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
     .                   RECORD, FOUND )
      IMPLICIT NONE
 
C$ Abstract
C
C     Read a pointing record from a CK segment, data type 3.
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
      LOGICAL               NEEDAV
      DOUBLE PRECISION      RECORD ( * )
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     DESCR      I   Segment descriptor.
C     SCLKDP     I   Pointing request time.
C     TOL        I   Time tolerance.
C     NEEDAV     I   Angular velocity request flag.
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
C     RECORD     is the record that CKE03 will evaluate to determine
C                the pointing.
C
C                When the request time falls within an interval for
C                which linear interpolation is valid, the values of
C                the two pointing instances that bracket the request
C                time are returned in RECORD as follows:
C
C                   RECORD( 1  ) = Left bracketing SCLK time.
C
C                   RECORD( 2  ) = lq0  \
C                   RECORD( 3  ) = lq1   \    Left bracketing
C                   RECORD( 4  ) = lq2   /      quaternion.
C                   RECORD( 5  ) = lq3  /
C
C                   RECORD( 6  ) = lav1 \     Left bracketing
C                   RECORD( 7  ) = lav2       angular velocity
C                   RECORD( 8  ) = lav3 /       ( optional )
C
C                   RECORD( 9  ) = Right bracketing SCLK time.
C
C                   RECORD( 10 ) = rq0  \
C                   RECORD( 11 ) = rq1   \    Right bracketing
C                   RECORD( 12 ) = rq2   /       quaternion.
C                   RECORD( 13 ) = rq3  /
C
C                   RECORD( 14 ) = rav1 \     Right bracketing
C                   RECORD( 15 ) = rav2       angular velocity
C                   RECORD( 16 ) = rav3 /       ( optional )
C
C                   RECORD( 17 ) = pointing request time, SCLKDP.
C
C                The quantities lq0 - lq3 and rq0 - rq3 are the
C                components of the quaternions that represent the
C                C-matrices associated with the times that bracket
C                the requested time.
C
C                The quantities lav1, lav2, lav3 and rav1, rav2, rav3
C                are the components of the angular velocity vectors at
C                the respective bracketing times. The components of the
C                angular velocity vectors are specified relative to
C                the inertial reference frame of the segment.
C
C                If the request time does not fall within an
C                interpolation interval, but is within TOL of an
C                interval endpoint, the values of that pointing
C                instance are returned in both parts of RECORD
C                ( i.e. RECORD(1-9) and RECORD(10-16) ).
C
C     FOUND      is true if a record was found to satisfy the pointing
C                request.  This occurs when the time for which pointing
C                is requested falls inside one of the interpolation
C                intervals, or when the request time is within the
C                tolerance of an interval endpoint.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the specified handle does not belong to an open DAF file,
C         an error is diagnosed by a routine that this routine calls.
C
C     2)  If DESCR is not a valid descriptor of a segment in the CK
C         file specified by HANDLE, the results of this routine are
C         unpredictable.
C
C     3)  If the segment is not of data type 3, as specified in the
C         third integer component of the segment descriptor, then
C         the error SPICE(WRONGDATATYPE) is signalled.
C
C     4)  If angular velocity data was requested but the segment
C         contains no such data, the error SPICE(NOAVDATA) is signalled.
C
C$ Files
C
C     The file containing the segment is specified by its handle and
C     should be opened for read or write access, either by CKLPF,
C     DAFOPR, or DAFOPW.
C
C$ Particulars
C
C     See the CK Required Reading file for a detailed description of
C     the structure of a type 3 pointing segment.
C
C     When the time for which pointing was requested falls within an
C     interpolation interval, then FOUND will be true and RECORD will
C     contain the pointing instances in the segment that bracket the
C     request time.  CKE03 will evaluate RECORD to give pointing at
C     the request time.
C
C     However, when the request time is not within any of the
C     interpolation intervals, then FOUND will be true only if the
C     interval endpoint closest to the request time is within the
C     tolerance specified by the user.  In this case both parts of
C     RECORD will contain this closest pointing instance, and CKE03
C     will evaluate RECORD to give pointing at the time associated
C     with the returned pointing instance.
C
C$ Examples
C
C     The CKRnn routines are usually used in tandem with the CKEnn
C     routines, which evaluate the record returned by CKRnn to give
C     the pointing information and output time.
C
C     The following code fragment searches through all of the segments
C     in a file applicable to the Mars Observer spacecraft bus that
C     are of data type 3, for a particular spacecraft clock time.
C     It then evaluates the pointing for that epoch and prints the
C     result.
C
C           CHARACTER*(20)        SCLKCH
C           CHARACTER*(20)        SCTIME
C           CHARACTER*(40)        IDENT
C
C           INTEGER               I
C           INTEGER               SC
C           INTEGER               INST
C           INTEGER               HANDLE
C           INTEGER               DTYPE
C           INTEGER               ICD      (    6 )
C
C           DOUBLE PRECISION      SCLKDP
C           DOUBLE PRECISION      TOL
C           DOUBLE PRECISION      CLKOUT
C           DOUBLE PRECISION      DESCR    (    5 )
C           DOUBLE PRECISION      DCD      (    2 )
C           DOUBLE PRECISION      RECORD   (   17 )
C           DOUBLE PRECISION      CMAT     ( 3, 3 )
C           DOUBLE PRECISION      AV       (    3 )
C
C           LOGICAL               NEEDAV
C           LOGICAL               FND
C           LOGICAL               SFND
C
C
C           SC     = -94
C           INST   = -94000
C           DTYPE  =  3
C           NEEDAV = .FALSE.
C
C     C
C     C     Load the MO SCLK kernel and the C-kernel.
C     C
C           CALL FURNSH ( 'MO_SCLK.TSC'       )
C           CALL DAFOPR ( 'MO_CK.BC',  HANDLE )
C     C
C     C     Get the spacecraft clock time. Then encode it for use
C     C     in the C-kernel.
C     C
C           WRITE (*,*) 'Enter spacecraft clock time string:'
C           READ (*,FMT='(A)') SCLKCH
C
C           CALL SCENCD ( SC, SCLKCH, SCLKDP )
C     C
C     C     Use a tolerance of 2 seconds ( half of the nominal
C     C     separation between MO pointing instances ).
C     C
C           CALL SCTIKS ( SC, '0000000002:000', TOL )
C
C     C
C     C     Search from the beginning of the CK file through all
C     C     of the segments.
C     C
C           CALL DAFBFS ( HANDLE )
C           CALL DAFFNA ( SFND   )
C
C           FND    = .FALSE.
C
C           DO WHILE ( ( SFND ) .AND. ( .NOT. FND ) )
C
C     C
C     C        Get the segment identifier and descriptor.
C     C
C
C              CALL DAFGN ( IDENT                 )
C              CALL DAFGS ( DESCR                 )
C     C
C     C        Unpack the segment descriptor into its integer and
C     C        double precision components.
C     C
C              CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
C
C     C
C     C        Determine if this segment should be processed.
C     C
C              IF ( ( INST          .EQ. ICD( 1 ) ) .AND.
C          .        ( SCLKDP + TOL  .GE. DCD( 1 ) ) .AND.
C          .        ( SCLKDP - TOL  .LE. DCD( 2 ) ) .AND.
C          .        ( DTYPE         .EQ. ICD( 3 ) )      ) THEN
C
C
C                 CALL CKR03 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
C          .                   RECORD, FND )
C
C                 IF ( FND ) THEN
C
C                    CALL CKE03 (NEEDAV,RECORD,CMAT,AV,CLKOUT)
C
C                    CALL SCDECD ( SC, CLKOUT, SCTIME )
C
C                    WRITE (*,*)
C                    WRITE (*,*) 'Segment identifier: ', IDENT
C                    WRITE (*,*)
C                    WRITE (*,*) 'Pointing returned for time: ',
C          .                      SCTIME
C                    WRITE (*,*)
C                    WRITE (*,*) 'C-matrix:'
C                    WRITE (*,*)
C                    WRITE (*,*) ( CMAT(1,I), I = 1, 3 )
C                    WRITE (*,*) ( CMAT(2,I), I = 1, 3 )
C                    WRITE (*,*) ( CMAT(3,I), I = 1, 3 )
C                    WRITE (*,*)
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
C     1) The file containing the segment should be opened for read
C        or write access either by CKLPF, DAFOPR, or DAFOPW.
C
C     2) The record returned by this routine is intended to be
C        evaluated by CKE03.
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
C-    SPICELIB Version 1.0.0, 25-NOV-1992 (JML)
C
C-&
 
C$ Index_Entries
C
C     read ck type_3 pointing data record
C
C-&
 
 
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      LOGICAL               FAILED
 
      INTEGER               LSTLTD
      INTEGER               LSTLED
 
      DOUBLE PRECISION      DPMAX
 
C
C     Local parameters
C
C        DIRSIZ     is the directory size.
C
C        BUFSIZ     is the maximum number of double precision numbers
C                   that we will read from the DAF file at one time.
C                   BUFSIZ is normally set equal to DIRSIZ.
C
C        ND         is the number of double precision components in an
C                   unpacked C-kernel segment descriptor.
C
C        NI         is the number of integer components in an unpacked
C                   C-kernel segment descriptor.
C
C        QSIZ       is the number of double precision numbers making up
C                   the quaternion portion of a pointing record.
C
C        QAVSIZ     is the number of double precision numbers making up
C                   the quaternion and angular velocity portion of a
C                   pointing record.
C
C        DTYPE      is the data type of the segment that this routine
C                   operates on.
C
C
      INTEGER               DIRSIZ
      PARAMETER           ( DIRSIZ = 100 )
 
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 100 )
 
      INTEGER               ND
      PARAMETER           ( ND     = 2 )
 
      INTEGER               NI
      PARAMETER           ( NI     = 6 )
 
      INTEGER               QSIZ
      PARAMETER           ( QSIZ   = 4 )
 
      INTEGER               QAVSIZ
      PARAMETER           ( QAVSIZ = 7 )
 
      INTEGER               DTYPE
      PARAMETER           ( DTYPE =  3 )
 
C
C     Local variables
C
      INTEGER               ICD    ( NI )
      INTEGER               BEG
      INTEGER               END
      INTEGER               LBEG
      INTEGER               LEND
      INTEGER               LHAND
      INTEGER               NUMREC
      INTEGER               NUMINT
      INTEGER               NRDIR
      INTEGER               NIDIR
      INTEGER               GROUP
      INTEGER               DIRLOC
      INTEGER               REMAIN
      INTEGER               SKIP
      INTEGER               GRPADD
      INTEGER               ADDR
      INTEGER               LADDR
      INTEGER               RADDR
      INTEGER               PSIZ
      INTEGER               I
      INTEGER               N
 
      DOUBLE PRECISION      DCD    ( ND     )
      DOUBLE PRECISION      BUFFER ( BUFSIZ )
      DOUBLE PRECISION      LSCLK
      DOUBLE PRECISION      RSCLK
      DOUBLE PRECISION      RDIFF
      DOUBLE PRECISION      LDIFF
      DOUBLE PRECISION      START
      DOUBLE PRECISION      NSTART
      DOUBLE PRECISION      PREVS
      DOUBLE PRECISION      PREVN
 
      LOGICAL               FND
 
 
C
C     Saved variables.
C
 
      SAVE                  PREVS
      SAVE                  PREVN
      SAVE                  LHAND
      SAVE                  LBEG
      SAVE                  LEND
 
 
C
C     Initial values.
C
      DATA    PREVS   / -1.D0 /
 
      DATA    PREVN   / -1.D0 /
 
      DATA    LHAND   / 0     /
 
      DATA    LBEG    / -1    /
 
      DATA    LEND    / -1    /
 
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKR03' )
      END IF
 
C
C     Start off with FOUND equal to false just in case a SPICELIB error
C     is signalled and the return mode is not set to ABORT.
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
 
      CALL DAFUS ( DESCR, ND, NI, DCD, ICD )
 
C
C     Check to make sure that the segment is type 3.
C
      IF ( ICD( 3 ) .NE. DTYPE ) THEN
 
         CALL SETMSG ( 'The segment is not a type 3 segment.  '   //
     .                 'Type is #'                                 )
         CALL ERRINT ( '#', ICD(3)                                 )
         CALL SIGERR ( 'SPICE(WRONGDATATYPE)'                      )
         CALL CHKOUT ( 'CKR03'                                     )
         RETURN
 
      END IF
 
 
C
C     Does this segment contain angular velocity?
C
      IF ( ICD ( 4 ) .EQ. 1 ) THEN
 
         PSIZ = QAVSIZ
 
      ELSE
 
         PSIZ = QSIZ
 
         IF ( NEEDAV ) THEN
 
            CALL SETMSG ( 'Segment does not contain angular velocity '//
     .                    'data.'                                      )
            CALL SIGERR ( 'SPICE(NOAVDATA)'                            )
            CALL CHKOUT ( 'CKR03'                                      )
            RETURN
 
         END IF
 
      END IF
 
C
C     The beginning and ending addresses of the segment are in the
C     descriptor.
C
      BEG = ICD( 5 )
      END = ICD( 6 )
 
 
C
C     The procedure used in finding a record to satisfy the request
C     for pointing is as follows:
C
C        1) Find the two pointing instances in the segment that bracket
C           the request time.
C
C           The pointing instance that brackets the request time on the
C           left is defined to be the one associated with the largest
C           time in the segment that is less than or equal to SCLKDP.
C
C           The pointing instance that brackets the request time on the
C           right is defined to be the one associated with the first
C           time in the segment greater than SCLKDP.
C
C           Since the times in the segment are strictly increasing the
C           left and right bracketing pointing instances are always
C           adjacent.
C
C        2) Determine if the bracketing times are in the same
C           interpolation interval.
C
C        3) If they are, then pointing at the request time may be
C           linearly interpolated from the bracketing times.
C
C        4) If the times that bracket the request time are not in the
C           same interval then, since they are adjacent in the segment
C           and since intervals begin and end at actual times, they must
C           both be interval endpoints.  Return the pointing instance
C           associated with the endpoint closest to the request time,
C           provided that it is within the tolerance.
C
 
 
C
C     Get the number of intervals and pointing instances ( records )
C     in this segment, and from that determine the number of respective
C     directory epochs.
C
      CALL DAFGDA ( HANDLE, END - 1, END, BUFFER )
 
      NUMINT  = NINT ( BUFFER ( 1 ) )
      NUMREC  = NINT ( BUFFER ( 2 ) )
 
      NIDIR   = ( NUMINT - 1 ) / DIRSIZ
      NRDIR   = ( NUMREC - 1 ) / DIRSIZ
 
C
C     Check the FAILED flag just in case HANDLE is not attached to
C     any DAF file and the error action is not set to ABORT. You need
C     need to do this only after the first call to DAFGDA.
C
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'CKR03' )
         RETURN
      END IF
 
C
C     To find the times that bracket the request time we will first
C     find the greatest directory time less than the request time.
C     This will narrow down the search to a group of DIRSIZ or fewer
C     times where the Jth group is defined to contain SCLK times
C     ((J-1)*DIRSIZ + 1) through (J*DIRSIZ).
C
C     For example if DIRSIZ = 100 then:
C
C                         group   first time #     last time #
C                         -----  ---------------   ------------
C                           1            1             100
C                           2          101             200
C                           .            .               .
C                           .            .               .
C                          10          901            1000
C                           .            .               .
C                           .            .               .
C                     NRDIR+1     (NRDIR)*100+1     NUMREC
C
C
C     Thus if the Ith directory time is the largest one less than
C     our request time SCLKDP, then we know that:
C
C       SCLKS ( DIRSIZ * I ) <  SCLKDP  <= SCLKS ( DIRSIZ * (I+1) )
C
C     where SCLKS is taken to be the array of NUMREC times associated
C     with the pointing instances.
C
C     Therefore, at least one of the bracketing times will come from
C     the (I+1)th group.
C
 
C
C     There is only one group if there are no directory epochs.
C
      IF ( NRDIR .EQ. 0 ) THEN
 
         GROUP = 1
 
      ELSE
C
C        Compute the location of the first directory epoch.  From the
C        beginning of the segment, we need to go through all of the
C        pointing numbers (PSIZ*NUMREC of them) and then through all of
C        the NUMREC SCLK times.
C
         DIRLOC = BEG + ( PSIZ + 1 ) * NUMREC
C
C        Search through the directory times.  Read in as many as BUFSIZ
C        directory epochs at a time for comparison.
C
         FND    = .FALSE.
         REMAIN =  NRDIR
         GROUP  =  0
 
         DO WHILE ( .NOT. FND )
C
C           The number of records to read into the buffer.
C
            N = MIN( REMAIN, BUFSIZ )
 
            CALL DAFGDA ( HANDLE, DIRLOC, DIRLOC + N - 1, BUFFER )
 
            REMAIN = REMAIN - N
C
C           Determine the last directory element in BUFFER that's less
C           than SCLKDP.
C
            I = LSTLTD( SCLKDP, N, BUFFER )
 
            IF ( I .LT. N ) THEN
 
               GROUP =  GROUP + I + 1
               FND   = .TRUE.
 
            ELSE IF ( REMAIN .EQ. 0 ) THEN
C
C              The request time is greater than the last directory time
C              so we want the last group in the segment.
C
               GROUP =  NRDIR + 1
               FND   = .TRUE.
 
            ELSE
C
C              Need to read another block of directory times.
C
               DIRLOC = DIRLOC + N
               GROUP  = GROUP  + N
 
            END IF
 
         END DO
 
      END IF
 
C
C     Now we know which group of DIRSIZ (or less) times to look at.
C     Out of the NUMREC SCLK times, the number that we should skip over
C     to get to the proper group is DIRSIZ * ( GROUP - 1 ).
C
      SKIP = DIRSIZ * ( GROUP - 1 )
 
C
C     From this we can compute the address in the segment of the group
C     of times we want.  From the beginning, we need to pass through
C     PSIZ * NUMREC pointing numbers to get to the first SCLK time.
C     Then we skip over the number just computed above.
C
      GRPADD = BEG  +  ( NUMREC * PSIZ )  +  SKIP
 
C
C     The number of times that we have to look at may be less than
C     DIRSIZ.  However many there are, go ahead and read them into the
C     buffer.
C
      N = MIN ( DIRSIZ, NUMREC - SKIP )
 
      CALL DAFGDA ( HANDLE, GRPADD, GRPADD + N - 1, BUFFER )
 
C
C     Find the largest time in the group less than or equal to the input
C     time.
C
      I = LSTLED ( SCLKDP, N, BUFFER )
 
C
C     Find the pointing instances in the segment that bracket the
C     request time and calculate the addresses for the pointing data
C     associated with these times. For cases in which the request time
C     is equal to one of the times in the segment, that time will be
C     the left bracketing time of the returned pair.
C
C     Need to handle the cases when the request time is greater than
C     the last or less than the first time in the segment separately.
C
 
      IF ( I .EQ. 0 ) THEN
 
         IF ( GROUP .EQ. 1 ) THEN
C
C           The time occurs before the first time in the segment. Since
C           this time cannot possibly be in any of the intervals, the
C           first time can satisfy the request for pointing only if it
C           is within the tolerance of the request time.
C
            IF ( ( BUFFER(1) - SCLKDP ) .LE. TOL ) THEN
 
               RECORD ( 1 ) = BUFFER(1)
               RECORD ( 9 ) = BUFFER(1)
C
C              Calculate the address of the quaternion and angular
C              velocity data.  Then read it from the file.
C
               CALL DAFGDA ( HANDLE, BEG, BEG + PSIZ - 1, BUFFER )
 
               CALL MOVED ( BUFFER, PSIZ, RECORD ( 2  ) )
               CALL MOVED ( BUFFER, PSIZ, RECORD ( 10 ) )
 
               RECORD ( 17 ) = SCLKDP
 
               FOUND = .TRUE.
 
            END IF
 
            CALL CHKOUT ( 'CKR03' )
            RETURN
 
         ELSE
 
C
C           The first time in the current group brackets the request
C           time on the right and the last time from the preceding
C           group brackets on the left.
C
            RSCLK = BUFFER ( 1 )
 
            RADDR = BEG  +  SKIP * PSIZ
 
            CALL DAFGDA ( HANDLE, GRPADD - 1, GRPADD - 1, LSCLK )
 
            LADDR = RADDR - PSIZ
 
         END IF
 
      ELSE  IF ( I .EQ. N ) THEN
 
C
C        There are two possible cases, but the same action can handle
C        both.
C
C        1) If this is the last group ( NRDIR + 1 ) then the request
C           time occurs on or after the last time in the segment.
C           In either case this last time can satisfy the request for
C           pointing only if it is within the tolerance of the request
C           time.
C
C        2) The request time is greater than or equal to the last time
C           in this group. Since this time is the same as the (I+1)th
C           directory time, and since the search on the directory times
C           used a strictly less than test, we know that the request
C           time must be equal to this time.  Just return the pointing
C           instance associated with the request time.  ( Note that
C           SCLKDP - BUFFER(N) will be zero in this case. )
C
 
         IF ( ( SCLKDP - BUFFER(N) ) .LE. TOL ) THEN
 
            RECORD ( 1 ) = BUFFER(N)
            RECORD ( 9 ) = BUFFER(N)
C
C           Calculate the address of the quaternion and angular
C           velocity data.  Then read it from the file.
C
            ADDR = BEG + PSIZ * ( SKIP + N - 1 )
 
            CALL DAFGDA ( HANDLE, ADDR, ADDR + PSIZ - 1, BUFFER )
 
            CALL MOVED ( BUFFER, PSIZ, RECORD ( 2  ) )
            CALL MOVED ( BUFFER, PSIZ, RECORD ( 10 ) )
 
            RECORD ( 17 ) = SCLKDP
 
            FOUND = .TRUE.
 
         END IF
 
         CALL CHKOUT ( 'CKR03' )
         RETURN
 
      ELSE
 
C
C        The bracketing times are contained in this group.
C
         LSCLK = BUFFER ( I     )
         RSCLK = BUFFER ( I + 1 )
 
         LADDR = BEG  +  ( SKIP + I - 1 ) * PSIZ
 
         RADDR = LADDR + PSIZ
 
      END IF
 
 
C
C     At this point we have the two times in the segment that bracket
C     the request time.  We also have the addresses of the pointing
C     data associated with those times. The task now is to determine
C     if the bracketing times fall in the same interval.  If so then
C     we can interpolate between them.  If they don't then return
C     pointing for whichever of the two times is closest to the
C     request time, provided that it is within the tolerance.
C
 
C
C     Find the interpolation interval that the request time is in and
C     determine if the bracketing SCLK's are both in it.
C
C     First check if the request time falls in the same interval as
C     it did last time.  We need to make sure that we are dealing
C     with the same segment as well as the same time range.
C
C
C     PREVS      is the start time of the interval that satisfied
C                the previous request for pointing.
C
C     PREVN      is the start time of the interval that followed
C                the interval specified above.
C
C     LHAND      is the handle of the file that PREVS and PREVN
C                were found in.
C
C     LBEG,      are the beginning and ending addresses of the
C     LEND       segment in the file LHAND that PREVS and PREVN
C                were found in.
C
      IF (  ( HANDLE .EQ. LHAND ) .AND.
     .      ( BEG    .EQ. LBEG  ) .AND.
     .      ( END    .EQ. LEND  ) .AND.
     .      ( SCLKDP .GE. PREVS ) .AND.
     .      ( SCLKDP .LT. PREVN )      ) THEN
 
         START  = PREVS
         NSTART = PREVN
 
      ELSE
 
C
C        The START times of all of the intervals are stored in the
C        segment and a directory of every hundredth START is also
C        stored. The procedure to find the bracketing interval start
C        times is identical to the one used above for finding the
C        bracketing times.
C
C        The directory epochs narrow down the search for the times that
C        bracket the request time to a group of DIRSIZ or fewer records.
C
 
C
C        There is only one group if there are no directory epochs.
C
         IF ( NIDIR .EQ. 0 ) THEN
 
            GROUP = 1
 
         ELSE
C
C           Compute the location of the first directory epoch.  From the
C           beginning of the segment, we need to go through all of the
C           pointing numbers (PSIZ*NUMREC of them), then through all of
C           the NUMREC SCLK times and NRDIR directory times, and then
C           finally through the NUMINT interval start times.
C
            DIRLOC = BEG + ( PSIZ + 1 ) * NUMREC  +  NRDIR  +  NUMINT
 
C
C           Locate the largest directory time less than the
C           request time SCLKDP.
C
C           Read in as many as BUFSIZ directory epochs at a time for
C           comparison.
C
            FND    = .FALSE.
            REMAIN =  NIDIR
            GROUP  =  0
 
            DO WHILE ( .NOT. FND )
C
C              The number of records to read into the buffer.
C
               N = MIN( REMAIN, BUFSIZ )
 
               CALL DAFGDA ( HANDLE, DIRLOC, DIRLOC + N - 1, BUFFER )
 
               REMAIN = REMAIN - N
C
C              Determine the last directory element in BUFFER that's
C              less than SCLKDP.
C
               I = LSTLTD ( SCLKDP, N, BUFFER )
 
               IF ( I .LT. N ) THEN
 
                  GROUP =  GROUP + I + 1
                  FND   = .TRUE.
 
               ELSE IF ( REMAIN .EQ. 0 ) THEN
C
C                 The request time is greater than the last directory
C                 time so we want the last group in the segment.
C
                  GROUP =  NIDIR + 1
                  FND   = .TRUE.
 
               ELSE
C
C                 Need to read another block of directory times.
C
                  DIRLOC = DIRLOC + N
                  GROUP  = GROUP  + N
 
               END IF
 
            END DO
 
         END IF
 
C
C        Now we know which group of DIRSIZ (or less) times to look at.
C        Out of the NUMINT SCLK START times, the number that we should
C        skip over to get to the proper group is DIRSIZ * ( GROUP - 1 ).
C
         SKIP = DIRSIZ * ( GROUP - 1 )
 
C
C        From this we can compute the address in the segment of the
C        group of times we want.  To get to the first interval start
C        time we must pass over PSIZ * NUMREC pointing numbers, NUMREC
C        SCLK times, and NRDIR SCLK directory times.  Then we skip
C        over the number just computed above.
C
 
         GRPADD = BEG  +  ( PSIZ + 1 ) * NUMREC  +  NRDIR  +  SKIP
 
C
C        The number of times that we have to look at may be less than
C        DIRSIZ.  However many there are, go ahead and read them into
C        the buffer.
C
         N = MIN ( DIRSIZ, NUMINT - SKIP )
 
         CALL DAFGDA ( HANDLE, GRPADD, GRPADD + N - 1, BUFFER )
C
C        Find the index of the largest time in the group that is less
C        than or equal to the input time.
C
         I = LSTLED ( SCLKDP, N, BUFFER )
 
         IF ( I .EQ. 0 ) THEN
C
C           The first start time in the buffer is the start of the
C           interval following the one containing the request time.
C
C           We don't need to check if GROUP = 1 because the case of
C           the request time occurring before the first time in the
C           segment has already been handled.
C
            NSTART = BUFFER (1)
 
            ADDR = GRPADD - 1
 
            CALL DAFGDA ( HANDLE, ADDR, ADDR, START )
 
         ELSE  IF ( I .EQ. N ) THEN
 
            IF ( GROUP .EQ. ( NIDIR + 1 ) ) THEN
C
C              This is the last interval in the segment.
C
               START = BUFFER ( N )
 
               NSTART = DPMAX ()
 
            ELSE
C
C              The last START time in this group is equal to the
C              request time.
C
               START = BUFFER ( N )
 
               ADDR = GRPADD + N
 
               CALL DAFGDA ( HANDLE, ADDR, ADDR, NSTART )
 
            END IF
 
         ELSE
C
C           The bracketing START times are contained in this group.
C
            START  = BUFFER ( I     )
            NSTART = BUFFER ( I + 1 )
 
         END IF
 
C
C        Save the information about the interval and segment.
C
         LHAND = HANDLE
         LBEG  = BEG
         LEND  = END
         PREVS = START
         PREVN = NSTART
 
      END IF
 
 
C
C     Check and see if the bracketing pointing instances belong
C     to the same interval.  If they do then we can interpolate
C     between them, if not then check to see if the closer of
C     the two to the request time lies within the tolerance.
C
C     The left bracketing time will always belong to the same
C     interval as the request time, therefore we need to check
C     only that the right bracketing time is less than the start
C     time of the next interval.
C
 
      IF ( RSCLK .LT. NSTART ) THEN
 
         RECORD ( 1 ) = LSCLK
 
         CALL DAFGDA ( HANDLE, LADDR, LADDR + PSIZ - 1, RECORD ( 2 ) )
 
         RECORD ( 9 ) = RSCLK
 
         CALL DAFGDA ( HANDLE, RADDR, RADDR + PSIZ - 1, RECORD ( 10 ) )
 
         RECORD ( 17 ) = SCLKDP
 
         FOUND = .TRUE.
 
      ELSE
 
         LDIFF = SCLKDP - LSCLK
         RDIFF = RSCLK  - SCLKDP
 
         IF ( ( LDIFF .LE. TOL ) .OR. ( RDIFF .LE. TOL ) ) THEN
C
C           Return the pointing instance closest to the request time.
C
C           If the request time is midway between LSCLK and RSCLK then
C           grab the pointing instance associated with the greater time.
C
            IF ( LDIFF .LT. RDIFF ) THEN
 
               RECORD ( 1 ) = LSCLK
               RECORD ( 9 ) = LSCLK
 
               CALL DAFGDA ( HANDLE, LADDR, LADDR+PSIZ-1, BUFFER )
 
               CALL MOVED ( BUFFER, PSIZ, RECORD ( 2  ) )
               CALL MOVED ( BUFFER, PSIZ, RECORD ( 10 ) )
 
            ELSE
 
               RECORD ( 1 ) = RSCLK
               RECORD ( 9 ) = RSCLK
 
               CALL DAFGDA ( HANDLE, RADDR, RADDR+PSIZ-1, BUFFER )
 
               CALL MOVED ( BUFFER, PSIZ, RECORD ( 2  ) )
               CALL MOVED ( BUFFER, PSIZ, RECORD ( 10 ) )
 
            END IF
 
            RECORD ( 17 ) = SCLKDP
 
            FOUND = .TRUE.
 
         END IF
 
      END IF
 
      CALL CHKOUT ( 'CKR03' )
 
      RETURN
      END
