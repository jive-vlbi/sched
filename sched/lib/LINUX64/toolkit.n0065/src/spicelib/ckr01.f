C$Procedure      CKR01 ( C-kernel, read pointing record, data type 1 )
 
      SUBROUTINE CKR01 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
     .                   RECORD, FOUND                       )
      IMPLICIT NONE
 
C$ Abstract
C
C     Read a pointing record from a CK segment, data type 1.
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
C     SCLKDP     I   Spacecraft clock time.
C     TOL        I   Time tolerance.
C     NEEDAV     I   True when angular velocity data is requested.
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
C     SCLKDP     is an encoded spacecraft clock time for which
C                pointing is being requested.  The SPICELIB routines
C                SCENCD and SCDECD are used to encode and decode SCLK
C                times.
C
C     TOL        is a time tolerance, measured in the same units as
C                encoded spacecraft clock.
C
C                The record returned by CKR01 is the one whose time is
C                closest to SCLKDP and within TOL units of SCLKDP.
C
C     NEEDAV     is true when angular velocity data is requested.
C
C
C$ Detailed_Output
C
C     RECORD     is the pointing record.  Contents are as follows:
C
C                   RECORD( 1 ) = CLKOUT
C
C                   RECORD( 2 ) = q0
C                   RECORD( 3 ) = q1
C                   RECORD( 4 ) = q2
C                   RECORD( 5 ) = q3
C
C                   RECORD( 6 ) = Av1  ]
C                   RECORD( 7 ) = Av2  |-- Returned optionally
C                   RECORD( 8 ) = Av3  ]
C
C                CLKOUT is the encoded spacecraft clock time for the
C                returned pointing values. CLKOUT will be the closest
C                time in the segment to the input time as long as it is
C                within the input tolerance (see FOUND below). If SCLKDP
C                falls at the exact midpoint of two times, the record
C                for the greater of the two will be returned.
C
C                The quantities q0 - q3 represent a quaternion.
C                The quantities Av1, Av2, and Av3 represent the angular
C                velocity vector, and are returned if the segment
C                contains angular velocity data and NEEDAV is true.
C                The components of the angular velocity vector are
C                specified relative to the inertial reference frame
C                for the segment.
C
C     FOUND      is true if a record was found to satisfy the pointing
C                request. FOUND will be false when there is no pointing
C                instance within the segment whose time falls within
C                the requested time tolerance on either side of the
C                input time.
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
C     3)  If the segment is not of data type 1, as specified in the
C         third integer component of the segment descriptor, then
C         the error SPICE(WRONGDATATYPE) is signalled.
C
C     4)  If there is a need for angular velocity data and the segment
C         contains no such data, the error SPICE(NOAVDATA) is signalled.
C
C$ Files
C
C     The file containing the segment is specified by its handle, and
C     should be opened for read, either by CKLPF or DAFOPR.
C
C$ Particulars
C
C     See the CK Required Reading file for a detailed description of
C     the structure of a type 1 pointing segment.
C
C     This routine searches a type 1 segment for the pointing instance
C     whose associated time is closest to the time that pointing was
C     requested for. If this time is within the tolerance specified by
C     the user, it sets FOUND equal to true and returns information in
C     the array RECORD that CKE01 uses to evaluate the pointing at the
C     time CLKOUT.
C
C$ Examples
C
C     The CKRnn routines are usually used in tandem with the CKEnn
C     routines, which evaluate the record returned by CKRnn to give
C     the pointing information and output time.
C
C     The following code fragment searches through a file (represented
C     by HANDLE) for all segments applicable to the Voyager 2 wide angle
C     camera, for a particular spacecraft clock time, which have data
C     type 1.  It then evaluates the pointing for that epoch and prints
C     the result.
C
C     C
C     C     - Get the spacecraft clock time. Must encode it for use
C     C       in the C-kernel.
C     C
C     C     - Set the time tolerance high to catch anything close to
C     C       the input time.
C     C
C     C     - We don't need angular velocity data.
C     C
C           SC     = -32
C           INST   = -32002
C           TOL    =  1000.D0
C           NEEDAV = .FALSE.
C           DTYPE  =  1
C     C
C     C     Load the Voyager 2 spacecraft clock kernel and the C-kernel.
C     C
C           CALL FURNSH ( 'VGR_SCLK.TSC'        )
C           CALL DAFOPR ( 'VGR2_CK.BC',  HANDLE )
C     C
C     C     Convert the input request time to ticks.
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
C          .        DTYPE         .EQ. ICD( 3 )  .AND.
C          .        SCLKDP + TOL  .GE. DCD( 1 )  .AND.
C          .        SCLKDP - TOL  .LE. DCD( 2 ) ) THEN
C
C                 CALL CKR01 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
C          .                   RECORD, FOUND )
C
C                 IF ( FOUND ) THEN
C
C                    CALL CKE01 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
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
C     J.E. McLean    (JPL)
C     M.J. Spencer   (JPL)
C     R.E. Thurman   (JPL)
C     I.M. Underwood (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.1, 22-AUG-2006 (EDW)
C
C        Replaced references to LDPOOL with references 
C        to FURNSH.
C
C-    SPICELIB Version 1.2.0, 07-SEP-2001 (EDW)
C
C        Replaced DAFRDA call with DAFGDA.
C        Added IMPLICIT NONE.
C
C-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.1.0, 30-AUG-1991 (JML)
C
C        This routine now checks the segment descriptor to
C        determine if it has been given a type 1 segment.
C
C        The FOUND flag is set to FALSE at the beginning of
C        the routine.
C
C        The particulars section was changed to provide a more
C        general description of the function of this routine. The
C        information that was originally in Particulars was moved
C        to the body of the code.
C
C        The example program was changed so that the tolerance
C        and data type are used in selecting which segments to read.
C
C-    SPICELIB Version 1.0.1, 02-NOV-1990 (JML)
C
C        The example program was corrected so that the input
C        instrument code was tested against ICD(1) instead of
C        ICD(3).
C
C-    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) (IMU)
C
C-&
 
C$ Index_Entries
C
C     read ck type_1 pointing data record
C
C-&
 
 
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 30-AUG-1991 (JML)
C
C        1) This routine now checks the segment descriptor, ICD(3),
C           to determine if it has been given a type 1 segment.
C
C        2) The FOUND flag is set to FALSE at the beginning of
C           the routine.  This is done so that if a SPICE error
C           is signalled, the FOUND flag will definitely be false.
C
C        3) The particulars section was changed to provide a more
C           general description of the function of this routine. The
C           information that was originally in Particulars was moved
C           to the body of the code.
C
C        4) The example program was changed so that the tolerance
C           and data type are used in selecting which segments to read.
C
C-    SPICELIB Version 1.0.1, 02-NOV-1990 (JML)
C
C        1) The example program was corrected so that the input
C           instrument code was tested against ICD(1) instead of
C           ICD(3).
C        2) ROTATIONS was removed from the Required Reading section.
C
C-    Beta Version 1.1.0, 29-AUG-1990 (MJS) (JEM)
C
C        The following changes were made as a result of the
C        NAIF CK Code and Documentation Review:
C
C        1) The variable SCLK was changed to SCLKDP.
C        2) The declarations for the parameters QSIZ, QAVSIZ, NDC, and
C           NIC were moved from the "Declarations" section of the
C           header to the "Local parameters" section of the code below
C           the header. These parameters are not meant to modified by
C           users.
C        3) The variable DIRSIZ has been parameterized in the code
C           following the header. DIRSIZ is still 100.
C        5) The header was improved and updated to reflect the changes.
C        6) The in-code comments were improved.
C
C-    Beta Version 1.0.0, 17-MAY-1990 (RET) (IMU)
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      INTEGER               LSTLED
      INTEGER               LSTCLD
 
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
 
      INTEGER               DIRSIZ
      PARAMETER           ( DIRSIZ = 100 )
 
      INTEGER               NDC
      PARAMETER           ( NDC    = 2 )
 
      INTEGER               NIC
      PARAMETER           ( NIC    = 6 )
 
      INTEGER               QSIZ
      PARAMETER           ( QSIZ   = 4 )
 
      INTEGER               QAVSIZ
      PARAMETER           ( QAVSIZ = 7 )
 
      INTEGER               DTYPE
      PARAMETER           ( DTYPE =  1 )
 
C
C     Local variables
C
      INTEGER               ICD    ( NIC )
      INTEGER               BEG
      INTEGER               END
      INTEGER               NREC
      INTEGER               NDIR
      INTEGER               PSIZ
      INTEGER               GROUP
      INTEGER               DIRLOC
      INTEGER               REMAIN
      INTEGER               SKIP
      INTEGER               GRPNDX
      INTEGER               I
      INTEGER               N
 
      DOUBLE PRECISION      DCD    ( NDC )
      DOUBLE PRECISION      BUFFER ( DIRSIZ )
 
      LOGICAL               FND
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKR01' )
      END IF
 
C
C     To minimize the number of file reads performed during the search,
C     a buffer of 100 double precision numbers is used to read the SCLK
C     times from the C-kernel.  If there are 10,001 or fewer pointing
C     records, at most four reads will be needed to satisfy the request:
C     one to read NREC, one to read in 100 or fewer directory times,
C     one to read 100 or fewer actual times, and then after the
C     appropriate record has been located, one to read the quaternion
C     and angular velocity data.
C
C     One more read would be required for every other group of 10,000
C     records in the segment.
C
 
C
C     Start off with FOUND set to FALSE.
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
C     Check to make sure that the segment is type 1.
C
      IF ( ICD( 3 ) .NE. DTYPE ) THEN
 
         CALL SETMSG ( 'The segment is not a type 1 segment.  ' //
     .                 'Type is #'                                 )
         CALL ERRINT ( '#', ICD(3)                                 )
         CALL SIGERR ( 'SPICE(WRONGDATATYPE)'                      )
         CALL CHKOUT ( 'CKR01'                                     )
         RETURN
 
      END IF
 
C
C     The size of the record returned depends on whether or not the
C     segment contains angular velocity data.
C
C     This is a convenient place to check if the need for angular
C     velocity data matches the availability.
C
      IF ( ICD( 4 ) .EQ. 1 ) THEN
 
         PSIZ = QAVSIZ
 
      ELSE
 
         PSIZ = QSIZ
 
         IF ( NEEDAV ) THEN
 
            CALL SETMSG ( 'Segment does not contain angular velocity '//
     .                    'data.'           )
            CALL SIGERR ( 'SPICE(NOAVDATA)' )
            CALL CHKOUT ( 'CKR01'           )
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
C     Get the number of records in this segment, and from that determine
C     the number of directory epochs.
C
      CALL DAFGDA ( HANDLE, END, END, BUFFER )
      NREC = INT( BUFFER(1) )
      NDIR = ( NREC - 1 )/DIRSIZ
 
C
C     The directory epochs narrow down the search to a group of DIRSIZ
C     or fewer records. The way the directory is constructed guarantees
C     that we will definitely find the closest time in the segment to
C     SCLKDP in the indicated group.
C
C     There is only one group if there are no directory epochs.
C
      IF ( NDIR .EQ. 0 ) THEN
         GROUP = 1
 
      ELSE
C
C        Compute the location of the first directory epoch.  From the
C        beginning of the segment, need to go through all of the
C        pointing numbers (PSIZ*NREC of them), then through all of
C        the SCLKDP times (NREC more) to get to the first SCLK
C        directory.
C
         DIRLOC = BEG + ( PSIZ + 1 ) * NREC
 
C
C        Locate the first directory epoch greater than SCLKDP. Read in
C        as many as DIRSIZ directory epochs at a time for comparison.
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
C           If we find the first directory time greater than or equal
C           to the epoch, we're done.
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
C     Out of the NREC SCLKDP times, the number that we should skip over
C     to get to the proper group is DIRSIZ*( GROUP - 1 ).
C
      SKIP = DIRSIZ*( GROUP - 1 )
 
C
C     From this we can compute the index into the segment of the group
C     of times we want.  From the beginning, need to pass through
C     PSIZ*NREC pointing numbers to get to the first SCLKDP time.
C     Then we skip over the number just computed above.
C
      GRPNDX = BEG + (NREC * PSIZ) + SKIP
 
C
C     The number of times that we have to look at may be less than
C     DIRSIZ.  However many there are, go ahead and read them into the
C     buffer.
C
      N = MIN( DIRSIZ, NREC - SKIP )
 
      CALL DAFGDA ( HANDLE, GRPNDX, GRPNDX + N - 1, BUFFER )
 
C
C     Find the time in the group closest to the input time, and see
C     if it's within tolerance.
C
      I = LSTCLD( SCLKDP, N, BUFFER )
 
      IF ( ABS( SCLKDP - BUFFER( I ) ) .GT. TOL ) THEN
 
         CALL CHKOUT ( 'CKR01' )
         RETURN
 
      END IF
 
C
C     Now we know the exact record that we want.
C
C     RECORD( 1 ) holds CLKOUT.
C
      FOUND       = .TRUE.
      RECORD( 1 ) =  BUFFER( I )
 
C
C     We need the Ith pointing record out of this group of DIRSIZ.
C     This group of DIRSIZ is SKIP records into the beginning
C     of the segment. And each record is PSIZ big.
C
      N = BEG + PSIZ*( SKIP + I - 1 )
 
      CALL DAFGDA ( HANDLE, N, N + PSIZ - 1, RECORD( 2 ) )
 
C
C     That is all.
C
      CALL CHKOUT ( 'CKR01' )
      RETURN
      END
