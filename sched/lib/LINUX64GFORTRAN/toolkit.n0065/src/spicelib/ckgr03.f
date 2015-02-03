C$Procedure      CKGR03 ( C-kernel, get record, type 03 )
 
      SUBROUTINE CKGR03 ( HANDLE, DESCR, RECNO, RECORD )
      IMPLICIT NONE
 
C$ Abstract
C
C     Given the handle and descriptor of a type 3 segment in a CK file,
C     return a specified pointing instance from that segment.
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
      DOUBLE PRECISION      DESCR   ( * )
      INTEGER               RECNO
      DOUBLE PRECISION      RECORD  ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   The handle of the file containing the segment.
C     DESCR      I   The segment descriptor.
C     RECNO      I   The number of the pointing instance to be returned.
C     RECORD     O   The pointing record.
C
C$ Detailed_Input
C
C     HANDLE     is the handle of the binary CK file containing the
C                desired segment. The file should have been opened
C                for read or write access, either by CKLPF, DAFOPR,
C                or DAFOPW.
C
C     DESCR      is the packed descriptor of the data type 3 segment.
C
C     RECNO      is the number of the discrete pointing instance to be
C                returned from the data type 3 segment.
C
C$ Detailed_Output
C
C     RECORD     is the pointing instance indexed by RECNO in the
C                segment.  The contents are as follows:
C
C                   RECORD( 1 ) = CLKOUT
C
C                   RECORD( 2 ) = q0
C                   RECORD( 3 ) = q1
C                   RECORD( 4 ) = q2
C                   RECORD( 5 ) = q3
C
C                   RECORD( 6 ) = av1  ]
C                   RECORD( 7 ) = av2  |-- Returned optionally
C                   RECORD( 8 ) = av3  ]
C
C                CLKOUT is the encoded spacecraft clock time associated
C                with the returned pointing values.
C
C                The quantities q0 - q3 are the components of the
C                quaternion that represents the C-matrix that transforms
C                vectors from the inertial reference frame of the
C                segment to the instrument frame at time CLKOUT.
C
C                The quantities av1, av2, and av3 represent the
C                angular velocity vector, and are returned only if
C                the segment contains angular velocity data. The
C                components of the angular velocity vector are
C                specified relative to the inertial reference
C                frame of the segment.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the segment is not of data type 3, the error
C         SPICE(CKWRONGDATATYPE) is signalled.
C
C     2)  If RECNO is less than one or greater than the number of
C         records in the specified segment, the error
C         SPICE(CKNONEXISTREC) is signalled.
C
C     3)  If the specified handle does not belong to any DAF file that
C         is currently known to be open, an error is diagnosed by a
C         routine that this routine calls.
C
C     4)  If DESCR is not a valid descriptor of a segment in the CK
C         file specified by HANDLE, the results of this routine are
C         unpredictable.
C
C$ Files
C
C     The file specified by HANDLE should be open for read or
C     write access.
C
C$ Particulars
C
C     For a detailed description of the structure of a type 3 segment,
C     see the CK required reading.
C
C     This is a utility routine that may be used to read the individual
C     pointing instances that make up a type 3 segment.  It is normally
C     used in conjunction with CKNR03, which gives the number of
C     pointing instances stored in a segment.
C
C$ Examples
C
C     Suppose that MOC.BC is a CK file that contains segments of
C     data type 3.  Then the following code fragment extracts the
C     SCLK time, boresight vector, and angular velocity vector for
C     each pointing instance in the first segment in the file.
C
C
C           INTEGER               ICD     ( 6 )
C           INTEGER               HANDLE
C           INTEGER               NREC
C           INTEGER               I
C
C           DOUBLE PRECISION      DCD     ( 2 )
C           DOUBLE PRECISION      DESCR   ( 5 )
C           DOUBLE PRECISION      RECORD  ( 8 )
C           DOUBLE PRECISION      QUAT    ( 4 )
C           DOUBLE PRECISION      AV      ( 3 )
C           DOUBLE PRECISION      BORE    ( 3 )
C           DOUBLE PRECISION      CMAT    ( 3, 3 )
C           DOUBLE PRECISION      SCLKDP
C
C           LOGICAL               FOUND
C           LOGICAL               AVSEG
C
C     C
C     C     First load the file. (The file may also be opened by using
C     C     CKLPF.)
C     C
C           CALL DAFOPR ( 'MOC.BC', HANDLE )
C     C
C     C     Begin forward search.  Find the first array.
C     C
C           CALL DAFBFS ( HANDLE )
C           CALL DAFFNA ( FOUND  )
C     C
C     C     Get segment descriptor.
C     C
C           CALL DAFGS ( DESCR )
C     C
C     C     Unpack the segment descriptor into its double precision
C     C     and integer components.
C     C
C           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
C     C
C     C     The data type for a segment is located in the third integer
C     C     component of the descriptor.
C     C
C           IF ( ICD( 3 ) .EQ. 3 ) THEN
C     C
C     C        Does the segment contain AV data?
C     C
C              AVSEG =  ( ICD(4) .EQ. 1 )
C     C
C     C        How many records does this segment contain?
C     C
C              CALL CKNR03 ( HANDLE, DESCR, NREC )
C
C              DO I = 1, NREC
C     C
C     C           Get the Ith pointing instance in the segment.
C     C
C                 CALL CKGR03 ( HANDLE, DESCR, I, RECORD )
C
C     C
C     C           Unpack RECORD into the time, quaternion, and av.
C     C
C                 SCLKDP = RECORD ( 1 )
C
C                 CALL MOVED ( RECORD(2), 4, QUAT )
C
C                 IF  ( AVSEG )  THEN
C                    CALL MOVED ( RECORD(6), 3, AV   )
C                 END IF
C
C     C
C     C           The boresight vector is the third row of the C-matrix.
C     C
C                 CALL Q2M ( QUAT, CMAT )
C
C                 BORE(1) = CMAT(3,1)
C                 BORE(2) = CMAT(3,2)
C                 BORE(3) = CMAT(3,3)
C     C
C     C           Write out the results.
C     C
C                 WRITE (*,*) 'Record: ', I
C                 WRITE (*,*)
C                 WRITE (*,*) 'SCLK time = ', SCLKDP
C                 WRITE (*,*)
C                 WRITE (*,*) 'boresight: ', BORE
C
C                 IF ( AVSEG ) THEN
C                    WRITE (*,*)
C                    WRITE (*,*) 'angular velocity: ', AV
C                 END IF
C
C              END DO
C
C           END IF
C
C$ Restrictions
C
C     1) The binary CK file containing the segment whose descriptor was
C        passed to this routine must be opened for read or write access
C        by either CKLPF, DAFOPR, DAFOPW.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     J.M. Lynch (JPL)
C
C$ Version
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
C     get ck type_3 record
C
C-&
 
 
 
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local parameters
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
C
 
      INTEGER               NDC
      PARAMETER           ( NDC    = 2 )
 
      INTEGER               NIC
      PARAMETER           ( NIC    = 6 )
 
      INTEGER               QSIZ
      PARAMETER           ( QSIZ   = 4 )
 
      INTEGER               QAVSIZ
      PARAMETER           ( QAVSIZ = 7 )
 
      INTEGER               DTYPE
      PARAMETER           ( DTYPE  = 3 )
 
C
C     Local variables
C
      INTEGER               ICD    ( NIC )
      INTEGER               NREC
      INTEGER               BEG
      INTEGER               END
      INTEGER               ADDR
      INTEGER               PSIZ
 
      DOUBLE PRECISION      DCD    ( NDC )
      DOUBLE PRECISION      NPOINT
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKGR03' )
      END IF
 
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
C     From the descriptor, determine
C
C       1 - Is this really a type 3 segment?
C       2 - The beginning address of the segment.
C       3 - The number of pointing instances in the segment (it's the
C           last word in the segment).
C       4 - The existence of angular velocity data, which determines how
C           big the pointing portion of the returned record will be.
C
      CALL DAFUS ( DESCR, NDC, NIC, DCD, ICD )
 
      IF ( ICD( 3 ) .NE. DTYPE ) THEN
         CALL SETMSG ( 'Data type of the segment should be 3: Passed '//
     .                 'descriptor shows type = #.'                    )
         CALL ERRINT ( '#', ICD( 3 )                                   )
         CALL SIGERR ( 'SPICE(CKWRONGDATATYPE)'                        )
         CALL CHKOUT ( 'CKGR03'                                        )
         RETURN
      END IF
 
      IF ( ICD( 4 ) .EQ. 1 ) THEN
         PSIZ = QAVSIZ
      ELSE
         PSIZ = QSIZ
      END IF
 
 
      BEG  = ICD( 5 )
      END  = ICD( 6 )
 
      CALL DAFGDA ( HANDLE, END, END, NPOINT )
 
      NREC = NINT ( NPOINT )
 
 
C
C     If a request was made for a record which doesn't exist, then
C     signal an error and leave.
C
      IF ( ( RECNO .LT. 1 ) .OR. ( RECNO .GT. NREC ) ) THEN
         CALL SETMSG ( 'Requested record number (#) does not exist. ' //
     .                 'There are # records in the segment.'           )
         CALL ERRINT ( '#', RECNO                                      )
         CALL ERRINT ( '#', NREC                                       )
         CALL SIGERR ( 'SPICE(CKNONEXISTREC)'                          )
         CALL CHKOUT ( 'CKGR03'                                        )
         RETURN
      END IF
 
C
C     Get the pointing record indexed by RECNO.
C
      ADDR = BEG + PSIZ * ( RECNO - 1 )
 
      CALL DAFGDA ( HANDLE, ADDR, ADDR + PSIZ - 1, RECORD( 2 ) )
 
C
C     Next get the SCLK time.  Need to go past all of the NREC pointing
C     records (PSIZ * NREC numbers), and then to the RECNOth SCLK
C     time.
C
      ADDR = BEG + PSIZ*NREC + RECNO - 1
 
      CALL DAFGDA ( HANDLE, ADDR, ADDR, RECORD( 1 ) )
 
      CALL CHKOUT ( 'CKGR03' )
      RETURN
      END
