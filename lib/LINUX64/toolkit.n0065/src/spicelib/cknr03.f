C$Procedure      CKNR03 ( C-kernel, number of records, type 03 )
 
      SUBROUTINE CKNR03 ( HANDLE, DESCR, NREC )
      IMPLICIT NONE
 
C$ Abstract
C
C     Given the handle of a CK file and the descriptor of a type 3
C     segment in that file, return the number of pointing instances
C     in that segment.
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
      INTEGER               NREC
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   The handle of the file containing the segment.
C     DESCR      I   The descriptor of the type 3 segment.
C     NREC       O   The number of pointing instances in the segment.
C
C$ Detailed_Input
C
C     HANDLE     is the handle of the binary CK file containing the
C                segment. The file should have been opened for read
C                or write access, either by CKLPF, DAFOPR, or DAFOPW.
C
C     DESCR      The packed descriptor of a data type 3 segment.
C
C$ Detailed_Output
C
C     NREC       The number of pointing instances in the type 3 segment.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the segment indicated by DESCR is not a type 3 segment,
C         the error 'SPICE(CKWRONGDATATYPE)' is signalled.
C
C     2)  If the specified handle does not belong to any DAF file that
C         is currently known to be open, an error is diagnosed by a
C         routine that this routine calls.
C
C     3)  If DESCR is not a valid descriptor of a segment in the CK
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
C     For a complete description of the internal structure of a type 3
C     segment, see the CK required reading.
C
C     This routine returns the number of discrete pointing instances
C     contained in the specified segment.  It is normally used in
C     conjunction with CKGR03 which returns the Ith pointing instance
C     in the segment.
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
C
C     C
C     C     Begin forward search.  Find the first array.
C     C
C           CALL DAFBFS ( HANDLE )
C           CALL DAFFNA ( FOUND  )
C
C     C
C     C     Get segment descriptor.
C     C
C           CALL DAFGS ( DESCR )
C
C     C
C     C     Unpack the segment descriptor into its double precision
C     C     and integer components.
C     C
C           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
C
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
C
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
C        by either CKLPF, DAFOPR, or DAFOPW.
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
C     number of ck type_3 records
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
C                   unpacked C-kernel descriptor.
C
C        NIC        is the number of integer components in an unpacked
C                   C-kernel descriptor.
C
C        DTYPE      is the data type of the segment that this routine
C                   operates on.
C
 
      INTEGER               NDC
      PARAMETER           ( NDC = 2 )
 
      INTEGER               NIC
      PARAMETER           ( NIC = 6 )
 
      INTEGER               DTYPE
      PARAMETER           ( DTYPE = 3 )
 
C
C     Local variables
C
      INTEGER               ICD    ( NIC )
 
      DOUBLE PRECISION      DCD    ( NDC )
      DOUBLE PRECISION      NPOINT
 
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKNR03' )
      END IF
 
 
C
C     The number of discrete pointing instances contained in a data
C     type 3 segment is stored in the last double precision word of
C     the segment.  Since the address of the last word is stored in
C     the sixth integer component of the segment descriptor, it is
C     a trivial matter to extract the count.
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
C
      CALL DAFUS ( DESCR, NDC, NIC, DCD, ICD )
 
C
C     If this segment is not of data type 3, then signal an error.
C
 
      IF ( ICD( 3 ) .NE. DTYPE ) THEN
         CALL SETMSG ( 'Data type of the segment should be 3: Passed '//
     .                 'descriptor shows type = #.'                   )
         CALL ERRINT ( '#', ICD ( 3 )                                 )
         CALL SIGERR ( 'SPICE(CKWRONGDATATYPE)'                       )
         CALL CHKOUT ( 'CKNR03'                                       )
         RETURN
      END IF
 
C
C     The number of records is the final word in the segment.
C
      CALL DAFGDA ( HANDLE, ICD(6), ICD(6), NPOINT )
 
      NREC = NINT ( NPOINT )
 
      CALL CHKOUT ( 'CKNR03' )
      RETURN
      END
