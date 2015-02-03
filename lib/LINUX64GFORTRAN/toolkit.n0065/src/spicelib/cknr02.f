C$Procedure      CKNR02 ( C-kernel, number of records, type 02 )
 
      SUBROUTINE CKNR02 ( HANDLE, DESCR, NREC )
 
C$ Abstract
C
C     Given the handle of a CK file and the descriptor of a type 2
C     segment in that file, return the number of pointing records
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
C     DESCR      I   The descriptor of the type 2 segment.
C     NREC       O   The number of records in the segment.
C
C$ Detailed_Input
C
C     HANDLE     is the handle of the binary CK file containing the
C                segment. The file should have been opened for read
C                or write access, either by CKLPF, DAFOPR, or DAFOPW.
C
C     DESCR      The packed descriptor of a data type 2 segment.
C
C$ Detailed_Output
C
C     NREC       The number of pointing records in the type 2 segment
C                associated with HANDLE and DESCR.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the segment indicated by DESCR is not a type 2 segment,
C         the error 'SPICE(CKWRONGDATATYPE)' is signalled.
C
C     2)  If the specified handle does not belong to any file that is
C         currently known to be open, an error is diagnosed by a
C         routine that this routine calls.
C
C     3)  If DESCR is not a valid descriptor of a segment in the CK
C         file specified by HANDLE, the results of this routine are
C         unpredictable.
C
C$ Files
C
C     The file specified by HANDLE should be open for read or write
C     access.
C
C$ Particulars
C
C     For a complete description of the internal structure of a type 2
C     segment, see the CK required reading.
C
C     This routine returns the number of pointing records contained
C     in the specified segment. It is normally used in conjunction
C     with CKGR02, which returns the Ith record in the segment.
C
C$ Examples
C
C     Suppose GLL_PLT.BC is a CK file that contains segments of data
C     type 2. Then the following code fragment uses CKNR02 and CKGR02
C     to extract each pointing record in the first segment in the file.
C
C           INTEGER               ICD     ( 6 )
C           INTEGER               HANDLE
C           INTEGER               NREC
C           INTEGER               I
C
C           DOUBLE PRECISION      DCD     ( 2  )
C           DOUBLE PRECISION      DESCR   ( 5  )
C           DOUBLE PRECISION      RECORD  ( 10 )
C
C           LOGICAL               FOUND
C
C     C
C     C     First load the file. ( The file may also be opened by using
C     C     CKLPF. )
C     C
C           CALL DAFOPR ( 'GLL_PLT.BC', HANDLE )
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
C           IF ( ICD( 3 ) .EQ. 2 ) THEN
C
C     C
C     C        How many records does this segment contain?
C     C
C              CALL CKNR02 ( HANDLE, DESCR, NREC )
C
C              DO I = 1, NREC
C
C     C
C     C           Get the Ith record in the segment.
C     C
C                 CALL CKGR02 ( HANDLE, DESCR, I, RECORD )
C     C
C     C           Process the pointing data.
C     C
C                 .
C                 .
C                 .
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
C-    SPICELIB Version 1.0.0, 25-NOV-1992 (JML)
C
C-&
 
C$ Index_Entries
C
C     number of ck type_2 records
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
C        DTYPE      is the data type.
C
 
      INTEGER               NDC
      PARAMETER           ( NDC   = 2 )
 
      INTEGER               NIC
      PARAMETER           ( NIC   = 6 )
 
      INTEGER               DTYPE
      PARAMETER           ( DTYPE = 2 )
 
C
C     Local variables
C
      INTEGER               BEG
      INTEGER               END
      INTEGER               ARRSIZ
      INTEGER               ICD    ( NIC )
 
      DOUBLE PRECISION      DCD    ( NDC )
 
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKNR02' )
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
C
      CALL DAFUS ( DESCR, NDC, NIC, DCD, ICD )
 
C
C     If this segment is not of data type 2, then signal an error.
C
 
      IF ( ICD( 3 ) .NE. DTYPE ) THEN
         CALL SETMSG ( 'Data type of the segment should be 2: Passed '//
     .                 'descriptor shows type = #.'                   )
         CALL ERRINT ( '#', ICD ( 3 )                                 )
         CALL SIGERR ( 'SPICE(CKWRONGDATATYPE)'                       )
         CALL CHKOUT ( 'CKNR02'                                       )
         RETURN
      END IF
 
C
C     The beginning and ending addresses of the segment are in the
C     descriptor.
C
      BEG = ICD( 5 )
      END = ICD( 6 )
 
C
C     Calculate the number of pointing records in the segment from
C     the physical size of the segment and knowledge of its structure.
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
 
 
      CALL CHKOUT ( 'CKNR02' )
      RETURN
      END
