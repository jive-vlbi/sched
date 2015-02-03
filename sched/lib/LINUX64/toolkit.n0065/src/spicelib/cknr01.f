C$Procedure      CKNR01 ( C-kernel, number of records, type 01 )
 
      SUBROUTINE CKNR01 ( HANDLE, DESCR, NREC )
      IMPLICIT NONE
 
C$ Abstract
C
C     Given the handle of a CK file and the descriptor of a data
C     type 1 segment in that file, return the number of pointing
C     records in that segment.
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
C     DESCR      I   The descriptor of the type 1 segment.
C     NREC       O   The number of records in the segment.
C
C$ Detailed_Input
C
C     HANDLE     is the handle of the binary CK file containing the
C                segment whose descriptor was also passed.  The file
C                should have been opened for read access, either by
C                CKLPF or DAFOPR.
C
C     DESCR      The packed descriptor of a data type 1 segment.
C
C$ Detailed_Output
C
C     NREC       The number of pointing records in the type 1 segment.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the segment indicated by DESCR is not a type 1 segment,
C         the error 'SPICE(CKWRONGDATATYPE)' is signalled.
C
C     2)  If the specified handle does not belong to any file that is
C         currently known to be open, an error is diagnosed by a
C         routine that this routine calls.
C
C     3)  If DESCR is not a valid, packed descriptor of a segment in
C         the CK file specified by HANDLE, the results of this routine
C         are unpredictable.
C
C$ Files
C
C     The file specified by HANDLE should be open for read access.
C
C$ Particulars
C
C     For a complete description of the internal structure of a type 1
C     segment, see the CK required reading.
C
C$ Examples
C
C     The following code fragment prints the records of the first
C     segment in a CK file.  Suppose MOC.CK is binary CK file that
C     contains segments of data type 1.
C
C           INTEGER               ICD     ( 6 )
C           INTEGER               HANDLE
C           INTEGER               NREC
C           INTEGER               I
C           DOUBLE PRECISION      DCD     ( 2 )
C           DOUBLE PRECISION      DESCR   ( 5 )
C           DOUBLE PRECISION      RECORD  ( 8 )
C           LOGICAL               FOUND
C
C     C
C     C     First load the file. (The file may also be opened by using
C     C     CKLPF.)
C     C
C           CALL DAFOPR ( 'MOC.CK', HANDLE )
C
C     C
C     C     Begin forward search.  Find first array.
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
C           IF ( ICD( 3 ) .EQ. 1 ) THEN
C
C     C
C     C        How many records does this segment contain?
C     C
C              CALL CKNR01 ( HANDLE, DESCR, NREC )
C
C              DO I = 1, NREC
C
C     C
C     C           Get the record associated with record number I.
C     C
C                 CALL CKGR01 ( HANDLE, DESCR, I, RECORD )
C                 WRITE (*,*) 'Record ', I, ':'
C                 WRITE (*,*)  RECORD
C              END DO
C
C           END IF
C
C$ Restrictions
C
C     The binay CK file containing the segment whose descriptor was
C     passed to this routine must be opened for read access by either
C     CKLPF or DAFOPR.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     J.E. McLean    (JPL)
C     M.J. Spencer   (JPL)
C     R.E. Thurman   (JPL)
C     I.M. Underwood (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
C
C        Replaced DAFRDA call with DAFGDA.
C        Added IMPLICIT NONE.
C
C-    SPICELIB Version 1.0.3, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.2, 06-MAR-1991 (JML)
C
C        A correction was made to the example program in the
C        header.  The array of double precision components of
C        the descriptor ( DCD ) had originally been declared
C        as an integer.
C
C-    SPICELIB Version 1.0.1, 02-NOV-1990 (JML)
C
C        The restriction that a C-kernel file must be loaded
C        was explicitly stated.
C
C-    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) (IMU)
C
C-&
 
C$ Index_Entries
C
C     number of ck type_1 records
C
C-&
 
 
 
 
C$ Revisions
C
C-    SPICELIB Version 1.0.2, 06-MAR-1991 (JML)
C
C        A correction was made to the example program in the
C        header.  The array of double precision components of
C        the descriptor ( DCD ) had originally been declared
C        as an integer.
C
C-    SPICELIB Version 1.0.1, 02-NOV-1990 (JML)
C
C        1) The restriction that a C-kernel file must be loaded
C           was explicitly stated.
C        2) Minor changes were made to the wording of the header.
C
C-    Beta Version 1.1.0, 28-AUG-1990 (MJS) (JEM)
C
C        The following changes were made as a result of the
C        NAIF CK Code and Documentation Review:
C
C        1) The name of this routine was changed from CK01NR to
C           CKNR01 in order to be consistent with the SPICELIB
C           naming convention.
C        2) The declarations for the parameters NDC and NIC were
C           moved from the "Declarations" section of the header to
C           the "Local parameters" section of the code below the
C           header. These parameters are not meant to modified by
C           users.
C        3) The variables INTDES and DPDES were changed to ICD and
C           DCD.
C        4) The header was corrected, improved, and updated to reflect
C           the changes.
C        5) The in-code comments were improved.
C
C-    Beta Version 1.0.0, 22-MAY-1990 (RET) (IMU)
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
      INTEGER               NDC
      PARAMETER           ( NDC = 2 )
 
      INTEGER               NIC
      PARAMETER           ( NIC = 6 )
 
C
C     Local variables
C
      INTEGER               ICD  ( NIC )
 
      DOUBLE PRECISION      DCD  ( NDC )
      DOUBLE PRECISION      N
 
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKNR01' )
      END IF
 
 
C
C     The number of pointing records contained in a data type 1
C     segment is stored in the final double precision word of the
C     segment.  Since the address of this very word is stored in the
C     sixth integer component of the segment descriptor, it is a trivial
C     matter to extract the count.
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
C     If this segment is not of data type 1, then signal an error.
C
 
      IF ( ICD( 3 ) .NE. 1 ) THEN
         CALL SETMSG ( 'Data type of the segment should be 1: Passed '//
     .                 'descriptor shows type = #.'                   )
         CALL ERRINT ( '#', ICD ( 3 )                                 )
         CALL SIGERR ( 'SPICE(CKWRONGDATATYPE)'                       )
         CALL CHKOUT ( 'CKNR01'                                       )
         RETURN
      END IF
 
C
C     The number of records is the final word in the segment.
C
      CALL DAFGDA ( HANDLE, ICD( 6 ), ICD( 6 ), N )
 
      NREC = INT( N )
 
      CALL CHKOUT ( 'CKNR01' )
      RETURN
      END
