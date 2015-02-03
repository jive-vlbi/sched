C$Procedure DAFARR ( DAF, add reserved records )
 
      SUBROUTINE DAFARR ( HANDLE, RESV )
 
C$ Abstract
C
C     Add a specified number of reserved records to a Double Precision
C     Array File (DAF).
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
C     DAF
C
C$ Keywords
C
C     FILES
C
C$ Declarations
 
      INTEGER               HANDLE
      INTEGER               RESV
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of a DAF file opened for writing.
C     RESV       I   Number of records to reserve.
C
C$ Detailed_Input
C
C     HANDLE      is the handle associated with a DAF file that has
C                 been opened with write access.
C
C     RESV        is the number of reserved records to be added
C                 to the specified file.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If RESV is less than one, the file is not changed.
C
C$ Files
C
C     See argument HANDLE.
C
C$ Particulars
C
C     Normally, the reserved records in an array file are reserved
C     when the file is created.  However, it may occasionally become
C     necessary to add reserved records---when the contents of one
C     file are appended to another, for example. (In this case, any
C     information in the reserved records of either file should
C     be included in the resulting file.)
C
C     The new reserved records are appended to the old ones. The new
C     reserved records are also NULL filled.
C
C$ Examples
C
C     In the following call to DAFARR, assume that HANDLE is the file
C     handle for a DAF file that has been opened for write access, and
C     that the DAF file already contains 12 reserved records (located in
C     records 2-13 of the physical file).
C
C        CALL DAFARR ( HANDLE, 7 )
C
C     After this call, the DAF file attached to HANDLE will contain 19
C     reserved records. The new reserved records are located in
C     records 14-20 of the physical file.
C
C$ Restrictions
C
C     1) This routine will only add reserved records to DAFs open for
C        write.  These files are implicitly of the native binary file
C        format.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer (JPL)
C     I.M. Underwood (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.5.0, 16-NOV-2001 (FST)
C
C        Added a call to DAFSIH to prevent this routine from
C        attempting to write to non-native binary file formats.
C        This will provide a more useful error diagnostic with
C        little impact on performance.
C
C-    SPICELIB Version 1.4.0, 08-MAR-1996 (KRG)
C
C        Added code to write NULL filled records to the file for the
C        new reserved records.
C
C-    SPICELIB Version 1.3.0, 12-MAY-1994 (KRG)
C
C        Added a missing call to CHKOUT before the RETURN statement in
C        the test
C
C              IF ( RESV .LT. 1 ) THEN
C                 RETURN
C              END IF
C
C-    SPICELIB Version 1.2.0, 30-SEP-1993 (KRG)
C
C        Detailed_Input and Examples section of the header were
C        modified.
C
C        Added calls to the FORTRAN intrinsic functions INT and
C        DBLE in the code that updates the summary record.
C
C        Modified an IF loop to make logic clearer.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 17-JUL-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     add daf reserved records
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.4.0, 08-MAR-1996 (KRG)
C
C        Added code to write NULL filled records to the file for the
C        new reserved records.
C
C-    SPICELIB Version 1.3.0, 12-MAY-1994 (KRG)
C
C        Added a missing call to CHKOUT before the RETURN statement in
C        the test
C
C              IF ( RESV .LT. 1 ) THEN
C                 RETURN
C              END IF
C
C-    SPICELIB Version 1.2.0, 30-SEP-1993 (KRG)
C
C        $ Detailed_Input section was modified. References to any
C        specific routines by name as a method for opening a DAF file
C        for write access were removed. The assumption is that a person
C        using DAF files would already know something about opening and
C        closing the files.
C
C        $ Examples section was modified. References to any specific
C        routines by name as a method for opening a DAF file for writing
C        were removed, and the example was reworded in such a way that
C        the use of the subroutine remained clear.
C
C        Added calls to the INT intrinsic function to convert a DP
C        number to an integer before assigning it to NEXT, which is an
C        integer variable. Also added calls to INT in IF statements
C        where comparisons were made between DP numbers and INTEGERs,
C        when integral values were actually being compared.
C
C        Added calls to the intrinsic function DBLE to convert an
C        integer, RESV, into a DP number when doing some arithmetic.
C
C        Took an ELSE IF clause out of the initial IF return  ELSE
C        check in END IF at the beginning of the routine. Replaced the
C        code:
C
C              IF ( RETURN () ) THEN
C                 RETURN
C
C              ELSE IF ( RESV .LT. 1 ) THEN
C                 RETURN
C
C              ELSE
C                 CALL CHKIN ( 'DAFARR' )
C              END IF
C
C        with the eqivalent code:
C
C              IF ( RETURN () ) THEN
C                 RETURN
C              ELSE
C                 CALL CHKIN ( 'DAFARR' )
C              END IF
C
C        C
C        C     Check to see if the number of records to be reserved is
C        C     less than one. If so, just return without changing
C        C     anything.
C        C
C              IF ( RESV .LT. 1 ) THEN
C                 RETURN
C              END IF
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 17-JUL-1990 (IMU)
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local parameters
C
C     IFNLEN      is the length of a DAF internal file name.
C
      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60       )
 
C
C     WPR         is the maximum number of double precision numbers
C                 (words) per record.
C
C     MAXD,       are the maximum number of double precision
C     MAXI,       numbers, integers, and characters, respectively,
C     MAXC        per record, not including space reserved for
C                 control information (3 dp numbers are reserved).
C                 There are two integers per double precision word,
C                 and eight characters per word.
C
      INTEGER               WPR
      PARAMETER           ( WPR    = 128      )
 
      INTEGER               MAXD
      PARAMETER           ( MAXD   = WPR  - 3 )
 
      INTEGER               MAXI
      PARAMETER           ( MAXI   = MAXD * 2 )
 
      INTEGER               MAXC
      PARAMETER           ( MAXC   = MAXD * 8 )
 
C
C     Local variables
C
      CHARACTER*(MAXC)      CREC
      CHARACTER*(IFNLEN)    IFNAME
 
      DOUBLE PRECISION      DC      ( MAXD )
      DOUBLE PRECISION      DREC    ( WPR  )
      DOUBLE PRECISION      SUM     ( MAXD )
 
      INTEGER               BEGBLK
      INTEGER               BWARD
      INTEGER               ENDBLK
      INTEGER               FREE
      INTEGER               FWARD
      INTEGER               I
      INTEGER               INCR
      INTEGER               IC      ( MAXI )
      INTEGER               ND
      INTEGER               NEXT
      INTEGER               NI
      INTEGER               RECNO
      INTEGER               WORD
 
      LOGICAL               FOUND
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFARR' )
      END IF
C
C
C     Check to see if the number of records to be reserved is less than
C     one. If so, just return without changing anything.
C
      IF ( RESV .LT. 1 ) THEN
         CALL CHKOUT ( 'DAFARR' )
         RETURN
      END IF
 
C
C     Before proceeding any further, check that the DAF associated
C     with HANDLE is available for write access.
C
      CALL DAFSIH ( HANDLE, 'WRITE' )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DAFARR' )
         RETURN
      END IF
 
C
C     Get the contents of the file record. If it fails, then just check
C     out and return, as an appropriate error message should have
C     already been set.
C
      CALL DAFRFR ( HANDLE, ND, NI, IFNAME, FWARD, BWARD, FREE )
 
      IF ( FAILED () ) THEN
 
         CALL CHKOUT ( 'DAFARR' )
         RETURN
 
      END IF
 
C
C     Okay, here's the plan. We are just going to move records
C     in the direction of the end of the file, starting
C     with the last record in the file and ending with the first
C     summary record.
C
C     After everything has been moved, the initial and final
C     addresses of all the arrays have to be incremented by the
C     same amount: the number of words per record (128) times
C     the number of new records.
C
      INCR = WPR * RESV
 
C
C     Before we do that, however, we should write some bogus records
C     to the end of the file, to make sure we don't run out of space
C     later on. If this doesn't work, we will leave the logical
C     contents of the file uncorrupted (although it may get larger).
C
      CALL DAFARW ( FREE, RECNO, WORD )
 
      DO I = 1, RESV
 
         CALL DAFWDR ( HANDLE, RECNO + I, DREC )
 
      END DO
 
      IF ( FAILED() ) THEN
 
         CALL CHKOUT ( 'DAFARR' )
         RETURN
 
      END IF
 
C
C     Records will be moved in `blocks', where each block contains
C
C        -- a summary record
C
C        -- a name record
C
C        -- one or more data records
C
C     The first block to be moved (that is, the last block in
C     the file) lies between the final summary record (BWARD) and
C     whatever record contains the first free address in the file.
C
      BEGBLK = BWARD
      CALL DAFARW ( FREE, ENDBLK, WORD )
 
      DO WHILE ( ( BEGBLK .GT. 0 )  .AND.  ( .NOT. FAILED() ) )
 
C
C        Move the data records first.
C
         DO RECNO = ENDBLK,  BEGBLK + 2,  -1
 
            CALL DAFRDR ( HANDLE, RECNO,        1, WPR, DREC, FOUND )
            CALL DAFWDR ( HANDLE, RECNO + RESV,         DREC        )
 
         END DO
 
C
C        Then the name record.
C
         RECNO = BEGBLK + 1
         CALL DAFRCR ( HANDLE, RECNO,        CREC )
         CALL DAFWCR ( HANDLE, RECNO + RESV, CREC )
 
C
C        Finally, the summary record.
C
C        To find the beginning of the next block, look at the backward
C        pointer from the summary record of the current block.
C
C        Be sure to adjust the forward and backward pointers;
C        otherwise, we won't be able to find the summaries again.
C
         RECNO = BEGBLK
         CALL DAFRDR ( HANDLE, RECNO, 1, WPR, DREC, FOUND )
 
         NEXT = INT ( DREC(2) )
 
         IF ( INT ( DREC(1) ) .GT. 0 ) THEN
            DREC(1) = DREC(1) + DBLE ( RESV )
         END IF
 
         IF ( INT ( DREC(2) ) .GT. 0 ) THEN
            DREC(2) = DREC(2) + DBLE ( RESV )
         END IF
 
         CALL DAFWDR ( HANDLE, RECNO + RESV, DREC )
 
C
C        The next block ends just before the current block begins.
C
         ENDBLK = BEGBLK  - 1
         BEGBLK = NEXT
 
      END DO
 
C
C     Rewrite the file record, to reflect the new organization of
C     the file.
C
      FWARD = FWARD + RESV
      BWARD = BWARD + RESV
      FREE  = FREE  + INCR
 
      CALL DAFWFR ( HANDLE, ND, NI, IFNAME, FWARD, BWARD, FREE )
 
C
C     Get the summary for each array, increment the addresses (stored
C     in the final two integer components), and replace the summary.
C
      CALL DAFBFS ( HANDLE )
      CALL DAFFNA ( FOUND  )
 
      DO WHILE ( FOUND  .AND.  ( .NOT. FAILED() ) )
 
         CALL DAFGS  ( SUM                 )
         CALL DAFUS  ( SUM, ND, NI, DC, IC )
 
         IC(NI-1) = IC(NI-1) + INCR
         IC(NI  ) = IC(NI  ) + INCR
 
         CALL DAFPS ( ND, NI, DC, IC, SUM )
         CALL DAFWS (                 SUM )
 
         CALL DAFFNA ( FOUND )
 
      END DO
C
C     Write NULL filled records to the reserved record area.
C
      DO I = 1, MAXC
         CREC(I:I) = CHAR(0)
      END DO
 
      I = FWARD-RESV
      DO RECNO = I, I + RESV - 1
         CALL DAFWCR ( HANDLE, RECNO, CREC )
      END DO
 
      CALL CHKOUT ( 'DAFARR' )
      RETURN
      END
