C$Procedure DAFRRR ( DAF, remove reserved records )
 
      SUBROUTINE DAFRRR ( HANDLE, RESV )
 
C$ Abstract
C
C     Remove a specified number of reserved records from a Double
C     Precision Array File (DAF).
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
C     HANDLE     I   DAF, opened for writing.
C     RESV       I   Number of records to remove.
C
C$ Detailed_Input
C
C     HANDLE      is the handle associated with a DAF that has been
C                 opened with write access.
C
C     RESV        is the number of reserved records to be removed
C                 from the specified file.
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
C     2) If RESV is greater than the number of reserved records in the
C        file, all of the reserved records are removed.
C
C$ Files
C
C     See argument HANDLE.
C
C$ Particulars
C
C     Normally, the reserved records in an array file are reserved
C     when the file is created. However, it may occasionally become
C     desirable to remove reserved records---when their contents are
C     significantly reduced, for example.
C
C     The records nearest the end of the file are removed. Note
C     that the physical size of the file is not reduced when reserved
C     records are removed.
C
C$ Examples
C
C     For the following call to DAFRRR, assume that HANDLE is the file
C     handle for a DAF file that has been opened for write access, and
C     that the DAF file already contains 12 reserved records (located in
C     records 2-13 of the physical file).
C
C        CALL DAFRRR ( HANDLE, 7 )
C
C     After this call to DAFRRR, the number of reserved records has been
C     decreased by 7, leaving only the first five of the original
C     reserved records, physical records 2-6.
C
C$ Restrictions
C
C     1) This routine will only remove reserve records from DAFs open
C        for write.  These files are implicitly of the native binary
C        file format.
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
C-    SPICELIB Version 1.2.0, 16-NOV-2001 (FST)
C
C        Added a call to DAFSIH to prevent this routine from
C        attempting to write to non-native binary file formats.
C        This will provide a more useful error diagnostic with
C        little impact on performance.
C
C-    SPICELIB Version 1.1.0, 30-SEP-1993 (KRG)
C
C        Detailed_Input and Examples section of the header were
C        modified.
C
C        Added calls to the FORTRAN intrinsic functions INT and
C        DBLE in the code that updates the summary record.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 18-JUL-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     remove daf reserved records
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 30-SEP-1993 (KRG)
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
C        number to an integer before assigning it to NEXT or ENDBLK,
C        both of which are integer variables. Also added calls to INT
C        in IF statements where comparisons were made between DP numbers
C        and INTEGERs, when integral values were actually being
C        compared.
C
C        Added calls to the intrinsic function DBLE to convert an
C        integer, REMOVE, into a DP number when doing some arithmetic.
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
C     WPR         is the maximum number of double precision
C                 numbers per record.  WPR stands for words
C                 per record.
C
      INTEGER               WPR
      PARAMETER           ( WPR    = 128      )
 
C
C     MAXD,       are the maximum number of double precision
C     MAXI,       numbers, integers, and characters, respectively,
C     MAXC        not including space reserved for control information.
C
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
      INTEGER               DECR
      INTEGER               ENDBLK
      INTEGER               FREE
      INTEGER               FWARD
      INTEGER               IC      ( MAXI )
      INTEGER               ND
      INTEGER               NEXT
      INTEGER               NI
      INTEGER               RECNO
      INTEGER               REMOVE
      INTEGER               WORD
 
      LOGICAL               FOUND
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFRRR' )
      END IF
 
C
C     Before proceeding any further, check that the DAF associated
C     with HANDLE is available for write access.
C
      CALL DAFSIH ( HANDLE, 'WRITE' )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DAFRRR' )
         RETURN
      END IF
 
C
C     Get the contents of the file record. If it fails, then just check
C     out and return, as an appropriate error message should have
C     already been set.
C
      CALL DAFRFR ( HANDLE, ND, NI, IFNAME, FWARD, BWARD, FREE )
 
      IF ( FAILED () ) THEN
 
         CALL CHKOUT ( 'DAFRRR' )
         RETURN
 
      END IF
 
C
C     Don't remove more than the current number of reserved records!
C     If there are none, check out.
C
      REMOVE = MIN ( RESV, FWARD - 2 )
 
      IF ( REMOVE .LT. 1 ) THEN
 
         CALL CHKOUT ( 'DAFRRR' )
         RETURN
 
      END IF
 
C
C     Okay, here's the plan. We are just going to move records
C     forward, starting with the first summary record in the file
C     and ending with the last data record.
C
C     After everything has been moved, the initial and final
C     addresses of all the arrays have to be decremented by the
C     same amount: the number of words per record (128) times
C     the number of records removed.
C
      DECR = WPR * REMOVE
 
C
C     Records will be moved in `blocks', where each block contains
C
C        -- a summary record
C
C        -- a name record
C
C        -- one or more data records
C
C     Most blocks lie between one summary record and the next.
C     The final block lies between the final summary record and
C     whatever data record contains the first free address.
C
C     BEGBLK is initially the first summary record location.
C
      BEGBLK = FWARD
 
      DO WHILE ( ( BEGBLK .GT. 0 )  .AND.  ( .NOT. FAILED() ) )
 
C
C        Move the summary record first. The location of the next
C        summary record determines the end of this block, and the
C        beginning of the next.
C
C        Be sure to adjust the forward and backward pointers;
C        otherwise, we won't be able to find the summaries again.
C
         RECNO = BEGBLK
         CALL DAFRDR ( HANDLE, RECNO, 1, WPR, DREC, FOUND )
 
         IF ( INT ( DREC(1) ) .GT. 0 ) THEN
            ENDBLK = INT ( DREC(1) ) - 1
            NEXT   = INT ( DREC(1) )
         ELSE
            CALL DAFARW ( FREE, ENDBLK, WORD )
            NEXT = 0
         END IF
 
         IF ( INT ( DREC(1) ) .GT. 0 ) THEN
            DREC(1) = DREC(1) - DBLE ( REMOVE )
         END IF
 
         IF ( INT ( DREC(2) ) .GT. 0 ) THEN
            DREC(2) = DREC(2) - DBLE ( REMOVE )
         END IF
 
         CALL DAFWDR ( HANDLE, RECNO - REMOVE, DREC )
 
C
C        Then the name record.
C
         RECNO = BEGBLK + 1
         CALL DAFRCR ( HANDLE, RECNO,          CREC )
         CALL DAFWCR ( HANDLE, RECNO - REMOVE, CREC )
 
C
C        Finally, the data records.
C
         DO RECNO = BEGBLK + 2, ENDBLK
            CALL DAFRDR ( HANDLE, RECNO,          1, WPR, DREC, FOUND )
            CALL DAFWDR ( HANDLE, RECNO - REMOVE,         DREC        )
         END DO
 
C
C        Start the next block, if one exists.
C
         BEGBLK = NEXT
 
      END DO
 
C
C     Rewrite the file record, to reflect the new organization of
C     the file.
C
      FWARD = FWARD - REMOVE
      BWARD = BWARD - REMOVE
      FREE  = FREE  - DECR
 
      CALL DAFWFR ( HANDLE, ND, NI, IFNAME, FWARD, BWARD, FREE )
 
C
C     Get the summary for each array, decrement the addresses (stored
C     in the final two integer components), and replace the summary.
C
      CALL DAFBFS ( HANDLE )
      CALL DAFFNA ( FOUND  )
 
      DO WHILE ( FOUND  .AND.  ( .NOT. FAILED() ) )
 
         CALL DAFGS  ( SUM                 )
         CALL DAFUS  ( SUM, ND, NI, DC, IC )
 
         IC(NI-1) = IC(NI-1) - DECR
         IC(NI  ) = IC(NI  ) - DECR
 
         CALL DAFPS ( ND, NI, DC, IC, SUM )
         CALL DAFWS (                 SUM )
 
         CALL DAFFNA ( FOUND )
 
      END DO
 
      CALL CHKOUT ( 'DAFRRR' )
      RETURN
      END
