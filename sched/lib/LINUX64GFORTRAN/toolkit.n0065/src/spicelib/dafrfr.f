C$Procedure DAFRFR ( DAF, read file record )

      SUBROUTINE DAFRFR ( HANDLE, ND, NI, IFNAME, FWARD, BWARD, FREE )

C$ Abstract
C
C     Read the contents of the file record of a DAF.
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
      INTEGER               ND
      INTEGER               NI
      CHARACTER*(*)         IFNAME
      INTEGER               FWARD
      INTEGER               BWARD
      INTEGER               FREE

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of an open DAF file.
C     ND         O   Number of double precision components in summaries.
C     NI         O   Number of integer components in summaries.
C     IFNAME     O   Internal file name.
C     FWARD      O   Forward list pointer.
C     BWARD      O   Backward list pointer.
C     FREE       O   Free address pointer.
C
C$ Detailed_Input
C
C     HANDLE      is the handle assigned to a DAF file opened for
C                 reading.
C
C$ Detailed_Output
C
C     ND,
C     NI          are the numbers of double precision and integer
C                 components, respectively, in each array summary in
C                 the specified file.
C
C     IFNAME      is the internal file name stored in the first
C                 (or file) record of the specified file.
C
C     FWARD       is the forward list pointer. This points to the
C                 first summary record in the file. (Records between
C                 the first record and the first summary record are
C                 reserved when the file is created, and are invisible
C                 to DAF routines.)
C
C     BWARD       is the backward list pointer. This points
C                 to the final summary record in the file.
C
C     FREE        is the free address pointer. This contains the
C                 first free address in the file. (That is, the
C                 initial address of the next array to be added
C                 to the file.)
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     1) If the handle passed to this routine is not the handle of an
C        open DAF file, the error will be signaled by a routine called
C        by this routine.
C
C     2) If the specified DAF file is not open for read access, the
C        error will be diagnosed by a routine called by this routine.
C
C     3) If the specified record cannot (for some reason) be read,
C        the error SPICE(DAFFRNOTFOUND) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The file record of a DAF is the only record that contains
C     any global information about the file. This record is created
C     when the file is created, and is updated only when new arrays
C     are added.
C
C     Like character records, file records are not buffered.
C
C$ Examples
C
C     In the following example, the value of the forward list
C     pointer is examined in order to determine the number of
C     reserved records in the DAF. These records are then read
C     and the contents printed to the screen.
C
C        CALL DAFRFR ( HANDLE, ND, NI, IFNAME, FWARD, BWARD, FREE )
C        CALL DAFHLU ( HANDLE, UNIT )
C
C        DO I = 2, FWARD - 1
C           READ  (UNIT,REC=I) PRIVATE(1:1000)
C           WRITE (*,*)        PRIVATE(1:1000)
C        END DO
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     NAIF Document 167.0, "Double Precision Array Files (DAF)
C     Specification and User's Guide"
C
C$ Author_and_Institution
C
C     K.R. Gehringer  (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.1.0, 30-DEC-2009 (EDW)
C
C        Expanded DAFFRNOTFOUND error message to identify the file 
C        handle corresponding to the error condition.
C
C        Reordered header sections to conform to SPICE format.
C        Merged the Revisions sections, now deleted, with Version.
C
C-    SPICELIB Version 3.0.0, 16-NOV-2001 (FST)
C
C        Updated this routine to utilize interfaces built on
C        the new handle manager to perform I/O operations.
C
C        This routine now utilizes ZZDAFGFR to retrieve information
C        from the file record.  As this private interface takes a
C        handle and performs the necessary logical unit to handle
C        mapping, the call to DAFHLU was removed.  The DAFSIH call
C        remains, since this insures that HANDLE is known to DAFAH.
CC
C-    SPICELIB Version 2.0.0, 04-OCT-1993 (KRG)
C
C        The error SPICE(DAFNOIDWORD) is no longer signaled by this
C        routine. The reason for this is that if DAFSIH returns OK then
C        the handle passed to this routine is indeed a valid DAF file
C        handle, otherwise the error is diagnosed by DAFSIH.
C
C        Added a call to DAFSIH to signal an invalid handle and a test
C        of FAILED () after it. This is to make sure that the DAF file
C        is open for reading. If this call succeeds, we know that we
C        have a valid DAF handle, so there is no need to check FAILED
C        after the call to DAFHLU.
C
C        The variable name DAFWRD was changed to IDWORD.
C
C        Added two new exceptions to the $ Exceptions section: 1 and 2.
C        The remaining exception (3) was already present. The exceptions
C        that were added are not new, but are being documented for the
C        first time.
C
C
C-    SPICELIB Version 1.0.3, 6-OCT-1992 (HAN)
C
C        Corrected a typo in the Brief_I/O section. ND was listed
C        twice as an input, and NI was not listed.
C
C-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&

C$ Index_Entries
C
C     read daf file record
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local Parameters
C
      INTEGER               IDWLEN
      PARAMETER           ( IDWLEN =  8 )

      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60)
 
C
C     Local variables
C
      CHARACTER*(IDWLEN)    IDWORD

      LOGICAL               FOUND

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFRFR' )
      END IF

C
C     Do some initializations
C
      IDWORD = ' '

C
C     Check to be sure that HANDLE is attached to a file that is open
C     with read access. If the call fails, check out and return.
C
      CALL DAFSIH ( HANDLE, 'READ' )
 
      IF ( FAILED () ) THEN
 
         CALL CHKOUT ( 'DAFRFR' )
         RETURN
 
      END IF
 
C
C     Retrieve all but the internal file name directly from the
C     file record.  Read the internal file name into a temporary
C     string, to be sure of the length. Check FOUND.
C
      CALL ZZDAFGFR ( HANDLE,
     .                IDWORD,
     .                ND,
     .                NI,
     .                IFNAME,
     .                FWARD,
     .                BWARD,
     .                FREE,
     .                FOUND   )
 
      IF ( .NOT. FOUND ) THEN
 
         CALL SETMSG ( 'File record not found for file handle #1. '
     .             //  'Check if program code uses '
     .             //  'handle #2 for a read or write operation.')
         CALL ERRINT ( '#1', HANDLE              )
         CALL ERRINT ( '#2', HANDLE              )
         CALL SIGERR ( 'SPICE(DAFFRNOTFOUND)'   )
         CALL CHKOUT ( 'DAFRFR'                 )
         RETURN
 
      END IF
 
      CALL CHKOUT ( 'DAFRFR' )
      RETURN
      END
