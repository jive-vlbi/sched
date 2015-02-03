C$Procedure DAFWFR ( DAF write file record )
 
      SUBROUTINE DAFWFR ( HANDLE, ND, NI, IFNAME, FWARD, BWARD, FREE )
 
C$ Abstract
C
C     Write or rewrite the contents of the file record of a DAF.
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
C     ND         I   Number of double precision components in summaries.
C     ND         I   Number of integer components in summaries.
C     IFNAME     I   Internal filename.
C     FWARD      I   Forward list pointer.
C     BWARD      I   Backward list pointer.
C     FREE       I   Free address pointer.
C
C$ Detailed_Input
C
C     HANDLE      is the handle associated with a DAF file opened for
C                 writing.
C
C     ND,
C     NI          are the numbers of double precision and integer
C                 components, respectively, in each array summary
C                 in the specified file.
C
C     IFNAME      is the internal file name to be stored in the first
C                 (or file) record of the specified file.
C
C     FWARD       is the forward list pointer. This points to the
C                 first summary record in the file.
C
C     BWARD       is the backward list pointer. This points to the
C                 final summary record in the file.
C
C     FREE        is the free address pointer. This contains the
C                 first free address in the file.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If the handle passed to this routine is not the handle of an
C        open DAF file, the error will be signaled by a routine called
C        by this routine.
C
C     2) If the specified DAF file is not open for write access, the
C        error will be diagnosed by a routine called by this routine.
C
C     3) If the file record cannot (for some reason) be written,
C        the error SPICE(DAFWRITEFAIL) is signaled.
C
C     4) If the attempt to read the file record fails, the error
C        SPICE(DAFREADFAIL) will be signaled.
C
C$ Particulars
C
C     The file record of a DAF is the only record that contains
C     any global information about the file. This record is created
C     when the file is created, and is updated only when new arrays
C     are added.
C
C        DO NOT CHANGE THE CONTENTS OF THE FILE RECORD UNLESS
C        YOU ARE ABSOLUTELY SURE YOU KNOW WHAT YOU ARE DOING.
C
C     Like character records, file records are not buffered.
C
C$ Examples
C
C     None.
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
C     F.S. Turner     (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.0.0, 27-NOV-2001 (FST)
C
C        Updated this routine to utilize new handle manager
C        interfaces.  Comments were expanded and clarified.
C
C-    SPICELIB Version 3.0.0, 21-MAR-1999 (FST)
C
C        This routine was modified to accomodate the preservation
C        of the FTP validation and binary file format strings that
C        are now part of the DAF file record.
C
C-    SPICELIB Version 2.0.0, 05-OCT-1993 (KRG)
C
C        The error SPICE(DAFNOIDWORD) is no longer signalled by this
C        routine. The reason for this is that if DAFSIH returns OK then
C        the handle passed to this routine is indeed a valid DAF file
C        handle, otherwise the error is diagnosed by DAFSIH.
C
C        Added two new exceptions to the $ Exceptions section: 1 and 4.
C        The remaining exceptions (2 and 3) were already present. The
C        exceptions that were added are not new, but are being
C        documented for the first time.
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
C     write daf file record
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 4.0.0, 27-NOV-2001 (FST)
C
C        The call to DAFHLU has been replaced with a call to
C        ZZDDHHLU, the handle manager interface for retrieving
C        a logical unit.  DAFHLU is no longer used, since it
C        locks the unit returned to its HANDLE, tying up resources
C        in the handle manager.
C
C-    SPICELIB Version 3.0.0, 21-MAR-1999 (FST)
C
C        In order to preserve the additional information that
C        now resides in the file record, this routine reads
C        the entire record into local buffers, including the
C        TAILEN characters that follow the actual data content.
C        The contents of the local buffers that correspond to
C        information brought in from the call sequence of the
C        routine are ignored when the record is rewritten.
C        However, the ID word, the file format string, and the
C        trailing TAILEN characters that contain the FTP validation
C        string are rewritten along with the input values.
C
C        This routine does not simply replace the FTP validation
C        string with the components from ZZFTPSTR, since that
C        would possibly validate a corrupt file created using a newer
C        Toolkit.
C
C-    SPICELIB Version 2.0.0, 05-OCT-1993 (KRG)
C
C        The error SPICE(DAFNOIDWORD) is no longer signalled by this
C        routine. The reason for this is that if DAFSIH returns OK then
C        the handle passed to this routine is indeed a valid DAF file
C        handle, otherwise the error is diagnosed by DAFSIH.
C
C        Added a call to DAFSIH to signal an invalid handle and a test
C        of FAILED () after it. This is to make sure that the DAF file
C        is open for writing. If this call succeeds, we know that we
C        have a valid DAF handle, so there is no need to check FAILED
C        after the call to DAFHLU.
C
C        Added code to read the file ID word so that it could be
C        preserved when the file record is written. This supports the ID
C        word format that contains type information.
C
C        Added variable IDWORD to the routine, as well as the parameters
C        IDWLEN and IFNLEN.
C
C        Added two new exceptions to the $ Exceptions section: 1 and 4.
C        The remaining exceptions (2 and 3) were already present. The
C        exceptions that were added are not new, but are being
C        documented for the first time.
C
C        Removed code that tested the sign of HANDLE to see if the file
C        was open for write access, HANDLE < 0. This test was no longer
C        necessary, as the call to DASSIH performs this test as well. No
C        sense doing it twice.
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
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local Parameters
C
      INTEGER               FMTLEN
      PARAMETER           ( FMTLEN =  8 )
 
      INTEGER               IDWLEN
      PARAMETER           ( IDWLEN =  8 )
 
      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60 )
 
C
C     The parameter TAILEN determines the tail length of a DAF file
C     record.  This is the number of bytes (characters) that
C     occupy the portion of the file record that follows the
C     integer holding the first free address.  For environments
C     with a 32 bit word length, 1 byte characters, and DAF
C     record sizes of 1024 bytes, we have:
C
C           8 bytes - IDWORD
C           4 bytes - ND     (32 bit integer)
C           4 bytes - NI     (32 bit integer)
C          60 bytes - IFNAME
C           4 bytes - FWARD  (32 bit integer)
C           4 bytes - BWARD  (32 bit integer)
C         + 4 bytes - FREE   (32 bit integer)
C          ---------
C          88 bytes - (All file records utilize this space.)
C
C     So the size of the remaining portion (or tail) of the DAF
C     file record for computing enviroments as described above
C     would be:
C
C        1024 bytes - DAF record size
C      -    8 bytes - DAF Binary File Format Word
C      -   88 bytes - (from above)
C       ------------
C         928 bytes - DAF file record tail length
C
C     Note: environments that do not have a 32 bit word length,
C     1 byte characters, and a DAF record size of 1024 bytes, will
C     require the adjustment of this parameter.
C
      INTEGER               TAILEN
      PARAMETER           ( TAILEN = 928 )
 
C
C     Local variables
C
      CHARACTER*(FMTLEN)    FORMAT
      CHARACTER*(IDWLEN)    IDWORD
      CHARACTER*(IFNLEN)    IFN
      CHARACTER*(IFNLEN)    LOCIFN
      CHARACTER*(TAILEN)    TAIL
 
      INTEGER               IOSTAT
      INTEGER               LOCFDR
      INTEGER               LOCFFA
      INTEGER               LOCLDR
      INTEGER               LOCND
      INTEGER               LOCNI
      INTEGER               UNIT
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFWFR' )
      END IF
 
C
C     Do some initializations
C
      IDWORD = ' '
 
C
C     Check to be sure that HANDLE is attached to a file that is open
C     with write access. If the call fails, check out and return.
C
      CALL DAFSIH ( HANDLE, 'WRITE' )
 
      IF ( FAILED () ) THEN
         CALL CHKOUT ( 'DAFWFR' )
         RETURN
      END IF
 
C
C     Get the logical unit for the file, as we know we have a valid DAF
C     handle with the correct access method.
C
      CALL ZZDDHHLU ( HANDLE, 'DAF', .FALSE., UNIT )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DAFWFR' )
         RETURN
      END IF
 
C
C     In order to maintain the integrity of the file ID word, the
C     file FORMAT, and the FTP string if present, we need to
C     read the entire file record into the appropriate sized local
C     buffers. The values of the LOCxxx variables are simply
C     ignored, since the caller passes new values in for updates.
C
      READ ( UNIT, REC=1, IOSTAT=IOSTAT) IDWORD,
     .                                   LOCND,
     .                                   LOCNI,
     .                                   LOCIFN,
     .                                   LOCFDR,
     .                                   LOCLDR,
     .                                   LOCFFA,
     .                                   FORMAT,
     .                                   TAIL
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL SETMSG ( 'Attempt to read the file record failed for'   //
     .                 ' file ''#''. IOSTAT = #'                      )
         CALL ERRFNM ( '#', UNIT                                      )
         CALL ERRINT ( '#', IOSTAT                                    )
         CALL SIGERR ( 'SPICE(DAFREADFAIL)'                           )
         CALL CHKOUT ( 'DAFWFR'                                       )
         RETURN
 
      END IF
 
C
C     Set the value of the internal filename before writing. This is to
C     guarantee that its length is ok.
C
      IFN = IFNAME
 
      WRITE (UNIT,
     .       REC=1,
     .       IOSTAT=IOSTAT) IDWORD,
     .                      ND,
     .                      NI,
     .                      IFN,
     .                      FWARD,
     .                      BWARD,
     .                      FREE,
     .                      FORMAT,
     .                      TAIL
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL SETMSG ( 'File record write failed. Value of IOSTAT ' //
     .                 'was #' )
         CALL ERRINT ( '#', IOSTAT )
         CALL SIGERR ( 'SPICE(DAFWRITEFAIL)' )
         CALL CHKOUT ( 'DAFWFR' )
         RETURN
 
      END IF
 
      CALL CHKOUT ( 'DAFWFR' )
      RETURN
      END
