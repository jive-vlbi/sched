C$Procedure DAFRCR ( DAF, read character record )
 
      SUBROUTINE DAFRCR ( HANDLE, RECNO, CREC )
 
C$ Abstract
C
C     Read the contents of a character record from a DAF.
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
      INTEGER               RECNO
      CHARACTER*(*)         CREC
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of DAF.
C     RECNO      I   Record number of character record.
C     CREC       O   Character record.
C
C$ Detailed_Input
C
C     HANDLE      is the handle associated with a DAF.
C
C     RECNO       is the record number of a character record within
C                 the file.
C
C$ Detailed_Output
C
C     CREC        contains the first 1000 characters of the specified
C                 record from the specified file.
C
C$ Parameters
C
C      None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If the declared length of CREC is not 1000 characters,
C        the error SPICE(DAFBADRECLEN) is signalled.
C
C     2) If the specified record cannot (for some reason) be read,
C        the error SPICE(DAFCRNOTFOUND) is signalled.
C
C$ Particulars
C
C     Unlike double precision records, character records are
C     not buffered. Also, while failing to find a specific double
C     precision record is indicated through the calling sequence,
C     failing to find a character record results in an error.
C
C$ Examples
C
C     In the following example, matching summary and name records are
C     read from a DAF:
C
C        CALL DAFGDR ( HANDLE, NEXT,   DREC, FOUND )
C        CALL DAFRCR ( HANDLE, NEXT+1, CREC        )
C
C     Note that a character record always immediately follows a summary
C     record.
C
C$ Restrictions
C
C     1) This routine is only used to read records on environments
C        whose characters are a single byte in size.  Updates
C        to this routine and routines in its call tree may be
C        required to properly handle other cases.
C
C$ Literature_References
C
C     NAIF Document 167.0, "Double Precision Array Files (DAF)
C     Specification and User's Guide"
C
C$ Author_and_Institution
C
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
C
C        Updated this routine to make proper use of the new
C        handle manager functionality installed underneath
C        DAF.
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
C     read daf character record
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
C
C        This routine now makes use of the handle manager
C        code.  A call to DAFSIH was inserted just after
C        the standard SPICE error handling code at the
C        head of the module.  This was done to insure that
C        the caller is referring to a legitmately loaded
C        DAF.  The penalty for performing this check is
C        a binary search on the number of loaded files,
C        which should be small compared to the actual READ
C        performed below.
C
C        The call to DAFHLU has been replaced with ZZDDHHLU,
C        since calls to DAFHLU locks handles to their logical
C        units.
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               UNIT
      INTEGER               IOSTAT
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFRCR' )
      END IF
 
C
C     Check to be sure that HANDLE is attached to a file that is open
C     with read access.  If the call fails, check out and return.
C
      CALL DAFSIH ( HANDLE, 'READ' )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DAFRCR' )
         RETURN
      END IF
 
C
C     Now make certain that the string to receive the contents of
C     the character record is the appropriate length.
C
      IF ( LEN ( CREC ) .NE. 1000 ) THEN
         CALL SETMSG ( 'Expected length of character record is 1000. '//
     .                 'Passed string has length #' )
         CALL ERRINT ( '#', LEN( CREC ) )
         CALL SIGERR ( 'SPICE(DAFBADCRECLEN)' )
 
      ELSE
 
C
C        Retrieve a logical unit for this handle.  This has the
C        side-effect of locking this UNIT to HANDLE.
C
         CALL ZZDDHHLU ( HANDLE, 'DAF', .FALSE., UNIT )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'DAFRCR' )
            RETURN
         END IF
 
         READ (UNIT,
     .         REC=RECNO,
     .         IOSTAT=IOSTAT) CREC
 
         IF ( IOSTAT .NE. 0 ) THEN
            CALL SETMSG ( 'Could not read record #. IOSTAT was #.' )
            CALL ERRINT ( '#', RECNO                               )
            CALL ERRINT ( '#', IOSTAT                              )
            CALL SIGERR ( 'SPICE(DAFCRNOTFOUND)'                   )
         END IF
 
      END IF
 
      CALL CHKOUT ( 'DAFRCR' )
      RETURN
      END
