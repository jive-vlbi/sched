C$Procedure DAFWCR ( DAF, write character record )
 
      SUBROUTINE DAFWCR ( HANDLE, RECNO, CREC )
 
C$ Abstract
C
C     Write or rewrite the contents of a character record to
C     a DAF.
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
C     CREC       I   Character record.
C
C$ Detailed_Input
C
C     HANDLE      is the handle associated with a DAF.
C
C     RECNO       is the record number of a character record within
C                 the file. If the record does not already exist, it
C                 is created. Otherwise its contents are overwritten.
C
C     CREC        contains the first 1000 characters of the specified
C                 record.
C
C$ Detailed_Output
C
C     None.
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
C     1) If the specified file is not open for write access, an error
C        is signaled by routines in the call tree of this routine.
C
C     2) If the declared length of CREC is not 1000 characters,
C        the error SPICE(DAFBADRECLEN) is signaled.
C
C     2) If the specified record cannot (for some reason) be written,
C        the error SPICE(DAFWRITEFAIL) is signaled.
C
C$ Particulars
C
C     Unlike double precision records, character records are
C     not buffered.
C
C$ Examples
C
C     In the following example, matching summary and name records are
C     written to a DAF:
C
C        CALL DAFWDR ( HANDLE, NEXT,   DREC )
C        CALL DAFWCR ( HANDLE, NEXT+1, CREC )
C
C     Note that a character record always immediately follows a summary
C     record.
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
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 27-NOV-2001 (FST)
C
C        Updated this routine to utilize new handle manager
C        interfaces.  Replaced the check of the input handle's
C        sign with the appropriate call to DAFSIH.
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
C     write daf character record
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 27-NOV-2001 (FST)
C
C        The call to DAFHLU has been replaced with a call to
C        ZZDDHHLU, the handle manager interface for retrieving
C        a logical unit.  DAFHLU is no longer used, since it
C        locks the unit returned to its HANDLE, tying up resources
C        in the handle manager.  A call to DAFSIH was inserted to
C        make certain that HANDLE is present in DAFAH's file table,
C        rather than simply checking the sign of HANDLE.
C
C-&
 
C
C     SPICELIB functions
C
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
         CALL CHKIN ( 'DAFWCR' )
      END IF
 
      CALL ZZDDHHLU ( HANDLE, 'DAF', .FALSE., UNIT )
 
C
C     Look out for
C
C       -- Writing to a file that is open for read-only.
C
C       -- Trying to write a record that doesn't have length 1000.
C
C       -- Failed write.
C
      CALL DAFSIH ( HANDLE, 'WRITE' )
 
      IF ( LEN ( CREC ) .NE. 1000 ) THEN
 
         CALL SETMSG ( 'Expected length of character record is 1000.' //
     .                 ' Length of passed record is #' )
         CALL ERRINT ( '#', LEN ( CREC ) )
         CALL SIGERR ( 'SPICE(DAFBADCRECLEN)' )
 
      ELSE
 
         WRITE (UNIT,
     .          REC=RECNO,
     .          IOSTAT=IOSTAT) CREC
 
         IF ( IOSTAT .NE. 0 ) THEN
            CALL SETMSG ( 'Character record write failed. Value of '  //
     .                    'IOSTAT was #' )
            CALL ERRINT ( '#', IOSTAT )
            CALL SIGERR ( 'SPICE(DAFWRITEFAIL)' )
         END IF
      END IF
 
      CALL CHKOUT ( 'DAFWCR' )
      RETURN
      END
