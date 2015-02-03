C$Procedure ZZFTPCHK ( Private --- Check for FTP Errors )
 
      SUBROUTINE ZZFTPCHK ( STRING, FTPERR )
 
C$ Abstract
C
C    Check a character string that may contain the FTP validation
C    string for FTP based errors.
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
C     None.
C
C$ Keywords
C
C     FILE
C     UTILITY
C
C$ Declarations
 
      IMPLICIT NONE
 
      INCLUDE              'zzftprms.inc'
 
      CHARACTER*(*)         STRING
      LOGICAL               FTPERR
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STRING     I   String that may contain the FTP validation string.
C     FTPERR     O   Logical indicating if FTP corruption occurred.
C
C$ Detailed_Input
C
C     STRING     is a string read in directly from a binary file.  This
C                string should, but does not have to, contain the FTP
C                validation string.  Typically this block of characters
C                is read in from the file record of the binary kernel.
C                If 'FTPSTR' or 'ENDFTP' occur anywhere in STRING, then
C                validation will be attempted.  Multiple occurrences of
C                these two special strings in STRING is also an issue.
C                See Restrictions for details.
C
C$ Detailed_Output
C
C     FTPERR     is a logical that indicates whether or not an FTP
C                error has occurred.  If an error is detected FTPERR
C                is set to TRUE, otherwise FTPERR is FALSE.  In the
C                event that STRING does not contain either of the FTP
C                bracketing strings, then the test will not be
C                performed.  Thus, FTPERR is set to FALSE.
C
C$ Parameters
C
C     See include file zzftprms.inc.
C
C$ Files
C
C     Although this routine validates information from a binary file,
C     it does not interact with the file directly, and relies upon
C     the caller to pass the appropriate string block.
C
C$ Exceptions
C
C     1) In the event that both the left and right end markers of the
C        FTP validation string are not present in STRING, the routine
C        assumes that this information is from a pre-FTP test file.  As
C        such, the file can not be validated, so FTPERR remains FALSE.
C
C     2) If the FTP string brackets 'FTPSTR' and 'ENDFTP' are present in
C        multiple places in the text block, then this routine assumes
C        the last occurrence of the substring these strings bracket is
C        the FTP test sequence that requires validation.  So if this
C        routine encounters a block of text:
C
C        ...FTPSTR:<TESTSQ>:ENDFTP...FTPSTR[THISISNOTATEST]ENDFTP...
C
C        where <TESTSQ> is the actual test sequence, then it will
C        incorrectly compare [THISISNOTATEST] to the test component
C        returned from ZZFTPSTR.
C
C$ Particulars
C
C     The purpose of this routine is to examine for FTP errors a
C     string brought in from a binary kernel.  This text may or may
C     not contain the FTP validation string defined in ZZFTPSTR.
C     However, if it contains at least one of the two bracketing
C     substrings ('FTPSTR' and 'ENDFTP'), then the routine assumes
C     that the text is subject to FTP validation. As a result of this,
C     the caller should avoid passing in user controlled chunks of
C     character data from the file.  If the user has decided to place
C     one of the FTP string bracket components in this portion of the
C     file, then ZZFTPCHK may be confused and incorrectly indicate an
C     error condition.
C
C$ Examples
C
C     The following code fragment from DAFOPR reads in the DAF file
C     record and attempts to examine the contents for FTP errors.
C     (Note: this code fragment is from a 32 bit word length, 1
C     byte character environment.)
C
C     C
C     C     Check for FTP transfer errors to prevent the user from
C     C     inadvertantly using a damaged kernel. First read the file
C     C     record into a string of 1000 characters.
C     C
C           READ ( UNIT = LUN, REC = 1, IOSTAT = IOSTAT ) FTPTST
C
C           IF ( IOSTAT .NE. 0 ) THEN
C
C              CLOSE       ( LUN                                     )
C              CALL SETMSG ( 'Error reading the file record from'   //
C          .                 ' the binary DAF file ''#''. IOSTAT'   //
C          .                 ' = #.'                                 )
C              CALL ERRCH  ( '#', FNAME                              )
C              CALL ERRINT ( '#', IOSTAT                             )
C              CALL SIGERR ( 'SPICE(FILEREADFAILED)'                 )
C              CALL CHKOUT ( 'DAFOPR'                                )
C              RETURN
C
C           END IF
C
C     C
C     C     Since we are dealing with DAF files, only place the
C     C     last 500 characters of data from the file record into
C     C     ZZFTPCHK.  This ensures that the internal filename
C     C     and the ID word do not interfere with the FTP validation
C     C     process.
C     C
C           CALL ZZFTPCHK ( FTPTST(501:1000), FTPERR )
C
C           IF ( FTPERR ) THEN
C
C              CLOSE       ( LUN                                      )
C              CALL SETMSG ( 'FTP transfer error.  This binary DAF, '//
C          .                 '''#'', has most likely been corrupted '//
C          .                 'by an ASCII mode FTP transfer. '       //
C          .                 'Re-obtain the file using IMAGE or '    //
C          .                 'BINARY transfer mode.'                  )
C              CALL ERRCH  ( '#', FNAME                               )
C              CALL SIGERR ( 'SPICE(FTPXFERERROR)'                    )
C              CALL CHKOUT ( 'DAFOPR'                                 )
C              RETURN
C
C           END IF
C
C$ Restrictions
C
C     1) STRING may contain multiple occurrences of the FTP bracketing
C        substrings ('FTPSTR' and 'ENDFTP'), but only if the last
C        occurrence of both brackets the actual data for validation.
C
C     2) This routine assumes the presence of either 'FTPSTR' or
C        'ENDFTP' in STRING indicates that validation is to be
C        attempted.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 22-MAR-1999 (FST)
C
C
C-&
 
C$ Index_Entries
C
C     check text block for FTP errors
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.0.0, 22-MAR-1999 (FST)
C
C        This routine does not require modification if the FTP
C        validation string is updated according to the guidelines
C        laid out in ZZFTPSTR.  The reason for this is the
C        verification algorithm extracts the chunk of text between
C        'FTPSTR' and 'ENDFTP'.  It then checks to see whether or
C        not this chunk is a subset of the test component stored
C        in ZZFTPSTR. Two cases:
C
C           (1) It is.  Then this indicates that at the worst,
C               the chunk is from a valid file with an earlier
C               version of the FTP validation string.
C
C           (2) It is not.  While this is a fair indication that
C               the file may be corrupt, it's not a complete
C               treatment, since we may be examining a file
C               created with a newer version of the FTP validation
C               string.  So now check to see whether that test
C               component from ZZFTPSTR is a subset of the text
C               chunk from STRING.  If it is, then the file is
C               as valid as far as this version of the toolkit is
C               concerned.  Otherwise, the file is damaged.
C
C-&
 
C
C     SPICELIB Functions
C
      INTEGER               POS
      INTEGER               RTRIM
 
C
C     Local Variables
C
      CHARACTER*(SIZDLM)    DELIM
      CHARACTER*(SIZEXP)    FILSTR
      CHARACTER*(SIZEND)    LFTBKT
      CHARACTER*(SIZSTR)    MEMSTR
      CHARACTER*(SIZEND)    RGTBKT
 
      INTEGER               FSMIDX
      INTEGER               LENGTH
      INTEGER               MSFIDX
 
      LOGICAL               FIRST
      LOGICAL               ISTHER
 
C
C     Saved Variables
C
      SAVE                  FIRST
      SAVE                  LFTBKT
      SAVE                  MEMSTR
      SAVE                  RGTBKT
 
C
C     Data Statements
C
      DATA   FIRST        / .TRUE. /
 
C
C     On the first pass through, fetch a copy of the current FTP
C     validation string.
C
      IF ( FIRST ) THEN
 
         CALL ZZFTPSTR ( MEMSTR, LFTBKT, RGTBKT, DELIM )
 
C
C        Don't fetch the string on subsequent calls to this routine.
C
         FIRST = .FALSE.
 
      END IF
 
C
C     Extract the FTP validation string from the block of text that
C     was passed into the routine via the argument STRING. Note,
C     if the bracketed substring in the text block STRING is larger
C     than the FILSTR string size, ZZRBRKST will truncate the data
C     that does not fit.  This loss of data is not an issue, since in
C     this case we may only validate the part of the substring near
C     the head, for which we have enough room in FILSTR.
C
      CALL ZZRBRKST ( STRING,
     .                LFTBKT ( 1:RTRIM(LFTBKT) ),
     .                RGTBKT ( 1:RTRIM(RGTBKT) ),
     .                FILSTR,
     .                LENGTH,
     .                ISTHER                      )
 
C
C     Now check ISTHER to see if either LFTBKT or RGTBKT was present
C     in the block of text from the file. If both are absent, then
C     we must assume that this text is from a pre-FTP validation file,
C     and as such do not return any indication of an error.
C
      IF ( .NOT. ISTHER ) THEN
 
         FTPERR = .FALSE.
 
C
C     If one of the brackets is present, then we may proceed with
C     validation.  First check to see if the length is 0.  If it is,
C     then at least one of the brackets was present, but ZZRBRKST was
C     unable to extract a properly bracketed substring.  This is an
C     error.
C
      ELSE IF ( LENGTH .LE. 0 ) THEN
 
         FTPERR = .TRUE.
 
C
C     Now we make it to this ELSE statement only if ISTHER is TRUE, and
C     LENGTH is a positive number.  Compare the contents of FILSTR
C     and MEMSTR.
C
      ELSE
 
C
C        First determine if the data from the file is a subset of
C        what is stored in memory.
C
         FSMIDX = POS (  MEMSTR,  FILSTR( 1:RTRIM(FILSTR) ),  1  )
 
C
C        In the event that FSMIDX is non-zero, we know that FILSTR
C        is a substring of MEMSTR, and thus we have validated all the
C        test clusters from the file.
C
         IF ( FSMIDX .NE. 0 ) THEN
 
            FTPERR = .FALSE.
 
C
C        If FSMIDX is 0, then we do not yet know whether or not the
C        file is valid.  Now it may be the case that this file contains
C        a newer FTP validation string than this version of the
C        toolkit is aware.  Check to see whether what's in memory
C        is a substring of what's in FILSTR.
C
         ELSE
 
            MSFIDX = POS (  FILSTR,  MEMSTR( 1:RTRIM(MEMSTR) ),  1  )
 
C
C           If this comes back as zero, then we definitely have
C           an FTP error. Set FTPERR appropriately.
C
            FTPERR = ( MSFIDX .EQ. 0 )
 
         END IF
 
      END IF
 
      RETURN
      END
