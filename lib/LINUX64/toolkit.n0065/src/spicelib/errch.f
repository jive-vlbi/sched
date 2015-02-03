C$Procedure      ERRCH  ( Insert String into Error Message Text )
 
      SUBROUTINE ERRCH ( MARKER, STRING )
 
C$ Abstract
C
C     Substitute a character string for the first occurrence of
C     a marker in the current long error message.
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
C     ERROR
C
C$ Keywords
C
C     ERROR
C     CONVERSION
C
C$ Declarations
 
      INCLUDE 'errhnd.inc'
 
      CHARACTER*(*)         MARKER
      CHARACTER*(*)         STRING
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  ---------------------------------------------------
C     MARKER     I   A substring of the error message to be replaced.
C     STRING     I   The character string to substitute for MARKER.
C
C$ Detailed_Input
C
C
C     MARKER     is a character string that marks a position in
C                the long error message where a character string
C                is to be substituted.  Leading and trailing blanks
C                in MARKER are not significant.
C
C                Case IS significant:  'XX' is considered to be
C                a different marker from 'xx'.
C
C     STRING     is a character string that will be substituted for
C                the first occurrence of MARKER in the long error
C                message.  This occurrence of the substring indicated
C                by MARKER will be removed and replaced by STRING.
C                Leading and trailing blanks in STRING are not
C                significant.  However, if STRING is completely blank,
C                a single blank character will be substituted for
C                the marker.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     LMSGLN  is the maximum length of the long error message.  See
C             the include file errhnd.inc for the value of LMSGLN.
C
C$ Exceptions
C
C     1)  If the character string resulting from the substitution
C         exceeds the maximum length of the long error message, the
C         long error message is truncated on the right.  No error is
C         signalled.
C
C     2)  If MARKER is blank, no substitution is performed.  No error
C         is signalled.
C
C     3)  If STRING is blank, then the first occurrence of MARKER is
C         replaced by a single blank.
C
C     4)  If MARKER does not appear in the long error message, no
C         substitution is performed.  No error is signalled.
C
C     5)  If changes to the long error message are disabled, this
C         routine has no effect.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The purpose of this routine is to allow you to tailor the long
C     error message to include specific information that is available
C     only at run time.  This capability is somewhat like being able to
C     put variables in your error messages.
C
C$ Examples
C
C      1)   In this example, the marker is  '#'.  We'll signal a file
C           open error, and we'll include in the error message the name
C           of the file we tried to open.  There are three steps:
C
C              -- Set the long message, using a marker for the location
C                 where a value is to be substituted.
C
C              -- Substitute the file name into the error message.
C
C              -- Signal the error (causing output of error messages)
C                 using the SPICELIB routine SIGERR.
C
C              C
C              C     Error on file open attempt.  Signal an error.
C              C     The character string variable FILE contains the
C              C     file name.
C              C
C              C     After the call to ERRCH, the long error message
C              C     will contain the file name held in the string
C              C     FILE.  For example, if FILE contains the name
C              C     'MYFILE.DAT', the long error message will be
C              C
C              C         'File open error.  File is MYFILE.DAT.'
C              C
C
C                    CALL SETMSG ( 'File open error.  File is #.' )
C                    CALL ERRCH  ( '#',  FILE                     )
C                    CALL SIGERR ( 'SPICE(FILEOPENFAILED)'        )
C
C
C      2)   Same example as (1), except this time we'll use a better-
C           looking and more descriptive marker than '#'.  Instead,
C           we'll use the marker 'FILENAME'.  This does not affect the
C           long error message; it just makes the code more readable.
C
C              C
C              C     Error on file open attempt.  Signal an error.
C              C     The character string variable FILE contains the
C              C     file name.
C              C
C                    CALL SETMSG ( 'File open error. File is FILENAME.')
C                    CALL ERRCH  ( 'FILENAME',  FILE                   )
C                    CALL SIGERR ( 'SPICE(FILEOPENFAILED)'             )
C
C
C      3)   Same example as (2), except this time there's a problem with
C           the variable FILE: it's blank.  This time, the code fragment
C
C              C
C              C     Error on file open attempt.  Signal an error.
C              C     The character string variable FILE contains the
C              C     file name.
C              C
C                    CALL SETMSG ( 'File open error. File is FILENAME.')
C                    CALL ERRCH  ( 'FILENAME',  FILE                   )
C
C           sets the long error message to
C
C              'File open error.  File is  '.
C
C
C$ Restrictions
C
C     1) The caller must ensure that the message length, after sub-
C        stitution is performed, doesn't exceed LMSGLN characters.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     J.E. McLean     (JPL)
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.1.0, 29-JUL-1997 (NJB)
C
C        Maximum length of the long error message is now represented
C        by the parameter LMSGLN.
C
C-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 2.0.0 25-MAR-1991  (JEM) (NJB)
C
C        When the input value of STRING is blank, this version
C        replaces the first occurrence of MARKER with a
C        single blank character.   Header was edited to improve
C        clarity.  Cosmetic changes to code were made.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990  (NJB)
C
C-&
 
C$ Index_Entries
C
C     insert string into error message text
C
C-&
 
 
 
 
C$ Revisions
C
C-    SPICELIB Version 2.1.0, 29-JUL-1997 (NJB)
C
C        Maximum length of the long error message is now represented
C        by the parameter LMSGLN.
C
C-    SPICELIB Version 2.0.0 25-MAR-1991  (JEM) (NJB)
C
C        When the input value of STRING is blank, this version
C        replaces the first occurrence of MARKER with a
C        single blank character.  The previous version made
C        no substitution, leaving the marker in the long error
C        message.
C
C        The $Exceptions, $Examples, and $Particulars sections were
C        updated to improve accuracy and clarity.  Some cosmetic
C        changes were made as well.
C
C        Also, some cosmetic changes to the code were made.
C-&
 
C
C     SPICELIB functions
C
      INTEGER               FRSTNB
      INTEGER               LASTNB
      INTEGER               NBLEN
      LOGICAL               ALLOWD
 
C
C     Local Variables:
C
      CHARACTER*(LMSGLN)    LNGMSG
      CHARACTER*(LMSGLN)    TMPMSG
 
      INTEGER               MRKPOS
 
 
C
C     Changes to the long error message must be allowed, or we do
C     nothing.
C
      IF ( .NOT. ALLOWD() ) THEN
         RETURN
      END IF
 
C
C     MARKER must have some non-blank characters, or we do nothing.
C
      IF (  LASTNB (MARKER) .EQ. 0  ) THEN
         RETURN
      END IF
 
C
C     Get a copy of the current long error message.
C
      CALL GETLMS ( LNGMSG )
 
C
C     Locate the leftmost occurrence of MARKER, if there is one
C     (ignoring leading and trailing blanks):
C
      MRKPOS = INDEX (  LNGMSG,
     .                  MARKER ( FRSTNB(MARKER):LASTNB(MARKER) )    )
 
      IF  ( MRKPOS .EQ. 0 ) THEN
C
C        MARKER does not occur in the long error message, so there's
C        no subsitution to perform.
C
         RETURN
 
      ELSE
C
C        We put together TMPMSG, a copy of LNGMSG with MARKER
C        replaced by STRING.
C
         IF ( MRKPOS .GT. 1 ) THEN
C
C           MARKER is not at the beginning of the long error message.
C
            IF (  MRKPOS + NBLEN(MARKER)  .LE.  LASTNB(LNGMSG)  ) THEN
C
C              There's more of the long message after the marker.
C
               IF ( STRING .NE. ' ' ) THEN
 
                  TMPMSG = LNGMSG ( :MRKPOS - 1 )                     //
     .                     STRING ( FRSTNB(STRING) : LASTNB(STRING) ) //
     .                     LNGMSG ( MRKPOS + NBLEN(MARKER): )
 
               ELSE
 
                  TMPMSG = LNGMSG ( :MRKPOS - 1 )                     //
     .                     ' '                                        //
     .                     LNGMSG ( MRKPOS + NBLEN(MARKER): )
 
              END IF
 
            ELSE
C
C              The long error message ends with MARKER.
C
               IF ( STRING .NE. ' ' ) THEN
 
                  TMPMSG = LNGMSG ( :MRKPOS - 1 )                     //
     .                     STRING ( FRSTNB(STRING) : LASTNB(STRING) )
 
               ELSE
 
                  TMPMSG = LNGMSG ( :MRKPOS - 1 )   //  ' '
 
               END IF
 
            END IF
 
 
         ELSE
C
C           The long error message starts with MARKER (MRKPOS is 1).
C
            IF ( NBLEN(MARKER) .LT. LASTNB(LNGMSG) ) THEN
C
C              There's more of the long message after the marker...
C
               IF ( STRING .NE. ' ' ) THEN
 
                  TMPMSG = STRING ( FRSTNB(STRING) : LASTNB(STRING) ) //
     .                     LNGMSG ( 1 + NBLEN(MARKER) : )
               ELSE
 
                  TMPMSG = ' ' //  LNGMSG ( 1 + NBLEN(MARKER) : )
 
               END IF
 
            ELSE
C
C              The marker's the whole string:
C
               IF ( STRING .NE. ' ' ) THEN
 
                  TMPMSG = STRING ( FRSTNB(STRING) : LASTNB(STRING) )
 
               ELSE
 
                  TMPMSG = ' '
 
               END IF
 
            END IF
 
         END IF
 
C
C        Update the long error message:
C
         CALL PUTLMS ( TMPMSG )
 
      END IF
 
      END
