C$Procedure      KXTRCT ( Extract a substring starting with a keyword )
 
      SUBROUTINE KXTRCT ( KEYWD,  TERMS, NTERMS,
     .                    STRING, FOUND, SUBSTR  )
 
C$ Abstract
C
C     Locate a keyword in a string and extract the substring from
C     the beginning of the first word following the keyword to the
C     beginning of the first subsequent recognized terminator of a list.
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
C      SEARCH, PARSING, PARSING
C
C$ Declarations
 
      CHARACTER*(*)    KEYWD
      CHARACTER*(*)    TERMS(*)
      INTEGER          NTERMS
      CHARACTER*(*)    STRING
      LOGICAL          FOUND
      CHARACTER*(*)    SUBSTR
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      KEYWD      I   Word that marks the beginning of text of interest.
C      TERMS      I   Set of words, any of which marks the end of text.
C      NTERMS     I   Number of TERMS.
C      STRING    I/O  String containing a sequence of words.
C      FOUND      O   TRUE if the keyword is found in the string.
C      SUBSTR     O   String from end of KEYWD to beginning of first
C                     TERMS item found.
C
C$ Detailed_Input
C
C      KEYWD      is a word used to mark the start of text of interest.
C
C      TERMS      is a set of words, any one of which may signal the
C                 end of text of interest.
C
C      NTERMS     is the number of TERMS.
C
C      STRING     is a character string made up of words, that may
C                 contain the keyword in KEYWD.
C
C$ Detailed_Output
C
C      STRING     is the input string stripped of all words from
C                 the beginning of the keyword KEYWD to the end of
C                 the last word preceding one of the words in TERMS
C                 (or the end of the string if none of the TERMS follows
C                 KEYWD in the string).
C
C      FOUND      is .TRUE. if KEYWD is present in the input STRING.
C
C      SUBSTR     is the substring that begins with the first word
C                 following KEYWD up to the beginning of any of the
C                 words in TERM or the end of the string.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      Definitions:
C
C      A WORD        is a set of consecutive non-blank characters
C                    delimited by blanks or either end of the string
C                    that contains them.
C
C      Given a string and a keyword this routine locates the first
C      occurrence of the keyword in the string and returns the
C      substring between the end of the keyword and the first occurrence
C      of any of the words in a list of terminating words.  If none
C      of the terminating words follows the keyword in the string,
C      the routine returns all of the string following the keyword.
C
C      If the next word following the keyword is a terminating word,
C      the substring returned will be a blank.
C
C      If the keyword can not be located in the string, the variable
C      FOUND will be returned as .FALSE. and the input string will be
C      unchanged.  The substring will be returned as a blank.
C
C      In all other cases, the part of the input string from the
C      beginning of the keyword to the start of the first terminating
C      word will be removed.  If no terminating word follows the keyword
C      the portion of the string from the keyword to the last non-blank
C      character of the string will be removed.
C
C$ Examples
C
C  Example 1.
C  ----------
C       Input:  STRING  'FROM 1 October 1984 12:00:00 TO 1 January 1987'
C               KEYWD   'TO'
C               TERMS   'FROM'
C                       'TO'
C                       'BEGINNING'
C                       'ENDING'
C
C       Output: STRING  'FROM 1 October 1984 12:00:00 '
C               FOUND   .TRUE.
C               SUBSTR  '1 January 1987'
C
C
C
C  Example 2.
C  ----------
C       Input:  STRING  'FROM 1 October 1984 12:00:00 TO 1 January 1987'
C               KEYWD   'FROM'
C               TERMS   'FROM'
C                       'TO'
C                       'BEGINNING'
C                       'ENDING'
C
C       Output: STRING  ' TO 1 January 1987'
C               FOUND   .TRUE.
C               SUBSTR  '1 October 1984 12:00:00'
C
C
C
C  Example 3.
C  ----------
C       Input:  STRING  'ADDRESS: 4800 OAK GROVE DRIVE PHONE: 354-4321 '
C               KEYWD   'ADDRESS:'
C               TERMS   'ADDRESS:'
C                       'PHONE:'
C                       'NAME:'
C
C       Output: STRING  ' PHONE: 354-4321 '
C               FOUND   .TRUE.
C               SUBSTR  '4800 OAK GROVE DRIVE'
C
C
C  Example 4.
C  ----------
C       Input:  STRING  'ADDRESS: 4800 OAK GROVE DRIVE PHONE: 354-4321 '
C               KEYWD   'NAME:'
C               TERMS   'ADDRESS:'
C                       'PHONE:'
C                       'NAME:'
C
C       Output: STRING  'ADDRESS: 4800 OAK GROVE DRIVE PHONE: 354-4321 '
C               FOUND   .FALSE.
C               SUBSTR  ' '
C
C$ Restrictions
C
C      It is the user's responsibility to make sure there is adequate
C      room in SUBSTR to contain the substring.
C
C      SUBSTR cannot overwrite STRING.
C
C$ Exceptions
C
C      Error free.
C
C$ Files
C
C      None.
C
C$ Author_and_Institution
C
C      H.A. Neilan     (JPL)
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
C
C-&
 
C$ Index_Entries
C
C     extract a substring starting with a keyword
C
C-&
 
 
 
C$ Revisions
C
C-     Beta Version 1.1.0, 28-FEB-1989 (WLT)
C
C         Reference to REMSUB replaced by SHIFTL.
C
C-     Beta Version 1.0.1, 10-FEB-1989 (HAN)
C
C         Contents of the Exceptions section was changed
C         to "error free" to reflect the decision that the
C         module will never participate in error handling.
C
C-&
 
 
C
C     SPICELIB functions
C
      INTEGER          WDINDX
      INTEGER          NBLEN
      INTEGER          ISRCHC
 
C
C     Local variables
C
      INTEGER          POSITN
 
      INTEGER          BERASE
      INTEGER          EERASE
      INTEGER          BEGSTR
      INTEGER          ENDSTR
 
      INTEGER          START
      INTEGER          B
      INTEGER          E
      INTEGER          DELIMS
 
C
C     Locate the keyword within the string.
C
      POSITN  =  WDINDX ( STRING, KEYWD )
 
C
C     If the keyword wasn't found, set the outputs and head for home.
C
      IF ( POSITN .EQ. 0 ) THEN
         FOUND  = .FALSE.
         SUBSTR = ' '
         RETURN
      ELSE
         FOUND  = .TRUE.
      END IF
 
C
C     Set the begin erase marker to the start of the current word
C     Set the end   erase marker to the end   of the current word
C
      BERASE  = POSITN
      EERASE  = POSITN + NBLEN(KEYWD) - 1
      START   = EERASE + 1
 
C
C     Find the begin and end of the next word.
C
      CALL FNDNWD ( STRING, START, B, E )
 
C
C     If there is a next word ( E came back non-zero ) see if its a
C     terminator.
C
      IF ( E .NE. 0 ) THEN
         DELIMS = ISRCHC( STRING(B:E), NTERMS, TERMS )
      END IF
 
C
C     If we found a terminator, or were already at the end of the
C     string, we are done.  Remove the keyword and put a blank in
C     SUBSTR
C
      IF (      ( E      .EQ. 0 )
     .     .OR. ( DELIMS .NE. 0 ) ) THEN
 
         CALL SHIFTL ( STRING(BERASE:), EERASE-BERASE+1, ' ',
     .                 STRING(BERASE:)                        )
         SUBSTR = ' '
         RETURN
 
      END IF
 
C
C     Ok. If we made it this far,  we have at least one legitimate word
C     following the keyword,  set the pointer for the start of the
C     substring (to return) to the beginning of this word.
C
      BEGSTR = B
 
C
C     Now we just examine each word until we run out of string or we
C     run into a terminator.
C
      DO WHILE ( (E .NE. 0) .AND. (DELIMS .EQ. 0) )
 
         ENDSTR = E
         EERASE = E
         START  = E + 1
 
         CALL FNDNWD ( STRING, START, B, E )
 
         IF ( E .NE. 0 ) THEN
            DELIMS = ISRCHC ( STRING(B:E), NTERMS, TERMS )
         END IF
 
      END DO
 
C
C     That's it, load the substring variable and remove the keyword
C     and words up to the terminator or end of the string --- whichever
C     came first.
C
      SUBSTR = STRING ( BEGSTR : ENDSTR )
 
      CALL SHIFTL ( STRING(BERASE:), EERASE-BERASE+1, ' ',
     .              STRING(BERASE:)                         )
 
      RETURN
      END
