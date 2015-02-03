C$Procedure      FNDNWD ( Find the next word after an index )
 
      SUBROUTINE FNDNWD ( STRING, START, B, E )
 
C$ Abstract
C
C     Find the beginning and end of the first word starting at
C     or after a specified character.
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
C     PARSING,  SEARCH,  WORD
C
C$ Declarations
 
      CHARACTER*(*)    STRING
      INTEGER          START
      INTEGER          B
      INTEGER          E
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STRING     I   A string to examine for words.
C     START      I   Position in the string to start looking for words.
C     B          O   String position of first character of the word.
C     E          O   String position of last character of the word.
C
C$ Detailed_Input
C
C     STRING     Is a character string that potentially consists of
C                words of text.
C
C     START      Is the index of a letter within the string from which
C                to start looking for the next word.
C
C$ Detailed_Output
C
C     B          Is the index of the first letter of the word substring
C                of STRING that begins at or after position START.  If
C                there are no such substrings I is returned as 0.
C
C     E          Is the index of the last letter of the word substring
C                of STRING that begins at or after position START.  If
C                there are no such substrings J is returned as 0.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C     Given a character string and location of a character within that
C     string, this routine finds the first full word of the string
C     that starts on or after the specified location.
C
C$ Examples
C                       1         2         3         4         5
C              12345678901234567890123456789012345678901234567890
C     STRING: 'Now is the time for all good men to go home to bed'
C
C     START    I      J
C     -----   ---    ---
C     1        1      3
C     2        5      6
C     3        5      6
C     4        5      6
C     5        5      6
C     6        8      10
C     7        8      10
C     8        8      10
C     9        12     15
C
C     48       48     50
C     49       0      0
C     111      0      0
C
C$ Restrictions
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 15-OCT-1993 (WLT)
C
C        The routine was completely rewritten with a resulting
C        increase in execution speed of between 2000% and 6000%.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
C
C-&
 
C$ Index_Entries
C
C     find the next word after an index
C
C-&
 
C$ Revisions
C
C-     SPICELIB Version 2.0.0, 15-OCT-1993 (WLT)
C
C         The routine was completely rewritten with a resulting
C         increase in execution speed of between 2000% and 6000%.
C         It was tested against the old version of the routine to
C         ensure that the functionality was exactly duplicated.
C
C-&
 
C
C     Local Variables
C
      INTEGER               BLANK
      INTEGER               L
      INTEGER               I
      INTEGER               N
      INTEGER               SIZE
 
      LOGICAL               LASTN
      LOGICAL               THISB
 
C
C     Set up neede parameters and check obvious out-of-bound cases.
C
      BLANK = ICHAR(' ')
      SIZE  = LEN(STRING)
 
      IF ( START .GT. SIZE ) THEN
         B = 0
         E = 0
         RETURN
      END IF
 
      N = MAX(1,START)
      L = N - 1
 
      IF ( L .LE. 0 ) THEN
         LASTN = .FALSE.
      ELSE
         LASTN = ICHAR( STRING(L:L) ) .NE. BLANK
      END IF
 
      THISB = ICHAR( STRING(N:N) ) .EQ. BLANK
 
C
C     Search for the beginning of a word (the last character
C     blank and the current non-blank).
C
      DO WHILE ( THISB .OR. LASTN )
 
         N = N + 1
 
         IF ( N .GT. SIZE ) THEN
            B = 0
            E = 0
            RETURN
         END IF
 
         LASTN = .NOT.   THISB
         THISB =  ICHAR( STRING(N:N) ) .EQ. BLANK
 
      END DO
 
C
C     If we get this far, we found the beginning of the
C     string.  To find the end look for the next blank and
C     back up one.
C
      B     =  N
 
      DO I = N+1, SIZE
 
         IF ( ICHAR(STRING(I:I)) .EQ. BLANK ) THEN
            E = I - 1
            RETURN
         END IF
 
      END DO
 
C
C     If we get this far, the word ends at the end of the
C     string.
C
      E = SIZE
      RETURN
 
      END
