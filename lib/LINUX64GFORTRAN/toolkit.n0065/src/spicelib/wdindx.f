C$Procedure            WDINDX ( Index of a Word Within a String )
 
      INTEGER FUNCTION WDINDX ( STRING, WORD )
 
C$ Abstract
C
C     Find the index of a word within a string.  If the word does not
C     exist as a word within the string, the value zero is returned.
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
C      PARSING,  SEARCH,  WORD
C
C$ Declarations
 
      CHARACTER*(*)    STRING
      CHARACTER*(*)    WORD
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      STRING     I   String of characters, potentially containing words
C      WORD       I   A string of consecutive non-blank letters.
C      WDINDX     O   The location of the word within the string.
C
C$ Detailed_Input
C
C      STRING     String of characters, potentially containing words.
C      WORD       A string of consecutive non-blank letters.
C
C$ Detailed_Output
C
C      WDINDX     The location of the word within the string.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      A word within a string is a substring beginning and ending with
C      a non-blank characters that is delimited by blanks on each end.
C      ( A blank is assumed to precede and follow the first and last
C      characters of a string. )
C
C      Given a word, this routine returns the index of the first letter
C      of the first word of STRING that matches the word.
C
C$ Examples
C
C              STRING:
C                       1         2         3         4
C      WORD    1234567890123456789012345678901234567890123456    WDINDX
C      ------  ----------------------------------------------    ------
C     'POT'   'PUT THE POTATOES IN THE POT'                      25
C     'TOES'                                                     0
C     'PUT'                                                      1
C     'THE'                                                      5
C     'IN THE'                                                   18
C     'THE PO'                                                   0
C
C$ Restrictions
C
C      None.
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
C     index of a word within a string
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
      INTEGER          FRSTNB
      INTEGER          LASTNB
 
C
C     Local variables
C
      INTEGER          I
      INTEGER          J
 
      INTEGER          BEGWD
      INTEGER          ENDWD
 
      INTEGER          BEGSTR
      INTEGER          ENDSTR
 
      INTEGER          STRLEN
      INTEGER          WDLEN
 
      INTEGER          BGTOND
 
C
C     Find the ends of the word and input string.
C
      ENDSTR = LASTNB ( STRING )
      BEGSTR = FRSTNB ( STRING )
 
      ENDWD  = LASTNB ( WORD   )
      BEGWD  = FRSTNB ( WORD   )
 
C
C     Get the offset from the beginning of the word to the end of the
C     word, the word length and the string length.
C
      BGTOND =     ENDWD  - BEGWD
      WDLEN  = 1 + BGTOND
      STRLEN = 1 + ENDSTR - BEGSTR
 
C
C     We deal with all of the pathologies first...
C
      IF (      ( ENDWD  .LT. 1      )
     .     .OR. ( STRLEN .LT. WDLEN  ) ) THEN
 
C
C        ... If we got a blank word or a string that is too short, then
C        the index of the word is zero.
C
         WDINDX =  0
         RETURN
 
      ELSE IF ( STRLEN .EQ. WDLEN ) THEN
 
C
C        ... the word and string have the same non-blank length.
C        Either they match up or they don't.  Find out and return.
C
         IF ( STRING(BEGSTR:ENDSTR) .EQ. WORD(BEGWD:ENDWD) ) THEN
            WDINDX = BEGSTR
         ELSE
            WDINDX = 0
         END IF
 
         RETURN
      END IF
 
C
C     Ok.  Now we've got a realistic case to deal with.  The string
C     length is longer than the word length.  Check to see if we have a
C     match at the beginning of the string.
C
      I      = BEGSTR
      J      = I + BGTOND
 
      IF (       ( STRING(I:J)     .EQ. WORD(BEGWD:ENDWD) )
     .     .AND. ( STRING(J+1:J+1) .EQ. ' '               )  ) THEN
 
         WDINDX = I
         RETURN
 
      END IF
 
C
C     No luck yet?  Search the string until we find a word match or
C     we run out of string to check.
C
      I      = BEGSTR + 1
      J      = I      + BGTOND
 
      DO WHILE (       ( J                    .LT. ENDSTR            )
     .           .AND.
     .           .NOT. (     (STRING(I  :J  ) .EQ. WORD(BEGWD:ENDWD) )
     .                  .AND.(STRING(I-1:I-1) .EQ. ' '               )
     .                  .AND.(STRING(J+1:J+1) .EQ. ' '               )
     .                 )
     .         )
         I   = I + 1
         J   = J + 1
      END DO
 
C
C     If J equals ENDSTR then no match was found in the interior of the
C     string.  We make a last check at the end.
C
      IF ( J .EQ. ENDSTR ) THEN
 
         IF (       ( STRING(I-1:I-1) .EQ. ' '               )
     .        .AND. ( STRING(I  :J  ) .EQ. WORD(BEGWD:ENDWD) ) ) THEN
            WDINDX = I
         ELSE
            WDINDX = 0
         END IF
 
      ELSE
C
C        The only way to get here is if we exited the above loop before
C        running out of room --- that is we had a word match.  Set
C        the index to the value of "I" that got us out of the loop.
C
         WDINDX    = I
 
      END IF
 
      RETURN
 
      END
