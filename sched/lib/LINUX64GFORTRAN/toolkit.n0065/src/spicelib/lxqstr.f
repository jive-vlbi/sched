C$Procedure      LXQSTR ( Lex quoted string )
 
      SUBROUTINE LXQSTR ( STRING, QCHAR, FIRST, LAST, NCHAR )
 
C$ Abstract
C
C     Lex (scan) a quoted string.
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
C     CHARACTER
C     PARSING
C     SCANNING
C     STRING
C     UTILITY
C
C$ Declarations
 
      CHARACTER*(*)         STRING
      CHARACTER*(1)         QCHAR
      INTEGER               FIRST
      INTEGER               LAST
      INTEGER               NCHAR
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     STRING     I   String to be scanned.
C     QCHAR      I   Quote delimiter character.
C     FIRST      I   Character position at which to start scanning.
C     LAST       O   Character position of end of token.
C     NCHAR      O   Number of characters in token.
C
C$ Detailed_Input
C
C     STRING         is a character string that may contain a `string
C                    token' starting at the character position
C                    indicated by the input argument FIRST (see below).
C                    String tokens are sequences of characters that
C                    represent literal strings.  Syntactically, a string
C                    token is a sequence of characters that begins and
C                    ends with a designated `quote character'.  Within
C                    the token, any occurrence of the quote character
C                    is indicated by an adjacent pair of quote
C                    characters:  for example, if the quote character is
C
C                       "
C
C                    then the token representing one instance of this
C                    character is
C
C                       """"
C
C                    Here the first quote indicates the beginning of the
C                    token, the next two quotes together indicate a
C                    single quote character that constitutes the
C                    `contents' of the token, and the final quote
C                    indicates the end of the token.
C
C     QCHAR          is the quote character.  This is always a single
C                    character.  The characters
C
C                       "  and '
C
C                    are common choices, but any non-blank character is
C                    accepted.  Case *is* significant in QCHAR.
C
C
C     FIRST          is the character position at which the routine
C                    is to start scanning a quoted string token.  Note
C                    that the character STRING(FIRST:FIRST) must equal
C                    QCHAR if a string token is to be found; this
C                    routine does *not* attempt to locate the first
C                    quoted string following the position FIRST.
C
C$ Detailed_Output
C
C     LAST           is the last character position such that the
C                    subtring STRING(FIRST:LAST) is a quoted string
C                    token, if such a substring exists.  Otherwise, the
C                    returned value of LAST is FIRST-1.
C
C     NCHAR          is the length of the string token found by this
C                    routine, if such a token exists.  This length
C                    includes the starting and ending bracketing quotes.
C                    If a string token is not found, the returned value
C                    of NCHAR is zero.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1) If the input argument FIRST is less than 1 or greater than
C        LEN(STRING)-1, the returned value of LAST is FIRST-1, and the
C        returned value of NCHAR is zero.
C
C     2) It is not an error for a quoted string token to consist of
C        two consecutive quote characters with no intervening
C        characters.  Calling routines that require special treatment
C        of null tokens must handle this case.
C
C     3) If the input argument QCHAR is blank, the returned value of
C        LAST is FIRST-1, and the returned value of NCHAR is zero.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Quote characters may be ANY non-blank character.  For example, the
C     ampersand
C
C        &
C
C     is a perfectly valid quote character.  If we were using the
C     ampersand as the quote character, then the term `doubled quote'
C     in the following discussion would refer to the sequence
C
C        &&
C
C     not the character
C
C        "
C
C     The string tokens identified by this routine are Fortran-style
C     quoted strings:  they start and end with quote characters.  In the
C     interior of any such token, any quote characters are represented
C     by doubled quote characters.  These rules imply that the number of
C     quote characters in a quoted string token is always even.  The end
C     of a quoted string token is located at the first even-numbered
C     quote character, counting from the initial quote character, that
C     is  not the first member of a pair of quotes indicating an
C     embedded quote character.
C
C     To map the token to the string of characters it represents, use
C     the SPICELIB subroutine PARSQS (String parse, quoted).  PARSQS
C     removes the bracketing quotes from a quoted string token and
C     converts each doubled quote between the bracketing quotes to a
C     single quote.  For example, the token
C
C        """"
C
C     identified by this routine would be mapped by PARSQS to a string
C     variable containing the single character
C
C        "
C
C$ Examples
C
C     1)  The table below illustrates the action of this routine.
C
C
C         STRING CONTENTS               QCHAR   FIRST   LAST   NCHAR
C         ==========================================================
C         The "SPICE" system            "       5       11     7
C         The "SPICE" system            "       1       0      0
C         The "SPICE" system            '       5       4      0
C         The """SPICE"" system"        "       5       22     18
C         The """SPICE"""" system       "       5       15     11
C         The &&&SPICE system           &       5       6      2
C         ' '                           '       1       3      3
C         ''                            '       1       2      2
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 25-FEB-2002 (NJB)
C
C        Corrected references to other SPICELIB routines in header.
C
C-    SPICELIB Version 1.0.0, 20-OCT-1994 (NJB)
C
C-&
 
C$ Index_Entries
C
C     scan quoted string token
C     lex quoted string token
C     recognize quoted string token
C
C-&
 
C
C     Local variables
C
      INTEGER               L
      INTEGER               LOC
      INTEGER               POS
 
      LOGICAL               EVEN
 
 
C
C     Error free, no check-in required.
C
      L    =   LEN(STRING)
 
C
C     Handle the cases in which we can tell right away that
C     no token can be found.
C
      IF (       (  FIRST                .LT.  1      )
     .      .OR. (  FIRST                .GT.  L-1    )
     .      .OR. (  QCHAR                .EQ.  ' '    )
     .      .OR. (  STRING(FIRST:FIRST)  .NE.  QCHAR  )   ) THEN
 
         LAST  = FIRST - 1
         NCHAR = 0
         RETURN
 
      END IF
 
C
C     We started out with a quote character, if we got this far.  Now
C     we have to see whether a quoted string token exists.  Note that
C     we can safely assume FIRST+1 does not exceed L.
C
      LOC = INDEX (  STRING( FIRST+1: L ),  QCHAR  )
 
      IF ( LOC .EQ. 0 ) THEN
 
         LAST  = FIRST - 1
         NCHAR = 0
         RETURN
 
      END IF
 
C
C     At this point, we have a candidate ending point for the token.
C     We must search for the actual end of the token.  The token ends
C     at the first even-numbered quote character that is not part of
C     an embedded pair of quotes.
C
C     Our method of looking for the end of the token will be to search
C     from left to right, keeping track of the rightmost character
C     position that could be the end of the string token, until we find
C     a definitive answer as to the status of our candidate.
C     The variable LAST will be used for this candidate character
C     position.  The variable EVEN will tell us whether we've seen an
C     even number of quotes.  The variable POS will point to the current
C     character to examine.
C
      LAST  =  FIRST + LOC
      EVEN  = .TRUE.
      POS   =  LAST  + 1
 
 
      DO WHILE (  POS  .LE.  L  )
 
         IF ( STRING(POS:POS) .EQ. QCHAR ) THEN
C
C           Each quote character we see toggles the quote parity.
C
            EVEN  =  .NOT. EVEN
C
C           If the current parity is even, the current quote character
C           becomes the candidate for the final quote.  This character
C           can lose out only to a quote that is further to the right.
C
            IF ( EVEN ) THEN
               LAST = POS
            END IF
 
         ELSE
 
            IF ( EVEN ) THEN
C
C              The last even-numbered quote was followed by a non-quote
C              character.  We're done.
C
               NCHAR  =  LAST - FIRST + 1
               RETURN
 
            END IF
 
         END IF
 
 
         POS  =  POS + 1
 
C
C        At this point in the loop,
C
C           EVEN indicates whether we've seen an even number of quote
C           characters.
C
C           LAST is the index, relative to the start of the string,
C           of the last even-numbered quote we've seen.  This is the
C           current candidate for the closing quote.
C
C           POS is the index of the next character to examine.
C
      END DO
 
C
C     Since there are no further characters to examine, the value of
C     LAST that we already have is the largest value we can get.
C
      NCHAR  =  LAST - FIRST + 1
 
 
      RETURN
      END
