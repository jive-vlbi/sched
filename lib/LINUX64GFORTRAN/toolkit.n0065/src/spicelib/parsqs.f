C$Procedure  PARSQS ( Parse quoted string token )
 
      SUBROUTINE PARSQS ( STRING, QCHAR, VALUE, LENGTH, ERROR, ERRMSG,
     .                    PTR                                          )
 
C$ Abstract
C
C     Parse a quoted string token.
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
      CHARACTER*(*)         VALUE
      INTEGER               LENGTH
      LOGICAL               ERROR
      CHARACTER*(*)         ERRMSG
      INTEGER               PTR
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     STRING     I   Quoted string to be parsed.
C     QCHAR      I   Quote delimiter character.
C     VALUE      O   Parsed string.
C     LENGTH     O   Number of significant characters in VALUE.
C     ERROR      O   Logical error flag.
C     ERRMSG     O   Message indicating whether errors have occurred.
C     PTR        O   Position in string where an error occurred.
C
C$ Detailed_Input
C
C     STRING         is a character string containing a `quoted string
C                    token'.  Quoted string tokens are sequences of
C                    characters that represent literal strings.
C                    Syntactically, a string token is a sequence of
C                    characters that begins and ends with a designated
C                    `quote character'.  Within the token, any
C                    occurrence of the quote character is indicated by
C                    an adjacent pair of quote characters:  for example,
C                    if the quote character is
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
C                    Leading and trailing blanks in STRING are ignored.
C                    The input string may not contain any trailing,
C                    non-blank characters after the final quote
C                    character.
C
C                    All blanks occurring between the bracketing
C                    quote characters in STRING are significant.
C
C
C     QCHAR          is the quote character.  This is always a single
C                    character.  The characters
C
C                       "  and '
C
C                    are common choices, but any non-blank character is
C                    accepted.  Case *is* significant in QCHAR.
C
C$ Detailed_Output
C
C     VALUE          is the string resulting from parsing STRING.
C                    VALUE is obtained from STRING by removing the
C                    bracketing quote characters and replacing each pair
C                    of quote characters in the interior of STRING with
C                    a singleton quote character.  The value resulting
C                    from parsing STRING will occupy the leftmost
C                    characters of VALUE, but will not be
C                    `left-justified', since leading blanks within
C                    the quoted string token in STRING are significant.
C
C     LENGTH         is the number of significant characters in VALUE.
C                    This is the number of characters in the string
C                    resulting from parsing the input string.  Because
C                    parsed strings containing embedded quote
C                    characters are shorter than the unparsed tokens
C                    that represent them, LENGTH may be less than the
C                    number of characters between the bracketing quote
C                    characters of the input string.
C
C     ERROR          is a logical flag indicating whether a parse error
C                    occurred; if so, ERROR is returned with the value
C                    .TRUE.
C
C     ERRMSG         is a message indicating that STRING could not be
C                    parsed due to an error in its structure.  If the
C                    input string token was successfully parsed, ERRMSG
C                    will be returned as a blank string.
C
C     PTR            indicates the character position at which an
C                    error in STRING was detected.  If STRING is
C                    correctly formed, PTR is returned as 0.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1) If the input argument QCHAR is blank, a parse error will be
C        indicated by ERROR; PTR will be set to 1.  The contents of
C        VALUE and LENGTH are undefined in this case.
C
C     2) If STRING is not a well-formed quoted string, a parse error
C        will be indicated by ERROR and PTR.  The contents of VALUE
C         and LENGTH are undefined in this case.
C
C     3) If the length of the output string VALUE is too short to
C        accommodate the parsed string token produced by this routine,
C        a parse error message to this effect is generated.  VALUE
C        will contain the as much as possible of the result, truncated
C        on the right.
C
C     4) If STRING consists of a null string token, that is, two
C        adjacent quote characters with nothing but blanks on either
C        side, a parse error will be indicated.  The contents of VALUE
C         and LENGTH are undefined in this case.
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
C     The string tokens that are expected inputs to this routine are
C     Fortran-style quoted strings:  they start and end with quote
C     characters.  In the interior of any such token, any quote
C     characters are represented by doubled quote characters.  These
C     rules imply that the number of quote characters in a valid quoted
C     string token is always even.  The end of a quoted string token is
C     located at the first even-numbered quote character, counting from
C     the initial quote character, that is  not the first member of a
C     pair of quotes indicating an embedded quote character.
C
C     This routine is meant to be used together with the SPICELIB
C     routine LXQSTR (Lex quoted string):  LXQSTR is used to identify
C     quoted string tokens, and this routine converts the tokens to
C     string values.
C
C$ Examples
C
C     1)  The table below illustrates the action of this routine.
C
C
C     STRING               QCHAR   VALUE           LENGTH       ERROR
C     =================================================================
C     "SPICE"              "       SPICE           5            .FALSE.
C     "SPICE"              '       <undefined>     <undefined>  .TRUE.
C     """SPICE"" system"   "       "SPICE" system  14           .FALSE.
C     " "                  "       <single blank>  1            .FALSE.
C     ''                   '       <undefined>     <undefined>  .TRUE.
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
C-    SPICELIB Version 1.1.0, 08-MAY-1996 (WLT)
C
C        Corrected the problem with an unintitialized variable
C        INLEN that was detected on the HP and reported by Steve
C        Schlaifer of MASL.
C
C-    SPICELIB Version 1.0.0, 21-NOV-1994 (NJB)
C
C-&
 
C$ Index_Entries
C
C     parse quoted string token
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               FRSTNB
      INTEGER               LASTNB
 
C
C     Local variables
C
      CHARACTER*(1)         CHR
 
      INTEGER               FIRST
      INTEGER               IPOS
      INTEGER               INLEN
      INTEGER               LAST
      INTEGER               OUTLEN
      INTEGER               OPOS
 
 
C
C     Error free, no check-in required.  No parse error to start with.
C     No characters in the parsed string to start with.
C
      ERROR  = .FALSE.
      ERRMSG = ' '
      PTR    = 0
      LENGTH = 0
 
C
C     Reject invalid quote characters.
C
      IF ( QCHAR .EQ. ' ' ) THEN
 
         ERROR  = .TRUE.
         ERRMSG = 'The quote character must be non-blank, but isn''t'
         PTR    = 1
 
      END IF
      
C
C     Grab the lengths of the string arguments.
C
      INLEN  =  LEN(STRING)
      OUTLEN =  LEN(VALUE )
 
C
C     The token to be parsed extends from the first non-blank
C     character to the last non-blank character of STRING.
C
      FIRST  =  FRSTNB(STRING)
      LAST   =  LASTNB(STRING)
 
      IF ( FIRST .EQ. 0 ) THEN
 
         ERROR  = .TRUE.
         ERRMSG = 'Blank input string'
         PTR    =  INLEN
         RETURN
 
      END IF
 
 
C
C     The input token must be bracketed by quote characters.
C
      IF ( STRING(FIRST:FIRST) .NE. QCHAR ) THEN
 
         ERROR  = .TRUE.
         ERRMSG = 'String token does not start with quote character'
         PTR    =  FIRST
         RETURN
 
      ELSE IF ( STRING(LAST:LAST) .NE. QCHAR ) THEN
 
         ERROR  = .TRUE.
         ERRMSG = 'String token does not end with quote character'
         PTR    =  LAST
         RETURN
 
      END IF
 
 
C
C     Null strings are not accepted.
C
      IF ( FIRST .EQ. LAST-1 ) THEN
 
         ERROR  = .TRUE.
         ERRMSG = 'Null (zero length) string token'
         PTR    = LAST
         RETURN
 
      END IF
 
 
C
C     Transfer the interior characters of the input string to the output
C     string, replacing each doubled quote character with a single quote
C     character.  The interior of the string must not contain any
C     un-doubled quotes; we have a parse error if we find any such
C     stragglers.
C
      OPOS  =  1
      IPOS  =  FIRST + 1
 
      DO WHILE (  ( IPOS .LE. LAST-1 ) .AND. ( OPOS .LE. OUTLEN )  )
C
C        At this point, IPOS points to the current input character to
C        examine; OPOS points to the currently available position to
C        write to in the output string.
C
         CHR  =  STRING(IPOS:IPOS)
 
         IF ( CHR .NE. QCHAR ) THEN
C
C           This is the normal, non-quote case.  Transfer the
C           character to the output string and advance both the input
C           and output character positions.
C
            VALUE(OPOS:OPOS)  =  CHR
            IPOS              =  IPOS   + 1
            OPOS              =  OPOS   + 1
            LENGTH            =  LENGTH + 1
 
         ELSE
C
C           We've encountered a quote character.  By construction, the
C           parity of this quote character must be odd.  The quote must
C           be followed immediately by a second, interior quote.
C
            IF ( IPOS .EQ. LAST-1 ) THEN
C
C              We're already looking at the last interior input
C              character.
C
               ERROR  = .TRUE.
               ERRMSG = 'Quote character is unmatched or else string '//
     .                  'ends without final quote; take your pick'
               PTR    = IPOS
               RETURN
 
            ELSE IF ( STRING(IPOS+1:IPOS+1) .NE. QCHAR ) THEN
 
               ERROR  =  .TRUE.
               ERRMSG =  'Interior quote character is not doubled'
               PTR    =  IPOS
               RETURN
 
            ELSE
C
C              This is the normal case; the quote character is doubled.
C              Transfer a single quote character to the output string,
C              and skip over the second quote in the input string.
C
               VALUE(OPOS:OPOS)  =  CHR
               OPOS              =  OPOS   + 1
               LENGTH            =  LENGTH + 1
               IPOS              =  IPOS   + 2
 
            END IF
 
         END IF
 
      END DO
 
 
      IF ( IPOS .LT. LAST-1 ) THEN
C
C        We must have stopped transferring characters to VALUE
C        because we ran out of room.
C
         ERROR  =  .TRUE.
         ERRMSG =  'Output string too short, truncated on right'
         PTR    =  IPOS
         RETURN
 
      END IF
 
 
      IF ( OPOS .LT. OUTLEN ) THEN
C
C        Blank-pad the trailing portion of the output string.
C
         VALUE(OPOS:) = ' '
 
      END IF
 
 
      RETURN
      END
