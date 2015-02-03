 
C$Procedure MATCHC ( Match the characters in two words )
 
      INTEGER FUNCTION MATCHC ( WORD, GUESS )
      IMPLICIT NONE
 
C$ Abstract
C
C      Assign a score to a pair of words which reflects the closeness
C      of the words in terms of the characters they contain. Disregard
C      the case of letters
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
C$ Keywords
C
C      SEARCH
C      UTILITY
C
C$ Declarations
 
      CHARACTER*(*)         WORD
      CHARACTER*(*)         GUESS
 
C$ Brief_I/O
C
C      Variable  I/O  Description
C      --------  ---  --------------------------------------------------
C      WORD       I   Word to be matched against initial guess.
C      GUESS      I   Initial guess.
C
C$ Detailed_Input
C
C      WORD       is a character string to be checked for a match
C                 against an initial guess. Non-printing characters
C                 (including blanks) are ignored. Typically, WORD will
C                 contain a single word. In any case, the significant
C                 part of WORD may not exceed 64 characters.
C
C      GUESS      is an initial guess at the value of the input
C                 word. Non-printing characters (including blanks)
C                 are ignored. Like WORD, this will typically be a
C                 single word. In any case, the significant part of
C                 GUESS may not exceed 64 characters.
C
C$ Detailed_Output
C
C      The function returns a score between 0 (indicating that WORD
C      and GUESS have no characters in common) and 100 (indicating
C      that WORD and GUESS have all their characters in common).
C
C$ Exceptions
C
C      1) If neither WORD nor GUESS contains any printing characters,
C         the function returns 0.
C
C$ Particulars
C
C      In order to determine whether a word (usually typed by a user)
C      matches any of a series of known words (keywords, for example),
C      it is necessary to be able to judge the "closeness" of an
C      arbitrary pair of words. Several algorithms exist which make
C      such a comparison, the best-known of which is probably the
C      Soundex algorithm.
C
C      The score assigned by MATCHC differs from most other algorithms
C      in that multiple occurrences of letters are counted as distinct
C      characters. This allows the lengths of characters to enter into
C      the computation.
C
C      Another difference is that MATCHC does not assign higher
C      weights to more "exotic" characters, like Q and Z, since these
C      are as likely to appear in mistyped words as are any other
C      characters. (Both Q and Z, for instance, are adjacent to A
C      on a standard keyboard.)
C
C      The score assigned by MATCHC is computed in this way.
C
C         1) The characters in each word are sorted, assigned
C            ordinal numbers, and placed in a set. Thus, the word
C            'APPEAL' gives rise to the set
C
C               'A1', 'A2', 'E1', 'L1', 'P1', 'P2'
C
C         2) The union and the symmetric difference of the sets
C            formed from WORD and GUESS are computed.
C
C         3) Letting #(U) and #(S) be the cardinalities of the
C            union and symmetric differences respectively, the
C            score assigned to the pair (WORD, GUESS) is
C
C                           #(S)
C              100 * ( 1 -  ---- )
C                           #(U)
C
C      When WORD and GUESS have no characters in common, the symmetric
C      difference and the union are equivalent, and the score is zero.
C      When they share the same characters (including multiply occurring
C      characters), the symmetric difference is empty, and the score
C      is 100.
C
C$ Examples
C
C
C$ Restrictions
C
C      1) MATCHC is case-sensitive. Lowercase characters do not match
C         uppercase characters, and vice versa.
C
C$ Common_Variables
C
C      None.
C
C$ Author_and_Institution
C
C      W.L. Taber     (JPL)
C      I.M. Underwood (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 9, 1994
C
C
C-     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of META/2
C         software as of May 3, 1994
C
C
C      Version B 1.0.0, 5-APR-1988
C
C-&
 
 
 
C
C     SPICELIB functions
C
 
C
C     The printable character set is bounded below by ASCII character
C     32 (SP) and above by ASCII character 127 (DEL).
C
      INTEGER               LBOUND
      PARAMETER           ( LBOUND =  33 )
 
      INTEGER               UBOUND
      PARAMETER           ( UBOUND = 126 )
 
      INTEGER               NCHARS
      PARAMETER           ( NCHARS = UBOUND - LBOUND + 1 )
 
      INTEGER               LSTCHR
      PARAMETER           ( LSTCHR = 255 )
 
C
C     Only the first 64 characters of WORD and GUESS are significant.
C
      INTEGER               MAXSIG
      PARAMETER           ( MAXSIG = 64 )
 
C
C     Local variables
C
      DOUBLE PRECISION      SCARD
      DOUBLE PRECISION      UCARD
 
      INTEGER               C
      INTEGER               GCOUNT   ( LBOUND : UBOUND )
      INTEGER               HIT      ( NCHARS )
      INTEGER               I
      INTEGER               J
      INTEGER               MN
      INTEGER               MX
      INTEGER               NSIG
      INTEGER               SCARDI
      INTEGER               TOTAL
      INTEGER               UCARDI
      INTEGER               UVALUE ( 0 : LSTCHR )
      INTEGER               WCOUNT   ( LBOUND : UBOUND )
 
      LOGICAL               FIRST
      SAVE
 
      DATA FIRST / .TRUE. /
 
      DATA (UVALUE(I), I = 0, 99 )
     .     /   0,   1,   2,   3,   4,   5,   6,   7,   8,   9,
     .        10,  11,  12,  13,  14,  15,  16,  17,  18,  19,
     .        20,  21,  22,  23,  24,  25,  26,  27,  28,  29,
     .        30,  31,  32,  33,  34,  35,  36,  37,  38,  39,
     .        40,  41,  42,  43,  44,  45,  46,  47,  48,  49,
     .        50,  51,  52,  53,  54,  55,  56,  57,  58,  59,
     .        60,  61,  62,  63,  64,  65,  66,  67,  68,  69,
     .        70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
     .        80,  81,  82,  83,  84,  85,  86,  87,  88,  89,
     .        90,  91,  92,  93,  94,  95,  96,  97,  98,  99 /
 
      DATA (UVALUE(I), I = 100, 199 )
     .     / 100, 101, 102, 103, 104, 105, 106, 107, 108, 109,
     .       110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
     .       120, 121, 122, 123, 124, 125, 126, 127, 128, 129,
     .       130, 131, 132, 133, 134, 135, 136, 137, 138, 139,
     .       140, 141, 142, 143, 144, 145, 146, 147, 148, 149,
     .       150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
     .       160, 161, 162, 163, 164, 165, 166, 167, 168, 169,
     .       170, 171, 172, 173, 174, 175, 176, 177, 178, 179,
     .       180, 181, 182, 183, 184, 185, 186, 187, 188, 189,
     .       190, 191, 192, 193, 194, 195, 196, 197, 198, 199 /
 
      DATA (UVALUE(I), I = 200, 255 )
     .     / 200, 201, 202, 203, 204, 205, 206, 207, 208, 209,
     .       210, 211, 212, 213, 214, 215, 216, 217, 218, 219,
     .       220, 221, 222, 223, 224, 225, 226, 227, 228, 229,
     .       230, 231, 232, 233, 234, 235, 236, 237, 238, 239,
     .       240, 241, 242, 243, 244, 245, 246, 247, 248, 249,
     .       250, 251, 252, 253, 254, 255 /
 
 
 
      DATA                  GCOUNT / NCHARS * 0 /
      DATA                  WCOUNT / NCHARS * 0 /
 
C
C     Initialize the character mapping "function" (array).
C
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
 
         UVALUE( ICHAR('a') ) = ICHAR( 'A' )
         UVALUE( ICHAR('b') ) = ICHAR( 'B' )
         UVALUE( ICHAR('c') ) = ICHAR( 'C' )
         UVALUE( ICHAR('d') ) = ICHAR( 'D' )
         UVALUE( ICHAR('e') ) = ICHAR( 'E' )
         UVALUE( ICHAR('f') ) = ICHAR( 'F' )
         UVALUE( ICHAR('g') ) = ICHAR( 'G' )
         UVALUE( ICHAR('h') ) = ICHAR( 'H' )
         UVALUE( ICHAR('i') ) = ICHAR( 'I' )
         UVALUE( ICHAR('j') ) = ICHAR( 'J' )
         UVALUE( ICHAR('k') ) = ICHAR( 'K' )
         UVALUE( ICHAR('l') ) = ICHAR( 'L' )
         UVALUE( ICHAR('m') ) = ICHAR( 'M' )
         UVALUE( ICHAR('n') ) = ICHAR( 'N' )
         UVALUE( ICHAR('o') ) = ICHAR( 'O' )
         UVALUE( ICHAR('p') ) = ICHAR( 'P' )
         UVALUE( ICHAR('q') ) = ICHAR( 'Q' )
         UVALUE( ICHAR('r') ) = ICHAR( 'R' )
         UVALUE( ICHAR('s') ) = ICHAR( 'S' )
         UVALUE( ICHAR('t') ) = ICHAR( 'T' )
         UVALUE( ICHAR('u') ) = ICHAR( 'U' )
         UVALUE( ICHAR('v') ) = ICHAR( 'V' )
         UVALUE( ICHAR('w') ) = ICHAR( 'W' )
         UVALUE( ICHAR('x') ) = ICHAR( 'X' )
         UVALUE( ICHAR('y') ) = ICHAR( 'Y' )
         UVALUE( ICHAR('z') ) = ICHAR( 'Z' )
 
      END IF
 
C
C     Tally up the characters in WORD.  Also, everytime a new
C     character is encountered, increment the number of characters
C     that have been observed and record which new character has
C     just been observed.
C
      NSIG  = 0
      TOTAL = 0
 
      DO I = 1, LEN ( WORD )
 
         C = UVALUE( ICHAR ( WORD(I:I) ) )
 
         IF ( C .GE. LBOUND  .AND.  C .LE. UBOUND ) THEN
 
            NSIG     = NSIG + 1
 
            IF ( NSIG .LE. MAXSIG ) THEN
 
               IF ( WCOUNT(C) .EQ. 0 ) THEN
                  TOTAL      = TOTAL + 1
                  HIT(TOTAL) = C
               END IF
 
               WCOUNT(C)  = WCOUNT(C) + 1
 
            END IF
 
         END IF
 
      END DO
 
C
C     Tally up the characters in GUESS.  Also, everytime a new
C     character is encountered, increment the number of characters
C     that have been observed and record which new character has
C     just been observed.
C
      NSIG = 0
 
      DO I = 1, LEN ( GUESS )
 
         C = UVALUE( ICHAR ( GUESS(I:I) ) )
 
         IF ( C .GE. LBOUND  .AND.  C .LE. UBOUND ) THEN
 
            NSIG     = NSIG + 1
 
            IF ( NSIG .LE. MAXSIG ) THEN
 
               IF ( WCOUNT(C) .EQ. 0 ) THEN
                  IF ( GCOUNT(C) .EQ. 0 ) THEN
                     TOTAL      = TOTAL + 1
                     HIT(TOTAL) = C
                  END IF
               END IF
 
               GCOUNT(C) = GCOUNT(C) + 1
 
            END IF
 
         END IF
 
      END DO
 
 
 
 
 
C
C     Now look through the list of characters that were hit
C     and compute their contributions to the cardinality
C     of the symmetric difference and unions of the letter sets.
C
      SCARDI = 0
      UCARDI = 0
 
      DO I = 1, TOTAL
 
         J = HIT(I)
 
         IF ( WCOUNT(J) .GT. GCOUNT(J) ) THEN
            MX = WCOUNT(J)
            MN = GCOUNT(J)
         ELSE
            MX = GCOUNT(J)
            MN = WCOUNT(J)
         END IF
 
         SCARDI = SCARDI + MX - MN
         UCARDI = UCARDI + MX
 
C
C        While we're here, set the counts back to zero in preparation
C        for the next time this routine gets called.
C
         WCOUNT(J) = 0
         GCOUNT(J) = 0
 
      END DO
 
      SCARD = DBLE(SCARDI)
      UCARD = DBLE(UCARDI)
 
C
C
C
C     And assign the score.
C
      IF ( UCARD .EQ. 0.D0 ) THEN
 
         MATCHC = 0
 
      ELSE IF ( SCARD .LE. 2.0D0 ) THEN
 
         MATCHC = INT ( 100.D0 * ( 1.D0 - (SCARD/UCARD)**2 ) )
 
      ELSE
 
         MATCHC = INT ( 100.D0 * ( 1.D0 - (SCARD/UCARD)    ) )
 
      END IF
 
      RETURN
      END
