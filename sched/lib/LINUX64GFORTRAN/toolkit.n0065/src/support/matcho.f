 
C$Procedure MATCHO ( Match the characters in two words )
 
      INTEGER FUNCTION MATCHO ( WORD, GUESS )
      IMPLICIT NONE
 
C$ Abstract
C
C      Assign a score to a pair of words which reflects the closeness
C      of the words in terms of the characters they contain and the
C      order in which the characters appear.
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
C                 against an initial guess. The (non-printing) ASCII
C                 characters 1 and 2 are ignored. Typically, WORD will
C                 contain a single word.
C
C      GUESS      is an initial guess at the value of the input
C                 word. The (non-printing) ASCII characters 1 and 2
C                 are ignored. Like WORD, this will typically be a
C                 single word.
C
C$ Detailed_Output
C
C      The function returns a score between 0 (indicating that WORD
C      and GUESS have no common character patterns) and 100 (indicating
C      that WORD and GUESS match very closely).
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
C      The score assigned by MATCHO indicates not only how many of the
C      letters two words have in common, but also the relative
C      difference between the order in which these letters appear.
C
C      MATCHO does not assign higher weights to more exotic characters,
C      like Q and Z, since these are as likely to appear in mistyped
C      words as are any other characters. (Both Q and Z, for instance,
C      are adjacent to A on a standard keyboard.)
C
C      The score assigned by MATCHO is computed in this way.
C
C          Suppose WORD is the string
C
C             w_1 w_2  ... w_n
C
C          and GUESS is the string
C
C             g_1 g_2 ... g_m
C
C          Each of the MATCHW templates
C
C          * w_i * w_j *  (where i < j)
C
C          is matched against GUESS and the total number of
C          matches tallied.  (There are n(n-1)/2 such templates)
C
C          Additionally 1 extra point is awarded for each match of GUESS
C          with a template of the form
C
C          * w_i w_i+1 * .
C
C          The total tally is multiplied by 200/n(n-1) and truncated to
C          100 if necessary to yield a GUESS to WORD tally.
C
C          Then the roles of WORD and GUESS are reversed and an
C          identical proceedure is followed to obtain a WORD to GUESS
C          tally.  The average of the two tallies is returned in
C          MATCHO.
C
C      Empirically it has been found that WORD and GUESS are in
C      close agreement if MATCHO is returned with a value of 75
C      or more.  Users may wish to use higher or lower score when
C      determining when a match between two words is close.
C
C$ Examples
C
C
C$ Restrictions
C
C      1) MATCHO is case-insensitive. Lowercase characters match
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
C      Version B 1.0.0, 7-APR-1988 (WLT) (IMU)
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               QRTRIM
      INTEGER               LTRIM
      LOGICAL               EQSTR
 
C
C     Local Parameters
C
      INTEGER               BASE
      PARAMETER           ( BASE = 1024 )
 
      INTEGER               MXWLEN
      PARAMETER           ( MXWLEN = 32 )
 
      INTEGER               MXPAIR
      PARAMETER           ( MXPAIR = MXWLEN*MXWLEN/2 )
 
      INTEGER               LSTCHR
      PARAMETER           ( LSTCHR = 255 )
 
 
 
C
C     Local variables
C
      INTEGER               C1
      INTEGER               C2
 
      INTEGER               G2C
      INTEGER               G2SEQ   ( MXWLEN )
      INTEGER               GCOUNT
      INTEGER               GF
      INTEGER               GL
      INTEGER               GLEN
      INTEGER               GMSCOR
      INTEGER               GP
      INTEGER               GPAIRS  ( MXPAIR )
      INTEGER               GSCORE
      INTEGER               GTALLY
 
      INTEGER               I
      INTEGER               J
 
      INTEGER               UVALUE ( 0 : LSTCHR )
      INTEGER               VALUE
 
      INTEGER               W2C
      INTEGER               W2SEQ   ( MXWLEN )
      INTEGER               WCOUNT
      INTEGER               WF
      INTEGER               WL
      INTEGER               WLEN
      INTEGER               WMSCOR
      INTEGER               WP
      INTEGER               WPAIRS  ( MXPAIR )
      INTEGER               WSCORE
      INTEGER               WTALLY
 
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
 
C
C     Set up the case insensitive mapping.
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
C     First get the ``dimensions'' of our two words (first non-blank,
C     last non-blank, and non-blank length).
C
      GF   = LTRIM ( GUESS )
      GL   = QRTRIM ( GUESS )
 
      WF   = LTRIM ( WORD  )
      WL   = QRTRIM ( WORD  )
 
      GLEN = GL - GF + 1
      WLEN = WL - WF + 1
 
C
C     Perform some of the obvious checks first.
C
      IF (  EQSTR( WORD(WF:WL), GUESS(GF:GL) ) ) THEN
 
         MATCHO = 100
         RETURN
 
      ELSE IF (      ( WLEN .LE. 1 )
     .          .OR. ( GLEN .LE. 1 ) ) THEN
 
         MATCHO = 0
         RETURN
 
      END IF
 
C
C     Initialize the score keeper and compute the length of GUESS.
C
      WMSCOR = ( (WLEN - 1) * WLEN ) / 2
      GMSCOR = ( (GLEN - 1) * GLEN ) / 2
 
C
C     We will encode ordered letter pairs as
C
C        BASE * ICHAR(first)   +   ICHAR(second)
C
C     Where BASE is chosen large enough so that we will never have
C     different pairs mapping to the same integer.
C
C     Compute the encoded collection of ordered pairs for
C     the GUESS (GCOUNT is the number of general pairs
C     G2C is the number of 2 character substrings) ...
C
      GCOUNT = 0
      G2C    = 0
 
      DO I = GF,GL-1
 
         C1 = UVALUE(  ICHAR( GUESS(I  :I  ) ) )
         C2 = UVALUE(  ICHAR( GUESS(I+1:I+1) ) )
 
         G2C        = G2C       + 1
         G2SEQ(G2C) = BASE * C1 + C2
 
         DO J = I+1, GL
 
            C1             = UVALUE(  ICHAR( GUESS(I:I) ) )
            C2             = UVALUE(  ICHAR( GUESS(J:J) ) )
 
            GCOUNT         = GCOUNT  + 1
            GPAIRS(GCOUNT) = BASE*C1 + C2
 
         END DO
      END DO
 
 
C
C     ... then construct the encoded ordered letter pairs for WORD.
C
      WCOUNT = 0
      W2C    = 0
 
      DO I = WF,WL-1
 
         C1 = UVALUE(  ICHAR( WORD(I  :I  ) ) )
         C2 = UVALUE(  ICHAR( WORD(I+1:I+1) ) )
 
         W2C        = W2C       + 1
         W2SEQ(W2C) = BASE * C1 + C2
 
         DO J = I+1, WL
 
            C1             = UVALUE(  ICHAR( WORD(I:I) ) )
            C2             = UVALUE(  ICHAR( WORD(J:J) ) )
 
            WCOUNT         = WCOUNT  + 1
            WPAIRS(WCOUNT) = BASE*C1 + C2
 
         END DO
 
      END DO
 
C
C     Now sort the various arrays of encoded letter pairs
C
      CALL SHELLI ( G2C,    G2SEQ  )
      CALL SHELLI ( GCOUNT, GPAIRS )
      CALL SHELLI ( W2C,    W2SEQ  )
      CALL SHELLI ( WCOUNT, WPAIRS )
 
      G2SEQ (G2C   +1) = 0
      GPAIRS(GCOUNT+1) = 0
      W2SEQ (W2C   +1) = 0
      WPAIRS(WCOUNT+1) = 0
 
C
C     First tally up the matches of the form *L1*L2*.  This is
C     virtually the same algorithm used for computing set
C     intersections.
C
      WP = 1
      GP = 1
 
      WTALLY = 0
      GTALLY = 0
 
      DO WHILE (      ( WP .LE. WCOUNT )
     .          .AND. ( GP .LE. GCOUNT ) )
 
         IF      ( WPAIRS(WP) .LT. GPAIRS(GP) ) THEN
 
            WP = WP + 1
 
         ELSE IF ( WPAIRS(WP) .GT. GPAIRS(GP) ) THEN
 
            GP = GP + 1
 
         ELSE
 
            VALUE = WPAIRS(WP)
 
            DO WHILE (       ( WPAIRS(WP) .EQ. VALUE  )
     .                 .AND. ( WP         .LE. WCOUNT ) )
 
               WTALLY = WTALLY + 1
               WP     = WP     + 1
 
            END DO
 
            DO WHILE (       ( GPAIRS(GP) .EQ. VALUE  )
     .                 .AND. ( GP         .LE. GCOUNT ) )
 
               GTALLY = GTALLY + 1
               GP     = GP     + 1
 
            END DO
 
         END IF
 
      END DO
 
 
C
C     Next tally up the various matches of the form *L1L2*
C
      WP = 1
      GP = 1
 
      DO WHILE (      ( WP .LE. W2C )
     .          .AND. ( GP .LE. G2C ) )
 
         IF      ( W2SEQ(WP) .LT. G2SEQ(GP) ) THEN
 
            WP = WP + 1
 
         ELSE IF ( W2SEQ(WP) .GT. G2SEQ(GP) ) THEN
 
            GP = GP + 1
 
         ELSE
 
            VALUE = W2SEQ(WP)
 
            DO WHILE (       ( W2SEQ(WP) .EQ. VALUE  )
     .                 .AND. ( WP        .LE. W2C    ) )
 
               WTALLY = WTALLY + 1
               WP     = WP     + 1
 
            END DO
 
            DO WHILE (       ( G2SEQ(GP) .EQ. VALUE  )
     .                 .AND. ( GP        .LE. G2C    ) )
 
               GTALLY = GTALLY + 1
               GP     = GP     + 1
 
            END DO
 
         END IF
 
      END DO
 
      GTALLY =  MIN ( GTALLY, GMSCOR )
      WTALLY =  MIN ( WTALLY, WMSCOR )
 
      WSCORE =  ( 100 * WTALLY ) / WMSCOR
      GSCORE =  ( 100 * GTALLY ) / GMSCOR
 
      MATCHO =  MIN(  ( WSCORE + GSCORE ) / 2,  100 )
 
      RETURN
      END
