C$Procedure      SCANIT ( Scan a character string )
 
      SUBROUTINE SCANIT ( STRING, START,  ROOM,
     .                    NMARKS, MARKS,  MRKLEN, PNTERS,
     .                    NTOKNS, IDENT,  BEG,    END     )
 
      IMPLICIT NONE
 
C$ Abstract
C
C     This routine serves as an umbrella routine for routines
C     that are used to scan a string for recognized and unrecognized
C     substrings.
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
C     SEARCH
C     PARSE
C
C$ Declarations
 
      CHARACTER*(*)         STRING
      INTEGER               ROOM
      INTEGER               NMARKS
      CHARACTER*(*)         MARKS   ( * )
      INTEGER               MRKLEN  ( * )
      INTEGER               PNTERS  ( * )
      INTEGER               START
      INTEGER               NTOKNS
      INTEGER               BEG     ( * )
      INTEGER               END     ( * )
      INTEGER               IDENT   ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     STRING     I   a string to be scanned.
C     ROOM       I   space available for located substrings.
C     NMARKS    I/O  number of recognizable substrings.
C     MARKS     I/O  recognizable substrings.
C     MRKLEN    I/O  an auxiliary array describing MARKS.
C     PNTERS    I/O  an auxiliary array describing MARKS.
C     START     I/O  position from which to commence/resume scanning.
C     NTOKNS     O   number of scanned substrings.
C     BEG        O   beginnings of scanned substrings.
C     END        O   endings of scanned substrings.
C     IDENT      O   position of scanned substring within array MARKS.
C
C$ Detailed_Input
C
C     STRING     is any character string that is to be scanned
C                to locate recognized and unrecognized substrings.
C
C     ROOM       is the amount of space available for storing the
C                results of scanning the string.
C
C     NMARKS     is the number of marks that will be
C                recognized substrings of STRING.
C
C     MARKS      is an array of marks that will be recognized
C                by the scanning routine.  The array must be
C                processed by a call to SCANPR before it can
C                be used by SCAN.  Further details are given
C                in documentation for the individual entry points.
C
C     MRKLEN     is an auxiliary array populated by SCANPR
C                for use by SCAN.  It should be declared with
C                length equal to the length of MARKS.
C
C     PNTERS     is an auxiliary array populated by SCANPR for
C                use by SCAN.  It should be declared in the
C                calling program as
C
C                   INTEGER  PNTERS ( RCHARS )
C
C                RCHARS is given by the expression
C
C                  MAX - MIN + 5
C
C                where
C
C                MAX is the maximum value of ICHAR(MARKS(I)(1:1))
C                    over the range I = 1, NMARKS
C
C                MIN is the minimum value of ICHAR(MARKS(I)(1:1))
C                    over the range I = 1, NMARKS
C
C               Further details are provided in the entry point
C               SCANPR.
C
C     START     is the position in the STRING from which scanning
C               should commence.
C
C$ Detailed_Output
C
C     NMARKS    is the number of marks in the array MARKS after it
C               has been prepared for SCANPR.
C
C     MARKS     is an array of recognizable substrings that has
C               been prepared for SCAN by SCANPR.  Note that MARKS
C               will be sorted in increasing order.
C
C     MRKLEN    is an auxiliary array, populated by SCANPR for
C               use by SCAN.
C
C     PNTERS    is an auxiliary array, populated by a call to
C               SCANPR and is intended for use by SCAN.
C
C     START     is the position from which scanning should continue
C               in order to fully scan STRING (if sufficient memory was
C               not provided in BEG, END, and IDENT on the current
C               call to SCAN).
C
C     NTOKNS    is the number of substrings identified in the current
C               scan of STRING.
C
C     BEG       Beginnings of scanned substrings.
C               This should be declared so that it is at least
C               as large as ROOM.
C
C     END       Endings of scanned substrings.
C               This should be declared so that it is at least
C               as large as ROOM.
C
C     IDENT     Positions of scanned substring within array MARKS.
C               If the substring STRING(BEG(I):END(I)) is not in the
C               list of MARKS then IDENT(I) will have the value 0.
C               This should be declared so that it is at least
C               as large as ROOM.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If this routine is called directly the error
C        'SPICE(BOGUSENTRY)' will be signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine serves as an umbrella routine for the two entry
C     points SCANPR and SCAN.  It can be used to locate keywords
C     or delimited substrings within a string.
C
C     The process of breaking a string into those substrings that
C     have recognizable meaning, is called "scanning."  The substrings
C     identified by the scanning process are called "tokens."
C
C     Scanning has many applications including:
C
C     -- the parsing of algebraic expressions
C
C     -- parsing calendar dates
C
C     -- processing text with embedded directions for displaying
C        the text.
C
C     -- interpretation of command languages
C
C     -- compilation of programming languages
C
C     This routine simplifies the process of scanning a string for
C     its tokens.
C
C$ Examples
C
C     Example 1.
C     ----------
C
C     Suppose you need to identify all of the words within a string
C     and wish to ignore punctuation marks such as ',', ':', ';', ' ',
C     '---'.
C
C     The first step is to load the array of marks as shown here:
C
C        The minimum ASCII code for the first character of a marker is
C        32 ( for ' ').
C
C        INTEGER               FCHAR
C        PARAMETER           ( FCHAR = 32 )
C
C        The maximum ASCII code for the first character of a marker is
C        59 (for ';' )
C
C        INTEGER               LCHAR
C        PARAMETER           ( LCHAR = 59 )
C
C        INTEGER               RCHAR
C        PARAMETER           ( RCHAR = LCHAR - FCHAR + 5 )
C
C        LOGICAL               FIRST
C        CHARACTER*(3)         MARKS
C        INTEGER               NMARKS ( 5     )
C        INTEGER               MRKLEN ( 5     )
C        INTEGER               PNTERS ( RCHAR )
C
C        INTEGER               ROOM
C        PARAMETER           ( ROOM = 50 )
C
C        INTEGER               BEG    ( ROOM  )
C        INTEGER               END    ( ROOM  )
C        INTEGER               IDENT  ( ROOM  )
C
C        SAVE                  FIRST
C        SAVE                  MARKS
C        SAVE                  MRKLEN
C        SAVE                  PNTERS
C
C        IF ( FIRST ) THEN
C
C           FIRST    = .FALSE.
C
C           MARKS(1) = ' '
C           MARKS(2) = '---'
C           MARKS(3) = ':'
C           MARKS(4) = ','
C           MARKS(5) = ';'
C
C           NMARKS   = 5
C
C           CALL SCANPR ( NMARKS, MARKS, MRKLEN, PNTERS )
C
C        END IF
C
C     Notice that the call to SCANPR is nested inside an
C     IF ( FIRST ) THEN ... END IF block.  In this and many applications
C     the marks that will be used in the scan are fixed.  Since the
C     marks are not changing, you need to process MARKS and set up
C     the auxiliary arrays MRKLEN and PNTERS only once (assuming that
C     you SAVE the appropriate variables as has been done above).
C     In this way if the code is executed many times, there is only
C     a small overhead required for preparing the data so that it
C     can be used efficiently in scanning.
C
C     To identify the substrings that represent words we scan the
C     string using the prepared MARKS, MRKLEN and PNTERS.
C
C        CALL SCAN ( STRING, MARKS,  MRKLEN, PNTERS, ROOM,
C       .            START,  NTOKNS, IDENT,  BEG,    END   )
C
C     To isolate only the words of the string, we examine the
C     array IDENT and keep only those Begin and Ends for which
C     the corresponding identity is non-positive.
C
C        KEPT = 0
C
C        DO I = 1, NTOKNS
C
C           IF ( IDENT(I) .LE. 0 ) THEN
C
C              KEPT      = KEPT + 1
C              BEG(KEPT) = BEG(I)
C              END(KEPT) = END(I)
C
C           END IF
C
C        END DO
C
C
C     Example 2.
C     ----------
C
C     To parse an algebraic expression such as
C
C        ( X + Y ) * ( 2*Z + SIN(W) ) ** 2
C
C     You would select '**', '*', '+', '-', '(', ')' and ' '
C     to be the markers.  Note that all of these begin with one
C     of the characters in the string ' !"#$%&''()*+,-./'
C     so that we can declare PNTERS to have length 20.
C
C     Prepare the MARKS, MRKLEN, and PNTERS.
C
C        LOGICAL               FIRST
C        CHARACTER*(4)         MARKS
C        INTEGER               NMARKS ( 8  )
C        INTEGER               MRKLEN ( 8  )
C        INTEGER               PNTERS ( 20 )
C
C        SAVE                  FIRST
C        SAVE                  MARKS
C        SAVE                  MRKLEN
C        SAVE                  PNTERS
C
C        IF ( FIRST ) THEN
C
C           MARKS(1) = '('
C           MARKS(2) = ')'
C           MARKS(3) = '+'
C           MARKS(4) = '-'
C           MARKS(5) = '*'
C           MARKS(6) = '/'
C           MARKS(7) = '**'
C           MARKS(8) = ' '
C
C           NMARKS   = 8
C
C           CALL SCANPR ( NMARKS, MARKS, MRKLEN, PNTERS )
C
C           Locate the blank character in MARKS once it has
C           been prepared.
C
C           BLANK = BSRCHC ( ' ', NMARKS, MARKS )
C
C        END IF
C
C
C     Once all of the initializations are out of the way,
C     we can scan an input string.
C
C        CALL SCAN ( STRING, MARKS,  MRKLEN, PNTERS, ROOM,
C       .            START,  NTOKNS, IDENT,  BEG,    END   )
C
C
C     Next eliminate any white space that was returned in the
C     list of tokens.
C
C     KEPT = 0
C
C     DO I = 1, NTOKNS
C
C        IF ( IDENT(I) .NE. BLANK ) THEN
C           KEPT        = KEPT + 1
C           BEG  (KEPT) = BEG   (I)
C           END  (KEPT) = END   (I)
C           IDENT(KEPT) = IDENT (I)
C        END IF
C
C     END DO
C
C     Now all of the substrings remaining point to grouping symbols,
C     operators, functions, or variables.  Given that the individual
C     "words" of the expression are now in hand, the meaning of the
C     expression is much easier to determine.
C
C     The rest of the routine is left as a non-trivial exercise
C     for the reader.
C
C$ Restrictions
C
C     The array of MARKS, MRKLEN, and PNTERS must be properly formatted
C     prior to calling SCAN.  This is accomplished by calling SCANPR.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    Spicelib Version  1.0.0, 26-JUL-1996 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Scan a string for recognized and unrecognized tokens
C     Parse a string
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      INTEGER               RTRIM
      INTEGER               NCPOS
 
 
C
C     Local variables
C
      CHARACTER*(1)         LETTER
 
      INTEGER               BACKUP
      INTEGER               EBLOCK
      INTEGER               FCHAR
      INTEGER               FINISH
      INTEGER               I
      INTEGER               INTVAL
      INTEGER               J
      INTEGER               JUMP
      INTEGER               L
      INTEGER               LAST
      INTEGER               LAST1
      INTEGER               LBOUND
      INTEGER               LCHAR
      INTEGER               N
      INTEGER               OFFSET
      INTEGER               SLOT
      INTEGER               STOP
      INTEGER               TEST
      INTEGER               THIS1
      INTEGER               UBOUND
 
      LOGICAL               EQUAL
      LOGICAL               KNOWN
 
 
 
 
      IF ( .NOT. RETURN () ) THEN
 
         CALL CHKIN  ( 'SCANIT'                                       )
         CALL SETMSG ( 'Your program has referenced the umbrella '    //
     .                 'subroutine SCANIT.  This may indicate a '     //
     .                 'programming error.'                           )
         CALL SIGERR ( 'SPICE(BOGUSENTRY)'                            )
         CALL CHKOUT ( 'SCANIT'                                       )
 
      END IF
 
      RETURN
 
 
 
C$Procedure SCANPR ( Scanning preparation )
 
      ENTRY SCANPR ( NMARKS, MARKS, MRKLEN, PNTERS )
 
C$ Abstract
C
C     Prepare recognized markers and auxiliary arrays for the
C     routine SCAN.
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
C     PARSING
C     UTILITY
C
C$ Declarations
C
C     INTEGER               NMARKS
C     CHARACTER*(*)         MARKS   ( * )
C     INTEGER               MRKLEN  ( * )
C     INTEGER               PNTERS  ( * )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NMARKS    I/O  Number of recognizable substrings.
C     MARKS     I/O  Recognizable substrings.
C     MRKLEN     O   auxiliary array describing MARKS.
C     PNTERS     O   auxiliary array describing MARKS.
C
C$ Detailed_Input
C
C     NMARKS     is the number of recognized marks that will be
C                recognized substrings of STRING.
C
C     MARKS      is an array of marks that will be recognized
C                by the scanning routine.  Leading and trailing
C                blanks are not significant.  (Except for the
C                blank character ' ', itself.  After all, some
C                part of it must be significant.)  Case of the
C                entries in MARKS is significant. The MARKS
C                'XX' and 'xx' are regarded as different MARKS.
C
C$ Detailed_Output
C
C     NMARKS     is the number of marks in the array MARKS after it
C                has been prepared for SCAN.
C
C     MARKS      is an array of recognizable substrings.
C                It has been prepared for use by SCAN
C                so as to be compatible with the other arrays.
C                It will be sorted in ascending order, left
C                justified and contain no duplicate entries.
C
C     MRKLEN     is an auxiliary array populated by SCANPR
C                for use by SCAN that describes MARKS.
C
C     PNTERS     is an auxiliary array populated by SCANPR for
C                use by SCAN.  It should be declared in the
C                calling program as
C
C                   INTEGER   PNTERS ( RCHARS )
C
C                RCHARS is given by the expression
C
C                  MAX - MIN + 5
C
C                where
C
C                MAX is the maximum value of ICHAR(MARKS(I)(1:1))
C                    over the range I = 1, NMARKS
C
C                MIN is the minimum value of ICHAR(MARKS(I)(1:1))
C                    over the range I = 1, NMARKS
C
C                Here are some typical values that may help you avoid
C                going through the computations above.  (This assumes
C                that ICHAR returns the ASCII code for a character.)
C
C                Scanning Situation           RCHAR
C                ------------------          -------------------
C                If NMARKS = 1
C                or all MARKS                   5
C                begin with the same
C                character.
C
C                All MARKS begin with
C                one of the characters          20
C                in the string
C                ' !"#$%&''()*+,-./'
C
C                All MARKS begin with
C                one of the characters          11
C                in the string
C                ':;<=>?@'
C
C                All MARKS begin with
C                one of the characters          37
C                in the string
C                ' !"#$%&''()*+,-./:;<=>?@'
C
C                All MARKS begin with
C                an upper case english letter   30
C
C                All MARKS begin with a
C                decimal digit                  14
C
C                All Marks begin with a
C                lower case english letter      30
C
C                All Marks begin with
C                a digit or upper case          47
C                character.
C
C                All Marks begin with a
C                printing character or          100
C                a blank.
C
C                Anything might be a mark       132
C
C                Finally, so you won't have to look it up elsewhere
C                here are the ASCII codes for the printing
C                characters and blanks.
C
C                (Common Punctuations) Character     ASCII Code
C                                      -----------   ----------
C                                      ' ' (space)     32
C                                      '!'             33
C                                      '"'             34
C                                      '#'             35
C                                      '$'             36
C                                      '%'             37
C                                      '&'             38
C                                      ''''            39
C                                      '('             40
C                                      ')'             41
C                                      '*'             42
C                                      '+'             43
C                                      ','             44
C                                      '-'             45
C                                      '.'             46
C                                      '/'             47
C
C
C                (Decimal Digits)      Character     ASCII Code
C                                      -----------   ----------
C                                      '0'             48
C                                      '1'             49
C                                      '2'             50
C                                      '3'             51
C                                      '4'             52
C                                      '5'             53
C                                      '6'             54
C                                      '7'             55
C                                      '8'             56
C                                      '9'             57
C
C                (More punctuation)    Character     ASCII Code
C                                      -----------   ----------
C                                      ':'             58
C                                      ';'             59
C                                      '<'             60
C                                      '='             61
C                                      '>'             62
C                                      '?'             63
C                                      '@'             64
C
C              (Uppercase characters)  Character     ASCII Code
C                                      -----------   ----------
C                                      'A'             65
C                                      'B'             66
C                                      'C'             67
C                                      'D'             68
C                                      'E'             69
C                                      'F'             70
C                                      'G'             71
C                                      'H'             72
C                                      'I'             73
C                                      'J'             74
C                                      'K'             75
C                                      'L'             76
C                                      'M'             77
C                                      'N'             78
C                                      'O'             79
C                                      'P'             80
C                                      'Q'             81
C                                      'R'             82
C                                      'S'             83
C                                      'T'             84
C                                      'U'             85
C                                      'V'             86
C                                      'W'             87
C                                      'X'             88
C                                      'Y'             89
C                                      'Z'             90
C
C                (More punctuation)    Character     ASCII Code
C                                      -----------   ----------
C                                      '['             91
C                                      '\'             92
C                                      ']'             93
C                                      '^'             94
C                                      '_'             95
C                                      '`'             96
C
C              (Lowercase characters)  Character     ASCII Code
C                                      -----------   ----------
C                                      'a'             97
C                                      'b'             98
C                                      'c'             99
C                                      'd'            100
C                                      'e'            101
C                                      'f'            102
C                                      'g'            103
C                                      'h'            104
C                                      'i'            105
C                                      'j'            106
C                                      'k'            107
C                                      'l'            108
C                                      'm'            109
C                                      'n'            110
C                                      'o'            111
C                                      'p'            112
C                                      'q'            113
C                                      'r'            114
C                                      's'            115
C                                      't'            116
C                                      'u'            117
C                                      'v'            118
C                                      'w'            119
C                                      'x'            120
C                                      'y'            121
C                                      'z'            122
C
C              (More punctuation)      Character     ASCII Code
C                                      -----------   ----------
C                                      '{'            123
C                                      '|'            124
C                                      '}'            125
C                                      '~'            126
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error Free.
C
C     1) A space is regarded as a special mark.  If MARKS(I) = ' ',
C        then MARKS(I) will match any consecutive sequence of blanks.
C
C     2) If NMARKS is less than or equal to zero, SCAN will always
C        find a single token, namely the entire string to be scanned.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine prepares the arrays MARKS, MRKLEN and PNTERS
C     so that they are suitable for input to the routine SCAN.
C
C     It is expected that users will need to scan many strings
C     and that from the programming point of view it is
C     easiest to simply supply a list of MARKS to a "formatting"
C     routine such as this so that the strings can then
C     be efficiently scanned by the routine SCAN.  This formatting
C     is the function of this routine.
C
C$ Examples
C
C     Suppose you need to identify all of the words within a string
C     and wish to ignore punctuation marks such as ' ', ',', ':', ';'
C     '---'.  Then the first step is to load the array of marks as
C     shown here:
C
C        The minimum ASCII code for the first character of a marker is
C        32 (for ' ').
C
C        INTEGER               FCHAR
C        PARAMETER           ( FCHAR = 32 )
C
C        The maximum ASCII code for the first character of a marker is
C        59 (for ';').
C
C        INTEGER               LCHAR
C        PARAMETER           ( LCHAR = 59 )
C
C
C        The proper size to declare PNTERS is given by the parameter
C        RCHAR defined in terms of LCHAR and FCHAR.
C
C        INTEGER               RCHAR
C        PARAMETER           ( RCHAR = LCHAR - FCHAR + 5 )
C
C        LOGICAL               FIRST
C        CHARACTER*(4)         MARKS
C        INTEGER               NMARKS ( 5     )
C        INTEGER               MRKLEN ( 5     )
C        INTEGER               PNTERS ( RCHAR )
C
C        SAVE                  FIRST
C        SAVE                  MARKS
C        SAVE                  MRKLEN
C        SAVE                  PNTERS
C
C        IF ( FIRST ) THEN
C
C           FIRST    = .FALSE.
C
C           MARKS(1) = ' '
C           MARKS(2) = '---'
C           MARKS(3) = ':'
C           MARKS(4) = ','
C           MARKS(5) = ';'
C
C           NMARKS   = 5
C
C           CALL SCANPR ( NMARKS, MARKS, MRKLEN, PNTERS )
C
C        END IF
C
C     Notice that the call to SCANPR is nested inside an
C     IF ( FIRST ) THEN ... END IF block.  In this and many applications
C     the marks that will used in the scan are fixed.  Since the marks
C     are not changing, you need to process MARKS and set up
C     the auxiliary arrays MRKLEN and PNTERS only once (assuming that
C     you SAVE the appropriate variables as has been done above).
C     In this way if the code is executed many times, there is only
C     a small overhead required for preparing the data so that it
C     can be used efficiently in scanning.
C
C
C$ Restrictions
C
C     MRKLEN and PNTERS must be declared to be at least as large
C     as indicated above.  If not, this routine will write
C     past the ends of these arrays.  Much unpleasantness may
C     ensue in the attempt to debug such problems.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    Spicelib Version  1.0.0, 26-JUL-1996 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Prepare for scanning strings
C     Prepare for parsing strings
C
C-&
C
C     We handle the case where NMARKS is non-positive separately.
C
      IF ( NMARKS .LE. 0 ) THEN
 
         PNTERS(1) = 0
         PNTERS(2) = 0
         PNTERS(3) = 0
         PNTERS(4) = 0
         PNTERS(5) = 0
 
         RETURN
      END IF
C
C     First left justify MARKS and remove duplicates.
C
      DO I = 1, NMARKS
         CALL LJUST ( MARKS(I), MARKS(I) )
      END DO
 
      N = NMARKS
 
C
C     Sort and remove duplicates from the array MARKS.
C
      CALL RMDUPC ( N, MARKS )
 
C
C     All of the MARKS have the same declared length.
C     However, since all of your marks may not have
C     the same intended length (for example '*' and
C     '**') it is desirable to be able to specify
C     how much of MARKS(I) should actually be used
C     when examining STRING for a substring match.
C     This is done with the array MRKLEN.
C     MARKS(I)(1:MRKLEN(I)) will be used when
C     scanning STRING.
C
C     Here is the expected structure of PNTERS.
C
C             PNTERS(1) = MIN ( ICHAR(MARKS(I)(1:1)  ), I=1,NMARKS )
C             PNTERS(2) = MAX ( ICHAR(MARKS(I)(1:1)  ), I=1,NMARKS )
C
C     For ease of further discussion let
C     MYCHAR(I) represent the characters from PNTERS(1)
C     to PNTERS(2), and assume that legitimate values of
C     I are from 1 to M.
C
C             PNTERS(3)   = 0
C             PNTERS(4)   = index of the last entry of MARKS
C                           that begins with the character
C                           MYCHAR(1).
C
C             PNTERS(5)   = index of the last entry of MARKS
C                           that begins with the character
C                           MYCHAR(2), if there is no such element
C                           of MARKS let PNTERS(5) = PNTERS(4)
C                .
C                .
C                .
C
C             PNTERS(3+K) = index of the last entry of MARKS
C                           that begins with the character
C                           MYCHAR(K), if there is no such element
C                           of MARKS, let PNTERS(3+K) =
C                           PNTERS(3+K-1)
C                .
C                .
C                .
C
C             PNTERS(3+M) = index of the last entry of MARKS
C                           that begins with the character
C                           MYCHAR(M).
C
C             PNTERS(4+M) = PNTERS(3+M)
C
C
 
C
C     Next determine the minimum and maximum ASCII values
C     of the first characters of the MARKS.
C
      FCHAR     = ICHAR ( MARKS(1)(1:1) )
      LCHAR     = ICHAR ( MARKS(N)(1:1) )
 
      PNTERS(1) = FCHAR
      PNTERS(2) = LCHAR
 
C
C     For the purposes of getting started, we will say the last
C     character that started a MARK was one before FCHAR.  We
C     will record the end of its block in slot 3 of PNTERS.
C
      LAST1     = FCHAR - 1
      SLOT      = 3
 
      DO  I = 1, N
 
         MRKLEN(I) = RTRIM( MARKS(I)      )
         THIS1     = ICHAR( MARKS(I)(1:1) )
 
         IF ( THIS1 .NE. LAST1 ) THEN
 
C
C           We need to record the address of the end of the last
C           block of MARKS that began with the same character.
C           This is of course one before the current value of I.
C
C           While we are at it, we might as well determine how
C           many possible first letters were "jumped" over in
C           going from the last first character to the current
C           first character.
C
            EBLOCK = I     - 1
            JUMP   = THIS1 - LAST1
 
C
C           The end of the block for all of the MARKS having
C           first character between the last one and this one
C           is the same.
C
            DO J = SLOT, SLOT + JUMP - 1
               PNTERS(J) = EBLOCK
            END DO
 
            SLOT  = SLOT  + JUMP
            LAST1 = THIS1
 
         END IF
 
      END DO
 
      PNTERS(SLOT  ) = N
      PNTERS(SLOT+1) = N
      NMARKS         = N
 
      RETURN
 
 
 
 
C$Procedure SCAN ( Scan a string for tokens )
 
      ENTRY SCAN   ( STRING,
     .               MARKS,  MRKLEN, PNTERS, ROOM,   START,
     .               NTOKNS, IDENT,  BEG,    END            )
 
C$ Abstract
C
C     This routine scans a string returning the beginning and
C     ends of recognized and unrecognized substrings.  The full
C     collection of these substrings partitions the string.
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
C     PARSING
C
C$ Declarations
C
C     CHARACTER*(*)         STRING
C     CHARACTER*(*)         MARKS   ( * )
C     INTEGER               MRKLEN  ( * )
C     INTEGER               PNTERS  ( * )
C     INTEGER               ROOM
C     INTEGER               START
C     INTEGER               NTOKNS
C     INTEGER               BEG     ( * )
C     INTEGER               END     ( * )
C     INTEGER               IDENT   ( * )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     STRING     I   string to be scanned.
C     MARKS      I   recognizable substrings.
C     MRKLEN     I   an auxiliary array describing MARKS.
C     PNTERS     I   an auxiliary array describing MARKS.
C     ROOM       I   space available for storing substring descriptions.
C     START     I/O  position from which to begin/resume scanning.
C     NTOKNS     O   number of scanned substrings.
C     BEG        O   beginnings of scanned substrings.
C     END        O   endings of scanned substrings.
C     IDENT      O   position of scanned substring within array MARKS.
C
C$ Detailed_Input
C
C     STRING     is any character string that is to be scanned
C                to locate recognized and unrecognized substrings.
C
C     MARKS      is an array of marks that will be recognized
C                by the scanning routine.  This array must be prepared
C                by calling the routine SCANPR.
C
C                Note that the blank string is interpreted
C                in a special way by SCAN.  If the blank character,
C                ' ', is one of the MARKS, it will match any unbroken
C                sequence of blanks in string.  Thus if ' ' is the only
C                marks supplied and STRING is
C
C                   'A   lot of      space '
C                    ......................
C
C                Then scan will locate the following substrings
C
C                'A'          STRING(1:1)    (unrecognized)
C                '   '        STRING(2:4)    (recognized --- all blanks)
C                'lot'        STRING(5:7)    (unrecognized)
C                ' '          STRING(8:8)    (recognized --- a blank)
C                'of'         STRING(9:10)   (unrecognized)
C                '      '     STRING(11:16)  (recognized --- all blanks)
C                'space'      STRING(17:21)  (unrecognized)
C                ' '          STRING(22:22)  (recognized --- a blank)
C
C     MRKLEN     is an auxiliary array populated by SCANPR
C                for use by SCAN.  It should be declared with
C                length equal to the length of MARKS.  It must
C                be prepared for use by the routine SCANPR.
C
C     PNTERS     is a specially structured array of integers that
C                describes the array MARKS.  It is must be filled
C                in by the routine SCANPR.  It should be declared
C                by the calling program as shown here:
C
C                   INTEGER  PNTERS ( RCHARS )
C
C                RCHARS is given by the expression
C
C                  MAX - MIN + 5
C
C                where
C
C                MAX is the maximum value of ICHAR(MARKS(I)(1:1))
C                    over the range I = 1, NMARKS
C
C                MIN is the minimum value of ICHAR(MARKS(I)(1:1))
C                    over the range I = 1, NMARKS
C
C                See SCANPR for a more detailed description of the
C                declaration of PNTERS.
C
C     ROOM       is the amount of space available for storing the
C                results of scanning the string.
C
C     START     is the position from which scanning should commence.
C               Values of START less than 1 are treated as 1.
C
C$ Detailed_Output
C
C     START     is the position from which scanning should continue
C               in order to fully scan STRING (if sufficient memory was
C               not provided in BEG, END, and IDENT on the current
C               call to SCAN).
C
C     NTOKNS    is the number of substrings identified in the current
C               scan of STRING.
C
C     BEG       Beginnings of scanned substrings.  This should be
C               declared so that it is at least as large as ROOM.
C
C     END       Endings of scanned substrings. This should be declared
C               so that it is at least as large as ROOM.
C
C     IDENT     Positions of scanned substring within array MARKS.
C               If the substring STRING(BEG(I):END(I)) is in the array
C               MARKS, then MARKS(IDENT(I)) will equal
C               STRING(BEG(I):END(I)).
C
C               If the substring STRING(BEG(I):END(I)) is not in the
C               list of MARKS then IDENT(I) will have the value 0.
C
C               IDENT should be declared so that it can contain at least
C               ROOM integers.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error Free.
C
C     1) A space is regarded as a special mark.  If MARKS(I) = ' ',
C        then MARKS(I) will match any consecutive sequence of blanks.
C
C     2) If START is less than 1 on input, it will be treated as
C        if it were 1.
C
C     3) If START is greater than the length of the string, no
C        tokens will be found and the value of START will return
C        unchanged.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine allows you to scan a string and partition it into
C     recognized and unrecognized substrings.
C
C     For some applications the recognized substrings serve only as
C     delimiters between the portions of the string
C     that are of interest to your application.  For other
C     applications the recognized substrings are equally important as
C     they may indicate operations that are to be performed on the
C     unrecognized portions of the string.  However, the techniques
C     required to scan the string are the same in both instances.  The
C     examples below illustrate some common situations.
C
C$ Examples
C
C     Example 1.
C     ----------
C
C     Suppose you wished to write a routine that would return the words
C     of a string.  The following routine shows how SCANPR and SCAN can
C     be used to accomplish this task.
C
C        SUBROUTINE GETWDS ( STRING, WDROOM, NWORDS, WORDS )
C
C        CHARACTER*(*)      STRING
C        INTEGER            WDROOM
C        INTEGER            NWORDS
C        CHARACTER*(*)      WORDS  ( * )
C
C
C        CHARACTER*(1)      MARKS  ( 1 )
C        INTEGER            MRKLEN ( 1 )
C        INTEGER            PNTERS ( 5 )
C
C        INTEGER            ROOM
C        PARAMETER        ( ROOM = 50 )
C
C        INTEGER            BEG   ( ROOM )
C        INTEGER            END   ( ROOM )
C        INTEGER            I
C        INTEGER            IDENT ( ROOM )
C        INTEGER            NMARKS
C        INTEGER            NTOKNS
C        INTEGER            START
C
C        LOGICAL            FIRST
C        SAVE               FIRST
C        DATA               FIRST  / .TRUE. /
C
C
C        On the first time through the routine, set up the MARKS
C        MRKLEN, and PNTERS arrays.
C
C        IF( FIRST ) THEN
C
C           FIRST    = .FALSE.
C           MARKS(1) = ' '
C           NMARKS   = 1
C
C           CALL SCANPR ( NMARKS, MARKS, MRKLEN, PNTERS )
C
C        END IF
C
C        Now simply scan the input string for words until we have
C        them all or until we run out of room.
C
C        START  = 1
C        NWORDS = 0
C
C        CALL SCAN ( STRING,
C                    MARKS,  MRKLEN, PNTERS, ROOM, START,
C                    NTOKNS, IDENT,  BEG,    END          )
C
C        If we found something in our scan, copy the substrings into the
C        words array.
C
C        DO WHILE (       ( NWORDS .LT. WDROOM )
C       .           .AND. ( NTOKNS .GT. 0      ) )
C
C
C           Step through the scanned substrings, looking for those
C           that are not blank ...
C
C           I = 1
C
C           DO WHILE (       ( NWORDS .LT. WDROOM )
C          .           .AND. ( I      .LE. NTOKNS ) )
C
C              Copy the non-blank substrings (those unidentified by
C              SCAN) into WORDS.
C
C              IF ( IDENT(I) .EQ. 0 ) THEN
C                 NWORDS        = NWORDS + 1
C                 WORDS(NWORDS) = STRING(BEG(I):END(I))
C              END IF
C
C              I      = I      + 1
C
C           END DO
C
C
C           Scan the STRING again for any substrings that might
C           remain.  Note that START is already pointing at the
C           point in the string from which to resume scanning.
C
C           CALL SCAN ( STRING,
C                       MARKS,  MRKLEN, PNTERS, ROOM, START,
C                       NTOKNS, IDENT,  BEG,    END          )
C        END DO
C
C        That's all, we've got all the substrings there were (or
C        that we had room for).
C
C        RETURN
C
C
C     Example 2.
C     ----------
C
C     To parse an algebraic expression such as
C
C        ( X + Y ) * ( 2*Z + SIN(W) ) ** 2
C
C     You would select '**', '*', '+', '-', '(', ')' and ' '
C     to be the markers.  Note that all of these begin with one
C     of the characters in the string ' !"#$%&''()*+,-./'
C     so that we can declare PNTERS to have length 20.
C
C     Prepare the MARKS, MRKLEN, and PNTERS.
C
C        CHARACTER*(4)         MARKS
C        INTEGER               NMARKS ( 8  )
C        INTEGER               MRKLEN ( 8  )
C        INTEGER               PNTERS ( 20 )
C
C        INTEGER               ROOM
C        PARAMETER           ( ROOM = 20 )
C
C        INTEGER               NTOKNS
C        INTEGER               BEG    ( ROOM )
C        INTEGER               END    ( ROOM )
C        INTEGER               IDENT  ( ROOM )
C
C        LOGICAL               FIRST
C        SAVE                  FIRST
C        SAVE                  MARKS
C        SAVE                  MRKLEN
C        SAVE                  PNTERS
C
C        DATA                  FIRST  / .TRUE. /
C
C        IF ( FIRST ) THEN
C
C           MARKS(1) = '('
C           MARKS(2) = ')'
C           MARKS(3) = '+'
C           MARKS(4) = '-'
C           MARKS(5) = '*'
C           MARKS(6) = '/'
C           MARKS(7) = '**'
C           MARKS(8) = ' '
C
C           NMARKS   = 8
C
C           CALL SCANPR ( NMARKS, MARKS, MRKLEN, PNTERS )
C
C           BLANK = BSRCHC ( ' ', NMARKS, MARKS )
C
C        END IF
C
C
C        Once all of the initializations are out of the way,
C        we can scan an input string.
C
C        CALL SCAN ( STRING, MARKS,  MRKLEN, PNTERS, ROOM,
C       .            START,  NTOKNS, IDENT,  BEG,    END  )
C
C
C        Next eliminate any white space that was returned in the
C        list of tokens.
C
C        KEPT = 0
C
C        DO I = 1, NTOKNS
C
C           IF ( IDENT(I) .NE. BLANK ) THEN
C
C              KEPT        = KEPT + 1
C              BEG  (KEPT) = BEG(I)
C              END  (KEPT) = END(I)
C              IDENT(KEPT) = IDENT(I)
C
C           END IF
C
C        END DO
C
C        Now all of the substrings remaining point to grouping symbols,
C        operators, functions, or variables.  Given that the individual
C        "words" of the expression are now in hand, the meaning of the
C        expression is much easier to determine.
C
C        The rest of the routine is left as a non-trivial exercise
C        for the reader.
C
C$ Restrictions
C
C     The arrays MARKS, MRKLEN, and PNTERS must be prepared by the
C     routine SCANPR prior to supplying them for use by SCAN.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 26-JUL-1996 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Scan a string for recognized and unrecognized tokens
C     Parse a string
C
C-&
 
 
C
C     All of the MARKS have the same declared length.
C     However, since all of your marks may not have
C     the same intended length (for example '*' and
C     '**') it is desirable to be able to specify
C     how much of MARKS(I) should actually be used
C     when examining STRING for a substring match.
C     This is done with the array MRKLEN.
C     MARKS(I)(1:MRKLEN(I)) will be used when
C     scanning STRING.
C
C     Here is the expected structure of PNTERS.
C
C             PNTERS(1) = MIN ( ICHAR(MARKS(I)(1:1)  )
C             PNTERS(2) = MAX ( ICHAR(MARKS(I)(1:1)  )
C
C     where I ranges from 1 to the number of MARKS stored
C     in MARKS.  For ease of further discussion let
C     MYCHAR(I) represent the characters from PNTERS(1)
C     to PNTERS(2), and assume that legitimate values of
C     I are from 1 to N.
C
C             PNTERS(3)   = 0
C             PNTERS(4)   = index of the last entry of MARKS
C                           that begins with the character
C                           MYCHAR(1).
C
C             PNTERS(5)   = index of the last entry of MARKS
C                           that begins with the character
C                           MYCHAR(2), if there is no such element
C                           of MARKS let PNTERS(5) = PNTERS(4)
C                .
C                .
C                .
C
C             PNTERS(3+K) = index of the last entry of MARKS
C                           that begins with the character
C                           MYCHAR(K), if there is no such element
C                           of MARKS, let PNTERS(3+K) =
C                           PNTERS(3+K-1)
C                .
C                .
C                .
C
C             PNTERS(3+N) = index of the last entry of MARKS
C                           that begins with the character
C                           MYCHAR(N).
C
C             PNTERS(4+N) = PNTERS(3+N)
C
C
C     Get the information concerning the range of the
C     marks from the PNTERS array.
C
      OFFSET = PNTERS(1) - 4
      LBOUND = PNTERS(1) - 1
      UBOUND = PNTERS(2) + 1
 
      LAST   =  LEN ( STRING )
      NTOKNS =  0
      BACKUP =  START - 1
      KNOWN  = .TRUE.
      START  =  MAX ( 1, START )
 
      DO WHILE ( START .LE. LAST )
 
C
C        Get the numeric code for this letter, and look up
C        the range of markers that begin with this letter.
C
         LETTER = STRING(START:START)
 
         INTVAL = MAX( LBOUND, MIN(ICHAR(LETTER), UBOUND) )
 
         TEST   = PNTERS( INTVAL - OFFSET     )
         FINISH = PNTERS( INTVAL - OFFSET - 1 )
 
         EQUAL  = .FALSE.
C
C        If TEST is greater than FINISH, then there is a range of
C        markers that start with this letter.
C
         DO WHILE ( TEST .GT. FINISH )
 
C
C           Look up the length of the next marker to test for
C           and compute where it would end in STRING if there
C           is a match.
C
            L    = MRKLEN ( TEST )
            STOP = BACKUP + L
 
C
C           Make sure that we are not going to violate any substring
C           references when we compare the current candidate mark with
C           the substring having the same length and starting at START.
C
            IF ( STOP .GT. LAST ) THEN
 
               TEST = TEST - 1
 
            ELSE
C
C              OK. The substring reference STRING(START:STOP) is
C              legal.  See if it is equal to the current test mark.
C
               EQUAL = MARKS(TEST)(1:L) .EQ. STRING(START:STOP)
 
C
C              If it isn't equal, just set up to test the next mark.
C
               IF ( .NOT. EQUAL ) THEN
 
                  TEST = TEST - 1
 
               ELSE
 
C
C                 If we were in the middle of an unrecognized string
C                 then, we need to check whether or not we have room
C                 to identify another token. If we don't we must return
C                 now.
C
                  IF (       (        .NOT. KNOWN )
     .                 .AND. ( NTOKNS .EQ.  ROOM  ) ) THEN
 
                     RETURN
                  END IF
 
C
C                 A space is a special kind of mark.  All white space
C                 is regarded as being the same.  If the current mark
C                 is a space, we need to collect all of the consecutive
C                 blanks beginning with the one at the START position.
C
                  IF ( MARKS(TEST) .EQ. ' ' ) THEN
 
                     STOP = NCPOS(STRING, ' ', START) - 1
 
                     IF ( STOP .LT. 0 ) THEN
                        STOP = LAST
                     END IF
 
                  END IF
 
 
C
C                 Ok. We have a new known token.
C
C                 1)  Record its begin, end, and identity.
C
C                 2)  Set TEST to FINISH so that the loop will end.
C
C                 3)  Set START to the current STOP so that later when
C                     we add 1, START will point to the beginning
C                     of the remainder of the string that needs to be
C                     scanned.
C
                  KNOWN            = .TRUE.
                  NTOKNS           = NTOKNS + 1
                  BEG   ( NTOKNS ) = START
                  END   ( NTOKNS ) = STOP
                  IDENT ( NTOKNS ) = TEST
                  TEST             = FINISH
                  START            = STOP
 
C
C                 If we have just used up all available room,
C                 position START so that we will be ready
C                 to continue scanning on a subsequent call
C                 and return.
C
                  IF ( NTOKNS .EQ. ROOM ) THEN
                     START = START + 1
                     RETURN
                  END IF
 
               END IF
 
            END IF
 
         END DO
 
C
C        If none of the markers matched a substring starting at
C        the current position, we are beginning or continuing
C        an unrecognized substring.
C
         IF ( .NOT. EQUAL ) THEN
 
C
C           If we are already in the middle of an unrecognized
C           substring, just extend our current unrecognized string.
C
            IF ( .NOT. KNOWN ) THEN
 
               END(NTOKNS)   = START
 
C
C           Otherwise, start up a new unrecognized substring.
C
            ELSE
 
               NTOKNS        =  NTOKNS + 1
               BEG  (NTOKNS) =  START
               END  (NTOKNS) =  START
               IDENT(NTOKNS) =  0
               KNOWN         = .FALSE.
 
            END IF
 
         END IF
 
         BACKUP = START
         START  = START + 1
 
      END DO
 
      RETURN
      END
