 
C$Procedure      CUTSTR ( Cut a long string into substrings )
 
      SUBROUTINE CUTSTR ( STRING, START, WIDTH, BREAKS, BEG, END )
      IMPLICIT NONE
 
C$ Abstract
C
C     Cut a long string into substrings, breaking at "good" points
C     whenever possible.
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
C     STRING
C     UTILITY
C
C$ Declarations
 
      CHARACTER*(*)         STRING
      INTEGER               START
      INTEGER               WIDTH
      CHARACTER*(*)         BREAKS
      INTEGER               BEG
      INTEGER               END
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STRING     I   Long string.
C     START      I   Nominal beginning of substring.
C     WIDTH      I   Maximum width of substrings.
C     BREAKS     I   Characters indicating good break points.
C     BEG        I   Beginning of substring.
C     END        O   End of substring.
C
C$ Detailed_Input
C
C     STRING      is an arbitrary character string. Typically, this
C                 is too wide to fit into an area of limited width:
C                 an element of a character array, for instance, or
C                 an area on a terminal screen.
C
C     START       is the nominal beginning of the next substring.
C                 (STRING(START:START) is the first character that
C                 can appear in the substring.) It is used to skip
C                 past substrings returned by previous calls.
C
C     WIDTH       is the width (in characters) of the limited area.
C                 Thus, it is the maximum width of the substrings
C                 to be returned.
C
C     BREAKS      is a collection of characters indicating preferred
C                 places to break the string into substrings: commas,
C                 colons, and periods, for instance. BREAKS is always
C                 treated as though it contains a space, whether it
C                 does or not. (That is, '+-=' is treated as ' +-='.)
C
C$ Detailed_Output
C
C     BEG,
C     END         are the endpoints of a substring no wider than
C                 WIDTH. Substrings always begin and end with non-blank
C                 characters. BEG is zero if no non-blank substring
C                 was found.
C
C$ Exceptions.
C
C     1) If STRING(START:) is blank or BEG is greater than the declared
C        length of STRING, both BEG and END are zero.
C
C     2) If START is less than one, the error 'SPICE(BEFOREBEGSTR)'
C        is signalled.
C
C     4) If WIDTH is less than one, the error 'SPICE(WIDTHTOOSMALL)'
C        is signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C      This routine is useful primarily for displaying messages on
C      terminal screens or within log files. Since messages can run
C      to hundreds of characters, while most output devices cannot
C      handle more than 80 or 132 characters at a time, it is necessary
C      to break the strings. The friendliness of a message is enhanced
C      if these breaks occur at "natural" places within the message,
C      rather than at rigid intervals.
C
C      The most natural breaks occur before spaces. Slightly less
C      natural breaks occur at the characters
C
C         Comma             ,
C         Period            .
C         Semicolon         ;
C         Colon             :
C         Question          ?
C         Exclamation       !
C         End parenthesis   )
C         End bracket       ]
C         End brace         }
C         End angle         >
C
C      or before the characters
C
C         Begin parenthesis   (
C         Begin bracket       [
C         Begin brace         {
C         Begin angle         <
C
C      At any rate, breaks should occur between adjacent letters or
C      numeric characters only as a last resort.
C
C      In the absence of other instructions, CUTSTR tries to break:
C
C         1) before      space
C
C         2) at           , . ; : - ) ] } >
C            or before              ( [ { <
C
C         3) at           ' "                   (even occurrence)
C            or before    ' "                   (odd  occurrence)
C
C         4) at           ? ! = _ %
C            or before    \ ~ $ @ ^ * / | & +
C
C      before forcing a break at an aribitrary location.
C
C      You may override these rules by supplying a set of preferred
C      characters in BREAKS. Before applying the rules shown above,
C      CUTSTR will try to break AT these characters. (However, breaks
C      always occur BEFORE spaces.)
C
C$ Examples
C
C      CUTSTR might typically be used to display a long diagnostic
C      messages on a terminal screen. For example, suppose that the
C      following message has been returned by a subroutine.
C
C         'I believe you have made an significant error by requesting
C          that I send to the printer a file containing 250 megabytes
C          of text information. The system manager is likely to be
C          very unhappy with such a request. I suggest you reconsider
C          your intended action.'
C
C      and that this needs to be displayed on a 40-character monitor.
C      The following code fragment
C
C         WIDTH  = 40
C         BREAKS = ' ,.'
C
C         CALL CUTSTR ( MSSG, 1, WIDTH, BREAKS, BEG, END )
C
C         DO WHILE ( BEG .NE. 0 )
C            WRITE (6,*) MSSG (BEG:END)
C
C            START = END + 1
C            CALL CUTSTR ( MSSG, START, WIDTH, BREAKS, BEG, END )
C         END DO
C
C      would display something like
C
C         I believe you have made an significant
C         error by requesting that I send to the
C         printer a file containing 250 megabytes
C         of text information. The system manager
C         is likely to be very unhappy with such a
C         request.  I suggest you reconsider your
C         intended action.
C
C      On a more whimsical note, you could indent each successive lines
C      by three characters: the code fragment
C
C         WIDTH  = 40
C         BREAKS = ' ,.'
C         INDENT = 1
C
C         CALL CUTSTR ( MSSG, 1, WIDTH, BREAKS, BEG, END )
C
C         DO WHILE ( BEG .NE. 0 )
C            TEMP           = ' '
C            TEMP(INDENT: ) = MSSG(BEG:END)
C            WRITE (6,*) TEMP
C
C            INDENT = INDENT + 3
C            WIDTH  = MAX ( WIDTH-3, 9 )
C
C            START = END + 1
C            CALL CUTSTR ( MSSG, START, WIDTH, BREAKS, BEG, END )
C         END DO
C
C      would display something like
C
C         I believe you have made an significant
C            error by requesting that I send to
C               the printer a file containing 250
C                  megabytes of text information.
C                     The system manager is likely
C                        to be very unhappy with
C                           such a request. I
C                              suggest you
C                                 reconsider your
C                                    intended
C                                       action.
C
C     Note that both loops terminate when BEG is zero. This indicates
C     that no substring was found (and that none will be returned by
C     subsequent calls). If the string is full, the loop will terminate
C     normally when START becomes greater than the length of the string.
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
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
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
C     Version B1.0.0, 29-APR-1988 (WLT) (IMU)
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               EVEN
      INTEGER               OCCURS
      LOGICAL               RETURN
 
C
C     Local variables
C
 
      INTEGER               A
      INTEGER               B
      INTEGER               BLANK
      INTEGER               P
 
      INTEGER               LENGTH
      INTEGER               LONG
 
      CHARACTER*15          PUNCT
      CHARACTER*15          QUOTE
      CHARACTER*15          OTHER
      CHARACTER*15          DEF
      CHARACTER*15          QTYPE
      CHARACTER*15          PTYPE
      CHARACTER*15          OTYPE
      CHARACTER*15          DTYPE
 
      INTEGER               PASS
 
      CHARACTER*1           THIS
      CHARACTER*1           NEXT
      INTEGER               HERE
      INTEGER               THERE
C
C     Parameters used to simulate an enumerated type for
C     the various passes required to break the string at
C     good places.  Note that the order is important.
C     This forces the routine to try spaces first, user
C     supplied preferences next, etc.    It is also
C     critical that these be defined to be a sequence
C     of consecutive integers.
C
      INTEGER               SPACE
      PARAMETER           ( SPACE  = 1 )
 
      INTEGER               PREF
      PARAMETER           ( PREF   = SPACE  + 1 )
 
      INTEGER               PUNC
      PARAMETER           ( PUNC   = PREF   + 1 )
 
      INTEGER               QUOT
      PARAMETER           ( QUOT   = PUNC   + 1 )
 
      INTEGER               OTHR
      PARAMETER           ( OTHR   = QUOT   + 1 )
 
      INTEGER               FAIL
      PARAMETER           ( FAIL   = OTHR   + 1 )
 
      INTEGER               FINSH
      PARAMETER           ( FINSH  = FAIL   + 1 )
C
C     The ASCII character value for the backslash is needed for
C     uniformity of porting this routine (Some platforms treat the
C     backslah as a special character and so we can't just use
C     the character in strings.)
C
      INTEGER               BSLASH
      PARAMETER           ( BSLASH = 92 )
 
C
C     Standard error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
 
C
C     Exceptions first. Is START outside the bounds of the string?
C
      LENGTH = LEN ( STRING )
 
      IF ( START .GT. LENGTH ) THEN
         BEG = 0
         END = 0
         RETURN
 
      ELSE IF ( START .LT. 1 ) THEN
         CALL CHKIN  ( 'CUTSTR' )
         CALL SIGERR ( 'SPICE(BEFOREBEGSTR)' )
         CALL CHKOUT ( 'CUTSTR' )
         RETURN
      END IF
 
C
C     Is the width reasonable?
C
      IF ( WIDTH .LT. 1 ) THEN
         CALL CHKIN  ( 'CUTSTR' )
         CALL SIGERR ( 'SPICE(WIDTHTOOSMALL)' )
         CALL CHKOUT ( 'CUTSTR' )
         RETURN
      END IF
 
C
C     Does the remainder of the string contain anything besides blanks?
C
      IF ( STRING(START: ) .EQ. ' ' ) THEN
         BEG   = 0
         END   = 0
         RETURN
      END IF
 
C
C     Obviously, we should try to get the longest possible substring.
C
C
      BEG   = START
      BLANK = ICHAR( ' ' )
 
      DO WHILE ( ICHAR(STRING(BEG:BEG)) .EQ. BLANK )
         BEG = BEG + 1
      END DO
 
      LONG = BEG + WIDTH - 1
 
C
C     The remainder of the substring may fit without a trim.
C     But drop trailing blanks anyway.
C
      IF ( LENGTH .LE. LONG ) THEN
 
         END = LENGTH
 
         DO WHILE ( ICHAR(STRING(END:END)) .EQ. BLANK )
            END = END - 1
         END DO
 
         RETURN
      END IF
 
 
C
C     Assign the default break characters. Each character in PUNCT,
C     QUOTE, or OTHER indicates a good place to break. The associated
C     type indicates whether the break should occur at or before the
C     the character:
C
C        Type         Break occurs
C        ----         ------------------------------------------------
C         A           At the character.
C         B           Before the character.
C         P           At an EVEN occurrence, or
C                     Before an ODD occurrence.
C
C
      PUNCT = ',.;:-)]}>([{<'
      PTYPE = 'AAAAAAAAABBBB'
 
      QUOTE = '"'''
      QTYPE = 'PP'
 
      OTHER = '?!~$@^=_%*/|&+' // CHAR(BSLASH)
      OTYPE = 'AABBBBAAABBBBB' // 'B'
 
C
C     We will do this in five passes. During the first pass, we will
C     try to break before a space. During the second pass, we will try
C     to break at one of the preferred characters. During the third,
C     fourth, and fifth passes, we will try to break at or before one
C     of the quotation, punctuation, or other default characters.
C
      PASS      = SPACE
      A         = ICHAR( 'A' )
      P         = ICHAR( 'P' )
      B         = ICHAR( 'B' )
 
 
      DO WHILE ( PASS .NE. FAIL )
 
         END = LONG
 
         DO WHILE ( END .GE. BEG )
 
            THIS = STRING(END   : END  )
            NEXT = STRING(END+1 : END+1)
 
C
C           Always break BEFORE a space.
C
            IF ( PASS .EQ. SPACE ) THEN
 
              IF ( ICHAR(NEXT) .EQ. BLANK ) THEN
                PASS = FINSH
              END IF
 
C
C           Always break AT a preferred character.
C
            ELSE IF ( PASS .EQ. PREF ) THEN
 
              IF ( INDEX ( BREAKS, THIS ) .GT. 0 ) THEN
                PASS = FINSH
              END IF
 
C
C           But with default characters, some break at, some
C           before, and some depend on the parity of strangers.
C
            ELSE
 
               HERE  = INDEX ( DEF, THIS )
               THERE = INDEX ( DEF, NEXT )
 
               IF ( HERE .GT. 0 ) THEN
 
                 IF ( ICHAR(DTYPE(HERE:HERE)) .EQ. A ) THEN
                   PASS = FINSH
 
                 ELSE IF ( ICHAR(DTYPE(HERE:HERE)) .EQ. P ) THEN
                   IF ( EVEN ( OCCURS ( STRING( :END), THIS ) ) ) THEN
                     PASS = FINSH
                   END IF
                 END IF
               END IF
 
               IF (      THERE .GT. 0
     .             .AND. PASS  .NE. FINSH ) THEN
 
                 IF ( ICHAR(DTYPE(THERE:THERE)) .EQ. B ) THEN
                   PASS = FINSH
 
                 ELSE IF ( ICHAR(DTYPE(THERE:THERE)) .EQ. P ) THEN
                   IF ( EVEN ( OCCURS ( STRING( :END), NEXT ) ) ) THEN
                     PASS = FINSH
                   END IF
                 END IF
 
               END IF
 
            END IF
 
C
C           If we've found a break point, remove any trailing blanks
C           before returning.
C
            IF ( PASS .EQ. FINSH ) THEN
               DO WHILE ( ICHAR(STRING(END:END)) .EQ. BLANK )
                  END = END - 1
               END DO
 
               RETURN
            ELSE
               END = END - 1
            END IF
 
         END DO
 
C
C        We may have to try another pass.
C
         PASS  = PASS + 1
 
C
C        In the final passes, only the character set changes.
C
         IF ( PASS .EQ. PUNC ) THEN
            DEF   = PUNCT
            DTYPE = PTYPE
 
         ELSE IF ( PASS .EQ. QUOT ) THEN
            DEF   = QUOTE
            DTYPE = QTYPE
 
         ELSE IF ( PASS .EQ. OTHR ) THEN
            DEF   = OTHER
            DTYPE = OTYPE
         END IF
 
      END DO
 
C
C     Looks like we'll have to do this the hard way.
C
      END = LONG
 
      RETURN
 
      END
