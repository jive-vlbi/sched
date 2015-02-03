C$Procedure      TOKENS ( Find tokens in SUBTeX input )
 
      SUBROUTINE TOKENS ( ACTION, SOURCE, N, TOKEN, LENGTH )
 
C$ Abstract
C
C     Find the next token in one or more lines of SUBTeX input.
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
C     SUBTeX
C
C$ Keywords
C
C     SUBTeX
C
C$ Declarations
 
      CHARACTER*(*)         ACTION
      CHARACTER*(*)         SOURCE   ( * )
      INTEGER               N
      CHARACTER*(*)         TOKEN
      INTEGER               LENGTH
 
C$ Detailed_Input
C
C     ACTION      determines whether the search begins at the first
C                 character of the first line ('NEW') or just after
C                 the end of the latest token found ('NEXT').
C
C     SOURCE      are one or more of SUBTeX source lines.
C
C     N           is the number of source lines.
C
C$ Detailed_Output
C
C     TOKEN       is a SUBTeX token. Currently, tokens are defined
C                 as words (in the SPICELIB sense), although a word
C                 may be broken into more than one token if its
C                 length exceeds LEN ( TOKEN ). Words are broken
C                 at delimiters recognized by the SPICELIB routine
C                 CUTSTR.
C
C     LENGTH      is the length of the token.
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ACTION     I   'NEW' or 'NEXT'.
C     SOURCE     I   SUBTeX source lines.
C     N          I   Number of source lines.
C     TOKEN      O   SUBTeX token.
C     LENGTH     O   Length of token.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If ACTION is not recognized, the error 'SUBTeX(BADTOKENACTION)'
C        is signalled.
C
C$ Particulars
C
C     This routine allows several input lines to be processed as
C     a single line, with no breaks.
C
C$ Examples
C
C     Let SOURCE contain the following lines.
C
C        '@chapter In Which Pooh Goes'
C        'Visiting and @tbr Gets Into'
C        'a Tight Place'
C
C     Then the following code fragment
C
C        CHARACTER*132      TOKEN
C          .
C          .
C
C        CALL TOKENS ( 'NEW', SOURCE, 3, TOKEN, L )
C
C        DO WHILE ( TOKEN .NE. ' ' )
C           WRITE (6,*) TOKEN, L
C
C           CALL TOKENS ( 'NEXT', SOURCE, 3, TOKEN, L )
C        END DO
C
C     produces the following output.
C
C        @chapter       8
C        In             2
C        Which          5
C        Pooh           4
C        Goes           4
C        Visiting       8
C        and            3
C        @tbr           4
C        Gets           4
C        Into           4
C        a              1
C        Tight          5
C        Place          5
C
C     If the declaration of TOKEN is changed to
C
C        CHARACTER*6        TOKEN
C
C     produces the following output.
C
C        @chapt         6
C        er             2
C        In             2
C        Which          5
C        Pooh           4
C        Goes           4
C        Visiti         6
C        ng             2
C        and            3
C        @tbr           4
C        Gets           4
C        Into           4
C        a              1
C        Tight          5
C        Place          5
C
C$ Restrictions
C
C        None.
C
C$ Literature_References
C
C$Include SUBTeX.REFS
C
C$ Author_and_Institution
C
C     I.M. Underwood (JPL)
C
C$ Version
C
C     Beta Version 1.0.1, 22-APR-1997 (WLT)
C
C       replaced backslashes '\' with ats '@'  (only affects comments).
C
C     Beta Version 1.0.0, 11-JUN-1988 (IMU)
C
C-&
 
 
 
C
C     SPICELIB functions
C
      INTEGER               CPOS
      INTEGER               NCPOS
      LOGICAL               RETURN
 
C
C     Local variables
C
      CHARACTER*5           WHAT
      INTEGER               LINE
      INTEGER               COL
      INTEGER               BEGIN
      INTEGER               END
      INTEGER               MAXLEN
      INTEGER               WRDLEN
 
C
C     Saved variables
C
      SAVE                  LINE
      SAVE                  COL
 
 
 
C
C     Standard SPICE error handling
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'TOKENS' )
      END IF
 
C
C     Shake or bake? All that changes is the starting point.
C
      CALL UCASE ( ACTION, WHAT )
 
      IF ( WHAT .EQ. 'NEW' ) THEN
         LINE = 1
         COL  = 1
 
      ELSE IF ( WHAT .NE. 'NEXT' ) THEN
         CALL SIGERR ( 'SUBTeX(BADTOKENACTION)' )
 
         CALL CHKOUT ( 'TOKENS' )
         RETURN
 
      ELSE IF ( LINE .GT. N ) THEN
         TOKEN  = ' '
         LENGTH = 0
 
         CALL CHKOUT ( 'TOKENS' )
         RETURN
      END IF
 
C
C     Anything left in the current line?
C
      BEGIN = NCPOS ( SOURCE(LINE), ' ', COL )
 
C
C     If not, try the next one (or two, or three...).
C
      DO WHILE ( BEGIN .EQ. 0 )
         LINE = LINE + 1
         COL  = 1
 
         IF ( LINE .LE. N ) THEN
            BEGIN = NCPOS ( SOURCE(LINE), ' ', COL )
         ELSE
            TOKEN  = ' '
            LENGTH = 0
 
            CALL CHKOUT ( 'TOKENS' )
            RETURN
         END IF
      END DO
 
C
C     We must have found something, or we wouldn't be here.
C
      END = CPOS ( SOURCE(LINE), ' ', BEGIN ) - 1
 
      IF ( END .LT. 0 ) THEN
         END = LEN ( SOURCE(LINE) )
      END IF
 
C
C     We may have to break the word to fit within the space allotted
C     for the token.
C
      MAXLEN = LEN ( TOKEN )
      WRDLEN = END - BEGIN + 1
 
      IF ( MAXLEN .LT. WRDLEN ) THEN
         CALL CUTSTR ( SOURCE(LINE), BEGIN, MAXLEN, ' ', BEGIN, END )
      END IF
 
      TOKEN  = SOURCE(LINE)(BEGIN : END)
      LENGTH = END - BEGIN + 1
 
C
C     Next time, pick up where we left off this time.
C
      IF ( END .EQ. LEN ( SOURCE(LINE) ) ) THEN
         LINE = LINE + 1
         COL  = 1
 
      ELSE
         COL  = END + 1
      END IF
 
      CALL CHKOUT ( 'TOKENS' )
      RETURN
      END
 
