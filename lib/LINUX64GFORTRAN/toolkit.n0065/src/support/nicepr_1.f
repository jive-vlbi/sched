C$Procedure     NICEPR_1 ( Nicely printed output -- test version )
 
      SUBROUTINE NICEPR_1 ( STRING, STYLE, MYIO )
      IMPLICIT NONE
 
C$ Abstract
C
C     Output a string to a unit using one of a set of available styles.
C     Format and output a string so that it has a pleasing appearance
C     (breaks for newlines occurring at natural places, margins set at
C     desired levels, etc.)
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
C     STRING
C     TEXT
C     UTILITY
C
C$ Declarations
 
      CHARACTER*(*)         STRING
      CHARACTER*(*)         STYLE
      EXTERNAL              MYIO
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  ---------------------------------------------------
C     STRING     I   Message to be printed.
C     STYLE      I   Format specification string.
C     MYIO       I   A routine that handles output
C
C$ Detailed_Input
C
C     STRING    A long string to be broken into columns and output.
C               If desired, the user can force various changes to the
C               format of the output by inserting control substrings
C               into the desired text, and specifying these control
C               strings in the character string STYLE.
C
C               Three control functions are possible.  They are:
C
C               1) Force a newline.
C               2) Force a newline and alter the margins for output.
C               3) Insert a vertical tab in the output.
C
C               To force a new line at some location of the string
C               during output you must put the KEYWORD 'NEWLINE'
C               into the string STYLE followed by a word that will
C               be used to signal a linebreak.  For example you
C               might use ' NEWLINE /cr '.  At any point of the
C               string that a newline is desired, insert the string
C               associated with the NEWLINE keyword ( in this case
C               '/cr' ).  Spaces are not required around the NEWLINE
C               control string (or any other control string).
C
C               To modify the margins after a line break, you insert
C               the line break control string into STRING and insert
C               immediately after it a string of the form (x:y) where
C               x and y are numeric strings.  The number x indicates
C               that the left margin should be moved x to the right.
C               The number y indicates the right margin should be
C               moved y to the left.  Both negative and positive
C               values are allowed.  Spaces are allowed within the
C               spaces between parentheses. In keeping with our
C               previous example both
C
C                      '/cr(5:-2)' and '/cr( 5 : -2 )'
C
C               directs the routine to force an line break; move the
C               left margin 5 to the right; move the right margin
C               -2 to the left (.i.e. 2 to the right).
C
C                      /cr (5: -2)
C
C               would be treated as simply a new line directive, the
C               remainder (5: -2) is treated as part of the string
C               to output.
C
C               To force a vertical tab at any point of the string
C               you must specify a vertical tab control string in
C               the style string.  Then at the point in string
C               where you want a vertical tab to appear, simply insert
C               the vertical tab string, spaces are NOT required
C               around the vertical tab string.
C
C               All control substrings in STRING are treated as having
C               zero width and invisible to output.
C
C     MYIO      Is a routine that takes a single string as input and
C               appropriately outputs the string.  It should be declared
C               external in the calling routine.
C
C     STYLE  is a character string that controls how the string
C            should be formatted on output and what substrings
C            of STRING will be treated as control characters
C            for display of STRING.  STYLE should consist of
C            a sequence of keyword/value word pairs.  That is,
C            it should consist of a sequence of words ( according
C            to the SPICE definition of a word ) in a pattern
C            as illustrated here:
C
C            Keyword_1 Value_1 Keyword_2 Value_2 ... Keyword_n Value_n
C
C            Acceptable keywords, their meanings, and expectations
C            regarding their associate values are spelled at below.
C
C          'FLAG'       is a keyword used to indicate that a string
C                       will prefix the output of STRING.  Moreover
C                       STRING will be printed in a block that is
C                       indented by one more than the nonblank length
C                       of the FLAG. (The appearance will parallel what
C                       you see here in this description, where 'FLAG'
C                       is the flag associated with this block of text.)
C
C                       If a flag is specified, the resulting output
C                       will consist of a flag, 1 space and formatted
C                       output text.
C
C                       Unless the FLAG keyword appears, no flag is
C                       used with the output.
C
C          'LEADER'     is the keyword used to indicate that the left
C                       margin of the output will always begin with
C                       the word that follows LEADER.  The leader
C                       string will not appear on the FLAG line
C                       if a FLAG is specified. The leader can
C                       be placed on the FLAG line by simply making
C                       it part of the flag.
C
C                       Unless the LEADER keyword appears, no leader is
C                       used with the output.
C
C          'TRAILER'    is the keyword used to indicate that the right
C                       margin of the output will always end with
C                       the word that follows TRAILER.  The trailer
C                       will appear in every line.
C
C           The effect of using the keywords LEADER, TRAILER and FLAG
C           is to change the margins specified (or implied) through
C           the use of LEFT and RIGHT.  The effective value of LEFT
C           will become LEFT + MAX ( LEN(LEADER), LEN(FLAG)+1 ).
C           The right margin becomes RIGHT - LEN(TRAILER).
C
C
C          'LEFT'       is the keyword used to indicate where the
C                       left margin of the output text should appear
C                       (either on the output screen or in a file).
C                       Note if a FLAG is present, when displayed the
C                       flag will start in this column of the output.
C                       The remaining text will be indented one
C                       more than the width of the nonblank portion of
C                       the flag.  If no flag is present, output will
C                       begin in the specified LEFT column.
C
C                       The word that immediately follows LEFT must
C                       successfully parse as an integer.
C
C                       If the LEFT keyword does not appear the left
C                       margin is set to 1.
C
C          'RIGHT'      is the keyword used to indicate where the
C                       right margin of the output text should appear
C                       (either on the output screen or in a file).
C
C                       The word that immediately follows RIGHT must
C                       successfully parse as an integer.
C
C                       If the RIGHT keyword does not appear the right
C                       margin is set to 80.
C
C          'NEWLINE'    is the keyword used to indicate what substring
C                       if any within the text string will be
C                       intrepreted as meaning "start a new line" and
C                       optionally "reset the margins."  (See STRING
C                       for details concerning the use of the newline
C                       substring.)
C
C                       If the keyword NEWLINE is not present, no
C                       substring of STRING will be interpreted as
C                       directing a newline to be started.
C
C          'VTAB'       is the keyword used to indicate what substring
C                       within the text string will be interpreted
C                       as meaning "start a new line, but indent it
C                       to the current position within this line."
C                       This is refered to as a vertical tab.
C
C                       If the keyword VTAB is not present no substring
C                       of STRING will be interpreted as a vertical
C                       tab.
C
C          'HARDSPACE'  is the keyword used to indicate what character
C                       within the text string will be processed as a
C                       normal text character, but will be written out
C                       as a space upon output. Note HARDSPACES in both
C                       the FLAG and LEADER will converted into spaces
C                       upon output.
C
C                       If the keyword HARDSPACE is not present, no
C                       character will be interpreted as a hard space.
C
C     MYIO      Is a routine that takes a single string as input and
C               appropriately outputs the string.  It should be declared
C               external in the calling routine.
C
C$ Detailed_Output
C
C     None.
C
C$ Exceptions
C
C     1)  If a keyword/value pair is entered more than once in
C         the style string, the last pair takes precedence.
C
C     2)  If a keyword appears without a following value in the
C         style string the SPICE error 'SPICE(UNBALACEDPAIR)' is
C         signaled.
C
C     3)  If a keyword is not recognized, the error 'SPICE(UNKNOWNKEY)'
C         is signaled
C
C     4)  If one of the margin keywords (LEFT RIGHT) is not followed
C         by a numeric string, the error 'SPICE(NONNUMERICSTRING)'
C         is signaled.
C
C     5)  If the left column becomes less than zero, or the right column
C         becomes less than the left column the error
C         'SPICE(INVALIDCOLUMN)' is signaled.
C
C     6)  If the number of columns from the left to the right margin
C         becomes less than or equal to the number of characters in the
C         flag (assuming one is specified) the error
C         'SPICE(SPACETOONARROW)' is signaled.
C
C      7) If output cannot be performed, the error 'SPICE(OUTPUTERROR)'
C         will be signaled and a descriptive long message set to
C         aid in determining the cause of the output failure.
C
C      8) If the right margin exceeds 512, the output will be truncated
C         at column 512.
C
C$ Particulars
C
C     This routine is designed to aid in the problem of creating
C     nice looking messages that must extend over 1 line.  It
C     allows the user to construct messages by simply appending,
C     prefixing or inserting text into an existing string until
C     the message is finished.  The user need not be concerned
C     about breaking up the message in good spots for output.
C     This routine searches the message in STRING for "good" places
C     at which to break STRING.
C
C     The user may specify a "flag" that will be used to prefix the
C     first output line, left and right margins for the output,
C     and special strings for forcing creation of newlines, changing
C     margins, and inserting vertical tabs.
C
C     This routine always sends to output a blank line prior to
C     the start of the output formatted string.
C
C     Since strings are often built by concatenation, the user may
C     want to compress out extra spaces in string before calling
C     this routine.  This routine breaks the input string at gaps
C     in the string, but does not get rid of large gaps within
C     a successfully broken output line. (See the examples below.)
C
C     For a discussion of the string breaking algorithm see the
C     particulars section of the SPICE routine CUTSTR.
C
C$ Files
C
C     The output is sent to the file or device connected to the logical
C     unit UNIT that has been appropriately prepared by the calling
C     program.
C
C$ Examples
C
C     Suppose
C
C           STYLE  = 'LEFT 10 RIGHT 50 '
C     and
C
C           STRING = 'Now is the time for all good men to come to ' //
C          .         'the aid of their party. Out with the bad ' //
C          .         'air and in with the good.  Health and purity '//
C          .         'preserve our essence. '
C
C     The the output would look like:
C
C     Column
C     1........10........20........30........40........50........60
C
C              Now is the time for all good men to come
C              the aid of their party.  Out with the
C              bad air and in with the good.  Health and
C              purity preserve our essence.
C
C
C
C     Suppose
C
C           STYLE  = 'FLAG Example: LEFT 10 RIGHT 50 '
C     and
C
C           STRING = 'Now is the time for all good men to come to ' //
C          .         'the aid of their party. Out with the bad ' //
C          .         'air and in with the good.  Health and purity '//
C          .         'preserve our essence. '
C
C     The the output would look like:
C
C     Column
C     1........10........20........30........40........50........60
C
C               Example: Now is the time for all good men
C                        to come to the aid of their
C                        party. Out with the bad air and
C                        in with the good.  Health and
C                        purity preserve our essence.
C
C
C
C
C
C     Suppose
C
C           STYLE  = 'FLAG Example: LEFT 10 RIGHT 50 NEWLINE .'
C     and
C
C           STRING = 'Now is the time for all good men to come to ' //
C          .         'the aid of their party. Out with the bad ' //
C          .         'air and in with the good.  Health and purity '//
C          .         'preserve our essence. '
C
C     The the output would look like:
C
C     Column
C     1........10........20........30........40........50........60
C
C              Example: Now is the time for all good men
C                       to come to the aid of their
C                       party
C                       Out with the bad air and in with
C                       the good
C                       Health and purity preserve our
C                       essence
C
C
C
C
C
C
C
C
C
C     Suppose
C
C           STYLE  = 'FLAG Example: LEFT 10 RIGHT 50 VTAB . HARDSPACE _'
C     and
C
C           STRING = '___ is the time for all good men to come to ' //
C          .         'the aid of their party. Out with the bad ' //
C          .         'air and in with the good.  Health and purity '//
C          .         'preserve our essence. '
C
C     The the output would look like:
C
C     Column
C     1........10........20........30........40........50........60
C
C              Example:     is the time for all good men
C                       to come to the aid of their
C                       party
C                             Out with the bad air and
C                       in with the good
C                                         Health and
C                       purity preserve our essence
C
C
C
C
C
C     Suppose
C
C        STYLE  = 'FLAG Error: LEFT 1 RIGHT 60 NEWLINE /cr VTAB /vt'
C
C     and
C
C        STRING = 'I believe the command you were attempting to enter'//
C       .         'was /cr/cr(5:5)FIND TIMES OF GREATEST ELONGATION ' //
C       .         'FOR VENUS /cr/cr(-5:-5) I was expecting to the '   //
C       .         'word GREATEST when I encountered the word GRETEST '//
C       .         'in your input command. /cr/cr(5:5) FIND TIMES OF ' //
C       .         '/vt/vt GRETEST /vt/vt ELONGATION FOR VENUS '       //
C       .         '/cr/cr(-5:-5) I think you left out the fourth '    //
C       .         'letter --- "A" .
C
C     The the output would look like:
C
C     Column
C     1........10........20........30........40........50........60
C
C     Error: I believe the command you were attempting to enter
C            was
C
C                 FIND TIMES OF GREATEST ELONGATION FOR VENUS
C
C            I was expecting to see the word GREATEST when I
C            encountered the word GRETEST in your input command.
C
C                 FIND TIMES OF
C
C                               GRETEST
C
C                                       ELONGATION FOR VENUS
C
C            I think you left out the fourth letter --- "A" .
C
C
C   Some care should be taken when choosing substrings to indidicate
C   newline and vertical tab control.  For example, suppose
C
C      STYLE  = ' FLAG NAIF: LEFT 6 RIGHT 56 NEWLINE xx VTAB AA '
C
C   and
C
C      STRING = 'Officials at Exxon today reported a deal with the '  //
C     .         'Automobile Association of America (AAA) that would ' //
C     .         'provide club memebers with discount prices on '      //
C     .         'gasoline. xxxx( 3:3) Spokesmen said AA "Get your '   //
C     .         'AAA membership cards now." AA xx(-3:-3)xx Texeco '   //
C     .         'officials had no comment.'
C
C
C     The the output would look like:
C
C     Column
C     1........10........20........30........40........50........60
C
C          NAIF: Officials at E
C                on today reported a deal with the Automobile
C                Assosiation of America (
C                                        A) that would provide
C                club members with discount prices on
C                gasoline.
C
C                   Spokesmen said
C                                   "Get your
C                                             A membership
C                   cards now."
C
C                Texeco officials had no comment.
C
C
C$ Restrictions
C
C     It is the responsibility of the calling program to properly
C     prepare the device/file associated with the logical unit UNIT
C     to receive the output from this routine.
C
C     The RIGHT margin must be less than or equal to 512.
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
C-     Support Version 1.1.0, 22-APR-1997 (WLT)
C
C        Modified calls to SETMSG to use a marker and then replace
C        marker using ERRCH.
C
C-     Test Utility Version 2.0.0, 7-APR-1995 (WLT)
C
C         The routine was updated to fixed DO WHILE loop problems
C         caused by accessing characters past the end of the string.
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
C
C     Beta Version 1.0.0, 12-AUG-1988 (WLT) (IMU)
C
C-&
 
 
 
 
C
C     SPICELIB Functions.
C
      LOGICAL               RETURN
      INTEGER               FRSTNB
      INTEGER               QLSTNB
      INTEGER               UPTO
      LOGICAL               MATCH
      INTEGER               POS
      INTEGER               NCPOS
C
C   Local Variables
C
      INTEGER               LEFT
      INTEGER               RIGHT
 
      LOGICAL               NLINTK
      LOGICAL               VTABTK
      LOGICAL               FLAGTK
      LOGICAL               HARDSP
 
      CHARACTER*1           HSPCHR
 
      INTEGER               BEG
      INTEGER               END
      INTEGER               B
      INTEGER               E
 
      INTEGER               VBEG
      INTEGER               VB
      INTEGER               VE
 
      INTEGER               FLAGB
      INTEGER               FLAGE
      INTEGER               PRAMBW
      INTEGER               PSTAMB
 
      INTEGER               VTABB
      INTEGER               VTABE
      INTEGER               VTABW
 
      INTEGER               NLINB
      INTEGER               NLINE
      INTEGER               NLINEW
 
      INTEGER               K
      CHARACTER*160         ERRORL
      CHARACTER*160         ERRORR
 
      CHARACTER*512         LINE
      CHARACTER*1           BREAKS
 
      INTEGER               LEFTB
      INTEGER               LEFTE
      INTEGER               RIGHTB
      INTEGER               RIGHTE
      INTEGER               NLEFT
      INTEGER               NRIGHT
 
      INTEGER               WIDTH
      INTEGER               W
      INTEGER               START
 
 
      INTEGER               VTABAT
      INTEGER               INDENT
 
      LOGICAL               LOOPED
      INTEGER               LRIGHT
 
      INTEGER               LAST
 
      LOGICAL               NEWLIN
      LOGICAL               MRGCHG
 
      LOGICAL               LEADTK
      LOGICAL               TRLTK
      INTEGER               FLAGW
      INTEGER               LEADRW
      INTEGER               TRAILW
      INTEGER               ORIGR
      INTEGER               ORIGL
      INTEGER               RMARG
      INTEGER               TRAILE
      INTEGER               TRAILB
      INTEGER               LEADRB
      INTEGER               LEADRE
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NICEPR_1' )
      END IF
 
C
C     Set the defaults and initial values.
C
C
C     Set the defaults and initial values.
C
      LEFT   = 1
      RIGHT  = 80
 
      FLAGTK = .FALSE.
      LEADTK = .FALSE.
      HARDSP = .FALSE.
      NLINTK = .FALSE.
      TRLTK  = .FALSE.
      VTABTK = .FALSE.
 
      HSPCHR = ' '
 
      FLAGW  = 0
      LEADRW = 0
      TRAILW = 0
      PRAMBW = 0
 
      VTABW  = 0
      BEG    = 1
      ERRORL = ' '
      ERRORR = ' '
      BREAKS = ' '
 
C
C     Parse the style string.
C
 
      CALL FNDNWD ( STYLE, BEG, B, E )
 
      DO WHILE ( B .NE. 0 )
 
         VBEG = E + 1
 
         CALL  FNDNWD ( STYLE, VBEG, VB, VE )
 
         IF ( VB .NE. 0 ) THEN
 
            IF      ( STYLE(B:E) .EQ. 'FLAG'    ) THEN
 
               FLAGB  = VB
               FLAGE  = VE
               FLAGW  = VE - VB + 2
               FLAGTK = .TRUE.
 
            ELSE IF ( STYLE(B:E) .EQ. 'LEADER'    ) THEN
 
               LEADRB = VB
               LEADRE = VE
               LEADRW = VE - VB + 1
               LEADTK = .TRUE.
 
 
            ELSE IF ( STYLE(B:E) .EQ. 'TRAILER'   ) THEN
 
               TRAILB = VB
               TRAILE = VE
               TRAILW = VE - VB + 1
               TRLTK  = .TRUE.
 
 
            ELSE IF ( STYLE(B:E) .EQ. 'VTAB'    ) THEN
 
               VTABB  = VB
               VTABE  = VE
               VTABW  = VE - VB + 1
               VTABTK = .TRUE.
 
            ELSE IF ( STYLE(B:E) .EQ. 'NEWLINE' ) THEN
 
               NLINB  = VB
               NLINE  = VE
               NLINEW = VE - VB + 1
               NLINTK = .TRUE.
 
            ELSE IF ( STYLE(B:E) .EQ. 'LEFT'    ) THEN
 
               CALL NPARSI ( STYLE(VB:VE), LEFT, ERRORL,  K )
               IF ( ERRORL .NE. ' ' ) THEN
 
                  CALL SETMSG ( 'The word following the keyword '     //
     .                          '''LEFT'' must parse as an '          //
     .                          'integer. #'                          )
                  CALL ERRCH  ( '#', ERRORL               )
                  CALL SIGERR ( 'SPICE(NONNUMERICSTRING)' )
                  CALL CHKOUT ( 'NICEPR_1' )
                  RETURN
 
               END IF
 
            ELSE IF ( STYLE(B:E) .EQ. 'RIGHT'   ) THEN
 
               CALL NPARSI ( STYLE(VB:VE), RIGHT, ERRORR, K )
               IF ( ERRORR .NE. ' ' ) THEN
 
                  CALL SETMSG ( 'The word following the keyword '     //
     .                          '''RIGHT'' must parse as an '         //
     .                          'integer. #'                          )
                  CALL ERRCH  ( '#', ERRORR               )
                  CALL SIGERR ( 'SPICE(NONNUMERICSTRING)' )
                  CALL CHKOUT ( 'NICEPR_1' )
                  RETURN
 
               END IF
 
            ELSE IF ( STYLE(B:E) .EQ. 'HARDSPACE'    ) THEN
 
               HARDSP = .TRUE.
 
               IF ( VB .NE. VE ) THEN
 
                  CALL SETMSG ( 'Hardspaces must be a single '        //
     .                          'character.  You have "#".'   )
                  CALL ERRCH  ( '#', STYLE(VB:VE)     )
                  CALL SIGERR ( 'SPICE(BADHARDSPACE)' )
                  CALL CHKOUT ( 'NICEPR_1'            )
                  RETURN
 
               ELSE
 
                  HSPCHR = STYLE(VB:VB)
 
               END IF
 
 
            ELSE
 
               LINE = STYLE ( B:E )
               CALL SUFFIX ( 'is not a recognized keyword for '       //
     .                       'the SPICELIB routine NICEIO. ',
     .                       1, LINE   )
 
               CALL SETMSG ( LINE                )
               CALL SIGERR ( 'SPICE(UNKNOWNKEY)' )
               CALL CHKOUT ( 'NICEPR_1' )
               RETURN
 
            END IF
 
            BEG = VE + 1
            CALL  FNDNWD ( STYLE, BEG, B, E )
 
         ELSE
               CALL SETMSG ( '# did not have an associated value' )
               CALL ERRCH  ( '#', STYLE(B:E)                      )
               CALL SIGERR ( 'SPICE(UNBALANCEDPAIR)'              )
               CALL CHKOUT ( 'NICEPR_1' )
               RETURN
         END IF
 
      END DO
 
C
C     So ends the parsing of the style string.  Now do the actual work.
C
      LINE = ' '
 
C
C     Determine how much space needs to be allocated for the
C     flag and leaders.
C
      ORIGR  = RIGHT
      ORIGL  = LEFT
      RMARG  = RIGHT
 
      PRAMBW = MAX ( FLAGW, LEADRW )
      PSTAMB = RIGHT - TRAILW + 1
      RIGHT  = RIGHT - TRAILW
 
      IF ( FLAGW .GT. 0 ) THEN
         LINE(ORIGL:) =  STYLE(FLAGB:FLAGE)
      ELSE IF ( LEADRW .GT. 0 ) THEN
         LINE(ORIGL:) = STYLE(LEADRB:LEADRE)
      END IF
 
      IF ( TRAILW .GT. 0 ) THEN
         LINE(PSTAMB:ORIGR) = STYLE(TRAILB:TRAILE)
      END IF
 
      B    = MAX(1,FRSTNB(STRING))
      LAST =       QLSTNB(STRING)
C
C     If there is a newline token, we have to write out empty lines
C     and modify the margins as we encounter newline tokens and
C     newline tokens with margin modifiers.  Typically the loop
C     in the if block below will never be exercised.
C
      IF ( NLINTK ) THEN
 
 
         E  = B+NLINEW-1
 
         IF ( E .LT. LAST ) THEN
            NEWLIN = STRING(B:E) .EQ. STYLE(NLINB:NLINE)
         ELSE
            NEWLIN = .FALSE.
         END IF
 
 
         DO WHILE ( NEWLIN )
C
C           See if the new line token is qualified so as to change
C           the margins of the output.
C
            IF ( E+1 .LT. LAST ) THEN
               MRGCHG = MATCH( STRING(E+1:), '(*:*)*' )
            ELSE
               MRGCHG = .FALSE.
            END IF
 
            IF ( MRGCHG ) THEN
 
C
C              Looks like we should change the columns. Locate the
C              tokens of the newline marker.
C
               CALL FNDNTK( STRING, '(:', E+1,   LEFTB,  LEFTE  )
               CALL FNDNTK( STRING, ':)', LEFTE, RIGHTB, RIGHTE )
 
C
C              Parse the strings representing the increments to left
C              and right column positions.
C
               ERRORL = ' '
               ERRORR = ' '
 
               IF (LEFTB .LE. LEFTE) THEN
                  CALL NPARSI( STRING(LEFTB:LEFTE),   NLEFT, ERRORL,  K)
               ELSE
                  NLEFT  = 0
               END IF
 
               IF ( RIGHTB .LE. RIGHTE ) THEN
                  CALL NPARSI( STRING(RIGHTB:RIGHTE), NRIGHT, ERRORR, K)
               ELSE
                  NRIGHT = 0
               END IF
 
C
C              Only if no errors were encountered during parsing do we
C              change the columns.
C
               IF (       ( ERRORL .EQ. ' ' )
     .              .AND. ( ERRORR .EQ. ' ' ) ) THEN
 
                  B     = RIGHTE + 2
                  LEFT  = LEFT   + NLEFT
                  RIGHT = RIGHT  - NRIGHT
 
                  RMARG = MAX ( ORIGR, RIGHT )
 
               ELSE
                  B     = B      + NLINEW
               END IF
 
            ELSE
               B = B + NLINEW
            END IF
 
C
C           Check for goofy margins.
C
            IF      ( LEFT .LT. 1     ) THEN
 
               CALL SETMSG ( 'The current value for the left column ' //
     .                       'is #. This is less than 1 and thus '    //
     .                       'not a valid value. '  )
               CALL ERRINT ( '#', LEFT              )
               CALL SIGERR ( 'SPICE(INVALIDCOLUMN)' )
               CALL CHKOUT ( 'NICEPR_1' )
               RETURN
 
            ELSE IF ( LEFT .GT. RIGHT ) THEN
               CALL SETMSG ( 'The current value for the left column ' //
     .                       'is greater than the value for the '     //
     .                       'right column. The value for the left '  //
     .                       'column is #.  The value for the right ' //
     .                       'column is #. '        )
               CALL ERRINT ( '#', LEFT              )
               CALL ERRINT ( '#', RIGHT             )
               CALL SIGERR ( 'SPICE(INVALIDCOLUMN)' )
               CALL CHKOUT ( 'NICEPR_1' )
               RETURN
            END IF
 
C
C           Output something, but first replace hard spaces by spaces
C
 
            IF ( HARDSP ) THEN
               CALL
     .         REPLCH ( LINE(1:RMARG), HSPCHR, ' ', LINE(1:RMARG) )
            END IF
 
            CALL MYIO ( LINE(1:RMARG) )
 
            LINE = ' '
 
            IF ( LEADTK ) THEN
               LINE (ORIGL:)  = STYLE(LEADRB:LEADRE)
            END IF
 
            IF ( TRLTK  ) THEN
               LINE (PSTAMB:) = STYLE(TRAILB:TRAILE)
            END IF
 
 
C
C           Adjust the beginning and ending of the next portion
C           of the string to examine.
C
            B = MAX( B, NCPOS( STRING, ' ', B ) )
            E = B + NLINEW - 1
 
            IF ( E .LT. LAST ) THEN
               NEWLIN = STRING(B:E) .EQ. STYLE(NLINB:NLINE)
            ELSE
               NEWLIN = .FALSE.
            END IF
 
         END DO
C
C        Find the next portion of the string to examine (it's up to
C        the next new line token or end of string whichever
C        comes first.
C
         E     = UPTO   ( STRING,  STYLE(NLINB:NLINE), B )
      ELSE
         E     = LAST
      END IF
 
 
C
C     Now we have are to the point of processing legitimate text.
C     Process the current substring  STRING(B:E).  It contains
C     no newline tokens.
C
      DO WHILE ( E .NE. 0 )
 
         WIDTH = RIGHT - LEFT + 1 - PRAMBW
 
         IF ( WIDTH .LT. 1 ) THEN
            CALL SIGERR ( 'SPICE(SPACETOONARROW)')
            CALL CHKOUT ( 'NICEPR_1' )
            RETURN
         END IF
 
         W      = WIDTH
         START  = B
         INDENT = 0
 
C
C        Grab the biggest piece of the substring that can be output
C        within the allowed space.
C
 
         CALL CUTSTR ( STRING(1:E), START, W, BREAKS, BEG, END )
 
         DO WHILE ( BEG .NE. 0 )
 
C
C           See if there are any vertical tab marks
C
            IF ( .NOT. VTABTK ) THEN
 
               LINE  (LEFT + PRAMBW : RIGHT     ) = STRING( BEG: END )
 
            ELSE
 
               VTABAT = POS( STRING(1:E), STYLE(VTABB:VTABE), START )
 
               IF ( VTABAT .GT. 0 .AND. VTABAT .LE. END ) THEN
C
C                 If there is a vertical tab at the beginning of the
C                 string, we don't need to modify LINE.
C
                  IF ( VTABAT .GT. BEG ) THEN
 
                     END    = VTABAT - 1
                     LINE(  LEFT+PRAMBW+INDENT:RIGHT) = STRING(BEG:END)
 
                     INDENT = INDENT + END - BEG + 1
                     END    = END    + VTABE - VTABB + 1
 
                  ELSE IF ( VTABAT .EQ. BEG ) THEN
 
                     LINE(  LEFT+PRAMBW+INDENT:RIGHT) = ' '
                     END    = BEG    + VTABE - VTABB
 
                  END IF
 
               ELSE
C
C                 We just fill out the rest of this line.  There will
C                 be no need to indent the next one.
C
                  LINE ( LEFT+PRAMBW+INDENT:RIGHT) = STRING(BEG:END)
                  INDENT = 0
 
               END IF
 
            END IF
 
C
C           Handle any hard spaces
C
            IF ( HARDSP ) THEN
               CALL
     .         REPLCH ( LINE(1:RMARG), HSPCHR, ' ', LINE(1:RMARG) )
            END IF
 
            CALL MYIO ( LINE(1:RMARG) )
 
            LINE  = ' '
 
            IF ( LEADTK ) THEN
               LINE (ORIGL:)  = STYLE(LEADRB:LEADRE)
            END IF
 
            IF ( TRLTK  ) THEN
               LINE (PSTAMB:) = STYLE(TRAILB:TRAILE)
            END IF
 
            START = END + 1
            W     = WIDTH - INDENT
 
            IF ( W .LT. 3 ) THEN
               W = WIDTH
            END IF
 
            CALL CUTSTR ( STRING(1:E), START, W, BREAKS, BEG, END )
 
         END DO
 
C
C        Check to see if we should be looking for a newline token.
C
         IF ( NLINTK ) THEN
 
C
C           Ok.  Get ready to jump through hoops again.  We have to
C           look for newline tokens, for all those in excess of one
C           in a row, we have to output a blank line.
C
            B      = E + 1
            E      = E + NLINEW
            LOOPED = .FALSE.
 
            IF ( E .LE. LAST ) THEN
               NEWLIN = STRING(B:E) .EQ. STYLE(NLINB:NLINE)
            ELSE
               NEWLIN = .FALSE.
            END IF
 
            DO WHILE ( NEWLIN )
 
               LRIGHT = RIGHT
C
C              See if the new line token is qualified so as to change
C              the margins of the output.
C
               IF ( E .GE. LAST ) THEN
 
C
C                 In this case we can't possibly match as in the case
C                 below
C
                  B = B + NLINEW
 
               ELSE IF ( MATCH( STRING(E+1:), '(*:*)*' ) ) THEN
 
C
C                 Looks like we should change the columns. Locate the
C                 tokens of the newline marker.
C
                  CALL FNDNTK( STRING, '(:', E,     LEFTB,  LEFTE  )
                  CALL FNDNTK( STRING, ':)', LEFTE, RIGHTB, RIGHTE )
 
C
C                 Parse the strings representing the increments to left
C                 and right column positions.
C
                  ERRORL = ' '
                  ERRORR = ' '
 
                  IF (LEFTB .LE. LEFTE) THEN
                     CALL NPARSI ( STRING(LEFTB:LEFTE), NLEFT,
     .                             ERRORL, K )
                  ELSE
                     NLEFT  = 0
                  END IF
 
                  IF ( RIGHTB .LE. RIGHTE ) THEN
                     CALL NPARSI ( STRING(RIGHTB:RIGHTE), NRIGHT,
     .                             ERRORR, K )
                  ELSE
                     NRIGHT = 0
                  END IF
 
C
C                 Only if no errors were encountered during parsing
C                 do we change the columns.
C
                  IF (       ( ERRORL .EQ. ' ' )
     .                 .AND. ( ERRORR .EQ. ' ' ) ) THEN
 
                     B     = RIGHTE + 2
                     LEFT  = LEFT   + NLEFT
                     RIGHT = RIGHT  - NRIGHT
                     RMARG = MAX(ORIGR, RIGHT )
 
                  ELSE
                     B     = B      + NLINEW
                  END IF
 
               ELSE
                  B = B + NLINEW
               END IF
 
C
C              Take care of the case when outdenting or indenting has
C              forced us into absurd margins.
C
               IF      ( LEFT .LT. 1     ) THEN
 
                  CALL SETMSG ( 'The current value for the left '     //
     .                          'column '                             //
     .                          'is #. This is less than 1 and thus ' //
     .                          'not a valid value. '  )
                  CALL ERRINT ( '#', LEFT              )
                  CALL SIGERR ( 'SPICE(INVALIDCOLUMN)' )
                  CALL CHKOUT ( 'NICEPR_1' )
                  RETURN
 
               ELSE IF ( LEFT .GT. RIGHT ) THEN
 
                  CALL SETMSG ( 'The current value for the left '     //
     .                          'column '                             //
     .                          'is greater than the value for the '  //
     .                          'right column. The value for the '    //
     .                          'left '                               //
     .                          'column is #.  The value for the '    //
     .                          'right column is #. '  )
                  CALL ERRINT ( '#', LEFT              )
                  CALL ERRINT ( '#', RIGHT             )
                  CALL SIGERR ( 'SPICE(INVALIDCOLUMN)' )
                  CALL CHKOUT ( 'NICEPR_1' )
                  RETURN
               END IF
 
C
C              Output something if this is not the first pass through
C              the loop.
C
               IF ( .NOT. LOOPED ) THEN
                  LOOPED = .TRUE.
                  LINE   = ' '
 
                  IF ( LEADTK ) THEN
                     LINE (ORIGL:)  = STYLE(LEADRB:LEADRE)
                  END IF
 
                  IF ( TRLTK  ) THEN
                     LINE (PSTAMB:) = STYLE(TRAILB:TRAILE)
                  END IF
 
               ELSE
C
C                 Handle any hard spaces
C
                  IF ( HARDSP ) THEN
                     CALL
     .               REPLCH( LINE(1:RMARG), HSPCHR, ' ', LINE(1:RMARG) )
                  END IF
 
                  CALL MYIO ( LINE(1:RMARG) )
 
               END IF
 
               B = MAX( B, NCPOS( STRING, ' ', B ) )
               E = B + NLINEW - 1
 
               IF ( E .LE. LAST ) THEN
                  NEWLIN = STRING(B:E) .EQ. STYLE(NLINB:NLINE)
               ELSE
                  NEWLIN = .FALSE.
               END IF
 
            END DO
 
            E = UPTO ( STRING, STYLE(NLINB:NLINE), B )
 
C
C           Just in case we went through the loop, and didn't
C           output a line, and we've reached the end of the
C           string.  We check and write a blank line if necessary
C
            IF ( LOOPED .AND. ( E .EQ. 0 )) THEN
 
C
C              Handle any hard spaces
C
               IF ( HARDSP ) THEN
                  CALL
     .            REPLCH ( LINE(1:RMARG), HSPCHR, ' ', LINE(1:RMARG) )
               END IF
 
               CALL MYIO ( LINE(1:RMARG) )
 
            END IF
 
         ELSE
            E = 0
         END IF
 
      END DO
 
      CALL CHKOUT ( 'NICEPR_1' )
      RETURN
 
      END
