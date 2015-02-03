C$Procedure M2GETD (META/2 --- select a named word, double precision )
 
      SUBROUTINE M2GETD ( NAME, STRING, FOUND, DP   )
      IMPLICIT NONE
 
C$ Abstract
C
C     Select the Nth substring associated with a matched, named META/2
C     template word and parse it as a double precision number.
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
C     META/2 --- a language specification language.
C
C$ Keywords
C
C     META/2
C     PARSING
C
C$ Declarations
 
      CHARACTER*(*)         NAME
      CHARACTER*(*)         STRING
      LOGICAL               FOUND
      DOUBLE PRECISION      DP
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NAME       I   the name of some matched META/2 template word.
C     STRING     I   the string that matched the META/2 template.
C     FOUND      O   returned TRUE if the request could be fulfilled.
C     DP         O   matching d.p. extracted and parsed from STRING.
C
C$ Detailed_Input
C
C     NAME       is the name of some named META/2 template word that
C                may have matched some portion of STRING.
C
C     STRING     is a string that successfully matched a META/2 template
C                containing the template word specified by NAME.
C
C$ Detailed_Output
C
C     FOUND      will be returned .TRUE. if the requested information
C                specified by NAME and STRING could be retrieved.
C                Otherwise it will be returned with a value of .FALSE.
C
C     DP         is the double precision number represented by the word
C                of STRING that was the first match with the NAMEd
C                META/2 template word.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the portion of STRING extracted is NOT a word, the error
C        'META/2(CORRUPTEDINPUTSTRING)' will be signalled.
C
C     2) If the portion of STRING extracted cannot be parsed as a
C        a double precision number, the error 'META/2(CORRUPTEDNUMBER)'
C         will be signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Users of META/2 need not only to be sure that strings match
C     anticipated syntax of the language they design, but they also
C     need to be able to extract the meaning of syntactically correct
C     commands or statements.  The routines
C
C        M2GETC  --- get a character string word
C        M2GETI  --- get and parse an integer
C        M2GETD  --- get and parse a double precision number
C        M2GETA  --- get all that matched
C        M2SELC  --- select the n'th character string word
C        M2SELI  --- select and parse the n'th integer
C        M2SELD  --- select and parse the n'th double precision number
C
C     exist to aid in the extraction of meaning from syntactically
C     correct strings.
C
C     To make use of this feature, you must add parsing information
C     to the language you design.  To do this you simply "name" template
C     words by appending to the syntax portion of the word a name
C     of your choosing surrounded by the square brackets '[' and ']'.
C     For example you might have a language template of the form:
C
C         OPEN @word
C
C     That would open the contents of a text file.  This statement
C     my itself can be used to make sure that a statement has
C     a recognizable form.  However, if the program is to take any
C     action corresponding in an expected way to such a statement
C     entered into a program, you must eventually find out what
C     "@word" matched.  To do this simply append a name to @word,
C     in this case a good name might be:
C
C        OPEN @word[textfile]
C
C     (Note that case is significant for named template words).
C     The template word "@word" in this syntax specification now
C     has a name: "textfile".  Once it is recognized that a string
C     has matched a template, you can now easily find the name
C     of the text file that a user specified by making the call
C
C        CALL M2GETC ( 'textfile', STRING, FOUND, FILE )
C
C     where STRING is the original, unaltered string that matched
C     the template "OPEN @word[textfile]".
C
C     FOUND will indicate whether or not a match for a template
C     word having name "textfile" was recorded (in this case it
C     will return with a value of .TRUE) and FILE will contain
C     the word of string that matched "@word[textfile]".
C
C     For many uses of META/2 you can ignore the FOUND flag.  Often
C     you know from the fact that the string matched the template
C     FOUND must be .TRUE.  However, in some cases the syntax will
C     not force a match to exist.  For example a statement that
C     matches the template below my not have values for "to"
C     or "from".  One will be present, but one might be absent.
C
C        SET LIMITS (1:2){ FROM @calendar[from]
C                        | TO   @calendar[to]    }
C
C     In such cases, may want to assign default values to the strings
C     you use to retrieve the calendar strings corresponding to
C     "to" and "from".  Or you may wish to examine the FOUND flag
C     after making the calls below.
C
C        CALL M2GETT ( 'from', STRING, FOUNDF, FROM )
C        CALL M2GETT ( 'to',   STRING, FOUNDT, TO   )
C
C     Note that if the logical flag returned is false, the value of
C     the output (in these examples FROM and TO) will not change from
C     the values they had upon input.  In this way you may assign
C     defaults to items that might be missing from a matched
C     string.  However, you should probably note that you are
C     assigning the defaults with a comment.  Without doing this
C     your intent will likely be unclear to another person who might
C     eventually need to read and understand your code.
C
C$ Examples
C
C     Suppose that a string matched the META/2 template
C
C        FIND @name[window] SEPARATION
C
C                      (2:2){ OF   @int[body1]    @int[body2]
C                           | FROM @int[observer]    }
C
C                      (1:1){ LESS[less]      THAN @number[bound]
C                           | GREATER[greater THAN @number[bound] }
C
C                      (0:1){ WITHIN INTERVAL[restricted]
C                             FROM @calendar[from] TO @calendar[to]    }
C
C
C      Then to extract the information in the string the following
C      sequence of calls will suffice.
C
C            CALL M2GETC ( 'window',    STRING, FOUND,  WINDOW )
C            CALL M2GETI ( 'body1',     STRING, FOUND,  BODY1  )
C            CALL M2GETI ( 'body2',     STRING, FOUND,  BODY2  )
C            CALL M2GETI ( 'observer',  STRING, FOUND,  OBS    )
C            CALL M2GETD ( 'bound',     STRING, FOUND,  BOUND  )
C            CALL M2GETA ( 'from',      STRING, FOUND,  FROM   )
C            CALL M2GETA ( 'to',        STRING, FOUND,  TO     )
C
C            LESS   = M2XIST ( 'less'        )
C            GREATR = M2XIST ( 'greater'     )
C            RSTRCT = M2XIST ( 'restriction' )
C
C       C
C       C    If they were supplied parse the bounds of the search
C       C    interval, otherwise just use the next decade.
C       C
C            IF ( RSTRCT ) THEN
C
C               CALL UTC2ET ( FROM, LOWER )
C               CALL UTC2ET ( TO,   UPPER )
C
C            ELSE
C
C               CALL UTC2ET ( '1 JAN 1990',  LOWER )
C               CALL UTC2ET ( '1 JAN 2000',  UPPER )
C
C            END IF
C
C       C
C       C    If we want the separation to be less than BOUND use
C       C    the next block.  Otherwise we will look for separation
C       C    greater than BOUND
C       C
C            IF ( LESS ) THEN
C
C                 search for "less than" separation
C
C            ELSE
C
C                 search for "greater than" separation
C
C            END IF
C
C       C
C       C    Finally, store the result of our computation in the
C       C    specified window.
C       C
C            CALL STORE_WINDOW ( RESULTS, WINDOW )
C
C$ Restrictions
C
C     It is vital that the string that matched a META/2 template
C     not be altered prior to calling any of the extraction routines.
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
C-    Beta Version 1.0.0, 27-NOV-1991 (WLT)
C
C-&
C
C$ Index_Entry
C
C     Extract first number matching a named template word
C
C-&
 
 
C
C     Local variables
C
      CHARACTER*(80)        ERROR
 
      DOUBLE PRECISION      MYDP
 
      INTEGER               B
      INTEGER               E
      INTEGER               F
      INTEGER               L
      INTEGER               P
      INTEGER               PNTER
 
 
 
C
C     First look up the beginning and endings of the requested word.
C
      CALL M2VGET ( NAME, 1, FOUND, B, E )
 
      IF ( .NOT. FOUND ) THEN
         RETURN
      END IF
 
C
C     First make sure there is nothing pathological about the string
C     we are dealing with.
C
      P = B - 1
      F = E + 1
      L = LEN(STRING)
 
      IF ( P .GT. 0 ) THEN
         IF ( STRING(P:P) .NE. ' ' ) THEN
            CALL CHKIN  ( 'M2GETD' )
            CALL SETMSG ( 'The input string has been modified since ' //
     .                    'it passed syntax validation in META/2. '   )
            CALL SIGERR ( 'META/2(CORRUPTEDINPUTSTRING)'              )
            CALL CHKOUT ( 'M2GETD' )
            RETURN
         END IF
      END IF
 
      IF ( F .LT. L ) THEN
         IF ( STRING(F:F) .NE. ' ' ) THEN
            CALL CHKIN  ( 'M2GETD' )
            CALL SETMSG ( 'The input string has been modified since ' //
     .                    'it passed syntax validation in META/2. '   )
            CALL SIGERR ( 'META/2(CORRUPTEDINPUTSTRING)'              )
            CALL CHKOUT ( 'M2GETD' )
            RETURN
         END IF
      END IF
 
 
      IF (      ( STRING(B:B) .EQ. ' ' )
     .     .OR. ( STRING(E:E) .EQ. ' ' ) ) THEN
         CALL CHKIN  ( 'M2GETD' )
         CALL SETMSG ( 'The input string has been modified since '    //
     .                 'it passed syntax validation in META/2. '      )
         CALL SIGERR ( 'META/2(CORRUPTEDINPUTSTRING)'                 )
         CALL CHKOUT ( 'M2GETD' )
         RETURN
      END IF
 
 
 
C
C     This is supposed to be an integer double precision number.
C     Parse it.
C
      CALL NPARSD ( STRING(B:E), MYDP,  ERROR, PNTER )
 
      IF ( ERROR .NE. ' ' ) THEN
         CALL CHKIN  ( 'M2GETD' )
         CALL SETMSG ( 'The item requested could not be parsed '      //
     .                 'as an integer. a number.'                     )
         CALL SIGERR ( 'META/2(CORRUPTEDNUMBER)'                      )
         CALL CHKOUT ( 'M2GETD' )
         RETURN
      END IF
 
C
C     Now do the actual assignment
C
      DP   = MYDP
 
      RETURN
      END
