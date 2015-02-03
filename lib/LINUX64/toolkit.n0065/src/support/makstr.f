C$Procedure      MAKSTR (Make String )
 
      SUBROUTINE MAKSTR ( PATTRN, THIS, NEXT )
 
C$ Abstract
C
C     Make a string matching a pattern.  This routine serves as an
C     umbrella routine for the two entry points FSTSTR and NXTSTR.
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
C      None.
C
C$ Keywords
C
C       Utility
C
C$ Declarations
 
      CHARACTER*(*)         PATTRN
      CHARACTER*(*)         THIS
      CHARACTER*(*)         NEXT
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      PATTRN     I   FSTSTR, NXTSTR
C      THIS       I   NXTSTR
C      NEXT       O   FSTSTR, NXSTR
C
C$ Detailed_Input
C
C     PATTRN      is a string that specifies a pattern that all strings
C                 in a sequence must match. There are several special
C                 substrings in PATTRN that must be recognized.
C
C                 1) A substring of the form '<*>' (where * is used
C                    as a variable length wildcard character) is called
C                    an expansion. The substring that occurs between
C                    the angle brackets < > is called the invisible
C                    portion of the expansion.  When the tokens of
C                    PATTRN are counted the invisible portion of the
C                    expansion is not counted.  Thus an expansion has
C                    exactly two tokens '<' and '>'  The invisible
C                    portion of the expansion must not contain
C                    any of the characters '<', '>', '{', or '}'.
C
C                 2) A substring of the form '{#-$}' where # and $
C                    stand for any chacter from the set
C                    '0', ... , '9', 'a', ... , 'z' is called a
C                    restriction.
C
C                 A pattern may consist of any collection of
C                 characters.  However, the characters '<' and
C                 '>' must always occur in balanced pairs with '<'
C                 on the left and '>' on the right. Moreover, they
C                 cannot be nested even if they are balanced. Similary
C                 '{' and '}' must always appear as a balanced pair
C                 and have exactly 3 characters between them.  The
C                 first is a lower case letter or a digit.  The second
C                 letter may be anything (usually a hyphen, colon or
C                 comma).  The third character must
C                 also be a letter between 0, ... ,9, a, b, ... , z
C                 and must occur later in the collating sequence than
C                 the first letter in the triple that occurs between
C                 '{' and '}'.
C
C                 For example the following are valid patterns
C
C                 PAT_<Value: >_{0-9}{a-z}{a-d}
C                 COUNTER{0-9}{0-9}{0-9}{0-9}
C                 COUNTER{0:9}{0,9}{a;b}
C
C                 but the following are not
C
C                 PAT_<<>>_{0-9}{a-z}{a-d}    --- Nested < >
C                 COUNTER{9-0}                --- 9 before 0
C                 PAT_{0to0}                  --- 4 characters between{}
C                 PAT_{A-Z}                   --- uppercase letters in{}
C                 PAT_{+-$}                   --- bad characters in {}
C
C                 Pattern should be viewed as consisting of a sequence
C                 of tokens.  The tokens consist of characters that
C                 are not part of an expansion or restriction
C                 restrictions and the '<' and '>' characters of
C                 any expansion.
C
C     THIS        is a string that should be incremented to get the
C                 NEXT string that matches the pattern.
C
C                 Note THIS must match the input pattern.
C
C                 The tokens of THIS are the characters upto and
C                 including the last non-blank character of THIS.
C
C                 This should have the same number of tokens as does
C                 PATTRN.
C
C                 Suppose that TOKTHS (I) is the I'th token of THIS
C                 and that TOKPAT(I) is the I'th token of PATTRN.
C
C                 If TOKPAT(I) is a restriction then TOKTHS(I) must
C                 be one of the characters belonging to the range
C                 of the restriction.
C
C                 Otherwise TOKPAT(I) and TOKTHS(I) match.
C
C                 Thus the pattern
C
C                   'XXX<value: >{0-9}{0-z}'
C
C                 Matches
C
C                   'THIS_5a'
C
C                 This kind of matching is of course a bit
C                 confusing.  It is probably more useful to
C                 have THIS take all of its tokens to be identical
C                 to the character tokens of of PATTRN and match
C                 the restriction tokens in all other cases.
C
C                 In particular, the routine FSTSTR, will take
C                 PATTRN as an input and produce the a first
C                 string in the sequence of strings that matches
C                 PATTRN by simply copying the character tokens
C                 of PATTRN to the output string and taking the
C                 lower bound of the restrictions of PATTRN
C                 to get the matching tokens for each restriction.
C
C                 See FSTSTR for a more complete discussion.
C
C$ Detailed_Output
C
C     NEXT        See the entry points FSTSTR and NXTSTR
C
C
C$ Parameters
C
C      None.
C
C$ Files
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     This is a rather immature routine that can assist with the
C     problem of constructing a sequence of character strings.
C
C     This routine was written as a support routine for the
C     SPICE program CHRISTEN and the routine NOMEN.  As such
C     it was possible to ensure that all of the detailed conditions
C     of PATTRN and THIS were met by the routines that call this.
C
C     However, this routine can prove useful in other contexts and
C     is provided so that you can easily produce a large sequence of
C     non-repeating character strings.  But  YOU MUST BE CAREFUL
C     WITH YOUR USE OF THIS ROUTINE.  Unlike most SPICE routines
C     there is no exception handling done.  If you pass in a bad PATTRN
C     or value for THIS that does not match PATTRN the result are
C     of this routine are unpredictable.  The routine will certainly
C     not diagnose the problem and can possibly cause your program
C     to crash with no diagnostics to help with finding the problem.
C
C     If you simply need to produce a sequence of strings, you
C     should probably avoid putting expansions ( substrings like
C     <something> ) in your input pattern.  These are special
C     strings that support the tasks needed by NOMEN and CHRISTEN.
C
C     Stick to simple patterns such as the one shown here:
C
C        PATTRN = 'base{0-z}{0-z}{0-z}{0-z}.tmp'
C
C        THIS   = 'base0000.tmp'
C
C     For creating file names or unique non-frequently repeating
C     strings, this will probably do the job.
C
C     Note that upper case letters are not supported in PATTRNs, this
C     is a UNIX-ish restriction (most file names are written in
C     lower case in UNIX).  This routine could be easily modified
C     to support a wider range of characters.  Or if you want all
C     uppercase characters, apply the SPICE routine UPPER to NEXT
C     when you get back from your call to NXTSTR.
C
C     Still even with all the restrictions and lack of exception
C     handling this does solve a basic problem of creating an
C     increasing sequence of character strings and saves you
C     from the task of figuring out the details (in particular
C     how to cascade up the string when you have many letters
C     to change to get to the next string).
C
C     The most common useage is to use FSTSTR to get a first string
C     in a sequence that matches PATTRN and then to call NXTSTR
C     to produce subsequent matching strings.
C
C$ Examples
C
C     See the inividiual entry points.
C
C$ Restrictions
C
C     There are lots of restrictions.  See the detailed input
C     and particulars for all the warnings.
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
C-     Support Version 1.1.0, 18-JUN-1999 (WLT)
C
C         Placed a RETURN statement before the first entry point
C         to protect against the coding error of calling the
C         subroutine MAKSTR directly.
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
C
C-    Prototype Version 1.0.0, 16-APR-1994 (WLT)
C
C
C-&
 
C
C     Spicelib functions
C
      INTEGER               RTRIM
 
C
C     Local Varialbes
C
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               MAX
      INTEGER               MIN
 
      LOGICAL               KEEP
 
      RETURN 
 
 
C$Procedure      FSTSTR ( First string matching a pattern )
 
      ENTRY FSTSTR ( PATTRN, NEXT )
 
 
C$ Abstract
C
C     Given a naming pattern, this routine produces the first
C     legal name implied by the pattern.
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
C      None.
C
C$ Keywords
C
C       UTILITY
C
C$ Declarations
C
C     IMPLICIT NONE
C     CHARACTER*(*)         PATTRN
C     CHARACTER*(*)         NEXT
C
C$ Brief_I/O
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      PATTRN     I   A pattern to use when constructing strings
C      NEXT       O   The first pattern that fits the input pattern
C
C$ Detailed_Input
C
C     PATTRN     is a pattern from which NEXT will be constructed.
C                See the discussion of PATTRN in the umbrella routine
C                for more details.
C
C$ Detailed_Output
C
C     NEXT       is the first string in the ASCII collating sequence
C                 that matches pattern.  The tokens of NEXT are the
C                 characters up to the last non-blank character.  The
C                 number of tokens in NEXT and PATTRN are the same.
C                 Moreover, the tokens of NEXT are constructed from
C                 PATTRN from the following rule:
C
C                    If TOKEN(I) is the I'th token of PATTRN and
C                    it is not a restriction (i.e. it's a single letter)
C                    then the I'th token of NEXT is TOKEN(I).
C
C                    If TOKEN(I) is the I'th token of PATTRN and it
C                    is a restriction then the I'th token of NEXT is
C                    the character of the restriction that follows
C                    the left brace '{' of the restriction.
C
C                 In particular this means that expansions are copied
C                 into NEXT as simply '<>'.
C
C$ Parameters
C
C      None.
C
C$ Files
C
C      None.
C
C$ Exceptions
C
C     1) If the output string is not long enough to hold the
C        first string that matches PATTRN the error
C        SPICE(OUTPUTTOOLONG) will be signalled.
C
C$ Particulars
C
C     This is a rather immature routine that is used by Christen for
C     generating the first string in a sequence of strings specified
C     by a naming convention.  There are plenty of things that could
C     go wrong if the input PATTRN is not well formed or if there
C     is not room in NEXT to hold the string that should be
C     constructed by this routine.  However, none of these problems
C     are checked for or diagnosed.
C
C     Nevertheless, this routine may prove useful in many contexts
C     where you need to create a sequence of names and simply want
C     to start with a pattern and let software handle the rest for
C     you.
C
C     Normal usage would be to use FSTSTR to get the first string
C     of a set specified by a string pattern and to then generate
C     the rest using the routine NXTSTR.  This can be useful in those
C     situations where you need to create a new file and don't want
C     to overwrite any existing file.
C
C     If you plan to make use of this routine in conjuction with
C     NXTSTR you should be sure to read the discussion of NXTSTR
C     that appears in the header to that routine.
C
C$ Examples
C
C     Suppose that you want to be able to create a file name
C     that can be used as a scratch area for some aspect of your
C     program.  You can use this routine in conjuction with NXTSTR
C     to generate a name of a NEW file for this purpose.
C
C        PATTRN = 'file{0-z}{0-z}{0-z}{0-z}.tmp'
C
C        CALL FSTSTR ( PATTRN, NAME )
C
C        DO WHILE ( EXISTS(NAME) )
C
C           THIS = NAME
C           CALL NXTSTR ( PATTRN, THIS, NAME )
C
C        END DO
C
C        CALL TXTOPN ( NAME, UNIT )
C
C
C$ Restrictions
C
C     There are lots of restrictions associated with PATTRN and
C     NEXT that are discussed above.  This routine doesn't perform
C     any error checking so you need to be sure that the inputs
C     are properly specified before you call this routine.
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
C-     Support Version 1.1.0, 18-JUN-1999 (WLT)
C
C         Placed a RETURN statement before the first entry point
C         to protect against the coding error of calling the
C         subroutine MAKSTR directly.
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
C
C-    Prototype Version 1.0.0, 17-APR-1994 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Get the first name in a sequence that matches a pattern
C
C-&
 
 
 
C
C     There are two things to handle:
C
C     balanced brackets: <>
C     balanced braces:   {}
C
C     We do this in one pass.
C
      NEXT  = ' '
      KEEP  = .TRUE.
      J     =  1
 
      DO I = 1, RTRIM(PATTRN)
 
         IF ( PATTRN(I:I) .EQ. '>' ) THEN
            KEEP = .TRUE.
         END IF
 
         IF ( PATTRN(I:I) .EQ. '{' ) THEN
            NEXT(J:J) = PATTRN(I+1:I+1)
            J         = J + 1
            KEEP      = .FALSE.
         END IF
 
         IF ( KEEP ) THEN
            NEXT(J:J) = PATTRN(I:I)
            J          = J + 1
         END IF
 
         IF ( PATTRN(I:I) .EQ. '<' ) THEN
            KEEP = .FALSE.
         END IF
 
         IF ( PATTRN(I:I) .EQ. '}' ) THEN
            KEEP = .TRUE.
         END IF
 
         IF ( J .GT. LEN(NEXT) ) THEN
            CALL CHKIN  ( 'FSTSTR' )
            CALL SETMSG ( 'The string provided for the first '
     .      //            'name is too short for the input '
     .      //            'pattern. ' )
            CALL SIGERR ( 'SPICE(OUTPUTTOOLONG)' )
            CALL CHKOUT ( 'FSTSTR' )
 
 
         END IF
 
      END DO
 
      RETURN
 
 
C$Procedure      NXTSTR (Next String)
 
      ENTRY NXTSTR ( PATTRN, THIS, NEXT )
 
C$ Abstract
C
C     Given a pattern for incrementing a string and a current
C     string value (that fits the pattern) produce the next
C     string in the sequence.
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
C      None.
C
C$ Keywords
C
C       UTILITY
C
C$ Declarations
C
C     IMPLICIT NONE
C
C     CHARACTER*(*)         PATTRN
C     CHARACTER*(*)         THIS
C     CHARACTER*(*)         NEXT
C
C$ Brief_I/O
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      PATTRN     I   a pattern to use to get NEXT from THIS
C      THIS       I   is a string that matches PATTRN
C      NEXT       O   is the first string after THIS to match PATTRN
C
C$ Detailed_Input
C
C     PATTRN      is a string that specifies a pattern that all strings
C                 in a sequence must match. See the discussion of
C                 PATTRN in the umbrella routine for more details.
C
C     THIS        is a string that should be incremented to get the
C                 NEXT string that matches the pattern.
C
C                 Note THIS must match the input pattern.
C
C                 The tokens of THIS are the characters upto and
C                 including the last non-blank character of THIS.
C
C                 This should have the same number of tokens as does
C                 PATTRN.
C
C                 Suppose that TOKTHS (I) is the I'th token of THIS
C                 and that TOKPAT(I) is the I'th token of PATTRN.
C
C                 If TOKPAT(I) is a restriction then TOKTHS(I) must
C                 be one of the characters belonging to the range
C                 of the restriction.
C
C                 Otherwise TOKPAT(I) and TOKTHS(I) match.
C
C                 Thus the pattern
C
C                   'XXX<value: >{0-9}{0-z}'
C
C                 Matches
C
C                   'THIS_5a'
C
C                 This kind of matching is of course a bit
C                 confusing.  It is probably more useful to
C                 have THIS take all of its tokens to be identical
C                 to the character tokens of of PATTRN and match
C                 the restriction tokens in all other cases.
C
C                 In particular, the routine FSTSTR, will take
C                 PATTRN as an input and produce the a first
C                 string in the sequence of strings that matches
C                 PATTRN by simply copying the character tokens
C                 of PATTRN to the output string and taking the
C                 lower bound of the restrictions of PATTRN
C                 to get the matching tokens for each restriction.
C
C                 See FSTSTR for a more complete discussion.
C
C$ Detailed_Output
C
C     NEXT        is the next string in the ascii collating
C                 sequence that matches PATTRN and is equal to
C                 THIS on the non-restriction matching letters
C                 of THIS.  There is one exception to this rule.
C                 If there is no such string, (i.e. THIS is the
C                 last string that can be produced that matches
C                 PATTRN) then NEXT will be the first string
C                 that matches PATTRN and is equal to THIS on the
C                 non-restriction matching letters of THIS.
C
C                 If PATTRN contains no restrictions, then NEXT
C                 will equal THIS.
C
C$ Parameters
C
C      None.
C
C$ Files
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     This is a rather immature routine that can assist with the
C     problem of constructing a sequence of character strings.
C
C     This routine was written as a support routine for the
C     SPICE program CHRISTEN and the routine NOMEN.  As such
C     it was possible to ensure that all of the detailed conditions
C     of PATTRN and THIS were met by the routines that call this.
C
C     However, this routine can prove useful in other contexts and
C     is provided so that you can easily produce a large sequence of
C     non-repeating character strings.  But  YOU MUST BE CAREFUL
C     WITH YOUR USE OF THIS ROUTINE.  Unlike most SPICE routines
C     there is no exception handling done.  If you pass in a bad PATTRN
C     or value for THIS that does not match PATTRN the result are
C     of this routine are unpredictable.  The routine will certainly
C     not diagnose the problem and can possibly cause your program
C     to crash with no diagnostics to help with finding the problem.
C
C     If you simply need to produce a sequence of strings, you
C     should probably avoid putting expansions ( substrings like
C     <something> ) in your input pattern.  These are special
C     strings that support the tasks needed by NOMEN and CHRISTEN.
C
C     Stick to simple patterns such as the one shown here:
C
C        PATTRN = 'base{0-z}{0-z}{0-z}{0-z}.tmp'
C
C        THIS   = 'base0000.tmp'
C
C     For creating file names or unique non-frequently repeating
C     strings, this will probably do the job.
C
C     Note that upper case letters are not supported in PATTRNs, this
C     is a UNIX-ish restriction (most file names are written in
C     lower case in UNIX).  This routine could be easily modified
C     to support a wider range of characters.  Or if you want all
C     uppercase characters, apply the SPICE routine UPPER to NEXT
C     when you get back from your call to NXTSTR.
C
C     Still even with all the restrictions and lack of exception
C     handling this does solve a basic problem of creating an
C     increasing sequence of character strings and saves you
C     from the task of figuring out the details (in particular
C     how to cascade up the string when you have many letters
C     to change to get to the next string).
C
C$ Examples
C
C     Suppose you wanted to create the sequence of strings that
C     give the times on a 24 hour clock.  I.e 00:00:00, 00:00:01, ...
C     23:59:59.  This routine is ideally suited to this task.
C
C        PATTRN = {0-2}{0-9}:{0-5}{0-9}:{0-5}{0-9}
C        START  = '29:59:59'
C        LAST   = '23:59:59'
C
C        THIS = START
C
C        DO WHILE ( NEXT .NE. LAST )
C
C           CALL NXTSTR ( PATTRN, THIS, NEXT )
C           WRITE (*,*) NEXT
C
C           THIS = NEXT
C
C        END DO
C
C
C     The output of the routine would be:
C
C        00:00:00
C        00:00:01
C        00:00:02
C
C           .
C           .
C           .
C
C        23:59:57
C        23:59:58
C        23:59:59
C
C
C$ Restrictions
C
C     There are lots of restrictions.  See the detailed input
C     and particulars for all the warnings.
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
C-     Support Version 1.1.0, 18-JUN-1999 (WLT)
C
C         Placed a RETURN statement before the first entry point
C         to protect against the coding error of calling the
C         subroutine MAKSTR directly.
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
C
C-    Prototype Version 1.0.0, 16-APR-1994 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Construct a non-repeating increasing sequence of strings
C
C-&
 
 
C
C     First copy THIS into NEXT and find the ends of PATTRN and NEXT.
C
      NEXT = THIS
      J    = RTRIM ( PATTRN )
      I    = RTRIM ( NEXT   )
C
C     We work backwards from the right end of the string.
C
      DO WHILE ( J .GT. 0 )
C
C        If the current character is a right brace we are going
C        to assume we are at the end of a restriction token.  Use
C        the range of the restriction and the current character
C        of NEXT to determine the "next" character and whether or
C        not we can quit now.
C
         IF ( PATTRN(J:J) .EQ. '}' ) THEN
 
            MAX  = ICHAR(PATTRN(J-1:J-1))
            MIN  = ICHAR(PATTRN(J-3:J-3))
            K    = ICHAR(NEXT  (I  :I  )) + 1
 
            IF ( K .GT. MAX ) THEN
C
C              Roll over the characters, We aren't done we
C              need to keep stepping back through the string
C
               NEXT(I:I) = CHAR(MIN)
 
            ELSE IF (       ( K .GT. ICHAR('9') )
     .                .AND. ( K .LT. ICHAR('a') ) ) THEN
C
C              By convention, the first character following '9' is 'a'.
C              Since we don't need to "roll over" this character we
C              are done at this point.
C
               NEXT(I:I) = 'a'
               RETURN
 
            ELSE
C
C              We didn't need to roll over the character so we just
C              put in the new one and we can quit now.
C
               NEXT(I:I) = CHAR(K)
               RETURN
 
            END IF
C
C           perform the arithmetic needed if we had to roll over the
C           character.
C
            J = J - 5
            I = I - 1
C
C        If the character is '>' we assume we are at the right end
C        of an expansion.
C
         ELSE IF ( PATTRN(J:J) .EQ. '>' ) THEN
C
C           Skip over the invisible portion of the expansion.
C
            DO WHILE ( PATTRN(J:J) .NE. '<' )
               J = J - 1
            END DO
 
            I = I - 1
 
         ELSE
C
C           Nothing to do, just back up to the character to the
C           left of the current character.
C
            J = J - 1
            I = I - 1
 
         END IF
 
      END DO
 
      RETURN
      END
 
 
 
 
 
 
 
