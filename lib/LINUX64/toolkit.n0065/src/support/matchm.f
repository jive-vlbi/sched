C$Procedure MATCHM ( Match string against multiple wildcard templates )
 
      LOGICAL FUNCTION MATCHM ( STRING, TEMPL, WSTR,
     .                                         WCHR,
     .                                         NOTCHR,
     .                                         ORCHR )
      IMPLICIT NONE
 
C$ Abstract
C
C      Determines whether or not a string matches any of a
C      collection of templates containing wildcard characters.
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
C      None
C
C$ Keywords
C
C      SEARCH
C      UTILITY
C
C$ Declarations
 
      CHARACTER*(*)         STRING
      CHARACTER*(*)         TEMPL
      CHARACTER*1           WSTR
      CHARACTER*1           WCHR
      CHARACTER*1           NOTCHR
      CHARACTER*1           ORCHR
 
C$ Brief_I/O
C
C      Variable  I/O  Description
C      --------  ---  --------------------------------------------------
C      STRING     I   String to be matched against templates.
C      TEMPL      I   Collection of templates.
C      WSTR       I   Wild string: represents any number of characters.
C      WCHR       I   Wild character: represents exactly one character.
C      NOTCHR     I   NOT character: negates one or more templates.
C      ORCHR      I   OR character: separates individual templates.
C
C$ Detailed_Input
C
C      STRING     is a character string to be checked for a match
C                 against the specified collection of templates.
C                 Leading and trailing blanks are ignored.
C
C      TEMPL      is a collection of individual templates to be
C                 compared against the specified string. Leading
C                 and trailing blanks are ignored. An empty (blank)
C                 template collection matches only an empty (blank)
C                 string.
C
C      WSTR       is the wild string token used in the templates.
C                 It represents from zero to any number of characters.
C                 Spaces may not be used as wild strings.
C
C      WCHR       is the wild character token used in the templates.
C                 It represents exactly one character. Spaces may not
C                 be used as wild characters.
C
C      NOTCHR     is the NOT character used in the templates.
C                 When it appears at the beginning of a template,
C                 it negates the template: that is, a string matches
C                 the negated template if it does not match the
C                 template itself. When it appears after the first
C                 character in a template, it is treated as an
C                 ordinary character. Spaces between a not character
C                 and the rest of a template are ignored.
C
C                 In addition, the NOT character may be used to negate
C                 the entire collection of templates by placing it by
C                 itself at the head of the collection.
C
C                 Spaces may not be used as NOT characters.
C
C      ORCHR      is the OR character used to separate individual
C                 templates in the collection. Spaces adjacent to
C                 the OR character are ignored. Consecutive OR
C                 characters separated only by zero or more spaces
C                 are considered to delimit a single blank template.
C
C                 Spaces may not be used as OR characters.
C
C$ Detailed_Output
C
C      The function is TRUE whenever the string matches the collection
C      of templates, and is FALSE otherwise.
C
C$ Exceptions
C
C      1) If the four special characters are not distinct, the error
C         SPICE(AMBIGTEMPL) is signalled.
C
C      2) If any of the four special characters is a space, the error
C         SPICE(ILLEGTEMPL) is signalled.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C      MATCHM is an extension of MATCHI, which matches a string against
C      a single template. The major differences are the addition of the
C      NOT character, and the ability to match against combinations of
C      individual templates.
C
C      Like MATCHI, MATCHM is case-insensitive. Uppercase templates
C      match lowercase strings, and vice versa.
C
C      In the following discussion, we will assume that the four
C      special characters are defined as follows.
C
C            WCHR    = '%'
C            WSTR    = '*'
C            NOTCHR  = '~'
C            ORCHR   = '|'
C
C
C      1. Normal Templates
C      -------------------
C
C      A normal individual template contains some combination of
C      ordinary characters, wild characters, and wild strings.
C      The rules governing these templates are identical to those
C      used by MATCHI.
C
C
C      2. Negated Templates
C      --------------------
C
C      Any normal individual template may be negated by prefixing
C      it with the NOT character. The NOT character, when embedded
C      within either a normal or a negated template, is treated as
C      an ordinary character. For example, the template
C
C         '~*WN%.FOR'
C
C      is negated, and matches any string that does NOT match the
C      normal template
C
C         '*WN%.FOR'
C
C      while the template
C
C         'Dr.~Sm%th*'
C
C      is not negated. In particular, double negations are not
C      recognized. That is, the template
C
C         '~~X*'
C
C      means "not like ~X*".
C
C      The NOT character, when it appears by itself,
C
C         '~'
C
C      is equivalent to the template
C
C         '~*'
C
C      which does not match any string.
C
C
C      3. Combining Templates
C      ----------------------
C
C      Frequently, you will wish to determine whether a string matches
C      any of a number of individual templates: for example, whether a
C      file name matches any of the templates
C
C         '*.FOR'
C         '*.F77'
C         '*.INC'
C
C      The individual templates can be collected together into a
C      single string, separated by the OR character,
C
C         '*.FOR  |  *.F77  |  *.INC'
C
C      (Spaces adjacent to the separators are ignored. That is, the
C      collection
C
C         '*.FOR|*.F77|*.INC'
C
C      is messier than, but equivalent to, the previous collection.)
C
C      Note that conssecutive OR characters separated by zero or
C      more blanks are considered to delimit a blank template.
C      Thus, the following, which match a blank string, are all
C      equivalent
C
C         '*.FOR || *.F77'
C         '*.FOR | | *.F77'
C         '*.FOR || | *.F77'
C         '*.FOR |||||| *.F77'
C
C
C      4. Combining Negated Templates
C      ------------------------------
C
C      Both normal and negated templates may be combined using the
C      OR character. However, negated templates should be combined
C      with great care. Recalling that the logical expression
C
C         ( ~A  |  ~B  |  ~C )
C
C      is equivalent to the expression
C
C         ~ ( A  &  B  &  C )
C
C      convince yourself that the collection
C
C         '~X* | ~Y*'
C
C      meaning "not like X* or not like Y*", really means "not like
C      both X* and Y*", and matches EVERY string. This is not to say
C      that such collections do not have their uses. Combinations
C      of negated templates are used to find strings for which
C      combinations of constraints are not met simultaneously.
C      For example, the collection
C
C         '~[* | ~*]'
C
C      ("does not begin with a left bracket, or does not end with
C      a right bracket", or "does not both begin with a left bracket
C      and end with a right bracket") may be used to detect strings
C      which cannot be VMS directory specifications.
C
C
C      5. Negating Combined Templates
C      ------------------------------
C
C      It is easy to mistakenly expect a combination of negated
C      templates to act like the negation of a combination of
C      templates, but they are very different things. Continuing
C      our example of Section 3, we may wish to know which file
C      names do NOT match any of our templates. Clearly
C
C         '~*.FOR | ~*.F77 | ~*.INC'
C
C      will not do the trick, as it matches every possible file name.
C      We need instead to group the individual templates under a single
C      negation:
C
C         '~( *.FOR | *.F77 | *.INC )'
C
C      However, this grouping is not indicated with parentheses,
C      but rather by placing a lone NOT character at the head of
C      the collection,
C
C         '~ | *.FOR | *.F77 | *.INC'
C
C      This syntax, while not immediately obvious, has at least
C      two advantages. First, it does not require any new special
C      characters. Second, it makes adding new individual templates
C      to the end of the list a trivial operation.
C
C
C      6. Advanced Topics
C      ------------------
C
C      The final level in the construction of template collections
C      involves the combination of normal and negated templates.
C      Consider the templates 'A*' and '*.FOR'. The set of strings
C      matching the collection
C
C         'A* | ~*.FOR'
C
C      ("begins with A or is not like *.FOR") is just the UNION
C      of the sets of the strings matching the individual templates.
C      This is true for any set of templates, negated or normal.
C
C      But there's more. De Morgan's Laws tell us that the complement
C      (negation) of a union of sets (templates) is the same as the
C      intersection of the complements of the sets. Thus, by negating
C      the original templates, and by negating the collection of the
C      negated templates, we end up with
C
C          '~ | ~A* | *.FOR'
C
C      meaning "not (does not begin with A or is like *.FOR)".
C      But this means "both begins with A and is not like *.FOR".
C      So the set of strings matching the collection is just the
C      INTERSECTION of the sets of strings matching the original
C      templates.
C
C$ Examples
C
C      The following examples are grouped according to the discussion
C      of the Particulars section. The nominal values of the special
C      characters are the same, namely
C
C         WCHR    = '%'
C         WSTR    = '*'
C         NOTCHR  = '~'
C         ORCHR   = '|'
C
C
C      1. Normal Templates
C      -------------------
C
C      Consider the following string
C
C         '  ABCDEFGHIJKLMNOPQRSTUVWXYZ '
C
C      and the following templates.
C
C         Template         Matches STRING?
C         ---------------  ---------------
C         '*A*'            Yes
C         'A%D*'           No
C         'A%C*'           Yes
C         '%A*'            No
C         ' A*   '         Yes
C
C         '%%CD*Z'         Yes
C         '%%CD'           No
C         'A*MN*Y*Z'       Yes
C         'A*MN*Y%Z'       No
C         '*BCD*Z*'        Yes
C         '*bcd*z*'        Yes
C
C
C      2. Negated Templates
C      --------------------
C
C      Consider the same string, and the following templates.
C
C         Template         Matches STRING?
C         ---------------  ---------------
C         '~%B*D'          Yes
C         '~%B*D*'         No
C         '~ABC'           Yes
C         '~ABC*'          No
C         '~~B*'           Yes
C
C      Note that in the final example, the second '~' is treated not as
C      a second negation but as an ordinary character.
C
C
C      3. Combining Templates
C      ----------------------
C
C      Consider the following strings and templates.
C
C           String          Template             Matches?
C           --------------  -------------------  --------
C           AKRON           *A*|*B*              Yes
C           BELOIT          *B*|*I*              Yes
C           CHAMPAGNE       *B*|*I*              No
C
C
C      4. Combining Negated Templates
C      ------------------------------
C
C      Consider the following strings and templates.
C
C           String          Template             Matches?
C           --------------  -------------------  --------
C           SEQUIOA         ~*A*|~*E*|~*I*       No
C           SAINT PAUL      ~*A*|~*E*|~*I*       Yes
C           HOUSTON         ~*A*|~*E*|~*I*       Yes
C
C
C      5. Negating Combined Templates
C      ------------------------------
C
C      Consider the following strings and templates.
C
C           String          Template             Matches?
C           --------------  -------------------  --------
C           DETROIT         ~|B*|D*              No
C           EUGENE          ~|B*|D*              Yes
C           FAIRBANKS       ~|*A*|*I*|*O*|*U*    No
C           GREENBELT       ~|*A*|*I*|*O*|*U*    Yes
C
C$ Restrictions
C
C      None.
C
C$ Parameters
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
C-     SUPPORT Version 2.3.0, 10-MAY-2006 (EDW)
C
C         Added logic to prevent the evaluation of TEMPL(BEG:BEG)
C         if BEG exceeds the length of TEMPL. Functionally, the
C         evaluation had no effect on MATCHM's output, but the ifort
C         F95 compiler flagged the evaluation as an array 
C         overrun error. This occurred because given:
C
C             A .AND. B
C
C         ifort evaluates A then B then performs the logical
C         comparison.
C
C         Edited header to match expected SPICE format.
C
C-     META/2 Configured Version 2.2.0, 28-DEC-1994 (WLT)
C
C         An initial value is given to MATCHM so that it will
C         have a value even if return mode is in effect.
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
C      Version B 1.0.0, 31-MAR-1988
C
C-&
 
C$ Index_Entries
C
C     string match to templates
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               FRSTNB
      INTEGER               QLSTNB
      LOGICAL               MATCHI
      INTEGER               UPTO
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               BEG
      INTEGER               END
      INTEGER               B
      INTEGER               E
 
      LOGICAL               MATCH
      LOGICAL               NEGATE
      LOGICAL               LOOP
 
C
C     Give the function an initial value.
C
      MATCHM = .FALSE.
 
C
C     Standard SPICE error handling
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'MATCHM' )
      END IF
 
C
C     Reject bad inputs.
C
      IF (      WSTR   .EQ. ' '
     .     .OR. WCHR   .EQ. ' '
     .     .OR. NOTCHR .EQ. ' '
     .     .OR. ORCHR  .EQ. ' ' ) THEN
 
         CALL SIGERR ( 'SPICE(ILLEGTEMPL)' )
         CALL CHKOUT ( 'MATCHM' )
         RETURN
 
      ELSE IF (      WSTR   .EQ. WCHR
     .          .OR. WSTR   .EQ. NOTCHR
     .          .OR. WSTR   .EQ. ORCHR
     .          .OR. WCHR   .EQ. NOTCHR
     .          .OR. WCHR   .EQ. ORCHR
     .          .OR. NOTCHR .EQ. ORCHR   ) THEN
 
         CALL SIGERR ( 'SPICE(AMBIGTEMPL)' )
         CALL CHKOUT ( 'MATCHM' )
         RETURN
 
      END IF
 
C
C     Ignore leading and trailing spaces in the collection.
C
      BEG  = FRSTNB ( TEMPL  )
      END  = QLSTNB ( TEMPL  )
 
C
C     A blank collection matches ONLY a blank string.
C
      IF ( BEG .EQ. 0 ) THEN
         MATCHM = ( STRING .EQ. ' ' )
         CALL CHKOUT ( 'MATCHM' )
         RETURN
      END IF
 
C
C     If the first template is the NOT character, the entire collection
C     is negated, and we can begin with the next template. Otherwise,
C     just start at the beginning again.
C
      B = BEG
      E = UPTO ( TEMPL(1:END), ORCHR, B )
  
      IF( E .GE. LEN(TEMPL) ) THEN
         NEGATE = .FALSE.
         BEG    = B      
      ELSE IF ( TEMPL(B:E) .EQ. NOTCHR .AND. 
     .          TEMPL(E+1:E+1) .EQ. ORCHR) THEN
         NEGATE = .TRUE.
         BEG    = E + 2
      ELSE
         NEGATE = .FALSE.
         BEG    = B
      END IF
 
C
C     Grab one template at a time, comparing them against the string
C     until a match has occured or until no templates remain.
C
      MATCH = .FALSE.
 
      DO WHILE ( ( BEG .LE. END ) .AND. (.NOT. MATCH ) )
 
         B = BEG
         E = UPTO ( TEMPL(1:END), ORCHR, B )
 
C
C        If we started on an OR character, then either we are
C        at the beginning of a string that starts with one,
C        or we just passed one and found another either next to
C        it, or separated by nothing but spaces. By convention,
C        either case is interpreted as a blank template.
C
         IF ( TEMPL(B:B) .EQ. ORCHR ) THEN
 
            MATCH  = ( STRING .EQ. ' ' )
            BEG    = BEG + 1
 
C
C        If this is a negated template, negate the results.
C        Remember that a NOT character by itself does not
C        matches anything.
C
         ELSE IF ( TEMPL(B:B) .EQ. NOTCHR ) THEN
 
            IF ( TEMPL(B:E) .EQ. NOTCHR ) THEN
               MATCH = .FALSE.
            ELSE
               MATCH = .NOT. MATCHI ( STRING, TEMPL(B+1:E), WSTR, WCHR )
            END IF
 
            BEG    = E + 2
 
C
C        Or a normal one?
C
         ELSE

            MATCH  = MATCHI ( STRING, TEMPL(B:E), WSTR, WCHR )
            BEG    = E + 2
 
         END IF
 
C
C        Skip any blanks before the next template.
C        The logic ensures no evaluation of TEMPL(BEG:BEG)
C        if BEG > LEN(TEMPL).
C
         LOOP = BEG .LT. END
         IF( LOOP ) THEN
            LOOP = LOOP .AND.  TEMPL(BEG:BEG) .EQ. ' '
         END IF
         
         DO WHILE ( LOOP )
            BEG = BEG + 1

            IF( BEG .GE. END ) THEN
               LOOP = .FALSE.
            ELSE IF ( TEMPL(BEG:BEG) .NE. ' ' ) THEN
                LOOP = .FALSE.
            ELSE
                LOOP = .TRUE.
            END IF

         END DO
 
      END DO
 
C
C     It doesn't happen often, but occasionally a template ends with
C     the OR character. This implies a blank template at the end of
C     the collection.
C
      IF ( TEMPL(END:END) .EQ. ORCHR ) THEN
         IF ( .NOT. MATCH ) THEN
            MATCH = ( STRING .EQ. ' ' )
         END IF
      END IF
 
C
C     Negate the results, if appropriate.
C
      IF ( NEGATE ) THEN
         MATCHM = .NOT. MATCH
      ELSE
         MATCHM =       MATCH
      END IF
 
      CALL CHKOUT ( 'MATCHM' )
      RETURN
      END
