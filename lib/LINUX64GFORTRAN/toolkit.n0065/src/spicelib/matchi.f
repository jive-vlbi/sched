C$Procedure            MATCHI ( Match string against wildcard template )
 
      LOGICAL FUNCTION  MATCHI ( STRING, TEMPL, WSTR, WCHR )
 
C$ Abstract
C
C     Determine whether a string is matched by a template containing
C     wild cards.  This routine is case-insensitive.
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
C     COMPARE
C
C$ Declarations
 
      CHARACTER*(*)    STRING
      CHARACTER*(*)    TEMPL
      CHARACTER*1      WSTR
      CHARACTER*1      WCHR
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STRING     I   String to be tested.
C     TEMPL      I   Template (with wild cards) to test against STRING.
C     WSTR       I   Wild string token.
C     WCHR       I   Wild character token.
C
C     The function returns .TRUE. if STRING matches TEMPL and otherwise
C     returns .FALSE.
C
C$ Detailed_Input
C
C     STRING      is the input character string to be tested for
C                 a match against the input template. Leading and
C                 trailing blanks are ignored.
C
C     TEMPL       is the input template to be tested for a match
C                 against the input string. TEMPL may contain wild
C                 cards. Leading and trailing blanks are ignored.
C
C     WSTR        is the wild string token used in the input template.
C                 The wild string token may represent from zero to
C                 any number of characters.
C
C     WCHR        is the wild character token used in the input
C                 template. The wild character token represents
C                 exactly one character.
C
C$ Detailed_Output
C
C     The function is true when the input string matches the input
C     template, and false otherwise. The string and template match
C     whenever the template can expand (through replacement of its
C     wild cards) to become the input string.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C     MATCHI ignores leading and trailing blanks in both the string
C     and the template. All of the following are equivalent (they
C     all return TRUE).
C
C        MATCHI ( 'ALCATRAZ',     'A*Z',      '*', '%' )
C        MATCHI ( '  ALCATRAZ  ', 'A*Z',      '*', '%' )
C        MATCHI ( 'ALCATRAZ',     '  A*Z  ',  '*', '%' )
C        MATCHI ( '  ALCATRAZ  ', '  A*Z  ',  '*', '%' )
C
C     MATCHI is case-insensitive:  uppercase characters match
C     lowercase characters, and vice versa. Wild characters match
C     characters of both cases.
C
C$ Exceptions
C
C     Error free.
C
C$ Examples
C
C     Let
C
C        STRING  = '  ABCDEFGHIJKLMNOPQRSTUVWXYZ  '
C        WSTR    = '*'
C        WCHR    = '%'
C
C     Then
C
C        if TEMPL is  '*A*'        MATCHI is   T
C                     'A%D*'                     F
C                     'A%C*'                   T
C                     '%A*'                      F
C                     '%%CD*Z'                 T
C                     '%%CD'                     F
C                     'A*MN*Y*Z'               T
C                     'A*MN*Y*%Z'                F
C                     '*BCD*Z*'                T
C                     '*bdc*z*'                  F
C                     ' *bcD*Z*  '             T
C
C$ Restrictions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.3.1, 11-NOV-2005 (NJB)
C
C        Corrected example calls in header; made other minor
C        edits to header.
C
C-     SPICELIB Version 1.1.0 08-JUN-1999 (WLT)
C
C         Fixed comments in detailed output and example sections.
C
C-     SPICELIB Version 1.0.0 01-DEC-1995 (WLT)
C
C-&
 
C$ Index_Entries
C
C     match string against wildcard template
C     test whether a string matches a wildcard template
C
C-&
 
 
C
C     SPICELIB functions
C
      INTEGER               FRSTNB
      INTEGER               LASTNB
      LOGICAL               NECHR
      LOGICAL               SAMCHI
      LOGICAL               SAMCH
 
 
C
C     Local Variables
C
      INTEGER               SFIRST
      INTEGER               TFIRST
      INTEGER               SLAST
      INTEGER               TLAST
 
      INTEGER               SLEN
      INTEGER               TLEN
 
      INTEGER               SCUR
      INTEGER               TCUR
 
      INTEGER               LEFT
      INTEGER               RIGHT
 
      INTEGER               I
      INTEGER               J
 
      LOGICAL               NOSUBM
 
C
C     Give the function an initial value of .FALSE.
C
      MATCHI = .FALSE.
 
C
C     First let's get everybody's measurments.
C
      SFIRST = FRSTNB ( STRING )
      SLAST  = LASTNB ( STRING )
      TFIRST = FRSTNB ( TEMPL  )
      TLAST  = LASTNB ( TEMPL  )
 
      TLEN   = TLAST - TFIRST + 1
      SLEN   = SLAST - SFIRST + 1
 
      SCUR   = MAX ( 1, SFIRST )
      TCUR   = TFIRST
 
 
C
C     A blank template matches a blank string, and nothing else.
C
      IF ( ( TLAST .EQ. 0 ) .AND. ( SLAST .EQ. 0 ) ) THEN
 
         MATCHI = .TRUE.
         RETURN
 
      ELSE IF ( TLAST .EQ. 0 ) THEN
 
         MATCHI = .FALSE.
         RETURN
 
      END IF
 
 
C
C     The beginning of the string and template must be identical
C     up to the first occurrence of a wild string.
C
 
      DO WHILE (       ( TCUR             .LE. TLAST )
     .           .AND. ( SCUR             .LE. SLAST )
     .           .AND. ( .NOT. SAMCH( TEMPL,TCUR, WSTR,1) ) )
 
         IF ( NECHR ( TEMPL(TCUR:TCUR) ,    STRING(SCUR:SCUR) )
     .        .AND. ( TEMPL(TCUR:TCUR) .NE. WCHR              ) ) THEN
 
            MATCHI = .FALSE.
            RETURN
 
         ELSE
 
            TCUR = TCUR + 1
            SCUR = SCUR + 1
 
         END IF
 
      END DO
 
C
C     There are a three ways we could have finished the loop above
C     without hitting a wild string.
C
C     Case 1.  Both the string and template ran out of characters at
C     the same time without running into a wild string in the template.
C
      IF (       ( TCUR .GT. TLAST )
     .     .AND. ( SCUR .GT. SLAST ) ) THEN
 
         MATCHI = .TRUE.
         RETURN
 
      END IF
 
C
C     Case 2. The template ran out of characters while there were still
C     characters remaining in the in the string.  No match.
C
      IF (       ( TCUR .GT. TLAST )
     .     .AND. ( SCUR .LE. SLAST ) ) THEN
 
         MATCHI = .FALSE.
         RETURN
 
      END IF
 
C
C     Case 3. The string ran out of characters while non-wild characters
C     remain in the template.
C
C     We have to check to see if any non-wild-string characters
C     remain.  If so, we DO NOT have a match.  On the other hand if
C     only wild string characters remain we DO have a match.
C
      IF (       ( TCUR .LE. TLAST )
     .     .AND. ( SCUR .GT. SLAST ) ) THEN
 
         MATCHI = .TRUE.
 
         DO I = TCUR, TLAST
            MATCHI = MATCHI .AND. ( TEMPL(I:I) .EQ. WSTR )
         END DO
 
         RETURN
 
      END IF
 
C
C     OK. There is only one way that you can get to this point.
C     It must be the case that characters remain in both the template
C     (TCUR .LE. TLAST) and the string (SCUR .LE. SLAST).  Moreover,
C     to get out of the first loop you had to hit a wild string
C     character.  Find the first non-wild-string character in the
C     template. (If there isn't one, we have a match.)
C
 
      DO WHILE (       ( TCUR             .LE. TLAST )
     .           .AND. ( SAMCH ( TEMPL,TCUR, WSTR,1 )  ) )
 
         TCUR = TCUR + 1
 
      END DO
 
      IF ( TCUR .GT. TLAST ) THEN
 
         MATCHI = .TRUE.
         RETURN
 
      END IF
 
C
C     Still here? Ok. We have a non-wild-string character at TCUR. Call
C     this position left and look for the right end of the maximum
C     length substring of TEMPL (starting at left) that does not have
C     a wild string character.
C
      LEFT = TCUR
 
      DO WHILE (       ( TCUR             .LE.   TLAST   )
     .           .AND. ( .NOT. SAMCH(TEMPL,TCUR, WSTR,1) ) )
 
         TCUR = TCUR + 1
 
      END DO
 
      RIGHT = TCUR - 1
 
 
      DO WHILE ( LEFT .LE. TLAST )
 
C
C        First see if there is enough room left in the string to
C        match TEMPL(LEFT:RIGHT)
C
         IF ( SLAST - SCUR .LT. RIGHT - LEFT ) THEN
 
            MATCHI = .FALSE.
            RETURN
 
         END IF
 
C
C        The substring TEMPL(LEFT:RIGHT) might be the end of the
C        string.  In such a case the ends of STRING must match
C        exactly with the end of TEMPL.
C
         IF ( RIGHT .EQ. TLAST ) THEN
 
            I = SLAST
            J = TLAST
 
            DO WHILE ( J .GE. LEFT )
 
               IF (      SAMCH (TEMPL,J, WCHR,  1 )
     .              .OR. SAMCHI(TEMPL,J, STRING,I )  ) THEN
 
                  J = J - 1
                  I = I - 1
 
               ELSE
 
                  MATCHI = .FALSE.
                  RETURN
 
               END IF
 
            END DO
 
C
C           If we made it through the loop, we've got a match.
C
            MATCHI = .TRUE.
            RETURN
         ELSE
 
C
C           In this case TEMPL(LEFT:RIGHT) is in between wild string
C           characters.  Try to find a substring at or to the right
C           of SCUR in STRING that matches TEMPL(LEFT:RIGHT)
C
            NOSUBM = .TRUE.
 
            DO WHILE ( NOSUBM )
 
               I = SCUR
               J = LEFT
 
               DO WHILE (     ( J           .LE. RIGHT           )
     .                   .AND.(      SAMCHI( STRING,I, TEMPL,J )
     .                          .OR. SAMCH ( WCHR,  1, TEMPL,J ) ) )
 
                  I = I + 1
                  J = J + 1
 
               END DO
 
C
C              If J made it past RIGHT, we have a substring match
C
               IF ( J .GT. RIGHT ) THEN
 
                  SCUR   = I
                  NOSUBM = .FALSE.
 
C
C              Otherwise, try the substring starting 1 to the right
C              of where our last try began.
C
               ELSE
 
                  SCUR = SCUR + 1
 
C
C                 Make sure there's room to even attempt a match.
C
                  IF ( SLAST - SCUR .LT. RIGHT - LEFT ) THEN
 
                     MATCHI = .FALSE.
                     RETURN
 
                  END IF
 
               END IF
            END DO
 
         END IF
 
C
C        If you have reached this point there must be something left
C        in the template and that something must begin with a wild
C        string character.  Hunt for the next substring that doesn't
C        contain a wild string character.
C
         DO WHILE (       ( TCUR             .LE. TLAST )
     .              .AND.   SAMCH ( TEMPL,TCUR,  WSTR,1) )
 
            TCUR = TCUR + 1
 
         END DO
 
         IF ( TCUR .GT. TLAST ) THEN
 
C
C           All that was left was wild string characters.  We've
C           got a match.
C
            MATCHI = .TRUE.
            RETURN
 
         END IF
 
C
C        Still here? Ok. We have a non-wild-string character at TCUR.
C        Call this position left and look for the right end of the
C        maximum length substring of TEMPL (starting at left) that
C        does not have a wild string character.
C
         LEFT = TCUR
 
         DO WHILE (       ( TCUR             .LE. TLAST )
     .              .AND. ( .NOT. SAMCH( TEMPL,TCUR, WSTR,1 ) ) )
 
            TCUR = TCUR + 1
 
         END DO
 
         RIGHT = TCUR - 1
 
      END DO
 
      END
