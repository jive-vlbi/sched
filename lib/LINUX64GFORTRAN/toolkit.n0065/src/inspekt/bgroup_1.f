C$Procedure BGROUP_1 ( Determine if groups are balanced )
 
      INTEGER FUNCTION BGROUP_1 (STRING, LGR, RGR, SGR )
      IMPLICIT NONE
 
C$ Abstract
C
C     Determine the extent to which a string with grouping characters
C     or strings is balanced.
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
C     GROUPING
C
C$ Keywords
C
C     PARSING
C     SEARCH
C     UTILITY
C
C$ Declarations
 
      CHARACTER*(*)         STRING
      CHARACTER*(*)         LGR
      CHARACTER*(*)         RGR
      CHARACTER*(*)         SGR
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STRING     I   Any character string
C     LGR        I   A string that marks the start of a group
C     RGR        I   A string that marks the end   of a group
C     SGR        I   A string that marks the end of all groups.
C
C     The function returns the depth of the last character of the string
C     or minus the position of the first negatively nested character.
C
C$ Detailed_Input
C
C     STRING     Any character string.
C
C     LGR        A character or string of characters that marks the
C                start of a group. Typically this will be a left
C                parenthesis, left square bracket, or left brace.
C
C     RGR        A character or string of characters that marks the
C                end of a group. Usually this will be a right
C                parenthesis, right square bracket, or right brace.
C
C     SGR        A character or string of characters that is used as a
C                super grouper.  If SGR is a blank string, it will be
C                assumed that no super grouper symbols are in the
C                input string.
C
C$ Detailed_Output
C
C     BGROUP_1   If the depth of characters in the string never becomes
C                negative, BGROUP_1 will be returned as the depth of the
C                last character of the input string.  I.E. if the string
C                is balanced BGROUP_1 will be zero, if not balanced (and
C                the depth is not negative for any character of the
C                string) it will be the minus the number of right
C                groupers that need to be appended to the string to
C                balance it.
C
C                If the depth ever becomes negative, BGROUP_1 will be
C                returned as the the index of the first
C                character in the string of negative depth.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C      1) If the super grouper symbol is not blank and any of the
C         grouping strings are initial substrings of one of the others
C         the error 'SPICE(NONDISTINCTPAIR)' will be signalled.
C
C      2) If the super grouper symbol is blank and either of the left
C         or right groupers is an initial substring of the other,
C         the error 'SPICE(NONDISTINCTPAIR)' will be signalled.
C
C$ Particulars
C
C     One symbol begins a group, another distinct symbol ends it.
C     The super grouper symbol ends all current groups.
C
C     Associated with each character of the string is a depth number.
C     The depth of a token is determined via the following
C     proceedure.
C
C        Initially assume that each character has depth zero.
C
C        Read the string from left to right.
C
C        If a left grouper is encountered
C
C           the depth is incremented by one and all characters of the
C           left grouper are assigned this depth.
C
C        If a right grouper is encountered
C
C           the depth is decremented by one and all characters of the
C           left grouper are assigned this depth.
C
C        If a super grouper is encountered
C
C           the depth is set to zero, all characters of the super
C           grouper are given the depth zero.
C
C        Otherwise
C
C           The character is given a depth equal to the last depth.
C
C     For example, suppose that the left and right groupers are '('
C     and ')' respectively and that ']' is the super grouper symbol.
C     Then the depths of each character of the string below is given
C     immediately beneath it.
C
C              1 + 2 * ( (3+3)*4 +(2+(3*4 + 3 ] + 27
C              00000000                       000000
C                      11    11111
C                        2222     222
C                                    333333333
C
C     This routine determines the depth of the last character of the
C     input string (provided the depth never bocomes negative.
C     If the depth should become negative, he value returned is minus
C     the index of the first character of the string having negative
C     depth.  By using this routine, one can repair bad input string
C     before they are processed by the group locating routines.
C
C     There are three routines available for locating the delimited
C     groups of a string.  They are:
C
C        FGROUP_1 --- Find the first simple group of a string.
C        DGROUP_1 --- Find the deepest group of a string.
C        NGROUP_1 --- Find the kth group of depth n.
C
C     Related routines are
C
C        SGROUP_1 --- Resolve super groupers
C        BGROUP_1 --- Determine how balanced a string is.
C
C     This routine determines how balanced a string is.
C
C$ Examples
C
C     The tables below lists a collection of sample input strings and
C     output values that should be expected for BGROUP.
C
C     E X A M P L E     I N P U T S    A N D    O U T P U T S
C
C     This first example illustrates how the routine works when
C     standard grouping symbols are used.
C
C           LGR     = '('
C           RGR     = ')'
C           SGR     = ' '
C
C           STRING                                    BGROUP
C           ==================================        ======
C          'A + ( B + C )*A + ( D + E         '           1
C          '1 + 2*( 3 + 4*( 5 + 6*( 7 + 8*( 9 '           4
C          '1 + 2*( 3 + 4*( 5 + 6*( 7 )))     '           0
C          '1 + 2*) 3 + 4*( 5                 '          -7
C                 -
C
C
C
C           LGR     = '('
C           RGR     = ')'
C           SGR     = ']'
C
C           STRING                                    BGROUP
C           ==================================        ======
C          'A + ( B + C )*A + ( D + E         '           1
C          'A + ( B + C )*A + ( D + E    ]    '           0
C          '1 + 2*( 3 + 4*( 5 + 6*( 7 + 8*( 9]'           0
C          '1 + 2*( 3 + 4*( 5 + 6*( 7] )))     '        -28
C                                      -
C          '1 + 2*] 3 + 4*( 5                 '           1
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
C     Beta Version 1.1.0, 28-Dec-1994 (WLT)
C
C        Gave BGROUP_1 an initial value of zero so that the function
C        will have a value when it returns even if an error is
C        signalled.
C
C     Beta Version 1.0.0, 18-OCT-1988 (WLT) (IMU)
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
 
      INTEGER               LENGTH
 
      INTEGER               LGLEN
      INTEGER               RGLEN
      INTEGER               SGLEN
 
      INTEGER               LGOFF
      INTEGER               RGOFF
      INTEGER               SGOFF
 
      INTEGER               NEST
      INTEGER               I
 
      INTEGER               R
      INTEGER               L
      INTEGER               S
 
      INTEGER               MINLR
      INTEGER               MINRS
      INTEGER               MINSL
 
      LOGICAL               SUPER
 
C%&END_DECLARATIONS
 
 
C
C     We assume to start with that the string is balanced.
C
      BGROUP_1 = 0
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BGROUP_1' )
      END IF
 
C
C     Get the lengths of the left and right groupers as well as
C     the input string.
C
      LENGTH   = LEN ( STRING )
      LGLEN    = LEN ( LGR    )
      RGLEN    = LEN ( RGR    )
      SGLEN    = LEN ( SGR    )
 
      LGOFF  = LGLEN  - 1
      RGOFF  = RGLEN  - 1
      SGOFF  = SGLEN  - 1
 
      MINLR  = MIN ( LGLEN, RGLEN )
      MINRS  = MIN ( SGLEN, RGLEN )
      MINSL  = MIN ( SGLEN, LGLEN )
C
C     Check for dumb errors
C
      IF ( SGR .EQ. ' ' ) THEN
 
         SUPER = .FALSE.
 
         IF (      ( RGR    .EQ. LGR   (1:MINLR) )
     .        .OR. ( LGR    .EQ. RGR   (1:MINLR) ) ) THEN
 
            CALL SIGERR ( 'SPICE(NONDISTINCTPAIR)' )
            CALL CHKOUT ( 'BGROUP_1'               )
            RETURN
 
         END IF
 
      ELSE
 
         SUPER = .TRUE.
 
         IF (      ( RGR    .EQ. LGR   (1:MINLR) )
     .        .OR. ( LGR    .EQ. RGR   (1:MINLR) )
     .        .OR. ( RGR    .EQ. SGR   (1:MINRS) )
     .        .OR. ( SGR    .EQ. RGR   (1:MINRS) )
     .        .OR. ( LGR    .EQ. SGR   (1:MINSL) )
     .        .OR. ( SGR    .EQ. LGR   (1:MINSL) ) ) THEN
 
            CALL SIGERR ( 'SPICE(NONDISTINCTPAIR)' )
            CALL CHKOUT ( 'BGROUP_1'               )
            RETURN
         END IF
      END IF
 
      NEST   =  0
      I      =  1
 
      DO WHILE ( I .LE. LENGTH )
 
C
C        Figure out where the ends of the groupers might appear in
C        the input string.
C
         R = I + RGOFF
         L = I + LGOFF
         S = I + SGOFF
 
         R = MIN ( R, LENGTH )
         L = MIN ( L, LENGTH )
         S = MIN ( S, LENGTH )
 
C
C        If this is a right grouper, we might be done.
C
         IF ( STRING(I:R) .EQ. RGR ) THEN
 
 
            IF ( NEST .EQ. 0 ) THEN
               BGROUP_1 = I
               CALL CHKOUT ( 'BGROUP_1' )
               RETURN
            END IF
 
            NEST   =  NEST + 1
            I      =  R    + 1
 
         ELSE IF ( STRING(I:L) .EQ. LGR ) THEN
 
            NEST   =  NEST - 1
            I      =  L    + 1
 
         ELSE IF ( SUPER .AND. (STRING(I:S) .EQ. SGR) ) THEN
 
            NEST   =  0
            I      =  L + 1
 
         ELSE
            I     = I + 1
         END IF
 
      END DO
 
      BGROUP_1 = NEST
 
      CALL CHKOUT ( 'BGROUP_1' )
 
      RETURN
      END
 
 
 
 
