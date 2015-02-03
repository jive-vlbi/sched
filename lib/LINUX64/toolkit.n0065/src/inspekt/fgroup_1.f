C$Procedure FGROUP_1 ( First simple group of a string )
 
      SUBROUTINE FGROUP_1 ( STRING, LGR, RGR, BEG, END, DEPTH )
      IMPLICIT NONE
 
C$ Abstract
C
C     Return the endpoints of the first simple group of a string.
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
      INTEGER               BEG
      INTEGER               END
      INTEGER               DEPTH
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STRING     I   Any character string
C     LGR        I   A string that marks the start of a group
C     RGR        I   A string that marks the end   of a group
C     BEG        O   Index of the left  character of first full group
C     END        O   Index of the right character of first full group
C     DEPTH      O   Nesting depth of the first full group
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
C$ Detailed_Output
C
C     BEG        Index of the left character of the first simple group.
C
C     END        Index of the last character of the first simple group.
C
C     DEPTH      Integer giving the nesting depth of the simple group
C                delimited by BEG and END.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If the input string is not balanced (unpaired groupers),
C        prior to the detection of the first simple group,
C        the error 'SPICE(UNBALANCEDGROUP)' will be signalled.
C        The values of BEG, END and DEPTH will be set to zero.
C
C     2) If no left or right groupers occur in the string, BEG and END
C        will point to the first and last characters of the string
C        respectively. (The last character has index equal to the
C        declared length of the string.)  DEPTH will be set to zero.
C
C     3) If either grouper is an introductory substring of the other
C        the error 'SPICE(NONDISTINCTPAIR)' will be signalled.
C        The values of BEG, END and DEPTH will be set to zero.
C
C
C$ Particulars
C
C     One symbol begins a group, another distinct symbol ends it.
C
C     Associated with each character of the string is a depth number.
C     The depth of a character is determined via the following
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
C           right grouper are assigned this depth.
C
C        Otherwise
C
C           The character is given a depth equal to the last depth.
C
C     For example, suppose that the groupers are '(' and ')'.  Then
C     the depths of each character of the string below is given
C     immediately beneath it.
C
C              1 + 2 * ( (3+3)*4 +(2+(3*4 + 3) ) ) + 27
C              00000000                          000000
C                      11    11111             11
C                        2222     222        22
C                                    33333333
C
C     The group of depth zero is simply the entire string. A group of
C     depth n begins at left grouper of depth n and ends on the
C     first right grouper of depth n-1. Taking the above example
C     again the groups of depth 2 are underlined.
C
C              1 + 2 * ( (3+3)*4 +(2+(3*4 + 3) ) ) + 27
C                        -----    --------------
C
C     A simple group, is a group that contains no groups of greater
C     depth, i.e. a substring that begins with a left grouper,
C     ends with a right grouper, and contains neither type of
C     grouper between them.  The simple groups of our example
C     are underlined below.
C
C              1 + 2 * ( (3+3)*4 +(2+(3*4 + 3) ) ) + 27
C                        -----       ---------
C
C     There are three routines available for locating the
C     groups of a string.  They are:
C
C        FGROUP --- Find the first simple group of a string.
C        DGROUP --- Find the deepest group of a string.
C        NGROUP --- Find the kth group of depth n.
C
C     Related routines are
C
C        SGROUP --- Resolve super groupers
C        BGROUP --- Determine how balanced a group is.
C
C     This routine finds the endpoints of the first simple group of
C     a string and its depth.
C
C$ Examples
C
C     The tables below lists a collection of sample input strings.
C     The substring STRING(BEG:END) is underlined.  The depth that
C     would be return for the nesting level is listed under the column
C     DEPTH.
C
C     E X A M P L E     I N P U T S    A N D    O U T P U T S
C
C     This first example illustrates how the routine works when
C     standard grouping symbols are used.
C
C           LGR     = '('
C           RGR     = ')'
C
C           STRING                                    DEPTH
C           ==============================            =====
C
C          '( ( 2 + 4 ) * 7 ) + 19        '             2
C             ---------
C
C          '.NOT. ( ( A .OR B ) .AND. C ) '             2
C                   -----------
C
C          '( ( (( ((( X + Y )            '             7
C                    ---------
C
C          'THIS HAS NO DELIMITERS        '             0
C           ------------------------------
C
C          'THE MAN ( JIM ) WENT HOME     '             1
C                   -------
C
C          '( 12*[3 + 5] )                '             1
C           --------------
C
C
C
C     The following example illustrate the need for care when choosing
C     left and right groupers of more than one character.
C
C
C           LGR    = '\beg'
C           RGR    = '\end'
C
C           STRING                                    DEPTH
C           ==============================            =====
C
C          'Data: \begin 1 2  3 4 \enddata'              1
C                 --------------------
C
C          '\begin \beg time \endit \end  '              2
C                  --------------
C
C
C
C     Finally the next example shows that you need to be careful
C     about passing trailing blanks attached to the right and left
C     groupers.
C
C
C           LGR    = ' [ '
C           RGR    = '] '
C
C           STRING                                    DEPTH
C           ==============================            =====
C
C          'A + [B+7]*32                  '             0
C           ------------------------------
C
C          'A + [ B + 7 ]*32              '       results in error
C                                                 SPICE(UNBALANCEDGROUP)
C
C          'A + [ B + 7] *32              '             1
C              ----------
C
C
C     ===============================================================
C
C
C     A    C O D E     E X A M P L E
C
C
C     The following loop shows how one might use this routine to
C     parse an arithmetic expression.
C
C        See if the string is BALANCED
C
C        DO WHILE ( BGROUP( STRING, LGR, RGR, SGR ) .NE. 0 )
C
C           Attempt to balance the string.
C
C        END DO
C
C        CALL FGROUP ( STRING, '(', ')', BEG, END, LEVEL )
C
C        DO WHILE ( LEVEL .GT. 0 )
C
C           Simplyfy the expression from BEG to END
C
C           Insert the simplified expression in place of the previous
C           one in STRING.
C
C           CALL FGROUP ( STRING, '(', ')', BEG, END, LEVEL )
C
C        END DO
C
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
C     Beta Version 1.0.0, 24-OCT-1988 (WLT) (IMU)
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
      INTEGER               LGOFF
      INTEGER               RGOFF
 
      INTEGER               NEST
      INTEGER               I
 
      INTEGER               R
      INTEGER               L
      INTEGER               MINLEN
 
C%&END_DECLARATIONS
 
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'FGROUP_1' )
      END IF
 
C
C     Get the lengths of the left and right groupers as well as
C     the input string.
C
 
      LENGTH = LEN ( STRING )
      LGLEN  = LEN ( LGR    )
      RGLEN  = LEN ( RGR    )
 
      LGOFF  = LGLEN  - 1
      RGOFF  = RGLEN  - 1
 
 
      MINLEN = MIN ( LGLEN, RGLEN )
 
C
C     Check for dumb errors
C
      IF (      ( RGR    .EQ. LGR(1:MINLEN) )
     .     .OR. ( LGR    .EQ. RGR(1:MINLEN) ) ) THEN
 
         CALL SIGERR ( 'SPICE(NONDISTINCTPAIR)' )
         CALL CHKOUT ( 'FGROUP_1'               )
         RETURN
 
      END IF
 
C
C     Now we have the expected case.  Initialize the items to be used
C     for the search loop.
C
      NEST   =  0
      BEG    =  1
      END    =  LENGTH
      I      =  1
 
      DO WHILE ( I .LE. LENGTH )
 
 
C
C        Figure out where the ends of the groupers might appear in
C        the input string.
C
         R = I + RGOFF
         L = I + LGOFF
 
         R = MIN ( R, LENGTH )
         L = MIN ( L, LENGTH )
 
C
C        If this is a right grouper, we are done one way or the other.
C
         IF ( STRING(I:R) .EQ. RGR ) THEN
 
            IF ( NEST .GT. 0 ) THEN
               END   = R
               DEPTH = NEST
            ELSE
               BEG   = 0
               END   = 0
               DEPTH = 0
 
               CALL SIGERR ( 'SPICE(UNBALANCEDGROUP)' )
 
            END IF
 
            CALL CHKOUT ( 'FGROUP_1' )
            RETURN
 
         ELSE IF ( STRING(I:L) .EQ. LGR ) THEN
 
            NEST   =  NEST + 1
            BEG    =  I
            I      =  L + 1
 
         ELSE
 
            I     = I + 1
 
         END IF
 
      END DO
 
C
C     If the routine makes it this far, we had better have a string
C     with no groupers, i.e. current nesting zero.  Otherwise we
C     have an error.
C
      IF ( NEST .NE. 0 ) THEN
 
         BEG   = 0
         END   = 0
         DEPTH = 0
 
         CALL SIGERR ( 'SPICE(UNBALANCEDGROUP)' )
 
      ELSE
 
         DEPTH = 0
         BEG   = 1
         END   = LENGTH
 
      END IF
 
 
      CALL CHKOUT ( 'FGROUP_1' )
      RETURN
 
      END
 
 
 
 
