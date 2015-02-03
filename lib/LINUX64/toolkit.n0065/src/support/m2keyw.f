C$Procedure      M2KEYW ( Determine whether or not a word is a keyword )
 
      LOGICAL FUNCTION M2KEYW ( WORD )
      IMPLICIT NONE
 
C$ Abstract
C
C     This function is true if the input string is a keyword in the
C     sense of META/2.
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
C     META/2 a language specification language.
C
C$ Keywords
C
C     ALPHANUMERIC
C     ASCII
C     PARSING
C     UTILITY
C     WORD
C
C$ Declarations
 
      CHARACTER*(*)         WORD
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     WORD       I   A character string word
C
C     The function is returned as .TRUE. if word is an META/2 keyword.
C
C$ Detailed_Input
C
C     WORD      is a character string that is assumed to have no
C               spaces between the first and last non-blank characters.
C
C$ Detailed_Output
C
C     M2KEYW    returns as .TRUE. if WORD is not one of the following:
C
C             '@alpha',   '@alpha(%*)',  '@calendar',    '@body',
C             '@day',
C             '@end'      '@english',    '@english(%*)', '@epoch',
C             '@int',     '@int(*:*)'    '@month',       '@month(%*)',
C             '@name',    '@name(%*)',   '@number'       '@number(*:*)',
C             '@then'     '@then(%*)',   '@time',        '@year',
C             '{',        '|',           '}'             '@unit'
C
C$ Error_Handling
C
C     None.
CC
C$ Input_Files
C
C     None.
C
C$ Output_Files
C
C     None.
C
C$ Particulars
C
C     This is a utility routine for the subroutine META2.  It
C     determines whether or not a word is a keyword in the sense
C     of the language META/2.
C
C$ Examples
C
C     WORD                                  M2KEYW
C     -------                               ------
C     @english(A*)                          .FALSE.
C     SPAM                                  .TRUE.
C     |                                     .FALSE.
C     19                                    .TRUE.
C     @bug                                  .TRUE.
C     @number                               .FALSE.
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
C-     META/2 Version 3.0.0, 23-MAR-2000 (WLT)
C
C         Extended the routine to handle the new meta-keyword @unit
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
C     Version B1.0.0, 22-MAR-1988 (WLT) (IMU)
C
C-&
 
 
 
C
C     SPICELIB functions
C
 
      INTEGER               BSRCHC
      LOGICAL               MATCHW
 
 
C
C     Local variables
C
      INTEGER               END
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               L
      INTEGER               LBRACE
      INTEGER               RBRACE
      INTEGER               BLANK
 
      CHARACTER*4           CWORD
 
      INTEGER               NQUICK
      PARAMETER           ( NQUICK = 20 )
 
      CHARACTER*4           QUICK  (NQUICK)
 
      INTEGER               CHECKS (NQUICK)
 
      INTEGER               PNTRS  (NQUICK)
 
      INTEGER               NSLOW
      PARAMETER           ( NSLOW = 25 )
 
      CHARACTER*16          SLOW   (NSLOW)
 
      LOGICAL               MATCH
 
      SAVE
C
C     We are going to look at the first four characters of the input
C     word.  If it doesn't match one of the following, then it isn't
C     a meta-2 specification word, it's a keyword.  The data in
C     this array should always be in increasing order.
C
 
 
      DATA                  QUICK  / ')',     '@alp',  '@bod',
     .                               '@cal',  '@day',
     .                               '@end',  '@eng',  '@epo',  '@int',
     .                               '@mon',  '@nam',  '@num',  '@the',
     .                               '@tim',  '@uni',  '@wor',  '@yea',
     .                               '{',     '|',     '}'    /
C
C     If after checking against the previous list we have a match,
C     then we need to do further checks to see if we have a
C     legitimate meta-2 specification word.  If we have a bracket or
C     vertical bar, we are done ( zero more checks are required ).
C     In other cases 1 or two more checks may be required.  The
C     data below tells how many further checks may be required.
C
 
 
 
      DATA                  CHECKS /  0,      2,        1,
     .                                1,      1,
     .                                1,      2,        1,       2,
     .                                2,      2,        2,       2,
     .                                1,      2,        2,       1,
     .                                0,      0,        0     /
C
C     The PNTRS array points to the slot in the SLOW check array
C     where our matching pattern templates reside for checking
C     the current input word.
C
 
 
      DATA                  PNTRS  /  0,      1,        3,
     .                                4,      5,
     .                                6,      7,        9,      10,
     .                               12,     14,       16,      18,
     .                               20,     21,       23,      25,
     .                               25,     25,       25    /
 
      DATA                  SLOW   /  '@alpha',        '@alpha(%*)',
     .                                '@body',         '@calendar',
     .                                '@day',          '@end',
     .                                '@english',      '@english(%*)',
     .                                '@epoch',        '@int',
     .                                '@int(*:*)',     '@month',
     .                                '@month(%*)',    '@name',
     .                                '@name(%*)',     '@number',
     .                                '@number(*:*)',  '@then',
     .                                '@then(%*)',     '@time',
     .                                '@unit',         '@unit(%*)',
     .                                '@word',         '@word(%*)',
     .                                '@year'                     /
 
 
 
      CWORD = WORD
      I     = BSRCHC ( CWORD, NQUICK, QUICK )
 
      IF ( I .EQ. 0 ) THEN
 
         M2KEYW = .TRUE.
         RETURN
 
      END IF
 
C
C     We only want to examine the portion of the word that preceeds
C     a parsing qualifier.  First locate the last non-blank character
C     of the word.
C
      LBRACE = ICHAR ( '['  )
      RBRACE = ICHAR ( ']'  )
      BLANK  = ICHAR ( ' '  )
      END    = LEN   ( WORD )
 
      DO WHILE ( END .GT. 1 .AND. ICHAR(WORD(END:END)) .EQ. BLANK )
         END = END - 1
      END DO
 
C
C     If the length is not at least 4 or the last character is not
C     a right brace, there is no name associated with this word.
C
      IF (      ( ICHAR(WORD(END:END)) .EQ. RBRACE )
     .    .AND. ( END                   .GE. 4      ) ) THEN
C
C        Ok. We have a chance at getting a name.  Look for
C        a left brace and if found set the name and class end.
C
         L = 2
 
         DO WHILE (L .LT. END - 1)
            IF ( ICHAR(WORD(L:L)) .EQ. LBRACE ) THEN
C
C              We've found the beginning of the name portion
C              of the word.  Record the end of the meta-2
C              word and then reset L so that we exit this loop.
C
               END    = L - 1
               L      = END
            END IF
            L = L + 1
         END DO
      END IF
 
 
      M2KEYW = .FALSE.
      K      = PNTRS(I)
      J      = 1
      MATCH  = .FALSE.
 
      DO WHILE ( (J .LE. CHECKS(I)) .AND.
     .            .NOT. MATCH              )
 
         MATCH  =  MATCHW ( WORD(1:END), SLOW(K), '*', '%' )
         M2KEYW = .NOT. MATCH
 
         K      = K + 1
         J      = J + 1
 
      END DO
 
 
      RETURN
      END
