C$Procedure      M2TRAN ( See if a word has a restriction template )
 
      SUBROUTINE M2TRAN ( STRING, BEG, END, BASE, KEY, TEMP )
      IMPLICIT NONE
 
C$ Abstract
C
C     Determine a META-WORD class and whether or not a word ends
C     with a substring of the (%*).  If it ends with such a substring
C     return pointers to the left and right parentheses.
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
C     PARSING
C     UTILITY
C     WORD
C
C$ Declarations
 
      CHARACTER*(*)         STRING
      INTEGER               BEG
      INTEGER               END
      CHARACTER*(*)         BASE
      LOGICAL               KEY
      LOGICAL               TEMP
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STRING     I   A META/2 language statement specification.
C     BEG       I/O  The beginning of a word in STRING
C     END       I/O  The end of a word in STRING
C     BASE       O   Portion of the word preceeding the template.
C     KEY        O   .TRUE. if the the substring is a keyword.
C     TEMP       O   .TRUE. if a restriction template is present.
C
C$ Detailed_Input
C
C     STRING(BEG:END)  is a META/2 word that potentially ends with a
C                      substring of the form (%*) where '%' and '*'
C                      stand for the wildstring and wildcharacter
C                      symbols.
C
C$ Detailed_Output
C
C     BEG        is the index of the first character of the restriction
C                template ( the left parenthesis )
C                first parenthesis '(' if a restriction template
C                is present.  If no restriction template is present
C                it is returned as END + 1.
C
C     END        is the index of the last character in the string.
C
C     BASE       is the portion of the string that precedes the
C                restriction template. If no template is present
C                BASE is assigned the value of word (with truncation
C                if BASE has shorter than END - BEG + 1 .
C
C     KEY        is returned as true if STRING(BEG:END) is a keyword
C                in the language that is being specified.  Otherwise
C                it is false.
C
C     TEMP       is returned as true if STRING(BEG:END) is a META-KEY
C                and ends with a restriction template. Otherwise it is
C                false.
C
C$ Error_Handling
C
C     None.  A restriction template is present or it isn't.
C
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
C      The list of META/2 keywords is given below.  A word of a
C      statement template is viewed as a language keyword if it is
C      not on this list.
C
C        '@alpha',   '@alpha(%*)',  '@body',        '@day',
C        '@end'      '@english',    '@english(%*)', '@epoch',
C        '@int',     '@int(*:*)'    '@month',       '@month(%*)',
C        '@name',    '@name(%*)',   '@number'       '@number(*:*)',
C        '@then'     '@then(%*)',   '@time',        '@unit',
C        '@year',    '}'
C
C      If the word is not a keyword, then it is examined and any
C      restriction templates are returned.
C
C      The restriction template is part of the META/2 language and is
C      described in the required reading section.  Briefly it is
C      a string at the end of a word that has the form
C
C      (x)
C
C      where x is any string of length at least 1.  The interpretation
C      of this string is handled in META2.
C
C      This is purely a utility for META2 and is not a general purpose
C      routine.
C
C$ Examples
C
C      None.
C
C$ Restrictions
C
C      None.
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
C-     META/2 Version 4.0.0, 23-MAR-2000 (WLT)
C
C         Extended the routine to add the keyword @unit to the
C         list of Meta/2 keywords.
C
C-     META/2 Configured Version 3.0.0, 14-AUG-1995 (WLT)
C
C         The keyword @body was out of order in the quick
C         check list below.  Who knows what other terrible
C         bugs this was causing.
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
C     Version B1.0.0, 23-MAR-1988 (WLT) (IMU)
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
      INTEGER               I
      INTEGER               J
      INTEGER               K
 
      CHARACTER*4           CWORD
 
      INTEGER               NQUICK
      PARAMETER           ( NQUICK = 19 )
 
      CHARACTER*4           QUICK  (NQUICK)
 
      INTEGER               CHECKS (NQUICK)
 
      INTEGER               PNTRS  (NQUICK)
 
      INTEGER               NFULL
      PARAMETER           ( NFULL = 26 )
 
      CHARACTER*16          FULL   (NFULL)
 
      LOGICAL               MATCH
      INTEGER               TEMPS  (NQUICK)
 
 
      SAVE
C
C     The array QUICK contains abbreviations of all of the know META-KEY
C     words in alphabetical order.
C
      DATA                  QUICK  / '@alp',  '@bod',  '@cal',  '@day',
     .                               '@end',
     .                               '@eng',  '@epo',  '@int',  '@mon',
     .                               '@nam',  '@num',  '@the',  '@tim',
     .                               '@uni',
     .                               '@wor',  '@yea',  '{',     '|',
     .                               '}'    /
 
 
 
C
C     The array TEMPS gives the character position within a word where
C     a template will be attached to a META-KEY word.
C     If the first portion of a word equals QUICK(I), TEMP(I) will be
C     the character immediately before the template (if one is present).
C
C     If a template is not allowed for a META-KEY word, TEMP will be 0.
C
      DATA                  TEMPS  /  6,       5,       0,       0,
     .                                0,
     .                                8,       0,       4,       6,
     .                                5,       7,       5,       0,
     .                                5,
     .                                5,       0,       0,       0,
     .                                0     /
 
C
C     The array CHECKS tells how many different ways a META-KEY word
C     can be represented.  For example @alpha or @alpha(template).
C     If a word matches up in the beginning with QUICK(I) then there
C     are at most CHECKS(I) checks that we must perform to see if it
C     is in fact a legitimate META-KEY word.
C
      DATA                  CHECKS /  2,       2,       1,       1,
     .                                1,
     .                                2,       1,       2,       2,
     .                                2,       2,       2,       1,
     .                                2,
     .                                2,       1,       0,       0,
     .                                0     /
 
 
C
C     PNTRS(I) points to the first position in the array FULL where
C     one should look to find the actual patterns that should be
C     checked to see if a word that matches the initial portion
C     in QUICK(I) is in fact a META-KEY
C
      DATA                  PNTRS  /  1,      3,        5,      6,
     .                                7,
     .                                8,     10,       11,      13,
     .                               15,     17,       19,      21,
     .                               22,
     .                               24,     26,       26,      26,
     .                               26      /
 
      DATA                  FULL   /  '@alpha',        '@alpha(%*)',
     .                                '@body',
     .                                '@body(%*)',     '@calendar',
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
     .                                '@year'                       /
 
 
 
C
C     First do a binary search on the abreviations of the META-KEYS
C     to see if this is a key word.
C
      CWORD = STRING(BEG:END)
      I     = BSRCHC ( CWORD, NQUICK, QUICK )
 
      IF ( I .EQ. 0 ) THEN
 
C
C        We didn't even match up with one of the abbreviations,  this
C        can't be a META-KEY and so must be a language specification
C        keyword.
C
         KEY    = .TRUE.
         TEMP   = .FALSE.
         BASE   = STRING(BEG:END)
         BEG    = END + 1
         RETURN
 
      ELSE
 
C
C        We at least match an abbreviation.  See if we match the
C        full expansion of the abbreviation.
C
         KEY    = .FALSE.
         K      = PNTRS(I)
         J      = 1
         MATCH  = .FALSE.
 
         DO WHILE ( (J .LE. CHECKS(I)) .AND.
     .              .NOT. MATCH              )
 
 
            MATCH  = MATCHW ( STRING(BEG:END), FULL(K), '*', '%' )
            KEY    = .NOT. MATCH
 
            K      = K + 1
            J      = J + 1
 
         END DO
 
         IF ( KEY ) THEN
 
 
            TEMP = .FALSE.
            BASE = STRING(BEG:END)
            BEG  = END + 1
            RETURN
 
         END IF
 
C
C        If we get this far we must have a META-KEY.  See if there
C        is a restriction template.
C
 
         IF ( STRING(BEG:END) .EQ. FULL(PNTRS(I)) ) THEN
 
C
C           There is no restriction template.
C
            BASE  = STRING(BEG:END)
            BEG   = END + 1
            TEMP  = .FALSE.
 
         ELSE
 
C
C           We have a restriction template.
C
 
            BASE  = FULL ( PNTRS(I) )
            BEG   = BEG + TEMPS(I)
            TEMP  = .TRUE.
 
         END IF
      END IF
 
 
      RETURN
      END
