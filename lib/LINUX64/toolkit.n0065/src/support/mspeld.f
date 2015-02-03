C$Procedure      MSPELD ( Misspelling diagnosis )
 
      SUBROUTINE MSPELD ( WORD, GUESS, CAUSE )
      IMPLICIT NONE
 
C$ Abstract
C
C     Diagnose possible spelling errors that might cause a word
C     to differ from another (known) word.
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
C     COMPARE
C     ERROR
C     PARSING
C     UTILITY
C     WORD
C
C$ Declarations
 
      CHARACTER*(*)         WORD
      CHARACTER*(*)         GUESS
      CHARACTER*(*)         CAUSE
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     WORD       I   A word that is thought to be misspelled.
C     GUESS      I   A word that is thought to be "close" to WORD.
C     CAUSE      O   A message indicating the difference between them.
C
C$ Detailed_Input
C
C     WORD       A word that is thought to be misspelled.
C
C     GUESS      A word that is thought to be "close" to WORD.
C
C$ Detailed_Output
C
C     CAUSE      A message that indicates the difference between WORD
C                and GUESS.
C
C$ Exceptions
C
C     1) CAUSE is blank whenever WORD and GUESS are the same.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     A number of spelling errors are due to the lack of cooperation
C     between the hands that do the typing and the brain that knows
C     how something should be spelled.  Four common errors are:
C
C        1) Leaving out a necessary character.
C        2) Adding an extra character.
C        3) Mistyping a single character.
C        4) Transposing two characters.
C
C     This routine creates "friendly" diagnostic messages indicating
C     whether or not the difference between WORD and GUESS could have
C     been caused by one of these simple errors.
C
C     This routine will typically be used only after the list of
C     guesses has been narrowed down to words that are "close" to
C     the unrecognized word.
C
C$ Examples
C$
C
C      WORD  :   LENGHT
C      GUESS :   LENGTH
C      CAUSE :  'It appears that you have transposed the fifth and
C                sixth letters of LENGTH (the letters T and H).'
C
C
C      WORD  :   EPHEMRIS
C      GUESS :   EPHEMERIS
C      CAUSE :  'It appears that you have left out the sixth letter of
C                EPHEMERIS. (The sixth letter should be E.)'
C
C      WORD  :   INTWGRATE
C      WORD  :   INTEGRATE
C      CAUSE :   'It appears that you have mistyped the fourth letter
C                 of INTEGRATE.  (The fourth letter should be E. You
C                 have W instead.)'
C
C      WORD :    INTERGER
C      GUESS:    INTEGER
C      CAUSE    'It appears that you have an extra letter at the fifth
C                letter of INTERGER. (The fifth letter R should be
C                removed.)'
C
C      WORD :    URUNAS
C      GUESS:    URANUS
C      CAUSE:   'I believe you meant URANUS. However, the actual
C                spelling error is not a simple one.'
C
C      WORD :    INTERDENOMINATIONAL
C      GUESS:    INTERDENOMINATIONAL
C      CAUSE:   ' '
C
C$ Restrictions
C
C      Any restrictions that apply to the words compared by MATCHE
C      apply as well to WORD and GUESS.
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
C     Version B1.0.0, 13-APR-1988 (WLT) (IMU)
C
C-&
 
 
 
C
C     Local variables
C
      INTEGER               LOC
      CHARACTER*12          DIAGNS
 
      CHARACTER*16          FIRST
      CHARACTER*16          LAST
 
 
      CALL MATCHE ( WORD, GUESS, DIAGNS, LOC )
 
      IF ( DIAGNS .EQ. 'IDENTITY' ) THEN
 
         CAUSE = ' '
 
      ELSE IF ( DIAGNS .EQ. 'TRANSPOSE' ) THEN
 
         CALL INTORD ( LOC,   FIRST )
         CALL INTORD ( LOC+1, LAST  )
 
         CALL LCASE  ( FIRST, FIRST )
         CALL LCASE  ( LAST,  LAST  )
 
         CAUSE = 'It appears that you have transposed the '
 
         CALL SUFFIX ( FIRST,              1, CAUSE )
         CALL SUFFIX ( 'and',              1, CAUSE )
         CALL SUFFIX ( LAST,               1, CAUSE )
         CALL SUFFIX ( 'letters of',       1, CAUSE )
         CALL SUFFIX ( GUESS,              1, CAUSE )
         CALL SUFFIX ( '(the letters',     1, CAUSE )
         CALL SUFFIX ( GUESS(LOC:LOC),     1, CAUSE )
         CALL SUFFIX ( 'and',              1, CAUSE )
         CALL SUFFIX ( GUESS(LOC+1:LOC+1), 1, CAUSE )
         CALL SUFFIX ( ').',               0, CAUSE )
 
 
      ELSE IF ( DIAGNS .EQ. 'INSERT' ) THEN
 
         CALL INTORD ( LOC,   FIRST )
         CALL LCASE  ( FIRST, FIRST )
 
         CAUSE = 'It appears that you have left out the '
 
         CALL SUFFIX ( FIRST,                1, CAUSE )
         CALL SUFFIX ( 'letter of ',         1, CAUSE )
         CALL SUFFIX ( GUESS,                1, CAUSE )
         CALL SUFFIX ( '. (The ',            0, CAUSE )
         CALL SUFFIX ( FIRST,                1, CAUSE )
         CALL SUFFIX ( 'letter should be ',  1, CAUSE )
         CALL SUFFIX ( GUESS(LOC:LOC),       1, CAUSE )
         CALL SUFFIX ( '.)',                 0, CAUSE )
 
 
      ELSE IF ( DIAGNS .EQ. 'REPLACE' ) THEN
 
         CALL INTORD ( LOC,   FIRST )
         CALL LCASE  ( FIRST, FIRST )
 
         CAUSE = 'It appears that you have mistyped the '
 
         CALL SUFFIX ( FIRST,                 1, CAUSE )
         CALL SUFFIX ( 'letter of ',          1, CAUSE )
         CALL SUFFIX ( GUESS,                 1, CAUSE )
         CALL SUFFIX ( '. (The ',             0, CAUSE )
         CALL SUFFIX ( FIRST,                 1, CAUSE )
         CALL SUFFIX ( 'letter should be ',   1, CAUSE )
         CALL SUFFIX ( GUESS(LOC:LOC),        1, CAUSE )
         CALL SUFFIX ( '. You have ',         0, CAUSE )
         CALL SUFFIX ( WORD(LOC:LOC),         1, CAUSE )
         CALL SUFFIX ( 'instead.)',           1, CAUSE )
 
 
      ELSE IF ( DIAGNS .EQ. 'REMOVE'  ) THEN
 
         CALL INTORD ( LOC,   FIRST )
         CALL LCASE  ( FIRST, FIRST )
 
         CAUSE = 'It appears that you have an extra letter at the '
 
         CALL SUFFIX ( FIRST,                   1, CAUSE )
         CALL SUFFIX ( 'letter of ',            1, CAUSE )
         CALL SUFFIX ( WORD,                    1, CAUSE )
         CALL SUFFIX ( '. (The ',               0, CAUSE )
         CALL SUFFIX ( FIRST,                   1, CAUSE )
         CALL SUFFIX ( 'letter ',               1, CAUSE )
         CALL SUFFIX ( WORD(LOC:LOC),           1, CAUSE )
         CALL SUFFIX ( 'should be removed.)',   1, CAUSE )
 
 
      ELSE
 
         CAUSE = 'I believe you meant '
 
         CALL SUFFIX ( GUESS,                              1, CAUSE )
         CALL SUFFIX ( '.  However, the actual spelling ', 1, CAUSE )
         CALL SUFFIX ( 'error is not a simple one.      ', 1, CAUSE )
 
 
      END IF
 
 
      RETURN
      END
