 
C$Procedure MATCHE ( Match two words, allowing for common errors )
 
      SUBROUTINE MATCHE ( WORD, GUESS, TRANSF, LOC )
      IMPLICIT NONE
 
C$ Abstract
C
C      Determines whether or not two words may be the same,
C      allowing for common typing errors.
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
C      SEARCH
C      UTILITY
C
C$ Declarations
 
      CHARACTER*(*)         WORD
      CHARACTER*(*)         GUESS
      CHARACTER*(*)         TRANSF
      INTEGER               LOC
 
C$ Brief_I/O
C
C      Variable  I/O  Description
C      --------  ---  --------------------------------------------------
C      WORD       I   Word to be matched against initial guess.
C      GUESS      I   Initial guess.
C      TRANSF     O   Transformation that makes WORD match GUESS.
C      LOC        O   Location at which to apply transformation.
C
C$ Detailed_Input
C
C      WORD       is a character string to be checked for a match
C                 against an initial guess. Leading and trailing
C                 blanks are ignored. Typically, WORD will contain
C                 a single word. In any case, the significant part
C                 of WORD may not exceed 64 characters.
C
C      GUESS      is an initial guess at the value of the input word.
C                 Leading and trailing blanks are ignored. Like WORD,
C                 this will typically be a single word.
C
C$ Detailed_Output
C
C      TRANSF     is the name of a transformation which, when applied
C                 to WORD, makes WORD match with GUESS. The possible
C                 transformations are:
C
C                    'TRANSPOSE'   Transpose two characters.
C
C                    'REPLACE'     Replace a single character.
C
C                    'INSERT'      Insert an extra character.
C
C                    'REMOVE'      Remove a character.
C
C                    'IDENTITY'    Do nothing.
C
C                 These reflect some of the most common typing mistakes.
C                 If none if these transformations will do the trick,
C                 TRANSF is 'NONE'.
C
C      LOC        is the location at which the indicated transformation
C                 should be applied.
C
C                    When TRANSF is   LOC is
C                    --------------   ------
C                    'TRANSPOSE'      Location of the first character
C                                     to be transposed.
C
C                    'REPLACE'        Location of the character to be
C                                     replaced.
C
C                    'INSERT'         Location at which the character
C                                     should be inserted.
C
C                    'REMOVE'         Location of the character to be
C                                     removed.
C
C                    'IDENTITY'       Zero.
C
C                    'NONE'           Zero.
C
C$ Exceptions
C
C      None.
C
C$ Particulars
C
C      Some typing mistakes should be relatively easy to catch, since
C      the difference between the intended word and the typed word may
C      involve a single transformation. MATCHE applies the most common
C      transformations to an input word, and attempt to match the
C      resulting word to a an initial guess.
C
C$ Examples
C
C      Let
C
C         GUESS = 'APPLE'
C
C      Then
C
C         If WORD is        TRANSF is         LOC is
C         -----------       -------------     ------
C         'APPEL'           'TRANSPOSE'        4
C         'APPLY'           'REPLACE'          5
C         'DAPPLE'          'REMOVE'           1
C         'APPLES'          'REMOVE'           5
C         'PPLE'            'INSERT'           1
C         'APPE'            'INSERT'           4
C         'APPL'            'INSERT'           5
C         'APPLE'           'IDENTITY'         0
C         'APPEAL'          'NONE'             0
C
C$ Restrictions
C
C      1) MATCHE is case-sensitive. Lowercase characters do not match
C         uppercase characters, and vice versa.
C
C      2) ASCII characters 1 and 2 are used internally as wildcard
C         characters, and should not appear in either WORD or GUESS.
C
C$ Common_Variables
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
C      Version B 1.0.0, 5-APR-1988
C
C-&
 
 
 
C
C     SPICELIB functions
C
      INTEGER               QRTRIM
      LOGICAL               EQSTR
 
C
C     Local variables
C
      CHARACTER*(65)        COPY
      CHARACTER*(65)        TEMPL
      CHARACTER*(65)        MYGUES
 
 
      INTEGER               CLEN
      INTEGER               I
 
 
C
C     Save a copy of the first 64 significant characters in a buffer,
C     from which we may construct templates.
C
      COPY = ' '
 
      CALL LJUST ( WORD,  COPY(1:64) )
      CALL LJUST ( GUESS, MYGUES     )
 
      CLEN = QRTRIM ( COPY )
 
C
C     Apply the transformations one at a time, in the order most
C     likely to succeed:
C
C        Removal
C        Transposition
C        Replacement
C        Insertion
C
C     Quit as soon as a possible match is found.
C
C     Actually, we need to check for identity first. Otherwise,
C     we're likely to find a transposition that yields the same
C     word: for example, transposing the second and third letters
C     of APPLE yields APPLE.
C
      IF ( EQSTR( WORD, MYGUES ) ) THEN
         TRANSF = 'IDENTITY'
         LOC    = 0
         RETURN
      END IF
 
 
C
C     Removal
C     -------
C
C     Remove the character at each location, and check against MYGUES.
C
      DO I = 1, CLEN
 
         CALL REMSUB ( COPY, I, I, TEMPL )
 
         IF ( EQSTR( TEMPL, MYGUES ) ) THEN
            TRANSF = 'REMOVE'
            LOC    = I
            RETURN
         END IF
      END DO
 
 
 
C
C     Transposition
C     -------------
C
C     Transpose each pair of characters, and check against MYGUES.
C
      DO I = 1, CLEN - 1
 
         TEMPL          = COPY
         TEMPL(I  :I  ) = COPY(I+1:I+1)
         TEMPL(I+1:I+1) = COPY(I  :I  )
 
         IF ( EQSTR( TEMPL, MYGUES ) ) THEN
            TRANSF = 'TRANSPOSE'
            LOC    = I
            RETURN
         END IF
 
      END DO
 
 
 
C
C     Replacement
C     -----------
C
C     Replace each character with a wild character, and check
C     against MYGUES.
C
      DO I = 1, CLEN
 
         TEMPL      = COPY
         TEMPL(I:I) = MYGUES(I:I)
 
         IF ( EQSTR( TEMPL, MYGUES ) ) THEN
            TRANSF = 'REPLACE'
            LOC    = I
            RETURN
         END IF
      END DO
 
 
 
C
C     Insertion
C     ---------
C
C     Insert a wild character at each location, and check against 
C     MYGUES.
C
      DO I = 1, CLEN + 1
 
 
         IF ( I .EQ. 1 ) THEN
            TEMPL(1:1) = MYGUES(1:1)
            TEMPL(2:)  = COPY
         ELSE IF ( I .EQ. CLEN + 1 ) THEN
            TEMPL      = COPY
            TEMPL(I:I) = MYGUES(I:I)
         ELSE
            TEMPL(1  :I-1) = COPY(1:I-1)
            TEMPL(I  :I  ) = MYGUES(I:I)
            TEMPL(I+1:   ) = COPY(I:)
         END IF
 
         IF ( EQSTR(TEMPL, MYGUES) ) THEN
            TRANSF = 'INSERT'
            LOC    = I
            RETURN
         END IF
      END DO
 
 
 
 
C
C     None of these transformations work.
C
      TRANSF = 'NONE'
      LOC    = 0
 
      RETURN
      END
