C$Procedure      M2BEGR ( See if a word begins with a range template )
 
      SUBROUTINE M2BEGR ( STRING, BEG, END, A, B )
      IMPLICIT NONE
 
C$ Abstract
C
C     Determine whether or not the substring STRING(BEG:END) begins
C     with a substring of the form (A:B) where A and B are integers.
C     If it does, et BEG is set to the index of the first character
C     following this substring and the integer values of A and B are
C     returned.  Otherwise no action is taken.
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
      INTEGER               A
      INTEGER               B
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STRING     I   A META/2 language statement specification.
C     BEG       I/0  The beginning of the substring on input and output
C     END       I/0  The end of the substring on input and output
C     A          O   Lower value of the range template
C     B          O   Upper value of the range template
C
C$ Detailed_Input
C
C     STRING(BEG:END)  is a word in the META/2 language.  It potentially
C                      begins with a substring of the form (A:B) where
C                      A and B are both chracter strings representing
C                      integers.
C
C
C$ Detailed_Output
C
C     BEG        On ouput BEG points to the beginning of the portion of
C                the input word that follows the range template (if
C                one was present)  Otherwise it remains unchanged.
C
C     END        points to the end of the input META/2 word.
C
C     A          is the value represented by the first numeric string
C                of the range template.  If a range template is not
C                present, A is not assigned a value.
C
C     B          is the value represented by the second numeric string
C                of the range template (if there is a second numeric
C                string)  If a range template is present, but no numeric
C                string is present B is assigned the value INTMAX().
C
C
C$ Error_Handling
C
C     None.  A range template is present or it isn't.
C
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
C      The range template is part of the META/2 language and is
C      described in the required reading section.  Briefly it is
C      a string at the beginning of a word that has the form
C
C      (A:B)
C
C      where A is a string representing a positive integer, and
C      B the null string or a string representing a positive integer
C      greater than A.
C
C      This routine determines if a range template is present and if so
C      what the values of A and B are.  If B is the null string it
C      is assumed to represent the largest positive integer.
C
C$ Examples
C
C      Consider the following
C
C      inputs                              outputs
C
C      STRING(BEG:END) BEG    END          BEG   END   A      B
C      --------------- ---    --- ---      ---   ---  ---    ---
C      (1:2)@number     5     16            10    16   1      2
C      1:2@number       7     16             7    16   x      x
C      (-1:23)@word     3     14             3    14   x      x
C      @frank           6     11             6    11   x      x
C      (4:)@spam(1:2)  54     67            58    67   4     INTMAX()
C      @spud(1:12)     10     20            10    20   x      x
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
      INTEGER               INTMAX
 
C
C     Local variables
C
      INTEGER               START
      INTEGER               I
      INTEGER               LPAREN
      INTEGER               RPAREN
      INTEGER               COLON
 
      LOGICAL               DIGIT ( 0 : 255 )
 
      CHARACTER*80          ERROR
      INTEGER               POINTR
 
 
      LOGICAL               FIRST
      SAVE
 
      DATA                  FIRST / .TRUE. /
 
 
 
 
      IF ( FIRST ) THEN
         FIRST = .FALSE.
 
         DO I = 0, 255
            DIGIT(I) = .FALSE.
         END DO
 
         DIGIT( ICHAR('0') ) = .TRUE.
         DIGIT( ICHAR('1') ) = .TRUE.
         DIGIT( ICHAR('2') ) = .TRUE.
         DIGIT( ICHAR('3') ) = .TRUE.
         DIGIT( ICHAR('4') ) = .TRUE.
         DIGIT( ICHAR('5') ) = .TRUE.
         DIGIT( ICHAR('6') ) = .TRUE.
         DIGIT( ICHAR('7') ) = .TRUE.
         DIGIT( ICHAR('8') ) = .TRUE.
         DIGIT( ICHAR('9') ) = .TRUE.
 
         LPAREN = ICHAR( '(' )
         RPAREN = ICHAR( ')' )
         COLON  = ICHAR( ':' )
 
      END IF
 
 
 
C
C     We need at least (x:) in order to have a range template,  that
C     means at least 4 characters.
C
      IF ( END - BEG .LT. 3 ) THEN
         RETURN
      END IF
 
      I = BEG
C
C     Range templates must begin with '('
C
      IF ( ICHAR(STRING(I:I)) .NE. LPAREN ) THEN
 
         RETURN
 
      END IF
 
      I     = I + 1
 
C
C     We must have at least 1 digit
C
      IF ( .NOT. DIGIT( ICHAR(STRING(I:I)) ) ) THEN
 
         RETURN
 
      ELSE
 
         I      = I + 1
 
      END IF
 
 
C
C     Now examin characters until we reach a non-digit
C     or run out of characters in the string.
C
      DO WHILE ( ( I .LE. END ) .AND. DIGIT(ICHAR(STRING(I:I))) )
 
         I     = I + 1
 
      END DO
 
 
C
C     If the last character encountered was a number or if it was
C     not a colon, we don't have a range template.
C
      IF (      DIGIT(ICHAR(STRING(I:I))) ) THEN
 
         RETURN
 
      ELSE IF ( ICHAR(STRING(I:I)) .NE. COLON ) THEN
 
         RETURN
 
      END IF
 
C
C     Ok. we've got an integer. Parse it and put the result
C     into A.
C
      CALL NPARSI ( STRING(BEG+1:I-1), A, ERROR, POINTR )
 
C
C     Just in case, make sure the number didn't cause an NPARSI error
C     (the only thing can go wrong is the number is too big)
C
      IF ( POINTR .NE. 0 ) THEN
         RETURN
      END IF
 
 
C
C     Look at the next letter ( if there is one ) and see if it
C     is a digit.
C
      I     = I + 1
      START = I
 
      IF ( I .GT. END  ) THEN
         RETURN
      END IF
 
 
C
C     Examine letters until we reach a non-digit or run out of
C     characters to examine.
C
      DO WHILE ( ( I .LT. END )  .AND.  DIGIT(ICHAR(STRING(I:I)))  )
         I     = I + 1
      END DO
 
C
C     If the last character is a digit (we ran out of letters)
C     or was not
C
      IF ( DIGIT(ICHAR(STRING(I:I)))            ) THEN
 
         RETURN
 
      ELSE IF (  ICHAR(STRING(I:I)) .NE. RPAREN ) THEN
 
         RETURN
 
      END IF
 
C
C     If the last character read is beyond the first character
C     after the ':', then we've got an integer.
C
      IF ( I .GT. START ) THEN
 
         CALL NPARSI ( STRING(START:I-1), B, ERROR, POINTR )
 
C
C        Make sure everythin parsed ok.
C
         IF ( POINTR .NE. 0 ) THEN
 
            RETURN
         ELSE IF ( B .LT. A   ) THEN
 
            RETURN
 
         ELSE
 
            BEG    = I + 1
            RETURN
 
         END IF
 
C
C     If the first character after the colon was the right parenthesis
C     put INTMAX into B
C
      ELSE
 
         B      = INTMAX()
         BEG    = I + 1
         RETURN
 
      END IF
 
      END
