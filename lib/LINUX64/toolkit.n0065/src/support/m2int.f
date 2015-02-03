C$Procedure      M2INT ( Determine whether or not a word is an integer )
 
      LOGICAL FUNCTION M2INT ( WORD )
      IMPLICIT NONE
 
C$ Abstract
C
C     This function is true if the input string is an integer in the
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
C     The function is returned as .TRUE. if word is a META/2 integer.
C
C$ Detailed_Input
C
C     WORD      is a character string that is assumed to have no
C               spaces between the first and last non-blank characters.
C
C$ Detailed_Output
C
C     M2INT     returns as .TRUE. if WORD is a META/2 integer.
C               Otherwise it is returned .FALSE.
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
C     determines whether or not a word is an integer in the sense
C     of the language META/2.
C
C$ Examples
C
C     WORD                                  M2INT
C     -------                               ------
C     SPAM                                  .FALSE.
C     1                                     .TRUE.
C     0.289E19                              .FALSE.
C     0.2728D12                             .FALSE.
C     -12.1892e-5                           .FALSE.
C     12.E29                                .FALSE.
C     12.E291                               .FALSE.
C     1.2E10                                .TRUE.
C     .E12                                  .FALSE.
C     1.2E.12                               .FALSE.
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
      INTEGER               INTMAX
      INTEGER               INTMIN
      INTEGER               QRTRIM
      INTEGER               LTRIM
C
C     Local variables
C
      INTEGER               END
      INTEGER               FACTOR
      INTEGER               I
      INTEGER               LENGTH
      INTEGER               MINUS
      INTEGER               PLUS
      INTEGER               START
      INTEGER               SUBSEQ
      INTEGER               VALUE
      INTEGER               ZERO
 
      LOGICAL               BAD ( 0 : 255 )
      LOGICAL               FIRST
      LOGICAL               USEMIN
      SAVE
 
      DATA                  FIRST /.TRUE./
 
      IF ( FIRST ) THEN
         FIRST = .FALSE.
 
         DO I = 0, 255
            BAD(I) = .TRUE.
         END DO
 
         MINUS             = ICHAR( '-' )
         PLUS              = ICHAR( '+' )
         ZERO              = ICHAR( '0' )
 
         BAD( ICHAR('0') ) = .FALSE.
         BAD( ICHAR('1') ) = .FALSE.
         BAD( ICHAR('2') ) = .FALSE.
         BAD( ICHAR('3') ) = .FALSE.
         BAD( ICHAR('4') ) = .FALSE.
         BAD( ICHAR('5') ) = .FALSE.
         BAD( ICHAR('6') ) = .FALSE.
         BAD( ICHAR('7') ) = .FALSE.
         BAD( ICHAR('8') ) = .FALSE.
         BAD( ICHAR('9') ) = .FALSE.
 
 
      END IF
 
      START        = LTRIM (WORD)
      END          = QRTRIM(WORD)
      LENGTH       = END - START + 1
      SUBSEQ       = START + 1
 
      IF ( LENGTH .EQ. 1 ) THEN
 
         BAD ( MINUS ) = .TRUE.
         BAD ( PLUS  ) = .TRUE.
 
         M2INT = .NOT. BAD( ICHAR(WORD(START:START)) )
         RETURN
 
      ELSE IF ( LENGTH .GT. 10 ) THEN
 
         M2INT = .FALSE.
 
      ELSE
 
         BAD ( MINUS ) = .FALSE.
         BAD ( PLUS  ) = .FALSE.
 
      END IF
 
      IF ( BAD( ICHAR(WORD(START:START)) ) ) THEN
         M2INT = .FALSE.
         RETURN
      END IF
 
      BAD ( MINUS ) = .TRUE.
      BAD ( PLUS  ) = .TRUE.
 
      DO I = SUBSEQ, END
         IF ( BAD( ICHAR(WORD(I:I)) ) ) THEN
            M2INT = .FALSE.
            RETURN
         END IF
      END DO
C
C     We allow 10 digit numbers only if the first character
C     is a '+' or '-'  So if we have 10 digits the first must
C     now be a "bad" character.
C
      USEMIN = ICHAR(WORD(START:START)) .EQ. MINUS
 
      IF (   BAD( ICHAR(WORD(START:START)) )   ) THEN
 
         IF ( LENGTH .LT. 11 ) THEN
            M2INT = .TRUE.
            RETURN
         END IF
 
         START  = SUBSEQ
 
      ELSE IF ( LENGTH .EQ. 11 ) THEN
 
         M2INT = .FALSE.
         RETURN
 
      ELSE IF ( LENGTH .LT. 10 ) THEN
 
         M2INT = .TRUE.
         RETURN
 
      END IF
 
      IF ( USEMIN ) THEN
 
         VALUE  = INTMIN()
         FACTOR = 1
 
         DO I  = END, START+1, -1
            VALUE  = VALUE  + ( ICHAR(WORD(I:I)) - ZERO )*FACTOR
            FACTOR = FACTOR * 10
         END DO
 
         IF ( ICHAR(WORD(START:START)) .GT. ICHAR('2') ) THEN
            M2INT = .FALSE.
         ELSE
            I     = START
            VALUE = VALUE  + ( ICHAR(WORD(I:I)) - ZERO )*FACTOR
            M2INT = VALUE .LE. 0
         END IF
      ELSE
         VALUE  = INTMAX()
         FACTOR = 1
         DO I  = END, START+1, -1
            VALUE  = VALUE  - ( ICHAR(WORD(I:I)) - ZERO )*FACTOR
            FACTOR = FACTOR * 10
         END DO
 
         IF ( ICHAR(WORD(START:START)) .GT. ICHAR('2') ) THEN
            M2INT = .FALSE.
         ELSE
            I     = START
            VALUE = VALUE  - ( ICHAR(WORD(I:I)) - ZERO )*FACTOR
            M2INT = VALUE .GE. 0
         END IF
      END IF
 
      RETURN
      END
