C$Procedure      M2TIME ( Determine whether or not a word is a time )
 
      LOGICAL FUNCTION M2TIME ( WORD )
      IMPLICIT NONE
 
C$ Abstract
C
C     This function is true if the input string is a time in the
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
C     The function is returned as .TRUE. if word is an META/2 time.
C
C$ Detailed_Input
C
C     WORD      is a character string that is assumed to have no
C               spaces between the first and last non-blank characters.
C
C$ Detailed_Output
C
C     M2TIME    returns as .TRUE. if WORD has the form
C
C                    hh:mm:ss.ssssss
C
C               where
C
C                    hh     stands for one or two digits and the number
C                           they represent is less than 24.
C
C                    mm     stands for one or two digits and the number
C                           they represent is less than 60
C
C                    ss.ss  stands for a decimal number less than 61.
C
C               Otherwise M2TIME is returned .FALSE.
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
C     determines whether or not a word is a time in the sense
C     of the language META/2.
C
C$ Examples
C
C     WORD                                  M2TIME
C     -------                               ------
C     SPAM                                  .FALSE.
C     _SPUD                                 .FALSE.
C     1:23:27                               .TRUE.
C     21.23.28                              .FALSE.
C     24:13:48.28                           .FALSE.
C     23:59:60.281                          .TRUE.
C     19:3:1                                .TRUE.
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
      INTEGER               LTRIM
      INTEGER               QRTRIM
 
      INTEGER               DIGIT
      PARAMETER           ( DIGIT  =  1 )
 
      INTEGER               COLON
      PARAMETER           ( COLON  = DIGIT  + 1 )
 
      INTEGER               POINT
      PARAMETER           ( POINT  = COLON  + 1 )
 
      INTEGER               OTHER
      PARAMETER           ( OTHER  = POINT  + 1 )
 
 
 
      INTEGER               CLASS  ( 0 : 255 )
      INTEGER               COMP
      INTEGER               COUNT
      INTEGER               END
      INTEGER               FACTOR ( 4 )
      INTEGER               I
      INTEGER               LIMIT  ( 4 )
      INTEGER               N
      INTEGER               START
      INTEGER               UBND   ( 4 )
      INTEGER               ZERO
 
      LOGICAL               COLOK  ( 4 )
      LOGICAL               FIRST
      LOGICAL               PNTOK  ( 4 )
 
      SAVE
 
      DATA                  FIRST / .TRUE. /
 
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
         DO I = 0, 255
            CLASS(I) = OTHER
         END DO
 
         CLASS( ICHAR('0') ) = DIGIT
         CLASS( ICHAR('1') ) = DIGIT
         CLASS( ICHAR('2') ) = DIGIT
         CLASS( ICHAR('3') ) = DIGIT
         CLASS( ICHAR('4') ) = DIGIT
         CLASS( ICHAR('5') ) = DIGIT
         CLASS( ICHAR('6') ) = DIGIT
         CLASS( ICHAR('7') ) = DIGIT
         CLASS( ICHAR('8') ) = DIGIT
         CLASS( ICHAR('9') ) = DIGIT
         CLASS( ICHAR(':') ) = COLON
         CLASS( ICHAR('.') ) = POINT
C
C        The following are the maximum values that are allowed
C        for each of the various components of the time string
C
         UBND  (  1 ) = 23
         UBND  (  2 ) = 59
         UBND  (  3 ) = 60
         UBND  (  4 ) = 10
C
C        The following are the maximum number of digits that
C        are allowed for each of the components of the time
C
         LIMIT (  1 ) = 2
         LIMIT (  2 ) = 2
         LIMIT (  3 ) = 2
         LIMIT (  4 ) = 100
C
C        The following logicals indicate whether or not it is
C        ok to end the N'th component of time with a colon.
C
         COLOK (  1 ) = .TRUE.
         COLOK (  2 ) = .TRUE.
         COLOK (  3 ) = .FALSE.
         COLOK (  4 ) = .FALSE.
C
C        The following logicals indicate whether or not it is
C        ok to end the N'th component of time with a decimal point.
C
         PNTOK (  1 ) = .FALSE.
         PNTOK (  2 ) = .FALSE.
         PNTOK (  3 ) = .TRUE.
         PNTOK (  4 ) = .FALSE.
C
C        The following are the factors used to construct the
C        integer value of a component COMP = FACTOR*COMP + Next digit.
C        Note that for the decimal portion of seconds we don't
C        really compute the value of the decimal part.  The
C        factor term just ensures that the loop below doesn't
C        have any special cases.
C
         FACTOR(  1 ) = 10
         FACTOR(  2 ) = 10
         FACTOR(  3 ) = 10
         FACTOR(  4 ) = 0
 
         ZERO         = ICHAR( '0' )
 
 
      END IF
 
 
      START  = LTRIM ( WORD )
      END    = QRTRIM( WORD )
      COMP   = 0
      N      = 1
      COUNT  = 0
      I      = START
      M2TIME = .TRUE.
 
      IF ( END - START .LT. 4 ) THEN
         M2TIME = .FALSE.
         RETURN
      END IF
 
      DO WHILE ( I .LE. END .AND. M2TIME )
C
C        If the next character is a digit, compute the accumulated
C        value of this component of the time.  Then check to
C        make sure that we don't have too many digits so far
C        in this component and that the value of this component
C        does not exceed the limits for this component.
C
         IF ( CLASS(  ICHAR( WORD(I:I)  ) ) .EQ. DIGIT ) THEN
 
            COUNT = COUNT + 1
            COMP  = FACTOR(N)*COMP + ICHAR(WORD(I:I)) - ZERO
 
            M2TIME   =       COUNT .LE. LIMIT(N)
     .                 .AND. COMP  .LE. UBND (N)
 
C
C        If the next character is a colon ':' then we are starting
C        a new component.  Make sure this is ok and that we actually
C        had a digit or two for the last component.  Increment the
C        component counter, set the number of characters found in
C        the next component to 0 and set the value of the next
C        component to zero.
C
         ELSE IF ( CLASS(  ICHAR( WORD(I:I)  ) ) .EQ. COLON ) THEN
 
            M2TIME = COLOK ( N ) .AND. COUNT .GT. 0
            COUNT  = 0
            COMP   = 0
            N      = N + 1
C
C        If the next character is decimal point, we are ending a
C        component and starting it's decimal portion.  Make sure
C        that a decimal point is allowed for this component and
C        that we had at least one digit in the component we were
C        examining up to this point.
C
         ELSE IF ( CLASS(  ICHAR( WORD(I:I)  ) )  .EQ. POINT ) THEN
 
            M2TIME = PNTOK ( N ) .AND. COUNT .GT. 0
            COUNT  = 0
            COMP   = 0
            N      = N + 1
C
C        If we hit some other character we don't have a time
C        word.
C
         ELSE
 
            M2TIME = .FALSE.
 
         END IF
 
         I = I + 1
 
      END DO
 
      M2TIME = M2TIME .AND. N .GE. 3
 
      RETURN
      END
