 
C$Procedure  HX2INT  ( Signed hexadecimal string to integer )
 
      SUBROUTINE HX2INT ( STRING, NUMBER, ERROR, ERRMSG )
 
C$ Abstract
C
C     Convert a signed hexadecimal string representation of an integer
C     to its equivalent integer.
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
C     ALPHANUMERIC
C     CONVERSION
C
C$ Declarations
C
      CHARACTER*(*)         STRING
      INTEGER               NUMBER
      LOGICAL               ERROR
      CHARACTER*(*)         ERRMSG
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STRING     I   Hexadecimal string to be converted to an integer.
C     NUMBER     O   Integer value to be returned.
C     ERROR      O   A logical flag which is .TRUE. on error.
C     ERRMSG     O   A descriptive error message.
C
C$ Detailed_Input
C
C     STRING   The hexadecimal string to be converted to an integer.
C
C              The following table describes the character set used
C              to represent the hexadecimal digits and their
C              corresponding values.
C
C              Character    Value           Character    Value
C              ---------    -----           ---------    -----
C                '0'          0                '8'          8
C                '1'          1                '9'          9
C                '2'          2              'A','a'       10
C                '3'          3              'B','b'       11
C                '4'          4              'C','c'       12
C                '5'          5              'D','d'       13
C                '6'          6              'E','e'       14
C                '7'          7              'F','f'       15
C
C             The plus sign, '+', and the minus sign, '-', are used as
C             well, and they have their usual meanings.
C
C             A hexadecimal character string parsed by this routine
C             should consist of a sign, '+' or '-' (the plus sign is
C             optional for nonnegative numbers), followed immediately
C             by a contiguous sequence of hexadecimal digits, e.g.:
C
C                (1)   +h h ... h
C                        1 2     n
C
C                (2)   -h h ... h
C                        1 2     n
C
C                (3)   h h ... h
C                       1 2     n
C
C             where h  represents an hexadecimal digit.
C                    i
C
C             STRING may have leading and trailing blanks, but blanks
C             embedded within the signficant portion of the character
C             string are not allowed. This includes any blanks which
C             appear between the sign character and the first
C             hexadecimal digit.
C
C$ Detailed_Output
C
C     NUMBER   The integer value to be returned. The value of this
C              variable is not changed if an error occurs while parsing
C              the hexadecimal character string.
C
C     ERROR    A logical flag which indicates whether an error occurred
C              while attempting to parse NUMBER from the hexadecimal
C              character string STRING. ERROR will have the value
C              .TRUE. if an error occurs. It will have the value
C              .FALSE. otherwise.
C
C     ERRMSG   Contains a descriptive error message if an error
C              occurs while attempting to parse NUMBER from the
C              hexadecimal character string STRING, blank otherwise.
C              The error message will be left justified.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)   If an unexpected character is encountered while parsing the
C          hexadecimal character string, an appropriate error message
C          will be set, and the routine will exit. The value of NUMBER
C          will be unchanged.
C
C     2)   If the string represents a number that is larger than
C          the maximum representable integer an appropriate error
C          message will be set, and the routine will exit. The value
C          of NUMBER will be unchanged.
C
C     3)   If the string represents a number that is smaller than
C          the minimum representable integer, an appropriate error
C          message will be set, and the routine will exit. The value
C          of NUMBER will be unchanged.
C
C     4)   If the input string is blank, an appropriate error message
C          will be set, and the routine will exit. The value of NUMBER
C          will be unchanged.
C
C     5)   If the error message string is not long enough to contain
C          the entire error message, the error message will be
C          truncated on the right.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine will convert a signed hexadecimal character string
C     representation of an integer into its equivalent integer. This
C     provides a machine independent mechanism for storing or porting
C     integer values. This routine is used by the routine HX2DP which
C     converts a character string representation of a double precision
C     into its equivalent double precision value.
C
C     This routine is one of a pair of routines which are used to
C     perform conversions between integers and equivalent signed
C     hexadecimal character strings:
C
C           INT2HX -- Convert an integer into a signed hexadecimal
C                     character string.
C
C           HX2INT -- Convert a signed hexadecimal character string
C                     into an integer.
C
C$ Examples
C
C     All of the values shown are for a two's complement 32 bit
C     representation for signed integers.
C
C     The following argument values illustrate the action of HX2INT for
C     various input values.
C
C         STRING                 NUMBER        ERROR   ERRMSG
C         ---------------------  ------------  ------  ------
C          '1'                    1            .FALSE.   ' '
C          '-1'                  -1            .FALSE.   ' '
C          'DF'                   223          .FALSE.   ' '
C          'Df'                   223          .FALSE.   ' '
C          '+3ABC'                15036        .FALSE.   ' '
C          'ff'                   255          .FALSE.   ' '
C          '-20'                 -32           .FALSE.   ' '
C          '0'                    0            .FALSE.   ' '
C
C          '7FFFFFFF'             2147483647   .FALSE.   ' '
C          (Maximum 32 bit integer)
C
C          '-7FFFFFFF'           -2147483647   .FALSE.   ' '
C          (Minimum 32 bit integer + 1)
C
C          '-80000000'           -2147483648   .FALSE.   ' '
C          (Minimum 32 bit integer)
C
C          STRING = ' '
C          NUMBER = ( Not defined )
C          ERROR  = .TRUE.
C          ERRMSG = 'ERROR: A blank input string is not allowed.'
C
C          STRING = '-AB238Q'
C          NUMBER = ( Not defined )
C          ERROR  = .TRUE.
C          ERRMSG = 'ERROR: Illegal character ''Q'' encountered.'
C
C          STRING = '- AAA'
C          NUMBER = ( Not defined )
C          ERROR  = .TRUE.
C          ERRMSG = 'ERROR: Illegal character '' '' encountered.'
C
C          STRING = '80000000'
C          NUMBER = ( Not defined )
C          ERROR  = .TRUE.
C          ERRMSG = 'ERROR: Integer too large to be represented.'
C
C          STRING = '-800F0000'
C          NUMBER = ( Not defined )
C          ERROR  = .TRUE.
C          ERRMSG = 'ERROR: Integer too small to be represented.'
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C      K.R. Gehringer   (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-     SPICELIB Version 1.1.0, 10-MAR-1994 (KRG)
C
C         Changed an IF test operand from .LE. to .LT. so that
C         the ELSE IF clause could be reached. This change has
C         NO effect on the execution of the routine because it
C         makes use of a base that is a power of 2 (16), so the
C         ELSE IF clause never needs to be reached. The algorithm
C         was meant to be as general as possible, however, so that
C         only the base and digits would need to be changed in order to
C         implement a different number base.
C
C-     SPICELIB Version 1.0.0, 22-OCT-1992 (KRG)
C
C-&
 
C$ Index_Entries
C
C     convert signed hexadecimal string to integer
C
C-&
 
C$ Revisions
C
C-     SPICELIB Version 1.1.0, 10-MAR-1994 (KRG)
C
C         Changed an IF test operand from .LE. to .LT. so that
C         the ELSE IF clause could be reached. This change has
C         NO effect on the execution of the routine because it
C         makes use of a base that is a power of 2 (16), so the
C         ELSE IF clause never needs to be reached. The algorithm
C         was meant to be as general as possible, however, so that
C         only the base and digits would need to be changed in order to
C         implement a different number base.
C
C         Old code was:
C
C            IF ( TMPNUM .LE. MAXI ) THEN
C
C               TMPNUM = TMPNUM * BASE + IDIGIT
C               POS    = POS + 1
C
C            ELSE IF ( ( TMPNUM .EQ. MAXI   ) .AND.
C     .                ( IDIGIT .LE. MAXMOD ) ) THEN
C
C               TMPNUM = TMPNUM * BASE + IDIGIT
C               POS    = POS + 1
C
C            ELSE ...
C
C         New code:
C
C            IF ( TMPNUM .LT. MAXI ) THEN
C
C               TMPNUM = TMPNUM * BASE + IDIGIT
C               POS    = POS + 1
C
C            ELSE IF ( ( TMPNUM .EQ. MAXI   ) .AND.
C     .                ( IDIGIT .LE. MAXMOD ) ) THEN
C
C               TMPNUM = TMPNUM * BASE + IDIGIT
C               POS    = POS + 1
C
C            ELSE ...
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               INTMAX
      INTEGER               INTMIN
C
C     Local Parameters
C
      INTEGER               BASE
      PARAMETER           ( BASE = 16  )
C
C     Local variables
C
      INTEGER               DIGBEG
      INTEGER               DIGEND
      INTEGER               IDIGIT
      INTEGER               IMINUS
      INTEGER               IPLUS
      INTEGER               ISPACE
      INTEGER               LCCBEG
      INTEGER               LCCEND
      INTEGER               LETTER
      INTEGER               MAXI
      INTEGER               MAXMOD
      INTEGER               MINI
      INTEGER               MINMOD
      INTEGER               POS
      INTEGER               STRBEG
      INTEGER               STREND
      INTEGER               TMPNUM
      INTEGER               UCCBEG
      INTEGER               UCCEND
 
      LOGICAL               FIRST
      LOGICAL               MORE
      LOGICAL               NEGTIV
C
C     Saved variables
C
      SAVE                  DIGBEG
      SAVE                  DIGEND
      SAVE                  LCCBEG
      SAVE                  LCCEND
      SAVE                  IMINUS
      SAVE                  IPLUS
      SAVE                  ISPACE
      SAVE                  MAXI
      SAVE                  MAXMOD
      SAVE                  MINI
      SAVE                  MINMOD
      SAVE                  UCCBEG
      SAVE                  UCCEND
 
      SAVE                  FIRST
C
C     Initial values
C
      DATA                  FIRST   / .TRUE. /
 
 
C
C     The input hexadecimal string is scanned from left to right, and
C     the integer is generated by repeated multiplications and additions
C     or subtractions.
C
C     If this is the first time that this routine has been called,
C     we need to do some setup stuff.
C
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
C
C        Initialize the upper and lower bounds for the decimal digits,
C        the upper and lower bounds for the uppercase hexadecimal
C        digits, the upper and lower bounds for the lowercase
C        hexadecimal digits, the space, the plus sign, and the minus
C        sign in the character sequence.
C
         DIGBEG = ICHAR( '0' )
         DIGEND = ICHAR( '9' )
         UCCBEG = ICHAR( 'A' )
         UCCEND = ICHAR( 'F' )
         LCCBEG = ICHAR( 'a' )
         LCCEND = ICHAR( 'f' )
         IMINUS = ICHAR( '-' )
         IPLUS  = ICHAR( '+' )
         ISPACE = ICHAR( ' ' )
C
C        Initialize some boundary values for error checking while
C        constructing the desired integer. These are used to help
C        determine integer overflow or integer underflow errors.
C
         MINI   = INTMIN() / BASE
         MINMOD = BASE * MINI - INTMIN()
         MAXI   = INTMAX() / BASE
         MAXMOD = INTMAX() - BASE * MAXI
 
      END IF
C
C     There are no errors initially, so set the error flag to
C     .FALSE.
C
      ERROR = .FALSE.
C
C     If the string is blank, set the error flag and return immediately.
C
      IF ( STRING .EQ. ' ' ) THEN
 
         ERROR  = .TRUE.
         ERRMSG = 'ERROR: A blank input string is not allowed.'
         RETURN
 
      END IF
C
C     Initialize a few other things.
C
      ERRMSG = ' '
      TMPNUM = 0
C
C     Assume that the number is nonnegative.
C
      NEGTIV = .FALSE.
C
C     Skip any leading white space. We know that there is at least
C     one nonblank character at this point, so we will not loop
C     off the end of the string.
C
      STRBEG = 1
 
      DO WHILE ( ICHAR( STRING(STRBEG:STRBEG) ) .EQ. ISPACE )
 
         STRBEG = STRBEG + 1
 
      END DO
C
C     Now, we want to find the end of the significant portion of
C     the input string.
C
      STREND = STRBEG + 1
      MORE   = .TRUE.
 
      DO WHILE ( MORE )
 
         IF ( STREND .LE. LEN( STRING ) ) THEN
 
            IF ( STRING(STREND:) .NE. ' ' ) THEN
 
               STREND = STREND + 1
 
            ELSE
 
               MORE = .FALSE.
 
            END IF
 
         ELSE
 
            MORE = .FALSE.
 
         END IF
 
      END DO
C
C     At this point, STREND is one larger than the length of the
C     significant portion of the string because we incremented
C     its value after the test. We will subtract one from the
C     value of STREND so that it exactly represents the position
C     of the last significant character in the string.
C
      STREND = STREND - 1
C
C     Set the position pointer to the beginning of the significant
C     part, i.e., the nonblank part, of the string, because we are
C     now ready to try and parse the number.
C
      POS = STRBEG
C
C     The first character should be a plus sign, '+', a minus sign,
C     '-', or a digit, '0' - '9', 'A' - 'F', or 'a' - 'f'. Anything
C     else is bogus, and we will catch it in the main loop below.
C
C     If the character is a minus sign, we want to set the value of
C     NEGTIV to .TRUE. and increment the position.
C
C     If the character is a plus sign, we want to increment the
C     position.
C
      IF ( ICHAR( STRING(POS:POS) ) .EQ. IMINUS ) THEN
 
         NEGTIV = .TRUE.
         POS    = POS + 1
 
      ELSE IF ( ICHAR( STRING(POS:POS) ) .EQ. IPLUS ) THEN
 
         POS = POS + 1
 
      END IF
C
C     When we build up the number from the hexadecimal string we
C     need to treat nonnegative numbers differently from negative
C     numbers. This is because on many computers the minimum
C     integer is one less than the negation of the maximum integer.
C     Negative numbers are the ones which truly might cause
C     problems, because ABS(minimum integer) may equal ABS(maximum
C     integer) + 1, on some machines. For example, on many machines
C     with 32 bit numbers, INTMIN = -2147483648 and INTMAX =
C     2147483647.
C
C     Build up the number from the hexadecimal character string.
C
      IF ( NEGTIV ) THEN
 
         DO WHILE ( POS .LE. STREND )
 
            LETTER = ICHAR( STRING(POS:POS) )
 
            IF ( ( LETTER .GE. DIGBEG ) .AND.
     .           ( LETTER .LE. DIGEND ) ) THEN
 
               IDIGIT = LETTER - DIGBEG
 
            ELSE IF ( ( LETTER .GE. UCCBEG ) .AND.
     .                ( LETTER .LE. UCCEND ) ) THEN
 
               IDIGIT = 10 + LETTER - UCCBEG
 
            ELSE IF ( ( LETTER .GE. LCCBEG ) .AND.
     .                ( LETTER .LE. LCCEND ) ) THEN
 
               IDIGIT = 10 + LETTER - LCCBEG
 
            ELSE
 
               ERROR  = .TRUE.
               ERRMSG = 'ERROR: Illegal character ''#'''  //
     .                  ' encountered.'
               CALL REPMC( ERRMSG, '#', CHAR( LETTER ), ERRMSG )
               RETURN
 
            END IF
 
            IF ( TMPNUM .GT. MINI ) THEN
 
               TMPNUM = TMPNUM * BASE - IDIGIT
               POS    = POS + 1
 
            ELSE IF ( ( TMPNUM .EQ. MINI   ) .AND.
     .                ( IDIGIT .LE. MINMOD ) ) THEN
 
               TMPNUM = TMPNUM * BASE - IDIGIT
               POS    = POS + 1
 
            ELSE
 
               ERROR  = .TRUE.
               ERRMSG = 'ERROR: Integer too small'  //
     .                  ' to be represented.'
               RETURN
 
            END IF
 
         END DO
 
      ELSE
 
         DO WHILE ( POS .LE. STREND )
 
            LETTER = ICHAR( STRING(POS:POS) )
 
            IF ( ( LETTER .GE. DIGBEG ) .AND.
     .           ( LETTER .LE. DIGEND ) ) THEN
 
               IDIGIT = LETTER - DIGBEG
 
            ELSE IF ( ( LETTER .GE. UCCBEG ) .AND.
     .                ( LETTER .LE. UCCEND ) ) THEN
 
               IDIGIT = 10 + LETTER - UCCBEG
 
            ELSE IF ( ( LETTER .GE. LCCBEG) .AND.
     .                ( LETTER .LE. LCCEND) ) THEN
 
               IDIGIT = 10 + LETTER - LCCBEG
 
            ELSE
 
               ERROR  = .TRUE.
               ERRMSG = 'ERROR: Illegal character ''#'''  //
     .                  ' encountered.'
               CALL REPMC( ERRMSG, '#', CHAR( LETTER ), ERRMSG )
               RETURN
 
            END IF
 
            IF ( TMPNUM .LT. MAXI ) THEN
 
               TMPNUM = TMPNUM * BASE + IDIGIT
               POS    = POS + 1
 
            ELSE IF ( ( TMPNUM .EQ. MAXI   ) .AND.
     .                ( IDIGIT .LE. MAXMOD ) ) THEN
 
               TMPNUM = TMPNUM * BASE + IDIGIT
               POS    = POS + 1
 
            ELSE
 
               ERROR  = .TRUE.
               ERRMSG = 'ERROR: Integer too large'  //
     .                  ' to be represented.'
               RETURN
 
            END IF
 
         END DO
 
      END IF
C
C     If we got to here, we have successfully parsed the hexadecimal
C     string into an integer. Set the value and return.
C
      NUMBER = TMPNUM
 
      RETURN
      END
