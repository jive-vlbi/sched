 
C$Procedure  DP2HX  ( D.p. number to hexadecimal string )
 
      SUBROUTINE DP2HX ( NUMBER, STRING, LENGTH )
 
C$ Abstract
C
C     Convert a double precision number to an equivalent character
C     string using a base 16 ``scientific notation.''
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
 
      DOUBLE PRECISION      NUMBER
      CHARACTER*(*)         STRING
      INTEGER               LENGTH
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NUMBER     I   D.p. number to be converted.
C     STRING     O   Equivalent character string, left justified.
C     LENGTH     O   Length of the character string produced.
C
C$ Detailed_Input
C
C     NUMBER   The double precision number to be converted to a
C              character string representation.
C
C$ Detailed_Output
C
C     STRING   The character string produced by this routine which
C              represents NUMBER in a base 16 ``scientific notation,''
C              e.g.:
C
C                 672.0 = '2A^3' = ( 2/16 + 10/( 16**2 ) ) * 16**3
C
C              and
C
C                 -11.0 = '-B^1' = - ( 11/16 ) * 16**1.
C
C              The following table describes the character set used to
C              represent the hexadecimal digits and their corresponding
C              values.
C
C                   Character    Value         Character    Value
C                   ---------    ------        ---------    ------
C                     '0'         0.0D0          '8'         8.0D0
C                     '1'         1.0D0          '9'         9.0D0
C                     '2'         2.0D0          'A'        10.0D0
C                     '3'         3.0D0          'B'        11.0D0
C                     '4'         4.0D0          'C'        12.0D0
C                     '5'         5.0D0          'D'        13.0D0
C                     '6'         6.0D0          'E'        14.0D0
C                     '7'         7.0D0          'F'        15.0D0
C
C              The carat, or hat, character, '^', is used to
C              distinguish the exponent.
C
C              The plus sign, '+', and the minus sign, '-', are used,
C              and they have their usual meanings.
C
C              In order to obtain the entire character string produced
C              by this routine, the output character string should be
C              at least N characters long, where
C
C
C                        # of bits per double precision mantissa + 3
C              N = 3 + ----------------------------------------------
C                                            4
C
C                        # of bits per double precision exponent + 3
C                    + ---------------------------------------------- .
C                                            4
C
C              There should be one character position for the sign of
C              the mantissa, one for the sign of the exponent, one for
C              the exponentiation character, and one for each
C              hexadecimal digit that could be produced from a mantissa
C              and an exponent.
C
C              The following table contains minimum output string
C              lengths necessary to obtain the complete character
C              string produced by this routine for some typical
C              implementations of double precision numbers.
C
C              Double precision number
C              Size Mantissa Exponent    Minimum output string length
C              bits   bits     bits
C              ---- -------- --------    ----------------------------
C              64   48       15          3 + 12 + 4 = 19
C              64   55+1     8           3 + 14 + 2 = 19 (VAX)
C              64   52       11          3 + 13 + 3 = 19 (IEEE)
C
C              The base 16 ``scientific notation'' character string
C              produced by this routine will be left justified and
C              consist of a contiguous sequence of characters with one
C              of following formats:
C
C                  (1)   h h h h  ... h ^H H  ... H
C                         1 2 3 4      n  1 2      m
C
C                  (2)   -h h h h  ... h ^H H  ... H
C                          1 2 3 4      n  1 2      m
C
C                  (3)   h h h h  ... h ^-H H  ... H
C                         1 2 3 4      n   1 2      m
C
C                  (4)   -h h h h  ... h ^-H H  ... H
C                          1 2 3 4      n   1 2      m
C
C              where
C
C                 h   and  H   denote hexadecimal digits
C                  i        j
C
C                 '^'          denotes exponentiation ( base 16 )
C
C              and
C
C                 '+' and '-'  have their usual interpretations.
C
C              The character string produced will be blank padded on
C              the right if LENGTH < LEN( STRING ).
C
C     LENGTH   Length of the base 16 ``scientific notation'' character
C              string produced by this routine.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)   If the output character string is not long enough to
C          contain the entire character string that was produced,
C          the string will be truncated on the right.
C
C     2)   If LEN( STRING ) > LENGTH, the output character string will
C          be blank padded on the right.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine converts a double precision number into an equivalent
C     character string using a base 16 ``scientific notation.'' This
C     representation allows the full precision of a number to be placed
C     in a format that is suitable for porting or archival storage.
C
C     This routine is one of a pair of routines which are used to
C     perform conversions between double precision numbers and
C     an equivalent base 16 ``scientific notation'' character string
C     representation:
C
C           DP2HX  -- Convert a double precision number into a base 16
C                     ``scientific notation'' character string.
C
C           HX2DP  -- Convert a base 16 ``scientific notation''
C                     character string into a double precision number.
C
C$ Examples
C
C     The following input and output argument values illustrate the
C     action of DP2HX for various input values of NUMBER.
C
C     Note: The hat or carat, '^', signals an exponent.
C
C         NUMBER             STRING                         LENGTH
C         -----------------  -----------------------------  ------
C              2.0D-9         89705F4136B4A6^-7             17
C              1.0D0          1^1                           3
C             -1.0D0         -1^1                           4
C           1024.0D0          4^3                           3
C          -1024.0D0         -4^3                           4
C         521707.0D0          7F5EB^5                       7
C             27.0D0          1B^2                          4
C              0.0D0          0^0                           3
C
C$ Restrictions
C
C     The maximum number of characters permitted in the output string
C     is specified by the local parameter STRLEN.
C
C$ Author_and_Institution
C
C     K.R. Gehringer   (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 10-MAR-1994 (KRG)
C
C        Fixed a typo in the description of the input argument STRING.
C        The example showing the expansion of 160 into hexadecimal
C        was incorrect. 160 was replaced with 672 which makes the
C        example correct.
C
C-    SPICELIB Version 1.0.0, 26-OCT-1992 (KRG)
C
C-&
 
C$ Index_Entries
C
C     convert d.p. to signed normalized hexadecimal string
C     convert d.p. number to encoded d.p. number
C     convert d.p. to base 16 scientific notation
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.0.1, 10-MAR-1994 (KRG)
C
C        Fixed a typo in the description of the input argument STRING.
C        The example showing the expansion of 160 into hexadecimal
C        was incorrect. 160 was replaced with 672 which makes the
C        example correct.
C
C        Old Example:
C
C           160.0 = '2A^3' = ( 2/16 + 10/( 16**2 ) ) * 16**3
C
C        New Example:
C
C           672.0 = '2A^3' = ( 2/16 + 10/( 16**2 ) ) * 16**3
C
C-&
 
 
C
C     Local Parameters
C
      CHARACTER*(1)         EXPCHR
      PARAMETER           ( EXPCHR = '^')
 
      DOUBLE PRECISION      BASE
      PARAMETER           ( BASE = 16.0D0 )
 
      DOUBLE PRECISION      INVBAS
      PARAMETER           ( INVBAS = 1.0D0/BASE )
 
      INTEGER               EXPINB
      PARAMETER           ( EXPINB = 1 )
 
      DOUBLE PRECISION      FACTR1
      PARAMETER           ( FACTR1 = 4294967296.0D0 )
 
      DOUBLE PRECISION      INVFC1
      PARAMETER           ( INVFC1 = 1.0D0/FACTR1 )
 
      INTEGER               EXPIN1
      PARAMETER           ( EXPIN1 = 8 )
 
      DOUBLE PRECISION      FACTR2
      PARAMETER           ( FACTR2 = 65536.0D0 )
 
      DOUBLE PRECISION      INVFC2
      PARAMETER           ( INVFC2 = 1.0D0/FACTR2 )
 
      INTEGER               EXPIN2
      PARAMETER           ( EXPIN2 = 4 )
 
      INTEGER               STRLEN
      PARAMETER           ( STRLEN = 255 )
 
      INTEGER               MAXDIG
      PARAMETER           ( MAXDIG = 16 )
C
C     Local variables
C
      CHARACTER*(1)         DIGITS(0:MAXDIG-1)
      CHARACTER*(STRLEN)    EXPSTR
      CHARACTER*(STRLEN)    TMPSTR
 
      DOUBLE PRECISION      REMNDR
      DOUBLE PRECISION      TMPNUM
 
      INTEGER               EXPLEN
      INTEGER               INTEXP
      INTEGER               POSITN
      INTEGER               RESULT
 
      LOGICAL               NEGTIV
      LOGICAL               POSTIV
C
C     Saved variables
C
      SAVE                  DIGITS
C
C     Initial values
C
      DATA                  DIGITS /
     .                               '0', '1', '2', '3',
     .                               '4', '5', '6', '7',
     .                               '8', '9', 'A', 'B',
     .                               'C', 'D', 'E', 'F'
     .                             /
 
 
C
C     Make a copy of the input so that it will not be changed by this
C     routine. Also, assume that we do not know the sign of the number.
C
      TMPNUM = NUMBER
      NEGTIV = .FALSE.
      POSTIV = .FALSE.
C
C     Check to see what the sign of the number is, because we treat
C     negative numbers, positive numbers and zero separately. This
C     simplifies the testing in the loop boundaries a bit, and removes
C     calls to DABS() that would otherwise have been necessary.
C
C     Set the appropriate logical flag for the sign of the input number.
C
      IF ( TMPNUM .LT. 0.0D0 ) THEN
 
         NEGTIV = .TRUE.
 
      ELSE IF ( TMPNUM .GT. 0.0D0 ) THEN
 
         POSTIV = .TRUE.
 
      END IF
C
C     If nonzero, a double precision number is first normalized,
C     so that it has a value between 1.0D0/BASE and 1.0D0 or -1.0D0
C     and -1/BASE. The hexadecimal digits in the mantissa  are found
C     by repeated applications of multiplication and truncation
C     operations. The hexadecimal digits will be in the correct order
C     when finished. The string will be left justified, and its length
C     will be set before returning.
C
C     Calculate the exponent of the number using multiple scaling
C     levels. The different scale factors, 16**8, 16**4, and 16,
C     provide a significant speed improvement for the normalization
C     process.
C
      INTEXP = 0
      IF ( NEGTIV ) THEN
 
         IF ( TMPNUM .GT. -1.0D0 ) THEN
C
C           ABS(TMPNUM) .LT. 1.0
C
            DO WHILE ( FACTR1 * TMPNUM .GT. -1.0D0 )
C
C              Scale the number and decrement the exponent.
C
               TMPNUM = TMPNUM * FACTR1
               INTEXP = INTEXP - EXPIN1
 
            END DO
 
            DO WHILE ( FACTR2 * TMPNUM .GT. -1.0D0 )
C
C              Scale the number and decrement the exponent.
C
               TMPNUM = TMPNUM * FACTR2
               INTEXP = INTEXP - EXPIN2
 
            END DO
 
            DO WHILE ( ( BASE * TMPNUM ) .GT. -1.0D0 )
C
C              Scale the number and decrement the exponent.
C
               TMPNUM = TMPNUM * BASE
               INTEXP = INTEXP - EXPINB
 
            END DO
C
C           At this point, -1 < TMPNUM <= -1/BASE.
C
         ELSE
C
C           ABS(TMPNUM) .GE. 1.0
C
            DO WHILE ( INVFC1 * TMPNUM .LE. -1.0D0 )
C
C              Scale the number and increment the exponent.
C
               TMPNUM = TMPNUM * INVFC1
               INTEXP = INTEXP + EXPIN1
 
            END DO
 
            DO WHILE ( INVFC2 * TMPNUM .LE. -1.0D0 )
C
C              Scale the number and increment the exponent.
C
               TMPNUM = TMPNUM * INVFC2
               INTEXP = INTEXP + EXPIN2
 
            END DO
 
            DO WHILE ( TMPNUM .LE. -1.0D0 )
C
C              Scale the number and increment the exponent.
C
               TMPNUM = TMPNUM * INVBAS
               INTEXP = INTEXP + EXPINB
 
            END DO
C
C           At this point, -1 < TMPNUM <= -1/BASE.
C
         END IF
 
      ELSE IF ( POSTIV ) THEN
 
         IF ( TMPNUM .LT. 1.0D0 ) THEN
C
C           ABS(TMPNUM) .LT. 1.0
C
            DO WHILE ( FACTR1 * TMPNUM .LT. 1.0D0 )
C
C              Scale the number and decrement the exponent.
C
               TMPNUM = TMPNUM * FACTR1
               INTEXP = INTEXP - EXPIN1
 
            END DO
 
            DO WHILE ( FACTR2 * TMPNUM .LT. 1.0D0 )
C
C              Scale the number and decrement the exponent.
C
               TMPNUM = TMPNUM * FACTR2
               INTEXP = INTEXP - EXPIN2
 
            END DO
 
            DO WHILE ( ( BASE * TMPNUM ) .LT. 1.0D0 )
C
C              Scale the number and decrement the exponent.
C
               TMPNUM = TMPNUM * BASE
               INTEXP = INTEXP - EXPINB
 
            END DO
C
C           At this point, 1/BASE <= TMPNUM < 1
C
         ELSE
C
C           ABS(TMPNUM) .GE. 1.0
C
            DO WHILE ( INVFC1 * TMPNUM .GE. 1.0D0 )
C
C              Scale the number and increment the exponent.
C
               TMPNUM = TMPNUM * INVFC1
               INTEXP = INTEXP + EXPIN1
 
            END DO
 
            DO WHILE ( INVFC2 * TMPNUM .GE. 1.0D0 )
C
C              Scale the number and increment the exponent.
C
               TMPNUM = TMPNUM * INVFC2
               INTEXP = INTEXP + EXPIN2
 
            END DO
 
            DO WHILE ( TMPNUM .GE. 1.0D0 )
C
C              Scale the number and increment the exponent.
C
               TMPNUM = TMPNUM * INVBAS
               INTEXP = INTEXP + EXPINB
 
            END DO
C
C           At this point, 1/BASE <= TMPNUM < 1
C
         END IF
 
      END IF
C
C     We do different things for the cases where the number to be
C     converted is positive, negative, or zero.
C
      IF ( NEGTIV ) THEN
C
C        Set the beginning position.
C
         POSITN = 1
C
C        Put the minus sign in place.
C
         TMPSTR(POSITN:POSITN) = '-'
C
C        Start with the remainder equal to the normalized value of the
C        original number.
C
         REMNDR = TMPNUM
C
C        Collect all of the digits in the string.
C
C        This stopping test works because the base is a power of
C        2 and the mantissa is composed of a sum of powers of 2.
C
         DO WHILE ( REMNDR .NE. 0.0D0 )
C
C           -1 < REMNDR <= -1/BASE
C
            POSITN                 = POSITN + 1
            TMPNUM                 = REMNDR * BASE
            RESULT                 = INT( TMPNUM )
            REMNDR                 = TMPNUM - DBLE( RESULT )
            TMPSTR (POSITN:POSITN) = DIGITS(-RESULT)
 
         END DO
C
C        Put the exponent on the end of the number and update the
C        position.
C
         CALL INT2HX( INTEXP, EXPSTR, EXPLEN )
         TMPSTR(POSITN+1:) = EXPCHR//EXPSTR(1:EXPLEN)
         POSITN            = POSITN + EXPLEN + 1
 
      ELSE IF ( POSTIV ) THEN
C
C        Set the beginning position.
C
         POSITN = 0
C
C        Start with the remainder equal to the normalized value of the
C        original number.
C
         REMNDR = TMPNUM
C
C        Collect all of the digits in the string.
C
C        This stopping test works because the base is a power of
C        2 and the mantissa is composed of a sum of powers of 2.
C
         DO WHILE ( REMNDR .NE. 0.0D0 )
C
C           1/BASE <= REMNDR < 1
C
            POSITN                 = POSITN + 1
            TMPNUM                 = REMNDR * BASE
            RESULT                 = INT( TMPNUM )
            REMNDR                 = TMPNUM - DBLE( RESULT )
            TMPSTR (POSITN:POSITN) = DIGITS(RESULT)
 
         END DO
C
C        Put the exponent on the end of the number and update the
C        position.
C
         CALL INT2HX( INTEXP, EXPSTR, EXPLEN )
         TMPSTR(POSITN+1:) = EXPCHR//EXPSTR(1:EXPLEN)
         POSITN            = POSITN + EXPLEN + 1
 
      ELSE
C
C        Treat zero as a special case, because it's easier.
C
         POSITN      = 3
         TMPSTR(1:3) = '0^0'
 
      END IF
C
C     Set the value for the length of the character string produced
C     before returning.
C
      LENGTH = POSITN
C
C     Set the value of the output string before returning. Let the
C     Fortran string assignment deal with the left justification, and
C     the truncation on the right if STRING is not long enough to
C     contain all of the characters produced.
C
      STRING = TMPSTR(:LENGTH)
 
      RETURN
      END
