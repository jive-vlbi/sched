 
C$Procedure  INT2HX  ( Integer to signed hexadecimal string )
 
      SUBROUTINE INT2HX ( NUMBER, STRING, LENGTH )
 
 
C$ Abstract
C
C     Convert an integer to an equivalent signed hexadecimal string.
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
 
      INTEGER               NUMBER
      CHARACTER*(*)         STRING
      INTEGER               LENGTH
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NUMBER     I   Integer to be converted.
C     STRING     O   Equivalent hexadecimal string, left justified.
C     LENGTH     O   The length of the hexadecimal string produced.
C
C$ Detailed_Input
C
C     NUMBER   The integer to be converted.
C
C$ Detailed_Output
C
C     STRING   The signed hexadecimal string representing the integer
C              NUMBER.
C
C              The following table describes the character set used
C              to represent the hexadecimal digits and their
C              corresponding values.
C
C              Character    Value           Character    Value
C              ---------    -----           ---------    -----
C                '0'          0                '8'          8
C                '1'          1                '9'          9
C                '2'          2                'A'         10
C                '3'          3                'B'         11
C                '4'          4                'C'         12
C                '5'          5                'D'         13
C                '6'          6                'E'         14
C                '7'          7                'F'         15
C
C              In order to obtain the entire signed hexadecimal number,
C              the output character string should be at least N
C              characters long, where
C
C                              # of bits per integer + 3
C                    N = 1 + ---------------------------- .
C                                         4
C
C              There should be 1 character position for the sign, and
C              one character position for each hexadecimal digit that
C              could be produced from any integer which can be
C              represented by a particular computer system.
C
C              The following table contains minimum output string
C              lengths necessary to obtain the complete hexadecimal
C              string for various integer sizes.
C
C                 Integer size in bits      Minimum output length
C                 --------------------      ---------------------
C                 8                         3
C                 16                        5
C                 32                        9
C                 36 (really,it exists)     10
C                 64                        17
C                 etc.
C
C              The hexadecimal character string produced by this
C              routine will be left justified and consist of a
C              contiguous sequence of hexadecimal digits, or in the
C              case of a negative number, a contiguous sequence of
C              hexadecimal digits immediately preceded by a minus
C              sign, '-', e.g.:
C
C                 (1)   h h ... h
C                        1 2     n
C
C                 (2)   -h h ... h
C                         1 2     n
C
C              where h  represents an hexadecimal digit.
C                     i
C
C              The character string produced will be blank padded on
C              the right if LENGTH < LEN( STRING ).
C
C     LENGTH   The length of the hexadecimal character string produced
C              by the conversion.
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
C          contain the entire hexadecimal string that was produced,
C          the hexadecimal string will be truncated on the right.
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
C     This routine will convert a signed integer into an equivalent
C     signed hexadecimal character string. This provides a machine
C     independent mechanism for storing or porting integer values.
C     This routine is used by the routine DP2HX which converts a
C     double precision value into an equivalent character string.
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
C     All of the values shown are for a two's complement representation
C     for signed integers.
C
C     The following input and output argument values illustrate the
C     action of INT2HX for various input values of NUMBER.
C
C         NUMBER       STRING           LENGTH
C         -----------  ---------------  ------
C          1           '1'              1
C         -1           '-1'             2
C          223         'DF'             2
C         -32          '-20'            3
C          0           '0'              1
C
C          2147483647  '7FFFFFFF'       8
C          (Maximum 32 bit integer)
C
C         -2147483647  '-7FFFFFFF'      9
C          (Minimum 32 bit integer + 1)
C
C         -2147483648  '-80000000'      9
C          (Minimum 32 bit integer)
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
C-    SPICELIB Version 1.0.0, 22-OCT-1992 (KRG)
C
C-&
 
C$ Index_Entries
C
C     convert integer to signed hexadecimal string
C
C-&
 
 
C
C     Local Parameters
C
      INTEGER               BASE
      PARAMETER           ( BASE   = 16 )
 
      INTEGER               MAXDIG
      PARAMETER           ( MAXDIG = 16 )
 
      INTEGER               STRLEN
      PARAMETER           ( STRLEN = 255 )
C
C     Local variables
C
      CHARACTER*(1)         DIGITS(0:MAXDIG-1)
      CHARACTER*(STRLEN)    TMPSTR
 
      INTEGER               BEGIN
      INTEGER               ITEMP
      INTEGER               REMNDR
      INTEGER               RESULT
C
C     Saved variables
C
      SAVE                  DIGITS
C
C     Local variables
C
      DATA                  DIGITS /
     .                               '0', '1', '2', '3',
     .                               '4', '5', '6', '7',
     .                               '8', '9', 'A', 'B',
     .                               'C', 'D', 'E', 'F'
     .                             /
 
 
C
C     The hexadecimal digits in the integer are found by repeated
C     applications of the "modulus" and division operations. We fill
C     the string in reverse order so that the digits are in the
C     correct order when we have finished building the string. We then
C     left justify the resulting string and set the value for its
C     length before returning.
C
C     Make a copy of the input so that it will not be changed by this
C     routine.
C
      ITEMP = NUMBER
C
C     We need to do different things for the cases where the integer to
C     be converted is positive, negative, or zero. ( Actually, the
C     positive case and the zero case are the same, but since we can
C     test for integer zero exactly it will save a few arithmetic
C     operations if we treat it as a special case. ) The case for a
C     negative integer is the only one which truly might cause problems,
C     because ABS(minimum integer) may equal ABS(maximum integer) + 1,
C     on some machines. For example, on many machines with 32 bit
C     integers, INTMIN = -2147483648 and INTMAX = 2147483647.
C
C     Set the beginning position of the hexadecimal number to be
C     one past the end of the character string that will hold the
C     hexadecimal representation of the input number. Before each
C     digit of the hexadecimal number is inserted into the character
C     string, the beginning position is decremented, so we always know
C     exactly where the hexadecimal string begins. This simplifies the
C     calculation of the length of the hexadecimal character string at
C     the end of the routine.
C
      BEGIN = STRLEN + 1
 
      IF ( ITEMP .LT. 0 ) THEN
C
C        Collect all of the digits in the string. We know we're done
C        when the value of ITEMP is equal to zero, thanks to the fact
C        that integer arithmetic operations are exact.
C
         DO WHILE ( ITEMP .NE. 0 )
 
            BEGIN               = BEGIN - 1
            RESULT              = ITEMP / BASE
            REMNDR              = RESULT * BASE - ITEMP
            ITEMP               = RESULT
            TMPSTR(BEGIN:BEGIN) = DIGITS(REMNDR)
 
         END DO
C
C        Put the minus sign in place.
C
         BEGIN               = BEGIN - 1
         TMPSTR(BEGIN:BEGIN) = '-'
 
      ELSE IF ( ITEMP .GT. 0 ) THEN
C
C        Collect all of the digits in the string. We know we're done
C        when the value of ITEMP is equal to zero, thanks to the fact
C        that integer arithmetic operations are exact.
C
         DO WHILE ( ITEMP .NE. 0 )
 
            BEGIN               = BEGIN - 1
            RESULT              = ITEMP / BASE
            REMNDR              = ITEMP - RESULT * BASE
            ITEMP               = RESULT
            TMPSTR(BEGIN:BEGIN) = DIGITS(REMNDR)
 
         END DO
 
      ELSE
C
C        Treat zero as a special case, because it's easier.
C
         BEGIN               = BEGIN - 1
         TMPSTR(BEGIN:BEGIN) = DIGITS(0)
 
      END IF
C
C     Set the value of the output string before returning. Let the
C     Fortran string assignment deal with the left justification, and
C     the truncation on the right if the output string STRING is not
C     long enough to contain all of the characters in the string
C     that was produced.
C
      STRING = TMPSTR(BEGIN:)
C
C     Also, set the value for the length of the hexadecimal string
C     before returning.
C
      LENGTH = STRLEN - BEGIN + 1
 
      RETURN
      END
