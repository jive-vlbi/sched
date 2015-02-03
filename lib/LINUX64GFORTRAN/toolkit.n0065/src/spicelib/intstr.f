C$Procedure  INTSTR  ( Integer to character string )
 
      SUBROUTINE INTSTR ( NUMBER, STRING )
 
C$ Abstract
C
C     Convert an integer to an equivalent character string.
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
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NUMBER     I   Integer to be converted.
C     STRING     O   Equivalent character string, left justified.
C
C$ Detailed_Input
C
C     NUMBER   The integer to be converted into a character string.
C
C$ Detailed_Output
C
C     STRING   The character string representing the integer NUMBER.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)   If the output character string is not large enough to
C          contain the entire character string produced, the output
C          character string will be truncated on the right.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine will convert a signed integer into an equivalent
C     decimal character string representation. The decimal digits of
C     the integer NUMBER are found by repeated applications of
C     "modulus" and division operations.
C
C$ Examples
C
C     The following argument values illustrate the use of INTSTR.
C
C         NUMBER        STRING
C         ------------  ---------------------
C          1            '-1'
C         -1            '-1'
C          223          '223'
C         -32           '-32'
C          0            '0'
C          2147483647   '2147483647'   ( Maximum 32 bit integer )
C         -2147483647   '-2147483647'  ( Minimum 32 bit integer + 1 )
C         -2147483647   '-2147483648'  ( Minimum 32 bit integer )
C
C$ Restrictions
C
C     This routine assumes that all signed integers will fit into a
C     character string with LINLEN or fewer digits. See the parameter
C     LINLEN below for the current value.
C
C$ Author_and_Institution
C
C     K.R. Gehringer   (JPL)
C     W.L. Taber       (JPL)
C     I.M. Underwood   (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 2.1.0, 11-MAY-1993 (HAN) (MJS)
C
C        DATA statement came before the SAVE statement. This is
C        a violation of the ANSI Standard. It is now the other way
C        around.
C
C-    SPICELIB Version 2.0.0, 14-OCT-1992 (KRG)
C
C        The routine was rewritten to fix a bug concerning the minimum
C        representable integer.
C
C        This routine used to negate a negative number before it began
C        generating its digits. This was a bad thing to do, because on
C        many machines the minimum representable integer and the
C        maximum representable integer have the following relationship:
C
C           ABS( minimum integer ) = 1 + ABS( maximum integer ).
C
C        Changing the sign of a negative number before converting it
C        to a character string would cause a program to crash if it
C        were attempting to convert the minimum representable integer
C        into a character string.
C
C-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.1   7-DEC-1990 (WLT)
C
C        References to the old name INT2CH were removed and
C        an exception added to that section.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (WLT)
C
C-&
 
C$ Index_Entries
C
C     convert integer to character string
C
C-&
 
 
C
C     Local Parameters
C
      INTEGER               BASE
      PARAMETER           ( BASE = 10 )
 
      INTEGER               STRLEN
      PARAMETER           ( STRLEN = 80 )
C
C     Local variables
C
      CHARACTER*(STRLEN)    TMPSTR
 
      CHARACTER*(1)         DIGITS(0:9)
 
      INTEGER               TMPNUM
      INTEGER               RESULT
      INTEGER               REMNDR
 
      INTEGER               I
 
C
C    Saved values
C
      SAVE DIGITS
 
 
      DATA                  DIGITS /
     .                               '0', '1', '2', '3',
     .                               '4', '5', '6', '7',
     .                               '8', '9'
     .                             /
 
 
 
C
C     The digits are generated in reverse order, so we fill the
C     character string in reverse order, from `right' to `left',
C     so that the digits are in the correct order when we are
C     done converting the integer. This is to avoid reversing the
C     character string before returning. The output character
C     string is then left justified upon exit.
C
C     Make a copy of the input so that it will not be modified.
C
      TMPNUM = NUMBER
C
C     Initialize the temporary character buffer used to store the
C     character string as it is generated to blanks.
C
      TMPSTR = ' '
C
C     We need to do different things for the cases where the number to
C     be converted is positive, negative, or zero. ( Actually, the
C     positive case and the zero case are the same, but since we can
C     test for integer zero exactly it will save a few arithmetic
C     operations if we treat it as a special case. ) The case for a
C     negative number is the only one which truly might cause problems,
C     because ABS(minimum integer) may equal ABS(maximum integer) + 1.
C     For 32 bit numbers, INTMIN = -214748368 and INTMAX = 214748367.
C     You should be able to see the repercussions of this.
C
      I = LEN( TMPSTR ) + 1
      IF ( TMPNUM .LT. 0 ) THEN
C
C        Collect all of the digits in the string.
C
         DO WHILE ( TMPNUM .NE. 0 )
 
            I      = I - 1
            RESULT = TMPNUM / BASE
            REMNDR = RESULT * BASE - TMPNUM
            TMPNUM = RESULT
 
            TMPSTR (I:I) = DIGITS(REMNDR)
 
         END DO
C
C        Put the minus sign in place.
C
         I           = I - 1
         TMPSTR(I:I) = '-'
 
      ELSE IF ( TMPNUM .GT. 0 ) THEN
C
C        Collect all of the digits in the string.
C
         DO WHILE ( TMPNUM .NE. 0 )
 
            I      = I - 1
            RESULT = TMPNUM / BASE
            REMNDR = TMPNUM - RESULT * BASE
            TMPNUM = RESULT
 
            TMPSTR (I:I) = DIGITS(REMNDR)
 
         END DO
 
      ELSE
C
C        Treat zero as a special case, because it's easier.
C
         I           = I - 1
         TMPSTR(I:I) = DIGITS(0)
 
      END IF
C
C     Set the value of the output string before returning. Let the
C     Fortran string equals deal with the left justification, and the
C     truncation on the right if the string STRING is not long enough
C     to contain all of the characters necessary.
C
      STRING = TMPSTR(I:LEN(TMPSTR))
 
      RETURN
      END
