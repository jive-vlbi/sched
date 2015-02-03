C$Procedure            BEDEC  ( Be a decimal number? )
 
      LOGICAL FUNCTION BEDEC  ( STRING )
 
C$ Abstract
C
C     Determine whether a string represents a decimal number.
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
C     WORDS
C
C$ Keywords
C
C     ALPHANUMERIC
C     NUMBERS
C     SCANNING
C     UTILITY
C
C$ Declarations
 
      CHARACTER*(*)         STRING
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     STRING     I   Character string.
C
C     The function returns TRUE if the string represents a decimal
C     number. Otherwise, it returns FALSE.
C
C$ Detailed_Input
C
C     STRING      is any string.
C
C$ Detailed_Output
C
C     If the input string contains a decimal number (as defined
C     in $Particulars below), the function returns TRUE. Otherwise,
C     the functions returns FALSE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     A decimal number may be constructed by concatenating
C     the following components in the order shown.
C
C        1) A sign ('+' or '-'), or the null string.
C
C        2) An unsigned integer (as defined by function BEUNS),
C           or the null string.
C
C        3) A decimal point, or the null string.
C
C        4) An unsigned integer, or the null string.
C
C$ Examples
C
C     Four classes of numbers recognized by the various BE functions.
C
C        UNS      unsigned integer
C        INT      integer                (includes INT)
C        DEC      decimal number         (includes UNS, INT)
C        NUM      number                 (includes UNS, INT, NUM)
C
C     The following table illustrates the differences between
C     the classes. (Any number of leading and trailing blanks
C     are acceptable.)
C
C        String                  Accepted by
C        ------------------      ------------------
C        0                       UNS, INT, DEC, NUM
C        21
C        21994217453648
C
C        +0                      INT, DEC, NUM
C        -13
C        +21946
C
C        1.23                    DEC, NUM
C        12.
C        .17
C        +4.1
C        -.25
C
C        2.3e17                  NUM
C        17.D-13275849
C        -.194265E+0004
C
C     Note that the functions don't take the magnitudes of the numbers
C     into account. They may accept numbers that cannot be represented
C     in Fortran variables. (For example, '2.19E999999999999' probably
C     exceeds the maximum floating point number on any machine, but
C     is perfectly acceptable to BENUM.)
C
C     The following strings are not accepted by any of the functions.
C
C        String             Reason
C        ---------------    ----------------------------------------
C        3/4                No implied operations (rational numbers)
C        37+14              No explicit operations
C        E12                Must have mantissa
C        217,346.91         No commas
C        3.14 159 264       No embedded spaces
C        PI                 No special numbers
C        FIVE               No textual numbers
C        CXIV               No roman numerals
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
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 01-DEC-1995 (WLT)
C
C-&
 
C$ Index_Entries
C
C     determine if a string is a decimal number
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               BEINT
      LOGICAL               BEUNS
      INTEGER               POS
 
C
C     Local Variables
C
      INTEGER               C
      INTEGER               D
      INTEGER               E
      INTEGER               L
 
 
C
C     First determine whether or not a decimal point is present.
C
      D = POS ( STRING, '.', 1 )
      C = D-1
      E = D+1
 
      IF ( D .EQ. 0 ) THEN
 
C
C        If there is no decimal point just apply the integer test.
C
         BEDEC = BEINT ( STRING )
 
      ELSE
 
C
C        A decimal point is present, get the length of the string
C        and see where the decimal point is relative to the last
C        character.
C
         L = LEN(STRING)
 
         IF ( L .EQ. 1 ) THEN
 
C
C           The string is one character long and a decimal point.
C           Sorry, this is not a decimal number.
C
            BEDEC = .FALSE.
 
         ELSE IF ( D .EQ. 1 ) THEN
 
C
C           The decimal point occurs as the first character of the
C           string.  The string following it must begin with
C           a non-blank character and be an unsigned integer.
C
            BEDEC =       STRING(E:E) .NE. ' '
     .               .AND. BEUNS  ( STRING(E:) )
 
         ELSE IF ( D .EQ. L ) THEN
 
C
C           The decimal point is the last character of the string.
C           The character that precedes it must be non-blank and
C           the substring to the left must be an integer.
C
            BEDEC =       STRING(C:C) .NE. ' '
     .               .AND. BEINT ( STRING(1:C) )
 
         ELSE IF ( STRING(C:C) .EQ. ' ' ) THEN
 
C
C           The decimal point occurs somewhere in the middle of the
C           string and the character preceding it is blank.
C
            BEDEC =        STRING(E:E) .NE. ' '
     .               .AND. STRING(1:C) .EQ. ' '
     .               .AND. BEUNS  ( STRING(E:) )
 
         ELSE IF ( STRING(E:E) .EQ. ' ' ) THEN
 
C
C           Again the decimal point occurs somewhere in the middle of
C           the string and the character following it is blank.
C
            BEDEC =        STRING(E:L) .EQ. ' '
     .               .AND. STRING(C:C) .NE. ' '
     .               .AND. BEINT ( STRING(1:C) )
 
         ELSE IF ( STRING(C:C) .EQ. '-' .OR. STRING(C:C) .EQ. '+' ) THEN
 
C
C           The decimal point is in the middle of the string and
C           is preceded by a '+' or '-'.  There should be nothing
C           preceeding the sign and what follows the decimal point
C           should be an unsigned integer. (we already know that the
C           character following the decimal point is not a blank)
C
            IF ( C .EQ. 1 ) THEN
               BEDEC = BEUNS (STRING(E:L))
            ELSE
               BEDEC =        BEUNS ( STRING(E:L) )
     .                  .AND. STRING(1:C-1) .EQ. ' '
            END IF
 
         ELSE
 
C
C            Last chance, the decimal point is in the middle of the
C            string.  The characters to the right and left of the
C            point are non-blank and we know the character to the
C            left of the point is not a sign.  The string left must
C            be an integer, the string to the right must be an
C            unsigned integer.
C
             BEDEC =       BEINT  ( STRING(1:C) )
     .               .AND. BEUNS  ( STRING(E:L) )
 
         END IF
 
      END IF
 
      RETURN
      END
