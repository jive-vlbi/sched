 
C$Procedure INTTXT ( Convert an integer to text )
 
      SUBROUTINE INTTXT ( N, STRING )
 
C$ Abstract
C
C     Convert an integer to an equivalent written phrase.
C     For example, convert 121 to 'ONE HUNDRED TWENTY-ONE'.
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
C     CONVERSION
C     PARSING
C     STRING
C     UNITS
C     UTILITY
C
C$ Declarations
 
      INTEGER               N
      CHARACTER*(*)         STRING
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     N          I   An integer (less than 10**12 in absolute value).
C     STRING     O   An English string representing the cardinal of N.
C
C$ Detailed_Input
C
C     N         is any integer (less than 10**12 in absolute value).
C               If N is less than 0, -N must be a legitimate number.
C
C$ Detailed_Output
C
C     STRING    is the English cardinal equivalent of N.  STRING will
C               contain only upper case letters.
C
C               The longest possible output string contains 145
C               characters. One such string is:
C
C                  'NEGATIVE '                                  //
C                  'SEVEN HUNDRED SEVENTY-SEVEN BILLION '       //
C                  'SEVEN HUNDRED SEVENTY-SEVEN MILLION '       //
C                  'SEVEN HUNDRED SEVENTY-SEVEN THOUSAND '      //
C                  'SEVEN HUNDRED SEVENTY-SEVEN'
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1) If the resulting text is longer than the output string,
C        it will be truncated on the right, leaving only the most
C        significant portion of the number.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is used primarily for constructing error messages.
C     For example, an overflow message might look like the following:
C
C        'An excess of seventy-four parameters was detected.'
C
C     A second use might be to write dollar amounts: it's much harder
C     to tamper with a string like
C
C        'Two thousand four hundred seventy-one dollars'
C
C     than with the equivalent string
C
C        '$ 2471.00'
C
C$ Examples
C
C     N           STRING
C     ------      ------------------------------------------
C     -43         NEGATIVE FORTY-THREE
C      1          ONE
C      2          TWO
C      3          THREE
C      4          FOUR
C      20         TWENTY
C      21         TWENTY-ONE
C      99         NINETY-NINE
C      82131      EIGHTY-TWO THOUSAND ONE HUNDRED THIRTY-ONE
C
C$ Restrictions
C
C      1) This routine assumes that N will always be less than
C         a trillion (10**12) in absolute value.
C
C      2) In the event that N is less than zero, this routine assumes
C         that -N is a legitimate integer on the host machine.
C
C      3) This routine assumes that an integer as large as 10**9
C         (one billion) is representable on the host machine.
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
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 15-AUG-1990 (WLT)
C
C-&
 
C$ Index_Entries
C
C     convert an integer to text
C
C-&
 
 
 
 
C
C     Local variables
C
      INTEGER               LNGEST
      PARAMETER           ( LNGEST = 9 )
 
      CHARACTER*(LNGEST)    NUMBER (19)
      CHARACTER*(LNGEST)    SUFF
      CHARACTER*(LNGEST)    TENS   ( 9)
 
      INTEGER               NUM
      INTEGER               PAD
      INTEGER               SPACE
      INTEGER               X
      INTEGER               Y
 
C
C     Saved variables
C
 
      SAVE                  NUMBER
      SAVE                  TENS
 
C
C     Initial values
C
      DATA       TENS    /  'TEN',     'TWENTY',   'THIRTY',
     .                      'FORTY',   'FIFTY',    'SIXTY',
     .                      'SEVENTY', 'EIGHTY',   'NINETY' /
 
      DATA       NUMBER  / 'ONE',         'TWO',
     .                     'THREE',       'FOUR',
     .                     'FIVE',        'SIX',
     .                     'SEVEN',       'EIGHT',
     .                     'NINE',        'TEN',
     .                     'ELEVEN',      'TWELVE',
     .                     'THIRTEEN',    'FOURTEEN',
     .                     'FIFTEEN',     'SIXTEEN',
     .                     'SEVENTEEN',   'EIGHTEEN',
     .                     'NINETEEN'                 /
 
 
C
C     Zero is easy.
C
      IF ( N .EQ. 0 ) THEN
 
         STRING = 'ZERO'
         RETURN
 
      END IF
 
C
C     If the number is negative, the string begins with the word
C     `NEGATIVE', and otherwise the number can be treated as though
C     it were positive.
C
      IF ( N .LT. 0 ) THEN
 
         NUM    = - N
         STRING = 'NEGATIVE'
 
      ELSE
 
         NUM    =   N
         STRING = ' '
 
      END IF
 
C
C     Construct the number portion, from left to right: billions,
C     then millions, and so on. In case of overflow, SUFFIX simply
C     leaves the output string unchanged, so there is no need to
C     check explicitly for truncation.
C
      DO WHILE ( NUM .GT. 0 )
 
C
C        Find the right unit (billion, million, or whatever),
C        and the number (X) of those units. X should always
C        be between zero and 999, regardless of the units.
C
         IF ( NUM .GE. 1000000000 ) THEN
 
            X    = NUM / 1000000000
            SUFF = 'BILLION'
            NUM  = NUM - X * 1000000000
 
         ELSE IF  ( NUM .GE. 1000000 ) THEN
 
            X    = NUM / 1000000
            SUFF = 'MILLION'
            NUM  = NUM - X * 1000000
 
         ELSE IF  ( NUM .GE. 1000 )    THEN
 
            X    = NUM / 1000
            SUFF = 'THOUSAND'
            NUM  = NUM - X * 1000
 
         ELSE
 
            X    = NUM
            SUFF = ' '
            NUM  = 0
 
         END IF
 
C
C        Convert X to text, ...
C
         SPACE = 1
 
         DO WHILE ( X .GT. 0 )
 
            IF ( STRING .EQ. ' ' ) THEN
               PAD = 0
            ELSE
               PAD = 1
            END IF
 
            IF ( X .GE. 100 ) THEN
 
               Y = X / 100
               X = X - Y*100
 
               CALL SUFFIX ( NUMBER(Y), PAD, STRING )
               CALL SUFFIX ( 'HUNDRED', 1,   STRING )
 
            ELSE IF ( X .GE. 20 ) THEN
 
               Y = X / 10
               X = X - Y*10
 
               CALL SUFFIX ( TENS(Y), PAD, STRING )
 
               IF ( X .NE. 0 ) THEN
                  CALL SUFFIX ( '-', 0, STRING )
                  SPACE = 0
               END IF
 
            ELSE
 
               Y = X
               X = 0
 
               IF ( STRING .EQ. ' ' ) THEN
                  SPACE = 0
               END IF
 
               CALL SUFFIX ( NUMBER(Y), SPACE, STRING )
 
            END IF
 
         END DO
 
C
C        ... then add the units. Repeat as necessary.
C
         CALL SUFFIX ( SUFF, 1, STRING )
 
      END DO
 
      RETURN
      END
