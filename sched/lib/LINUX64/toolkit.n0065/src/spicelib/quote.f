C$Procedure      QUOTE ( Enclose in quotes )
 
      SUBROUTINE QUOTE ( IN, LEFT, RIGHT, OUT )
 
C$ Abstract
C
C      Enclose (quote) the non-blank part of a character string
C      between delimiting symbols.
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
C      CHARACTER,  PARSING
C
C$ Declarations
 
      CHARACTER*(*)    IN
      CHARACTER*1      LEFT
      CHARACTER*1      RIGHT
      CHARACTER*(*)    OUT
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      IN         I   Input string.
C      LEFT       I   Left delimiter.
C      RIGHT      I   Right delimiter.
C      OUT        O   Output (quoted) string.
C
C$ Detailed_Input
C
C      IN          is the input string to be quoted.
C
C      LEFT,
C      RIGHT       are the left and right delimiters to be used in
C                  quoting the input string. These may be the same
C                  character (apostrophe, vertical bar), complementary
C                  characters (left and right parentheses, brackets,
C                  or braces), or two totally unrelated characters.
C
C$ Detailed_Output
C
C      OUT         is the output string. This is the non-blank part
C                  of the input string delimited by LEFT and RIGHT.
C                  If the output string is not large enough to contain
C                  the quoted string, it is truncated on the right.
C                  (The right delimiter would be lost in this case.)
C
C                  If the input string is blank, the output string is
C                  a single quoted blank.
C
C                  OUT may overwrite IN.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      The first character of the output string is the left delimiter,
C      LEFT. This is followed immediately by the non-blank part of the
C      input string, which is in turn followed by the right delimiter,
C      RIGHT.
C
C      If the input string is blank (has no non-blank characters),
C      a single quoted blank is returned.
C
C$ Examples
C
C      Let
C            IN    = '    This string has leading and trailing blanks  '
C            LEFT  = '('
C            RIGHT = ')'
C
C      Then
C            OUT   = '(This string has leading and trailing blanks)    '
C
C      Or, let IN = '         '. Then OUT = '( )'.
C
C
C$ Restrictions
C
C      None.
C
C$ Exceptions
C
C      Error free.
C
C$ Files
C
C      None.
C
C$ Author_and_Institution
C
C      I.M. Underwood  (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     enclose in quotes
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
      INTEGER          FRSTNB
      INTEGER          LASTNB
 
 
 
C
C     Check for blank string first.
C
      IF ( IN .EQ. ' ' ) THEN
         OUT = LEFT
         CALL SUFFIX ( RIGHT, 1, OUT )
 
      ELSE
         OUT = IN(FRSTNB(IN) : LASTNB(IN))
         CALL PREFIX ( LEFT,  0, OUT )
         CALL SUFFIX ( RIGHT, 0, OUT )
      END IF
 
      RETURN
      END
