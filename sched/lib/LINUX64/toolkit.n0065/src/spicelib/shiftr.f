C$Procedure      SHIFTR ( Shift right )
 
      SUBROUTINE SHIFTR ( IN, NSHIFT, FILLC, OUT )
 
C$ Abstract
C
C      Shift the contents of a character string to the right.
C      Characters moved past the end of the input string are
C      lost. Vacant spaces are filled with a specified character.
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
C      CHARACTER,  UTILITY
C
C$ Declarations
 
      CHARACTER*(*)      IN
      INTEGER            NSHIFT
      CHARACTER*1        FILLC
      CHARACTER*(*)      OUT
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      IN         I   Input string.
C      NSHIFT     I   Number of times to shift.
C      FILLC      I   Character to fill spaces left vacant.
C      OUT        O   Shifted string.
C
C$ Detailed_Input
C
C      IN          is the input character string.
C
C      NSHIFT      is the number of times the string is to be
C                  shifted. If NSHIFT is negative, OUT will be
C                  identical to IN.
C
C      FILLC       is the character with which spaces left vacant by
C                  the shift are to be filled.
C
C$ Detailed_Output
C
C      OUT         is the output string. This is the input string,
C                  shifted N times, filled with FILLC.
C
C                  OUT may overwrite IN.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      As a string is shifted left or right, the leftmost or
C      rightmost characters of the string disappear (as if pushed
C      off the end of the string). This is true regardless of
C      the length of the output string.
C
C      The remaining characters are shifted simultaneously, and
C      the spaces vacated by those characters are filled with a
C      replacement character.
C
C$ Examples
C
C    If FILLC = ' '
C
C       'abcde'   shifted left twice becomes     'cde  '
C       'abcde'   shifted right once becomes     ' abcd'
C
C    If FILLC = '.'
C
C       '12345 '  shifted right once becomes     '.12345'
C       'Apple '  shifted left ten times becomes '......'
C
C    Given the declarations
C
C       CHARACTER*3         SHORT
C       CHARACTER*10        LONG
C
C    The calls
C
C       CALL SHIFTR ( 'abcde ', 2, '-', SHORT )
C       CALL SHIFTR ( 'abcde ', 2, '-', LONG  )
C
C    yield the strings
C
C       SHORT = '--a'
C       LONG  = '--abcd    '
C
C    while the calls
C
C       CALL SHIFTL ( 'abcde ', 2, '-', SHORT )
C       CALL SHIFTL ( 'abcde ', 2, '-', LONG  )
C
C    yield the strings
C
C       SHORT = 'cde'
C       LONG  = 'cde ..     '
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
C      M.J. Spencer    (JPL)
C      I.M. Underwood  (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-     SPICELIB Version 2.0.1, 22-AUG-2001 (EDW)
C
C         Corrected ENDDO to END DO.
C
C-     SPICELIB Version 2.0.0, 01-SEP-1994 (MJS)
C
C         This version correctly handles negative shifts.
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
C     shift right
C
C-&
 
C
C     Local variables
C
      INTEGER          INLEN
      INTEGER          OUTLEN
      INTEGER          N
      INTEGER          NSAVE
      INTEGER          NFILL
      INTEGER          I
      INTEGER          S
 
C
C     Get the length of the input, output strings.
C
      INLEN  = LEN ( IN  )
      OUTLEN = LEN ( OUT )
 
C
C     If the shift is zero or negative, the string is not changed.
C     If longer than the input string, the entire string is shifted.
C
      S = MAX ( NSHIFT, 0 )
      N = MIN ( INLEN,  S )
 
C
C     Figure out how many characters in the input string will
C     be saved (will not be shifted off the end of the string,
C     and will fit in the output string), and how many fill
C     characters will be needed (no more than NSHIFT, no fewer
C     than zero).
C
      NSAVE = ( INLEN - N ) - MAX ( 0, INLEN - OUTLEN )
      NFILL =                 MIN ( N, OUTLEN         )
 
C
C     Move the saved characters to output.
C
      DO I = NSAVE, 1, -1
         OUT(I+S:I+S) = IN(I:I)
      END DO
 
C
C     Add as many fill characters as appropriate.
C
      DO I = 1, NFILL
         OUT(I:I) = FILLC
      END DO
 
C
C     Pad the output string with blanks (to cover any previous
C     ugliness there).
C
      IF ( OUTLEN .GT. INLEN ) THEN
         OUT(INLEN+1: ) = ' '
      END IF
 
      RETURN
      END
