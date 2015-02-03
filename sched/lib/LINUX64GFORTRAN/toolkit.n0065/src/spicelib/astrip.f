C$Procedure     ASTRIP ( STRIP Ascii characters from a string )
 
      SUBROUTINE ASTRIP ( INSTR, ASCIIB, ASCIIE, OUTSTR )
 
C$ Abstract
C
C      Remove from a character string all characters which fall
C      between specified starting and ending characters, inclusive.
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
C      ASCII,  CHARACTER
C
C$ Declarations
 
      CHARACTER*(*)    INSTR
      CHARACTER*1      ASCIIB
      CHARACTER*1      ASCIIE
      CHARACTER*(*)    OUTSTR
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      INSTR      I   Input string.
C      ASCIIB     I   First ASCII character in range to be stripped.
C      ASCIIE     I   Last ASCII character in range to be stripped.
C      OUTSTR     O   Output (stripped) string.
C
C$ Detailed_Input
C
C      INSTR       Is a character string from which all characters
C                  between ASCIIB and ASCIIE, inclusive, are to be
C                  removed.
C
C      ASCIIB      Is the first ASCII character in the range of
C                  characters to be removed from the input string.
C                  ASCIIB is itself removed from the string, if
C                  it occurs.
C
C      ASCIIE      Is the last ASCII character in the range of
C                  characters to be removed from the input string.
C                  ASCIIE is itself removed from the string, if
C                  it occurs.
C
C$ Detailed_Output
C
C      OUTSTR      Is the input string after all the character
C                  between ASCIIB and ASCIIE, inclusive, have
C                  been removed.
C
C                  If OUTSTR is not large enough to hold the output
C                  string, it is truncated on the right.
C
C                  OUTSTR may overwrite INSTR.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      ASTRIP checks each character
C      in INSTR to determine if it falls between the characters ASCIIB
C      and ASCIIE.  If so this character is removed from the string
C      (and the string is shortened). Remaining characters are copied
C      to the output string.
C
C$ Examples
C
C      The following examples illustrate the use of ASTRIP.
C
C            ASCIIB = 'b'
C            ASCIIE = 'k'
C            INSTR  = 'Now is the time for all good men to come quick.'
C            OUTSTR = 'Now s t tm or all oo mn to om qu.'
C
C            ASCIIB = 'a'
C            ASCIIE = 'z'
C            INSTR  = 'SELECT column TIME FROM table TEST'
C            OUTSTR = 'SELECT TIME FROM TEST'
C
C            ASCIIB = 'a'
C            ASCIIE = 'z'
C            INSTR  = 'this is going to be an empty string'
C            OUTSTR = ' '
C
C            ASCIIB = '!'
C            ASCIIE = '!'
C            INSTR  = 'Only 32 more shopping days until Christmas!'
C            OUTSTR = 'Only 32 more shopping days until Christmas'
C
C      ASTRIP may also be used to strip ASCII control characters
C      (line feeds, tab stops, and so on), as shown in the example
C      below.
C
C            ASCIIB = CHAR ( 0  )
C            ASCIIE = CHAR ( 31 )
C            CALL ASTRIP ( STRING, ASCIIB, ASCIIE, STRING )
C
C$ Restrictions
C
C      If ASCIIB and ASCIIE are not properly ordered (that is,
C      if ICHAR(ASCIIB) is not less than or equal to ICHAR(ASCIIE))
C      then ASTRIP will not function as described. (In fact, it will
C      copy the input string to the output string without change.)
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
C      W.L. Taber      (JPL)
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
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     strip ascii characters from a string
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
      INTEGER          LASTNB
 
C
C     Local Variables
C
      INTEGER          OUTLEN
      INTEGER          LAST
      INTEGER          LWRBND
      INTEGER          UPRBND
      INTEGER          I
      INTEGER          J
      INTEGER          K
 
 
 
C
C     Find the length of the output string. We don't want to
C     exceed it.
C
      OUTLEN = LEN ( OUTSTR )
 
C
C     Find the last non-blank character of the input string.
C
      LAST   = LASTNB ( INSTR )
 
C
C     Get the numeric representation of ASCIIB and ASCIIE.
C
      LWRBND = ICHAR ( ASCIIB )
      UPRBND = ICHAR ( ASCIIE )
 
C
C     Step through INSTR (I) a character at a time, transferring
C     characters to OUTSTR (J) whenever they fall outside the range
C     [ASCIIB, ASCIIE].
C
C     If the end of OUTSTR is reached, stop transferring characters
C     and return.
C
      J    = 0
 
      DO I = 1, LAST
 
         K = ICHAR ( INSTR(I:I) )
 
         IF ( ( K .LT. LWRBND ) .OR. ( K .GT. UPRBND ) ) THEN
C
C           The character is kept.  Note that if the user inputs
C           ASCIIB and ASCIIE in the wrong order this test will
C           always succeed so that the output string will be
C           the same as the input string.
C
            J           = J + 1
            OUTSTR(J:J) = INSTR(I:I)
 
            IF ( J .EQ. OUTLEN ) THEN
               RETURN
            END IF
 
         END IF
 
      END DO
 
C
C     Pad the output string with blanks.
C
      IF ( J .LT. OUTLEN ) THEN
         OUTSTR(J+1: ) = ' '
      END IF
 
      RETURN
      END
