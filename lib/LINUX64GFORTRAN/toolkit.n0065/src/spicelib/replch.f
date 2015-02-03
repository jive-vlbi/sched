C$Procedure      REPLCH ( Replace characters in a string )
 
      SUBROUTINE REPLCH ( INSTR, OLD, NEW, OUTSTR )
 
C$ Abstract
C
C      Replace all occurrences of a single character with a second
C      character.
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
      CHARACTER*1      OLD
      CHARACTER*1      NEW
      CHARACTER*(*)    OUTSTR
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      INSTR      I   Input string.
C      OLD        I   Character to be replaced.
C      NEW        I   Replacement character.
C      OUTSTR     O   Output string.
C
C$ Detailed_Input
C
C      INSTR       is the input character string, possibly containing
C                  one or more occurrences of the character OLD.
C
C      OLD         is the character to be replaced wherever it occurs in
C                  the input string.
C
C      NEW         is the character which is to replace each occurrence
C                  of the character OLD in the output string.
C
C$ Detailed_Output
C
C      OUTSTR      is the output string. This is the input string
C                  with every occurrence of the character OLD replaced
C                  by the character NEW.
C
C                  OUTSTR may overwrite INSTR.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      Copy the contents of the input string to the output string
C      a character at a time, replacing each occurrence of OLD with NEW.
C      If the output string is not long enough to contain the input
C      string, it is truncated on the right.
C
C$ Exceptions
C
C      Error free.
C
C$ Files
C
C      None.
C
C$ Examples
C
C      Let
C            INSTR  = 'Woodsy is the Anti-Pollution Owl.'
C            OLD    = 'O'
C            NEW    = 'E'
C      then
C            OUTSTR = 'Woodsy is the Anti-Pollution Ewl.'
C
C      Note the case-sensitivity of REPLCH. The lowercase o's are
C      not affected.
C
C      REPLCH may similarly be used to replace control characters
C      (such as tab stops, line feeds, and nulls) with regular ASCII
C      characters (such as blanks).
C
C$ Restrictions
C
C      REPLCH is sensitive to case, as shown in the examples above.
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
C     replace characters in a string
C
C-&
 
 
 
 
C
C   Local Variables
C
      INTEGER          I
 
 
 
C
C     Move the input string to the output string. If it's too long,
C     this will truncate it.
C
      OUTSTR = INSTR
 
C
C     Check each character of OUTSTR and replace as necessary.
C
      DO I = 1, LEN ( OUTSTR )
 
         IF ( OUTSTR(I:I) .EQ. OLD ) THEN
            OUTSTR(I:I) = NEW
         END IF
 
      END DO
 
      RETURN
      END
