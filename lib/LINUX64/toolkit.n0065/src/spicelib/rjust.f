C$Procedure      RJUST ( Right justify a character string )
 
      SUBROUTINE RJUST ( INPUT, OUTPUT )
 
C$ Abstract
C
C      Right justify a character string.
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
C      ASCII,  CHARACTER,  STRING
C
C$ Declarations
 
      CHARACTER*(*)    INPUT
      CHARACTER*(*)    OUTPUT
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      INPUT      I   Input character string.
C      OUTPUT     O   Output character string, right justified.
C
C$ Detailed_Input
C
C      INPUT       is the input character string.
C
C$ Detailed_Output
C
C      OUTPUT      is the output character string, right justified.
C                  If INPUT is too large to fit into OUTPUT, it is
C                  truncated on the left.
C
C                  OUTPUT may overwrite INPUT.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      Any trailing blanks in the input string are removed, and
C      the remaining string is copied to the output string.
C
C$ Examples
C
C      The following examples should illustrate the use of RJUST.
C
C         'ABCDE          '   becomes  '          ABCDE'
C         'AN EXAMPLE     '            '     AN EXAMPLE'
C         '   AN EXAMPLE  '            '     AN EXAMPLE'
C         '               '            '               '
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
C     right justify a character_string
C
C-&
 
 
 
 
C$ Revisions
C
C-     Beta Version 1.1.0, 11-DEC-1989 (IMU)
C
C         Did not work on Sun when INPUT and OUTPUT were
C         the same string, and where the initial and final
C         locations of the non-blank part of the string
C         overlapped.
C
C         The solution is to move the characters one by one,
C         starting from the right side of the input string.
C         That way, nothing gets clobbered.
C
C-&
 
 
 
C
C     SPICELIB functions
C
      INTEGER               FRSTNB
      INTEGER               LASTNB
 
C
C     Local variables
C
      INTEGER               FIRST
      INTEGER               I
      INTEGER               LAST
      INTEGER               LOC
      INTEGER               START
 
 
 
C
C     Blank string? It's all the same.
C
      IF ( INPUT .EQ. ' ' ) THEN
         OUTPUT = INPUT
 
C
C     Get the first non-blank character. Start OUTPUT at that point.
C
      ELSE
         FIRST  = FRSTNB ( INPUT )
         LAST   = LASTNB ( INPUT )
         START  = LEN ( OUTPUT ) - (LAST-FIRST)
 
C
C        If the input string is too long (START < 1), move FIRST
C        up a little to truncate on the left.
C
         IF ( START .LT. 1 ) THEN
            FIRST = FIRST + (1-START)
            START = 1
         END IF
 
C
C        Move the characters in reverse order, to keep from stomping
C        anything if the operation is being done in place.
C
         LOC = LEN ( OUTPUT )
 
         DO I = LAST, FIRST, -1
            OUTPUT(LOC:LOC) = INPUT(I:I)
            LOC = LOC - 1
         END DO
 
C
C        Clear the first part of OUTPUT, if necessary.
C
         IF ( START .GT. 1 ) THEN
            OUTPUT(1:START-1) = ' '
         END IF
 
      END IF
 
      RETURN
      END
