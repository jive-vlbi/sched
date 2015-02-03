C$Procedure LJUST ( Left justify a character string )
 
      SUBROUTINE LJUST ( INPUT, OUTPUT )
 
C$ Abstract
C
C     Left justify a character string.
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
C     ASCII,  CHARACTER,  STRING
C
C$ Declarations
 
      CHARACTER*(*)    INPUT
      CHARACTER*(*)    OUTPUT
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     INPUT      I   Input character string.
C     OUTPUT     O   Output character string, left justified.
C
C$ Detailed_Input
C
C     INPUT       is the input character string.
C
C$ Detailed_Output
C
C     OUTPUT      is the output character string, left justified.
C
C                 OUTPUT may overwrite INPUT.
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
C     Leading blanks are removed from the input character string.
C     If the output string is not large enough to hold the left
C     justified string, it is truncated on the right.
C
C$ Examples
C
C     The following examples illustrate the use of LJUST.
C
C            'ABCDE'             becomes   'ABCDE'
C            'AN EXAMPLE'                  'AN EXAMPLE'
C            '   AN EXAMPLE  '             'AN EXAMPLE'
C            '               '             ' '
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
C     I.M. Underwood  (JPL)
C     W.L. Taber      (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 29-JUL-2013 (BVS)
C
C        Added the quick return branch for input strings that are
C        already left-justified. Removed the initial check for blank
C        input and changed logic to return an empty string after
C        scanning the input. Re-ordered header sections.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     left justify a character_string
C
C-&
 
 
C
C     Local variables
C
      INTEGER               LI
      INTEGER               LO
      INTEGER               I
      INTEGER               J
      INTEGER               POS
 
 
C
C     Is the first character of the input string non-blank? If yes, the
C     input string is already left-justified. There is nothing to do
C     but to assign the input string to the output string.
C
      IF ( INPUT(1:1) .NE. ' ' ) THEN

         OUTPUT = INPUT
 
      ELSE
 
C
C        Get the first non-blank character. Start OUTPUT at that point.
C
         LI = LEN    ( INPUT )
         LO = LEN    ( OUTPUT)
 
         J  = 1
 
C
C        Set I equal to position of first non-blank character of INPUT.
C
         I   = 0
         POS = 1
 
         DO WHILE ( I .EQ. 0 .AND. POS .LE. LI )
 
            IF ( INPUT(POS:POS) .NE. ' ' )  THEN
               I    =  POS
            ELSE
               POS  =  POS + 1
            END IF
 
         END DO

C
C        Did we find a non-blank character? If not, the input string is
C        blank. Set output to blank.
C        
         IF ( I .EQ. 0 ) THEN

            OUTPUT = ' '

         ELSE
   
C
C           I is now the index of the first non-blank character of
C           INPUT.
C
            DO WHILE  ( ( I .LE. LI ) .AND. ( J .LE. LO ) )
               OUTPUT(J:J) = INPUT(I:I)
               J           = J + 1
               I           = I + 1
            END DO
            
            IF ( J .LE. LO ) THEN
               OUTPUT(J:) = ' '
            END IF
 
         END IF

      END IF
 
      RETURN
      END
