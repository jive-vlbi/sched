C$Procedure      LCASE ( Convert to lowercase )
 
      SUBROUTINE LCASE ( IN, OUT )
 
C$ Abstract
C
C      Convert the characters in a string to lowercase.
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
 
      CHARACTER*(*)    IN
      CHARACTER*(*)    OUT
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      IN         I   Input string.
C      OUT        O   Output string, all lowercase.
C
C$ Detailed_Input
C
C      IN          is the input string.
C
C$ Detailed_Output
C
C      OUT         is the output string. This is the input string
C                  with all uppercase letters converted to lowercase.
C                  Non-letters are not affected.
C
C                  OUT may overwrite IN.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      Convert each uppercase character in IN to lowercase.
C
C$ Examples
C
C      'This is an EXAMPLE'     becomes  'this is an example'
C      '12345 +-=? > * $ &'              '12345 +-=? > * $ &'
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
C      K.R. Gehringer  (JPL)
C      None.
C
C$ Version
C
C-     SPICELIB Version 1.1.0, 13-MAR-1996 (KRG)
C
C         Removed the calls to the string lexicographic functions.
C
C         Modified the algorithm to use the ICHAR() intrinsic function
C         and some local integer storage for the bases of the lower and
C         upper case letters.
C
C         Added a "FIRST" clause to the code so that the lower and
C         upper case bases and the separation between them are only 
C         initialized the first time the subroutine is called rather 
C         than every time.
C
C         These changes were made to improve the execution speed of
C         the subroutine
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
C     convert to lowercase
C
C-&

C
C     Local variables
C
      INTEGER          I
      INTEGER          ICH
C
C     Saved variables
C
      INTEGER          SHIFT
      INTEGER          UPPERA
      INTEGER          UPPERZ

      LOGICAL          FIRST

      SAVE             FIRST
      SAVE             SHIFT
      SAVE             UPPERA
      SAVE             UPPERZ
C
C     Initial Data
C
      DATA             FIRST / .TRUE. /
C
C     Do some set up stuff the first time through so that we do not
C     need to reinitialize the boundary values used for comparisons
C     and the shift on each call.
C
      IF ( FIRST ) THEN
         FIRST  = .FALSE.
         UPPERA = ICHAR ('A')
         UPPERZ = ICHAR ('Z')
         SHIFT  = ICHAR ('a') - UPPERA
      END IF

C
C     Move the string from IN to OUT. Step through OUT one character
C     at a time, translating letters between 'A' and 'Z' to lowercase.
C
      OUT   = IN
 
      DO I = 1, LEN (OUT)

         ICH = ICHAR( OUT(I:I) )

         IF ( ( ICH .GE. UPPERA ) .AND. ( ICH .LE. UPPERZ ) ) THEN
            OUT(I:I) = CHAR ( ICH + SHIFT )
         END IF 

      END DO
 
      RETURN
      END
