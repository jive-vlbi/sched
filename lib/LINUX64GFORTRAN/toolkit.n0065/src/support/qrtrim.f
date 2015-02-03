C$Procedure      QRTRIM (Quick right trim )
 
      INTEGER FUNCTION QRTRIM ( STRING )
 
C$ Abstract
C
C     This is a "faster" version of the SPICELIB routine RTRIM.
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
C      None.
C
C$ Keywords
C
C      ASCII,  CHARACTER,  SEARCH
C
C$ Declarations
 
      IMPLICIT NONE
      CHARACTER*(*)         STRING
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C
C     STRING     I   String to be trimmed.
C
C     The function returns the maximum of 1 and the location of the
C     last non-blank character in STRING.
C
C$ Detailed_Input
C
C     STRING         is a string to be trimmed:  the location of the
C                    last non-blank character is desired.
C
C$ Detailed_Output
C
C     The function returns the maximum of 1 and the location of the
C     last non-blank character in STRING.
C
C     In particular, when STRING is blank, the function returns the
C     value 1.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     When writing a character string to a file, we usually are content
C     to omit the trailing blanks.  We'd like to use LASTNB as an upper
C     substring bound, but we have to handle the case where LASTNB
C     returns 0, so we write:
C
C
C        WRITE ( UNIT, '(A)' ),  STRING ( : MAX (1, LASTNB (STRING)) )
C
C
C     This can be simplified using QRTRIM:
C
C
C        WRITE ( UNIT, '(A)' ),  STRING ( : QRTRIM (STRING) )  )
C
C      This routine has the same function as the SPICE routine
C      RTRIM however, it turns out to be substantially faster
C      when applied to long strings.  This is somewhat surprising
C      but happens due to a combination of machine instructions
C      available for comparing strings and the ineffective optimizations
C      performed by all compilers we've examined.  See the code
C      for more details regarding how this routine takes advantage
C      of native instructions and ineffective optimizations.
C
C$ Examples
C
C     1)  Write the non-blank portion of each element of a character
C         cell to file SPUD.DAT:
C
C            DO I = 1,  CARDC (CELL)
C
C               CALL WRLINE ('SPUD.DAT',
C           .                 CELL(I) ( LTRIM (CELL) : QRTRIM (CELL) ) )
C
C            END DO
C
C         When CELL(I) is blank, the string ' ' will be written.
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
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 9, 1994
C
C
C-     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of META/2
C         software as of May 3, 1994
C
C
C-    SPICELIB Version 1.0.0, 22-APR-1994 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Get the index of the last non-blank character of a string.
C     Right trim a string
C
C-&
      INTEGER               B
      INTEGER               I
      INTEGER               L
      INTEGER               M
      INTEGER               NL
      INTEGER               BLANK
      INTEGER               TUNE
      PARAMETER           ( TUNE = 16 )
 
 
      BLANK = ICHAR(' ')
      L     =  LEN(STRING)
C
C     If this is a short string there is no particular advantage
C     to be gained by making use of the binary search idea.
C     The speed up just doesn't buy much when compared with
C     the loop overhead.
C
      IF ( L .LE. 32 ) THEN
 
         DO I = L, 1, -1
            IF ( ICHAR(STRING(I:I)) .NE. BLANK ) THEN
               QRTRIM = I
               RETURN
            END IF
         END DO
 
         QRTRIM = 1
         RETURN
 
      END IF
 
 
      B     =  1
      NL    =  L - 1
 
C
C     We want M to be ( B + NL ) / 2   but right now that's L/2
C
      M     =  L / 2
 
 
      DO WHILE ( L - B .GT. TUNE )
C
C         What is true right now?  The string from L+1 on out
C         is blank.  L > B; L-1 = NL >= B;  M = (B + NL) / 2;
C         and M >= B,  B is at least one and if greater than 1
C         there must be a non-blank character between B and the
C         end of the string.
C
         IF ( ICHAR(STRING(L:L)) .NE. BLANK ) THEN
 
            QRTRIM = L
            RETURN
 
         ELSE IF ( ICHAR(STRING(M:M)) .NE. BLANK ) THEN
 
            L = NL
            B = M
 
         ELSE IF ( STRING(M+1:NL) .EQ. ' ' ) THEN
 
C
C            If you got here, the STRING(L:L) is a blank.
C            The string from L+1 on out is blank.
C            The string from M to NL (=L-1) is blank.  Thus the
C            string from M out is blank.
C
C            M is greater than or equal to B
C            If M  is less than B + 2, then L will become
C            B or less and there will not be a
C            next pass through the loop.  That means that
C            we will never get to this point again and don't
C            have to worry about the reference STRING(M:NL)
C            giving us an access violation.
C
            L  = M - 1
C
C            With the new value of L, we now know that STRING(L+1:)
C            is blank.
C
         ELSE
C
C            If you get to this point all of the string from
C            L out is blank and L is greater than M.
C            There is a non-blank character between M+1 and NL.
C            If L should become equal to B below, then the loop
C            will not be executed again.  That means again that
C            we don't have to worry about STRING(M:NL) being
C            an ill formed string.
C
            L  =  NL
            B  =  M + 1
C
C            With the new value of L, we now know that STRING(L+1:)
C            is blank.
C
         END IF
 
         NL =  L - 1
         M  = (B + NL) / 2
 
C
C         What's true now?  The string from L+1 on out is blank.
C         Somewhere between B and L is a non-blank character.
C
      END DO
C
C      Either B never changed from 1 or B was set to a value such that
C      there was a non-blank character between B and the end of
C      the string,  And the string from L+1 out to the end is
C      blank.  Since we want this to mimick RTRIM, we are done.
C
      DO I = L, 1, -1
 
         IF ( ICHAR(STRING(I:I)) .NE. BLANK  ) THEN
            QRTRIM = I
            RETURN
         END IF
      END DO
 
      QRTRIM = 1
      RETURN
      END
