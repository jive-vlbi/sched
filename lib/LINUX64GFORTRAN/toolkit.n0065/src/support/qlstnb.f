C$Procedure      QLSTNB (Quick LAST non-blank character)
 
      INTEGER FUNCTION QLSTNB ( STRING )
 
C$ Abstract
C
C     This is a "faster" version of the SPICELIB routine LASTNB.
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
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      STRING     I   Input character string.
C      QLSTNB     O   Index of the last non-blank character in STRING.
C
C$ Detailed_Input
C
C      STRING      is the input character string.
C
C$ Detailed_Output
C
C      QLSTNB      is the index of the last non-blank character
C                  in the input string. If there are no non-blank
C                  characters in the string, QLSTNB is zero.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      If the string is blank, return zero. Otherwise, step through
C      the string one character at a time until something other than
C      a blank is found. Return the index of that something within
C      the string.
C
C      This routine has the same function as the SPICE routine
C      LASTNB however, it turns out to be substantially faster
C      when applied to longer strings.  This is somewhat surprising
C      but happens due to a combination of machine instructions
C      available for comparing strings and the ineffective optimizations
C      performed by all compilers we've examined.  See the code
C      for more details regarding how this routine takes advantage
C      of native instructions and ineffective optimizations.
C
C$ Examples
C
C      The following examples illustrate the use of QLSTNB.
C
C            QLSTNB ( 'ABCDE'              )   = 5
C            QLSTNB ( 'AN EXAMPLE'         )   = 10
C            QLSTNB ( 'AN EXAMPLE        ' )   = 10
C            QLSTNB ( '                  ' )   = 0
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
               QLSTNB = I
               RETURN
            END IF
         END DO
 
         QLSTNB = 0
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
 
            QLSTNB = L
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
            QLSTNB = I
            RETURN
         END IF
      END DO
 
      QLSTNB = 0
      RETURN
      END
