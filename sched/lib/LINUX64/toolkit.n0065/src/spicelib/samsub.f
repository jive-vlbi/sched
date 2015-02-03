C$Procedure      SAMSUB (Same substrings)
 
      LOGICAL FUNCTION SAMSUB ( STR1, B1, E1,  STR2, B2, E2 )
 
C$ Abstract
C
C     Determine whether or not two substrings are the same
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
C       UTILITY
C
C$ Declarations
 
      IMPLICIT NONE
 
      CHARACTER*(*)         STR1
      INTEGER               B1
      INTEGER               E1
      CHARACTER*(*)         STR2
      INTEGER               B2
      INTEGER               E2
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  -------------------------------------------------
C     STR1       I   A string
C     B1         I   Beginning of a substring in STR1
C     E1         I   End of s substring in STR1
C     STR2       I   A second string
C     B2         I   The beginning of a substring in STR2
C     E2         I   The end  of s substring in STR2
C
C     The function returns .TRUE. if the substrings are identical
C
C$ Detailed_Input
C
C     STR1       is a character string
C
C     B1         are integers giving the beginning and ending of a
C     E1         substring in STR1
C
C     STR2       is a character string
C
C     B2         are integers giving the beginning and ending of a
C     E2         substring in STR2
C
C$ Detailed_Output
C
C     The function returns TRUE if the two substrings STR(B1:E1) and
C     STR(B2:E2) have the same length and the same characters.
C
C     If any of the indices B1, E1, B2, E2 are out of range or out
C     of order the function returns FALSE.
C
C$ Parameters
C
C      None.
C
C$ Files
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C     1) If any of the B1, E1, B2, E2 are out of range or if an
C        ending substring index is before a beginning substring
C        index, the function returns false.
C
C$ Particulars
C
C     This routine is a macro for comparing two substrings of
C     strings and handles all of the bounds checking to avoid
C     out of range errors with string indices.
C
C$ Examples
C
C     Suppose a string contains a number of occurrences of some
C     particular substring in sequence and that you need to locate
C     the first character that is out of this sequence or the
C     end of the string.
C
C     If one ignores boundary constraints this can easily be
C     coded as shown here:  We assume the particular substring is
C
C     '/beg'
C
C        B = 1
C        E = B + LEN('/beg' )
C
C        DO WHILE (       E           .LE. LEN(STR)
C                   .AND. STRING(B:E) .EQ. '/beg' )
C
C           B = B + LEN('/beg')
C           E = E + LEN('/beg')
C
C        END DO
C
C        IF ( B .LT. LEN(STR) ) THEN
C
C           we've found the start of a substring of interest
C
C        ELSE
C
C           there is no substring to find.
C
C        END IF
C
C     Unfortunately, you can't rely upon FORTRAN to check the boundary
C     condition: E .LE. LEN(STR) and skip the second test if the first
C     condition if false.  As a result you can get an out of range
C     error.
C
C     Instead you could code:
C
C     B = 1
C     E = B + LEN('/beg')
C
C     IF ( E .LE. LEN(STR) ) THEN
C        ALIKE = STRINB(B:E) .EQ. '/beg'
C     ELSE
C        ALIKE = .FALSE.
C     END IF
C
C     DO WHILE ( ALIKE )
C
C           B = B + LEN('/beg')
C           E = E + LEN('/beg')
C
C        IF ( E .LE. LEN(STR) ) THEN
C           ALIKE = STRINB(B:E) .EQ. '/beg'
C        ELSE
C           ALIKE = .FALSE.
C        END IF
C
C     END DO
C
C
C     However, this is code is far more effort.  Using this routine
C     you can make a much simpler block of code.
C
C     B = 1
C     E = B + LEN('/beg' )
C
C     DO WHILE ( SAMSUB(STR,B,E, '/beg',1,4 ) )
C
C        B = B + LEN('/beg')
C        E = E + LEN('/beg')
C
C     END DO
C
C
C
C$ Restrictions
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
C-    SPICELIB Version 1.0.0, 31-MAR-1995 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Check equality of two substrings.
C
C-&
 
 
      IF (      ( E1      .LT. B1           )
     .     .OR. ( E2      .LT. B2           )
     .     .OR. ( B1      .LT. 1            )
     .     .OR. ( B2      .LT. 1            )
     .     .OR. ( E1      .GT. LEN(STR1)    )
     .     .OR. ( E2      .GT. LEN(STR2)    )
     .     .OR. ( E1 - B1 .NE. E2 - B2      )  ) THEN
 
         SAMSUB = .FALSE.
         RETURN
 
      END IF
 
      SAMSUB = STR1(B1:E1) .EQ. STR2(B2:E2)
 
      RETURN
      END
