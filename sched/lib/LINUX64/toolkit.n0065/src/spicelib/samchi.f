C$Procedure   SAMCHI ( Same character --- case insensitive )
 
      LOGICAL FUNCTION SAMCHI ( STR1, L1, STR2, L2 )
 
C$ Abstract
C
C     Determine if two characters from different strings are the
C     same when the case of the characters is ignored.
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
C     UTILITY
C
C$ Declarations
 
      IMPLICIT NONE
 
      CHARACTER*(*)         STR1
      INTEGER               L1
      CHARACTER*(*)         STR2
      INTEGER               L2
 
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STR1       I   A character string
C     L1         I   The location (index) of a character in STR1
C     STR2       I   A character string
C     L2         I   The location (index) of a character in STR2
C
C     The function returns TRUE if the two characters are the
C     same up to case.
C
C$ Detailed_Input
C
C     STR1       is a character string
C
C     L1         is the location (index) of a character in STR1
C
C     STR2       is a character string
C
C     L2         is the location (index) of a character in STR2
C
C$ Detailed_Output
C
C
C     The function returns TRUE if the characters STR1(L1:L1) and
C     STR2(L2:L2) are the same when the case of the characters is
C     ignored.
C
C     If the characters are different or L1 or L2 is out of range the
C     function returns FALSE.
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
C     1) If either L1 or L2 is out of range the function returns FALSE.
C
C$ Particulars
C
C     This is a utility function for determining whether or not
C     two characters in different strings are the same up to case.
C     This function is intended for situation in which you need
C     to search two strings for a match (or mismatch).
C
C$ Examples
C
C     Often you need to scan through two string comparing
C     character by character until a case insensitive mismatch
C     occurs.  The usual way to code this is
C
C        DO WHILE (        L1 .LE. LEN(STR1)
C                    .AND. L2 .LE. LEN(STR2)
C                    .AND. EQCHR( STR1(L1:L1),STR2(L2:L2) ) )
C
C           L1 = L1 + 1
C           L2 = L2 + 1
C
C        END DO
C
C        Check L1, L2 to make sure we are still in range, etc.
C
C     The problem with this loop is that even though the check to make
C     sure that L1 and L2 are in range is performed, FORTRAN may
C     go ahead and compute the equality condition even though one of the
C     first two steps failed.  This can lead to out of range errors
C     and possible halting of your program depending upon how
C     the routine is compiled.   An alternative way to code this is
C
C        IF ( L1 .LE. LEN(STR1) .AND. L2 .LE. LEN(STR2) ) THEN
C           ALIKE = EQCHR( STR1(L1:L1),STR2(L2:L2) )
C        ELSE
C           ALIKE = .FALSE.
C        END IF
C
C        DO WHILE ( ALIKE )
C
C           L1 = L1 + 1
C           L2 = L2 + 1
C
C           IF ( L1 .LE. LEN(STR1) .AND. L2 .LE. LEN(STR2) ) THEN
C              ALIKE = EQCHR( STR1(L1:L1), STR2(L2:L2) )
C           ELSE
C              ALIKE = .FALSE.
C           END IF
C        END DO
C
C     However this is a much more complicated section of code.  This
C     routine allows you to code the above loops as:
C
C
C        DO WHILE ( SAMCHI ( STR1,L1, STR2,L2 )  )
C           L1 = L1 + 1
C           L2 = L2 + 1
C        END DO
C
C     The boundary checks are automatically performed and out
C     of range errors are avoided.
C
C
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 31-MAR-1995 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Check two character substrings for case insensitive equal
C
C-&
 
C
C     Spicelib Functions
C
      LOGICAL               EQCHR
 
 
      IF (      L1 .LT. 1
     .     .OR. L2 .LT. 1
     .     .OR. L1 .GT. LEN(STR1)
     .     .OR. L2 .GT. LEN(STR2)  ) THEN
 
         SAMCHI = .FALSE.
         RETURN
      END IF
 
      SAMCHI = EQCHR( STR1(L1:L1), STR2(L2:L2) )
      RETURN
 
      END
 
