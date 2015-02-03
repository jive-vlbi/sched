C$Procedure      EQCHR (Equivalent characters)

      LOGICAL FUNCTION EQCHR ( A, B )

C$ Abstract
C
C     This function determines whether two characters are
C     equivalent when the case of the characters is ignored.
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
C       CHARACTER
C
C$ Declarations

      IMPLICIT NONE

      CHARACTER*(1)         A
      CHARACTER*(1)         B

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     A          I   one of the characters to check
C     B          I   the other character to check
C
C     The function returns .TRUE. if the characters are equivalent
C
C$ Detailed_Input
C
C     A           are two characters that are to be compared to see
C     B           if they are the same letter (although possibly
C                 having different case such as 'a' and 'A')
C
C$ Detailed_Output
C
C     The function returns the value .TRUE. if the two input characters
C     are the same or can be made the same by converting both to
C     upper or lower case.
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
C     This is a utility routine for comparing two characters to
C     see if they are the same when converted to upper case.  It
C     is particularly useful when writing string analysis routines
C     that should be case insensitive.  Instead of writing the
C     expression
C
C        A .EQ. B
C
C     use the expression
C
C        EQCHR ( A, B )
C
C     in all tests of equivalence for characters.
C
C$ Examples
C
C     Suppose you want to determine whether or not two strings
C     are the same if differences in the case of letters are ignored.
C     The following code fragment shows how you can use this routine
C     to check for the equivalence of character strings.
C
C        MORE  = .TRUE.
C        SAME  = .TRUE.
C        L1    =  LEN(STR1)
C        L2    =  LEN(STR2)
C        CHECK = MIN ( L1, L2 )
C
C        DO WHILE ( SAME .AND. MORE )
C
C           SAME = EQCHR( STR1(I:I), STR2(I:I) )
C           I    = I + 1
C           MORE = I .LT. CHECK
C
C        END DO
C
C        IF ( .NOT. SAME ) THEN
C
C           There's nothing to do, we already know the strings
C           are not the same.
C
C        ELSE IF ( L1 .LT. L2 ) THEN
C
C           The only way the strings can be regarded as being equal
C           is if the extra unchecked characters in STR2 are all blank.
C
C           SAME = STR2(I:) .EQ. ' '
C
C        ELSE
C
C           Same test as previous one but with STR1 this time.
C
C           SAME = STR1(I:) .EQ. ' '
C
C        END IF
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C     E.D. Wright     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-     Spicelib Version 2.0.0, 17-SEP-1998 (EDW)
C
C        Replace the UVALUE data statement with a loop to fill
C        UVALUE.  The Absoft Mac compiler failed to compile the
C        data statement correctly, and so this function failed
C        to work properly in all situations on the Mac.  The
C        corrects the problem and functions on all platforms.
C
C-    SPICELIB Version 1.0.0, 16-MAY-1995 (WLT)
C
C-&


C$ Index_Entries
C
C     Check two characters for case insensitive equality
C
C-&

C
C     Entry points.
C
      LOGICAL               NECHR
C
C     Range of characters
C
      INTEGER               LSTCHR
      PARAMETER           ( LSTCHR = 255 )
C
C     Local Variables
C
C     The array UVALUE contains the ICHAR values for the upper case
C     version of each character.
C
      INTEGER               UVALUE ( 0 : LSTCHR )
      INTEGER               I
      INTEGER               J

      LOGICAL               FIRST
      SAVE

      DATA FIRST / .TRUE. /


C
C     The first time through the loop we set the upper case values
C     for each of the lower case letters.
C
      IF ( FIRST ) THEN

         DO I = 0, LSTCHR
            UVALUE(I) = I
         END DO

         FIRST = .FALSE.

         UVALUE( ICHAR('a') ) = ICHAR( 'A' )
         UVALUE( ICHAR('b') ) = ICHAR( 'B' )
         UVALUE( ICHAR('c') ) = ICHAR( 'C' )
         UVALUE( ICHAR('d') ) = ICHAR( 'D' )
         UVALUE( ICHAR('e') ) = ICHAR( 'E' )
         UVALUE( ICHAR('f') ) = ICHAR( 'F' )
         UVALUE( ICHAR('g') ) = ICHAR( 'G' )
         UVALUE( ICHAR('h') ) = ICHAR( 'H' )
         UVALUE( ICHAR('i') ) = ICHAR( 'I' )
         UVALUE( ICHAR('j') ) = ICHAR( 'J' )
         UVALUE( ICHAR('k') ) = ICHAR( 'K' )
         UVALUE( ICHAR('l') ) = ICHAR( 'L' )
         UVALUE( ICHAR('m') ) = ICHAR( 'M' )
         UVALUE( ICHAR('n') ) = ICHAR( 'N' )
         UVALUE( ICHAR('o') ) = ICHAR( 'O' )
         UVALUE( ICHAR('p') ) = ICHAR( 'P' )
         UVALUE( ICHAR('q') ) = ICHAR( 'Q' )
         UVALUE( ICHAR('r') ) = ICHAR( 'R' )
         UVALUE( ICHAR('s') ) = ICHAR( 'S' )
         UVALUE( ICHAR('t') ) = ICHAR( 'T' )
         UVALUE( ICHAR('u') ) = ICHAR( 'U' )
         UVALUE( ICHAR('v') ) = ICHAR( 'V' )
         UVALUE( ICHAR('w') ) = ICHAR( 'W' )
         UVALUE( ICHAR('x') ) = ICHAR( 'X' )
         UVALUE( ICHAR('y') ) = ICHAR( 'Y' )
         UVALUE( ICHAR('z') ) = ICHAR( 'Z' )

      END IF

      I     = ICHAR(A)
      J     = ICHAR(B)

      IF ( I .GT. LSTCHR .OR. J .GT. LSTCHR ) THEN
         EQCHR = I .EQ. J
      ELSE
         EQCHR = UVALUE(I) .EQ. UVALUE(J)
      END IF

      RETURN



C$Procedure      NECHR (Not Equivalent characters)

      ENTRY NECHR ( A, B )

C$ Abstract
C
C     This function determines whether two characters are
C     not equivalent if the case of the characters is ignored.
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
C       CHARACTER
C
C$ Declarations
C
C     CHARACTER*(1)         A
C     CHARACTER*(1)         B
C
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      A          I   one of the characters to check
C      B          I   the other character to check
C
C      The function returns .TRUE. if the characters are not equivalent
C
C$ Detailed_Input
C
C     A           are two characters that are to be compared to see
C     B           if they are different letters. Letters that have
C                 the same value when converted to uppercase are
C                 considered to be equivalent.
C
C$ Detailed_Output
C
C     The function returns the value .FALSE. if the two input characters
C     are the same or can be made the same by converting both to
C     upper or lower case.  Otherwise it returns .TRUE.
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
C$ Particulars
C
C     This routine simply determines the truth value of .NOT. EQCHR.
C     See the entry point EQCHR for a discussion of that function.
C
C$ Examples
C
C     Suppose you want to determine whether or not two strings
C     are the same up to differences in case.  The following
C     code fragment shows how you can use this routine to check
C     for the equivalence of character strings.
C
C        MORE  = .TRUE.
C        SAME  = .TRUE.
C        L1    =  LEN(STR1)
C        L2    =  LEN(STR2)
C        CHECK = MIN ( L1, L2 )
C
C        DO WHILE ( SAME .AND. MORE )
C
C           IF ( NECHR(STR1(I:I),STR2(I:I) ) THEN
C              SAME = .FALSE.
C           END IF
C
C           I    = I + 1
C           MORE = I .LT. CHECK
C
C        END DO
C
C        IF ( .NOT. SAME ) THEN
C
C           There's nothing to do, we already know the strings
C           are not the same.
C
C        ELSE IF ( L1 .LT. L2 ) THEN
C
C           The only way the strings can be regarded as being equal
C           is if the extra unchecked characters in STR2 are all blank.
C
C           SAME = STR2(I:) .EQ. ' '
C
C        ELSE
C
C           Same test as previous one but with STR1 this time.
C
C           SAME = STR1(I:) .EQ. ' '
C
C        END IF
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
C-     Spicelib Version 2.0.0, 17-SEP-1998 (EDW)
C
C        Replace the UVALUE data statement with a loop to fill
C        UVALUE.  The Absoft Mac compiler failed to compile the
C        data statement correctly, and so this function failed
C        to work properly in all situations on the Mac.  The
C        corrects the problem and functions on all platforms.
C
C-     Spicelib Version 1.0.0, 16-MAY-1995
C
C-&

C$ Index_Entries
C
C     Check two characters for case insensitive not equal
C
C-&


      IF ( FIRST ) THEN

         FIRST = .FALSE.

         DO I = 0, LSTCHR
            UVALUE(I) = I
         END DO

         UVALUE( ICHAR('a') ) = ICHAR( 'A' )
         UVALUE( ICHAR('b') ) = ICHAR( 'B' )
         UVALUE( ICHAR('c') ) = ICHAR( 'C' )
         UVALUE( ICHAR('d') ) = ICHAR( 'D' )
         UVALUE( ICHAR('e') ) = ICHAR( 'E' )
         UVALUE( ICHAR('f') ) = ICHAR( 'F' )
         UVALUE( ICHAR('g') ) = ICHAR( 'G' )
         UVALUE( ICHAR('h') ) = ICHAR( 'H' )
         UVALUE( ICHAR('i') ) = ICHAR( 'I' )
         UVALUE( ICHAR('j') ) = ICHAR( 'J' )
         UVALUE( ICHAR('k') ) = ICHAR( 'K' )
         UVALUE( ICHAR('l') ) = ICHAR( 'L' )
         UVALUE( ICHAR('m') ) = ICHAR( 'M' )
         UVALUE( ICHAR('n') ) = ICHAR( 'N' )
         UVALUE( ICHAR('o') ) = ICHAR( 'O' )
         UVALUE( ICHAR('p') ) = ICHAR( 'P' )
         UVALUE( ICHAR('q') ) = ICHAR( 'Q' )
         UVALUE( ICHAR('r') ) = ICHAR( 'R' )
         UVALUE( ICHAR('s') ) = ICHAR( 'S' )
         UVALUE( ICHAR('t') ) = ICHAR( 'T' )
         UVALUE( ICHAR('u') ) = ICHAR( 'U' )
         UVALUE( ICHAR('v') ) = ICHAR( 'V' )
         UVALUE( ICHAR('w') ) = ICHAR( 'W' )
         UVALUE( ICHAR('x') ) = ICHAR( 'X' )
         UVALUE( ICHAR('y') ) = ICHAR( 'Y' )
         UVALUE( ICHAR('z') ) = ICHAR( 'Z' )

      END IF

      I     = ICHAR(A)
      J     = ICHAR(B)

      IF ( I .GT. LSTCHR .OR. J .GT. LSTCHR ) THEN
         NECHR = I .NE. J
      ELSE
         NECHR = UVALUE(I) .NE. UVALUE(J)
      END IF

      RETURN


      END
