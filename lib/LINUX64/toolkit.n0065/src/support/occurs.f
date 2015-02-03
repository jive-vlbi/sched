 
C$Procedure OCCURS ( Count occurrences of a substring in a string )
 
      INTEGER FUNCTION OCCURS ( STR, SUB )
      IMPLICIT NONE
 
C$ Abstract
C
C     Count the number of times that a substring occurs within
C     a character string.
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
C     STRING
C     UTILITY
C
C$ Declarations
 
      CHARACTER*(*)         STR
      CHARACTER*(*)         SUB
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STR        I   Character string.
C     C          I   Target substring.
C
C$ Detailed_Input
C
C     STR         is an arbitrary character string.
C
C     SUB         is an arbitrary character string.
C
C$ Detailed_Output
C
C     The function returns the number of occurrences of the substring
C     within the string.
C
C$ Exceptions.
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Typically, this would be used to count the number of times
C     that a single character occurs within a string: for example,
C     to determine whether the number of left parentheses in an
C     expression matches the number of right parentheses.
C
C     The occurrences found by OCCURS are independent: that is,
C     the number of occurrences of 'XXX' in 'XXXXXXXX' is two,
C     and not six.
C
C$ Examples
C
C     The following code fragment checks to make sure that the
C     number of left parentheses in an expression matches the number
C     of right delimiters in the same expression.
C
C       IF ( OCCURS ( EXPR, '(' ) - OCCURS ( EXPR, ')' ) .NE. 0 ) THEN
C         WRITE (6,*) 'Parenthesis mismatch.'
C       END IF
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
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
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
C     Version B1.0.0, 29-APR-1988 (WLT) (IMU)
C
C-&
 
 
 
C
C     Local variables
C
 
      INTEGER               LSTR
      INTEGER               LSUB
      INTEGER               I
 
      LSTR   = LEN ( STR )
      LSUB   = LEN ( SUB )
 
      I      = 0
      OCCURS = 0
 
      DO WHILE ( I .LE. LSTR - LSUB )
 
         IF ( STR(I+1:I+LSUB) .EQ. SUB ) THEN
            OCCURS = OCCURS + 1
            I      = I      + LSUB
 
         ELSE
            I      = I      + 1
         END IF
 
      END DO
 
      RETURN
      END
