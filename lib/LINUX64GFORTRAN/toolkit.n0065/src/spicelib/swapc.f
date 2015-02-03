C$Procedure      SWAPC ( Swap character values )
 
      SUBROUTINE SWAPC ( A, B )
 
C$ Abstract
C
C      Swap the contents of two character strings.
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
C      UTILITY
C
C$ Declarations
 
      CHARACTER*(*)    A
      CHARACTER*(*)    B
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      A         I/O  First string.
C      B         I/O  Second string.
C
C$ Detailed_Input
C
C      A,
C      B           are two character strings, the contents of which
C                  are to be swapped (exchanged).
C
C$ Detailed_Output
C
C      A,
C      B           are the same two character strings, after their
C                  contents have been exchanged.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      This is just shorthand notation for the code fragment
C
C            TEMP = A
C            A    = B
C            B    = TEMP
C
C      The characters in the string are swapped one at a time, so
C      no intermediate string (TEMP) is needed. This means that the
C      strings may be of any length.
C
C$ Examples
C
C      Let
C            A = 11.D0
C            B = 22.D0
C
C      Then after calling SWAPD (A,B),
C
C            A = 22.D0
C            B = 11.D0
C
C$ Restrictions
C
C      None.
C
C$ Exceptions
C
C     Error free.
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
C     swap character values
C
C-&
 
 
 
 
C
C     Local variables
C
      INTEGER          ALEN
      INTEGER          BLEN
      INTEGER          SHORT
 
      CHARACTER*1      TEMP
      INTEGER          I
 
 
C
C     Get the lengths of the strings.
C
      ALEN  = LEN ( A )
      BLEN  = LEN ( B )
      SHORT = MIN ( ALEN, BLEN )
 
C
C     Keep going until the end of the shorter string is reached.
C
      DO I = 1, SHORT
 
         TEMP   = A(I:I)
         A(I:I) = B(I:I)
         B(I:I) = TEMP
 
      END DO
 
C
C     If either string is longer than the shortest one, pad it
C     with blanks.
C
      IF ( ALEN .GT. SHORT ) THEN
         A(SHORT+1: ) = ' '
 
      ELSE IF ( BLEN .GT. SHORT ) THEN
         B(SHORT+1: ) = ' '
      END IF
 
      RETURN
      END
