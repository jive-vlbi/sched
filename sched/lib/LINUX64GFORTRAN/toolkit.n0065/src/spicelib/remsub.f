C$Procedure      REMSUB ( Remove a substring )
 
      SUBROUTINE REMSUB ( IN, LEFT, RIGHT, OUT )
 
C$ Abstract
C
C      Remove the substring (LEFT:RIGHT) from a character string.
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
C      ASSIGNMENT,  CHARACTER,  STRING
C
C$ Declarations
 
      CHARACTER*(*)    IN
      INTEGER          LEFT
      INTEGER          RIGHT
      CHARACTER*(*)    OUT
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      IN         I   Input string.
C      LEFT       I   Position of first character to be removed.
C      RIGHT      I   Position of last character to be removed.
C      OUT        O   Output string.
C
C$ Detailed_Input
C
C      IN          is an input character string, from which a substring
C                  is to be removed.
C
C      LEFT,
C      RIGHT       are the ends of the substring to be removed.
C
C$ Detailed_Output
C
C      OUT         is the output string. This is equivalent to the
C                  string that would be created by the concatenation
C
C                        OUT = IN(1 : LEFT-1) // IN(RIGHT+1 : )
C
C                  If the string is too long to fit into OUT, it is
C                  truncated on the right.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      Move the characters, beginning with RIGHT, one at a time to the
C      positions immediately following LEFT. This has the same effect
C      as the concatenation
C
C            OUT = IN(1 : LEFT-1) // IN(RIGHT+1 : )
C
C      Because this operation is not standard for strings of length (*),
C      this routine does not use concatenation.
C
C$ Examples
C
C      The following examples illustrate the use of REMSUB.
C
C      IN                 LEFT  RIGHT        OUT
C      -----------------  ----  -----        ------------------------
C      'ABCDEFGHIJ'          3      5        'ABFGHIJ'
C      'The best rabbit'     5      8        'The  rabbit'
C      'The other woman'     1      4        'other woman'
C      'An Apple a day'      2      2        'A apple a day'
C      'An Apple a day'      5      2         An error is signalled.
C      'An Apple a day'      0      0         An error is signalled.
C      'An Apple a day'     -3      3         An error is signalled.
C
C      Whenever an error has been signalled, the contents of OUT are
C      unpredictable.
C
C$ Restrictions
C
C      None.
C
C$ Exceptions
C
C      If LEFT > RIGHT, RIGHT < 1, LEFT < 1, RIGHT > LEN(IN), or
C      LEFT > LEN(IN), the error SPICE(INVALIDINDEX) is signalled.
C
C$ Files
C
C      None.
C
C$ Author_and_Institution
C
C      H.A. Neilan     (JPL)
C      W.L. Taber      (JPL)
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
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     remove a substring
C
C-&
 
 
 
C$ Revisions
C
C-     Beta Version 2.0.0, 5-JAN-1989 (HAN)
C
C         Error handling was added to detect invalid character
C         positions. If LEFT > RIGHT, RIGHT < 1, LEFT < 1,
C         RIGHT > LEN(IN), or LEFT > LEN(IN), an error is signalled.
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
      INTEGER          LASTNB
      LOGICAL          RETURN
 
C
C     Other functions
C
      INTEGER          LEN
 
C
C     Local variables
C
      INTEGER          INLEN
      INTEGER          OUTLEN
 
      INTEGER          L
      INTEGER          R
 
      INTEGER          I
      INTEGER          J
 
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'REMSUB' )
      END IF
 
C
C     If a character position is out of range, signal an error.
C
      IF ( ( LEFT  .GT. RIGHT    )   .OR.
     .     ( RIGHT .LT. 1        )   .OR.
     .     ( LEFT  .LT. 1        )   .OR.
     .     ( RIGHT .GT. LEN (IN) )   .OR.
     .     ( LEFT  .GT. LEN (IN) ) )       THEN
 
         CALL SETMSG ( 'Left location was *. Right location was *.' )
         CALL ERRINT ( '*', LEFT             )
         CALL ERRINT ( '*', RIGHT            )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)' )
         CALL CHKOUT ( 'REMSUB' )
         RETURN
 
      ELSE
 
         L = LEFT
         R = RIGHT
 
      END IF
 
 
C
C     How much of the input string will we use? And how big is the
C     output string?
C
      INLEN  = LASTNB ( IN  )
      OUTLEN = LEN    ( OUT )
 
C
C     Copy the first part of the input string. (One character at a
C     time, in case this is being done in place.)
C
      DO I = 1, MIN ( L-1, OUTLEN )
 
         OUT(I:I) = IN(I:I)
 
      END DO
 
C
C     Now move the rest of the string over.
C
      I = L
      J = R + 1
 
      DO WHILE ( ( I .LE. OUTLEN ) .AND. ( J .LE. INLEN ) )
 
         OUT(I:I) = IN(J:J)
         I        = I + 1
         J        = J + 1
 
      END DO
 
C
C     Pad with blanks, if necessary.
C
      IF ( I .LE. OUTLEN ) THEN
 
         OUT(I: ) = ' '
 
      END IF
 
 
      CALL CHKOUT ( 'REMSUB' )
      RETURN
      END
