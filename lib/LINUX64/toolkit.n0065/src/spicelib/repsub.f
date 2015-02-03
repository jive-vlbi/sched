C$Procedure      REPSUB ( Replace one substring with another )
 
      SUBROUTINE REPSUB ( IN, LEFT, RIGHT, STRING, OUT )
 
C$ Abstract
C
C     Replace the substring (LEFT:RIGHT) with a string of any length.
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
C     ASSIGNMENT
C     CHARACTER
C     STRING
C
C$ Declarations
 
      CHARACTER*(*)         IN
      INTEGER               LEFT
      INTEGER               RIGHT
      CHARACTER*(*)         STRING
      CHARACTER*(*)         OUT
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     IN         I   Input string.
C     LEFT,
C     RIGHT      I   Ends of substring to be replaced.
C     STRING     I   Replacement string.
C     OUT        O   Resulting string.
C
C$ Detailed_Input
C
C     IN         is an arbitrary character string.
C
C     LEFT,
C     RIGHT      are the ends of the substring to be replaced.
C                Legitimate substrings satisfy the following
C                conditions
C
C                    RIGHT > LEFT - 2
C                    LEFT  > 1
C                    RIGHT < LEN(STRING) + 1
C
C                This allows users to refer to zero-length substrings
C                (null substrings) of IN.
C
C     STRING     is the replacement string. Essentially, the
C                substring (LEFT:RIGHT) is removed from the
C                input string, and STRING is inserted at the
C                point of removal.
C
C$ Detailed_Output
C
C     OUT        is the resulting string. OUT may overwrite IN.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If RIGHT is one less than LEFT, the substring to
C        replace will be the null substring.  In this case,
C        STRING will be inserted between IN(:RIGHT) and IN(LEFT:).
C
C     2) If LEFT is smaller than one, the error SPICE(BEFOREBEGSTR)
C        is signalled.
C
C     3) If RIGHT is greater than the length of the input string,
C        the error SPICE(PASTENDSTR) is signalled.
C
C     4) If RIGHT is less than LEFT-1, the error SPICE(BADSUBSTR)
C        is signalled.
C
C     5) Whenever the output string is too small to hold the result,
C        the result is truncated on the right.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Ideally, replacement could be done with simple concatenation,
C
C        OUT = IN(1:LEFT-1) // STRING // IN(RIGHT+1: )
C
C     but the Fortran 77 standard makes this illegal for strings of
C     unknown length.
C
C$ Examples
C
C     A typical use for this routine might be to replace all
C     occurrences of one word in a string with another word.
C     For example, the following code fragment replaces every
C     occurrence of the word 'AND' with the word 'OR' in the
C     character string LINE.
C
C        LEFT = WDINDX ( LINE, 'AND' )
C
C        DO WHILE ( LEFT .NE. 0 )
C           CALL   REPSUB ( LINE, LEFT, LEFT+2, 'OR', LINE )
C           LEFT = WDINDX ( LINE, 'AND' )
C        END DO
C
C     This routine can also be used to insert substring between
C     two characters.  Consider the string:
C
C         IN   = 'The defendent,, was found innocent.'
C
C     to insert ' Emelda Marcos' between the first and second commas
C     determine the location of the pair ',,'
C
C        RIGHT = POS ( IN, ',,', 1 )
C        LEFT  = RIGHT + 1
C
C     then
C
C        CALL REPSUB ( IN, LEFT, RIGHT, ' Emelda Marcos', OUT )
C
C     The output (OUT) will have the value:
C
C        'The defendent, Emelda Marcos, was found innocent.'
C
C$ Restrictions
C
C     The memory used by STRING and OUT must be disjoint. The memory
C     used by IN and OUT must be identical or disjoint.
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
C-    SPICELIB Version 1.0.2, 17-JUN-1999 (WLT)
C
C        Fixed example code fragment.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 24-AUG-1990 (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     replace one substring with another substring
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      INTEGER               SUMAI
 
C
C     Local variables
C
      INTEGER               INLEN
      INTEGER               STRLEN
      INTEGER               OUTLEN
 
      INTEGER               REMAIN
      INTEGER               USE         ( 3 )
 
      INTEGER               END
      INTEGER               NEXT
      INTEGER               I
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'REPSUB' )
      END IF
 
C
C     Get the lengths of all the strings involved in this transaction.
C
      INLEN  = LEN ( IN     )
      STRLEN = LEN ( STRING )
      OUTLEN = LEN ( OUT    )
 
C
C     Reject bad inputs.
C
      IF ( LEFT .LT. 1 ) THEN
         CALL SETMSG ( 'REPSUB error: LEFT (#) must not be less '     //
     .                 'than 1.' )
 
         CALL ERRINT ( '#', LEFT )
         CALL SIGERR ( 'SPICE(BEFOREBEGSTR)' )
         CALL CHKOUT ( 'REPSUB' )
         RETURN
 
      ELSE IF ( RIGHT .GT. INLEN ) THEN
         CALL SETMSG ( 'REPSUB error: RIGHT (#) must not exceed '     //
     .                 'length of IN (#).' )
         CALL ERRINT ( '#', RIGHT )
         CALL ERRINT ( '#', INLEN )
         CALL SIGERR ( 'SPICE(PASTENDSTR)' )
         CALL CHKOUT ( 'REPSUB' )
         RETURN
 
      ELSE IF ( RIGHT .LT. LEFT-1 ) THEN
         CALL SETMSG ( 'REPSUB error: LEFT (#) must not exceed '      //
     .                 'RIGHT+1 (# + 1). ' )
         CALL ERRINT ( '#', LEFT  )
         CALL ERRINT ( '#', RIGHT )
         CALL SIGERR ( 'SPICE(BADSUBSTR)' )
         CALL CHKOUT ( 'REPSUB' )
         RETURN
 
      END IF
 
C
C     Consider three separate sections:
C
C        1) The front of the original string.
C
C        2) The replacement string.
C
C        3) The end of the original string.
C
C     Determine how much of each section to use in the output string.
C     REMAIN is the number of characters that will fit in the output
C     string.
C
      REMAIN = OUTLEN
      USE(1) = MIN ( REMAIN, LEFT - 1 )
 
      REMAIN = REMAIN - USE(1)
      USE(2) = MIN ( REMAIN, STRLEN )
 
      REMAIN = REMAIN - USE(2)
      USE(3) = MIN ( REMAIN, INLEN - RIGHT )
 
C
C     Move the third section first. It gets moved back to front
C     or front to back, depending on whether the replacement string
C     is longer than the original substring. The main thing is to
C     avoid overwriting characters that have yet to be moved.
C
      END = SUMAI ( USE, 3 )
 
      IF ( LEFT + STRLEN .GT. RIGHT ) THEN
 
         NEXT = END
 
         DO I = USE(3), 1, -1
            OUT(NEXT:NEXT) = IN(RIGHT+I:RIGHT+I)
            NEXT           = NEXT - 1
         END DO
 
      ELSE
 
         NEXT = LEFT + STRLEN
 
         DO I = 1, USE(3)
            OUT(NEXT:NEXT) = IN(RIGHT+I:RIGHT+I)
            NEXT           = NEXT + 1
         END DO
 
      END IF
 
C
C     The first two sections can be moved directly to the front of
C     the output string.
C
      NEXT = 1
 
      DO I = 1, USE(1)
         OUT(NEXT:NEXT) = IN(I:I)
         NEXT           = NEXT + 1
      END DO
 
      DO I = 1, USE(2)
         OUT(NEXT:NEXT) = STRING(I:I)
         NEXT           = NEXT + 1
      END DO
 
C
C     Pad with blanks, if the output string was not filled.
C
      IF ( END .LT. OUTLEN ) THEN
         OUT(END+1: ) = ' '
      END IF
 
 
      CALL CHKOUT ( 'REPSUB' )
      RETURN
      END
