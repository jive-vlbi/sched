C$Procedure      ZZREPSUB ( Replace one substring with another )
 
      SUBROUTINE ZZREPSUB ( IN, LEFT, RIGHT, STRING, OUT )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
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
 
      IMPLICIT NONE
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
C     2) If LEFT is smaller than one, it's treated as 1.
C
C     3) If RIGHT is greater than the length of the input string,
C        it is treated as being the length of the string.
C
C     4) If RIGHT is less than LEFT-1, no substitution is made.
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
C     This private routine is basically just a copy of the SPICE
C     routine REPSUB with all error handling removed and "reasonable"
C     interpretations used for exceptional cases.
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
C           CALL REPSUB ( LINE, LEFT, LEFT+2, 'OR', LINE )
C           LEFT = WDINDX ( LINE, 'AND' )
C        END DO
C
C     This routine can also be used to insert substring between
C     two characters.  Consider the string:
C
C         IN   = 'The defendant,, was found innocent.'
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
C        'The defendant, Emelda Marcos, was found innocent.'
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
C-    SPICELIB Version 1.0.0, 27-APR-1996 (WLT)
C
C
C-&
 
 
 
 
 
C
C     SPICELIB functions
C
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
      INTEGER               MYLEFT
      INTEGER               MYRGHT
 
C
C     Get the lengths of all the strings involved in this transaction.
C
      INLEN  = LEN ( IN     )
      STRLEN = LEN ( STRING )
      OUTLEN = LEN ( OUT    )
      MYLEFT = MIN( INLEN+1, MAX(1, LEFT ) )
      MYRGHT = MIN( INLEN,   MAX(0, RIGHT) )
C
C     Reject bad inputs.
C
      IF ( MYLEFT .LT. 1 ) THEN
 
         MYLEFT = 1
 
      ELSE IF ( MYRGHT .GT. INLEN ) THEN
 
         MYRGHT = INLEN
 
      ELSE IF ( RIGHT .LT. LEFT-1 ) THEN
 
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
      USE(1) = MIN ( REMAIN, MYLEFT - 1 )
 
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
 
      IF ( MYLEFT + STRLEN .GT. RIGHT ) THEN
 
         NEXT = END
 
         DO I = USE(3), 1, -1
            OUT(NEXT:NEXT) = IN(RIGHT+I:RIGHT+I)
            NEXT           = NEXT - 1
         END DO
 
      ELSE
 
         NEXT = MYLEFT + STRLEN
 
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
 
 
      RETURN
      END
