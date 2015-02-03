C$Procedure      SHIFTC ( Shift a character string )
 
      SUBROUTINE SHIFTC ( IN, DIR, NSHIFT, FILLC, OUT )
 
C$ Abstract
C
C      Shift the contents of a character string to the left or right.
C      Characters moved past the beginning or end of the string are
C      lost. Vacant spaces are filled with a specified character.
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
C      CHARACTER,  UTILITY
C
C$ Declarations
 
      CHARACTER*(*)      IN
      CHARACTER*1        DIR
      INTEGER            NSHIFT
      CHARACTER*1        FILLC
      CHARACTER*(*)      OUT
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      IN         I   Input string.
C      DIR        I   Direction to shift.
C      NSHIFT     I   Number of times to shift.
C      FILLC      I   Character to fill spaces left vacant.
C      OUT        O   Shifted string.
C
C$ Detailed_Input
C
C      IN          is the input character string.
C
C      DIR         is the direction in which the characters in the
C                  string are to be shifted.
C
C                        'L' or 'l'  to shift left.
C                        'R' or 'r'  to shift right.
C
C      NSHIFT      is the number of times the string is to be
C                  shifted.
C
C      FILLC       is the character with which spaces left vacant by
C                  the shift are to be filled.
C
C$ Detailed_Output
C
C      OUT         is the output string. This is the input string,
C                  shifted N times, filled with FILLC.
C
C                  OUT may overwrite IN.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      The first NSHIFT characters of the output string are filled
C      with the fill character, and the input string is appended.
C
C$ Examples
C
C    If FILLC = ' '
C
C           'abcde'   shifted left twice becomes     'cde  '
C           'abcde'   shifted right once becomes     ' abcd'
C
C    If FILLC = '.'
C
C           '12345 '  shifted right once becomes     '.12345'
C           'Apple '  shifted left ten times becomes '......'
C
C$ Restrictions
C
C     SHIFTC is being maintained for historical reasons only.
C     To avoid the overhead imposed by the error handling in this
C     routine, use the equivalent routines SHIFTL and SHIFTR.
C
C$ Exceptions
C
C     1) A negative shift in one direction is equal to a positive
C        shift in the other.
C
C     2) If a legal direction ('L', 'l', 'R', 'r') is not supplied,
C        the error 'SPICE(ILLEGSHIFTDIR)' is signalled.
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
C     shift a character_string
C
C-&
 
 
C$ Revisions
C
C-    Beta Version 1.1.0, 17-OCT-1988 (IMU)
C
C        Dick Simpson reported that the statement
C
C           OUT(N+1: ) = IN
C
C        which began the right-shift section failed on his Data
C        General, presumably because it requires temporary buffering
C        of characters. The new version seems to work for all cases.
C        It has been tested on the VAX and on the Sun (f77 compiler).
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SHIFTC' )
      END IF
 
 
C
C     Hand off to one of the other routines.
C
      IF ( DIR .EQ. 'L' .OR. DIR .EQ. 'l' ) THEN
 
         IF ( NSHIFT .GE. 0 ) THEN
            CALL SHIFTL ( IN, NSHIFT, FILLC, OUT )
 
         ELSE
            CALL SHIFTR ( IN, -NSHIFT, FILLC, OUT )
         END IF
 
      ELSE IF ( DIR .EQ. 'R' .OR. DIR .EQ. 'r' ) THEN
 
         IF ( NSHIFT .GE. 0 ) THEN
            CALL SHIFTR ( IN, NSHIFT, FILLC, OUT )
 
         ELSE
            CALL SHIFTL ( IN, -NSHIFT, FILLC, OUT )
         END IF
 
      ELSE
         CALL SETMSG ( 'Shift direction (#) must be L, l, R, or r.' )
         CALL ERRCH  ( '#', DIR )
 
         CALL SIGERR ( 'SPICE(ILLEGSHIFTDIR)' )
      END IF
 
      CALL CHKOUT ( 'SHIFTC' )
      RETURN
      END
