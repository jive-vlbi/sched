C$Procedure      LX4SGN (Scan for signed integer)
 
      SUBROUTINE LX4SGN ( STRING, FIRST, LAST, NCHAR )
 
C$ Abstract
C
C     Scan a string from a specified starting position for the
C     end of a signed integer.
C
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
C       PARSING
C
C$ Declarations
 
      CHARACTER*(*)         STRING
      INTEGER               FIRST
      INTEGER               LAST
      INTEGER               NCHAR
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      STRING     I   any character string
C      FIRST      I   first character to scan from in STRING
C      LAST       O   last character that is part of a signed integer
C      NCHAR      O   number of characters in the signed integer.
C
C$ Detailed_Input
C
C     STRING      is any character string.
C
C     FIRST       is the location in the string to beginning scanning
C                 for a signed integer.  It is assumed that the
C                 signed integer begins at FIRST.
C
C$ Detailed_Output
C
C     LAST        is the last character at or after FIRST such that
C                 the substring STRING(FIRST:LAST) is a signed
C                 integer.  If there is no such substring, LAST
C                 will be returned with the value FIRST-1.
C
C     NCHAR       is the number of characters in the signed integer
C                 that begins at FIRST and ends at last.  If there
C                 is no such string NCHAR will be given the value 0.
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
C     1) If FIRST is beyond either end of the string, then
C        LAST will be returned with the value FIRST and NCHAR
C        will be returned with the value 0.
C
C     2) If STRING(FIRST:FIRST) is not part of a signed integer
C        then LAST will be returned with the value FIRST-1 and NCHAR
C        will be returned with the value 0.
C
C$ Particulars
C
C     This routine allows you to scan forward in a string to locate
C     a signed integer that begins on the input character FIRST.  Note
C     that all unsigned integers are included in the list of signed
C     integers.  The signed integers may in addition have a leading
C     plus ('+') or minus ('-') sign.
C
C
C$ Examples
C
C     Suppose you believe that a string has the form
C
C        X%Y%Z
C
C     where X, Y, and Z are signed integers of some unknown
C     length and % stands for some non-digit character. You could
C     use this routine to locate the signed integers in the
C     string as shown below.  We'll keep track of the beginning and
C     ending of the signed integers in the integer arrays B and E.
C
C     FIRST = 1
C     I     = 0
C
C     DO WHILE ( FIRST .LT. LEN(STRING) )
C
C        CALL LX4SGN ( STRING, FIRST, LAST, NCHAR )
C
C        IF ( NCHAR .GT. 0 ) THEN
C
C           I     = I    + 1
C           B(I)  = FIRST
C           E(I)  = LAST
C           FIRST = LAST + 2
C
C        ELSE
C
C           FIRST = FIRST + 1
C
C        END IF
C
C     END DO
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
C-    SPICELIB Version 1.0.0, 12-JUL-1994 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Scan a string for a signed integer.
C
C-&
 
      INTEGER               F
      INTEGER               I
      INTEGER               L
 
      LAST  = FIRST - 1
      L     = LEN ( STRING )
C
C     If start is beyond the ends of the string, we  can quit now.
C
      IF ( FIRST .LT. 1 .OR. FIRST .GT. L ) THEN
         NCHAR = 0
         RETURN
      END IF
C
C     There are two cases to take care of (and in both cases
C     LX4UNS does almost all of the work).
C
      I = ICHAR( STRING(FIRST:FIRST) )
 
      IF ( I .EQ. ICHAR('+' ) .OR. I .EQ. ICHAR('-') ) THEN
C
C        Case 1. The string begins with a + or -.  There must
C        be an unsigned integer following.
C
         F = FIRST + 1
 
         CALL LX4UNS ( STRING, F, LAST, NCHAR )
 
         IF ( NCHAR .EQ. 0 ) THEN
            LAST  = FIRST - 1
         ELSE
            NCHAR = NCHAR + 1
         END IF
 
      ELSE
C
C        Case 2.  The leading character is not a sign character.
C        We simply check to see how much unsigned integer we have.
C
         CALL LX4UNS ( STRING, FIRST, LAST, NCHAR )
 
      END IF
 
      RETURN
      END
