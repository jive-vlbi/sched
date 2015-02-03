C$Procedure      LX4UNS (Scan for unsigned integer)
 
      SUBROUTINE LX4UNS ( STRING, FIRST, LAST, NCHAR )
 
C$ Abstract
C
C     Scan a string from a specified starting position for the
C     end of an unsigned integer.
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
C      PARSING
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
C      LAST       O   last character that is part of an unsigned integer
C      NCHAR      O   number of characters in the unsigned integer.
C
C$ Detailed_Input
C
C     STRING      is any character string.
C
C     FIRST       is the location in the string to beginning scanning
C                 for an unsigned integer.  It is assumed that the
C                 unsigned integer begins at FIRST.
C
C$ Detailed_Output
C
C     LAST        is the last character at or after FIRST such that
C                 the substring STRING(FIRST:LAST) is an unsigned
C                 integer.  If there is no such substring, LAST
C                 will be returned with the value FIRST-1.
C
C     NCHAR       is the number of characters in the unsigned integer
C                 that begins at FIRST and ends at last.  If there
C                 is no such string NCHAR will be given the value 0.
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
C     1) If FIRST is beyond either end of the string, then
C        LAST will be returned with the value FIRST and NCHAR
C        will be returned with the value 0.
C
C     2) If STRING(FIRST:FIRST) is not part of an unsigned integer
C        then LAST will be returned with the value FIRST-1 and NCHAR
C        will be returned with the value 0.
C
C$ Particulars
C
C     This routine allows you to scan forward in a string to locate
C     an unsigned integer that begins on the input character FIRST.
C
C
C$ Examples
C
C     Suppose you believe that a string has the form
C
C        X%Y%Z
C
C     where X, Y, and Z are unsigned integers of some unknown
C     length and % stands for some non-digit character. You could
C     use this routine to locate the unsigned integers in the
C     string as shown below.  We'll keep track of the beginning and
C     ending of the unsigned integers in the integer arrays B and E.
C
C     FIRST = 1
C     I     = 0
C
C     DO WHILE ( FIRST .LT. LEN(STRING) )
C
C        CALL LX4UNS ( STRING, FIRST, LAST, NCHAR )
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
C     1) Assumes ICHAR returns values in the range [-128, 255].
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 03-DEC-2001 (NJB)
C
C        Updated to work if non-printing characters are present in
C        the input string.  Updated Restrictions section.
C
C-    SPICELIB Version 1.0.0, 12-JUL-1994 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Scan a string for an unsigned integer.
C
C-&
 
C
C     Local parameters
C
      INTEGER               MAXC
      PARAMETER           ( MAXC    =   255 )

      INTEGER               MINC
      PARAMETER           ( MINC    =  -128 )

C
C     Local variables
C 
      INTEGER               I
      INTEGER               L
 
      LOGICAL               DIGIT ( MINC : MAXC )
      LOGICAL               DOINIT
 
C
C     Saved variables
C
      SAVE
 
C
C     Initial values
C
      DATA                  DOINIT / .TRUE. /
 
C
C     First we perform some initializations that are needed on
C     each pass through this routine.
C
      IF ( DOINIT ) THEN
 
         DOINIT = .FALSE.
 
         DO I = MINC, MAXC
            DIGIT(I)  = .FALSE.
         END DO
 
         DIGIT(ICHAR('0')) = .TRUE.
         DIGIT(ICHAR('1')) = .TRUE.
         DIGIT(ICHAR('2')) = .TRUE.
         DIGIT(ICHAR('3')) = .TRUE.
         DIGIT(ICHAR('4')) = .TRUE.
         DIGIT(ICHAR('5')) = .TRUE.
         DIGIT(ICHAR('6')) = .TRUE.
         DIGIT(ICHAR('7')) = .TRUE.
         DIGIT(ICHAR('8')) = .TRUE.
         DIGIT(ICHAR('9')) = .TRUE.
 
      END IF
 
 
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
C     Now for the real work of the routine. Examine characters one
C     at a time...
C
      DO I = FIRST, L
C
C        If this character is a digit, move the LAST pointe one
C        further down on the string.  Otherwise set NCHAR and return.
C
         IF ( DIGIT(ICHAR(STRING(I:I))) ) THEN
            LAST  = LAST + 1
         ELSE
            NCHAR = LAST + 1 - FIRST
            RETURN
         END IF
      END DO
 
      NCHAR = LAST + 1 - FIRST
 
      RETURN
      END
