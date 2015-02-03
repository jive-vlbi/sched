 
C$Procedure UPTO ( Up to the next index of a substring )
 
      INTEGER FUNCTION UPTO ( STRING, SUBSTR, START )
      IMPLICIT NONE
 
C$ Abstract
C
C     Return up to (but not including) the index of the next occurrence
C     of a substring within a string, after some initial offset.
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
C     CHARACTER, PARSING, SEARCH, STRING, TEXT
C
C$ Declarations
 
      CHARACTER*(*)         STRING
      CHARACTER*(*)         SUBSTR
      INTEGER               START
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STRING     I   Input string.
C     SUBSTR     I   Target substring.
C     START      I   Begin searching here.
C
C$ Detailed_Input
C
C     STRING      is an arbitrary input string.
C
C     SUBSTR      is the target substring to be located.
C
C     START       is the location at which to begin searching. That is,
C                 the search is confined to STRING(START: ).
C
C$ Detailed_Output
C
C     The function returns one less than the next location of the
C     target substring within the string, or the length of the string
C     if the substring is not found.
C
C$ Exceptions
C
C     1) If START is greater than the length of the string, the
C        function returns zero.
C
C     2) If START is less than one it is treated as if were one.
C
C$ Particulars
C
C     UPTO is used primarily for extracting substrings bounded by
C     a delimiter. Because the function returns the length of the
C     string when the target substring is not found, the reference
C
C        NEXT = STRING ( START : UPTO ( STRING, SUBSTR, START ) )
C
C     is always legal.
C
C$ Examples
C
C     The following code fragment extracts (and prints) substrings
C     bounded by slash (/) characters.
C
C        BEGIN = 1
C        END   = BEGIN
C
C        DO WHILE ( END .NE. 0 )
C           END   = UPTO ( STR, '/', BEGIN )
C
C           WRITE (6,*) 'Next token is ', STR(BEGIN:END)
C
C           BEGIN = END + 2
C        END DO
C
C     Notice that the loop ends when BEGIN is greater than the length
C     of the string, causing the function to return zero.
C
C     Notice also that the last token in the string is printed whether
C     or not the string ends with a slash.
C
C     If STRING is
C
C        'first/second/third/fourth'
C
C     the output from the fragment is
C
C        Next token is first
C        Next token is second
C        Next token is third
C        Next token is fourth
C
C     Contrast this with the following fragment, written using the
C     intrinsic function INDEX.
C
C        BEGIN = 1
C        END   = BEGIN
C
C        DO WHILE ( END .NE. 0 )
C           I = INDEX ( STR(BEGIN: ), '/' )
C
C           IF ( I .GT. 0 ) THEN
C              END = BEGIN + I - 1
C           ELSE
C              END = LEN ( STR )
C           END IF
C
C           WRITE (6,*) 'Next token is ', STR(BEGIN:END)
C
C           BEGIN = END + 2
C
C           IF ( BEGIN .GT. LEN ( STR ) ) THEN
C              END = 0
C           END IF
C        END DO
C$ Files
C
C     None.
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
C     Version B1.0.0, 4-APR-1988, (IMU) (WLT)
C
C-&
 
 
 
C
C     Local variables
C
      INTEGER               STRLEN
      INTEGER               I
      INTEGER               B
 
C
C     Just like it says in the header.
C
      STRLEN = LEN ( STRING )
      B      = MAX ( 1, START )
 
      IF ( B .GT. STRLEN ) THEN
         UPTO = 0
 
      ELSE
         I = INDEX ( STRING(B: ), SUBSTR )
 
         IF ( I .GT. 0 ) THEN
            UPTO = B + I - 2
         ELSE
            UPTO = STRLEN
         END IF
      END IF
 
      RETURN
      END
