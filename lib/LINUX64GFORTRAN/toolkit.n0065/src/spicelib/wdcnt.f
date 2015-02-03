C$Procedure WDCNT ( Word Count )
 
      INTEGER FUNCTION WDCNT ( STRING )
 
C$ Abstract
C
C      Return the number of words in a string.
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
C      STRING,  WORD
C
C$ Declarations
 
      CHARACTER*(*)    STRING
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      STRING     I   Input character string.
C      WDCNT      O   The number of words in string.
C
C$ Detailed_Input
C
C      STRING      is the input string to be parsed. It contains
C                  some number of words, where a word is any string
C                  of consecutive non-blank characters delimited
C                  by a blank or by either end of the string.
C
C$ Detailed_Output
C
C      WDCNT       is the number of words in the input character
C                  string.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C      Error free.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C      WDCNT, like NTHWD and NEXTWD, is useful primarily for parsing
C      input commands consisting of one or more words, where a word is
C      defined to be any sequence of consecutive non-blank characters
C      delimited by either a blank or by either end of the string.
C
C$ Examples
C
C      The following examples illustrate the use of WDCNT.
C
C            WDCNT ( 'Now is the time'  )   = 4
C            WDCNT ( '  for all  '      )   = 2
C            WDCNT ( 'good,men.to_come' )   = 1
C            WDCNT ( ' '                )   = 0
C
C$ Restrictions
C
C      None.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      I.M. Underwood  (JPL)
C
C$ Version
C
C-     SPICELIB Version 1.1.0, 10-JAN-2005 (EDW)
C
C         Added logic to prevent the evaluation of STRING(LOC:LOC)
C         if LOC exceeds the length of string. Functionally, the
C         evaluation had no effect on WDCNT's output, but the NAG
C         F95 compiler flagged the evaluation as an array 
C         overrun error. This occurred because given:
C
C             A .AND. B
C
C         NAG evaluates A then B then performs the logical
C         comparison.
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     word count
C
C-&
 
 
 
 
 
C
C     Local variables
C
      INTEGER          N
      INTEGER          LOC
      INTEGER          LENGTH
      LOGICAL          CONT
 
 
C
C     This is just NTHWD, except that it keeps looking until
C     it finds the last word.
C
 
C
C     Trivial case first.
C
      IF ( STRING .EQ. ' ' ) THEN
 
         WDCNT = 0
         RETURN
 
      ELSE
 
         LENGTH = LEN ( STRING )
 
      END IF
 
C
C     Skip leading blanks.
C
      LOC = 1
 
      DO WHILE ( STRING(LOC:LOC) .EQ. ' ' )
         LOC = LOC + 1
      END DO
 
C
C     Keep stepping through STRING, counting words as we go.
C     (The current word is ended whenever a blank is encountered.)
C     Quit when the end of the string is reached.
C
C     N is the number of words found so far.
C     LOC is the current location in STRING.
C
      N = 1
 
      DO WHILE ( LOC .LT. LENGTH )
 
         LOC = LOC + 1
 
C
C        Blank signals end of the current word.
C
         IF ( STRING(LOC:LOC) .EQ. ' ' ) THEN
 
C
C           Skip ahead to the next word. Ensure no
C           evaluation of STRING(LOC:LOC) when
C           LOC = LENGTH+1.
C
            CONT = LOC .LE. LENGTH
            IF( CONT ) THEN 
               CONT = CONT .AND. STRING(LOC:LOC) .EQ. ' '
            END IF

            DO WHILE ( CONT )
               LOC = LOC + 1
               
               CONT = LOC .LE. LENGTH
               IF( CONT ) THEN 
                  CONT = CONT .AND. STRING(LOC:LOC) .EQ. ' '
               END IF
                
            END DO
 
C
C           If not at the end of the string, we have another word.
C
            IF ( LOC .LE. LENGTH ) THEN
               N = N + 1
            END IF
 
         END IF
 
      END DO
 
C
C     Return the number of words found.
C
      WDCNT = N
 
      RETURN
      END
 
 
