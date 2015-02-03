C$Procedure      NTHWD ( Nth word in a character string )
 
      SUBROUTINE NTHWD ( STRING, NTH, WORD, LOC )
 
C$ Abstract
C
C      Return the Nth word in a character string, and its location
C      in the string.
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
C      CHARACTER,  PARSING,  SEARCH,  WORD
C
C$ Declarations
 
      CHARACTER*(*)    STRING
      INTEGER          NTH
      CHARACTER*(*)    WORD
      INTEGER          LOC
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      STRING     I   Input character string.
C      NTH        I   Index of the word to be returned.
C      WORD       O   The N'TH word in STRING.
C      LOC        O   Location of WORD in STRING.
C
C$ Detailed_Input
C
C      STRING     is the input string to be parsed. It contains
C                 some number of words, where a word is any string
C                 of consecutive non-blank characters.
C
C      NTH        is the index of the word to be returned. (One for
C                 the first word, two for the second, and so on.)
C
C$ Detailed_Output
C
C      WORD       is the N'th word in STRING. If STRING is blank,
C                 or NTH is nonpositive or too large, WORD is blank.
C
C                 WORD may overwrite STRING.
C
C      LOC        is the location of WORD in STRING. (That is, WORD
C                 begins at STRING(LOC:LOC). If STRING is blank, or
C                 NTH is nonpositive or too large, LOC is zero.
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
C      NTHWD, like NEXTWD, is useful primarily for parsing input
C      commands consisting of one or more words, where a word is
C      defined to be any sequence of consecutive non-blank characters.
C      Successive calls to NEXTWD allow the calling routine to neatly
C      parse and process one word at a time.
C
C      The chief difference between the two routines is that
C      NTHWD allows the calling routine to access the words making
C      up the input string in random order. (NEXTWD allows only
C      sequential access.)
C
C$ Examples
C
C      Let STRING be ' Now is the time,   for all good men     to come.'
C
C      If N = -1   WORD = ' '          LOC =  0
C              0          ' '                 0
C              1,         'Now'               2
C              2,         'is'                6
C              3,         'the'               9
C              4,         'time,'            13
C              5,         'for'              21
C              6,         'all'              25
C              7,         'good'             29
C              8,         'men'              34
C              9,         'to'               42
C             10,         'come.'            45
C             11,         ' '                 0
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
C-     SPICELIB Version 1.1.0, 10-MAY-2006 (EDW)
C
C         Added logic to prevent the evaluation of STRING(I:I)
C         if I exceeds the length of STRING. Functionally, the
C         evaluation had no effect on NTHWD's output, but the ifort
C         F95 compiler flagged the evaluation as an array 
C         overrun error. This occurred because given:
C
C             A .AND. B
C
C         ifort evaluates A then B then performs the logical
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
C     nth word in a character_string
C
C-&
 
 
 
 
C
C     Local variables
C
      INTEGER          N
      INTEGER          I
      INTEGER          LENGTH

      LOGICAL          LOOP
 
 
 
C
C     Trivial cases first. Blank STRING? Nonpositive NTH?
C
      IF ( STRING .EQ. ' ' .OR. NTH .LT. 1 ) THEN
 
         WORD = ' '
         LOC  =  0
         RETURN
 
      END IF
 
C
C     Skip leading blanks.
C
      LOC = 1
 
      DO WHILE ( STRING(LOC:LOC) .EQ. ' ' )
         LOC = LOC + 1
      END DO
 
C
C     If we wanted the first word, we have the location. Otherwise,
C     keep stepping through STRING. Quit when the N'TH word is found,
C     or when the end of the string is reached. (The current word is
C     ended whenever a blank is encountered.)
C
C     N is the number of words found so far.
C     I is the current location in STRING.
C
      N      = 1
      I      = LOC
      LENGTH = LEN ( STRING )
 
      DO WHILE ( I .LT. LENGTH .AND. N .LT. NTH )
 
         I = I + 1
 
C
C        Blank signals end of the current word.
C
         IF ( STRING(I:I) .EQ. ' ' ) THEN
 
C
C           Skip ahead to the next one.  The logic ensures no
C           evaluation of STRING(I:I) if I > LEN(STRING).
C
            LOOP = I .LE. LENGTH
            IF( LOOP ) THEN
               LOOP = LOOP .AND. STRING(I:I) .EQ. ' '
            END IF

            DO WHILE ( LOOP )
               I = I + 1
               
               IF( I .GT. LENGTH ) THEN
                  LOOP = .FALSE.
               ELSE IF ( STRING(I:I) .NE. ' ' ) THEN
                  LOOP = .FALSE.
               ELSE
                  LOOP = .TRUE.
               END IF

            END DO
 
C
C           If not at the end of the string, we have another word.
C
            IF ( I .LE. LENGTH ) THEN
               N   = N + 1
               LOC = I
            END IF
 
         END IF
 
      END DO
 
C
C     Couldn't find enough words? Return blank and zero.
C
      IF ( N .LT. NTH ) THEN
         WORD = ' '
         LOC  =  0
 
C
C     Otherwise, find the rest of WORD (it continues until the next
C     blank), and return the current LOC.
C
      ELSE
         I = INDEX ( STRING(LOC: ), ' ' )
 
         IF ( I .EQ. 0 ) THEN
            WORD = STRING(LOC: )
         ELSE
            WORD = STRING(LOC:LOC+I-1)
         END IF
      END IF
 
      RETURN
      END
 
