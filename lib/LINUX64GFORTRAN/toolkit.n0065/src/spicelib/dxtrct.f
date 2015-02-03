C$Procedure      DXTRCT (Extract Double Precision Values From A String)
 
      SUBROUTINE DXTRCT ( KEYWD,   MAXWDS ,  STRING,
     .                    NFOUND,  PARSED,   VALUES   )
 
C$ Abstract
C
C     Locate a keyword and succeeding numeric words within a string.
C     Parse and store the numeric words.  Remove the keyword and
C     numeric words from the input string.
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
C      PARSING,  WORD
C
C$ Declarations
 
      CHARACTER*(*)      KEYWD
      INTEGER            MAXWDS
      CHARACTER*(*)      STRING
      INTEGER            NFOUND
      INTEGER            PARSED
      DOUBLE PRECISION   VALUES ( * )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      KEYWD      I   Keyword used to mark start of a set of numbers.
C      MAXWDS     I   Maximum number of numeric words that can be parsed
C      STRING    I/O  String potentially containing KEYWD and numbers.
C      NFOUND     O   Number of numeric words found following the KEYWD.
C      PARSED     O   Number of numeric words translated and returned.
C      VALUES     O   The double precision values for the numbers.
C
C$ Detailed_Input
C
C      KEYWD      is a word used to mark the start of a set of numeric
C                 words of interest.
C
C      MAXWDS     is the maximum number of numeric words that can be
C                 parsed and returned.
C
C      STRING     is a string potentially containing KEYWD and numbers.
C
C$ Detailed_Output
C
C      STRING     is the input string stripped of all parsed
C                 numeric words.  If there was room available to parse
C                 all of the numeric words associated with KEYWD, the
C                 keyword that marked the beginning of the parsed
C                 numbers in the original string will also be removed.
C
C      NFOUND     is the number of numeric words that were found
C                 following KEYWD but preceding the next non-numeric
C                 word of the string.  If the KEYWD is not present in
C                 the string, NFOUND is returned as -1.  If the keyword
C                 is located but the next word in the string is
C                 non-numeric NFOUND will be returned as 0.
C
C      PARSED     is the number of numeric words that were actually
C                 parsed and stored in the output array VALUES.  If no
C                 values are parsed PARSED is returned as 0.
C
C      VALUES     are the double precision values for the parsed
C                 numeric words that follow the first occurance of the
C                 keyword but precede the next non-numeric word.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      Definitions:
C
C      A WORD        is a set of consecutive non-blank characters
C                    delimited by blanks or the end of the string
C                    that contains them.
C
C      A NUMERIC WORD  a word that can be parsed by the
C                      SPICELIB routine NPARSD without error.  All
C                      FORTRAN numeric representations are numeric
C                      words. In addition 'PI', 'Pi', 'pI', and 'pi'
C                      are all recognized as having the value:
C
C                        3.1415926535897932384626D0
C
C                      See NPARSD FOR A a full description of legitimate
C                      numeric words.
C
C      Given a string and a keyword this routine locates the first
C      occurrance of the keyword in the string and returns the double
C      precision representations of up to MAXWDS succeeding numeric
C      words.  All parsed numeric words are removed from the string.
C      If every numeric word following KEYWD but preceding the next
C      non-numeric word is parsed,  KEYWD will also be removed from
C      the string.
C
C      If the keyword cannot be located in the string, the variable
C      NFOUND will be returned as -1 and the string will be unchanged.
C
C      In all other cases, some part of the string (possibly all of it)
C      will be removed.
C
C$ Examples
C
C     Input   STRING  'LONGITUDE 39.2829  LATITUDE 24.27682'
C             KEYWD   'LONGITUDE'
C             MAXWDS   4
C
C     Output: STRING  '  LATITUDE 24.27682'
C             NFOUND  1
C             PARSED  1
C             VALUES  3.92829D+01
C
C
C
C     Input   STRING  'THIS IS A BAD STRING FOR NUMBERS'
C             KEYWD   'RADIUS'
C             MAXWDS  2
C
C     Output: STRING  'THIS IS A BAD STRING FOR NUMBERS'
C             NFOUND  -1
C             PARSED   0
C             VALUES   (unchanged)
C
C
C
C     Input   STRING  'PRIMES 11  13 17 19 23 NON-PRIMES 12 14 15'
C             KEYWD   'PRIMES'
C             MAXWDS  3
C
C     Output: STRING  'PRIMES  19 23 NON-PRIMES 12 14 15'
C             NFOUND  5
C             PARSED  3
C             VALUES  1.1D+01
C                     1.3D+01
C                     1.7D+01
C
C     Input   STRING  'PRIMES 11  13 17 19 23 NON-PRIMES 12 14 15'
C             KEYWD   'PRIMES'
C             MAXWDS  5
C
C     Output: STRING  ' NON-PRIMES 12 14 15'
C             NFOUND  5
C             PARSED  5
C             VALUES  1.1D+01
C                     1.3D+01
C                     1.7D+01
C                     1.9D+01
C                     2.3D+01
C
C$ Restrictions
C
C      None.
C
C$ Exceptions
C
C      Error free.
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
C-     SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.1.0, 23-MAY-1990 (HAN)
C
C         The variable FOUND was changed to NFOUND.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     extract d.p. values from a string
C
C-&
 
 
 
 
C$ Revisions
C
C-     SPICELIB Version 1.1.0, 23-MAY-1990 (HAN)
C
C         The variable FOUND was changed to NFOUND. Other SPICELIB
C         routines that use the variable FOUND declare it as a logical.
C         In order to conform to this convention, FOUND was changed to
C         NFOUND to indicate that it has an integer value, not a logical
C         value.
C
C-&
 
 
C
C     SPICELIB functions
C
      INTEGER          LASTNB
      INTEGER          WDINDX
      INTEGER          NBLEN
 
C
C     Local variables
C
      INTEGER          LENGTH
      INTEGER          POSITN
 
      INTEGER          BERASE
      INTEGER          EERASE
      INTEGER          FALLBK
 
      INTEGER          START
      INTEGER          I
      INTEGER          J
 
 
      DOUBLE PRECISION X
      CHARACTER*80     ERROR
      INTEGER          PNTR
 
C
C     No keywords or numbers have been located yet.
C
      NFOUND  =  0
      PARSED  =  0
 
C
C     Locate the keyword within the string and get the length of the
C     string.
C
      POSITN  =  WDINDX ( STRING, KEYWD )
      LENGTH  =  LASTNB ( STRING      )
 
      IF ( POSITN .EQ. 0 ) THEN
         NFOUND = -1
         PARSED =  0
         RETURN
      END IF
 
C
C     Set the begin erase marker to the start of the current word
C     Set the end   erase marker to the end   of the current word
C
      BERASE  = POSITN
      EERASE  = POSITN + NBLEN(KEYWD) - 1
 
      START   = EERASE + 1
 
      IF ( START .LT. LENGTH ) THEN
 
C
C        Locate the next word and try to parse it ...
C
         CALL FNDNWD ( STRING,      START, I,     J    )
         CALL NPARSD ( STRING(I:J), X,     ERROR, PNTR )
 
         IF ( ERROR .EQ. ' ' ) THEN
C
C           ...  mark its starting position as a possible starting
C           point for deletion if we run out of room for parsed numbers.
C
            FALLBK         = I
            EERASE         = J
            START          = J      + 1
            NFOUND         = NFOUND + 1
            PARSED         = PARSED + 1
            VALUES(PARSED) = X
         END IF
 
      ELSE
 
         STRING(BERASE: )  = ' '
         RETURN
 
      END IF
 
 
C
C     Now find all of the succeeding numeric words until we run out of
C     numeric words or string to look at.
C
      DO WHILE (       ( START .LT. LENGTH )
     .           .AND. ( ERROR .EQ. ' '    ) )
 
C
C        Find the next word and try to parse it as a number.
C
         CALL FNDNWD ( STRING,      START, I,     J       )
         CALL NPARSD ( STRING(I:J), X,     ERROR, PNTR )
 
         IF ( ERROR .EQ. ' ' ) THEN
 
C
C           It's a number! Congratulations!
C
            NFOUND = NFOUND + 1
 
C
C           If there is room ...
C
            IF ( NFOUND .LE. MAXWDS ) THEN
 
C
C              1.  Increment the counter PARSED.
C              2.  Load the DP value into the output array.
C              3.  Set the pointer for the end of the erase
C                   region to be the end of this word.
C
               PARSED         = PARSED + 1
               VALUES(PARSED) = X
               EERASE         = J
 
            ELSE
 
C
C              Set the pointer of the begin erase region to be the
C              the pointer set up just for this occasion.
C
               BERASE = FALLBK
 
            END IF
 
C
C           Set the place to begin looking for the next word to be
C           at the first character following the end of the current
C           word.
C
            START = J + 1
 
         END IF
 
      END DO
 
C
C     Remove the parsed words from the string.
C
      I = BERASE
      J = EERASE + 1
 
      DO WHILE ( J .LE. LENGTH )
         STRING(I:I) = STRING(J:J)
         I           = I + 1
         J           = J + 1
      END DO
 
      STRING(I:) = ' '
 
      RETURN
 
      END
 
