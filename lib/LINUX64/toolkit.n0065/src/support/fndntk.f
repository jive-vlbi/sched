C$Procedure      FNDNTK ( Find the next token in a string )
 
      SUBROUTINE FNDNTK ( STRING, DELIMS, START, BEG, END )
      IMPLICIT NONE
 
C$ Abstract
C
C      Find the next token in a string delimited by multiple delimiters.
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
C      CHARACTER,  STRING,  PARSING
C
C$ Declarations
 
      CHARACTER*(*)         STRING
      CHARACTER*(*)         DELIMS
      INTEGER               START
      INTEGER               BEG
      INTEGER               END
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      STRING     I    String of items delimited by DELIMS.
C      DELIMS     I    Single characters which delimit items.
C      START      I    Point to start looking for a token in the string.
C      BEG        O    Beginning index of the token.
C      END        O    End index of the token.
C
C$ Detailed_Input
C
C      STRING      is a character string containing tokens delimited
C                  by any one of the characters in the string DELIMS.
C                  Consecutive delimiters, and delimiters at the
C                  beginning and end of the string, are considered to
C                  delimit null items. A blank string is considered to
C                  contain a single (blank) item.
C
C      DELIMS      contains the individual characters which delimit
C                  the items in the string. These may be any ASCII
C                  characters, including blanks.
C
C                  However, by definition, consecutive blanks are NOT
C                  considered to be consecutive delimiters. Nor is
C                  a blank and any other delimiter considered to be
C                  consecutive delimiters. In addition, leading and
C                  trailing blanks are ignored.  (See "Particulars"
C                  for a discussion of blanks and how they are treated.)
C
C      START       is the point in the string to begin looking for the
C                  next token in the string.  To search for tokens in
C                  a string begin with START = 1, and for subsequent
C                  calls set START to END + 2, where END was returned
C                  by the previous call.
C
C$ Detailed_Output
C
C      BEG         is the beginning of the substring containing the
C                  token.
C
C      END         is the end of the substring containing the token.
C
C$ Exceptions
C
C      1.  If START is less than 1 it will be treated as though it were
C          1.
C
C      2.  If START is the declared length of the string plus 1 and the
C          last non-blank character is a delimiter (or the string is
C          blank) START will be regarded as pointing at a null token.
C          BEG = LEN(STRING) + 1, END = LEN(STRING).
C
C      3.  If START is at least two greater than the declared length of
C          the string, BEG and END will be returned as zero.
C
C
C$ Particulars
C
C
C      For the purposes of discussion, we regard STRING to be a
C      substring of the string that begins with a meta-delimiter
C      followed by STRING and ends with a meta-delimiter.  The
C      meta-delimiters have indexes 0 and LEN(STRING)+1.
C      Meta-delimiters are non-blank delimiters.
C
C      A token is a substring that
C
C         1.  begins with a non-blank character,
C         2.  ends   with a non-blank character,
C         3.  contains no delimiters
C         4.  cannot be extended on either end without violating
C             on of the first 3 conditions.
C
C      A good question to ask at this point is:
C
C         "Suppose that ',' is a delimiter and the string contains
C          the substring ',    ,' .  Is there a token between
C          the two commas?
C
C      Our answer to this question is "Yes".  But from the rules
C      1 through 4 above, whatever it is can contain no characters.
C      We call such a token a null token.  Another question:
C
C         "Ok. There's a token. Where does it begin and end?"
C
C      Now we have to adopt some convention.  The only consistent
C      one we could think of was this:  The null token begins at
C      the second delimiter and ends at the previous character.
C
C      Beginning at the second delimiter seems reasonable.  The
C      only consistent way to define the end is to give an index
C      such that the length computation END - BEG + 1 yields 0.
C      So whatever, we do for the beginning, end must be BEG - 1.
C
C      Choosing the beginning to be the second of the two delimiters
C      makes it possible to easily move on to the next delimiter.
C      If the assignment START = END + 2 is made after a call to
C      the routine, then START will always point beyond the end
C      of the token just found and will always point no further
C      than the beginning of the next token ( if there is one).
C      If we keep in mind that there are meta-delimiters at the ends
C      of the string then a string that begins with ',    ...'
C      begins with a null token. A string that ends with ...   ,  '
C      ends with a null token.  In the first case the beginning
C      of the null token is at character 1 of the string. In the second
C      case the null token begins at LEN(STRING) + 1,  i.e. at the
C      meta-delimiter past the end of the string.
C
C      Using these conventions, this routine finds the beginning and
C      end of the first token that begins at or following the input
C      START position in the string.  If no tokens follow the input
C      index, then both BEG and END will be returned as zero.  This is
C      the only case in which BEG will be returned as non-positive.
C
C$ Examples
C
C      STRING =
C
C      'A FEW OF US, THE BAD-BOYS, WENT TO TOWN IN 8//1984-'
C
C                1         2         3         4         5
C       123456789012345678901234567890123456789012345678901
C
C       If DELIMS = ' ,-/'
C
C          Tokens      BEG      END
C          ------      ---      ---
C          'A'          1        1
C          'FEW'        3        5
C          'OF'         7        8
C          'US'        10       11
C          'THE'       14       16
C          'BAD'       18       20
C          'BOYS'      22       24
C          'WENT'      28       31
C          'TO'        33       34
C          'TOWN'      36       39
C          'IN'        41       42
C          '8'         44       44
C           null       46       45
C          '1984'      47       50
C           null       52       51
C
C
C       If DELIMS = ',/'
C
C          Tokens                BEG      END
C          ------                ---      ---
C          'A FEW OF US'           1       11
C          'THE BAD-BOYS'         18       25
C          'WENT TO TOWN IN 8'    28       44
C           null                  46       45
C          '1984-'                47       51
C
C
C       To get all of the tokens in a string the following loop of code
C       will suffice
C
C
C             BEG   = 1
C             START = 1
C
C             DO WHILE ( BEG .NE. 0 )
C
C                CALL FNDNTK ( STRING, DELIMS, START, BEG, END )
C
C                   do something with the token STRING(BEG:END) taking
C                   appropriate care of the null tokens.
C
C                START = END + 2
C
C             END DO
C
C
C$ Restrictions
C
C      None.
C
C$ Files
C
C      None.
C
C$ Author_and_Institution
C
C      I.M. Underwood (JPL)
C      W.L. Taber     (JPL)
C
C$ Literature_References
C
C      None.
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
C     Version B1.0.0, 3-MAY-1988 (WLT) (IMU)
C
C-&
 
 
 
C
C     Local variables
C
 
      LOGICAL               SPACE
      INTEGER               LAST
      INTEGER               EOL
      INTEGER               B
      LOGICAL               BLANK
      INTEGER               NBR
      INTEGER               NBL
      LOGICAL               DELIMR
      LOGICAL               DELIML
      LOGICAL               NODELM
 
C%&END_DECLARATIONS
 
 
C
C     First we gather some data regarding the input string and
C     delimiters
C
      SPACE = INDEX( DELIMS, ' ' ) .NE. 0
      LAST  = LEN  ( STRING        )
      EOL   = LAST + 1
      B     =  MAX( 1, START )
 
C
C     We don't have to do anything if we are starting past the end of
C     the string.
C
      IF ( B .GT. EOL ) THEN
         BEG = 0
         END = 0
         RETURN
      END IF
 
C
C     Find the first non-blank character at or to the right of where
C     we are starting.
C
      BLANK = .TRUE.
      NBR   =  B
 
      DO WHILE ( BLANK )
 
         IF      ( NBR  .GE. EOL )            THEN
            BLANK = .FALSE.
         ELSE IF ( STRING(NBR:NBR) .NE. ' ' ) THEN
            BLANK = .FALSE.
         ELSE
            NBR    = NBR + 1
         END IF
 
      END DO
 
C
C     Find the first non-blank character and first non-blank delimiter
C     to the left of the starting point.
C
      BLANK  = .TRUE.
      NBL    = B - 1
 
      DO WHILE ( BLANK )
 
         IF ( NBL .LE. 0 ) THEN
            BLANK = .FALSE.
         ELSE IF ( STRING(NBL:NBL) .NE. ' ' ) THEN
            BLANK = .FALSE.
         ELSE
            NBL   = NBL - 1
         END IF
      END DO
 
C
C     If both the preceeding non-blank character and the following
C     non-blank character are delimiters, we have a null item.
C
      IF ( NBR .GE. EOL ) THEN
         DELIMR = .TRUE.
      ELSE
         DELIMR = INDEX( DELIMS, STRING(NBR:NBR) ) .NE. 0
      END IF
 
      IF ( NBL .LE. 0   ) THEN
         DELIML = .TRUE.
      ELSE
         DELIML = INDEX( DELIMS, STRING(NBL:NBL) ) .NE. 0
      END IF
 
      IF ( DELIMR .AND. DELIML ) THEN
 
         BEG = NBR
         END = BEG - 1
 
         RETURN
 
      END IF
 
C
C     Still here? See if we were past the last delimiter.
C
      IF ( ( NBR .GE. EOL ) .AND. ( .NOT. DELIML) )  THEN
 
         BEG = 0
         END = 0
         RETURN
 
      END IF
 
C
C     If the left most non-blank is a delimiter OR a blank is a
C     delimiter and the non-blank character to the left is at least
C     two characters away from the right non-blank character, then
C     we have a token beginning at the right non-blank. We just need
C     to find the right boundary.
C
      IF ( DELIML .OR. (       ( NBR - NBL .GE. 2 )
     .                   .AND. ( SPACE            )
     .                   .AND. ( .NOT. DELIMR     ) ) ) THEN
 
         BEG = NBR
         END = BEG
 
C
C        Note: DELIMR is already .FALSE. or else we couldn't get to
C        this point.
C
         DO WHILE ( .NOT. DELIMR )
 
            IF ( END + 1 .GE. EOL ) THEN
               DELIMR = .TRUE.
            ELSE IF ( INDEX( DELIMS, STRING(END+1:END+1) ) .NE. 0 ) THEN
               DELIMR = .TRUE.
            ELSE
               END    = END + 1
            END IF
 
         END DO
 
C
C        Back up END to the first non-blank that precedes it.
C
         DO WHILE ( STRING(END:END) .EQ. ' ')
            END = END - 1
         END DO
 
         RETURN
      END IF
 
C
C     Still here? In that case we were in the middle of something
C     to start with.  Move the pointer forward until we reach a
C     delimiter.
C
C     Keep in mind that DELIMR still has the information as to whether
C     or not NBR points to a non-blank delimiter. We are going to use
C     this information to determine whether to look for a delimiter
C     first or not.
C
      IF ( .NOT. DELIMR ) THEN
 
         NODELM = .TRUE.
         B      =  NBR
 
         DO WHILE ( NODELM )
 
            NBR    = NBR + 1
 
            IF ( NBR .GE. EOL ) THEN
               NODELM = .FALSE.
            ELSE
               NODELM = INDEX ( DELIMS, STRING(NBR:NBR) ) .EQ. 0
            END IF
 
         END DO
 
 
C
C        If a space is a delimiter and we happen to have landed on one,
C        we want to continue until we hit a non-blank delimiter or just
C        before a non-blank character.
C
         IF ( SPACE .AND. NBR .LT. EOL ) THEN
 
            NODELM = STRING(NBR:NBR) .EQ. ' '
 
            DO WHILE ( NODELM )
 
               NBR = NBR + 1
 
               IF ( NBR .EQ. EOL ) THEN
                  NODELM = .FALSE.
               ELSE IF ( INDEX ( DELIMS, STRING(NBR:NBR) ) .NE. 0 ) THEN
                  NODELM = STRING(NBR:NBR) .EQ. ' '
               ELSE IF (                 STRING(NBR:NBR) .NE. ' ' ) THEN
                  NODELM = .FALSE.
C
C                 Back up one, to just before the non-blank character
C
                  NBR    = NBR - 1
               END IF
            END DO
         END IF
C
C        Since we did not start on a delimiter if we reached the end of
C        the string before hitting one, then there is no token to find
C        here.
C
         IF ( NBR .GE. EOL ) THEN
 
            BEG = 0
            END = 0
 
            RETURN
 
         END IF
      END IF
 
C
C     Still here?  Then starting at the first character to the right of
C     the delimiter, find the next non-blank character, and the next
C     right delimiter after that.
C
      NBL   =  NBR
      BLANK = .TRUE.
 
      DO WHILE (BLANK)
 
         NBL = NBL + 1
 
         IF ( NBL .GE. EOL ) THEN
            BLANK = .FALSE.
         ELSE
            BLANK = STRING(NBL:NBL) .EQ. ' '
         END IF
 
      END DO
 
C
C     Now locate the next delimiter.
C
      NBR    = NBL - 1
      DELIMR = .FALSE.
 
      DO WHILE ( .NOT. DELIMR )
 
         NBR =  NBR + 1
 
         IF ( NBR .GE. EOL ) THEN
            DELIMR = .TRUE.
         ELSE
            DELIMR = INDEX( DELIMS, STRING(NBR:NBR) ) .NE. 0
         END IF
 
      END DO
 
      BEG = NBL
      END = NBR-1
 
      IF ( END .GT. BEG ) THEN
 
C
C        Backup until we are at a non-space.
C
         DO WHILE ( ( STRING(END:END) .EQ. ' ' ) .AND. (END .GT. BEG) )
            END = END - 1
         END DO
 
      END IF
 
      RETURN
 
      END
