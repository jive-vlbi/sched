C$Procedure      FNDPTK ( Find the previous token in a string )
 
      SUBROUTINE FNDPTK ( STRING, DELIMS, START, BEG, END )
      IMPLICIT NONE
 
C$ Abstract
C
C      Find the previous token in a string delimited by multiple
C      delimiters.
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
C                  previous token in the string.  To search for tokens
C                  in a string begin with START = LEN(STRING) + 2 and
C                  for subsequent calls set START to BEG, where BEG
C                  was returned by the previous call.
C
C$ Detailed_Output
C
C      BEG         is the beginning of the substring containing the
C                  token.
C
C      END         is the end of the substring containing the token.
C
C$ Parameters
C
C     None.
C
C
C$ Exceptions
C
C      1.  If START is more than two greater than the length of the
C          string it will be treated as though its length is two more
C          than the length of the string.  Then if there is a null
C          string at the end of the string BEG will point to
C          LEN(STRING) + 1, otherwise it will point to the beginning
C          of the last token in the string.
C
C      2.  If START is LEN(STRING) + 1, BEG will point to the beginning
C          of the last token that preceeds the end of the string.
C
C      2.  If START is at less than or equal to 1, BEG and END will be
C          returned as zero.
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
C          the substring ',    ,' .  Is there a delimiter between
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
C      end of the last token that ends strictly before the input
C      START position in the string.  If no tokens preceeded the input
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
C             START = LEN ( STRING ) + 2
C
C             DO WHILE ( BEG .NE. 0 )
C
C                CALL FNDPTK ( STRING, DELIMS, START, BEG, END )
C
C                   do something with the token STRING(BEG:END) taking
C                   appropriate care of the null tokens.
C
C                START = BEG
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
 
C$ Index_Entries
C
C     Find a token preceding a location in a string
C
C-&

 
C
C     SPICE funtions.
C
      LOGICAL               RETURN
      INTEGER               CPOSR
      INTEGER               NCPOS
      INTEGER               NCPOSR
C
C     Local variables
C
 
      INTEGER               LAST
      INTEGER               EOL
      INTEGER               B
      LOGICAL               ATDELM
      LOGICAL               ONSPCE
 
C%&END_DECLARATIONS
 
C
C     Standard SPICE error handling
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'FNDPTK' )
      END IF
 
C
C     First we gather some data regarding the input string and
C     delimiters
C
      LAST  = LEN  ( STRING        )
      EOL   = LAST + 1
      B     = MIN( EOL+1, START )
 
C
C     We don't have to do anything if we are starting past the end of
C     the string.
C
      IF ( B .LT. 1 ) THEN
         BEG = 0
         END = 0
         CALL  CHKOUT ( 'FNDPTK' )
         RETURN
      END IF
 
      IF ( B .LT. EOL ) THEN
         ONSPCE = STRING(B:B) .EQ. ' '
      ELSE
         ONSPCE = .FALSE.
      END IF
 
C
C     Are we currently pointing at a delimiter?
C
      IF      ( B                            .GT. EOL ) THEN
         ATDELM = .FALSE.
      ELSE IF ( B                            .EQ. EOL ) THEN
         ATDELM = .TRUE.
      ELSE IF ( INDEX( DELIMS, STRING(B:B) ) .NE. 0   ) THEN
         ATDELM = .TRUE.
      ELSE
         ATDELM = .FALSE.
      END IF
 
 
      IF ( ATDELM ) THEN
C
C        Yes.  Move left to a non-blank character
C
         B = NCPOSR( STRING, ' ', B-1 )
 
C
C        If we didn't find a non-blank, then there is not a previous
C        token.
C
         IF ( B .EQ. 0 ) THEN
 
            BEG = 0
            END = 0
            CALL  CHKOUT ( 'FNDPTK' )
            RETURN
 
         END IF
C
C        Still here? Are we currently pointing at a delimiter?
C
         IF ( INDEX( DELIMS, STRING(B:B) ) .NE. 0   ) THEN
 
C
C           Yes. Move left to a non-blank.
C
            B = NCPOSR( STRING, ' ', B-1 )
         END IF
 
C
C        Move left to a delimiter, then Move right 1
C
         B = CPOSR ( STRING, DELIMS, B ) + 1
 
C
C     Are we on a space?
C
      ELSE IF  ( ONSPCE ) THEN
 
C
C        Yes.  (note: space is not a delimiter ) Find the next
C        non-blank to the right.
C
         B = NCPOS ( STRING, ' ', B )
 
C
C        Is this a delimiter?
C
         IF ( B .EQ. 0 ) THEN
 
C
C           it was all blanks to the end of the string.  Make the
C           B point to the end + 1, that is a delimiter
C
            B = EOL
            B = CPOSR ( STRING, DELIMS, B )
 
         ELSE IF ( INDEX(DELIMS, STRING(B:B)) .EQ. 0 ) THEN
 
C
C           No.   Move left to the first delimiter.
C
            B = CPOSR( STRING, DELIMS, B )
 
C
C           If we ran off the front of the string without hitting a
C           delimiter, there isn't a previous token.  Checkout and
C           head for home.
C
            IF ( B .EQ. 0 ) THEN
               BEG = 0
               END = 0
               CALL  CHKOUT ( 'FNDPTK' )
               RETURN
            END IF
 
         END IF
 
C
C        Move left to the first delimiter.
C        Move right 1
C
         B = CPOSR( STRING, DELIMS, B-1 ) + 1
 
      ELSE
C
C     Otherwise
C
C        Move left to the first delimiter.
C
 
         IF ( B .GT. EOL ) THEN
            B = EOL
         ELSE
            B = CPOSR ( STRING, DELIMS, B )
 
C
C           B is now pointing at a delimiter.
C
         END IF
 
C----------
         IF ( B .EQ. 0 ) THEN
            BEG = 0
            END = 0
            CALL  CHKOUT ( 'FNDPTK' )
            RETURN
         END IF
C
C        Move left to the first non-blank  (here or to the left)
C
         IF ( B .LT. EOL ) THEN
 
            B = NCPOSR ( STRING, ' ', B )
 
C
C           B is now pointing to the first non-blank character to the
C           left of the token we started in.
C
            IF (       ( INDEX ( DELIMS, STRING(B:B) ) .NE. 0   )
     .           .AND. ( INDEX ( DELIMS, ' '         ) .NE. 0   )
     .           .AND. ( STRING(B-1:B-1)               .EQ. ' ' ) )THEN
 
C
C              Move backwards to the true delimiter for the token
C              that ends here.
C
               B = NCPOSR ( STRING, ' ', B - 1 ) + 1
 
            END IF
 
         ELSE
 
C
C           If we were at or beyond the EOL position, we need to
C           know if backing up to a non-blank puts us on a delimiter
C           or not.  If it does reset B to EOL.
C
            B = NCPOSR ( STRING, ' ', B )
 
            IF ( INDEX ( DELIMS, STRING(B:B) ) .NE. 0 ) THEN
               B = EOL
            END IF
 
         END IF
 
 
C
C        Move left to the first deliter, and then move right 1.
C
         B = CPOSR ( STRING, DELIMS, B-1 ) + 1
 
      END IF
 
      CALL FNDNTK ( STRING, DELIMS, B, BEG, END )
 
 
      CALL  CHKOUT ( 'FNDPTK' )
      RETURN
 
      END
