C$Procedure      NEWFIL_1 ( Generate a filename that does not exist )
 
      SUBROUTINE NEWFIL_1 ( PATTRN, FILE )
 
C$ Abstract
C
C     This routine generates a filename that is derived from
C     the input PATTRN and returns the name that was generated
C     in FILE.
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
C       FILES
C
C$ Declarations
 
      CHARACTER*(*)         PATTRN
      CHARACTER*(*)         FILE
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      PATTRN     I   is a name pattern following the rules of MAKSTR
C      FILE       O   the name of the file generated.
C
C$ Detailed_Input
C
C     PATTRN      The description below is lifted without change
C                 from the routine MAKSTR.
C
C                 PATTRN is a string that specifies a pattern that
C                 all strings in a sequence must match. There are
C                 several special substrings in PATTRN that must
C                 be recognized.
C
C                 1) A substring of the form '<*>' (where * is used
C                    as a variable length wildcard character) is called
C                    an expansion. The substring that occurs between
C                    the angle brackets < > is called the invisible
C                    portion of the expansion.  When the tokens of
C                    PATTRN are counted the invisible portion of the
C                    expansion is not counted.  Thus an expansion has
C                    exactly two tokens '<' and '>'  The invisible
C                    portion of the expansion must not contain
C                    any of the characters '<', '>', '{', or '}'.
C
C                 2) A substring of the form '{#-$}' where # and $
C                    stand for any chacter from the set
C                    '0', ... , '9', 'a', ... , 'z' is called a
C                    restriction.
C
C                 A pattern may consist of any collection of
C                 characters.  However, the characters '<' and
C                 '>' must always occur in balanced pairs with '<'
C                 on the left and '>' on the right. Moreover, they
C                 cannot be nested even if they are balanced. Similary
C                 '{' and '}' must always appear as a balanced pair
C                 and have exactly 3 characters between them.  The
C                 first is a lower case letter or a digit.  The second
C                 letter may be anything (usually a hyphen, colon or
C                 comma).  The third character must
C                 also be a letter between 0, ... ,9, a, b, ... , z
C                 and must occur later in the collating sequence than
C                 the first letter in the triple that occurs between
C                 '{' and '}'.
C
C                 For example the following are valid patterns
C
C                 PAT_<Value: >_{0-9}{a-z}{a-d}
C                 COUNTER{0-9}{0-9}{0-9}{0-9}
C                 COUNTER{0:9}{0,9}{a;b}
C
C                 but the following are not
C
C                 PAT_<<>>_{0-9}{a-z}{a-d}    --- Nested < >
C                 COUNTER{9-0}                --- 9 before 0
C                 PAT_{0to0}                  --- 4 characters between{}
C                 PAT_{A-Z}                   --- uppercase letters in{}
C                 PAT_{+-$}                   --- bad characters in {}
C
C                 Pattern should be viewed as consisting of a sequence
C                 of tokens.  The tokens consist of characters that
C                 are not part of an expansion or restriction
C                 restrictions and the '<' and '>' characters of
C                 any expansion.
C
C$ Detailed_Output
C
C     FILE        is a string that is the name of the file that was 
C                 generated.  The name of the file will match the
C                 input PATTRN and will be the first name generated
C                 from PATTRN that does not exist.  See the routine
C                 MAKSTR for a more detailed explanation of the names
C                 that are generated using FSTSTR and NXTSTR.
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
C     None.
C
C$ Particulars
C
C     This is a utility routine for creating a file name that
C     can be opened without fear of name collisions, i.e., it
C     creates tha name of a file that does not exist, thus
C     guaranteeing that you can open the file.
C
C$ Examples
C
C     Suppose that you need a utility file for holding some
C     temporary data structure in a program that makes use
C     of NSPIO for its IO.  Then you could make the following
C     call
C
C        PATTRN = 'util{0-9}{0-9}{0-9}{0-9}.tmp'
C
C        CALL NEWFIL ( PATTRN, FILE )
C
C     If successful, FILE will hold the name of the new file.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C      K.R. Gehringer  (JPL)
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-     Beta Version 1.0.0, 30-MAY-1996 (KRG) (WLT)
C
C-&
 
C$ Index_Entries
C
C     Create a new file name from a pattern
C
C-&
 
C
C     Spicelib routines.
C
      LOGICAL               EXISTS
      LOGICAL               RETURN
C
C     Local Parameters
C
C     Length of a filename.
C
      INTEGER               FNMLEN
      PARAMETER           ( FNMLEN = 255 )
C
C     Local Variables
C
      CHARACTER*(FNMLEN)    FNAME
      CHARACTER*(FNMLEN)    NAME
      CHARACTER*(FNMLEN)    THIS

      LOGICAL               DONE
      LOGICAL               NOMORE

      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN  ( 'NEWFIL_1' )
      END IF
 
      FNAME = ' '
C
C     Get the first filename in the pattern space. 
C
      CALL FSTSTR ( PATTRN, FNAME )

      NAME   = FNAME
      NOMORE = .FALSE.
      DONE   = .FALSE.
C
C     Look for a file name that does not already exist.
C
      DO WHILE ( .NOT. DONE )

         THIS = NAME
         NAME = ' '

         CALL NXTSTR ( PATTRN, THIS, NAME )

         DONE = NAME .EQ. FNAME

         IF ( .NOT. DONE ) THEN
            IF ( .NOT. EXISTS(NAME) ) THEN
               DONE = .TRUE.
            END IF
         ELSE
            NOMORE = .TRUE.
         END IF
         
      END DO
 
      IF ( NOMORE ) THEN
         FILE = ' '
         CALL SETMSG ( 'It was not possible to create a file'
     .   //            ' name using ''#'' as the pattern. All'
     .   //            ' of the file names that can be generated'
     .   //            ' from this pattern already exist.'         )
         CALL ERRCH  ( '#', PATTRN                                 )
         CALL SIGERR ( 'SPICE(CANNOTMAKEFILE)'                     )
         CALL CHKOUT ( 'NEWFIL_1'                                  )
         RETURN
      END IF

      FILE = NAME

      CALL CHKOUT ( 'NEWFIL_1' )
      RETURN
 
      END
 
 
 
 
 
 
