C$Procedure      NEWFIL ( Open a new file on the specified port )
 
      SUBROUTINE NEWFIL ( PATTRN, PORT, FILE )
 
C$ Abstract
C
C     This routine opens a port with a file that is created from
C     the input PATTRN and returns the name of the FILE attached
C     to the port.
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
 
      IMPLICIT NONE
      CHARACTER*(*)         PATTRN
      CHARACTER*(*)         PORT
      CHARACTER*(*)         FILE
 
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      PATTRN     I   is a name pattern following the rules of MAKSTR
C      PORT       I   the port to which the FILE should be attached.
C      FILE       O   the name of the file attached to the port.
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
C     PORT        is the name of an NSPIO port that will be opened
C                 with the file name generated from PATTRN.
C
C     FILE        is a string that is the name of the file that is
C                 open and attached to the specified PORT.  The
C                 name of the file will match the input PATTRN
C                 and will be the first name generated from PATTRN
C                 that can be opened.  See the routine MAKSTR for
C                 a more detailed explanation of the names
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
C     1) If the file cannot be opened, the error
C        CMLOOP(CANNOTMAKEFILE) will be signalled.
C
C$ Particulars
C
C     This is a utility routine for creating a file name that
C     can be opened without fear of name collisions and attached
C     to one of the file ports supported by NSPIO.  In this way
C     you have a high likelyhood of success in opening a log file
C     or utility file for use by your program (this assumes that
C     you have adequate privelege to open a file in the directory
C     implied or specified by PATTRN).
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
C        CALL NEWFIL ( PATTRN, 'UTILITY', FILE )
C
C     If successful, FILE will hold the name of the file that
C     was opened and is attached to the UTILITY port of NSPIO.
C     Otherwise FILE will be returned as a blank and the
C     FAILED flag will have been set by the call to SIGERR
C     made in this routine.
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
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
C
C-    SPICELIB Version 1.0.0, 21-APR-1994 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Create a file name and attach it to an I/O port
C
C-&
 
      INCLUDE              'commdpar.inc'
C
C     Spicelib routines.
C
      LOGICAL               EXISTS
      LOGICAL               FAILED
 
C
C     Local Parameters
C
      INTEGER               MAXTRY
      PARAMETER           ( MAXTRY = 20 )
 
      CHARACTER*(FILEN)     FNAME
      CHARACTER*(FILEN)     NAME
      CHARACTER*(FILEN)     THIS
 
      INTEGER               BADOPN
 
      LOGICAL               MORE
 
 
 
      CALL CHKIN  ( 'NEWFIL' )
 
      FNAME = ' '
      NAME  = ' '
      THIS  = ' '
 
      CALL FSTSTR ( PATTRN, FNAME )
      NAME   =  FNAME
      MORE   = .TRUE.
      BADOPN =  0
 
      DO WHILE ( BADOPN .LT. MAXTRY )
C
C        Look for a file name that does not already exist.
C
         DO WHILE ( EXISTS(NAME) .AND. MORE )
            THIS = NAME
            NAME = ' '
            CALL NXTSTR ( PATTRN, THIS, NAME )
            MORE = NAME .NE. FNAME
         END DO
 
         IF ( .NOT. MORE ) THEN
            FILE = ' '
            CALL SETMSG ( 'It was not possible to create a # '
     .      //            'file as specified. All appropriately '
     .      //            'named files already exist.' )
            CALL ERRCH  ( '#', PORT )
            CALL SIGERR ( 'CMLOOP(CANNOTMAKEFILE)' )
            CALL CHKOUT ( 'NEWFIL' )
            RETURN
         ELSE
            FILE = NAME
         END IF
 
C
C        Ok.  We've got a good candidate, try to attach it to the
C        specified port.
C
         CALL NSPOPN ( PORT, FILE )
 
         IF ( FAILED() ) THEN
 
            BADOPN = BADOPN + 1
C
C           We will try a few more times on the off chance that
C           some other program used the same name first.  This
C           is not likely, file protection problems or PATTRN
C           specifications are a more probable cause of the trouble,
C           but we try anyway.
C
 
            IF ( BADOPN .LT. MAXTRY ) THEN
               CALL RESET
            END IF
 
         ELSE
C
C           We were successful in opening the port with the
C           specified name.  We can quit now.
C
            CALL CHKOUT ( 'NEWFIL' )
            RETURN
         END IF
 
      END DO
C
C     If you get to this point, a file was not succesfully
C     attached to PORT.  But NSPIO has already diagnosed
C     the problem as much as we're going to.  Just set FILE
C     to a blank and return.
C
      FILE = ' '
      CALL CHKOUT ( 'NEWFIL' )
 
 
      RETURN
 
      END
