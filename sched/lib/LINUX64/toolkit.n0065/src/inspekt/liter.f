C$Procedure      LITER ( Process a SUBTeX literal block )
 
      SUBROUTINE LITER ( SOURCE, N )
 
C$ Abstract
C
C     Process a block of literal text delimited by @literal* and
C     |endliteral or @exliteral* and !endliteral.
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
C     SUBTeX
C
C$ Keywords
C
C     SUBTeX
C
C$ Declarations
 
      CHARACTER*(*)         SOURCE   ( * )
      INTEGER               N
 
C$ Detailed_Input
C
C     SOURCE      are the source lines containing a block or literal
C                 text delimited byby @literal and |endliteral or
C                 @exliteral and !endliteral.
C
C     N           is the number of source lines.
C
C$ Detailed_Output
C
C     Processed lines are saved in the temporary buffer.
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     SOURCE     I   Source lines.
C     N          I   Number of source lines.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Particulars
C
C     Literal sections are left unformatted. They are indented to
C     account for LEFTSKIP and LITERALINDENT. Lines are truncated
C     on the right if too long.
C
C$ Examples
C
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C$Include SUBTeX.REFS
C
C$ Author_and_Institution
C
C     I.M. Underwood   (JPL)
C     H.A. Neilan      (JPL)
C
C$ Version
C
C-    Beta Version 3.0.0, 18-AUG-1995 (WLT)
C
C         Removed the code Hester added to write warning messages
C         Instead the characters >! appear in the rightmost two
C         columns if text won't fit.
C
C-    Beta Version 2.0.0, 24-MAY-1994 (HAN)
C
C         Added code to write warning messages if the literal
C         section lines are longer than 67 characters. They are
C         truncated in that case, and util now there was no
C         indication that truncation occurred.
C
C-    Beta Version 1.0.0, 11-JUN-1988 (IMU)
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      INTEGER               LTRIM
      INTEGER               RTRIM
 
C
C     Local variables
C
      INTEGER               WIDTH
      INTEGER               LSKIP
      INTEGER               INDENT
      INTEGER               MORE
      INTEGER               ALLOWD
      INTEGER               NEED
 
 
      CHARACTER*(32)        CSEQ
      CHARACTER*(32)        LTYPE
      CHARACTER*(32)        IGNORE
      CHARACTER*132         LINE
      INTEGER               BEGIN
      INTEGER               I
      INTEGER               J
 
C
C     Standard SPICE error handling
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'LITER' )
      END IF
 
C
C     What kind of literal section is this? (Normal, item, parameter,
C     or variable.)
C
      CALL NTHWD ( SOURCE(1), 1, CSEQ, I )
 
      IF (             CSEQ .EQ. '@literal'
     .           .OR.  CSEQ .EQ. '@exliteral' ) THEN
 
         LTYPE = 'NORMAL'
 
      ELSE IF (        CSEQ .EQ. '@literalitem'
     .           .OR.  CSEQ .EQ. '@exliteralitem' ) THEN
 
         LTYPE = 'ITEM'
 
      ELSE IF (        CSEQ .EQ. '@literalparam'
     .           .OR.  CSEQ .EQ. '@exliteralparam' ) THEN
 
         LTYPE = 'PARAMETER'
 
      ELSE IF (        CSEQ .EQ. '@literalvar'
     .           .OR.  CSEQ .EQ. '@exliteralvar' ) THEN
 
         LTYPE = 'VARIABLE'
 
      END IF
 
C
C     Determine what kind of |newpage or !newpage to ignore.
C
      IF ( CSEQ(1:3) .EQ. '@li' ) THEN
         IGNORE = '|newpage'
      ELSE
         IGNORE = '!newpage'
      END IF
 
C
C     Retrieve the required parameters, and compute the total
C     indentation required (as explained below).
C
      CALL PARAMS ( 'GET', 'PAGEWIDTH',     WIDTH  )
      CALL PARAMS ( 'GET', 'LEFTSKIP',      LSKIP  )
      CALL PARAMS ( 'GET', 'LITERALINDENT', INDENT )
 
      BEGIN = LSKIP + INDENT + 1
 
      IF ( LTYPE .EQ. 'ITEM' ) THEN
         CALL PARAMS ( 'GET', 'ITEMINDENT', MORE )
         BEGIN = BEGIN + MORE
 
         CALL PARAMS ( 'GET', 'ITEMSKIP', MORE )
         BEGIN = BEGIN + MORE
 
      ELSE IF ( LTYPE .EQ. 'PARAMETER' ) THEN
         CALL PARAMS ( 'GET', 'ITEMINDENT', MORE )
         BEGIN = BEGIN + MORE
 
         CALL PARAMS ( 'GET', 'PARAMNAMESIZE', MORE )
         BEGIN = BEGIN + MORE
 
         CALL PARAMS ( 'GET', 'PARAMNAMESKIP', MORE )
         BEGIN = BEGIN + MORE
 
      ELSE IF ( LTYPE .EQ. 'VARIABLE' ) THEN
         CALL PARAMS ( 'GET', 'ITEMINDENT', MORE )
         BEGIN = BEGIN + MORE
 
         CALL PARAMS ( 'GET', 'VARNAMESIZE', MORE )
         BEGIN = BEGIN + MORE
 
         CALL PARAMS ( 'GET', 'VARNAMESKIP', MORE )
         BEGIN = BEGIN + MORE
 
         CALL PARAMS ( 'GET', 'VARTYPESIZE', MORE )
         BEGIN = BEGIN + MORE
 
         CALL PARAMS ( 'GET', 'VARTYPESKIP', MORE )
         BEGIN = BEGIN + MORE
      END IF
 
C
C     Ignore the first and last (delimiting) lines. All but the
C     exceptional remaining lines (including blank lines) remain
C     unchanged, and begin in column
C
C        LEFTSKIP + LITERALINDENT + 1
C
C     for normal literal sections. For literal items, add
C
C        ITEMINDENT + ITEMSKIP
C
C     For literal parameters, add
C
C        PARAMINDENT + PARAMNAMESIZE + PARAMNAMESKIP
C
C     For literal variables, add
C
C        VARINDENT + VARNAMESIZE + VARNAMESKIP
C                  + VARTYPESIZE + VARTYPESKIP
C
C
C     The exceptional lines are those that begin with |newline or
C     !newline.  These turn into blank lines.
C
C     If this is a plain old literal area, we'll check to see if
C     truncation is going to occur. If it is, print out a warning
C     message.
C
      LINE   = ' '
 
      DO I = 2, N-1
 
         J = LTRIM( SOURCE(I) )
 
         ALLOWD = WIDTH - BEGIN + 1
         NEED   = RTRIM( SOURCE(I) )
 
 
         IF ( SOURCE(I)(J:) .EQ. IGNORE ) THEN
            LINE = ' '
         ELSE
            LINE(BEGIN:WIDTH) = SOURCE(I)
            IF ( NEED .GT. ALLOWD ) THEN
               LINE(WIDTH-1:WIDTH) = '>!'
            END IF
         END IF
 
         CALL TEMPB ( 'ADD', LINE )
 
      END DO
 
C
C     Every literal block is followed by a blank line.
C
      CALL TEMPB ( 'ADD', ' '  )
 
      CALL CHKOUT ( 'LITER' )
      RETURN
      END
 
 
 
 
