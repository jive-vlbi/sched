C$Procedure      PARAM ( Process a SUBTeX parameter item )
 
      SUBROUTINE PARAM ( SOURCE, N )
 
C$ Abstract
C
C     Process a @param or @moreparam control sequence.
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
C     SOURCE      are the source lines containing a @param control
C                 sequence, followed by a parameter name and
C                 explanatory paragraph; or containing @moreparam
C                 and a paragraph.
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
C     I.M. Underwood (JPL)
C
C$ Version
C
C     Beta Version 1.0.0, 11-JUN-1988 (IMU)
C
C-&
 
 
C
C     SPICELIB functions
C
      INTEGER               POS
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               WIDTH
      INTEGER               LSKIP
      INTEGER               RSKIP
      INTEGER               INDENT
      INTEGER               NSIZE
      INTEGER               NSKIP
 
      CHARACTER*132         LINE
      INTEGER               NL
 
      CHARACTER*132         TOKEN
      INTEGER               L
 
      INTEGER               PGWID
      INTEGER               BEGIN
      INTEGER               BEGINN
      INTEGER               REMAIN
      INTEGER               I
 
      INTEGER               MAXVAR
      PARAMETER           ( MAXVAR = 10 )
 
      CHARACTER*132         NAMES    ( MAXVAR )
      INTEGER               NN
 
C
C     Standard SPICE error handling
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PARAM' )
      END IF
 
C
C     Retrieve the required parameters.
C
      CALL PARAMS ( 'GET', 'PAGEWIDTH',     WIDTH  )
      CALL PARAMS ( 'GET', 'LEFTSKIP',      LSKIP  )
      CALL PARAMS ( 'GET', 'RIGHTSKIP',     RSKIP  )
      CALL PARAMS ( 'GET', 'ITEMINDENT',    INDENT )
      CALL PARAMS ( 'GET', 'PARAMNAMESIZE', NSIZE  )
      CALL PARAMS ( 'GET', 'PARAMNAMESKIP', NSKIP  )
 
C
C     The first token should be @param.
C
      CALL TOKENS ( 'NEW', SOURCE, N, TOKEN, L )
 
C
C     The next set of tokens (up to a delimiting period) is the
C     set of variable names. Begin a new one every time a carriage
C     return (@cr) is encountered. Store up to 10.
C
      CALL CLEARC ( MAXVAR, NAMES )
      NN = 1
 
      IF ( TOKEN .EQ. '@param' ) THEN
 
         CALL TOKENS ( 'NEXT', SOURCE, N, TOKEN, L )
 
         DO WHILE ( POS ( TOKEN, '.', 1 ) .EQ. 0 )
            IF ( TOKEN .EQ. '@cr' ) THEN
               NN = NN + 1
 
            ELSE IF ( NN .LE. MAXVAR ) THEN
               CALL SUFFIX ( TOKEN, 1, NAMES(NN) )
            END IF
 
            CALL TOKENS ( 'NEXT', SOURCE, N, TOKEN, L )
         END DO
 
         IF (        TOKEN .NE. '.'
     .        .AND.  TOKEN .NE. '@cr.'
     .        .AND.  NN    .LT. MAXVAR ) THEN
 
            I = POS ( TOKEN, '.', 1 ) - 1
            CALL SUFFIX ( TOKEN(1:I), 1, NAMES(NN) )
         END IF
 
      END IF
 
C
C     The rest of the text is reformatted into a paragraph of width
C
C        PAGEWIDTH - LEFTSKIP
C                  - RIGHTSKIP
C                  - ITEMINDENT
C                  - PARAMNAMESIZE
C                  - PARAMNAMESKIP
C
C     beginning in column
C
C        LEFTSKIP + ITEMINDENT + PARAMNAMESIZE + PARAMTYPESKIP + 1
C
C     Keep grabbing tokens until they run out. Start a new line whenever
C     the current line becomes full. REMAIN is the number of spaces
C     remaining in the current line.
C
C     Names begin in column
C
C        LEFTSKIP + ITEMINDENT + 1
C
C     until they run out.
C
      PGWID  = WIDTH - LSKIP
     .               - RSKIP
     .               - INDENT
     .               - NSIZE
     .               - NSKIP
 
      BEGIN  = LSKIP + INDENT
     .               + NSIZE
     .               + NSKIP  + 1
 
      BEGINN = LSKIP + INDENT + 1
      REMAIN = PGWID
 
      NL     = 1
      LINE   = ' '
      TOKEN  = ' '
      CALL TOKENS ( 'NEXT', SOURCE, N, TOKEN( :PGWID), L )
 
      DO WHILE ( TOKEN .NE. ' ' )
         IF ( L .GT. REMAIN  .OR.  TOKEN .EQ. '@newline' ) THEN
            IF ( NL .LE. NN ) THEN
               CALL LJUST ( NAMES(NL), LINE(BEGINN:BEGINN+NSIZE-1) )
            END IF
 
            CALL TEMPB ( 'ADD', LINE )
            NL     = NL + 1
            LINE   = ' '
            REMAIN = PGWID
 
            LINE(BEGIN: ) = TOKEN
            REMAIN        = REMAIN - L - 1
 
         ELSE IF ( LINE(BEGIN: ) .EQ. ' ' ) THEN
            LINE(BEGIN: ) = TOKEN
            REMAIN        = REMAIN - L - 1
 
         ELSE
            CALL SUFFIX ( TOKEN, 1, LINE(BEGIN: ) )
            REMAIN = REMAIN - L - 1
         END IF
 
         CALL TOKENS ( 'NEXT', SOURCE, N, TOKEN( :PGWID), L )
      END DO
 
      IF ( LINE .NE. ' ' ) THEN
         IF ( NL .LE. NN ) THEN
            CALL LJUST ( NAMES(NL), LINE(BEGINN:BEGINN+NSIZE-1) )
         END IF
 
         CALL TEMPB ( 'ADD', LINE )
         NL = NL + 1
      END IF
 
 
C
C     There may, in fact, be more lines of names than lines
C     of text.
C
      DO I = NL, NN
         LINE = ' '
 
         IF ( I .LE. NN ) THEN
            CALL LJUST ( NAMES(I), LINE(BEGINN:BEGINN+NSIZE-1) )
         END IF
 
         CALL TEMPB ( 'ADD', LINE )
         LINE   = ' '
      END DO
 
C
C     Every parameter item is followed by a blank line.
C
      CALL TEMPB ( 'ADD', ' '  )
 
      CALL CHKOUT ( 'PARAM' )
      RETURN
      END
 
