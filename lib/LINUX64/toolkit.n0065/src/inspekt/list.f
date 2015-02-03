C$Procedure      LIST ( Process a SUBTeX list item )
 
      SUBROUTINE LIST ( SOURCE, N )
 
C$ Abstract
C
C     Process a @newlist, @numitem, @symitem, or @paritem control
C     sequence.
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
C     SOURCE      are the source lines containing a @newlist, @numitem,
C                 @symitem, or @paritem control sequence, followed by
C                 an associated paragraph of text.
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
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               WIDTH
      INTEGER               LSKIP
      INTEGER               RSKIP
      INTEGER               INDENT
      INTEGER               ISKIP
      INTEGER               INDEX
 
      CHARACTER*132         LINE
      CHARACTER*132         TOKEN
      CHARACTER*12          CSEQ
      INTEGER               L
      INTEGER               PGWID
      INTEGER               BEGIN
      INTEGER               REMAIN
      CHARACTER*5           MARKER
 
 
C
C     Standard SPICE error handling
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'LIST' )
      END IF
 
C
C     Retrieve the required parameters.
C
      CALL PARAMS ( 'GET', 'PAGEWIDTH',  WIDTH  )
      CALL PARAMS ( 'GET', 'LEFTSKIP',   LSKIP  )
      CALL PARAMS ( 'GET', 'RIGHTSKIP',  RSKIP  )
      CALL PARAMS ( 'GET', 'ITEMINDENT', INDENT )
      CALL PARAMS ( 'GET', 'ITEMSKIP',   ISKIP  )
 
C
C     The first token should be a recognized control sequence.
C
      CALL TOKENS ( 'NEW', SOURCE, N, CSEQ, L )
 
C
C     @newlist just resets the list index. That's all.
C
      IF ( CSEQ .EQ. '@newlist' ) THEN
         CALL PARAMS ( 'SET', 'LISTINDEX', 1 )
 
         CALL CHKOUT ( 'LIST' )
         RETURN
      END IF
 
C
C     The principal difference between the various items is the
C     marker that begins the first line.
C
      IF ( CSEQ .EQ. '@numitem' ) THEN
         CALL PARAMS ( 'GET', 'LISTINDEX', INDEX     )
         CALL PARAMS ( 'SET', 'LISTINDEX', INDEX + 1 )
 
         CALL INTSTR ( INDEX,      MARKER )
         CALL SUFFIX ( '.',    0,  MARKER )
 
      ELSE IF ( CSEQ .EQ. '@symitem' ) THEN
         MARKER = '--'
 
      ELSE IF ( CSEQ .EQ. '@paritem' ) THEN
         MARKER = ' '
      END IF
 
C
C     The rest of the text is reformatted into a paragraph of width
C
C        PAGEWIDTH - LEFTSKIP - RIGHTSKIP - ITEMINDENT - ITEMSKIP
C
C     beginning in column
C
C        LEFTSKIP + ITEMINDENT + ITEMSKIP + 1
C
C     The first line contains the marker, right-justified to column
C
C        LEFTSKIP + ITEMINDENT
C
C     Keep grabbing tokens until the run out. Start a new line whenever
C     the current line becomes full. REMAIN is the number of spaces
C     remaining in the current line.
C
      PGWID  = WIDTH - LSKIP  - RSKIP - INDENT - ISKIP
      BEGIN  = LSKIP + INDENT + ISKIP + 1
      REMAIN = PGWID
 
      LINE   = ' '
      CALL RJUST ( MARKER, LINE(1 : LSKIP+INDENT) )
 
      TOKEN  = ' '
      CALL TOKENS ( 'NEXT', SOURCE, N, TOKEN( :PGWID), L )
 
      DO WHILE ( TOKEN .NE. ' ' )
         IF ( L .GT. REMAIN  .OR.  TOKEN .EQ. '@newline' ) THEN
            CALL TEMPB ( 'ADD', LINE )
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
         CALL TEMPB ( 'ADD', LINE )
      END IF
 
C
C     Every list item is followed by a blank line.
C
      CALL TEMPB ( 'ADD', ' '  )
 
      CALL CHKOUT ( 'LIST' )
      RETURN
      END
 
