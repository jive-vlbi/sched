C$Procedure      TITLE ( Process a SUBTeX title )
 
      SUBROUTINE TITLE ( SOURCE, N )
 
C$ Abstract
C
C     Process a @chapter, @section, or @subsection control sequence.
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
C     SOURCE      are the source lines containing a @chapter, @section,
C                 or @subsection control sequence, followed by a title.
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
C     Titles are not indented, regardless of the current value of
C     LEFTSKIP. Each title is preceded and followed by a blank line.
C
C     A title may be broken into several lines by occurrences of
C     the @newline control sequence.
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
      CHARACTER*132         LINE
      CHARACTER*132         TOKEN
      CHARACTER*12          CSEQ
      INTEGER               L
      INTEGER               PGWID
      INTEGER               BEGIN
      INTEGER               REMAIN
      INTEGER               UNDER
 
 
 
 
C
C     Standard SPICE error handling
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'TITLE' )
      END IF
 
C
C     Retrieve the required parameters.
C
      CALL PARAMS ( 'GET', 'PAGEWIDTH', WIDTH )
 
C
C     The first token should be @chapter, @section, or @subsection.
C
      CALL TOKENS ( 'NEW', SOURCE, N, CSEQ, L )
 
C
C     One difference between the various titles is the number
C     of blank lines preceding them.
C
      IF ( CSEQ .EQ. '@chapter' ) THEN
         CALL TEMPB ( 'ADD', ' '  )
 
      ELSE IF ( CSEQ .EQ. '@section' ) THEN
         CALL TEMPB ( 'ADD', ' '  )
 
      ELSE IF ( CSEQ .EQ. '@subsection' ) THEN
         CALL TEMPB ( 'ADD', ' ' )
      END IF
 
C
C     Keep grabbing tokens until they run out. Start a new line
C     whenever:
C
C        - the current line becomes full.
C        - the current token is @newline.
C
C     REMAIN is the number of spaces remaining in the current line.
C
      PGWID  = WIDTH
      BEGIN  = 1
 
      LINE   = ' '
      REMAIN = PGWID
 
      TOKEN = ' '
      CALL TOKENS ( 'NEXT', SOURCE, N, TOKEN( :PGWID), L )
 
      DO WHILE ( TOKEN .NE. ' ' )
         IF ( TOKEN .EQ. '@newline' ) THEN
            CALL TEMPB ( 'ADD', LINE )
            LINE   = ' '
            REMAIN = PGWID
 
         ELSE IF ( L .GT. REMAIN ) THEN
            CALL TEMPB ( 'ADD', LINE )
            LINE   = ' '
            REMAIN = PGWID
 
            LINE(BEGIN: ) = TOKEN
            REMAIN        = REMAIN - L - 1
 
         ELSE IF ( LINE .EQ. ' ' ) THEN
            LINE   = TOKEN
            REMAIN = REMAIN - L - 1
 
         ELSE
            CALL SUFFIX ( TOKEN, 1, LINE )
            REMAIN = REMAIN - L - 1
         END IF
 
         CALL TOKENS ( 'NEXT', SOURCE, N, TOKEN( :PGWID), L )
      END DO
 
      IF ( LINE .NE. ' ' ) THEN
         CALL TEMPB ( 'ADD', LINE )
      END IF
 
C
C     Other differences are the way they are (or are not) underlined,
C     and the number of blank lines following them.
C
      IF ( CSEQ .EQ. '@chapter' ) THEN
         UNDER = WIDTH
 
         LINE  = ' '
         CALL SHIFTC ( LINE, 'R', UNDER, '=', LINE )
         CALL TEMPB  ( 'ADD', LINE )
 
C         LINE  = ' '
C         CALL SHIFTC ( LINE, 'R', UNDER, '-', LINE )
C         CALL TEMPB ( 'ADD', LINE )
 
         CALL TEMPB ( 'ADD', ' '  )
 
      ELSE IF ( CSEQ .EQ. '@section' ) THEN
         UNDER = INT ( 0.75 * REAL ( WIDTH ) )
 
         LINE  = ' '
         CALL SHIFTC ( LINE, 'R', UNDER, '-', LINE )
         CALL TEMPB ( 'ADD', LINE )
 
         CALL TEMPB ( 'ADD', ' '  )
 
      ELSE IF ( CSEQ .EQ. '@subsection' ) THEN
         CALL TEMPB ( 'ADD', ' ' )
      END IF
 
      CALL CHKOUT ( 'TITLE' )
      RETURN
      END
 
