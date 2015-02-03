C$Procedure      OTHER ( Process everything else )
 
      SUBROUTINE OTHER ( SOURCE, N )
 
C$ Abstract
C
C     Process whatever doesn't get processed elsewhere.
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
C     SOURCE      are source lines containing something besides a
C                 recognized chunk of SUBTeX source.
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
C     Everything that doesn't fit elsewhere is treated as a normal
C     paragraph.
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
 
      CHARACTER*132         LINE
      CHARACTER*132         TOKEN
      INTEGER               L
      INTEGER               PGWID
      INTEGER               BEGIN
      INTEGER               REMAIN
 
C
C     Standard SPICE error handling
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'OTHER' )
      END IF
 
 
C
C     Retrieve the required parameters.
C
      CALL PARAMS ( 'GET', 'PAGEWIDTH', WIDTH )
      CALL PARAMS ( 'GET', 'LEFTSKIP',  LSKIP )
      CALL PARAMS ( 'GET', 'RIGHTSKIP', RSKIP )
 
C
C     Keep grabbing tokens until they run out. Start a new line
C     whenever:
C
C        - the current line becomes full.
C        - the current token is @tbr.
C
C     REMAIN is the number of spaces remaining in the current line.
C
      PGWID = WIDTH - LSKIP - RSKIP
      BEGIN = LSKIP + 1
 
      LINE   = ' '
      REMAIN = PGWID
 
      TOKEN = ' '
      CALL TOKENS ( 'NEW', SOURCE, N, TOKEN( :PGWID), L )
 
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
            LINE(BEGIN: ) = TOKEN
            REMAIN        = REMAIN - L - 1
 
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
C     Every paragraph is followed by a blank line.
C
      CALL TEMPB ( 'ADD', ' '  )
 
      CALL CHKOUT ( 'OTHER' )
      RETURN
      END
 
