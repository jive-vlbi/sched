 
C$Procedure      PROC ( Process a chunk of SUBTeX source )
 
      SUBROUTINE PROC ( BUFFER, FIRST, LAST )
 
C$ Abstract
C
C     Process one meaningful unit (chunk) of a SUBTeX source buffer.
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
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      CHARACTER*(*)         BUFFER  ( LBCELL:* )
      INTEGER               FIRST
      INTEGER               LAST
 
C$ Detailed_Input
C
C     BUFFER      is a buffer containing lines of SUBTeX source text.
C
C     FIRST,
C     LAST        are the indices of the first and last lines of
C                 the chunk to be processed.
C
C$ Detailed_Output
C
C     The processed line is saved in the temporary buffer.
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     BUFFER     I   Input source.
C     FIRST,
C     LAST       I   Bounds of chunk to be processed.
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
C
C$ Examples
C
C
C
C$ Restrictions
C
C
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
C     Beta Version 1.1.0,  8-JAN-1996 (WLT)
C
C        Changed the call to SIZE to remove the extra argument
C        that is not needed by the routine
C
C        It was 
C 
C           CALL SIZE ( BUFFER(FIRST), N )
C
C        Now it's
C
C           CALL SIZE ( BUFFER(FIRST) )
C
C     Beta Version 1.0.0, 11-JUN-1988 (IMU)
C
C-&
 
 
C
C     Entry points
C
 
C
C     SPICELIB functions
C
      INTEGER               CPOS
      INTEGER               NCPOS
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               BEGIN
      INTEGER               END
      CHARACTER*32          CSEQ
      INTEGER               N
 
 
C
C     Standard SPICE error handling
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PROC' )
      END IF
 
C
C     The type of processing depends on the type of chunk.
C     The type of chunk is indicated by the initial control sequence.
C
      BEGIN = NCPOS ( BUFFER(FIRST), '  ',     1 )
      END   = CPOS  ( BUFFER(FIRST), ' {', BEGIN ) - 1
 
      CSEQ  = BUFFER(FIRST)(BEGIN:END)
      N     = LAST - FIRST + 1
 
 
      IF (      CSEQ .EQ. '@chapter'
     .     .OR. CSEQ .EQ. '@section'
     .     .OR. CSEQ .EQ. '@subsection' ) THEN
 
         CALL TITLE ( BUFFER(FIRST), N )
 
 
      ELSE IF (      CSEQ .EQ. '@setvarsize'
     .          .OR. CSEQ .EQ. '@setparamsize' ) THEN
 
         CALL SIZE ( BUFFER(FIRST) )
 
      ELSE IF (      CSEQ .EQ. '@var'
     .          .OR. CSEQ .EQ. '@morevar' ) THEN
 
         CALL VAR ( BUFFER(FIRST), N )
 
      ELSE IF (      CSEQ .EQ. '@param'
     .          .OR. CSEQ .EQ. '@moreparam' ) THEN
 
         CALL PARAM ( BUFFER(FIRST), N )
 
      ELSE IF (      CSEQ .EQ. '@literal'
     .          .OR. CSEQ .EQ. '@literalitem'
     .          .OR. CSEQ .EQ. '@literalparam'
     .          .OR. CSEQ .EQ. '@literalvar'
     .          .OR. CSEQ .EQ. '@exliteral'
     .          .OR. CSEQ .EQ. '@exliteralitem'
     .          .OR. CSEQ .EQ. '@exliteralparam'
     .          .OR. CSEQ .EQ. '@exliteralvar'   ) THEN
 
         CALL LITER ( BUFFER(FIRST), N )
 
 
      ELSE IF (      CSEQ .EQ. '@newlist'
     .          .OR. CSEQ .EQ. '@numitem'
     .          .OR. CSEQ .EQ. '@paritem'
     .          .OR. CSEQ .EQ. '@symitem' ) THEN
 
         CALL LIST ( BUFFER(FIRST), N )
 
      ELSE
 
         CALL OTHER ( BUFFER(FIRST), N )
 
      END IF
 
      CALL CHKOUT ( 'PROC' )
      RETURN
      END
 
