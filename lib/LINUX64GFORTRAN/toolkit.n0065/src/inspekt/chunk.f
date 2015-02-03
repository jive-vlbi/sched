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
 
 
      SUBROUTINE CHUNK ( BUFFER, FIRST, LAST )
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      CHARACTER*(*)         BUFFER  ( LBCELL:* )
      INTEGER               FIRST
      INTEGER               LAST
 
C
C     Find the next `chunk' of a FORTeX source buffer. The chunk begins
C     sometime after BUFFER(FIRST), and ends at BUFFER(LAST).
C
 
C
C$ Revisions
C
C-    Faketex version 1.3.0 5-DEC-1995  WLT
C
C        Set I = TOUCHI( I ) in the IF ( RETURN() ) block so that buggy 
C        compilers won't complain that it isn't used.
C
C-    Faketex version 1.2.0 17-NOV-1995 NJB
C
C        Data statement for TERMS broken up into multiple statements
C        to avoid violation of continuation limit on Sun.
C
C-    Faketex version 1.1.0 16-MAY-1994 NJB
C
C        Substring bounds on line 106 safeguarded to stay in range.
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               CARDC
      INTEGER               CPOS
      INTEGER               ISRCHC
      INTEGER               NCPOS
      INTEGER               LTRIM
      INTEGER               TOUCHI
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               ENDBUF
      INTEGER               TERM
      INTEGER               NTERM
      INTEGER               I
      INTEGER               J
 
      INTEGER               BEGIN
      INTEGER               END
      CHARACTER*32          CSEQ
 
      INTEGER               INDEX
 
      CHARACTER*32          TERMS (24)
      
      DATA                ( TERMS(I), I = 1, 10 )            /
     .                                   '|endliteral    ',
     .                                   '!endliteral    ',
     .                                   '@chapter       ',
     .                                   '@section       ',
     .                                   '@setvarsize    ',
     .                                   '@var           ',
     .                                   '@setparamsize  ',
     .                                   '@param         ',
     .                                   '@literal       ',
     .                                   '@literalitem   '   /
     
      DATA                ( TERMS(I), I = 11, 20 )           /
     .                                   '@literalparam  ',
     .                                   '@literalvar    ',
     .                                   '@exliteral     ',
     .                                   '@exliteralitem ',
     .                                   '@exliteralparam',
     .                                   '@exliteralvar  ',
     .                                   '@newlist       ',
     .                                   '@newpage       ',
     .                                   '@numitem       ',
     .                                   '@paritem       '   /
     
     
      DATA                ( TERMS(I), I = 21, 24 )           /
     .                                   '@symitem       ',
     .                                   '@moreparam     ',
     .                                   '@morevar       ',
     .                                   '               '   /
 
 
C
C     Standard SPICE error handling
C
      IF ( RETURN() ) THEN
         I = 0
         I = TOUCHI( I )
         RETURN
      ELSE
         CALL CHKIN ( 'CHUNK' )
      END IF
C
C     Because we can safely assume that the first line of the chunk
C     is not inside a literal section, we can skip blank lines and
C     @newpage directives with impunity to find the beginning of the
C     chunk.
C
      ENDBUF = CARDC ( BUFFER )
 
      J      = LTRIM ( BUFFER(FIRST) )
 
      DO WHILE (               ( FIRST             .LT. ENDBUF     )
     .           .AND.  (      ( BUFFER(FIRST)     .EQ. ' '        )
     .                    .OR. ( BUFFER(FIRST)(J:) .EQ. '@newpage' )))
         FIRST = FIRST + 1
      END DO
 
      LAST = FIRST
 
C
C     A literal chunk may be terminated only by an explicit end marker
C     (|endliteral or !endliteral) or the end of the buffer. A normal
C     chunk is terminated by the beginning of another chunk, a
C     blank line, or a @newpage.
C
      BEGIN = MAX ( 1,      NCPOS ( BUFFER(FIRST), '  ',     1 )      )
      END   = MAX ( BEGIN,  CPOS  ( BUFFER(FIRST), ' {', BEGIN ) - 1  )
 
      CSEQ  = BUFFER(FIRST)(BEGIN:END)
 
      IF ( CSEQ( :8) .EQ. '@literal' ) THEN
         TERM  =  1
         NTERM =  1
 
      ELSE IF ( CSEQ( :10) .EQ. '@exliteral' ) THEN
         TERM  =  2
         NTERM =  1
 
      ELSE
         TERM  =  3
         NTERM = 22
      END IF
 
C
C     Check subsequent lines until the proper terminator or the end
C     of the buffer is reached.
C
      INDEX = 0
 
      DO WHILE ( INDEX .EQ. 0  .AND.  LAST .LT. ENDBUF )
         LAST = LAST + 1
 
         IF ( BUFFER(LAST) .EQ. ' ' ) THEN
            CSEQ = ' '
         ELSE
            BEGIN = NCPOS ( BUFFER(LAST), '  ', 1 )
 
            END   = MAX ( BEGIN,
     .                    CPOS  ( BUFFER(LAST), ' {', BEGIN ) - 1 )
 
            CSEQ  = BUFFER(LAST)(BEGIN:END)
         END IF
 
         INDEX = ISRCHC ( CSEQ, NTERM, TERMS(TERM) )
      END DO
 
C
C     Only a literal section retains the line that terminates it.
C
      IF ( TERM .GT. 2  .AND.  LAST .NE. ENDBUF ) LAST = LAST - 1
 
      CALL CHKOUT ( 'CHUNK' )
      RETURN
      END
 
