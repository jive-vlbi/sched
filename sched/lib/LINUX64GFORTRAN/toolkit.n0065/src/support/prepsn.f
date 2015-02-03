C$Procedure      PREPSN (Pretty print syntax definition)
 
      SUBROUTINE PREPSN ( STRING )
 
C$ Abstract
C
C     This routine prepares a string having a META/2 syntax description
C     for printing via NICEIO, NICEPR or NICEBT
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
C       FORMATTING
C
C$ Declarations
 
      IMPLICIT NONE
 
      INTEGER               MAXLEN
      PARAMETER           ( MAXLEN = 2000 )
 
      CHARACTER*(*)         STRING
 
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      STRING    I/O  a string to be prepare for display
C
C$ Detailed_Input
C
C     STRING     is a string that contains a META/2 syntax description.
C
C$ Detailed_Output
C
C     STRING     is the same string after having carriage return
C                markers inserted into it for use by display routines
C                NICEIO, NICEPR or NICEBT
C
C$ Parameters
C
C      MAXLEN    is the maximum length string that can be supported
C                for pretty printing.
C
C$ Files
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     This routine allows you to easily prepare a META/2 syntax
C     specification for display using one of the routines NICEIO
C     NICEPR or NICEBT.  The routine steps through the input
C     routine a word at a time to locate the markers used in
C     META/2 switches.  It assumes the string '/cr' is used for
C     the new line marker within a string.
C
C     Newlines are always inserted at the beginning of a switch (x:y){,
C     after a switch separator '|' and after the end of a switch }.
C     Care is taken so that the construct
C
C       } (x:y){
C
C     becomes
C
C       }/cr (x:y){
C
C     and not
C
C       }/cr(x:y){
C
C     or
C
C       }/cr/cr (x:y){
C
C$ Examples
C
C     This routine is meant for internal use by the routine
C     META_2.  However, if you have a sequence of strings that
C     you would like to prepare for display in documentation
C     you might want to use this routine together with
C     NICEIO or one of its cousins for preparing your documentation.
C
C        DO I = 1, NSYN
C
C           TEMP = SYNTAX
C
C           CALL PREPSN ( TEMP )
C           CALL NICEIO ( TEMP, UNIT, 'LEFT 1 RIGHT 78' )
C           WRITE (UNIT,*) ' '
C
C        END DO
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
C-     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 9, 1994
C
C
C-     META/2 Configured Version 1.0.0, 9-MAY-1994 (WLT)
C
C         This is the configured version of META/2
C         software as of May 9, 1994
C
C
C-&
 
 
C$ Index_Entries
C
C     «We need a permuted index entry»
C
C-&
 
      CHARACTER*(MAXLEN)    LONG
 
      INTEGER               START
      INTEGER               B
      INTEGER               E
      INTEGER               END
      INTEGER               INDNBY
      INTEGER               R
      INTEGER               RTRIM
 
      LOGICAL               BEGIN
      LOGICAL               INDENT
      LOGICAL               CRLAST
 
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 63 )
 
      CHARACTER*(WDSIZE)    WORD
      CHARACTER*(WDSIZE)    OUTDNT
C
C     Set the initial states.
C
C        START  we start looking at the string at the first character
C        E      end of the first word (we have to start somewhere)
C        END    is the end of the local buffer LONG.
C        INDBY  is the amount we've indented things.
C        LONG   is our local string for creating the pretty print string
C        OUTDNT is the string for controlling out-denting
C        BEGIN  we have not begun processing a swithc
C        INDENT we have not indented
C        CRLAST we did not put a '/cr' in the last word we processed.
C
 
      START  =  1
      E      =  1
      END    =  1
      INDNBY =  0
      LONG   = ' '
      OUTDNT = ' '
      BEGIN  = .FALSE.
      INDENT = .FALSE.
      CRLAST = .FALSE.
C
C     Process the string a word at a time  untill we've seen it all.
C
      DO WHILE (E .NE. 0 )
 
         CALL FNDNWD ( STRING, START, B, E )
 
         IF ( E .GT. 0 ) THEN
 
 
            IF ( STRING(E:E) .EQ. '{' ) THEN
C
C              There was a word left in the string.  The beginning
C              of a switch ends with '{'
C
               BEGIN  = .TRUE.
               INDENT = .FALSE.
 
               IF ( CRLAST ) THEN
                  CRLAST = .FALSE.
                  WORD   = ' ' // STRING(B:E)
               ELSE
                  WORD(1:8) = '/cr(:1) '
                  WORD(9:)  = STRING(B:E)
               END IF
C
C              We shall indent (if we do at all) by the number
C              of characters that precede the left bracket '{'
C
               INDNBY = E - B
 
 
            ELSE IF ( STRING(B:E) .EQ. '|' ) THEN
C
C              Switch separators appear all by themselves a words.
C
               IF ( BEGIN ) THEN
C
C                 This is the first separator of this switch, we
C                 are probably going to indent.  And we are no
C                 longer in the beginning simple template of the
C                 switch.
C
                  BEGIN  = .FALSE.
                  INDENT = .TRUE.
 
                  IF ( INDNBY .GT. 0 ) THEN
C
C                    Create the indent and outdent strings.
C
                     WORD   = '/cr(#:)|'
                     OUTDNT = '/cr(-#:)'
                     CALL REPMI ( WORD,   '#', INDNBY, WORD   )
                     CALL REPMI ( OUTDNT, '#', INDNBY, OUTDNT )
                  ELSE
                     WORD   = '/cr|'
                     OUTDNT = '/cr(0:0)'
                  END IF
 
               ELSE
C
C                 We are not at the beginning so there is no
C                 need to indent.
C
                  WORD = '/cr|'
 
               END IF
 
            ELSE IF ( STRING(B:B) .EQ. '}' ) THEN
C
C              We are at the end of a switch (there might be some
C              other stuff such as user punctuation in the string
C              so we don't require STRING(B:E) .EQ. '}'
C
               BEGIN = .FALSE.
 
               IF ( INDENT ) THEN
                  INDENT = .FALSE.
                  WORD   = STRING(B:E) // OUTDNT
               ELSE
                  WORD   = STRING(B:E) // '/cr(0:0)'
               END IF
C
C              We just put in a carriage return at the end of a switch.
C              Set our logical flag that says we did this.
C
               CRLAST = .TRUE.
 
            ELSE
C
C              This word is to be treated as an ordinatry word.
C
               WORD   = STRING(B:E)
               CRLAST = .FALSE.
 
            END IF
 
            R               = RTRIM(WORD)
            LONG(END:END+R) = WORD
            END             = END + R + 1
 
         END IF
 
         START = E + 1
      END DO
C
C     That's all folks.  Move our long string into STRING and
C     return.
C
      STRING = LONG(1:END)
 
      RETURN
      END
