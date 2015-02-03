C$Procedure      PSTACK (Save paragraphs of text in a paragraph stack)
 
      SUBROUTINE PSTACK ( DEPTH, LINE, BUFFER )
 
C$ Abstract
C
C     Buffer and fetch paragraphs of text.  Buffering is performed
C     a line at a time.  Fetching is done a "paragraph" at a time.
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
C       UTILITY
C
C$ Declarations
 
      IMPLICIT NONE
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               DEPTH
      CHARACTER*(*)         LINE
      CHARACTER*(*)         BUFFER ( LBCELL : * )
 
      INTEGER               NPGRPH
      PARAMETER           ( NPGRPH = 20 )
 
      INTEGER               AVESIZ
      PARAMETER           ( AVESIZ = 20 )
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 132 )
 
 
 
C$ Brief_I/O
C
C      VARIABLE  I/O  ENTRY POINT
C      --------  ---  --------------------------------------------------
C      DEPTH      I   GETBUF, GETBSZ
C      LINE       I   PUTBUF
C      BUFFER    I/O  PUTBUF
C      NPGRPH     P   Number of paragraphs that can be buffered
C      AVESIZ     P   Average number of lines per paragraph
C      LNSIZE     P   Number of characters per line in a paragraph.
C
C$ Detailed_Input
C
C     DEPTH       is the depth in the "paragraph-stack" from which to
C                 fetch a "paragraph" of text.  The top-most
C                 level of the paragraph stack is at depth 1.  The
C                 next level down in the stack is at depth 2, etc.
C
C     LINE        is a line of text that should be added to the
C                 current "paragraph" of buffered text.
C
C     BUFFER      is a properly initialized cell that will be used
C                 to fetch saved lines of text.
C
C$ Detailed_Output
C
C     BUFFER      contains the paragraph of text from paragraph buffer
C                 at the depth specified in the call to GETBUF.
C
C$ Parameters
C
C      NPGRPH     is the maximum number of paragraphs that will be
C                 buffered. This should be at least 1
C
C      AVESIZ     is the average number of lines per paragraph. This
C                 should be at least 10.
C
C      LNSIZE     is the number of characters per line in a paragraph.
C                 This should be at least 80.
C
C$ Exceptions
C
C     Error free.
C
C     1) If the DEPTH specified is deeper than the deepest level of
C        the stack.  The deepest paragraph will be returned.
C
C     2) If the DEPTH specified is zero or less, the BUFFER will be
C        returned with no lines of text.
C
C     3) If no lines were buffered at a particular depth of the
C        paragraph stack, the paragraph buffer will be returned
C        with no lines of text.
C
C
C
C$ Files
C
C      None.
C
C$ Particulars
C
C     Consider the following problem.
C
C     1) You have a program that uses strings of text as commands
C        to controll the action of the program.
C
C     2) Many commands are too long to fit within the space provided
C        by a terminal (or terminal window) line.
C
C     3) Your program captures full commands by reading terminal
C        (or terminal window) lines one at a time with continuation
C        and concatenation to create a full command.
C
C           COMMAND = ' '
C
C           DO WHILE ( MORE(COMMAND) )
C
C              READ  (*,FMT='(A)' ) LINE
C              CALL SUFFIX ( LINE, 1, COMMAND )
C
C              ...
C           END DO
C     (For convenience the original set of input lines forming the
C     command is called a paragraph.)
C
C     4) You would like to preserve the original format of the command
C        as it was typed.
C
C     This routine serves as an umbrella routine for a family of
C     entry points that perform the buffering and fetching of the
C     original input lines to your program.  Moreover, it buffers
C     upto 20 of the input paragraphs so that you can easily recall
C     the history of the command sequence entered in your program.
C
C$ Examples
C
C     Following the scenario above, here is how you would go about
C     buffering a paragraph of input.
C
C        Set up for the buffering of the next paragraph.
C
C        CALL RSTBUF ( )
C
C        Empty out the command we will be constructing.
C
C        COMMAND = ' '
C        MORE    = .TRUE.
C
C        DO WHILE ( MORE )
C
C           READ  (*,FMT='(A)' ) LINE
C           CALL PUTBUF ( LINE )
C           CALL SUFFIX ( LINE, 1, COMMAND )
C
C           Examine line or command as appropriate to determine if
C           we should expect more text for the command we are
C           constructing.
C
C           ...
C
C        END DO
C
C     Once paragraphs have been buffered, you may fetch the last command
C     (depth 1), next to last command (depth 2) and so on to a depth
C     of MAXDPT buffered paragraphs.  To do this you must create
C     a character cell and initialize it so that the input lines
C     can be returned exactly as they were input.
C
C        Declaration of the buffer used for returning input lines.
C
C        INTEGER               LBCELL
C        PARAMETER           ( LBCELL = -5 )
C
C        INTEGER               LNSIZE
C        PARAMETER           ( LNSIZE = Number of characters
C                                       used the declaration of
C                                       LINE used in the last code
C                                       fragment )
C
C
C        INTEGER               MAXLIN
C        PARAMETER           ( MAXLIN = Maximum number of lines that
C                                       will ever be used to create
C                                       a command. )
C
C        CHARACTER*(LNSIZE)    BUFFER ( LBCELL : MAXLIN )
C
C
C        Initialize the cell BUFFER
C
C        CALL SSIZEC ( MAXLIN, BUFFER )
C
C        Fetch the next to last command entered to the program.
C
C        DEPTH = 2
C
C        CALL GETBUF ( DEPTH, BUFFER )
C
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C
C$ Version
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
C
C-    SPICELIB Version 1.0.0, 12-APR-1994 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Buffer paragraphs text a line at a time
C     Fetch buffered lines of text a paragraph at a time
C
C-&
C
C     Local Parameters
C
      INTEGER               PBEG
      PARAMETER           ( PBEG = 1 )
 
      INTEGER               PNXT
      PARAMETER           ( PNXT = 2 )
 
      INTEGER               MAXLIN
      PARAMETER           ( MAXLIN = AVESIZ*NPGRPH )
 
C
C     Spicelib Functions
C
      INTEGER               SIZEC
 
C
C     Local Buffers
C
 
      CHARACTER*(LNSIZE)    LINES  (     MAXLIN )
 
      INTEGER               BEGEND ( 2 , NPGRPH )
      INTEGER               BUFFRD
      INTEGER               CURRNT
 
C
C     In-line function dummy arguments
C
      INTEGER               N
      INTEGER               RANGE
C
C     In-line functions
C
      INTEGER               NEXT
      INTEGER               PREV
C
C     Local Variables
C
 
 
      INTEGER               BACKUP
      INTEGER               BSIZE
      INTEGER               GETAT
      INTEGER               GOTTEN
      INTEGER               I
      INTEGER               LAST
      INTEGER               PUTAT
      INTEGER               QUIT
      INTEGER               START
 
      LOGICAL               FIRST
 
      SAVE
 
 
      DATA                  BUFFRD / 0 /
      DATA                  CURRNT / 1 /
      DATA                  FIRST  / .TRUE. /
 
C
C     In-line functions for computing the next and previous item
C     in a circular list of items.
C
      PREV ( N, RANGE ) = ( N - 1 )
     .                  + ( ( RANGE - N + 1 ) / RANGE ) * RANGE
 
      NEXT ( N, RANGE ) = ( N + 1 )
     .                  - ( N / RANGE ) * RANGE
 
 
 
      RETURN
 
 
C$Procedure      RSTBUF (Reset paragraph buffering)
 
      ENTRY RSTBUF ( )
 
C$ Abstract
C
C     Reset the paragraph buffering so that a new paragraph
C     of text can be buffered.
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
C       UTILITY
C
C$ Declarations
C
C     Later. 
C
C$ Brief_I/O
C
C     None.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C     This entry point works by side effect.  It resets the internal
C     parameters of the paragraph buffering code so that programs
C     may begin buffering a new paragraph of text and distinguish
C     it from previously buffered paragraphs.
C
C     This routine should only be called when you want to start
C     buffering text as a new paragraph.
C
C$ Examples
C
C     See the umbrella routine PSTACK
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C
C$ Version
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
C
C-    SPICELIB Version 1.0.0, 12-APR-1994 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Prepare for buffering paragraphs of text.
C
C-&
 
 
C
C     On the first call to the buffering routines we need to
C     initialize our buffering pointers.
C
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
 
         CURRNT                 = 1
         BUFFRD                 = 1
         BEGEND( PBEG, CURRNT ) = 1
         BEGEND( PNXT, CURRNT ) = 1
 
         DO I = 1, MAXLIN
            LINES(I) = ' '
         END DO
 
      ELSE
 
C
C        Store the current buffer pointer and compute the
C        next one.
C
         BUFFRD = MIN ( BUFFRD+1, NPGRPH )
         LAST   = CURRNT
         RANGE  = NPGRPH
         CURRNT = NEXT(CURRNT,RANGE)
C
C        Now compute the pointers to the beginning and ending of
C        data in the buffer that saves input lines.
C
         RANGE = MAXLIN
         BEGEND ( PBEG, CURRNT ) = NEXT   ( BEGEND(PNXT,LAST), RANGE )
         BEGEND ( PNXT, CURRNT ) = BEGEND ( PBEG,  CURRNT )
 
      END IF
      RETURN
 
 
C$Procedure      PUTBUF ( Put a line of text in the paragraph buffer )
 
 
      ENTRY PUTBUF ( LINE )
 
 
C$ Abstract
C
C     Append the input line of text to the current paragraph
C     that is being buffered.
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
C       UTILITY
C
C$ Declarations
C
C      Later. 
C
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      LINE       I   a line of text to append to the current paragraph
C
C$ Detailed_Input
C
C     LINE        is a line of text that will be appended to the
C                 paragraph that was begun with the last call to
C                 RSTBUF.
C
C                 LINE should be declared to be no more than LNSIZE
C                 characters in length (See PSTACK for the value
C                 of LNSIZE.)
C
C$ Detailed_Output
C
C     None
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C     This routine works in conjuction with RSTBUF so that the input
C     line of text is appended to the paragraph of text that was begun
C     by the last call to RSTBUF.
C
C$ Examples
C
C     See the example in the umbrella routine PSTACK
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C
C$ Version
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
C
C-    SPICELIB Version 1.0.0, 12-APR-1994 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Add a line of text to the current paragraph of input.
C
C-&
C
C     If things haven't already been initialized, we do so now.
C
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
 
         CURRNT                 = 1
         BUFFRD                 = 1
         BEGEND( PBEG, CURRNT ) = 1
         BEGEND( PNXT, CURRNT ) = 1
 
         DO I = 1, MAXLIN
            LINES(I) = ' '
         END DO
 
      END IF
 
C
C     Store the input line.
C
      RANGE         = MAXLIN
      PUTAT         = BEGEND( PNXT, CURRNT )
      LINES(PUTAT)  = LINE
C
C     Find out where to put the next line of input.
C
      BEGEND(PNXT,CURRNT) = NEXT(PUTAT,RANGE )
 
 
      RETURN
 
 
 
C$Procedure      GETBUF (Get a paragraph at specified depth in a buffer)
 
      ENTRY GETBUF ( DEPTH, BUFFER )
 
C$ Abstract
C
C     Fetch the paragraph at the specified depth and return it in the
C     supplied buffer.
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
C       UTILITY
C
C$ Declarations
C
C      Later.
C
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      DEPTH      I   Depth in paragraph stack to fetch data from
C      BUFFER    I/O  An pre-initialized cell to return data in.
C
C$ Detailed_Input
C
C     DEPTH       is the depth of the paragraph to return.  DEPTH
C                 should be a positive integer between 1 (the current
C                 paragraph depth) and NPGRPH (the most deeply buffered
C                 paragraph).  If DEPTH is zero or more, no lines
C                 will be returned.  If DEPTH is larger than the
C                 deepest available buffered paragraph, the most
C                 deeply buffered paragraph will be returned.
C
C     BUFFER      a properly initialized cell into which lines of
C                 text may be stored.
C
C$ Detailed_Output
C
C     BUFFER      is the input buffer but now with the requested
C                 paragraph stored in it.  The first line of the
C                 paragraph appears in BUFFER(1), the second line
C                 in BUFFER(2), etc.  The actual number of lines
C                 in the buffer is equal to the cardinality of BUFFER
C                 on output.
C
C                 If no lines were available to put in BUFFER, the
C                 cardinality of buffer will be zero.
C
C                 It is recommended that BUFFER be declared by the
C                 calling routine with size no more than LNSIZE.
C                 (See the umbrella routine for the value of LNSIZE).
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  If DEPTH is zero or negative, BUFFER will be returned with
C         a cardinality of zero and no valid lines of text.
C
C     2)  If DEPTH specifies a paragraph beyond the depth of those
C         that have been buffered, BUFFER will be returned with a
C         cardinality of zero and no valid lines of text.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C     This entry point enables you to retrieve buffered paragraphs
C     of text.  The paragraph to retrieve is specified by its depth
C     in the paragraph stack buffer.
C
C$ Examples
C
C     See the umbrella routine for an example of usage.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C
C$ Version
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
C
C-    SPICELIB Version 1.0.0, 12-APR-1994 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Retrieve a paragraph from a specified depth in the stack
C
C-&
C
C     First empty the buffer where we will be sending the buffered
C     inputs.
C
      BSIZE = SIZEC (        BUFFER )
      CALL   SSIZEC ( BSIZE, BUFFER )
C
C     DEPTH represents how deep we want to push down into the
C     buffer of items.  1 is the current, 2 is immediately before
C     that and so on...
C
      BACKUP = MIN ( DEPTH-1, BUFFRD-1 )
 
 
      IF ( BACKUP .LT. 0 ) THEN
C
C        This is probably a mistake, but we will not pass any
C        moral judgements on the request to get data, we simply
C        return the buffer empty.
C
         RETURN
 
      END IF
C
C     Backup from the current position the appropriate number to
C     find out where to get the buffered input lines.
C
      GETAT  = CURRNT
      RANGE  = NPGRPH
 
      DO I = 1, BACKUP
         GETAT = PREV(GETAT,RANGE)
      END DO
 
 
      START  = BEGEND ( PBEG, GETAT )
      QUIT   = BEGEND ( PNXT, GETAT )
      GOTTEN = 0
      RANGE  = MAXLIN
 
      DO WHILE ( START .NE. QUIT .AND. GOTTEN .LE. BSIZE )
 
         GOTTEN         = GOTTEN + 1
         BUFFER(GOTTEN) = LINES  ( START )
         START          = NEXT   ( START, RANGE)
 
      END DO
 
      CALL SCARDC ( GOTTEN, BUFFER )
 
      RETURN
 
 
 
C$Procedure      GETBSZ (Get current size of paragraph buffer)
 
      ENTRY GETBSZ ( DEPTH )
 
C$ Abstract
C 
C     Return the number of paragraphs that are buffered.
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
C       Utility
C
C$ Declarations
C
C      Later.
C
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      DEPTH      O   the current maximum depth of the paragraph buffer
C
C$ Detailed_Input
C
C     None
C
C$ Detailed_Output
C
C     DEPTH       is the maximum depth of the paragraph buffer for which
C                 data can be returned at the time the call to GETBSZ
C                 is issued.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C     This entry point allows you to easily retrieve the number of
C     paragraphs that are available in the paragraph stack for
C     retrieval.
C
C
C$ Examples
C
C     Suppose that you wish to retrieve all of the paragraphs that
C     have been buffered.  The code fragment below shows how to use
C     this routine in conjunction with the entry GETBUF to retrieve
C     the paragraphs.
C
C        Initialize the cell we are using to retrieve paragraphs.
C
C        CALL SSIZEC ( BSIZE, BUFFER )
C
C        Find out the current number of paragraphs that are available
C        for retrieval
C
C        CALL GETBSZ ( N )
C
C        Finally fetch the paragraphs starting at the bottom of the
C        stack and working our way to the top of the stack.
C
C        DO WHILE ( N .GT. 0 )
C
C           CALL GETBUF ( N, BUFFER )
C
C           Do something with the retrieved buffer
C
C           N = N - 1
C
C        END DO
C
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C
C$ Version
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
C
C-    SPICELIB Version 1.0.0, 12-APR-1994 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Get the number of buffered paragraphs
C
C-&
 
      DEPTH = BUFFRD
 
      RETURN
 
C$Procedure      DMPBUF ( Dump the last buffered paragraph )
 
      ENTRY DMPBUF ( )
 
 
C$ Abstract
C
C     This entry point removes the top paragraph from the top of the
C     paragraph stack.
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
C       Utility
C
C$ Declarations
C
C     Later.
C
C$ Brief_I/O
C
C     None.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C     This routine works by side effect.  It makes the top paragraph
C     in the paragraph stack unavailable---in effect deleting it
C     from the paragraph stack.
C
C$ Examples
C
C     Suppose that there are some paragraphs that have a special
C     meta-meaning in the operation of a program.  It may be
C     desirable to remove these paragraphs from the paragraph stack.
C
C     For example suppose that the paragraph stack contains lines
C     of text that make up commands to a program.  And suppose that
C     the command RECALL is a meta-command that tells the program
C     to recall one of the commands in the stack.  It is likely that
C     you do not want RECALL to be added to the stack.  So when
C     the RECALL command is encountered in preprocessing of commands,
C     you can call DMPBUF to remove it from the stack of commands.
C
C     Yes, this example is a bit vague.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C
C$ Version
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
C
C-    SPICELIB Version 1.0.0, 13-APR-1994 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Remove a paragraph from the top of the paragraph stack
C
C-&
 
 
      BUFFRD = MAX ( BUFFRD-1, 0 )
      RANGE  = NPGRPH
      CURRNT = PREV(CURRNT,RANGE)
 
      RETURN
 
      END
