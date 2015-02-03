C$Procedure      ERRPRT ( Get/Set Error Output Items )
 
      SUBROUTINE ERRPRT ( OP, LIST )
 
C$ Abstract
C
C      Retrieve or set the list of error message items
C      to be output when an error is detected.
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
C      ERROR
C
C$ Keywords
C
C      ERROR
C
C$ Declarations
 
       CHARACTER*(*)          OP
       CHARACTER*(*)          LIST
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      OP         I   The operation:  'GET' or 'SET'.
C      LIST      I/O  Specification of error messages to be output.
C
C$ Detailed_Input
C
C      OP      indicates the operation to be performed.  Possible
C              values are 'GET' and 'SET'.
C
C              'SET' means, "the following list specifies the default
C              selection of error messages to be output."  These are
C              the messages that will be output to the default error
C              output device (selected by ERRDEV) when an error is
C              detected.
C
C              'GET' means, "return the current list of error output
C              items."  This is the exact list that was set by the
C              last call to this routine with the 'SET' option.
C
C              The option can be specified in mixed case.  For example,
C              the following call will work:
C
C              CALL ERRPRT ( 'SeT' , 'ALL' )
C
C
C      LIST    is a list of error message items.  The items
C              are delimited by commas.  The items that can be
C              in the list are the words:
C
C              1.  SHORT        ...indicates the short error message
C              2.  EXPLAIN      ...the explanation of the short message
C              3.  LONG         ...the long error message
C              4.  TRACEBACK    ...the traceback
C              5.  ALL          ...indicates "output all messages"
C              6.  NONE         ...indicates "don't output any messages"
C              7.  DEFAULT      ...same as ALL, but includes default
C                                  message
C
C              A "list" is a character string containing some or
C              all of the above words, delimited by commas.  Examples
C              are:
C
C              1.  'SHORT, EXPLAIN'
C              2.  'SHORT, LONG'
C              3.  'ALL'
C              4.  'NONE'
C              5.  'ALL, NONE, ALL, SHORT, NONE'
C
C              Each word in the list can be thought of as
C              "flipping a switch" to enable or disable the output
C              of the message(s) indicated by the word.  The
C              words are acted on in the order they occur in the
C              list, starting with the leftmost word.  As examples,
C              consider the sample lists above.
C
C              The effect of the first list above, 'SHORT, EXPLAIN',
C              is to enable the output of the short error message
C              and the explanatory text corresponding to it.
C
C              The effect of the second list is to enable the output
C              of the short and long messages.
C
C              The effect of the third list is to enable the output of
C              all of the error messages (short, long, explanation
C              of the short message, and traceback).
C
C              The effect of the fourth list is to disable output of
C              all of the messages.
C
C              The effect of the fifth list is to disable output of
C              all of the messages.  The reason for this is that
C              the words in the list are responded to in order,
C              from left to right, and "NONE" is the last word.
C
C              If any words other than SHORT, LONG, EXPLAIN, ALL,
C              DEFAULT, TRACEBACK or NONE appear in LIST, those words
C              that are recognized are responded to.  The words
C              that are not recognized are diagnosed as
C              erroneous, and error messages are generated
C              for each such unrecognized word.
C
C              The length of LIST is caller-defined, but only
C              the first 100 characters of LIST will be saved
C              for later retrieval.
C
C              Only the first 10 items in the list are used;
C              the rest are ignored.
C
C$ Detailed_Output
C
C      LIST    is a list of error message items.  The value of
C              LIST is that set by the last call to this routine
C              using the 'SET' option.  See "Detailed Input"
C              for a description of the possible values and
C              meanings of LIST.
C
C              The initial value returned is 'DEFAULT'.
C
C              Only the first 100 characters of LIST are saved
C              when the list is set; any additional characters
C              are truncated.  Therefore, the first 100
C              characters, at most, of the saved value of LIST
C              will be returned.
C
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C      This routine detects invalid values of the argument, OP.
C      Invalid words in LIST are also detected.  The short
C      error messages corresponding to these errors are:
C
C      1.  'SPICE(INVALIDOPERATION)'    ... bad value of OP
C      2.  'SPICE(INVALIDLISTITEM)'     ... bad value in LIST
C
C
C      Also, this routine is part of the SPICELIB error
C      handling mechanism.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C      Please read the "required reading"!
C
C      This routine is intended to be used in conjunction with
C      ERRDEV, which selects the default output device to which
C      the error messages selected by this routine will be
C      output.
C
C      Additionally, the error response action must be
C      something other than 'IGNORE' if the error messages
C      are to be output.  Possible choices of the error
C      response action are 'RETURN', 'REPORT', 'ABORT', 'DEFAULT', and
C      'IGNORE'.  Use ERRACT to set the error response action.
C
C
C      Only the first 100 characters of LIST are saved.
C
C      The default set of error messages that are output is the
C      set specified by 'DEFAULT'; i.e., all of them, including
C      the 'default' message.
C
C
C$ Examples
C
C      1.  In this example, we select as the output device
C          the file, SPUD.DAT, and then select the error
C          messages to be output.  We choose the short
C          error message and the traceback.  Since a
C          different set of messages may have been selected
C          previously, we clear the old setting by putting
C          the word, 'NONE', at the beginning of the list.
C
C      C
C      C      Set the error output device to SPUD.DAT:
C      C
C
C             CALL ERRDEV (  'SET',  'SPUD.DAT'  )
C
C      C
C      C      Choose error messages:
C      C
C
C             CALL ERRPRT (  'SET',  'NONE, SHORT, TRACEBACK'  )
C
C
C
C$ Restrictions
C
C      The device to which the selected error messages will
C      be written must be selected via ERRDEV; otherwise,
C      messages will be written to the initial default device.
C
C      Only the first 100 characters of LIST are saved.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      N.J. Bachman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 28-AUG-1999 (NJB)
C
C        Output string is now built on the fly.  The routine previously
C        returned a saved string which could fail to represent correctly
C        the set of selected message types.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     get/set error output items
C
C-&
 
 
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 28-AUG-1999 (NJB)
C
C        Output string is now built on the fly.  The routine previously
C        returned a saved string which could fail to represent correctly
C        the set of selected message types.
C
C-    Beta Version 1.2.0, 16-FEB-1988 (NJB)
C
C        Declaration of the unused variable TMPLST removed.
C        Trace participation added.  This routine now checks in
C        and checks out.  However, it does not test RETURN,
C        because it should be able to execute in RETURN mode when
C        an error condition exists.
C
C-    Beta Version 1.1.0, 06-OCT-1988 (NJB)
C
C        Superfluous references to LASTNB removed.  These references
C        were so many tonsils; they really had no function.
C
C-&
 
 
 
C
C     SPICELIB functions
C
 
      LOGICAL               SETPRT
      LOGICAL               MSGSEL
 
C
C     Local Variables:
C
 
      INTEGER               WRDLEN
      PARAMETER           ( WRDLEN = 9 )
 
      INTEGER               MAXWRD
      PARAMETER           ( MAXWRD = 10 )
 
      CHARACTER*(WRDLEN)    WORDS ( MAXWRD )
      CHARACTER*(WRDLEN)    UPWORD
 
      INTEGER               NUMWRD
 
 
      INTEGER               I
 
      LOGICAL               SHORT
      LOGICAL               LONG
      LOGICAL               EXPL
      LOGICAL               TRACE
      LOGICAL               DFAULT
      LOGICAL               STATUS
 
      INTEGER               OPLEN
      PARAMETER           ( OPLEN = 3 )
      CHARACTER*(OPLEN)     UPOP
      CHARACTER*(OPLEN)     LOCOP
 
 
 
      INTEGER               LSTLEN
      PARAMETER           ( LSTLEN = 100 )

 
C
C     Executable Code:
C
 
      CALL CHKIN ( 'ERRPRT' )
 
C
C     We first initialize the message selection flags to
C     correspond to the current selection of error messages:
C
      SHORT  = MSGSEL ( 'SHORT'     )
      LONG   = MSGSEL ( 'LONG'      )
      EXPL   = MSGSEL ( 'EXPLAIN'   )
      TRACE  = MSGSEL ( 'TRACEBACK' )
      DFAULT = MSGSEL ( 'DEFAULT'   )
 
C
C     We save the operation string as input, and get
C     an upper case version for our own use:
C
      CALL LJUST ( OP,   UPOP )
      CALL UCASE ( UPOP, UPOP )
 
      IF (  UPOP  .EQ. 'GET' ) THEN
C
C        Construct a string indicating which messages are enabled.
C 
         LIST = ' '
         
         IF ( SHORT ) THEN
         
            LIST = 'SHORT'
            
         END IF
         

         IF ( LONG ) THEN
            
            IF ( LIST .EQ. ' ' ) THEN
               LIST = 'LONG'
            ELSE
               CALL SUFFIX ( ', LONG', 0, LIST )
            END IF
         
         END IF

 
         IF ( EXPL ) THEN
            
            IF ( LIST .EQ. ' ' ) THEN
               LIST = 'EXPLAIN'
            ELSE
               CALL SUFFIX ( ', EXPLAIN', 0, LIST )
            END IF
         
         END IF

 
         IF ( TRACE ) THEN
            
            IF ( LIST .EQ. ' ' ) THEN
               LIST = 'TRACEBACK'
            ELSE
               CALL SUFFIX ( ', TRACEBACK', 0, LIST )
            END IF
         
         END IF

 
         IF ( DFAULT ) THEN
            
            IF ( LIST .EQ. ' ' ) THEN
               LIST = 'DEFAULT'
            ELSE
               CALL SUFFIX ( ', DEFAULT', 0, LIST )
            END IF
         
         END IF

 
      ELSE IF ( UPOP .EQ. 'SET' ) THEN
C
C        We parse the list of words, converting each word
C        to upper case, testing each word for validity,
C        and "flipping the switches" to enable or disable
C        the output of the various error messages as
C        directed by each word, starting with the leftmost.
C        We update local flags according to the words we
C        recognize, and update the global flags when we're
C        done parsing the list.
C
C        If an invalid word is encountered, we signal an
C        error, and continue parsing the list.
C
C
 
 
         CALL LPARSE ( LIST,  ','  ,  MAXWRD,  NUMWRD, WORDS )
 
         DO I = 1, NUMWRD
 
            CALL UCASE  ( WORDS(I), UPWORD )
 
            IF ( UPWORD .EQ. 'SHORT' ) THEN
 
               SHORT  =  .TRUE.
 
            ELSE IF ( UPWORD .EQ. 'LONG' ) THEN
 
               LONG   =  .TRUE.
 
            ELSE IF ( UPWORD .EQ. 'EXPLAIN' ) THEN
 
               EXPL   =  .TRUE.
 
            ELSE IF ( UPWORD .EQ. 'TRACEBACK' ) THEN
 
               TRACE  =  .TRUE.
 
            ELSE IF ( UPWORD .EQ. 'ALL' ) THEN
 
               SHORT  =  .TRUE.
               LONG   =  .TRUE.
               EXPL   =  .TRUE.
               TRACE  =  .TRUE.
 
            ELSE IF ( UPWORD .EQ. 'DEFAULT' ) THEN
 
               SHORT  =  .TRUE.
               LONG   =  .TRUE.
               EXPL   =  .TRUE.
               TRACE  =  .TRUE.
               DFAULT =  .TRUE.
 
            ELSE IF ( UPWORD .EQ. 'NONE' ) THEN
 
               SHORT  =  .FALSE.
               LONG   =  .FALSE.
               EXPL   =  .FALSE.
               TRACE  =  .FALSE.
               DFAULT =  .FALSE.
 
 
            ELSE IF  (  UPWORD .NE. ' '  )  THEN
C
C              Oops! Invalid word...
C
               CALL SETMSG ( 'ERRPRT: An invalid list item was found' //
     .                       ' in the error message list.  The word'  //
     .                       ' was:' //WORDS(I)
     .                     )
 
               CALL SIGERR ( 'SPICE(INVALIDLISTITEM)' )
 
 
            END IF
C
C           At this point, we have either set some set of
C           flags in response to WORD, or determined that
C           WORD was invalid.
C
 
         END DO
C
C        We've now responded to all words in LIST.
C
 
C
C        Now we store the flag values we've set, for global
C        consumption (SETPRT doesn't actually detect errors).
C
 
         STATUS  =  SETPRT ( SHORT, EXPL, LONG, TRACE, DFAULT )
 
      ELSE
 
C
C        An invalid value of OP was supplied.
C
 
         LOCOP = OP
 
         CALL SETMSG ( 'ERRPRT:  An invalid value of OP was supplied.'//
     .                 '  The value was: '  // LOCOP   )
 
         CALL SIGERR ( 'SPICE(INVALIDOPERATION)' )
 
      END IF
 
      CALL CHKOUT ( 'ERRPRT' )
      RETURN
      END
 
