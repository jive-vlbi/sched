C$Procedure     ERRACT  ( Get/Set Default Error Action )
 
      SUBROUTINE ERRACT ( OP, ACTION )
 
C$ Abstract
C
C     Retrieve or set the default error action.
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
C     ERROR
C
C$ Keywords
C
C     ERROR
C
C$ Declarations
 
      CHARACTER*(*)                 OP
      CHARACTER*(*)                 ACTION
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      OP         I   Operation -- 'GET' or 'SET'
C      ACTION    I/O  Error response action
C
C$ Detailed_Input
C
C      OP       Indicates the operation -- 'GET' or 'SET'.  'GET' means,
C               "Set ACTION to the current value of the error response
C               action."
C               'SET' means, "update the error response action to the
C               value indicated by ACTION."
C
C               OP may be in mixed case; for example,
C
C                      CALL ERRACT ( 'gEt', ACTION )
C
C               will work.
C
C
C      ACTION   When OP is 'SET', ACTION is an input argument.  It
C               takes the values,  'ABORT',  'IGNORE',
C               'REPORT', 'RETURN', and 'DEFAULT'.
C
C               Please read the "required reading" file if you
C               haven't already done so!
C
C               Briefly, the meanings of the error response
C               choices are as follows:
C
C               1.  'ABORT'  --  When an error is detected by a
C                                SPICELIB routine, or when
C                                ANY routine signals detection
C                   of an error via a call to SIGERR, the
C                   toolkit will output any error messages that
C                   it has been enabled to output (see ERRPRT
C                   and ERRDEV also ), and then execute a
C                   FORTRAN STOP statement.
C
C               2.  'REPORT' --  In this mode, the toolkit does
C                                NOT abort when errors are detected.
C                                When SIGERR is called to report
C                   an error, all error messages that the toolkit
C                   is enabled to output will be sent to the
C                   designated error output device.  Similarly,
C                   a call to SETMSG will result in the long
C                   error message being output, if the toolkit
C                   is enabled to output it.
C
C
C               3.  'RETURN' --  In this mode, the toolkit also
C                                does NOT abort when errors are
C                                detected.  Instead, error messages
C                   are output if the toolkit is enabled to do
C                   so, and subsequently, ALL TOOLKIT ROUTINES
C                   RETURN IMMEDIATELY UPON ENTRY until the
C                   error status is reset via a call to RESET.
C                   (No, RESET itself doesn't return on entry).
C                   Resetting the error status will cause the
C                   toolkit routines to resume their normal
C                   execution threads.
C
C
C
C               4.  'IGNORE' --  The toolkit will not take any
C                                action in response to errors;
C                                calls to SIGERR will have no
C                                effect.
C
C
C               5.  'DEFAULT' -- This mode is the same as 'ABORT',
C                                except that an additional error
C                                message is output.  The additional
C                                message informs the user that the
C                                error response action can be
C                                modified, and refers to documentation
C                                of the error handling feature.
C
C
C
C               ACTION may be in mixed case; for example,
C
C                          CALL ERRACT ( 'SET', 'igNORe' )
C
C               will work.
C
C$ Detailed_Output
C
C      ACTION   When OP is 'GET', ACTION is the current error response
C               action.  Possible values are:  'ABORT', 'REPORT',
C               'RETURN', and 'IGNORE'.  See "Detailed Input"
C               for descriptions of the meanings of these values.
C
C               ACTION is not an output unless OP is 'GET'.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C      This routine detects invalid values of OP and ACTION.
C
C      The short error messages set by this routine are:
C
C      1. 'SPICE(INVALIDOPERATION)'    -- bad OP value
C      2. 'SPICE(INVALIDACTION)'       -- bad ACTION value.
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
C      First of all, please read the ``required reading'' file.
C      The information below will make a lot more sense if you do.
C
C      Here is a brief discussion of how to use this routine.
C
C      If you are a user, you will probably be interested
C      in only the 'SET' operation (as far as this routine is
C      concerned, ok?).  As indicated in the "detailed
C      input" section above, the choices for ACTION are
C      'ABORT', 'REPORT', 'RETURN', 'IGNORE', and 'DEFAULT'.  These
C      choices control the way the toolkit behaves when an
C      error is detected.  The toolkit thinks an error has
C      been detected when SIGERR is called.
C
C      1.  'ABORT'   In this mode, the toolkit sends error messages
C          to the error output device and then stops.
C          This is the default mode.  It is probably
C          the one to choose for running non-interactive programs.
C          You may also wish to use this for programs which
C          have many bugs, or in other cases where continued
C          operation following detection of an error isn't useful.
C
C      2.  'REPORT'  In this mode, the toolkit sends error messages
C          to the error output device and keeps going.  This mode
C          may be useful if you are debugging a large program,
C          since you can get more information from a single test run.
C          You will probably want to use ERRDEV to indicate a file
C          where your error messages should be sent.
C
C      3.  'RETURN'  In this mode, the toolkit also sends error messages
C           to the error output device and "keeps going".  But
C           instead of following their normal execution threads,
C           the toolkit routines will simply return immediately upon
C           entry, once an error has been detected.
C           The availability of this feature makes it safe to call
C           multiple toolkit routines without checking the error
C           status after each one returns; if one routine detects
C           an error, subsequent calls to toolkit routines will have
C           no effect; therefore, no crash will occur.  The error
C           messages set by the routine which detected the error
C           will remain available for retrieval by GETMSG.
C
C      4.   'IGNORE'  This mode can be dangerous!  It is best
C           used when running a program whose behavior you
C           understand well, in cases where you wish to suppress
C           annoying messages.  BUT, if an unexpected error
C           occurs, you won't hear about it from anyone, except
C           possibly your run-time system.
C
C      5.  'DEFAULT'  As the name suggests, this is the default
C           error handling mode.  The error handling mechanism
C           starts out in this mode when a program using the
C           toolkit is run, and the mode remains 'DEFAULT' until
C           it is changed via a call to this routine.
C           This mode is the same as 'ABORT',
C           except that an additional error message is output.
C           The additional message informs the user that the
C           error response action can be modified, and refers
C           to documentation of the error handling feature.
C
C
C      NOTE:
C
C          By default, error messages are printed to the screen
C          when errors are detected.  You may want to send them
C          to a different output device, or choose a subset to
C          output.  Use the routines ERRDEV and ERRPRT to choose
C          the output device and select the messages to output,
C          respectively.
C
C          You can also suppress the automatic output of messages
C          and retrieve them directly in your own program.  GETMSG
C          can be used for this.  To make sure that the messages
C          retrieved correspond to the FIRST error that occurred,
C          use 'RETURN' mode.  In 'REPORT' mode, new messages
C          overwrite old ones in the SPICELIB message storage
C          area, so GETMSG will get the messages from the LATEST
C          error that occurred.
C
C
C$ Examples
C
C
C      1.  Setting up 'ABORT' mode:
C
C
C          C
C          C      We wish to have our program abort if an error
C          C      is detected.  But instead of having the error
C          C      messages printed on the screen, we want them
C          C      to be written to the file, ERROR_LOG.DAT
C          C      (This is valid VAX/VMS file name; syntax
C          C      on your system may be different ).
C          C
C          C      We want to see all of the messages, so we
C          C      call ERRPRT, using the 'ALL' option.
C          C
C          C      Finally, we call ERRACT to set the action to 'ABORT':
C          C
C
C                 CALL ERRDEV ( 'SET', 'ERROR_LOG.DAT' )
C
C                 CALL ERRPRT ( 'SET',  'ALL'  )
C
C                 CALL ERRACT ( 'SET', 'ABORT' )
C
C
C
C      2.  Setting up 'REPORT' mode:
C
C          C
C          C      This is the same thing as before, except
C          C      that the argument supplied to ERRACT
C          C      is different.
C          C
C
C                 CALL ERRDEV ( 'SET', 'ERROR_LOG.DAT' )
C
C                 CALL ERRPRT ( 'SET',   'ALL'  )
C
C                 CALL ERRACT ( 'SET', 'REPORT' )
C
C
C      3.  Setting up 'RETURN' mode:  This is the same
C          as example #2, except that the ERRACT call becomes:
C
C                 CALL ERRACT ( 'SET', 'RETURN' )
C
C
C
C      4.  Setting up 'IGNORE' mode:
C
C          C      In this case, we aren't going to have
C          C      ANY error messages (unless the call
C          C      to ERRACT itself fails), so we don't
C          C      really need to call ERRPRT and ERRDEV.
C          C      (If the call to ERRACT DOES fail, which
C          C      it can do only if we misspell "IGNORE,"
C          C      the resulting error messages will go to
C          C      the screen).
C
C
C                 CALL ERRACT ( 'SET', 'IGNORE' )
C
C$ Restrictions
C
C      None.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      N.J. Bachman    (JPL)
C      K.R. Gehringer  (JPL)
C
C$ Version
C
C-     SPICELIB Version 2.0.0, 22-APR-1996 (KRG)
C
C         This subroutine has been modified in an attempt to improve 
C         the general performance of the SPICELIB error handling 
C         mechanism. The specific modification has been to change the 
C         type of the error action passed to PUTACT from a short 
C         character string to an integer. This change is backwardly 
C         incompatible because the type of the input argument has 
C         changed. This should pose no difficulties because PUTACT is a 
C         private subroutine used by the error handling system, and 
C         hence isolated from direct use.
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     get/set default error action
C
C-&

C$ Revisions
C
C-     SPICELIB Version 2.0.0, 22-APR-1996 (KRG)
C
C         This subroutine has been modified in an attempt to improve 
C         the general performance of the SPICELIB error handling 
C         mechanism. The specific modification has been to change the 
C         type of the error action passed to PUTACT from a short 
C         character string to an integer. This change is backwardly 
C         incompatible because the type of the input argument has 
C         changed. This should pose no difficulties because PUTACT is a 
C         private subroutine used by the error handling system, and 
C         hence isolated from direct use.
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     Beta Version 1.1.0, 28-FEB-1989 (NJB)
C
C         Trace participation added.  This routine now checks in
C         and checks out.  However, it does not test RETURN,
C         because it should be able to execute in RETURN mode when
C         an error condition exists.
C
C-&

C
C     SPICELIB Functions
C
      INTEGER               ISRCHC

C
C     Local Parameters
C
C     Define the length of an option.
C
      INTEGER               OPLEN
      PARAMETER           ( OPLEN = 3 )
C
C     Define the maximum length of an action.
C
      INTEGER               ACTLEN
      PARAMETER           ( ACTLEN = 7 )
C
C     Define the number of actions
C
      INTEGER               NUMACT
      PARAMETER           ( NUMACT = 5 )
C
C     Local Variables
C
      CHARACTER*(OPLEN)     LOCOP
      CHARACTER*(ACTLEN)    ACTNS (NUMACT)
      CHARACTER*(ACTLEN)    LOCACT

      INTEGER               IACT
C
C     Saved Variables
C
      SAVE                  ACTNS

C
C     Initial Values:
C
      DATA                  ACTNS / 'ABORT  ',
     .                              'REPORT ',
     .                              'RETURN ',
     .                              'IGNORE ',
     .                              'DEFAULT' /
C
C     Executable Code:
C
 
      CALL CHKIN ( 'ERRACT' )
 
C
C     We convert the input values to upper case, as needed. Note: we 
C     only check the first character of the input variable OP, as that 
C     is sufficient to distinguish 'GET' from 'SET'
C
      CALL LJUST ( OP,    LOCOP )
      CALL UCASE ( LOCOP, LOCOP )
 
      IF ( LOCOP .EQ. 'GET' ) THEN
 
         CALL GETACT ( IACT )

         ACTION = ACTNS(IACT)
 
      ELSE IF ( LOCOP .EQ. 'SET' ) THEN

         CALL LJUST ( ACTION, LOCACT )
         CALL UCASE ( LOCACT, LOCACT )

         IACT = ISRCHC ( LOCACT, NUMACT, ACTNS )
 
         IF ( IACT .GT. 0 ) THEN

            CALL PUTACT ( IACT )
 
         ELSE
C
C           We have an invalid value of ACTION
C
            LOCACT = ACTION
 
            CALL SETMSG ( 'ERRACT: An invalid value of ACTION '  //
     .                    'was supplied.  The value was:  '      //
     .                     LOCACT
     .                  )
 
            CALL SIGERR ( 'SPICE(INVALIDACTION)' )
 
         END IF
 
C
C        We've set the error action, or signalled an error.
C
 
      ELSE
 
C
C        We have an invalid value of OP
C
         LOCOP = OP
 
         CALL SETMSG ( 'ERRACT: An invalid value of OP '  //
     .                 'was supplied.  The value was:  '  //
     .                  LOCOP
     .               )
 
         CALL SIGERR ( 'SPICE(INVALIDOPERATION)' )

      END IF
 
C
C     We've performed the requested operation, or signalled an
C     error.
C
      CALL CHKOUT ( 'ERRACT' )
      RETURN

      END
