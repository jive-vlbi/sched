C$Procedure      SIGERR ( Signal Error Condition )
 
      SUBROUTINE SIGERR ( MSG )
 
C$ Abstract
C
C     Inform the SPICELIB error processing mechanism that an error has
C     occurred, and specify the type of error.
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
 
      CHARACTER*(*)                 MSG
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      MSG        I   A short error message.
C
C$ Detailed_Input
C
C      MSG     A ``short'' error message.
C              MSG indicates the type of error that has occurred.
C
C              The exact format that MSG must follow is
C              described in the required reading file, error.req.
C              Only the first 25 characters of MSG will be stored;
C              additional characters will be truncated.
C
C              Generally, MSG will be stored internally by the SPICELIB
C              error handling mechanism.  The only exception
C              is the case in which the user has commanded the error
C              handling mechanism to ``ignore'' the error indicated by
C              MSG.
C
C              As a default, MSG will be output to the screen.
C              See the required reading file for a discussion of how
C              to customize SPICELIB error handling behavior, and
C              in particular, the disposition of MSG.
C
C$ Detailed_Output
C
C      None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C      This routine does not detect any errors.
C
C      However, this routine is part of the interface to the
C      SPICELIB error handling mechanism.  For this reason,
C      this routine does not participate in the trace scheme,
C      even though it has external references.
C
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
C      This is the routine used by SPICELIB to signal the detection
C      of errors.
C
C      Making a call to SIGERR is the way to inform the error
C      handling mechanism that an error has occurred.
C
C      Specifically, the effects of this routine are:
C
C      1.  If responding to the error indicated by MSG has
C          not been disabled:
C
C          a. MSG will be stored internally.  As a result,
C             The SPICELIB routine, GETMSG, will be able to
C             retrieve MSG, until MSG has been ``erased''
C             by a call to RESET, or overwritten by another
C             call to SIGERR.
C
C          b. An indication of an ``error condition'' will
C             be set internally.  The SPICELIB logical
C             function, FAILED, will take the value, .TRUE.,
C             as a result, until the error condition is
C             negated by a call to RESET.
C
C          c. All of the error messages that have been selected
C             for automatic output via ERRPRT will be output.
C             The set of messages is some subset of { short message,
C             long message, explanation of short message,
C             traceback, and default message }.
C
C          d. If the error response mode is not 'RETURN',
C             Setting of the long error message is enabled.
C             You can't re-set the long error message, once
C             it has been set, without first signalling an error.
C
C          e. In 'RETURN' mode, further signalling of error
C             messages, and setting of the long message, are disabled.
C             (These capabilities can be re-enabled by calling RESET).
C
C
C      2.  If the error handling mechanism has been commanded to
C          ``ignore'' the error indicated by MSG, the call to SIGERR
C          has no effect.
C
C      If you wish to set the long error message, call
C      SETMSG BEFORE calling SIGERR.
C
C
C$ Examples
C
C
C  1.  In the following example, N is supposed to be less than
C      MAXLUN.  If it isn't, an error condition exists.
C
C      C
C      C      We will need a free logical unit.  But only if we don't
C      C      have too many files open already.
C      C
C
C             IF ( N .EQ. MAXLUN ) THEN
C
C                CALL SIGERR ( 'SPICE(TOOMANYFILESOPEN)' )
C                RETURN
C
C             END IF
C
C
C  2.  This time, we want to set the long error message, too.
C
C
C
C             IF ( N .EQ. MAXLUN ) THEN
C
C                CALL SETMSG ( 'RDTEXT:  Can't open another file; ' //
C            .                 'max number of files open at once '  //
C            .                 'for reading by RDTEXT is 20'  )
C
C                CALL SIGERR ( 'SPICE(TOOMANYFILESOPEN)' )
C                RETURN
C
C             END IF
C
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
C-    SPICELIB Version 2.1.1, 18-APR-2014 (BVS)
C
C        Minor header edits.
C
C-    SPICELIB Version 2.1.0, 26-JUL-1996 (KRG)
C
C        The STOP statement in this subroutine has been replaced
C        with a call to the subroutine BYEBYE which passes a failure
C        status to the operating system or command shell/environment
C        on all platforms which support this capability.
C
C-    SPICELIB Version 2.0.0, 22-APR-1996 (KRG)
C
C        This subroutine has been modified in an attempt to improve 
C        the general performance of the SPICELIB error handling 
C        mechanism. The specific modification has been to change the 
C        type of the error action from a short character string to an 
C        integer. This change is backwardly incompatible because the 
C        type has changed.
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
C     signal error condition
C
C-&

C$ Revisions
C
C-     SPICELIB Version 2.1.0, 26-JUL-1996 (KRG)
C
C         The STOP statement in this subroutine has been replaced
C         with a call to the subroutine BYEBYE which passes a failure
C         status to the operating system or command shell/environment
C         on all platforms which support this capability.
C
C-     SPICELIB Version 2.0.0, 22-APR-1996 (KRG)
C
C         This subroutine has been modified in an attempt to improve 
C         the general performance of the SPICELIB error handling 
C         mechanism. The specific modification has been to change the 
C         type of the error action from a short character string to an 
C         integer. This change is backwardly incompatible because the 
C         type has changed.
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-&

C
C     SPICELIB functions:
C
 
      LOGICAL               FAILED
      LOGICAL               SETERR
      LOGICAL               ACCEPT
 
C
C     Local Parameters
C
C     Define mnemonics for the integer action codes used by the error 
C     handling. See ERRACT for the character string equivalents used.
C
      INTEGER               IABRT
      PARAMETER           ( IABRT =          1 )

      INTEGER               IREPRT
      PARAMETER           ( IREPRT = IABRT + 1 )

      INTEGER               IRETRN
      PARAMETER           ( IRETRN = IREPRT + 1 )

      INTEGER               IIGNOR
      PARAMETER           ( IIGNOR = IRETRN + 1 )

      INTEGER               IDEFLT
      PARAMETER           ( IDEFLT = IIGNOR + 1 )
C
C     Length for output messages default settings.
C
      INTEGER               DEFLEN
      PARAMETER           ( DEFLEN = 40)
C
C     Local Variables:
C
      CHARACTER*(DEFLEN)    DEFMSG
      CHARACTER*(DEFLEN)    ERRMSG

      INTEGER               ACTION
 
      LOGICAL               STAT
      SAVE
C
C     Initial Values
C
C     Define the default error message strings for OUTMSG.
C
      DATA       DEFMSG / 'SHORT, EXPLAIN, LONG, TRACEBACK, DEFAULT' /
      DATA       ERRMSG / 'SHORT, EXPLAIN, LONG, TRACEBACK'          /
C
C     We must first check whether the error indicated by
C     MSG is one we're supposed to ignore...
C
C     There are two cases in which we do not want to respond
C     to the signalled error.
C
C     1.  When the error action is 'IGNORE'.  The user has
C         commanded that all messages be ignored.
C
C     2.  When the error action is 'RETURN', and an error
C         condition already exists.  We wish to preserve the
C         error data from the FIRST error until the user/
C         user's program has reset the error status via
C         a call to RESET.
C
 
      CALL GETACT ( ACTION )
 
      IF ( ACTION .NE. IIGNOR ) THEN
 
         IF ( ( ACTION .NE. IRETRN ) .OR. ( .NOT. FAILED() ) ) THEN
C
C           This one's for real.  Indicate an error condition, and
C           store the short error message.
C
C           Note:  the following strange -- looking function
C           reference sets the toolkit error status.  STAT
C           doesn't have any meaning.
C
            STAT = SETERR ( .TRUE. )
 
            CALL PUTSMS ( MSG )
C
C           Create a frozen copy of the traceback:
C
            CALL FREEZE
 
C
C           Now we output the error data that are available at this 
C           time, and whose output has been enabled.  The choice of
C           data is any combination of the following:
C
C              1. The short error message
C              2. The explanation of the short error message
C              3. The traceback
C              4. The long error message
C              5. The default message
C
C           Note that OUTMSG outputs only those messages which have
C           been SELECTED for output, via a call to ERRPRT, except
C           if the error action is DEFAULT.  In that case, the
C           default message selection applies.
C
            IF (  ACTION .NE. IDEFLT ) THEN
 
               CALL OUTMSG ( ERRMSG )
 
            ELSE
 
               CALL OUTMSG ( DEFMSG )
 
            END IF
 
            IF (  ACTION .EQ. IRETRN )  THEN
C
C              Don't accept new long error messages or updates
C              to current long error message:
C              (STAT has no meaning).
C
               STAT  =  ACCEPT ( .FALSE. )
 
            ELSE
 
               STAT  =  ACCEPT ( .TRUE. )
 
            END IF

         ELSE 
 
            STAT  =  ACCEPT ( .FALSE. )
 
         END IF

      END IF 
C
C     We could be in ABORT or DEFAULT mode.
C
 
      IF ( ( ACTION .EQ. IDEFLT ) .OR. ( ACTION .EQ. IABRT ) ) THEN
 
         CALL BYEBYE ( 'FAILURE' )
 
      END IF
 
C
C     That's all, folks!
C
      RETURN
      END
