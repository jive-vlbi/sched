C$Procedure      RETURN ( Immediate Return Indicator )
 
      LOGICAL FUNCTION RETURN ()
 
C$ Abstract
C
C     True if SPICELIB routines should return immediately upon entry.
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
C
C      None.
C
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C
C      The function returns the value, .TRUE., if and only if SPICELIB
C      routines should return immediately upon entry.
C
C$ Detailed_Input
C
C      None.
C
C$ Detailed_Output
C
C      The function returns the value, .TRUE., if and only if SPICELIB
C      routines should return immediately upon entry.  The criterion
C      for this is that the error response action is set to
C      'RETURN', and an error condition exists.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C      This routine does not detect any errors.
C
C      However, this routine is part of the SPICELIB error
C      handling mechanism.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C      Please read the "required reading" first!
C
C      This routine can be referenced in non-toolkit code; in
C      fact, its use is encouraged.  Its purpose is to signal
C      to the routine calling it that the caller should
C      return immediately.  The reference to RETURN should
C      be the first executable line of the calling program.
C
C      In 'RETURN' mode, SPICELIB routines
C      that have external references, or that can
C      detect errors, return immediately upon entry when an
C      error condition exists.  They use RETURN to determine
C      when these conditions are met.  Non--toolkit routines
C      can do the same.
C
C      Additionally, when an error is signaled in 'RETURN' mode,
C      no further errors can be signaled until the error condition
C      is reset by a call to RESET.  Calls to SIGERR simply have
C      no effect.  Therefore, the error messages set in response
C      to the FIRST error that was detected will be saved until
C      RESET is called.  These messages can be retrieved by
C      calls to GETMSG.
C
C      There are a number of advantages to using this mechanism.
C      First, the likelihood of an error resulting in crash
C      in a different routine is greatly reduced.  Second,
C      a program does not have to test the error status
C      (using a reference to FAILED) after each call to a toolkit
C      routine, but rather can make one test of status at the end
C      of a series of calls.  See "Examples" below.
C
C      See the subroutine ERRACT for definitions of the error action 
C      codes.
C
C$ Examples
C
C      1.  In this example, we show how to place a reference
C          to RETURN in your code:
C
C          C
C          C     No executable lines precede this one.
C          C
C          C     Test whether to return before doing
C          C     anything else.
C          C
C
C                IF ( RETURN() )  RETURN
C
C
C                [ rest of code goes here]
C
C                          .
C                          .
C                          .
C
C
C      2.  Here's how one might code a sequence of calls
C          to routines with code that follows the pattern
C          given in example #1 above:
C
C                         .
C                         .
C                         .
C
C                [ code may go here ]
C
C          C
C          C     We call routines A, B, and C;  then we
C          C     test for errors, using the SPICELIB error
C          C     status indicator, FAILED:
C          C
C
C                CALL  A
C                CALL  B
C                CALL  C
C
C                IF ( FAILED() ) THEN
C
C          C
C          C        If we're here, an error occurred.  The
C          C        error might have been detected by A, B, C,
C          C        or by a routine called by one of them.
C          C        Get the explanation of the short error message
C          C        and output it using the routine, USER_OUT
C          C        [USER_OUT is a fictitious routine]:
C          C
C
C                   CALL GETMSG ( 'EXPLAIN', MSG )
C
C                   CALL USER_OUT ( MSG )
C
C                END IF
C
C                [ rest of code goes here ]
C
C                           .
C                           .
C                           .
C
C
C
C$ Restrictions
C
C      This routine has no effect unless the error action is 'RETURN'!
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
C-     SPICELIB Version 2.1.0, 04-APR-2014 (NJB)
C
C         Re-organized code to improve efficiency in the non-error
C         case.
C
C-     SPICELIB Version 2.0.0, 22-APR-1996 (KRG)
C
C         This subroutine has been modified in an attempt to improve 
C         the general performance of the SPICELIB error handling 
C         mechanism. The specific modification has been to change the 
C         type of error action from a short character string to an 
C         integer. This change is backwardly incompatible because the 
C         type has changed.
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
C     immediate return indicator
C
C-&

C$ Revisions
C
C-     SPICELIB Version 2.0.0, 22-APR-1996 (KRG)
C
C         This subroutine has been modified in an attempt to improve 
C         the general performance of the SPICELIB error handling 
C         mechanism. The specific modification has been to change the 
C         type of error action from a short character string to an 
C         integer. This change is backwardly incompatible because the 
C         type has changed.
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     Beta Version 1.1.0, 17-FEB-1989 (NJB)
C
C         Added parentheses to the declaration of RETURN.
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               FAILED

C
C     Local Parameters
C
C     Define the mnemonic for the return action.
C
      INTEGER               IRETRN
      PARAMETER           ( IRETRN = 3 )
C
C     Local Variables
C
      INTEGER               ACTION
C
C     Immediate return is indicated only in 'RETURN' mode,
C     when an error condition is in effect:
C
      IF ( .NOT. FAILED() ) THEN

         RETURN = .FALSE.
         RETURN

      END IF

C
C     At this point, we know a SPICE error condition exists.
C     
      CALL GETACT ( ACTION )
 
      RETURN  =  ACTION .EQ. IRETRN
 
      RETURN
      END
