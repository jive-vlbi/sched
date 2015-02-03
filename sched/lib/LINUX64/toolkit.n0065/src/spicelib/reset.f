C$Procedure      RESET ( Reset Error Status )
 
      SUBROUTINE RESET
 
C$ Abstract
C
C     Reset the SPICELIB error status to a value of "no error."
C     As a result, the status routine, FAILED, will return a value
C     of .FALSE.
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
C
C     None.
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
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
C     None.
C
C$ Exceptions
C
C     This routine does not detect any errors.
C
C     However, this routine is part of the SPICELIB error
C     handling mechanism.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Please read the "required reading" first!
C
C     The effects of this routine are:
C
C     1. The SPICELIB status is set to a value of "no error."
C
C     2. The long and short error messages are set to blank.
C
C     3. Setting of the long error message is re-enabled.
C
C
C     Subsequent to a call to RESET, references to the status
C     indicator function, FAILED, will return a value of .FALSE.,
C     until an error is detected.
C
C     This routine should be called in cases where one wishes
C     to attempt to continue processing after detection of an
C     error, and the 'RETURN' error action is being used.  When
C     the error response action is set to 'RETURN', routines
C     that have external references, or that can
C     detect errors, return immediately upon entry when an
C     error condition exists.  This prevents a program from
C     crashing, but does not allow for a recovery attempt.
C
C     If one does wish to attempt to recover,
C     in general the procedure is to test for an error
C     condition, and if one exists, respond to the error
C     (by outputting diagnostic messages, for example).  Next,
C     a call to RESET can be made.  After resetting the
C     error status, the normal execution thread can be resumed.
C
C     It is also appropriate to call this routine when the error
C     response action is 'REPORT', if one wishes to recover
C     from errors.
C
C$ Examples
C
C     1.  In this example, we try to read a line from the file,
C         SPUD.DAT, using the toolkit routine, RDTEXT.
C         When FAILED indicates an error, we grab the short
C         error message and its explanation, using GETMSG (see),
C         log the messages using our user-defined routine,
C         USER_LOG (NOT a SPICELIB routine), reset the
C         status, and keep going.
C
C     C
C     C      We read a line from SPUD.DAT:
C     C
C
C            CALL RDTEXT ( 'SPUD.DAT', LINE, EOF )
C
C            IF ( FAILED() ) THEN
C     C
C     C         Oops! an error occurred during the read.
C     C         Recover the short error message and its
C     C         explanation, reset the error status,
C     C         log the messages, and continue...
C     C
C
C               CALL GETMSG   ( 'SHORT'    ,    SMSG )
C               CALL GETMSG   ( 'EXPLAIN'  ,    EXPL )
C
C               CALL USER_LOG (  SMSG )
C               CALL USER_LOG (  EXPL )
C
C               CALL RESET
C
C            END IF
C
C$ Restrictions
C
C     It can be dangerous to call this routine without
C     RESPONDING to the error condition first; by calling
C     RESET, you are wiping out the SPICELIB's knowledge of
C     the error.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     K.R. Gehringer  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 14-MAR-1996 (KRG)
C
C        Removed the call to FREEZE at the end of this subroutine.
C        The call had no effect other than to copy the current 
C        stack in the trace package from the active stack into the 
C        frozen stack. The frozen stack could NEVER be accessed
C        after this copying action; the only time the frozen stack
C        could be accessed is when a program is executing in 'RETURN'
C        mode and FAILED is .TRUE., i.e. after an error has been
C        signalled, causing the active stack at the time of the 
C        error to be copied to the frozen stack. So this copying
C        of the active stack on a RESET of the error handling 
C        accomplishes nothing.
C
C        References to the setting of the frozen traceback were
C        removed from the header as well.
C
C        A missing Fortran RETURN statement was also added before the 
C        END statement
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
C     reset error status
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               SETERR
      LOGICAL               ACCEPT
 
C
C     Local Variables:
C
 
      LOGICAL               STAT
 
C
C     Executable Code:
C
C     This odd-looking function reference resets the error
C     status to indicate "no error":
C
      STAT = SETERR ( .FALSE. )
 
C
C     Wipe out the short and long error messages:
C
      CALL PUTSMS ( ' ' )
      CALL PUTLMS ( ' ' )
 
C
C     Allow long error message to be updated:
C
      STAT = ACCEPT ( .TRUE. )
 
      RETURN
      END
