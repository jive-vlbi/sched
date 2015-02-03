C$Procedure      SETERR ( Set Error Status )
 
      LOGICAL FUNCTION SETERR ( STATUS )
 
C$ Abstract
C
C     Set the SPICELIB error status.  DO NOT CALL THIS ROUTINE.
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
 
       LOGICAL               STATUS
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      STATUS     I   Status indicator.
C
C
C      The function takes an UNSPECIFIED (and meaningless) value
C      on exit.
C
C$ Detailed_Input
C
C      STATUS  Indicates the new status.  When .TRUE., it
C              means that an error condition exists.
C
C$ Detailed_Output
C
C      None.
C
C      This purpose of this routine is to set status; the
C      function takes an UNSPECIFIED value on exit.  The
C      assigned value does not have any meaning.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C      None.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C      DO NOT CALL THIS ROUTINE.
C
C      This is a data structure access routine for the
C      SPICELIB status.  This routine should be used for no
C      other purpose; in particular, it should not be used
C      to signal errors.  Use SIGERR or FAILED for that.
C
C      This routine assigns a value to SETERR on exit.
C      However, the value is not meaningful.
C
C$ Examples
C
C      None.  DON'T CALL THIS ROUTINE.
C
C      No examples.  If you don't know EXACTLY what a
C      ``data structure access routine'' is, don't call
C      this routine.  If you do know, you don't need an
C      example.
C
C$ Restrictions
C
C      DON'T CALL THIS ROUTINE.
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
C     None.
C
C-&
 
 
 
C$ Revisions
C
C-     Beta Version 1.0.1, 08-FEB-1989 (NJB)
C
C         Warnings added to discourage use of this routine in
C         non-error-handling code.
C
C-&
 
 
 
C
C     Local Variables:
C
 
C
C     The SPICELIB status:
C
 
      LOGICAL               SVSTAT
 
      SAVE                  SVSTAT
 
C
C     Declaration of the entry point, FAILED:
C
 
      LOGICAL FAILED
 
 
C
C     Initial values:
C
 
      DATA    SVSTAT      / .FALSE. /
 
C
C     Executable Code:
C
 
 
      SVSTAT = STATUS
 
C
C     Give SETERR a value; the value does not have any
C     meaning, but it appears standard FORTRAN requires this.
C
 
      SETERR = .TRUE.
 
 
      RETURN
 
 
 
 
C$Procedure      FAILED ( Error Status Indicator )
 
       ENTRY FAILED ()
 
C$ Abstract
C
C      True if an error condition has been signalled via SIGERR.
C      FAILED is the SPICELIB status indicator.
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
C      The function takes the value .TRUE. if an error condition
C      was detected; it is .FALSE. otherwise.
C
C
C$ Detailed_Input
C
C      None.
C
C$ Detailed_Output
C
C      Please read the required reading file before reading this!
C
C      The value taken by FAILED indicates status.
C
C      The status value applies to the SPICELIB routines,
C      and to any other routines which call the status-setting
C      routine, SIGERR.
C
C      When FAILED has the value, .TRUE., an error condition
C      exists.   .FALSE. means "no error."
C
C      More specifically, when FAILED has the value .TRUE.,
C      some routine has indicated an error by calling the
C      SPICELIB routine, SIGERR.  All SPICELIB routines
C      which can detect errors do this.  Non-SPICELIB
C      routines may also reference SIGERR if desired.
C
C      When FAILED has the value .FALSE., either no routine
C      has yet signalled an error via SIGERR, or the status
C      has been reset using, what else, RESET.
C
C      FAILED is initialized to have the value, .FALSE.
C      This indicates a  "no error" status.
C
C      See "particulars" below for (slightly) more information.
C
C
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C      None.
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
C      See the required reading file for details of error
C      processing.  However, here are some notes:
C
C      When any SPICELIB routine detects an error, the
C      status is set to indicate an error condition via
C      a call to SIGERR.   After SIGERR
C      returns, further calls to FAILED will return the
C      value, .TRUE., indicating an error condition.
C
C      Non-SPICELIB routines may also call SIGERR to indicate
C      an error condition; FAILED will reflect such calls
C      as well.
C
C      It is possible to re-set the error status to indicate
C      "no error" using the SPICELIB routine, RESET (see).
C
C      The effect on FAILED of resetting the status is
C      that FAILED will again return the value .FALSE.,
C      indicating "no error."
C
C      One of the main virtues of the SPICELIB error
C      handling mechanism is that you don't HAVE to test the
C      error status after every call to a SPICELIB routine.
C      If you set the error handling mode to 'RETURN', using
C      the routine, ERRACT, SPICELIB routines won't crash
C      when an error occurs; following the detection of the
C      error, each routine will return immediately upon entry.
C      Therefore, you call several SPICELIB routines in a
C      row, and just test status at the end of the sequence
C      of calls, if you wish.  See "examples" below.
C
C
C$ Examples
C
C      1.  Here's an example of a simple call to RDTEXT, followed
C          by a test of the status.
C
C
C      C
C      C     We read a line of text from file SPUD.DAT:
C      C
C
C            CALL RDTEXT ( 'SPUD.DAT', LINE, EOF )
C
C            IF ( FAILED() ) THEN
C
C      C        An error occurred during the read.
C
C               [respond to error here]
C
C            END IF
C
C
C      2.    Here's an example in which we don't want to
C            put the error test inside our loop.  We just
C            test the error status after the loop terminates.
C            We can do this because we (that is, you, the user)
C            have made the call,
C
C                   CALL ERRACT ( 'RETURN' )
C
C            prior to execution of the following code.  If an
C            error does occur, the remaining calls to RDTEXT
C            will have no effect.  Here's the example:
C
C      C
C      C     We read the first 5000 lines of a file, or until
C      C     EOF is reached, whichever comes first:
C      C
C      C     Note:  the "DO WHILE" construct is available in
C      C     VAX FORTRAN.
C      C
C
C            LCOUNT = 0
C            DO WHILE (  ( .NOT. EOF ) .AND. ( LCOUNT .LE. 5000 )  )
C
C               CALL RDTEXT ( 'SPUD.DAT', LINE(LCOUNT), EOF )
C
C               LCOUNT = LCOUNT + 1
C
C            END DO
C
C            IF ( FAILED() ) THEN
C      C
C      C        An error occurred during the read
C      C
C               [respond to error here]
C
C            END IF
C
C
C
C$ Restrictions
C
C      This routine automatically detects errors occurring in
C      the SPICELIB code.  To make this routine work
C      for your own routines, your routines must call SIGERR
C      to report errors.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      N.J. Bachman    (JPL)
C      H.A. Neilan     (JPL)
C
C$ Version
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
C     error status indicator
C
C-&
 
 
 
C$ Revisions
C
C-     Beta Version 1.1.0, 18-DEC-1989 (HAN)
C
C         Empty parentheses added to the ENTRY statement in order to
C         comply with the ANSI Fortran 77 Standard.
C
C-&
 
 
 
 
 
C
C     Executable Code:
C
 
C
C     Grab saved status value:
C
 
      FAILED = SVSTAT
 
      END
 
