C$Procedure      ACCEPT ( Accept New Long Error Message )
 
      LOGICAL FUNCTION ACCEPT ( OK )
 
C$ Abstract
C
C      Indicate to the SPICELIB error handling mechanism whether or not
C      a replacement or modification of the long error message can be
C      accepted.  DO NOT CALL THIS ROUTINE.
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
 
       LOGICAL               OK
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      OK         I   Indicates whether long error msg changes are ok.
C
C      The function takes an UNSPECIFIED value on exit.
C
C$ Detailed_Input
C
C      OK       Indicates to the error handling mechanism whether
C               replacement of or changes to the long error message
C               are to be allowed; for them to be allowed,
C               both of the following must be true:
C
C               1. No error condition exists, or the error response
C                  action is not 'RETURN'.
C
C               2. The current error response mode is not 'IGNORE'.
C
C
C$ Detailed_Output
C
C      The function is assigned a value on output, but the
C      value is not meaningful.
C
C$ Parameters
C
C      None.
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
C      DO NOT CALL THIS ROUTINE.
C
C$ Examples
C
C      None.
C
C$ Restrictions
C
C      DO NOT CALL THIS ROUTINE.
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
C
C-     Beta Version 1.1.0, 13-DEC-1989 (NJB)
C
C         ACCEPT must return a value, in order to comply with the
C         Fortran standard.  So, now it does.  The value has no
C         meaning, as far as the specification of ACCEPT is
C         concerned.
C
C-     Beta Version 1.0.1, 08-FEB-1989 (NJB)
C
C         Warnings added to discourage use of this routine in
C         non-error-handling code.
C
C-&
 
 
 
C
C     SPICELIB functions:
C
 
 
      LOGICAL               ALLOWD
 
 
C
C     Local Variables:
C
 
 
      LOGICAL               SAVOK
 
      SAVE                  SAVOK
 
 
C
C     Initial Values:
C
 
      DATA       SAVOK  /  .TRUE.  /
 
C
C     Executable Code:
C
 
 
 
      SAVOK  =  OK
 
      ACCEPT = .FALSE.
 
      RETURN
 
 
 
 
C$Procedure      ALLOWD    (Are Changes of Long Error Message Allowed?)
 
       ENTRY  ALLOWD ()
 
C$ Abstract
C
C      True if replacement or modification of the long error message
C      is allowed.  DO NOT CALL THIS ROUTINE.
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
C      The function takes the value, .TRUE., if replacement or
C      modification of the long error message is currently allowed.
C
C$ Detailed_Input
C
C      None.
C
C$ Detailed_Output
C
C      The function takes the value, .TRUE., if replacement of or
C      changes to the long error message are to be allowed; for them
C      to be allowed, both of the following must be true:
C
C      1. No error condition exists, or the error response
C         action is not 'RETURN'.
C
C      2. The current error response mode is not 'IGNORE'.
C
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C      This routine does not detect any errors.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C      DO NOT CALL THIS ROUTINE.
C
C      Non-error handling routines should not call this routine.  Such
C      routines can set the long error message using SETMSG, which
C      itself calls this routine to test whether an update is allowed.
C
C      The initial value returned by ALLOWD is .FALSE.
C
C$ Examples
C
C      None.
C
C$ Restrictions
C
C      DO NOT CALL THIS ROUTINE.
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
C     allow changes of long error message
C
C-&
 
 
C$ Revisions
C
C-     Beta Version 1.1.0, 18-DEC-1989 (HAN)
C
C         Empty parentheses added to the ENTRY statement in order to
C         comply with the ANSI Fortran 77 Standard.
C
C-     Beta Version 1.0.1, 08-FEB-1989 (NJB)
C
C         Warnings added to discourage use of this routine in
C         non-error-handling code.
C
C-&
 
 
C
C     Executable Code:
C
 
      ALLOWD = SAVOK
 
 
      END
