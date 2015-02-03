C$Procedure      GETMSG ( Get Error Message )
 
      SUBROUTINE GETMSG ( OPTION, MSG )
 
C$ Abstract
C
C     Retrieve the current short error message,
C     the explanation of the short error message, or the
C     long error message.
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
 
      CHARACTER*(*)          OPTION
      CHARACTER*(*)          MSG
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      OPTION     I   Indicates type of error message.
C      MSG        O   The error message to be retrieved.
C
C
C$ Detailed_Input
C
C      OPTION  Indicates the type of error message to be retrieved.
C              The choices are:  The current short error message,
C              the explanation of the short error message,
C              or the current long error message.
C
C              Possible values of OPTION are:
C
C              'SHORT'   -- indicates that the short message is to
C                           be retrieved
C
C              'EXPLAIN' -- indicates that the explanation of the
C                           short message is to be retrieved
C
C              'LONG'    -- indicates that the long message is to
C                           be retrieved
C
C              The input strings indicating the choice of option
C              may be in mixed case.  For example, there is no
C              problem with the call,
C
C                    CALL GETMSG ( 'loNg' , MSG )
C
C$ Detailed_Output
C
C      MSG     Is the error message to be retrieved.
C              Its value depends on OPTION, and on whether
C              an error condition exists.
C
C              When there is no error condition, MSG is blank.
C
C
C              If an error condition does exist,
C
C                When OPTION is
C
C                'SHORT'    --  MSG is the current short error message.
C                               This is a very condensed, 25-character
C                               description of the error.
C
C                'EXPLAIN'  --  MSG is the explanation of the current
C                               short error message.  This is a one-line
C                               expansion of the text of the short
C                               message.
C
C                               All SPICELIB short error messages
C                               do have corresponding explanation text.
C                               For other short error messages, if
C                               there is no explanation text, MSG
C                               will be blank.
C
C                'LONG'     --  MSG is the current long error message.
C                               The long error message is a detailed
C                               explanation of the error, possibly
C                               containing data specific to the
C                               particular occurrence of the error.
C                               Not all errors have long error messages.
C                               If there is none, MSG will be blank.
C                               Long error messages are no longer than
C                               320 characters.
C
C                invalid    --  MSG will remain unchanged from
C                               its value on input.
C
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C      Errors detected:
C
C      1.  SPICE(INVALIDMSGTYPE)
C
C          This routine signals an error condition if the input,
C          OPTION, is invalid.  In that case no messages are
C          returned; MSG retains the value it had on input.
C
C
C      This routine is part of the interface to the
C      SPICELIB error handling mechanism.  For this reason,
C      this routine does not participate in the trace scheme,
C      even though it has external references.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C      Please read the "required reading" first!
C
C      A good time to call this routine would be when an error
C      condition exists, as indicated by the SPICELIB function,
C      FAILED.
C
C      See the example below for a serving suggestion.
C
C      GETMSG isn't too useful if an error condition doesn't
C      exist, since it will return a blank string in that case.
C
C
C$ Examples
C
C
C      Here's an example of a real-life call to GETMSG to get the
C      explanation of the current short error message.
C
C      In this example, a SPICELIB routine, RDTEXT, is called.
C      Following the return from RDTEXT, the logical function,
C      FAILED, is tested to see whether an error occurred.
C      If it did, the message is retrieved and output via
C      a user-defined output routine:
C
C
C      C
C      C     We call RDTEXT; then test for errors...
C      C
C            CALL RDTEXT ( FILE, LINE, EOF )
C
C            IF ( FAILED ) THEN
C
C      C
C      C        Get explanation text for the current short message
C      C        and print it:
C      C
C
C               CALL GETMSG ( 'EXPLAIN', TEXT )
C
C               CALL USER_DEFINED_OUTPUT ( TEXT )
C
C                     .
C                     .   [Do more stuff here]
C                     .
C
C            END IF
C
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
C     get error message
C
C-&
 
 
C
C     Local Variables:
C
 
C
C     Length of short error message:
C
 
      INTEGER               SHRTLN
      PARAMETER           ( SHRTLN = 25 )
 
      CHARACTER*(SHRTLN)    SHRTMS
 
C
C     Upper case version of the option:
C
 
      INTEGER               OPTLEN
      PARAMETER           ( OPTLEN = 10 )
 
      CHARACTER*(OPTLEN)    UPOPT
      CHARACTER*(OPTLEN)    LOCOPT
 
C
C     Heeeeeeeeeeeeeeeeeeeeer's the code!
C
 
C
C     We only speak upper case in this routine,
C     so convert any lower case letters in OPTION
C     to upper case.  We save the original OPTION
C     string just in case we need to echo it in
C     an error message.
C
 
 
      CALL LJUST ( OPTION, UPOPT )
      CALL UCASE ( UPOPT,  UPOPT )
 
      IF ( UPOPT .EQ. 'SHORT' ) THEN
 
C
C        Retrieve short message:
C
         CALL GETSMS ( MSG )
 
 
      ELSE IF ( UPOPT .EQ. 'EXPLAIN' ) THEN
 
C
C        Get current short message; then get explanation
C        corresponding to current short error message:
C
 
         CALL GETSMS  ( SHRTMS )
         CALL EXPLN   ( SHRTMS, MSG )
 
 
      ELSE IF ( UPOPT .EQ. 'LONG' ) THEN
 
C
C        Grab long error message:
C
         CALL GETLMS  ( MSG )
 
      ELSE
 
C
C        Invalid value of OPTION!!  Signal error, and set long
C        error message as well:
C
 
         LOCOPT  =  OPTION
 
         CALL SETMSG (
     .                'GETMSG: An invalid value of OPTION was'        //
     .                ' input.  Valid choices are ''SHORT'','         //
     .                '       ''EXPLAIN'', or ''LONG''.  The value'   //
     .                ' that was input was:  '                        //
     .                 LOCOPT                                      )
 
         CALL SIGERR ( 'SPICE(INVALIDMSGTYPE)' )
 
 
 
      END IF
 
 
      END
