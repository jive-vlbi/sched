C$Procedure      PUTSMS ( Store Short Error Message )
 
      SUBROUTINE PUTSMS ( MSG )
 
C$ Abstract
C
C     PUTSMS is a low-level data structure access routine which stores
C     the short error message.  DO NOT CALL THIS ROUTINE.  USE SIGERR,
C     NOT PUTSMS, TO SIGNAL ERRORS.
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
 
      INCLUDE 'errhnd.inc'
 
      CHARACTER*(*)          MSG
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     MSG        I   A short error message.
C
C$ Detailed_Input
C
C     MSG   The current short error message.  This value will be saved.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     SMSGLN  is the maximum length of the short error message.  See
C             the include file errhnd.inc for the value of SMSGLN.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This is a data structure access routine for the
C     toolkit short error message.  This routine should be
C     used for no other purpose; in particular, it should
C     not be used to signal errors.  Use SIGERR for that.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     Calls to this routine by routines other than the
C     SPICELIB error handling routines may interfere
C     with error processing.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 29-JUL-1997 (NJB)
C
C        Maximum length of the short error message is now represented
C        by the parameter SMSGLN.  Miscellaneous header fixes were
C        made.  Some indentation and vertical white space abnormalities
C        in the code were fixed.  Some dubious comments were deleted
C        from the code.
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
C     None.
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 29-JUL-1997 (NJB)
C
C        Maximum length of the short error message is now represented
C        by the parameter SMSGLN.  Miscellaneous header fixes were
C        made.  Some indentation and vertical white space abnormalities
C        in the code were fixed.  Some dubious comments were deleted
C        from the code.
C
C-    Beta Version 1.0.1, 08-FEB-1989 (NJB)
C
C        Warnings added to discourage use of this routine in
C        non-error-handling code.
C
C-&
 
 
C
C     Local Variables:
C
 
C
C     The current short error message:
C
      CHARACTER*(SMSGLN)    SAVMSG
 
      SAVE                  SAVMSG
 
C
C     Initial values:
C
      DATA    SAVMSG      / ' ' /
 
C
C     Executable Code:
C
      SAVMSG = MSG
 
      RETURN
 
 
 
 
C$Procedure      GETSMS ( Get Short Error Message )
 
       ENTRY GETSMS ( MSG )
 
C$ Abstract
C
C     Return the value of the current short error message.
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
C     CHARACTER*(*)          MSG
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C
C     MSG        O   The current short error message.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     MSG    is the current short error message.  See the
C            "required reading" file for a detailed discussion
C            of error messages.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
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
C     See the required reading file for details of error processing.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 29-JUL-1997 (NJB)
C
C        Maximum length of the short error message is now represented
C        by the parameter SMSGLN.  Miscellaneous header fixes were
C        made.  Some indentation and vertical white space abnormalities
C        in the code were fixed.  Some dubious comments were deleted
C        from the code.
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
C     None.
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 29-JUL-1997 (NJB)
C
C        Maximum length of the short error message is now represented
C        by the parameter SMSGLN.  Miscellaneous header fixes were
C        made.  Some indentation and vertical white space abnormalities
C        in the code were fixed.  Some dubious comments were deleted
C        from the code.
C
C-&
 
 
 
C
C     Grab saved short message:
C
      MSG = SAVMSG
 
      END
