C$Procedure     SETMSG  ( Set Long Error Message )
 
      SUBROUTINE SETMSG ( MSG )
 
C$ Abstract
C
C     Set the value of the current long error message.
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
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     MSG        I   A long error message.
C
C$ Detailed_Input
C
C     MSG     A ``long'' error message.
C             MSG is a detailed description of the error.
C             MSG is supposed to start with the name of the
C             module which detected the error, followed by a
C             colon.  Example:
C
C                'RDTEXT:  There are no more free logical units'
C
C             Only the first LMSGLN characters of MSG are stored;
C             any further characters are truncated.  
C
C             Generally, MSG will be stored internally by the SPICELIB
C             error handling mechanism.  The only exception
C             is the case in which the user has commanded the
C             toolkit to ``ignore'' the error indicated by MSG.
C
C             As a default, MSG will be output to the screen.
C             See the required reading file for a discussion of how
C             to customize toolkit error handling behavior, and
C             in particular, the disposition of MSG.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     LMSGLN  is the maximum length of the long error message.  See
C             the include file errhnd.inc for the value of LMSGLN.
C
C$ Exceptions
C
C     This routine does not detect any errors.
C
C     However, this routine is part of the interface to the
C     SPICELIB error handling mechanism.  For this reason,
C     this routine does not participate in the trace scheme,
C     even though it has external references.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The SPICELIB routine SIGERR should always be called
C     AFTER this routine is called, when an error is detected.
C
C     The effects of this routine are:
C
C        1.  If acceptance of a new long error message is
C            allowed:
C
C            MSG will be stored internally.  As a result,
C            The SPICELIB routine, GETMSG, will be able to
C            retrieve MSG, until MSG has been ``erased''
C            by a call to RESET, or overwritten by another
C            call to SETMSG.
C
C
C        2.  If acceptance of a new long error message is not allowed,
C            a call to this routine has no effect.
C
C$ Examples
C
C
C      In the following example, N is supposed to be less than
C      MAXLUN.  If it isn't, an error condition exists.
C
C      C
C      C      We will need a free logical unit.  But only if we don't
C      C      have too many files open already.
C      C
C
C             IF ( N .EQ. MAXLUN ) THEN
C
C                CALL SETMSG ( 'RDTEXT: Too many files open already' )
C                CALL SIGERR ( 'SPICE(TOOMANYFILESOPEN)' )
C
C                RETURN
C
C             END IF
C
C
C$ Restrictions
C
C     SIGERR must be called once after each call to this routine.
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
C-    SPICELIB Version 1.0.2, 29-JUL-1997 (NJB)
C
C        Maximum length of the long error message is now represented
C        by the parameter LMSGLN.  Miscellaneous header fixes were
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
C     set long error message
C
C-&
 
 
 
C$ Revisions
C
C-    SPICELIB Version 1.0.2, 29-JUL-1997 (NJB)
C
C        Maximum length of the long error message is now represented
C        by the parameter LMSGLN.  Miscellaneous header fixes were
C        made.  Some indentation and vertical white space abnormalities
C        in the code were fixed.  Some dubious comments were deleted
C        from the code.
C
C-     Beta Version 1.1.0, 17-FEB-1989 (NJB)
C
C         Declarations of the unused variable STAT and unused function
C         ACCEPT removed.
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               ALLOWD
 
C
C     We store the long error message only when updates
C     of the long message are allowed:
C
      IF ( ALLOWD() ) THEN

         CALL PUTLMS ( MSG )

      END IF

      END
