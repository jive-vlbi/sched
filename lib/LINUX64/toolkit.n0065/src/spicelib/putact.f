C$Procedure      PUTACT ( Store Error Response Action )
 
      SUBROUTINE PUTACT ( ACTION )
 
C$ Abstract
C
C     PUTACT is a low-level data structure access routine which
C     stores the error response action.  DO NOT CALL THIS ROUTINE.
C     USE ERRACT, NOT PUTACT, TO SET THE CURRENT ERROR RESPONSE ACTION.
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
 
      INTEGER               ACTION
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      ACTION     I   The integer code for the error response action.
C
C$ Detailed_Input
C
C      ACTION    The new integer code for the error response action.  
C                This code is saved for use by the error handling 
C                system.
C
C$ Detailed_Output
C
C      None.
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
C      This is a data structure access routine for the SPICELIB
C      error response action.  This routine should be used for
C      no other purpose. In particular, it should not be used
C      by non-SPICELIB routines to set up an error response;
C      use ERRACT for that.
C
C$ Examples
C
C      None.
C
C$ Restrictions
C
C      DO NOT CALL THIS ROUTINE.
C
C      Calls to this routine by routines other than the
C      SPICELIB error handling routines may interfere
C      with error processing.
C
C      See the subroutine ERRACT for the definitions of the error
C      action codes.
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
C         type of the saved error action from a short character string 
C         to an integer. This change is backwardly incompatible 
C         because the type of the input argument has changed. This 
C         should pose no difficulties because it is a private subroutine 
C         used by the error handling system, and hence isolated from 
C         direct use.
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
C-     SPICELIB Version 2.0.0, 22-APR-1996 (KRG)
C
C         This subroutine has been modified in an attempt to improve 
C         the general performance of the SPICELIB error handling 
C         mechanism. The specific modification has been to change the 
C         type of the saved error action from a short character string 
C         to an integer. This change is backwardly incompatible 
C         because the type of the input argument has changed. This 
C         should pose no difficulties because it is a private subroutine 
C         used by the error handling system, and hence isolated from 
C         direct use.
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     Beta Version 1.0.1, 08-FEB-1989 (NJB)
C
C         Warnings added to discourage use of this routine in
C         non-error-handling code.
C
C-&

C
C     Local Prameters:
C
C     Define the mnemonic for the default error action.
C
      INTEGER               IDEFLT
      PARAMETER           ( IDEFLT = 5 )
C
C     Local Variables:
C
C     The current error response action:
C
 
      INTEGER                   SAVACT
 
      SAVE                      SAVACT
 
C
C     Initial values:
C
 
      DATA    SAVACT            / IDEFLT /

C
C     Executable Code:
C

      SAVACT = ACTION
 
      RETURN


C$Procedure  GETACT ( Get Error Response Action )
 
       ENTRY GETACT ( ACTION )
 
C$ Abstract
C
C      Return the value of the current error response action.
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
C      INTEGER               ACTION
C
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C
C      ACTION     O   The integer code for the error response action.
C
C$ Detailed_Input
C
C      None.
C
C$ Detailed_Output
C
C      ACTION    is the integer code for the current error response
C                action.  See the ERRACT subroutine and the "required 
C                reading" file for a detailed discussion of error 
C                response actions.
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
C      None.
C
C$ Examples
C
C      None.
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
C      K.R. Gehrigner  (JPL)
C
C$ Version
C
C-     SPICELIB Version 2.0.0, 22-APR-1996 (KRG)
C
C         This subroutine has been modified in an attempt to improve 
C         the general performance of the SPICELIB error handling 
C         mechanism. The specific modification has been to change the 
C         type of the saved error action from a short character string 
C         to an integer. This change is backwardly incompatible 
C         because the type of the input argument has changed. This 
C         should pose no difficulties because it is a private subroutine 
C         used by the error handling system, and hence isolated from 
C         direct use.
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
C-     SPICELIB Version 2.0.0, 22-APR-1996 (KRG)
C
C         This subroutine has been modified in an attempt to improve 
C         the general performance of the SPICELIB error handling 
C         mechanism. The specific modification has been to change the 
C         type of the saved error action from a short character string 
C         to an integer. This change is backwardly incompatible 
C         because the type of the input argument has changed. This 
C         should pose no difficulties because it is a private subroutine 
C         used by the error handling system, and hence isolated from 
C         direct use.
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-&

C
C     Executable Code:
C
 
C
C     Grab saved error response action:
C
 
      ACTION = SAVACT

      RETURN
      END
