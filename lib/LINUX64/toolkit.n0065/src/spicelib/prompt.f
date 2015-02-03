C$Procedure      PROMPT ( Prompt a user for a string )
 
      SUBROUTINE PROMPT ( PRMPT, STRING )
 
C$ Abstract
C
C     This routine prompts a user for keyboard input.
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
C     None.
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
 
      CHARACTER*(*)         PRMPT
      CHARACTER*(*)         STRING
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     PRMPT      I   The prompt to use when asking for input.
C     STRING     O   The response typed by a user.
C
C$ Detailed_Input
C
C     PRMPT      is a character string that will be displayed from the
C                current cursor position and describes the input that
C                the user is expected to enter.  The string PRMPT should
C                be relatively short, i.e., 50 or fewer characters, so
C                that a response may be typed on the line where the
C                prompt appears.
C
C                All characters (including trailing blanks) in PRMPT
C                are considered significant and will be displayed.
C
C$ Detailed_Output
C
C     STRING     is a character string that contains the string
C                entered by the user.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This subroutine uses discovery check-in so that it may be called
C     after an error has occurred.
C
C     1) If the attempt to write the prompt to the standard output
C        device fails, returning an IOSTAT value not equal to zero, the
C        error SPICE(WRITEFAILED) will be signalled.
C
C     2) If the attempt to read the response from the standard input
C        device fails, returning an IOSTAT value not equal to zero, the
C        error SPICE(READFAILED) will be signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This is a utility that allows you to "easily" request information
C     from a program user.  At a high level, it frees you from the
C     peculiarities of a particular implementation of FORTRAN cursor
C     control.
C
C$ Examples
C
C     Suppose you wanted to ask a user to input an answer to
C     a question such as "Do you want to try again? (Y/N) "
C     and leave the cursor at the end of the question as shown here:
C
C        Do you want to try again? (Y/N) _
C
C     (The underscore indicates the cursor position).
C
C     The following line of code will do what you want.
C
C        CALL PROMPT ( 'Do you want to try again? (Y/N) ', ANSWER )
C
C$ Restrictions
C
C     This routine is environment specific.  Standard FORTRAN does not
C     provide for user control of cursor position after write
C     statements.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer (JPL)
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.25.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    SPICELIB Version 3.24.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    SPICELIB Version 3.23.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    SPICELIB Version 3.22.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    SPICELIB Version 3.21.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GCC_C.
C
C-    SPICELIB Version 3.20.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    SPICELIB Version 3.19.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-CC_C.
C
C-    SPICELIB Version 3.18.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
C
C-    SPICELIB Version 3.17.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-    SPICELIB Version 3.16.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    SPICELIB Version 3.15.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 3.14.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-64BIT-MS_C.
C
C-    SPICELIB Version 3.13.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-INTEL_C.
C
C-    SPICELIB Version 3.12.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    SPICELIB Version 3.11.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 3.10.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    SPICELIB Version 3.9.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    SPICELIB Version 3.8.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    SPICELIB Version 3.7.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-LINUX-64BIT-GCC_C.
C
C-    SPICELIB Version 3.6.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-INTEL_C.
C
C-    SPICELIB Version 3.5.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    SPICELIB Version 3.4.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-    SPICELIB Version 3.3.0, 26-OCT-2005 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-GCC_C.
C
C-    SPICELIB Version 3.2.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN_C.
C
C-    SPICELIB Version 3.1.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    SPICELIB Version 3.0.5, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 3.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 3.0.3, 24-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    SPICELIB Version 3.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 3.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 3.0.0, 08-APR-1998 (NJB)
C
C        Module was updated for the PC-LINUX platform.
C
C-    SPICELIB Version 2.0.0, 20-JUL-1995 (WLT) (KRG)
C
C        This routine now participates in error handling.  It
C        checks to make sure no I/O errors have occurred while
C        attempting to write to standard output or read from standard
C        input. It uses discovery checkin if an error is detected.
C
C        Restructured the subroutine a little bit; the writing of the
C        prompt is the only bit that is environment specific, so the
C        code was rearranged to reflect this. There is now only a single
C        READ statement.
C
C-    SPICELIB Version 1.0.0, 15-OCT-1992 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Prompt for keyboard input
C     Prompt for input with a user supplied message
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 3.0.0, 08-APR-1998 (NJB)
C
C        Module was updated for the PC-LINUX platform.
C
C-    SPICELIB Version 2.0.0, 20-JUL-1995 (WLT) (KRG)
C
C        This routine now participates in error handling.  It
C        checks to make sure no I/O errors have occurred while
C        attempting to write to standard output or read from standard
C        input. It uses discovery checkin if an error is detected.
C
C        Restructured the subroutine a little bit; the writing of the
C        prompt is the only bit that is environment specific, so the
C        code was rearranged to reflect this. There is now only a single
C        READ statement.
C
C-&
 
C
C     Local variables
C
      INTEGER               IOSTAT
 
C
 
C
C
C     The code below should be used in the following environments:
C
C     SUN/Fortran,
C     HP/HP-Fortran,
C     Silicon Graphics/Silicon Graphics Fortran,
C     DEC Alpha-OSF/1--DEC Fortran,
C     NeXT/Absoft Fortran
C     PC Linux/Fort77
C
      WRITE (*,FMT='(A,$)', IOSTAT=IOSTAT ) PRMPT
 
C
C     If none of the write statements above works on a particular
C     unsupported platform, read on...
C
C     Although, this isn't really what you want, if you need to port
C     this quickly to an environment that does not support the format
C     statement in any of the cases above, you can comment out the
C     write statement above and un-comment the write statement below.
C     In this way you can get a program working quickly in the new
C     environment while you figure out how to control cursor
C     positioning.
C
C      WRITE (*,*, IOSTAT=IOSTAT ) PRMPT
C
C     Check for a write error. It's not likely, but the standard output
C     can be redirected. Better safe than confused later.
C
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL CHKIN ( 'PROMPT' )
         CALL SETMSG ( 'An error occurred while attempting to '
     .   //            'write a prompt to the standard output '
     .   //            'device, possibly because standard output '
     .   //            'has been redirected to a file. There is '
     .   //            'not much that can be done about this if it '
     .   //            'happens. We do not try to determine whether '
     .   //            'standard output has been redirected, so be '
     .   //            'sure that there are sufficient resources '
     .   //            'available for the operation being performed.' )
         CALL SIGERR ( 'SPICE(WRITEFAILED)'                           )
         CALL CHKOUT ( 'PROMPT'                                       )
         RETURN
 
      END IF
 
C
C     Now that we've written out the prompt and there was no error, we
C     can read in the response.
C
      READ  (*,FMT='(A)', IOSTAT=IOSTAT ) STRING
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL CHKIN ( 'PROMPT' )
         CALL SETMSG ( 'An error occurred while attempting to '
     .   //            'retrieve a reply to the prompt "#".  A '
     .   //            'possible cause is that you have '
     .   //            'exhausted the input buffer while '
     .   //            'attempting to type your response.  It '
     .   //            'may help if you limit your response to # '
     .   //            'or fewer characters. '                      )
         CALL ERRCH  ( '#', PRMPT                                   )
         CALL ERRINT ( '#', MIN(LEN(STRING),131)                    )
         CALL SIGERR ( 'SPICE(READFAILED)'                          )
         CALL CHKOUT ( 'PROMPT'                                     )
         RETURN
 
      END IF
 
      RETURN
      END
