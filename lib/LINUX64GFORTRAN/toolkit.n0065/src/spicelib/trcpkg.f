C$Procedure      TRCPKG ( Trace package )
 
      SUBROUTINE TRCPKG ( DEPTH, INDEX, MODULE, TRACE, NAME )
 
C$ Abstract
C
C     Maintain a trace of subroutine calls for error messages.
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
 
      INTEGER               DEPTH
      INTEGER               INDEX
      CHARACTER*(*)         MODULE
      CHARACTER*(*)         TRACE
      CHARACTER*(*)         NAME
 
      INTEGER               FILEN
      PARAMETER           ( FILEN  = 255 )
 
      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN =  32 )
 
      INTEGER               MAXMOD
      PARAMETER           ( MAXMOD = 100 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  ENTRY
C     --------  ---  --------------------------------------------------
C
C     DEPTH      O   TRCDEP
C     DEPTH      O   TRCMXD
C     INDEX      I   TRCNAM
C     NAME       O   TRCNAM
C     MODULE     I   CHKIN, CHKOUT
C     TRACE      O   QCKTRC
C
C     FILEN      P
C     NAMLEN     P
C     MAXMOD     P
C
C$ Detailed_Input
C
C     See the ENTRY points for discussions of their arguments.
C
C$ Detailed_Output
C
C     See the ENTRY points for discussions of their arguments.
C
C$ Parameters
C
C     FILEN          is the maximum length of a file name.
C
C     NAMLEN         is the maximum length of the significant
C                    portion of a module name.
C
C     MAXMOD         is the maximum storage depth for names in the
C                    traceback stack.
C
C$ Exceptions
C
C     1)  If TRCPKG is called directly, the error SPICE(BOGUSENTRY) is
C         signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The entry points declared in this routine are:
C
C     CHKIN
C     CHKOUT
C     TRCDEP
C     TRCMXD
C     TRCNAM
C     QCKTRC
C     FREEZE
C     TRCOFF
C
C     This routine serves as an umbrella that allows the entry
C     points to share data.  TRCPKG should never be called directly.
C
C     See the subroutine ERRACT for descriptions of the error actions
C     and codes.
C
C$ Examples
C
C     See the entry points CHKIN, CHKOUT, TRCDEP, TRCMXD, TRCNAM,
C     QCKTRC, FREEZE, and TRCOFF for examples.
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
C     K.R. Gehringer  (JPL)
C     H.A. Neilan     (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.26.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    SPICELIB Version 4.25.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    SPICELIB Version 4.24.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    SPICELIB Version 4.23.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    SPICELIB Version 4.22.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GCC_C.
C
C-    SPICELIB Version 4.21.0, 29-JUL-2013 (BVS)
C
C        Changed logic in the CHKIN and CHKOUT entries to check if the
C        first character of the input value is not a space and, if so,
C        bypass the call to FRSTNB. This change speeds up the execution
C        by ~20%.
C
C-    SPICELIB Version 4.20.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    SPICELIB Version 4.19.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-CC_C.
C
C-    SPICELIB Version 4.18.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
C
C-    SPICELIB Version 4.17.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-    SPICELIB Version 4.16.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    SPICELIB Version 4.15.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 4.14.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-64BIT-MS_C.
C
C-    SPICELIB Version 4.13.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-INTEL_C.
C
C-    SPICELIB Version 4.12.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    SPICELIB Version 4.11.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 4.10.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    SPICELIB Version 4.9.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    SPICELIB Version 4.8.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    SPICELIB Version 4.7.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-LINUX-64BIT-GCC_C.
C
C-    SPICELIB Version 4.6.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-INTEL_C.
C
C-    SPICELIB Version 4.5.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    SPICELIB Version 4.4.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-    SPICELIB Version 4.3.0, 26-OCT-2005 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-GCC_C.
C
C-    SPICELIB Version 4.2.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN_C.
C
C-    SPICELIB Version 4.1.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    SPICELIB Version 4.0.5, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 4.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 4.0.3, 24-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    SPICELIB Version 4.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 4.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 4.0.0, 07-APR-1998 (NJB)
C
C        Module was updated for the PC-LINUX platform.
C
C        Bug fix:  the previous version of entry point CHKOUT failed to
C        make a correct module name comparison when the input name
C        exceeded NAMLEN characters in length.  Now only the initial
C        NAMLEN non-blank characters (at most) of the input name are
C        used in the comparison.
C
C-    SPICELIB Version 3.0.0, 12-MAR-1996 (KRG)
C
C        The structure of this routine has completely changed. A stack,
C        implemented as an array of character strings, is now used to
C        store subroutine names that use the CHKIN and CHKOUT entry
C        points. This change simplified the individual entry points as
C        well as speeding up the process of checking in and checking
C        out.
C
C        The error action mechanism has been changed as well. GETACT
C        now uses an integer code rather than a short character
C        string to represent the error action. The entry points affected
C        by this change are: TRCDEP, TRCNAM, QCKTRC.
C
C-    SPICELIB Version 2.0.0, 11-NOV-1993 (HAN)
C
C         Module was updated to include the values for FILEN and
C         NAMLEN for the Silicon Graphics, DEC Alpha-OSF/1, and
C         NeXT platforms. The previous value of 256 for Unix
C         platforms was changed to 255.
C
C-    SPICELIB Version 1.3.0, 23-OCT-1992 (NJB)
C
C        Bug fix made to routine QCKTRC:  a section of code which
C        itself is exercised only if a bug is present inserted the
C        wrong variable into an error message.
C
C-     SPICELIB Version 1.2.0, 12-OCT-1992 (HAN)
C
C        Module was updated to include the values of the parameters
C        for the Hewlett Packard UX 9000/750 environment.
C
C-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.1.0, 18-JUN-1990 (NJB)
C
C        Added declarations for trace disabling. Re-organized
C        declarations.  Updated comments.
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
C-    SPICELIB Version 4.0.0, 07-APR-1998 (NJB)
C
C        Module was updated for the PC-LINUX platform.
C
C        Bug fix:  the previous version of entry point CHKOUT failed to
C        make a correct module name comparison when the input name
C        exceeded NAMLEN characters in length.  Now only the initial
C        NAMLEN non-blank characters (at most) of the input name are
C        used in the comparison.
C
C-    SPICELIB Version 3.0.0, 12-MAR-1996 (KRG)
C
C        The structure of this routine has completely changed. A stack,
C        implemented as an array of character strings, is now used to
C        store subroutine names that use the CHKIN and CHKOUT entry
C        points. This change simplified the individual entry points as
C        well as speeding up the process of checking in and checking
C        out.
C
C        The error action mechanism has been changed as well. GETACT
C        now uses an integer code rather than a short character
C        string to represent the error action. The entry points affected
C        by this change are: TRCDEP, TRCNAM, QCKTRC.
C
C-    SPICELIB Version 2.0.0, 11-NOV-1993 (HAN)
C
C         Module was updated to include the values for FILEN and
C         NAMLEN for the Silicon Graphics, DEC Alpha-OSF/1, and
C         NeXT platforms. The previous value of 256 for Unix
C         platforms was changed to 255.
C
C-    SPICELIB Version 1.3.0, 23-OCT-1992 (NJB)
C
C        Bug fix made to routine QCKTRC:  a section of code which
C        itself is exercised only if a bug is present inserted the
C        wrong variable into an error message.
C
C-     SPICELIB Version 1.2.0, 12-OCT-1992 (HAN)
C
C        Module was updated to include the values of the parameters
C        for the Hewlett Packard UX 9000/750 environment.
C
C        The code was also reformatted so that a utility program can
C        create the source file for a specific environment given a
C        master source file.
C
C
C-    SPICELIB Version 1.1.0, 18-JUN-1990 (NJB)
C
C        Added declarations for trace disabling. Re-organized
C        declarations.  Updated comments to reflect inclusion
C        of the new entry point TRCOFF.  Also updated the header
C        to make the style more parallel to other SPICELIB
C        umbrella routines.  Updated the description line and
C        abstract, in particular.
C
C-    Beta Version 1.0.1, 08-FEB-1989 (NJB)
C
C        Warnings added to discourage use of this routine.
C
C-&
 
C
C     SPICELIB functions:
C
      INTEGER               FRSTNB
      INTEGER               RTRIM
 
      LOGICAL               FAILED
C
C     Local parameters
C
C     This is the length for a local temporary string used to help
C     format error messages. It and the character string are only
C     present to avoid real or potential problems with pedantic
C     FORTRAN compilers. 80 characters should be more than sufficient
C     to contain a module name.
C
      INTEGER               TMPLEN
      PARAMETER           ( TMPLEN = 80 )
C
C     The integer mnemonic for the RETURN error action.
C
      INTEGER               IRETRN
      PARAMETER           ( IRETRN = 3 )
 
C
C     Local Variables:
C
      CHARACTER*(FILEN)     DEVICE
      CHARACTER*(11)        STRING
      CHARACTER*(TMPLEN)    TMPNAM
 
      INTEGER               ACTION
      INTEGER               FIRST
      INTEGER               I
      INTEGER               L
 
C
C     Saved variables:
C
      CHARACTER*(NAMLEN)    STACK (MAXMOD)
      CHARACTER*(NAMLEN)    FROZEN(MAXMOD)
 
      INTEGER               FRZCNT
      INTEGER               FRZOVR
      INTEGER               MAXDEP
      INTEGER               MODCNT
      INTEGER               OVRFLW
 
      LOGICAL               NOTRC
 
      SAVE                  NOTRC
      SAVE                  FROZEN
      SAVE                  FRZCNT
      SAVE                  FRZOVR
      SAVE                  MAXDEP
      SAVE                  MODCNT
      SAVE                  OVRFLW
      SAVE                  STACK
 
C
C     Initial values:
C
      DATA    NOTRC    / .FALSE. /
      DATA    FRZCNT   /  0     /
      DATA    FRZOVR   /  0     /
      DATA    MAXDEP   /  0     /
      DATA    MODCNT   /  0     /
      DATA    OVRFLW   /  0     /
 
C
C     Executable Code:
C
      CALL WRLINE ( 'SCREEN', 'SPICE(BOGUSENTRY)'                  )
      CALL WRLINE ( 'SCREEN', 'TRCPKG: You have called an entry '
     .//            'that performs no run-time function. '         )
 
      RETURN
 
 
C$Procedure      CHKIN ( Module Check In )
 
      ENTRY CHKIN ( MODULE )
 
C$ Abstract
C
C     Inform the SPICELIB error handling mechanism of entry into a
C     routine.
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
C     CHARACTER*(*)          MODULE
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  ---------------------------------------------------
C     MODULE     I   The name of the calling routine.
C     FILEN      P   Maximum length of file name.
C
C$ Detailed_Input
C
C     MODULE         is the name of the routine calling CHKIN.  The
C                    named routine is supposed to be `checking in'
C                    when it calls CHKIN; that is, the call should be
C                    the first executable statement following the
C                    reference to the function RETURN (which should be
C                    the first executable statement).
C
C                    Only the first NAMLEN non-blank characters in
C                    a module name are stored for use in a traceback
C                    by this subroutine.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     FILEN          is the maximum file name length that can be
C                    accommodated by this routine.
C
C$ Exceptions
C
C     CHKIN does not signal errors; rather it writes error messages,
C     so as to avoid recursion.
C
C
C     1)  If the traceback storage area overflows, the short error
C         message SPICE(TRACEBACKOVERFLOW) is written to the error
C         output device.
C
C     2)  If the input argument MODULE is blank, the short error message
C         SPICE(BLANKMODULENAME) is written to the error output device.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is part of the SPICELIB error handling mechanism.
C
C     Conceptually, the effect of this routine is to `push' the
C     supplied module name onto a stack.  The routine CHKOUT performs
C     the inverse, or `pop', operation.
C
C     Every routine that participates in the traceback scheme should
C     have a call to CHKIN as the second executable statement.  The
C     first executable statements should be:
C
C        IF ( RETURN() ) THEN
C           RETURN
C        ELSE
C           CALL CHKIN ( module )
C        END IF
C
C     Here module is the name of the routine in which this code appears.
C
C     The line of code preceding the END or any RETURN statement should
C     be
C
C         CALL CHKOUT ( module )
C
C
C     All SPICELIB routines should call CHKIN and CHKOUT, unless they
C     are classified as `error free'.  Programs linked with SPICELIB
C     may also use CHKIN and CHKOUT.
C
C     Routines that don't call CHKIN and CHKOUT won't appear in the
C     traceback.
C
C     All routines that call CHKIN must also call CHKOUT, or else the
C     trace mechanism will become very confused.
C
C     It is possible to disable check-ins (and check-outs) by calling
C     the entry point TRCOFF.  CHKIN and CHKOUT will return immediately
C     upon entry after TRCOFF has been called.  It is not possible to
C     re-enable check-ins and check-outs after calling TRCOFF. Routines
C     that don't call CHKIN and CHKOUT won't appear in the traceback.
C
C$ Examples
C
C     See `Particulars' for an example of how to call this routine.
C
C$ Restrictions
C
C     Routines that call this routine must call CHKOUT immediately
C     prior to any RETURN or END statement.
C
C     Module names are assumed to have no embedded blanks.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer  (JPL)
C     N.J. Bachman    (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.1.0, 29-JUL-2013 (BVS)
C
C        Changed logic to check if the first character of the input
C        value is not a space and, if so, bypass the call to FRSTNB.
C        This change speeds up the execution by ~20%.
C
C-    SPICELIB Version 4.0.5, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 4.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 4.0.3, 24-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    SPICELIB Version 4.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 4.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 4.0.0, 07-APR-1998 (NJB)
C
C        Module was updated for the PC-LINUX platform.
C
C        Bug fix:  the previous version of entry point CHKOUT failed to
C        make a correct module name comparison when the input name
C        exceeded NAMLEN characters in length.  Now only the initial
C        NAMLEN non-blank characters (at most) of the input name are
C        used in the comparison.
C
C-    SPICELIB Version 3.0.0, 12-MAR-1996 (KRG)
C
C        The structure of this routine has completely changed. A stack,
C        implemented as an array of character strings, is now used to
C        store subroutine names that use the CHKIN and CHKOUT entry
C        points. This change simplified the individual entry points as
C        well as speeding up the process of checking in and checking
C        out.
C
C        The short error dealing with embedded blanks has been removed,
C        since the new implementation is not hampered by Embedded
C        blanks.
C
C-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 2.0.0, 15-JUN-1990  (NJB)
C
C        Disabling of check-ins implemented.  Many parts of the
C        header have be re-written.  Weird spacing ameliorated.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990  (NJB)
C
C-&
 
C$ Index_Entries
C
C     module check in
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 4.0.0, 07-APR-1998 (NJB)
C
C        Module was updated for the PC-LINUX platform.
C
C        Bug fix:  the previous version of entry point CHKOUT failed to
C        make a correct module name comparison when the input name
C        exceeded NAMLEN characters in length.  Now only the initial
C        NAMLEN non-blank characters (at most) of the input name are
C        used in the comparison.
C
C-    SPICELIB Version 3.0.0, 12-MAR-1996 (KRG)
C
C        The structure of this routine has completely changed. A stack,
C        implemented as an array of character strings, is now used to
C        store subroutine names that use the CHKIN and CHKOUT entry
C        points. This change simplified the individual entry points as
C        well as speeding up the process of checking in and checking
C        out.
C
C        The short error dealing with embedded blanks has been removed,
C        since the new implementation is not hampered by Embedded
C        blanks.
C
C-    SPICELIB Version 2.0.0, 15-JUN-1990  (NJB)
C
C        Disabling of check-ins implemented.  Many parts of the
C        header have be re-written.  Weird spacing ameliorated.
C
C-    Beta Version 1.1.1, 10-FEB-1988  (NJB)
C
C        Parameter declarations documented.  Parameters section added,
C        and parameter declarations listed in `Brief I/O'.
C
C-    Beta Version 1.1.0, 27-OCT-1988  (NJB)
C
C        Cosmetic improvement to code.  Condensed a continued
C        statement into one line.
C
C-&
 
C
C     Get out immediately if tracing is disabled.
C
      IF ( NOTRC ) THEN
         RETURN
      END IF
C
C     Get the position of the first and last non-blank characters in
C     input module name, and set the length of the module name.
C
      IF ( MODULE(1:1) .NE. ' ' ) THEN
         FIRST  =  1
      ELSE
         FIRST  =  FRSTNB ( MODULE )
      END IF
 
C
C     Check to see if the module name is blank.
C
      IF ( FIRST .GT. 0  ) THEN
C
C        If there is room for the name, place it at the top of the
C        stack. If not, increment the overflow counter and signal an
C        error.
C
         IF ( MODCNT .LT. MAXMOD ) THEN
 
            MODCNT        = MODCNT + 1
            STACK(MODCNT) = MODULE ( FIRST: )
 
         ELSE
 
            OVRFLW = OVRFLW + 1
 
            CALL GETDEV ( DEVICE )
 
            CALL WRLINE ( DEVICE,  'SPICE(TRACEBACKOVERFLOW)' )
            CALL WRLINE ( DEVICE,  'CHKIN:  The trace storage is '
     .      //            'completely full.  No further module '
     .      //            'names can be added.' )
 
         END IF
C
C        Keep track of the maximum depth encountered.
C
         IF ( ( MODCNT + OVRFLW ) .GT. MAXDEP ) THEN
            MAXDEP = MODCNT + OVRFLW
         END IF
 
      ELSE
 
         CALL GETDEV (  DEVICE  )
 
         CALL WRLINE (  DEVICE,  'SPICE(BLANKMODULENAME)'         )
         CALL WRLINE (  DEVICE,  'CHKIN:  An attempt to check'
     .   //                      ' in was made without supplying'
     .   //                      ' a module name.'                )
 
      END IF
C
C     We're done now, so return.
C
      RETURN
 
 
C$Procedure      CHKOUT ( Module Check Out )
 
      ENTRY CHKOUT ( MODULE )
 
C$ Abstract
C
C     Inform the SPICELIB error handling mechanism of exit from a
C     routine.
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
C     CHARACTER*(*)        MODULE
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     MODULE     I   The name of the calling routine.
C     NAMLEN     P   Maximum module name length.
C     FILEN      P   Maximum file name length.
C
C$ Detailed_Input
C
C     MODULE         is the name of the routine calling CHKOUT.  The
C                    named routine is supposed to be `checking out'
C                    when it calls CHKOUT; that is, the call should be
C                    the last executable statement preceding any exit
C                    from the routine.
C
C                    Only the first NAMLEN non-blank characters in
C                    a module name are used when checking out.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     FILEN          is the maximum file name length that can be
C                    accommodated by this routine.
C
C     NAMLEN         is the maximum module name length that can be
C                    accommodated by this routine.
C
C$ Exceptions
C
C     CHKOUT does not signal errors; rather it writes error messages,
C     so as to avoid recursion.
C
C     1)  If the input module name MODULE does not match the name popped
C         from the trace stack, the short error message
C         SPICE(NAMESDONOTMATCH) is written to the error output device.
C
C     2)  If the trace stack is empty, the short error message
C         SPICE(TRACESTACKEMPTY) is written to the error output device.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is part of the SPICELIB error handling mechanism.
C
C     Conceptually, the effect of this routine is to `pop' a module
C     name from a stack.  The routine CHKIN performs the inverse, or
C     `push' operation.
C
C     Every routine that participates in the traceback scheme should
C     have a call to CHKIN as the second executable statement.
C     The first executable statements should be:
C
C        IF ( RETURN() ) THEN
C           RETURN
C        ELSE
C           CALL CHKIN ( module )
C        END IF
C
C     Here module is the name of the routine in which this code appears.
C
C     The line of code preceding the END or any RETURN statement
C     should be
C
C        CALL CHKOUT  ( module )
C
C     All SPICELIB routines should call CHKIN and CHKOUT, unless they
C     are classified as `error free'.  Programs linked with SPICELIB
C     may also use CHKIN and CHKOUT.
C
C     Routines that don't call CHKIN and CHKOUT won't appear in the
C     traceback.
C
C     All routines that call CHKIN must also call CHKOUT, or else the
C     trace mechanism will become very confused.
C
C     It is possible to disable check-ins (and check-outs) by calling
C     the entry point TRCOFF.  CHKIN and CHKOUT will return immediately
C     upon entry after TRCOFF has been called.  It is not possible to
C     re-enable check-ins and check-outs after calling TRCOFF. Routines
C     that don't call CHKIN and CHKOUT won't appear in the traceback.
C
C$ Examples
C
C     1)  Call CHKOUT before a RETURN statement:
C
C            IF ( FAILED() ) THEN
C               CALL CHKOUT ( module )
C               RETURN
C            END IF
C
C
C     2)  Call CHKOUT before an END statement:
C
C            CALL CHKOUT ( module )
C            END
C
C
C     3)  Only ONE call to CHKOUT is needed here:
C
C            CALL CHKOUT ( module )
C            RETURN
C            END
C
C$ Restrictions
C
C     Routines that call this routine must call CHKIN as the second
C     executable statement. (The first is a call to RETURN).
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     K.R. Gehringer  (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.1.0, 29-JUL-2013 (BVS)
C
C        Changed logic to check if the first character of the input
C        value is not a space and, if so, bypass the call to FRSTNB.
C        This change speeds up the execution by ~20%.
C
C-    SPICELIB Version 4.0.5, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 4.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 4.0.3, 24-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    SPICELIB Version 4.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 4.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 4.0.0, 30-OCT-1997 (NJB)
C
C        Module was updated for the PC-LINUX platform.
C
C        Bug fix:  The previous version failed to make a correct
C        module name comparison when the input name exceeded NAMLEN
C        characters in length.  Now only the initial NAMLEN non-blank
C        characters (at most) of the input name are used in the
C        comparison.
C
C-    SPICELIB Version 3.0.0, 12-MAR-1996 (KRG)
C
C        The structure of this routine has completely changed. A stack,
C        implemented as an array of character strings, is now used to
C        store subroutine names that use the CHKIN and CHKOUT entry
C        points. This change simplified the individual entry points as
C        well as speeding up the process of checking in and checking
C        out.
C
C-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 2.0.0, 15-JUN-1990 (NJB)
C
C        Disabling of check-ins implemented.  Many parts of the
C        header have be re-written.  Weird spacing ameliorated.
C        Removed a bug check.  Short error messages made more
C        specific.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     module check out
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 4.0.0, 30-OCT-1997 (NJB)
C
C        Module was updated for the PC-LINUX platform.
C
C        Bug fix:  The previous version failed to make a correct
C        module name comparison when the input name exceeded NAMLEN
C        characters in length.  Now only the initial NAMLEN non-blank
C        characters (at most) of the input name are used in the
C        comparison.
C
C-    SPICELIB Version 3.0.0, 12-MAR-1996 (KRG)
C
C        The structure of this routine has completely changed. A stack,
C        implemented as an array of character strings, is now used to
C        store subroutine names that use the CHKIN and CHKOUT entry
C        points. This change simplified the individual entry points as
C        well as speeding up the process of checking in and checking
C        out.
C
C-    SPICELIB Version 2.0.0, 15-JUN-1990 (NJB)
C
C        Disabling of check-ins implemented.  Many parts of the
C        header have be re-written.  Weird spacing ameliorated.
C        Removed a bug check.  Short error messages changed from
C        SPICE(INVALIDCHECKOUT) to SPICE(NAMESDONOTMATCH) and
C        SPICE(TRACESTACKEMPTY).
C
C-    Beta Version 1.1.1, 10-FEB-1988 (NJB)
C
C        Parameter declarations documented.  Parameters section added,
C        and parameter declarations listed in `Brief I/O'.
C
C-    Beta Version 1.1.0, 27-OCT-1988 (NJB)
C
C        Cosmetic improvement to code.  Removed a blank line
C        separating the first line of a statement from the next
C        continuation line, and condensed and re-organized
C        the statement.  Note:  the precompiler failed to properly
C        convert the original statement into standard FORTRAN.
C
C-&
 
C
C     Get out immediately if tracing is disabled.
C
      IF ( NOTRC ) THEN
         RETURN
      END IF
C
C     Check to be sure we can remove a module name from the stack,
C     i.e., that we have not overflowed.
C
      IF ( OVRFLW .EQ. 0 ) THEN
C
C        We are not in overflow mode, compare the module name on
C        the top of the stack with the module name passed to us. If
C        they differ, it's an error. Regardless, we decrement the
C        module count.
C
 
         IF ( MODCNT .GT. 0 ) THEN
C
C           Make the comparison using at most NAMLEN characters of the
C           initial non-blank sub-string of MODULE.
C
            IF ( MODULE(1:1) .NE. ' ' ) THEN
               FIRST  =  1
            ELSE
               FIRST  =  FRSTNB ( MODULE )
            END IF
 
            L      =  MIN ( LEN(MODULE), FIRST+NAMLEN-1 )
 
            IF (  STACK(MODCNT)  .NE.  MODULE( FIRST : L )  ) THEN
 
               TMPNAM = MODULE( FIRST: )
               CALL GETDEV ( DEVICE )
               CALL WRLINE ( DEVICE, 'SPICE(NAMESDONOTMATCH)'    )
               CALL WRLINE ( DEVICE, 'CHKOUT:  Caller is '
     .         //            TMPNAM(:RTRIM(TMPNAM))
     .         //            '; popped name is '
     .         //            STACK(MODCNT)(:RTRIM(STACK(MODCNT)))//'.' )
 
            END IF
 
            MODCNT = MODCNT - 1
 
         ELSE
 
            CALL GETDEV ( DEVICE )
 
            CALL WRLINE ( DEVICE, 'SPICE(TRACESTACKEMPTY)' )
            CALL WRLINE ( DEVICE, 'CHKOUT: An attempt to check'
     .      //                    ' out was made when no modules'
     .      //                    ' were checked in.' )
 
         END IF
 
      ELSE
C
C        Overflow case: just decrement the overflow count.
C
         OVRFLW = OVRFLW - 1
 
      END IF
C
C     Return to the caller.
C
      RETURN
 
 
C$Procedure      TRCDEP ( Traceback depth )
 
      ENTRY TRCDEP ( DEPTH )
 
C$ Abstract
C
C     Return the number of modules in the traceback representation.
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
C     INTEGER               DEPTH
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  ---------------------------------------------------
C
C     DEPTH      O   The number of modules in the traceback.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     DEPTH          Indicates the number of module names in the
C                    traceback representation.
C
C                    The module names represent modules in a call chain,
C                    with the first name being the top-level module,
C                    and the name with index DEPTH being the lowest
C                    level module.
C
C                    The meaning of the traceback depends on the state
C                    of the error handling mechanism.  There are two
C                    cases:
C
C                       1.  In 'RETURN' mode, when an error is
C                           signaled, the traceback at that point is
C                           saved.  TRCDEP, TRCNAM, and QCKTRC will
C                           return values pertaining to the saved
C                           traceback.
C
C                       2.  In all other modes, the traceback represents
C                           the CURRENT call chain.  TRCDEP, TRCNAM,
C                           and QCKTRC will return values pertaining to
C                           the current trace representation.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is part of the SPICELIB error handling mechanism.
C
C$ Examples
C
C     1)  You can use this routine, together with TRCNAM, to create a
C         traceback report. We might wish to create such a report when
C         we have detected an error condition (see FAILED).
C
C         In this example, we assume that the error has already been
C         detected, and that we wish to create a traceback report.  We
C         assume the existence of two user-supplied routines:
C
C            USER_TRACE_FORMAT   --   creates a traceback report in the
C                                     format preferred by the user
C
C            USER_TRACE_INIT     --   indicates that a traceback report
C                                     is to be created; it also
C                                     indicates how many module names
C                                     will be in the report
C
C            C
C            C     Get the trace depth, and retrieve that number of
C            C     module names from the traceback representation.
C            C     Call USER_TRACE_INIT to indicate that a traceback
C            C     report is to be created containing `DEPTH'
C            C     number of module names. Input each of these names,
C            C     as they are retrieved, to USER_TRACE_FORMAT.
C            C
C
C                 CALL TRCDEP           ( DEPTH )
C
C                 CALL USER_TRACE_INIT  ( DEPTH )
C
C
C                 DO INDEX = 1, DEPTH
C
C                    CALL TRCNAM     ( INDEX, MODULE )
C
C                    CALL USER_TRACE_FORMAT ( MODULE )
C
C                 END DO
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
C     K.R. Gehringer  (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.0.5, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 4.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 4.0.3, 24-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    SPICELIB Version 4.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 4.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 2.0.0, 12-MAR-1996 (KRG)
C
C        The structure of this routine has completely changed. A stack,
C        implemented as an array of character strings, is now used to
C        store subroutine names that use the CHKIN and CHKOUT entry
C        points. This change simplified the individual entry points as
C        well as speeding up the process of checking in and checking
C        out.
C
C        The error action mechanism has been changed as well. GETACT
C        now uses an integer code rather than a short character
C        string to represent the error action. The entry points affected
C        by this change are: TRCDEP, TRCNAM, QCKTRC.
C
C-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.1, 15-JUN-1990  (NJB)
C
C        Some comments updated.  Some cosmetic changes too.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990  (NJB)
C
C-&
 
C$ Index_Entries
C
C     traceback depth
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 12-MAR-1996 (KRG)
C
C        The structure of this routine has completely changed. A stack,
C        implemented as an array of character strings, is now used to
C        store subroutine names that use the CHKIN and CHKOUT entry
C        points. This change simplified the individual entry points as
C        well as speeding up the process of checking in and checking
C        out.
C
C        The error action mechanism has been changed as well. GETACT
C        now uses an integer code rather than a short character
C        string to represent the error action. The entry points affected
C        by this change are: TRCDEP, TRCNAM, QCKTRC.
C
C-    SPICELIB Version 1.0.1, 15-JUN-1990  (NJB)
C
C        Some comments updated.  Some cosmetic changes too.
C-&
 
C
C     Find the error handling mode.
C
      CALL GETACT ( ACTION )
C
C     If we're in 'RETURN' mode, and an error has occurred, we want to
C     use the frozen version of the traceback.  Otherwise, we want to
C     get the use the current module stack depth.
C
      IF  (   ( ACTION .EQ. IRETRN )  .AND.   FAILED()   )   THEN
 
         DEPTH = FRZCNT + FRZOVR
 
      ELSE
 
         DEPTH = MODCNT + OVRFLW
 
      END IF
C
C     Return to the caller.
C
      RETURN
 
 
C$Procedure      TRCMXD ( Maximum traceback depth encountered. )
 
      ENTRY TRCMXD ( DEPTH )
 
C$ Abstract
C
C     Return the maximum number of modules encountered in the
C     traceback so far.
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
C     INTEGER               DEPTH
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  ---------------------------------------------------
C
C     DEPTH      O   The maximum number of modules encountered.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     DEPTH          Indicates the maximum number of module
C                    names encountered in the traceback stack.
C                    This would be the longest call chain that
C                    occurred during the run of a program.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is part of the SPICELIB error handling mechanism.
C
C$ Examples
C
C     1)  You can use this routine to determine the length of the
C         longest sequence of subroutine calls in a program. Suppose
C         that you have a program, PROGRAM, that uses the SPICELIB
C         error handling with CHKIN and CHKOUT, and has three
C         subroutines, SUB_A, SUB_B, and SUB_C. The program and
C         subroutines have the following relationships:
C
C             PROGRAM calls SUB_A and SUB_C
C             SUB_C   calls SUB_B
C
C         If at the end of the program you were to call TRCMXD,
C
C            CALL TRCMXD ( MAXDEP )
C
C         to obtain the maximum depth reached, MAXDEP would have a
C         value of three (3), because the program checked in, SUB_C
C         checked in, and SUB_B checked in during the longest call
C         chain in the program.
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
C     K.R. Gehringer  (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.0.5, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 4.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 4.0.3, 24-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    SPICELIB Version 4.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 4.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 1.0.0, 12-MAR-1996  (KRG)
C
C-&
 
C$ Index_Entries
C
C     traceback maximum depth
C
C-&
 
C
C     It doesn't get any easier than this, simply set the maximum
C     depth and return.
C
      DEPTH = MAXDEP
 
      RETURN
 
 
C$Procedure      TRCNAM ( Get Module Name from Traceback )
 
      ENTRY TRCNAM ( INDEX, NAME )
 
C$ Abstract
C
C     Return the name of the module having the specified position in
C     the trace representation.  The first module to check in is at
C     position 1.
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
C     INTEGER               INDEX
C     CHARACTER*(*)         NAME
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C
C     INDEX      I   The position of the requested module name.
C     NAME       O   The name in the #INDEX position in the traceback.
C     FILEN      P   Maximum file name length.
C
C$ Detailed_Input
C
C     INDEX          is the position in the traceback of the requested
C                    module name.  The first module to check in is in
C                    the first position; the last to check in the
C                    position indicated by the argument, DEPTH,
C                    returned by TRCDEP.  Note that the first module to
C                    check in is at the top of the traced call chain.
C
C$ Detailed_Output
C
C     NAME           is the name of the module in the position within
C                    the traceback indicated by INDEX.
C
C                    The meaning of the traceback depends on the state
C                    of the error handling mechanism.  There are two
C                    cases:
C
C                       1.  In 'RETURN' mode, when an error is
C                           signaled, the traceback at that point is
C                           saved.  TRCDEP, TRCNAM, and QCKTRC will
C                           return values pertaining to the saved
C                           traceback.
C
C                       2.  In all other modes, the traceback represents
C                           the CURRENT call chain.  TRCDEP, TRCNAM,
C                           and QCKTRC will return values pertaining to
C                           the current trace representation.
C
C$ Parameters
C
C      FILEN   is the maximum file name length that can be
C              accommodated by this routine.
C
C$ Exceptions
C
C     Because this routine is below SIGERR in the calling hierarchy,
C     this routine can not call SIGERR in the event of an error.
C     Therefore, this routine outputs error messages, rather than
C     signaling errors.
C
C     1)  This routine detects the condition of INDEX being out of
C         range.  The short error message set in that case is
C        'SPICE(INVALIDINDEX)'.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is part of the SPICELIB error handling mechanism.
C
C$ Examples
C
C     1)  You can use this routine, together with TRCNAM, to create a
C         traceback report. We might wish to create such a report when
C         we have detected an error condition (see FAILED).
C
C         In this example, we assume that the error has already been
C         detected, and that we wish to create a traceback report.  We
C         assume the existence of two user-supplied routines:
C
C            USER_TRACE_FORMAT   --   creates a traceback report in the
C                                     format preferred by the user
C
C            USER_TRACE_INIT     --   indicates that a traceback report
C                                     is to be created; it also
C                                     indicates how many module names
C                                     will be in the report
C
C            C
C            C     Get the trace depth, and retrieve that number of
C            C     module names from the traceback representation.
C            C     Call USER_TRACE_INIT to indicate that a traceback
C            C     report is to be created containing `DEPTH'
C            C     number of module names. Input each of these names,
C            C     as they are retrieved, to USER_TRACE_FORMAT.
C            C
C
C                 CALL TRCDEP           ( DEPTH )
C
C                 CALL USER_TRACE_INIT  ( DEPTH )
C
C
C                 DO INDEX = 1, DEPTH
C
C                    CALL TRCNAM     ( INDEX, MODULE )
C
C                    CALL USER_TRACE_FORMAT ( MODULE )
C
C                 END DO
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
C     K.R. Gehringer  (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.0.5, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 4.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 4.0.3, 24-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    SPICELIB Version 4.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 4.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 2.0.0, 12-MAR-1996 (KRG)
C
C        The structure of this routine has completely changed. A stack,
C        implemented as an array of character strings, is now used to
C        store subroutine names that use the CHKIN and CHKOUT entry
C        points. This change simplified the individual entry points as
C        well as speeding up the process of checking in and checking
C        out.
C
C        The exception:
C
C           2)  If INDEX is in range, but no module name is found
C               at the indicated location in the trace representation,
C               the error message 'SPICE(INVALIDINDEX)' is set.
C
C        has been removed. The only way in which a module name cannot
C        be found for a specified index is if we have overflowed the
C        stack storage for module names, and in this case we return the
C        message '<Name Not Available>'.
C
C        The error action mechanism has been changed as well. GETACT
C        now uses an integer code rather than a short character
C        string to represent the error action. The entry points affected
C        by this change are: TRCDEP, TRCNAM, QCKTRC.
C
C-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.1.0, 15-JUN-1990  (NJB)
C
C        Error messages streamlined. Some comments updated.
C        Some cosmetic changes too.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990  (NJB)
C
C-&
 
C$ Index_Entries
C
C     get module name from traceback
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 12-MAR-1996 (KRG)
C
C        The structure of this routine has completely changed. A stack,
C        implemented as an array of character strings, is now used to
C        store subroutine names that use the CHKIN and CHKOUT entry
C        points. This change simplified the individual entry points as
C        well as speeding up the process of checking in and checking
C        out.
C
C        The exception:
C
C           2)  If INDEX is in range, but no module name is found
C               at the indicated location in the trace representation,
C               the error message 'SPICE(INVALIDINDEX)' is set.
C
C        has been removed. The only way in which a module name cannot
C        be found for a specified index is if we have overflowed the
C        stack storage for module names, and in this case we return the
C        message '<Name Not Available>'.
C
C        The error action mechanism has been changed as well. GETACT
C        now uses an integer code rather than a short character
C        string to represent the error action. The entry points affected
C        by this change are: TRCDEP, TRCNAM, QCKTRC.
C
C-    SPICELIB Version 1.1.0, 15-JUN-1990  (NJB)
C
C        Error messages streamlined. Some comments updated.
C        Some cosmetic changes too.
C
C-    Beta Version 1.1.1, 10-FEB-1988  (NJB)
C
C        Parameter declarations documented.  Parameters section added,
C        and parameter declarations listed in `Brief I/O'.
C
C-    Beta Version 1.1.0, 27-OCT-1988  (NJB)
C
C        Added test for failure to remove name from trace
C        representation.  If LOC equals 0 on return from
C        NTHWD, the error SPICE(INVALIDINDEX) is reported.
C        SIGERR is not called; that would be overly recursive.
C
C        Cosmetic changes to header and code were made.  Indentation
C        of some header items was changed, and some blank lines
C        were removed from the code.
C-&
 
C
C     Get the error handling mode.
C
      CALL GETACT ( ACTION )
C
C     If we're in 'RETURN' mode, and an error has occurred, we want to
C     use the frozen version of the traceback.  Otherwise, we want to
C     get the module name from the current traceback.
C
      IF ( ( ACTION .EQ. IRETRN ) .AND. FAILED() ) THEN
C
C        Check the input index. It must be positive and less than the
C        current stack depth.
C
         IF ( ( INDEX .LE. 0 ) .OR. ( INDEX .GT. FRZCNT+FRZOVR ) ) THEN
C
C           Invalid index...we output the error messages directly
C           in this case:
C
            CALL  GETDEV ( DEVICE )
            CALL  WRLINE ( DEVICE,  'SPICE(INVALIDINDEX)' )
            CALL  INTSTR ( INDEX, STRING )
            CALL  WRLINE ( DEVICE, 'TRCNAM: An invalid index was '    //
     .                     'input.  The value was: '                  //
     .                     STRING( :RTRIM(STRING) )//'.'  )
            RETURN
 
         END IF
C
C        We're OK, so get the name or not available.
C
         IF ( INDEX .LE. MAXMOD )  THEN
            NAME = FROZEN(INDEX)
         ELSE
            NAME = '<Overflow No Name Available>'
         END IF
 
      ELSE
C
C        Otherwise, use current traceback:
C
C        Check the input index. It must be positive and less than the
C        current stack depth.
C
         IF ( ( INDEX .LE. 0 ) .OR. ( INDEX .GT. MODCNT+OVRFLW ) ) THEN
C
C           Invalid index...we output the error messages directly
C           in this case:
C
            CALL  GETDEV ( DEVICE )
            CALL  WRLINE ( DEVICE,  'SPICE(INVALIDINDEX)' )
            CALL  INTSTR ( INDEX, STRING )
            CALL  WRLINE ( DEVICE, 'TRCNAM: An invalid index was '    //
     .                     'input.  The value was: '                  //
     .                     STRING( :RTRIM(STRING) )//'.'  )
            RETURN
 
         END IF
C
C        We're OK, so get the name or name not available.
C
         IF ( INDEX .LE. MAXMOD )  THEN
            NAME = STACK(INDEX)
         ELSE
            NAME = '<Overflow No Name Available>'
         END IF
 
      END IF
 
      RETURN
 
 
C$Procedure      QCKTRC ( Get Quick Traceback )
 
      ENTRY QCKTRC ( TRACE )
 
C$ Abstract
C
C     Return a string containing a traceback.
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
C     CHARACTER*(*)         TRACE
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C
C     TRACE      O   A traceback report.
C     NAMLEN     P   Maximum module name length.
C     FILEN      P   Maximum file name length.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     TRACE          is a list of module names, delimited by the
C                    string, ' -->'.  An example would be
C
C                       'SPUD -->SPAM -->FOOBAR'.
C
C                    In general, the meaning of the trace is as
C                    follows:
C
C                    The first name in the list is the name of the first
C                    module to check in (that hasn't yet checked out).
C                    The last name is the name of the module at the end
C                    of the call chain; this is the last module that
C                    checked in.
C
C                    The meaning of the traceback depends on the state
C                    of the error handling mechanism.  There are two
C                    cases:
C
C                       1.  In 'RETURN' mode, when an error is
C                           signaled, the traceback at that point is
C                           saved.  TRCDEP, TRCNAM, and QCKTRC will
C                           return values pertaining to the saved
C                           traceback.
C
C                       2.  In all other modes, the traceback represents
C                           the CURRENT call chain.  TRCDEP, TRCNAM,
C                           and QCKTRC will return values pertaining to
C                           the current trace representation.
C
C                    Any module names exceeding NAMLEN characters in
C                    length are truncated on the right.
C
C$ Parameters
C
C     FILEN          is the maximum file name length that can be
C                    accommodated by this routine.
C
C     NAMLEN         is the maximum module name length that can be
C                    accommodated by this routine.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is part of the SPICELIB error handling mechanism.
C
C$ Examples
C
C     1)  Here's an example of how to use this routine:
C
C           C
C           C     We call RDTEXT and test for an error condition.
C           C     If an error occurred, we get the traceback and
C           C     long error message and output them using the
C           C     user-defined routine, USER_ERROR.
C           C
C
C                 CALL RDTEXT ( FILE, LINE, EOF )
C
C                 IF ( FAILED() ) THEN
C
C                    CALL QCKTRC     ( TRACE )
C                    CALL USER_ERROR ( TRACE )
C
C                    CALL GETMSG     ( 'LONG', MSG )
C                    CALL USER_ERROR (         MSG )
C
C                 END IF
C
C$ Restrictions
C
C     It is assumed no module names exceed NAMLEN characters in length.
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
C-    SPICELIB Version 4.0.5, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 4.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 4.0.3, 24-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    SPICELIB Version 4.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 4.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 2.0.0, 12-MAR-1996 (KRG)
C
C        The structure of this routine has completely changed. A stack,
C        implemented as an array of character strings, is now used to
C        store subroutine names that use the CHKIN and CHKOUT entry
C        points. This change simplified the individual entry points as
C        well as speeding up the process of checking in and checking
C        out.
C
C        The error action mechanism has been changed as well. GETACT
C        now uses an integer code rather than a short character
C        string to represent the error action. The entry points affected
C        by this change are: TRCDEP, TRCNAM, QCKTRC.
C
C-    SPICELIB Version 1.2.0, 23-OCT-1992 (NJB)
C
C        Bug fix made to routine QCKTRC:  a section of code which
C        itself is exercised only if a bug is present inserted the
C        wrong variable into an error message.
C
C-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.1.0, 15-JUN-1990  (NJB)
C
C        Error messages streamlined. Some comments updated.
C        Some cosmetic changes too.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990  (NJB)
C
C-&
 
C$ Index_Entries
C
C     get quick traceback
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 12-MAR-1996 (KRG)
C
C        The structure of this routine has completely changed. A stack,
C        implemented as an array of character strings, is now used to
C        store subroutine names that use the CHKIN and CHKOUT entry
C        points. This change simplified the individual entry points as
C        well as speeding up the process of checking in and checking
C        out.
C
C        The error action mechanism has been changed as well. GETACT
C        now uses an integer code rather than a short character
C        string to represent the error action. The entry points affected
C        by this change are: TRCDEP, TRCNAM, QCKTRC.
C
C-    SPICELIB Version 1.2.0, 23-OCT-1992 (NJB)
C
C        Bug fix made to routine QCKTRC:  a section of code which
C        itself is exercised only if a bug is present inserted the
C        wrong variable into an error message.  The variable in
C        question was the input argument INDEX; the correct variable
C        to insert in the message is the local variable POS.
C
C-    SPICELIB Version 1.1.0, 15-JUN-1990  (NJB)
C
C        Error messages streamlined. Some comments updated.
C        Some cosmetic changes too. Use of SUFFIX made more
C        rational.
C
C-    Beta Version 1.1.1, 10-FEB-1988  (NJB)
C
C        Parameter declarations documented.  Parameters section added,
C        and parameter declarations listed in `Brief I/O'.
C
C-    Beta Version 1.1.0, 06-OCT-1988  (NJB)
C
C        Added test for failure to remove name from trace
C        representation.  If LOC equals 0 on return from
C        NTHWD, the error SPICE(INVALIDINDEX) is reported.
C        SIGERR is not called; that would be overly recursive.
C
C        Also, some cosmetic changes to code were made.  Some
C        unnecessary continuation lines were removed.
C-&
 
C
C     Be sure that the output string is empty.
C
      TRACE = ' '
C
C     Get the error handling mode.
C
      CALL GETACT ( ACTION )
C
C     If we're in 'RETURN' mode, and an error has occurred, we want to
C     use the frozen version of the traceback.  Otherwise, we want to
C     use the current traceback.
C
      IF  (  ( ACTION .EQ. IRETRN )  .AND.  FAILED()  ) THEN
 
         DO I = 1, FRZCNT
 
            IF ( I .GT. 1 ) THEN
               CALL SUFFIX ( '-->',  1, TRACE )
               CALL SUFFIX ( FROZEN(I), 1, TRACE )
            ELSE
               CALL SUFFIX ( FROZEN(I), 0, TRACE )
            END IF
 
         END DO
 
         IF ( FRZOVR .GT. 0 ) THEN
 
            CALL SUFFIX ( '-->',   1, TRACE )
            IF ( FRZOVR .GT. 1 ) THEN
               CALL INTSTR ( FRZOVR, STRING )
               CALL SUFFIX ( '<', 1, TRACE )
               CALL SUFFIX ( STRING,  0, TRACE )
               CALL SUFFIX ( 'Names Overflowed>', 1, TRACE )
            ELSE
               CALL SUFFIX ( '<One Name Overflowed>', 1, TRACE )
            END IF
 
         END IF
 
      ELSE
 
         DO I = 1, MODCNT
 
            IF ( I .GT. 1 ) THEN
               CALL SUFFIX ( '-->',  1, TRACE )
               CALL SUFFIX ( STACK(I), 1, TRACE )
            ELSE
               CALL SUFFIX ( STACK(I), 0, TRACE )
            END IF
 
         END DO
 
         IF ( OVRFLW .GT. 0 ) THEN
 
            CALL SUFFIX ( '-->',  1, TRACE )
            IF ( OVRFLW .GT. 1 ) THEN
               CALL INTSTR ( OVRFLW, STRING )
               CALL SUFFIX ( '<', 1, TRACE )
               CALL SUFFIX ( STRING,  0, TRACE )
               CALL SUFFIX ( 'Names Overflowed>', 1, TRACE )
            ELSE
               CALL SUFFIX ( '<One Name Overflowed>', 1, TRACE )
            END IF
 
         END IF
 
      END IF
 
      RETURN
 
 
C$Procedure   FREEZE   ( Get frozen copy of traceback )
 
      ENTRY FREEZE
 
C$ Abstract
C
C     Make a copy of the current traceback.  This copy is frozen, i.e.
C     unchanged, until the next call to FREEZE. DO NOT CALL THIS
C     ROUTINE.
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
C
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
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     DO NOT CALL THIS ROUTINE.
C
C     When the error response action is 'RETURN', and an error is
C     signaled, a copy of the traceback is saved for later retrieval
C     by the application program.  This is called the `frozen' version
C     of the traceback.  FREEZE is used to create this frozen version.
C
C     This routine is called by the SPICELIB routines SIGERR and RESET.
C
C$ Examples
C
C     1)
C         C
C         C     Create a frozen traceback:
C         C
C               CALL FREEZE
C
C$ Restrictions
C
C     For SPICELIB error handling only.
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
C-    SPICELIB Version 4.0.5, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 4.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 4.0.3, 24-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    SPICELIB Version 4.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 4.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 2.0.0, 12-MAR-1996 (KRG)
C
C        The structure of this routine has completely changed. A stack,
C        implemented as an array of character strings, is now used to
C        store subroutine names that use the CHKIN and CHKOUT entry
C        points. This change simplified the individual entry points as
C        well as speeding up the process of checking in and checking
C        out.
C
C-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.1, 15-JUN-1990  (NJB)
C
C       Some comments changed. Cosmetic changes too.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990  (NJB)
C
C-&
 
C$ Index_Entries
C
C     None.
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 12-MAR-1996 (KRG)
C
C        The structure of this routine has completely changed. A stack,
C        implemented as an array of character strings, is now used to
C        store subroutine names that use the CHKIN and CHKOUT entry
C        points. This change simplified the individual entry points as
C        well as speeding up the process of checking in and checking
C        out.
C
C-    SPICELIB Version 1.0.1, 15-JUN-1990  (NJB)
C
C       Some comments changed. Cosmetic changes too.
C
C-    Beta Version 1.0.1, 08-FEB-1989 (NJB)
C
C        Warnings added to discourage use of this routine in
C        non-error-handling code.
C
C-&
 
C
C     Create a frozen version of the traceback. To do this, we move
C     the current traceback state into the freezer..
C
 
      FRZCNT = MODCNT
      FRZOVR = OVRFLW
 
      DO I = 1, MODCNT
         FROZEN(I) = STACK(I)
      END DO
 
      RETURN
 
 
C$Procedure  TRCOFF  ( Turn tracing off )
 
      ENTRY TRCOFF
 
C$ Abstract
C
C     Disable tracing.
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
C
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
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine disables tracing.  Checking in or out does not modify
C     the current traceback any further after TRCOFF is called. The
C     routines TRCNAM, TRCDEP, and QCKTRC will return information
C     based on the traceback at the point where TRCOFF is called.
C
C     Once tracing has been disabled, it cannot be re-enabled.
C
C     Additionally, TRCOFF blanks out the existing trace, since the
C     trace will usually be invalid at the time an error is signaled.
C     The frozen copy of the trace, if there is one, is not modified.
C
C$ Examples
C
C     1)    C
C           C     Program initialization:
C           C
C                      .
C                      .
C                      .
C           C
C           C     We disable tracing to enhance speed:
C           C
C                 CALL TRCOFF
C           C
C           C     More initialization code:
C           C
C                      .
C                      .
C                      .
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
C     N.J. Bachman   (JPL)
C     K.R. Gehringer  (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.0.5, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 4.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 4.0.3, 24-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    SPICELIB Version 4.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 4.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 2.0.0, 12-MAR-1996 (KRG)
C
C        The structure of this routine has completely changed. A stack,
C        implemented as an array of character strings, is now used to
C        store subroutine names that use the CHKIN and CHKOUT entry
C        points. This change simplified the individual entry points as
C        well as speeding up the process of checking in and checking
C        out.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 11-JUL-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     turn tracing off
C
C-&
 
C
C     Indicate that tracing is disabled:
C
      NOTRC = .TRUE.
 
C
C     The stack depth becomes 0 (it will be referenced if TRCDEP is
C     called). The overflow count set to 0 as well, for consistency;
C     it will not be referenced again after this code is executed.
C
      MODCNT = 0
      OVRFLW = 0
 
      RETURN
 
      END
