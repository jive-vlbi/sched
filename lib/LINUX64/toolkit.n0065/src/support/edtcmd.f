C$Procedure     EDTCMD ( Edit a file using a specified text editor )
 
      SUBROUTINE EDTCMD ( CMD, FILE )
 
C$ Abstract
C
C     Edit a file using a specified editor.
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
C     SYSTEM
C     UTILITY
C
C$ Declarations
 
      CHARACTER*(*)         CMD
      CHARACTER*(*)         FILE
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     CMD        I   Command string used to invoke editor.
C     FILE       I   Name of file to edit.
C
C$ Detailed_Input
C
C     CMD            is a character string containing the command
C                    used to invoke a text editor available on the
C                    system under which the calling program is running.
C                    This routine will invoke the specified editor
C                    using FILE as the target file to edit.  The name
C                    of the file to be edited is not included in the
C                    command; this name is input as a separate argument.
C
C                    Case sensitivity of CMD varies with the system on
C                    which the calling program is run.
C
C                    Trailing white space in CMD is not significant.
C
C
C     FILE           is the name of a file that is to be edited.  FILE
C                    need not exist at the time this routine is called.
C
C                    Case sensitivity of FILE varies with the system on
C                    which the calling program is run.
C
C                    Trailing white space in FILE is not significant.
C
C$ Detailed_Output
C
C     None.  See $Particulars for further information on the action of
C     this routine.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the specified edit command fails, the error will be
C         diagnosed by routines called by this routine.
C
C     2)  If the editing session started by this routine is terminated
C         abnormally, the effect on the operation of the calling program
C         is unspecified.
C
C$ Files
C
C     See $Particulars.
C
C$ Particulars
C
C     This routine should be used with caution; calls to this routine
C     may have unintended side effects on the operation of the calling
C     program.  A solid understanding of the global operation of the
C     calling program is a prerequisite for programmers wishing to
C     use this routine.
C
C     The input argument FILE should unambiguously designate a file
C     that can be edited by the specified editor on the system under
C     which the calling program is being run.  The calling program
C     should have read or write privileges consistent with the intended
C     mode of access to FILE.
C
C     This routine may fail to recover in a predictable fashion from
C     abnormal termination of an editing session.
C
C$ Examples
C
C     1)   On a VAX/VMS system, the EDT editor could be invoked by
C          the calls
C
C             CALL EDTCMD ( 'EDIT/EDT',  FILE  )
C
C          or
C
C             CALL EDTCMD ( 'EDIT/EDT/COMMAND = <command file>',  FILE )
C
C
C     2)   On a Unix system, the emacs editor could be invoked
C          (normally) by the calls
C
C              CALL EDTCMD ( 'emacs', FILE )
C
C          or
C
C              CALL EDTCMD ( '/usr/bin/emacs', FILE )
C
C
C$ Restrictions
C
C     1)   The means by which this routine invokes an editor are system-
C          dependent; invoking the editor may have side effects that
C          affect the operation of the calling program.  For example,
C          on Unix systems, this routine may start a new shell in which
C          to run the editor; starting a new shell may interfere with
C          any sequential file I/O in progress at the time the shell is
C          started.
C
C          See the code for implementation details.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    Beta Version 2.27.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    Beta Version 2.26.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    Beta Version 2.25.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    Beta Version 2.24.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    Beta Version 2.23.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GCC_C.
C
C-    Beta Version 2.22.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    Beta Version 2.21.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-CC_C.
C
C-    Beta Version 2.20.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
C
C-    Beta Version 2.19.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-    Beta Version 2.18.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    Beta Version 2.17.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    Beta Version 2.16.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-64BIT-MS_C.
C
C-    Beta Version 2.15.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-INTEL_C.
C
C-    Beta Version 2.14.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    Beta Version 2.13.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    Beta Version 2.12.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    Beta Version 2.11.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    Beta Version 2.10.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    Beta Version 2.9.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-LINUX-64BIT-GCC_C.
C
C-    Beta Version 2.8.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-INTEL_C.
C
C-    Beta Version 2.7.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    Beta Version 2.6.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-    Beta Version 2.5.0, 26-OCT-2005 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-GCC_C.
C
C-    Beta Version 2.4.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN_C.
C
C-    Beta Version 2.3.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    Beta Version 2.2.5, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    Beta Version 2.2.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    Beta Version 2.2.3, 20-SEP-1999 (NJB)
C
C        CSPICE and PC-LINUX environment lines were added.  Some
C        typos were corrected.
C
C-    Beta Version 2.2.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    Beta Version 2.2.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    Beta Version 2.2.0, 12-AUG-1996 (WLT)
C
C        Added DEC-OSF1 to the list of supported environments
C
C-    Beta Version 2.1.0, 10-JAN-1996 (WLT)
C
C        Added PC-LAHEY to the list of supported environments.
C
C-    Beta Version 2.0.0, 16-JUN-1995 (WLT)(HAN)
C
C        Created master file from collection of machine dependent
C        routines.  Copyright notice added.
C
C-    Beta Version 1.0.0, 16-AUG-1994 (NJB)
C
C-&
 
C$ Index_Entries
C
C     invoke a text editor within a program
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               RTRIM
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               CMDLEN
      PARAMETER           ( CMDLEN = 255 )
 
C
C     Local variables
C
      CHARACTER*(CMDLEN)    LOCCMD
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EDTCMD' )
      END IF
 
C
C
C     SUN:
C
C        Computer:         Sun SPARCstation 2
C        Operating System: Sun OS 4.1.2
C        Fortran:          Sun FORTRAN 1.3.1
C
C     HP:
C
C        Computer:         HP 715/50
C        Operating System: HP-UX 9.01
C        Fortran:          HP-UX.09.00.24
C                          HP-UX FORTRAN/9000
C                             Series 700 B2408A.09.00
C                             Series 800 B2409B.09.00
C
C     NEXT:
C
C        Computer:         NeXT
C        Operating System: NeXtStep 3.0, 3.2
C        Fortran:          Absoft Fortran V3.2
C        NEXT (NeXT 3.0, Absoft Fortran 3.2):
C
C        Computer:         Alpha/OSF1
C        Operating System: OSF1 V3.2
C        Fortran:        : DEC Fortran Compiler Driver V3.5-053
C
C
C     Build the edit command to be passed to the system.
C
      LOCCMD = CMD
      CALL SUFFIX ( FILE, 1, LOCCMD )
 
C
C     For safety, append a null to the command.  If the user has
C     linked to a C version of the "system" function, this may
C     save us much grief.
C
      CALL SUFFIX ( CHAR(0), 0, LOCCMD )
 
C
C     Invoke the editor.
C
      CALL EXESYS (   LOCCMD( :RTRIM(LOCCMD) )   )
 
      CALL CHKOUT ( 'EDTCMD' )
      RETURN
      END
