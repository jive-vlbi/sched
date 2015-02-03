C$Procedure   EXESYS  ( Execute system command )
 
      SUBROUTINE EXESYS ( CMD )
      IMPLICIT NONE
 
C$ Abstract
C
C     Execute an operating system command.
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
 
      CHARACTER*(*)         CMD
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     CMD        I   Command to be executed.
C
C$ Detailed_Input
C
C     CMD            is a character string containing a command
C                    recognized by the command line interpreter of
C                    the operating system.  The significance of case
C                    in CMD is system-dependent.  Trailing white space
C                    is not significant.
C
C$ Detailed_Output
C
C     None.   See $Particulars for a description of the action of this
C     routine.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input command is not executed successfully, and if
C         this routine is able to detect the failure, the error
C         SPICE(SYSTEMCALLFAILED) is signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Most popular operating systems provide a Fortran-callable
C     interface that allows a program to execute an operating system
C     command by passing the command, in the form of a string, to the
C     operating system's command interpreter. This routine encapulates
C     most of the system-dependent code required to execute operating
C     system commands in this manner.  The input commands are of course
C     system-dependent.
C
C     Side effects of this routine vary from system to system.
C     See $Restrictions for more information.
C
C     Error checking capabilities also vary from system to system; this
C     routine does the best it can to diagnose errors resulting from
C     the attempt to execute the input command.
C
C$ Examples
C
C     1)  Unix:  copy the file spud.dat to the file spam.dat.  Test
C         whether the copy command was executed successfully.
C
C         For safety, we recommend appending a null character to the
C         command.
C
C            CALL EXESYS (  'cp spud.dat spam.dat'//CHAR(O)  )
C
C            IF ( FAILED() ) THEN
C
C               [process error condition]
C
C            END IF
C
C
C     2)  VMS:  same action as in example (1):
C
C            CALL EXESYS ( 'COPY  SPUD.DAT;  SPAM.DAT;' )
C
C            IF ( FAILED() ) THEN
C
C               [process error condition]
C
C            END IF
C
C$ Restrictions
C
C     1)  This routine should be used with caution; executing a system
C         command from within your program may have surprising side
C         effects.  For example, the Sun Fortran Reference Manual [1]
C         gives this warning:
C
C               *System* flushes all open files.  For output files,
C               the buffer is flushed to the actual file.  For input
C               files, the position of the pointer is unpredictable.
C
C     2)  Under Sun Fortran
C
C            -- The shell used to execute the command is determined by
C               the environment variable SHELL.
C
C            -- The command string cannot exceed 1024 characters in
C               length.
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
C-    Beta Version 2.26.0, 02-APR-2014 (BVS)
C
C        Changed PC-CYGWIN-GFORTRAN and PC-CYGWIN-64BIT-GFORTRAN
C        to be like other GFORTRAN environments.
C
C-    Beta Version 2.25.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    Beta Version 2.24.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    Beta Version 2.23.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    Beta Version 2.22.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    Beta Version 2.21.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GCC_C.
C
C-    Beta Version 2.20.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    Beta Version 2.19.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-CC_C.
C
C-    Beta Version 2.18.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
C
C-    Beta Version 2.17.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-    Beta Version 2.16.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    Beta Version 2.15.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    Beta Version 2.14.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-64BIT-MS_C.
C
C-    Beta Version 2.13.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-INTEL_C.
C
C-    Beta Version 2.12.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    Beta Version 2.11.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    Beta Version 2.10.0, 06-APR-2009 (EDW)
C
C        Updated for PC-LINUX-GFORTRAN MAC-OSX-GFORTRAN. Eliminated
C        environment descriptions. Most were out-of-date or wrong.
C        IMPLICIT NONE now included in all environments.
C
C-    Beta Version 2.9.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    Beta Version 2.8.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-LINUX-64BIT-GCC_C.
C
C-    Beta Version 2.7.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-INTEL_C.
C
C-    Beta Version 2.6.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    Beta Version 2.5.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-    Beta Version 2.4.0, 26-OCT-2005 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-GCC_C.
C
C-    Beta Version 2.3.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN_C.
C
C-    Beta Version 2.2.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    Beta Version 2.1.5, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    Beta Version 2.1.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    Beta Version 2.1.3, 22-SEP-1999 (NJB)
C
C        CSPICE and PC-LINUX environment lines were added.  Some
C        typos were corrected.
C
C-    Beta Version 2.1.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    Beta Version 2.1.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    Beta Version 2.1.0, 12-AUG-1996 (WLT)
C
C        Added the DEC-OSF1 environment.
C
C-    Beta Version 2.0.0, 16-JUN-1995 (WLT)(HAN)
C
C        Master version of machine dependent collections.
C        Copyright notice added.
C
C-    Beta Version 1.0.0, 16-AUG-1994 (NJB)
C
C-&
 
 
C$ Index_Entries
C
C     execute an operating system command
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               RTRIM
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               STATUS
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EXESYS' )
      END IF
 
 
 
      CALL SYSTEM ( CMD( :RTRIM(CMD) ), STATUS  )
 
      IF ( STATUS .NE. 0 ) THEN
C
C        Uh, we've got a problem.
C
         CALL SETMSG ( 'The "system" call returned ' //
     .                 'code # in response to command #.'        )
         CALL ERRINT ( '#',  STATUS                              )
         CALL ERRCH  ( '#',  CMD                                 )
         CALL SIGERR ( 'SPICE(SYSTEMCALLFAILED)'                 )
         CALL CHKOUT ( 'EXESYS'                                  )
         RETURN
 
      END IF
 
      CALL CHKOUT ( 'EXESYS' )
      RETURN
      END
