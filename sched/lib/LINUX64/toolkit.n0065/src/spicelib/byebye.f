C$Procedure      BYEBYE ( Exit a program indicating an error status )
 
      SUBROUTINE BYEBYE ( STATUS )
 
C$ Abstract
C
C     Exit an executing program returning a success or failure status
C     to the operating system, if this capability is supported, or
C     simply exit the program.
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
 
      CHARACTER*(*)         STATUS
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     STATUS     I   A string indicating the exit status of a program.
C
C$ Detailed_Input
C
C     STATUS  This is a character string which indicates the status
C             to use when exiting a program. The two status values
C             currently supported are 'SUCCESS' and 'FAILURE', which
C             have their obvious meanings. The case of the input is
C             not important, i.e., 'Success' or 'failure' are accepted.
C
C             If STATUS has a value of 'SUCCESS', then the calling
C             program will be terminated with a status that indicates
C             success.
C
C             If STATUS has a value of 'FAILURE', then the calling
C             program will be terminated with a status that indicates
C             failure.
C
C             If STATUS has a value that is not recognized, the calling
C             program will be terminated with a status that indicates
C             failure.
C
C             For environments which do not support the passage of a
C             status indicator to the operating system the value of
C             STATUS is not significant; an executing program will be
C             terminated using the Fortran STOP statement.
C
C             See the value of the parameter STLEN declared in the
C             Local Parameters section for the maximum significant
C             length of an exit status string.
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
C     This subroutine is called by SIGERR to exit a program
C     returning a success or failure indication to the operating
C     system when this is possible. If returning a status
C     indication to the operating system is not possible, this
C     subroutine simply executes a Fortran STOP statement.
C
C$ Examples
C
C     To exit a program indicating success:
C
C        CALL BYEBYE ( 'SUCCESS' )
C
C     To exit a program indicating failure:
C
C        CALL BYEBYE ( 'FAILURE' )
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
C     K.R. Gehringer     (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.15.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    SPICELIB Version 2.14.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    SPICELIB Version 2.13.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    SPICELIB Version 2.12.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    SPICELIB Version 2.11.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    SPICELIB Version 2.10.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    SPICELIB Version 2.9.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 2.8.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    SPICELIB Version 2.7.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 2.6.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    SPICELIB Version 2.5.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    SPICELIB Version 2.4.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    SPICELIB Version 2.3.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    SPICELIB Version 2.2.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-    SPICELIB Version 2.1.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    SPICELIB Version 2.0.5, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 2.0.4, 28-AUG-2000 (EDW)
C
C        Included PROMPT call for MAC Absoft environment.
C        Without something to pause execution, the output window
C        collapses at program end.
C
C-    SPICELIB Version 2.0.3, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 2.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C        The success flag for SGI-N32 is '0', failure '1'.  These
C        were simply 0 and 1 respectively when the environment was
C        just SGI.
C
C-    SPICELIB Version 2.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 2.0.0, 05-APR-1998 (NJB)
C
C        Added the PC-LINUX environment.
C
C-    SPICELIB Version 1.0.0, 25-APR-1996 (KRG)
C
C-&
 
C$ Index_Entries
C
C     gracefully exit a program
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 05-APR-1998 (NJB)
C
C        Added the PC-LINUX environment.
C
C-&
 
 
C
C     Local Parameters
C
C     Define the maximum length for the usable portion of a status
C     string.
C
      INTEGER               STLEN
      PARAMETER           ( STLEN = 7 )
C
C     Define the notion of 'SUCCESS' and 'FAILURE'.
C
      INTEGER               FAILUR
      PARAMETER           ( FAILUR = 1 )
 
      INTEGER               SUCCES
      PARAMETER           ( SUCCES = 0 )
 
 
      CHARACTER*(STLEN)     CHSTAT
 
      LOGICAL               MYSTAT
 
      CALL LJUST ( STATUS, CHSTAT )
      CALL UCASE ( CHSTAT, CHSTAT )
 
      IF ( CHSTAT .EQ. 'SUCCESS' ) THEN
         MYSTAT = .TRUE.
      ELSE
         MYSTAT = .FALSE.
      END IF
 
      IF ( MYSTAT ) THEN
         CALL EXIT ( SUCCES )
      ELSE
         CALL EXIT ( FAILUR )
      END IF
 
C
C     We never really get here, but for pedantic reasons we must
C     have a RETURN statement in a subroutine.
C
      RETURN
 
      END
