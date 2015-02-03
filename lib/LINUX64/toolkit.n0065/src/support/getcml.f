C$Procedure      GETCML ( Get the command line )
 
      SUBROUTINE GETCML ( LINE )
 
C$ Abstract
C
C     Return command line arguments in a single string.
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
 
 
      CHARACTER*(*)         LINE
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     LINE       O   The command line arguments.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     LINE      is the string containing the command line arguments.
C               The actual command is not contained in the string.
C
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C     This routine does not participate in the SPICELIB error handling
C     mechanism.
C
C     Any other exceptions are unknown at this time.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The source code for platforms for which we have no documentation
C     was created as a result of porting the Sun OS/Sun Fortran version
C     of the routine to detemine if it executed successfully on the
C     target platform. This is how we generated the code for the Sun
C     Solaris/Sun Fortran, Silicon Graphics IRIX/SGI Fortran, and
C     DEC Alpha OSF-1/DEC Fortran platforms.
C
C$ Examples
C
C     If inputs is a Fortran program, and the following command is
C     executed from a command line,
C
C       % inputs this is the command line input
C
C     the following string will be returned by GETCML:
C
C       this is the command line input
C
C     The Mac Classic version creates a window into which the user
C     inputs the command line arguments (if any).
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     1. "FORTRAN/9000 Reference HP 9000 Series 700 Computers,"
C        First Edition, June 1991, Hewlett Packard Company,
C        pages 9-29, 9-30, 18-2 through 18-5.
C
C     2. "Language Guide," accompaniment to Microsoft FORTRAN
C        PowerStation Compiler, version 1.0, 1993, pages 296-297,
C        401-402.
C
C     3. "Absoft FORTRAN 77 Compatibility Libraries,"
C        Absoft Corporation, 1991-1992, pages 3-10 and 3-14.
C
C     4. "Lahey F77L3-EM/32 FORTRAN Programmer's Reference," Lahey
C        Computer Systems, Inc., Revision C, January 1992, page 35.
C
C$ Author_and_Institution
C
C     K.R. Gehringer (JPL)
C     H.A. Neilan    (JPL)
C     M.J. Spencer   (JPL)
C
C$ Version
C
C-    SPICELIB Version 6.16.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    SPICELIB Version 6.15.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    SPICELIB Version 6.14.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    SPICELIB Version 6.13.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    SPICELIB Version 6.12.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    SPICELIB Version 6.11.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    SPICELIB Version 6.10.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 6.9.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    SPICELIB Version 6.8.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 6.7.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    SPICELIB Version 6.6.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    SPICELIB Version 6.5.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    SPICELIB Version 6.4.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    SPICELIB Version 6.3.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-    SPICELIB Version 6.2.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    SPICELIB Version 6.1.1, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 6.1.0, 14-NOV-2001 (EDW)
C
C        Add code to create an input window for arguments
C        under Mac Absoft (Classic) compiler. The enviroment
C        ID is MACPPC.
C
C-    SPICELIB Version 6.0.0, 14-OCT-1999 (WLT)
C
C        The VMS environment was modified to return the command
C        line after applying LCASE.
C
C-    SPICELIB Version 5.0.3, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 5.0.0, 24-NOV-1998 (WLT)
C
C        Added references for the MAC.
C
C-    SPICELIB Version 4.0.0, 05-APR-1998 (NJB)
C
C        Added references for the PC-LINUX environment.  Added SAVE
C        statements for the variable FIRST.
C
C-    SPICELIB Version 3.2.0, 18-AUG-1995 (KRG) (HAN)
C
C        Added code for the PC-Lahey environment. Increased the size
C        of ARGLEN to 512 as a result of truncation problems on some
C        Unix platforms.
C
C-    SPICELIB Version 3.1.0, 5-JAN-1995 (HAN)
C
C        Removed Sun Solaris environment since it is now the same
C        as the Sun OS 4.1.x environment.
C        Removed DEC Alpha/OpenVMS environment since it is now the
C        same as the VAX environment.
C
C-    SPICELIB Version 3.0.0, 8-JUL-1994 (HAN)
C
C        Added code for the NeXT/Absoft Fortran, HP-UX/HP Fortran,
C        Silicon Graphics (SGI) IRIX/SGI Fortran,
C        Sun Solaris/Sun Fortran, and DEC Alpha OSF-1/DEC Fortran
C        environments.
C
C-    SPICELIB Version 2.0.0, 15-MAR-1994 (HAN) (MJS)
C
C        Added code for the HP and the PC/Microsoft Powerstation
C        environments.
C
C-    SPICELIB Version 1.0.0, 19-NOV-1992 (HAN)
C
C-&
 
C$ Index_Entries
C
C   get command line arguments
C
C-&
 
 
 
C
C     Other functions
C
      INTEGER               IARGC
 
C
C     Local Variables
C
      INTEGER               ARGLEN
      PARAMETER           ( ARGLEN = 512 )
 
      CHARACTER*(ARGLEN)    ARGMNT
 
      INTEGER               HOWMNY
      INTEGER               I
 
      LOGICAL               FIRST
 
      SAVE                  FIRST
 
      DATA                  FIRST  / .TRUE. /
 
C
C     Call the FORTRAN library function iargc to determine how many
C     words are on the command line. Then, get the arguments one at a
C     time and construct the output string.
C
      HOWMNY = IARGC()
      I      = 1
      LINE   = ' '
 
      IF ( HOWMNY .EQ. 0 ) THEN
         RETURN
      END IF
 
      DO WHILE ( I .LE. HOWMNY )
 
         CALL GETARG ( I, ARGMNT )
 
         IF ( FIRST ) THEN
            CALL SUFFIX ( ARGMNT, 0, LINE )
            FIRST = .FALSE.
         ELSE
            CALL SUFFIX ( ARGMNT, 1, LINE )
         END IF
 
         I = I + 1
 
      END DO
 
      RETURN
      END
