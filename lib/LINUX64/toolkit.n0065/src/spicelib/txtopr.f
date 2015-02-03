C$Procedure      TXTOPR ( Text file, open for read )
 
      SUBROUTINE TXTOPR ( FNAME, UNIT )
 
C$ Abstract
C
C     Open a text file for read access.
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
C     TEXT
C
C$ Keywords
C
C     FILES
C     TEXT
C
C$ Declarations
 
      CHARACTER*(*)         FNAME
      INTEGER               UNIT
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     FNAME      I   Name of file.
C     UNIT       O   Logical unit.
C
C$ Detailed_Input
C
C     FNAME          is the name of the text file to be opened.
C
C$ Detailed_Output
C
C     UNIT           is the logical unit connected to the opened file.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If FNAME is a blank string, the error SPICE(BLANKFILENAME) is
C        signalled.
C
C     2) If the file cannot be opened, the error SPICE(FILEOPENFAILED)
C        is signalled.
C
C$ Files
C
C     See FNAME and UNIT above.
C
C$ Particulars
C
C     In SPICELIB, a text file is formatted and sequential and may
C     contain only printable ASCII characters and blanks (ASCII 32-127).
C     When printing a text file, records are single spaced; the first
C     character will not be interpreted as a carriage control character.
C
C     TXTOPR opens an existing text file for read access and makes use
C     of the SPICELIB mechanism for coordinating use of logical units.
C
C     System Dependencies
C     ===================
C
C     The open statement will include the following keyword = value
C     pairs:
C
C            UNIT   =  UNIT
C            FILE   =  FNAME
C            FORM   = 'FORMATTED'
C            ACCESS = 'SEQUENTIAL'
C            STATUS = 'OLD'
C            IOSTAT =  IOSTAT
C
C     In addition, the statement will include
C
C            CARRIAGECONTROL = 'LIST'
C            READONLY
C
C     for the Vax and the OS X Absoft compiler, or
C
C            MODE            = 'READ'
C
C     for the IBM pc.
C
C$ Examples
C
C     The following example reads the first line from an input file,
C     'INPUT.TXT', and writes it to an output file, 'OUTPUT.TXT'.
C
C        CALL TXTOPR ( 'INPUT.TXT',  IN  )
C        CALL TXTOPN ( 'OUTPUT.TXT', OUT )
C
C        READ  ( IN,  FMT='(A)' ) LINE
C        WRITE ( OUT, FMT='(A)' ) LINE
C
C        CLOSE ( IN  )
C        CLOSE ( OUT )
C
C$ Restrictions
C
C     The file, FNAME, must exist prior to calling TXTOPR.
C
C$ Literature_References
C
C     1. "Lahey F77L EM/32 FORTRAN Language Reference Manual", page
C        145.
C
C     2. "Absoft FORTRAN 77 Language Reference Manual", page 7-12 for
C        the NeXT.
C
C$ Author_and_Institution
C
C     J.E. McLean    (JPL)
C     H.A. Neilan    (JPL)
C     M.J. Spencer   (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.25.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    SPICELIB Version 2.24.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    SPICELIB Version 2.23.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    SPICELIB Version 2.22.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    SPICELIB Version 2.21.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GCC_C.
C
C-    SPICELIB Version 2.20.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    SPICELIB Version 2.19.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-CC_C.
C
C-    SPICELIB Version 2.18.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
C
C-    SPICELIB Version 2.17.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-    SPICELIB Version 2.16.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    SPICELIB Version 2.15.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 2.14.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-64BIT-MS_C.
C
C-    SPICELIB Version 2.13.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-INTEL_C.
C
C-    SPICELIB Version 2.12.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    SPICELIB Version 2.11.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 2.10.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    SPICELIB Version 2.9.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    SPICELIB Version 2.8.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    SPICELIB Version 2.7.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-LINUX-64BIT-GCC_C.
C
C-    SPICELIB Version 2.6.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-INTEL_C.
C
C-    SPICELIB Version 2.5.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    SPICELIB Version 2.4.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-    SPICELIB Version 2.3.0, 26-OCT-2005 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-GCC_C.
C
C-    SPICELIB Version 2.2.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN_C.
C
C-    SPICELIB Version 2.1.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    SPICELIB Version 2.0.6, 24-APR-2003 (EDW)
C
C        Added MAC-OSX-F77 to the list of platforms
C        that require READONLY to read write protected
C        kernels.
C
C-    SPICELIB Version 2.0.5, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 2.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 2.0.3, 16-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    SPICELIB Version 2.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 2.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 2.0.0, 05-APR-1998 (NJB)
C
C        References to the PC-LINUX environment were added.
C
C-    SPICELIB Version 1.3.0, 11-NOV-1993 (HAN)
C
C         Module was updated for the Silicon Graphics, DEC Alpha-OSF/1,
C         and NeXT platforms.
C
C-    SPICELIB Version 1.2.0, 12-OCT-1992 (HAN)
C
C        The code was also reformatted so that a utility program can
C        create the source file for a specific environment given a
C        master source file.
C
C-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.1.0, 13-NOV-1991 (MJS)
C
C        Module updated to allow portability to the Lahey F77L EM/32
C        FORTRAN V 4.0 environment.
C
C-    SPICELIB Version 1.0.0, 05-APR-1991 (JEM)
C
C-&
 
C$ Index_Entries
C
C     text file open for read
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 05-APR-1998 (NJB)
C
C        References to the PC-LINUX environment were added.
C
C-    SPICELIB Version 1.3.0, 11-NOV-1993 (HAN)
C
C         Module was updated for the Silicon Graphics, DEC Alpha-OSF/1,
C         and NeXT platforms.
C
C-    SPICELIB Version 1.2.0, 12-OCT-1992 (HAN)
C
C        The code was also reformatted so that a utility program can
C        create the source file for a specific environment given a
C        master source file.
C
C-    SPICELIB Version 1.1.0, 13-NOV-1991 (MJS)
C
C        Module updated to allow portability to the Lahey F77L EM/32
C        FORTRAN V 4.0 environment.
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               IOSTAT
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'TXTOPR' )
      END IF
 
      IF ( FNAME .EQ. ' ' ) THEN
         CALL SETMSG ( 'A blank string is unacceptable as a file name' )
         CALL SIGERR ( 'SPICE(BLANKFILENAME)'                          )
         CALL CHKOUT ( 'TXTOPR'                                        )
         RETURN
      END IF
 
      CALL GETLUN ( UNIT )
 
      OPEN ( UNIT            =  UNIT,
     .       FILE            =  FNAME,
     .       FORM            = 'FORMATTED',
     .       ACCESS          = 'SEQUENTIAL',
     .       STATUS          = 'OLD',
     .       IOSTAT          =  IOSTAT      )
 
      IF ( IOSTAT .NE. 0 ) THEN
         CALL SETMSG ( 'Could not open file #. IOSTAT was #. ' )
         CALL ERRCH  ( '#', FNAME                              )
         CALL ERRINT ( '#', IOSTAT                             )
         CALL SIGERR ( 'SPICE(FILEOPENFAILED)'                 )
         CALL CHKOUT ( 'TXTOPR'                                )
         RETURN
      END IF
 
      CALL CHKOUT ( 'TXTOPR' )
      RETURN
      END
