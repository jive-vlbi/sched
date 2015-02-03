C$Procedure      TXTOPS ( Text file, open scratch )
 
      SUBROUTINE TXTOPS (  UNIT )
 
C$ Abstract
C
C     Open a scratch text file for subsequent write access.
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
C     FILES
C     TEXT
C
C$ Declarations
 
      INTEGER               UNIT
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     UNIT       O   Logical unit.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     UNIT           is the logical unit connected to the opened
C                    scratch file.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the file cannot be opened, the error SPICE(FILEOPENFAILED)
C        is signalled.
C
C$ Files
C
C     See UNIT above.
C
C$ Particulars
C
C     In SPICELIB, a text file is formatted and sequential and may
C     contain only printable ASCII characters and blanks (ASCII 32-127).
C     When printing a text file, records are single spaced; the first
C     character will not be interpreted as a carriage control character.
C
C     TXTOPS opens a scratch text file and makes use of the SPICELIB
C     mechanism for coordinating the use of logical units.
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
C            STATUS = 'SCRATCH'
C            IOSTAT =  IOSTAT
C
C     In addition, the statement will include
C
C            CARRIAGECONTROL = 'LIST'
C
C     for the Vax and Macintosh.
C
C$ Examples
C
C     The following example reads a line from an input file,
C     'INPUT.TXT', and writes it to an output file, 'OUTPUT.TXT'.
C
C        CALL TXTOPR ( 'INPUT.TXT',  IN  )
C        CALL TXTOPS ( OUT )
C
C        READ  ( IN,  FMT='(A)' ) LINE
C        WRITE ( OUT, FMT='(A)' ) LINE
C
C        CLOSE ( IN  )
C        CLOSE ( OUT )
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     1. "Absoft FORTRAN 77 Language Reference Manual", page 7-12 for
C        the NeXT.
C
C$ Author_and_Institution
C
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.17.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    SPICELIB Version 1.16.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    SPICELIB Version 1.15.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    SPICELIB Version 1.14.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    SPICELIB Version 1.13.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GCC_C.
C
C-    SPICELIB Version 1.12.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    SPICELIB Version 1.11.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-CC_C.
C
C-    SPICELIB Version 1.10.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
C
C-    SPICELIB Version 1.9.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-    SPICELIB Version 1.8.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    SPICELIB Version 1.7.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 1.6.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-64BIT-MS_C.
C
C-    SPICELIB Version 1.5.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-INTEL_C.
C
C-    SPICELIB Version 1.4.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    SPICELIB Version 1.3.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 1.2.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    SPICELIB Version 1.1.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    SPICELIB Version 1.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 1.0.3, 21-SEP-1999 (NJB)
C
C        CSPICE and PC-LINUX environment lines were added.  Some
C        typos were corrected.
C
C-    SPICELIB Version 1.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 1.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 1.0.0, 20-FEB-1996 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     text file open scratch
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.0.0, 20-FEB-1996 (WLT)
C
C        This routine is basically a simple tweak of TXTOPN.
C        It replaces txtopn that Mike Spencer wrote because
C        the master file could not be located.
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
         CALL CHKIN ( 'TXTOPS' )
      END IF
 
 
      CALL GETLUN ( UNIT )
 
      OPEN ( UNIT            =  UNIT,
     .       FORM            = 'FORMATTED',
     .       ACCESS          = 'SEQUENTIAL',
     .       STATUS          = 'SCRATCH',
     .       IOSTAT          =  IOSTAT      )
 
      IF ( IOSTAT .NE. 0 ) THEN
         CALL SETMSG ( 'Could not scratch file. IOSTAT was #. ' )
         CALL ERRINT ( '#', IOSTAT                             )
         CALL SIGERR ( 'SPICE(FILEOPENFAILED)'                 )
         CALL CHKOUT ( 'TXTOPS'                                )
         RETURN
      END IF
 
      CALL CHKOUT ( 'TXTOPS' )
      RETURN
      END
 
