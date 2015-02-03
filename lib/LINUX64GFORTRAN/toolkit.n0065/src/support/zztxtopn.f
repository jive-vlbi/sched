C$Procedure ZZTXTOPN ( Private Routine -- Text file, open new )
 
      SUBROUTINE ZZTXTOPN ( FNAME, UNIT, SUCCSS )
 
C$ Abstract
C
C     Open a new text file for subsequent write access, without
C     signaling an error on failure.
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
 
      CHARACTER*(*)         FNAME
      INTEGER               UNIT
      LOGICAL               SUCCSS
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     FNAME      I   Name of file.
C     UNIT       O   Logical unit.
C     SUCCSS     O   Logical that indicates successful open.
C
C$ Detailed_Input
C
C     FNAME          is the name of the new text file to be opened.
C
C$ Detailed_Output
C
C     UNIT           is the logical unit connected to the opened file.
C
C     SUCCSS         is the logical flag that indicates whether the
C                    file was opened successfully or not.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the file cannot be opened, SUCCSS is returned .FALSE.
C
C     2) If FNAME is a blank string, the error SPICE(BLANKFILENAME) is
C        signaled.
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
C     ZZTXTOPN opens a new text file and makes use of the SPICELIB
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
C            STATUS = 'NEW'
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
C        CALL ZZTXTOPN ( 'OUTPUT.TXT', OUT, OK )
C
C        IF ( OK ) THEN
C
C           READ  ( IN,  FMT='(A)' ) LINE
C           WRITE ( OUT, FMT='(A)' ) LINE
C
C        END IF
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
C     J.E. McLean    (JPL)
C     H.A. Neilan    (JPL)
C
C$ Version
C
C-    NSPIO Version 2.25.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    NSPIO Version 2.24.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    NSPIO Version 2.23.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    NSPIO Version 2.22.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    NSPIO Version 2.21.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GCC_C.
C
C-    NSPIO Version 2.20.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    NSPIO Version 2.19.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-CC_C.
C
C-    NSPIO Version 2.18.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
C
C-    NSPIO Version 2.17.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-    NSPIO Version 2.16.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    NSPIO Version 2.15.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    NSPIO Version 2.14.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-64BIT-MS_C.
C
C-    NSPIO Version 2.13.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-INTEL_C.
C
C-    NSPIO Version 2.12.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    NSPIO Version 2.11.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    NSPIO Version 2.10.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    NSPIO Version 2.9.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    NSPIO Version 2.8.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    NSPIO Version 2.7.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-LINUX-64BIT-GCC_C.
C
C-    NSPIO Version 2.6.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-INTEL_C.
C
C-    NSPIO Version 2.5.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    NSPIO Version 2.4.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-    NSPIO Version 2.3.0, 26-OCT-2005 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-GCC_C.
C
C-    NSPIO Version 2.2.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN_C.
C
C-    NSPIO Version 2.1.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    NSPIO Version 2.0.1, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    NSPIO Version 2.0.0, 10-FEB-2000 (FST)
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
         CALL CHKIN ( 'ZZTXTOPN' )
      END IF
 
      SUCCSS = .TRUE.
 
      IF ( FNAME .EQ. ' ' ) THEN
         SUCCSS = .FALSE.
         CALL SETMSG ( 'A blank string is unacceptable as a file name' )
         CALL SIGERR ( 'SPICE(BLANKFILENAME)'                          )
         CALL CHKOUT ( 'ZZTXTOPN'                                      )
         RETURN
      END IF
 
      CALL GETLUN ( UNIT )
 
      OPEN ( UNIT            =  UNIT,
     .       FILE            =  FNAME,
     .       FORM            = 'FORMATTED',
     .       ACCESS          = 'SEQUENTIAL',
     .       STATUS          = 'NEW',
     .       IOSTAT          =  IOSTAT      )
 
      IF ( IOSTAT .NE. 0 ) THEN
         SUCCSS = .FALSE.
         CALL CHKOUT ( 'ZZTXTOPN' )
         RETURN
      END IF
 
      CALL CHKOUT ( 'ZZTXTOPN' )
      RETURN
      END
