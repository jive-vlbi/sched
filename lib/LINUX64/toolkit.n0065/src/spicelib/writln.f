C$Procedure      WRITLN ( Write a text line to a logical unit )
 
      SUBROUTINE WRITLN ( LINE, UNIT )
 
C$ Abstract
C
C     Write a single line of text to the Fortran logical unit UNIT.
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
C     ASCII
C     TEXT
C     FILES
C
C$ Declarations
 
      CHARACTER*(*)      LINE
      INTEGER            UNIT
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     LINE       I   The line which is to be written to UNIT.
C     UNIT       I   The Fortran unit number to use for output.
C
C$ Detailed_Input
C
C     LINE     This contains the text line which is to be written
C              to UNIT.
C
C              The value of this variable is not modified.
C
C     UNIT     The Fortran unit number for the output. This may be
C              either the unit number for the terminal, or the unit
C              number of a previously opened text file.
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
C     1)   If an error occurs while attempting to write to the text
C          file attached to UNIT, the error SPICE(FILEWRITEFAILED) will
C          be signalled.
C
C     This routine only checks in with the error handler in the event
C     that an error occurred. (Discovery check in)
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine will write a single text line to the device
C     specified by UNIT. UNIT may be the terminal, or it may be
C     a logical unit number obtained from a Fortran OPEN or INQUIRE
C     statement. When written, the line will have trailing spaces
C     removed.
C
C$ Examples
C
C     CALL WRITLN( LINE, UNIT )
C
C     You have now written a line of text to unit UNIT.
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
C     K.R. Gehringer (JPL)
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
C-    SPICELIB Version 2.20.1, 18-MAY-2010 (BVS)
C
C        Removed "C$" marker from text in the header.
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
C-    SPICELIB Version 2.0.0, 08-APR-1998 (NJB)
C
C        Module was updated for the PC-LINUX platform.
C
C-    SPICELIB Version 1.1.1, 20-AUG-1996 (WLT)
C
C        Corrected the heading for the Index_Entries section.
C
C-    SPICELIB Version 1.1.0, 29-FEB-1996 (KRG)
C
C        For the Macintosh, we need to use real Fortran I/O, i.e.,
C        using the first column for carriage control. The change
C        was to move the MAC environment indicator from one
C        environment case to the other.
C
C        Also, for UNIX environments, the parameter STDOUT is no
C        longer defined. This only appears for platforms that
C        need it to differentiate between writing to a file and
C        the terminal screen (standard output), currently: VAX,
C        PC-LAHEY, PC-MS, and MAC.
C
C-    SPICELIB Version 1.0.0, 20-DEC-1995 (KRG)
C
C        The routine graduated
C
C-    Beta Version 3.1.0, 18-AUG-1995 (KRG)
C
C        Moved the PC-LAHEY environment indicator from one environment
C        case to the other. The Lahey compiler on the PC does treat text
C        files and the standard output device differently.
C
C-    Beta Version 3.0.1, 01-JAN-1995 (KRG)
C
C        Moved the description of the input variable UNIT from the $
C        Detailed_Output section of the header to the correct location
C        in the $ Detailed_Input section of the header.
C
C-    Beta Version 3.0.0, 11-JUL-1994 (HAN)
C
C        Edited master source file to correct the code for the
C        PC/Microsoft FORTRAN PowerStation environment. It should use
C        the same code as the VAX, not the PC/Lahey Fortran code. Also,
C        code was included for the DEC Alpha OpenVMS/DEC Fortran and
C        Sun Solaris/Sun Fortran environments.
C
C-    Beta Version 2.0.0, 30-MAR-1994 (HAN)
C
C        Edited master source file to include new environments:
C        Silicon Graphics IRIX/Silicon Graphics Fortran,
C        DEC Alpha-OSF/1, and NeXT/Absoft Fortran.
C
C-    Beta Version 1.0.0, 17-DEC-1992 (KRG)
C
C-&
 
C$ Index_Entries
C
C     write a text line to a logical unit
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 08-APR-1998 (NJB)
C
C        Module was updated for the PC-LINUX platform.
C
C-    SPICELIB Version 1.1.0, 29-FEB-1996 (KRG)
C
C        For the Macintosh, we need to use real Fortran I/O, i.e.,
C        using the first column for carriage control. The change
C        was to move the MAC environment indicator from one
C        environment case to the other.
C
C        Also, for UNIX environments, the parameter STDOUT is no
C        longer defined. This only appears for platforms that
C        need it to differentiate between writing to a file and
C        the terminal screen (standard output), currently: VAX,
C        PC-LAHEY, PC-MS, and MAC.
C
C-    SPICELIB Version 1.0.0, 20-DEC-1995 (KRG)
C
C        The routine graduated
C
C-    Beta Version 3.1.0, 18-AUG-1995 (KRG)
C
C        Moved the PC-LAHEY environment indicator from one environment
C        case to the other. The Lahey compiler on the PC does treat text
C        files and the standard output device differently.
C
C-    Beta Version 3.0.1, 01-JAN-1995 (KRG)
C
C        Moved the description of the input variable UNIT from the $
C        Detailed_Output section of the header to the correct location
C        in the $ Detailed_Input section of the header.
C
C-    Beta Version 3.0.0, 11-JUL-1994 (HAN)
C
C        Edited master source file to correct the code for the
C        PC/Microsoft FORTRAN PowerStation environment. It should use
C        the same code as the VAX, not the PC/Lahey Fortran code. Also,
C        code was included for the DEC Alpha OpenVMS/DEC Fortran and
C        Sun Solaris/Sun Fortran environments.
C
C-    Beta Version 2.0.0, 30-MAR-1994 (HAN)
C
C        Edited master source file to include new environments:
C        Silicon Graphics IRIX/Silicon Graphics Fortran,
C        DEC Alpha-OSF/1, and NeXT/Absoft Fortran.
C
C-    Beta Version 1.0.0, 17-DEC-1992 (KRG)
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               RTRIM
 
C
C     Local variables
C
      INTEGER               IOSTAT
 
C
C     UNIX based fortran implementations typically do not distinguish
C     between a text file and the standard output unit, so no leading
C     vertical spacing character is required.
C
      WRITE ( UNIT, FMT='(A)', IOSTAT=IOSTAT ) LINE(:RTRIM(LINE))
C
C     Check to see if we got a write error, and signal it if we did.
C     Also check in and check out.
C
      IF ( IOSTAT .NE. 0  ) THEN
 
         CALL CHKIN  ( 'WRITLN'                                  )
         CALL SETMSG ( 'Error Writing to file: #. IOSTAT = #.'   )
         CALL ERRFNM ( '#', UNIT                                 )
         CALL ERRINT ( '#', IOSTAT                               )
         CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'                  )
         CALL CHKOUT ( 'WRITLN'                                  )
         RETURN
 
      END IF
 
      RETURN
      END
 
