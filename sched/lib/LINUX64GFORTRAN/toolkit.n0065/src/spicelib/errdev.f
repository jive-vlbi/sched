C$Procedure      ERRDEV ( Get/Set Error Output Device Name )
 
      SUBROUTINE ERRDEV ( OP, DEVICE )
 
C$ Abstract
C
C     Retrieve or set the name of the current output
C     device for error messages.
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
 
      CHARACTER*(*)          OP
      CHARACTER*(*)          DEVICE
 
      INTEGER                FILEN
      PARAMETER            ( FILEN = 255 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OP         I   The operation:  'GET' or 'SET'.
C     DEVICE    I/O  The device name.
C     FILEN      P   Maximum length of file name.
C
C$ Detailed_Input
C
C     OP      indicates the operation to be performed.  Possible
C             values are 'GET' and 'SET'.  'GET' means, "set
C             DEVICE equal to the name of the current error
C             output device"  'SET' means, "set the name of the
C             current error output device equal to the value of
C             DEVICE."
C
C     DEVICE  is an input when OP has the value, 'SET'.  It
C             indicates an output device to which error messages
C             are to be sent.  Possible values for DEVICE are:
C
C              1.    A file name.  Note that the name must not
C                    be any of the reserved strings below.
C
C              2.    'SCREEN'    The output will go to the
C                     screen.  This is the default device.
C
C              3.    'NULL'      The data will not be output.
C
C              'SCREEN' and 'NULL' can be written in mixed
C              case.  For example, the following call will work:
C
C              CALL ERRDEV ( 'SET' , 'screEn' )
C
C$ Detailed_Output
C
C     DEVICE  is an output when OP is 'GET'.  It is the
C             current error output device.  See "Detailed
C             Input" for possible values and meanings.
C
C$ Parameters
C
C     FILEN   The maximum length of a file name that can be
C             processed by this routine. See the Literature_References
C             section for more information.
C
C$ Exceptions
C
C     This routine detects the following errors:
C
C     1.  'SPICE(INVALIDOPERATION)'  ...Invalid value of the
C                                       argument, OP.
C
C     2.  'SPICE(DEVICENAMETOOLONG)' ...Device name exceeds
C                                       FILEN characters
C
C
C     Also, this routine is part of the SPICELIB error
C     handling mechanism.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Please read the "required reading"!
C
C     This routine can't tell whether the name supplied
C     to indicate the output device is valid.  Be careful!
C
C$ Examples
C
C     1.  In this example, we select as the output device
C         the file, SPUD.DAT.
C
C      C
C      C      Set the error output device to SPUD.DAT:
C      C
C
C             CALL ERRDEV (  'SET',  'SPUD.DAT'  )
C
C
C$ Restrictions
C
C     This routine has no capability of determining the validity
C     of the name of an output device.  Care must be taken
C     to ensure that the file named is the correct one.
C
C     The device name is assumed to be no longer than FILEN characters.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     H.A. Neilan     (JPL)
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
C-    SPICELIB Version 2.0.3, 24-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    SPICELIB Version 2.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
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
C        References to the PC-LINUX environment were added.
C
C-    SPICELIB Version 1.2.0, 3-NOV-1993 (HAN)
C
C        Module was updated to include the value for FILEN
C        for the Silicon Graphics, DEC Alpha-OSF/1, and
C        NeXT platforms. Also, the previous value of 256 for
C        Unix platforms was changed to 255.
C
C-    SPICELIB Version 1.1.0, 9-OCT-1992 (HAN)
C
C        Updated module for multiple environments.
C
C        The code was also reformatted so that a utility program can
C        create the source file for a specific environment given a
C        master source file.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     get/set error output device name
C
C-&
 
 
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 05-APR-1998 (NJB)
C
C        References to the PC-LINUX environment were added.
C
C-     SPICELIB Version 1.2.0, 3-NOV-1993 (HAN)
C
C         Module was updated to include the value for FILEN
C         for the Silicon Graphics, DEC Alpha-OSF/1, and
C         NeXT platforms. Also, the previous value of 256 for
C         Unix platforms was changed to 255.
C
C-     SPICELIB Version 1.1.0, 9-OCT-1992 (HAN)
C
C         Updated module for multiple environments.
C
C         The code was also reformatted so that a utility program can
C         create the source file for a specific environment given a
C         master source file.
C
C-     Beta Version 1.1.0, 16-FEB-1989 (NJB)
C
C        File name length parameter added to parameters section.
C        Declaration of the unused function FRSTNB removed.
C        Trace participation added.  This routine now checks in
C        and checks out.  However, it does not test RETURN,
C        because it should be able to execute in RETURN mode when
C        an error condition exists.
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
      INTEGER               LASTNB
 
C
C     Local Variables:
C
 
      CHARACTER*(FILEN)     UPNAM
      CHARACTER*(FILEN)     LOCNAM
 
      INTEGER               OPLEN
      PARAMETER           ( OPLEN = 3 )
 
      CHARACTER*(OPLEN)     UPOP
      CHARACTER*(OPLEN)     LOCOP
 
C
C     Initial Values:
C
 
 
 
C
C     Executable Code:
C
 
      CALL CHKIN ( 'ERRDEV' )
 
C
C     We save the operation string as input, and get
C     an upper case version for our own use:
C
 
 
      CALL LJUST ( OP,   UPOP )
      CALL UCASE ( UPOP, UPOP )
 
      IF ( UPOP  .EQ. 'GET' ) THEN
 
         CALL GETDEV ( DEVICE )
 
      ELSE IF ( UPOP  .EQ. 'SET' ) THEN
 
 
C
C        We want the reserved words to be in upper
C        case for our own use.  So, save the input value
C        and get an upper case version:
C
 
         CALL LJUST ( DEVICE, UPNAM )
         CALL UCASE ( UPNAM,  UPNAM )
 
         IF (   LASTNB( UPNAM )   .GT.   FILEN    )  THEN
 
            LOCNAM = DEVICE
 
            CALL SETMSG ( 'ERRDEV:  Device name exceeds FILEN'        //
     .                    ' characters; device selection not updated.'//
     .                    ' The first FILEN characters of the name'   //
     .                    ' were:  '//LOCNAM
     .                  )
 
            CALL SIGERR ( 'SPICE(DEVICENAMETOOLONG)' )
 
            CALL CHKOUT ( 'ERRDEV' )
            RETURN
 
         END IF
 
 
         IF (  ( UPNAM  .EQ. 'SCREEN' ) .OR.
     .         ( UPNAM  .EQ. 'NULL'   )        )  THEN
 
C
C           Store upper case version of DEVICE:
C
 
            CALL PUTDEV ( UPNAM )
 
         ELSE
 
C
C           We assume we've got a file name...
C           Store it as it was input.
C
            CALL PUTDEV ( DEVICE )
 
         END IF
 
      ELSE
 
C
C        An invalid value of OP was supplied.
C
 
         LOCOP = OP
 
         CALL SETMSG ( 'ERRDEV:  An invalid value of OP was supplied.'//
     .                 '  The value was: '  // LOCOP   )
 
         CALL SIGERR ( 'SPICE(INVALIDOPERATION)' )
 
      END IF
 
      CALL CHKOUT ( 'ERRDEV' )
      RETURN
      END
