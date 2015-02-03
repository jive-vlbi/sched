C$Procedure      WRLINE ( Write Output Line to a Device )
 
      SUBROUTINE WRLINE ( DEVICE, LINE )
 
C$ Abstract
C
C     Write a character string to an output device.
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
C     TEXT
C     FILES
C     ERROR
C
C$ Declarations
 
      CHARACTER*(*)          DEVICE
      CHARACTER*(*)          LINE
 
      INTEGER                FILEN
      PARAMETER            ( FILEN = 255 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     DEVICE     I   A string specifying an output device.
C     LINE       I   A line of text to be output.
C     FILEN      P   Maximum length of a file name.
C
C$ Detailed_Input
C
C     LINE           is a line of text to be written to the output
C                    device specified by DEVICE.
C
C     DEVICE         is the output device to which the line of text
C                    will be written.
C
C                    Possible values and meanings of DEVICE are:
C
C                       a device name   This may be the name of a
C                                       file, or any other name that
C                                       is valid in a FORTRAN OPEN
C                                       statement.  For example, on a
C                                       VAX, a logical name may be
C                                       used.
C
C                                       The device name must not
C                                       be any of the reserved strings
C                                       below.
C
C
C                       'SCREEN'        The output will go to the
C                                       terminal screen.
C
C
C                       'NULL'          The data will not be output.
C
C
C                 'SCREEN' and 'NULL' can be written in mixed
C                  case.  For example, the following call will work:
C
C                  CALL WRLINE ( 'screEn', LINE )
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     FILEN        is the maximum length of a file name.
C
C$ Exceptions
C
C     This routine is a special case as far as error handling
C     is concerned because it is called to output error
C     messages resulting from errors detected by other routines.
C     In such a case, calling SIGERR would constitute recursion.
C     Therefore, this routine prints error messages rather
C     than signalling errors via SIGERR and setting the long
C     error message via SETMSG.
C
C     The following exceptional cases are treated as errors:
C
C     1)  SPICE(NOFREELOGICALUNIT) -- No logical unit number
C         is available to refer to the device.
C
C     2)  SPICE(FILEOPENFAILED) -- General file open error.
C
C     3)  SPICE(FILEWRITEFAILED) -- General file write error.
C
C     4)  SPICE(INQUIREFAILED) -- INQUIRE statement failed.
C
C     5)  Leading blanks in (non-blank) file names are not
C         significant.  The file names
C
C             'MYFILE.DAT'
C             '   MYFILE.DAT'
C
C         are considered to name the same file.
C
C     6)  If different names that indicate the same file are supplied
C         to this routine on different calls, all output associated
C         with these calls WILL be written to the file.  For example,
C         on a system where logical filenames are supported, if
C         ALIAS is a logical name pointing to MYFILE, then the calls
C
C             CALL WRLINE ( 'MYFILE', 'This is the first line'  )
C             CALL WRLINE ( 'ALIAS',  'This is the second line' )
C
C         will place the lines of text
C
C              'This is the first line'
C              'This is the second line'
C
C         in MYFILE.  See $Restrictions for more information on use
C         of logical names on VAX systems.
C
C$ Files
C
C     1)  If DEVICE specifies a device other than 'SCREEN' or 'NULL',
C         that device is opened (if it's not already open) as a NEW,
C         SEQUENTIAL, FORMATTED file.  The logical unit used is
C         determined at run time.
C
C$ Particulars
C
C     If the output device is a file that is not open, the file will
C     be opened (if possible) as a NEW, sequential, formatted file,
C     and the line of text will be written to the file.  If the file
C     is already opened as a sequential, formatted file, the line of
C     text will be written to the file.
C
C     Use the entry point CLLINE to close files opened by WRLINE.
C
C$ Examples
C
C     1)  Write a message to the screen:
C
C                CALL WRLINE ( 'SCREEN', 'Here''s a message.' )
C
C         The text
C
C                Here's a message.
C
C         will be written to the screen.
C
C
C     2)  Write out all of the elements of a character string array
C         to a file.
C
C                CHARACTER*(80)          STRING ( ASIZE )
C                             .
C                             .
C                             .
C                DO I = 1, ASIZE
C                   CALL WRLINE ( FILE, STRING(I) )
C                END DO
C
C
C     3)  Set DEVICE to NULL to suppress output:
C
C             C
C             C     Ask the user whether verbose program output is
C             C     desired.  Set the output device accordingly.
C             C
C                   WRITE (*,*) 'Do you want to see test results '    //
C                  .            'on the screen?'
C                   READ  (*,FMT='(A)') VERBOS
C
C                   CALL LJUST ( VERBOS, VERBOS )
C                   CALL UCASE ( VERBOS, VERBOS )
C
C                   IF ( VERBOS(1:1) .EQ. 'Y' ) THEN
C                      DEVICE = 'SCREEN'
C                   ELSE
C                      DEVICE = 'NULL'
C                   ENDIF
C                             .
C                             .
C                             .
C             C
C             C     Output test results.
C             C
C                   CALL WRLINE ( DEVICE, STRING )
C                             .
C                             .
C                             .
C
C$ Restrictions
C
C     1)  File names must not exceed FILEN characters.
C
C     2)  On VAX systems, caution should be exercised when using
C         multiple logical names to point to the same file.  Logical
C         name translation supporting execution of the Fortran
C         INQUIRE statement does not appear to work reliably in all
C         cases, which may lead this routine to believe that different
C         logical names indicate different files.  The specific problem
C         that has been observed is that logical names that include
C         disk specifications are not always recognized as pointing
C         to the file they actually name.
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
C-    SPICELIB Version 4.25.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    SPICELIB Version 4.24.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    SPICELIB Version 4.23.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    SPICELIB Version 4.22.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    SPICELIB Version 4.21.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GCC_C.
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
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 4.0.3, 16-SEP-1999 (NJB)
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
C        References to the PC-LINUX environment were added.  The
C        write format for the case where the output device is the
C        screen has been made system-dependent; list-directed output
C        format is now used for systems that require a leading carriage
C        control character; other systems use character format. The
C        write format for the case where the output device is a file
C        has been changed from list-directed to character.
C
C
C-    SPICELIB Version 3.0.0, 11-NOV-1993 (HAN)
C
C        Module was updated to include the value for FILEN
C        and the appropriate OPEN statement for the Silicon
C        Graphics, DEC Alpha-OSF/1, and NeXT platforms. The previous
C        value of 256 for Unix platforms was changed to 255.
C
C-    SPICELIB Version 2.1.0, 13-OCT-1992 (HAN)
C
C       Module was updated to include the value of FILEN for the
C       Hewlett Packard UX 9000/750 environment.
C
C       The code was also reformatted so that a utility program can
C       create the source file for a specific environment given a
C       master source file.
C
C-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C       Comment section for permuted index source lines was added
C       following the header.
C
C-    SPICELIB Version 2.0.0, 26-MAR-1991 (NJB)
C
C       This routine now can write to files that have been opened
C       by other routines.
C
C       The limit imposed by this routine on the number of files it
C       can open has been removed.
C
C       The output file is now opened as a normal text file on
C       VAX systems.
C
C       Improper treatment of the case where DEVICE is blank was
C       remedied.
C
C       Unneeded variable declarations and references were removed.
C
C       Initialization of SAVED variables was added.
C
C       All occurrences of "PRINT *" have been replaced by
C       "WRITE (*,*)".
C
C       Calls to UCASE and LJUST replace in-line code that performed
C       these operations.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     write output line to a device
C
C-&
 
 
 
 
C$ Revisions
C
C-    SPICELIB Version 4.0.0, 07-APR-1998 (NJB)
C
C        References to the PC-LINUX environment were added.
C
C        The write format for the case where the output device is the
C        screen has been made system-dependent; list-directed output
C        format is now used for systems that require a leading carriage
C        control character; other systems use character format. The
C        write format for the case where the output device is a file
C        has been changed from list-directed to character.
C
C-    SPICELIB Version 3.0.0, 11-NOV-1993 (HAN)
C
C         Module was updated to include the value for FILEN
C         and the appropriate OPEN statement for the Silicon
C         Graphics, DEC Alpha-OSF/1, and NeXT platforms. The previous
C         value of 256 for Unix platforms was changed to 255.
C
C-     SPICELIB Version 2.1.0, 13-OCT-1992 (HAN)
C
C        Module was updated to include the value of FILEN for the
C        Hewlett Packard UX 9000/750 environment.
C
C        The code was also reformatted so that a utility program can
C        create the source file for a specific environment given a
C        master source file.
C
C-    SPICELIB Version 2.0.0, 25-MAR-1991 (NJB)
C
C        1)  This routine now can write to files that have been opened
C            by other routines.  WRLINE uses an INQUIRE statement to
C            determine whether the file indicated by DEVICE is open,
C            and if it is, WRLINE does not attempt to open it.  This
C            allows use of WRLINE to feed error output into a log file
C            opened by another routine.
C
C            The header has been updated accordingly.
C
C            This fix also fixes a bug wherein this routine would treat
C            different character strings naming the same file as though
C            they indicated different files.
C
C        2)  The limit imposed by this routine on the number of files it
C            can open has been removed.  The file database used in
C            previous versions of this routine is no longer used.
C
C        3)  On VAX systems, this routine now opens the output file
C            (when required to do so) as a normal text file.
C
C        4)  Improper treatment of the case where DEVICE is blank was
C            remedied.  Any value of DEVICE that is not equal to
C            'SCREEN' or 'NULL' after being left-justified and
C            converted to upper case is considered to be a file name.
C
C        5)  Unneeded variable declarations and references were removed.
C            The arrays called STATUS and FILES are not needed.
C
C        6)  All instances if "PRINT *" have been replaced by
C            "WRITE (*,*)" because Language Systems Fortran on the
C            Macintosh interprets "PRINT *" in a non-standard manner.
C
C        7)  Use of the EXIST specifier was added to the INQUIRE
C            statement used to determine whether the file named by
C            DEVICE is open.  This is a work-around for a rather
C            peculiar behavior of at least one version of Sun Fortran:
C            files that don't exist may be considered to be open, as
C            indicated by the OPENED specifier of the INQUIRE statement.
C
C        8)  One other thing:  now that LJUST and UCASE are error-free,
C            WRLINE uses them; this simplifies the code.
C
C
C-    Beta Version 1.2.0, 27-FEB-1989 (NJB)
C
C        Call to GETLUN replaced by call to FNDLUN, which is error-free.
C        Call to IOERR replaced with in-line code to construct long
C        error message indicating file open failure. Arrangement of
C        declarations changed.  Keywords added. FILEN declaration
C        moved to "declarations" section.  Parameters section added.
C
C-    Beta Version 1.1.0, 06-OCT-1988 (NJB)
C
C        Upper bound of written substring changed to prevent use of
C        invalid substring bound.  Specifically, LASTNB ( LINE ) was
C        replaced by  MAX ( 1, LASTNB (LINE) ).  This upper bound
C        now used in the PRINT statement as well.
C
C-&
 
 
 
C
C     SPICELIB functions
C
      INTEGER               LTRIM
      INTEGER               RTRIM
 
C
C     Local variables
C
      CHARACTER*240         ERROR
      CHARACTER*11          ERRSTR
      CHARACTER*(FILEN)     TMPNAM
 
      INTEGER               IOSTAT
      INTEGER               UNIT
 
      LOGICAL               EXISTS
      LOGICAL               OPENED
 
 
C
C     Executable Code:
C
      CALL LJUST ( DEVICE, TMPNAM )
      CALL UCASE ( TMPNAM, TMPNAM )
 
C
C     TMPNAM is now left justified and is in upper case.
C
      IF ( TMPNAM .EQ. 'NULL' ) THEN
 
         RETURN
 
      ELSE IF ( TMPNAM .EQ. 'SCREEN' ) THEN
 
         WRITE ( *, FMT='(A)', IOSTAT=IOSTAT ) LINE(:RTRIM(LINE))
         RETURN
 
      END IF
 
C
C     Find out whether we'll need to open the file.
C
C     We use the EXIST inquiry specifier because files that don't exist
C     may be (possibly due to a Sun compiler bug) deemed to be OPEN by
C     Sun Fortran.
C
      INQUIRE ( FILE         = DEVICE( LTRIM(DEVICE): ),
     .          OPENED       = OPENED,
     .          EXIST        = EXISTS,
     .          NUMBER       = UNIT,
     .          IOSTAT       = IOSTAT   )
 
 
      IF ( IOSTAT .NE. 0 ) THEN
C
C        This is weird.  How can an INQUIRE statement fail,
C        if the syntax is correct?  But just in case...
C
         WRITE (*,*) 'SPICE(INQUIREFAILED)'
         WRITE (*,*) 'WRLINE: File = ', DEVICE, 'IOSTAT = ', IOSTAT
         RETURN
 
      END IF
 
 
      IF ( .NOT. ( OPENED .AND. EXISTS )  ) THEN
C
C        We will need a free logical unit.  There is always the chance
C        that no units are available.
C
         CALL FNDLUN ( UNIT )
 
         IF ( UNIT .LT. 1 ) THEN
 
            WRITE (*,*) 'SPICE(NOFREELOGICALUNIT)'
            WRITE (*,*) ' '
            WRITE (*,*) 'WRLINE: Maximum number of logical units '    //
     .                  'that can be allocated by SPICELIB has '      //
     .                  'already been reached'
            RETURN
 
         END IF
 
C
C        Okay, we have a unit. Open the file, and hope nothing
C        goes awry. (On the VAX, the qualifier
C
C           CARRIAGECONTROL = 'LIST'
C
C        may be inserted into the OPEN statement.)
C
 
         OPEN ( FILE            = DEVICE( LTRIM(DEVICE): ),
     .          UNIT            = UNIT,
     .          STATUS          = 'NEW',
     .          IOSTAT          = IOSTAT )
 
         IF ( IOSTAT .NE. 0 ) THEN
 
            WRITE (*,*) 'SPICE(FILEOPENFAILED)'
            WRITE (*,*) ' '
 
            ERROR = 'WRLINE: An error occurred while attempting to open'
 
            CALL SUFFIX ( DEVICE, 1, ERROR )
            CALL SUFFIX ( '.',    0, ERROR )
            CALL SUFFIX ( 'The value of IOSTAT returned was', 2,
     .                    ERROR  )
            CALL SUFFIX ( ':',    0, ERROR  )
            CALL INTSTR ( IOSTAT,    ERRSTR )
            CALL SUFFIX ( ERRSTR, 1, ERROR  )
            CALL SUFFIX ( '.',    0, ERROR  )
 
            WRITE (*,*) ERROR
            RETURN
 
         END IF
C
C        Whew! We're ready to write to this file.
C
      END IF
 
C
C     At this point, either we opened the file, or it was already
C     opened by somebody else.
C
C     This is the easy part. Write the next line to the file.
C
      WRITE ( UNIT, FMT='(A)', IOSTAT=IOSTAT ) LINE(:RTRIM(LINE))
 
C
C     Well, what happened? Any non-zero value for IOSTAT indicates
C     an error.
C
      IF ( IOSTAT .NE. 0 ) THEN
 
         ERROR = 'WRLINE: An error occurred while attempting to '     //
     .           'WRITE to '
 
         CALL SUFFIX ( DEVICE, 1, ERROR  )
         CALL SUFFIX ( '.',    0, ERROR  )
         CALL SUFFIX ( 'The value of IOSTAT returned was', 2,
     .                 ERROR  )
         CALL SUFFIX ( ':',    0, ERROR  )
         CALL INTSTR ( IOSTAT,    ERRSTR )
         CALL SUFFIX ( ERRSTR, 1, ERROR  )
         CALL SUFFIX ( '.',    0, ERROR  )
 
         WRITE (*,*) ERROR
 
         RETURN
 
      END IF
 
      RETURN
 
 
 
C$Procedure  CLLINE ( Close a device )
 
      ENTRY CLLINE ( DEVICE )
 
C$ Abstract
C
C      Close a device.
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
C      None.
C
C$ Keywords
C
C      TEXT, FILES, ERROR
C
C$ Declarations
C
C      CHARACTER*(*)        DEVICE
C
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      DEVICE     I   Device to be closed.
C
C$ Detailed_Input
C
C      DEVICE         is the name of a device which is currently
C                     opened for reading or writing.
C
C$ Detailed_Output
C
C      None.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C      This routine is called by SPICELIB error handling routines, so
C      it cannot use the normal SPICELIB error signalling mechanism.
C      Instead, it writes error messages to the screen if necessary.
C
C      1)  If the device indicated by DEVICE was not opened by WRLINE,
C          this routine closes it anyway.
C
C      2)  If the INQUIRE performed by this routine fails, an error
C          diagnosis is printed to the screen.
C
C$ Files
C
C      This routin
C
C$ Particulars
C
C      CLLINE closes a device that is currently open.
C
C$ Examples
C
C      1)  Write two lines to the file, SPUD.DAT (VAX file name
C          syntax), and then close the file.
C
C          CALL WRLINE ( 'SPUD.DAT', ' This is line 1 ' )
C          CALL WRLINE ( 'SPUD.DAT', ' This is line 2 ' )
C          CALL CLLINE ( 'SPUD.DAT' )
C
C$ Restrictions
C
C      None.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      N.J. Bachman    (JPL)
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
C        environments are now explicitely given.  New
C        environments are WIN-NT
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
C-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 2.0.0, 26-MAR-1991 (NJB)
C
C        All occurrences of "PRINT *" have been replaced by
C        "WRITE (*,*)".
C
C        Also, this routine now closes the device named by DEVICE
C        whether or not the device was opened by WRLINE.
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
C-    SPICELIB Version 2.0.0, 26-MAR-1991 (NJB)
C
C        All instances if "PRINT *" have been replaced by "WRITE (*,*)"
C        because Language Systems Fortran on the Macintosh interprets
C        "PRINT *" in a non-standard manner.
C
C        This routine no longer has to maintain the file database, since
C        WRLINE does not use it any more.
C
C        Also, this routine now closes the device named by DEVICE,
C        whether or not the device was opened by WRLINE.
C
C-    Beta Version 1.0.1, 08-NOV-1988 (NJB)
C
C        Keywords added.
C-&
 
 
C
C     Find the unit connected to DEVICE.
C
      INQUIRE ( FILE         = DEVICE( LTRIM(DEVICE): ),
     .          NUMBER       = UNIT,
     .          IOSTAT       = IOSTAT   )
 
      IF ( IOSTAT .NE. 0 ) THEN
C
C        This is weird.  How can an INQUIRE statement fail,
C        if the syntax is correct?  But just in case...
C
         WRITE (*,*) 'SPICE(INQUIREFAILED)'
         WRITE (*,*) 'CLLINE:  File = ', DEVICE, 'IOSTAT = ', IOSTAT
         RETURN
 
      END IF
 
      CLOSE ( UNIT )
 
      RETURN
      END
