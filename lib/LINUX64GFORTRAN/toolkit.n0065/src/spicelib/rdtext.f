C$Procedure      RDTEXT ( Read a line from a text file )
 
      SUBROUTINE RDTEXT ( FILE, LINE, EOF )
 
C$ Abstract
C
C     Read the next line of text from a text file.
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
 
      CHARACTER*(*)       FILE
      CHARACTER*(*)       LINE
      LOGICAL             EOF
 
      INTEGER             MAXLEN
      PARAMETER         ( MAXLEN = 255 )
 
      INTEGER             MAXOPN
      PARAMETER         ( MAXOPN =  96 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  ---------------------------------------------------
C     FILE       I   Name of text file.
C     LINE       O   Next line from the text file.
C     EOF        O   End-of-file indicator.
C     MAXOPN     P   Maximum number of open files.
C     MAXLEN     P   Maximum file name length.
C
C$ Detailed_Input
C
C     FILE        is the name of the text file from which the next
C                 line is to be read. If the file is not currently
C                 open, it is opened with a logical unit determined
C                 at run time, and the first line of the file is
C                 returned. Otherwise, the next line not yet read
C                 from the file is read and returned.
C
C$ Detailed_Output
C
C     LINE        is next line of text in the specified file.
C                 If the end of the file is reached, LINE is blank.
C
C     EOF         is true when the end of the file is reached, and is
C                 otherwise false.
C
C$ Parameters
C
C     MAXOPN      is the maximum number of files that can be kept
C                 open simultaneously by RDTEXT.
C
C                 VAX:
C
C                  The default number of files that can be open at one
C                  time during a user's process is determined by the
C                  value of FILLM. This number is usually 20, but it
C                  may be changed by a user with sufficient privileges.
C
C                 IBM PC / Microsoft FORTRAN 5.0:
C
C                  The default value for the maximum number of files
C                  open at one time is 20. This value may be changed
C                  by modifying the appropriate startup files as
C                  specified in the reference documentation.
C
C                 IBM PC / Linux / Fort77:
C
C                  An experiment showed that a program can
C                  simultaneiously open one file for each available
C                  logical unit; this amounts to 96 files.
C
C                 Sun / Sun FORTRAN:
C
C                  "The maximum number of logical units that a program
C                  can have open at one time is the same as the SunOS
C                  system limit, currently 64."
C
C                 HP-UX 9000/750, FORTRAN/9000 Series 700 computers and
C                 Silicon Graphics:
C
C                  NAIF used a program to determine this value. Also,
C                  the values can be found by executing the command
C                  "man limits" and reading the value for OPEN_MAX.
C                  This value is listed as 60, but two units are used
C                  for standard output and standard error.
C
C                 DEC Alpha-OSF/1:
C
C                  The comment in the output from the command
C                  "man limits" stated that the value of OPEN_MAX was
C                  64, but that it was "OBSOLETE, sysconf() interface
C                  should be used". Looking into sysconf did not produce
C                  any numbers, so the value is set at 20 because it
C                  works!
C
C                 NeXT/Absoft Fortran:
C
C                  We couldn't find any documentation that addressed
C                  this value, so we set it to 20.
C
C
C     MAXLEN      is the maximum length of the file names that may
C                 used to identify the files opened by RDTEXT.
C
C$ Exceptions
C
C     1) If too many files are open already, the error
C        SPICE(TOOMANYFILESOPEN) is signalled.
C
C     2) If the attempt to open the file fails, the error
C        SPICE(FILEOPENFAILED) is signalled.
C
C     3) If the attempt to read from the file fails, the error
C        SPICE(FILEREADFAILED) is signalled.
C
C     4) If the attempt to "inquire" the status of the file fails,
C        the error SPICE(INQUIREFAILED) is signalled.
C
C$ Files
C
C     See input FILE.
C
C$ Particulars
C
C     RDTEXT reads the next line from a text file. If the file is
C     not currently open, it is opened with a logical unit determined
C     at run time, and the first line of the file is returned.
C     Otherwise, the next line not yet read from the file is returned.
C
C     If the end of the file is reached, a blank line is returned,
C     the end-of-file indicator is true, and the file is closed.
C
C     Several files may be opened and read simultaneously. Thus,
C     you may begin reading from one file before the end of another
C     file has been reached. RDTEXT maintains a separate file pointer
C     for each file.
C
C$ Examples
C
C     Let FILE.1 contain the following lines.
C
C        Mary had a little lamb
C        Everywhere that Mary went
C
C     Let FILE.2 contain the following lines.
C
C        Its fleece was white as snow.
C        The lamb was sure to go.
C
C     Then the code fragment
C
C        DO I = 1, 2
C           CALL RDTEXT ( 'FILE.1', LINE, EOF )
C           WRITE (6,*) LINE
C
C           CALL RDTEXT ( 'FILE.2', LINE, EOF )
C           WRITE (6,*) LINE
C        END DO
C
C     produces the following output
C
C        Mary had a little lamb
C        Its fleece was white as snow.
C        Everywhere that Mary went
C        The lamb was sure to go.
C
C$ Restrictions
C
C     1) The values of MAXOPN and MAXLEN should not exceed any
C        corresponding limits imposed by the operating system.
C
C     2) If the input file is a print file, the carriage control
C        character at the beginning of a given line will be considered
C        part of the line. (Text files have no carriage control
C        characters.)
C
C     3) In order to avoid access violations, the VAX/VMS version of
C        RDTEXT uses the VAX READONLY qualifier to open files. This
C        must be removed or replaced when the routine is ported to
C        non-VAX/VMS systems.
C
C     4)  On VAX systems, caution should be exercised when using
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
C     1. "VAX/VMS Guide to VAX/VMS System Management and Daily
C        Operations", Digital Equipment Corporation, September 1984,
C        Section 6.1.7, page 6-6.
C
C     2. "Microsoft FORTRAN Reference", Microsoft Corporation
C        1989, Section C.3, page 404.
C
C     3. "Sun FORTRAN Programmer's Guide", Sun Microsystems,
C        Revision A of 6 May 1988, Section 7.2, page 73.
C
C     4. The Unix Man Pages for limits on the HP and Silicon Graphics.
C        The value of OPEN_MAX refers to the number of files a process
C        can have open.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     H.A. Neilan     (JPL)
C     M.J. Spencer    (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 6.25.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    SPICELIB Version 6.24.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    SPICELIB Version 6.23.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    SPICELIB Version 6.22.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    SPICELIB Version 6.21.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GCC_C.
C
C-    SPICELIB Version 6.20.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    SPICELIB Version 6.19.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-CC_C.
C
C-    SPICELIB Version 6.18.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
C
C-    SPICELIB Version 6.17.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-    SPICELIB Version 6.16.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    SPICELIB Version 6.15.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 6.14.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-64BIT-MS_C.
C
C-    SPICELIB Version 6.13.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-INTEL_C.
C
C-    SPICELIB Version 6.12.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    SPICELIB Version 6.11.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 6.10.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    SPICELIB Version 6.9.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    SPICELIB Version 6.8.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    SPICELIB Version 6.7.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-LINUX-64BIT-GCC_C.
C
C-    SPICELIB Version 6.6.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-INTEL_C.
C
C-    SPICELIB Version 6.5.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    SPICELIB Version 6.4.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-    SPICELIB Version 6.3.0, 26-OCT-2005 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-GCC_C.
C
C-    SPICELIB Version 6.2.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN_C.
C
C-    SPICELIB Version 6.1.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    SPICELIB Version 6.0.6, 24-APR-2003 (EDW)
C
C        Added MAC-OSX-F77 to the list of platforms
C        that require READONLY to read write protected
C        kernels.
C
C-    SPICELIB Version 6.0.5, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 6.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 6.0.3, 16-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    SPICELIB Version 6.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 6.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 6.0.0, 07-APR-1998 (NJB)
C
C        Module was updated for the PC-LINUX platform.
C
C-    SPICELIB Version 5.0.0, 9-NOV-1993 (HAN)
C
C         Module was updated to include the values for MAXLEN and
C         MAXOPN and the appropriate OPEN statement for the Silicon
C         Graphics, DEC Alpha-OSF/1, and NeXT platforms. The previous
C         value of 256 for Unix platforms was changed to 255.
C
C-    SPICELIB Version 4.1.0, 12-OCT-1992 (HAN)
C
C        Module was updated to include the parameters for the
C        Hewlett Packard UX 9000/750 environment.
C
C-    SPICELIB Version 4.0.0, 20-MAY-1992 (MJS)
C
C        INDEX saved.
C
C-    SPICELIB Version 3.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 3.0.0, 19-JUL-1991 (NJB)
C
C        Version 2.0.0 of RDTEXT produced a Fortran run-time error
C        if the input argument FILE was blank.  This has been
C        repaired.
C
C-    SPICELIB Version 2.0.0, 26-MAR-1991 (MJS) (NJB)
C
C        Value of N was initialized to zero. LINE is now filled
C        with blanks when an error occurs or when an end of file
C        is reached. Some small fix-ups in the header, including
C        re-ordering the sections correctly.
C
C-    SPICELIB Version 1.0.1, 20-MAR-1990 (HAN)
C
C        Parameters section was updated to include the values
C        of MAXOPN for several machines. Sources of these values
C        are listed in the Literature References section.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     read a line from a text file
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 6.0.0, 07-APR-1998 (NJB)
C
C        Module was updated for the PC-LINUX platform.
C
C-    SPICELIB Version 5.0.0, 9-NOV-1993 (HAN)
C
C         Module was updated to include the values for MAXLEN and
C         MAXOPN and the appropriate OPEN statement for the Silicon
C         Graphics, DEC Alpha-OSF/1, and NeXT platforms. The previous
C         value of 256 for Unix platforms was changed to 255.
C
C-    SPICELIB Version 4.1.0, 12-OCT-1992 (HAN)
C
C        Module was updated to include the parameters for the
C        Hewlett Packard UX 9000/750 environment.
C
C        The code was also reformatted so that a utility program can
C        create the source file for a specific environment given a
C        master source file.
C
C-    SPICELIB Version 4.0.0, 26-MAY-1992 (MJS)
C
C        The variable INDEX was saved. Prior to this fix, when RDTEXT
C        closed a file, INDEX was used without being assigned a value.
C        Since INDEX always points to the current file (unit), saving
C        INDEX fixed this problem.
C
C-    SPICELIB Version 3.0.0, 19-JUL-1991 (NJB)
C
C        Version 2.0.0 of RDTEXT produced a Fortran run-time error
C        if the input argument FILE was blank.  This has been
C        repaired.
C
C-    SPICELIB Version 2.0.0, 26-MAR-1991 (MJS) (NJB)
C
C       In past versions when an end of file was reached or when error
C       occured while reading the text file, LINE was returned with
C       its previous value. Now LINE is returned with blanks, in
C       accordance with the specifications given in the header.
C       The variable N, representing the number of files currently
C       open, was initialized to zero.
C
C       The method of checking whether the file to be read is one
C       already opened for reading by this routine has been improved.
C       Formerly, the input file name was compared against a list of
C       names of routines already opened by RDTEXT.  If the input name
C       pointed to a file that had been opened using a different name,
C       RDTEXT would not recognize that the new name pointed to a file
C       that was already open.  The technique used now greatly reduces
C       the chance of such an error.  The input file name is compared
C       to the previous input file name, and if the names do not agree,
C       an INQUIRE is performed to test whether the file named by the
C       input file name is already open.  Only if this INQUIRE
C       indicates that the file is not already open will RDTEXT attempt
C       to open the file.
C
C-    Beta Version 1.1.0, 17-FEB-1989 (IMU) (NJB)
C
C       The primary change was the addition of error handling.
C       At the same time, the parameters MAXOPN and MAXLEN were
C       moved into the calling sequence.  The call to IOERR was
C       replaced by a call to SETMSG.  The declaration of the unused
C       function FAILED was deleted. Finally, all internal references
C       to the entry point WRTEXT (which was dropped when the routine
C       left OPTLIB) were removed.
C
C
C-&
 
 
 
C
C     SPICELIB functions
C
      INTEGER             ISRCHI
      LOGICAL             RETURN
 
C
C     Local variables
C
      CHARACTER*(MAXLEN)  LSTFIL
      INTEGER             UNITS   ( MAXOPN )
      INTEGER             LSTUNT
      INTEGER             UNIT
 
      INTEGER             N
      INTEGER             NUMBER
      INTEGER             INDEX
      INTEGER             IOSTAT
      INTEGER             I
 
      LOGICAL             SAME
 
C
C     Save the names of the files, their associated logical units, and
C     the number of files opened.
C
      SAVE                UNITS
      SAVE                N
      SAVE                LSTFIL
      SAVE                LSTUNT
      SAVE                INDEX
 
C
C     Initial values
C
      DATA                N          /  0  /
      DATA                LSTFIL     / ' ' /
 
 
 
C
C     Set up the error processing.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'RDTEXT' )
      END IF
 
C
C     We will keep track of which files are open by storing the unit
C     numbers of those files. When a user requests a file to be read,
C     we first check if it is the same file as just previously read, if
C     not we use an INQUIRE statement to determine the open status and
C     unit number of the file. If the file is open we'll read it, if
C     not, well, we'll open it first. We could just skip the first
C     part, that is just use the INQUIRE statement, but that would
C     involve executing quite a few INQUIRE statements when just
C     reading one file and making this routine a much slower routine.
C
 
C
C     Are we reading the same file?
C
      SAME = ( LSTFIL .EQ. FILE )  .AND. ( LSTFIL .NE. ' ' )
 
      IF ( .NOT. SAME ) THEN
 
C
C        We still might have the same file. For example these three
C        names (on the VAX) are different but they represent the
C        same file:
C
C           1) MY$DISK:[MYDIR]MYFILE.DAT;
C
C           2) MYFILE.DAT;1
C
C           3) MYFILE.DAT
C
C        In other words, the user may have entered a different file
C        specification for the same file.
C
         NUMBER = 0
 
         INQUIRE (  FILE    =  FILE,
     .              NUMBER  =  NUMBER,
     .              IOSTAT  =  IOSTAT  )
 
         IF ( IOSTAT .NE. 0 ) THEN
C
C           This is weird.  How can an INQUIRE statement fail,
C           if the syntax is correct?  But just in case...
C
            CALL SETMSG ( 'INQUIRE error.  File = #, IOSTAT = #.' )
            CALL ERRCH  ( '#', FILE                               )
            CALL ERRINT ( '#', IOSTAT                             )
            CALL SIGERR ( 'SPICE(INQUIREFAILED)'                  )
            CALL CHKOUT ( 'RDTEXT'                                )
            RETURN
 
         END IF
 
         INDEX = ISRCHI ( NUMBER, N, UNITS )
 
         IF ( INDEX .EQ. 0 ) THEN
C
C           Well, we will treat it as a new file then.  We will
C           need a free logical unit. But only if we don't
C           have too many files open already.
C
            IF ( N .EQ. MAXOPN ) THEN
               CALL SETMSG ( 'Too many files open already.' )
               CALL SIGERR ( 'SPICE(TOOMANYFILESOPEN)'      )
               CALL CHKOUT ( 'RDTEXT' )
               RETURN
            ELSE
               CALL GETLUN ( UNIT )
            END IF
 
C
C           Okay, we have a unit. Open the file, and hope nothing
C           goes awry. The READONLY qualifier is nonstandard, but
C           helpful where allowed. (Standard disclaimer.)
C
 
            OPEN ( FILE            = FILE,
     .             UNIT            = UNIT,
     .             STATUS          = 'OLD',
     .             IOSTAT          = IOSTAT )
 
            IF ( IOSTAT .NE. 0 ) THEN
               CALL SETMSG ( 'Could not open #.'     )
               CALL ERRCH  ( '#', FILE               )
               CALL SIGERR ( 'SPICE(FILEOPENFAILED)' )
               CALL CHKOUT ( 'RDTEXT' )
               RETURN
            END IF
 
C
C           Whew! We're ready to read from this file. Save
C           the pertinent information:
C
C               - The number of files currently open.
C               - The logical unit connected to this file.
C               - The index of the file within the UNITS array.
C
            N         = N + 1
            UNITS(N)  = UNIT
            INDEX     = N
 
         END IF
 
         LSTFIL = FILE
         LSTUNT = UNITS(INDEX)
 
      END IF
 
 
 
 
 
C
C     This is the easy part. Read the next line from the file.
C
      READ (LSTUNT,FMT='(A)',IOSTAT=IOSTAT) LINE
 
C
C     Well, what happened? An end-of-file condition is indicated by
C     a negative value for IOSTAT. Any other non-zero value indicates
C     some other error. In any event, close the file immediately.
C     Repack the UNITS array, so that subsequent calls will not try to
C     read from the file without reopening it.
C
      EOF = ( IOSTAT .LT. 0 )
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         CLOSE ( UNITS(INDEX) )
 
         DO I = INDEX+1, N
            UNITS(I-1)  = UNITS(I)
         END DO
 
         N = N - 1
 
C
C        Fill LINE with blanks.
C
         LINE = ' '
 
C
C        LSTFIL is no longer valid
C
         LSTFIL = ' '
 
C
C        If this is just the end of the file, don't report an error.
C        (All files have to end sometime.)
C
         IF ( .NOT. EOF ) THEN
            CALL SETMSG ( 'Could not read from #.' )
            CALL ERRCH  ( '#', FILE                )
            CALL SIGERR ( 'SPICE(FILEREADFAILED)'  )
            CALL CHKOUT ( 'RDTEXT' )
            RETURN
         END IF
 
      END IF
 
      CALL CHKOUT ( 'RDTEXT' )
      RETURN
 
 
 
 
C$Procedure  CLTEXT ( Close a text file opened by RDTEXT)
 
      ENTRY  CLTEXT ( FILE )
 
C$ Abstract
C
C     Close a text file currently opened by RDTEXT.
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
C     FILES,  TEXT
C
C$ Declarations
C
C     None.
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FILE       I   Text file to be closed.
C
C$ Detailed_Input
C
C     FILE        is the name of a text file which is currently
C                 opened for reading or writing by RDTEXT.
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
C     1) If the attempt to "inquire" the status of the file fails,
C        the error SPICE(INQUIREFAILED) is signalled.
C
C$ Files
C
C     The text file, FILE, was previously opened by RDTEXT.
C
C$ Particulars
C
C     CLTEXT closes one of the files currently opened for reading or
C     writing by RDTEXT. If the specified file is not open, nothing
C     happens.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     1)  On VAX systems, caution should be exercised when using
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
C     W.L. Taber      (JPL)
C     N.J. Bachman    (JPL)
C     M.J. Spencer    (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 6.0.5, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 6.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 6.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 6.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 3.0.0, 27-SEP-1994 (WLT)
C
C        The check of RETURN was removed so that routines that need
C        to close a text file can do so even if an error has been
C        detected somewhere else in a user's program.
C
C-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 2.0.0, 26-MAR-1991 (MJS) (NJB)
C
C        Method of recognizing whether input file name points to
C        a file opened by RDTEXT has been improved.  Header indentation
C        fixed.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     close a text file opened by rdtext
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 3.0.0, 27-SEP-1994 (WLT)
C
C        The check of RETURN was removed so that routines that need
C        to close a text file can do so even if an error has been
C        detected somewhere else in a user's program.
C
C-    SPICELIB Version 2.0.0, 26-MAR-1991 (MJS) (NJB)
C
C        Method of recognizing whether input file name points to
C        a file opened by RDTEXT has been improved.  Formerly, CLTEXT
C        compared the input file name to a list of names of files
C        opened by RDTEXT.  If the input name pointed to a file that
C        had been opened using a different name, CLTEXT would not
C        recognize that the new name pointed to a file that was already
C        open.  The technique used now greatly reduces the chance of
C        such an error.  Now, and INQUIRE is performed to obtain the
C        unit number attached to the file named by the input file name.
C        If this unit is attached to a file opened by RDTEXT, CLTEXT
C        will close that file.
C
C        Header indentation was fixed.
C
C
C-    Beta Version 1.1.0, 8-JAN-1989 (IMU)
C
C        References to WRTEXT removed.
C
C-&
 
 
 
C
C     Set up the error processing.
C
      CALL CHKIN ( 'CLTEXT' )
 
 
C
C     Which file?
C
      NUMBER = 0
 
      INQUIRE (  FILE    =  FILE,
     .           NUMBER  =  NUMBER,
     .           IOSTAT  =  IOSTAT   )
 
      IF ( IOSTAT .NE. 0 ) THEN
C
C        This is weird.  How can an INQUIRE statement fail,
C        if the syntax is correct?  But just in case...
C
         CALL SETMSG ( 'INQUIRE error.  File = #, IOSTAT = #.' )
         CALL ERRCH  ( '#', FILE                               )
         CALL ERRINT ( '#', IOSTAT                             )
         CALL SIGERR ( 'SPICE(INQUIREFAILED)'                  )
         CALL CHKOUT ( 'CLTEXT'                                )
         RETURN
 
      END IF
 
      INDEX = ISRCHI ( NUMBER, N, UNITS )
 
      IF ( INDEX .GT. 0 ) THEN
 
         CLOSE ( UNITS(INDEX) )
 
         IF ( UNITS(INDEX) .EQ. LSTUNT ) THEN
            LSTFIL = ' '
         END IF
 
C
C        Remember all that salient information about the file?
C        Lose it.
C
         DO I = INDEX+1, N
            UNITS(I-1)  = UNITS(I)
         END DO
 
         N = N - 1
 
 
      END IF
 
      CALL CHKOUT ( 'CLTEXT' )
 
      RETURN
      END
