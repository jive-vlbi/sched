C$Procedure ZZGETFAT ( Get file architecture, type, and unit )
 
      SUBROUTINE ZZGETFAT ( FILE, ARCH, TYPE, NUMBER )
 
C$ Abstract
C
C     Determine the file architecture and file type of most SPICE kernel
C     files.
C
C     NOTE: This routine is currently for use ONLY with the SPACIT
C           and TOBIN utility programs. Use it at your own risk.
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
C     KERNEL
C     UTILITY
C
C$ Declarations
 
      IMPLICIT NONE
 
      CHARACTER*(*)         FILE
      CHARACTER*(*)         ARCH
      CHARACTER*(*)         TYPE
      INTEGER               NUMBER
 
C
C     The record length should be big enough to hold 128 double
C     precision numbers.
C
C     For some environments, record length is measured in longwords,
C     since our records are unformatted, with two longwords per double
C     precision number. The value of RECL is 256.
C
C     Environment: VAX/VMS, VAX FORTRAN
C     Source:      Programming in VAX Fortran
C
C     Environment: Silicon Graphics IRIX OS, SGI FORTRAN 77
C     Source:      NAIF Program
C
C     Environment: DEC Alpha 3000/4000, OSF/1, DEC FORTRAN-77
C     Source:      NAIF Program
C
C     For the following environments, record length is measured in
C     characters (bytes) with eight characters per double precision
C     number. The value of RECL is 1024.
C
C     Environment: Sun, Sun FORTRAN
C     Source:      Sun Fortran Programmer's Guide
C
C     Environment: PC, MS FORTRAN
C     Source:      Microsoft Fortran Optimizing Compiler User's Guide
C
C     Environment: Macintosh, Language Systems FORTRAN
C     Source:      Language Systems FORTRAN Reference Manual,
C                  Version 1.2, page 12-7
C
C     Environment: PC, Lahey F77 EM/32 Version 4.0
C     Source:      Lahey F77 EM/32 Language Reference Manual,
C                  page 144
C
C     Environment: HP-UX 9000/750, FORTRAN/9000 Series 700 computers
C     Source:      FORTRAN/9000 Reference-Series 700 Computers,
C                  page 5-110
C
C     Environment: NeXT/Mach OS, Absoft Fortran
C     Source:      NAIF Program
C
 
      INTEGER               RECL
      PARAMETER           ( RECL   = 1024 )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      FILE       I   The name of a file to be examined.
C      ARCH       O   The architecture of the kernel file.
C      TYPE       O   The type of the kernel file.
C      NUMBER     O   The logical unit number for the open file FILE.
C
C$ Detailed_Input
C
C     FILE        is the name of a SPICE kernel file whose architecture
C                 and type are desired. This file must be closed when
C                 this routine is called.
C
C$ Detailed_Output
C
C     ARCH        is the file architecture of the SPICE kernel file
C                 specified be FILE. If the architecture cannot be
C                 determined or is not recognized the value '?' is
C                 returned.
C
C                 Architectures currently recognized are:
C
C                    DAF - The file is based on the DAF architecture.
C                    DAS - The file is based on the DAS architecture.
C                    XFR - The file is in a SPICE transfer file format.
C                    DEC - The file is an old SPICE decimal text file.
C                    ASC -- An ASCII text file.
C                    KPL -- Kernel Pool File (i.e., a text kernel)
C                    TXT -- An ASCII text file.
C                    TE1 -- Text E-Kernel type 1.
C                     ?  - The architecture could not be determined.
C
C                 This variable must be at least 3 characters long.
C
C     TYPE        is the type of the SPICE kernel file. If the type
C                 can not be determined the value '?' is returned.
C
C                 Kernel file types may be any sequence of at most four
C                 printing characters. NAIF has reserved for its use
C                 types which contain all upper case letters.
C
C                 A file type of 'PRE' means that the file is a
C                 pre-release file.
C
C                 This variable may be at most 4 characters long.
C
C     NUMBER      The logical unit number assigned to the file FILE
C                 when opened.  An inyteger, returned to the calling
C                 routine.
C
C$ Parameters
C
C     RECL        is the record length of a binary kernel file. Each
C                 record must be large enough to hold 128 double
C                 precision numbers. The units in which the record
C                 length must be specified vary from environment to
C                 environment. For example, VAX Fortran requires
C                 record lengths to be specified in longwords,
C                 where two longwords equal one double precision
C                 number.
C
C$ Exceptions
C
C      1)  If the inquire on the filename specified by FILE fails for
C          some reason, the error SPICE(INQUIREERROR) will be signalled.
C
C      2)  If the file specified by FILE is already open, the error
C          SPICE(FILECURRENTLYOPEN) will be signalled.
C
C      3)  If the file specified by FILE does not exist, the error
C          SPICE(NOSUCHFILE) will be signalled.
C
C      4)  If the attempt to open the file specified by FILE fails, the
C          error SPICE(FILEOPENFAILED) will be signalled.
C
C      5)  If all attempts to open the file specified by FILE fail, the
C          error SPICE(FILEOPENFAILED) will be signalled.
C
C      6)  If all attempts to read from the file specified be FILE
C          fail, the error SPICE(FILEREADFAILED) will be signalled.
C
C$ Files
C
C     The SPICE kernel file specified by FILE is opened and then
C     closed by this routine to determine its file architecture and
C     type. Names of open files should not be passed to this routine.
C
C$ Particulars
C
C     This subroutine is a support utility routine that determines the
C     architecture and type of a SPICE kernel file.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     This routine should only be called as part of spacit or tobin
C     by spat2b.
C
C     The file to be examined must be closed when this routine is
C     invoked.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer  (JPL)
C     H.A. Neilan     (JPL)
C     W.L. Taber      (JPL)
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    Beta Version 1.25.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    Beta Version 1.24.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    Beta Version 1.23.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    Beta Version 1.22.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    Beta Version 1.21.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GCC_C.
C
C-    Beta Version 1.20.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    Beta Version 1.19.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-CC_C.
C
C-    Beta Version 1.18.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
C
C-    Beta Version 1.17.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-    Beta Version 1.16.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    Beta Version 1.15.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    Beta Version 1.14.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-64BIT-MS_C.
C
C-    Beta Version 1.13.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-INTEL_C.
C
C-    Beta Version 1.12.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    Beta Version 1.11.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    Beta Version 1.10.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    Beta Version 1.9.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    Beta Version 1.8.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    Beta Version 1.7.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-LINUX-64BIT-GCC_C.
C
C-    Beta Version 1.6.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-INTEL_C.
C
C-    Beta Version 1.5.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    Beta Version 1.4.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-    Beta Version 1.3.0, 26-OCT-2005 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-GCC_C.
C
C-    Beta Version 1.2.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN_C.
C
C-    Beta Version 1.1.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    Beta Version 1.0.3, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    Beta Version 1.0.2, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    Beta Version 1.0.1, 21-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    Beta Version 1.0.0, 19-MAR-1999 (EDW)
C
C        This routine is a modification of the GETFAT routine.
C        Both have the same basic functionality, but this routine
C        will ignore all data until a known NAIF file identifier
C        is found.  The derivation of file type and architecture
C        proceeds as in GETFAT.  Note:  the file is not closed
C        on exit.
C
C        The logic for the case architecture = DAF, type = unknown, '?',
C        has been removed.
C
C-&
 
C
C$ Index_Entries
C
C     determine the architecture and type of a kernel file
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local parameters
C
C
C     The following parameters point to the various slots in the
C     integer portion of the DAF descriptor where the values are
C     located.
C
      INTEGER               BODID
      PARAMETER           ( BODID  =  1 )
 
      INTEGER               CENID
      PARAMETER           ( CENID  = BODID  + 1 )
 
      INTEGER               SPKFRM
      PARAMETER           ( SPKFRM = CENID  + 1 )
 
      INTEGER               SPKTYP
      PARAMETER           ( SPKTYP = SPKFRM + 1 )
 
      INTEGER               START
      PARAMETER           ( START  = SPKTYP + 1 )
 
      INTEGER               FINISH
      PARAMETER           ( FINISH = START  + 1 )
 
      INTEGER               CKRATE
      PARAMETER           ( CKRATE = SPKTYP     )
 
C
C     These parameters give the number of integer and double precision
C     components of the descriptor for SPK and CK files.
C
      INTEGER               ND
      PARAMETER           ( ND = 2 )
 
      INTEGER               NI
      PARAMETER           ( NI = 6 )
C
C     The size of a summary.
C
      INTEGER               SUMSIZ
      PARAMETER           ( SUMSIZ = ND + ((NI+1)/2) )
 
C
C     Set the length of a SPICE kernel file ID word.
C
      INTEGER               IDLEN
      PARAMETER           ( IDLEN = 12 )
C
C     Set minimum and maximum values for the range of ASCII printing
C     characters.
C
      INTEGER               MINPCH
      PARAMETER           ( MINPCH = 32 )
 
      INTEGER               MAXPCH
      PARAMETER           ( MAXPCH = 126 )
C
C     Local Variables
C
      CHARACTER*(IDLEN)     IDWORD
      CHARACTER*(IDLEN)     TMPWRD
 
 
      INTEGER               I
      INTEGER               IOSTAT
 
      LOGICAL               DIROPN
      LOGICAL               SEQOPN
      LOGICAL               CHECK
      LOGICAL               EXIST
      LOGICAL               OPENED
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN  ( 'ZZGETFAT' )
      END IF
 
C
C     Initialize the temporary storage variables that we use.
C
      IDWORD = ' '
      SEQOPN = .FALSE.
      CHECK  = .TRUE.
 
 
C
C     If the filename we got is blank, signal an error and return.
C
      IF ( FILE .EQ. ' ' ) THEN
 
         CALL SETMSG ( 'The file name is blank.' )
         CALL SIGERR ( 'SPICE(BLANKFILENAME)'    )
         CALL CHKOUT ( 'ZZGETFAT'                )
         RETURN
 
      END IF
 
C
C     We'll do a bit of inquiring before we try opening anything.
C
      INQUIRE ( FILE   = FILE,
     .          EXIST  = EXIST,
     .          IOSTAT = IOSTAT,
     .          OPENED = OPENED )
 
C
C     Not too likely, but if the INQUIRE statement fails...
C
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL SETMSG ( 'IOSTAT error in INQUIRE statement. IOSTAT '   //
     .                 '= #.'                                          )
         CALL ERRINT ( '#', IOSTAT                                     )
         CALL SIGERR ( 'SPICE(INQUIREERROR)'                           )
         CALL CHKOUT ( 'ZZGETFAT'                                      )
         RETURN
 
      END IF
C
C     Note: the following two tests MUST be performed in the order in
C           which they appear, since in some environments files that do
C           not exist are considered to be open.
C
C     By calling this routine, the user implies that the file exists.
C
      IF ( .NOT. EXIST ) THEN
 
         CALL SETMSG ( 'The kernel file ''#'' does not exist.'         )
         CALL ERRCH  ( '#', FILE                                       )
         CALL SIGERR ( 'SPICE(NOSUCHFILE)'                             )
         CALL CHKOUT ( 'ZZGETFAT'                                      )
         RETURN
 
      END IF
 
C
C     This routine should not be called if the file is already open.
C
      IF ( OPENED ) THEN
 
         CALL SETMSG ( 'The kernel file ''#'' is already open.'        )
         CALL ERRCH  ( '#', FILE                                       )
         CALL SIGERR ( 'SPICE(FILECURRENTLYOPEN)'                      )
         CALL CHKOUT ( 'ZZGETFAT'                                      )
         RETURN
 
      END IF
 
C
C     Open the file with a record length of RECL (the length of the
C     DAF and DAS records). We assume, for now, that opening the file as
C     a direct access file will work.
C
 
      DIROPN = .TRUE.
 
      CALL GETLUN ( NUMBER )
 
      OPEN ( UNIT               = NUMBER,
     .       FILE               = FILE,
     .       ACCESS             = 'DIRECT',
     .       RECL               = RECL,
     .       STATUS             = 'OLD',
     .       IOSTAT             = IOSTAT    )
 
C
C     If we had trouble opening the file, try opening it as a sequential
C     file.
C
      IF ( IOSTAT .NE. 0 ) THEN
 
         DIROPN = .FALSE.
 
         OPEN ( UNIT            = NUMBER,
     .          FILE            = FILE,
     .          ACCESS          = 'SEQUENTIAL',
     .          STATUS          = 'OLD',
     .          IOSTAT          = IOSTAT    )
C
C        If we still have problems opening the file, we don't have a
C        clue about the file architecture and type.
C
         IF ( IOSTAT .NE. 0 ) THEN
 
            ARCH = '?'
            TYPE = '?'
            CALL SETMSG ( 'Attempt to open the file ''#'' failed.'  //
     .                 ' IOSTAT = #.'                                )
            CALL ERRCH  ( '#', FILE                                  )
            CALL ERRINT ( '#', IOSTAT                                )
            CALL SIGERR ( 'SPICE(FILEOPENFAILED)'                    )
            CALL CHKOUT ( 'ZZGETFAT'                                 )
            RETURN
 
         END IF
 
      END IF
 
C
C     We opened the file successfully, so let's try to read from the
C     file. We need to be sure to use the correct form of the read
C     statement, depending on whether the file was opened with direct
C     acces or sequential access.
C
      IF ( DIROPN ) THEN
 
         READ ( NUMBER, REC=1, IOSTAT=IOSTAT ) TMPWRD
C
C        If we couldn't read from the file as a direct access file with
C        a fixed record length, then try to open the file as a
C        sequential file and read from it.
C
         IF ( IOSTAT .EQ. 0 ) THEN
 
            SEQOPN = .TRUE.
            DIROPN = .FALSE.
 
            CLOSE ( NUMBER )
 
            OPEN ( UNIT   = NUMBER,
     .             FILE   = FILE,
     .             ACCESS = 'SEQUENTIAL',
     .             STATUS = 'OLD',
     .             IOSTAT = IOSTAT    )
C
C           If we could not open the file, we don't have a clue about
C           the file architecture and type.
C
            IF ( IOSTAT .NE. 0 ) THEN
 
               ARCH = '?'
               TYPE = '?'
               CALL SETMSG ( 'Attempt to open the file ''#''' //
     .                       ' failed. IOSTAT = #.'            )
               CALL ERRCH  ( '#', FILE                         )
               CALL ERRINT ( '#', IOSTAT                       )
               CALL SIGERR ( 'SPICE(FILEOPENFAILED)'           )
               CALL CHKOUT ( 'ZZGETFAT'                        )
               RETURN
 
            END IF
C
C           Try to read from the file.
C
            READ ( NUMBER, FMT='(A)', IOSTAT=IOSTAT ) TMPWRD
 
         END IF
 
      ELSE
 
         SEQOPN = .TRUE.
         READ ( NUMBER, FMT='(A)', IOSTAT=IOSTAT ) TMPWRD
 
      END IF
 
C
C     If we had an error while reading, we don't recognize this file.
C
      IF ( IOSTAT .NE. 0 ) THEN
 
         ARCH = '?'
         TYPE = '?'
         CLOSE       ( NUMBER                                      )
         CALL SETMSG ( 'Attempt to read from file ''#'' failed.'  //
     .                 ' IOSTAT = #.'                              )
         CALL ERRCH  ( '#', FILE                                   )
         CALL ERRINT ( '#', IOSTAT                                 )
         CALL SIGERR ( 'SPICE(FILEREADFAILED)'                     )
         CALL CHKOUT ( 'ZZGETFAT'                                  )
         RETURN
 
      END IF
 
 
C
C     Loop until a known NAIF file ID word is found.
C
      DO WHILE ( CHECK )
 
C
C        At this point, we have a candidate for an ID word. To avoid
C        difficulties with Fortran I/O and other things, we will now
C        replace any non printing ASCII characters with blanks.
C
         DO I = 1, IDLEN
            IF (      ( ICHAR(TMPWRD(I:I)) .LT. MINPCH )
     .           .OR. ( ICHAR(TMPWRD(I:I)) .GT. MAXPCH ) ) THEN
               TMPWRD(I:I) = ' '
            END IF
         END DO
 
C
C        Identify the architecture and type, if we can.
C
         CALL LJUST  ( TMPWRD, TMPWRD         )
         CALL UCASE  ( TMPWRD, TMPWRD         )
         CALL NEXTWD ( TMPWRD, IDWORD, TMPWRD )
 
         IF ( IDWORD .EQ. 'DAFETF' ) THEN
C
C           We have a DAF encoded transfer file.
C
            ARCH = 'XFR'
            TYPE = 'DAF'
            CHECK = .FALSE.
 
         ELSE IF ( IDWORD .EQ. 'DASETF') THEN
C
C           We have a DAS encoded transfer file.
C
            ARCH = 'XFR'
            TYPE = 'DAS'
            CHECK = .FALSE.
 
         ELSE IF ( IDWORD(1:10) .EQ. '''NAIF/DAF''' ) THEN
C
C           We have an old DAF decimal text file.
C
            ARCH = 'DEC'
            TYPE = 'DAF'
            CHECK = .FALSE.
 
         ELSE IF ( IDWORD(1:8) .EQ. 'NAIF/DAS' ) THEN
C
C           We have a pre release DAS binary file.
C
            ARCH = 'DAS'
            TYPE = 'PRE'
            CHECK = .FALSE.
 
         ELSE
C
C           Get the architecture and type from the ID word, if we can.
C
            CALL IDW2AT ( IDWORD(1:8), ARCH, TYPE )
 
            IF ( ( ARCH .EQ. 'DAF' ) .AND. ( TYPE .EQ. '?' ) ) THEN
 
               CHECK = .FALSE.
 
            ELSE
 
C
C              No identification on line.  Read another line.
C
               IF ( SEQOPN ) THEN
                  READ ( NUMBER, FMT='(A)', IOSTAT=IOSTAT ) TMPWRD
               ELSE
                  READ ( NUMBER, REC=1, IOSTAT=IOSTAT ) TMPWRD
               END IF
 
C
C              If IOSTAT is a negative value, we probably hit an
C              end-of-file.  Error out.
C
               IF ( IOSTAT .LT. 0 ) THEN
 
                  ARCH = '?'
                  TYPE = '?'
                  CLOSE       ( NUMBER )
                  CALL SETMSG ( 'Encountered end-of-file of # before '
     .                          // ' finding known SPICE ID word.'    )
                  CALL ERRCH  ( '#', FILE                             )
                  CALL SIGERR ( 'SPICE(ENDOFFILE)'               )
                  CALL CHKOUT ( 'ZZGETFAT'                            )
                  RETURN
 
               END IF
 
 
            END IF
 
 
         END IF
 
      END DO
 
 
      CALL CHKOUT ( 'ZZGETFAT' )
      RETURN
 
      END
 
