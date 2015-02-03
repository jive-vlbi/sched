C$Procedure GETFAT ( Get file architecture and type )
 
      SUBROUTINE GETFAT ( FILE, ARCH, KERTYP )
 
C$ Abstract
C
C     Determine the architecture and type of SPICE kernels.
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
 
      INCLUDE              'zzddhman.inc'
 
      CHARACTER*(*)         FILE
      CHARACTER*(*)         ARCH
      CHARACTER*(*)         KERTYP
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      FILE       I   The name of a file to be examined.
C      ARCH       O   The architecture of the kernel file.
C      KERTYP     O   The type of the kernel file.
C
C$ Detailed_Input
C
C     FILE        is the name of a SPICE kernel file whose architecture
C                 and type are desired.
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
C     KERTYP      is the type of the SPICE kernel file. If the type
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
C      1) If the filename specified is blank, then the error
C         SPICE(BLANKFILENAME) is signaled.
C
C      2) If any inquire on the filename specified by FILE fails for
C         some reason, the error SPICE(INQUIREERROR) is signaled.
C
C      3) If the file specified by FILE does not exist, the error
C         SPICE(FILENOTFOUND) is signaled.
C
C      4) If the file specified by FILE is already open but not through
C         SPICE interfaces, the error SPICE(EXTERNALOPEN) is signaled.
C
C      5) If an attempt to open the file specified by FILE fails when
C         this routine requires that it succeed, the error
C         SPICE(FILEOPENFAILED) is signaled.
C
C      6) If an attempt to read the file specified by FILE fails when
C         this routine requires that it succeed, the error
C         SPICE(FILEREADFAILED) is signaled.
C
C      7) Routines in the call tree of this routine may trap and
C         signal errors.
C
C      8) If the ID word in a DAF based kernel is NAIF/DAF, then the
C         algorithm GETFAT uses to distinguish between CK and SPK
C         kernels may result in an indeterminate KERTYP if the SPK or
C         CK files have invalid first segments.
C
C$ Files
C
C     The SPICE kernel file specified by FILE is examined by this
C     routine to determine its architecture and type.  If the file
C     named by FILE is not connected to a logical unit or loaded
C     in the handle manager, this routine will OPEN and CLOSE it.
C
C$ Particulars
C
C     This subroutine is a support utility routine that determines the
C     architecture and type of a SPICE kernel file.
C
C$ Examples
C
C     Suppose you wish to write a single routine for loading binary
C     kernels. You can use this routine to determine the type of the
C     file and  then pass the file to the appropriate low level file
C     loader to handle the actual loading of the file.
C
C        CALL GETFAT ( FILE, ARCH, KERTYP )
C
C        IF ( KERTYP .EQ. 'SPK' ) THEN
C
C           CALL SPKLEF ( FILE, HANDLE )
C
C        ELSE IF ( KERTYP .EQ. 'CK' ) THEN
C
C           CALL CKLPF ( FILE, HANDLE )
C
C        ELSE IF ( KERTYP .EQ. 'EK' ) THEN
C
C           CALL EKLEF ( FILE, HANDLE )
C
C        ELSE
C
C           WRITE (*,*) 'The file could not be identified as a known'
C           WRITE (*,*) 'kernel type.  Did you load the wrong file'
C           WRITE (*,*) 'by mistake?'
C
C        END IF
C
C
C$ Restrictions
C
C     1) In order to properly determine the type of DAF based binary
C        kernels, the routine requires that their first segments and
C        the meta data necessary to address them are valid.
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
C     F.S. Turner     (JPL)
C     E.D. Wright     (JPL)
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
C-    SPICELIB Version 4.0.2, 24-APR-2003 (EDW)
C
C        Added MAC-OSX-F77 to the list of platforms
C        that require READONLY to read write protected
C        kernels.
C
C-    SPICELIB Version 4.0.1, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 4.0.0, 22-AUG-2001 (WLT) (FST) (EDW)
C
C        Added code so that the architecture and type of open binary
C        SPICE kernels can be determined.
C
C        Added exception for MACPPC_C (CodeWarrior Mac classic).
C        Reduced RECL value to 12 to prevent expression of
C        the fseek bug.
C
C-    SPICELIB Version 3.2.0, 06-DEC-1999 (WLT)
C
C        The heuristics for distinguishing between CK and SPK have
C        been enhanced so that the routine is no longer requires
C        that TICKS in C-kernels be positive or integral.
C
C-    SPICELIB Version 3.1.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 3.1.3, 22-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    SPICELIB Version 3.1.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 3.1.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 3.1.0, 11-FEB-1999 (FST)
C
C        Added an integrality check to Test 3. If LASTDP is not
C        an integral value, then GETFAT simply returns KERTYP = '?',
C        since it is of an indeterminate type.
C
C-    SPICELIB Version 3.0.0, 07-APR-1998 (NJB)
C
C        Module was updated for the PC-LINUX platform.
C
C-     SPICELIB Version 2.0.0, 19-DEC-1995 (KRG)
C
C         Added several new features to the subroutine:
C
C         - Error handling has been enhanced.
C         - Several new file architectures have been added.
C
C         Removed the mention of 1000 characters as a candidate for the
C         record length of a file.
C
C         Added the exception for a blank filename to the header. The
C         error is signalled, but it was not listed in the header.
C
C         Added IOSTAT values to the appropriate error messages.
C
C         Non-printing characters are replaced with blanks in the ID
C         word when it is read. This deals with the case where a
C         platform allows a text file to be opened as an unformatted
C         file and the ID word does not completely fill 8 characters.
C
C-    SPICELIB Version 1.4.0, 5-JAN-1995 (HAN)
C
C        Removed ENV11 since it is now the same as ENV2.
C        Removed ENV10 since it is the same as the VAX environment.
C
C-    SPICELIB Version 1.3.0, 30-AUG-1994 (HAN)
C
C        Added two new environments, DEC Alpha/OpenVMS and
C        Sun/Solaris, to the source master file.
C
C-     SPICELIB Version 1.2.0, 25-MAR-1994 (HAN)
C
C         Added two new environments, DEC Alpha/OpenVMS and
C         Sun/Solaris, to the source master file.
C
C-     SPICELIB Version 1.1.0, 25-MAR-1994 (HAN)
C
C         Modified master source code file to use READONLY on platforms
C         that support it. Also, changed some local declaration comment
C         lines to match the standard NAIF template.
C
C-     SPICELIB Version 1.0.0, 24-JUL-1993 (WLT) (HAN) (KRG)
C
C-&
 
 
C$ Index_Entries
C
C     determine the architecture and type of a kernel file
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 4.0.0, 22-AUG-2001 (WLT) (FST)
C
C        Added code so that the architecture and type of open binary
C        SPICE kernels can be determined.  This uses the new DAF/DAS
C        handle manager as well as examination of handles of open DAS
C        files.  Currently the handle manager deals only with DAF
C        files. This routine should be updated again when the DAS
C        system is integrated with the handle manager.
C
C        Some slight changes were required to support ZZDDHFNH on
C        the VAX environment.  This resulted in the addition of
C        the logical USEFNH that is set to true in most
C        environments, and never used again other than to allow
C        the invocation of the ZZDDHFNH module.
C
C-     SPICELIB Version 2.0.0, 19-DEC-1995 (KRG)
C
C         Added several new features to the subroutine:
C
C         - Error handling has been enhanced.
C         - Several new file architectures have been added.
C
C         Removed the mention of 1000 characters as a candidate for the
C         record length of a file. It seems unlikely that we will
C         encounter an environment where 1000 characters of storage is
C         larger than the storage necessary for 128 double precision
C         numbers; typically there are 8 characters per double precision
C         number, yeilding 1024 characters.
C
C         Added the exception for a blank filename to the header. The
C         error is signalled, but it was not listed in the header.
C
C         Added IOSTAT values to the appropriate error messages.
C
C         Non-printing characters are replaced with blanks in the ID
C         word when it is read. This deals with the case where a
C         platform allows a text file to be opened as an unformatted
C         file and the ID word does not completely fill 8 characters.
C
C-&
C
C     SPICELIB functions
C
      INTEGER               CARDI
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
C
C     Set the length of a SPICE kernel file ID word.
C
      INTEGER               IDLEN
      PARAMETER           ( IDLEN = 12 )
 
      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
C
C     Set minimum and maximum values for the range of ASCII printing
C     characters.
C
      INTEGER               MINPCH
      PARAMETER           ( MINPCH = 32 )
 
      INTEGER               MAXPCH
      PARAMETER           ( MAXPCH = 126 )
 
      INTEGER               MAXHND
      PARAMETER           ( MAXHND = 100 )
 
C
C     Local Variables
C
      CHARACTER*(WDSIZE)    FILARC
      CHARACTER*(FILSIZ)    FNAME
      CHARACTER*(IDLEN)     IDWORD
      CHARACTER*(IDLEN)     TMPWRD
 
      INTEGER               HANDLE
      INTEGER               HANDLES ( LBCELL: MAXHND )
      INTEGER               I
      INTEGER               INTAMN
      INTEGER               INTARC
      INTEGER               INTBFF
      INTEGER               IOSTAT
      INTEGER               MYUNIT
      INTEGER               NUMBER
      INTEGER               UNIT
      INTEGER               WHICH
 
      LOGICAL               DIROPN
      LOGICAL               EXIST
      LOGICAL               FOUND
      LOGICAL               NOTDAS
      LOGICAL               OPENED
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN  ( 'GETFAT' )
      END IF
 
C
C     Initialize the temporary storage variables that we use.
C
      IDWORD = ' '
 
C
C     If the filename we have is blank, signal an error and return.
C
      IF ( FILE .EQ. ' ' ) THEN
 
         CALL SETMSG ( 'The file name is blank.' )
         CALL SIGERR ( 'SPICE(BLANKFILENAME)'    )
         CALL CHKOUT ( 'GETFAT'                  )
         RETURN
 
      END IF
 
C
C     See if this is a binary file that is currently open
C     within the SPICE binary file management subsystem.  At
C     the moment, as far as we know, the file is not opened.
C
      OPENED = .FALSE.
 
      CALL ZZDDHFNH ( FILE, HANDLE, FOUND )
 
      IF ( FOUND ) THEN
 
C
C        If the file was recognized, we need to get the unit number
C        associated with it.
C
         CALL ZZDDHNFO ( HANDLE, FNAME, INTARC, INTBFF, INTAMN, FOUND )
 
C
C        Translate the architecture ID to a string and retrieve the
C        logical unit to use with this file.
C
         CALL ZZDDHGSD ( 'ARCH', INTARC, FILARC )
         CALL ZZDDHHLU ( HANDLE, FILARC, .FALSE., NUMBER )
 
         OPENED = .TRUE.
 
      ELSE
 
C
C        We'll do a bit of inquiring before we try opening anything.
C
         INQUIRE ( FILE   = FILE,
     .             EXIST  = EXIST,
     .             IOSTAT = IOSTAT,
     .             OPENED = OPENED )
 
C
C        Not too likely, but if the INQUIRE statement fails...
C
         IF ( IOSTAT .NE. 0 ) THEN
            CALL SETMSG ( 'IOSTAT error in INQUIRE statement. IOSTAT '
     .      //              '= #.'                                    )
            CALL ERRINT ( '#', IOSTAT                                 )
            CALL SIGERR ( 'SPICE(INQUIREERROR)'                       )
            CALL CHKOUT ( 'GETFAT'                                    )
            RETURN
         END IF
 
C
C        Note: the following two tests MUST be performed in the order
C        in which they appear, since in some environments files that do
C        not exist are considered to be open.
C
         IF ( .NOT. EXIST ) THEN
 
            CALL SETMSG ( 'The kernel file ''#'' does not exist.'   )
            CALL ERRCH  ( '#', FILE                                 )
            CALL SIGERR ( 'SPICE(FILENOTFOUND)'                     )
            CALL CHKOUT ( 'GETFAT'                                  )
            RETURN
 
         END IF
 
C
C        If the file is already open, it may be a DAS file.
C
         IF ( OPENED ) THEN
 
C
C           At the moment, the handle manager doesn't manage DAS
C           handles.  As a result we need to treat the case of an open
C           DAS separately. When the Handle Manager is hooked in with
C           DAS as well as DAF, we should remove the block below.
C
C           ===================================================
C           DAS DAS DAS DAS DAS DAS DAS DAS DAS DAS DAS DAS DAS
C           vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
C
C           This file may or may not be a DAS file.  Until we
C           have determined otherwise, we assume it is not
C           a DAS file.
C
            NOTDAS = .TRUE.
 
            INQUIRE ( FILE   = FILE,
     .                NUMBER = UNIT,
     .                IOSTAT = IOSTAT )
 
            IF ( IOSTAT .NE. 0 ) THEN
               CALL SETMSG ( 'IOSTAT error in INQUIRE statement. '
     .         //            'IOSTAT = #.'                       )
               CALL ERRINT ( '#', IOSTAT                         )
               CALL SIGERR ( 'SPICE(INQUIREERROR)'               )
               CALL CHKOUT ( 'GETFAT'                            )
               RETURN
            END IF
 
C
C           Get the set of handles of open DAS files.  We will
C           translate each of these handles to the associated
C           logical unit.  If the tranlation matches the result
C           of the inquire, this must be a DAS file and we
C           can proceed to determine the type.
C
            CALL SSIZEI ( MAXHND, HANDLES )
            CALL DASHOF ( HANDLES )
 
            WHICH  = CARDI(HANDLES)
 
            DO WHILE ( WHICH .GT. 0 )
 
               CALL DASHLU ( HANDLES(WHICH), MYUNIT )
 
               IF ( UNIT .EQ. MYUNIT ) THEN
                  NUMBER = MYUNIT
                  WHICH  = 0
                  NOTDAS = .FALSE.
               ELSE
                  WHICH = WHICH - 1
               END IF
 
            END DO
 
C
C           If we reach this point and do not have a DAS, there
C           is no point in going on.  The user has opened this
C           file outside the SPICE system.  We shall not attempt
C           to determine its type.
C
            IF ( NOTDAS ) THEN
               CALL SETMSG ( 'The file ''#'' is already open.' )
               CALL ERRCH  ( '#', FILE                         )
               CALL SIGERR ( 'SPICE(EXTERNALOPEN)'             )
               CALL CHKOUT ( 'GETFAT'                          )
               RETURN
            END IF
 
C           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
C           DAS DAS DAS DAS DAS DAS DAS DAS DAS DAS DAS DAS DAS
C           ===================================================
C
         END IF
 
      END IF
 
C
C     Open the file with a record length of RECL (the length of the
C     DAF and DAS records). We assume, for now, that opening the file as
C     a direct access file will work.
C
      DIROPN = .TRUE.
 
C
C     If the file is not already open (probably the case that
C     happens most frequently) we try opening it for direct access
C     and see if we can locate the idword.
C
      IF ( .NOT. OPENED ) THEN
 
         CALL GETLUN ( NUMBER )
 
         OPEN ( UNIT               = NUMBER,
     .          FILE               = FILE,
     .          ACCESS             = 'DIRECT',
     .          RECL               = RECL,
     .          STATUS             = 'OLD',
     .          IOSTAT             = IOSTAT    )
 
C
C     If we had trouble opening the file, try opening it as a
C     sequential file.
C
         IF ( IOSTAT .NE. 0 ) THEN
 
            DIROPN = .FALSE.
 
            OPEN ( UNIT            = NUMBER,
     .             FILE            = FILE,
     .             ACCESS          = 'SEQUENTIAL',
     .             STATUS          = 'OLD',
     .             IOSTAT          = IOSTAT    )
 
C
C        If we still have problems opening the file, we don't have a
C        clue about the file architecture and type.
C
            IF ( IOSTAT .NE. 0 ) THEN
 
               ARCH   = '?'
               KERTYP = '?'
               CALL SETMSG ( 'Attempt to open the file ''#'' failed.'
     .         //           ' IOSTAT = #.'                          )
               CALL ERRCH  ( '#', FILE                              )
               CALL ERRINT ( '#', IOSTAT                            )
               CALL SIGERR ( 'SPICE(FILEOPENFAILED)'                )
               CALL CHKOUT ( 'GETFAT'                               )
               RETURN
 
            END IF
 
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
         IF ( IOSTAT .NE. 0 ) THEN
 
            IF ( OPENED ) THEN
 
C
C              Something has gone wrong here.  The file was opened
C              as either a DAF or DAS prior to the call to GETFAT.
C              We retrieved the unit number maintained by the
C              underlying binary file management system, but we
C              were unable to read the file as direct access.
C              There's nothing we can do but abandon our quest to
C              determine the type of the file.
C
               CALL SETMSG ( 'The file ''#'' is opened as a binary '
     .         //            'SPICE kernel.  But it cannot be '
     .         //            'read using a direct access read. '
     .         //            'The value of IOSTAT returned by the '
     .         //            'attempted READ is #. '                 )
               CALL ERRCH  ( '#', FILE                               )
               CALL ERRINT ( '#', IOSTAT                             )
               CALL SIGERR ( 'SPICE(FILEREADFAILED)'                 )
               CALL CHKOUT ( 'GETFAT'                                )
               RETURN
 
            END IF
 
C
C           If we reach this point, the file was opened locally
C           as a direct access file.  We could not read it that
C           way, so we'll try using a sequential read.   However,
C           we first need to close the file and then reopen it
C           for sequential reading.
C
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
 
               ARCH   = '?'
               KERTYP = '?'
               CALL SETMSG ( 'Attempt to open the file ''#'''
     .         //            ' failed. IOSTAT = #.'           )
               CALL ERRCH  ( '#', FILE                        )
               CALL ERRINT ( '#', IOSTAT                      )
               CALL SIGERR ( 'SPICE(FILEOPENFAILED)'          )
               CALL CHKOUT ( 'GETFAT'                         )
               RETURN
 
            END IF
 
C
C           Try to read from the file.
C
            READ ( NUMBER, FMT='(A)', IOSTAT=IOSTAT ) TMPWRD
 
         END IF
 
      ELSE
 
         READ ( NUMBER, FMT='(A)', IOSTAT=IOSTAT ) TMPWRD
 
      END IF
 
C
C     If we had an error while reading, we don't recognize this file.
C
      IF ( IOSTAT .NE. 0 ) THEN
 
         ARCH   = '?'
         KERTYP = '?'
         CLOSE       ( NUMBER                                      )
         CALL SETMSG ( 'Attempt to read from file ''#'' failed.'  //
     .                 ' IOSTAT = #.'                              )
         CALL ERRCH  ( '#', FILE                                   )
         CALL ERRINT ( '#', IOSTAT                                 )
         CALL SIGERR ( 'SPICE(FILEREADFAILED)'                     )
         CALL CHKOUT ( 'GETFAT'                                    )
         RETURN
 
      END IF
 
C
C     Close the file (if we opened it here), as we do not need it
C     to be open any more.
C
      IF ( .NOT. OPENED ) THEN
         CLOSE ( NUMBER )
      END IF
 
C
C     At this point, we have a candidate for an ID word. To avoid
C     difficulties with Fortran I/O and other things, we will now
C     replace any non printing ASCII characters with blanks.
C
      DO I = 1, IDLEN
         IF (      ( ICHAR(TMPWRD(I:I)) .LT. MINPCH )
     .        .OR. ( ICHAR(TMPWRD(I:I)) .GT. MAXPCH ) ) THEN
            TMPWRD(I:I) = ' '
         END IF
      END DO
 
C
C     Identify the architecture and type, if we can.
C
      CALL LJUST  ( TMPWRD, TMPWRD         )
      CALL UCASE  ( TMPWRD, TMPWRD         )
      CALL NEXTWD ( TMPWRD, IDWORD, TMPWRD )
 
      IF ( IDWORD .EQ. 'DAFETF' ) THEN
 
C
C        We have a DAF encoded transfer file.
C
         ARCH   = 'XFR'
         KERTYP = 'DAF'
 
      ELSE IF ( IDWORD .EQ. 'DASETF') THEN
 
C
C        We have a DAS encoded transfer file.
C
         ARCH   = 'XFR'
         KERTYP = 'DAS'
 
      ELSE IF ( IDWORD(1:10) .EQ. '''NAIF/DAF''' ) THEN
 
C
C        We have an old DAF decimal text file.
C
         ARCH   = 'DEC'
         KERTYP = 'DAF'
 
      ELSE IF ( IDWORD(1:8) .EQ. 'NAIF/DAS' ) THEN
 
C
C        We have a pre release DAS binary file.
C
         ARCH   = 'DAS'
         KERTYP = 'PRE'
 
      ELSE
 
C
C        Get the architecture and type from the ID word, if we can.
C
         CALL IDW2AT ( IDWORD(1:8), ARCH, KERTYP )
 
      END IF
 
C
C     If the architecture is DAF and the type is unknown, '?', then we
C     have either an SPK file, a CK file, or something we don't
C     understand. Let's check it out.
C
      IF ( ( ARCH .EQ. 'DAF' ) .AND. ( KERTYP .EQ. '?' ) ) THEN
 
C
C        We have a DAF file and we do not know what the type is. This
C        situation can occur for older SPK and CK files, before the ID
C        word was used to store type information.
C
C        We use Bill's (WLT'S) magic heuristics to determine the type
C        of the file.
C
C        Open the file and pass the handle to the private routine
C        that deals with the dirty work.
C
         CALL DAFOPR ( FILE, HANDLE )
         CALL ZZCKSPK( HANDLE, KERTYP )
         CALL DAFCLS ( HANDLE )
 
      END IF
 
      CALL CHKOUT ( 'GETFAT' )
      RETURN
 
      END
