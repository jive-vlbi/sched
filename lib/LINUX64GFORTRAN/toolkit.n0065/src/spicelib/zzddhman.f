C$Procedure ZZDDHMAN ( Private --- DAF/DAS Handle Manager )
 
      SUBROUTINE ZZDDHMAN ( LOCK,   ARCH,   FNAME,  METHOD,
     .                      HANDLE, UNIT,   INTAMH, INTARC,
     .                      INTBFF, NATIVE, FOUND,  KILL    )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This is an umbrella routine for a collection of entry points
C     to the DAF/DAS handle manager.
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
C     DAF
C     DAS
C     PRIVATE
C
C$ Declarations
 
      IMPLICIT NONE
 
      INCLUDE              'zzddhman.inc'
 
      LOGICAL               LOCK
      CHARACTER*(*)         ARCH
      CHARACTER*(*)         FNAME
      CHARACTER*(*)         METHOD
      INTEGER               HANDLE
      INTEGER               UNIT
      INTEGER               INTAMH
      INTEGER               INTARC
      INTEGER               INTBFF
      LOGICAL               NATIVE
      LOGICAL               FOUND
      LOGICAL               KILL
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     LOCK       I   HLU
C     ARCH      I/O  OPN, CLS, HLU, UNL
C     FNAME     I/O  OPN, NFO, FNH
C     METHOD    I/O  OPN
C     HANDLE    I/O  OPN, CLS, HLU, UNL, ISN, NFO, FNH, LUH
C     UNIT      I/O  HLU, LUH
C     INTAMH     O   NFO
C     INTARC     O   NFO
C     INTBFF     O   NFO
C     NATIVE     O   ISN
C     FOUND      O   ISN, NFO, FNH, LUH
C     KILL       I   CLS
C
C$ Detailed_Input
C
C     See the entry points for descriptions of their inputs.
C
C$ Detailed_Output
C
C     See the entry points for descriptions of their outputs.
C
C$ Parameters
C
C     See the include file 'zzddhman.inc' for details of parameter
C     definitions used within this module.
C
C$ Exceptions
C
C     1) If ZZDDHMAN is called directly, the error SPICE(BOGUSENTRY)
C        is signaled.
C
C     2) See entry points ZZDDHOPN, ZZDDHCLS, ZZDDHHLU, ZZDDHUNL,
C        ZZDDHISN, ZZDDHNFO, ZZDDHFNH, and ZZDDHLUH for exceptions
C        specific to those entry points.
C
C$ Files
C
C     This set of routines is intended to provide low-level services
C     for the creation, updating, and reading of Fortran direct access
C     files utilized by the DAF and DAS systems within SPICE.
C
C$ Particulars
C
C     ZZDDHMAN serves as an umbrella, allowing data to be shared by
C     its entry points:
C
C        ZZDDHOPN       Open file.
C        ZZDDHCLS       Close file.
C        ZZDDHHLU       Handle to logical unit.
C        ZZDDHUNL       Unlock handle from unit.
C        ZZDDHISN       Is the file native architecture?
C        ZZDDHNFO       Fetch information about a handle.
C        ZZDDHFNH       Filename to handle.
C        ZZDDFLUH       Logical unit to handle.
C
C     This umbrella serves a variety of functions to the DAS/DAF
C     families of routines.
C
C        (1) DAF/DAS handle consolidation
C        (2) Binary file format detection and tracking
C        (3) FTP error detection services
C        (4) Logical unit sharing
C        (5) Filename and unit to handle mapping services
C
C$ Examples
C
C     See individual entry points for pointers to modules that utilize
C     their capabilities.
C
C$ Restrictions
C
C     1) Changing the current working directory of a program when
C        more than UTSIZE files are loaded into this interface requires
C        that all filenames passed into ZZDDHOPN are specified with
C        absolute pathnames.  Otherwise the OPEN/CLOSE switching
C        logic will fail to OPEN files that are loaded.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C     E.D. Wright     (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.1.0, 26-APR-2012 (BVS)
C
C        Added the "magic number" column to the file table.
C
C-    SPICELIB Version 2.0.1, 24-APR-2003 (EDW)
C
C        Added MAC-OSX-F77 to the list of platforms
C        that require READONLY to read write protected
C        kernels.
C
C-    SPICELIB Version 2.0.0, 07-AUG-2002 (FST)
C
C        The entry point ZZDDHOPN now invokes ZZPLTCHK, to verify
C        that the runtime environment's binary file format matches
C        the one for which the toolkit is configured.
C
C        The entry point ZZDDHCLS has had its argument list augmented
C        to include a "KILL" flag.  Check the entry point header for
C        details.
C
C-    SPICELIB Version 1.0.0, 06-NOV-2001 (FST)
C
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 07-AUG-2002 (FST)
C
C        The toolkit source code is far more sensitive to blind
C        porting of source packaged for one environment to another.
C        This sensitivity has already caused a few of our users
C        some difficulty.  In an attempt to address these problems
C        with future toolkits, ZZDDHOPN now invokes ZZPLTCHK on
C        it's first pass.  This will perform any necessary checks
C        on the runtime environment against the values recorded
C        in ZZPLATFM and other environment specific components
C        of the library.
C
C        As of this release, all that is verified is that the
C        BFF ID listed in ZZPLATFM is compatible with the runtime
C        environment.  See ZZPLTCHK's header for deatils.
C
C-&
 
C
C     SPICELIB Functions
C
      INTEGER               BSRCHI
      INTEGER               ISRCHC
      INTEGER               ISRCHI
      INTEGER               RTRIM
      INTEGER               ZZDDHCLU
 
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local Variables
C
      CHARACTER*(FILEN)     LOCFNM
      CHARACTER*(STRSIZ)    TMPSTR
 
      DOUBLE PRECISION      MNM
 
      INTEGER               ACCMET
      INTEGER               BFF
      INTEGER               FILARC
      INTEGER               I
      INTEGER               INQHAN
      INTEGER               IOSTAT
      INTEGER               LCHAR
      INTEGER               LOCKED
      INTEGER               LOCLUN
      INTEGER               SUPIDX
 
      LOGICAL               ERROR
      LOGICAL               INQEXT
      LOGICAL               INQOPN
      LOGICAL               LOCFND
      LOGICAL               PLATOK
 
C
C     This logical allows initialization code to execute.
C
      LOGICAL               FIRST
      LOGICAL               OPNFST
 
C
C     These strings store the labels for the parameters defined
C     in the include file and retrieved by ZZDDHINI.
C
      CHARACTER*(STRSIZ)    STRAMH ( NUMAMH )
      CHARACTER*(STRSIZ)    STRARC ( NUMARC )
      CHARACTER*(STRSIZ)    STRBFF ( NUMBFF )
 
C
C     The file table consists of a set of arrays which serve as
C     'columns' of the table.  The sets of elements having the same
C     index in the arrays form the 'rows' of the table.  Each column
C     contains a particular type of information; each row contains
C     all of the information pertaining to a particular file.
C
C     All column names in the file table begin with 'FT'.  The columns
C     are:
C
C        ABS      Absolute value of HAN
C        AMH      File access method
C        ARC      File architecture
C        BFF      Binary file format
C        HAN      Handle
C        NAM      Filename
C        RTM      RTRIM (right trimmed value for NAM)
C        MNM      Unique DP number (the Magic NuMber ;)
C
C     New 'rows' are added to the end of the list; the list is repacked
C     whenever a file is removed from the list.
C
C     NFT is the number of files currently loaded; this may not be
C     greater than FTSIZE.  FINDEX refers to a file of interest within
C     the table.  Since handles are always assigned in an increasing
C     fashion, FTABS is guaranteed to be a sorted list.  We will use
C     this fact to improve handle lookups in the file table.
C
      INTEGER               NFT
 
      INTEGER               FTABS  ( FTSIZE )
      INTEGER               FTAMH  ( FTSIZE )
      INTEGER               FTARC  ( FTSIZE )
      INTEGER               FTBFF  ( FTSIZE )
      INTEGER               FTHAN  ( FTSIZE )
      CHARACTER*(FILEN)     FTNAM  ( FTSIZE )
      INTEGER               FTRTM  ( FTSIZE )
      DOUBLE PRECISION      FTMNM  ( FTSIZE )
 
      INTEGER               FINDEX
 
C
C     NEXT stores the next handle to be used for file access.  This
C     could be either for read or write based operations. NEXT is
C     incremented just before entries in the file table are made.
C     It begins as zero valued.
C
      INTEGER               NEXT
 
C
C     The unit table consists of a set of arrays which serve as
C     'columns' of the table.  The sets of elements having the same
C     index in the arrays form the 'rows' of the table.  Each column
C     contains a particular type of information; each row contains
C     all of the information pertaining to a particular logical unit.
C
C     All column names in the unit table begin with 'UT'.  The columns
C     are:
C
C        CST      Cost to remove the file from the unit table
C        HAN      Handle
C        LCK      Is this logical unit locked to this handle?
C        LUN      Logical unit
C
C     New 'rows' are added to the end of the list; the list is repacked
C     whenever a logical unit is no longer needed.
C
C     NUT is the number of units currently stored in the table; this
C     may not exceed UTSIZE.  UINDEX referes to a unit of interest
C     within the table.
C
      INTEGER               NUT
 
      INTEGER               UTCST ( UTSIZE )
      INTEGER               UTHAN ( UTSIZE )
      LOGICAL               UTLCK ( UTSIZE )
      INTEGER               UTLUN ( UTSIZE )
 
      INTEGER               UINDEX
 
C
C     The following stores the native binary file format, a list of
C     codes for supported binary formats, and the number of entries
C     in SUPBFF.
C
      INTEGER               NATBFF
      INTEGER               SUPBFF ( NUMBFF )
      INTEGER               NUMSUP
 
C
C     Request counter used to determine cost.
C
      INTEGER               REQCNT
 
C
C     Saved Variables
C
      SAVE                  FIRST
      SAVE                  OPNFST
 
      SAVE                  STRAMH
      SAVE                  STRARC
      SAVE                  STRBFF
 
      SAVE                  NFT
      SAVE                  FTABS
      SAVE                  FTAMH
      SAVE                  FTARC
      SAVE                  FTBFF
      SAVE                  FTHAN
      SAVE                  FTNAM
      SAVE                  FTRTM
      SAVE                  FTMNM
 
      SAVE                  NEXT
 
      SAVE                  NUT
      SAVE                  UTCST
      SAVE                  UTHAN
      SAVE                  UTLCK
      SAVE                  UTLUN
 
      SAVE                  NATBFF
      SAVE                  SUPBFF
      SAVE                  NUMSUP
 
      SAVE                  REQCNT
 
 
C
C     Data Statements
C
      DATA                  FIRST  / .TRUE.                  /
      DATA                  OPNFST / .TRUE.                  /
      DATA                  NFT    / 0                       /
      DATA                  NEXT   / 0                       /
      DATA                  NUT    / 0                       /
      DATA                  REQCNT / 0                       /
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN  ( 'ZZDDHMAN' )
         CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
         CALL CHKOUT ( 'ZZDDHMAN' )
      END IF
 
      RETURN
 
 
 
 
C$Procedure ZZDDHOPN ( Private --- Load file )
 
      ENTRY ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Load a new direct access file.
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
C     DAS
C     DAF
C     PRIVATE
C
C$ Declarations
C
C     CHARACTER*(*)         FNAME
C     CHARACTER*(*)         METHOD
C     CHARACTER*(*)         ARCH
C     INTEGER               HANDLE
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FNAME      I   Name of file to be loaded.
C     METHOD     I   Access method used to load the file.
C     ARCH       I   Expected architecture of the file to load.
C     HANDLE     O   Handle assigned to file.
C
C$ Detailed_Input
C
C     FNAME      is the file name of the file to be loaded for direct
C                access.
C
C     METHOD     is the method by which to load the file. Acceptable
C                values are:
C
C                   'READ'    - Load existing file for read access.
C                   'WRITE'   - Load existing file for write access.
C                   'SCRATCH' - Load scratch file.
C                   'NEW'     - Load a new file for write access.
C
C                Note: The value of METHOD is case-insensitive.
C
C     ARCH       is the architecture of the file to be loaded.
C                Acceptable values are:
C
C                   'DAF'    - Load a DAF file
C                   'DAS'    - Load a DAS file
C
C                Note: The value of ARCH is case-insensitive.
C
C$ Detailed_Output
C
C     HANDLE     is the file handle associated with the file.  This
C                handle is used to identify the file in subsequent
C                calls to other routines.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) The error SPICE(UNSUPPORTEDMETHOD) is signaled when the
C        METHOD input argument is improperly specified.  The value of
C        the output argument HANDLE is undefined when this error is
C        signaled.
C
C     2) The error SPICE(UNSUPPORTEDARCH) is signaled when the ARCH
C        input argument is improperly specified.  The value of the
C        output argument HANDLE is undefined when this error is
C        signaled.
C
C     3) The error SPICE(UTFULL) is signaled whenever METHOD is
C        set to 'SCRATCH' and no available units exist in the
C        unit table for locking.  The value of the output argument
C        HANDLE is undefined when this error is signaled.
C
C     4) The error SPICE(BLANKFILENAME) is signaled whenever METHOD
C        is set to 'READ', 'WRITE', or 'NEW' and the FNAME argument
C        is a blank string.  The value of the output argument HANDLE
C        is undefined when this error is signaled.
C
C     5) The error SPICE(FILENOTFOUND) is signaled whenever METHOD
C        is set to 'READ' or 'WRITE' and an INQUIRE performed on FNAME
C        indicates the file does not exist.  The value of the output
C        argument HANDLE is undefined when this error is signaled.
C
C     6) The error SPICE(IMPROPEROPEN) is signaled if the file
C        associated with FNAME is attached to a unit from some
C        source external to ZZDDHMAN's entry points.  The value of the
C        output argument HANDLE is undefined when this error is
C        signaled.
C
C     7) The error SPICE(FILARCMISMATCH) is signaled when a file is
C        loaded for 'READ' or 'WRITE' and the architecture of the
C        existing file disagrees with that of the input argument ARCH.
C        The value of the output argument HANDLE is undefined when
C        this error is signaled.
C
C     8) The error SPICE(FILEOPENCONFLICT) is signaled when an attempt
C        to load an already loaded file for any access other than READ.
C        The value of the output argument HANDLE is undefined when this
C        error is signaled.
C
C     9) The error SPICE(RWCONFLICT) is signaled when an attempt to
C        load a file for READ access that is already loaded into the
C        handle manager with a conflicting access method.  The value of
C        the output argument HANDLE is undefined when this error is
C        signaled.
C
C    10) The error SPICE(FTFULL) is signaled when an attempt to load
C        more than the maximum number of allowable files, FTSIZE,
C        is made.  The value of the output argument HANDLE is undefined
C        when this error is signaled.
C
C    11) The error SPICE(FILEOPENFAIL) is signaled whenever the
C        the file open fails with non-zero IOSTAT.  The value of the
C        output argument HANDLE is undefined when this error is
C        signaled.
C
C    12) The error SPICE(UNSUPPORTEDBFF) is signaled whenever the file
C        to be opened utilizes a binary file format that the platform
C        does not currently support.  The value of the output argument
C        HANDLE is undefined when this error is signaled.
C
C    13) When loading files with METHOD set to 'NEW', any errors
C        generated by this routine will cause the newly created file
C        to be deleted.
C
C    14) If the toolkit source is improperly configured for the
C        runtime environment, routines in the call tree of this
C        routine may signal errors.
C
C$ Files
C
C     1) All direct access files loaded by this routine for
C        access methods other than 'SCRATCH' are specified by name.
C
C     2) Files opened with access method 'SCRATCH' are referenced
C        only by their logical unit.
C
C$ Particulars
C
C     This private routine is designed to provide a common, unified
C     file load interface for DAF and DAS.
C
C$ Examples
C
C     See DAFOPR, DAFONW, DAFOPW, DASOPR, DASOPS, DASONW, DASOPW for
C     sample usage.
C
C$ Restrictions
C
C     1) Files loaded through this interface should not be opened by
C        any other mechanism until the appropriate call to ZZDDHCLS
C        is made.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.1.0, 26-APR-2012 (BVS)
C
C        Updated for the new "magic number" column in the file table.
C
C-    SPICELIB Version 2.0.0, 07-AUG-2002 (FST)
C
C        This entry point was updated to perform checks on the
C        runtime environment, to verify that the source is properly
C        configured for execution on this environment.  See the
C        Revisions section of ZZDDHMAN for details.
C
C-    SPICELIB Version 1.0.0, 06-NOV-2001 (FST)
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZDDHOPN' )
      END IF
 
C
C     Do the initialization tasks.
C
      IF ( FIRST ) THEN
 
         CALL ZZDDHINI ( NATBFF, SUPBFF, NUMSUP,
     .                   STRAMH, STRARC, STRBFF  )
 
C
C        Check FAILED() to handle the unlikely event that
C        ZZDDHINI signaled SPICE(BUG).
C
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZDDHOPN' )
            RETURN
         END IF
 
C
C        Clear FIRST since we've done the initialization.
C
         FIRST = .FALSE.
 
      END IF
 
C
C     On first pass, perform any runtime environment checks.
C
      IF ( OPNFST ) THEN
 
         CALL ZZPLTCHK ( PLATOK )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZDDHOPN' )
            RETURN
         END IF
 
C
C        Clear OPNFST, since we've performed the diagnostics.
C
         OPNFST = .FALSE.
 
      END IF
 
C
C     Initialize the value of HANDLE to 0.  In the event an error
C     is signaled this invalid value will be returned to the caller
C     for safety.
C
      HANDLE = 0
 
C
C     Left justify FNAME to compress off any leading spaces.
C
      CALL LJUST ( FNAME, LOCFNM )
 
C
C     Translate the value of the requested access method to the
C     corresponding integer code.
C
      TMPSTR = METHOD
      CALL UCASE ( TMPSTR, TMPSTR )
      ACCMET = ISRCHC ( TMPSTR, NUMAMH, STRAMH )
 
C
C     Check if the code was located.
C
      IF ( ACCMET .EQ. 0 ) THEN
 
C
C        Recall HANDLE was initialized to 0, and this invalid
C        value is returned to the caller.
C
         CALL SETMSG ( 'The attempt to load file, ''#'', with access '
     .   //            'method, ''#'', failed because this access '
     .   //            'method is unsupported.'                        )
         CALL ERRCH  ( '#', LOCFNM                                     )
         CALL ERRCH  ( '#', METHOD                                     )
         CALL SIGERR ( 'SPICE(UNSUPPORTEDMETHOD)'                      )
         CALL CHKOUT ( 'ZZDDHOPN'                                      )
         RETURN
 
      END IF
 
C
C     Translate the value of the requested file architecture to
C     the appropriate integer code.
C
      TMPSTR = ARCH
      CALL UCASE ( TMPSTR, TMPSTR )
      FILARC = ISRCHC ( TMPSTR, NUMARC, STRARC )
 
C
C     Check if the code was located.
C
      IF ( FILARC .EQ. 0 ) THEN
 
C
C        Recall HANDLE was initialized to 0, and this invalid
C        value is returned to the caller.
C
         CALL SETMSG ( 'The attempt to load file, ''#'', with '
     .   //            'architecture, ''#'', failed because this '
     .   //            'file architecture is unsupported.'             )
         CALL ERRCH  ( '#', LOCFNM                                     )
         CALL ERRCH  ( '#', ARCH                                       )
         CALL SIGERR ( 'SPICE(UNSUPPORTEDARCH)'                        )
         CALL CHKOUT ( 'ZZDDHOPN'                                      )
         RETURN
 
      END IF
 
C
C     Perform any preliminary checks that must be done before
C     fetching a logical unit from the unit table.  This requires
C     branching based on ACCMET's value.
C
      IF ( ACCMET .EQ. SCRTCH ) THEN
 
C
C        Check to see if there are enough units available for locking
C        in the unit table.  If not, signal an error as all files
C        open with SCRTCH access must be locked to their units.
C
         LOCKED = ZZDDHCLU ( UTLCK, NUT )
 
         IF ( LOCKED .GE. (UTSIZE - RSVUNT) ) THEN
 
C
C           Recall HANDLE was initialized to 0, and this invalid
C           value is returned to the caller.
C
            CALL SETMSG ( 'The maximum number of units are locked '
     .      //            'to handles.  As such, there is no room '
     .      //            'to open the requested scratch file.'     )
            CALL SIGERR ( 'SPICE(UTFULL)'                           )
            CALL CHKOUT ( 'ZZDDHOPN'                                )
            RETURN
 
         END IF
 
C
C     The NEW, READ, and WRITE access methods perform the same
C     checks on LOCFNM.
C
      ELSE IF ( ( ACCMET .EQ. NEW   ) .OR.
     .          ( ACCMET .EQ. READ  ) .OR.
     .          ( ACCMET .EQ. WRITE )      ) THEN
 
C
C        Check for a non-blank file name.
C
         IF ( LOCFNM .EQ. ' ' ) THEN
 
C
C           Recall HANDLE was initialized to 0, and this invalid
C           value is returned to the caller.
C
            CALL SETMSG ( 'The attempt to load the file has failed, '
     .      //            'because the filename is blank.'            )
            CALL SIGERR ( 'SPICE(BLANKFILENAME)'                      )
            CALL CHKOUT ( 'ZZDDHOPN'                                  )
            RETURN
 
         END IF
 
      END IF
 
      MNM = 0.D0
 
C
C     In the READ or WRITE cases verify that LOCFNM is not already
C     in the file table.
C
      IF ( ( ACCMET .EQ. READ  ) .OR.
     .     ( ACCMET .EQ. WRITE )      ) THEN
 
C
C        Check to see if the file associated with LOCFNM is already in
C        the file table.
C
         CALL ZZDDHF2H ( LOCFNM,  FTABS,  FTAMH,  FTARC, FTBFF,
     .                   FTHAN,   FTNAM,  FTRTM,  FTMNM, NFT,   UTCST,
     .                   UTHAN,   UTLCK,  UTLUN,  NUT,   INQEXT,
     .                   INQOPN,  INQHAN, LOCFND, MNM                 )
 
C
C        First, check FAILED(), and return if anything has gone awry.
C        Recall HANDLE was initialized to 0, and this invalid
C        value is returned to the caller.
C
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZDDHOPN' )
            RETURN
         END IF
 
C
C        Now perform some simple sanity checks before preparing to
C        load the file.  First check to see if the file exists, it must
C        if we are going to open it with ACCMET set to READ or WRITE.
C
         IF ( .NOT. INQEXT ) THEN
 
C
C           Recall HANDLE was initialized to 0, and this invalid
C           value is returned to the caller.
C
            CALL SETMSG ( 'The file ''#'' does not exist.' )
            CALL ERRCH  ( '#', LOCFNM                      )
            CALL SIGERR ( 'SPICE(FILENOTFOUND)'            )
            CALL CHKOUT ( 'ZZDDHOPN'                       )
            RETURN
 
         END IF
 
C
C        Now if the file was not found in the file table, and it is
C        attached to a unit, this presents a problem.
C
         IF ( ( .NOT. LOCFND ) .AND. ( INQOPN ) ) THEN
 
C
C           Get the unit to include in the error message.
C
            INQUIRE ( FILE   = LOCFNM,
     .                NUMBER = LOCLUN,
     .                IOSTAT = IOSTAT  )
 
C
C           Since we performed a very similar INQUIRE statement in
C           ZZDDHF2H, a non-zero IOSTAT value indicates a severe error.
C
            IF ( IOSTAT .NE. 0 ) THEN
 
C
C              Recall HANDLE was initialized to 0, and this invalid
C              value is returned to the caller.
C
               CALL SETMSG ( 'INQUIRE failed.' )
               CALL SIGERR ( 'SPICE(BUG)'      )
               CALL CHKOUT ( 'ZZDDHOPN'        )
               RETURN
 
            END IF
 
C
C           Signal the error. Recall HANDLE was initialized to 0, and
C           this invalid value is returned to the caller.
C
            CALL SETMSG ( 'The file ''#'' is already connected to '
     .      //            'unit #.'                                 )
            CALL ERRCH  ( '#', LOCFNM                               )
            CALL ERRINT ( '#', LOCLUN                               )
            CALL SIGERR ( 'SPICE(IMPROPEROPEN)'                     )
            CALL CHKOUT ( 'ZZDDHOPN'                                )
            RETURN
 
         END IF
 
C
C        Lastly check to see if the file in the file table, and
C        perform the appropriate sanity checks.
C
         IF ( LOCFND ) THEN
 
            FINDEX = BSRCHI ( ABS(INQHAN), NFT, FTABS )
 
C
C           Check to see if the requested architecture does not match
C           that of the entry in the file table.
C
            IF ( FILARC .NE. FTARC(FINDEX) ) THEN
 
C
C              Recall HANDLE was initialized to 0, and this invalid
C              value is returned to the caller.
C
               CALL SETMSG ( 'The attempt to load file ''#'' as a # '
     .         //            'has failed because it is already loaded '
     .         //            'as a #.'                                 )
               CALL ERRCH  ( '#', LOCFNM                               )
               CALL ERRCH  ( '#', STRARC(FILARC)                       )
               CALL ERRCH  ( '#', STRARC(FTARC(FINDEX))                )
               CALL SIGERR ( 'SPICE(FILARCMISMATCH)'                   )
               CALL CHKOUT ( 'ZZDDHOPN'                                )
               RETURN
 
            END IF
 
C
C           Check to see if the access method is anything other
C           than READ.  If so, signal the appropriate error.
C           Note: this is only for READ.
C
            IF ( ACCMET .NE. READ ) THEN
 
C
C              Recall HANDLE was initialized to 0, and this invalid
C              value is returned to the caller.
C
               CALL SETMSG ( 'File ''#'' already loaded.' )
               CALL ERRCH  ( '#', LOCFNM                  )
               CALL SIGERR ( 'SPICE(FILEOPENCONFLICT)'    )
               CALL CHKOUT ( 'ZZDDHOPN'                   )
               RETURN
 
            END IF
 
C
C           If we reach here, then we have a file that exists
C           in the table, and the caller is attempting to load it
C           for READ access.  Check to make certain it is not
C           already loaded with another method.
C
            IF ( ACCMET .NE. FTAMH(FINDEX) ) THEN
 
C
C              Recall HANDLE was initialized to 0, and this invalid
C              value is returned to the caller.
C
               CALL SETMSG ( 'Unable to load file ''#'' for # '
     .         //            'access.  It is already loaded with '
     .         //            'the conflicting access #.'           )
               CALL ERRCH  ( '#', LOCFNM                           )
               CALL ERRCH  ( '#', STRAMH(ACCMET)                   )
               CALL ERRCH  ( '#', STRAMH(FTAMH(FINDEX))            )
               CALL SIGERR ( 'SPICE(RWCONFLICT)'                   )
               CALL CHKOUT ( 'ZZDDHOPN'                            )
               RETURN
 
            END IF
 
C
C           If we make it this far, the file is in the file table
C           and all the sanity checks have passed. Return to the
C           caller as this is effectively a no-op.
C
            HANDLE = FTHAN ( FINDEX )
 
            CALL CHKOUT( 'ZZDDHOPN' )
            RETURN
 
         END IF
 
      END IF
 
C
C     Now check to see if there is room in the file table for this
C     new file.
C
      IF ( NFT .EQ. FTSIZE ) THEN
 
C
C        Recall HANDLE was initialized to 0, and this invalid
C        value is returned to the caller.
C
         CALL SETMSG ( 'The file table is full, with # entries. '
     .   //            'As a result, the file ''#'' could not be '
     .   //            'loaded.'                                   )
         CALL ERRINT ( '#', NFT                                    )
         CALL ERRCH  ( '#', LOCFNM                                 )
         CALL SIGERR ( 'SPICE(FTFULL)'                             )
         CALL CHKOUT ( 'ZZDDHOPN'                                  )
         RETURN
 
      END IF
 
C
C     We are about to attempt a HANDLE to LUN connection, increment
C     the request counter.
C
      CALL ZZDDHRCM ( NUT, UTCST, REQCNT )
 
C
C     Free up a logical unit in the UNIT table for our usage.
C
      CALL ZZDDHGTU ( UTCST, UTHAN, UTLCK, UTLUN, NUT, UINDEX )
 
C
C     Check FAILED() since ZZDDHGTU may have invoked GETLUN.
C     Recall HANDLE was initialized to 0, and this invalid
C     value is returned to the caller.
C
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZDDHOPN' )
         RETURN
      END IF
 
C
C     Trim up the filename.
C
      IF ( ACCMET .NE. SCRTCH ) THEN
         LCHAR = RTRIM ( LOCFNM )
      END IF
 
C
C     If we have made it this far, then we're ready to perform the
C     appropriate open.  First get the handle ready.
C
      NEXT = NEXT + 1
 
C
C     Determine the sign of the new handle based on the requested
C     METHOD.
C
      IF ( ACCMET .EQ. READ ) THEN
         UTHAN ( UINDEX ) = NEXT
      ELSE
         UTHAN ( UINDEX ) = -NEXT
      END IF
 
C
C     The code that follows is structured a little strangely.  This
C     discussion is an attempt to clarify what the code does and
C     the motivation that led to its peculiar construction.
C
C     First, the file, scratch or otherwise, is opened with the
C     appropriate OPEN statement.  Then, the logical ERROR is set
C     to TRUE or FALSE depending on whether and IOSTAT error has
C     occurred as a result of the OPEN.  At this point, the code
C     enters into a IF block structured in the following manner:
C
C        IF ( ERROR ) THEN
C
C           Signal the IOSTAT related error from the OPEN statement.
C
C        ELSE IF ( ACCMET .EQ SCRTCH ) THEN
C
C           Attempt to INQUIRE on the UNIT assigned to the scratch
C           file to determine its name.  Store a default value,
C           in the event one is not returned.
C
C        ELSE IF ( ACCMET .EQ. READ ) .OR. ( ACCMET .EQ. WRITE ) THEN
C
C           Examine the preexisting file to determine if its FTP
C           detection string, file architecture, and binary
C           file format are acceptable.  If not, then signal the
C           error, set ERROR to TRUE, and do not check out or
C           return.
C
C        END IF
C
C        IF ( ERROR ) THEN
C
C           Remove the UNIT from the unit table. Decrement NEXT,
C           since the current value is not to be assigned as
C           a handle for this file. Check out and return.
C
C        END IF
C
C     The reason the code is structured in this unusual fashion
C     is to allow for a single treatment of the clean up on error
C     code to exist.
C
 
C
C     Perform the OPEN.  Branch on the appropriate access method.
C
      IF ( ACCMET .EQ. SCRTCH ) THEN
 
         OPEN ( UNIT   = UTLUN(UINDEX),
     .          ACCESS = 'DIRECT',
     .          RECL   = RECL,
     .          STATUS = 'SCRATCH',
     .          IOSTAT = IOSTAT           )
 
         BFF = NATBFF
 
      ELSE IF ( ACCMET .EQ. NEW ) THEN
 
         OPEN ( UNIT   = UTLUN(UINDEX),
     .          FILE   = LOCFNM(1:LCHAR),
     .          ACCESS = 'DIRECT',
     .          RECL   = RECL,
     .          STATUS = 'NEW',
     .          IOSTAT = IOSTAT           )
 
         BFF = NATBFF
 
      ELSE IF ( ACCMET .EQ. READ ) THEN
 
         OPEN ( UNIT   = UTLUN(UINDEX),
     .          FILE   = LOCFNM(1:LCHAR),
     .          ACCESS = 'DIRECT',
     .          RECL   = RECL,
     .          STATUS = 'OLD',
     .          IOSTAT = IOSTAT           )
 
      ELSE IF ( ACCMET .EQ. WRITE ) THEN
 
         OPEN ( UNIT   = UTLUN(UINDEX),
     .          FILE   = LOCFNM(1:LCHAR),
     .          ACCESS = 'DIRECT',
     .          RECL   = RECL,
     .          STATUS = 'OLD',
     .          IOSTAT = IOSTAT           )
 
      END IF
 
C
C     Verify that IOSTAT is non-zero.
C
      ERROR = ( IOSTAT .NE. 0 )
 
C
C     Partially process the error.
C
      IF ( ERROR ) THEN
 
C
C        Now signal the error, but delay cleaning up and checking
C        out until leaving this IF block.
C
         IF ( ACCMET .EQ. SCRTCH ) THEN
            CALL SETMSG ( 'Attempt to open scratch file failed. '
     .      //            'IOSTAT was #.'                         )
         ELSE IF ( ACCMET .EQ. NEW ) THEN
            CALL SETMSG ( 'Attempt to create new file, ''$'' '
     .      //            'failed. IOSTAT was #.'                 )
         ELSE
            CALL SETMSG ( 'Attempt to open file, ''$'' for % '
     .      //            'access failed. IOSTAT was #.'          )
         END IF
 
         CALL ERRCH  ( '$', LOCFNM                                )
         CALL ERRCH  ( '%', STRAMH ( ACCMET )                     )
         CALL ERRINT ( '#', IOSTAT                                )
         CALL SIGERR ( 'SPICE(FILEOPENFAIL)'                      )
 
C
C     If no IOSTAT based error has occurred as a result of the OPEN
C     statement, then perform any remaining checks or I/O operations
C     that are necessary to support loading the file.
C
      ELSE IF ( ACCMET .EQ. SCRTCH ) THEN
 
C
C        Inquire on the logical unit to produce the file name for
C        the scratch file.  Set the initial value of LOCFNM, in case
C        the INQUIRE does not replace it.
C
         LOCFNM = '# SCRATCH FILE'
         CALL REPMC ( LOCFNM, '#', STRARC(FILARC), LOCFNM )
 
         INQUIRE ( UNIT   = UTLUN(UINDEX),
     .             NAME   = LOCFNM,
     .             IOSTAT = IOSTAT         )
 
C
C        In the event that this INQUIRE failed, replace the value
C        stored in LOCFNM with the initial value.
C
         IF ( IOSTAT .NE. 0 ) THEN
 
            LOCFNM = '# SCRATCH FILE'
            CALL REPMC ( LOCFNM, '#', STRARC(FILARC), LOCFNM )
 
         END IF
 
C
C        Store the RTRIM value of this filename in LCHAR.
C
         LCHAR = RTRIM ( LOCFNM )
 
      ELSE IF ( ( ACCMET .EQ. READ  ) .OR.
     .          ( ACCMET .EQ. WRITE )      ) THEN
 
C
C        Check for FTP errors, verify that FILARC is appropriate,
C        and determine the binary file format of the preexisting
C        file LOCFNM.
C
         CALL ZZDDHPPF ( UTLUN(UINDEX), FILARC, BFF )
 
C
C        Set ERROR.
C
         ERROR = FAILED()
 
C
C        If no error has occurred, verify that BFF is among the
C        list of supported format ID codes for the requested access
C        method.
C
         IF ( .NOT. ERROR ) THEN
 
C
C           This platform supports reading from files whose
C           format codes are listed in SUPBFF.
C
            IF ( ACCMET .EQ. READ ) THEN
 
               SUPIDX = ISRCHI ( BFF, NUMSUP, SUPBFF )
 
               IF ( SUPIDX .EQ. 0 ) THEN
 
C
C                 Delay clean up and check out.
C
                  ERROR = .TRUE.
 
                  IF ( BFF .EQ. 0 ) THEN
 
                     CALL SETMSG ( 'Attempt to open file, ''#'', for '
     .               //            'read access has failed.  This '
     .               //            'file utilizes an unknown binary '
     .               //            'file format.  This error may '
     .               //            'result from attempting to open a '
     .               //            'corrupt file or one of an '
     .               //            'unknown type.'                     )
                     CALL ERRCH  ( '#', LOCFNM                         )
                     CALL SIGERR ( 'SPICE(UNSUPPORTEDBFF)'             )
 
                  ELSE
 
                     CALL SETMSG ( 'Attempt to open file, ''#'', for '
     .               //            'read access has failed.  The '
     .               //            'non-native binary file format '
     .               //            '''#'' is not currently supported '
     .               //            'on this platform.  Obtain a '
     .               //            'transfer format version, and '
     .               //            'convert it to the native format. '
     .               //            'See the Convert User''s Guide '
     .               //            'for details.'                      )
                     CALL ERRCH  ( '#', LOCFNM                         )
                     CALL ERRCH  ( '#', STRBFF(BFF)                    )
                     CALL SIGERR ( 'SPICE(UNSUPPORTEDBFF)'             )
 
                  END IF
 
               END IF
 
C
C           This platform only supports writing to files whose
C           binary formats are native.
C
            ELSE
 
C
C              Delay clean up and check out.
C
               IF ( BFF .EQ. 0 ) THEN
 
                  ERROR = .TRUE.
 
                  CALL SETMSG ( 'Attempt to open file, ''#'', for '
     .            //            'write access has failed.  This '
     .            //            'file utilizes an unknown binary '
     .            //            'file format.  This error may '
     .            //            'result from attempting to open a '
     .            //            'corrupt file or one of an unknown '
     .            //            'type.'                              )
                  CALL ERRCH  ( '#', LOCFNM                          )
                  CALL SIGERR ( 'SPICE(UNSUPPORTEDBFF)'              )
 
               ELSE IF ( BFF .NE. NATBFF ) THEN
 
                  ERROR = .TRUE.
 
                  CALL SETMSG ( 'Attempt to open file, ''#'', for '
     .            //            'write access has failed.  This '
     .            //            'file utilizes the non-native '
     .            //            'binary file format ''#''.  At '
     .            //            'this time only files of the '
     .            //            'native format, ''#'', are '
     .            //            'supported for write access.  See '
     .            //            'the Convert User''s Guide for '
     .            //            'details.'                          )
                  CALL ERRCH  ( '#', LOCFNM                         )
                  CALL ERRCH  ( '#', STRBFF(BFF)                    )
                  CALL ERRCH  ( '#', STRBFF(NATBFF)                 )
                  CALL SIGERR ( 'SPICE(UNSUPPORTEDBFF)'             )
 
               END IF
 
            END IF
 
         END IF
 
      END IF
 
C
C     If an error has occurred as a result of opening the file or
C     examining its contents, clean up and check out.
C
      IF ( ERROR ) THEN
 
C
C        Close the unit we were using.  Remember to delete the file
C        if it was a 'new' one.
C
         IF ( ACCMET .EQ. NEW ) THEN
            CLOSE ( UNIT = UTLUN(UINDEX), STATUS = 'DELETE' )
         ELSE
            CLOSE ( UNIT = UTLUN(UINDEX) )
         END IF
 
C
C        Remove the unit from the unit table, since this UNIT
C        is no longer in use.
C
         CALL ZZDDHRMU ( UINDEX, NFT, UTCST, UTHAN, UTLCK, UTLUN, NUT )
 
C
C        Decrement NEXT since this handle was never assigned to
C        a file.
C
         NEXT = NEXT - 1
 
C
C        Recall HANDLE was initialized to 0, and this invalid
C        value is returned to the caller.
C
         CALL CHKOUT ( 'ZZDDHOPN' )
         RETURN
 
      END IF
 
 
C
C     Finish filling out the unit table.
C
      UTCST ( UINDEX ) = REQCNT
 
C
C     Only scratch files get the units locked to handles, this is
C     because they only exist as long as they have a unit.
C
      UTLCK ( UINDEX ) = ACCMET .EQ. SCRTCH
 
C
C     Now fill out the file table.
C
      NFT = NFT + 1
 
C
C     Use the absolute value of the handle used to index the file
C     table.
C
      FTABS ( NFT ) = IABS ( UTHAN( UINDEX ) )
 
C
C     Assign access method, file architecture, and native binary file
C     format to the appropriate columns.
C
      FTAMH ( NFT ) = ACCMET
      FTARC ( NFT ) = FILARC
      FTBFF ( NFT ) = BFF
 
C
C     Assign the handle, filename, RTRIM ( FTNAM(NFT) ) as FTRTM, and
C     unique DP number as FTMNM.
C
      FTHAN ( NFT ) = UTHAN ( UINDEX )
      FTNAM ( NFT ) = LOCFNM(1:LCHAR)
      FTRTM ( NFT ) = LCHAR
      FTMNM ( NFT ) = MNM
 
C
C     Assign HANDLE the value of the new handle.
C
      HANDLE = FTHAN( NFT )
 
      CALL CHKOUT ( 'ZZDDHOPN' )
      RETURN
 
 
 
 
C$Procedure ZZDDHCLS ( Private --- Close file )
 
      ENTRY ZZDDHCLS ( HANDLE, ARCH, KILL )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Close the file associated with HANDLE.
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
C     DAF
C     DAS
C     PRIVATE
C
C$ Declarations
C
C     INTEGER               HANDLE
C     CHARACTER*(*)         ARCH
C     LOGICAL               KILL
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle associated with the file to close.
C     ARCH       I   Expected architecture of the handle to close.
C     KILL       I   Logical indicating whether to delete the file.
C
C$ Detailed_Input
C
C     HANDLE     is the file handle associated with the file that is
C                to be closed.
C
C     ARCH       is the expected architecture of the file associated
C                with HANDLE.
C
C     KILL       is a logical that indicates whether to kill the file
C                associated with HANDLE.  Essentially it performs:
C
C                   CLOSE ( UNIT, STATUS = 'DELETE')
C
C                on the file.  This only works if HANDLE is currently
C                assigned a UNIT in the UNIT table.
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
C     1) SPICE(FILARCMISMATCH) is signaled if the specified architecture
C        does not match the one listed in the file table.
C
C     2) SPICE(INVALIDACCESS) is signaled if KILL is set to .TRUE., and
C        HANDLE is associated with a file open for READ access.
C
C     3) SPICE(FILENOTCONNECTED) is signaled if KILL is set to .TRUE.,
C        and the file associated with handle is not currently in the
C        unit table.  The file is removed from the file table (closed)
C        as a result, even if this error is signaled.
C
C     4) If HANDLE is not found in the file table, this routine simply
C        returns to the caller.
C
C$ Files
C
C     This routine will close the file associated with HANDLE if it
C     is currently utilizing a logical unit.
C
C$ Particulars
C
C     This routine closes files in the file table and performs
C     any necessary operations to facilitate the proper disconnect
C     from any logical unit.
C
C     This routine may also be used to delete a file that is open
C     for write access if it currently has an entry in the UNIT table.
C
C$ Examples
C
C     See DAFCLS or DASLLC.
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
C     F.S. Turner     (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.1.0, 26-APR-2012 (BVS)
C
C        Updated for the new "magic number" column in the file table.
C
C-    SPICELIB Version 2.0.0, 02-APR-2001 (FST)
C
C        Added a "KILL" argument to the argument list of the routine.
C        This will allow certain raw close statements to be replaced
C        with calls to ZZDDHCLS.
C
C-    SPICELIB Version 1.0.0, 06-NOV-2001 (FST)
C
C-&
C
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 02-APR-2002 (FST)
C
C        Added the error SPICE(FILENOTCONNECTED) since the KILL
C        functionality is only required in the entry points:
C
C            DASFM - DASOPN, DASONW
C            DAFAH - DAFOPN, DAFONW
C
C        These routines open new files, so they reference newly
C        created handles that have entries in the UNIT table. Thus
C        the decision was made to signal the error
C        SPICE(FILENOTCONNECTED) rather than connect a file not
C        present in the unit table when KILL is set.
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZDDHCLS' )
      END IF
 
C
C     Do the initialization tasks.
C
      IF ( FIRST ) THEN
 
         CALL ZZDDHINI ( NATBFF, SUPBFF, NUMSUP,
     .                   STRAMH, STRARC, STRBFF  )
 
C
C        Check FAILED() only to trap the possibility of ZZDDHINI
C        signaling SPICE(BUG).
C
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZDDHCLS' )
            RETURN
         END IF
 
C
C        Clear FIRST since we've done the initialization.
C
         FIRST = .FALSE.
 
      END IF
 
C
C     Find the file in the handle table.
C
      FINDEX = BSRCHI ( ABS(HANDLE), NFT, FTABS )
 
C
C     Check to see whether we found the handle or not.
C
      IF ( FINDEX .EQ. 0 ) THEN
         CALL CHKOUT ( 'ZZDDHCLS' )
         RETURN
      ELSE IF ( FTHAN(FINDEX) .NE. HANDLE ) THEN
         CALL CHKOUT ( 'ZZDDHCLS' )
         RETURN
      END IF
 
C
C     Before actually closing the file, check the input architecture
C     matches that listed in the file table for this handle.  This is
C     to prevent one architecture's code from stepping on anothers.
C
      TMPSTR = ARCH
      CALL UCASE ( TMPSTR, TMPSTR )
      FILARC = ISRCHC ( TMPSTR, NUMARC, STRARC )
 
C
C     Check to see if FILARC matches the code stored in the FTARC
C     column of the file table for this handle.  If it doesn't,
C     signal an error.
C
      IF ( FILARC .NE. FTARC(FINDEX) ) THEN
 
         CALL SETMSG ( 'Logical unit associated with # file $, '
     .   //            'is trying to be closed by routines in '
     .   //            'in the % system.'                         )
         CALL ERRCH  ( '#', STRARC( FTARC(FINDEX) )               )
         CALL ERRCH  ( '%', TMPSTR                                )
         CALL ERRCH  ( '$', FTNAM(FINDEX)                         )
         CALL SIGERR ( 'SPICE(FILARCMISMATCH)'                    )
         CALL CHKOUT ( 'ZZDDHCLS'                                 )
         RETURN
 
      END IF
 
C
C     Now check that if KILL is set, the file is accessible for
C     WRITE.
C
      IF ( ( KILL ) .AND. ( FTAMH(FINDEX) .EQ. READ ) ) THEN
 
         CALL SETMSG ( '# file $ is open for READ access.  Attempt '
     .   //            'to close and delete file has failed. '       )
         CALL ERRCH  ( '#', STRARC( FTARC(FINDEX) )                  )
         CALL ERRCH  ( '#', FTNAM(FINDEX)                            )
         CALL SIGERR ( 'SPICE(INVALIDACCESS)'                        )
         CALL CHKOUT ( 'ZZDDHCLS'                                    )
         RETURN
 
      END IF
 
C
C     Buffer the access method for HANDLE, since we may need it
C     when deciding which close to perform.
C
      ACCMET = FTAMH (FINDEX)
 
C
C     If we reach here, we need to remove the row FINDEX from
C     the file table.
C
      DO I = FINDEX+1, NFT
 
         FTABS(I-1) = FTABS(I)
         FTAMH(I-1) = FTAMH(I)
         FTARC(I-1) = FTARC(I)
         FTBFF(I-1) = FTBFF(I)
         FTHAN(I-1) = FTHAN(I)
         FTNAM(I-1) = FTNAM(I)
         FTRTM(I-1) = FTRTM(I)
         FTMNM(I-1) = FTMNM(I)
 
      END DO
 
      NFT = NFT - 1
 
C
C     Locate HANDLE in the unit table.
C
      UINDEX = ISRCHI ( HANDLE, NUT, UTHAN )
 
      IF ( UINDEX .NE. 0 ) THEN
 
C
C        Close the unit.
C
         IF ( ( KILL ) .AND. ( ACCMET .NE. SCRTCH ) ) THEN
            CLOSE ( UNIT = UTLUN(UINDEX), STATUS = 'DELETE' )
         ELSE
            CLOSE ( UNIT = UTLUN(UINDEX) )
         END IF
 
C
C        Remove its entry from the unit table.
C
         CALL ZZDDHRMU ( UINDEX, NFT, UTCST, UTHAN, UTLCK, UTLUN, NUT )
 
      ELSE
 
C
C        First, check to see if KILL is set, if it is signal an error
C        since we are unable to delete the file.
C
         IF ( ( KILL ) .AND. ( ACCMET .NE. SCRTCH ) ) THEN
 
            CALL SETMSG ( 'File successfully closed.  Unable to '
     .      //            'delete file as requested.  File not '
     .      //            'currently present in the UNIT table. ' )
            CALL SIGERR ( 'SPICE(FILENOTCONNECTED)'               )
            CALL CHKOUT ( 'ZZDDHCLS'                              )
            RETURN
 
         END IF
 
C
C        If we were unable to find the HANDLE in the unit table,
C        check to see if we have to clean up the UNIT table.
C
         IF ( NFT .LT. NUT ) THEN
 
            UINDEX = ISRCHI ( 0, NUT, UTHAN )
 
C
C           Now check to see if we located a zero valued handle.
C           If we did not manage to, then this is an error condition,
C           since we have more LUNs listed in the unit table than
C           files in the file table.
C
            IF ( UINDEX .EQ. 0 ) THEN
               CALL SETMSG ( 'There are less files in the file table '
     .         //            'than units in the unit table, and no '
     .         //            'row with a zero-valued handle can be '
     .         //            'found.  This should never occur.'        )
               CALL SIGERR ( 'SPICE(BUG)'                              )
               CALL CHKOUT ( 'ZZDDHCLS'                                )
               RETURN
            END IF
 
C
C           Free the unit.
C
            CALL FRELUN ( UTLUN(UINDEX) )
 
C
C           Compress the table.
C
            DO I = (UINDEX + 1), NUT
 
               UTCST(I-1) = UTCST(I)
               UTHAN(I-1) = UTHAN(I)
               UTLCK(I-1) = UTLCK(I)
               UTLUN(I-1) = UTLUN(I)
 
            END DO
 
C
C           Decrement NUT.
C
            NUT = NUT - 1
 
         END IF
      END IF
 
      CALL CHKOUT ( 'ZZDDHCLS' )
      RETURN
 
 
 
 
C$Procedure ZZDDHHLU ( Private --- Handle to Logical Unit )
 
      ENTRY ZZDDHHLU ( HANDLE, ARCH, LOCK, UNIT )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Return the logical unit associated with a handle, in the event
C     the handle is not connected to a unit, connect it to one.
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
C     DAS
C     DAF
C     PRIVATE
C
C$ Declarations
C
C     INTEGER               HANDLE
C     CHARACTER*(*)         ARCH
C     LOGICAL               LOCK
C     INTEGER               UNIT
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle associated with the file of interest.
C     ARCH       I   Expected file architecture.
C     LOCK       I   Logical indicating to lock UNIT to HANDLE.
C     UNIT       O   Corresponding logical unit.
C
C$ Detailed_Input
C
C     HANDLE     is the handle associated with the file to retrieve a
C                logical unit.
C
C     ARCH       is the expected file architecture of the file
C                associated with HANDLE.
C
C     LOCK       is a logical that indicates whether the UNIT should be
C                locked to HANDLE.  Locked units will keep the files
C                open and assigned the same logical unit.  They may
C                only be unlocked by calling ZZDDHUNL.
C
C$ Detailed_Output
C
C     UNIT       is the logical unit that is currently assigned to
C                HANDLE.  This unit may be used to perform I/O
C                operations on the file.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) The error SPICE(NOSUCHHANDLE) is signaled when HANDLE is not
C        found in the file table.  The value of UNIT is undefined when
C        this error occurs.
C
C     2) The error SPICE(FILARCMISMATCH) is signaled if the specified
C        architecture does not match the one listed for HANDLE in the
C        file table.  The value of UNIT is undefined when this error
C        occurs.
C
C     3) SPICE(FILEOPENFAIL) is signaled only when an attempt to
C        attach a logical unit to the file associated with HANDLE
C        fails.  The value of UNIT is undefined when this error
C        occurs.
C
C     4) The error SPICE(HLULOCKFAILED) is signaled when the input
C        LOCK logical has a value of TRUE, and there are no free
C        'lockable' units left in the unit table.  The value of UNIT
C        returned when this error is signaled is undefined.
C
C$ Files
C
C     If HANDLE refers to a file not currently connected to a logical
C     unit, this routine will locate an entry in the unit table;
C     disconnect it if necessary; and connect the file associated with
C     HANDLE in its place.
C
C$ Particulars
C
C     This routine is used to retrieve a logical unit for a desired
C     handle.  It also serves as a mechanism for locking the UNIT
C     to HANDLE relationship for a particular handle.  This routine
C     can not be used to unlock this relationship.  See ZZDDHUNL for
C     that functionality.
C
C$ Examples
C
C     See DAFHLU or DASHLU for sample usage.
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
C     F.S. Turner     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 06-NOV-2001 (FST)
C
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZDDHHLU' )
      END IF
 
C
C     Do the initialization tasks.
C
      IF ( FIRST ) THEN
 
         CALL ZZDDHINI ( NATBFF, SUPBFF, NUMSUP,
     .                   STRAMH, STRARC, STRBFF  )
 
C
C        Check FAILED() only to trap the possibility of ZZDDHINI
C        signaling SPICE(BUG).
C
         IF ( FAILED() ) THEN
            UNIT = 0
            CALL CHKOUT ( 'ZZDDHHLU' )
            RETURN
         END IF
 
C
C        Clear FIRST since we've done the initialization.
C
         FIRST = .FALSE.
 
      END IF
 
C
C     Locate HANDLE in the file table.
C
      FINDEX = BSRCHI ( ABS(HANDLE), NFT, FTABS )
 
      IF ( FINDEX .EQ. 0 ) THEN
         ERROR = .TRUE.
      ELSE IF ( FTHAN(FINDEX) .NE. HANDLE ) THEN
         ERROR = .TRUE.
      ELSE
         ERROR = .FALSE.
      END IF
 
      IF ( ERROR ) THEN
 
         UNIT = 0
 
         CALL SETMSG ( 'There is no file loaded with handle = #' )
         CALL ERRINT ( '#', HANDLE                               )
         CALL SIGERR ( 'SPICE(NOSUCHHANDLE)'                     )
         CALL CHKOUT ( 'ZZDDHHLU'                                )
         RETURN
      END IF
 
C
C     Before actually fetching the unit, check the input architecture
C     matches that listed in the file table for this handle.  This is
C     to prevent one architectures code from stepping on anothers.
C
      TMPSTR = ARCH
      CALL UCASE ( TMPSTR, TMPSTR )
      FILARC = ISRCHC ( TMPSTR, NUMARC, STRARC )
 
C
C     Check to see if FILARC matches the code stored in the FTARC
C     column of the file table for this handle.  If it doesn't,
C     signal an error.
C
      IF ( FILARC .NE. FTARC(FINDEX) ) THEN
 
         UNIT = 0
 
         CALL SETMSG ( 'Logical unit associated with # file $, '
     .   //            'is trying to be unlocked by routines in '
     .   //            'in the % system.'                         )
         CALL ERRCH  ( '#', STRARC( FTARC(FINDEX) )               )
         CALL ERRCH  ( '%', TMPSTR                                )
         CALL ERRCH  ( '$', FTNAM(FINDEX)                         )
         CALL SIGERR ( 'SPICE(FILARCMISMATCH)'                    )
         CALL CHKOUT ( 'ZZDDHHLU'                                 )
         RETURN
 
      END IF
 
C
C     If we make it this far, then we will be processing a handle
C     to logical unit request.  Increment REQCNT.
C
      CALL ZZDDHRCM ( NUT, UTCST, REQCNT )
 
C
C     Now check to see if the handle is already present in the
C     unit table.
C
      UINDEX = ISRCHI ( HANDLE, NUT, UTHAN )
 
C
C     Check to see if we didn't locate the HANDLE in the table.
C     If we didn't, open the file associated with HANDLE again,
C     and get it into the unit table.
C
      IF ( UINDEX .EQ. 0 ) THEN
 
C
C        We need a unit from the unit table, get one.
C
         CALL ZZDDHGTU ( UTCST, UTHAN, UTLCK, UTLUN, NUT, UINDEX )
 
C
C        Check FAILED, since ZZDDHGTU may have invoked GETLUN.
C
         IF ( FAILED() ) THEN
 
            UNIT = 0
 
            CALL CHKOUT ( 'ZZDDHHLU' )
            RETURN
         END IF
 
C
C        Re-attach the file to a logical unit.  Branch based on the
C        access method stored in the file table.
C
         IF ( ( FTAMH(FINDEX) .EQ. NEW   ) .OR.
     .        ( FTAMH(FINDEX) .EQ. WRITE )      ) THEN
 
            OPEN ( UNIT   = UTLUN(UINDEX),
     .             FILE   = FTNAM(FINDEX)(1:FTRTM(FINDEX)),
     .             ACCESS = 'DIRECT',
     .             RECL   = RECL,
     .             STATUS = 'OLD',
     .             IOSTAT = IOSTAT           )
 
         ELSE IF ( FTAMH(FINDEX) .EQ. READ ) THEN
 
            OPEN ( UNIT   = UTLUN(UINDEX),
     .             FILE   = FTNAM(FINDEX)(1:FTRTM(FINDEX)),
     .             ACCESS = 'DIRECT',
     .             RECL   = RECL,
     .             STATUS = 'OLD',
     .             IOSTAT = IOSTAT           )
 
         ELSE
 
            UNIT = 0
 
            CALL SETMSG ( 'Invalid access method.  This error '
     .      //            'should never be signalled.'          )
            CALL SIGERR ( 'SPICE(BUG)'                          )
            CALL CHKOUT ( 'ZZDDHHLU'                            )
            RETURN
         END IF
 
C
C        Check IOSTAT for troubles.
C
         IF ( IOSTAT .NE. 0 ) THEN
 
C
C           The re-open was unsuccessful, leave the entry in the file
C           table and clean up the row in the unit table before
C           returning.  Normally when we call ZZDDHRMU it is to
C           remove a unit from the unit table.  In this case we
C           know the unit will remain since we have not decreased
C           the entries in the file table.
C
            CALL ZZDDHRMU ( UINDEX, NFT,   UTCST, UTHAN,
     .                      UTLCK,  UTLUN, NUT           )
 
C
C           Now signal the error.
C
            UNIT = 0
 
            CALL SETMSG ( 'Attempt to reconnect logical unit to '
     .      //            'file ''#'' failed. IOSTAT was #.'      )
            CALL ERRCH  ( '#', FTNAM(FINDEX)                      )
            CALL ERRINT ( '#', IOSTAT                             )
            CALL SIGERR ( 'SPICE(FILEOPENFAIL)'                   )
            CALL CHKOUT ( 'ZZDDHHLU'                              )
            RETURN
 
         END IF
 
C
C        Lastly populate the unit table values.
C
         UTHAN(UINDEX) = FTHAN(FINDEX)
         UTLCK(UINDEX) = .FALSE.
 
      END IF
 
C
C     At this point UINDEX points to the row in the unit table that
C     contains the connection information.  We need to update the cost
C     row with the new value of REQCNT, and then set the lock row to
C     TRUE if a lock request was made.
C
      UTCST(UINDEX) = REQCNT
 
      IF ( LOCK .AND. (.NOT. UTLCK(UINDEX)) ) THEN
 
C
C        First check to see if we have enough lockable units
C        left in the unit table.
C
         LOCKED = ZZDDHCLU ( UTLCK, NUT )
 
         IF ( LOCKED .GE. ( UTSIZE - RSVUNT - SCRUNT ) ) THEN
 
            UNIT = 0
 
            CALL SETMSG ( 'Unable to lock handle for file ''#'' '
     .      //            'to a logical unit.  There are no rows '
     .      //            'available for locking in the unit table.' )
            CALL ERRCH  ( '#', FTNAM(FINDEX)                         )
            CALL SIGERR ( 'SPICE(HLULOCKFAILED)'                     )
            CALL CHKOUT ( 'ZZDDHHLU'                                 )
            RETURN
 
         END IF
 
         UTLCK(UINDEX) = .TRUE.
 
      END IF
 
C
C     Set the value of UNIT and return.
C
      UNIT = UTLUN(UINDEX)
 
      CALL CHKOUT ( 'ZZDDHHLU' )
      RETURN
 
 
 
 
C$Procedure ZZDDHUNL ( Private --- Unlock Logical Unit from Handle )
 
      ENTRY ZZDDHUNL ( HANDLE, ARCH )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Unlock a logical unit from the specified handle.
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
C     DAS
C     DAF
C     PRIVATE
C
C$ Declarations
C
C     INTEGER               HANDLE
C     CHARACTER*(*)         ARCH
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle assigned to the file to unlock.
C     ARCH       I   Expected architecture of the handle to unlock.
C
C$ Detailed_Input
C
C     HANDLE     is the file handle associated with the file that
C                is to have its logicial unit lock released.
C
C     ARCH       is the expected architecture of the file associated
C                with HANDLE.
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
C     1) If HANDLE is 0, not found in the unit table, or found and
C        not currently locked, this routine just returns to the
C        caller.
C
C     2) SPICE(FILARCMISMATCH) is signaled if the specified architecture
C        does not match the one listed in the file table.
C
C     3) If HANDLE is associated with a scratch file, this routine
C        simply returns, as scratch files may not be unlocked from
C        their logical units.
C
C$ Files
C
C     This routine does not explicitly alter the open or closed
C     state of the file associated with HANDLE.
C
C$ Particulars
C
C     This routine allows users to unlock a handle from it's logical
C     unit in the event a handle to logical unit request was made
C     with the LOCK argument set to true.  (DAFHLU and DASHLU both
C     lock units, and require a call to this routine to unlock them).
C
C$ Examples
C
C     See some routine that calls this one (TBD).
C
C$ Restrictions
C
C     This routine utilizes discovery check in and out.  However,
C     routines in the initialization loop may signal the error
C     SPICE(BUG) under the conditions of the existence of a bug.
C     Since this routine utilizes discovery check in and out,
C     no check in or out is performed around the initialization
C     block.  This is by design.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 06-NOV-2001 (FST)
C
C
C-&
 
C
C     Standard SPICE discovery error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
C
C     Do the initialization tasks.
C
      IF ( FIRST ) THEN
 
         CALL ZZDDHINI ( NATBFF, SUPBFF, NUMSUP,
     .                   STRAMH, STRARC, STRBFF  )
 
C
C        Check FAILED() only to trap the possibility of ZZDDHINI
C        signaling SPICE(BUG).  No check out is performed, see the
C        $Restrictions section of the entry point header for details.
C
         IF ( FAILED() ) THEN
            RETURN
         END IF
 
C
C        Clear FIRST since we've done the initialization.
C
         FIRST = .FALSE.
 
      END IF
C
C     Prevent the user from locating zero handle rows.  This is not
C     really necessary since zero handle rows in the unit table are
C     empty and awaiting connections.  The state of the UTLCK is
C     not significant.
C
      IF ( HANDLE .EQ. 0 ) THEN
         RETURN
      END IF
 
C
C     Look up the handle in the unit table.
C
      UINDEX = ISRCHI ( HANDLE, NUT, UTHAN )
 
C
C     Now check the results of the lookup.  If HANDLE was not found
C     in the unit table or the unit was not locked, just return as
C     there is nothing to do.
C
      IF ( UINDEX .EQ. 0 ) THEN
         RETURN
      ELSE IF ( .NOT. UTLCK(UINDEX) ) THEN
         RETURN
      END IF
 
C
C     Now look up the handle in the table. Remember FTABS is a sorted
C     list in increasing order.
C
      FINDEX = BSRCHI ( ABS(HANDLE), NFT, FTABS )
 
C
C     Check to see if HANDLE is in the file table.  We know it has
C     to be since it is in the unit table if we make it this far.
C     These checks are just for safety's sake.
C
      IF ( FINDEX .EQ. 0 ) THEN
 
         CALL CHKIN  ( 'ZZDDHUNL'                                  )
         CALL SETMSG ( 'HANDLE # was not found in the file table '
     .   //            'but was located in the unit table.  This '
     .   //            'error should never occur.'                 )
         CALL ERRINT ( '#', HANDLE                                 )
         CALL SIGERR ( 'SPICE(BUG)'                                )
         CALL CHKOUT ( 'ZZDDHUNL'                                  )
         RETURN
 
      ELSE IF ( FTHAN(FINDEX) .NE. HANDLE ) THEN
 
         CALL CHKIN  ( 'ZZDDHUNL'                                  )
         CALL SETMSG ( 'HANDLE # was not found in the file table '
     .   //            'but was located in the unit table.  This '
     .   //            'error should never occur.'                 )
         CALL ERRINT ( '#', HANDLE                                 )
         CALL SIGERR ( 'SPICE(BUG)'                                )
         CALL CHKOUT ( 'ZZDDHUNL'                                  )
         RETURN
 
      END IF
 
C
C     Before actually unlocking the unit, check the input architecture
C     matches that listed in the file table for this handle.  This is
C     to prevent one architectures code from stepping on anothers.
C
      TMPSTR = ARCH
      CALL UCASE ( TMPSTR, TMPSTR )
      FILARC = ISRCHC ( TMPSTR, NUMARC, STRARC )
 
C
C     Check to see if FILARC matches the code stored in the FTARC
C     column of the file table for this handle.  If it doesn't,
C     signal an error.
C
      IF ( FILARC .NE. FTARC(FINDEX) ) THEN
 
         CALL CHKIN  ( 'ZZDDHUNL'                                 )
         CALL SETMSG ( 'Logical unit associated with # file $, '
     .   //            'is trying to be unlocked by routines in '
     .   //            'in the % system.'                         )
         CALL ERRCH  ( '#', STRARC( FTARC(FINDEX) )               )
         CALL ERRCH  ( '%', TMPSTR                                )
         CALL ERRCH  ( '$', FTNAM(FINDEX)                         )
         CALL SIGERR ( 'SPICE(FILARCMISMATCH)'                    )
         CALL CHKOUT ( 'ZZDDHUNL'                                 )
         RETURN
 
      END IF
 
C
C     Lastly, check to see if the access method for HANDLE indicates
C     scratch access.  If it is, just return, since scratch files
C     can not have their units unlocked.
C
      IF ( FTAMH(FINDEX) .EQ. SCRTCH ) THEN
         RETURN
      END IF
 
      UTLCK(UINDEX) = .FALSE.
 
      RETURN
 
 
 
 
C$Procedure ZZDDHNFO ( Private --- Get information about a Handle )
 
      ENTRY ZZDDHNFO ( HANDLE, FNAME, INTARC, INTBFF, INTAMH, FOUND )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Get information about the file attached to HANDLE.
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
C     DAF
C     DAS
C     PRIVATE
C
C$ Declarations
C
C     INTEGER               HANDLE
C     CHARACTER*(*)         FNAME
C     INTEGER               INTARC
C     INTEGER               INTBFF
C     INTEGER               INTAMH
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle assigned to file of interest.
C     FNAME      O   Name of the file associated with HANDLE.
C     INTARC     O   Integer code for FNAME's file architecture.
C     INTBFF     O   Integer code for FNAME's binary file format.
C     INTAMH     O   Integer code for FNAME's access method.
C     FOUND      O   Logical that indicates if HANDLE was found.
C
C$ Detailed_Input
C
C     HANDLE     is the file handle associated with the file for which
C                information is requested.
C
C$ Detailed_Output
C
C     FNAME      is the name of the file used associated with HANDLE.
C                This is the name used to load the file originally.
C
C     INTARC     is an integer code for FNAME's file architecture.
C                See the include file 'zzddhman.inc' for particulars.
C                The following are possible outputs:
C
C                   DAS - Direct Access, Segregated File Architecture
C                   DAF - Double Precision Array File Architecture
C
C     INTBFF     is an integer code that represents FNAME's binary
C                file format.  See the include file 'zzddhman.inc' for
C                particulars.  The following are the possible outputs:
C
C                   BIGI3E - Big Endian IEEE Floating Point Format
C                   LTLI3E - Little Endian IEEE Floating Point Format
C                   VAXGFL - VAX G-Float Format
C                   VAXDFL - VAX D-Float Format
C
C     INTAMH     is an integer code that represents FNAME's access
C                method.  See the include file 'zzddhman.inc' for
C                particulars.  The following are possible outputs:
C
C                   READ   - File was loaded for read access
C                   WRITE  - File was loaded for read/write access
C                   NEW    - New file was created for read/write access
C                   SCRTCH - Scratch file created for read/write access
C
C     FOUND      is a logical if set to TRUE indicates that HANDLE
C                was located in the file table.  Otherwise, it was
C                not found in the table.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1) If FOUND is FALSE, then the other output arguments
C        are undefined.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine provides access to information necessary for
C     translation and other I/O based tasks to modules that are
C     not entry points to this handle manager.
C
C$ Examples
C
C     See ZZDAFGFR, ZZDAFGSR, or ZZDAFGDR for sample usage.
C
C$ Restrictions
C
C     Routines in the call tree of this routine may signal the error
C     SPICE(BUG) under the conditions of the existence of a bug
C     in routines the initialization loop invokes.  Since this
C     routine is error free with the exception of this bug condition,
C     it does not participate in tracing by design.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 06-NOV-2001 (FST)
C
C
C-&
 
C
C     Do the initialization tasks.
C
      IF ( FIRST ) THEN
 
         CALL ZZDDHINI ( NATBFF, SUPBFF, NUMSUP,
     .                   STRAMH, STRARC, STRBFF  )
 
C
C        Check FAILED(), and return on failure.  We are not checking
C        out or in since this routine would be error free if not for
C        the possibility of ZZDDHINI signaling SPICE(BUG).  See
C        $Restrictions for details.
C
         IF ( FAILED() ) THEN
            RETURN
         END IF
 
C
C        Clear FIRST since we've done the initialization.
C
         FIRST = .FALSE.
 
      END IF
 
C
C     Look up the handle in the table.  Remember FTABS is sorted
C     listed in increasing order.
C
      FINDEX = BSRCHI ( ABS(HANDLE), NFT, FTABS )
 
C
C     Check to see if HANDLE is in the handle table.  Remember that
C     we are indexing the table using the absolute value of handle.
C     So include a check to see that HANDLE is FTHAN(FINDEX).
C
      IF ( FINDEX .EQ. 0 ) THEN
         FNAME  = ' '
         INTARC = 0
         INTBFF = 0
         INTAMH = 0
         FOUND  = .FALSE.
         RETURN
      ELSE IF ( FTHAN(FINDEX) .NE. HANDLE ) THEN
         FNAME  = ' '
         INTARC = 0
         INTBFF = 0
         INTAMH = 0
         FOUND  = .FALSE.
         RETURN
      END IF
 
C
C     If we make it this far, then we have a handle that is in
C     the handle table at row FINDEX.
C
      FOUND  = .TRUE.
      FNAME  = FTNAM(FINDEX) ( 1:FTRTM(FINDEX) )
      INTARC = FTARC(FINDEX)
      INTBFF = FTBFF(FINDEX)
      INTAMH = FTAMH(FINDEX)
 
      RETURN
 
 
 
 
C$Procedure ZZDDHISN ( Private --- Is Handle Native? )
 
      ENTRY ZZDDHISN ( HANDLE, NATIVE, FOUND )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Determine whether the file attached to HANDLE is uses the
C     binary file format native to the system.
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
C     DAS
C     DAF
C     PRIVATE
C
C$ Declarations
C
C     INTEGER               HANDLE
C     LOGICAL               NATIVE
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle assigned to file to determine format.
C     NATIVE     O   Indicates if the file format is native.
C     FOUND      O   Indicates if HANDLE is currently attached to file.
C
C$ Detailed_Input
C
C     HANDLE     is the file handle associated with the file that is
C                to be determined to be native or not.
C
C$ Detailed_Output
C
C     NATIVE     is a logical that when set to TRUE indicates that
C                the file associated with HANDLE is of the native
C                binary file format for the current platform. If
C                FALSE, then the file is of an alien file format.
C
C     FOUND      is a logical that when set to TRUE indicates that
C                HANDLE was found in the file table and is associated
C                with a file.  If FALSE, then NATIVE remains unchanged,
C                since the file was not found in the table.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error Free.
C
C     1) In the event that HANDLE can not be found in the file table,
C        FOUND is set to FALSE and NATIVE is left unchanged.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine simply answers the question:  "Is the file attached
C     to this handle of the native binary file format?"
C
C$ Examples
C
C     See DAFRDA for sample usage.
C
C$ Restrictions
C
C     Routines in the call tree of this routine may signal the error
C     SPICE(BUG) under the conditions of the existence of a bug
C     in routines the initialization loop invokes.  Since this
C     routine is error free with the exception of this bug condition,
C     it does not participate in tracing by design.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 06-NOV-2001 (FST)
C
C
C-&
 
C
C     Do the initialization tasks.
C
      IF ( FIRST ) THEN
 
         CALL ZZDDHINI ( NATBFF, SUPBFF, NUMSUP,
     .                   STRAMH, STRARC, STRBFF  )
 
C
C        Check FAILED(), and return on failure.  We are not checking
C        out or in since this routine would be error free if not for
C        the possibility of ZZDDHINI signaling SPICE(BUG).  See
C        $Restrictions for details.
C
         IF ( FAILED() ) THEN
            RETURN
         END IF
 
C
C        Clear FIRST since we've done the initialization.
C
         FIRST = .FALSE.
 
      END IF
 
C
C     Look up the handle in the table. Remember FTABS is sorted
C     listed in increasing order.
C
      FINDEX = BSRCHI ( ABS(HANDLE), NFT, FTABS )
 
C
C     Check to see if HANDLE is in the handle table.  Remember
C     that we are indexing the table using the absolute value of
C     handle.  So include a check to see that HANDLE is FTHAN(FINDEX).
C
      IF ( FINDEX .EQ. 0 ) THEN
         FOUND = .FALSE.
         RETURN
      ELSE IF ( FTHAN(FINDEX) .NE. HANDLE ) THEN
         FOUND = .FALSE.
         RETURN
      END IF
 
C
C     If we make it this far, then we have found HANDLE in the file
C     table.  Set NATIVE appropriately and FOUND to TRUE.
C
      NATIVE = NATBFF .EQ. FTBFF(FINDEX)
      FOUND  = .TRUE.
 
      RETURN
 
 
 
 
 
C$Procedure ZZDDHFNH ( Private --- Filename to Handle )
 
      ENTRY ZZDDHFNH ( FNAME, HANDLE, FOUND )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Retrieve handle associated with filename.
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
C     DAF
C     DAS
C     PRIVATE
C
C$ Declarations
C
C     CHARACTER*(*)         FNAME
C     INTEGER               HANDLE
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FNAME      I   Name of a file previously loaded with ZZDDHOPN.
C     HANDLE     O   Corresponding file handle.
C     FOUND      O   Logical indicating whether HANDLE was located.
C
C$ Detailed_Input
C
C     FNAME      is the name of a file previously loaded with ZZDDHOPN.
C
C$ Detailed_Output
C
C     HANDLE     is the handle associated with the file.
C
C     FOUND      is a logical when TRUE indicates HANDLE was located
C                for FNAME.  If FALSE no handle was found associated
C                with FNAME.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) ZZDDHF2H in the call tree of this routine performs I/O
C        functions and may signal errors that are the result of
C        I/O failures.  See ZZDDHF2H header for details.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is provided for completeness and serves only to
C     support the DAFFNH and DASFNH interfaces.
C
C$ Examples
C
C     See DAFFNH or DASFNH for sample usage.
C
C$ Restrictions
C
C     1) On VAX environments, this routine may only be used when
C        FNAME refers to a DAF or DAS file.  An error may be
C        signaled when used with unopened files that utilize other
C        architectures.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.1.0, 26-APR-2012 (BVS)
C
C        Updated for the new "magic number" column in the file table.
C
C-    SPICELIB Version 1.0.0, 06-NOV-2001 (FST)
C
C
C-&
 
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZDDHFNH' )
      END IF
 
C
C     Do the initialization tasks.
C
      IF ( FIRST ) THEN
 
         CALL ZZDDHINI ( NATBFF, SUPBFF, NUMSUP,
     .                   STRAMH, STRARC, STRBFF  )
 
C
C        Check FAILED() only to trap the possibility of ZZDDHINI
C        signaling SPICE(BUG).
C
         IF ( FAILED() ) THEN
            HANDLE = 0
            CALL CHKOUT ( 'ZZDDHFNH' )
            RETURN
         END IF
 
C
C        Clear FIRST since we've done the initialization.
C
         FIRST = .FALSE.
 
      END IF
 
C
C     Left justify FNAME to trim any leading white space.
C
      CALL LJUST ( FNAME, LOCFNM )
 
C
C     Look up FNAME in the handle table.
C
      CALL ZZDDHF2H ( LOCFNM,  FTABS,  FTAMH, FTARC, FTBFF,
     .                FTHAN,   FTNAM,  FTRTM, FTMNM, NFT,   UTCST,
     .                UTHAN,   UTLCK,  UTLUN, NUT,   INQEXT,
     .                INQOPN,  INQHAN, FOUND, MNM                 )
 
C
C     Check found and set HANDLE if we have got one.  No need to
C     check FAILED() since ZZDDHF2H returns FOUND set to FALSE on
C     error.
C
      IF ( FOUND ) THEN
         HANDLE = INQHAN
      ELSE
         HANDLE = 0
      END IF
 
      CALL CHKOUT ( 'ZZDDHFNH' )
      RETURN
 
 
 
 
C$Procedure ZZDDHLUH ( Private --- Logical Unit to Handle )
 
      ENTRY ZZDDHLUH ( UNIT, HANDLE, FOUND )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Return the handle associated with a logical unit.
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
C     DAS
C     DAF
C     PRIVATE
C
C$ Declarations
C
C     INTEGER               UNIT
C     INTEGER               HANDLE
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     UNIT       I   Logical unit connected to a file.
C     HANDLE     O   Corresponding handle.
C     FOUND      O   Logical indicating the handle was located.
C
C$ Detailed_Input
C
C     UNIT       is the logical unit to which a file managed by DDH is
C                currently connected.
C
C$ Detailed_Output
C
C     HANDLE     is the handle associated with the logical unit of
C                interest.
C
C     FOUND      is a logical flag if TRUE indicates that a HANDLE
C                was found associated with UNIT.  If FALSE indicates
C                no handle was found for UNIT.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1) If UNIT is not found in the unit table, HANDLE is undefined
C        and FOUND is set to .FALSE.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is provided for completeness and serves only to
C     support the DAFLUH and DASLUH interfaces.
C
C$ Examples
C
C     See DAFLUH or DASLUH for usage.
C
C$ Restrictions
C
C     Routines in the call tree of this routine may signal the error
C     SPICE(BUG) under the conditions of the existence of a bug
C     in routines the initialization loop invokes.  Since this
C     routine is error free with the exception of this bug condition,
C     it does not participate in tracing by design.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 06-NOV-2001 (FST)
C
C
C-&
 
C
C     Do the initialization tasks.
C
      IF ( FIRST ) THEN
 
         CALL ZZDDHINI ( NATBFF, SUPBFF, NUMSUP,
     .                   STRAMH, STRARC, STRBFF  )
 
C
C        Check FAILED(), and return on failure.  We are not checking
C        out or in since this routine would be error free if not for
C        the possibility of ZZDDHINI signaling SPICE(BUG).  See
C        $Restrictions for details.
C
         IF ( FAILED() ) THEN
            HANDLE = 0
            RETURN
         END IF
 
C
C        Clear FIRST since we've done the initialization.
C
         FIRST = .FALSE.
 
      END IF
 
C
C     Look up the unit in the table.
C
      UINDEX = ISRCHI ( UNIT, NUT, UTLUN )
 
      IF ( UINDEX .EQ. 0 ) THEN
         HANDLE = 0
         FOUND  = .FALSE.
         RETURN
      ELSE IF ( UTHAN(UINDEX) .EQ. 0 ) THEN
         HANDLE = 0
         FOUND  = .FALSE.
         RETURN
      END IF
 
C
C     We've got a handle, store the value and return.
C
      HANDLE = UTHAN(UINDEX)
      FOUND  = .TRUE.
 
      RETURN
 
      END
