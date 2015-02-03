C$Procedure KEEPER ( Keeps track of SPICE kernels )
 
      SUBROUTINE KEEPER ( WHICH,  KIND,
     .                    FILE,   COUNT, FILTYP,  HANDLE,
     .                    SOURCE, FOUND )
 
C$ Abstract
C
C     This routine is an umbrella for a collection of entry points
C     that manage the loading and unloading of SPICE kernels from
C     an application program.
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
C
C$ Declarations
 
      IMPLICIT NONE
      CHARACTER*(*)         KIND
      INTEGER               WHICH
      CHARACTER*(*)         FILE
      INTEGER               COUNT
      CHARACTER*(*)         FILTYP
      INTEGER               HANDLE
      CHARACTER*(*)         SOURCE
      LOGICAL               FOUND
 

      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )

      INTEGER               FTSIZE
      PARAMETER           ( FTSIZE = 5000 )

      INTEGER               MAXTXT
      PARAMETER           ( MAXTXT = 300 )

      INTEGER               MAXFIL
      PARAMETER           ( MAXFIL = FTSIZE + MAXTXT )
 
 
C$ Brief_I/O
C
C     VARIABLE  I/O  ENTRY POINT
C     --------  ---  --------------------------------------------------
C     KIND       I   KTOTAL, KDATA
C     FILE      I/O  FURNSH, KDATA,  UNLOAD, KINFO
C     FILTYP    I/O  KTOTAL, KDATA,  KINFO
C     COUNT      O   KTOTAL
C     HANDLE     O   KDATA,  KINFO
C     SOURCE     O   KDATA.  KINFO
C     FOUND      O   KDATA.  KINFO
C     FILSIZ     P   Maximum file name length.
C     MAXFIL     P   Is the maximum number of files that can be loaded.
C
C
C$ Detailed_Input
C
C     See Individual Entry points.
C
C$ Detailed_Output
C
C     See Individual Entry points.
C
C$ Parameters
C
C     FILSIZ    is the maximum file name length that can be
C               accommodated by this set of routines.
C     
C
C     MAXFIL    is the number of entries that can be stored in KEEPER's
C               kernel database.  Each time a kernel is loaded via
C               FURNSH, a database entry is created for that kernel.
C               If a meta-kernel is loaded, a database entry is created
C               for the meta-kernel itself and for all files referenced
C               in the meta-kernel's KERNELS_TO_LOAD specification.
C               Unloading a kernel or meta-kernel deletes database
C               entries created when the file was loaded.
C
C               The parameter MAXFIL is an upper bound on number of
C               SPICE kernels that can be loaded at any time via the
C               KEEPER interface, but the number of kernels that can be
C               loaded may be smaller, since re-loading a loaded kernel
C               or meta-kernel results in creation of additional
C               database entries.
C
C               Kernels loaded into the KEEPER system are subject to
C               constraints imposed by lower-level subsystems. The
C               binary kernel systems (SPK, CK, binary PCK, and EK)
C               have their own limits on the maximum number of kernels
C               that may be loaded.
C
C               The total number of DAF-based files (this set includes
C               SPKs, CKs, and binary PCKs) that may be loaded at any
C               time may not exceed 1000.  This limit applies whether
C               the files are loaded via FURNSH or lower-level loaders
C               such as SPKLEF or DAFOPR.  File access performance
C               normally will degrade as the number of loaded kernels
C               increases.
C
C               The total number of DAS-based files that may be loaded
C               at any time is currently limited to 20 files.
C
C$ Exceptions
C
C     1) If the main routine KEEPER is called, the error
C       'SPICE(BOGUSENTRY)' will be signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine serves as an umbrella for a collection of
C     entry points that unify the task of loading, tracking,
C     and unloading SPICE kernels.  A description of each entry
C     point is given below:
C
C     FURNSH    Furnish a kernel to a program.  This entry point
C               provides a single interface for loading kernels into
C               your application program.  All SPICE kernels (Text
C               kernels, SPK, CK, Binary PCK, and EK) can be loaded
C               through this entry point.  In addition, special text
C               kernels, called meta-Text kernels, that contain a list
C               of other kernels to load can be processed by FURNSH.
C
C               Meta-text kernels allow you to easily control which
C               kernels will be loaded by your program without having
C               to write your own kernel managing routines.
C
C     KTOTAL    returns the number of kernels that are currently
C               available to your program as a result of previous calls
C               to FURNSH and UNLOAD.
C
C     KDATA     provides an interface for retrieving (in order of their
C               specification through FURNSH) kernels that are active in
C               your application.
C
C     KINFO     allows you to retrieve information about a loaded
C               kernel using the name of that kernel.
C
C     KCLEAR    Unloads all kernels that were loaded via the KEEPER
C               system, clears the kernel pool, and re-initializes the
C               KEEPER system.
C              
C     UNLOAD    provides an interface for unloading kernels that have
C               been loaded via the routine FURNSH.
C
C     For more details concerning any particular entry point, see the
C     header for that entry point.
C
C$ Examples
C
C     The code fragment below illustrates the use of the various entry
C     points of KEEPER.  The details of creating meta-text kernels are
C     not discussed here, but are spelled out in the entry point
C     FURNSH.
C
C
C     Load several kernels into the program.
C
C
C     CALL FURNSH ( 'myspk.bsp'    )
C     CALL FURNSH ( 'myck.bc'      )
C     CALL FURNSH ( 'leapsecs.ker' )
C     CALL FURNSH ( 'sclk.tsc'     )
C     CALL FURNSH ( 'metatext.ker' )
C
C     See how many kernels have been loaded.
C
C     CALL KTOTAL ( 'ALL', COUNT )
C
C     WRITE (*,*) 'The total number of kernels is: ', COUNT
C
C     Summarize the kernels and types.
C
C     DO WHICH = 1, COUNT
C
C        CALL KDATA( WHICH, 'ALL', FILE, FILTYP, SOURCE, HANDLE, FOUND )
C
C        IF ( .NOT. FOUND ) THEN
C
C           WRITE (*,*) 'This is NOT supposed to happen.  Call NAIF'
C           WRITE (*,*) 'and let them know of this problem.'
C
C        ELSE
C
C           WRITE (*,*)
C           WRITE (*,*) 'File  : ', FILE
C           WRITE (*,*) 'Type  : ', FILTYP
C           WRITE (*,*) 'Handle: ', HANDLE
C
C           IF ( SOURCE .NE. ' ' ) THEN
C              WRITE (*,*) 'This file was loaded via meta-text kernel:'
C              WRITE (*,*) SOURCE
C           END IF
C
C        END IF
C
C     END DO
C
C
C     Unload the first kernel we loaded.
C
C     CALL UNLOAD ( 'myspk.bsp' )
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
C     C.H. Acton      (JPL)
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.1.0, 01-JUL-2014 (NJB) (BVS)
C
C        Updated the discussion of kernel variable watches in entry
C        points KCLEAR and UNLOAD. Added to the FURNSH header mention
C        of the effects of failure during text kernel or meta-kernel
C        loading.
C
C     Last update was 12-APR-2012 (BVS)
C
C        Increased FTSIZE (from 1000 to 5000).
C
C        Changed to use SEPOOL instead of STPOOL to reduce loading time
C        for large meta-kernels due to n^2 delay in STPOOL.
C
C-    SPICELIB Version 4.0.2, 13-APR-2011 (EDW)
C
C        Trivial edit to KCLEAR Restrictions, replaced P*POOL with
C        PXPOOL. The "*" character causes the HTML documentation
C        script to create a link for the "POOL" substring.
C
C-    SPICELIB Version 4.0.1, 10-FEB-2010 (EDW)
C
C        Added mention of the restriction on kernel pool variable 
C        names to MAXLEN (defined in pool.f) characters or less.
C
C-    SPICELIB Version 4.0.0, 02-APR-2009 (NJB)
C
C        Continued path values are now supported. FURNSH now rejects
C        file names longer than FILSIZ characters.
C
C        Deleted references to unneeded variable DOALL. Made
C        THSTYP declaration compatible with TYPES array.
C
C-    SPICELIB Version 3.0.1, 27-APR-2007 (NJB)
C
C        Fixed header typo: added quotes to literal string
C        input arguments in example FURNSH calls.
C
C-    SPICELIB Version 3.0.0, 15-NOV-2006 (NJB)
C
C        Added entry point KCLEAR. Bug fix:  meta-kernel unloading bug
C        in UNLOAD was corrected. Some header updates were made.
C
C-    SPICELIB Version 2.0.2, 29-JUL-2003 (NJB) (CHA)
C
C        Only the header of the entry point FURNSH was modified.
C        Numerous updates were made to improve clarity.  Some
C        corrections were made.
C
C-    SPICELIB VERSION 2.0.1, 06-DEC-2002 (NJB)
C
C        Typo in header example was corrected.
C
C-    SPICELIB VERSION 2.0.0, 07-JAN-2002 (WLT)
C
C        Added a call to CVPOOL in FURNSH so that watches that are
C        triggered are triggered by loading Meta-kernels and not by
C        some external interaction with the kernel pool.
C
C        Added code to make sure that UNLOAD has the effect of
C        loading all remaining kernels in the order they were first
C        introduced.
C
C-    SPICELIB Version 1.1.0, 19-SEP-2000 (WLT)
C
C        Corrected the error message template used
C        by ZZLDKER
C
C-    SPICELIB Version 1.0.1, 16-DEC-1999 (NJB)
C
C        Documentation fix:  corrected second code example in the
C        header of the entry point FURNSH.  The example previously used
C        the kernel variable PATH_NAMES; that name has been replaced
C        with the correct name PATH_VALUES.
C
C-    SPICELIB Version 1.0.0, 01-JUL-1999 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Generic loading and unloading of SPICE kernels
C
C-&
 
C
C     SPICELIB Functions
C 
      INTEGER               ISRCHC
      INTEGER               POS
      INTEGER               RTRIM

      LOGICAL               EQSTR
      LOGICAL               FAILED
      LOGICAL               RETURN
      LOGICAL               SAMSUB
 
C
C     Here we set up the database of loaded kernels
C
C     The name of every file loaded through this interface will
C     be stored in the array FILES.
C
      CHARACTER*(FILSIZ)    FILES ( MAXFIL )
      SAVE                  FILES
C
C     The handle of every loaded file will be stored in the array
C     HANDLS.  If the file is a text kernel it will be assigned the
C     handle 0.
C
      INTEGER               HANDLS( MAXFIL )
      SAVE                  HANDLS
C
C     The source of each file specified will be stored in the integer
C     array SOURCE.  If the file is loaded directly, its source
C     will be zero.  If it is loaded as the result of meta-information
C     in a text kernel, the index of the source file in FILES will
C     be stored in SRCES.
C
      INTEGER               SRCES ( MAXFIL )
      SAVE                  SRCES
C
C     The file type of every loaded kernel will be stored in the array
C     TYPES.
C
      INTEGER               TYPLEN
      PARAMETER           ( TYPLEN = 8 )

      CHARACTER*(TYPLEN)    TYPES ( MAXFIL )
      SAVE                  TYPES
 
C
C     The number of files loaded through this interfaces is kept in the
C     integer LOADED.
C
      INTEGER               LOADED
      SAVE                  LOADED
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
 
      INTEGER               NKNOWN
      PARAMETER           ( NKNOWN = 3 )
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      INTEGER               MSGSIZ
      PARAMETER           ( MSGSIZ = 500 )
 
      CHARACTER*(1)         NORC
      CHARACTER*(FILSIZ)    FIL2LD
  
      CHARACTER*(FILSIZ)    PVALUE
      CHARACTER*(LNSIZE)    SYMBOL
 
      CHARACTER*(MSGSIZ)    NOFILE
 
 
      CHARACTER*(WDSIZE)    KNOWN ( NKNOWN )
      SAVE                  KNOWN
 
      CHARACTER*(TYPLEN)    THSTYP
 
      INTEGER               B
      INTEGER               CURSRC
      INTEGER               D
      INTEGER               DOLLAR
      INTEGER               E
      INTEGER               FIDX
      INTEGER               FILNUM
      INTEGER               FNMLEN
      INTEGER               HITS
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               LIDX
      INTEGER               MYHAND
      INTEGER               N
      INTEGER               N1
      INTEGER               N2
      INTEGER               N3
      INTEGER               NPATHS
      INTEGER               NPVALS
      INTEGER               R
      INTEGER               SIZE
      INTEGER               SRC
      INTEGER               START
      INTEGER               USE
 
      LOGICAL               ADD
      LOGICAL               DIDCK
      LOGICAL               DIDEK
      LOGICAL               DIDPCK
      LOGICAL               DIDSPK
      LOGICAL               DIDTXT
      LOGICAL               DOCK
      LOGICAL               DOEK
      LOGICAL               DOMETA
      LOGICAL               DOPCK
      LOGICAL               DOSPK
      LOGICAL               DOTEXT
      LOGICAL               FIRST
      SAVE                  FIRST
 
      LOGICAL               FND
      LOGICAL               GOTIT
      LOGICAL               OK
      LOGICAL               PATHS
      LOGICAL               UPDATE
 

      DATA                  FIRST     / .TRUE. /
      DATA                  LOADED    /  0  /
 
 
 
      CALL CHKIN  ( 'KEEPER' )
      CALL SETMSG ( 'The routine KEEPER is an umbrella for a '
     .//            'collection of entry points that manage the '
     .//            'loading, tracking and unloading of SPICE '
     .//            'kernels.  KEEPER should not be called '
     .//            'directly. It is likely that a programming '
     .//            'error has been made. ' )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)'  )
      CALL CHKOUT ( 'KEEPER' )
      RETURN
 
 
 
 
 
C$Procedure      FURNSH ( Furnish a program with SPICE kernels )
 
      ENTRY FURNSH ( FILE )
 
C$ Abstract
C
C     Load one or more SPICE kernels into a program.
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
C      UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         FILE
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FILE       I   SPICE kernel file (text or binary).
C     FILSIZ     P   Maximum file name length.
C
C$ Detailed_Input
C
C     FILE       is a SPICE kernel file.  The file may be either binary
C                or text. If the file is a binary SPICE kernel it will
C                be loaded into the appropriate SPICE subsystem.  If
C                FILE is a SPICE text kernel it will be loaded into the
C                kernel pool. If FILE is a SPICE meta-kernel containing
C                initialization instructions (through use of the
C                correct kernel pool variables), the files specified in
C                those variables will be loaded into the appropriate
C                SPICE subsystem.
C
C                The SPICE text kernel format supports association of
C                names and data values using a "keyword = value"
C                format. The keyword-value pairs thus defined are
C                called "kernel variables."  
C
C                While any information can be placed in a text kernel
C                file, the following string valued kernel variables are
C                recognized by SPICE as meta-kernel keywords:
C
C                     KERNELS_TO_LOAD
C                     PATH_SYMBOLS
C                     PATH_VALUES
C
C                Each kernel variable is discussed below.
C
C                KERNELS_TO_LOAD   is a list of SPICE kernels to be
C                                  loaded into a program.  If file
C                                  names do not fit within the kernel
C                                  pool 80 character limit, they may be
C                                  continued to subsequent array
C                                  elements by placing the continuation
C                                  character ('+') at the end of an
C                                  element and then placing the
C                                  remainder of the file name in the
C                                  next array element.  (See the
C                                  examples below for an illustration
C                                  of this technique or consult the
C                                  routine STPOOL for further details.)
C
C                                  You may use one or more PATH_SYMBOL
C                                  assignments (see below) to specify
C                                  strings to be substituted for some
C                                  part of a file name.
C
C                PATH_SYMBOLS      is a list of strings (without
C                                  embedded blanks) which if
C                                  encountered following the '$'
C                                  character will be replaced with the
C                                  corresponding PATH_VALUES string.
C                                  Note that PATH_SYMBOLS are
C                                  interpreted only in values
C                                  associated with the KERNELS_TO_LOAD
C                                  variable. There must be a one-to-one
C                                  correspondence between the values
C                                  supplied for PATH_SYMBOLS and
C                                  PATH_VALUES. For the purpose of
C                                  determining this correspondence, any
C                                  path value that is continued over
C                                  multiple array elements counts as a
C                                  single value.
C
C                PATH_VALUES       is a list of expansions to use when
C                                  PATH_SYMBOLS are encountered. If
C                                  path values do not fit within the
C                                  kernel pool 80 character limit, they
C                                  may be continued in the same way as
C                                  file names (see the KERNELS_TO_LOAD
C                                  description above).
C
C               These kernel pool variables persist within the kernel
C               pool only until all kernels associated with the
C               variable KERNELS_TO_LOAD have been loaded.  Once all
C               specified kernels have been loaded, the variables
C               KERNELS_TO_LOAD, PATH_SYMBOLS and PATH_VALUES are
C               removed from the kernel pool.
C
C$ Detailed_Output
C
C     None. The routine loads various SPICE kernels for use by your
C     application.  
C
C$ Parameters
C
C     FILSIZ    is the maximum file name length that can be
C               accommodated by this routine.
C
C     MAXFIL    is the number of entries that can be stored in KEEPER's
C               kernel database.  Each time a kernel is loaded via
C               FURNSH, a database entry is created for that kernel.
C               If a meta-kernel is loaded, a database entry is created
C               for the meta-kernel itself and for all files referenced
C               in the meta-kernel's KERNELS_TO_LOAD specification.
C               Unloading a kernel or meta-kernel deletes database
C               entries created when the file was loaded.
C
C               The parameter MAXFIL is an upper bound on number of
C               SPICE kernels that can be loaded at any time via the
C               KEEPER interface, but the number of kernels that can be
C               loaded may be smaller, since re-loading a loaded kernel
C               or meta-kernel results in creation of additional
C               database entries.
C
C               Kernels loaded into the KEEPER system are subject to
C               constraints imposed by lower-level subsystems. The
C               binary kernel systems (SPK, CK, binary PCK, and EK)
C               have their own limits on the maximum number of kernels
C               that may be loaded.
C
C               The total number of DAF-based files (this set includes
C               SPKs, CKs, and binary PCKs) that may be loaded at any
C               time may not exceed 1000.  This limit applies whether
C               the files are loaded via FURNSH or lower-level loaders
C               such as SPKLEF or DAFOPR.  File access performance
C               normally will degrade as the number of loaded kernels
C               increases.
C
C               The total number of DAS-based files that may be loaded
C               at any time is currently limited to 20 files.
C
C$ Exceptions
C
C     1) If a problem is encountered while trying to load FILE,
C        it will be diagnosed by a routine in the call tree of this
C        routine.
C
C     2) If the input FILE is a meta-kernel and some file in the
C        KERNELS_TO_LOAD assignment cannot be found, or if an error
C        occurs while trying to load a file specified by this
C        assignment, the error will be diagnosed by a routine in the
C        call tree of this routine, and this routine will return. Any
C        files loaded prior to encountering the missing file will
C        remain loaded.
C
C     3) If an attempt to load a text kernel fails while the kernel is
C        being parsed, any kernel variable assignments made before
C        the failure occurred will be retained in the kernel pool.
C
C     4) If a PATH_SYMBOLS assignment is specified without a
C        corresponding PATH_VALUES assignment, the error
C        SPICE(NOPATHVALUE) will be signaled.
C
C     5) If a meta-text kernel is supplied to FURNSH that contains
C        instructions specifying that another meta-text kernel be
C        loaded, the error SPICE(RECURSIVELOADING) will be signaled.
C
C     6) If the input file name has non-blank length exceeding FILSIZ
C        characters, the error SPICE(FILENAMETOOLONG) is signaled.
C
C     7) If the input file is a meta-kernel and some file in the
C        KERNELS_TO_LOAD assignment has name length exceeding FILSIZ
C        characters, the error SPICE(FILENAMETOOLONG) is signaled.
C
C     8) If the input file is a meta-kernel and some value in the
C        PATH_VALUES assignment has length exceeding FILSIZ
C        characters, the error SPICE(PATHTOOLONG) is signaled.
C
C     9) If the input file is a meta-kernel and some file in the
C        KERNELS_TO_LOAD assignment has, after symbol substitution,
C        combined name and path length exceeding FILSIZ characters, the
C        error SPICE(FILENAMETOOLONG) is signaled.
C
C    10) The error 'SPICE(BADVARNAME)' signals from a routine in the
C        call tree of FURNSH if a kernel pool variable name length
C        exceeds MAXLEN characters (defined in pool.f).
C
C    
C$ Files
C
C     The input FILE is examined and loaded into the appropriate SPICE
C     subsystem.  If the file is a meta-kernel, any kernels specified
C     by the KERNELS_TO_LOAD keyword (and if present, the PATH_SYMBOLS
C     and PATH_VALUES keywords) are loaded as well.
C
C$ Particulars
C
C     This routine provides a uniform interface to the SPICE kernel
C     loading systems.  It allows you to easily assemble a list of
C     SPICE kernels required by your application and to modify that set
C     without modifying the source code of programs that make use of
C     these kernels.
C
C$ Examples
C
C     Example 1
C     ---------
C
C     Load the leapseconds kernel naif0007.tls and the planetary
C     ephemeris SPK file de405s.bsp.
C
C        CALL FURNSH ( 'naif0007.tls' )
C        CALL FURNSH ( 'de405s.bsp'   )
C
C
C     Example 2
C     ---------
C
C     This example illustrates how you could create a meta-kernel file
C     for a program that requires several text and binary kernels.
C 
C     First create a list of the kernels you need in a text file as
C     shown below.
C
C        \begintext
C
C           Here are the SPICE kernels required for my application
C           program.
C
C           Note that kernels are loaded in the order listed. Thus we
C           need to list the highest priority kernel last.
C
C
C        \begindata
C
C        KERNELS_TO_LOAD = ( 
C
C              '/home/mydir/kernels/spk/lowest_priority.bsp',
C              '/home/mydir/kernels/spk/next_priority.bsp',
C              '/home/mydir/kernels/spk/highest_priority.bsp',
C              '/home/mydir/kernels/text/leapsecond.ker',
C              '/home/mydir/kernels+',
C              '/custom+',
C              '/kernel_data/constants.ker',
C              '/home/mydir/kernels/text/sclk.tsc',
C              '/home/mydir/kernels/ck/c-kernel.bc' )
C
C
C     Note that the file name
C
C        /home/mydir/kernels/custom/kernel_data/constants.ker
C
C     is continued across several lines in the right hand side of the
C     assignment of the kernel variable KERNELS_TO_LOAD.
C
C     Once you've created your list of kernels, call FURNSH near the
C     beginning of your application program to load the meta-kernel
C     automatically at program start up.
C
C        CALL FURNSH ( 'myfile.txt' )
C
C     This will cause each of the kernels listed in your meta-kernel
C     to be loaded.
C
C
C     Example 3
C     ---------
C
C     This example illustrates how you can simplify the previous
C     kernel list by using PATH_SYMBOLS.
C
C
C        \begintext
C
C           Here are the SPICE kernels required for my application
C           program.
C
C           We are going to let A substitute for the directory that
C           contains SPK files; B substitute for the directory that
C           contains C-kernels; and C substitute for the directory that
C           contains text kernels.  And we'll let D substitute for
C           a "custom" directory that contains a special planetary
C           constants kernel made just for our mission.
C
C           Note that our PATH_VALUES and the corresponding
C           PATH_SYMBOLS must be listed in the same order.
C
C
C        \begindata
C
C        PATH_VALUES  = ( '/home/mydir/kernels/spk',
C                         '/home/mydir/kernels/ck',
C                         '/home/mydir/kernels/text',
C                         '/home/mydir/kernels/custom/kernel_data' )
C
C        PATH_SYMBOLS = ( 'A',
C                         'B',
C                         'C',
C                         'D'  )
C
C        KERNELS_TO_LOAD = (  '$A/lowest_priority.bsp',
C                             '$A/next_priority.bsp',
C                             '$A/highest_priority.bsp',
C                             '$C/leapsecond.ker',
C                             '$D/constants.ker',
C                             '$C/sclk.tsc',
C                             '$B/c-kernel.bc'         )
C
C
C     Example 4
C     ---------
C
C     This example illustrates continuation of path values. The
C     meta-kernel shown here is a modified version of that from
C     example 3.
C     
C        \begintext
C
C           Here are the SPICE kernels required for my application
C           program.
C
C           We are going to let A substitute for the directory that
C           contains SPK files; B substitute for the directory that
C           contains C-kernels; and C substitute for the directory that
C           contains text kernels.  And we'll let D substitute for
C           a "custom" directory that contains a special planetary
C           constants kernel made just for our mission.
C
C           Note that our PATH_VALUES and the corresponding
C           PATH_SYMBOLS must be listed in the same order.
C
C           The values for path symbols A and D are continued over
C           multiple lines.
C
C        \begindata
C
C        PATH_VALUES  = ( '/very_long_top_level_path_name/mydir/+',
C                         'kernels/spk',
C                         '/home/mydir/kernels/ck',
C                         '/home/mydir/kernels/text',
C                         '/very_long_top_level_path_name+',
C                         '/mydir/kernels/custom+',
C                         '/kernel_data'                )
C
C        PATH_SYMBOLS = ( 'A',
C                         'B',
C                         'C',
C                         'D'  )
C
C        KERNELS_TO_LOAD = (  '$A/lowest_priority.bsp',
C                             '$A/next_priority.bsp',
C                             '$A/highest_priority.bsp',
C                             '$C/leapsecond.ker',
C                             '$D/constants.ker',
C                             '$C/sclk.tsc',
C                             '$B/c-kernel.bc'         )
C 
C$ Restrictions
C
C     1) A meta-kernel cannot reference another meta-kernel.
C
C     2) Failure during an attempt to load a text kernel or a 
C        meta-kernel can result in a subset of the intended kernel
C        variables being set or a subset of the intended files
C        being loaded. FURNSH does not "clean up" so as to undo the
C        effects of a failed load operation.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     C.H. Acton      (JPL)
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.1.0, 01-JUL-2014 (NJB) (BVS)
C
C        Updated discussion of partially completed kernel loading.
C
C     Last update was 12-APR-2012 (BVS)
C
C        Changed to use SEPOOL instead of STPOOL to reduce loading time
C        for large meta-kernels due to n^2 delay in STPOOL.
C
C-    SPICELIB Version 4.0.1, 10-FEB-2010 (EDW)
C
C        Added mention of the restriction on kernel pool variable 
C        names to MAXLEN (defined in pool.f) characters or less.
C
C-    SPICELIB Version 4.0.0, 02-APR-2009 (NJB)
C
C        Continued path values are now supported. FURNSH now rejects
C        file names longer than FILSIZ characters.
C
C-    SPICELIB Version 2.0.3, 27-APR-2007 (NJB)
C
C        Fixed header typo: added quotes to literal string
C        input arguments in example FURNSH calls.
C
C-    SPICELIB Version 2.0.2, 15-NOV-2006 (NJB)
C
C        Added description of parameter MAXFIL to header.
C
C-    SPICELIB Version 2.0.1, 29-JUL-2003 (NJB) (CHA)
C
C        Numerous updates to improve clarity.  Some corrections were
C        made.
C
C-    SPICELIB VERSION 2.0.0, 23-AUG-2001 (WLT)
C
C        Added a call to CVPOOL in FURNSH so that watches that are
C        triggered are triggered by loading Meta-kernels and not by
C        some external interaction with the kernel pool.
C
C-    SPICELIB Version 1.1.0, 19-SEP-2000 (WLT)
C
C        Corrected the error message template used
C        by ZZLDKER
C
C-    SPICELIB Version 1.0.1, 16-DEC-1999 (NJB)
C
C        Documentation fix:  corrected second code example in the
C        header of this entry point.  The example previously used the
C        kernel variable PATH_NAMES; that name has been replaced with
C        the correct name PATH_VALUES.
C
C-    SPICELIB Version 1.0.0, 01-JUL-1999 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Load SPICE kernels from a list of kernels
C
C-&
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'FURNSH')
 
      IF ( FIRST ) THEN
 
         FIRST    = .FALSE.
         KNOWN(1) = 'KERNELS_TO_LOAD'
         KNOWN(2) = 'PATH_SYMBOLS'
         KNOWN(3) = 'PATH_VALUES'
         LOADED   =  0
 
         CALL SWPOOL ( 'FURNSH', NKNOWN, KNOWN )
         CALL CVPOOL ( 'FURNSH', UPDATE )
 
      END IF
 
C
C     Reject excessively long file names.
C
      IF ( RTRIM(FILE) .GT. FILSIZ ) THEN

         CALL SETMSG ( 'Input file name <#> has length @ characters. '
     .   //            'The limit on the length of file names stored '
     .   //            'by FURNSH is @ characters.'                   )
         CALL ERRCH  ( '#', FILE                                      )
         CALL ERRINT ( '@', RTRIM(FILE)                               )
         CALL ERRINT ( '@', FILSIZ                                    )
         CALL SIGERR ( 'SPICE(FILENAMETOOLONG)'                       )
         CALL CHKOUT ( 'FURNSH'                                       )
         RETURN

      END IF

C
C     Make sure we have room to load at least one more file.
C
      IF ( LOADED .EQ. MAXFIL ) THEN
 
         CALL SETMSG ( 'There is no room left in KEEPER to load '
     .   //            'another SPICE kernel.  The current limit '
     .   //            'on the number of files that can be '
     .   //            'loaded is #.  If you really need more '
     .   //            'than this many files, you should '
     .   //            'increase the parameter MAXFIL in the '
     .   //            'subroutine KEEPER. ' )
 
         CALL ERRINT ( '#', MAXFIL )
         CALL SIGERR ( 'SPICE(NOMOREROOM)'  )
         CALL CHKOUT ( 'FURNSH' )
         RETURN
 
      END IF
C
C     We don't want external interactions with the kernel pool to
C     have any affect on FURNSH's watch so we check the watcher
C     here prior to the call to ZZLDKER.
C
      CALL CVPOOL ( 'FURNSH', UPDATE )
 
C
C     Set a preliminary value for the error message in case the
C     call to ZZLDKER doesn't succeed.
C
      NOFILE = 'The attempt to load "#" by the routine FURNSH '
     .//       'failed. It #'
 
      CALL ZZLDKER ( FILE, NOFILE, THSTYP, MYHAND )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'FURNSH' )
         RETURN
      END IF
 
 
      LOADED = LOADED + 1
      CURSRC = LOADED
 
      FILES (LOADED) = FILE
      TYPES (LOADED) = THSTYP
      HANDLS(LOADED) = MYHAND
      SRCES (LOADED) = 0
 
       CALL CVPOOL ( 'FURNSH', UPDATE )
 
      IF ( .NOT. UPDATE ) THEN
C
C        Nothing to do.  None of the control variables
C        were set in FILE.
C
         CALL CHKOUT ( 'FURNSH' )
         RETURN
 
      END IF
 
C
C     See what is present in the kernel pool: Are any path symbols
C     defined?
C
      CALL DTPOOL ( 'PATH_SYMBOLS', PATHS, NPATHS, NORC )
 
      IF (  PATHS .AND. ( NORC .EQ. 'C' )  ) THEN
C
C        Make sure that the values are equal in number. We need to
C        use STPOOL to count the path values, since some of them
C        might span multiple array elements.
C
         I = 1
         CALL STPOOL( 'PATH_VALUES', I, '+', PVALUE, SIZE, OK )

         DO WHILE (  OK  .AND.  ( .NOT. FAILED() )  )
C
C           Reject excessively long path names.
C
            IF ( SIZE .GT. FILSIZ ) THEN

               CALL SETMSG ( 'In meta-kernel <#>, the path at '
     .         //            'index # in the PATH_VALUES list has '
     .         //            'length # characters; the limit is # '
     .         //            'characters.'                          )
               CALL ERRCH  ( '#', FILE                              )
               CALL ERRINT ( '#', I                                 )
               CALL ERRINT ( '#', SIZE                              )
               CALL ERRINT ( '#', FILSIZ                            )
               CALL SIGERR ( 'SPICE(PATHTOOLONG)'                   )
               CALL CHKOUT ( 'FURNSH'                               )
               RETURN

            END IF

            I = I + 1
            CALL STPOOL( 'PATH_VALUES', I, '+', PVALUE, SIZE, OK )

         END DO

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'FURNSH' )
            RETURN
         END IF

         NPVALS = I - 1

         IF ( NPVALS .NE. NPATHS ) THEN

            CALL SETMSG ( 'Number of path symbols is #; number of '
     .      //            'path values is #; counts must match.'  )
            CALL ERRINT ( '#', NPATHS                             )
            CALL ERRINT ( '#', NPVALS                             )
            CALL SIGERR ( 'SPICE(PATHMISMATCH)'                   )
            CALL CHKOUT ( 'FURNSH'                                )
            RETURN

         END IF

      ELSE

         PATHS = .FALSE.

      END IF
  
C
C     This kernel appears to be a legitimate meta-text kernel. Mark
C     it as such and then process its contents.
C
      TYPES(LOADED) = 'META'
 
C
C     Now load all kernels specified in the KERNELS_TO_LOAD variable.
C
      FILNUM = 1
      FIDX   = 1
 
      CALL SEPOOL( 'KERNELS_TO_LOAD',  FIDX,    '+', 
     .             FIL2LD,             FNMLEN,  LIDX,  OK )
 
      DO WHILE (  OK  .AND.  ( .NOT. FAILED() )  )
C
C        Reject excessively long file names.
C
         IF ( FNMLEN .GT. FILSIZ ) THEN

            CALL SETMSG ( 'In meta-kernel <#>, the file name at '
     .      //            'index # in the KERNELS_TO_LOAD list has '
     .      //            'length # characters; the limit is # '
     .      //            'characters.'                             )
            CALL ERRCH  ( '#', FILE                                 )
            CALL ERRINT ( '#', FILNUM                               )
            CALL ERRINT ( '#', FNMLEN                               )
            CALL ERRINT ( '#', FILSIZ                               )
            CALL SIGERR ( 'SPICE(FILENAMETOOLONG)'                  )
            CALL CHKOUT ( 'FURNSH'                                  )
            RETURN

         END IF

C
C        Make sure we have room to load at least one more file.
C
         IF ( LOADED .EQ. MAXFIL ) THEN
 
            CALL SETMSG ( 'There is no room left in KEEPER to '
     .      //            'load another SPICE kernel. The '
     .      //            'current limit on the number of files '
     .      //            'that can be loaded is #.'             )
 
            CALL ERRINT ( '#', MAXFIL )
            CALL SIGERR ( 'SPICE(NOMOREROOM)'  )
            CALL CHKOUT ( 'FURNSH' )
            RETURN
 
         END IF
 
C
C        Resolve any path symbols that may be present.
C        Make sure we have room to load at least one more file.
C
         IF ( PATHS ) THEN
 
            START  = 1
            DOLLAR = POS( FIL2LD, '$', START )
 
            DO WHILE ( DOLLAR .GT. 0 )
C
C              Determine the longest path symbol that fits into the
C              current file name.  We fetch path symbols one at a
C              time and see if they match the portion of the
C              string that follows the '$'.  The longest match
C              is the one we use as a symbol.
C
               SIZE = 0
               USE  = 0
               D    = DOLLAR
 
 
               DO I = 1, NPATHS
 
                  CALL GCPOOL( 'PATH_SYMBOLS', I, 1, N, SYMBOL, FND )

                  R =  RTRIM ( SYMBOL )
 
                  IF (       R  .GT. SIZE
     .                 .AND. SAMSUB( SYMBOL,1,R, FIL2LD,D+1,D+R ) ) THEN
                     USE  = I
                     SIZE = R
                  END IF
 
               END DO
C
C              If we found a matching path symbol, get the corresponding
C              value and put it into the file name.
C
               IF ( USE .GT. 0 ) THEN
C
C                 Get the path value having index USE in the set of
C                 path values. Note that we've already checked that
C                 the path value will fit in PVALUE.
C 
                  CALL STPOOL( 'PATH_VALUES', USE, '+', PVALUE, N, FND )

C
C                 When the path is substituted for the symbol, the
C                 total length of the path and file name must fit in
C                 the name buffer.
C
                  IF ( ( FNMLEN + N - SIZE - 1 )  .GT.  FILSIZ ) THEN

                     CALL SETMSG ( 'In meta-kernel <#>, the path at '
     .               //            'index # in the PATH_SYMBOLS list '
     .               //            'has # characters and the '
     .               //            'file name at index # has '
     .               //            '# characters. The combined path '
     .               //            'and file name has # characters; '
     .               //            'the limit is # characters.'       )
                     CALL ERRCH  ( '#', FILE                          )
                     CALL ERRINT ( '#', USE                           )
                     CALL ERRINT ( '#', N                             )
                     CALL ERRINT ( '#', FILNUM                        )
                     CALL ERRINT ( '#', FNMLEN                        )
                     CALL ERRINT ( '#', FNMLEN + N                    )
                     CALL ERRINT ( '#', FILSIZ                        )
                     CALL SIGERR ( 'SPICE(FILENAMETOOLONG)'           )
                     CALL CHKOUT ( 'FURNSH'                           )
                     RETURN

                  END IF

                  CALL REPSUB( FIL2LD, D, D+SIZE, PVALUE(1:N), FIL2LD )
 
               END IF
C
C              Look for the next occurrence of a '$' after the last
C              place we found one.
C
               START  = DOLLAR + 1
               DOLLAR = POS( FIL2LD, '$', START )
 
            END DO
 
         END IF
C
C        If any path symbols were present, they have now been
C        resolved.  Let ZZLDKER handle the task of loading this
C        kernel.  Make up a message template for use if ZZLDKER
C        runs into a problem.
C
         NOFILE = 'The @ file ''#'' specified by KERNELS_TO_LOAD '
     .   //       'in the file @ #'
 
         CALL REPMOT ( NOFILE, '@', FILNUM, 'L', NOFILE )
         CALL REPMC  ( NOFILE, '@', FILE,        NOFILE )
 
         CALL ZZLDKER( FIL2LD, NOFILE, THSTYP, MYHAND )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'FURNSH' )
            RETURN
         END IF
 
         IF ( THSTYP .EQ. 'TEXT' ) THEN
C
C           See if we stepped on any of the recognized variables.  If
C           we did, there's no point in trying to continue.
C
            CALL CVPOOL ( 'FURNSH', UPDATE )
 
            IF ( UPDATE ) THEN
C
C              First clean up the debris created by this attempt
C              at recursion.
C
               DO I = 1, NKNOWN
                  CALL DVPOOL ( KNOWN(I) )
               END DO
C
C              Take care of any watcher activation caused by the
C              mop-up of the preceding loop.
C
               CALL CVPOOL ( 'FURNSH', UPDATE )
 
               CALL SETMSG ( 'Hmmm.  This is interesting. In the '
     .         //            'meta-text kernel ''#'' you''ve '
     .         //            'requested that the text kernel '
     .         //            '''#'' be loaded. This second file '
     .         //            'is also a "meta-text" kernel and '
     .         //            'specifies new kernel loading '
     .         //            'instructions. Although you receive '
     .         //            'high marks for creativity, this '
     .         //            'path is fraught with peril and can '
     .         //            'not be supported by FURNSH. ' )
 
               CALL ERRCH  ( '#', FILE   )
               CALL ERRCH  ( '#', FIL2LD )
               CALL SIGERR ( 'SPICE(RECURSIVELOADING)' )
               CALL CHKOUT ( 'FURNSH' )
               RETURN
            END IF
 
         END IF
C
C        Add the latest file loaded to our database of loaded
C        files.
C
         LOADED         = LOADED + 1
         FILES (LOADED) = FIL2LD
         TYPES (LOADED) = THSTYP
         HANDLS(LOADED) = MYHAND
         SRCES (LOADED) = CURSRC

C
C        Get the name of the next file to load.
C
         FILNUM = FILNUM + 1
         FIDX   = LIDX   + 1 

         CALL SEPOOL( 'KERNELS_TO_LOAD',  FIDX,    '+', 
     .                 FIL2LD,            FNMLEN,  LIDX,  OK )

      END DO
 
C
C     Last Step.  Remove the special variables from the kernel pool.
C
      DO I = 1, NKNOWN
         CALL DVPOOL ( KNOWN(I) )
      END DO
 
      CALL CVPOOL ( 'FURNSH', UPDATE )
 
      CALL CHKOUT ( 'FURNSH' )
      RETURN
 
 
 
C$Procedure      KTOTAL ( Kernel Totals )
 
      ENTRY KTOTAL ( KIND, COUNT )
 
C$ Abstract
C
C     Return the number of kernels that are currently loaded
C     via the KEEPER interface and that are of a specified type.
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
C
C$ Declarations
C
C     CHARACTER*(*)         KIND
C     INTEGER               COUNT
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     KIND       I   A list of kinds of kernels to count.
C     COUNT      O   The number of kernels of type KIND.
C
C$ Detailed_Input
C
C     KIND       is a list of types of kernels to count when
C                computing loaded kernels.  KIND should consist
C                of a list of words of kernels to examine.  Recognized
C                types are
C
C                   SPK  --- all SPK files are counted in the total.
C                   CK   --- all CK files are counted in the total.
C                   PCK  --- all binary PCK files are counted in the
C                            total.
C                   EK   --- all EK files are counted in the total.
C                   TEXT --- all text kernels that are not meta-text
C                            kernels are included in the total.
C                   META --- all meta-text kernels are counted in the
C                            total.
C                   ALL  --- every type of kernel is counted in the
C                            total.
C
C                 KIND is case insensitive.  If a word appears in KIND
C                 that is not one of those listed above it is ignored.
C
C                 See the Examples section for illustrations of the
C                 use of KIND.
C
C$ Detailed_Output
C
C     COUNT       is the number of kernels loaded through FURNSH that
C                 belong to the list specified by KIND.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If a word on the list specified by KIND is not recognized
C        it is ignored.
C
C     2) If KIND is blank, or none of the words in KIND is on the
C        list specified above, COUNT will be returned as zero.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     KTOTAL allows you to easily determine the number of kernels
C     loaded via the interface FURNSH that are of a type of interest.
C
C$ Examples
C
C     Suppose you wish to determine the number of SPK kernels that
C     have been loaded via the interface FURNSH.  Assign KIND
C     the value 'SPK' and call KTOTAL as shown:
C
C
C        KIND = 'SPK'
C        CALL KTOTAL ( KIND, COUNT )
C
C        WRITE (*,*) 'The number of loaded SPK files is: ', COUNT
C
C     To determine the number of text kernels that are loaded that
C     are not meta-kernels:
C
C        KIND = 'TEXT'
C        CALL KTOTAL ( KIND, NTEXT )
C
C        WRITE (*,*) 'The number of non-meta-text kernels loaded is: '
C       .             NTEXT
C
C     To determine the number of SPK, CK and PCK kernels loaded
C     make the following call:
C
C        KIND = 'SPK PCK CK'
C        CALL KTOTAL ( KIND, COUNT )
C
C
C     To get a count of all loaded kernels
C
C        KIND = 'ALL'
C        CALL KTOTAL ( KIND, COUNT )
C
C        WRITE (*,*) 'There are ', COUNT, ' SPICE kernels loaded.'
C
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
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 02-APR-2009 (NJB)
C
C        Deleted reference to unneeded variable DOALL.
C
C-    SPICELIB Version 1.0.0, 01-JUL-1999 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Number of loaded kernels of a given type
C
C-&
 
      IF ( LOADED .EQ. 0 ) THEN
         COUNT = 0
         RETURN
      END IF
 
      CALL CHKIN  ( 'KTOTAL' )
C
C     Parse KIND to see which kernels are of interest.
C
      DOSPK  = .FALSE.
      DOCK   = .FALSE.
      DOTEXT = .FALSE.
      DOMETA = .FALSE.
      DOEK   = .FALSE.
      DOPCK  = .FALSE.
      START  = 1
 
      CALL FNDNWD ( KIND, START, B, E )
 
 
      DO WHILE ( B .GT. 0 )
 
         IF ( EQSTR( KIND(B:E), 'ALL' ) )THEN
            COUNT = LOADED
            CALL CHKOUT ( 'KTOTAL' )
            RETURN
         ELSE
            DOCK   = DOCK   .OR. EQSTR ( KIND(B:E), 'CK' )
            DOEK   = DOEK   .OR. EQSTR ( KIND(B:E), 'EK' )
            DOMETA = DOMETA .OR. EQSTR ( KIND(B:E), 'META' )
            DOPCK  = DOPCK  .OR. EQSTR ( KIND(B:E), 'PCK' )
            DOSPK  = DOSPK  .OR. EQSTR ( KIND(B:E), 'SPK' )
            DOTEXT = DOTEXT .OR. EQSTR ( KIND(B:E), 'TEXT' )
         END IF
 
         START = E + 1
         CALL FNDNWD ( KIND, START, B, E )
 
      END DO
 
      COUNT = 0
 
 
      DO I = 1, LOADED
 
         ADD =      ( TYPES(I) .EQ. 'CK'   .AND. DOCK   )
     .         .OR. ( TYPES(I) .EQ. 'EK'   .AND. DOEK   )
     .         .OR. ( TYPES(I) .EQ. 'META' .AND. DOMETA )
     .         .OR. ( TYPES(I) .EQ. 'PCK'  .AND. DOPCK  )
     .         .OR. ( TYPES(I) .EQ. 'SPK'  .AND. DOSPK  )
     .         .OR. ( TYPES(I) .EQ. 'TEXT' .AND. DOTEXT )
 
         IF ( ADD ) THEN
            COUNT = COUNT + 1
         END IF
 
      END DO
 
      CALL CHKOUT ( 'KTOTAL' )
      RETURN
 
 
C$Procedure      KDATA ( Kernel Data )
 
      ENTRY KDATA ( WHICH, KIND, FILE, FILTYP, SOURCE, HANDLE, FOUND )
 
C$ Abstract
C
C     Return data for the nth kernel that is among a list of specified
C     kernel types.
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
C
C$ Declarations
C
C     INTEGER               WHICH
C     CHARACTER*(*)         KIND
C     CHARACTER*(*)         FILE
C     CHARACTER*(*)         FILTYP
C     CHARACTER*(*)         SOURCE
C     INTEGER               HANDLE
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     WHICH      I   Index of kernel to fetch from the list of kernels.
C     KIND       I   The kind of kernel to which fetches are limited.
C     FILE       O   The name of the kernel file.
C     FILTYP     O   The type of the kernel.
C     SOURCE     O   Name of the source file used to load FILE.
C     HANDLE     O   The handle attached to FILE.
C     FOUND      O   TRUE if the specified file could be located.
C
C$ Detailed_Input
C
C     WHICH      is the number of the kernel to fetch (matching the
C                type specified by KIND) from the list of kernels that
C                have been loaded through the entry point FURNSH but
C                that have not been unloaded through the entry point
C                UNLOAD.
C
C     KIND       is a list of types of kernels to be considered when
C                fetching kernels from the list of loaded kernels. KIND
C                should consist of words from list of kernel types
C                given below.
C
C                   SPK  --- All SPK files are counted in the total.
C                   CK   --- All CK files are counted in the total.
C                   PCK  --- All binary PCK files are counted in the
C                            total.
C                   EK   --- All EK files are counted in the total.
C                   TEXT --- All text kernels that are not meta-text
C                            kernels are included in the total.
C                   META --- All meta-text kernels are counted in the
C                            total.
C                   ALL  --- Every type of kernel is counted in the
C                            total.
C
C                 KIND is case insensitive.  If a word appears in KIND
C                 that is not one of those listed above it is ignored.
C
C                 See the entry point KTOTAL for examples of the use
C                 of KIND.
C
C$ Detailed_Output
C
C     FILE        is the name of the WHICH'th file of a type matching
C                 KIND that is currently loaded via FURNSH.  FILE
C                 will be blank if there is not a WHICH'th kernel.
C
C     FILTYP      is the type of the kernel specified by FILE.  FILE
C                 will be blank if there is no file matching the
C                 specification of WHICH and KIND.
C
C     SOURCE      is the name of the source file that was used to
C                 specify FILE as one to load.  If FILE was loaded
C                 directly via a call to FURNSH, SOURCE will be blank.
C                 If there is no file matching the specification of
C                 WHICH and KIND, SOURCE will be blank.
C
C     HANDLE      is the handle attached to FILE if it is a binary
C                 kernel.  If FILE is a text kernel or meta-text kernel
C                 HANDLE will be zero.  If there is no file matching
C                 the specification of WHICH and KIND, HANDLE will be
C                 set to zero.
C
C     FOUND       is returned TRUE if a FILE matching the specification
C                 of WHICH and KIND exists.  If there is no such file,
C                 FOUND will be set to FALSE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1) If a file is not loaded matching the specification of WHICH
C        and KIND, FOUND will be FALSE, FILE, FILTYP, and SOURCE
C        will be blank and HANDLE will be set to zero.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This entry point allows you to determine which kernels have
C     been loaded via FURNSH and to obtain information sufficient
C     to directly query those files.
C
C$ Examples
C
C     The following example shows how you could print a summary
C     of SPK files that have been loaded through the interface
C     FURNSH.
C
C
C     CALL KTOTAL ( 'SPK', COUNT )
C
C     IF ( COUNT .EQ. 0 ) THEN
C        WRITE (*,*) 'There are no SPK files loaded at this time.'
C     ELSE
C        WRITE (*,*) 'The loaded SPK files are: '
C        WRITE (*,*)
C     END IF
C
C     DO WHICH = 1, COUNT
C
C        CALL KDATA( WHICH, 'SPK', FILE, FILTYP, SOURCE, HANDLE, FOUND )
C        WRITE (*,*) FILE
C
C     END DO
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
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 02-APR-2009 (NJB)
C
C        Deleted reference to unneeded variable DOALL.
C
C-    SPICELIB Version 1.0.1, 06-DEC-2002 (NJB)
C
C        Typo in header example was corrected.
C
C-    SPICELIB Version 1.0.0, 01-JUL-1999 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Retrieve information on loaded SPICE kernels
C
C-&
 
      FILE   = ' '
      FILTYP = ' '
      SOURCE = ' '
      HANDLE =  0
      FOUND  = .FALSE.
 
      IF ( WHICH .LT. 1 .OR. WHICH .GT. LOADED ) THEN
         RETURN
      END IF
C
C     Parse KIND to see which kernels are of interest.
C
      DOSPK  = .FALSE.
      DOCK   = .FALSE.
      DOTEXT = .FALSE.
      DOMETA = .FALSE.
      DOEK   = .FALSE.
      DOPCK  = .FALSE.
      START  = 1
      CALL FNDNWD ( KIND, START, B, E )
 
      DO WHILE ( B .GT. 0 )
 
         IF ( EQSTR( KIND(B:E), 'ALL' ) )THEN
C
C           There's no point in going on, we can fill in the output
C           variables right now.
C
            FOUND  = .TRUE.
            FILE   = FILES (WHICH)
            FILTYP = TYPES (WHICH)
            HANDLE = HANDLS(WHICH)
 
            IF ( SRCES(WHICH) .NE. 0 ) THEN
               SOURCE = FILES( SRCES(WHICH) )
            END IF
            RETURN
 
         ELSE
            DOCK   = DOCK   .OR. EQSTR ( KIND(B:E), 'CK' )
            DOEK   = DOEK   .OR. EQSTR ( KIND(B:E), 'EK' )
            DOMETA = DOMETA .OR. EQSTR ( KIND(B:E), 'META' )
            DOPCK  = DOPCK  .OR. EQSTR ( KIND(B:E), 'PCK' )
            DOSPK  = DOSPK  .OR. EQSTR ( KIND(B:E), 'SPK' )
            DOTEXT = DOTEXT .OR. EQSTR ( KIND(B:E), 'TEXT' )
         END IF
 
         START = E + 1
         CALL FNDNWD ( KIND, START, B, E )
 
      END DO
 
C
C     Examine the loaded kernels one at a time until we match
C     WHICH files of the specified KIND.
C
      HITS = 0
 
      DO I = 1, LOADED
 
         ADD =      ( TYPES(I) .EQ. 'CK'   .AND. DOCK   )
     .         .OR. ( TYPES(I) .EQ. 'EK'   .AND. DOEK   )
     .         .OR. ( TYPES(I) .EQ. 'META' .AND. DOMETA )
     .         .OR. ( TYPES(I) .EQ. 'PCK'  .AND. DOPCK  )
     .         .OR. ( TYPES(I) .EQ. 'SPK'  .AND. DOSPK  )
     .         .OR. ( TYPES(I) .EQ. 'TEXT' .AND. DOTEXT )
 
         IF ( ADD ) THEN
 
            HITS = HITS + 1
C
C           If we've reached the specified number, fill in the
C           requested information and return.
C
            IF ( HITS .EQ. WHICH ) THEN
 
               FOUND  = .TRUE.
               FILE   = FILES (I)
               FILTYP = TYPES (I)
               HANDLE = HANDLS(I)
 
               IF ( SRCES(I) .NE. 0 ) THEN
                  SOURCE = FILES( SRCES(I) )
               END IF
               RETURN
            END IF
         END IF
 
      END DO
 
      RETURN
 
 
 
C$Procedure      KINFO ( Kernel Information )
 
      ENTRY KINFO ( FILE, FILTYP, SOURCE, HANDLE, FOUND )
 
C$ Abstract
C
C     Return information about a specific kernel
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
C
C$ Declarations
C
C     CHARACTER*(*)         FILE
C     CHARACTER*(*)         FILTYP
C     CHARACTER*(*)         SOURCE
C     INTEGER               HANDLE
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FILE       I   Name of a kernel to fetch information for
C     FILTYP     O   The type of the kernel
C     SOURCE     O   Name of the source file used to load FILE.
C     HANDLE     O   The handle attached to FILE.
C     FOUND      O   TRUE if the specified file could be located.
C
C$ Detailed_Input
C
C     FILE       is the name of a kernel file for which KEEPER
C                information is desired.
C
C$ Detailed_Output
C
C     FILTYP      is the type of the kernel specified by FILE.  FILE
C                 will be blank if FILE is not on the list of loaded
C                 kernels.
C
C     SOURCE      is the name of the source file that was used to
C                 specify FILE as one to load.  If FILE was loaded
C                 directly via a call to FURNSH, SOURCE will be blank.
C                 If FILE is not on the list of loaded kernels, SOURCE
C                 will be blank
C
C     HANDLE      is the handle attached to FILE if it is a binary
C                 kernel.  If FILE is a text kernel or meta-text kernel
C                 HANDLE will be zero.  If FILE is not on the list of
C                 loaded kernels, HANDLE will be set to zero.
C
C     FOUND       is returned TRUE if FILE is on the KEEPER list of
C                 loaded kernels.  If there is no such file, FOUND will
C                 be set to FALSE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1) If the specified file is not on the list of files that
C        are currently loaded via the interface FURNSH, FOUND
C        will be FALSE, HANDLE will be set to zero and FILTYP
C        and SOURCE will be set to blanks.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This entry point allows you to request information directly
C     for a specific SPICE kernel.
C
C$ Examples
C
C     Suppose you wish to determine the type of a loaded kernel
C     so that you can call the correct summarizing routines
C     for the kernel.  The following bit of pseudo code shows
C     how you might use this entry point together with summarizing
C     code to produce a report on the file.  (Note that the
C     routines SPK_SUMMRY, CK_SUMMRY, PCK_SUMMRY and EK_SUMMRY
C     are simply names to indicate what you might do with the
C     information returned by KINFO.  They are not routines that
C     are part of the SPICE toolkit.)
C
C     FILE = '<name of the file of interest>'
C
C     CALL KINFO ( FILE, FILTYP, SOURCE, HANDLE, FOUND )
C
C     IF ( .NOT. FOUND ) THEN
C        WRITE (*,*) FILE
C        WRITE (*,*) 'is not loaded at this time.'
C     ELSE
C
C        IF      ( FILTYP .EQ. 'SPK' ) THEN
C
C           WRITE (*,*) FILE
C           WRITE (*,*) 'is an SPK file.'
C
C           CALL SPK_SUMMRY ( HANDLE )
C
C        ELSE IF ( FILTYP .EQ. 'CK'  ) THEN
C
C           WRITE (*,*) FILE
C           WRITE (*,*) 'is a CK file.'
C
C           CALL CK_SUMMRY ( HANDLE )
C
C        ELSE IF ( FILTYP .EQ. 'PCK' ) THEN
C
C           WRITE (*,*) FILE
C           WRITE (*,*) 'is a  PCK file.'
C
C           CALL PCK_SUMMRY ( HANDLE )
C
C        ELSE IF ( FILTYP .EQ. 'EK'  ) THEN
C
C           WRITE (*,*) FILE
C           WRITE (*,*) 'is an EK file.'
C
C           CALL EK_SUMMRY ( HANDLE )
C
C        ELSE IF ( FILTYP .EQ. 'META') THEN
C           WRITE (*,*) FILE
C           WRITE (*,*) 'is a meta-text kernel.'
C        ELSE
C           WRITE (*,*) FILE
C           WRITE (*,*) 'is a text kernel.'
C        END IF
C
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
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 01-JUL-1999 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Fetch information about a loaded SPICE kernel
C
C-&
 
 
 
      FILTYP = ' '
      SOURCE = ' '
      HANDLE =  0
      FOUND  = .FALSE.
 
      I = ISRCHC ( FILE, LOADED, FILES )
 
      IF ( I .GT. 0 ) THEN
 
         FOUND  = .TRUE.
         FILTYP = TYPES (I)
         HANDLE = HANDLS(I)
 
         IF ( SRCES(I) .NE. 0 ) THEN
            SOURCE = FILES( SRCES(I) )
         END IF
 
      END IF
 
      RETURN
 


 
C$Procedure      KCLEAR ( Keeper clear )
 
      ENTRY KCLEAR
 
C$ Abstract
C
C     Clear the KEEPER subsystem: unload all kernels, clear the kernel
C     pool, and re-initialize the subsystem. Existing watches on kernel
C     variables are retained.
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
C
C$ Declarations
C
C     None.
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     None.
C
C$ Detailed_Input
C
C     None.  This routine operates by side effects.  See Particulars
C     below.
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
C     1) Any errors that occur when setting a kernel pool watch
C        or checking watched variables will be diagnosed by
C        routines in the call tree of this routine.
C
C$ Files
C
C     See Particulars.
C
C$ Particulars
C
C     This entry point allows you re-initialize the KEEPER system with
C     a single call.  
C
C     This routine unloads all kernels from their kernel-type-specific
C     kernel management subsystems (SPKBSR, CKBSR, etc.), clears the
C     kernel pool, clears KEEPER's internal file database, and re-sets
C     the watch status for the kernel variables used to load kernels
C     via meta-kernels. As a side effect of clearing the kernel pool,
C     all watched variables are marked as updated. Note that clearing
C     the kernel pool does not delete watches (aka "watchers"). Watches
C     can be deleted by calling the POOL entry point DWPOOL.
C
C     This capability, though implemented in Fortran, is particularly
C     relevant to SPICE implementations such as Icy, for which the
C     state of the KEEPER system persists after any Icy-based IDL
C     script is run. Successive runs of Icy-based scripts may perform
C     in unexpected ways when scripts access data loaded during runs of
C     previous scripts.
C     
C     Cleaning up after such programs using explicit UNLOAD commands is
C     tedious and error-prone.  One call to this routine sets the
C     KEEPER system to its initial state, preventing unintentional
C     interaction between scripts via KEEPER's state.
C
C$ Examples
C
C     Clear the KEEPER system; check for residual loaded files.
C     We shouldn't find any.
C
C         CALL KCLEAR
C         CALL KTOTAL ( 'ALL', N )
C         WRITE (*,*) 'Count of loaded kernels after KCLEAR call: ', N
C
C$ Restrictions
C
C     Calling this routine will wipe out any kernel pool data
C     inserted via the PXPOOL API routines.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 01-JUL-2014 (NJB) (EDW)
C
C        Updated the discussion of kernel variable watchers.
C
C     Last update was 13-APR-2011 (EDW)
C
C        Trivial edit to Restrictions, replaced P*POOL with
C        PXPOOL. The "*" character causes the HTML documentation
C        script to create a link for the "POOL" substring.
C
C-    SPICELIB Version 1.0.0, 15-NOV-2006 (NJB)
C
C-&
 
C$ Index_Entries
C
C     Re-initialize the keeper system
C     Clear the keeper system
C     Unload all kernels
C
C-&
 
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'KCLEAR' )

C
C     Unloading all kernels is actually much less work than
C     unloading just a few of them.  We unload all of the
C     binary kernels via the "unload" routines for their
C     respective subsystems, then clear the kernel pool.
C
      DO I = 1, LOADED

         IF (      TYPES(I) .EQ. 'SPK' ) THEN

            CALL SPKUEF ( HANDLS(I) )

         ELSE IF ( TYPES(I) .EQ. 'CK' ) THEN

            CALL CKUPF ( HANDLS(I) )

         ELSE IF ( TYPES(I) .EQ. 'PCK' ) THEN

            CALL PCKUOF ( HANDLS(I) )

         ELSE IF ( TYPES(I) .EQ. 'EK' ) THEN

            CALL EKUEF ( HANDLS(I) )

         END IF

      END DO

      CALL CLPOOL

C
C     Although it's not strictly necessary, we initialize
C     KEEPER's database arrays.  This step may occasionally
C     be helpful for debugging.
C
      DO I = 1, LOADED

         FILES (I)  = ' '
         HANDLS(I) = 0
         SRCES (I) = 0
         TYPES (I) = ' '

      END DO

C
C     There's just one counter that indicates the number of
C     database entries:  LOADED.  Set this counter to
C     its initial state.
C
      LOADED = 0

C
C     Calling CLPOOL doesn't remove watches, but it does send a message
C     to each agent indicating that its variables have been touched.
C     Clear this indication by calling CVPOOL.  (This is done for
C     safety; the current implementation of FURNSH doesn't require it.)
C     
      CALL CVPOOL ( 'FURNSH', UPDATE )
 

      CALL CHKOUT ( 'KCLEAR' )
      RETURN





 
C$Procedure      UNLOAD ( Unload a kernel )
 
      ENTRY UNLOAD ( FILE )
 
C$ Abstract
C
C     Unload a SPICE kernel.
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
C
C$ Declarations
C
C     CHARACTER*(*)         FILE
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FILE       I   The name of a kernel to unload.
C
C$ Detailed_Input
C
C     FILE       is the name of a file to unload.  This file
C                should be one loaded through the interface FURNSH.
C                If the file is not on the list of loaded kernels
C                no action is taken.
C
C                Note that if FILE is a meta-text kernel, all of
C                the files loaded as a result of loading the meta-text
C                kernel will be unloaded.
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
C     Error free.
C
C     1) If the specified kernel is not on the list of loaded kernels
C        no action is taken.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The call
C
C        CALL UNLOAD ( FILE )
C
C     has the effect of "erasing" the last previous call:
C
C        CALL FURNSH ( FILE )
C
C     This interface allows you to unload binary and text kernels.
C     Moreover, if you used a meta-text kernel to set up your
C     working environment, you can unload all of the kernels loaded
C     through the meta-kernel by unloading the meta-kernel.
C
C     The usual usage of FURNSH is to load each file needed by your
C     program exactly one time.  However, it is possible to load a
C     kernel more than one time.  (Usually, this is a result of loading
C     meta-kernels without taking the care needed to ensure that the
C     meta-kernels do not specify the same file more than once.)  The
C     effect of unloading a kernel that has been loaded more than once
C     is to "undo" the last loading of the kernel.  Depending upon the
C     kernel and its relationship to other loaded kernels, this may
C     have no visible effect on the working of your program.  To
C     illustrate this behavior suppose that you have a collection of
C     files FILE1, FILE2, FILE3, FILE4, FILE5, FILE6, FILE7, FILE8,
C     META1, META2  where FILE1 ... FILE8 are SPICE kernels and META1
C     and META2 are meta-kernels with the specified kernels to load as
C     shown below.
C      
C
C         META1:
C            KERNELS_TO_LOAD = ( FILE2,
C                                FILE3,
C                                FILE4,
C                                FILE5 )
C
C         META2:
C            KERNELS_TO_LOAD = ( FILE2,
C                                FILE3,
C                                FILE7,
C                                FILE8 )
C
C
C      The following sequence of calls
C
C          CALL FURNSH ( FILE1 )
C          CALL FURNSH ( FILE2 )
C          CALL FURNSH ( FILE3 )
C          CALL FURNSH ( META1 )
C          CALL FURNSH ( FILE6 )
C          CALL FURNSH ( META2 )
C
C      has the effect:
C 
C          "Load" FILE1
C          "Load" FILE2
C          "Load" FILE3
C          "Load" META1 as a text kernel and then...
C                "Load" FILE2 (note that it was loaded from META1)
C                "Load" FILE3 (note that it was loaded from META1)
C                "Load" FILE4 (note that it was loaded from META1)
C                "Load" FILE5 (note that it was loaded from META1)
C          "Load" FILE6
C          "Load" META2 as a text kernel and then...
C                "Load" FILE2 (note that it was loaded from META2)
C                "Load" FILE3 (note that it was loaded from META2) *
C                "Load" FILE7 (note that it was loaded from META2)
C                "Load" FILE8 (note that it was loaded from META2)
C          
C      If we  UNLOAD FILE3
C
C         CALL UNLOAD ( FILE3 )
C         
C      we locate the last time FILE3 was loaded (* above) and modify the
C      state of loaded kernels so that it looks as if we had made the
C      following sequence of "load" operations.
C      
C          "Load" FILE1
C          "Load" FILE2
C          "Load" FILE3
C          "Load" META1 as a text kernel and then...
C                "Load" FILE2 (note that it was loaded from META1)
C                "Load" FILE3 (note that it was loaded from META1)
C                "Load" FILE4 (note that it was loaded from META1)
C                "Load" FILE5 (note that it was loaded from META1)
C          "Load" FILE6
C          "Load" META2 as a text kernel and then...
C                "Load" FILE2 (note that it was loaded from META2)
C                "Load" FILE7 (note that it was loaded from META2)
C                "Load" FILE8 (note that it was loaded from META2)
C
C      As you can see, the data from FILE3 is still available to the
C      program.  All that may have changed is the usage priority
C      associated with that data.
C
C      If we unload META2 (or META1) we remove all remaining files that
C      are noted as being loaded from META2 (or META1)
C
C          CALL UNLOAD ( META2 )
C
C      produces the following load state for the program:
C      
C          "Load" FILE1
C          "Load" FILE2
C          "Load" FILE3
C          "Load" META1 as a text kernel and then...
C                "Load" FILE2 (note that it was loaded from META1)
C                "Load" FILE3 (note that it was loaded from META1)
C                "Load" FILE4 (note that it was loaded from META1)
C                "Load" FILE5 (note that it was loaded from META1)
C          "Load" FILE6
C
C      If we had unloaded META1 instead, we would have this load state.
C
C          "Load" FILE1
C          "Load" FILE2
C          "Load" FILE3
C          "Load" FILE6
C          "Load" META2 as a text kernel and then...
C                "Load" FILE2 (note that it was loaded from META2)
C                "Load" FILE7 (note that it was loaded from META2)
C                "Load" FILE8 (note that it was loaded from META2)
C      
C      So we see that unloading a file does not necessarily make its
C      data unavailable to your program.  Unloading modifies the
C      precedence of the files loaded in your program. The data
C      associated with an unloaded file becomes unavailable only when
C      the file has been unloaded as many times as it was loaded.
C
C      When would you encounter such a scenario? The situation of
C      loading a file more than once might appear if you were trying to
C      contrast the results of computations performed with two
C      different meta-kernels.  In such a scenario you might load a
C      "baseline" set of kernels early in your program and then load
C      and unload meta-kernels to compare results between the two
C      different sets of data.
C      
C     Unloading Text Kernels or Meta-Kernels
C     --------------------------------------
C
C     Part of the action of unloading text (or meta-kernels) is
C     the clearing of the kernel pool and re-loading any kernels that
C     were not in the specified set of kernels to unload.  Since
C     loading of text kernels is not a very fast process, unloading
C     text kernels takes considerably longer than unloading binary
C     kernels.  Moreover, since the kernel pool is cleared, any kernel
C     pool variables you have set from your program by using one of the
C     interfaces PCPOOL, PDPOOL, PIPOOL, or LMPOOL will be removed from
C     the kernel pool.  For this reason, if you plan to use this
C     feature in your program, together with one of the routines
C     specified above, you will need to take special precautions to
C     make sure kernel pool variables required by your program do not
C     inadvertently disappear.
C
C     As a side effect of unloading a text kernel, all watched kernel
C     variables are marked as updated. Note that unloading a text
C     kernel does not delete watchers. Watchers can be deleted by
C     calling the POOL entry point DWPOOL.
C
C$ Examples
C
C     Suppose that you wish to compare two different sets of kernels
C     used to describe the geometry of a mission (for example a predict
C     model and a reconstructed model). You can place all of the
C     kernels for one model in one meta-text kernel, and the other set
C     in a second meta-text kernel.  Let's call these PREDICT.MTA and
C     ACTUAL.MTA.
C
C        CALL FURNSH ( 'PREDCT.MTA' )
C
C        compute quantities of interest and store them
C        for comparison with results of reconstructed
C        (actual) kernels.
C
C        Now unload the predict model and load the reconstructed
C        model.
C
C        CALL UNLOAD ( 'PREDCT.MTA' )
C        CALL FURNSH ( 'ACTUAL.MTA' )
C
C        re-compute quantities of interest and compare them
C        with the stored quantities.
C
C$ Restrictions
C
C     See the note regarding the unloading of Text and meta-text
C     Kernels.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.0.1 01-JUL-2014(NJB)
C
C        Updated discussion of kernel variable watchers.
C
C-    SPICELIB Version 3.0.0 15-NOV-2006 (NJB)
C
C        Bug fix:  corrected update of source pointers when a
C        meta-kernel is unloaded.  Previously source pointers
C        having higher indices than those of the files referenced
C        by the meta kernel were not adjusted when the database
C        was compressed.
C
C-    SPICELIB VERSION 2.0.0, 23-AUG-2001 (WLT)
C
C        Added code to make sure that UNLOAD has the effect of
C        loading all remaining kernels in the order they were first
C        introduced.
C
C-    SPICELIB Version 1.0.0, 01-JUL-1999 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Unload a SPICE kernel
C
C-&

 
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN  ( 'UNLOAD' )
 

      DIDSPK = .FALSE.
      DIDPCK = .FALSE.
      DIDCK  = .FALSE.
      DIDEK  = .FALSE.
      DIDTXT = .FALSE.
 
C
C     First locate the file we need to unload, we search backward
C     through the list of loaded files so that we unload in the right
C     order.
C
      GOTIT = .FALSE.
      I     = LOADED
 
      DO WHILE ( .NOT. GOTIT .AND. I .GT. 0 )
 
         IF ( FILES(I) .EQ. FILE ) THEN
            GOTIT = .TRUE.
         ELSE
            I     = I - 1
         END IF
 
      END DO
 
C
C     If we didn't locate the requested file, there is nothing to do.
C
      IF ( .NOT. GOTIT ) THEN
         CALL CHKOUT ( 'UNLOAD' )
         RETURN
      END IF
 
C
C     We need to know what type of file we've got so that we
C     can take the correct "unload" action.
C
      IF (      TYPES(I) .EQ. 'SPK' ) THEN
         CALL SPKUEF ( HANDLS(I) )
         DIDSPK = .TRUE.
      ELSE IF ( TYPES(I) .EQ. 'CK' ) THEN
         CALL CKUPF ( HANDLS(I) )
         DIDCK = .TRUE.
      ELSE IF ( TYPES(I) .EQ. 'PCK' ) THEN
         CALL PCKUOF ( HANDLS(I) )
         DIDPCK = .TRUE.
      ELSE IF ( TYPES(I) .EQ. 'EK' ) THEN
         CALL EKUEF ( HANDLS(I) )
         DIDEK = .TRUE.
      ELSE IF ( TYPES(I) .EQ. 'TEXT' ) THEN
         CALL CLPOOL
         DIDTXT = .TRUE.
      ELSE IF ( TYPES(I) .EQ. 'META' ) THEN
C
C        This is a special case, we need to undo the effect of loading
C        the meta-kernel.  This means we need to unload all kernels
C        that were loaded using this meta-kernel.
C
         DIDTXT = .TRUE.
         SRC    = I
 

         DO J = LOADED, SRC+1, -1
 
            IF ( SRCES(J) .EQ. SRC ) THEN
C
C              This file was loaded by the meta-kernel of interest.
C              We only need to unload the binary kernels as we
C              will get rid of all text kernels by clearing the
C              kernel pool.
C
               IF (      TYPES(J) .EQ. 'SPK' ) THEN
                  CALL SPKUEF ( HANDLS(J) )
                  DIDSPK = .TRUE.
               ELSE IF ( TYPES(J) .EQ. 'CK' ) THEN
                  CALL CKUPF ( HANDLS(J) )
                  DIDCK = .TRUE.
               ELSE IF ( TYPES(J) .EQ. 'PCK' ) THEN
                  CALL PCKUOF ( HANDLS(J) )
                  DIDPCK = .TRUE.
               ELSE IF ( TYPES(J) .EQ. 'EK' ) THEN
                  CALL EKUEF ( HANDLS(J) )
                  DIDEK = .TRUE.
               END IF
 
               N1 = LOADED
               N2 = LOADED
               N3 = LOADED
               CALL REMLAC ( 1, J, FILES,  N1 )
               CALL REMLAC ( 1, J, TYPES,  N2 )
               CALL REMLAI ( 1, J, SRCES,  N3 )
               CALL REMLAI ( 1, J, HANDLS, LOADED )

C              
C              Each time we delete an item from the database, any
C              pointer to a location past the deletion point must be
C              updated to reflect the compression of the database.
C              Files loaded from meta kernels are always recorded
C              in the database *after* their sources, so each pointer
C              value is less than the index at which it occurs.
C              So, we need examine only those entries from index J
C              upwards.
C
               DO K = J, LOADED

                  IF ( SRCES(K) .GT. J ) THEN
C
C                    This pointer is affected by the deletion of
C                    the Jth database entry.
C
                     SRCES(K) = SRCES(K)-1

                  END IF

               END DO
                
            END IF
 
         END DO
C
C        Now clear the kernel pool.
C
         CALL CLPOOL
 
      END IF
C
C     Remove the I'th kernel from our local database.
C
      N1 = LOADED
      N2 = LOADED
      N3 = LOADED
      CALL REMLAC ( 1, I, FILES,  N1 )
      CALL REMLAC ( 1, I, TYPES,  N2 )
      CALL REMLAI ( 1, I, SRCES,  N3 )
      CALL REMLAI ( 1, I, HANDLS, LOADED )

C
C     Update any source pointers affected by the deletion of the Ith
C     database entry.
C
      DO J = I, LOADED

         IF ( SRCES(J) .GT. I ) THEN
C
C           This pointer is affected by the deletion of the Ith
C           database entry.
C
            SRCES(J) = SRCES(J)-1

         END IF

      END DO

C
C     If we unloaded a text kernel, we now need to reload all
C     of the text kernels that were not unloaded.
C
      IF ( DIDTXT ) THEN
 
         DO I = 1, LOADED
 
            IF (      TYPES(I) .EQ. 'TEXT'
     .           .OR. TYPES(I) .EQ. 'META' ) THEN
 
               CALL LDPOOL ( FILES(I) )
 
               IF ( TYPES(I) .EQ. 'META' ) THEN
C
C                 Clean up any debris that may have been left lying
C                 around because we reloaded a meta-text kernel.
C
                  DO J = 1, NKNOWN
                     CALL DVPOOL ( KNOWN(J) )
                  END DO
 
                  CALL CVPOOL ( 'FURNSH', UPDATE )
 
               END IF
 
            END IF
 
         END DO
 
      END IF
 
C
C     If any SPK files were unloaded, we need to reload everything
C     to establish the right priority sequence for segments.
C
      IF ( DIDSPK ) THEN
 
         DO I = 1, LOADED
            IF ( TYPES(I) .EQ. 'SPK' ) THEN
               CALL SPKLEF ( FILES(I), HANDLS(I) )
            END IF
         END DO
 
      END IF
C
C     If any CK files were unloaded, we need to reload all of the
C     C-kernels to make sure that we have the correct priorities
C     for the remaining C-kernels.
C
      IF ( DIDCK ) THEN
 
         DO I = 1, LOADED
            IF ( TYPES(I) .EQ. 'CK' ) THEN
               CALL CKLPF ( FILES(I), HANDLS(I) )
            END IF
         END DO
 
      END IF
 
C
C     If any binary PCK files were unloaded, we need to reload any
C     remaining ones to re-establish the correct priorities for
C     kernels.
C
      IF ( DIDPCK ) THEN
 
         DO I = 1, LOADED
            IF ( TYPES(I) .EQ. 'PCK' ) THEN
               CALL PCKLOF ( FILES(I), HANDLS(I) )
            END IF
         END DO
 
      END IF
C
C     Finally, if any E-kernels were unloaded, we reload the remaining
C     kernels to make sure the state is restored to the correct set
C     of loaded kernels.
C
      IF ( DIDEK ) THEN
 
         DO I = 1, LOADED
            IF ( TYPES(I) .EQ. 'EK' ) THEN
               CALL EKLEF ( FILES(I), HANDLS(I) )
            END IF
         END DO
 
      END IF
 
      CALL CHKOUT ( 'UNLOAD' )
      RETURN 
 
      END
