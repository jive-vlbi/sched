C$Procedure PCKBSR ( PCK, buffer segments for readers )

      SUBROUTINE PCKBSR ( FNAME,
     .                    HANDLE,
     .                    BODY,
     .                    ET,
     .                    DESCR,
     .                    IDENT,
     .                    FOUND   )

      IMPLICIT NONE

C$ Abstract
C
C     Load and unload PCK binary files for use by the readers.
C     Buffer segments for readers.
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
C     PCK
C
C$ Keywords
C
C     PCK
C     FILES
C
C$ Declarations

      CHARACTER*(*)         FNAME
      INTEGER               HANDLE
      INTEGER               BODY
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      DESCR    ( * )
      CHARACTER*(*)         IDENT
      LOGICAL               FOUND

      INTEGER               FTSIZE
      PARAMETER           ( FTSIZE =   5000 )

      INTEGER               BTSIZE
      PARAMETER           ( BTSIZE =     20 )

      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL =     -5 )

      INTEGER               STSIZE
      PARAMETER           ( STSIZE =   5000 )


C$ Brief_I/O
C
C     Variable  I/O  Entry points
C     --------  ---  --------------------------------------------------
C     FNAME      I   PCKLOF
C     HANDLE    I/O  PCKLOF, PCKUOF, PCKSFS
C     BODY       I   PCKSFS
C     ET         I   PCKSFS
C     DESCR      O   PCKSFS
C     IDENT      O   PCKSFS
C
C$ Detailed_Input
C
C     FNAME      is the name of an PCK file to be loaded.
C
C     HANDLE     on input is the handle of an PCK file to be
C                unloaded.
C
C     BODY       is the NAIF integer code of an ephemeris object,
C                typically a solar system body.
C
C     ET         is a time, in seconds past the epoch J2000 TDB.
C
C$ Detailed_Output
C
C     HANDLE     on output is the handle of the binary PCK file
C                containing a located segment.
C
C     DESCR      is the descriptor of a located segment.
C
C     IDENT      is the identifier of a located segment.
C
C     FOUND      is a logical flag indicating whether a segment meeting
C                the search criteria was found. FOUND will have the
C                value .TRUE. if an appropriate segment was found during
C                the search; it will have the value of .FALSE.
C                otherwise. If FOUND has the value .FALSE., then either
C                an appropriate segment could not be found in any of the
C                loaded files or there were no PCK kernel files loaded
C                when the request for a segment was made.
C
C$ Parameters
C
C     FTSIZE     is the maximum number of files that may be loaded
C                by PCKLOF at any given time for use by the PCK readers.
C
C     BTSIZE     is the maximum number of bodies whose segments can be
C                buffered by PCKSFS.
C
C     STSIZE     Maximum number of segments that can be buffered at any
C                given time by PCKSFS.
C
C$ Exceptions
C
C     1) If PCKBSR is called directly, the error 'SPICE(BOGUSENTRY)'
C        is signaled.
C
C     2) See entry points PCKLOF, PCKUOF, and PCKSFS for exceptions
C        specific to them.
C
C$ Files
C
C     PCK kernel  files are indicated by filename before loading
C     (see PCKLOF) and handle after loading (all other places).
C
C$ Particulars
C
C     PCKBSR serves as an umbrella, allowing data to be shared by its
C     entry points:
C
C        PCKLOF       Load PCK binary file.
C        PCKUOF       Unload PCK binary file.
C        PCKSFS       Select file and segment.
C
C     Before a file can be read by the PCK kernel readers, it must be
C     loaded by PCKLOF, which among other things, calls routines to
C     open the specified file.
C
C     Multiple files may be loaded for use simultaneously, and a file
C     need only be loaded once to become a potential search target
C     for any number of subsequent reads.
C
C     Once a PCK kernel file is loaded and opened, it is assigned a file
C     handle, which is used by the calling program to refer to the file
C     in all subsequent calls to PCK routines.
C
C     A file may be removed from the list of files searched by using
C     PCKUOF to unload it.
C
C     PCKSFS performs the search for segments within a file for the
C     PCK kernel readers.  It searches through the most recently loaded
C     files first.  Within a single file, PCKSFS searches through
C     the segments in reverse order, beginning with the last segment in
C     the file.  The search stops when the first appropriate segment is
C     found or all files and segments have been searched without a
C     match.
C
C     PCKSFS buffers information from loaded PCK files to improve access
C     time by preventing unnecessary file reads during segment searches.
C
C$ Examples
C
C     Example 1:
C     ---------
C
C     Suppose that the data of interest are contained in the file
C     THE_MISSION.PCK, and that we want to generate a table containing
C     the descriptors of the PCK segments, or a message indicating that
C     no segment was found, for various request times. We are interested
C     in the data coverage of the segments in the file.
C
C     Let
C
C        PCK_HANDL      be the handle for the mission PCK file.
C        HANDLE         be the handle obtained from a segment search. In
C                       this example, because there is only a single
C                       file, this will always have the same value.
C        BODY           be the NAIF ID code for the body of interest.
C        BEG_ET         be the beginning epoch for a data table that
C                       is generated.
C        END_ET         be the ending epoch for a data table that is
C                       generated.
C        DELTA          be the time step, in seconds, between
C                       consecutive times for a data table that is
C                       generated.
C        ET             be the epoch of interest for a segment
C                       search to get a data table entry.
C        DESCR ( 5 )    be the descriptor of the PCK segment that is
C                       found.
C        IDENT          be the identifier of the PCK segment that is
C                       found.
C        TABLE          be the logical unit for the data table that is
C                       generated.
C        ENTRY          be a string to hold a formatted PCK segment
C                       descriptor which is to be written to the table.
C        FOUND          be a logical flag indicating that an
C                       appropriate PCK segment has been found.
C
C     The two routine names FORMAT_ENTRY and WRITE_ENTRY are used here
C     for purposes of demonstration only. Routines with these names do
C     not exist in SPICELIB. FORMAT_ENTRY is used to format a PCK
C     segment descriptor into a character string for the table
C     generated, and WRITE_ENTRY is used to write an entry to the file.
C
C     The code fragment below loads PCK files and performs searches for
C     various epochs, generating a table containing the segment
C     descriptors, if found, or a message indicating that a segment
C     descriptor was not found.
C
C     C
C     C     Load the mission PCK file.
C     C
C           CALL PCKLOF ( 'THE_MISSION.PCK',  PCK_HANDL )
C
C     C
C     C     Search for segments using evenly spaced epochs between
C     C     BEG_ET and END_ET.
C     C
C           ET = BEG_ET
C
C           DO WHILE ( ET .LE. END_ET )
C
C     C
C     C        Locate the applicable segment (handle and descriptor).
C     C
C              CALL PCKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
C
C              IF ( FOUND ) THEN
C
C                 CALL FORMAT_ENTRY ( DESCR, ENTRY )
C
C              ELSE
C
C                 ENTRY = '***** SEGMENT NOT FOUND *****'
C
C              END IF
C
C              CALL WRITE_ENTRY ( ET, ENTRY, TABLE )
C
C     C
C     C        Increment the epoch.
C     C
C              ET = ET + DELTA
C
C           END DO
C
C     Example 2:
C     ---------
C
C     In this example multiple PCK files are loaded and searched for
C     segments.
C
C     Let
C
C        PCK_HANDL      be the handle used when loading PCK files.
C        HANDLE         be the handle obtained from a segment search. In
C                       this example, because there is only a single
C                       file, this will always have the same value.
C        BODY           be the NAIF ID code for the body of interest.
C        ET             be the epoch of interest for a segment
C                       search to get a data table entry.
C        DESCR ( 5 )    be the descriptor of the PCK segment that is
C                       found.
C        IDENT          be the identifier of the PCK segment that is
C                       found.
C        FOUND          be a logical flag indicating that an
C                       appropriate PCK segment has been found.
C
C     The code fragment below loads several PCK files and then performs
C     a search for an appropriate segment.
C
C     C
C     C     Load the PCK files. We can reuse the variable PCK_HANDL
C     C     because the handle for the appropriate file is returned by
C     C     the search.
C     C
C           CALL PCKLOF ( 'FIRST.PCK',   PCK_HNDL )
C           CALL PCKLOF ( 'SECOND.PCK',  PCK_HNDL )
C           CALL PCKLOF ( 'THIRD.PCK',   PCK_HNDL )
C           CALL PCKLOF ( 'FOURTH.PCK',  PCK_HNDL )
C           CALL PCKLOF ( 'FIFTH.PCK',   PCK_HNDL )
C
C     C
C     C     Do some computation that yields a body and epoch
C     C     of interest.
C     C
C                              .
C                              .
C                              .
C     C
C     C     Search for an appropriate segment in the loaded files.
C     C
C
C           CALL PCKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
C
C           IF ( FOUND ) THEN
C
C              Display results.
C
C           ELSE
C
C              WRITE (*,*) 'Sorry, no segment was found.'
C
C           END IF
C
C
C$ Restrictions
C
C     1) If Fortran I/O errors occur while searching a loaded PCK
C        file, the internal state of this suite of routines may
C        be corrupted.  It may be possible to correct the state
C        by unloading the pertinent PCK files and then re-loading
C        them.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     K.S. Zukor      (JPL)
C     J.M. Lynch      (JPL)
C     R.E. Thurman    (JPL)
C     W.L. Taber      (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 17-MAR-2014 (NJB)
C
C        Updated segment pool initialization condition in entry
C        point PCKLOF so that the pool is initialized only if the file
C        table is empty.
C     
C-    SPICELIB Version 1.4.0, 03-JAN-2014 (BVS)(EDW)
C
C        Minor edits to Procedure; clean trailing whitespace.
C
C        Increased FTSIZE (from 1000 to 5000).
C
C        Increased STSIZE (from 100 to 5000).
C
C-    SPICELIB Version 1.3.0, 01-MAR-2011 (NJB)
C
C        Bug fix:
C
C          In the PCKSFS 'MAKE ROOM' state, when the suspended activity
C          is 'ADD TO FRONT' and no segment table room is available,
C          the body table's pointer to the current segment list
C          is now set to null. Previously the pointer was allowed to go
C          stale.
C
C-    SPICELIB Version 1.2.0, 08-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in MOVED calls in entry points PCKUOF and PCKSFS.
C
C-    SPICELIB Version 1.1.0, 08-NOV-2001 (NJB)
C
C        Bug fixes:
C
C           1) When a segment list is freed because the entire list
C              is contributed by a single PCK file, and the list is
C              too large to be buffered, the corresponding body table
C              pointer is now set to null.
C
C           2) An algorithm change has eliminated a bug caused by not
C              updating the current body index when body table entries
C              having empty segment lists were compressed out of the
C              body table.  Previously the body table pointer BINDEX
C              could go stale after the compression.
C
C           3) When a already loaded kernel is re-opened with DAFOPR,
C              it now has its link count reset to 1 via a call to
C              DAFCLS.
C
C           4) The load routine PCKLOF now resets all file numbers when
C              the next file number reaches INTMAX()-1, thereby
C              avoiding arithmetic overflow.
C
C           5) The unload routine PCKUOF now calls RETURN() on entry and
C              returns if so directed.
C
C           6) In PCKSFS, DAF calls are followed by tests of FAILED()
C              in order to ensure that the main state loop terminates.
C
C           7) In PCKSFS, a subscript bound violation in a loop
C              termination test was corrected.
C
C        The "re-use interval" feature was introduced to improve speed
C        in the case where repeated, consecutive requests are satisified
C        by the same segment.  For each body, the associated re-use
C        interval marks the time interval containing the previous
C        request time for which the previously returned segment provides
C        the  highest-priority data available.
C
C        The segment list cost algorithm was modified slightly:
C        the contribution of a file search to the cost of a list
C        is included only when the file search is completed.  The
C        cost of finding the re-use interval is accounted for when
C        unbuffered searches are required.
C
C        The file table size has been increased to 1000, in order
C        to take advantage of the DAF system's new ability to load
C        1000 files.
C
C        Various small updates and corrections were made to the
C        comments throughout the file.
C
C        In order to simplify the source code, the in-line singly
C        linked list implementation of the segment table has been
C        replaced by an implementation relying on the SPICELIB
C        doubly linked list routines.
C
C-    SPICELIB Version 1.0.0, 16-MAR-1994 (KSZ)
C
C        This differs only slightly from the SPKXXX code.
C        The main difference is that the SFS subroutine returns
C        FOUND = .FALSE. if no files are found, rather than returning
C        an error.
C
C-&

C$ Index_Entries
C
C     buffer PCK segments for readers
C
C-&

C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      DPMIN

      INTEGER               INTMAX
      INTEGER               ISRCHI
      INTEGER               LNKNFN
      INTEGER               LNKNXT
      INTEGER               LNKPRV
      INTEGER               LNKTL

      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters
C
      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ = 5 )

      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN = 40 )

      INTEGER               SLEN
      PARAMETER           ( SLEN   = 15 )

      INTEGER               ND
      PARAMETER           ( ND     = 2 )

      INTEGER               NI
      PARAMETER           ( NI     = 5 )


C
C     Constants used in the doubly linked list structure:
C
      INTEGER               FORWRD
      PARAMETER           ( FORWRD  = 1 )

      INTEGER               BCKWRD
      PARAMETER           ( BCKWRD  = 2 )

C
C     Local variables
C

C
C
C     The file table contains the handle and file number of each file
C     that has been loaded for use with the PCK readers. File
C     numbers begin at one, and are incremented until they reach a
C     value of INTMAX() - 1, at which point they are mapped to the
C     range 1:NFT, where NFT is the number of loaded PCK files.
C
C     (A file number is similar to a file handle, but it is assigned
C     and used exclusively by this module. The purpose of file numbers
C     is to keep track of the order in which files are loaded and the
C     order in which they are searched.)
C
C     All names begin with FT.
C
C        HAN      Handle
C        NUM      File number
C
C     NFT is the number of files that have been loaded. NEXT is
C     incremented whenever a new file is loaded to give the file
C     number of the file. FINDEX is the index of whatever file is
C     of current interest at any given time.
C
C     New files are added at the end of the table. As files are
C     removed, succeeding files are moved forward to take up the
C     slack. This keeps the table ordered by file number.
C
      INTEGER               NFT
      INTEGER               FTHAN    ( FTSIZE )
      INTEGER               FTNUM    ( FTSIZE )
      INTEGER               NEXT
      INTEGER               FINDEX

C
C     The body table contains the beginning of the list of the stored
C     segments for each body, and the expense at which that list
C     was constructed. (The expense of a body list is the number of
C     segment descriptors examined during the construction of the list.)
C     It also contains the highest and lowest file numbers searched
C     during the construction of the list.
C
C     For each body, the time bounds of the "re-use interval" of the
C     last segment found are stored.  This interval is the maximal
C     interval containing the epoch of the last request for data for
C     this body, such that the interval is not masked by higher-priority
C     segments.  The handle, segment descriptor, and segment identifier
C     returned on the last request are also stored.
C
C     All names begin with BT.
C
C        BOD      Body
C        EXP      Expense
C        HFS      Highest file (number) searched
C        LFS      Lowest  file (number) searched
C        BEG      Beginning of segment list
C        LB       Lower bound of the re-use interval of
C                 previous segment returned.
C        UB       Upper bound of the re-use interval of
C                 previous segment returned.
C        PRVD     Previous descriptor returned.
C        PRVI     Previous segment identifier returned.
C        PRVH     Previous handle returned.
C        CHKP     Logical indicating that previous segment should
C                 be checked to see whether it satisfies a request.
C        RUEX     Expense of the re-use interval.
C
C     NBT is the number of bodies for which segments are currently
C     being stored in the table. BINDEX is the index of whatever
C     body is of current interest at any given time.
C
C     New bodies are added at the end of the table. As bodies are
C     removed, the last body is moved forward to take up the slack.
C     This keeps the entries in the table contiguous.
C
      CHARACTER*(SIDLEN)    BTPRVI   ( BTSIZE )

      DOUBLE PRECISION      BTPRVD   ( DSCSIZ, BTSIZE )
      DOUBLE PRECISION      BTLB     ( BTSIZE )
      DOUBLE PRECISION      BTUB     ( BTSIZE )

      INTEGER               BINDEX
      INTEGER               BTBEG    ( BTSIZE )
      INTEGER               BTBOD    ( BTSIZE )
      INTEGER               BTEXP    ( BTSIZE )
      INTEGER               BTHFS    ( BTSIZE )
      INTEGER               BTLFS    ( BTSIZE )
      INTEGER               BTPRVH   ( BTSIZE )
      INTEGER               BTRUEX   ( BTSIZE )
      INTEGER               NBT

      LOGICAL               BTCHKP   ( BTSIZE )

C
C     The segment table contains the handle, descriptor, and identifier
C     for each segment that has been found so far.
C
C     The segment table is implemented as a set of arrays indexed by
C     a SPICE doubly linked list structure.  For each body in the
C     body table, there is a segment table list; each node of a list
C     points to data associated with a segment.  In each list, the head
C     node corresponds to the highest-priority segment in that list,
C     and segment priority decreases in the forward direction.
C
C     All names begin with ST.
C
C        POOL     Doubly linked list pool.
C        HAN      Handle
C        DES      Descriptor
C        IDNT     Identifier
C
C     New segments are added to the front or end of a body list
C     as appropriate, according to the rules spelled out under
C     entry point PCKSFS.
C
      INTEGER               STPOOL ( 2, LBPOOL : STSIZE )
      INTEGER               STHAN  (             STSIZE )
      DOUBLE PRECISION      STDES  ( DSCSIZ,     STSIZE )
      CHARACTER*(SIDLEN)    STIDNT (             STSIZE )

C
C     Other stuff
C
      CHARACTER*(SLEN)      DOING
      CHARACTER*(SLEN)      STACK    ( 2 )
      CHARACTER*(SLEN)      STATUS
      CHARACTER*(SLEN)      URGENT

      DOUBLE PRECISION      DCD      ( ND )

      INTEGER               CHEAP
      INTEGER               COST
      INTEGER               CRFLBG
      INTEGER               HEAD
      INTEGER               I
      INTEGER               ICD      ( NI )
      INTEGER               J
      INTEGER               MINEXP
      INTEGER               NEW
      INTEGER               NXTSEG
      INTEGER               P
      INTEGER               TAIL
      INTEGER               TOP

      LOGICAL               FND
      LOGICAL               FNDHAN

C
C     Saved variables
C
      SAVE                  BTBEG,
     .                      BTBOD,
     .                      BTCHKP,
     .                      BTEXP,
     .                      BTHFS,
     .                      BTLB,
     .                      BTLFS,
     .                      BTPRVD,
     .                      BTPRVH,
     .                      BTPRVI

      SAVE                  BTRUEX,
     .                      BTUB,
     .                      FTHAN,
     .                      FTNUM,
     .                      NBT,
     .                      NEXT,
     .                      NFT,
     .                      STDES,
     .                      STHAN,
     .                      STIDNT

      SAVE                  STPOOL


C
C     Initial values
C
      DATA                  NFT       / 0 /
      DATA                  NBT       / 0 /
      DATA                  NEXT      / 0 /

C
C     Nobody has any business calling PCKBSR directly.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'PCKBSR' )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'PCKBSR' )

      RETURN



C$Procedure PCKLOF ( PCK, load binary file )

      ENTRY PCKLOF ( FNAME, HANDLE )

C$ Abstract
C
C
C     Load a binary PCK file for use by the readers.  Return the
C     handle of the loaded file which is used by other PCK routines to
C     refer to the file.
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
C     PCK
C
C$ Keywords
C
C     PCK
C     FILES
C
C$ Declarations
C
C     CHARACTER*(*)         FNAME
C     INTEGER               HANDLE
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     FNAME      I   Name of the file to be loaded.
C     HANDLE     O   Loaded file's handle.
C     FTSIZE     P   Maximum number of loaded PCK files.
C
C$ Detailed_Input
C
C     FNAME      Character name of the file to be loaded.
C
C$ Detailed_Output
C
C     HANDLE     Integer handle assigned to the file upon loading.
C                Almost every other PCK routine will subsequently use
C                this number to refer to the file.
C
C$ Parameters
C
C     FTSIZE     is the maximum number of PCK files that may
C                be loaded simultaneously under any circumstances.
C                FTSIZE is currently set to match the maximum number
C                of DAF files that may be loaded simultaneously.
C
C$ Exceptions
C
C     1) If an attempt is made to open more DAF files than is specified
C        by the parameter FTSIZE in DAFAH, an error is signaled by a
C        routine in the call tree of this routine.
C
C     2) If an attempt is made to load more files than is specified
C        by the local paramater FTSIZE, and if the DAF system has
C        room to load another file, the error SPICE(PCKFILETABLEFULL)
C        signaled.  The current setting of FTSIZE does not allow this
C        situation to arise:  the DAF system will trap the error
C        before this routine has the chance.
C
C$ Files
C
C     A file specified by FNAME, to be loaded.  The file is assigned a
C     handle by PCKLOF, which will be used by most other routines to
C     refer to it.
C
C$ Particulars
C
C     If there is room for a new file in the file table, PCKLOF creates
C     an entry for it and loads the file for reading using DAFOPR.
C
C$ Examples
C
C     See the Example above, in PCKBSR.
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
C     K.S. Zukor      (JPL)
C     J.M. Lynch      (JPL)
C     R.E. Thurman    (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 17-MAR-2014 (NJB)
C
C        Updated segment pool initialization condition in entry
C        point PCKLOF so that the pool is initialized only if the file
C        table is empty.
C     
C-    SPICELIB Version 1.1.1, 03-JAN-2014 (EDW)
C
C        Minor edits to Procedure; clean trailing whitespace.
C        Removed unneeded Revisions section.
C
C-    SPICELIB Version 1.1.0, 08-NOV-2001 (NJB)
C
C        Bug fixes:
C
C        1) When an already loaded kernel is opened with DAFOPR,
C           it now has its link count reset to 1 via a call to
C           DAFCLS.
C
C        2) This routine now resets all file numbers when
C           the next file number reaches INTMAX()-1, thereby avoiding
C           arithmetic overflow.  The numbers in the file table
C           are replaced with consecutive integers in the range
C           1 : NFT, such that the ordering of the numbers is not
C           changed.  The HFS and LFS arrays are updated accordingly.
C
C        Also, the flags indicating validity of the re-use intervals
C        are set to .FALSE. here.
C
C-    SPICELIB Version 1.0.0, 16-MAR-1994 (KSZ)
C
C-&

C$ Index_Entries
C
C     load PCK file
C
C-&

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PCKLOF' )
      END IF

C
C     Any time we load a file, there is a possibility that the
C     re-use intervals are invalid because they're been superseded
C     by higher-priority data.  Since we're not going to examine
C     the loaded file, simply indicate that all of the re-use
C     intervals are invalid.
C
      DO I = 1, NBT
         BTCHKP(I) = .FALSE.
      END DO

C
C     Nothing works unless at least one file has been loaded, so this
C     is as good a place as any to initialize the segment table pool.
C     We want to avoid unnecessary initializations, so we only
C     initialize the list when no files are loaded. It's quite possible
C     to have files loaded and an empty body table, so we don't
C     want to re-initialize just because there are no body table
C     entries.
C
      IF ( NFT .EQ. 0 ) THEN
         CALL LNKINI ( STSIZE, STPOOL )
      END IF

C
C     To load a new file, first try to open it for reading.
C
      CALL DAFOPR ( FNAME, HANDLE )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'PCKLOF' )
         RETURN
      END IF

C
C     Determine if the file is already in the table.
C
      FINDEX = ISRCHI ( HANDLE, NFT, FTHAN )

      IF ( FINDEX .GT. 0 ) THEN
C
C        The last call we made to DAFOPR added another DAF link to
C        the PCK file.  Remove this link.
C
         CALL DAFCLS ( HANDLE )

C
C        Remove the file from the file table and remove its segments
C        from the segment table.  If the segment list for a body
C        becomes empty, remove that body from the body table.
C
         NFT = NFT - 1

         DO I = FINDEX, NFT
            FTHAN( I ) = FTHAN( I + 1 )
            FTNUM( I ) = FTNUM( I + 1 )
         END DO

         I = 1

         DO WHILE ( I .LE. NBT )

            P = BTBEG( I )

            DO WHILE ( P .GT. 0 )
C
C              Find the successor of P, if any.
C
               NXTSEG = LNKNXT ( P, STPOOL )

               IF ( STHAN( P ) .EQ. HANDLE ) THEN
C
C                 The segment corresponding to node P came from
C                 the file we're unloading.  Delete the node for
C                 P from the segment list for body I; if P happens
C                 to be the head node for body I's segment list,
C                 make the successor of P the head of the list.
C
                  CALL LNKFSL ( P, P, STPOOL )

                  IF ( P .EQ. BTBEG(I) ) THEN
                     BTBEG( I ) = NXTSEG
                  END IF

               END IF
C
C              Update P.
C
               P = NXTSEG

            END DO

C
C           If the list for this body is now empty, shorten the current
C           table by one: put all the entries for the last body in the
C           table into the space occupied by the one we've deleted.
C
            IF ( BTBEG( I ) .LE. 0 ) THEN
C
C              Because all of the re-use intervals are invalid, we need
C              not copy the saved items associated with them.  The
C              items not copied are
C
C                 BTCHKP
C                 BTLB
C                 BTPRVD
C                 BTPRVH
C                 BTPRVI
C                 BTRUEX
C                 BTUB
C
               BTBOD( I ) = BTBOD( NBT )
               BTEXP( I ) = BTEXP( NBT )
               BTHFS( I ) = BTHFS( NBT )
               BTLFS( I ) = BTLFS( NBT )
               BTBEG( I ) = BTBEG( NBT )

               NBT = NBT - 1

            ELSE

               I = I + 1

            END IF

         END DO

      ELSE
C
C        This is a new file.  Make sure that there are unused slots
C        in the file table.
C
         IF ( NFT .EQ. FTSIZE ) THEN
C
C           This error case can occur only if FTSIZE is larger than
C           the maximum number of open DAF files.  Currently FTSIZE
C           is equal to this limit.
C
            CALL DAFCLS ( HANDLE )

            CALL SETMSG ( 'The internal file table is already full, ' //
     .                    'with # entries.'         )
            CALL ERRINT ( '#', FTSIZE               )
            CALL SIGERR ( 'SPICE(PCKFILETABLEFULL)' )
            CALL CHKOUT ( 'PCKLOF'                  )
            RETURN

         END IF

      END IF

C
C     Determine the next file number.  Note that later code assumes
C     that the file number can be incremented by 1, so we can't allow
C     the file number to reach INTMAX().
C
      IF ( NEXT .LT. INTMAX()-1 ) THEN

         NEXT = NEXT + 1

      ELSE
C
C        The user is to be congratulated:  we've run out of file
C        numbers.
C
C        Re-set the valid file numbers so they lie in the range 1:NFT,
C        with the Ith file in the file table having file number I.
C        First update the LFS and HFS components of the body table
C        according to this mapping.
C
C        Set any body table entries that are lower than FTNUM(1) to
C        zero.
C
         DO I = 1, NBT
C
C           Re-map the HFS table for the Ith body.
C
            J = ISRCHI ( BTHFS(I), NFT, FTNUM )

            IF ( J .GT. 0 ) THEN
C
C              The highest file searched for body I is the Jth file
C              in the file table.
C
               BTHFS(I) = J

            ELSE
C
C              The highest file searched for body I is not in the file
C              table.  This occurs when the highest file searched has
C              been unloaded.  Note that this assigment makes all files
C              appear to be "new" when a lookup for body I is performed.
C
               BTHFS(I) = 0

            END IF

C
C           Re-map the LFS table for the Ith body.
C
            J = ISRCHI ( BTLFS(I), NFT, FTNUM )

            IF ( J .GT. 0 ) THEN
C
C              The lowest file searched for body I is the Jth file
C              in the file table.
C
               BTLFS(I) = J

            ELSE
C
C              The lowest file searched for body I is not in the file
C              table.  This occurs when the lowest file searched has
C              been unloaded.  Force reconstruction of the list by
C              making all files "new."
C
               BTLFS(I) = 0
               BTHFS(I) = 0

            END IF

         END DO

C
C        Re-map the file number table itself.
C
         DO I = 1, NFT

            FTNUM(I) = I

         END DO

C
C        Assign a new file number.
C
         NEXT = NFT + 1

      END IF


      NFT        = NFT  + 1
      FTHAN(NFT) = HANDLE
      FTNUM(NFT) = NEXT

      CALL CHKOUT ( 'PCKLOF' )
      RETURN



C$Procedure PCKUOF ( PCK, unload binary file )

      ENTRY PCKUOF ( HANDLE )

C$ Abstract
C
C     Unload a binary PCK file so that it will no longer be searched by
C     the readers.
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
C     PCK
C
C$ Keywords
C
C     PCK
C     FILES
C
C$ Declarations
C
C     INTEGER               HANDLE
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of file to be unloaded
C
C$ Detailed_Input
C
C     HANDLE     Integer handle assigned to the file upon loading.
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
C     1) Unloading a file that has not been loaded is a no-op.
C        No error is signaled.
C
C$ Files
C
C     The file referred to by HANDLE is unloaded.
C
C$ Particulars
C
C     A file is removed from consideration by the readers by a call to
C     PCKUOF.
C
C     If the file specified by HANDLE is not currently loaded in the
C     PCK system, no action is taken.
C
C$ Examples
C
C     See the Example above, in PCKBSR.
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
C     K.S. Zukor      (JPL)
C     J.M. Lynch      (JPL)
C     R.E. Thurman    (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.1.1, 03-JAN-2014 (EDW)
C
C        Minor edits to Procedure; clean trailing whitespace.
C        Removed unneeded Revisions section.
C
C-    SPICELIB Version 4.1.0, 08-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in MOVED call.
C
C-    SPICELIB Version 1.1.0, 08-NOV-2001 (NJB)
C
C        Bug fixes:
C
C        1) This routine now calls RETURN() on entry and
C           returns if so directed.
C
C        Also, the flags indicating validity of those re-use intervals
C        whose data comes from the unloaded file are set to .FALSE.
C
C-    SPICELIB Version 1.0.0, 16-MAR-1994 (KSZ)
C
C-&

C$ Index_Entries
C
C     unload PCK file
C
C-&

      IF ( RETURN() ) THEN
         RETURN
      END IF

C
C     All of the stored segments from the file must be removed
C     from the segment table (by returning the corresponding nodes
C     to the segment table pool.)
C
C     Don't do anything if the given handle is not in the file table.
C
      FINDEX = ISRCHI ( HANDLE, NFT, FTHAN )

      IF ( FINDEX .EQ. 0 ) THEN
         RETURN
      END IF

C
C     First get rid of the entry in the file table. Close the file
C     before wiping out the handle.
C
      CALL DAFCLS ( FTHAN(FINDEX) )

      NFT = NFT - 1

      DO I = FINDEX, NFT
         FTHAN( I ) = FTHAN( I + 1 )
         FTNUM( I ) = FTNUM( I + 1 )
      END DO

C
C     Check each body list individually. Note that the first node
C     on each list, having no predecessor, must be handled specially.
C
      I = 1

      DO WHILE ( I .LE. NBT )

         P = BTBEG( I )

         DO WHILE ( P .GT. 0 )

            NXTSEG = LNKNXT ( P, STPOOL )

            IF ( STHAN(P) .EQ. HANDLE ) THEN

               IF ( P .EQ. BTBEG(I) ) THEN
                  BTBEG( I ) = NXTSEG
               END IF

               CALL LNKFSL ( P, P, STPOOL )

            END IF

            P = NXTSEG

         END DO

C
C        If we happened to get rid of all of the segments for this
C        body, then the body should be deleted from the table: shift
C        all entries for the body at the end of the table into the
C        space occupied by the deleted body.
C
         IF ( BTBEG(I) .LE. 0 ) THEN

            IF ( I .NE. NBT ) THEN

               BTBOD (I) = BTBOD (NBT)
               BTEXP (I) = BTEXP (NBT)
               BTHFS (I) = BTHFS (NBT)
               BTLFS (I) = BTLFS (NBT)
               BTBEG (I) = BTBEG (NBT)
               BTLB  (I) = BTLB  (NBT)
               BTUB  (I) = BTUB  (NBT)
               BTPRVH(I) = BTPRVH(NBT)
               BTPRVI(I) = BTPRVI(NBT)
               BTCHKP(I) = BTCHKP(NBT)
               BTRUEX(I) = BTRUEX(NBT)

               CALL MOVED ( BTPRVD(1,NBT), DSCSIZ, BTPRVD(1,I) )

            END IF

            NBT = NBT - 1

         ELSE

            I = I + 1

         END IF

      END DO

C
C     Any time we unload a file, we may be removing the file
C     providing data for the re-use interval for one or more bodies.
C     For each body, if the handle associated with the re-use interval
C     happens to be that of the file we're unloading, indicate
C     that the re-use interval is invalid.
C
      DO I = 1, NBT

         IF ( BTCHKP(I) ) THEN

            IF ( BTPRVH(I) .EQ. HANDLE ) THEN
               BTCHKP(I) = .FALSE.
            END IF

         END IF

      END DO

      RETURN



C$Procedure PCKSFS ( PCK, select file and segment )

      ENTRY PCKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )

C$ Abstract
C
C     Search through loaded files to find the first segment applicable
C     to the body and time specified.  Buffer searched segments in the
C     process, to attempt to avoid re-reading files.
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
C     PCK
C
C$ Keywords
C
C     PCK
C     FILES
C
C$ Declarations
C
C     INTEGER               BODY
C     DOUBLE PRECISION      ET
C     INTEGER               HANDLE
C     DOUBLE PRECISION      DESCR  ( * )
C     CHARACTER*(*)         IDENT
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     BODY       I   Body ID.
C     ET         I   Ephemeris time.
C     HANDLE     O   Handle of file containing the applicable segment.
C     DESCR      O   Descriptor of the applicable segment.
C     IDENT      O   Identifier of the applicable segment.
C     FOUND      O   Indicates whether or not a segment was found.
C
C$ Detailed_Input
C
C     BODY       is the NAIF integer code of an ephemeris object,
C                typically a solar system body.
C
C     ET         is a time, in seconds past the epoch J2000 TDB.
C
C$ Detailed_Output
C
C     HANDLE     on output is the handle of the binary PCK file
C                containing a located segment.
C
C     DESCR      is the descriptor of a located segment.
C
C     IDENT      is the identifier of a located segment.
C
C     FOUND      indicates whether a requested segment was found or not.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If an attempt is made to call PCKSFS when there aren't any
C        files loaded, the error SPICE(NOLOADEDFILES) is signaled.
C
C$ Files
C
C     All files loaded by PCKLOF are potential search targets for
C     PCKSFS.
C
C$ Particulars
C
C     This routine finds the highest-priority segment, in any loaded
C     PCK file, such that the segment provides data for the specified
C     body and epoch.
C
C$ Examples
C
C     See the Example above, in PCKBSR.
C
C$ Restrictions
C
C     1) If Fortran I/O errors occur while searching a loaded PCK
C        file, the internal state of this suite of routines may
C        be corrupted.  It may be possible to correct the state
C        by unloading the pertinent PCK files and then re-loading
C        them.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     K.S. Zukor      (JPL)
C     R.E. Thurman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.2.1, 03-JAN-2014 (EDW)
C
C        Minor edits to Procedure; clean trailing whitespace.
C        Removed unneeded Revisions section.
C
C-    SPICELIB Version 4.2.0, 01-MAR-2011 (NJB)
C
C        Bug fix:
C
C          In the PCKSFS 'MAKE ROOM' state, when the suspended activity
C          is 'ADD TO FRONT' and no segment table room is available,
C          the body table's pointer to the current segment list
C          is now set to null. Previously the pointer was allowed to go
C          stale.
C
C-    SPICELIB Version 4.1.0, 08-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in MOVED call.
C
C-    SPICELIB Version 1.1.0, 08-NOV-2001 (NJB)
C
C        Bug fixes:
C
C           1) When a segment list is freed because the entire list
C              is contributed by a single PCK file, and the list is
C              too large to be buffered, the corresponding body table
C              pointer is now set to null.
C
C           2) An algorithm change has eliminated a bug caused by not
C              updating the current body index when body table entries
C              having empty segment lists were compressed out of the
C              body table.  Previously the body table pointer BINDEX
C              could go stale after the compression.
C
C           3) DAF calls are now followed by tests of FAILED()
C              in order to ensure that the main state loop terminates.
C
C           4) A subscript bound violation in a loop termination test
C              was corrected.  The loop is located in the
C              'SEARCH W/O BUFFERING' block; it finds the start of a
C              partial list that is to be freed.
C
C        The "re-use interval" feature was introduced to improve speed
C        in the case where repeated, consecutive requests are satisified
C        by the same segment.
C
C        The segment list cost algorithm was modified slightly:
C        the contribution of a file search to the cost of a list
C        is included only when the file search is completed.  The
C        cost of finding the re-use interval is accounted for when
C        unbuffered searches are required.
C
C        The file table size has been increased to 1000, in order
C        to take advantage of the DAF system's new ability to load
C        1000 files.
C
C        The body table size has been increased to 200 in order to
C        decrease the chance of thrashing due to swapping segment
C        lists for different bodies.
C
C        Various small updates and corrections were made to the
C        comments throughout the file.
C
C        In order to simplify the source code, the in-line singly
C        linked list implementation of the segment table has been
C        replaced by an implementation relying on the SPICELIB
C        doubly linked list routines.
C
C-    SPICELIB Version 1.0.0, 16-MAR-1994 (KSZ)
C
C        This differs only slightly from the SPKXXX code.
C        The main difference is that the SFS subroutine returns
C        FOUND = FALSE if no files are found, rather than returning
C        an error.
C
C-&

C$ Index_Entries
C
C     select PCK file and segment
C
C-&

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PCKSFS' )
      END IF

C
C     Assume the segment is not found, until it actually is.
C
      FOUND = .FALSE.

C
C     Buffering segments involves maintaining three tables:  the
C     file table, the body table, and the segment table.  The routine
C     is broken down into various tasks, described below, which
C     perform these manipulations.  A description of the components
C     of each table is provided in the declarations section of PCKBSR.

C
C     Return FOUND as .FALSE. if no files are loaded.  Unlike the SPK
C     case, it's not an error to call this routine if no files are
C     loaded.
C
      IF ( NFT .EQ. 0 ) THEN
         CALL CHKOUT ( 'PCKSFS' )
         RETURN
      END IF

C
C     The stack of suspended tasks is empty.
C
      TOP = 0

C
C     In the following loop, we will try to simplify things by
C     doing exactly one thing on each pass through the loop.
C     After each pass, the status of the loop (STATUS) will be
C     adjusted to reflect the next thing that needs to be done.
C     Occasionally, the current task will have to be interrupted
C     until another task can be carried out. (For example, when
C     collecting new segments, an interrupt might place a segment
C     at the front or end of the current body list; when placing
C     the segment on the list, a second interrupt might free up
C     room in the segment table in order to allow the addition
C     to proceed.) In this case, the current task will be saved and
C     restored after the more urgent task has been completed.
C
C     The loop can terminate in only one of two ways (unless an
C     error occurs). First, if an applicable segment is found in
C     the segment table, the  handle, descriptor, and identifier for
C     the segment are returned immediately.  Second, if the table
C     does not contain an applicable segment, and if no files remain
C     to be searched, the loop terminates normally, and no data are
C     returned.
C
C     The individual tasks are described below.
C
C     'NEW BODY'
C
C
C        This indicates that the specified body has no segments stored
C        for it at all. It must be added to the body table.  (This is
C        followed immediately by an OLD FILES search, in which every
C        file loaded is considered an old file.)
C
C     'NEW FILES'
C
C        This indicates that at least one new file has been added
C        since the last time the segment list for the specified
C        body was searched. Find the oldest of these new files,
C        and begin a NEW SEGMENTS search in forward order for
C        segments to add to the front of the list.
C
C     'NEW SEGMENTS'
C
C        Continue a NEW FILES search, adding segments for the specified
C        body to the front of the list.
C
C     'OLD FILES'
C
C        This indicates that although the list has been searched
C        and found to contain no applicable segment, some of the
C        older files remain to be searched. Find the newest of these
C        old files, and begin an OLD SEGMENTS search in backward order.
C
C     'OLD SEGMENTS'
C
C        Continue an OLD FILES search, adding segments for the specified
C        body to the end of the list.
C
C     'CHECK LIST'
C
C        This indicates that the list is ready to be searched,
C        either because no new files have been added, or because
C        segments from a new file or an old file have recently
C        been added.
C
C        The list is never checked until all new files have been
C        searched.
C
C        If an applicable segment is found, it is returned.
C
C     'MAKE ROOM' (Interrupt)
C
C        This indicates that one of the bodies must be removed,
C        along with its stored segments, to make room for another
C        body or segment.  The body (other than the one being searched
C        for) with the smallest expense is selected for this honor.
C
C     'ADD TO FRONT' (Interrupt)
C
C        This indicates that a segment has been found (during the
C        course of a NEW FILES search) and must be added to the front
C        of the list.
C
C     'ADD TO END' (Interrupt)
C
C        This indicates that a segment has been found (during the
C        course of an OLD FILES search) and must be added to the end
C        of the list.
C
C     'SUSPEND'
C
C        This indicates that the current task (DOING) should be
C        interrupted until a more urgent task (URGENT) can be
C        carried out. The current task is placed on a stack for
C        safekeeping.
C
C     'RESUME'
C
C        This indicates that the most recently interrupted task
C        should be resumed immediately.
C
C     '?'
C
C        This indicates that the next task is not immediately
C        apparent: if new files exist, they should be searched;
C        otherwise the list should be checked.
C


C
C     Is the body already in the body table?  This determines what the
C     first task should be.
C
      BINDEX = ISRCHI ( BODY, NBT, BTBOD )

      IF ( BINDEX .EQ. 0 ) THEN

         STATUS = 'NEW BODY'

      ELSE
C
C        Much of the time, the segment used to satisfy the previous
C        request for a given body will also satisfy the current request
C        for data for that body.  Check whether this is the case.
C
         IF ( BTCHKP(BINDEX) ) THEN
C
C           The previous segment found for the current body is a
C           viable candidate for the current request.  See whether
C           the input ET value falls into the re-use interval for this
C           body:  the time interval for which the previously returned
C           segment for this body provides the highest-priority
C           coverage.
C
C           We treat the re-use interval as topologically open because
C           one or both endpoints may belong to higher-priority
C           segments.
C
            IF (       ( ET .GT. BTLB(BINDEX) )
     .           .AND. ( ET .LT. BTUB(BINDEX) )  ) THEN
C
C              The request time is covered by the segment found on
C              the previous request for data for the current body,
C              and this interval is not masked by any higher-priority
C              segments.  The previous segment for this body satisfies
C              the request.
C
               HANDLE = BTPRVH(BINDEX)
               IDENT  = BTPRVI(BINDEX)

               CALL MOVED ( BTPRVD(1,BINDEX), DSCSIZ, DESCR )

               FOUND  = .TRUE.

               CALL CHKOUT ( 'PCKSFS' )
               RETURN

            END IF

C
C           Adjust the expense here. If the expense of the list
C           contains a component due to the cost of finding the
C           unbuffered segment providing data for re-use, subtract
C           that component from the expense.
C
            BTEXP(BINDEX)  = BTEXP(BINDEX) - BTRUEX(BINDEX)
            BTRUEX(BINDEX) = 0

C
C           The re-use interval becomes invalid if it didn't satisfy
C           the request.  The validity flag gets re-set below.
C
C           At this point, the previous segment is not a candidate
C           to satisfy the request---at least not until we've verified
C           that
C
C              - The previous segment is still available.
C
C              - The previous segment hasn't been superseded by a more
C                recently loaded segment.
C
            BTCHKP(BINDEX) = .FALSE.

         END IF


C
C        If the segment list for this body is empty, make sure the
C        expense is reset to 0.
C
         IF ( BTBEG(BINDEX) .EQ. 0 ) THEN

            BTEXP(BINDEX) = 0

         END IF


         STATUS = '?'

      END IF


      DO WHILE ( STATUS .NE. 'HOPELESS' )
C
C        If new files have been added, they have to be searched.
C        Otherwise, we can go right to the list of stored segments.
C
         IF ( STATUS .EQ. '?' ) THEN
C
C           There are two ways to get to this point.
C
C           1)  Status may have been set to '?' prior to the
C               loop DO WHILE ( STATUS .NE. HOPELESS ).
C
C           2)  Status was set to '?' by the NEW SEGMENTS block
C               of code as the result of finishing the read of
C               a new file.
C

            IF ( BTHFS( BINDEX ) .LT. FTNUM( NFT ) ) THEN
               STATUS = 'NEW FILES'
            ELSE
               STATUS = 'CHECK LIST'
            END IF



         ELSE IF ( STATUS .EQ. 'NEW BODY' ) THEN
C
C           New bodies are added to the end of the body table. If the
C           table is full, one of the current occupants must be
C           removed to make room for the new one.
C
C           Setting LFS to one more than the highest current
C           file number means the OLD FILES SEARCH that follows will
C           begin with the last-loaded file.
C
C           There is one way to get here:
C
C           1)  The variable STATUS was set to NEW BODY prior to the
C               loop DO WHILE ( STATUS .NE. HOPELESS ).
C
C           Find the cheapest slot in the body table to store
C           the initial information about this body.
C
C           NOTE:  This used to be handled by the MAKE ROOM section.
C           However, trying to handle this special case there was
C           just more trouble than it was worth.
C

            IF ( NBT .LT. BTSIZE ) THEN
C
C              If the body table isn't full, the cheapest place is
C              just the next unused row of the table.
C
               NBT   = NBT + 1
               CHEAP = NBT

            ELSE
C
C              The body table is full.  Find the least
C              expensive body in the table and remove it.
C
               CHEAP  = 1
               MINEXP = BTEXP(1)

               DO I = 2, NBT

                  IF ( BTEXP(I) .LT. MINEXP ) THEN
                     CHEAP  = I
                     MINEXP = BTEXP(I)
                  END IF

               END DO

C
C              If there are any segments associated with the
C              least expensive body, we put them back on the free
C              list.
C
               HEAD = BTBEG(CHEAP)

               IF ( HEAD .GT. 0 ) THEN

                  TAIL =  - LNKPRV ( HEAD, STPOOL )
                  CALL LNKFSL ( HEAD, TAIL, STPOOL )

               END IF

            END IF

C
C           Set up a body table entry for the new body.
C
            BTBOD (CHEAP) = BODY
            BTEXP (CHEAP) = 0
            BTHFS (CHEAP) = FTNUM(NFT)
            BTLFS (CHEAP) = FTNUM(NFT) + 1
            BTBEG (CHEAP) = 0
            BTCHKP(CHEAP) = .FALSE.

C
C           The following items associated with the re-use interval
C           need not be initialized at this point:
C
C              BTRUEX
C              BTLB
C              BTUB
C              BTPRVH
C              BTPRVI
C              BTPRVD
C
C           However, we'll give these items initial values to
C           help prevent compilation warnings from zealous
C           compilers.
C
            BTRUEX(CHEAP) = 0
            BTLB  (CHEAP) = DPMIN()
            BTUB  (CHEAP) = DPMAX()
            BTPRVH(CHEAP) = 0
            BTPRVI(CHEAP) = ' '
            CALL CLEARD ( DSCSIZ, BTPRVD(1,CHEAP) )

C
C           BINDEX is the body table index of the new entry.
C
            BINDEX = CHEAP

C
C           Now search the loaded PCK files for segments relating to
C           this body.  We start with the last-loaded files and
C           work backwards.
C
            STATUS = 'OLD FILES'



         ELSE IF ( STATUS .EQ. 'NEW FILES' ) THEN
C
C           When new files exist, they should be searched in forward
C           order, beginning with the oldest new file not yet searched.
C           All new files must be searched before the list can be
C           checked, to ensure that the best (newest) segments are
C           being used.
C
C           Begin a forward search, and prepare to look for individual
C           segments from the file.
C
C           The only way to get here is to have STATUS set to
C           the value NEW FILES in the STATUS .EQ. '?' block
C           of the IF structure.
C
C           Find the next file to search; set FINDEX to the
C           corresponding file table entry.
C
            FINDEX = 1

            DO WHILE ( BTHFS( BINDEX ) .GE. FTNUM( FINDEX ) )
               FINDEX = FINDEX + 1
            END DO

            BTHFS( BINDEX ) = FTNUM( FINDEX )

            CALL DAFBFS ( FTHAN( FINDEX ) )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'PCKSFS' )
               RETURN
            END IF

            STATUS = 'NEW SEGMENTS'

C
C           The cost of the list contributed by the new file is
C           zero so far.
C
            COST = 0


         ELSE IF ( STATUS .EQ. 'NEW SEGMENTS' ) THEN
C
C           New files are searched in forward order. Segments, when
C           found, are inserted at the front of the list. Invisible
C           segments (alpha > omega) are ignored.
C
C           Each segment examined, whether applicable or not, adds to
C           the expense of the list.
C
C           The only way to get here is from the NEW FILES block
C           of the IF structure.

            CALL DAFFNA ( FND )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'PCKSFS' )
               RETURN
            END IF

            IF ( .NOT. FND ) THEN
C
C              We're out of segments in the current file.  Decide
C              whether we need to examine another new file, or
C              whether we're ready to check the list.
C
               STATUS = '?'
               BTEXP( BINDEX ) = BTEXP( BINDEX ) + COST

            ELSE

               CALL DAFGS ( DESCR )
               CALL DAFUS ( DESCR, ND, NI, DCD, ICD )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'PCKSFS' )
                  RETURN
               END IF

               IF (       ( ICD(1) .EQ. BODY   )
     .              .AND. ( DCD(1) .LE. DCD(2) )  ) THEN

                  DOING  = 'NEW SEGMENTS'
                  URGENT = 'ADD TO FRONT'
                  STATUS = 'SUSPEND'

               END IF

               COST = COST + 1

            END IF
C
C           If we haven't reset the status, we'll return for another
C           'NEW SEGMENTS' pass.
C

         ELSE IF ( STATUS .EQ. 'OLD FILES' ) THEN
C
C           When old files must be searched (because the segments
C           in the list are inadequate), they should be searched
C           in backward order, beginning with the newest old file
C           not yet searched. The segment list will be re-checked
C           after each file is searched.  If a match is found,
C           the search terminates, so some old files may not be
C           searched.
C
C           Search from the end, and prepare to look for individual
C           segments from the file.
C
C           You can get to this block in two ways.
C
C           1) We can have a NEW BODY
C
C           2) We have checked the current list (CHECK LIST) for
C              this body, didn't find an applicable segment and
C              have some files left that have not been seached.

            FINDEX = NFT

            DO WHILE ( BTLFS( BINDEX ) .LE. FTNUM( FINDEX ) )
               FINDEX = FINDEX - 1
            END DO

            CALL DAFBBS ( FTHAN( FINDEX ) )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'PCKSFS' )
               RETURN
            END IF


            STATUS = 'OLD SEGMENTS'
C
C           The next thing we'll do is search through all the segments
C           of this file for those that applicable to this body.
C           The cost of the list contributed by the current file is
C           zero so far.
C
            COST = 0


         ELSE IF ( STATUS .EQ. 'OLD SEGMENTS' ) THEN
C
C           Old files are searched in backward order. Segments, when
C           found, are inserted at the end of the list. Invisible
C           segments (alpha > omega) are ignored.
C
C           Each segment examined, whether applicable or not, adds to
C           the expense of the list.
C
C           There is only one way to get here---from the
C           block 'OLD FILES'.  Note we do not add to the
C           expense of the list for this body until we've
C           completely searched this file.
C
            CALL DAFFPA ( FND )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'PCKSFS' )
               RETURN
            END IF

            IF ( .NOT. FND ) THEN
C
C              We've been through all of the segments in this file.
C              Change the lowest file searched indicator for this body
C              to be the current file, and go check the current list.
C
               BTLFS( BINDEX ) =  FTNUM( FINDEX )
               BTEXP( BINDEX ) =  BTEXP( BINDEX ) + COST
               STATUS          = 'CHECK LIST'

            ELSE

               CALL DAFGS ( DESCR )
               CALL DAFUS ( DESCR, ND, NI, DCD, ICD )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'PCKSFS' )
                  RETURN
               END IF

               IF (       ( ICD(1) .EQ. BODY   )
     .              .AND. ( DCD(1) .LE. DCD(2) )  )  THEN

                  DOING  = 'OLD SEGMENTS'
                  URGENT = 'ADD TO END'
                  STATUS = 'SUSPEND'

               END IF

               COST = COST + 1

            END IF
C
C           If we haven't reset the status, we'll return for another
C           'OLD SEGMENTS' pass.
C

         ELSE IF ( STATUS .EQ. 'CHECK LIST'   ) THEN
C
C           Okay, all the new files (and maybe an old file or two) have
C           been searched. Time to look at the list of segments stored
C           for the body to see if one applicable to the specified
C           epoch is hiding in there. If so, return it.  If not,
C           try another old file.  If there are no more old files,
C           give up the ghost.
C
C           There are two ways to get to this point.
C
C           1) From the '?' block.
C           2) From the 'OLD SEGMENTS' block.
C
C           For every segment examined, initialize the re-use interval
C           associated with the current body.
C
            BTLB(BINDEX) = DPMIN()
            BTUB(BINDEX) = DPMAX()
            P            = BTBEG( BINDEX )

            DO WHILE ( P .GT. 0 )

               IF ( ET .GT. STDES(2,P) ) THEN
C
C                 ET is to the right of the coverage interval of this
C                 segment.
C
                  BTLB(BINDEX) = MAX ( BTLB(BINDEX), STDES(2,P) )


               ELSE IF ( ET .LT. STDES(1,P) ) THEN
C
C                 ET is to the left of the coverage interval of this
C                 segment.
C
                  BTUB(BINDEX) = MIN ( BTUB(BINDEX), STDES(1,P) )

               ELSE
C
C                 The segment coverage interval includes ET.
C
                  CALL MOVED ( STDES( 1,P ), DSCSIZ, DESCR )
                  IDENT  = STIDNT( P )
                  HANDLE = STHAN ( P )
                  FOUND  = .TRUE.

C
C                 Set the re-use interval for the current body.
C
                  BTLB(BINDEX) = MAX ( BTLB(BINDEX), STDES(1,P) )
                  BTUB(BINDEX) = MIN ( BTUB(BINDEX), STDES(2,P) )

C
C                 Save the returned output items, in case this segment
C                 may satisfy the next request.
C
                  BTPRVH(BINDEX) =  HANDLE
                  BTPRVI(BINDEX) =  IDENT
                  CALL MOVED ( DESCR, DSCSIZ, BTPRVD(1,BINDEX) )
                  BTCHKP(BINDEX) =  .TRUE.

                  CALL CHKOUT ( 'PCKSFS' )
                  RETURN

               END IF

C
C              Get the next node.  We avoid LNKNXT here in order
C              to speed up the operation.
C
               P = STPOOL ( FORWRD, P )

            END DO

C
C           If we're still here we didn't have information for this
C           body in the segment list.
C
C           If there are more files, search them.
C           Otherwise, things are hopeless, set the status that way.
C
            IF ( BTLFS( BINDEX ) .GT. FTNUM( 1 ) ) THEN
               STATUS = 'OLD FILES'
            ELSE
               STATUS = 'HOPELESS'
            END IF



         ELSE IF ( STATUS .EQ. 'MAKE ROOM' ) THEN
C
C           When adding a segment to a full segment table, one of
C           the current bodies must be dropped. The ideal candidate
C           is the one whose list was constructed at the lowest expense.
C           The candidate should be removed from the body table, and
C           its list transferred to the segment table pool.
C
C           There is ``room'' if the segment table pool contains at
C           least one free node.
C
C           It is possible that a single body requires more than the
C           entire segment table for its own segments. Two things might
C           happen in such a case:
C
C              1) If the list under consideration was being added to at
C                 the end, then a search is continued without buffering
C                 any segments.
C
C              2) If the list was being added to at the beginning, then
C                 that means there was a NEW FILES search going on, and
C                 so a brand new list is constructed for the body, much
C                 as in a 'NEW BODY' task.
C
C           There are two different ways to get to this point.
C
C              1) From 'ADD TO FRONT' if the segment table pool is full.
C              2) From 'ADD TO END' if the segment table pool is full.
C
C           Try to make room by deleting a segment list.  CHEAP will
C           be the index of the "cheapest" segment list in the body
C           table.
C
            MINEXP = INTMAX()
            CHEAP  = 0

            DO I = 1, NBT

               IF ( I .NE. BINDEX ) THEN
C
C                 This list is for a body other than the current
C                 one.
C
                  IF (      ( BTEXP(I) .LT. MINEXP   )
     .                 .OR. ( CHEAP    .EQ. 0        )  )THEN
C
C                    This list is the cheapest seen so far,
C                    possibly because it's the first one
C                    considered.  At the moment, it's as good
C                    a candidate for removal as any.
C
                     CHEAP  = I
                     MINEXP = BTEXP(I)

                  END IF

               END IF

            END DO


           IF ( CHEAP .EQ. 0 ) THEN
C
C              What we do if there are no delete-able segments
C              depends on the task that was suspended before entering
C              'MAKE ROOM'.
C
               IF ( STACK( TOP ) .EQ. 'ADD TO END' ) THEN
C
C                 There's nothing left to do but search the remaining
C                 files and segments without buffering them.
C
                  STATUS = 'SEARCH W/O BUFF'


               ELSE
C
C                 STACK(TOP) is set to 'ADD TO FRONT'.
C
C                 If there is no room left in the table in the middle
C                 of an attempt to add to the front of the list, just
C                 start from scratch by treating all files as
C                 unsearched and doing an OLD FILES search, as would
C                 be done for a new body.
C
C                 Return the current list to the segment table pool.
C
C                 Note that, according to the specification of the
C                 SPICELIB doubly linked list routines, the backward
C                 pointer of a list head is the negative of the tail
C                 node.
C
                  P    =   BTBEG ( BINDEX )
                  TAIL = - LNKPRV( P, STPOOL )

                  CALL LNKFSL ( P, TAIL, STPOOL )

C
C                 Re-initialize the table for this body, and initiate
C                 an 'OLD FILES' search, just as in 'NEW BODY'.
C                 Also, reset the suspended task stack to be empty.
C
                  BTBEG( BINDEX ) = 0
                  BTEXP( BINDEX ) = 0
                  BTHFS( BINDEX ) = FTNUM( NFT )
                  BTLFS( BINDEX ) = FTNUM( NFT ) + 1
                  STATUS          = 'OLD FILES'
                  TOP             = 0

               END IF

            ELSE
C
C              Return this cheapest list to the segment pool.
C
               P = BTBEG  ( CHEAP )

               IF ( P .GT. 0 ) THEN

                  TAIL = - LNKPRV ( P, STPOOL )
                  CALL LNKFSL ( P, TAIL, STPOOL )

               END IF

C
C              Fill the deleted body's space in the table with
C              the final entry in the table.
C
               IF ( CHEAP .NE. NBT ) THEN

                  BTBOD ( CHEAP ) = BTBOD ( NBT )
                  BTEXP ( CHEAP ) = BTEXP ( NBT )
                  BTHFS ( CHEAP ) = BTHFS ( NBT )
                  BTLFS ( CHEAP ) = BTLFS ( NBT )
                  BTBEG ( CHEAP ) = BTBEG ( NBT )
                  BTLB  ( CHEAP ) = BTLB  ( NBT )
                  BTUB  ( CHEAP ) = BTUB  ( NBT )
                  BTPRVH( CHEAP ) = BTPRVH( NBT )
                  BTPRVI( CHEAP ) = BTPRVI( NBT )
                  BTRUEX( CHEAP ) = BTRUEX( NBT )
                  BTCHKP( CHEAP ) = BTCHKP( NBT )


                  CALL MOVED ( BTPRVD(1,NBT), DSCSIZ, BTPRVD(1,CHEAP) )

               END IF

C
C              If the final entry in the table happened to be the
C              current body of interest, then we also have to change
C              the current body index.
C
               IF ( BINDEX .EQ. NBT ) THEN
                  BINDEX = CHEAP
               END IF

C
C              One less body now.
C
               NBT    = NBT - 1
               STATUS = 'RESUME'

            END IF
C
C           Either we made room by freeing a non-empty segment list,
C           or we're going to work without additional space.  In the
C           latter case, the state is now 'OLD FILES' or
C           'SEARCH W/O BUFF'.
C


         ELSE IF ( STATUS .EQ. 'ADD TO FRONT' ) THEN
C
C           The current segment information should be linked in at
C           the head of the segment list for the current body, and
C           the pertinent body table entry should point to the new
C           head of the list.
C
C           The only way to get here is from the block NEW SEGMENTS
C           after suspending that task.
C
            IF ( LNKNFN(STPOOL) .EQ. 0 ) THEN
C
C              There's no room left in the segment pool.  We must make
C              room before continuing.
C
               DOING  = 'ADD TO FRONT'
               URGENT = 'MAKE ROOM'
               STATUS = 'SUSPEND'

            ELSE
C
C              Allocate a node and link it to the front of the list
C              for the current body.
C
               CALL LNKAN ( STPOOL, NEW )

               STHAN( NEW ) = FTHAN( FINDEX )
               CALL MOVED ( DESCR, DSCSIZ, STDES(1,NEW) )
               CALL DAFGN ( STIDNT(NEW) )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'PCKSFS' )
                  RETURN
               END IF

C
C              If the current list is empty, this append operation
C              is a no-op.
C
               CALL LNKILB ( NEW, BTBEG(BINDEX), STPOOL )
               BTBEG( BINDEX ) = NEW

               STATUS  = 'RESUME'

            END IF



         ELSE IF ( STATUS .EQ. 'ADD TO END' ) THEN
C
C           The current segment information should be linked in at
C           the tail of the segment list for the current body.
C
C           The only way to get to this task is from the OLD SEGMENTS
C           block after suspending that task.
C
            IF ( LNKNFN(STPOOL) .EQ. 0 ) THEN
C
C              There's no room left in the segment pool.  We must make
C              room before continuing.
C
               DOING  = 'ADD TO END'
               URGENT = 'MAKE ROOM'
               STATUS = 'SUSPEND'

            ELSE
C
C              Allocate a new node in the segment table pool.
C
               CALL LNKAN ( STPOOL, NEW )

               STHAN( NEW ) = FTHAN( FINDEX )
               CALL MOVED ( DESCR, DSCSIZ, STDES( 1,NEW ) )
               CALL DAFGN ( STIDNT( NEW ) )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'PCKSFS' )
                  RETURN
               END IF

               IF ( BTBEG(BINDEX) .LE. 0 ) THEN
C
C                 This is the first node in the list for this body.
C
                  BTBEG( BINDEX ) = NEW

               ELSE
C
C                 Link the new node to the tail of the list.
C
                  TAIL = - LNKPRV ( BTBEG(BINDEX), STPOOL )
                  CALL LNKILA ( TAIL, NEW, STPOOL )

               END IF

               STATUS = 'RESUME'

            END IF



         ELSE IF ( STATUS .EQ. 'SEARCH W/O BUFF' ) THEN
C
C           When the segment table is completely full, continue
C           the search by looking through the unchecked portion
C           of the segment list for the current body, and
C           then searching old, unchecked files without buffering
C           their segments.
C
C           The only way to get here is from the MAKE ROOM state
C           via the block ADD TO END.  If you get here there is no
C           free space in the segment table pool.
C
C           At this point, we need to initialize the cost of
C           the re-use interval.
C
            BTRUEX(BINDEX) = 0

C
C           Need to find the portion of the current body's segment
C           list which comes from the current file of interest.  It
C           will be returned to the segment table pool, since the
C           remainder of the file's segments can't be added to the list.
C
            CRFLBG = BTBEG( BINDEX )
            FNDHAN = .FALSE.

            DO WHILE (  ( .NOT. FNDHAN ) .AND. ( CRFLBG .GT. 0 )  )

               FNDHAN = STHAN( CRFLBG ) .EQ. FTHAN( FINDEX )

               IF ( .NOT. FNDHAN ) THEN
C
C                 Get the next node.  We avoid LNKNXT here in order
C                 to speed up the operation.
C
                  CRFLBG = STPOOL ( FORWRD, CRFLBG )

               END IF

            END DO

            IF ( CRFLBG .GT. 0 ) THEN
C
C              The sub-list from the current node onwards is to be
C              returned to the segment table pool.  Save this node,
C              since we'll finish searching the list before freeing
C              the sub-list.
C
               P = CRFLBG

C
C              It may be that the sub-list we're deleting is the
C              entire segment list for this body.  If so, the
C              corresponding body table entry should be set to
C              a non-positive value to indicate an empty segment list.
C
               IF ( P .EQ. BTBEG(BINDEX) ) THEN

                  BTBEG(BINDEX) = 0
C
C                 Also in this case, we must initialize the re-use
C                 interval for this body.
C
                  BTLB(BINDEX) = DPMIN()
                  BTUB(BINDEX) = DPMAX()

               END IF

C
C              Finish searching through the incomplete list for the
C              desired segment.
C
               DO WHILE ( CRFLBG .GT. 0 )
C
C                 Every segment seen from the current file contributes
C                 to the expense of the re-use interval.
C
                  BTRUEX(BINDEX) = BTRUEX(BINDEX) + 1


                  IF ( ET .GT. STDES(2,CRFLBG) ) THEN
C
C                    ET is to the right of the coverage interval of this
C                    segment.
C
                     BTLB(BINDEX) = MAX( BTLB(BINDEX), STDES(2,CRFLBG) )


                  ELSE IF ( ET .LT. STDES(1,CRFLBG) ) THEN
C
C                    ET is to the left of the coverage interval of this
C                    segment.
C
                     BTUB(BINDEX) = MIN( BTUB(BINDEX), STDES(1,CRFLBG) )

                  ELSE
C
C                    The segment coverage interval includes ET.
C
                     CALL MOVED ( STDES( 1,CRFLBG ), DSCSIZ, DESCR )

                     IDENT  = STIDNT( CRFLBG )
                     HANDLE = STHAN ( CRFLBG )
                     FOUND  = .TRUE.

C
C                    Set the re-use interval for the current body.
C
                     BTLB(BINDEX) = MAX( BTLB(BINDEX), STDES(1,CRFLBG) )
                     BTUB(BINDEX) = MIN( BTUB(BINDEX), STDES(2,CRFLBG) )

C
C                    Save the output items, in case this
C                    segment may be satisfy the next request.
C
                     BTPRVH(BINDEX) =  HANDLE
                     BTPRVI(BINDEX) =  IDENT
                     CALL MOVED ( DESCR, DSCSIZ, BTPRVD(1,BINDEX) )
                     BTCHKP(BINDEX) =  .TRUE.

C
C                    Update the expense of the list to reflect
C                    the cost of locating this segment.
C
                     BTEXP(BINDEX) = BTEXP(BINDEX) + BTRUEX(BINDEX)

C
C                    Free the sub-list we were searching.
C
                     TAIL = LNKTL ( CRFLBG, STPOOL )
                     CALL LNKFSL ( P, TAIL, STPOOL )

                     CALL CHKOUT ( 'PCKSFS' )
                     RETURN

                  END IF

C                 Get the next node.  We avoid LNKNXT here in order
C                 to speed up the operation.
C
                  CRFLBG = STPOOL ( FORWRD, CRFLBG )

               END DO

C
C              Return the sub-list to the segment table pool.
C              CRFLBG at this point is the negative of the list head.
C              The list tail is (by the spec of the SPICELIB doubly
C              linked list routines) the negative of the predecessor
C              of the head.
C
C              Note the list is always non-empty.
C
               TAIL = -LNKPRV( -CRFLBG, STPOOL )

               CALL LNKFSL ( P, TAIL, STPOOL )

            END IF

C
C           Search through the remaining files without buffering.
C           Recall that a search is already in progress and that a
C           segment is currently under consideration (FND = .TRUE.).
C
            DO WHILE ( FINDEX .GT. 0 )

               DO WHILE ( FND )
C
C                 Each segment found contributes to the expense of the
C                 re-use interval.
C
                  BTRUEX(BINDEX) = BTRUEX(BINDEX) + 1

                  CALL DAFGS ( DESCR )
                  CALL DAFUS ( DESCR, ND, NI, DCD, ICD )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( 'PCKSFS' )
                     RETURN
                  END IF

                  IF ( BODY .EQ. ICD(1) ) THEN
C
C                    This is a segment for the body of interest.
C                    Update the re-use interval for this body.
C
                     IF ( ET .GT. DCD(2) ) THEN
C
C                       ET is to the right of the coverage interval
C                       of this segment.
C
                        BTLB(BINDEX) = MAX( BTLB(BINDEX), DCD(2) )


                     ELSE IF ( ET .LT. DCD(1) ) THEN
C
C                       ET is to the left of the coverage interval
C                       of this segment.
C
                        BTUB(BINDEX) = MIN( BTUB(BINDEX), DCD(1) )

                     ELSE
C
C                       The segment coverage interval includes ET.
C
                        CALL DAFGN ( IDENT )

                        IF ( FAILED() ) THEN
                           CALL CHKOUT ( 'PCKSFS' )
                           RETURN
                        END IF

                        HANDLE = FTHAN( FINDEX )
                        FOUND  = .TRUE.

C
C                       Set the re-use interval for the current body.
C
                        BTLB(BINDEX) = MAX( BTLB(BINDEX), DCD(1) )
                        BTUB(BINDEX) = MIN( BTUB(BINDEX), DCD(2) )

C
C                       Save the output items, in case this
C                       segment may satisfy the next request.
C
                        BTPRVH(BINDEX) =  HANDLE
                        BTPRVI(BINDEX) =  IDENT
                        CALL MOVED ( DESCR, DSCSIZ, BTPRVD(1,BINDEX) )
                        BTCHKP(BINDEX) =  .TRUE.

C
C                       Update the expense of the list to reflect
C                       the cost of locating this segment.
C
                        BTEXP(BINDEX) = BTEXP(BINDEX) + BTRUEX(BINDEX)

                        CALL CHKOUT ( 'PCKSFS' )
                        RETURN

                     END IF

                  END IF

                  CALL DAFFPA ( FND )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( 'PCKSFS' )
                     RETURN
                  END IF

               END DO

C
C              Try the next oldest file.
C
               FINDEX = FINDEX -1

               IF ( FINDEX .GT. 0 ) THEN

                  CALL DAFBBS ( FTHAN( FINDEX ) )
                  CALL DAFFPA ( FND             )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( 'PCKSFS' )
                     RETURN
                  END IF

               END IF

            END DO

C
C           If you get to here, sorry.
C
            BTRUEX(BINDEX) = 0
            STATUS         = 'HOPELESS'

C
C        When a task is suspended, the current activity is placed on
C        a stack, to be restored later. Two levels are provided, since
C        some interrupts can be interrupted by others.
C
         ELSE IF ( STATUS .EQ. 'SUSPEND' ) THEN

            TOP          = TOP + 1
            STACK( TOP ) = DOING
            STATUS       = URGENT

         ELSE IF ( STATUS .EQ. 'RESUME' ) THEN
C
C           Pop the status stack.
C
            STATUS = STACK( TOP )
            TOP    = TOP - 1

         END IF

      END DO

C
C     If we didn't find a segment, don't attempt to use saved
C     outputs from a previous call.  BINDEX will always be set
C     at this point.  Also, zero out the expense of the re-use
C     interval.
C
      IF ( BINDEX .GT. 0 ) THEN

         BTCHKP(BINDEX) = .FALSE.
         BTRUEX(BINDEX) =  0

      END IF


      CALL CHKOUT ( 'PCKSFS' )
      RETURN
      END

