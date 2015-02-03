 
C$Procedure      CKBSR ( C-kernel, buffer segments for readers )
 
      SUBROUTINE CKBSR ( FNAME,
     .                   HANDLE,
     .                   INST,
     .                   SCLKDP,
     .                   TOL,
     .                   NEEDAV,
     .                   DESCR,
     .                   SEGID,
     .                   FOUND   )

      IMPLICIT NONE
 
C$ Abstract
C
C     Load and unload files for use by the readers. Buffer segments 
C     for readers.
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
C     CK
C     DAF
C
C$ Keywords
C
C     POINTING
C
C$ Declarations

 
      CHARACTER*(*)         FNAME
      INTEGER               HANDLE
      INTEGER               INST
      DOUBLE PRECISION      SCLKDP
      DOUBLE PRECISION      TOL
      LOGICAL               NEEDAV
      DOUBLE PRECISION      DESCR    ( * )
      CHARACTER*(*)         SEGID
      LOGICAL               FOUND
 
      INTEGER               FTSIZE
      PARAMETER           ( FTSIZE =  5000 )
 
      INTEGER               ITSIZE
      PARAMETER           ( ITSIZE =  100 )
  
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL =   -5 )

      INTEGER               STSIZE
      PARAMETER           ( STSIZE = 100000 )
 
C$ Brief_I/O
C
C     Variable  I/O  Entry points
C     --------  ---  --------------------------------------------------
C     FNAME      I   CKLPF
C     HANDLE    I,O  CKLPF, CKUPF, CKSNS
C     INST       I   CKBSS
C     SCLKDP     I   CKBSS
C     TOL        I   CKBSS
C     NEEDAV     I   CKBSS
C     DESCR      O   CKSNS
C     SEGID      O   CKSNS
C     FOUND      O   CKSNS
C
C$ Detailed_Input
C
C     FNAME      is the name of a binary C-kernel file to be loaded.
C
C     HANDLE     on input is the handle of a binary C-kernel file to be
C                unloaded.
C
C
C     The purpose of entry points CKBSS and CKSNS is to search for
C     segments in CK files matching certain criteria. The four
C     quantities below establish these search criteria.
C
C
C     INST       is the NAIF ID of an instrument.
C
C     SCLKDP     is an encoded spacecraft clock time.
C
C     TOL        is a time tolerance, measured in the same units as
C                encoded spacecraft clock.
C
C     NEEDAV     indicates whether or not angular velocity data are
C                required.
C
C                If true, only segments containing pointing and angular
C                velocity data will be checked. If false, segments
C                containing just pointing data will also be considered.
C
C
C     A segment matches the CKBSS/CKSNS search criteria when the
C     following statements are true.
C
C        1) INST matches the instrument number for the segment.
C
C        2) The time interval [SCLKDP - TOL, SCLKDP + TOL] intersects
C           the time interval of the segment.
C
C        3) If angular velocity data are required, as indicated by
C           NEEDAV, the segment contains angular velocity data.
C
C
C$ Detailed_Output
C
C     HANDLE     on output is the handle of the C-kernel file
C                containing a located segment.
C
C     DESCR      is the packed descriptor of a located segment.
C
C     SEGID      is the identifier of a located segment.
C
C     FOUND      indicates whether a requested segment was found or not.
C
C$ Parameters
C
C     FTSIZE     is the maximum number of pointing files that can
C                be loaded by CKLPF at any given time for use by the
C                readers.
C
C     ITSIZE     is the maximum number of instruments whose segments
C                are buffered by CKSNS.
C
C     STSIZE     is the maximum number of segments that can be buffered
C                at any given time by CKSNS.
C
C$ Exceptions
C
C     1) If CKBSR is called directly, the error SPICE(CKBOGUSENTRY)
C        is signaled.
C
C     2) See entry points CKLPF, CKUPF, CKBSS, and CKSNS for exceptions
C        specific to them.
C
C$ Files
C
C     C-kernel pointing files are indicated by filename before loading
C     (see CKLPF) and handle after loading (all other places).
C
C$ Particulars
C
C     CKBSR serves as an umbrella, allowing data to be shared by its
C     entry points:
C
C        CKLPF       Load pointing file.
C        CKUPF       Unload pointing file.
C        CKBSS       Begin search for segment.
C        CKSNS       Select next segment.
C
C     Before a file can be read by the C-kernel readers, it must be
C     loaded by CKLPF, which among other things load the file into
C     the DAF subsystem.
C
C     Up to FTSIZE files may be loaded for use simultaneously, and a 
C     file only has to be loaded once to become a potential search 
C     target for any number of subsequent reads.
C
C     Once a C-kernel has been loaded, it is assigned a file
C     handle, which is used to keep track of the file internally, and
C     which is used by the calling program to refer to the file in all
C     subsequent calls to CK routines.
C
C     A file may be removed from the list of files for potential
C     searching by unloading it via a call to CKUPF.
C
C     CKBSS and CKSNS are used together to search through loaded files
C     for segments.
C
C     CKBSS sets up the search. You tell it the instrument and time
C     that you are interested in, and whether you require segments
C     containing angular velocity data.
C
C     CKSNS finds segments matching the search criteria set up by
C     CKBSS. Last-loaded files get searched first, and individual files
C     are searched backwards.
C
C     When an applicable segment is found, CKSNS returns that segment's
C     descriptor and identifier, along with the handle of the file
C     containing the segment.
C
C     Subsequent calls to CKSNS continue the search, picking up where
C     the previous call to this routine left off.
C
C     CKSNS uses information on loaded files to manage a buffer
C     of saved segment descriptors and identifiers. The buffer is used
C     to speed up access time by minimizing file reads.
C
C$ Examples
C
C     Suppose that pointing data for the Voyager 2 narrow angle camera
C     for a certain interval of time are contained in three separate
C     files:  ORIGINAL.CK contains an original complete set of pointing
C     data and UPDATE_1.CK and UPDATE_2.CK contain two separate pointing
C     updates for certain pictures in the same time period.
C
C     In the following example, pointing from the C-kernel is extracted
C     in two different ways for the purpose of comparing the two
C     updates:
C
C     First, the original pointing file and one of the update files are
C     both loaded and pointing is retrieved for all of the pictures.
C     The update file is searched through first, and if no data for the
C     desired picture is located, then the original file provides the
C     requested pointing.
C
C     Then, the first update file is unloaded, the second update file
C     is loaded, and the same search is performed, as above.
C
C     Throughout the two searches, a ficticious non-SPICELIB routine
C     named WRTABL writes an entry into a table that contains
C     the pointing of the camera and the file from which the pointing
C     came, if such pointing was found.  WRERR, another ficticious,
C     non-SPICELIB routine writes an error message if no such pointing
C     was found.
C
C     It is assumed that an array (FDS) exists that contains character
C     representations of the spacecraft clock time for each picture,
C     and that there are NPICS pictures.
C
C           INTEGER               NPICS
C           PARAMETER           ( NPICS = 100 )
C
C           INTEGER               HANDLE
C           INTEGER               HNORIG
C           INTEGER               HUPDT
C           INTEGER               UPDATE
C           INTEGER               INST
C           INTEGER               SC
C           INTEGER               I
C
C           DOUBLE PRECISION      DESCR    (    5 )
C           DOUBLE PRECISION      SCLKDP
C           DOUBLE PRECISION      TOL
C           DOUBLE PRECISION      CLKOUT
C           DOUBLE PRECISION      CMAT     ( 3, 3 )
C           DOUBLE PRECISION      AV       (    3 )
C
C           CHARACTER*(12)        FDS      ( NPICS )
C           CHARACTER*(25)        FNAME
C           CHARACTER*(40)        SEGID
C           CHARACTER*(12)        OUTFDS
C           CHARACTER*(12)        TOLSTR
C           CHARACTER*(25)        UDFILE   (    2 )
C
C           LOGICAL               PFOUND
C           LOGICAL               SFOUND
C           LOGICAL               NEEDAV
C
C
C           UDFILE ( 1 ) = 'UPDATE_1.CK'
C           UDFILE ( 2 ) = 'UPDATE_2.CK'
C
C     C
C     C     The NAIF integer ID codes for the Voyager 2 spacecraft
C     C     and the narrow angle camera on Voyager 2 are -32 and
C     C     -32001, respectively.
C     C
C           SC           = -32
C           INST         = -32001
C     C
C     C     Load the Voyager SCLK file.
C     C
C           CALL FURNSH ( 'VG2_SCLK.TSC' )
C
C     C
C     C     Allow a time tolerance of 400 line counts.  Convert
C     C     the tolerance to 'ticks', the units of encoded spacecraft
C     C     clock time.
C     C
C           TOLSTR  = '0:00:400'
C           CALL SCTIKS ( SC, TOLSTR, TOL )
C
C     C
C     C     Don't care about angular velocity data.
C     C
C           NEEDAV = .FALSE.
C
C     C
C     C     Load the original CK file first.
C     C
C           CALL CKLPF ( 'ORIGINAL.CK', HNORIG )
C
C
C           DO UPDATE = 1, 2
C     C
C     C        Load the update file.  Last-loaded files get searched
C     C        first, so the update file will be searched before
C     C        the original file.
C     C
C              CALL CKLPF ( UDFILE ( UPDATE ), HUPDT )
C
C              DO I = 1, NPICS
C
C     C
C     C           Encode the character string representation of
C     C           spacecraft clock time in FDS.
C     C
C                 CALL SCENCD ( SC, FDS( I ), SCLKDP )
C
C     C
C     C           Begin a search for this instrument and time, and
C     C           get the first applicable segment.
C     C
C                 CALL CKBSS ( INST,   SCLKDP, TOL,   NEEDAV  )
C                 CALL CKSNS ( HANDLE, DESCR,  SEGID, SFOUND  )
C
C     C
C     C           Keep trying candidate segments until a segment can
C     C           produce a pointing instance within the specified
C     C           time tolerance of SCLKDP, the encoded spacecraft
C     C           clock time.
C     C
C                 PFOUND = .FALSE.
C
C                 DO WHILE (  SFOUND .AND. ( .NOT. PFOUND )  )
C
C                    CALL CKPFS ( HANDLE, DESCR, SCLKDP, TOL,  NEEDAV,
C          .                      CMAT,   AV,    CLKOUT, PFOUND       )
C
C                    IF ( PFOUND ) THEN
C
C     C                 Get the name of the file from whence the
C     C                 pointing instance came, decode the spacecraft
C     C                 clock time associated with the instance, and
C     C                 write the results to the table.
C     C
C                       CALL DAFHFN ( HANDLE, FNAME          )
C                       CALL SCDECD ( SC,     CLKOUT, OUTFDS )
C
C                       CALL WRTABL ( FDS( I ), OUTFDS, CMAT, FNAME )
C
C                    ELSE
C     C
C     C                 Look for another candidate segment.
C     C
C                       CALL CKSNS ( HANDLE, DESCR, SEGID, SFOUND )
C
C                    END IF
C
C                 END DO
C
C                 IF ( .NOT. PFOUND ) THEN
C
C                    CALL WRERR ( FDS( I ) )
C
C                 END IF
C
C              END DO
C
C     C
C     C        Unload the update file.  The original file stays loaded.
C     C
C              CALL CKUPF  ( HUPDT )
C
C           END DO
C
C$ Restrictions
C
C     1) If Fortran I/O errors occur while searching a loaded CK
C        file, the internal state of this suite of routines may
C        be corrupted.  It may be possible to correct the state
C        by unloading the pertinent CK files and then re-loading
C        them.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     J.M. Lynch     (JPL)
C     J.E. McLean    (JPL)
C     B.V. Semenov   (JPL)
C     M.J. Spencer   (JPL)
C     R.E. Thurman   (JPL)
C     I.M. Underwood (JPL)
C
C$ Version
C
C-    SPICELIB Version 5.0.0, 17-MAR-2014 (NJB)
C
C        Updated segment pool initialization condition in entry
C        point CKLPF so that the pool is initialized only if the file
C        table is empty.
C     
C-    SPICELIB Version 4.6.0, 13-JUN-2013 (BVS)
C
C        Increased FTSIZE (from 1000 to 5000).
C
C        Increased STSIZE (from 50000 to 100000).
C
C-    SPICELIB Version 4.5.0, 24-FEB-2011 (NJB)
C
C        Bug fixes: 
C
C          1) In the CKSNS 'MAKE ROOM' state, when the
C             suspended activity is 'ADD TO FRONT' and no segment table
C             room is available, the instrument table's pointer to the
C             current segment list is now set to null. Previously the
C             pointer was allowed to go stale.
C
C          2) In CKUPF, the null pointer test used to determine
C             eligibility for segment list deletion now uses the .LE.
C             operator instead of the .EQ. operator.
C
C
C-    SPICELIB Version 4.4.0, 07-APR-2010 (NJB)
C
C        Increased STSIZE to 50000.
C
C-    SPICELIB Version 4.3.1, 28-FEB-2008 (BVS) 
C
C        Corrected the contents of the Required_Reading section
C        of the CKHAVE entry point header.
C
C-    SPICELIB Version 4.3.0, 23-OCT-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments in
C        MOVED calls in entry points CKUPF and CKSNS.  Replaced header
C        reference to LDPOOL with reference to FURNSH.
C
C-    SPICELIB Version 4.2.0, 30-DEC-2004 (NJB)
C
C        Increased STSIZE to 20000.
C
C-    SPICELIB Version 4.1.0, 20-NOV-2001 (NJB)
C
C        Bug fixes:  
C          
C           1) When a segment list is freed because the entire list 
C              is contributed by a single CK file, and the list is
C              too large to be buffered, the corresponding instrument
C              table pointer is now set to null.  
C
C           2) An algorithm change has eliminated a bug caused by not 
C              updating the current instrument index when instrument
C              table entries having empty segment lists were compressed
C              out of the instrument table.  Previously the instrument
C              table pointer IINDEX could go stale after the
C              compression.
C
C           3) When a already loaded kernel is re-opened with DAFOPR, 
C              it now has its link count reset to 1 via a call to 
C              DAFCLS.
C
C           4) The load routine CKLPF now resets all file numbers when 
C              the next file number reaches INTMAX()-1, thereby 
C              avoiding arithmetic overflow.  
C
C           5) The unload routine CKUPF now calls RETURN() on entry and
C              returns if so directed.
C
C           6) In CKSNS, DAF calls are followed by tests of FAILED()
C              in order to ensure that the main state loop terminates.
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
C        The instrument table size has been increased to 100 in order 
C        to decrease the chance of thrashing due to swapping segment
C        lists for different bodies.
C     
C        Various small updates and corrections were made to the 
C        comments throughout the file.
C
C-    SPICELIB Version 4.0.0, 17-FEB-2000 (WLT)
C
C        Added the Entry point CKHAVE
C
C-    SPICELIB Version 3.0.0, 03-MAR-1999 (WLT)
C
C        The parameter STSIZE was increased from 1000 to 4000 to
C        avoid the buffering error that exists in the CKBSR.
C
C-    SPICELIB Version 2.0.0, 25-NOV-1992 (JML)
C
C     1) When loading a file, CKLPF now checks if the file table is
C        full only after determining that the file is not currently
C        loaded. Previously, if the file table was full and an attempt
C        was made to reload a file, an error was signaled. A new
C        exception was added as a result of this change.
C
C     2) A bug in the way that CKLPF and CKUPF clean up the instrument
C        tables after a file is unloaded was fixed.
C
C     3) Variable declarations were added to the example program
C        so that it can now be compiled.
C
C     4) The length of the elements in the array of segment
C        indentifiers ( STIDNT ) was changed from 56 to 40.
C
C-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.1.0, 01-NOV-1990 (JML)
C
C        An intial value was assigned to the variable STATUS so
C        that an error will be signaled if CKSNS is called
C        without CKBSS ever having been called to initiate the
C        search.
C
C-    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) (IMU)
C
C-&
 
C$ Index_Entries
C
C     buffer ck segments for readers
C
C-&
 
 
 
 
C$ Revisions
C
C-    SPICELIB Version 4.5.0, 24-FEB-2011 (NJB)
C
C        Bug fixes: 
C
C          1) In the CKSNS 'MAKE ROOM' state, when the
C             suspended activity is 'ADD TO FRONT' and no segment table
C             room is available, the instrument table's pointer to the
C             current segment list is now set to null. Previously the
C             pointer was allowed to go stale.
C
C          2) In CKUPF, the null pointer test used to determine
C             eligibility for segment list deletion now uses the .LE.
C             operator instead of the .EQ. operator.
C
C
C-    SPICELIB Version 4.3.0, 23-OCT-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments in
C        MOVED calls in entry points CKUPF and CKSNS.  Replaced header
C        reference to LDPOOL with reference to FURNSH.
C
C-    SPICELIB Version 4.2.0, 30-DEC-2004 (NJB)
C
C        Increased STSIZE to 20000.
C
C-    SPICELIB Version 4.1.0, 20-NOV-2001 (NJB)
C
C        Bug fixes:  
C          
C           1) When a segment list is freed because the entire list 
C              is contributed by a single CK file, and the list is
C              too large to be buffered, the corresponding instrument
C              table pointer is now set to null.  
C
C           2) An algorithm change has eliminated a bug caused by not 
C              updating the current instrument index when instrument
C              table entries having empty segment lists were compressed 
C              out of the instrument.  Previously the instrument table 
C              pointer IINDEX could go stale after the compression.
C
C           3) When a already loaded kernel is re-opened with DAFOPR, 
C              it now has its link count reset to 1 via a call to 
C              DAFCLS.
C
C           4) The load routine CKLPF now resets all file numbers when 
C              the next file number reaches INTMAX()-1, thereby 
C              avoiding arithmetic overflow.  
C
C           5) The unload routine CKUPF now calls RETURN() on entry and
C              returns if so directed.
C
C           6) In CKSNS, DAF calls are followed by tests of FAILED()
C              in order to ensure that the main state loop terminates.
C
C        The "re-use interval" feature was introduced to improve speed 
C        in the case where repeated, consecutive requests are satisified
C        by the same segment.  For each instrument, the associated 
C        re-use interval marks the time interval containing the previous
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
C        The instrument table size has been increased to 100 in order 
C        to decrease the chance of thrashing due to swapping segment
C        lists for different instruments.
C     
C        Various small updates and corrections were made to the 
C        comments throughout the file.
C
C        In order to simplify the source code, the in-line singly
C        linked list implementation of the segment table has been
C        replaced by an implementation relying on the SPICELIB 
C        doubly linked list routines.
C
C
C-    SPICELIB Version 2.0.0, 25-NOV-1992 (JML)
C
C     1) When loading a file, CKLPF now checks if the file table is
C        full only after determining that the file is not currently
C        loaded. Previously, if the file table was full and an attempt
C        was made to reload a file, an error was signaled. A new
C        exception was added as a result of this change.
C
C     2) A bug in the way that CKLPF and CKUPF clean up the instrument
C        tables after a file is unloaded was fixed.
C
C     3) Variable declarations were added to the example program
C        so that it can now be compiled.
C
C     4) The length of the elements in the array of segment
C        indentifiers ( STIDNT ) was changed from 56 to 40.
C
C-    SPICELIB Version 1.1.0, 01-NOV-1990 (JML)
C
C        An intial value was assigned to the variable STATUS so
C        that an error will be signaled if CKSNS is called
C        without CKBSS ever having been called to initiate the
C        search.
C
C
C-    Beta Version 1.1.0, 28-AUG-1990 (MJS) (JEM)
C
C        The following changes were made as a result of the
C        NAIF CK Code and Documentation Review:
C
C           1) The variable SCLK  was changed to SCLKDP.
C           2) The variable IDENT was changed to SEGID.
C           3) The parameterized values for FTSIZE and ITSIZE were
C               increased from 5 to 20.
C           4) The paramterized value for STSIZE was increased from 100
C              to 1000.
C           5) The local variables INTDES and DPDES were changed to
C              ICD and DCD.
C           6) The extended SAVE statement was broken in to single
C              SAVE statements.
C           7) Header and internal documentation was corrected and
C              updated.
C
C-    Beta Version 1.0.0, 14-MAR-1990 (RET) (IMU)
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
C        ND         is the number of double precision components in an
C                   unpacked C-kernel descriptor.
C
C        NI         is the number of integer components in an unpacked
C                   C-kernel descriptor.
C
C        DSCSIZ     is the number of components in a packed C-kernel
C                   descriptor.  All DAF summaries have this formulaic
C                   relationship between the number of its integer and
C                   double precision components and the number of packed
C                   components.
C
 
      INTEGER               ND
      PARAMETER           ( ND     = 2 )
 
      INTEGER               NI
      PARAMETER           ( NI     = 6 )
  
      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ = ND + (NI + 1)/2 )

      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN = 40 )

      INTEGER               SLEN
      PARAMETER           ( SLEN   = 40 )
 
C
C     Constants used in the doubly linked list structure:
C     
      INTEGER               FORWRD
      PARAMETER           ( FORWRD  = 1 )
 
      INTEGER               BCKWRD
      PARAMETER           ( BCKWRD  = 2 )

      INTEGER               FREE
      PARAMETER           ( FREE    = 0 )


C
C     Local variables
C
 
C
C     The file table contains the handle and file number of each file
C     that has been loaded for use with the CK readers. File
C     numbers begin at one, and are incremented until they reach a 
C     value of INTMAX() - 1, at which point they are mapped to the
C     range 1:NFT, where NFT is the number of loaded CK files.
C
C     A file number is similar to a file handle, but it is assigned
C     and used exclusively by this module. The purpose of file numbers
C     is to keep track of the order in which files are loaded and the
C     order in which they are searched.
C
C     All names begin with FT.
C
C        HAN      Handle
C        NUM      File number
C
C     NFT is the number of currently loaded CK files. NEXT is
C     incremented whenever a new file is loaded to give the file
C     number for that file. FINDEX is the index of whatever file is
C     of current interest.
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
C     The instrument table contains the beginning of the list of the
C     stored segments for each spacecraft/instrument pair, and the
C     expense at which that list was constructed. (The expense of an
C     instrument list is the number of segment descriptors examined
C     during the construction of the list.) It also contains the
C     highest and lowest file numbers searched during the construction
C     of the list.
C
C     For each instrument, the time bounds of the "re-use interval" 
C     of the last segment found are stored.  This interval is the 
C     maximal interval containing the epoch of the last request for 
C     data for this instrument, such that the interval is not masked 
C     by higher-priority segments.  The handle, segment descriptor, 
C     and segment identifier returned on the last request are also 
C     stored.
C     
C     The reuse-interval is computed without regard to presence of
C     angular velocity:  all segments seen while searching for
C     a segment satisfying a request are used to define the bounds
C     of the re-use interval.
C
C     Re-use intervals are defined on the *first* search following
C     a setup call to CKBSS.  If a search is resumed (multiple calls
C     to CKSNS are made consecutively), the re-use interval becomes
C     invalid after the first CKSNS call. 
C
C     All names begin with IT.
C
C        INS      Spacecraft/instrument number
C        EXP      Expense
C        HFS      Highest file (number) searched
C        LFS      Lowest  file (number) searched
C        BEG      Beginning of segment list
C        LB       Lower bound of effective coverage interval of
C                 previous segment returned.
C        UB       Upper bound of effective coverage interval of
C                 previous segment returned.
C        PRVD     Previous descriptor.
C        PRVF     Previous descriptor angular velocity flag.  Angular
C                 velocity is present when ITPRVF is non-zero.
C        PRVI     Previous segment identifier returned.
C        PRVH     Previous handle returned.
C        CHKP     Logical indicating that previous segment should
C                 be checked to see whether it satisfies a request.
C        RUEX     Expense of the re-use interval.
C
C     NIT is the number of instruments for which segments are currently
C     being stored in the table. IINDEX is the index of whatever
C     instrument is of current interest at any given time.
C
C     New instruments are added at the end of the table. As instruments
C     are removed, the last instrument is moved forward to take up the
C     slack. This keeps the entries in the table contiguous.
C
      CHARACTER*(SIDLEN)    ITPRVI   ( ITSIZE )

      DOUBLE PRECISION      ITPRVD   ( DSCSIZ, ITSIZE )
      DOUBLE PRECISION      ITLB     ( ITSIZE )
      DOUBLE PRECISION      ITUB     ( ITSIZE )

      INTEGER               IINDEX
      INTEGER               ITBEG    ( ITSIZE )
      INTEGER               ITEXP    ( ITSIZE )
      INTEGER               ITHFS    ( ITSIZE )
      INTEGER               ITINS    ( ITSIZE )
      INTEGER               ITLFS    ( ITSIZE )
      INTEGER               ITPRVF   ( ITSIZE )
      INTEGER               ITPRVH   ( ITSIZE )
      INTEGER               ITRUEX   ( ITSIZE )
      INTEGER               NIT
 
      LOGICAL               ITCHKP   ( ITSIZE )

C
C     The segment table contains the handle, descriptor, and identifier
C     for each segment that has been found so far.
C
C     The segment table is implemented as a set of arrays indexed by
C     a SPICE doubly linked list structure.  For each instrument
C     in the instrument table, there is a segment table list; each 
C     node of a list points to data associated with a segment.  In 
C     each list, the head node corresponds to the highest-priority 
C     segment in that list, and segment priority decreases in the 
C     forward direction.
C
C     All names begin with ST.
C
C        IDNT     Identifier
C        DCD      Double Precision component of descriptor
C        HAN      Handle
C        ICD      Integer component of descriptor
C        POOL     Doubly linked list pool.
C
C     New segments are added to the front or end of an instrument list
C     as appropriate, according to the rules spelled out under
C     entry point CKSNS.
C
      CHARACTER*(SIDLEN)    STIDNT (              STSIZE )

      DOUBLE PRECISION      STDCD  ( ND,          STSIZE )

      INTEGER               STHAN  (              STSIZE )
      INTEGER               STICD  ( NI,          STSIZE )
      INTEGER               STPOOL ( 2,  LBPOOL : STSIZE )
 
C
C     Other local variables
C 
      CHARACTER*(SLEN)      DOING
      CHARACTER*(SLEN)      STACK    ( 2 )
      CHARACTER*(SLEN)      STATUS
      CHARACTER*(SLEN)      URGENT

      DOUBLE PRECISION      ALPHA
      DOUBLE PRECISION      DCD    ( ND )
      DOUBLE PRECISION      OMEGA
      DOUBLE PRECISION      REQT
      DOUBLE PRECISION      SAVTOL
 
      INTEGER               CHEAP
      INTEGER               COST
      INTEGER               HEAD
      INTEGER               I
      INTEGER               ICD   ( NI )
      INTEGER               J
      INTEGER               MINEXP
      INTEGER               NEW
      INTEGER               NXTSEG
      INTEGER               P
      INTEGER               SAVEP
      INTEGER               SCINST
      INTEGER               SLBEG
      INTEGER               TAIL
      INTEGER               TOP
  
      LOGICAL               AVNEED
      LOGICAL               FND
      LOGICAL               FNDHAN
      LOGICAL               FRESUB
      LOGICAL               NEWSCH
 
C
C     Saved variables
C
      SAVE                  ALPHA
      SAVE                  AVNEED
      SAVE                  FINDEX
      SAVE                  FND
      SAVE                  FRESUB
      SAVE                  FTHAN
      SAVE                  FTNUM
      SAVE                  IINDEX
      SAVE                  ITBEG
      SAVE                  ITCHKP
      SAVE                  ITEXP
      SAVE                  ITHFS
      SAVE                  ITINS
      SAVE                  ITLB
      SAVE                  ITLFS
      SAVE                  ITPRVD
      SAVE                  ITPRVF
      SAVE                  ITPRVH
      SAVE                  ITPRVI
      SAVE                  ITUB
      SAVE                  ITRUEX
      SAVE                  NEXT
      SAVE                  NEWSCH
      SAVE                  NFT
      SAVE                  NIT
      SAVE                  OMEGA
      SAVE                  REQT
      SAVE                  SAVEP
      SAVE                  SAVTOL
      SAVE                  SCINST
      SAVE                  SLBEG
      SAVE                  STATUS
      SAVE                  STDCD
      SAVE                  STHAN
      SAVE                  STICD
      SAVE                  STIDNT
      SAVE                  STPOOL
      SAVE                  TOP
 
C
C     Initial values
C
      DATA                  FRESUB    / .FALSE.       /
      DATA                  NFT       / 0             /
      DATA                  NIT       / 0             /
      DATA                  NEXT      / 0             /
      DATA                  SAVEP     / 0             /
      DATA                  SAVTOL    / 0.D0          /
      DATA                  STATUS    / 'BOGUS ENTRY' /
 
 
C
C     Nobody has any business calling CKBSR directly.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'CKBSR' )
      CALL SIGERR ( 'SPICE(CKBOGUSENTRY)' )
      CALL CHKOUT ( 'CKBSR' )
 
      RETURN
 
 
C$Procedure CKLPF ( C-kernel, load pointing file )
 
      ENTRY CKLPF ( FNAME, HANDLE )
 
C$ Abstract
C
C     Load a CK pointing file for use by the CK readers.  Return that
C     file's handle, to be used by other CK routines to refer to the
C     file.
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
C     CK
C     DAF
C
C$ Keywords
C
C     POINTING
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
C     FNAME      I   Name of the CK file to be loaded.
C     HANDLE     O   Loaded file's handle.
C     FTSIZE     P   Maximum number of loaded CK files.
C
C$ Detailed_Input
C
C     FNAME      is the name of a C-kernel file to be loaded.
C
C$ Detailed_Output
C
C     HANDLE     is an integer handle assigned to the file upon loading.
C                Almost every other CK routine will subsequently use
C                this number to refer to the file.
C
C$ Parameters
C
C     FTSIZE     is the maximum number of CK files that may 
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
C        room to load another file, the error SPICE(CKTOOMANYFILES)
C        signaled.  The current setting of FTSIZE does not allow this
C        situation to arise:  the DAF system will trap the error 
C        before this routine has the chance.
C
C     3) If the file specified by FNAME can not be opened, an error
C        is signaled by a routine that this routine calls.
C
C     4) If the file specified by FNAME has already been loaded,
C        it will become the "last-loaded" file.  The readers
C        search the last-loaded file first.
C
C$ Files
C
C     The C-kernel file specified by FNAME is loaded.  The file is
C     assigned an integer handle by CKLPF.  Other CK routines will refer
C     to this file by its handle.
C
C$ Particulars
C
C     See Particulars above, in CKBSR.
C
C     If there is room for a new file, CKLPF opens the file for
C     reading.  This routine must be called prior to a call to CKGP or
C     CKGPAV.
C
C     CK readers search files loaded with CKLPF in the reverse order
C     in which they were loaded.  That is, last-loaded files are
C     searched first.
C
C$ Examples
C
C     See the Example above, in CKBSR.
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
C     N.J. Bachman   (JPL)
C     J.M. Lynch     (JPL)
C     J.E. McLean    (JPL)
C     M.J. Spencer   (JPL)
C     R.E. Thurman   (JPL)
C     I.M. Underwood (JPL)
C
C$ Version
C
C-    SPICELIB Version 5.0.0, 17-MAR-2014 (NJB)
C
C        Updated segment pool initialization condition in entry
C        point CKLPF so that the pool is initialized only if the file
C        table is empty.
C     
C-    SPICELIB Version 4.1.0, 20-NOV-2001 (NJB)
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
C-    SPICELIB Version 4.0.0, 17-FEB-2000 (WLT)
C
C        Added the Entry point CKHAVE
C
C-    SPICELIB Version 3.0.0, 03-MAR-1999 (WLT)
C
C        The parameter STSIZE was increased from 1000 to 4000 to
C        avoid the buffering error that exists in the CKBSR.
C
C-    SPICELIB Version 2.0.0, 25-NOV-1992 (JML)
C
C     1) When loading a file, CKLPF now checks if the file table is
C        full only after determining that the file is not currently
C        loaded. Previously, if the file table was full and an attempt
C        was made to reload a file, an error was signaled.  A new
C        exception was added as a result of this change.
C
C     2) A bug in the way that CKLPF and CKUPF clean up the instrument
C        tables after a file is unloaded was fixed.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) (IMU)
C
C-&
 
C$ Index_Entries
C
C     load ck pointing file
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 4.1.0, 20-NOV-2001 (NJB)
C
C        Bug fixes:  
C
C        1) When a loaded kernel is opened with DAFOPR,
C           it now has its link count reset to 1 via a call to 
C           DAFCLS.
C        
C        2) This routine now resets all file numbers when 
C           the next file number reaches INTMAX()-1, thereby avoiding 
C           arithmetic overflow.  The numbers in the file table 
C           are replaced with consecutive integers in the range
C           1 : NFT, such that the ordering of the numbers is not
C           changed.  The HFS and LFS arrays are updated accordingly.
C           HFS and LFS entries that have gone stale are set to zero.
C
C        Also, the flags indicating validity of the re-use intervals
C        are set to .FALSE. here.
C
C-    SPICELIB Version 2.0.0, 25-NOV-1992 (JML)
C
C        Temp version for testing purposes.
C
C     1) When loading a file, CKLPF now checks if the file table is
C        full only after determining that the file is not currently
C        loaded. Previously, if the file table was full and an attempt
C        was made to reload a file, an error was signaled.  A new
C        exception was added as a result of this change.
C
C     2) A bug in the way that CKLPF and CKUPF clean up the instrument
C        tables after a file is unloaded was fixed.
C
C        If as the result of loading a file that was previously loaded,
C        there are no more segments buffered for a particular
C        instrument, the counter variable for the instruments is no
C        longer incremented.
C
C        The following code fragment changed:
C
C           IF ( ITBEG( I ) .EQ. 0 ) THEN
C
C              .
C              .
C              .
C              NIT = NIT - 1
C
C           END IF
C
C           I = I + 1
C
C        This is the fix:
C
C           IF ( ITBEG( I ) .EQ. 0 ) THEN
C
C              .
C              .
C              .
C              NIT = NIT - 1
C
C           ELSE
C
C              I = I + 1
C
C           END IF
C
C-    Beta Version 1.1.0, 28-AUG-1990 (MJS) (JEM)
C
C        Header documentation was updated, and error handling was
C        modified.
C
C-    Beta Version 1.0.0, 14-MAR-1990 (RET) (IMU)
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKLPF' )
      END IF 

C
C     Don't allow a search to continue after loading a file; a new
C     search should be re-started.
C
      STATUS = 'BOGUS ENTRY'

C
C     Since a current search cannot be continued at this point,
C     free the left-over partial list searched in the 
C     'CHECK PARTIAL LIST' state, if the list is present.
C
      IF ( FRESUB ) THEN
C
C        Return the partial list to the free list. 
C
         TAIL = LNKTL( SLBEG, STPOOL )
         CALL LNKFSL ( SLBEG, TAIL, STPOOL )

         FRESUB = .FALSE.

      END IF

C
C     Any time we load a file, there is a possibility that the 
C     re-use intervals are invalid because they're been superseded
C     by higher-priority data.  Since we're not going to examine
C     the loaded file, simply indicate that all of the re-use 
C     intervals are invalid.
C
      DO I = 1, NIT
         ITCHKP(I) = .FALSE.
      END DO

C
C     Nothing works unless at least one file has been loaded, so this
C     is as good a place as any to initialize the segment table pool.
C     We want to avoid unnecessary initializations, so we only
C     initialize the list when no files are loaded. It's quite possible
C     to have files loaded and an empty instrument table, so we don't
C     want to re-initialize just because there are no instrument table
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
         CALL CHKOUT ( 'CKLPF' )
         RETURN
      END IF
 
C
C     Determine if the file is already in the table.
C
      FINDEX = ISRCHI ( HANDLE, NFT, FTHAN )
  
      IF ( FINDEX .GT. 0 ) THEN
C
C        The last call we made to DAFOPR added another DAF link to
C        the CK file.  Remove this link.
C
         CALL DAFCLS ( HANDLE )

C
C        Handle is already in the table.  Remove it.
C
         NFT = NFT - 1
 
         DO I = FINDEX, NFT
            FTHAN( I ) = FTHAN( I + 1 )
            FTNUM( I ) = FTNUM( I + 1 )
         END DO
 
C
C        Unlink any segments that came from this file.
C
         I = 1
 
         DO WHILE ( I .LE. NIT )
 
            P = ITBEG( I )
 
            DO WHILE ( P .GT. 0 )
C
C              Find the successor of P, if any.
C 
               NXTSEG = LNKNXT ( P, STPOOL )
 
               IF ( STHAN( P ) .EQ. HANDLE ) THEN
C
C                 The segment corresponding to node P came from
C                 the file we're unloading.  Delete the node for
C                 P from the segment list for instrument I; if P happens
C                 to be the head node for instrument I's segment list,
C                 make the successor of P the head of the list.
C
                  CALL LNKFSL ( P, P, STPOOL )

                  IF ( P .EQ. ITBEG(I) ) THEN
                     ITBEG( I ) = NXTSEG 
                  END IF
 
               END IF 
C
C              Update P.
C 
               P = NXTSEG
 
            END DO
 
C
C           If the list for this instrument is now empty, shorten the
C           current table by one: put all the entries for the last
C           instrument in the table into the space occupied by the
C           one we've deleted.
C
            IF ( ITBEG( I ) .LE. 0 ) THEN
C
C              Because all of the re-use intervals are invalid, we need
C              not copy the saved items associated with them.  The
C              items not copied are
C
C                 ITCHKP
C                 ITLB
C                 ITPRVD
C                 ITPRVF
C                 ITPRVH
C                 ITPRVI
C                 ITRUEX
C                 ITUB
C  
               ITINS( I ) = ITINS( NIT )
               ITEXP( I ) = ITEXP( NIT )
               ITHFS( I ) = ITHFS( NIT )
               ITLFS( I ) = ITLFS( NIT )
               ITBEG( I ) = ITBEG( NIT )
 
               NIT = NIT - 1
 
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
 
            CALL DAFCLS ( HANDLE )
 
            CALL SETMSG ( 'Number of files loaded is at a maximum, ' //
     .                    'as specified by the parameter FTSIZE, '   //
     .                    'the value of which is #. You will need '  //
     .                    'to either load fewer files, or change '   //
     .                    'the parameter FTSIZE.'                     )
            CALL ERRINT ( '#', FTSIZE                                 )
            CALL SIGERR ( 'SPICE(CKTOOMANYFILES)'                     )
            CALL CHKOUT ( 'CKLPF'                                     )
            RETURN
 
         END IF
 
      END IF
 
C
C     Determine the next file number.
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
C        First update the LFS and HFS components of the instrument table
C        according to this mapping.  
C  
C        Set any instrument table entries that are lower than FTNUM(1) 
C        to zero.  
C
         DO I = 1, NIT 
C
C           Re-map the HFS table for the Ith instrument.
C
            J = ISRCHI ( ITHFS(I), NFT, FTNUM )

            IF ( J .GT. 0 ) THEN
C
C              The highest file searched for instrument I is the Jth 
C              file in the file table.
C
               ITHFS(I) = J

            ELSE
C
C              The highest file searched for instrument I is not in the
C              file table.  This occurs when the highest file searched 
C              has been unloaded.  Note that this assigment makes all 
C              files appear to be "new" when a lookup for instrument 
C              I is performed.
C
               ITHFS(I) = 0

            END IF

C
C           Re-map the LFS table for the Ith instrument.
C
            J = ISRCHI ( ITLFS(I), NFT, FTNUM )

            IF ( J .GT. 0 ) THEN
C
C              The lowest file searched for instrument I is the Jth file
C              in the file table.
C
               ITLFS(I) = J

            ELSE
C
C              The lowest file searched for instrument I is not in the 
C              file table.  This occurs when the lowest file searched 
C              has been unloaded.  Zero out both the lowest and
C              highest file searched to force reconstruction of the 
C              list.
C
               ITLFS(I) = 0
               ITHFS(I) = 0

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

C
C     Now add this file to file table.
C
      NFT        = NFT  + 1
      FTHAN(NFT) = HANDLE
      FTNUM(NFT) = NEXT


      CALL CHKOUT ( 'CKLPF' )
      RETURN
 
 


C$Procedure CKUPF ( C-kernel, Unload pointing file )
 
      ENTRY CKUPF ( HANDLE )
 
C$ Abstract
C
C     Unload a CK pointing file so that it will no longer be searched
C     by the readers.
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
C     CK
C     DAF
C
C$ Keywords
C
C     POINTING
C
C$ Declarations
C
C     INTEGER               HANDLE
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of CK file to be unloaded
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
C     See Particulars section above, in CKBSR.
C
C     Unloading a file with CKUPF removes that file from consideration
C     by the CK readers.  In doing so, it frees up space for another
C     file to be loaded.
C
C$ Examples
C
C     See the Example above, in CKBSR.
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
C     N.J. Bachman   (JPL)
C     J.M. Lynch     (JPL)
C     R.E. Thurman   (JPL)
C     I.M. Underwood (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.3.0, 24-FEB-2011 (NJB)
C
C        Bug fix: the null pointer test used to determine eligibility
C        for segment list deletion now uses the .LE. operator instead
C        of the .EQ. operator.
C
C-    SPICELIB Version 4.2.0, 08-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in MOVED call.   
C
C-    SPICELIB Version 4.1.0, 20-NOV-2001 (NJB)
C
C        Bug fixes:  
C
C        1) This routine now calls RETURN() on entry and 
C           returns if so directed.
C
C        Also, the flags indicating validity of those re-use intervals
C        whose data comes from the unloaded file are set to .FALSE.
C        
C-    SPICELIB Version 4.0.0, 17-FEB-2000 (WLT)
C
C        Added the Entry point CKHAVE
C
C-    SPICELIB Version 3.0.0, 03-MAR-1999 (WLT)
C
C        The parameter STSIZE was increased from 1000 to 4000 to
C        avoid the buffering error that exists in the CKBSR.
C
C-    SPICELIB Version 2.0.0, 25-NOV-1992 (JML)
C
C     1) A bug in the way that CKLPF and CKUPF clean up the instrument
C        tables after a file is unloaded was fixed.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) (IMU)
C
C-&
 
C$ Index_Entries
C
C     unload ck pointing file
C
C-&
 

C$ Revisions
C
C-    SPICELIB Version 4.3.0, 24-FEB-2011 (NJB)
C
C        Bug fix: the null pointer test used to determine eligibility
C        for segment list deletion now uses the .LE. operator instead
C        of the .EQ. operator.
C
C-    SPICELIB Version 4.2.0, 08-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in MOVED call.   
C
C-    SPICELIB Version 4.1.0, 20-NOV-2001 (NJB)
C
C        Bug fixes:  
C
C        1) This routine now calls RETURN() on entry and 
C           returns if so directed.
C
C        Also, the flags indicating validity of those re-use intervals
C        whose data comes from the unloaded file are set to .FALSE.
C
C-    SPICELIB Version 2.0.0, 25-NOV-1992 (JML)
C
C     1) A bug in the way that CKLPF and CKUPF clean up the instrument
C        tables after a file is unloaded was fixed.
C
C        If as the result of unloading a file there are no more
C        segments buffered for a particular instrument, the counter
C        variable for the instruments in the instrument table is no
C        longer incremented.
C
C        The following code fragment changed:
C
C           IF ( ITBEG( I ) .EQ. 0 ) THEN
C
C              .
C              .
C              .
C              NIT = NIT - 1
C
C           END IF
C
C           I = I + 1
C
C        This is the fix:
C
C           IF ( ITBEG( I ) .EQ. 0 ) THEN
C
C              .
C              .
C              .
C              NIT = NIT - 1
C
C           ELSE
C
C              I = I + 1
C
C           END IF
C
C-    Beta Version 1.0.1, 29-AUG-1990 (MJS) (JEM)
C
C        Comments were updated.
C
C-    Beta Version 1.0.0, 07-SEP-1990 (RET) (IMU)
C
C-&
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'CKUPF' )

C
C     Don't allow a search to continue after unloading a file; a new
C     search should be re-started.
C
      STATUS = 'BOGUS ENTRY'

C
C     Since a current search cannot be continued at this point,
C     free the left-over partial list searched in the 
C     'CHECK PARTIAL LIST' state, if the list is present.
C
      IF ( FRESUB ) THEN
C
C        Return the partial list to the free list. 
C
         TAIL = LNKTL( SLBEG, STPOOL )
         CALL LNKFSL ( SLBEG, TAIL, STPOOL )

         FRESUB = .FALSE.

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
         CALL CHKOUT ( 'CKUPF' )
         RETURN
      END IF
C
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
C     Check each instrument list individually. Note that the first
C     node on each list, having no predecessor, must be handled
C     specially.
C
      I = 1
 
      DO WHILE ( I .LE. NIT )
 
         P = ITBEG( I )
 
         DO WHILE ( P .GT. 0 )
 
            NXTSEG = LNKNXT ( P, STPOOL )

            IF ( STHAN(P) .EQ. HANDLE ) THEN
       
               IF ( P .EQ. ITBEG(I) ) THEN
                  ITBEG( I ) = NXTSEG
               END IF
C
C              Free this segment table entry.
C            
               CALL LNKFSL ( P, P, STPOOL )

            END IF

            P = NXTSEG
 
         END DO
 
C
C        If the list for this instrument is now empty, shorten the
C        current table by one: put all the entries for the last
C        instrument in the table into the space occupied by the
C        one we've deleted.
C
         IF ( ITBEG(I) .LE. 0 ) THEN

            IF ( I .NE. NIT ) THEN

               ITINS (I) = ITINS (NIT)
               ITEXP (I) = ITEXP (NIT)
               ITHFS (I) = ITHFS (NIT)
               ITLFS (I) = ITLFS (NIT)
               ITBEG (I) = ITBEG (NIT)
               ITLB  (I) = ITLB  (NIT)
               ITUB  (I) = ITUB  (NIT)
               ITPRVF(I) = ITPRVF(NIT)
               ITPRVH(I) = ITPRVH(NIT)
               ITPRVI(I) = ITPRVI(NIT)
               ITCHKP(I) = ITCHKP(NIT)
               ITRUEX(I) = ITRUEX(NIT)

               CALL MOVED ( ITPRVD(1,NIT), DSCSIZ, ITPRVD(1,I) )

            END IF

            NIT = NIT - 1

         ELSE

            I = I + 1

         END IF

      END DO

C
C     Any time we unload a file, we may be removing the file
C     providing data for the re-use interval for one or more 
C     instruments.  For each instrument, if the handle associated 
C     with the re-use interval happens to be that of the file 
C     we're unloading, indicate that the re-use interval is invalid.
C
      DO I = 1, NIT

         IF ( ITCHKP(I) ) THEN

            IF ( ITPRVH(I) .EQ. HANDLE ) THEN
               ITCHKP(I) = .FALSE.
            END IF

         END IF

      END DO


      CALL CHKOUT ( 'CKUPF' )
      RETURN

 
 
 
C$Procedure CKBSS ( C-kernel, begin search for segment )
 
      ENTRY CKBSS ( INST, SCLKDP, TOL, NEEDAV )
 
C$ Abstract
C
C     Initiate search through loaded files to find segments applicable
C     to the spacecraft instrument and time specified.
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
C     CK
C     DAF
C
C$ Keywords
C
C     POINTING
C
C$ Declarations
C
C     INTEGER               INST
C     DOUBLE PRECISION      SCLKDP
C     DOUBLE PRECISION      TOL
C     LOGICAL               NEEDAV
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     INST       I   Spacecraft and instrument ID.
C     SCLKDP     I   Encoded spacecraft clock time.
C     TOL        I   Time tolerance.
C     NEEDAV     I   Is there a need for angular velocity?
C
C$ Detailed_Input
C
C     CKBSS sets up a search for segments. The four quantities below
C     establish the search criteria.
C
C
C     INST       is the NAIF ID of an instrument.
C
C     SCLKDP     is an encoded spacecraft clock time.
C
C     TOL        is a time tolerance, measured in the same units as
C                encoded spacecraft clock.
C
C     NEEDAV     indicates whether or not angular velocity data is
C                required.
C
C                If true, only segments containing pointing and angular
C                velocity data will be checked. If false, segments
C                containing just pointing data will also be considered.
C
C
C     A segment matches the CKBSS/CKSNS search criteria when the
C     following statements are true.
C
C        1) INST matches the instrument number for the segment.
C
C        2) The time interval [SCLKDP - TOL, SCLKDP + TOL] intersects
C           the time interval of the segment.
C
C        3) If angular velocity data is required, as indicated by
C           NEEDAV, the segment contains angular velocity data.
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
C     1) If no files have been loaded, the error SPICE(NOLOADEDFILES)
C        is signaled.
C
C$ Files
C
C     All files loaded by CKLPF are potential search targets for
C     CKSNS.
C
C$ Particulars
C
C     CKBSS sets up a search for segments by CKSNS. It records the
C     instrument and time to be searched for, and whether to require
C     segments containing angular velocity data. If angular velocity
C     data are required, only segments containing angular velocity
C     data will be returned by CKSNS. If angular velocity data are
C     not required, segments returned by CKSNS may or may not contain
C     angular velocity data.
C
C     CKBSS determines the first task that CKSNS will have to perform
C     if it is called to get an applicable segment.
C
C$ Examples
C
C     See Examples in CKBSR.
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
C     N.J. Bachman   (JPL)
C     M.J. Spencer   (JPL)
C     J.E. McLean    (JPL)
C     R.E. Thurman   (JPL)
C     I.M. Underwood (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.1.0, 20-NOV-2001 (NJB)
C
C        Updated to support new doubly-linked list implementation:
C        partial segment list that cannot be buffered is now
C        deallocated here rather than in CKSNS.  Minor changes to
C        comments were made as well.
C
C-    SPICELIB Version 4.0.0, 17-FEB-2000 (WLT)
C
C        Added the Entry point CKHAVE
C
C-    SPICELIB Version 3.0.0, 03-MAR-1999 (WLT)
C
C        The parameter STSIZE was increased from 1000 to 4000 to
C        avoid the buffering error that exists in the CKBSR.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) (IMU)
C
C-&
 
C$ Index_Entries
C
C     begin search for ck segment
C
C-&
 
 
 
C$ Revisions
C
C-    SPICELIB Version 4.1.0, 20-NOV-2001 (NJB)
C
C        Updated to support new doubly-linked list implementation:
C        partial segment list that cannot be buffered is now
C        deallocated here rather than in CKSNS.  Minor changes to
C        comments were made as well.
C
C-    Beta Version 1.1.0, 28-AUG-1990 (MJS) (JEM)
C
C        The following changes were made as a result of the
C        NAIF CK Code and Documentation Review:
C
C          1) The variable SCLK was changed to SCLKDP.
C          2) Header documentation was updated.
C
C-    Beta Version 1.0.0, 20-APR-1990 (RET) (IMU)
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKBSS' )
      END IF

C
C     If we're starting a new search after passing through the 
C     'CHECK PARTIAL LIST' state, free the left-over partial list
C     that was searched in that state, if necessary.
C
      IF ( FRESUB ) THEN
C
C        Return the partial list to the free list. 
C
         TAIL = LNKTL( SLBEG, STPOOL )
         CALL LNKFSL ( SLBEG, TAIL, STPOOL )

         FRESUB = .FALSE.

      END IF

C
C     Make copies of the instrument ID code and angular velocity flag.
C     Save the request time itself.
C
C     And form the endpoints of the acceptable time interval using the
C     input time and time tolerance.
C
      SCINST = INST
      ALPHA  = SCLKDP - TOL
      OMEGA  = SCLKDP + TOL
      AVNEED = NEEDAV
      REQT   = SCLKDP
      SAVTOL = TOL

C
C     There must be at least one file loaded.
C
      IF ( NFT .EQ. 0 ) THEN
 
         CALL SETMSG ( 'At least one CK file needs must be loaded ' //
     .                 'by CKLPF before beginning a search.'       )
         CALL SIGERR ( 'SPICE(NOLOADEDFILES)'                      )
         CALL CHKOUT ( 'CKBSS'                                     )
         RETURN
 
      END IF
 
C
C     The stack of suspended tasks is empty.
C
      TOP = 0

C
C     Is the instrument already in the instrument table?  The answer
C     determines what the first task for CKSNS will be.
C
      IINDEX = ISRCHI ( SCINST, NIT, ITINS )
 
      IF ( IINDEX .EQ. 0 ) THEN

         STATUS = 'NEW INSTRUMENT'

      ELSE
C
C        Set the status so that CKSNS will determine whether to check
C        the segment list, search new files, or return data from the
C        re-use interval.
C
         STATUS = '?'

      END IF

C
C     Indicate a new search has started.
C
      NEWSCH = .TRUE.

 
      CALL CHKOUT ( 'CKBSS' )
      RETURN
 
 
 
C$Procedure CKSNS ( C-kernel, Select next segment )
 
      ENTRY CKSNS ( HANDLE, DESCR, SEGID, FOUND )
 
C$ Abstract
C
C     Search through loaded files to find a segment matching the
C     requested instrument, time, and need for angular velocity.
C     Buffer segment descriptors, identifiers, and handles in the
C     process to minimize file reads.
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
C     CK
C     DAF
C
C$ Keywords
C
C     POINTING
C
C$ Declarations
C
C     INTEGER               HANDLE
C     DOUBLE PRECISION      DESCR  ( * )
C     CHARACTER*(*)         SEGID
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     O   Handle of file containing the applicable segment.
C     DESCR      O   Descriptor of the applicable segment.
C     SEGID      O   Identifier of the applicable segment.
C     FOUND      O   True if a segment was found.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     HANDLE     is an integer handle of the file containing the
C                segment matching the instrument and time
C                specifications made in the last call to CKBSS.
C
C     DESCR,
C     SEGID      are the descriptor and identifier of the segment found
C                which matches the instrument and time specifications
C                made in the last call to CKBSS.
C
C     FOUND      is true if an applicable segment was found.  False
C                otherwise.  If FOUND is false, the values of the
C                other arguments are meaningless.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If CKSNS is called without CKBSS ever having been called,
C        the error 'SPICE(CALLCKBSSFIRST)' is signaled.
C
C     2) If no segment is found that matches the search criteria,
C        FOUND is set to false, but the values of HANDLE, DESCR,
C        and SEGID will be meaningless.
C
C$ Files
C
C     All files loaded by CKLPF are potential search targets for
C     CKSNS. The files are all referred to by their integer handles.
C
C$ Particulars
C
C     CKSNS is used to locate segments based on the search criteria
C     established by the most recent call to CKBSS.  When a segment
C     is found it will have the following characteristics:
C
C        1) Its instrument will match the instrument specified in the
C           call to CKBSS.
C
C        2) Its time interval will intersect the time interval
C
C              [SCLKDP - TOL, SCLKDP + TOL],
C
C           where SCLKDP and TOL were specified in the call to CKBSS.
C
C        3) If there is a need for angular velocity data, as specified
C           by NEEDAV in the call to CKBSS, a returned segment
C           will contain angular velocity data. If there is no need
C           for such data, the returned segment may or may not contain
C           angular velocity data.
C
C     The first call to CKSNS following a call to CKBSS starts a search
C     through loaded files and either returns the first applicable
C     segment, or indicates that no segment was found.
C
C     CKSNS searches through last-loaded files first. Individual
C     files are searched backwards, so that segments that were inserted
C     last into the file get checked first.
C
C     Subsequent calls to CKSNS pick up the search exactly where the
C     previous calls left off. If a segment is not found, future calls
C     will also indicate that no segment could be found, until a new
C     search is begun.
C
C     CKSNS also buffers segment descriptors and identifiers, to
C     attempt to minimize file reads.
C
C$ Examples
C
C     See Examples in CKBSR.
C
C$ Restrictions
C
C     1) This subroutine assumes that a search has been initiated by
C        a call to CKBSS.
C
C     2) When a CK file is loaded or unloaded, a new search must 
C        be started via a call to CKBSS before this routine may
C        be called.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     J.E. McLean    (JPL)
C     M.J. Spencer   (JPL)
C     R.E. Thurman   (JPL)
C     I.M. Underwood (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.5.0, 24-FEB-2011 (NJB)
C
C        Bug fix: in the 'MAKE ROOM' state, when the suspended activity
C        is 'ADD TO FRONT' and no segment table room is available, the
C        instrument table's pointer to the current segment list is now
C        set to null. Previously the pointer was allowed to go stale.
C
C-    SPICELIB Version 4.2.0, 08-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in MOVED call.   
C
C-    SPICELIB Version 4.1.0, 20-NOV-2001 (NJB)
C
C        Bug fixes:  
C          
C           1) When a segment list is freed because the entire list 
C              is contributed by a single CK file, and the list is
C              too large to be buffered, the corresponding intrument
C              table pointer is now set to null.  
C
C           2) An algorithm change has eliminated a bug caused by not 
C              updating the current instrument index when instrument
C              table entries  having empty segment lists were compressed
C              out of the instrument table.  Previously the instrument
C              table pointer IINDEX could go stale after the 
C              compression.
C
C           3) DAF calls are now followed by tests of FAILED()
C              in order to ensure that the main state loop terminates.
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
C        The instrument table size has been increased to 100 in order to
C        decrease the chance of thrashing due to swapping segment
C        lists for different bodies.
C     
C        Various small updates and corrections were made to the 
C        comments throughout the file.
C
C-    SPICELIB Version 4.0.0, 17-FEB-2000 (WLT)
C
C        Added the Entry point CKHAVE
C
C-    SPICELIB Version 3.0.0, 03-MAR-1999 (WLT)
C
C        The parameter STSIZE was increased from 1000 to 4000 to
C        avoid the buffering error that exists in the CKBSR.
C
C-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.1.0, 01-NOV-1990 (JML)
C
C        A check on the initial value of the variable STATUS
C        was added in order to detect the situation in which
C        CKBSS was never called to initiate a search.
C
C
C-    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) (IMU)
C
C-&
 
C$ Index_Entries
C
C     select next ck segment
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 4.5.0, 24-FEB-2011 (NJB)
C
C        Bug fix: in the 'MAKE ROOM' state, when the suspended activity
C        is 'ADD TO FRONT' and no segment table room is available, the
C        instrument table's pointer to the current segment list is now
C        set to null. Previously the pointer was allowed to go stale.
C
C-    SPICELIB Version 4.2.0, 08-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in MOVED call.   
C
C-    SPICELIB Version 4.1.0, 20-NOV-2001 (NJB)
C
C        Bug fixes:  
C          
C           1) When a segment list is freed because the entire list 
C              is contributed by a single CK file, and the list is
C              too large to be buffered, the corresponding instrument
C              table pointer is now set to null.  
C
C           2) An algorithm change has eliminated a bug caused by not 
C              updating the current instrument index when instrument
C              table entries  having empty segment lists were compressed
C              out of the instrument table.  Previously the instrument
C              table pointer IINDEX could go stale after the 
C              compression.
C
C           3) DAF calls are now followed by tests of FAILED()
C              in order to ensure that the main state loop terminates.
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
C        The instrument table size has been increased to 100 in order to
C        decrease the chance of thrashing due to swapping segment
C        lists for different instruments.
C     
C        Various small updates and corrections were made to the 
C        comments throughout the file.
C
C
C-    SPICELIB Version 1.1.0, 01-NOV-1990 (JML)
C
C        A check on the initial value of the variable STATUS
C        was added in order to detect the situation in which
C        CKBSS was never called to initiate a search.
C
C
C-    Beta Version 1.1.0, 28-AUG-1990 (MJS) (JEM)
C
C        The following changes were made as a result of the
C        NAIF CK Code and Documentation Review:
C
C           1) The variable IDENT was changed to SEGID.
C           2) The local variables INTDES and DPDES were changed to
C              ICD and DCD.
C           3) Header and internal documentation was corrected and
C              updated.
C
C-    Beta Version 1.0.0, 20-APR-1990 (RET) (IMU)
C
C-&
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKSNS' )
      END IF

C
C     Nothing's been found yet.
C
      FOUND = .FALSE.

C
C     Initialize the segment list pointer to the saved value from
C     the previous pass through this routine, if any.
C
      P = SAVEP

C
C     CKSNS buffers segment descriptors and identifiers, to
C     attempt to minimize file reads. Buffering segments involves
C     maintaining three tables:  the file table, the instrument table,
C     and the segment table. CKSNS is broken down into various tasks,
C     described in the code below, which perform these manipulations.
C
C     A description of the components of each table is provided in
C     the declarations section of CKBSR.
C
C     Basically, the buffering is performed as follows: once a request
C     for a segment for a particular instrument is made, if there are
C     no adequate entries in the buffer already, a search is made
C     through loaded files for applicable segments.  Every segment
C     pertaining to that instrument in a searched file is buffered,
C     before a check of the current buffer is made.  If the search
C     doesn't turn up a segment matching the specified search criteria
C     the next file is searched and new segments are added to the list,
C     and so on.
C
C     The information in the segment table (ST) is stored in a
C     doubly-linked list. Each node in the list contains several
C     individual pieces of data, which are stored in parallel
C     arrays.
C
C     In the following loop, we will try to simplify things by
C     doing exactly one thing on each pass through the loop.
C     After each pass, the status of the loop (STATUS) will be
C     adjusted to reflect the next thing that needs to be done.
C     The first task is set by CKBSS.
C
C     Occasionally, the current task will have to be interrupted
C     until another task can be carried out. (For example, when
C     collecting new segments, an interrupt might place a segment
C     at the front or end of the current instrument list; when placing
C     the segment on the list, a second interrupt might free
C     room in the segment table in order to allow the addition
C     to proceed.) In this case, the current task will be saved and
C     restored after the more urgent task has been completed.
C
C     The loop can terminate in only one of two ways (unless an error
C     occurs). First, if an applicable segment is found in the segment 
C     table, the handle, descriptor, and identifier for the segment 
C     are returned immediately.  Second, if the table does not contain 
C     an applicable segment, and if no files remain to be searched, 
C     the loop terminates normally, and no data are returned.
C
C     The status is saved on exit, however, so that subsequent calls
C     will resume a search exactly where previous calls left off.
C
C     Each status is described below.
C
C     'NEW INSTRUMENT'
C
C        This indicates that the specified spacecraft/instrument has
C        no segments stored for it at all. It must be added to the
C        instrument table.  (This is followed immediately by an
C        OLD FILES search, in which every file loaded is considered an
C        old file.)
C
C     'NEW FILES'
C
C        This indicates that at least one new file has been added
C        since the last time the segment list for the specified
C        instrument was searched. Find the oldest of these new files,
C        and begin a NEW SEGMENTS search in forward order for
C        segments to add to the front of the list.
C
C     'NEW SEGMENTS'
C
C        Continue a NEW FILES search, adding segments for the specified
C        instrument to the front of the list.
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
C        instrument to the end of the list.
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
C        This indicates that one of the instruments must be removed,
C        along with its stored segments, to make room for another
C        instrument or segment.  The instrument (other than the
C        specified instrument) with the smallest expense is selected
C        for this honor.
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
C     'PREPARE PARTIAL LIST'
C
C        This indicates that an attempt to 'MAKE ROOM' failed when
C        trying to 'ADD TO END' because all of the segments in the
C        table were for the instrument being searched on.  The partial 
C        list is found that contains all of the segments that were in 
C        the process of being added to the table for the current old 
C        file.  Next a 'CHECK PARTIAL LIST' is performed. Following 
C        that, a 'SEARCH W/O BUFF' is performed on all unsearched 
C        files.
C
C     'CHECK PARTIAL LIST'
C
C        This indicates that a portion of the list can't be buffered.
C        Before this portion is freed, it is to be checked for 
C        applicable segments.
C
C     'SEARCH W/O BUFF'
C
C        This indicates that the segment table was too small to handle
C        all of the segments for the current instrument, and that the
C        remaining unchecked old files should be searched for applicable
C        segments, without buffering the segments.
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
C     'HOPELESS'
C
C        This indicates that the table does not contain an applicable
C        segment, and no files remain to be searched.
C
C      'BOGUS ENTRY'
C
C        This is the initial value of STATUS and indicates that no
C        call to CKBSS was ever made. If this is the case then an
C        error will be signaled.
C
      
      IF ( STATUS .EQ. 'BOGUS ENTRY' )  THEN
 
         CALL SETMSG ( 'Must begin a search by calling CKBSS first.' )
         CALL SIGERR ( 'SPICE(CALLCKBSSFIRST)' )
         CALL CHKOUT ( 'CKSNS'                 )
         RETURN
 
      END IF
 

      DO WHILE ( STATUS .NE. 'HOPELESS' )
C
C        If new files have been added, they have to be searched.
C        Otherwise, go right to the list of stored segments.
C
         IF ( STATUS .EQ. '?' ) THEN
C
C           There are two ways to get to this point.
C
C           1)  Status may have been set to '?' by CKBSS.
C
C           2)  Status was set to '?' by the NEW SEGMENTS block
C               of code as the result of finishing the read of
C               a new file.
C
 
            IF ( ITHFS( IINDEX ) .LT. FTNUM( NFT ) ) THEN
 
               STATUS = 'NEW FILES'
 
            ELSE
C
C              Much of the time, the segment used to satisfy the 
C              previous request will also satisfy the current 
C              request.  Check whether this is the case.
C
               IF ( ITCHKP(IINDEX) ) THEN
C
C                 The previous segment found for the current instrument
C                 is a viable candidate for the current request.  See 
C                 whether the request time REQT falls into the time 
C                 interval for which this segment provides the 
C                 highest-priority coverage.
C
C                 We treat the re-use interval as topologically open 
C                 because one or both endpoints may belong to 
C                 higher-priority segments.
C
                  IF (      ( REQT .GT. (ITLB(IINDEX) + SAVTOL) ) 
     .                .AND. ( REQT .LT. (ITUB(IINDEX) - SAVTOL) ) ) THEN
C
C                    The request time falls into the portion of
C                    the re-use interval that isn't blocked by 
C                    higher-priority segments, when the coverage of
C                    those segments is extended in either direction
C                    by TOL.
C                    
                     IF (      ( .NOT. AVNEED          ) 
     .                    .OR. ( ITPRVF(IINDEX) .NE. 0 )  ) THEN
C
C                       This segment has angular velocity if we 
C                       need it.  The segment satisfies the 
C                       request.
C
                        HANDLE = ITPRVH(IINDEX)
                        SEGID  = ITPRVI(IINDEX)
                    
                        CALL MOVED ( ITPRVD(1,IINDEX), DSCSIZ, DESCR )

                        FOUND  = .TRUE.

C
C                       We can only use the re-use interval once on
C                       a given search.  If this search is continued,
C                       we'll have to check the list.  Prepare now.
C
                        SAVEP  =  ITBEG( IINDEX )
                        STATUS = 'CHECK LIST'

                        CALL CHKOUT ( 'CKSNS' )
                        RETURN

                     END IF
C
C                    We needed angular velocity data but didn't have 
C                    it if we reached this point.
C
                  END IF

C
C                 Adjust the expense here. If the expense of the list 
C                 contains a component due to the cost of finding the
C                 unbuffered segment providing data for re-use, subtract
C                 that component from the expense.
C
                  ITEXP(IINDEX)  = ITEXP(IINDEX) - ITRUEX(IINDEX)
                  ITRUEX(IINDEX) = 0

C
C                 The re-use interval becomes invalid if it didn't 
C                 satisfy the request.  The validity flag gets 
C                 re-set below.
C
C                 At this point, the previous segment is not a candidate
C                 to satisfy the request---at least not until we've done
C                 some file searches to verify that
C
C                    - The previous segment is still available.
C
C                    - The previous segment hasn't been superseded by a 
C                      more recently loaded segment.
C
C                 Carry on with the usual search algorithm.
C
                  ITCHKP(IINDEX) = .FALSE.

               END IF

C
C              If the segment list for this instrument is empty, make 
C              sure the expense is reset to 0.
C 
               IF ( ITBEG(IINDEX) .EQ. 0 ) THEN
                  ITEXP(IINDEX) = 0
               END IF

C
C              Prepare to look at the first segment in the list for
C              this instrument.
C 
               P      =  ITBEG( IINDEX )
               STATUS = 'CHECK LIST'
 
            END IF
 


         ELSE IF ( STATUS .EQ. 'NEW INSTRUMENT' ) THEN
C
C           New instruments are added to the end of the instrument 
C           table. If the table is full, one of the current occupants 
C           must be removed to make room for the new one.
C
C           Setting LFS to one more than the highest current file 
C           number means the 'OLD FILES' search that follows will
C           begin with the last-loaded file.
C
C           There is one way to get here:
C
C           1)  The variable STATUS was set to NEW INSTRUMENT prior 
C               in CKBSS.
C
C           Find the cheapest slot in the instrument table to store
C           the initial information about this instrument.
C
C           NOTE:  This used to be handled by the MAKE ROOM section.
C           However, trying to handle this special case there was
C           just more trouble than it was worth.
C
            IF ( NIT .LT. ITSIZE ) THEN
C
C              If the instrument table isn't full, the cheapest place is
C              just the next unused row of the table.
C
               NIT   = NIT + 1
               CHEAP = NIT
 
            ELSE
C
C              The instrument table is full.  Find the least
C              expensive instrument in the table and remove it.
C
               CHEAP  = 1
               MINEXP = ITEXP(1)
 
               DO I = 2, NIT
 
                  IF ( ITEXP(I) .LT. MINEXP ) THEN
                     CHEAP  = I
                     MINEXP = ITEXP(I)
                  END IF
 
               END DO

C
C              If there are any segments associated with the
C              least expensive instrument, we put them back on the free
C              list.
C
               HEAD = ITBEG(CHEAP)

               IF ( HEAD .GT. 0 ) THEN

                  TAIL =  - LNKPRV ( HEAD, STPOOL )
                  CALL LNKFSL ( HEAD, TAIL, STPOOL )

               END IF

            END IF

C
C           Set up a table entry for the new instrument. 
C 
            ITINS (CHEAP) = SCINST
            ITEXP (CHEAP) = 0
            ITHFS (CHEAP) = FTNUM(NFT)
            ITLFS (CHEAP) = FTNUM(NFT) + 1
            ITBEG (CHEAP) = 0
            ITCHKP(CHEAP) = .FALSE.
            IINDEX        = CHEAP

C
C           The following items associated with the re-use interval
C           need not be initialized at this point:
C
C              ITRUEX
C              ITLB
C              ITUB
C              ITPRVF
C              ITPRVH
C              ITPRVI
C              ITPRVD
C
C           However, we'll give these items initial values to 
C           help prevent compilation warnings from zealous 
C           compilers.
C
            ITRUEX(CHEAP) = 0
            ITLB  (CHEAP) = DPMIN()
            ITUB  (CHEAP) = DPMAX()
            ITPRVF(CHEAP) = 0
            ITPRVH(CHEAP) = 0
            ITPRVI(CHEAP) = ' '
            CALL CLEARD ( DSCSIZ, ITPRVD(1,CHEAP) )

C
C           Now search all of the files for segments relating to
C           this instrument.
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
 
            FINDEX = 1
 
            DO WHILE ( ITHFS(IINDEX) .GE. FTNUM(FINDEX) )
 
               FINDEX = FINDEX + 1
 
            END DO
 
            ITHFS( IINDEX ) = FTNUM( FINDEX )
 
            CALL DAFBFS ( FTHAN( FINDEX ) )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'CKSNS' )
               RETURN
            END IF
            
            STATUS = 'NEW SEGMENTS'
 
C
C           The cost of the list contributed by the new file is
C           zero so far.
C
            COST   = 0


         ELSE IF ( STATUS .EQ. 'NEW SEGMENTS' ) THEN
C
C           New files are searched in forward order. Segments, when
C           found, are inserted at the front of the list. Invisible
C           segments (initial time > final time) are ignored.
C
C           Each segment examined, whether applicable or not, adds to
C           the expense of the list.
C
C           The only way to get here is from the NEW FILES block
C           of the IF structure.
 
            CALL DAFFNA ( FND )
 
            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'CKSNS' )
               RETURN
            END IF

            IF ( .NOT. FND ) THEN
C
C              We're out of segments in the current file.  Decide
C              whether we need to examine another new file, or 
C              whether we're ready to check the list.
C
               STATUS = '?'
               ITEXP( IINDEX ) = ITEXP( IINDEX ) + COST
 
            ELSE
 
               CALL DAFGS ( DESCR )
               CALL DAFUS ( DESCR, ND, NI, DCD, ICD )
 
               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'CKSNS' )
                  RETURN
               END IF

               IF (       ( ICD(1) .EQ. SCINST )
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
C           When old files must be searched (because the segments in 
C           the list are inadequate), they should be searched in
C           backward order, beginning with the newest old file not 
C           yet searched.  The segment list will be re-checked 
C           after each file is searched.  If a match is found, 
C           the search terminates, so some old files may not be 
C           searched.
C
C           Begin a backwards search, and prepare to look for 
C           individual segments from the file.
C
C           You can get to this block in two ways.
C
C           1) We can have a NEW INSTRUMENT.
C
C           2) We have checked the current list (CHECK LIST) for
C              this instrument, didn't find an applicable segment and
C              have some files left that have not been seached.
 
            FINDEX = NFT
 
            DO WHILE ( ITLFS( IINDEX ) .LE. FTNUM( FINDEX ) )
               FINDEX = FINDEX - 1
            END DO
 
            CALL DAFBBS ( FTHAN( FINDEX ) )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'CKSNS' )
               RETURN
            END IF

            STATUS = 'OLD SEGMENTS'
 
C
C           The next thing we'll do is search through all the segments
C           of this file for those that applicable to this instrument.
C           The cost of the list contributed by the current file is
C           zero so far.
C
            COST = 0
  
C
C        Old files are searched in backward order. Segments, when
C        found, are inserted at the end of the list. Invisible
C        segments (initial time > final time) are ignored.
C
C        Each segment examined, whether applicable or not, adds to
C        the expense of the list.
C
         ELSE IF ( STATUS .EQ. 'OLD SEGMENTS' ) THEN
C
C           There is only one way to get here---from the
C           block 'OLD FILES'.  Note we do not add to the
C           expense of the list for this instrument until we've
C           completely searched this file.
C 
            CALL DAFFPA ( FND )
 
            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'CKSNS' )
               RETURN
            END IF

            IF ( .NOT. FND ) THEN
C
C              All of the segments in this file have been exhausted.
C              Change the lowest file searched indicator for this
C              instrument to be the current file, and go check the
C              current list.
C
               ITLFS( IINDEX ) =  FTNUM( FINDEX )
               ITEXP( IINDEX ) =  ITEXP( IINDEX ) + COST
               P               =  ITBEG( IINDEX )
               STATUS          = 'CHECK LIST'
 
            ELSE
 
               CALL DAFGS ( DESCR )
               CALL DAFUS ( DESCR, ND, NI, DCD, ICD )
 
               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'CKSNS' )
                  RETURN
               END IF

               IF (      ( ICD(1) .EQ. SCINST )
     .             .AND. ( DCD(1) .LE. DCD(2) )  ) THEN
 
                  DOING  = 'OLD SEGMENTS'
                  URGENT = 'ADD TO END'
                  STATUS = 'SUSPEND'
 
               END IF
 
               COST = COST + 1
 
            END IF
 


         ELSE IF ( STATUS .EQ. 'CHECK LIST' ) THEN
C
C           Okay, all the new files (and maybe an old file or two)
C           have been searched. Time to look at the list of segments
C           stored for the instrument, to see if there is one applicable
C           to the specified epoch and need for angular velocity data.
C
C           If so, return it.  If not, try another old file.  If there
C           are no more old files, give up the ghost.
C
C           There are two ways to get to this point.
C
C           1) From the '?' block.
C           2) From the 'OLD SEGMENTS' block.
C
C           For every segment examined, adjust the re-use interval
C           associated with the current instrument.
C
C           P always points to the current segment in the list. Reject
C           a segment if there is a need for angular velocity data and
C           the segment doesn't have it.
C
C           If this is a new search, initialize the re-use interval.
C           If we're resuming a search, the re-use interval is invalid.
C
            IF ( NEWSCH ) THEN

               ITLB(IINDEX) = DPMIN()
               ITUB(IINDEX) = DPMAX() 

            END IF

            DO WHILE ( P .GT. 0 )

               IF ( NEWSCH ) THEN
C
C                 Trim the re-use interval if the request time lies
C                 outside of the current segment.
C
                  IF ( REQT .GT. STDCD(2,P) ) THEN
C
C                    REQT is to the right of the coverage interval of 
C                    this segment.  Trim the re-use interval on the 
C                    left, if necessary.
C
                     ITLB(IINDEX) = MAX ( ITLB(IINDEX), STDCD(2,P) )


                  ELSE IF ( REQT .LT. STDCD(1,P) ) THEN
C
C                    REQT is to the left of the coverage interval of 
C                    this segment.  Trim the re-use interval on the 
C                    right, if necessary.
C
                     ITUB(IINDEX) = MIN ( ITUB(IINDEX), STDCD(1,P) )

                  END IF

               END IF
               

               IF (       ( OMEGA .GE. STDCD(1,P) ) 
     .              .AND. ( ALPHA .LE. STDCD(2,P) )  ) THEN
C
C                 The segment coverage interval intersects the request
C                 interval ALPHA:OMEGA.
C
                  IF (  (.NOT. AVNEED) .OR. (STICD(4,P) .NE. 0)  ) THEN
C
C                    This segment satisfies the request.
C 
                     CALL DAFPS ( ND,         NI,
     .                            STDCD(1,P), STICD(1,P), DESCR )
 
                     SEGID  =  STIDNT( P )
                     HANDLE =  STHAN ( P )
                     FOUND  = .TRUE.
 
C
C                    If the segment actually contains the request
C                    time, and if this is a new search, set the 
C                    re-use interval.  We require the request time 
C                    to be in the interior of the interval:  it 
C                    cannot be one of the endpoints.
C
                     IF (         NEWSCH
     .                    .AND. ( REQT .GT. STDCD(1,P) )
     .                    .AND. ( REQT .LT. STDCD(2,P) )  ) THEN     
C
C                       Set the re-use interval for the current 
C                       instrument.
C
                        ITLB(IINDEX) = MAX ( ITLB(IINDEX), STDCD(1,P) )
                        ITUB(IINDEX) = MIN ( ITUB(IINDEX), STDCD(2,P) )
                  
C
C                       Save the returned output items, in case this 
C                       segment may satisfy the next request.  
C
                        ITPRVH(IINDEX) =  HANDLE
                        ITPRVI(IINDEX) =  SEGID
                        ITPRVF(IINDEX) =  STICD(4,P)

                        CALL MOVED ( DESCR, DSCSIZ, ITPRVD(1,IINDEX) )

                        ITCHKP(IINDEX) =  .TRUE.

                     END IF

C
C                    Go ahead and move the pointer up before returning
C                    so that the search for the next applicable segment
C                    will start at the right place.
C
                     SAVEP = STPOOL ( FORWRD, P )

C
C                    Indicate the first pass of this search has been
C                    completed.
C
                     NEWSCH = .FALSE.

                     CALL CHKOUT ( 'CKSNS' )
                     RETURN
 
                  END IF

               END IF
C
C              Get the next node.  We avoid LNKNXT here in order
C              to speed up the operation.
C 
               P = STPOOL ( FORWRD, P )
 
            END DO
 
C
C           If we're still here we didn't have information for this
C           instrument in the segment list.
C
C           If there are more files, search them.
C           Otherwise, things are hopeless, set the status that way.
C 
            IF ( ITLFS( IINDEX ) .GT. FTNUM( 1 ) ) THEN
               STATUS = 'OLD FILES'
            ELSE
               STATUS = 'HOPELESS'
            END IF
 


         ELSE IF ( STATUS .EQ. 'MAKE ROOM' ) THEN
C
C           When adding a new segment to a full table, one of the 
C           current instruments must be dropped.  The ideal 
C           candidate is the one whose list was constructed at the 
C           lowest expense.  The candidate should be removed from 
C           the instrument table, and its list transferred to the 
C           segment table pool. 
C
C           There is ``room'' if the segment table pool contains at 
C           least one free node.
C
C           It is possible that a single instrument requires more 
C           than the entire segment table for its own segments. 
C           Two things might happen in such a case:
C
C              1) If the list under consideration was being added to at
C                 the end, then a search is continued without buffering
C                 any segments.
C
C              2) If the list was being added to at the beginning, then
C                 that means there was a NEW FILES search going on, and
C                 so a brand new list is constructed for the instrument,
C                 much as in a 'NEW INSTRUMENT' task.
C
C           There are two different ways to get to this point.
C
C              1) From 'ADD TO FRONT' if the segment table pool is full.
C              2) From 'ADD TO END' if the segment table pool is full.
C
C           Try to make room by deleting a segment list.  CHEAP will 
C           be the index of the "cheapest" segment list in the 
C           instrument table.
C
            MINEXP = INTMAX()
            CHEAP  = 0
 
            DO I = 1, NIT
 
               IF ( I .NE. IINDEX ) THEN

                  IF (      ( ITEXP(I) .LT. MINEXP   ) 
     .                 .OR. ( CHEAP    .EQ. 0        )  )THEN
C
C                    This list is the cheapest seen so far,
C                    possibly because it's the first one 
C                    considered.  At the moment, it's as good
C                    a candidate for removal as any.
C 
                     CHEAP  = I
                     MINEXP = ITEXP(I)
 
                  END IF

               END IF
 
            END DO
 

            IF ( CHEAP .EQ. 0 ) THEN
C
C              If there are no deleteable segments, the Thing To
C              Do depends on the task that was suspended before
C              entering MAKE ROOM.
C
               IF ( STACK(TOP) .EQ. 'ADD TO END' ) THEN
C
C                 The segment meta-data from the current file cannot
C                 be buffered.  We'll search the partial list of
C                 segments from this file, then proceed to search
C                 the rest of the file and any other old files, until
C                 we find an applicable segment or run out of segments.
C                 
                  STATUS = 'PREPARE PARTIAL LIST'
 

               ELSE
C
C                 STACK(TOP) is set to 'ADD TO FRONT'.  
C
C                 If there is no room left in the table in the middle
C                 of an attempt to add to the front of the list, just
C                 start from scratch by effectively initiating a 'NEW
C                 INSTRUMENT' task.
C
C                 Return the current list to the segment table pool.
C                 Note this list is non-empty.
C
                  P    =   ITBEG ( IINDEX )
                  TAIL = - LNKPRV( P, STPOOL )

                  CALL LNKFSL ( P, TAIL, STPOOL )
C
C                 Re-initialize the table for this instrument, and
C                 initiate an 'OLD FILES' search, just as in 'NEW
C                 INSTRUMENT'.
C
                  ITBEG( IINDEX ) = 0
                  ITEXP( IINDEX ) = 0
                  ITHFS( IINDEX ) = FTNUM( NFT )
                  ITLFS( IINDEX ) = FTNUM( NFT ) + 1

                  STATUS = 'OLD FILES'

               END IF
 
C
C              Unwind the stack; we've set the target states already.
C
               TOP = 0
 
            ELSE 
C
C              Return this cheapest list to the segment pool.  This
C              list could be empty.
C
               HEAD = ITBEG( CHEAP )

               IF ( HEAD .GT. 0 ) THEN

                  TAIL = - LNKPRV ( HEAD, STPOOL )

                  CALL LNKFSL ( HEAD, TAIL, STPOOL )

               END IF

C
C              Fill the deleted instrument's space in the table with
C              the final entry in the table.
C
               IF ( CHEAP .NE. NIT ) THEN

                  ITINS (CHEAP) = ITINS (NIT)
                  ITEXP (CHEAP) = ITEXP (NIT)
                  ITHFS (CHEAP) = ITHFS (NIT)
                  ITLFS (CHEAP) = ITLFS (NIT)
                  ITBEG (CHEAP) = ITBEG (NIT)
                  ITLB  (CHEAP) = ITLB  (NIT)
                  ITUB  (CHEAP) = ITUB  (NIT)
                  ITPRVH(CHEAP) = ITPRVH(NIT)
                  ITPRVI(CHEAP) = ITPRVI(NIT)
                  ITPRVF(CHEAP) = ITPRVF(NIT)
                  ITCHKP(CHEAP) = ITCHKP(NIT)
                  ITRUEX(CHEAP) = ITRUEX(NIT)

                  CALL MOVED ( ITPRVD(1,NIT), DSCSIZ, ITPRVD(1,CHEAP) )

               END IF

               IF ( IINDEX .EQ. NIT ) THEN
                  IINDEX = CHEAP
               END IF

C
C              One less instrument now.
C
               NIT    = NIT - 1
               STATUS = 'RESUME'
 
            END IF
C
C           Either we made room by freeing a non-empty segment list,
C           or we're going to work without additional space.  In the
C           latter case, the state is now 'OLD FILES' or 
C           'PREPARE PARTIAL LIST'.
C
 

         ELSE IF ( STATUS .EQ. 'ADD TO FRONT' ) THEN
C
C           The current segment information should be linked in at
C           the head of the segment list for the current instrument, 
C           and the pertinent instrument table entry should point 
C           to the new head of the list.
C
C           The only way to get here is from the block NEW SEGMENTS
C           after suspending that task.
 
            IF ( LNKNFN(STPOOL) .EQ. 0 ) THEN

               DOING  = 'ADD TO FRONT'
               URGENT = 'MAKE ROOM'
               STATUS = 'SUSPEND'
 
            ELSE
C
C              Allocate a node and link it to the front of the list
C              for the current instrument.
C
               CALL LNKAN ( STPOOL, NEW )

               STHAN( NEW ) = FTHAN( FINDEX )

               CALL DAFGN ( STIDNT(NEW) )
 
               CALL DAFUS ( DESCR, ND, NI, STDCD(1,NEW), STICD(1,NEW) )
 
               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'CKSNS' )
                  RETURN
               END IF

C
C              If the current list is empty, this append operation
C              is a no-op.
C 
               CALL LNKILB ( NEW, ITBEG(IINDEX), STPOOL )
               ITBEG( IINDEX ) = NEW
 
               STATUS  = 'RESUME'

            END IF
 

         ELSE IF ( STATUS .EQ. 'ADD TO END' ) THEN
C
C           The current segment information should be linked in at
C           the tail of the segment list for the current instrument.
C
C           The only way to get to this task is from the OLD SEGMENTS
C           block after suspending that task.
C 
            IF ( LNKNFN(STPOOL) .EQ. 0 ) THEN
 
               DOING  = 'ADD TO END'
               URGENT = 'MAKE ROOM'
               STATUS = 'SUSPEND'
 
            ELSE
C
C              Allocate a new node in the segment table pool.
C
               CALL LNKAN ( STPOOL, NEW )
 
               STHAN( NEW ) = FTHAN( FINDEX )
 
               CALL DAFGN ( STIDNT(NEW) )
 
               CALL DAFUS ( DESCR, ND, NI, STDCD(1,NEW), STICD(1,NEW) )
 
               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'CKSNS' )
                  RETURN
               END IF
  
               IF ( ITBEG(IINDEX) .LE. 0 ) THEN
C
C                 This is the first node in the list for this 
C                 instrument.
C
                  ITBEG( IINDEX ) = NEW
 
               ELSE
C
C                 Link the new node to the tail of the list.
C
                  TAIL = - LNKPRV ( ITBEG(IINDEX), STPOOL )
                  CALL LNKILA ( TAIL, NEW, STPOOL )

               END IF
  
               STATUS = 'RESUME'
 
            END IF
 

         ELSE IF ( STATUS .EQ. 'PREPARE PARTIAL LIST' ) THEN
C
C           When the segment table is completely full, continue
C           the search by looking through the unchecked portion 
C           of the segment list for the current instrument, and
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
            ITRUEX(IINDEX) = 0

C
C           Find the portion of the current instrument's segment list
C           which comes from the current file of interest.  SLBEG
C           will point to the beginning of this sublist.
C
            SLBEG  = ITBEG( IINDEX )
            FNDHAN = .FALSE.

            DO WHILE (  ( .NOT. FNDHAN ) .AND. ( SLBEG .GT. 0 )  )

               FNDHAN = STHAN(SLBEG) .EQ. FTHAN(FINDEX)

               IF ( .NOT. FNDHAN ) THEN 
C
C                 Get the next node.  We avoid LNKNXT here in order
C                 to speed up the operation.
C 
                  SLBEG = STPOOL ( FORWRD, SLBEG )
 
               END IF

            END DO
 
C
C           If the list contains segments from the current file, 
C           check that portion of the list.
C
C           Otherwise, finish searching old files without buffering
C           anything.
C
            IF ( SLBEG .GT. 0 ) THEN
C
C              The partial list from the current node onwards is to be 
C              returned to the free list.  Save this node, since
C              we'll finish searching the list before freeing the
C              partial list.  
C
               P = SLBEG

C
C              Record the fact that we'll need to free the partial list
C              later.
C
               FRESUB = .TRUE.

C
C              It may be that the partial list we're going to delete is 
C              the entire segment list for this instrument.  If so, the 
C              corresponding instrument table entry should be set to 
C              a non-positive value to indicate an empty segment list.
C
               IF ( P .EQ. ITBEG(IINDEX) ) THEN

                  ITBEG(IINDEX) = 0

C
C                 Also in this case, we must initialize the time
C                 bounds for this instrument.
C
                  ITLB(IINDEX) = DPMIN()
                  ITUB(IINDEX) = DPMAX() 

               END IF

               STATUS = 'CHECK PARTIAL LIST'

 
            ELSE

               STATUS = 'SEARCH W/O BUFF'
 
            END IF
 

         ELSE IF ( STATUS .EQ. 'CHECK PARTIAL LIST' ) THEN 
C
C           The only ways to get here are from the 
C           'PREPARE PARTIAL LIST' state, or by resuming a search of 
C           the partial list.
C
C           The portion of the segment list from the current file
C           is to be checked.
C
C           BEG points to the current segment in the temporary portion
C           of the list.
C
C           Reject a segment if there is a need for angular velocity
C           data and the segment doesn't have it.
C
            DO WHILE ( P .GT. 0 )
C
C              If this is a new search, update the re-use interval
C              and its expense.
C
               IF ( NEWSCH ) THEN
C
C                 Every segment seen from the current file contributes
C                 to the expense of the re-use interval.
C
                  ITRUEX(IINDEX) = ITRUEX(IINDEX) + 1

C
C                 Trim the re-use interval if the request time lies 
C                 outside the coverage of the current segment.
C
                  IF ( REQT .GT. STDCD(2,P) ) THEN
C
C                    REQT is to the right of the coverage interval of 
C                    this segment.  Trim the re-use interval on the 
C                    left, if necessary.
C
                     ITLB(IINDEX) = MAX ( ITLB(IINDEX), STDCD(2,P) )


                  ELSE IF ( REQT .LT. STDCD(1,P) ) THEN
C
C                    REQT is to the left of the coverage interval of 
C                    this segment.  Trim the re-use interval on the 
C                    right, if necessary.
C
                     ITUB(IINDEX) = MIN ( ITUB(IINDEX), STDCD(1,P) )

                  END IF

               END IF
C
C              We've updated the re-use interval if so required.
C               

               IF (       ( OMEGA .GE. STDCD(1,P) ) 
     .              .AND. ( ALPHA .LE. STDCD(2,P) )  ) THEN
C
C                 The segment coverage interval intersects the request
C                 interval ALPHA:OMEGA.
C
                  IF (  (.NOT. AVNEED) .OR. (STICD(4,P) .NE. 0) ) THEN
C
C                    This segment satisfies the request.  Set the
C                    output arguments.
C 
                     CALL DAFPS ( ND,         NI,
     .                            STDCD(1,P), STICD(1,P), DESCR )
 
                     SEGID  =  STIDNT( P )
                     HANDLE =  STHAN ( P )
                     FOUND  = .TRUE.
 
C
C                    If this is the first pass performed for the
C                    current search, then we can set the re-use 
C                    interval.  The re-use interval becomes invalid
C                    after the first pass.                       
C
C                    If the segment actually contains the request
C                    time, set the re-use interval.  We require
C                    the request time to be in the interior of the
C                    interval:  it cannot be one of the endpoints.
C
                     IF (         NEWSCH
     .                    .AND. ( REQT .GT. STDCD(1,P) )
     .                    .AND. ( REQT .LT. STDCD(2,P) )  ) THEN
C
C                       Adjust the re-use interval for the current 
C                       instrument.
C                  
                        ITLB(IINDEX) = MAX( ITLB(IINDEX), STDCD(1,P) )
                        ITUB(IINDEX) = MIN( ITUB(IINDEX), STDCD(2,P) )
C
C                       Save the returned output items, in case this 
C                       segment may satisfy the next request.  
C
                        ITPRVH(IINDEX) =  HANDLE
                        ITPRVI(IINDEX) =  SEGID
                        ITPRVF(IINDEX) =  STICD(4,P)

                        CALL MOVED ( DESCR, DSCSIZ, ITPRVD(1,IINDEX) )

                        ITCHKP(IINDEX) =  .TRUE.
C
C                       Update the expense of the list to reflect 
C                       the cost of locating this segment.
C
                        ITEXP(IINDEX) = ITEXP(IINDEX) + ITRUEX(IINDEX)

                     END IF
C
C                    We've set the re-use interval.
C
C                    Go ahead and move the pointer up before returning
C                    so that the search for the next applicable segment
C                    will start at the right place.
C
C                    We avoid LNKNXT here in order to speed up the 
C                    operation.
C 
                     SAVEP = STPOOL ( FORWRD, P )

C
C                    We cannot free the partial list yet, because
C                    we may return to search it again if the current
C                    segment doesn't have pointing that satisfies
C                    the caller's request.  The list will be freed
C                    at the start of the next search if it's not
C                    freed at the end of this block or in the 
C                    'SEARCH W/O BUFFERING' block.
C
C                    Indicate the first pass of this search has been
C                    completed.
C
                     NEWSCH = .FALSE.

                     CALL CHKOUT ( 'CKSNS' )
                     RETURN
                     
                  END IF
C
C                 Getting here implies angular velocity was
C                 requested but was not present in the segment.
C 
               END IF
C
C              The current segment didn't match.  Look at the next
C              segment in the list.
C 
               P = STPOOL ( FORWRD, P )
 
            END DO
C
C           We're done looking at the partial list.
C
C           Return the partial list to the segment table pool.
C           P at this point is the negative of the list head.
C           The list tail is (by the spec of the SPICELIB doubly
C           linked list routines) the negative of the predecessor 
C           of the head. 
C
C           Note the list is always non-empty at this point.
C
            TAIL = -LNKPRV( -P, STPOOL )

            CALL LNKFSL ( SLBEG, TAIL, STPOOL )
 
            FRESUB = .FALSE.

C
C           Search the remaining files.
C
            STATUS = 'SEARCH W/O BUFF'
 

         ELSE IF ( STATUS .EQ. 'SEARCH W/O BUFF' ) THEN
C
C           The only ways to get here are from the 
C           'PREPARE PARTIAL LIST' and 'CHECK PARTIAL LIST' states.
C
C           When the segment table is full with the current instrument's
C           segments and any freed up portions have been checked, 
C           continue the search for applicable segments in old files, 
C           without buffering any of the segments in the segment table.
C
C           Recall that a search is already in progress and that a
C           segment is currently under consideration (FND = .TRUE.).
C
            DO WHILE ( FINDEX .GT. 0 )
 
               DO WHILE ( FND )

                  IF ( NEWSCH ) THEN
C
C                    Each segment found contributes to the expense of 
C                    the re-use interval.
C                 
                     ITRUEX(IINDEX) = ITRUEX(IINDEX) + 1
 
                  END IF

                  CALL DAFGS ( DESCR )
                  CALL DAFUS ( DESCR, ND, NI, DCD, ICD )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( 'CKSNS' )
                     RETURN
                  END IF


                  IF ( SCINST .EQ. ICD(1) ) THEN
C
C                    This is a segment for the instrument of interest.

                     IF ( NEWSCH ) THEN
C
C                       Update the re-use interval for this instrument.
C
                        IF ( REQT .GT. DCD(2) ) THEN
C
C                          REQT is to the right of the coverage interval
C                          of this segment.  Trim the re-use interval 
C                          on the left, if necessary.
C
                           ITLB(IINDEX) = MAX ( ITLB(IINDEX), DCD(2) )


                        ELSE IF ( REQT .LT. DCD(1) ) THEN
C
C                          REQT is to the left of the coverage interval 
C                          of this segment.  Trim the re-use interval 
C                          on the right, if necessary.
C
                           ITUB(IINDEX) = MIN ( ITUB(IINDEX), DCD(1) )

                        END IF

                     END IF
C
C                    We've trimmed the re-use interval if necessary.
C                     
                     IF (       ( OMEGA .GE. DCD(1) ) 
     .                    .AND. ( ALPHA .LE. DCD(2) )  ) THEN
C
C                       The segment coverage interval intersects the 
C                       request interval ALPHA:OMEGA.
C
                        IF (      ( .NOT. AVNEED  ) 
     .                       .OR. ( ICD(4) .NE. 0 )  ) THEN
C
C                          This segment satisfies the request.  Set
C                          the output arguments.
C 
                           CALL DAFPS ( ND, NI, DCD, ICD, DESCR )
 
                           CALL DAFGN ( SEGID )                        

                           HANDLE =  FTHAN(FINDEX)
                           FOUND  = .TRUE.
 
                           IF ( NEWSCH ) THEN
C
C                             Adjust the re-use interval for the current
C                             instrument.
C
                              ITLB(IINDEX) = MAX( ITLB(IINDEX), DCD(1) )
                              ITUB(IINDEX) = MIN( ITUB(IINDEX), DCD(2) )
                  
C
C                             Save the returned output items, in case 
C                             this segment may satisfy the next request.
C
                              ITPRVH(IINDEX) =  HANDLE
                              ITPRVI(IINDEX) =  SEGID
                              ITPRVF(IINDEX) =  ICD(4)

                              CALL MOVED( DESCR, 
     .                                    DSCSIZ, ITPRVD(1,IINDEX) )

                              ITCHKP(IINDEX) =  .TRUE.
                             
C
C                             Update the expense of the list to reflect 
C                             cost of locating this segment.
C
                              ITEXP(IINDEX) =   ITEXP(IINDEX) 
     .                                        + ITRUEX(IINDEX)
                           END IF
C
C                          The re-use interval is set.
C
C                          Go ahead and point to the next segment in the
C                          file in case an attempt is made to continue 
C                          the search: you want to pick up exactly where
C                          you  left off.
C
                           CALL DAFFPA ( FND )
C
C                          Indicate the first pass of this search has 
C                          been completed.
C
                           NEWSCH = .FALSE.

                           CALL CHKOUT( 'CKSNS' )
                           RETURN
                           
                        END IF
C
C                       Getting here implies angular velocity was
C                       requested but was not present in the segment.
C
                     END IF
C
C                    The current segment's coverage didn't intersect
C                    the request interval.
C
                  END IF
C
C                 The current segment didn't contain data for the
C                 specified instrument.
C
C                 Look at the next segment in the current file.
C 
                  CALL DAFFPA ( FND )
 
               END DO
 
C
C              Try the next oldest file.
C
               FINDEX = FINDEX -1
 
               IF ( FINDEX .GT. 0 ) THEN
 
                  CALL DAFBBS ( FTHAN( FINDEX ) )
                  CALL DAFFPA ( FND             )
 
               END IF
 
            END DO
 
C
C           There's nothing nowhere if you get to here.
C
            ITRUEX(IINDEX) = 0
            STATUS         = 'HOPELESS'
 

         ELSE IF ( STATUS .EQ. 'SUSPEND' ) THEN
C
C           When a task is suspended, the current activity is placed on
C           a stack, to be restored later. Two levels are provided, 
C           since some interrupts can be interrupted by others.
C
            TOP          = TOP + 1
            STACK( TOP ) = DOING
            STATUS       = URGENT
 
         ELSE IF ( STATUS .EQ. 'RESUME' ) THEN
 
            STATUS = STACK( TOP )
            TOP    = TOP - 1

         END IF
 
      END DO
 
C
C     Can only get here if status is 'HOPELESS', in which case a
C     segment was not found.
C
      FOUND = .FALSE.

C
C     If we didn't find a segment, don't attempt to use saved
C     outputs from a previous call.  IINDEX will always be set
C     at this point.  Also, make sure the expense of the re-use
C     interval is zeroed out.
C
      IF ( IINDEX .GT. 0 ) THEN

         ITCHKP(IINDEX) = .FALSE.
         ITRUEX(IINDEX) =  0

      END IF

C
C     For safety, indicate the first pass of this search has been 
C     completed.  Normally, we won't return here before CKBSS is 
C     called again, but it's possible.
C
      NEWSCH = .FALSE.


      CALL CHKOUT ( 'CKSNS' )
      RETURN




 
C$Procedure      CKHAVE ( C-kernels --- Have some )
 
      ENTRY CKHAVE ( FOUND )
 
C$ Abstract
C
C     Determine whether or not any C-kernels are currently loaded.
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
C     CK
C     DAF
C
C$ Keywords
C
C     C-KERNEL
C
C$ Declarations
C
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FOUND      O   TRUE if at least one C-kernel is loaded.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     FOUND      is returned with the value TRUE if at least one
C                C-kernel is currently loaded.  Otherwise it returns
C                the value FALSE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This entry point allows the user to query the set of "loaded"
C     C-kernels to make sure that at least one C-kernel has been loaded.
C     This allows you to avoid making a search of an empty set of
C     loaded kernels which forces a SPICELIB error to be signaled.
C
C$ Examples
C
C     Suppose you want to call on of the C-kernel readers, but wish
C     to handle the exceptional case of "no kernels loaded" so that
C     the SPICELIB exception handling mechanism is avoided in the
C     case of an empty set of loaded kernels.  The code fragment
C     below shows how you might do this:
C
C        CALL CKHAVE ( LOADED )
C
C        IF ( LOADED ) THEN
C
C           CALL CKGP ( ... )
C
C        ELSE
C
C           take some kind of "reasonable action"
C
C        END IF
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
C-    SPICELIB Version 4.0.2, 28-FEB-2008 (BVS) 
C
C        Corrected the contents of the Required_Reading section.
C
C-    SPICELIB Version 4.0.1, 31-OCT-2001 (NJB)
C
C        Typo corrected.
C
C-    SPICELIB Version 4.0.0, 17-FEB-2000 (WLT)
C
C        Added the Entry point CKHAVE
C
C-    SPICELIB Version 3.0.0, 03-MAR-1999 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Determine whether any C-kernels are loaded
C
C-&
 
      FOUND = NFT .GT. 0
      RETURN
 
      END
