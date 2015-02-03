C$Procedure      DASA2L ( DAS, address to physical location )
 
      SUBROUTINE DASA2L ( HANDLE, TYPE,   ADDRSS,
     .                    CLBASE, CLSIZE, RECNO,  WORDNO )
 
C$ Abstract
C
C     Map a DAS address to a physical location in a specified DAS file.
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
C     DAS
C
C$ Keywords
C
C     DAS
C     FILES
C     TRANSFORMATION
C     UTILITY
C
C$ Declarations
 
      IMPLICIT NONE
 
      INTEGER               HANDLE
      INTEGER               TYPE
      INTEGER               ADDRSS
      INTEGER               CLBASE
      INTEGER               CLSIZE
      INTEGER               RECNO
      INTEGER               WORDNO
 
      INTEGER               CHAR
      PARAMETER           ( CHAR   =  1  )
 
      INTEGER               INT
      PARAMETER           ( INT    =  3  )
 
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   DAS file handle.
C     TYPE       I   Data type specifier.
C     ADDRSS     I   DAS address of a word of data type TYPE.
C     CLBASE,
C     CLSIZE     O   Cluster base record number and size.
C     RECNO,
C     WORDNO     O   Record/word pair corresponding to ADDRSS.
C     CHAR       P   Parameter indicating character data type.
C     INT        P   Parameter indicating integer data type.
C
C$ Detailed_Input
C
C     HANDLE         is the file handle of an open DAS file.
C
C     TYPE           is a data type specifier. TYPE may be any of
C                    the parameters
C
C                       CHAR
C                       DP
C                       INT
C
C                    which indicate `character', `double precision',
C                    and `integer' respectively.
C
C
C     ADDRSS         is the address in a DAS of a word of data
C                    type TYPE. For each data type (double precision,
C                    integer, or character), addresses range
C                    from 1 to the maximum current value for that type,
C                    which is available from DAFRFR.
C
C$ Detailed_Output
C
C     CLBASE,
C     CLSIZE         are, respectively, the base record number and
C                    size, in records, of the cluster containing the
C                    word corresponding to ADDRSS. The cluster spans
C                    records numbered CLBASE through CLBASE +
C                    CLSIZE - 1.
C
C     RECNO,
C     WORD           are, respectively, the number of the physical
C                    record and the number of the word within the
C                    record that correspond to ADDRSS. Word numbers
C                    start at 1 and go up to NC, ND, or NI in
C                    character, double precision, or integer records
C                    respectively.
C
C$ Parameters
C
C     CHAR,
C     DP,
C     INT            are data type specifiers which indicate
C                    `character', `double precision', and `integer'
C                    respectively. These parameters are used in
C                    all DAS routines that require a data type
C                    specifier as input.
C
C$ Exceptions
C
C     1)  If TYPE is not recognized, the error SPICE(DASINVALIDTYPE)
C         will be signaled.
C
C     2)  ADDRSS must be between 1 and LAST inclusive, where LAST
C         is last address in the DAS for a word of the specified
C         type. If ADDRSS is out of range, the error
C         SPICE(DASNOSUCHADDRESS) will be signaled.
C
C     3)  If this routine doesn't find an expected cluster descriptor
C         in a directory record, the error SPICE(BADDASDIRECTORY) is
C         signaled.
C
C     4)  If the input handle is invalid, the error will be diagnosed
C         by routines called by this routine.
C
C     If any of the above exceptions occur, the output arguments may
C     contain bogus information.
C
C$ Files
C
C     See the description of the argument HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     The DAS architecture allows a programmer to think of the data
C     within a DAS file as three one-dimensional arrays: one of
C     double precision numbers, one of integers, and one of characters.
C     This model allows a programmer to ask the DAS system for the
C     `nth double precision number (or integer, or character) in the
C     file'.
C
C     DAS files are Fortran direct access files, so to find the
C     `nth double precision number', you must have the number of the
C     record containing it and the `word number', or position, within
C     the record of the double precision number. This routine finds
C     the record/word number pair that specify the physical location
C     in a DAS file corresponding to a DAS address.
C
C     As opposed to DAFs, the mapping of addresses to physical
C     locations for a DAS file depends on the organization of data in
C     the file. For example, given a fixed set of DAS file summary
C     parameters, the physical location of the nth double precision
C     number can depend on how many integer and character records have
C     been written prior to the record containing that double precision
C     number.
C
C     The cluster information output from this routine allows the
C     caller to substantially reduce the number of directory reads
C     required to read a from range of addresses that spans
C     multiple physical records; the reading program only need call
C     this routine once per cluster read, rather than once per
C     physical record read.
C
C$ Examples
C
C     1)  Use this routine to read integers from a range of
C         addresses. This is done in the routine DASRDI.
C
C            C
C            C     Decide how many integers to read.
C            C
C                  NUMINT = LAST - FIRST + 1
C                  NREAD  = 0
C
C            C
C            C     Find out the physical location of the first
C            C     integer. If FIRST is invalid, DASA2L will take care
C            C     of the problem.
C            C
C                  CALL DASA2L (  HANDLE,  INT,     FIRST,
C                 .               CLBASE,  CLSIZE,  RECNO,  WORDNO  )
C
C            C
C            C     Read as much data from record RECNO as necessary.
C            C
C                  N  =  MIN ( NUMINT,  NWI - WORDNO + 1 )
C
C                  CALL DASRRI ( HANDLE, RECNO, WORDNO, WORDNO + N-1,
C                 .              DATA                                 )
C
C                  NREAD  =  N
C                  RECNO  =  RECNO + 1
C
C            C
C            C     Read from as many additional records as necessary.
C            C
C                  DO WHILE ( NREAD .LT. NUMINT )
C            C
C            C        At this point, RECNO if RECNO refers to 
C            C        a record in the current cluster, RECNO 
C            C        is the correct number of the record to read 
C            C        from next. Otherwise, the next cluster of
C            C        records containing integer data must be located.
C            C        CLBASE is the number of the first record of
C            C        the cluster we're about to read from.
C            C
C                     IF (  RECNO  .LT.  ( CLBASE + CLSIZE )  ) THEN
C            C
C            C           We can continue reading from the current
C            C           cluster.
C            C
C                        N  =  MIN ( NUMINT - NREAD,  NWI )
C
C                        CALL DASRRI (  HANDLE,
C                 .                     RECNO,
C                 .                     1,
C                 .                     N,
C                 .                     DATA ( NREAD + 1 )   )
C
C                        NREAD   =   NREAD + N
C                        RECNO   =   RECNO + 1
C
C
C                     ELSE
C            C
C            C           We must find the next integer cluster to
C            C           read from. The first integer in this
C            C           cluster has address FIRST + NREAD.
C            C
C                        CALL DASA2L ( HANDLE,
C                 .                    INT,
C                 .                    FIRST + NREAD,
C                 .                    CLBASE,
C                 .                    CLSIZE,
C                 .                    RECNO,
C                 .                    WORDNO  )
C
C                     END IF
C
C                  END DO
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
C     K.R. Gehringer (JPL)
C     N.J. Bachman   (JPL)
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0 APR-15-2014 (NJB)
C     
C        Previous update was 25-FEB-2014 
C
C        Bug fix: value of variable FAST for "unknown" files with one
C        directory record is now stored in TBFAST. The routine
C        previously computed correct outputs but did so more slowly
C        than necessary when multiple "fast" files were accessed.
C
C        Functional change: new entries in the file attribute table are
C        now inserted at index 1; the existing part of the table is
C        shifted to make room. Old entries drop off the end of the
C        list. The previous algorithm simply overwrote the first entry
C        once the table became full. 
C     
C        The file attribute table was expanded to store values of a
C        "read only" flag for each file. This enables the routine to
C        avoid look up of maximum addresses for known, read-only,
C        non-segregated files.
C       
C        Tests of FAILED and backup loop termination checks
C        were added. Logic was introduced to prevent reliance on
C        previous values of logical flags unless those flags were
C        set on a successful call. On any call that fails, the 
C        table entry for the current file is marked as unused by
C        setting the handle entry to zero.
C
C        The state variables FIRST and RDONLY have been removed.
C
C        Unneeded declarations were removed.
C
C        The code was re-structured to improve clarity.
C     
C-    SPICELIB Version 1.2.1 20-NOV-2001 (NJB)
C
C        Comment fix: diagram showing directory record pointers
C        incorrectly showed element 2 of the record as a backward
C        pointer. The element is actually a forward pointer.
C
C-    SPICELIB Version 1.2.0 03-JUL-1996 (NJB)
C
C        Bug fix: calculation to determine whether file is segregated
C        has been fixed.
C
C-    SPICELIB Version 1.1.1 19-DEC-1995 (NJB)
C
C        Corrected title of permuted index entry section.
C
C-    SPICELIB Version 1.1.0, 03-NOV-1995 (NJB)
C
C        Re-written to optimize address calculations for segregated,
C        read-only files.
C
C-    SPICELIB Version 1.0.1, 26-OCT-1993 (KRG)
C
C        Fixed a typo in the $ Brief_I/O section of the header.
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if the DAS open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT)
C
C-&
 
C$ Index_Entries
C
C     map DAS logical address to physical location
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.2.0 03-JUL-1996 (NJB)
C
C        Bug fix: calculation to determine whether file is segregated
C        has been fixed. An incorrect variable name used in a bound
C        calculation resulted in an incorrect determination of whether
C        a file was segregated, and caused arithmetic overflow for
C        files with large maximum addresses.  
C
C        In the previous version, the number of DAS words in a cluster
C        was incorrectly calculated as the product of the maximum
C        address of the cluster's data type and the number of words of
C        that data type in a DAS record. The correct product involves
C        the number of records in the cluster and the number of words of
C        that data type in a DAS record.
C
C-    SPICELIB Version 1.1.0, 03-NOV-1995 (NJB)
C
C        Re-written to optimize address calculations for segregated,
C        read-only files.
C
C-    SPICELIB Version 1.0.1, 26-OCT-1993 (KRG)
C
C        Fixed a typo in the $ Brief_I/O section of the header.
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if the DAS open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT)
C
C-&
 
C
C     Programmer's note: the TSPICE routine P_DASA2L must be
C     kept in sync with this routine. Current version of that
C     routine is 
C
C        TSPICE Version 1.0.0 APR-11-2014 (NJB)
C


C
C     SPICELIB functions
C
      INTEGER               ISRCHI
      LOGICAL               FAILED

C
C     Local parameters
C
 
C
C     Words per data record, for each data type:
C
      INTEGER               NWC
      PARAMETER           ( NWC = 1024 )
 
      INTEGER               NWD
      PARAMETER           ( NWD =  128 )
 
      INTEGER               NWI
      PARAMETER           ( NWI =  256 )
 
C
C     Directory forward pointer location
C
      INTEGER               FWDLOC
      PARAMETER           ( FWDLOC = 2 )
 
C
C     Directory address range locations
C
      INTEGER               CHRRNG
      PARAMETER           ( CHRRNG =          3 )
 
      INTEGER               DPRNG
      PARAMETER           ( DPRNG  = CHRRNG + 2 )
 
      INTEGER               INTRNG
      PARAMETER           ( INTRNG = DPRNG  + 2 )
 
C
C     Index of highest address in a `range array':
C 
      INTEGER               HIGH
      PARAMETER           ( HIGH = 2 )
 
C
C     Location of first type descriptor
C
      INTEGER               BEGDSC
      PARAMETER           ( BEGDSC = 9 )
 
C
C     Access word length
C
      INTEGER               ACCLEN
      PARAMETER           ( ACCLEN = 10 )
 
C
C     File table size
C
      INTEGER               MAXFIL
      PARAMETER           ( MAXFIL = 20 )

      INTEGER               MAXVEC
      PARAMETER           ( MAXVEC = 3 * MAXFIL )

 
C
C     Local variables
C
      CHARACTER*(ACCLEN)    ACCESS
 
      INTEGER               BASERC
      INTEGER               CURTYP
      INTEGER               DIRREC ( NWI )
      INTEGER               DSCLOC
      INTEGER               FIDX
      INTEGER               FREE
      INTEGER               HIADDR
      INTEGER               I
      INTEGER               J
      INTEGER               LSTREC ( 3 )
      INTEGER               LSTWRD ( 3 )
      INTEGER               MXADDR
      INTEGER               NCOMC
      INTEGER               NCOMR
      INTEGER               NDIRS
      INTEGER               NEXT   ( 3 )
      INTEGER               NFILES
      INTEGER               NREC
      INTEGER               NRESVC
      INTEGER               NRESVR
      INTEGER               NTYPES
      INTEGER               NW     ( 3 )
      INTEGER               NXTREC
      INTEGER               PREV   ( 3 )
      INTEGER               PRVHAN
      INTEGER               PRVTYP
      INTEGER               RANGE  ( 2 )
      INTEGER               RNGLOC ( 3 )
      INTEGER               TBBASE ( 3, MAXFIL )
      INTEGER               TBFWRD (    MAXFIL )
      INTEGER               TBHAN  (    MAXFIL )
      INTEGER               TBMXAD ( 3, MAXFIL )
      INTEGER               TBSIZE ( 3, MAXFIL )
      INTEGER               UB
      INTEGER               UNIT
      LOGICAL               FAST
      LOGICAL               KNOWN
      LOGICAL               PRVOK
      LOGICAL               TBFAST ( MAXFIL )
      LOGICAL               TBRDON ( MAXFIL )
      LOGICAL               SAMFIL
      LOGICAL               SEGOK
 
C
C     Saved variables
C
      SAVE
 
C
C     Initial values
C
C
C     NEXT and PREV map the DAS data type codes to their
C     successors and predecessors, respectively.
C
      DATA                  NEXT   /  2,   3,   1   /
      DATA                  PREV   /  3,   1,   2   / 
      DATA                  NW     /  NWC,    NWD,   NWI    /
      DATA                  RNGLOC /  CHRRNG, DPRNG, INTRNG /
 
      DATA                  FAST   / .FALSE.          /
      DATA                  FIDX   / 0                /
      DATA                  KNOWN  / .FALSE.          /
      DATA                  NFILES / 0                /
      DATA                  PRVHAN / 0                /
      DATA                  PRVOK  / .FALSE.          /
      DATA                  TBBASE / MAXVEC * -1      /
      DATA                  TBFAST / MAXFIL * .FALSE. /
      DATA                  TBFWRD / MAXFIL * -1      /
      DATA                  TBHAN  / MAXFIL *  0      /
      DATA                  TBMXAD / MAXVEC * -1      /
      DATA                  TBRDON / MAXFIL * .FALSE. /
      DATA                  TBSIZE / MAXVEC * -1      / 
 
C
C     Discovery check-in is used in this routine, even though
C     this routine calls routines that can signal errors. This
C     routine is a special case, because fast operation is very
C     important.
C
C
C     DAS files have the following general structure:
C
C           +------------------------+
C           |      file record       |
C           +------------------------+
C           |    reserved records    |
C           |                        |
C           +------------------------+
C           |     comment records    |
C           |                        |
C           |                        |
C           |                        |
C           +------------------------+
C           | first data directory   |
C           +------------------------+
C           |      data records      |
C           |                        |
C           |                        |
C           |                        |
C           |                        |
C           +------------------------+
C                       .
C                       .
C           +------------------------+
C           | last data directory    |
C           +------------------------+
C           |     data records       |
C           |                        |
C           |                        |
C           +------------------------+
C
C
C        Within each DAS data record, word numbers start at one and
C        increase up to NWI, NWD, or NWC: the number of words in an
C        integer, double precision, or character data record.
C
C
C           +--------------------------------+
C           |       |       |   ...  |       |
C           +--------------------------------+
C               1      2                NWD
C
C           +--------------------------------+
C           |   |   |       ...          |   |
C           +--------------------------------+
C             1   2                       NWI
C
C           +------------------------------------+
C           | | |           ...                | |
C           +------------------------------------+
C            1 2                               NWC
C
C
C        Directories are single records that describe the data
C        types of data records that follow. The directories
C        in a DAS file form a doubly linked list: each directory
C        contains forward and backward pointers to the next and
C        previous directories.
C
C        Each directory also contains, for each data type, the lowest
C        and highest logical address occurring in any of the records
C        described by the directory.
C
C        Following the pointers and address range information is
C        a sequence of data type descriptors. These descriptors
C        indicate the data type of data records following the
C        directory record. Each descriptor gives the data type
C        of a maximal set of contiguous data records, all having the
C        same type. By `maximal set' we mean that no data records of
C        the same type bound the set of records in question.
C
C        Pictorially, the structure of a directory is as follows:
C
C           +----------------------------------------------------+
C           | <pointers> | <address ranges> | <type descriptors> |
C           +----------------------------------------------------+
C
C        where the <pointers> section looks like
C
C           +-----------------------------------------+
C           | <backward pointer> | <forward pointer>  |
C           +-----------------------------------------+
C
C        the <address ranges> section looks like
C
C           +-------------------------------------------+
C           | <char range> | <d.p. range> | <int range> |
C           +-------------------------------------------+
C
C        and each range looks like one of:
C
C           +------------------------------------------------+
C           | <lowest char address> | <highest char address> |
C           +------------------------------------------------+
C
C           +------------------------------------------------+
C           | <lowest d.p. address> | <highest d.p. address> |
C           +------------------------------------------------+
C
C           +------------------------------------------------+
C           | <lowest int address>  | <highest int address>  |
C           +------------------------------------------------+
C
C        The type descriptors implement a run-length encoding
C        scheme. The first element of the series of descriptors
C        occupies two integers: it contains a type code and a count.
C        The rest of the descriptors are just signed counts; the data
C        types of the records they describe are deduced from the sign
C        of the count and the data type of the previous descriptor.
C        The method of finding the data type for a given descriptor
C        in terms of its predecessor is as follows: if the sign of a
C        descriptor is positive, the type of that descriptor is the
C        successor of the type of the preceding descriptor in the
C        sequence of types below. If the sign of a descriptor is
C        negative, the type of the descriptor is the predecessor of the
C        type of the preceding descriptor.
C
C           C  -->  D  -->  I  -->  C
C
C        For example, if the preceding type is `I', and a descriptor
C        contains the number 16, the type of the descriptor is `C',
C        whereas if the descriptor contained the number -800, the type
C        of the descriptor would be `D'.
C
C
C     Logic cases
C     ===========
C
C     There are three kinds of file attributes that this
C     routine distinguishes:
C
C        Attributes
C        ----------
C        "FAST"           read-only and segregated
C        "READONLY"       read-only and unsegregated
C        "WRITABLE"       writable
C
C     There are three kinds of file histories that this
C     routine distinguishes:
C
C        History
C        -------
C        "SAME"           file is the same as seen on 
C                         the previous call
C
C        "KNOWN"          file is not the same as seen
C                         on the previous call, but file
C                         information is buffered
C
C        "UNKNOWN"        file information is not buffered.
C
C     All combinations of attributes and history are possible,
C     so there are nine cases.
C
C     Mapping actions to cases
C     ========================
C
C        Action                             Cases
C        ------                             -----
C        Set SAMFIL, PRVOK                  ALL
C        Data type check                    ALL
C        Set KNOWN                          not (FAST and SAME)
C        Get access method                  UNKNOWN
C        Buffer insertion                   UNKNOWN
C        Set                                
C            TBHAN
C            TBRDON
C            TBFAST
C            TBFWRD                         UNKNOWN
C        Get file summary                   UNKNOWN or WRITABLE
C        Set TBMXAD                         UNKNOWN or WRITABLE
C        Segregation check                  UNKNOWN and not WRITABLE
C        Set TBBASE, TBSIZE                 FAST and UNKNOWN
C        Set FAST                           not SAME
C        Address range check                ALL        
C        Address search                     READONLY or WRITABLE
C        Set CLBASE, CLSIZE                 ALL
C
C     ========================
C
C
C     Make sure the data type is valid.
C
      IF (  ( TYPE .LT. CHAR ) .OR. ( TYPE .GT. INT )  ) THEN
 
         CALL CHKIN  ( 'DASA2L'                           )
         CALL DASHLU (  HANDLE, UNIT                      )
         CALL SETMSG ( 'Invalid data type: #. File was #' )
         CALL ERRINT ( '#',  TYPE                         )
         CALL ERRFNM ( '#',  UNIT                         )
         CALL SIGERR ( 'SPICE(DASINVALIDTYPE)'            )
         CALL CHKOUT ( 'DASA2L'                           )
         RETURN
 
      END IF
 
C
C     Decide whether we're looking at the same file as we did on the
C     last call. We can use data from the previous call only if that
C     call succeeded.
C
      SAMFIL =  ( HANDLE .EQ. PRVHAN )  .AND.  PRVOK
C
C     PRVOK defaults to .FALSE. and will be reset if this call
C     succeeds.
C        
      PRVOK  = .FALSE.

C     
C     Fast files get priority handling. If we have a fast file
C     that we saw on the previous call, skip directly to the
C     address range check.
C
      IF (  .NOT. ( FAST .AND. SAMFIL )  ) THEN
C        
C        Is this a file we recognize?
C
         IF ( SAMFIL ) THEN

            KNOWN = .TRUE.

         ELSE

            FIDX  =  ISRCHI ( HANDLE, NFILES, TBHAN )
            KNOWN =  FIDX .GT. 0

         END IF

         IF ( KNOWN ) THEN

            FAST = TBFAST(FIDX)

         ELSE
C
C           This file is not in our list. We'll buffer information
C           about this file.
C
C           Shift the table and insert the new entry at the front. The
C           entry at the back will be lost if the table is full.
C
C           Note that unused entries (those for which the DAS handle is
C           0) will drop out of the list automatically.
C
            UB = MIN ( NFILES, MAXFIL-1 )

            DO I = UB, 1, -1

               TBHAN (I+1) = TBHAN (I)
               TBRDON(I+1) = TBRDON(I)
               TBFAST(I+1) = TBFAST(I)
               TBFWRD(I+1) = TBFWRD(I)

               DO J = 1, 3
                  TBBASE( J, I+1 ) = TBBASE( J, I )
                  TBSIZE( J, I+1 ) = TBSIZE( J, I )
                  TBMXAD( J, I+1 ) = TBMXAD( J, I )
               END DO

            END DO
C
C           Insert the new table entry at index 1.
C
            NFILES       =  MIN ( NFILES+1,  MAXFIL )
            FIDX         =  1
            TBHAN(FIDX)  =  HANDLE
C
C           Set FAST to .FALSE. until we find out whether the file
C           is read-only and segregated.
C
            FAST          =  .FALSE.
            TBFAST(FIDX)  =  FAST
C
C           FIDX is now set whether or not the current file is known.
C
C           TBRDON(FIDX) and TBFAST(FIDX) are set.
C
C           Find out whether the file is open for read or write access.
C           We consider the file to be `slow' until we find out
C           otherwise. The contents of the arrays TBBASE, TBSIZE, and
C           TBMXAD are left undefined for slow files.
C
            CALL DASHAM ( HANDLE, ACCESS )

            IF ( FAILED() ) THEN
C
C              Make sure the current table entry won't be found
C              on a subsequent search.
C              
               TBHAN(FIDX) = 0
               RETURN

            END IF

C
C           TBRDON(FIDX) indicates whether the file is read-only.
C 
            TBRDON(FIDX) =  ACCESS .EQ. 'READ'

         END IF

C
C        FIDX, KNOWN and TBRDON( FIDX ) are set.
C
C        Get the file summary if it isn't known already.
C
         IF (  .NOT.  ( KNOWN .AND. TBRDON(FIDX) )  ) THEN
C
C           The file is new or it's writable; in either case the
C           maximum addresses are unknown. Get the current address
C           range for the file.
C
            CALL DASHFS ( HANDLE,
     .                    NRESVR,
     .                    NRESVC,
     .                    NCOMR,
     .                    NCOMC,
     .                    FREE,
     .                    TBMXAD(1,FIDX),
     .                    LSTREC,
     .                    LSTWRD )

            IF ( FAILED() ) THEN
C
C              Make sure the current table entry won't be found
C              on a subsequent search.
C              
               TBHAN(FIDX) = 0
               RETURN

            END IF

C
C           Set the forward cluster pointer.
C           
            TBFWRD(FIDX) = NRESVR + NCOMR + 2

         END IF

C
C        TBMXAD is set.
C
C        If this is an unknown file and is read-only, determine
C        whether the file is segregated
C
         IF (  ( .NOT. KNOWN ) .AND.  TBRDON(FIDX)  ) THEN
C
C           The file is read-only; we need to know whether it is
C           segregated. If so, there are at most three cluster
C           descriptors, and the first directory record's maximum
C           address for each type matches the last logical address for
C           that type.
C
C           FAST has been initialized to .FALSE. above.
C
C           NREC is the record number of the first directory record.
C
            NREC   = TBFWRD( FIDX )

            CALL DASRRI ( HANDLE, NREC, 1, NWI, DIRREC )

            NXTREC = DIRREC( FWDLOC )

            IF ( NXTREC .LE. 0 ) THEN
C
C              If this file is segregated, there are at most three
C              cluster descriptors, and each one points to a cluster
C              containing all records of the corresponding data type.
C              For each data type having a non-zero maximum address,
C              the size of the corresponding cluster must be large
C              enough to hold all addresses of that type.
C
               NTYPES = 0

               DO I = 1, 3

                  IF ( TBMXAD(I,FIDX) .GT. 0 ) THEN
                     NTYPES = NTYPES + 1
                  END IF

               END DO
 
C
C              Now look at the first NTYPES cluster descriptors,
C              collecting cluster bases and sizes as we go.
C
               BASERC  =  NREC   + 1
               PRVTYP  =  PREV ( DIRREC(BEGDSC) )
               DSCLOC  =  BEGDSC + 1
               SEGOK   = .TRUE.

               DO WHILE (       ( DSCLOC .LE. (BEGDSC+NTYPES) )
     .                    .AND.   SEGOK                        )
C
C                 Find the type of the current descriptor.
C
                  IF ( DIRREC(DSCLOC) .GT. 0 ) THEN
                     CURTYP  =  NEXT( PRVTYP )
                  ELSE
                     CURTYP  =  PREV( PRVTYP )
                  END IF

                  PRVTYP                 =   CURTYP
                  TBBASE( CURTYP, FIDX ) =   BASERC
                  TBSIZE( CURTYP, FIDX ) =   ABS ( DIRREC( DSCLOC ) )
                  BASERC                 =   BASERC
     .                                     + TBSIZE (CURTYP, FIDX )
 
                  SEGOK   =       TBMXAD( CURTYP, FIDX )
     .                      .LE.  TBSIZE( CURTYP, FIDX ) * NW(CURTYP)
 
                  DSCLOC  =  DSCLOC + 1
C
C                 This loop will terminate after at most 3
C                 iterations. No further checks are needed.
C
               END DO

C
C              Update FAST and TBFAST based on the segregation check.
C
               FAST         = SEGOK
               TBFAST(FIDX) = FAST
C
C              If the file is FAST, 
C
C                 TBBASE
C                 TBSIZE
C
C              have been updated as well.
C               
            END IF

         END IF
C
C        End of the segregation check.
C
      END IF
C
C     End of the NOT FAST or NOT SAME case.
C
C     At this point we have the logical address ranges for the 
C     file. Check the input address against them.
C
      MXADDR = TBMXAD( TYPE, FIDX )

      IF (  ( ADDRSS .LT. 1 ) .OR. ( ADDRSS .GT. MXADDR )  ) THEN 
C
C        Make sure the current table entry won't be found on a
C        subsequent search.
C              
         TBHAN(FIDX) = 0

         CALL CHKIN  ( 'DASA2L'                                   )
         CALL DASHLU (  HANDLE, UNIT                              )
         CALL SETMSG ( 'ADDRSS was #; valid range for type # is '  
     .   //            '# to #.  File was #'                      )
         CALL ERRINT ( '#',  ADDRSS                               )
         CALL ERRINT ( '#',  TYPE                                 )
         CALL ERRINT ( '#',  1                                    )
         CALL ERRINT ( '#',  MXADDR                               )
         CALL ERRFNM ( '#',  UNIT                                 )
         CALL SIGERR ( 'SPICE(DASNOSUCHADDRESS)'                  )
         CALL CHKOUT ( 'DASA2L'                                   )
         RETURN

      END IF

C
C     If we're looking at a "fast" file, we know the cluster base and
C     size. HIADDR is the highest address (not necessarily in use) in
C     the cluster.
C
      IF ( TBFAST(FIDX) ) THEN
C
C        The current file is "fast": read-only and segregated.
C
         CLBASE = TBBASE( TYPE, FIDX )
         CLSIZE = TBSIZE( TYPE, FIDX )
         HIADDR = CLSIZE * NW(TYPE)

      ELSE
C
C        If we're not looking at a "fast" file, find the cluster
C        containing the input address, for the input data type.
C
C        Find out which directory describes the cluster containing this
C        word. To do this, we must traverse the directory list. The
C        first directory record comes right after the last comment
C        record. (Don't forget the file record when counting the
C        predecessors of the directory record.)
C
C        Note that we don't need to worry about not finding a directory
C        record that contains the address we're looking for, since
C        we've already checked that the address is in range.
C
         NREC  = TBFWRD( FIDX )
         NDIRS = 1
 
         CALL DASRRI ( HANDLE,
     .                 NREC,
     .                 RNGLOC(TYPE),
     .                 RNGLOC(TYPE)+1,
     .                 RANGE )
 
 
         DO WHILE ( RANGE(HIGH) .LT. ADDRSS )
C
C           The record number of the next directory is the forward
C           pointer in the current directory record. Update NREC with
C           this pointer. Get the address range for the specified type
C           covered by this next directory record.
C
            CALL DASRRI ( HANDLE, NREC, FWDLOC, FWDLOC, NXTREC )
 
            NREC  = NXTREC
            NDIRS = NDIRS + 1
 
            CALL DASRRI ( HANDLE,
     .                    NREC,
     .                    RNGLOC(TYPE),
     .                    RNGLOC(TYPE)+1,
     .                    RANGE           )

            IF ( FAILED() ) THEN
C
C              Make sure the current table entry won't be found
C              on a subsequent search.
C              
               TBHAN(FIDX) = 0
               
               RETURN

            END IF
 
         END DO
 
C
C        NREC is now the record number of the directory that contains
C        the type descriptor for the address we're looking for.
C
C        Our next task is to find the descriptor for the cluster
C        containing the input address. To do this, we must examine the
C        directory record in `left-to-right' order. As we do so, we'll
C        keep track of the highest address of type TYPE occurring in
C        the clusters whose descriptors we've seen. The variable HIADDR
C        will contain this address.
C
         CALL DASRRI ( HANDLE, NREC, 1, NWI, DIRREC )

         IF ( FAILED() ) THEN
C
C           Make sure the current table entry won't be found on a
C           subsequent search.
C              
            TBHAN(FIDX) = 0

            RETURN

         END IF
 
C
C        In the process of finding the physical location corresponding
C        to ADDRSS, we'll find the record number of the base of the
C        cluster containing ADDRSS. We'll start out by initializing
C        this value with the number of the first data record of the
C        next cluster.
C
         CLBASE  =  NREC + 1
 
C
C        We'll initialize HIADDR with the value preceding the lowest
C        address of type TYPE described by the current directory.
C
         HIADDR  =  DIRREC(RNGLOC(TYPE)) - 1
 
C
C        Initialize the number of records described by the last seen
C        type descriptor. This number, when added to CLBASE, should
C        yield the number of the first record of the current cluster;
C        that's why it's initialized to 0.
C
         CLSIZE  =  0
 
C
C        Now find the descriptor for the cluster containing ADDRSS.
C        Read descriptors until we get to the one that describes the
C        record containing ADDRSS. Keep track of descriptor data
C        types as we go. Also count the descriptors.
C
C        At this point, HIADDR is less than ADDRSS, so the loop will
C        always be executed at least once.
C
         PRVTYP  =  PREV ( DIRREC(BEGDSC) )
         DSCLOC  =  BEGDSC + 1
 
         DO WHILE ( HIADDR .LT. ADDRSS )

            IF ( DSCLOC .GT. NWI ) THEN
C
C              This situation shouldn't occur, but it might if the
C              DAS file is corrupted.          
C
C              Make sure the current table entry won't be found
C              on a subsequent search.
C              
               TBHAN(FIDX) = 0

               CALL CHKIN  ( 'DASA2L'                               )
               CALL SETMSG ( 'Directory record # in DAS file with '
     .         //            'handle # is probably corrupted. No '
     .         //            'high cluster address at or above the '
     .         //            'input address # was found, though it '
     .         //            'should have been. High address was #. '
     .         //            'Data type was #.'                     )
               CALL ERRINT ( '#', NREC                              )
               CALL ERRINT ( '#', HANDLE                            )
               CALL ERRINT ( '#', ADDRSS                            )
               CALL ERRINT ( '#', HIADDR                            )
               CALL ERRINT ( '#', TYPE                              )
               CALL SIGERR ( 'SPICE(BADDASDIRECTORY)'               )
               CALL CHKOUT ( 'DASA2L'                               )
               RETURN

            END IF
C
C           Update CLBASE so that it is the record number of the
C           first record of the current cluster.
C
            CLBASE = CLBASE + CLSIZE
C
C           Find the type of the current descriptor.
C
            IF ( DIRREC(DSCLOC) .GT. 0 ) THEN
               CURTYP  =   NEXT( PRVTYP )
            ELSE
               CURTYP  =   PREV( PRVTYP )
            END IF

C
C           Forgetting to update PRVTYP is a Very Bad Thing (VBT).
C
            PRVTYP = CURTYP
 
C
C           If the current descriptor is of the type we're interested
C           in, update the highest address count.
C
            IF ( CURTYP .EQ. TYPE ) THEN
               HIADDR = HIADDR + ( NW(TYPE) * ABS( DIRREC(DSCLOC) ) )
            END IF
 
C
C           Compute the number of records described by the current
C           descriptor. Update the descriptor location.
C
            CLSIZE  =  ABS (  DIRREC( DSCLOC )  )
            DSCLOC  =  DSCLOC + 1
 
         END DO
C
C        At this point, the variables
C
C           CLBASE
C           CLSIZE
C           HIADDR
C
C        are set.
C
      END IF

C
C     At this point,
C
C        -- CLBASE is properly set: it is the record number of the
C           first record of the cluster containing ADDRSS.
C
C        -- CLSIZE is properly set: it is the size of the cluster
C           containing ADDRSS.
C
C        -- HIADDR is the last logical address in the cluster
C           containing ADDRSS.
C
C     Now we must find the physical record and word corresponding
C     to ADDRSS. The structure of the cluster containing ADDRSS and
C     HIADDR is shown below:
C
C        +--------------------------------------+
C        |                                      |  Record # CLBASE
C        +--------------------------------------+
C                           .
C                           .
C                           .
C        +--------------------------------------+
C        |      |ADDRSS|                        |  Record # RECNO
C        +--------------------------------------+
C                           .
C                           .
C                           .
C        +--------------------------------------+  Record #
C        |                               |HIADDR|
C        +--------------------------------------+  CLBASE + CLSIZE - 1
C
C
      RECNO  =     ( CLBASE + CLSIZE - 1 )
     .          -  ( HIADDR - ADDRSS     ) / NW(TYPE)
 
      WORDNO =    ADDRSS    -   (  (ADDRSS-1) / NW(TYPE) ) * NW(TYPE)
C
C     Update PRVHAN and set PRVOK to .TRUE. only if the call succeeded.
C 
      PRVHAN =  HANDLE
      PRVOK  = .TRUE.

      RETURN
      END
