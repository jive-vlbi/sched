C$Procedure EKQMGR  ( EK, query manager )
 
      SUBROUTINE EKQMGR (  CINDEX,  ELMENT,  EQRYC,   EQRYD,   EQRYI,
     .                     FNAME,   ROW,     SELIDX,  COLUMN,  HANDLE,
     .                     N,       TABLE,   ATTDSC,  CCOUNT,  FOUND,
     .                     NELT,    NMROWS,  SEMERR,  ERRMSG,  CDATA,
     .                     DDATA,   IDATA,   NULL                     )
       
C$ Abstract
C
C     Manage query operations on EK files.
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
C     EK
C
C$ Keywords
C
C     EK
C     FILES
C     SEARCH
C
C$ Declarations
 
 
      INCLUDE  'ekattdsc.inc'
      INCLUDE  'ekbool.inc'
      INCLUDE  'ekcnamsz.inc'
      INCLUDE  'ekcoldsc.inc'
      INCLUDE  'ekjrs.inc'
      INCLUDE  'ekopcd.inc'
      INCLUDE  'ekqlimit.inc'
      INCLUDE  'ekquery.inc'
      INCLUDE  'eksegdsc.inc'
      INCLUDE  'ektnamsz.inc'
      INCLUDE  'ektype.inc'
 
      INTEGER               FTSIZE
      PARAMETER           ( FTSIZE =   20 )
 
      INTEGER               STSIZE
      PARAMETER           ( STSIZE =  200 )
 
      INTEGER               MXTBLD
      PARAMETER           ( MXTBLD =  100 )
 
      INTEGER               MXCLLD
      PARAMETER           ( MXCLLD =  500 )
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
 
      INTEGER               CINDEX
      INTEGER               ELMENT
      CHARACTER*(*)         EQRYC
      DOUBLE PRECISION      EQRYD  ( * )
      INTEGER               EQRYI  ( LBCELL : * )
      CHARACTER*(*)         FNAME
      INTEGER               ROW
      INTEGER               SELIDX
      CHARACTER*(*)         COLUMN
      INTEGER               HANDLE
      INTEGER               N
      CHARACTER*(*)         TABLE
      INTEGER               ATTDSC ( ADSCSZ )
      INTEGER               CCOUNT
      LOGICAL               FOUND
      INTEGER               NELT
      INTEGER               NMROWS
      LOGICAL               SEMERR
      CHARACTER*(*)         ERRMSG
      CHARACTER*(*)         CDATA
      DOUBLE PRECISION      DDATA
      INTEGER               IDATA
      LOGICAL               NULL
 
 
C$ Brief_I/O
C
C     Variable  I/O  Entry points
C     --------  ---  --------------------------------------------------
C     CINDEX     I   EKCII
C     ELMENT     I   EKGC, EKGD, EKGI
C     EQRYC      I   EKSRCH
C     EQRYD      I   EKSRCH
C     EQRYI      I   EKSRCH
C     FNAME      I   EKLEF
C     ROW        I   EKGC, EKGD, EKGI, EKNELT
C     SELIDX     I   EKGC, EKGD, EKGI, EKNELT
C     COLUMN    I-O  EKCIN, EKGC, EKGD, EKGI, EKNELT, EKCII
C     HANDLE    I-O  EKLEF, EKUEF
C     N         I-O  EKTNAM, EKNTAB
C     TABLE     I-O  EKCCNT, EKCII, EKTNAM
C     ATTDSC     O   EKCII, EKCIN
C     CCOUNT     O   EKCCNT
C     FOUND      O   EKCIN, EKGC, EKGD, EKGI
C     NELT       O   EKNELT
C     NMROWS     O   EKSRCH
C     SEMERR     O   EKSRCH
C     ERRMSG     O   EKSRCH
C     CDATA      O   EKGC
C     DDATA      O   EKGD
C     IDATA      O   EKGI
C     NULL       O   EKGC, EKGD, EKGI
C     FTSIZE     P   All
C     MAXCON     P   All
C     MXCLLD     P   All
C     STSIZE     P   All
C     MAXORD     P   All
C     CNAMSZ     P   All
C     ITSIZE     P   All
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
C     FTSIZE         is the maximum number of EK files that may be
C                    loaded.  Any other DAS files loaded by the calling
C                    program count against this limit.
C
C     STSIZE         is the size of the segment table; this is the
C                    maximum number of segments that can be loaded at
C                    one time.
C
C     MXTBLD         is the maximum number of tables that can be loaded
C                    at any time.  A table can consist of multiple
C                    segments.
C
C     MXCLLD         is the maximum number of columns that can be loaded
C                    at any time.  A column may be spread across
C                    multiple segments; in this case, the portions of
C                    the column contained in each segment count against
C                    this limit.
C
C     ADSCSZ         is the size of column attribute descriptor.
C                    (Defined in ekattdsc.inc.)
C
C     LBCELL         is the SPICELIB cell lower bound.
C
C     Many other parameters are defined in the include files referenced
C     above.  See those files for details.
C
C
C$ Exceptions
C
C     1)  If this routine is called directly, the error
C         SPICE(BOGUSENTRY) is signaled.
C
C     See the headers of the entry points for descriptions of exceptions
C     specific to those routines.
C
C$ Files
C
C     This suite of routines reads binary `sequence component' EK files.
C     In order for a binary EK file to be accessible to this routine,
C     the file must be `loaded' via a call to the entry point EKLEF.
C
C     Text format EK files cannot be used by this routine; they must
C     first be converted by binary format by the NAIF Toolkit utility
C     SPACIT.
C
C$ Particulars
C
C     EKQMGR is an umbrella routine for its entry points:  all variables
C     used by the entry points are declared here.
C
C     EKQMGR supports loading and unloading EK files, executing queries,
C     and fetching the results of executed queries.  The entry points
C     and their functions are:
C
C        File loading and unloading:
C
C           EKLEF  ( EK, load event file   )
C           EKUEF  ( EK, unload event file )
C
C        Query execution:
C
C           EKSRCH ( EK, search for events )
C
C        Fetching query results:
C
C           EKGC   ( EK, get event data, character        )
C           EKGD   ( EK, get event data, double precision )
C           EKGI   ( EK, get event data, integer          )
C
C        Utilities:
C
C           EKNTAB ( EK, return the number of loaded tables        )
C           EKTNAM ( EK, return the names of loaded tables         )
C           EKCCNT ( EK, return the column count of a table        )
C           EKCII  ( EK, look up column info by index              )
C           EKNELT ( EK, return number of elements in column entry )
C
C
C     To issue queries to the EK system, users would normally call the
C     high-level interface routine EKFIND.  EKFIND parses queries and
C     converts them to the encoded form expected by EKSRCH.  It is
C     possible to call EKSRCH directly, but this should not be attempted
C     by others than EK masters.  EKFIND is not an entry point of
C     EKQMGR, but instead is a separate subroutine.
C
C$ Examples
C
C     1)  Query the EK system and fetch data matching queries.
C         The code fragment shown here does not rely on advance
C         knowledge of the input query or the contents of any loaded EK
C         files.
C
C         To simplify the example, we assume that all data are scalar.
C         This assumption relieves us of the need to test the size of
C         column entries before fetching them.  In the event that a
C         column contains variable-size array entries, the entry point
C         EKNELT may be called to obtain the size of column entries to
C         be fetched.  See EKNELT for an example.
C
C
C            C
C            C     Load EK file.  Also load leapseconds file for
C            C     time conversion.
C            C
C                  CALL EKLEF  ( EK, HANDLE )
C                  CALL FURNSH ( LEAP       )
C
C            C
C            C     Prompt for query.  Parse the SELECT clause using
C            C     EKPSEL.
C            C
C                  CALL PROMPT ( 'Enter query > ', QUERY )
C
C                  CALL EKPSEL ( QUERY,
C                                N,
C                                XBEGS,
C                                XENDS,
C                                XBEGS,
C                                XTYPES,
C                                XCLASS,
C                                TABS,
C                                COLS,
C                                ERROR,
C                                ERRMSG )
C
C
C                  IF ( ERROR ) THEN
C
C                     WRITE (*,*) ERRMSG
C
C                  ELSE
C            C
C            C        Submit query to the EK query system.
C            C
C                     CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG )
C
C                     IF ( ERROR ) THEN
C
C                        WRITE (*,*) ERRMSG
C
C                     ELSE
C            C
C            C           Fetch the rows that matched the query.
C            C
C                        DO ROW = 1, NMROWS
C            C
C            C              Fetch data from the Ith row.
C            C
C                           WRITE (*,*) ' '
C                           WRITE (*,*) 'ROW = ', ROW
C
C                           DO COL = 1, N
C            C
C            C                 Fetch the data from the Jth selected
C            C                 column.
C            C
C                              IF ( XCLASS(COL) .EQ. 'COL' ) THEN
C
C                                 OUTSTR  =  COLS(COL)
C                                 CALL PREFIX ( '.',       0, OUTSTR )
C                                 CALL PREFIX ( TABS(COL), 0, OUTSTR )
C                                 WRITE (*,*) 'COLUMN = ', OUTSTR
C
C                              ELSE
C
C                                 B  =  XBEGS(COL)
C                                 E  =  XENDS(COL)
C                                 WRITE (*,*) 'ITEM = ', QUERY(B:E)
C
C                              END IF
C
C                              IF ( XTYPES(COL) .EQ. 'CHR' ) THEN
C
C                                 CALL EKGC ( COL,   ROW,   1,
C                 .                           CDATA, NULL,  FOUND )
C
C                                 IF ( NULL ) THEN
C                                    WRITE (*,*) '<Null>'
C                                 ELSE
C                                    WRITE (*,*) CDATA
C                                 END IF
C
C
C                              ELSE IF ( XTYPES(COL) .EQ. 'DP' ) THEN
C
C                                 CALL EKGD ( COL,   ROW,   1,
C                 .                           DDATA, NULL,  FOUND )
C
C                                 IF ( NULL ) THEN
C                                    WRITE (*,*) '<Null>'
C                                 ELSE
C                                    WRITE (*,*) DDATA
C                                 END IF
C
C
C                              ELSE IF ( XTYPES(COL) .EQ. 'INT' ) THEN
C
C                                 CALL EKGI ( COL,   ROW,   1,
C                 .                           IDATA, NULL,  FOUND )
C
C                                 IF ( NULL ) THEN
C                                    WRITE (*,*) '<Null>'
C                                 ELSE
C                                    WRITE (*,*) IDATA
C                                 END IF
C
C
C                              ELSE
C            C
C            C                    The item is a time value.  Convert it
C            C                    to UTC for output.
C            C
C                                 CALL EKGD   ( COL,   ROW,   1,
C                 .                             TDATA, NULL,  FOUND )
C
C                                 IF ( NULL ) THEN
C                                    WRITE (*,*) '<Null>'
C                                 ELSE
C                                    CALL ET2UTC ( TDATA, 'C', 3, UTC )
C                                    WRITE (*,*) UTC
C                                 END IF
C
C                              END IF
C
C                           END DO
C            C
C            C              We're done with the column having index COL.
C            C
C                        END DO
C            C
C            C           We're done with the row having index ROW.
C            C
C                     END IF
C            C
C            C        We either processed the query or had an error.
C            C
C                  END IF
C            C
C            C     We either parsed the SELECT clause or had an error.
C            C
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
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.3, 10-FEB-2014 (BVS)
C
C        Added descriptions of ADSCSZ and LBCELL to the Parameters
C        section of the header.
C
C-    SPICELIB Version 2.0.2, 22-AUG-2006 (EDW)
C
C        Replaced references to LDPOOL with references 
C        to FURNSH.
C
C-    SPICELIB Version 2.0.1, 22-SEP-2004 (EDW)
C
C        Removed from the header descriptions, all occurrences of the 
C        token used to mark the $Procedure section.
C
C-    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB)
C
C        Bug fix:   When an already loaded kernel is opened with EKOPR,
C        it now has its link count reset to 1 via a call to EKCLS.
C        
C-    SPICELIB Version 1.3.0, 12-FEB-1999 (NJB)
C
C        Bug fix:  in entry point EKNELT, there was a error handling
C        branch that called CHKOUT where CHKIN should have been called.
C        This has been fixed.
C
C-    SPICELIB Version 1.2.0, 21-JUL-1998 (NJB)
C
C        In the entry point EKSRCH, a ZZEKJSQZ call was added after 
C        the ZZEKJOIN call.  This change reduces the scratch area usage 
C        for intermediate results of joins.  It also prevents ZZEKJOIN 
C        from being handed a join row set containing a segment vector 
C        having no corresponding row vectors.
C
C-    SPICELIB Version 1.1.0, 07-JUL-1996 (NJB)
C
C        Code fixes were made in routines
C
C           EKNELT, EKGC, EKGD, EKGI
C
C        Version lines were fixed in all routines:  versions were 
C        changed from "Beta" to "SPICELIB."
C
C-    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB)
C
C-&
 
C$ Index_Entries
C
C     Manage EK query operations
C
C-&

C$ Revisions
C 
C-    SPICELIB Version 1.3.0, 12-FEB-1999 (NJB)
C
C        Bug fix:  in entry point EKNELT, there was a error handling
C        branch that called CHKOUT where CHKIN should have been called.
C        This has been fixed.
C
C-    SPICELIB Version 1.2.0, 21-JUL-1998 (NJB)
C
C        In the entry point EKSRCH, a ZZEKJSQZ call was added after 
C        the ZZEKJOIN call.  This change reduces the scratch area usage 
C        for intermediate results of joins.  It also prevents ZZEKJOIN 
C        from being handed a join row set containing a segment vector 
C        having no corresponding row vectors.
C
C-    SPICELIB Version 1.1.0, 07-JUL-1996 (NJB)
C
C        Code fixes were made in routines
C
C           EKNELT, EKGC, EKGD, EKGI
C
C        Version lines were fixed in all routines:  versions were 
C        changed from "Beta" to "SPICELIB."
C
C-&

 
C
C     SPICELIB functions
C
      INTEGER               CARDC
      INTEGER               ISRCHC
 
      LOGICAL               EQSTR
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Non-SPICELIB functions
C
      INTEGER               EKNSEG
      INTEGER               ZZEKESIZ
 
      LOGICAL               ZZEKRMCH
      LOGICAL               ZZEKVMCH
 
C
C     Linked list functions:
C
C        Find next node
C        Find tail of list
C        Return number of free nodes
C
C
      INTEGER               LNKNXT
      INTEGER               LNKTL
      INTEGER               LNKNFN
 
 
C
C     Local parameters
C
 
C
C     Maximum number of constraints allowed in a single query:
C
      INTEGER               MAXSEG
      PARAMETER           ( MAXSEG = STSIZE )
 
      INTEGER               CTSIZE
      PARAMETER           ( CTSIZE = MXCLLD )
 
      INTEGER               DTSIZE
      PARAMETER           ( DTSIZE = MAXSEG * MXCLLD / 10 )
 
 
C
C     Miscellaneous parameters
C
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )
 
      INTEGER               NIL
      PARAMETER           ( NIL    =  0 )
 
C
C     Number of data types
C
      INTEGER               NTYPES
      PARAMETER           ( NTYPES = 4 )
 
C
C     Length of strings used for data type names.
C
      INTEGER               TYPLEN
      PARAMETER           ( TYPLEN = 4 )
 
C
C     Chunk size for buffered DAS integer reads.
C
      INTEGER               CHNKSZ
      PARAMETER           ( CHNKSZ = 1000 )
 
C
C     Length of status strings.
C
      INTEGER               SHORT
      PARAMETER           ( SHORT = 80 )
 
 
C
C     Local variables
C
 
C
C     As do the CK and SPK `BSR' entry points, the EKQMGR entry points
C     make use of an amusing panoply of tables, linked lists, and
C     pointers.  Here's where they're declared and described.
C
C
C     The file table contains a list of handles of loaded EK files.
C     Entries in the table are organized as a doubly linked list.
C     Names of file table variables begin with the string 'FT'.
C
C        The maximum number of EK files that can be loaded is FTSIZE.
C
C        The linked list pool used to index table entries is called
C        FTPOOL.
C
C        FTHAN is an array containing file handles of loaded EKs.
C
C        FTHEAD is the head node of the file list.
C
      INTEGER               FTPOOL ( 2,  LBPOOL : FTSIZE )
      INTEGER               FTHAN  (              FTSIZE )
      INTEGER               FTHEAD
 
 
C
C     The table list contains table names, segment table pointers,
C     and column table pointers for every table associated with a
C     loaded segment.  The segment table pointers indicate the head node
C     of the segment list for each table.  The column table pointers
C     indicate the column names and attributes associated with each
C     table.
C
C     The entries of the table list are organized as a doubly linked
C     list.  All variables in the table list have names starting with
C     the string 'TB'.
C
C        MXTBLD is the maximum number of tables that can be
C        accommodated by the table list.
C
C        TBPOOL is the doubly linked list pool used to index the
C        table list.
C
C        TBNAMS is an array of table names.
C
C        TBSTPT is an array containing pointers to the heads of segment
C        lists corresponding to segments belonging to the table.
C
C        TBNCOL is the number of columns in each table.
C
C        TBCTPT is an array of pointers to lists of column table
C        entries giving the names and attributes of the columns in each
C        table.
C
C        TBFILS is an array containing, for each table, handles of the
C        files that contain segments belonging to that table.
C
C        TBFLSZ is an array of sizes of handle lists for each table
C        entry.
C
C        TBHEAD is the head node of the table list.
C
C
      INTEGER               TBPOOL ( 2,  LBPOOL : MXTBLD )
      INTEGER               TBSTPT (              MXTBLD )
      INTEGER               TBNCOL (              MXTBLD )
      CHARACTER*(TNAMSZ)    TBNAMS (              MXTBLD )
      INTEGER               TBCTPT (              MXTBLD )
      INTEGER               TBFILS ( FTSIZE,      MXTBLD )
      INTEGER               TBFLSZ (              MXTBLD )
      INTEGER               TBHEAD
 
 
C
C
C     The segment table contains descriptive information for each
C     loaded segment.  Entries in the table are indexed by a linked
C     list pool containing a doubly linked list for each system (or
C     instrument) for which segments are loaded.
C
C     Names of segment table variables begin with the string 'ST'.
C
C        The maximum number of segments that can be loaded is MAXSEG.
C        Currently, the value of MAXSEG is just the size of the segment
C        table, STSIZE.
C
C        The linked list pool used to index segment table entries is
C        called STPOOL.
C
C        For each loaded segment, the following information is stored:
C
C           -- The file handle of the EK containing the segment.
C
C           -- The index of the segment within the EK that contains it.
C              Indices start at 1 and end with the segment count for the
C              EK file.
C
C           -- The segment descriptor.
C
C           -- The number of rows in the segment.
C
C           -- The number of columns in the segment.
C
C           -- A pointer to a list of column descriptors.  The
C              column descriptor table contains a complete descriptor
C              for every loaded column.
C
C
C
      INTEGER               STPOOL ( 2, LBPOOL : MAXSEG )
      INTEGER               STHAN  (             MAXSEG )
      INTEGER               STSIDX (             MAXSEG )
      INTEGER               STDSCS ( SDSCSZ,     MAXSEG )
      INTEGER               STNROW (             MAXSEG )
      INTEGER               STNCOL (             MAXSEG )
      INTEGER               STDTPT (             MAXSEG )
 
C
C     The column descriptor table contains a column descriptor for
C     every loaded column.  This table allows segments to share the
C     area used for buffering descriptors, making it reasonable for
C     the buffer space to have room for fewer than
C
C        MXCLLD * MAXSEG
C
C     column descriptors.
C
C     The space in the table is organized as a doubly linked list.
C
      INTEGER               DTPOOL ( 2,       LBPOOL:DTSIZE )
      INTEGER               DTDSCS ( CDSCSZ,  DTSIZE        )
 
C
C     The column attribute table contains attribute information for
C     every column in every loaded segment.  There is one entry per
C     column name; columns with the same names and different data
C     types may not be loaded simultaneously.
C
C     The entries of the column table are organized as a doubly linked
C     list.  All variables in the column table have names starting with
C     the string 'CT'.
C
C        CTSIZE is the maximum number of distinct column declarations
C        that can be accommodated by the column table.
C
C        CTPOOL is the doubly linked list pool used to index the column
C        table.
C
C        CTNAMS is an array containing column names.
C
C        CTCLAS is an array containing column class specifiers.
C
C        CTTYPS is an array containing column data types.
C
C        CTLENS is an array containing column string length specifiers.
C
C        CTFIXD is an array of logical flags indicating whether the
C        columns they correspond to have fixed size.
C
C        CTSIZS is an array of integers indicating the number of array
C        elements per column entry, for fixed-size columns.
C
C        CTINDX is an array of logical flags that indicate whether the
C        columns they correspond to are indexed.
C
C        CTNULL is an array of logical flags that indicate whether the
C        columns they correspond to may contain null values.
C
C
      INTEGER               CTPOOL ( 2,       LBPOOL : CTSIZE )
      CHARACTER*(CNAMSZ)    CTNAMS (                   CTSIZE )
      INTEGER               CTCLAS (                   CTSIZE )
      INTEGER               CTTYPS (                   CTSIZE )
      INTEGER               CTLENS (                   CTSIZE )
      LOGICAL               CTFIXD (                   CTSIZE )
      INTEGER               CTSIZS (                   CTSIZE )
      LOGICAL               CTINDX (                   CTSIZE )
      LOGICAL               CTNULL (                   CTSIZE )
 
 
C
C
C     Other local variables
C
      CHARACTER*(TYPLEN)    CHTYPE ( NTYPES )
      CHARACTER*(CNAMSZ)    CNAMS  ( MXCLLD )
      CHARACTER*(CNAMSZ)    CNMSET ( LBCELL : MXCLLD )
      CHARACTER*(CNAMSZ)    COLNAM
      CHARACTER*(TNAMSZ)    FRMALS ( MAXTAB )
      CHARACTER*(TNAMSZ)    FRMTAB ( MAXTAB )
      CHARACTER*(CNAMSZ)    LCNAME
      CHARACTER*(TNAMSZ)    LTNAME
      CHARACTER*(SHORT)     PROBLM
      CHARACTER*(CNAMSZ)    RCNAME
      CHARACTER*(TNAMSZ)    RTNAME
      CHARACTER*(SHORT)     STATE
      CHARACTER*(TNAMSZ)    TABNAM
      CHARACTER*(TNAMSZ)    TABVEC ( LBCELL : MAXTAB )
 
      DOUBLE PRECISION      DVALS  ( MAXCON )
 
      INTEGER               BEGIDX
      INTEGER               CBEGS  ( MAXCON )
      INTEGER               CDSCRS ( CDSCSZ,  MXCLLD )
      INTEGER               CENDS  ( MAXCON )
      INTEGER               CJBEG
      INTEGER               CJEND
      INTEGER               CJROWS
      INTEGER               CJSIZE
      INTEGER               CNSTYP ( MAXCON )
      INTEGER               COL
      INTEGER               COLPTR
      INTEGER               CONJ
      INTEGER               CVLEN
      INTEGER               CTNEW
      INTEGER               DELSEG
      INTEGER               DTNEW
      INTEGER               DTYPE  ( MAXCON )
      INTEGER               ENDIDX
      INTEGER               I
      INTEGER               IVALS  ( MAXCON )
      INTEGER               J
      INTEGER               JBASE1
      INTEGER               JBASE2
      INTEGER               JSIZE
      INTEGER               K
      INTEGER               KEY
      INTEGER               KEYDSC ( CDSCSZ )
      INTEGER               L
      INTEGER               LCIDX  ( MAXCON )
      INTEGER               LELTS  ( MAXCON )
      INTEGER               LDSCRS ( CDSCSZ, MAXCON )
      INTEGER               LTBIDX ( MAXCON )
      INTEGER               LXBEG
      INTEGER               LXEND
      INTEGER               NACT
      INTEGER               NCOLS
      INTEGER               NCONJ
      INTEGER               NEW
      INTEGER               NEXT
      INTEGER               NMATCH
      INTEGER               NORDER
      INTEGER               NPCOL
      INTEGER               NSEG
      INTEGER               NSEL
      INTEGER               NSV
      INTEGER               NTAB
      INTEGER               OCOLS  ( MAXORD )
      INTEGER               OELTS  ( MAXORD )
      INTEGER               OPS    ( MAXCON )
      INTEGER               ORDBAS
      INTEGER               OTABS  ( MAXORD )
      INTEGER               PTROFF
      INTEGER               R
      INTEGER               RBAS   ( MXJOIN )
      INTEGER               RCIDX  ( MAXCON )
      INTEGER               RDSCRS ( CDSCSZ, MAXCON )
      INTEGER               RELTS  ( MAXCON )
      INTEGER               RESBAS
      INTEGER               ROWIDX
      INTEGER               ROWVEC ( MAXTAB )
      INTEGER               RSIZE  ( MXJRS  )
      INTEGER               RTBIDX ( MAXCON )
      INTEGER               RTOTAL
      INTEGER               RWVBAS
      INTEGER               SELCOL ( MAXSEL )
      INTEGER               SELCTP ( MAXSEL )
      INTEGER               SELTAB ( MAXSEL )
      INTEGER               SEG
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               SEGVEC ( MAXTAB )
      INTEGER               SENSE  ( MAXORD )
      INTEGER               SGVBAS
      INTEGER               SIZES  ( MAXCON )
      INTEGER               STNEW
      INTEGER               T
      INTEGER               TAB
      INTEGER               TABIDX
      INTEGER               TBCURR
      INTEGER               TOP
      INTEGER               TPTVEC ( LBCELL : MAXTAB )
      INTEGER               UBASE  ( MXJRS )
      INTEGER               UNIT
      INTEGER               UNROWS
      INTEGER               USIZE
 
      LOGICAL               ACTIVC ( MAXCON )
      LOGICAL               ACTIVV ( MAXCON )
      LOGICAL               ATTMCH
      LOGICAL               CMTCH
      LOGICAL               DOSORT
      LOGICAL               FIRST
      LOGICAL               FND
      LOGICAL               INDEXD
      LOGICAL               KEYFND
      LOGICAL               NULSOK
      LOGICAL               PRESNT
      LOGICAL               SORTED
      LOGICAL               VMTCH
 
C
C
C     Saved variables
C
      SAVE
 
C
C
C     Initial values
C
      DATA                  LELTS  /  MAXCON * 1 /
      DATA                  OELTS  /  MAXORD * 1 /
      DATA                  RELTS  /  MAXCON * 1 /
 
      DATA                  CHTYPE / 'CHR', 'DP', 'INT', 'TIME' /
 
      DATA                  FTHEAD /  0       /
      DATA                  TBHEAD /  0       /
 
      DATA                  FIRST  / .TRUE.   /
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKQMGR' )
      END IF
 
C
C     Never come here.
C
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
 
 
      CALL CHKOUT ( 'EKQMGR' )
      RETURN
 
 
 
 
 
 
 
C$Procedure     EKLEF  ( EK, load event file )
 
      ENTRY EKLEF ( FNAME, HANDLE )
 
C$ Abstract
C
C     Load an EK file, making it accessible to the EK readers.
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
C     EK
C
C$ Keywords
C
C     EK
C     FILES
C     SEARCH
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
C     FNAME      I   Name of EK file to load.
C     HANDLE     O   File handle of loaded EK file.
C
C$ Detailed_Input
C
C     FNAME          is the name of a binary EK file to be loaded.
C
C$ Detailed_Output
C
C     HANDLE         is the handle of the EK file.  The file is
C                    accessible by the EK reader routines once it
C                    has been loaded.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the EK file indicated by FNAME contains a column whose
C         name matches that of a column in an already loaded EK, but
C         whose declared attributes don't match those of the loaded
C         column of the same name, the error SPICE(BADATTRIBUTES) is
C         signaled.  HANDLE is is undefined in this case.
C
C     2)  Loading an EK file that is already loaded does not cause side
C         effects.  The handle already associated with the file will be
C         returned.
C
C     3)  If a file open error occurs, the problem will be diagnosed by
C         routines called by this routine.  HANDLE is undefined in
C         this case.
C
C     4)  If loading the input file would cause the maximum number of
C         loaded EK files to be exceeded, the error
C         SPICE(EKFILETABLEFULL) will be signaled.  HANDLE is
C         undefined in this case.  This routine will attempt to
C         unload the file from the DAS system.
C
C     5)  If loading the input file would cause the maximum number of
C         loaded DAS files to be exceeded, the error will be diagnosed
C         by routines called by this routine.  HANDLE is undefined in
C         this case.  This routine will attempt to unload the file
C         from the DAS system.
C
C     6)  If loading the input file would cause the maximum number of
C         segments allowed in loaded EK files to be exceeded, the error
C         SPICE(EKSEGMENTTABLEFULL) will be signaled.  HANDLE is
C         is undefined in this case.  This routine will attempt to
C         unload the file from the DAS system.
C
C     7)  If loading the input file would cause the maximum number of
C         columns allowed in loaded EK files to be exceeded, the error
C         SPICE(EKCOLDESCTABLEFULL) will be signaled.  HANDLE is
C         is undefined in this case.  This routine will attempt to
C         unload the file from the DAS system.
C
C     8)  If loading the input file would cause the maximum allowed
C         number of columns having distinct attributes in loaded EK
C         files to be exceeded, the error SPICE(EKCOLATTRTABLEFULL) will
C         be signaled.  HANDLE is is undefined in this case.  This
C         routine will attempt to unload the file from the DAS system.
C
C     9)  If loading the input file would cause the maximum number of
C         instrument codes allowed in loaded EK files to be exceeded,
C         the error SPICE(EKIDTABLEFULL) will be signaled.  HANDLE is
C         is undefined in this case.  This routine will attempt to
C         unload the file from the DAS system.
C
C     10) If the input file does not contain at least one segment, the
C         error SPICE(EKNOSEGMENTS) will be signaled.
C
C$ Files
C
C     See description of FNAME in $Detailed_Input.
C
C$ Particulars
C
C     This routine makes EK files known to the EK system.  It is
C     necessary to load EK files using this routine in order to
C     query the files using the EK readers.
C
C$ Examples
C
C     1)  Load three EK files.  During query execution, all files
C         will be searched.
C
C            DO I = 1, 3
C               CALL EKLEF ( EK(I), HANDLE )
C            END DO
C
C            [Perform queries]
C
C
C     2)  Load 25 EK files sequentially, unloading the previous file
C         before each new file is loaded.  Unloading files prevents
C         them from being searched during query execution.
C
C            DO I = 1, 25
C
C               CALL EKLEF ( EK(I), HANDLE )
C
C               [Perform queries]
C
C               CALL EKUEF ( HANDLE )
C
C            END DO
C
C$ Restrictions
C
C     1)  EK files containing columns having the same name but
C         inconsistent declarations are not diagnosed.  Such kernels
C         are invalid in any case.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB)
C
C        Bug fix:   When an already loaded kernel is opened with EKOPR,
C        it now has its link count reset to 1 via a call to EKCLS.
C        
C-    SPICELIB Version 1.0.1, 07-JUL-1996 (NJB)
C
C        Previous version line was changed from "Beta" to "SPICELIB."  
C
C-    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB)
C
C-&
 
C$ Index_Entries
C
C     load EK file
C     load E-Kernel
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKLEF' )
      END IF
 
C
C     Here's a brief overview of what follows:
C
C        -- We do some once-per-program run initializations.
C
C        -- We do some simple error checks.  We need to make sure
C           that DAS can load the file, and that the EK architecture is
C           the right kind.
C
C        -- We need to make sure that there's enough space in our
C           data structures to hold the information about the new
C           EK.  Some of these checks are simple; we do these first.
C           However, checking that we have enough room in the column
C           table is best done by simply loading the column data into
C           the table.  If we run out of room, we abort the load.
C
C        -- We also need to make sure that the column attributes for
C           any two columns with the same name in the same table are
C           identical.  This is easy to do if the attributes for every
C           column we've encountered have been loaded into the column
C           table.
C
C        -- We save the table name and column names and attributes for
C           each new table we encounter.  For each table, we maintain a
C           list of handles of files that contain segments in that
C           table.
C
C        -- We make a segment table entry for each segment we find.
C
C        -- We save the column descriptor for each column we find,
C           associating it with the segment table entry for the segment
C           containing the column.  The column descriptor entries are
C           linked together in the same order that the corresponding
C           column names appear in the parent table's column name list;
C           this order is not necessarily the order that the columns
C           have within the segment.
C
C        -- We maintain a list of handles of loaded EKs.
C
C        If we run out of room in the column table, we clean up our
C        mess.  This means removing the current file's contributions
C        to the column table, segment table, file table, and if
C        necessary, the table list.
C
C
C     On the first pass through this routine, initialize the tables,
C     if it hasn't been done yet.
C
      IF ( FIRST ) THEN
C
C        Initialize the file table pool, segment table pool, column
C        descriptor pool, column table pool, and table list pool.
C
         CALL LNKINI ( FTSIZE, FTPOOL )
         CALL LNKINI ( STSIZE, STPOOL )
         CALL LNKINI ( DTSIZE, DTPOOL )
         CALL LNKINI ( CTSIZE, CTPOOL )
         CALL LNKINI ( MXTBLD, TBPOOL )
 
         FTHEAD = 0
         TBHEAD = 0
 
         FIRST  = .FALSE.
 
      END IF
 
C
C     Open the EK file for read access.  Bail out now if this doesn't
C     work.  This retreat will protect the various tables from
C     corruption.
C
      CALL EKOPR ( FNAME, HANDLE )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'EKLEF' )
         RETURN
      END IF
 
C
C     Check to see whether the named EK has already been loaded.
C     If so, we've added another link to the EK, which must be
C     removed.
C
      I = FTHEAD
 
      DO WHILE ( I .GT. 0 )
 
         IF ( HANDLE .EQ. FTHAN(I) ) THEN
C
C           The last call we made to EKOPR added another link to
C           the EK file.  Remove this link.
C
            CALL DASCLS ( HANDLE )

            CALL CHKOUT ( 'EKLEF' )
            RETURN

         END IF
 
         I = LNKNXT ( I, FTPOOL )
 
      END DO
 
C
C     Nothing doing unless the architecture is correct.  This file
C     should be a paged DAS EK.
C
      CALL ZZEKPGCH ( HANDLE, 'READ' )
 
C
C     Before getting too involved with this new EK file, let's check it
C     out.  We must have enough room to accommodate it in the file
C     table, segment table, table list, and column table.
C
C     Make sure there's enough room in the file table.
C
 
      IF (  LNKNFN(FTPOOL) .EQ. 0 ) THEN
C
C        Sorry, there are no free file table entries left.
C
         CALL DASHLU ( HANDLE,   UNIT                                  )
         CALL EKCLS  ( HANDLE                                          )
         CALL SETMSG ( 'The EK file # could not be loaded; the '      //
     .                 'maximum number of loaded EKs has already '    //
     .                 'been reached.'                                 )
         CALL ERRFNM ( '#',      UNIT                                  )
         CALL SIGERR ( 'SPICE(EKFILETABLEFULL)'                        )
         CALL CHKOUT ( 'EKLEF'                                         )
         RETURN
 
      END IF
 
C
C     Find out how many segments are in the new kernel, and make
C     sure there's enough room in the segment table.
C
      NSEG = EKNSEG ( HANDLE )
 
      IF ( NSEG .GT. LNKNFN(STPOOL) ) THEN
C
C        There are too many segments for the amount of space we've got
C        left.
C
         CALL DASHLU ( HANDLE,   UNIT                                  )
         CALL EKCLS  ( HANDLE                                          )
         CALL SETMSG ( 'The EK file # could not be loaded; the '      //
     .                 'maximum number of loaded segments has '       //
     .                 'already been reached.'                         )
         CALL ERRFNM ( '#',      UNIT                                  )
         CALL SIGERR ( 'SPICE(EKSEGTABLEFULL)'                         )
         CALL CHKOUT ( 'EKLEF'                                         )
         RETURN
 
      ELSE IF ( NSEG .LT. 1 ) THEN
 
         CALL DASHLU ( HANDLE,   UNIT                                  )
         CALL EKCLS  ( HANDLE                                          )
         CALL SETMSG ( 'The EK file # contains no segments.'           )
         CALL ERRFNM ( '#',      UNIT                                  )
         CALL SIGERR ( 'SPICE(EKNOSEGMENTS)'                           )
         CALL CHKOUT ( 'EKLEF'                                         )
         RETURN
 
      END IF
 
 
C
C     At this point, the file has insinuated itself into our confidence,
C     justified or not.  We'll attempt to load the segment and column
C     tables, and we'll update the table list if new tables are
C     introduced in this file.
C
      SEG    =   1
      STATE  =  'LOAD_FILE_TABLE'
 
      DO WHILE ( STATE .NE. 'DONE' )
 
 
         IF ( STATE .EQ. 'LOAD_FILE_TABLE' ) THEN
C
C           Allocate a file table entry and link the new entry in before
C           the current head of the list.  Update the list head pointer.
C           Record the file handle in the new file table entry.
C
            CALL LNKAN  ( FTPOOL, NEW            )
            CALL LNKILB ( NEW,    FTHEAD, FTPOOL )
 
            FTHEAD         =   NEW
            FTHAN ( NEW )  =   HANDLE
 
            STATE          =  'SUMMARIZE_SEGMENT'
 
 
 
         ELSE IF ( STATE .EQ. 'SUMMARIZE_SEGMENT' ) THEN
C
C           Get the summary information for this segment.
C
            CALL ZZEKSINF (  HANDLE,  SEG,     TABNAM,
     .                       SEGDSC,  CNAMS,   CDSCRS  )
 
            NCOLS  =  SEGDSC(NCIDX)
 
C
C           Before going further, check the segment for duplicate
C           column names.  Bail out if we find any.
C
            CALL SSIZEC ( MXCLLD,        CNMSET    )
            CALL MOVEC  ( CNAMS,  NCOLS, CNMSET(1) )
            CALL VALIDC ( MXCLLD, NCOLS, CNMSET    )
 
            IF ( CARDC(CNMSET) .LT. NCOLS ) THEN
 
               STATE   =  'ABORT'
               PROBLM  =  'DUPLICATE_COLUMN_NAMES'
 
            ELSE
 
               STATE   =  'FIND_TABLE'
 
            END IF
 
 
 
         ELSE IF ( STATE .EQ. 'FIND_TABLE' ) THEN
C
C           Traverse the table list, checking for a match.
C
            TBCURR  =  TBHEAD
            PRESNT  =  .FALSE.
 
            DO WHILE (  ( TBCURR .GT. 0 ) .AND. ( .NOT. PRESNT ) )
 
               IF ( TABNAM .EQ. TBNAMS(TBCURR) ) THEN
                  PRESNT =  .TRUE.
               ELSE
                  TBCURR      =  LNKNXT ( TBCURR, TBPOOL )
               END IF
 
            END DO
 
C
C           If TABNAM is the name of a table we know about, go on to
C           fill out the segment list entry for the current segment.
C           If we didn't find TABNAM, we have a new table.  Make a table
C           list entry for it.
C
            IF ( PRESNT ) THEN
C
C              Before going further, make sure the number of columns
C              in the segment matches the number of columns in the
C              parent table.
C
               IF ( NCOLS .NE. TBNCOL(TBCURR) ) THEN
 
                  NPCOL  =  TBNCOL(TBCURR)
                  STATE  = 'ABORT'
                  PROBLM = 'COLUMN_NUMBER_MISMATCH'
 
               ELSE
C
C                 Add the current file to the list of files containing
C                 the current table.
C
                  TBFILS ( 1, TBCURR )  =  HANDLE
                  TBFLSZ (    TBCURR )  =  TBFLSZ(TBCURR) + 1
 
                  STATE = 'MAKE_SEGMENT_TABLE_ENTRY'
 
               END IF
 
            ELSE
C
C              This segment belongs to a new table.
C
               STATE = 'MAKE_TABLE_LIST_ENTRY'
 
            END IF
 
 
 
         ELSE IF ( STATE .EQ. 'MAKE_TABLE_LIST_ENTRY' ) THEN
C
C           Allocate a table list entry, if we can.
C
 
            IF ( LNKNFN(TBPOOL) .EQ. 0 ) THEN
C
C              Oops, we're out of room.
C
               STATE   =  'ABORT'
               PROBLM  =  'TABLE_LIST_FULL'
 
            ELSE
C
C              We have an entry; link it to the tail of the table list.
C              For consistency with the case in which the table entry
C              already exists, we'll call the table list node TBCURR.
C
C              If this is the first table in the table list, set the
C              table head pointer.
C
               CALL LNKAN  ( TBPOOL, TBCURR  )
 
               IF ( TBHEAD .LE. 0 ) THEN
                  TBHEAD  =  TBCURR
               ELSE
                  CALL LNKILB ( TBHEAD, TBCURR, TBPOOL )
               END IF
 
C
C              Fill in the table name.
C
               TBNAMS ( TBCURR )   =  TABNAM
C
C              Since this table is new, the file list for this table
C              contains only the handle of the current EK.
C
               TBFILS ( 1, TBCURR ) = HANDLE
               TBFLSZ (    TBCURR ) = 1
 
C
C              Initialize the column count, column table pointer, and
C              segment list pointer for this table.
C
               TBNCOL(TBCURR)   =  NCOLS
               TBCTPT(TBCURR)   =  0
               TBSTPT(TBCURR)   =  0
 
C
C              Go on to add a segment table entry for the current
C              segment.
C
               STATE  =  'MAKE_SEGMENT_TABLE_ENTRY'
 
            END IF
 
 
         ELSE IF ( STATE .EQ. 'MAKE_SEGMENT_TABLE_ENTRY' ) THEN
C
C           Add the data for the current segment to the segment
C           table.
C
C           Allocate a segment table entry.  We've already verified
C           that there's enough room.
C
            CALL LNKAN ( STPOOL, STNEW )
 
C
C           Link this segment table entry to the tail of the segment
C           list for the parent table, or, if the tail is NIL, just set
C           the segment list pointer to the current segment node.
C
            IF ( TBSTPT(TBCURR) .LE. 0 ) THEN
 
               TBSTPT(TBCURR)  =  STNEW
 
            ELSE
 
               CALL LNKILB ( TBSTPT(TBCURR), STNEW, STPOOL )
 
            END IF
 
C
C           At this point, we can fill in all elements of the segment
C           table entry except for the pointers into the column table
C           and the column base addresses.
C
            STHAN (STNEW)   =  HANDLE
            STSIDX(STNEW)   =  SEG
            STNROW(STNEW)   =  SEGDSC(NRIDX)
            STNCOL(STNEW)   =  SEGDSC(NCIDX)
            STDTPT(STNEW)   =  0
 
            CALL MOVEI ( SEGDSC, SDSCSZ, STDSCS(1,STNEW) )
 
C
C           The next step is to set up the column attributes and
C           descriptors.
C
            STATE  =  'MAKE_COLUMN_TABLE_ENTRIES'
 
 
 
         ELSE IF ( STATE .EQ. 'MAKE_COLUMN_TABLE_ENTRIES' ) THEN
 
 
            IF ( PRESNT ) THEN
C
C              If the current table was present before loading the
C              current segment, we must make sure that the attributes
C              of the columns in this segment match those of the table
C              to which the segment belongs.
C
C              We must load the column descriptors for this segment
C              in the *same order* as those for every other segment
C              in the table.  This order matches that of the columns
C              in the column attribute list for the table.
C
C              For each column in the column list of the current table,
C              check the list of column names for the current segment,
C              looking for a match.
C
               J   =   TBCTPT ( TBCURR )
 
               DO WHILE (       ( J     .GT.  0       )
     .                    .AND. ( STATE .NE. 'ABORT'  )   )
 
 
                  K  =  ISRCHC ( CTNAMS(J), NCOLS, CNAMS )
 
 
                  IF ( K .GT. 0 ) THEN
C
C                    We have a name match.  At this point, we must
C                    check that the column's other attributes---data
C                    type, size, and whether the column is
C                    indexed---match as well.  It's an error if they
C                    don't.
C
                     INDEXD = CDSCRS(IXTIDX,K) .NE. IFALSE
                     NULSOK = CDSCRS(NFLIDX,K) .NE. IFALSE
                     ATTMCH =
     .                          ( CDSCRS(CLSIDX, K) .EQ.  CTCLAS(J) )
     .                    .AND. ( CDSCRS(TYPIDX, K) .EQ.  CTTYPS(J) )
     .                    .AND. ( CDSCRS(LENIDX, K) .EQ.  CTLENS(J) )
     .                    .AND. ( CDSCRS(SIZIDX, K) .EQ.  CTSIZS(J) )
     .                    .AND. ( INDEXD            .EQV. CTINDX(J) )
     .                    .AND. ( NULSOK            .EQV. CTNULL(J) )
 
 
                     IF ( ATTMCH ) THEN
C
C                       Great, the attributes match.  Actually, the
C                       addition of the current segment can *change*
C                       one attribute of the current table:  the
C                       maximum non-blank width associated with the
C                       current column, if the column has character
C                       type.  We'll make this change after we're
C                       sure we won't have to undo it.
C
C                       Store the column descriptor for this column
C                       in the descriptor table.  We'll need to
C                       allocate a descriptor table entry first.
C
                        IF ( LNKNFN( DTPOOL ) .EQ. 0 ) THEN
C
C                          No free nodes left in the descriptor table.
C
                           STATE  = 'ABORT'
                           PROBLM = 'DESCRIPTOR_TABLE_FULL'
 
                        ELSE
C
C                          A free node is available.  Link it in
C                          at the tail of the descriptor list for
C                          the current segment.
C
                           CALL LNKAN  ( DTPOOL, DTNEW )
 
                           IF ( STDTPT(STNEW) .LE. 0 ) THEN
 
                              STDTPT(STNEW) = DTNEW
 
                           ELSE
 
                              CALL LNKILB (STDTPT(STNEW), DTNEW, DTPOOL)
 
                           END IF
 
C
C                          Fill in the descriptor.
C
                           CALL MOVEI (  CDSCRS(1,K),
     .                                   CDSCSZ,
     .                                   DTDSCS(1,DTNEW)  )
 
                        END IF
C
C                       We filled in a descriptor table entry, or
C                       else we ran out of room.
C
 
                     ELSE
C
C                       Seriously bad news.  Someone's tried to
C                       load an EK containing a column with
C                       attributes that conflict with those of a
C                       loaded column of the same name in the
C                       current table.
C
                        COLNAM  =   CTNAMS(J)
                        STATE   =  'ABORT'
                        PROBLM  =  'MISMATCHED_COLUMN_ATTRIBUTES'
 
                     END IF
 
 
                  ELSE
C
C                    No name match; the current column from the current
C                    table is not present in the segment we're looking
C                    at.
C
                     COLNAM  =   CTNAMS(J)
                     STATE   =  'ABORT'
                     PROBLM  =  'MISSING_COLUMN'
 
                  END IF
C
C                 The current column matched one in the column list
C                 for the current table, or else we have a problem.
C
C                 Advance to the next column in the table's column list.
C
                  IF ( STATE .NE. 'ABORT' ) THEN
                     J  =  LNKNXT ( J, CTPOOL )
                  END IF
 
               END DO
C
C              We've made descriptor table entries for each column in
C              the current segment, or else we have an error.
C
 
 
            ELSE
C
C              We need to set up the column attribute entries for
C              the new table introduced by loading this segment.  We
C              also need to set up descriptor table entries for the
C              segment.  We *don't* have to check the consistency of
C              the attributes of the columns.
C
               K  =  1
 
               DO WHILE (  (K .LE. NCOLS) .AND. (STATE .NE. 'ABORT')  )
C
C                 Allocate a new entry in the column attribute table and
C                 link it to the tail of the column list for the
C                 current table.  If the column list is empty, update
C                 the list head.
C
                  IF (  LNKNFN(CTPOOL) .EQ. 0 ) THEN
C
C                    There's no more space to store attribute
C                    descriptors.
C
                     STATE  =  'ABORT'
                     PROBLM =  'ATTRIBUTE_TABLE_FULL'
 
                  ELSE
 
                     CALL LNKAN  ( CTPOOL, CTNEW )
 
                     IF  ( TBCTPT(TBCURR) .LE. 0 ) THEN
 
                        TBCTPT(TBCURR)  =  CTNEW
 
                     ELSE
 
                        CALL LNKILB ( TBCTPT(TBCURR), CTNEW, CTPOOL )
 
                     END IF
 
C
C                    Fill in the new column attribute entry with the
C                    attributes for this column.
C
                     CTNAMS(CTNEW)  =  CNAMS (       K)
                     CTCLAS(CTNEW)  =  CDSCRS(CLSIDX,K)
                     CTTYPS(CTNEW)  =  CDSCRS(TYPIDX,K)
                     CTLENS(CTNEW)  =  CDSCRS(LENIDX,K)
                     CTSIZS(CTNEW)  =  CDSCRS(SIZIDX,K)
                     CTINDX(CTNEW)  =  CDSCRS(IXTIDX,K) .NE. IFALSE
                     CTFIXD(CTNEW)  =  CDSCRS(SIZIDX,K) .NE. IFALSE
                     CTNULL(CTNEW)  =  CDSCRS(NFLIDX,K) .NE. IFALSE
 
C
C                    Store the column descriptor for this column
C                    in the descriptor table.  We'll need to
C                    allocate a descriptor table entry first.
C
                     IF ( LNKNFN( DTPOOL ) .EQ. 0 ) THEN
C
C                       No free nodes left in the descriptor table.
C
                        STATE  = 'ABORT'
                        PROBLM = 'DESCRIPTOR_TABLE_FULL'
 
                     ELSE
C
C                       A free node is available.  Link it in at the
C                       tail of the descriptor list for the current
C                       segment.
C
                        CALL LNKAN  ( DTPOOL, DTNEW )
 
                        IF ( STDTPT(STNEW) .LE. 0 ) THEN
 
                           STDTPT(STNEW) = DTNEW
 
                        ELSE
 
                           CALL LNKILB ( STDTPT(STNEW), DTNEW, DTPOOL )
 
                        END IF
 
C
C                       Fill in the descriptor.
C
                        CALL MOVEI (  CDSCRS(1,K),
     .                                CDSCSZ,
     .                                DTDSCS(1,DTNEW)  )
 
                     END IF
 
                  END IF
C
C                 We created attribute and descriptor entries for the
C                 current column, or we encountered an error.
C
                  IF ( STATE .NE. 'ABORT' ) THEN
C
C                    Consider the next column.
C
                     K  =  K + 1
 
                  END IF
 
 
               END DO
C
C              We created attribute and descriptor entries for every
C              column in the current segment, or we encountered an
C              error.
C
 
            END IF
C
C           We've processed the current segment in the new file, or
C           else we have an error condition.
C
 
            IF ( STATE .NE. 'ABORT' ) THEN
C
C              We're ready to look at the next segment in the new file.
C
               STATE  =  'NEXT_SEGMENT'
 
            END IF
 
 
 
         ELSE IF ( STATE .EQ. 'NEXT_SEGMENT' ) THEN
 
 
            IF ( SEG .LT. NSEG ) THEN
 
               SEG    =   SEG + 1
               STATE  =   'SUMMARIZE_SEGMENT'
 
            ELSE
C
C              We're done with all of the segments.
C
               STATE  =  'DONE'
 
            END IF
 
 
         ELSE IF ( STATE .EQ. 'ABORT' ) THEN
C
C           We must clean up all the data structure additions we made to
C           accommodate the new file.
C
C           Basically, we unload the new file.  We defer the call to
C           EKCLS until after we've reported the error.
C
C           The file table is first.  The file is at the head of the
C           list.  If the file has a successor, that file is now at the
C           head of the list.
C
 
            FTHEAD  =  LNKNXT ( NEW, FTPOOL )
 
 
            IF ( FTHEAD .LT. 0 ) THEN
C
C              There are no files left.  Clean up the whole shebang.
C
               CALL LNKINI ( FTSIZE, FTPOOL )
               CALL LNKINI ( STSIZE, STPOOL )
               CALL LNKINI ( DTSIZE, DTPOOL )
               CALL LNKINI ( CTSIZE, CTPOOL )
               CALL LNKINI ( MXTBLD, TBPOOL )
 
               FTHEAD  =  0
               TBHEAD  =  0
 
 
            ELSE
C
C              If we arrived here, the file we're unloading is not the
C              only loaded file.
C
C              Free the file table entry for the file.  The entry can be
C              regarded as a sublist that starts and ends with the Ith
C              node, so we can call the `free sublist' routine to get
C              rid of it.
C
               CALL LNKFSL ( NEW, NEW, FTPOOL )
 
C
C              It's time to clean up the table list, segment table,
C              column attribute table, and column descriptor table.  The
C              plan is to traverse the table list, and for each member
C              of the list, traverse the corresponding segment list,
C              removing from the list information about segments and
C              columns in the file we're unloading.  If the segment list
C              for any table becomes empty, we remove the entry for that
C              table from the table list.
C
               TBCURR = TBHEAD
 
               DO WHILE ( TBCURR .GT. 0 )
C
C                 See whether the current table is in the file we're
C                 unloading.
C
                  I  =  1
 
                  DO WHILE (       ( I .LE.  TBFLSZ(TBCURR) )
     .                       .AND. (   .NOT. FND            )  )
 
                     IF ( TBFILS(I,TBCURR) .EQ. HANDLE ) THEN
C
C                       This table is affected by unloading the file.
C
                        FND  =  .TRUE.
 
                     ELSE
C
C                       Look at the next file handle.
C
                        I    =   I + 1
 
                     END IF
 
                  END DO
 
 
                  IF ( FND ) THEN
C
C                    Update the information for the current table to
C                    reflect the unloading of the specified EK.
C
C                    Unloading the specified EK removes one handle from
C                    the list of file handles associated with this
C                    table.  Compress this handle out of the list.
C
                     DO J  =  I,   TBFLSZ(TBCURR) - 1
 
                        TBFILS( J, TBCURR )  =  TBFILS( J+1, TBCURR )
 
                     END DO
 
                     TBFLSZ(TBCURR)  =  TBFLSZ(TBCURR) - 1
 
C
C                    Traverse the segment list for this table, looking
C                    for segments in the specified EK.
C
                     DELSEG  =  TBSTPT ( TBCURR )
 
                     DO WHILE ( DELSEG .GT. 0 )
 
                        IF ( STHAN(DELSEG) .EQ. HANDLE ) THEN
C
C                          This segment is aboard the sinking ship.  Put
C                          it out of its misery.
C
C                          First, euthanize its column descriptors.
C                          These descriptors are linked together, so we
C                          can free all of them in one shot.
C
                           J  =  STDTPT ( DELSEG )
 
                           IF ( J .GT. 0 ) THEN
                              K  =  LNKTL  ( J,    DTPOOL )
                              CALL LNKFSL  ( J, K, DTPOOL )
                           END IF
 
C
C                          Now we can delete the segment table entry
C                          itself.  This deletion may necessitate
C                          updating the segment list pointer in the
C                          parent table's table list entry.
C
                           IF ( DELSEG .EQ. TBSTPT(TBCURR) ) THEN
 
                              TBSTPT(TBCURR) =  LNKNXT( DELSEG, STPOOL )
 
                           END IF
 
 
                           NEXT = LNKNXT ( DELSEG,         STPOOL )
                           CALL LNKFSL   ( DELSEG, DELSEG, STPOOL )
 
C
C                          The segment we just freed may have been the
C                          last one belonging to this table.  We deal
C                          with this possibility later, below the end of
C                          the current loop.
C
                           DELSEG  = NEXT
 
                        ELSE
 
                           DELSEG  = LNKNXT ( DELSEG, STPOOL )
 
                        END IF
 
 
                     END DO
 
C
C                    We've examined all of the segments in the current
C                    table.
C
C                    If the segment list for the current table became
C                    empty as a result of our having plundered the
C                    segment table, delete the entry for this table from
C                    the table list. We do *not* need to concern
C                    ourselves with the possibility that this deletion
C                    will empty the table list, since we know we're
C                    not unloading the last loaded file.  However, we
C                    may need to update the head-of-list pointer for the
C                    table list.
C
                     IF ( TBSTPT(TBCURR) .LE. 0 ) THEN
C
C                       There are no loaded segments left for this
C                       table.
C
C                       Delete the list of column attribute entries for
C                       the columns in this table, then delete the
C                       table's entry from the table list.
C
C                       The column attribute entries are linked, so we
C                       can free them in one shot.  Don't crash if the
C                       column attribute list is empty.
C
                        J  =  TBCTPT ( TBCURR )
 
                        IF ( J .GT. 0 ) THEN
 
                           K  =  LNKTL  ( J,    CTPOOL )
                           CALL LNKFSL  ( J, K, CTPOOL )
 
                        END IF
 
 
                        IF ( TBCURR .EQ. TBHEAD ) THEN
C
C                          The entry for this table is at the head of
C                          the table list.  Update the head of the list.
C
                           TBHEAD  =  LNKNXT ( TBCURR, TBPOOL )
                           NEXT    =  TBHEAD
 
                        ELSE
 
                           NEXT    =  LNKNXT ( TBCURR, TBPOOL )
 
                        END IF
 
C
C                       Make the entry for this table go away.
C
                        CALL LNKFSL ( TBCURR, TBCURR, TBPOOL )
 
C
C                       Look at the next table.
C
                        TBCURR  =  NEXT
 
 
                     ELSE
C
C                       We're done with the current table.  Look at the
C                       next one.
C
                        TBCURR = LNKNXT ( TBCURR, TBPOOL )
 
                     END IF
C
C                    We've cleaned up the table entry for the current
C                    table, if it was necessary to do so.
C
 
                  ELSE
C
C                    The current table is not affected by unloading this
C                    file.  Examine the next table.
C
                     TBCURR = LNKNXT ( TBCURR, TBPOOL )
 
                  END IF
 
C
C                 We've processed the current table.
C
               END DO
 
 
            END IF
 
C
C           We've cleaned up after the aborted partial load.
C
C           Now that the mess has been arranged, tell the user what the
C           problem was.
C
            CALL DASHLU ( HANDLE, UNIT )
 
            IF ( PROBLM .EQ. 'TABLE_LIST_FULL' ) THEN
 
               CALL SETMSG ( 'The EK file # could not be loaded; the '//
     .                       'maximum number of distinct tables has ' //
     .                       'already been reached.'                   )
               CALL ERRFNM ( '#',      UNIT                            )
               CALL SIGERR ( 'SPICE(EKTABLELISTFULL)'                  )
 
 
            ELSE IF ( PROBLM .EQ. 'DUPLICATE_COLUMN_NAMES' ) THEN
 
               CALL SETMSG ( 'The EK file # could not be loaded; the '//
     .                       'segment # contains duplicate column '   //
     .                       'names in table #.'                       )
               CALL ERRFNM ( '#',      UNIT                            )
               CALL ERRINT ( '#',      SEG                             )
               CALL ERRCH  ( '#',      TABNAM                          )
               CALL SIGERR ( 'SPICE(EKCOLNUMMISMATCH)'                 )
 
 
            ELSE IF ( PROBLM .EQ. 'COLUMN_NUMBER_MISMATCH' ) THEN
 
               CALL SETMSG ( 'The EK file # could not be loaded; the '//
     .                       'number of columns (#) in segment # '    //
     .                       'does not match the number of columns '  //
     .                       '(#) in the parent table #.'              )
               CALL ERRFNM ( '#',      UNIT                            )
               CALL ERRINT ( '#',      NCOLS                           )
               CALL ERRINT ( '#',      SEG                             )
               CALL ERRINT ( '#',      NPCOL                           )
               CALL ERRCH  ( '#',      TABNAM                          )
               CALL SIGERR ( 'SPICE(EKCOLNUMMISMATCH)'                 )
 
 
            ELSE IF ( PROBLM .EQ. 'MISMATCHED_COLUMN_ATTRIBUTES' ) THEN
 
               CALL SETMSG ( 'EK file # contains a column '           //
     .                       'whose attributes conflict with a '      //
     .                       'loaded column.  The offending '         //
     .                       'column name is #; the column is '       //
     .                       'in segment #* of the file.'              )
               CALL ERRFNM ( '#',  UNIT                                )
               CALL ERRCH  ( '#',  COLNAM                              )
               CALL ERRINT ( '*',  SEG                                 )
               CALL SIGERR ( 'SPICE(BADATTRIBUTES)'                    )
 
 
            ELSE IF ( PROBLM .EQ. 'DESCRIPTOR_TABLE_FULL' ) THEN
 
               CALL SETMSG ( 'The EK file # could not be loaded; the' //
     .                        'maximum allowed number of loaded '     //
     .                        'columns already been reached.'          )
               CALL ERRFNM ( '#',      UNIT                            )
               CALL SIGERR ( 'SPICE(COLDESCTABLEFULL)'                 )
 
 
            ELSE IF ( PROBLM .EQ. 'ATTRIBUTE_TABLE_FULL' ) THEN
 
               CALL SETMSG ( 'The EK file # could not be loaded; the '//
     .                       'maximum number of columns having'       //
     .                       'distinct attributes has already been '  //
     .                       'reached.'                                )
               CALL ERRFNM ( '#',      UNIT                            )
               CALL SIGERR ( 'SPICE(EKCOLATTRTABLEFULL)'               )
 
 
            ELSE IF ( PROBLM .EQ. 'MISSING_COLUMN' ) THEN
 
               CALL SETMSG ( 'The EK file # could not be loaded; the '//
     .                       'column # in already loaded table # is ' //
     .                       'not present in segment # in the EK file.')
               CALL ERRFNM ( '#',      UNIT                            )
               CALL ERRCH  ( '#',      COLNAM                          )
               CALL ERRCH  ( '#',      TABNAM                          )
               CALL ERRINT ( '#',      SEG                             )
               CALL SIGERR ( 'SPICE(EKMISSINGCOLUMN)'                  )
 
 
            ELSE
 
               CALL SETMSG ( 'The EK file # could not be loaded; the '//
     .                       'problem "#" occurred while attempting ' //
     .                       'to load the file.  By way, there is a ' //
     .                       'bug in EKLEF if you see this message.'   )
               CALL ERRFNM ( '#',      UNIT                            )
               CALL ERRCH  ( '#',      PROBLM                          )
               CALL SIGERR ( 'SPICE(BUG)'                              )
 
            END IF
 
 
            CALL EKCLS  (  HANDLE )
 
            CALL CHKOUT ( 'EKLEF' )
            RETURN
 
         END IF
 
 
      END DO
C
C     At this point, we've made the file table, table list, segment
C     table, column descriptor table, and column attribute table updates
C     necessary to reflect the presence of the new file.
C
 
      CALL CHKOUT ( 'EKLEF' )
      RETURN
 
 
 
 
 
 
 
 
C$Procedure     EKUEF  ( EK, unload event file )
 
      ENTRY EKUEF ( HANDLE )
 
C$ Abstract
C
C     Unload an EK file, making its contents inaccessible to the
C     EK reader routines, and clearing space in order to allow other
C     EK files to be loaded.
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
C     EK
C
C$ Keywords
C
C     EK
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
C     HANDLE     I   Handle of EK file.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle returned by EKLEF.
C
C$ Detailed_Output
C
C     None.  See $Particulars for a description of the effect of this
C     routine.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  Unloading a file that is not loaded has no effect.
C
C$ Files
C
C     See the description of the input argument HANDLE in
C     $Detailed_Input.
C
C$ Particulars
C
C     This routine removes information about an EK file from the
C     EK system, freeing space to increase the number of other EK
C     files that can be loaded.  The file is also unloaded from
C     the DAS system and closed.
C
C$ Examples
C
C     1)  Load 25 EK files sequentially, unloading the previous file
C         before each new file is loaded.  Unloading files prevents
C         them from being searched during query execution.
C
C            DO I = 1, 25
C
C               CALL EKLEF ( EK(I), HANDLE )
C
C               [Perform queries]
C
C               CALL EKUEF ( HANDLE )
C
C            END DO
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
C
C$ Version
C 
C-    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB)
C
C        Bug fix:   When an already loaded kernel is opened with EKOPR,
C        it now has its link count reset to 1 via a call to EKCLS.
C        
C-    SPICELIB Version 1.0.1, 07-JUL-1996 (NJB)
C
C        Previous version line was changed from "Beta" to "SPICELIB."  
C
C-    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB)
C
C-&
 
C$ Index_Entries
C
C     unload EK file
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKUEF' )
      END IF
 
C
C     On the first pass through this routine, initialize the tables,
C     if it hasn't been done yet.
C
      IF ( FIRST ) THEN
C
C        Initialize the file table pool, segment table pool, column
C        descriptor pool, column table pool, and table list pool.
C
         CALL LNKINI ( FTSIZE, FTPOOL )
         CALL LNKINI ( STSIZE, STPOOL )
         CALL LNKINI ( DTSIZE, DTPOOL )
         CALL LNKINI ( CTSIZE, CTPOOL )
         CALL LNKINI ( MXTBLD, TBPOOL )
 
         FTHEAD = 0
         TBHEAD = 0
 
         FIRST  = .FALSE.
 
      END IF
 
C
C     Check to see whether the named EK has been loaded.  Do nothing
C     if not.
C
      I    =   FTHEAD
      FND  =  .FALSE.
 
      DO WHILE (  ( I .GT. 0 ) .AND. ( .NOT. FND )  )
 
         IF ( HANDLE .EQ. FTHAN(I) ) THEN
            FND  =  .TRUE.
         ELSE
            I    =  LNKNXT ( I, FTPOOL )
         END IF
 
      END DO
 
      IF ( .NOT. FND ) THEN
         CALL CHKOUT ( 'EKUEF' )
         RETURN
      END IF
 
C
C     If we got to here, HANDLE points to a loaded EK file.  It's
C     time to wipe from the EK tables all trivial fond records
C     pertaining to the file in question.
C
C     The file table is first.
C
      IF ( I .EQ. FTHEAD ) THEN
C
C        The file is at the head of the list.  If the file has a
C        successor, that file is now at the head of the list.
C
         FTHEAD  =  LNKNXT ( I, FTPOOL )
 
         IF ( FTHEAD .LT. 0 ) THEN
C
C           There are no files left.  Clean up the whole shebang.
C
            CALL LNKINI ( FTSIZE, FTPOOL )
            CALL LNKINI ( STSIZE, STPOOL )
            CALL LNKINI ( DTSIZE, DTPOOL )
            CALL LNKINI ( CTSIZE, CTPOOL )
            CALL LNKINI ( MXTBLD, TBPOOL )
 
            FTHEAD  =  0
            TBHEAD  =  0
 
C
C           Close the EK file, to keep the DAS system's bookkeeping
C           up to date.
C
            CALL EKCLS  ( HANDLE )
 
            CALL CHKOUT ( 'EKUEF' )
            RETURN
 
         END IF
 
      END IF
 
C
C     If we arrived here, the file we're unloading is not the only
C     loaded file.
C
C     Free the file table entry for the file.  The entry can be
C     regarded as a sublist that starts and ends with the Ith node,
C     so we can call the `free sublist' routine to get rid of it.
C
      CALL LNKFSL ( I, I, FTPOOL )
 
C
C     It's time to clean up the table list, segment table, column
C     attribute table, and column descriptor table.  The plan is
C     to traverse the table list, and for each member of the list,
C     traverse the corresponding segment list, removing from the list
C     information about segments and columns in the file we're
C     unloading.  If the segment list for any table becomes empty, we
C     remove the entry for that table from the table list.
C
      TBCURR = TBHEAD
 
      DO WHILE ( TBCURR .GT. 0 )
C
C        See whether the current table is in the file we're unloading.
C
            I  =  1
 
            DO WHILE (  ( I .LE. TBFLSZ(TBCURR) ) .AND. ( .NOT. FND )  )
 
            IF ( TBFILS(I,TBCURR) .EQ. HANDLE ) THEN
C
C              This table is affected by unloading the file.
C
               FND  =  .TRUE.
 
            ELSE
C
C              Look at the next file handle.
C
               I    =   I + 1
 
            END IF
 
         END DO
 
 
         IF ( FND ) THEN
C
C           Update the information for the current table to reflect
C           the unloading of the specified EK.
C
C           Unloading the specified EK removes one handle from the
C           list of file handles associated with this table.  Compress
C           this handle out of the list.
C
            DO J  =  I,   TBFLSZ(TBCURR) - 1
 
               TBFILS( J, TBCURR )  =  TBFILS( J+1, TBCURR )
 
            END DO
 
            TBFLSZ(TBCURR)  =  TBFLSZ(TBCURR) - 1
 
C
C           Traverse the segment list for this table, looking
C           for segments in the specified EK.
C
            SEG  =  TBSTPT ( TBCURR )
 
            DO WHILE ( SEG .GT. 0 )
 
               IF ( STHAN(SEG) .EQ. HANDLE ) THEN
C
C                 This segment is aboard the sinking ship.  Put it
C                 out of its misery.
C
C                 First, euthanize the segment's column descriptors.
C                 These descriptors are linked together, so we can free
C                 all of them in one shot.  Don't crash if the column
C                 descriptor list is empty.
C
                  J  =  STDTPT ( SEG )
 
                  IF ( J .GT. 0 ) THEN
                     K  =  LNKTL  ( J,    DTPOOL )
                     CALL LNKFSL  ( J, K, DTPOOL )
                  END IF
 
C
C                 Now we can delete the segment table entry itself.
C                 This deletion may necessitate updating the segment
C                 list pointer in the parent table's table list entry.
C
                  IF ( SEG .EQ. TBSTPT(TBCURR) ) THEN
 
                     TBSTPT(TBCURR)  =  LNKNXT( SEG, STPOOL )
 
                  END IF
 
 
                  NEXT = LNKNXT ( SEG,      STPOOL )
                  CALL LNKFSL   ( SEG, SEG, STPOOL )
 
                  SEG  = NEXT
 
               ELSE
 
                  SEG  = LNKNXT ( SEG, STPOOL )
 
               END IF
 
 
            END DO
 
C
C           We've examined all of the segments in the current table.
C
C           If the segment list for the current table became empty
C           as a result of our having plundered the segment table,
C           delete the entry for this table from the table list.  We do
C           *not* need to concern ourselves with the possibility that
C           this deletion will empty the table list, since we know we're
C           not unloading the last loaded file.  However, we may need to
C           update the head-of-list pointer for the table list.
C
            IF ( TBSTPT(TBCURR) .LE. 0 ) THEN
C
C              There are no loaded segments left for this table.
C
C              Delete the list of column attribute entries for the
C              columns in this table, then delete the table's entry from
C              the table list.
C
C              The column attribute entries are linked, so we can free
C              them in one shot.
C
               J  =  TBCTPT ( TBCURR )
 
               IF ( J .GT. 0 ) THEN
                  K  =  LNKTL  ( J,    CTPOOL )
                  CALL LNKFSL  ( J, K, CTPOOL )
               END IF
 
 
               IF ( TBCURR .EQ. TBHEAD ) THEN
C
C                 The entry for this table is at the head of the
C                 table list.  Update the head of the list.
C
                  TBHEAD  =  LNKNXT ( TBCURR, TBPOOL )
                  NEXT    =  TBHEAD
 
               ELSE
 
                  NEXT    =  LNKNXT ( TBCURR, TBPOOL )
 
               END IF
 
C
C              Make the entry for this table go away.
C
               CALL LNKFSL ( TBCURR, TBCURR, TBPOOL )
 
C
C              The successor of the current node is the next node to
C              examine.
C
               TBCURR  =  NEXT
 
 
            ELSE
C
C              We're done with the current table.  Look at the next one.
C
               TBCURR = LNKNXT ( TBCURR, TBPOOL )
 
            END IF
C
C           We've cleaned up the table entry for the current table,
C           if it was necessary to do so.
C
 
         ELSE
C
C           The current table is not affected by unloading this file.
C           Examine the next table.
C
            TBCURR = LNKNXT ( TBCURR, TBPOOL )
 
         END IF
C
C        We've processed the current table.
C
      END DO
 
C
C     Don't forget to unload the EK file from the DAS system.
C
      CALL EKCLS  ( HANDLE )
 
      CALL CHKOUT ( 'EKUEF' )
      RETURN
 
 
 
 
 
 
 
C$Procedure     EKNTAB  ( EK, return number of loaded tables )
 
      ENTRY EKNTAB ( N )
 
C$ Abstract
C
C     Return the number of loaded EK tables.
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
C     EK
C
C$ Keywords
C
C     EK
C     FILES
C
C$ Declarations
C
C     INTEGER               N
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     N          O   Number of loaded tables.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     N              is the number of loaded tables.  The count refers
C                    to the number of logical tables; if multiple
C                    segments contain data for the same table, these
C                    segments collectively contribute only one table
C                    to the count.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     The returned count is based on the currently loaded EK files.
C     These files must be loaded via the entry point EKLEF.
C
C$ Particulars
C
C     This routine is a utility that provides the caller with the
C     number of loaded tables.  Callers of EKTNAM can use this count
C     as the upper bound on set of table indices when looking up table
C     names.
C
C$ Examples
C
C     1)  Suppose we have the following list of EK files and tables
C         contained in those files:
C
C            File name        Table name
C            ---------        ----------
C
C            FILE_1.EK        TABLE_1
C                             TABLE_2
C
C            FILE_2.EK        TABLE_1
C                             TABLE_3
C
C            FILE_3.EK        TABLE_2
C                             TABLE_3
C                             TABLE_4
C
C
C         Then after loading these files, the call
C
C            CALL EKNTAB ( N )
C
C         returns the value N = 4.
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
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB)
C
C        Bug fix:   When an already loaded kernel is opened with EKOPR,
C        it now has its link count reset to 1 via a call to EKCLS.
C
C-    SPICELIB Version 1.0.1, 07-JUL-1996 (NJB)
C
C        Previous version line was changed from "Beta" to "SPICELIB."  
C
C-    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB)
C
C-&
 
C$ Index_Entries
C
C     return number of loaded tables
C
C-&
 
 
      IF ( FIRST ) THEN
C
C        Initialize the file table pool, segment table pool, column
C        descriptor pool, column table pool, and table list pool.
C
         CALL LNKINI ( FTSIZE, FTPOOL )
         CALL LNKINI ( STSIZE, STPOOL )
         CALL LNKINI ( DTSIZE, DTPOOL )
         CALL LNKINI ( CTSIZE, CTPOOL )
         CALL LNKINI ( MXTBLD, TBPOOL )
 
         FTHEAD = 0
         TBHEAD = 0
 
         FIRST  = .FALSE.
 
      END IF
C
C     Return the number of loaded tables.
C
      N  =   MXTBLD - LNKNFN( TBPOOL )
      RETURN
 
 
 
 
 
 
C$Procedure     EKTNAM  ( EK, return name of loaded table )
 
      ENTRY EKTNAM ( N, TABLE )
 
C$ Abstract
C
C     Return the name of a specified, loaded table.
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
C     EK
C
C$ Keywords
C
C     EK
C     FILES
C
C$ Declarations
C
C     INTEGER               N
C     CHARACTER*(*)         TABLE
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     N          I   Index of table.
C     TABLE      O   Name of table.
C
C$ Detailed_Input
C
C     N              is the index of the table whose name is desired.
C                    The value of N ranges from 1 to the number of
C                    loaded tables, which count may be obtained from
C                    EKNTAB.
C
C$ Detailed_Output
C
C     TABLE          is the name of the Nth loaded table.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If this routine is called when no files are loaded, the
C         error SPICE(NOLOADEDFILES) is signaled.
C
C     2)  If the input N is out of range, the error SPICE(INVALDINDEX)
C         is signaled.
C
C$ Files
C
C     The returned name is based on the currently loaded EK files.
C
C$ Particulars
C
C     This routine is a utility that provides the caller with the
C     name of a specified loaded table.  The index of a table with
C     a given name depends on the kernels loaded and possibly on
C     the order in which the files have been loaded.
C
C$ Examples
C
C     1)  Dump the names of the loaded tables.
C
C         CALL EKNTAB ( N )
C
C         DO I = 1, N
C            CALL EKTNAM ( I, TABLE )
C            WRITE (*,*) TABLE
C         END DO
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
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB)
C
C        Bug fix:   When an already loaded kernel is opened with EKOPR,
C        it now has its link count reset to 1 via a call to EKCLS.
C
C-    SPICELIB Version 1.0.1, 07-JUL-1996 (NJB)
C
C        Previous version line was changed from "Beta" to "SPICELIB."  
C
C-    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB)
C
C-&
 
C$ Index_Entries
C
C     return name of a loaded table
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKTNAM' )
      END IF
 
      IF ( FIRST ) THEN
C
C        Initialize the file table pool, segment table pool, column
C        descriptor pool, column table pool, and table list pool.
C
         CALL LNKINI ( FTSIZE, FTPOOL )
         CALL LNKINI ( STSIZE, STPOOL )
         CALL LNKINI ( DTSIZE, DTPOOL )
         CALL LNKINI ( CTSIZE, CTPOOL )
         CALL LNKINI ( MXTBLD, TBPOOL )
 
         FTHEAD = 0
         TBHEAD = 0
 
         FIRST  = .FALSE.
 
      END IF
 
C
C     There nothing to fetch if no files are loaded.  A sure
C     symptom of this problem is that the file list is empty.
C
      IF ( FTHEAD .LE. 0 ) THEN
         CALL SETMSG ( 'No E-kernels are currently loaded.' )
         CALL SIGERR ( 'SPICE(NOLOADEDFILES)'               )
         CALL CHKOUT ( 'EKTNAM'                             )
         RETURN
      END IF
 
 
      TBCURR  =  TBHEAD
      FND     =  .FALSE.
      I       =  0
 
      DO WHILE (  ( TBCURR .GT. 0 ) .AND. ( .NOT. FND )  )
 
         I   =  I + 1
 
         IF ( I .EQ. N ) THEN
            FND     =  .TRUE.
            TABLE   =   TBNAMS(TBCURR)
         ELSE
            TBCURR  =   LNKNXT ( TBCURR, TBPOOL )
         END IF
 
      END DO
 
      IF ( .NOT. FND ) THEN
         CALL SETMSG ( 'The index # does not correspond to a loaded ' //
     .                 'table.'                                        )
         CALL ERRINT ( '#',  N                                         )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                           )
      END IF
 
 
      CALL CHKOUT ( 'EKTNAM' )
      RETURN
 
 
 
 
 
 
 
 
 
C$Procedure     EKCCNT  ( EK, column count )
 
      ENTRY EKCCNT ( TABLE, CCOUNT )
 
C$ Abstract
C
C     Return the number of distinct columns in a specified, currently
C     loaded table
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
C     EK
C
C$ Keywords
C
C     EK
C     FILES
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         TABLE
C     INTEGER               CCOUNT
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TABLE      I   Name of table.
C     CCOUNT     O   Count of distinct, currently loaded columns.
C
C$ Detailed_Input
C
C     TABLE          is the name of a currently loaded table.  Case
C                    is not significant in the table name.
C
C$ Detailed_Output
C
C     CCOUNT         is the number of distinct columns in TABLE.
C                    Columns that have the same name but belong to
C                    different segments that are considered to be
C                    portions of the same column, if the segments
C                    containing those columns belong to TABLE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the specified table is not loaded, the error
C         SPICE(TABLENOTLOADED) is signaled.
C
C$ Files
C
C     See the header of EKQMGR for a description of files used
C     by this routine.
C
C$ Particulars
C
C     This routine is a utility intended for use in conjunction with
C     the entry point EKCII.  These routines can be used to find the
C     names and attributes of the columns that are currently loaded.
C
C$ Examples
C
C     1)  Dump the names and attributes of the columns in each loaded
C         table.  EKCCNT is used to obtain column counts.
C
C            C
C            C     Get the number of loaded tables.
C            C
C                  CALL EKNTAB ( NTAB )
C
C                  DO TAB = 1, NTAB
C            C
C            C        Get the name of the current table, and look up
C            C        the column count for this table.
C            C
C                     CALL EKTNAM ( TAB,    TABNAM )
C                     CALL EKCCNT ( TABNAM, NCOLS  )
C
C                     WRITE (*,*) 'TABLE = ', TABNAM
C                     WRITE (*,*) ' '
C
C            C
C            C        For each column in the current table, look up the
C            C        column's attributes.  The attribute block
C            C        index parameters are defined in the include file
C            C        ekattdsc.inc.
C            C
C                     DO I = 1, NCOLS
C
C                        CALL EKCII ( TABNAM, I, COLNAM, ATTDSC )
C
C                        WRITE (*,*) 'COLUMN = ', COLNAM
C
C            C
C            C           Write out the current column's data type.
C            C
C                        IF ( ATTDSC(ATTTYP) .EQ. CHR ) THEN
C                           WRITE (*,*) 'TYPE   =  CHR'
C
C                           IF ( ATTDSC(ATTLEN) .EQ. -1 ) THEN
C                              WRITE (*,*) 'STRING LENGTH = VARIABLE.'
C                           ELSE
C                              WRITE (*,*) 'STRING LENGTH = ',
C                 .                         ATTDSC(ATTLEN)
C                           END IF
C
C                        ELSE IF ( ATTDSC(ATTTYP) .EQ. DP ) THEN
C                           WRITE (*,*) 'TYPE   =  DP'
C
C                        ELSE IF ( ATTDSC(ATTTYP) .EQ. INT ) THEN
C                           WRITE (*,*) 'TYPE   =  INT'
C
C                        ELSE
C                           WRITE (*,*) 'TYPE   =  TIME'
C                        END IF
C
C            C
C            C           Write out the current column's entry size.
C            C
C                        WRITE (*,*) 'SIZE   = ', ATTDSC(ATTSIZ)
C
C            C
C            C           Indicate whether the current column is indexed.
C            C
C                        IF ( ATTDSC(ATTIDX) .EQ. -1 ) THEN
C                           WRITE (*,*) 'NOT INDEXED'
C                        ELSE
C                           WRITE (*,*) 'INDEXED'
C                        END IF
C
C            C
C            C           Indicate whether the current column allows
C            C           null values.
C            C
C                        IF ( ATTDSC(ATTNFL) .EQ. -1 ) THEN
C                           WRITE (*,*) 'NULL VALUES NOT ALLOWED'
C                        ELSE
C                           WRITE (*,*) 'NULL VALUES ALLOWED'
C                        END IF
C
C                     END DO
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
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB)
C
C        Bug fix:   When an already loaded kernel is opened with EKOPR,
C        it now has its link count reset to 1 via a call to EKCLS.
C
C-    SPICELIB Version 1.0.1, 07-JUL-1996 (NJB)
C
C        Misspelling of "conjunction" was fixed.
C
C-    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB)
C
C
C-&
 
C$ Index_Entries
C
C     return the number of loaded EK columns
C     return the count of loaded EK columns
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKCCNT' )
      END IF
 
C
C     On the first pass through this routine, initialize the tables,
C     if it hasn't been done yet.
C
      IF ( FIRST ) THEN
C
C        Initialize the file table pool, segment table pool, column
C        descriptor pool, column table pool, and table list pool.
C
         CALL LNKINI ( FTSIZE, FTPOOL )
         CALL LNKINI ( STSIZE, STPOOL )
         CALL LNKINI ( DTSIZE, DTPOOL )
         CALL LNKINI ( CTSIZE, CTPOOL )
         CALL LNKINI ( MXTBLD, TBPOOL )
 
         FTHEAD = 0
         TBHEAD = 0
 
         FIRST  = .FALSE.
 
      END IF
 
 
C
C     Find the table.  If there's no match, the number of loaded columns
C     is zero.
C
      TBCURR   =  TBHEAD
      FND      =  .FALSE.
 
      DO WHILE (  ( TBCURR .GT. 0 ) .AND. ( .NOT. FND )  )
 
         IF (  EQSTR( TABLE, TBNAMS(TBCURR) )  ) THEN
            FND     =  .TRUE.
         ELSE
            TBCURR  =  LNKNXT ( TBCURR, TBPOOL )
         END IF
 
      END DO
 
 
      IF ( .NOT. FND ) THEN
 
         CCOUNT = 0
         CALL SETMSG ( 'The table # is not currently loaded.' )
         CALL ERRCH  ( '#',  TABLE                            )
         CALL SIGERR ( 'SPICE(TABLENOTLOADED)'                )
         CALL CHKOUT ( 'EKCCNT'                               )
         RETURN
 
      ELSE
C
C        Count the columns in the attribute table for the current table.
C
         CCOUNT  =  0
         COL     =  TBCTPT ( TBCURR )
 
         DO WHILE ( COL .GT. 0 )
            CCOUNT  =  CCOUNT + 1
            COL     =  LNKNXT ( COL, CTPOOL )
         END DO
 
      END IF
 
      CALL CHKOUT ( 'EKCCNT' )
      RETURN
 
 
 
 
 
 
 
C$Procedure     EKCII  ( EK, column info by index )
 
      ENTRY EKCII ( TABLE, CINDEX,  COLUMN,  ATTDSC )
 
C$ Abstract
C
C     Return attribute information about a column belonging to a loaded
C     EK table, specifying the column by table and index.
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
C     EK
C
C$ Keywords
C
C     EK
C     FILES
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         TABLE
C     INTEGER               CINDEX
C     CHARACTER*(*)         COLUMN
C     INTEGER               ATTDSC ( ADSCSZ )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TABLE      I   Name of table containing column.
C     CINDEX     I   Index of column whose attributes are to be found.
C     COLUMN     O   Name of column.
C     ATTDSC     O   Column attribute descriptor.
C
C$ Detailed_Input
C
C     TABLE          is the name of a loaded EK table.  Case is not
C                    significant.
C
C     CINDEX         is the index, within TABLE's column attribute
C                    table, of the column whose attributes are to be
C                    found.  The indices of the column table entries
C                    range from 1 to CCOUNT, where CCOUNT is the value
C                    returned by the entry point EKCCNT.
C
C$ Detailed_Output
C
C     COLUMN         is the name of the specified column.
C
C     ATTDSC         is a column attribute descriptor.  ATTDSC is an
C                    integer array containing descriptive information
C                    that applies uniformly to all loaded columns
C                    having the name COLUMN.  The following parameter
C                    values occur in ATTDSC:
C
C                       IFALSE:  -1
C                       ITRUE:    1
C                       CHR:      1
C                       DP:       2
C                       INT:      3
C                       TIME:     4
C
C                    The meanings of the elements of ATTDSC are given
C                    below.  The indices of the elements are
C                    parameterized; the parameter values are defined
C                    in the include file ekattdsc.inc.
C
C                       ATTDSC(ATTCLS):   Column class code
C
C                       ATTDSC(ATTTYP):   Data type code---CHR, DP, INT,
C                                         or TIME
C
C                       ATTDSC(ATTLEN):   String length; applies to CHR
C                                         type.  Value is IFALSE for
C                                         variable-length strings.
C
C                       ATTDSC(ATTSIZ):   Column entry size; value is
C                                         IFALSE for variable-size
C                                         columns.  Here `size' refers
C                                         to the number of array
C                                         elements in a column entry.
C
C                       ATTDSC(ATTIDX):   Index flag; value is ITRUE if
C                                         column is indexed, IFALSE
C                                         otherwise.
C
C                       ATTDSC(ATTNFL):   Null flag; value is ITRUE if
C                                         column may contain null
C                                         values, IFALSE otherwise.
C
C$ Parameters
C
C     ADSCSZ         is the size of column attribute descriptor.
C                    (Defined in ekattdsc.inc.)
C
C$ Exceptions
C
C     1)  If the specified table is not loaded, the error
C         SPICE(TABLENOTLOADED) is signaled.
C
C     2)  If the input argument CINDEX is less than one or greater
C         than the number of columns in TABLE, the error
C         SPICE(INVALIDINDEX) is signaled.
C
C$ Files
C
C     See the header of EKQMGR for a description of files used
C     by this routine.
C
C$ Particulars
C
C     This routine is a utility that allows a calling routine to
C     determine the attributes of the currently loaded columns.
C
C$ Examples
C
C     1)  Dump the names and attributes of the columns in each loaded
C         table.  EKCII is used to obtain column attributes.
C
C            C
C            C     Get the number of loaded tables.
C            C
C                  CALL EKNTAB ( NTAB )
C
C                  DO TAB = 1, NTAB
C            C
C            C        Get the name of the current table, and look up
C            C        the column count for this table.
C            C
C                     CALL EKTNAM ( TAB,    TABNAM )
C                     CALL EKCCNT ( TABNAM, NCOLS  )
C
C                     WRITE (*,*) 'TABLE = ', TABNAM
C                     WRITE (*,*) ' '
C
C            C
C            C        For each column in the current table, look up the
C            C        column's attributes.  The attribute block
C            C        index parameters are defined in the include file
C            C        ekattdsc.inc.
C            C
C                     DO I = 1, NCOLS
C
C                        CALL EKCII ( TABNAM, I, COLNAM, ATTDSC )
C
C                        WRITE (*,*) 'COLUMN = ', COLNAM
C
C            C
C            C           Write out the current column's data type.
C            C
C                        IF ( ATTDSC(ATTTYP) .EQ. CHR ) THEN
C                           WRITE (*,*) 'TYPE   =  CHR'
C
C                           IF ( ATTDSC(ATTLEN) .EQ. -1 ) THEN
C                              WRITE (*,*) 'STRING LENGTH = VARIABLE.'
C                           ELSE
C                              WRITE (*,*) 'STRING LENGTH = ',
C                 .                         ATTDSC(ATTLEN)
C                           END IF
C
C                        ELSE IF ( ATTDSC(ATTTYP) .EQ. DP ) THEN
C                           WRITE (*,*) 'TYPE   =  DP'
C
C                        ELSE IF ( ATTDSC(ATTTYP) .EQ. INT ) THEN
C                           WRITE (*,*) 'TYPE   =  INT'
C
C                        ELSE
C                           WRITE (*,*) 'TYPE   =  TIME'
C                        END IF
C
C            C
C            C           Write out the current column's entry size.
C            C
C                        WRITE (*,*) 'SIZE   = ', ATTDSC(ATTSIZ)
C
C            C
C            C           Indicate whether the current column is indexed.
C            C
C                        IF ( ATTDSC(ATTIDX) .EQ. -1 ) THEN
C                           WRITE (*,*) 'NOT INDEXED'
C                        ELSE
C                           WRITE (*,*) 'INDEXED'
C                        END IF
C
C            C
C            C           Indicate whether the current column allows
C            C           null values.
C            C
C                        IF ( ATTDSC(ATTNFL) .EQ. -1 ) THEN
C                           WRITE (*,*) 'NULL VALUES NOT ALLOWED'
C                        ELSE
C                           WRITE (*,*) 'NULL VALUES ALLOWED'
C                        END IF
C
C                     END DO
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
C     N.J. Bachman   (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.1, 10-FEB-2014 (BVS)
C
C        Added description of ADSCSZ to the Parameters section of the
C        header.
C
C-    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB)
C
C        Bug fix:   When an already loaded kernel is opened with EKOPR,
C        it now has its link count reset to 1 via a call to EKCLS.
C
C-    SPICELIB Version 1.0.1, 07-JUL-1996 (NJB)
C
C        Previous version line was changed from "Beta" to "SPICELIB."  
C
C-    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB)
C
C-&
 
C$ Index_Entries
C
C     return information on loaded EK column specified by index
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKCII' )
      END IF
 
C
C     On the first pass through this routine, initialize the tables,
C     if it hasn't been done yet.
C
      IF ( FIRST ) THEN
C
C        Initialize the file table pool, segment table pool, column
C        descriptor pool, column table pool, and table list pool.
C
         CALL LNKINI ( FTSIZE, FTPOOL )
         CALL LNKINI ( STSIZE, STPOOL )
         CALL LNKINI ( DTSIZE, DTPOOL )
         CALL LNKINI ( CTSIZE, CTPOOL )
         CALL LNKINI ( MXTBLD, TBPOOL )
 
         FTHEAD = 0
         TBHEAD = 0
 
         FIRST  = .FALSE.
 
      END IF
 
 
C
C     Find the table.  If there's no match, the number of loaded columns
C     is zero.
C
      TBCURR   =  TBHEAD
      FND      =  .FALSE.
 
      DO WHILE (  ( TBCURR .GT. 0 ) .AND. ( .NOT. FND )  )
 
         IF (  EQSTR( TABLE, TBNAMS(TBCURR) )  ) THEN
            FND     =  .TRUE.
         ELSE
            TBCURR  =  LNKNXT ( TBCURR, TBPOOL )
         END IF
 
      END DO
 
 
      IF ( .NOT. FND ) THEN
 
         CALL SETMSG ( 'The table # is not currently loaded.' )
         CALL ERRCH  ( '#',  TABLE                            )
         CALL SIGERR ( 'SPICE(TABLENOTLOADED)'                )
         CALL CHKOUT ( 'EKCII'                                )
         RETURN
 
      END IF
 
 
C
C     Locate the named column in the column attribute table.
C
      I    =  0
      COL  =  TBCTPT ( TBCURR )
 
      DO WHILE (  ( COL .GT. 0 )  .AND. ( I .LT. CINDEX )  )
 
         I  =  I + 1
 
         IF ( I .EQ. CINDEX ) THEN
C
C           We've found the column.  Set the output arguments using
C           its attributes.
C
            COLUMN    = CTNAMS(COL)
 
            ATTDSC(1) = CTCLAS(COL)
            ATTDSC(2) = CTTYPS(COL)
            ATTDSC(3) = CTLENS(COL)
            ATTDSC(4) = CTSIZS(COL)
 
            IF ( CTINDX(COL) ) THEN
               ATTDSC(5) = ITRUE
            ELSE
               ATTDSC(5) = IFALSE
            END IF
 
            IF ( CTNULL(COL) ) THEN
               ATTDSC(6) = ITRUE
            ELSE
               ATTDSC(6) = IFALSE
            END IF
 
            CALL CHKOUT ( 'EKCII' )
            RETURN
         ELSE
            COL = LNKNXT ( COL, CTPOOL )
         END IF
 
      END DO
 
C
C     We end up here if we ran out of columns before finding the
C     CINDEXth one, or if CINDEX was non-positive.
C
      CALL SETMSG ( 'Column indices for table # range from # to #; ' //
     .              'requested index was #.'                           )
      CALL ERRCH  ( '#',  TABNAM                                       )
      CALL ERRINT ( '#',  MAX( 1, I )                                  )
      CALL ERRINT ( '#',  I                                            )
      CALL ERRINT ( '#',  CINDEX                                       )
      CALL SIGERR ( 'SPICE(INVALIDINDEX)'                              )
 
      CALL CHKOUT ( 'EKCII' )
      RETURN
 
 
 
 
 
 
 
 
 
 
 
 
C$Procedure     EKSRCH  ( EK, search for events )
 
      ENTRY EKSRCH ( EQRYI, EQRYC, EQRYD, NMROWS, SEMERR, ERRMSG )
 
C$ Abstract
C
C     Search for EK events matching a specified set of constraints.
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
C     EK
C
C$ Keywords
C
C     EK
C     EVENT
C     FILES
C     SEARCH
C
C$ Declarations
C
C     INTEGER               EQRYI  ( LBCELL : * )
C     CHARACTER*(*)         EQRYC
C     DOUBLE PRECISION      EQRYD  ( * )
C     INTEGER               NMROWS
C     LOGICAL               SEMERR
C     CHARACTER*(*)         ERRMSG
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     EQRYI      I   Integer component of encoded query.
C     EQRYC      I   Character component of encoded query.
C     EQRYD      I   D.p. component of encoded query.
C     NMROWS     O   Number of rows matching query constraints.
C     SEMERR     O   Flag indicating whether semantic error occurred.
C     ERRMSG     O   Message describing semantic error, if any.
C
C$ Detailed_Input
C
C     EQRYI,
C     EQRYC,
C     EQRYD          are, respectively, the integer, character, and
C                    double precision portions of an encoded query.
C                    The query must have been parsed and must have
C                    its table and column names resolved.  Time values
C                    must have been resolved.  The query is expected
C                    to be semantically correct.
C
C$ Detailed_Output
C
C     NMROWS         is the number of rows matching the input query
C                    constraints.
C
C     SEMERR         is a logical flag indicating whether a semantic
C                    error was detected while attempting to respond to
C                    the input query.
C
C     ERRMSG         is a descriptive error message that is set if a
C                    semantic error is detected.  Otherwise, ERRMSG
C                    is returned blank.
C
C     See $Particulars for a description of the effect of this
C     routine.
C
C$ Parameters
C
C     LBCELL         is the SPICELIB cell lower bound.
C
C$ Exceptions
C
C     1)  If this routine is called when no files are loaded, the
C         error SPICE(NOLOADEDFILES) is signaled.
C
C     2)  If the structure of the input query is invalid, this routine
C         may fail in mysterious ways.
C
C$ Files
C
C     See the header of EKQMGR for a description of files used
C     by this routine.
C
C$ Particulars
C
C     NAIF Toolkit-based applications will rarely need to call this
C     routine directly; the high-level routine EKFIND should normally
C     be used to query the EK system.
C
C     Because the structure of encoded queries is not part of the
C     SPICELIB user interface, we strongly recommend that users'
C     applications not call this routine directly.
C
C$ Examples
C
C     See the header of the umbrella subroutine EKQMGR for a
C     comprehensive example of the use of EKQMGR's entry points.
C
C$ Restrictions
C
C     1) This routine should normally not be called directly from
C        users' applications.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.1, 10-FEB-2014 (BVS)
C
C        Added description of LBCELL to the Parameters section of the
C        header.
C
C-    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB)
C
C        Bug fix:   When an already loaded kernel is opened with EKOPR,
C        it now has its link count reset to 1 via a call to EKCLS.
C
C-    SPICELIB Version 1.2.0, 21-JUL-1998 (NJB)
C
C        ZZEKJSQZ call was added after the ZZEKJOIN call.  This change
C        reduces the scratch area usage for intermediate results of
C        joins.  It also prevents ZZEKJOIN from being handed a join
C        row set containing a segment vector having no corresponding
C        row vectors.
C
C        Removed a comment in the join loop indicating that non-join
C        constraints involving comparisons of column entries in the 
C        table were being activated.  This comment was incorrect; the
C        constraints in question were applied earlier.
C
C-    SPICELIB Version 1.0.1, 07-JUL-1996 (NJB)
C
C        Previous version line was changed from "Beta" to "SPICELIB."  
C
C-    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB)
C
C-&
 
C$ Index_Entries
C
C     search for events in loaded EK files
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKSRCH' )
      END IF
 
C
C     There nothing to search if no files are loaded.  A sure
C     symptom of this problem is that the file list is empty.
C
      IF ( FTHEAD .LE. 0 ) THEN
         CALL SETMSG ( 'No E-kernels are currently loaded.' )
         CALL SIGERR ( 'SPICE(NOLOADEDFILES)'               )
         CALL CHKOUT ( 'EKSRCH'                             )
         RETURN
      END IF
 
C
C     No error to begin with.
C
      SEMERR  =  .FALSE.
      ERRMSG  =   ' '
      NMROWS  =   0
 
 
      IF ( FIRST ) THEN
C
C        Initialize the file table pool, segment table pool, column
C        descriptor pool, column table pool, and table list pool.
C
         CALL LNKINI ( FTSIZE, FTPOOL )
         CALL LNKINI ( STSIZE, STPOOL )
         CALL LNKINI ( DTSIZE, DTPOOL )
         CALL LNKINI ( CTSIZE, CTPOOL )
         CALL LNKINI ( MXTBLD, TBPOOL )
 
         FTHEAD = 0
         TBHEAD = 0
 
         FIRST  = .FALSE.
 
      END IF
 
C
C     Read some of our favorite things from the query.  We need:
C
C        - the table count
C        - the SELECT clause column count
C        - the order-by column count
C        - the table and alias list
C
      CALL ZZEKREQI ( EQRYI, 'NUM_TABLES',        NTAB   )
      CALL ZZEKREQI ( EQRYI, 'NUM_SELECT_COLS',   NSEL   )
      CALL ZZEKREQI ( EQRYI, 'NUM_ORDERBY_COLS',  NORDER )
 
      DO I = 1, NTAB
         CALL ZZEKQTAB ( EQRYI, EQRYC, I, FRMTAB(I), FRMALS(I) )
      END DO
 
C
C     Initialize the table vectors.  Also initialize a vector of column
C     list pointers.
C
      CALL SSIZEC ( MAXTAB, TABVEC )
      CALL SSIZEI ( MAXTAB, TPTVEC )
 
C
C     Fill in the FROM table vector and corresponding column pointer
C     vector.  It's an error if a table referenced in the FROM clause
C     can't be found.
C
      DO I = 1, NTAB
C
C        Find the table list entry for this table name.
C
         TBCURR  =  TBHEAD
         FND     = .FALSE.
 
         DO WHILE (  ( TBCURR .GT. 0 ) .AND. ( .NOT. FND ) )
 
            IF ( TBNAMS(TBCURR) .EQ. FRMTAB(I) ) THEN
C
C              We've found the table list entry for the Ith table.
C
               CALL APPNDC ( FRMTAB(I), TABVEC )
               CALL APPNDI ( TBCURR,    TPTVEC )
               FND     = .TRUE.
            ELSE
               TBCURR  =  LNKNXT ( TBCURR, TBPOOL )
            END IF
 
         END DO
 
         IF ( .NOT. FND ) THEN
            CALL SETMSG ( 'The table # is not currently loaded.' )
            CALL ERRCH  ( '#',   FRMTAB(I)                       )
            CALL SIGERR ( 'SPICE(INVALIDTABLENAME)'              )
            CALL CHKOUT ( 'EKSRCH'                               )
            RETURN
         END IF
 
      END DO
 
C
C     Since this is a new search, re-initialize the stack in the EK
C     scratch area.  Also initialize our total segment list count.
C
      CALL ZZEKSTOP ( TOP )
      CALL ZZEKSDEC ( TOP )
 
C
C     Initialize the size of the join row set union for the current
C     query.  At this point, no matching rows have been found.
C
      USIZE  =  0
      UNROWS =  0
 
C
C     Get the number of conjunctions and the sizes of the conjunctions.
C
      CALL ZZEKREQI ( EQRYI, 'NUM_CONJUNCTIONS',  NCONJ  )
      CALL CLEARI   ( MAXCON, SIZES )
 
      DO I = 1, NCONJ
         CALL ZZEKQCNJ ( EQRYI, I, SIZES(I) )
      END DO
 
C
C     For each conjunction of constraints, we'll build a join row
C     set representing the row vectors matching those constraints.
C     The final result will be a join row set union representing the
C     row vectors satisfying at least one conjunction.
C
C     We want to build a join row set even if there are *no*
C     constraints.  Therefore, we always make at least one pass
C     through the loop below.
C
      CJEND = 0
 
      DO CONJ  =  1,  MAX( 1, NCONJ )
C
C        Our objective is to build a join row set representing the table
C        defined by the FROM columns and the input constraints.  To do
C        this, we'll first build a trivial join row set for each table;
C        this join row set represents the rows that satisfy constraints
C        on columns in that table.  Having done this, we'll produce a
C        final (for this conjunction) join row set that represents the
C        constrained join of the FROM tables.  The base address of this
C        join row set will be stored in the array UBASE.
C
C        We'll start out by recording the FROM table indices and column
C        list indices of columns listed in the constraints.
C
         IF ( NCONJ .EQ. 0 ) THEN
            CJSIZE  =  0
         ELSE
            CJSIZE  =  SIZES(CONJ)
         END IF
 
         CJBEG  =  CJEND + 1
         CJEND  =  CJEND + CJSIZE
 
         DO I  =  1, CJSIZE
 
            CALL ZZEKQCON ( EQRYI,     EQRYC,      EQRYD,     CJBEG+I-1,
     .                      CNSTYP(I),
     .                      LTNAME,    LTBIDX(I),  LCNAME,    LCIDX(I),
     .                      OPS(I),
     .                      RTNAME,    RTBIDX(I),  RCNAME,    RCIDX(I),
     .                      DTYPE(I),
     .                      CBEGS(I),  CENDS(I),   DVALS(I),  IVALS(I) )
         END DO
 
 
         DO T = 1, NTAB
C
C           We will build a trivial (one-table) join row set for the
C           current table.
C
C           Initialize the join row set.  Retain the base address.  We
C           can fill in the table count right away; the count is 1.
C
            CALL ZZEKSTOP ( RBAS(T) )
 
            DO I = 1, JSCIDX
               CALL ZZEKSPSH ( 1, 0 )
            END DO
 
            CALL ZZEKSUPD ( RBAS(T)+JTCIDX, RBAS(T)+JTCIDX, 1 )
 
C
C           Count the loaded segments for the current table.  We'll
C           leave enough room in the join row set for each segment.
C
            TAB = TPTVEC(T)
            I   = TBSTPT(TAB)
            NSV = 0
 
            DO WHILE ( I .GT. 0 )
 
               CALL ZZEKSPSH ( 1, 0 )
 
               NSV  =  NSV + 1
               I    =  LNKNXT ( I, STPOOL )
 
            END DO
 
C
C           Save room for the row vector base addresses and counts.
C
            DO I = 1, 2*NSV
               CALL ZZEKSPSH ( 1, 0 )
            END DO
 
C
C           At this point, we can set the segment vector count in the
C           join row set.
C
            CALL ZZEKSUPD ( RBAS(T)+JSCIDX, RBAS(T)+JSCIDX, NSV )
 
C
C           Find the matching rows in the segments belonging to the
C           current table.
C
            SEG     =  TBSTPT(TAB)
            NSEG    =  0
            RTOTAL  =  0
 
            DO WHILE ( SEG .GT. 0 )
 
               NSEG = NSEG + 1
C
C              The segment vector for this segment is trivial:  it's
C              just the segment's index in the segment table.
C
               SGVBAS  =  RBAS(T) + JSVBAS + ( NSEG - 1 )
 
               CALL ZZEKSUPD ( SGVBAS+1, SGVBAS+1, SEG )
 
C
C              Label as `inactive' any constraints that don't apply to
C              this table.  Join constraints are inactive at this stage
C              of the game.  Label all other constraints `active'.
C              We'll keep track of column and value constraints
C              separately.
C
               DO I = 1, CJSIZE
C
C                 Each constraint is active to start with.
C
                  ACTIVC(I) = CNSTYP(I) .EQ. EQCOL
                  ACTIVV(I) = CNSTYP(I) .EQ. EQVAL
 
C
C                 The parent table of the LHS column must be the Tth
C                 table, or this constraint does not apply.
C
C                 We'll also exclude join constraints.  Note that
C                 constraints comparing values from two columns need not
C                 be join constraints:  it's possible that the column on
C                 the right belongs to the same FROM table as the
C                 column on the left.
C
                  IF ( LTBIDX(I) .NE. T ) THEN
 
                     ACTIVC(I) = .FALSE.
                     ACTIVV(I) = .FALSE.
 
 
                  ELSE IF ( CNSTYP(I) .EQ. EQCOL ) THEN
 
                     IF ( LTBIDX(I) .NE. RTBIDX(I) ) THEN
C
C                       This is a join constraint; disable it.
C
                        ACTIVC(I) = .FALSE.
 
                     END IF
 
                  END IF
 
 
               END DO
 
 

C
C              At this point, we'll have to search the segment for
C              matching rows.  Pick a key column for the segment.  To
C              do this, we'll need to pack an array with column
C              descriptors for each active constraint.  The
C              descriptor for the column on the left side of the Ith
C              constraint will be placed in elements LDSCRS(*,I), if
C              the Ith constraint is active.
C
               CALL CLEARI ( CDSCSZ*MAXCON, LDSCRS )

               DO I = 1, CJSIZE

                  IF ( ACTIVV(I) ) THEN
C
C                     Look up the column descriptor for this
C                     constraint.
C
                     J  =  STDTPT(SEG)

                     DO K = 2, LCIDX(I)
                        J  = LNKNXT(J,DTPOOL)
                     END DO

                     CALL MOVEI ( DTDSCS(1,J), CDSCSZ, LDSCRS(1,I) )

                  END IF

               END DO


               CALL ZZEKKEY ( STHAN (SEG),     STDSCS(1,SEG),
     .                        STNROW(SEG),     CJSIZE,
     .                        LCIDX,           LDSCRS,
     .                        OPS,             DTYPE,
     .                        EQRYC,           CBEGS,
     .                        CENDS,           DVALS,
     .                        IVALS,           ACTIVV,
     .                        KEY,             KEYDSC,
     .                        BEGIDX,          ENDIDX,
     .                        KEYFND                        )

C
C              ZZEKKEY has updated ACTIVV to reflect the application
C              of constraints that were used to determine BEGIDX and
C              ENDIDX.
C
               IF ( KEYFND ) THEN

                  INDEXD  =  .TRUE.

               ELSE
C
C                 A key column could not be determined from the
C                 active constraints.  We'll use the first column of
C                 the segment as the key column.
C
                  INDEXD  =  .FALSE.
                  BEGIDX  =   1
                  ENDIDX  =   STNROW(SEG)

               END IF

C
C              Whether or not we have any matching rows, we'll need
C              to record how many we have.  Save the offset from the
C              join row set base for the pointer to the row vectors.
C              The row vector count follows this pointer.
C
               PTROFF = JSVBAS  +  NSV  +  (NSEG - 1) * 2   +   1


               IF ( ENDIDX .GE. BEGIDX ) THEN
C
C                 Initialize the count of matching rows for this
C                 segment.  The current stack top is the base address
C                 for the row vectors; save the offset of this
C                 address from the join row set's base.
C                 Also compute the base address of the segment vector
C                 for the current segment.
C
                  NMATCH = 0
                  CALL ZZEKSTOP ( RWVBAS )

                  CALL ZZEKSUPD ( RBAS(T)+PTROFF,
     .                            RBAS(T)+PTROFF,   RWVBAS-RBAS(T) )

C
C                 Count the active constraints.  While we're at it,
C                 fill in the descriptor lists LDSCRS and RDSCRS
C                 with, respectively, the descriptors for the columns
C                 on the left hand sides and right hand sides of
C                 these constraints.
C
                  CALL CLEARI ( CDSCSZ*MAXCON, LDSCRS )
                  CALL CLEARI ( CDSCSZ*MAXCON, RDSCRS )
                  NACT = 0

                  DO I = 1, CJSIZE

                     IF ( ACTIVC(I) .OR. ACTIVV(I) ) THEN

                        NACT = NACT + 1
C
C                       Look up the column descriptor for this
C                       constraint.

                        J  =  STDTPT(SEG)

                        DO K = 2, LCIDX(I)
                           J  = LNKNXT(J,DTPOOL)
                        END DO

                        CALL MOVEI(DTDSCS(1,J), CDSCSZ, LDSCRS(1,I) )


                        J  =  STDTPT(SEG)

                        DO K = 2, RCIDX(I)
                           J  = LNKNXT(J,DTPOOL)
                        END DO

                        CALL MOVEI(DTDSCS(1,J), CDSCSZ, RDSCRS(1,I) )

                     END IF

                  END DO


                  IF ( NACT .GT. 0 ) THEN
C
C                    There are still active constraints left, so
C                    proceed linearly through the remaining rows,
C                    testing each one against these constraints. Add
C                    matching rows to the current join row set.
C
                     DO R = BEGIDX, ENDIDX


                        IF ( INDEXD ) THEN

                           CALL ZZEKIXLK ( STHAN(SEG),
     .                                     KEYDSC,
     .                                     R,
     .                                     ROWIDX )
                        ELSE
C
C                          Look up the record pointer for row R.
C
                           CALL ZZEKRPLK ( STHAN(SEG),
     .                                     STDSCS(1,SEG),
     .                                     R,
     .                                     ROWIDX  )
                        END IF

C
C                       Test the row against both value and column
C                       constraints.  For now, we supply an array
C                       of default column entry element indices.
C
                        VMTCH = ZZEKRMCH ( CJSIZE,     ACTIVV,
     .                                     STHAN(SEG), STDSCS(1,SEG),
     .                                     LDSCRS,     ROWIDX,
     .                                     LELTS,      OPS,
     .                                     DTYPE,      EQRYC,
     .                                     CBEGS,      CENDS,
     .                                     DVALS,      IVALS     )

                        CMTCH = .TRUE.

C
C                       Note that ZZEKVMCH expects a set of inputs
C                       that are not really parallel to those
C                       expected by ZZEKRMCH.  We feed the
C                       column comparison constraints to ZZEKVMCH
C                       one at a time.
C
                        DO J = 1, CJSIZE

                           CMTCH =       CMTCH
     .                             .AND. ZZEKVMCH ( 1,
     .                                              ACTIVC(J),
     .                                              STHAN(SEG),
     .                                              STDSCS(1,SEG),
     .                                              LDSCRS(1,J),
     .                                              ROWIDX,
     .                                              1,
     .                                              OPS(J),
     .                                              STHAN(SEG),
     .                                              STDSCS(1,SEG),
     .                                              RDSCRS(1,J),
     .                                              ROWIDX,
     .                                              1            )
                        END DO


                        IF ( CMTCH .AND. VMTCH ) THEN
C
C                          Push the `augmented row vector' for the
C                          current row onto the stack.  In this case,
C                          of course, the augmented row vector is
C                          trivial:  it consists of the row number,
C                          followed by the base address of the parent
C                          segment vector.
C
                           NMATCH = NMATCH + 1

                           CALL ZZEKSPSH ( 1, ROWIDX         )
                           CALL ZZEKSPSH ( 1, SGVBAS-RBAS(T) )

                        END IF

                     END DO


                  ELSE
C
C                    All the rows indicated by BEGIDX and ENDIDX
C                    match the constraints.  This code section should
C                    be upgraded to transfer the row numbers in
C                    chunks.
C
                     NMATCH  =  ENDIDX - BEGIDX + 1


                     DO R = BEGIDX, ENDIDX

                        IF ( INDEXD ) THEN
C
C                          Look up the record pointer for row R
C                          from the column index.
C
                           CALL ZZEKIXLK ( STHAN(SEG),
     .                                     KEYDSC,
     .                                     R,
     .                                     ROWIDX )
                        ELSE
C
C                          Look up the record pointer for row R.
C
                           CALL ZZEKRPLK ( STHAN(SEG),
     .                                     STDSCS(1,SEG),
     .                                     R,
     .                                     ROWIDX  )
                        END IF


                        CALL ZZEKSPSH ( 1, ROWIDX         )
                        CALL ZZEKSPSH ( 1, SGVBAS-RBAS(T) )

                     END DO

                  END IF

C
C                 Fill in the row count for this segment in the join row
C                 set.
C
                  CALL ZZEKSUPD ( RBAS(T)+PTROFF+1,
     .                            RBAS(T)+PTROFF+1, NMATCH )
 
               END IF
 
C
C              Take a look at the next segment.  Update the total count
C              of matching rows for this table.
C
               SEG    = LNKNXT ( SEG, STPOOL )
               RTOTAL = RTOTAL + NMATCH
 
            END DO
 
C
C           Fill in the size and count information for the join row set.
C
            CALL ZZEKSTOP ( TOP )
            RSIZE(T)  =  TOP - RBAS(T)
 
            CALL ZZEKSUPD ( RBAS(T)+JSZIDX, RBAS(T)+JSZIDX, RSIZE(T) )
            CALL ZZEKSUPD ( RBAS(T)+JRCIDX, RBAS(T)+JRCIDX, RTOTAL   )
 
C
C           Compress out any empty segment vectors from the join row
C           set.
C
            CALL ZZEKJSQZ ( RBAS(T) )
 
C
C           At this point, we've filled in the entire join row set for
C           table T.
C
         END DO
 
C
C        Join the trivial join row sets, producing a final join row set
C        for the current conjunction.  Retain the base address of this
C        join row set, if it is non-empty.  Update the size of the join
C        row set union.
C
C
         RESBAS = RBAS(1)
 
         DO T = 2, NTAB
C
C           Arm the join constraints!  Turn on the constraints that
C           have the Tth table on the one side, and tables
C           1, 2, ... , T on the other.
C
            DO I = 1, CJSIZE
 
               ACTIVC(I)  =  .FALSE.
 
               IF ( CNSTYP(I) .EQ. EQCOL ) THEN
 
                  L  =  LTBIDX(I)
                  R  =  RTBIDX(I)
 
                  IF (      ( L .GE. 1           )
     .                .AND. ( L .LE. T           )
     .                .AND. ( R .GE. 1           )
     .                .AND. ( R .LE. T           )
     .                .AND. ( L .NE. R           )
     .                .AND. (      ( R .EQ. T )
     .                        .OR. ( L .EQ. T )  )  )  THEN
 
                     ACTIVC(I)  =  .TRUE.
 
                  END IF
 
               END IF
 
            END DO
 
C
C           The base address of the first join row set is the base
C           address of the result of the previous join.  The first time
C           through, the base of the join row set for table 1 is used.
C
            IF ( T .EQ. 2 ) THEN
               JBASE1  =  RBAS( 1 )
            ELSE
               JBASE1  =  RESBAS
            END IF
 
            JBASE2  = RBAS( T )
 
            CALL ZZEKJOIN (  JBASE1,   JBASE2,   CJSIZE,  ACTIVC,
     .                       LTBIDX,   LCIDX,    LELTS,   OPS,
     .                       RTBIDX,   RCIDX,    RELTS,   STHAN,
     .                       STDSCS,   STDTPT,   DTPOOL,  DTDSCS,
     .                       RESBAS,   JSIZE                       )
 
            CALL ZZEKJSQZ ( RESBAS )
            
         END DO
 
C
C        At this point, we've found the matching rows for the current
C        query conjunction.  Update the size of the join row set union
C        corresponding to the current query.  Save the base address of
C        the final join row set.  Update the total number of matching
C        rows in the join row set union.
C
         USIZE         =  USIZE  + 1
         UBASE(USIZE)  =  RESBAS
 
         CALL ZZEKSRD ( RESBAS+JRCIDX, RESBAS+JRCIDX, CJROWS )
 
         UNROWS        =  UNROWS + CJROWS
 
C
C        Remove redundant row vectors from the join row set union.
C        These row vectors may arise in the execution of queries whose
C        WHERE clauses contain multiple conjunctions.
C
         CALL ZZEKWEED ( USIZE, UBASE, UNROWS )
 
C
C        Initialize the addressing function for the current join row
C        set union.
C
         IF ( USIZE .GT. 0 ) THEN
            CALL ZZEKVSET ( USIZE, UBASE )
         END IF
 
      END DO
 
C
C     At this point, we've formed the join row set union that
C     represents the set of row vectors matching the entire query.
C
      NMROWS  =  UNROWS
 
C
C     Get the tables and columns of from the SELECT clause.  For
C     each qualifying table, we need the index in the FROM clause
C     of that table.  For each column, we need the column table
C     index.
C
      DO I = 1, NSEL
 
         CALL ZZEKQSEL ( EQRYI,  EQRYC,  I,      LXBEG,  LXEND,
     .                   TABNAM, TABIDX, COLNAM, K             )
 
C
C        Locate the column's attribute information.  Retain the column's
C        index within the parent table's column list.
C
         TAB     =  TPTVEC( TABIDX )
         J       =  TBCTPT(TAB)
         COL     =  0
         FND     =  .FALSE.
 
         DO WHILE (  ( J .GT. 0 ) .AND. ( .NOT. FND )  )
 
            COL = COL + 1
 
            IF ( CTNAMS(J) .EQ. COLNAM ) THEN
               FND  =  .TRUE.
            ELSE
               J    =  LNKNXT( J, CTPOOL )
            END IF
 
         END DO
 
         IF ( .NOT. FND ) THEN
            CALL SETMSG ( '# is not name of a column in FROM table #.' )
            CALL ERRCH  ( '#', COLNAM                                  )
            CALL ERRINT ( '#', TABIDX                                  )
            CALL SIGERR ( 'SPICE(BUG)'                                 )
            CALL CHKOUT ( 'EKSRCH'                                     )
            RETURN
         END IF
 
         SELCTP(I)  =  J
         SELCOL(I)  =  COL
         SELTAB(I)  =  TABIDX
 
      END DO
 
C
C     Enable sorting of the matching row vectors, if necessary.  The
C     first fetch request will invoke the sort.
C
      DOSORT  =  ( NORDER .GT. 0 ) .AND. ( NMROWS .GT. 0 )
      SORTED  =  .FALSE.
 
      IF ( DOSORT ) THEN
 
         DO I = 1, NORDER
 
            CALL ZZEKQORD (  EQRYI,     EQRYC,   I,         TABNAM,
     .                       OTABS(I),  COLNAM,  OCOLS(I),  SENSE(I)  )
         END DO
 
      END IF
 
 
      CALL CHKOUT ( 'EKSRCH' )
      RETURN
 
 
 
 
 
 
 
C$Procedure     EKNELT  ( EK, get number of elements in column entry )
 
      ENTRY EKNELT ( SELIDX, ROW, NELT )
 
C$ Abstract
C
C     Return the number of elements in a specified column entry in
C     the current row.
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
C     EK
C
C$ Keywords
C
C     EK
C
C$ Declarations
C
C     INTEGER               SELIDX
C     INTEGER               ROW
C     INTEGER               NELT
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     SELIDX     I   Index of parent column in SELECT clause.
C     ROW        I   Row containing element.
C     NELT       O   Number of elements in entry in current row.
C
C$ Detailed_Input
C
C     SELIDX         is the SELECT clause index of the column to
C                    fetch from.
C
C     ROW            is the index of the row containing the element.
C                    This number refers to a member of the set of rows
C                    matching a query.  ROW must be in the range
C
C                      1 : NMROWS
C
C                    where NMROWS is the matching row count returned
C                    by EKSRCH.
C
C$ Detailed_Output
C
C     NELT           is the number of elements in the column entry
C                    belonging to the specified column in the current
C                    row.
C
C                    Null entries in variable-size columns are
C                    considered to have size 1.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If this routine is called when no E-kernels have been loaded,
C         the error SPICE(NOLOADEDFILES) is signaled.
C
C     2)  If SELIDX is outside of the range established by the
C         last query passed to EKSRCH, the error SPICE(INVALIDINDEX)
C         will be signaled.
C
C     3)  If ROW is outside of the range established by the
C         last query passed to EKSRCH, the error SPICE(INVALIDINDEX)
C         will be signaled.
C
C$ Files
C
C     See the header of EKQMGR for a description of files used
C     by this routine.
C
C$ Particulars
C
C     This routine is meant to be used in conjunction with the EKQMGR
C     fetch entry points EKGC, EKGD, and EKGI.  This routine
C     allows the caller of those routines to determine appropriate
C     loop bounds to use to fetch each column entry in the current row.
C
C$ Examples
C
C     1)  Suppose the EK table TAB contains the following columns:
C
C
C            Column name   Data Type   Size
C            -----------   ---------   ----
C            IARRAY        INT         10
C            DARRAY        DP          VARIABLE
C            CARRAY        CHR         VARIABLE
C
C
C         Suppose the query
C
C            QUERY = 'SELECT IARRAY, DARRAY, CARRAY FROM TAB'
C
C         is issued to EKFIND via the call
C
C            CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG )
C
C         To fetch and dump column values from the rows that satisfy the
C         query, the loop below could be used.  Note that we don't check
C         the FOUND flags returned by the fetch routines since we know
C         in advance how many elements are contained in each column
C         entry we fetch.
C
C
C                  DO ROW = 1, NMROWS
C
C                     WRITE (*,*) ' '
C                     WRITE (*,*) 'ROW  = ', ROW
C                     WRITE (*,*) ' '
C
C            C
C            C        Fetch values from column IARRAY in the current
C            C        row.  Since IARRAY was the first column selected,
C            C        the selection index SELIDX is set to 1.
C            C
C                     SELIDX = 1
C
C                     ELTIDX = 1
C                     ISNULL = .FALSE.
C
C                     DO WHILE ( ( ELTIDX .LE. 10 ) .AND. .NOT. ISNULL )
C            C
C            C           If the column entry is null, we'll be kicked
C            C           out of this loop after the first iteration.
C            C
C                        CALL EKGI ( SELIDX,         ROW,     ELTIDX,
C                                    IVALS(ELTIDX),  ISNULL,  FOUND   )
C
C                        ELTIDX = ELTIDX + 1
C
C                     END DO
C
C                     WRITE (*,*) ' '
C                     WRITE (*,*) 'COLUMN = IARRAY'
C                     WRITE (*,*) ' '
C
C                     IF ( ISNULL ) THEN
C                        WRITE (*,*) '<Null>'
C                     ELSE
C                        WRITE (*,*) ( IVALS(I), I = 1, 10 )
C                     END IF
C
C            C
C            C        Fetch values from column DARRAY in the current
C            C        row.  Since DARRAY contains variable-size array
C            C        elements, we call EKNELT to determine how many
C            C        elements to fetch.
C            C
C                     SELIDX = 2
C
C                     CALL EKNELT ( SELIDX, ROW, NELT )
C
C                     ELTIDX = 1
C                     ISNULL = .FALSE.
C
C                     DO WHILE (       ( ELTIDX .LE.  NELT   )
C                 .              .AND. (        .NOT. ISNULL )  )
C
C                        CALL EKGD ( SELIDX,         ROW,     ELTIDX,
C                                    DVALS(ELTIDX),  ISNULL,  FOUND   )
C
C                        ELTIDX = ELTIDX + 1
C
C                     END DO
C
C                     WRITE (*,*) ' '
C                     WRITE (*,*) 'COLUMN = DARRAY'
C                     WRITE (*,*) ' '
C
C                     IF ( ISNULL ) THEN
C                        WRITE (*,*) '<Null>'
C                     ELSE
C                        WRITE (*,*) ( DVALS(I), I = 1, NELT )
C                     END IF
C
C            C
C            C        Fetch values from column CARRAY in the current
C            C        row.
C            C
C                     SELIDX = 3
C                     CALL EKNELT ( SELIDX, ROW, NELT )
C
C                     ELTIDX = 1
C                     ISNULL = .FALSE.
C
C                     DO WHILE (       ( ELTIDX .LE.  NELT   )
C                 .              .AND. (        .NOT. ISNULL )  )
C
C                        CALL EKGC ( SELIDX,         ROW,     ELTIDX,
C                                    CVALS(ELTIDX),  ISNULL,  FOUND   )
C
C                        ELTIDX = ELTIDX + 1
C
C                     END DO
C
C                     WRITE (*,*) ' '
C                     WRITE (*,*) 'COLUMN = CARRAY'
C                     WRITE (*,*) ' '
C
C                     IF ( ISNULL ) THEN
C                        WRITE (*,*) '<Null>'
C                     ELSE
C                        WRITE (*,*) ( CVALS(I), I = 1, NELT )
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
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB)
C
C        Bug fix:   When an already loaded kernel is opened with EKOPR,
C        it now has its link count reset to 1 via a call to EKCLS.
C
C-    SPICELIB Version 1.2.0, 12-FEB-1999 (NJB)
C
C        Bug fix:  There was a error handling branch that called CHKOUT 
C        where CHKIN should have been called.  This has been fixed.
C
C-    SPICELIB Version 1.1.0, 09-JUL-1996 (NJB)
C
C        Bug fix:  EKNELT now initiates a sort operation if sorted
C        outputs are required and EKNELT is called after query 
C        resolution but before the fetch routines.  Also, addressing
C        for sorted query results has been fixed.
C
C        Misspelling of "issued" was fixed.  Previous version line was 
C        changed from "Beta" to "SPICELIB."  
C
C
C-    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB)
C
C-&
 
C$ Index_Entries
C
C     return the number of elements in a column entry
C
C-&

C$ Revisions
C
C-    SPICELIB Version 1.2.0, 12-FEB-1999 (NJB)
C
C        Bug fix:  There was a error handling branch that called CHKOUT 
C        where CHKIN should have been called.  This has been fixed.
C
C-    SPICELIB Version 1.1.0, 09-JUL-1996 (NJB)
C
C        Bug fix:  EKNELT now initiates a sort operation if sorted
C        outputs are required and EKNELT is called after query 
C        resolution but before the fetch routines.  Also, addressing
C        for sorted query results has been fixed.  The fix involved
C        copying the sort invocation and addressing code from the
C        fetch routines.
C
C        Misspelling of "issued" was fixed.  Previous version line was 
C        changed from "Beta" to "SPICELIB."  
C
C-&
 
 
C
C     Use discovery check-in for speed.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
C
C     The request doesn't make sense if no files are loaded.  A sure
C     symptom of this problem is that the file list is empty.
C
      IF ( FTHEAD .LE. 0 ) THEN
         CALL CHKIN  ( 'EKNELT'                             )
         CALL SETMSG ( 'No E-kernels are currently loaded.' )
         CALL SIGERR ( 'SPICE(NOLOADEDFILES)'               )
         CALL CHKOUT ( 'EKNELT'                             )
         RETURN
      END IF
 
C
C     The row number must be valid, or we can't proceed.
C
      IF (  ( ROW .LT. 1 ) .OR. ( ROW .GT. UNROWS )  ) THEN
 
         CALL CHKIN  ( 'EKNELT'                                        )
         CALL SETMSG ( 'Row indices for query result range from 1 to '//
     .                 '#; requested row index was #.'                 )
         CALL ERRINT ( '#',  UNROWS                                    )
         CALL ERRINT ( '#',  ROW                                       )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                           )
         CALL CHKOUT ( 'EKNELT'                                        )
         RETURN
 
      END IF
 
C
C     Make sure the SELECT clause column index is valid.
C
      IF (  ( SELIDX .LT. 1 ) .OR. ( SELIDX .GT. NSEL )  ) THEN
         CALL CHKIN  ( 'EKNELT'                                   )
         CALL SETMSG ( 'The SELECT column index # is out of the ' //
     .                 'valid range 1:#'                          )
         CALL ERRINT ( '#', SELIDX                                )
         CALL ERRINT ( '#', NTAB                                  )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                      )
         CALL CHKOUT ( 'EKNELT'                                   )
         RETURN
      END IF
 
C
C     If it hasn't been done yet, and if it needs to be done, sort the
C     matching row vectors.
C
      IF ( DOSORT ) THEN
 
         CALL ZZEKJSRT ( USIZE,   UBASE,   NORDER,  OTABS,
     .                   OCOLS,   OELTS,   SENSE,   STHAN,
     .                   STDSCS,  STDTPT,  DTPOOL,  DTDSCS,  ORDBAS  )
 
         DOSORT  =  .FALSE.
         SORTED  =  .TRUE.
      END IF
 
C
C     Look up the segment vector and row vector for the current row.
C
      IF ( SORTED ) THEN
         CALL ZZEKSRD  ( ORDBAS+ROW,  ORDBAS+ROW,  I      )
         CALL ZZEKVCAL ( I,           RWVBAS,      SGVBAS )
      ELSE
         CALL ZZEKVCAL ( ROW,         RWVBAS,      SGVBAS )
      END IF
      
      CALL ZZEKSRD  ( RWVBAS+1, RWVBAS+NTAB, ROWVEC )
      CALL ZZEKSRD  ( SGVBAS+1, SGVBAS+NTAB, SEGVEC )
 
      TABIDX  =  SELTAB(SELIDX)
      ROWIDX  =  ROWVEC(TABIDX)
      SEG     =  SEGVEC(TABIDX)
      COL     =  SELCOL(SELIDX)
 
      COLPTR = STDTPT(SEG)
 
      DO I = 2, COL
         COLPTR = LNKNXT( COLPTR, DTPOOL )
      END DO
 
      NELT  =  ZZEKESIZ (  STHAN (   SEG   ),
     .                     STDSCS(1, SEG   ),
     .                     DTDSCS(1, COLPTR),
     .                     ROWIDX             )
 
 
      RETURN
 
 
 
 
 
C$Procedure     EKGC  ( EK, get event data, character )
 
      ENTRY EKGC ( SELIDX, ROW, ELMENT, CDATA, NULL, FOUND )
 
C$ Abstract
C
C     Return an element of an entry in a column of character
C     type in a specified row.
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
C     EK
C
C$ Keywords
C
C     ASSIGNMENT
C     EK
C
C$ Declarations
C
C     INTEGER               SELIDX
C     INTEGER               ROW
C     INTEGER               ELMENT
C     CHARACTER*(*)         CDATA
C     LOGICAL               NULL
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     SELIDX     I   Index of parent column in SELECT clause.
C     ROW        I   Row to fetch from.
C     ELMENT     I   Index of element, within column entry, to fetch.
C     CDATA      O   Character string element of column entry.
C     NULL       O   Flag indicating whether column entry was null.
C     FOUND      O   Flag indicating whether column was present in row.
C
C$ Detailed_Input
C
C     SELIDX         is the SELECT clause index of the column to
C                    fetch from.
C
C     ROW            is the output row containing the entry to fetch
C                    from.
C
C     ELMENT         is the index of the element of the column entry
C                    to fetch.  The normal range of ELMENT is from 1 to
C                    the size of the column's entry, but ELMENT is
C                    allowed to exceed the number of elements in the
C                    column entry; if it does, FOUND is returned .FALSE.
C                    This allows the caller to read data from the column
C                    entry in a loop without checking the number of
C                    available elements first.
C
C                    Null values in variable-sized columns are
C                    considered to have size 1.
C
C$ Detailed_Output
C
C     CDATA          is the requested element of the specified column
C                    entry.  If the entry is null, CDATA is undefined.
C
C                    If CDATA is too short to accommodate the requested
C                    column entry element, the element is truncated on
C                    the right to fit CDATA.  If CDATA is longer than
C                    the element, CDATA is returned blank-padded on
C                    the right.
C
C     NULL           is a logical flag indicating whether the entry
C                    belonging to the specified column in the specified
C                    row is null.
C
C     FOUND          is a logical flag indicating whether the specified
C                    element was found.  If the element does not exist,
C                    FOUND is returned .FALSE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input argument ELMENT is less than 1, FOUND is returned
C         .FALSE., and the error SPICE(INVALIDINDEX) is signaled.
C         However, ELMENT is allowed to be greater than the number of
C         elements in the specified column entry; this allows the caller
C         to read data from the column entry in a loop without checking
C         the number of available elements first.  If ELMENT is greater
C         than the number of available elements, FOUND is returned
C         .FALSE.
C
C     2)  If SELIDX is outside of the range established by the
C         last query passed to EKSRCH, the error SPICE(INVALIDINDEX)
C         will be signaled.
C
C     3)  If the input argument ROW is less than 1 or greater than
C         the number of rows matching the query, FOUND is returned
C        .FALSE., and the error SPICE(INVALIDINDEX) is signaled.
C
C     4)  If the specified column does not have character type, the
C         error SPICE(INVALIDTYPE) is signaled.
C
C     5)  If this routine is called when no E-kernels have been loaded,
C         the error SPICE(NOLOADEDFILES) is signaled.
C
C$ Files
C
C     See the header of EKQMGR for a description of files used
C     by this routine.
C
C$ Particulars
C
C     This routine allows retrieval of data from character columns.
C
C     This routine returns one element at a time in order to save the
C     caller from imposing a limit on the size of the column entries
C     that can be handled.
C
C$ Examples
C
C     1)  Suppose the EK table TAB contains the following columns:
C
C            Column name   Data Type   Size
C            -----------   ---------   ----
C            CHR_COL_1     CHR         1
C            CHR_COL_2     CHR         VARIABLE
C            CHR_COL_3     CHR         10
C
C
C         Suppose the query
C
C            QUERY = 'SELECT CHR_COL_1 FROM TAB'
C
C         is issued to EKFIND via the call
C
C            CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG )
C
C         To fetch and dump column values from the rows that satisfy the
C         query, the loop below could be used.  Note that we don't check
C         the FOUND flags returned by EKGC since we know that every
C         entry in column CHR_COL_1 contains one element.
C
C            C
C            C     Since CHR_COL_1was the first column selected,
C            C     the selection index SELIDX is set to 1.
C            C     The column is scalar, so the element index ELTIDX
C            C     is set to 1.  The variable NMROWS is the number of
C            C     matching rows returned by EKFIND.
C            C
C
C                  SELIDX = 1
C                  ELTIDX = 1
C
C                  DO ROW = 1, NMROWS
C
C                     WRITE (*,*) ' '
C                     WRITE (*,*) 'ROW  = ', ROW
C                     WRITE (*,*) ' '
C
C            C
C            C        Fetch values from column CHR_COL_1.
C            C
C                     CALL EKGC ( SELIDX,  ROW,     ELTIDX,
C                                 CVAL,    ISNULL,  FOUND   )
C
C                     IF ( ISNULL ) THEN
C                        WRITE (*,*) '<Null>'
C                     ELSE
C                        WRITE (*,*) CVAL
C                     END IF
C
C                  END DO
C
C
C
C     2)  Suppose the EK table TAB is as in example 1, and we issue
C         the query
C
C            QUERY = 'SELECT CHR_COL_1, CHR_COL_2, CHR_COL_3 FROM TAB'
C
C         to EKFIND via the call
C
C            CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG )
C
C         To fetch and dump column values from the rows that satisfy the
C         query, the loop below could be used.  Note that we don't check
C         the FOUND flags returned by EKGC since we know in advance how
C         many elements are contained in each column entry we fetch.
C
C
C                  DO ROW = 1, NMROWS
C
C                     WRITE (*,*) ' '
C                     WRITE (*,*) 'ROW  = ', ROW
C                     WRITE (*,*) ' '
C
C            C
C            C        Fetch values from column CHR_COL_1.  Since
C            C        CHR_COL_1 was the first column selected, the
C            C        selection index SELIDX is set to 1.
C            C
C                     SELIDX = 1
C                     ELTIDX = 1
C                     CALL EKGC ( SELIDX,    ROW,     ELTIDX,
C                                 CVALS(1),  ISNULL,  FOUND   )
C
C                     WRITE (*,*) ' '
C                     WRITE (*,*) 'COLUMN = CHR_COL_1'
C                     WRITE (*,*) ' '
C
C                     IF ( ISNULL ) THEN
C                        WRITE (*,*) '<Null>'
C                     ELSE
C                        WRITE (*,*) CVALS(1)
C                     END IF
C
C            C
C            C        Fetch values from column CHR_COL_2 in the current
C            C        row.  Since CHR_COL_2 contains variable-size array
C            C        elements, we call EKNELT to determine how many
C            C        elements to fetch.
C            C
C                     SELIDX = 2
C                     CALL EKNELT ( SELIDX, ROW, NELT )
C
C                     ELTIDX = 1
C                     ISNULL = .FALSE.
C
C                     DO WHILE (       ( ELTIDX .LE.  NELT   )
C                 .              .AND. (        .NOT. ISNULL )  )
C
C                        CALL EKGC ( SELIDX,         ROW,     ELTIDX,
C                                    CVALS(ELTIDX),  ISNULL,  FOUND   )
C
C                        ELTIDX = ELTIDX + 1
C
C            C
C            C           If the column entry is null, we'll be kicked
C            C           out of this loop after the first iteration.
C            C
C                     END DO
C
C                     WRITE (*,*) ' '
C                     WRITE (*,*) 'COLUMN = CHR_COL_2'
C                     WRITE (*,*) ' '
C
C                     IF ( ISNULL ) THEN
C                        WRITE (*,*) '<Null>'
C                     ELSE
C                        WRITE (*,*) ( CVALS(I), I = 1, NELT )
C                     END IF
C
C            C
C            C        Fetch values from column CHR_COL_3 in the current
C            C        row.  We need not call EKNELT since we know how
C            C        many elements are in each column entry.
C            C
C                     SELIDX = 3
C                     ELTIDX = 1
C                     ISNULL = .FALSE.
C
C                     DO WHILE (       ( ELTIDX .LE.  10    )
C                 .              .AND. (        .NOT. ISNULL )  )
C
C                        CALL EKGC ( SELIDX,         ROW,     ELTIDX,
C                                    CVALS(ELTIDX),  ISNULL,  FOUND   )
C
C                        ELTIDX = ELTIDX + 1
C
C                     END DO
C
C                     WRITE (*,*) ' '
C                     WRITE (*,*) 'COLUMN = CHR_COL_3'
C                     WRITE (*,*) ' '
C
C                     IF ( ISNULL ) THEN
C                        WRITE (*,*) '<Null>'
C                     ELSE
C                        WRITE (*,*) ( CVALS(I), I = 1, 10 )
C                     END IF
C
C                  END DO
C
C
C     3)  See the $Examples section of the umbrella routine EKQMGR
C         for an example in which the names and data types of the
C         columns from which to fetch data are not known in advance.
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
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB)
C
C        Bug fix:   When an already loaded kernel is opened with EKOPR,
C        it now has its link count reset to 1 via a call to EKCLS.
C
C-    SPICELIB Version 1.1.0, 07-JUL-1996 (NJB)
C
C        Redundant CHKIN call removed from SELIDX error check.
C        Misspelling of "issued" was fixed.  Previous version line
C        was changed from "Beta" to "SPICELIB."
C
C-    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB)
C
C-&
 
C$ Index_Entries
C
C     fetch element from character column entry
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 07-JUL-1996 (NJB)
C
C        Redundant CHKIN call removed from SELIDX error check.
C        Misspelling of "issued" was fixed.  Previous version line
C        was changed from "Beta" to "SPICELIB."
C
C-&

 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKGC' )
      END IF
 
C
C     Nothing found yet.
C
      FOUND = .FALSE.
 
C
C     There nothing to fetch if no files are loaded.  A sure
C     symptom of this problem is that the file list is empty.
C
      IF ( FTHEAD .LE. 0 ) THEN
         CALL SETMSG ( 'No E-kernels are currently loaded.' )
         CALL SIGERR ( 'SPICE(NOLOADEDFILES)'               )
         CALL CHKOUT ( 'EKGC'                               )
         RETURN
      END IF
 
C
C     The row number must be valid, or we can't proceed.
C
      IF (  ( ROW .LT. 1 ) .OR. ( ROW .GT. UNROWS )  ) THEN
 
         CALL SETMSG ( 'Row indices for query result range from 1 to '//
     .                 '#; requested row index was #.'                 )
         CALL ERRINT ( '#',  UNROWS                                    )
         CALL ERRINT ( '#',  ROW                                       )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                           )
         CALL CHKOUT ( 'EKGC'                                          )
         RETURN
 
      END IF
 
C
C     The element index must be positive.
C
      IF (  ELMENT .LT. 1 ) THEN
         CALL SETMSG ( 'ELMENT must be positive but was #.' )
         CALL ERRINT ( '#',  ELMENT                         )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                )
         CALL CHKOUT ( 'EKGC'                               )
         RETURN
      END IF
 
C
C     Make sure the SELECT clause column index is valid.
C
      IF (  ( SELIDX .LT. 1 ) .OR. ( SELIDX .GT. NSEL )  ) THEN
         CALL SETMSG ( 'The SELECT column index # is out of the ' //
     .                 'valid range 1:#'                          )
         CALL ERRINT ( '#', SELIDX                                )
         CALL ERRINT ( '#', NTAB                                  )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                      )
         CALL CHKOUT ( 'EKGC'                                     )
         RETURN
      END IF
 
C
C     COL is the column's index within the parent
C     table's column list.
C
      TABIDX  =  SELTAB( SELIDX )
      COL     =  SELCOL( SELIDX )
      COLPTR  =  SELCTP( SELIDX )
      TAB     =  TPTVEC( TABIDX )
 
C
C     Make sure the column has character type.
C
      IF (  CTTYPS(COLPTR) .NE. CHR ) THEN
 
         CALL SETMSG ( 'Column # has data type #.'      )
         CALL ERRCH  ( '#', CTNAMS( COLPTR )            )
         CALL ERRCH  ( '#', CHTYPE( CTTYPS(COLPTR) )    )
         CALL SIGERR ( 'SPICE(INVALIDTYPE)'             )
         CALL CHKOUT ( 'EKGC'                           )
         RETURN
 
      END IF
 
C
C     If it hasn't been done yet, and if it needs to be done, sort the
C     matching row vectors.
C
      IF ( DOSORT ) THEN
 
         CALL ZZEKJSRT ( USIZE,   UBASE,   NORDER,  OTABS,
     .                   OCOLS,   OELTS,   SENSE,   STHAN,
     .                   STDSCS,  STDTPT,  DTPOOL,  DTDSCS,  ORDBAS  )
 
         DOSORT  =  .FALSE.
         SORTED  =  .TRUE.
      END IF
 
C
C     Look up the segment vector and row vector for the current row.
C
      IF ( SORTED ) THEN
         CALL ZZEKSRD  ( ORDBAS+ROW,  ORDBAS+ROW,  I      )
         CALL ZZEKVCAL ( I,           RWVBAS,      SGVBAS )
      ELSE
         CALL ZZEKVCAL ( ROW,         RWVBAS,      SGVBAS )
      END IF
 
      CALL ZZEKSRD  ( RWVBAS+1, RWVBAS+NTAB, ROWVEC )
      CALL ZZEKSRD  ( SGVBAS+1, SGVBAS+NTAB, SEGVEC )
 
C
C     Identify the segment containing the column entry of interest.
C     Obtain the column descriptor for the column.
C
      ROWIDX  =  ROWVEC(TABIDX)
      SEG     =  SEGVEC(TABIDX)
 
      J       =  STDTPT(SEG)
 
      DO I = 2, COL
         J = LNKNXT( J, DTPOOL )
      END DO
 
C
C     Look up the element.
C
      CALL ZZEKRSC ( STHAN(SEG),
     .               STDSCS(1,SEG),
     .               DTDSCS(1,J),
     .               ROWIDX,
     .               ELMENT ,
     .               CVLEN,
     .               CDATA,
     .               NULL,
     .               FOUND         )
 
 
 
      CALL CHKOUT ( 'EKGC' )
      RETURN
 
 
 
 
 
 
C$Procedure     EKGD  ( EK, get event data, double precision )
 
      ENTRY EKGD ( SELIDX, ROW, ELMENT, DDATA, NULL, FOUND )
 
C$ Abstract
C
C     Return an element of an entry in a column of double precision
C     or `time' type in a specified row.
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
C     EK
C
C$ Keywords
C
C     ASSIGNMENT
C     EK
C
C$ Declarations
C
C     INTEGER               SELIDX
C     INTEGER               ROW
C     INTEGER               ELMENT
C     DOUBLE PRECISION      DDATA
C     LOGICAL               NULL
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     SELIDX     I   Index of parent column in SELECT clause.
C     ROW        I   Row to fetch from.
C     ELMENT     I   Index of element, within column entry, to fetch.
C     DDATA      O   D.p. element of column entry.
C     NULL       O   Flag indicating whether column entry was null.
C     FOUND      O   Flag indicating whether column was present in row.
C
C$ Detailed_Input
C
C     SELIDX         is the SELECT clause index of the column to
C                    fetch from.
C
C     ROW            is the output row containing the entry to fetch
C                    from.
C
C     ELMENT         is the index of the element of the column entry
C                    to fetch.  The normal range of ELMENT is from 1 to
C                    the size of the column's entry, but ELMENT is
C                    allowed to exceed the number of elements in the
C                    column entry; if it does, FOUND is returned .FALSE.
C                    This allows the caller to read data from the column
C                    entry in a loop without checking the number of
C                    available elements first.
C
C                    Null values in variable-sized columns are
C                    considered to have size 1.
C
C$ Detailed_Output
C
C     DDATA          is the requested element of the specified column
C                    entry.  If the entry is null, DDATA is undefined.
C
C     NULL           is a logical flag indicating whether the entry
C                    belonging to the specified column in the specified
C                    row is null.
C
C     FOUND          is a logical flag indicating whether the specified
C                    element was found.  If the element does not exist,
C                    FOUND is returned .FALSE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input argument ELMENT is less than 1, FOUND is returned
C         .FALSE., and the error SPICE(INVALIDINDEX) is signaled.
C         However, ELMENT is allowed to be greater than the number of
C         elements in the specified column entry; this allows the caller
C         to read data from the column entry in a loop without checking
C         the number of available elements first.  If ELMENT is greater
C         than the number of available elements, FOUND is returned
C         .FALSE.
C
C     2)  If SELIDX is outside of the range established by the
C         last query passed to EKSRCH, the error SPICE(INVALIDINDEX)
C         will be signaled.
C
C     3)  If the input argument ROW is less than 1 or greater than
C         the number of rows matching the query, FOUND is returned
C        .FALSE., and the error SPICE(INVALIDINDEX) is signaled.
C
C     4)  If the specified column does not have DP or TIME type, the
C         error SPICE(INVALIDTYPE) is signaled.
C
C     5)  If this routine is called when no E-kernels have been loaded,
C         the error SPICE(NOLOADEDFILES) is signaled.
C
C$ Files
C
C     See the header of EKQMGR for a description of files used
C     by this routine.
C
C$ Particulars
C
C     This routine allows retrieval of data from double precision or
C     `time' columns.
C
C     This routine returns one element at a time in order to save the
C     caller from imposing a limit on the size of the column entries
C     that can be handled.
C
C$ Examples
C
C     1)  Suppose the EK table TAB contains the following columns:
C
C            Column name   Data Type   Size
C            -----------   ---------   ----
C            DP_COL_1      DP          1
C            DP_COL_2      DP          VARIABLE
C            DP_COL_3      DP          10
C            TIME          TIME        1
C
C
C         Suppose the query
C
C            QUERY = 'SELECT DP_COL_1 FROM TAB'
C
C         is issued to EKFIND via the call
C
C            CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG )
C
C         To fetch and dump column values from the rows that satisfy the
C         query, the loop below could be used.  Note that we don't check
C         the FOUND flags returned by EKGD since we know that every
C         entry in column DP_COL_1 contains one element.
C
C            C
C            C     Since DP_COL_1was the first column selected,
C            C     the selection index SELIDX is set to 1.
C            C     The column is scalar, so the element index ELTIDX
C            C     is set to 1.  The variable NMROWS is the number of
C            C     matching rows returned by EKFIND.
C            C
C
C                  SELIDX = 1
C                  ELTIDX = 1
C
C                  DO ROW = 1, NMROWS
C
C                     WRITE (*,*) ' '
C                     WRITE (*,*) 'ROW  = ', ROW
C                     WRITE (*,*) ' '
C
C            C
C            C        Fetch values from column DP_COL_1.
C            C
C                     CALL EKGD ( SELIDX,  ROW,     ELTIDX,
C                                 DVAL,    ISNULL,  FOUND   )
C
C                     IF ( ISNULL ) THEN
C                        WRITE (*,*) '<Null>'
C                     ELSE
C                        WRITE (*,*) DVAL
C                     END IF
C
C                  END DO
C
C
C     2)  Suppose the EK table TAB is as in example 1, and we issue
C         the query
C
C            QUERY = 'SELECT TIME FROM TAB'
C
C         to EKFIND via the call
C
C            CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG )
C
C         We wish to dump the time values as UTC calendar dates.
C         The code fragment below carries out this task.  We assume
C         a leapseconds kernel is loaded.  The variable UTC shown
C         below should be declared as a character string.
C
C                  SELIDX = 1
C                  ELTIDX = 1
C
C                  DO ROW = 1, NMROWS
C
C                     WRITE (*,*) ' '
C                     WRITE (*,*) 'ROW  = ', ROW
C                     WRITE (*,*) ' '
C
C            C
C            C        Fetch values from column TIME.
C            C
C                     CALL EKGD ( SELIDX,  ROW,     ELTIDX,
C                                 DVAL,    ISNULL,  FOUND   )
C
C                     IF ( ISNULL ) THEN
C                        WRITE (*,*) '<Null>'
C                     ELSE
C                        CALL ET2UTC ( DVAL, 'C', 3, UTC )
C                        WRITE (*,*) UTC
C                     END IF
C
C                  END DO
C
C
C     3)  Suppose the EK table TAB is as in example 1, and we issue
C         the query
C
C            QUERY = 'SELECT DP_COL_1, DP_COL_2, DP_COL_3 FROM TAB'
C
C         to EKFIND via the call
C
C            CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG )
C
C         To fetch and dump column values from the rows that satisfy the
C         query, the loop below could be used.  Note that we don't check
C         the FOUND flags returned by EKGD since we know in advance how
C         many elements are contained in each column entry we fetch.
C
C                  DO ROW = 1, NMROWS
C
C                     WRITE (*,*) ' '
C                     WRITE (*,*) 'ROW  = ', ROW
C                     WRITE (*,*) ' '
C
C            C
C            C        Fetch values from column DP_COL_1.  Since
C            C        DP_COL_1was the first column selected, the
C            C        selection index SELIDX is set to 1.
C            C
C                     SELIDX = 1
C                     ELTIDX = 1
C                     CALL EKGD ( SELIDX,    ROW,     ELTIDX,
C                                 DVALS(1),  ISNULL,  FOUND   )
C
C                     WRITE (*,*) ' '
C                     WRITE (*,*) 'COLUMN = DP_COL_1'
C                     WRITE (*,*) ' '
C
C                     IF ( ISNULL ) THEN
C                        WRITE (*,*) '<Null>'
C                     ELSE
C                        WRITE (*,*) DVALS(1)
C                     END IF
C
C            C
C            C        Fetch values from column DP_COL_2 in the current
C            C        row.  Since DP_COL_2 contains variable-size array
C            C        elements, we call EKNELT to determine how many
C            C        elements to fetch.
C            C
C                     SELIDX = 2
C                     CALL EKNELT ( SELIDX, ROW, NELT )
C
C                     ELTIDX = 1
C                     ISNULL = .FALSE.
C
C                     DO WHILE (       ( ELTIDX .LE.  NELT   )
C                 .              .AND. (        .NOT. ISNULL )  )
C
C                        CALL EKGD ( SELIDX,         ROW,     ELTIDX,
C                                    DVALS(ELTIDX),  ISNULL,  FOUND   )
C
C                        ELTIDX = ELTIDX + 1
C
C            C
C            C           If the column entry is null, we'll be kicked
C            C           out of this loop after the first iteration.
C            C
C                     END DO
C
C                     WRITE (*,*) ' '
C                     WRITE (*,*) 'COLUMN = DP_COL_2'
C                     WRITE (*,*) ' '
C
C                     IF ( ISNULL ) THEN
C                        WRITE (*,*) '<Null>'
C                     ELSE
C                        WRITE (*,*) ( DVALS(I), I = 1, NELT )
C                     END IF
C
C            C
C            C        Fetch values from column DP_COL_3 in the current
C            C        row.  We need not call EKNELT since we know how
C            C        many elements are in each column entry.
C            C
C                     SELIDX = 3
C                     ELTIDX = 1
C                     ISNULL = .FALSE.
C
C                     DO WHILE (       ( ELTIDX .LE.  10    )
C                 .              .AND. (        .NOT. ISNULL )  )
C
C                        CALL EKGD ( SELIDX,         ROW,     ELTIDX,
C                                    DVALS(ELTIDX),  ISNULL,  FOUND   )
C
C                        ELTIDX = ELTIDX + 1
C
C                     END DO
C
C                     WRITE (*,*) ' '
C                     WRITE (*,*) 'COLUMN = DP_COL_3'
C                     WRITE (*,*) ' '
C
C                     IF ( ISNULL ) THEN
C                        WRITE (*,*) '<Null>'
C                     ELSE
C                        WRITE (*,*) ( DVALS(I), I = 1, 10 )
C                     END IF
C
C                  END DO
C
C
C     4)  See the $Examples section of the umbrella routine EKQMGR
C         for an example in which the names and data types of the
C         columns from which to fetch data are not known in advance.
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
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB)
C
C        Bug fix:   When an already loaded kernel is opened with EKOPR,
C        it now has its link count reset to 1 via a call to EKCLS.
C
C-    SPICELIB Version 1.1.0, 07-JUL-1996 (NJB)
C
C        Redundant CHKIN call removed from SELIDX error check.
C        Misspelling of "issued" was fixed.  Previous version line
C        was changed from "Beta" to "SPICELIB."
C
C-    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB)
C
C-&
 
C$ Index_Entries
C
C     fetch element from double precision column entry
C
C-&
 

C$ Revisions
C
C-    SPICELIB Version 1.1.0, 07-JUL-1996 (NJB)
C
C        Redundant CHKIN call removed from SELIDX error check.
C        Misspelling of "issued" was fixed.  Previous version line
C        was changed from "Beta" to "SPICELIB."
C
C-&

 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKGD' )
      END IF
 
C
C     Nothing found yet.
C
      FOUND = .FALSE.
 
C
C     There nothing to fetch if no files are loaded.  A sure
C     symptom of this problem is that the file list is empty.
C
      IF ( FTHEAD .LE. 0 ) THEN
         CALL SETMSG ( 'No E-kernels are currently loaded.' )
         CALL SIGERR ( 'SPICE(NOLOADEDFILES)'               )
         CALL CHKOUT ( 'EKGD'                               )
         RETURN
      END IF
 
C
C     The row number must be valid, or we can't proceed.
C
      IF (  ( ROW .LT. 1 ) .OR. ( ROW .GT. UNROWS )  ) THEN
 
         CALL SETMSG ( 'Row indices for query result range from 1 to '//
     .                 '#; requested row index was #.'                 )
         CALL ERRINT ( '#',  UNROWS                                    )
         CALL ERRINT ( '#',  ROW                                       )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                           )
         CALL CHKOUT ( 'EKGD'                                          )
         RETURN
 
      END IF
 
C
C     The element index must be positive.
C
      IF (  ELMENT .LT. 1 ) THEN
         CALL SETMSG ( 'ELMENT must be positive but was #.' )
         CALL ERRINT ( '#',  ELMENT                         )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                )
         CALL CHKOUT ( 'EKGD'                               )
         RETURN
      END IF
 
C
C     Make sure the SELECT clause column index is valid.
C
      IF (  ( SELIDX .LT. 1 ) .OR. ( SELIDX .GT. NSEL )  ) THEN
         CALL SETMSG ( 'The SELECT column index # is out of the ' //
     .                 'valid range 1:#'                          )
         CALL ERRINT ( '#', SELIDX                                )
         CALL ERRINT ( '#', NTAB                                  )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                      )
         CALL CHKOUT ( 'EKGD'                                     )
         RETURN
      END IF
 
C
C     COL is the column's index within the parent
C     table's column list.
C
      TABIDX  =  SELTAB( SELIDX )
      COL     =  SELCOL( SELIDX )
      COLPTR  =  SELCTP( SELIDX )
      TAB     =  TPTVEC( TABIDX )
 
C
C     Make sure the column has double precision or `time' type.
C
      IF (       ( CTTYPS(COLPTR) .NE. DP   )
     .     .AND. ( CTTYPS(COLPTR) .NE. TIME )  ) THEN
 
         CALL SETMSG ( 'Column # has data type #.'    )
         CALL ERRCH  ( '#', CTNAMS( COLPTR )          )
         CALL ERRCH  ( '#', CHTYPE( CTTYPS(COLPTR) )  )
         CALL SIGERR ( 'SPICE(INVALIDTYPE)'           )
         CALL CHKOUT ( 'EKGD'                         )
         RETURN
 
      END IF
 
C
C     If it hasn't been done yet, and if it needs to be done, sort the
C     matching row vectors.
C
      IF ( DOSORT ) THEN
 
         CALL ZZEKJSRT ( USIZE,   UBASE,   NORDER,  OTABS,
     .                   OCOLS,   OELTS,   SENSE,   STHAN,
     .                   STDSCS,  STDTPT,  DTPOOL,  DTDSCS,  ORDBAS  )
 
         DOSORT  =  .FALSE.
         SORTED  =  .TRUE.
      END IF
 
C
C     Look up the segment vector and row vector for the current row.
C
      IF ( SORTED ) THEN
         CALL ZZEKSRD  ( ORDBAS+ROW,  ORDBAS+ROW,  I      )
         CALL ZZEKVCAL ( I,           RWVBAS,      SGVBAS )
      ELSE
         CALL ZZEKVCAL ( ROW,         RWVBAS,      SGVBAS )
      END IF
 
      CALL ZZEKSRD  ( RWVBAS+1, RWVBAS+NTAB, ROWVEC )
      CALL ZZEKSRD  ( SGVBAS+1, SGVBAS+NTAB, SEGVEC )
 
C
C     Identify the segment containing the column entry of interest.
C     Obtain the column descriptor for the column.
C
      ROWIDX  =  ROWVEC(TABIDX)
      SEG     =  SEGVEC(TABIDX)
 
      J       =  STDTPT(SEG)
 
      DO I = 2, COL
         J = LNKNXT( J, DTPOOL )
      END DO
 
C
C     Look up the element.
C
      CALL ZZEKRSD ( STHAN (   SEG),
     .               STDSCS(1, SEG),
     .               DTDSCS(1, J  ),
     .               ROWIDX,
     .               ELMENT ,
     .               DDATA,
     .               NULL,
     .               FOUND          )
 
 
      CALL CHKOUT ( 'EKGD' )
      RETURN
 
 
 
 
 
 
 
C$Procedure     EKGI  ( EK, get event data, integer )
 
      ENTRY EKGI ( SELIDX, ROW, ELMENT, IDATA, NULL, FOUND )
 
C$ Abstract
C
C     Return an element of an entry in a column of integer
C     type in a specified row.
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
C     EK
C
C$ Keywords
C
C     ASSIGNMENT
C     EK
C
C$ Declarations
C
C     INTEGER               SELIDX
C     INTEGER               ROW
C     INTEGER               ELMENT
C     INTEGER               IDATA
C     LOGICAL               NULL
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     SELIDX     I   Index of parent column in SELECT clause.
C     ROW        I   Row to fetch from.
C     ELMENT     I   Index of element, within column entry, to fetch.
C     IDATA      O   Integer element of column entry.
C     NULL       O   Flag indicating whether column entry was null.
C     FOUND      O   Flag indicating whether column was present in row.
C
C$ Detailed_Input
C
C     SELIDX         is the SELECT clause index of the column to
C                    fetch from.
C
C     ROW            is the output row containing the entry to fetch
C                    from.
C
C     ELMENT         is the index of the element of the column entry
C                    to fetch.  The normal range of ELMENT is from 1 to
C                    the size of the column's entry, but ELMENT is
C                    allowed to exceed the number of elements in the
C                    column entry; if it does, FOUND is returned .FALSE.
C                    This allows the caller to read data from the column
C                    entry in a loop without checking the number of
C                    available elements first.
C
C                    Null values in variable-sized columns are
C                    considered to have size 1.
C
C$ Detailed_Output
C
C     IDATA          is the requested element of the specified column
C                    entry.  If the entry is null, IDATA is undefined.
C
C     NULL           is a logical flag indicating whether the entry
C                    belonging to the specified column in the specified
C                    row is null.
C
C     FOUND          is a logical flag indicating whether the specified
C                    element was found.  If the element does not exist,
C                    FOUND is returned .FALSE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input argument ELMENT is less than 1, FOUND is returned
C         .FALSE., and the error SPICE(INVALIDINDEX) is signaled.
C         However, ELMENT is allowed to be greater than the number of
C         elements in the specified column entry; this allows the caller
C         to read data from the column entry in a loop without checking
C         the number of available elements first.  If ELMENT is greater
C         than the number of available elements, FOUND is returned
C         .FALSE.
C
C     2)  If SELIDX is outside of the range established by the
C         last query passed to EKSRCH, the error SPICE(INVALIDINDEX)
C         will be signaled.
C
C     3)  If the input argument ROW is less than 1 or greater than
C         the number of rows matching the query, FOUND is returned
C        .FALSE., and the error SPICE(INVALIDINDEX) is signaled.
C
C     4)  If the specified column does not have integer type, the
C         error SPICE(INVALIDTYPE) is signaled.
C
C     5)  If this routine is called when no E-kernels have been loaded,
C         the error SPICE(NOLOADEDFILES) is signaled.
C
C$ Files
C
C     See the header of EKQMGR for a description of files used
C     by this routine.
C
C$ Particulars
C
C     This routine allows retrieval of data from integer columns.
C
C     This routine returns one element at a time in order to save the
C     caller from imposing a limit on the size of the column entries
C     that can be handled.
C
C$ Examples
C
C     1)  Suppose the EK table TAB contains the following columns:
C
C            Column name   Data Type   Size
C            -----------   ---------   ----
C            INT_COL_1     INT         1
C            INT_COL_2     INT         VARIABLE
C            INT_COL_3     INT         10
C
C
C         Suppose the query
C
C            QUERY = 'SELECT INT_COL_1 FROM TAB'
C
C         is issued to EKFIND via the call
C
C            CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG )
C
C         To fetch and dump column values from the rows that satisfy the
C         query, the loop below could be used.  Note that we don't check
C         the FOUND flags returned by EKGI since we know that every
C         entry in column INT_COL_1 contains one element.
C
C            C
C            C     Since INT_COL_1was the first column selected,
C            C     the selection index SELIDX is set to 1.
C            C     The column is scalar, so the element index ELTIDX
C            C     is set to 1.  The variable NMROWS is the number of
C            C     matching rows returned by EKFIND.
C            C
C
C                  SELIDX = 1
C                  ELTIDX = 1
C
C                  DO ROW = 1, NMROWS
C
C                     WRITE (*,*) ' '
C                     WRITE (*,*) 'ROW  = ', ROW
C                     WRITE (*,*) ' '
C
C            C
C            C        Fetch values from column INT_COL_1.
C            C
C                     CALL EKGI ( SELIDX,  ROW,     ELTIDX,
C                                 IVAL,    ISNULL,  FOUND   )
C
C                     IF ( ISNULL ) THEN
C                        WRITE (*,*) '<Null>'
C                     ELSE
C                        WRITE (*,*) IVAL
C                     END IF
C
C                  END DO
C
C
C
C     2)  Suppose the EK table TAB is as in example 1, and we issue
C         the query
C
C            QUERY = 'SELECT INT_COL_1, INT_COL_2, INT_COL_3 FROM TAB'
C
C         to EKFIND via the call
C
C            CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG )
C
C         To fetch and dump column values from the rows that satisfy the
C         query, the loop below could be used.  Note that we don't check
C         the FOUND flags returned by EKGI since we know in advance how
C         many elements are contained in each column entry we fetch.
C
C
C                  DO ROW = 1, NMROWS
C
C                     WRITE (*,*) ' '
C                     WRITE (*,*) 'ROW  = ', ROW
C                     WRITE (*,*) ' '
C
C            C
C            C        Fetch values from column INT_COL_1.  Since
C            C        INT_COL_1 was the first column selected, the
C            C        selection index SELIDX is set to 1.
C            C
C                     SELIDX = 1
C                     ELTIDX = 1
C                     CALL EKGI ( SELIDX,    ROW,     ELTIDX,
C                                 IVALS(1),  ISNULL,  FOUND   )
C
C                     WRITE (*,*) ' '
C                     WRITE (*,*) 'COLUMN = INT_COL_1'
C                     WRITE (*,*) ' '
C
C                     IF ( ISNULL ) THEN
C                        WRITE (*,*) '<Null>'
C                     ELSE
C                        WRITE (*,*) IVALS(1)
C                     END IF
C
C            C
C            C        Fetch values from column INT_COL_2 in the current
C            C        row.  Since INT_COL_2 contains variable-size array
C            C        elements, we call EKNELT to determine how many
C            C        elements to fetch.
C            C
C                     SELIDX = 2
C                     CALL EKNELT ( SELIDX, ROW, NELT )
C
C                     ELTIDX = 1
C                     ISNULL = .FALSE.
C
C                     DO WHILE (       ( ELTIDX .LE.  NELT   )
C                 .              .AND. (        .NOT. ISNULL )  )
C
C                        CALL EKGI ( SELIDX,         ROW,     ELTIDX,
C                                    IVALS(ELTIDX),  ISNULL,  FOUND   )
C
C                        ELTIDX = ELTIDX + 1
C
C            C
C            C           If the column entry is null, we'll be kicked
C            C           out of this loop after the first iteration.
C            C
C                     END DO
C
C                     WRITE (*,*) ' '
C                     WRITE (*,*) 'COLUMN = INT_COL_2'
C                     WRITE (*,*) ' '
C
C                     IF ( ISNULL ) THEN
C                        WRITE (*,*) '<Null>'
C                     ELSE
C                        WRITE (*,*) ( IVALS(I), I = 1, NELT )
C                     END IF
C
C            C
C            C        Fetch values from column INT_COL_3 in the current
C            C        row.  We need not call EKNELT since we know how
C            C        many elements are in each column entry.
C            C
C                     SELIDX = 3
C                     ELTIDX = 1
C                     ISNULL = .FALSE.
C
C                     DO WHILE (       ( ELTIDX .LE.  10    )
C                 .              .AND. (        .NOT. ISNULL )  )
C
C                        CALL EKGI ( SELIDX,         ROW,     ELTIDX,
C                                    IVALS(ELTIDX),  ISNULL,  FOUND   )
C
C                        ELTIDX = ELTIDX + 1
C
C                     END DO
C
C                     WRITE (*,*) ' '
C                     WRITE (*,*) 'COLUMN = INT_COL_3'
C                     WRITE (*,*) ' '
C
C                     IF ( ISNULL ) THEN
C                        WRITE (*,*) '<Null>'
C                     ELSE
C                        WRITE (*,*) ( IVALS(I), I = 1, 10 )
C                     END IF
C
C                  END DO
C
C
C     3)  See the $Examples section of the umbrella routine EKQMGR
C         for an example in which the names and data types of the
C         columns from which to fetch data are not known in advance.
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
C
C$ Version
C
C-    SPICELIB Version 2.0.1, 22-SEP-2004 (EDW)
C
C        Edited 1.1.0 Version entry to not include
C        the token used to mark the $Procedure section.
C
C-    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB)
C
C        Bug fix:   When an already loaded kernel is opened with EKOPR,
C        it now has its link count reset to 1 via a call to EKCLS.
C
C-    SPICELIB Version 1.1.0, 07-JUL-1996 (NJB)
C
C        Redundant CHKIN call removed from SELIDX error check.
C        Misspelling of "issued" was fixed.  Previous version line
C        was changed from "Beta" to "SPICELIB."  Header $Procedure
C        line was corrected to indicate integer data type.
C
C-    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB)
C
C-&
 
C$ Index_Entries
C
C     fetch element from integer column entry
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.1.1, 22-SEP-2004 (EDW)
C
C        Edited 1.1.0 Version entry to not include
C        the token used to mark the $Procedure section.
C
C-    SPICELIB Version 1.1.0, 07-JUL-1996 (NJB)
C
C        Redundant CHKIN call removed from SELIDX error check.
C        Misspelling of "issued" was fixed.  Previous version line
C        was changed from "Beta" to "SPICELIB."  Header $Procedure
C        line was corrected to indicate integer data type.
C
C-&


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKGI' )
      END IF
 
C
C     Nothing found yet.
C
      FOUND = .FALSE.
 
C
C     There nothing to fetch if no files are loaded.  A sure
C     symptom of this problem is that the file list is empty.
C
      IF ( FTHEAD .LE. 0 ) THEN
         CALL SETMSG ( 'No E-kernels are currently loaded.' )
         CALL SIGERR ( 'SPICE(NOLOADEDFILES)'               )
         CALL CHKOUT ( 'EKGI'                               )
         RETURN
      END IF
 
C
C     The row number must be valid, or we can't proceed.
C
      IF (  ( ROW .LT. 1 ) .OR. ( ROW .GT. UNROWS )  ) THEN
 
         CALL SETMSG ( 'Row indices for query result range from 1 to '//
     .                 '#; requested row index was #.'                 )
         CALL ERRINT ( '#',  UNROWS                                    )
         CALL ERRINT ( '#',  ROW                                       )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                           )
         CALL CHKOUT ( 'EKGI'                                          )
         RETURN
 
      END IF
 
C
C     The element index must be positive.
C
      IF (  ELMENT .LT. 1 ) THEN
         CALL SETMSG ( 'ELMENT must be positive but was #.' )
         CALL ERRINT ( '#',  ELMENT                         )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                )
         CALL CHKOUT ( 'EKGI'                               )
         RETURN
      END IF
 
C
C     Make sure the SELECT clause column index is valid.
C
      IF (  ( SELIDX .LT. 1 ) .OR. ( SELIDX .GT. NSEL )  ) THEN
         CALL SETMSG ( 'The SELECT column index # is out of the ' //
     .                 'valid range 1:#'                          )
         CALL ERRINT ( '#', SELIDX                                )
         CALL ERRINT ( '#', NTAB                                  )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                      )
         CALL CHKOUT ( 'EKGI'                                     )
         RETURN
      END IF
 
C
C     COL is the column's index within the parent
C     table's column list.
C
      TABIDX  =  SELTAB( SELIDX )
      COL     =  SELCOL( SELIDX )
      COLPTR  =  SELCTP( SELIDX )
      TAB     =  TPTVEC( TABIDX )
 
C
C     Make sure the column has integer type.
C
      IF ( CTTYPS(COLPTR) .NE. INT ) THEN
 
         CALL SETMSG ( 'Column # has data type #.'    )
         CALL ERRCH  ( '#', CTNAMS( COLPTR )          )
         CALL ERRCH  ( '#', CHTYPE( CTTYPS(COLPTR) )  )
         CALL SIGERR ( 'SPICE(INVALIDTYPE)'           )
         CALL CHKOUT ( 'EKGI'                         )
         RETURN
 
      END IF
 
C
C     If it hasn't been done yet, and if it needs to be done, sort the
C     matching row vectors.
C
      IF ( DOSORT ) THEN
 
         CALL ZZEKJSRT ( USIZE,   UBASE,   NORDER,  OTABS,
     .                   OCOLS,   OELTS,   SENSE,   STHAN,
     .                   STDSCS,  STDTPT,  DTPOOL,  DTDSCS,  ORDBAS  )
 
         DOSORT  =  .FALSE.
         SORTED  =  .TRUE.
      END IF
 
C
C     Look up the segment vector and row vector for the current row.
C
      IF ( SORTED ) THEN
         CALL ZZEKSRD  ( ORDBAS+ROW,  ORDBAS+ROW,  I      )
         CALL ZZEKVCAL ( I,           RWVBAS,      SGVBAS )
      ELSE
         CALL ZZEKVCAL ( ROW,         RWVBAS,      SGVBAS )
      END IF
 
      CALL ZZEKSRD  ( RWVBAS+1, RWVBAS+NTAB, ROWVEC )
      CALL ZZEKSRD  ( SGVBAS+1, SGVBAS+NTAB, SEGVEC )
 
C
C     Identify the segment containing the column entry of interest.
C     Obtain the column descriptor for the column.
C
      ROWIDX  =  ROWVEC(TABIDX)
      SEG     =  SEGVEC(TABIDX)
 
      J       =  STDTPT(SEG)
 
      DO I = 2, COL
         J = LNKNXT( J, DTPOOL )
      END DO
 
C
C     Look up the element.
C
      CALL ZZEKRSI ( STHAN (   SEG),
     .               STDSCS(1, SEG),
     .               DTDSCS(1, J  ),
     .               ROWIDX,
     .               ELMENT ,
     .               IDATA,
     .               NULL,
     .               FOUND           )
 
 
      CALL CHKOUT ( 'EKGI' )
      RETURN
      END
