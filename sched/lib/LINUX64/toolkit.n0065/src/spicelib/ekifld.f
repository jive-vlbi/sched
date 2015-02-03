C$Procedure      EKIFLD ( EK, initialize segment for fast write )
 
      SUBROUTINE EKIFLD (  HANDLE,  TABNAM,  NCOLS,  NROWS,
     .                     CNAMES,  DECLS,   SEGNO,  RCPTRS  )
 
C$ Abstract
C
C     Initialize a new E-kernel segment to allow fast writing.
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
 
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               HANDLE
      CHARACTER*(*)         TABNAM
      INTEGER               NCOLS
      INTEGER               NROWS
      CHARACTER*(*)         CNAMES ( * )
      CHARACTER*(*)         DECLS  ( * )
      INTEGER               SEGNO
      INTEGER               RCPTRS ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     TABNAM     I   Table name.
C     NCOLS      I   Number of columns in the segment.
C     NROWS      I   Number of rows in the segment.
C     CNAMES     I   Names of columns.
C     DECLS      I   Declarations of columns.
C     SEGNO      O   Segment number.
C     RCPTRS     O   Array of record pointers.
C
C$ Detailed_Input
C
C     HANDLE         is the handle of an EK file open for write access.
C                    A new segment is to be created in this file.
C
C     TABNAM         is the name of the EK table to which the current
C                    segment belongs.  All segments in the EK file
C                    designated by HANDLE must have identical column
C                    attributes. TABNAM must not exceed TNAMSZ (64)
C                    characters in length.  Case is not significant.
C                    Table names must start with a letter and contain
C                    only characters from the set {A-Z,a-z,0-9,$,_}.
C
C     NCOLS          is the number of columns in a new segment.
C
C     NROWS          is the number of rows in a new segment.  Each
C                    column to be added to the segment must contain
C                    the number of entries indicated by NROWS.
C
C     CNAMES,
C     DECLS          are, respectively, and array of column names and
C                    their corresponding declarations:  the Ith element
C                    of CNAMES and the Ith element of DECLS apply to
C                    the Ith column in the segment.
C
C                    Column names must not exceed CNAMSZ (32) characters
C                    in length.  Case is not significant.  Column names
C                    must start with a letter and contain only
C                    characters from the set {A-Z,a-z,0-9,$,_}.
C
C                    The declarations are strings that contain
C                    `keyword=value' assignments that define the
C                    attributes of the columns to which they apply.  The
C                    column attributes that are defined by a column
C                    declaration are:
C
C                       DATATYPE
C                       SIZE
C                       <is the column indexed?>
C                       <does the column allow null values?>
C
C                    The form of a declaration is
C
C                       'DATATYPE  = <type>,
C                        SIZE      = <size>,
C                        INDEXED   = <boolean>,
C                        NULLS_OK  = <boolean>'
C
C                    For example, an indexed, scalar, integer column
C                    that allows null values would have the declaration
C
C                       'DATATYPE  = INTEGER,
C                        SIZE      = 1,
C                        INDEXED   = TRUE,
C                        NULLS_OK  = TRUE'
C
C                    Commas are required to separate the assignments
C                    within declarations; white space is optional;
C                    case is not significant.
C
C                    The order in which the attribute keywords are
C                    listed in declaration is not significant.
C
C                    Every column in a segment must be declared.
C
C                    Each column entry is effectively an array, each
C                    element of which has the declared data type.  The
C                    SIZE keyword indicates how many elements are in
C                    each entry of the column in whose declaration the
C                    keyword appears.  Note that only scalar-valued
C                    columns (those for which SIZE = 1) may be
C                    referenced in query constraints.  A size
C                    assignment has the syntax
C
C                       SIZE = <integer>
C
C                    or
C                       SIZE = VARIABLE
C
C                    The size value defaults to 1 if omitted.
C
C                    The DATATYPE keyword defines the data type of
C                    column entries.  The DATATYPE assignment syntax
C                    has any of the forms
C
C                       DATATYPE = CHARACTER*(<length>)
C                       DATATYPE = CHARACTER*(*)
C                       DATATYPE = DOUBLE PRECISION
C                       DATATYPE = INTEGER
C                       DATATYPE = TIME
C
C                    As the datatype declaration syntax suggests,
C                    character strings may have fixed or variable
C                    length.  Variable-length strings are allowed only
C                    in columns of size 1.
C
C                    Optionally, scalar-valued columns may be indexed.
C                    To create an index for a column, use the assignment
C
C                       INDEXED = TRUE
C
C                    By default, columns are not indexed.
C
C                    Optionally, any column can allow null values.  To
C                    indicate that a column may allow null values, use
C                    the assigment
C
C                       NULLS_OK = TRUE
C
C                    in the column declaration.  By default, null
C                    values are not allowed in column entries.
C
C$ Detailed_Output
C
C     SEGNO          is the number of the segment created by this
C                    routine.  Segment numbers are used as unique
C                    identifiers by other EK access routines.
C
C     RCPTRS         is an array of record pointers for the input
C                    segment.  This array must not be modified by the
C                    caller.
C
C                    The array RCPTRS must be passed as an input to
C                    each column addition routine called while
C                    writing the specified segment.
C
C                    RCPTRS must be declared with dimension NROWS.
C
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If HANDLE is invalid, the error will be diagnosed by routines
C         called by this routine.
C
C     2)  If TABNAM is more than TNAMSZ characters long, the error
C         is diagnosed by routines called by this routine.
C
C     3)  If TABNAM contains any nonprintable characters, the error
C         is diagnosed by routines called by this routine.
C
C     4)  If NCOLS is non-positive, the error is diagnosed by routines
C         called by this routine.
C
C     5)  If NROWS is non-positive, the error SPICE(INVALIDCOUNT)
C         is signalled.
C
C     6)  If any column name exceeds CNAMSZ characters in length, the
C         error is diagnosed by routines called by this routine.
C
C     7)  If any column name contains non-printable characters, the
C         error is diagnosed by routines called by this routine.
C
C     8)  If a declaration cannot be understood by this routine, the
C         error is diagnosed by routines called by this routine.
C
C     9)  If an non-positive string length or element size is specified,
C         the error is diagnosed by routines called by this routine.
C
C    10)  If an I/O error occurs while reading or writing the indicated
C         file, the error will be diagnosed by routines called by this
C         routine.
C
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This routine prepares an EK for the creation of a new segment via
C     the fast column writer routines.  After this routine is called,
C     the columns of the segment are filled in by calls to the fast
C     column writer routines of the appropriate data types.  The fast
C     column writer routines are:
C
C        EKACLC {EK, add column, character}
C        EKACLD {EK, add column, double precision}
C        EKACLI {EK, add column, integer}
C
C     When all of the columns have been added, the write operation is
C     completed by a call to EKFFLD {EK, finish fast write}.
C
C     The segment is not valid until EKFFLD has been called.
C
C     The EK system supports only one fast write at a time.  It is
C     not possible use the fast write routines to simultaneously write
C     multiple segments, either in the same EK file or in different
C     files.
C
C$ Examples
C
C     1)  Suppose we have an E-kernel named ORDER_DB.EK which contains
C         records of orders for data products.  The E-kernel has a
C         table called DATAORDERS that consists of the set of columns
C         listed below:
C
C            DATAORDERS
C
C               Column Name     Data Type
C               -----------     ---------
C               ORDER_ID        INTEGER
C               CUSTOMER_ID     INTEGER
C               LAST_NAME       CHARACTER*(*)
C               FIRST_NAME      CHARACTER*(*)
C               ORDER_DATE      TIME
C               COST            DOUBLE PRECISION
C
C         The order database also has a table of items that have been
C         ordered.  The columns of this table are shown below:
C
C            DATAITEMS
C
C               Column Name     Data Type
C               -----------     ---------
C               ITEM_ID         INTEGER
C               ORDER_ID        INTEGER
C               ITEM_NAME       CHARACTER*(*)
C               DESCRIPTION     CHARACTER*(*)
C               PRICE           DOUBLE PRECISION
C
C
C         We'll suppose that the file ORDER_DB.EK contains two segments,
C         the first containing the DATAORDERS table and the second
C         containing the DATAITEMS table.
C
C         Below, we show how we'd open a new EK file and create the
C         first of the segments described above.
C
C
C            C
C            C     Open a new EK file.  For simplicity, we will not
C            C     reserve any space for the comment area, so the
C            C     number of reserved comment characters is zero.
C            C     The variable IFNAME is the internal file name.
C            C
C                  NRESVC  =  0
C                  IFNAME  =  'Test EK/Created 20-SEP-1995'
C
C                  CALL EKOPN ( 'ORDER_DB.EK', IFNAME, NRESVC, HANDLE )
C
C            C
C            C     Set up the table and column names and declarations
C            C     for the DATAORDERS segment.  We'll index all of
C            C     the columns.  All columns are scalar, so we omit
C            C     the size declaration.  Only the COST column may take
C            C     null values.
C            C
C                  TABLE     =  'DATAORDERS'
C                  NCOLS     =  6
C
C                  CNAMES(1) =  'ORDER_ID'
C                  CDECLS(1) =  'DATATYPE = INTEGER, INDEXED = TRUE'
C
C                  CNAMES(2) =  'CUSTOMER_ID'
C                  CDECLS(2) =  'DATATYPE = INTEGER, INDEXED = TRUE'
C
C                  CNAMES(3) =  'LAST_NAME'
C                  CDECLS(3) =  'DATATYPE = CHARACTER*(*),' //
C                 .             'INDEXED  = TRUE'
C
C                  CNAMES(4) =  'FIRST_NAME'
C                  CDECLS(4) =  'DATATYPE = CHARACTER*(*),' //
C                 .             'INDEXED  = TRUE'
C
C                  CNAMES(5) =  'ORDER_DATE'
C                  CDECLS(5) =  'DATATYPE = TIME, INDEXED  = TRUE'
C
C                  CNAMES(6) =  'COST'
C                  CDECLS(6) =  'DATATYPE = DOUBLE PRECISION,' //
C                 .             'INDEXED  = TRUE'           //
C                 .             'NULLS_OK = TRUE'
C
C            C
C            C     Start the segment.  We presume the number of  rows
C            C     of data is known in advance.
C            C
C                  CALL EKIFLD ( HANDLE,  TABNAM,  NCOLS,  NROWS,
C                 .              CNAMES,  CDECLS,  SEGNO,  RCPTRS )
C
C            C
C            C     At this point, arrays containing data for the
C            C     segment's columns may be filled in.  The names
C            C     of the data arrays are shown below.
C            C
C            C        Column           Data array
C            C
C            C        'ORDER_ID'       ORDIDS
C            C        'CUSTOMER_ID'    CSTIDS
C            C        'LAST_NAME'      LNAMES
C            C        'FIRST_NAME'     FNAMES
C            C        'ORDER_DATE'     ONAMES
C            C        'COST'           COSTS
C            C
C
C                     [ Fill in data arrays here.]
C
C            C
C            C     The SIZES array shown below is ignored for scalar
C            C     and fixed-size array columns, so we need not
C            C     initialize it.  For variable-size arrays, the
C            C     Ith element of the SIZES array must contain the size
C            C     of the Ith column entry in the column being written.
C            C     Normally, the SIZES array would be reset for each
C            C     variable-size column.
C            C
C            C     The NLFLGS array indicates which entries are null.
C            C     It is ignored for columns that don't allow null
C            C     values.  In this case, only the COST column allows
C            C     nulls.
C            C
C            C     Add the columns of data to the segment.  All of the
C            C     data for each column is written in one shot.
C            C
C                  CALL EKACLI ( HANDLE, SEGNO,  'ORDER_ID',
C                 .              ORDIDS, SIZES,  NLFLGS,  WKINDX )
C
C                  CALL EKACLI ( HANDLE, SEGNO,  'CUSTOMER_ID',
C                 .              CSTIDS, SIZES,  NLFLGS,  WKINDX )
C
C                  CALL EKACLC ( HANDLE, SEGNO,  'LAST_NAME',
C                 .              LNAMES, SIZES,  NLFLGS,  WKINDX )
C
C                  CALL EKACLC ( HANDLE, SEGNO,  'FIRST_NAME',
C                 .              FNAMES, SIZES,  NLFLGS,  WKINDX )
C
C
C                  CALL UTC2ET ( ODATE,  ET )
C                  CALL EKACLD ( HANDLE, SEGNO,  'ORDER_DATE',
C                 .              ODATES, SIZES,  NLFLGS,  WKINDX )
C
C
C                     [Set the NLFLGS array here.]
C
C                  CALL EKACLD ( HANDLE, SEGNO,  'COST',
C                 .              COSTS,  SIZES,  NLFLGS,  WKINDX )
C
C            C
C            C     Complete the segment.  The RCPTRS array is that
C            C     returned by EKIFLD.
C            C
C                  CALL EKFFLD ( HANDLE, SEGNO, RCPTRS )
C
C            C
C            C     At this point, the second segment could be
C            C     created by an analogous process.  In fact, the
C            C     second segment could be created at any time; it is
C            C     not necessary to populate the first segment with
C            C     data before starting the second segment.
C            C
C
C            C
C            C     The file must be closed by a call to EKCLS.
C            C
C                  CALL EKCLS ( HANDLE )
C
C
C$ Restrictions
C
C     1)  Only one segment can be created at a time using the fast
C         write routines.
C
C     2)  No other EK operation may interrupt a fast write.  For
C         example, it is not valid to issue a query while a fast write
C         is in progress.
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
C-    SPICELIB Version 1.1.1, 10-JAN-2002 (NJB)
C
C        Documentation change:  instances of the phrase "fast load"
C        were replaced with "fast write."  Corrected value of table
C        name size in header comment.
C
C-    SPICELIB Version 1.1.0, 18-JUN-1999 (WLT)
C
C        Balanced CHKIN/CHKOUT calls.
C
C-    Beta Version 1.0.0, 25-OCT-1995 (NJB)
C
C-&
 
C$ Index_Entries
C
C     start new E-kernel segment for fast writing
C     start new EK segment for fast writing
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               MBASE
      INTEGER               P
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               STYPE
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKIFLD' )
      END IF
 
C
C     Check out NROWS.
C
      IF ( NROWS .LT. 1 ) THEN
 
         CALL SETMSG ( 'Number of rows must be > 0, was #. ' )
         CALL ERRINT ( '#',  NROWS                           )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                 )
         CALL CHKOUT ( 'EKIFLD'                              )
         RETURN
 
      END IF
 
C
C     Create the segment's metadata.
C
      CALL EKBSEG ( HANDLE, TABNAM, NCOLS, CNAMES, DECLS, SEGNO )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'EKIFLD' )
         RETURN
      END IF
 
C
C     Fill the number of rows into the (file's) segment descriptor.
C
      CALL ZZEKMLOC (  HANDLE,  SEGNO,        P,            MBASE  )
      CALL DASUDI   (  HANDLE,  MBASE+NRIDX,  MBASE+NRIDX,  NROWS  )
 
C
C     Read in the segment descriptor, and get the segment's type.
C
      CALL ZZEKSDSC (  HANDLE,  SEGNO,  SEGDSC )
 
      STYPE  =  SEGDSC(EKTIDX)
 
C
C     Complete the fast write preparations appropriate to the segment's
C     type.
C
      IF ( STYPE .EQ. 1 ) THEN
 
         CALL ZZEKIF01 ( HANDLE, SEGNO, RCPTRS )
 
 
      ELSE IF ( STYPE .EQ. 2 ) THEN
 
         CALL ZZEKIF02 ( HANDLE, SEGNO )
 
      ELSE
 
         CALL SETMSG ( 'Segment type # is not currently supported.' )
         CALL ERRINT ( '#', STYPE                                   )
         CALL SIGERR ( 'SPICE(BUG)'                                 )
         CALL CHKOUT ( 'EKIFLD'                                     )
         RETURN
 
      END IF
 
 
      CALL CHKOUT ( 'EKIFLD' )
      RETURN
      END
