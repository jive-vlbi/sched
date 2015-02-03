C$Procedure     EKACLD ( EK, add d.p. column to segment )
 
      SUBROUTINE EKACLD (  HANDLE,  SEGNO,   COLUMN,  DVALS,
     .                     ENTSZS,  NLFLGS,  RCPTRS,  WKINDX  )
 
C$ Abstract
C
C     Add an entire double precision column to an EK segment.
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
 
 
      INCLUDE  'ekcoldsc.inc'
      INCLUDE  'eksegdsc.inc'
      INCLUDE  'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               SEGNO
      CHARACTER*(*)         COLUMN
      DOUBLE PRECISION      DVALS  ( * )
      INTEGER               ENTSZS ( * )
      LOGICAL               NLFLGS ( * )
      INTEGER               RCPTRS ( * )
      INTEGER               WKINDX ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   EK file handle.
C     SEGNO      I   Number of segment to add column to.
C     COLUMN     I   Column name.
C     DVALS      I   Double precision values to add to column.
C     ENTSZS     I   Array of sizes of column entries.
C     NLFLGS     I   Array of null flags for column entries.
C     RCPTRS     I   Record pointers for segment.
C     WKINDX    I-O  Work space for column index.
C
C$ Detailed_Input
C
C     HANDLE         the handle of an EK file that is open for writing.
C                    A "begin segment for fast write" operation must
C                    have already been performed for the designated
C                    segment.
C
C     SEGNO          is the number of the segment to which
C                    data is to be added.
C
C     COLUMN         is the name of the column to be added.  All of
C                    the data for the named column will be added in
C                    one shot.
C
C     DVALS          is an array containing the entire set of column
C                    entries for the specified column.  The entries
C                    are listed in row-order:  the column entry for the
C                    first row of the segment is first, followed by the
C                    column entry for the second row, and so on.  The
C                    number of column entries must match the declared
C                    number of rows in the segment.  For columns having
C                    fixed-size entries, a null entry must be allocated
C                    the same amount of space occupied by a non-null
C                    entry in the array DVALS.  For columns having
C                    variable-size entries, null entries do not require
C                    any space in the DVALS array, but in any case must
C                    have their allocated space described correctly by
C                    the corresponding element of the ENTSZS array
C                    (described below).
C
C     ENTSZS         is an array containing sizes of column entries.
C                    The Ith element of ENTSZS gives the size of the
C                    Ith column entry.  ENTSZS is used only for columns
C                    having variable-size entries.  For such columns,
C                    the dimension of ENTSZS must be at least NROWS.
C                    The size of null entries should be set to zero.
C
C                    For columns having fixed-size entries, the
C                    dimension of this array may be any positive value.
C
C     NLFLGS         is an array of logical flags indicating whether
C                    the corresponding entries are null.  If the Ith
C                    element of NLFLGS is .FALSE., the Ith column entry
C                    defined by DVALS and ENTSZS is added to the
C                    current segment in the specified kernel file.
C
C                    If the Ith element of NLFGLS is .TRUE., the
C                    contents of the Ith column entry are undefined.
C
C                    NLFLGS is used only for columns that allow null
C                    values; it's ignored for other columns.
C
C     RCPTRS         is an array of record pointers for the input
C                    segment.  This array is obtained as an output
C                    from EKIFLD, the routine called to initiate a
C                    fast write.
C
C     WKINDX         is a work space array used for building a column
C                    index.  If the column is indexed, the dimension of
C                    WKINDX must be at NROWS, where NROWS is the number
C                    of rows in the column.  If the column is not
C                    indexed, this work space is not used, so the
C                    dimension may be any positive value.
C
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
C     1)  If HANDLE is invalid, the error will be diagnosed by routines
C         called by this routine.
C
C     2)  If COLUMN is not the name of a declared column, the error will
C         be diagnosed by routines called by this routine.
C
C     3)  If COLUMN specifies a column of whose data type is not
C         integer, the error SPICE(WRONGDATATYPE) will be signalled.
C
C     4)  If the specified column already contains ANY entries, the
C         error will be diagnosed by routines called by this routine.
C
C     5)  If an I/O error occurs while reading or writing the indicated
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
C     This routine operates by side effects:  it modifies the named
C     EK file by adding data to the specified column.  This routine
C     writes the entire contents of the specified column in one shot.
C     This routine creates columns much more efficiently than can be
C     done by sequential calls to EKACED, but has the drawback that
C     the caller must use more memory for the routine's inputs.  This
C     routine cannot be used to add data to a partially completed
C     column.
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
C            C     of the Ith column entry in the column being loaded.
C            C     Normally, the SIZES array would be reset for each
C            C     variable-size column.
C            C
C            C     The NLFLGS array indicates which entries are null.
C            C     It is ignored for columns that don't allow null
C            C     values.  In this case, only the COST column allows
C            C     nulls.
C            C
C            C     Add the columns of data to the segment.  All of the
C            C     data for each column is loaded in one shot.
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
C-    SPICELIB Version 1.0.1, 09-JAN-2002 (NJB)
C
C        Documentation change:  instances of the phrase "fast load"
C        were replaced with "fast write."
C
C-    Beta Version 1.0.0, 08-NOV-1995 (NJB)
C
C-&
 
 
C$ Index_Entries
C
C     write entire d.p. column to EK segment
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
      INTEGER               CLASS
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               DTYPE
      INTEGER               SEGDSC ( SDSCSZ )
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKACLD' )
      END IF
 
C
C     Find the descriptors for the specified segment and column.
C
      CALL ZZEKSDSC ( HANDLE,  SEGNO,   SEGDSC          )
      CALL ZZEKCDSC ( HANDLE,  SEGDSC,  COLUMN,  COLDSC )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'EKACLD' )
         RETURN
      END IF
 
C
C     This column had better be of d.p. type.
C
      CLASS  =  COLDSC ( CLSIDX )
      DTYPE  =  COLDSC ( TYPIDX )
 
      IF ( ( DTYPE .NE. DP ) .AND. ( DTYPE .NE. TIME )  ) THEN
 
         CALL SETMSG ( 'Column # is of type #; EKACLD only works '  //
     .                 'with d.p. or TIME columns.'                  )
         CALL ERRCH  ( '#',  COLUMN                                  )
         CALL ERRINT ( '#',  DTYPE                                   )
         CALL SIGERR ( 'SPICE(WRONGDATATYPE)'                        )
         CALL CHKOUT ( 'EKACLD'                                      )
         RETURN
 
      END IF
 
C
C     Hand off the task to the routine of the appropriate class.
C
      IF ( CLASS .EQ. 2 ) THEN
C
C        Class 2 columns contain d.p. scalars.
C
         CALL ZZEKAC02 (  HANDLE,  SEGDSC,  COLDSC,  DVALS,
     .                             NLFLGS,  RCPTRS,  WKINDX )
 
 
      ELSE IF ( CLASS .EQ. 5 ) THEN
C
C        Class 5 columns contain d.p. arrays.
C
         CALL ZZEKAC05 ( HANDLE, SEGDSC, COLDSC, DVALS, ENTSZS, NLFLGS )
 
 
      ELSE IF ( CLASS .EQ. 8 ) THEN
C
C        Class 8 columns contain fixed-count d.p. scalars.
C
         CALL ZZEKAC08 ( HANDLE, SEGDSC, COLDSC, DVALS, NLFLGS, WKINDX )
 
 
      ELSE
C
C        This is an unsupported column class.
C
         CALL SETMSG ( 'Unsupported column class code # found in '   //
     .                 'descriptor for column #.'                      )
         CALL ERRINT ( '#', CLASS                                      )
         CALL ERRCH  ( '#', COLUMN                                     )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                           )
         CALL CHKOUT ( 'EKACLD'                                        )
         RETURN
 
      END IF
 
      CALL CHKOUT ( 'EKACLD' )
      RETURN
      END
