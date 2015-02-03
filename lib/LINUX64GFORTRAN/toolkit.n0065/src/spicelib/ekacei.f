C$Procedure     EKACEI ( EK, add integer data to column )
 
      SUBROUTINE EKACEI (  HANDLE,  SEGNO,  RECNO,  COLUMN,
     .                     NVALS,   IVALS,  ISNULL          )
 
C$ Abstract
C
C     Add data to an integer column in a specified EK record.
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
 
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               SEGNO
      INTEGER               RECNO
      CHARACTER*(*)         COLUMN
      INTEGER               NVALS
      INTEGER               IVALS  ( * )
      LOGICAL               ISNULL
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   EK file handle.
C     SEGNO      I   Index of segment containing record.
C     RECNO      I   Record to which data is to be added.
C     COLUMN     I   Column name.
C     NVALS      I   Number of values to add to column.
C     IVALS      I   Integer values to add to column.
C     ISNULL     I   Flag indicating whether column entry is null.
C
C$ Detailed_Input
C
C     HANDLE         is the handle of an EK file open for write access.
C
C     SEGNO          is the index of the segment to which data is to
C                    be added.
C
C     RECNO          is the index of the record to which data is to be
C                    added.  This record number is relative to the start
C                    of the segment indicated by SEGNO; the first
C                    record in the segment has index 1.
C
C     COLUMN         is the name of the column to which data is to be
C                    added.
C
C     NVALS,
C     IVALS          are, respectively, the number of values to add to
C                    the specified column and the set of values
C                    themselves.  The data values are written into the
C                    specified column and record.
C
C                    If the column has fixed-size entries, then NVALS
C                    must equal the entry size for the specified column.
C
C                    Only one value can be added to a virtual column.
C
C
C     ISNULL         is a logical flag indicating whether the entry is
C                    null.  If ISNULL is .FALSE., the column entry
C                    defined by NVALS and IVALS is added to the
C                    specified kernel file.
C
C                    If ISNULL is .TRUE., NVALS and IVALS are ignored.
C                    The contents of the column entry are undefined.
C                    If the column has fixed-length, variable-size
C                    entries, the number of entries is considered to
C                    be 1.
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
C     2)  If SEGNO is out of range, the error will be diagnosed by
C         routines called by this routine.
C
C     3)  If COLUMN is not the name of a declared column, the error
C         will be diagnosed by routines called by this routine.
C
C     4)  If COLUMN specifies a column of whose data type is not
C         integer, the error SPICE(WRONGDATATYPE) will be
C         signalled.
C
C     5)  If RECNO is out of range, the error will be diagnosed by
C         routines called by this routine.
C
C     6)  If the specified column has fixed-size entries and NVALS
C         does not match this size, the error will be diagnosed by
C         routines called by this routine.
C
C     7)  If the specified column has variable-size entries and NVALS
C         is non-positive, the error will be diagnosed by routines
C         called by this routine.
C
C     8)  If an attempt is made to add a null value to a column that
C         doesn't take null values, the error will be diagnosed by
C         routines called by this routine.
C
C     9)  If COLUMN specifies a column of whose class is not
C         an character class known to this routine, the error
C         SPICE(NOCLASS) will be signalled.
C
C     10) If an I/O error occurs while reading or writing the indicated
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
C     EK file by adding data to the specified record in the specified
C     column.  Data may be added to a segment in random order; it is not
C     necessary to fill in columns or rows sequentially.  Data may only
C     be added one column entry at a time.
C
C$ Examples
C
C     1)  Add the value 999 to the third record of the column ICOL in
C         the fifth segment of an EK file designated by HANDLE.
C
C            CALL EKACEI ( HANDLE, 5, 3, 'ICOL', 1, 999, .FALSE. )
C
C
C     2)  Same as (1), but this time add a null value.  The argument
C         999 is ignored because the null flag is set to .TRUE.
C
C            CALL EKACEI ( HANDLE, 5, 3, 'ICOL', 1, 999, .TRUE. )
C
C
C     3)  Add an array IBUFF of 10 values to the third record of the
C         column IARRAY in the fifth segment of an EK file designated by
C         HANDLE.
C
C            CALL EKACEI ( HANDLE, 5, 3, 'IARRAY', 10, IBUFF, .FALSE. )
C
C
C     4)  A more detailed example.
C
C         Suppose we have an E-kernel named ORDER_DB.EK which contains
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
C         If we wanted to insert a new record into the DATAORDERS
C         table in position 1, we'd make the following calls:
C
C            C
C            C     Open the database for write access.  This call is
C            C     made when the file already exists.  See EKOPN for
C            C     an example of creating a new file.
C            C
C                  CALL EKOPW ( 'ORDER_DB.EK', HANDLE )
C
C            C
C            C     Append a new, empty record to the DATAORDERS
C            C     table. Recall that the DATAORDERS table
C            C     is in segment number 1.  The call will return
C            C     the number of the new, empty record.
C            C
C                  CALL EKAPPR ( HANDLE, 1, RECNO )
C
C            C
C            C     At this point, the new record is empty.  A valid EK
C            C     cannot contain empty records.  We fill in the data
C            C     here.  Data items are filled in one column at a time.
C            C     The order in which the columns are filled in is not
C            C     important.  We use the EKACEx (add column entry)
C            C     routines to fill in column entries.  We'll assume
C            C     that no entries are null.  All entries are scalar,
C            C     so the entry size is 1.
C            C
C                  ISNULL   =  .FALSE.
C                  ESIZE    =  1
C
C            C
C            C     The following variables will contain the data for
C            C     the new record.
C            C
C                  ORDID    =   10011
C                  CUSTID   =   531
C                  LNAME    =   'Scientist'
C                  FNAME    =   'Joe'
C                  ODATE    =   '1995-SEP-20'
C                  COST     =   0.D0
C
C            C
C            C     Note that the names of the routines called
C            C     correspond to the data types of the columns:  the
C            C     last letter of the routine name is C, I, or D,
C            C     depending on the data type.  Time values are
C            C     converted to ET for storage.
C            C
C                  CALL EKACEI ( HANDLE, SEGNO,  RECNO, 'ORDER_ID',
C                 .              SIZE,   ORDID,  ISNULL               )
C
C                  CALL EKACEI ( HANDLE, SEGNO,  RECNO, 'CUSTOMER_ID',
C                 .              SIZE,   CUSTID, ISNULL               )
C
C                  CALL EKACEC ( HANDLE, SEGNO,  RECNO, 'LAST_NAME',
C                 .              SIZE,   LNAME,  ISNULL               )
C
C                  CALL EKACEC ( HANDLE, SEGNO,  RECNO, 'FIRST_NAME',
C                 .              SIZE,   FNAME,  ISNULL               )
C
C
C                  CALL UTC2ET ( ODATE,  ET )
C                  CALL EKACED ( HANDLE, SEGNO,  RECNO, 'ORDER_DATE',
C                 .              SIZE,   ET,     ISNULL               )
C
C                  CALL EKACED ( HANDLE, SEGNO,  RECNO, 'COST',
C                 .              SIZE,   COST,   ISNULL               )
C
C            C
C            C     Close the file to make the update permanent.
C            C
C                  CALL EKCLS ( HANDLE )
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
C-    SPICELIB Version 1.1.0, 18-JUN-1999 (WLT)
C
C        Removed an unbalanced call to CHKOUT.
C
C-    Beta Version 1.0.0, 26-SEP-1995 (NJB)
C
C-&
 
C$ Index_Entries
C
C     add integer data to EK column
C     add data to EK
C     write integer data to EK column
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
 
C
C     Local variables
C
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               CLASS
      INTEGER               DTYPE
      INTEGER               RECPTR
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               UNIT
 
C
C     Use discovery check-in.
C
C     First step:  find the descriptor for the named segment.  Using
C     this descriptor, get the column descriptor.
C
      CALL ZZEKSDSC (  HANDLE,  SEGNO,  SEGDSC          )
      CALL ZZEKCDSC (  HANDLE,  SEGDSC, COLUMN, COLDSC  )
 
      IF ( FAILED() ) THEN
         RETURN
      END IF
 
C
C     This column had better be of integer type.
C
      DTYPE  =  COLDSC(TYPIDX)
 
      IF ( DTYPE .NE. INT ) THEN
 
         CALL CHKIN  ( 'EKACEI'                                        )
         CALL DASHLU ( HANDLE,  UNIT                                   )
         CALL SETMSG ( 'Column # is of type #; EKACEI only works '    //
     .                 'with integer columns.  RECNO = #; SEGNO = '   //
     .                 '#; EK = #.'                                    )
         CALL ERRCH  ( '#',  COLUMN                                    )
         CALL ERRINT ( '#',  DTYPE                                     )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL ERRINT ( '#',  SEGNO                                     )
         CALL ERRFNM ( '#',  UNIT                                      )
         CALL SIGERR ( 'SPICE(WRONGDATATYPE)'                          )
         CALL CHKOUT ( 'EKACEI'                                        )
         RETURN
 
      END IF
 
C
C     Look up the record pointer for the target record.
C
      CALL ZZEKTRDP ( HANDLE, SEGDSC(RTIDX), RECNO, RECPTR )
C
C
C     Now it's time to add data to the file.
C
      CLASS  =  COLDSC ( CLSIDX )
 
 
      IF ( CLASS .EQ. 1 ) THEN
C
C        Class 1 columns contain scalar integer data.
C
         CALL ZZEKAD01 ( HANDLE, SEGDSC, COLDSC, RECPTR, IVALS, ISNULL )
 
 
      ELSE IF ( CLASS .EQ. 4 ) THEN
C
C        Class 4 columns contain array-valued integer data.
C
         CALL ZZEKAD04 ( HANDLE,  SEGDSC, COLDSC,
     .                   RECPTR,  NVALS,  IVALS,  ISNULL )
 
      ELSE
C
C        This is an unsupported integer column class.
C
         SEGNO  =  SEGDSC ( SNOIDX )
 
         CALL CHKIN  ( 'EKACEI'                                        )
         CALL DASHLU ( HANDLE,  UNIT                                   )
         CALL SETMSG ( 'Class # from input column descriptor is not ' //
     .                 'a supported integer class.  COLUMN = #; '     //
     .                 'RECNO = #; SEGNO = #; EK = #.'                 )
         CALL ERRINT ( '#',  CLASS                                     )
         CALL ERRCH  ( '#',  COLUMN                                    )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL ERRINT ( '#',  SEGNO                                     )
         CALL ERRFNM ( '#',  UNIT                                      )
         CALL SIGERR ( 'SPICE(NOCLASS)'                                )
         CALL CHKOUT ( 'EKACEI'                                        )
         RETURN
 
      END IF
 
 
      RETURN
      END
