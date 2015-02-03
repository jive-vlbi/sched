C$Procedure      EKAPPR ( EK, append record onto segment )
 
      SUBROUTINE EKAPPR ( HANDLE, SEGNO, RECNO )
 
C$ Abstract
C
C     Append a new, empty record at the end of a specified E-kernel
C     segment.
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
C     PRIVATE
C     UTILITY
C
C$ Declarations
 
 
      INCLUDE 'ekdatpag.inc'
      INCLUDE 'ekpage.inc'
      INCLUDE 'ekrecptr.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               SEGNO
      INTEGER               RECNO
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     SEGNO      I   Segment number.
C     RECNO      O   Number of appended record.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle of an EK open for write access.
C
C     SEGNO          is the number of the segment to which the record
C                    is to be added.
C
C$ Detailed_Output
C
C     RECNO          is the number of the record appended by this
C                    routine.  RECNO is used to identify the record
C                    when writing column entries to it.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If HANDLE is invalid, the error will be diagnosed by routines
C         called by this routine.  The file will not be modified.
C
C     2)  If SEGNO is out of range, the error SPICE(INVALIDINDEX)
C         will be signalled.  The file will not be modified.
C
C     3)  If an I/O error occurs while reading or writing the indicated
C         file, the error will be diagnosed by routines called by this
C         routine.  The file may be corrupted.
C
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This routine operates by side effects:  It appends a new, empty
C     record structure to an EK segment.  The ordinal position of the
C     new record is one greater than the previous number of records in
C     in the segment.
C
C     After a new record has been appended to a segment by this routine,
C     the record must be populated with data using the EKACEx
C     routines.  EKs are valid only when all of their column entries
C     are initialized.
C
C     To insert a record into a segment at a specified ordinal position,
C     use the routine EKAPPR.
C
C     This routine cannot be used with the "fast write" suite of
C     routines.  See the EK Required Reading for a discussion of the
C     fast writers.
C
C     When a record is inserted into an EK file that is not shadowed,
C     the status of the record starts out set to OLD.  The status
C     does not change when data is added to the record.
C
C     If the target EK is shadowed, the new record will be given the
C     status NEW.  Updating column values in the record does not change
C     its status.  When changes are committed, the status is set to OLD.
C     If a rollback is performed before changes are committed, the
C     record is deleted.  Closing the target file without committing
C     changes implies a rollback.
C
C$ Examples
C
C     1)  Append a record to a specified segment.
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
C-    SPICELIB Version 1.0.1, 09-JAN-2002 (NJB)
C
C        Documentation change:  instances of the phrase "fast load"
C        were replaced with "fast write."
C
C-    Beta Version 1.0.0, 19-DEC-1995 (NJB)
C
C-&
 
C$ Index_Entries
C
C     append record to EK segment
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
      INTEGER               MP
      INTEGER               NREC
      INTEGER               SEGDSC ( SDSCSZ )
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKAPPR' )
      END IF
 
C
C     Before trying to actually write anything, do every error
C     check we can.
C
C     Is this file handle valid--is the file open for paged write
C     access?  Signal an error if not.
C
      CALL ZZEKPGCH ( HANDLE, 'WRITE' )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'EKAPPR' )
         RETURN
      END IF
 
C
C     Look up the integer metadata page and page base for the segment.
C     Given the base address, we can read the pertinent metadata in
C     one shot.
C
      CALL ZZEKMLOC ( HANDLE,  SEGNO,  MP,  MBASE  )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'EKAPPR' )
         RETURN
      END IF
 
      CALL DASRDI ( HANDLE,  MBASE+1,  MBASE+SDSCSZ,  SEGDSC )
 
C
C     Obtain the number of records already present.
C
      NREC   =  SEGDSC ( NRIDX )
 
C
C     Insert the new record at the end of the segment.
C
      RECNO  =  NREC + 1
 
      CALL EKINSR ( HANDLE, SEGNO, RECNO )
 
 
      CALL CHKOUT ( 'EKAPPR' )
      RETURN
      END
