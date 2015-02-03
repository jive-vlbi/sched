C$Procedure      EKINSR ( EK, insert record into segment )
 
      SUBROUTINE EKINSR ( HANDLE, SEGNO, RECNO )
 
C$ Abstract
C
C     Add a new, empty record to a specified E-kernel segment at
C     a specified index.
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
 
 
      INCLUDE 'ekcoldsc.inc'
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
C     RECNO      I   Record number.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle of an EK open for write access.
C
C     SEGNO          is the number of the segment to which the record
C                    is to be added.
C
C     RECNO          is the index of the new record.  RECNO must be
C                    in the range 1 : (NREC+1), where NREC is the
C                    number of records in the segment prior to the
C                    insertion.  If RECNO is equal to NREC+1, the
C                    new record is appended.  Otherwise, the new
C                    record has the ordinal position specified by
C                    RECNO, and the records previously occupying
C                    positions RECNO : NREC have their indexes
C                    incremented by 1.
C
C$ Detailed_Output
C
C     None.  See the $Particulars section for a description of the
C     effect of this routine.
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
C     3)  If RECNO is out of range, the error SPICE(INVALIDINDEX)
C         will be signalled.  The file will not be modified.
C
C     4)  If an I/O error occurs while reading or writing the indicated
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
C     This routine operates by side effects:  It adds a new, empty
C     record structure to an EK segment at a specified ordinal position.
C
C     After a record has been inserted into a segment by this routine,
C     the record must be populated with data using the EKACEx
C     routines.  EKs are valid only when all of their column entries
C     are initialized.
C
C     To append a record to a segment, use the routine EKAPPR.
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
C     1)  Insert a record into a specified E-kernel segment at a
C         specified ordinal position.
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
C            C     Open the database for write access.
C            C
C                  CALL EKOPW ( 'ORDER_DB.EK', HANDLE )
C
C            C
C            C     Insert a new, empty record into the DATAORDERS
C            C     table at record number 1.  This moves the existing
C            C     records down, so the old record 1 becomes record 2,
C            C     and so on.  Recall that the DATAORDERS table
C            C     is in segment number 1.
C            C
C                  RECNO = 1
C                  SEGNO = 1
C
C                  CALL EKINSR ( HANDLE, SEGNO, RECNO )
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
C     insert record into EK segment
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
      INTEGER               BASE
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               LASTP
      INTEGER               LASTW
      INTEGER               MBASE
      INTEGER               MP
      INTEGER               NCOLS
      INTEGER               NLINKS
      INTEGER               NREC
      INTEGER               P
      INTEGER               RECBAS
      INTEGER               RECPTR ( IPSIZE )
      INTEGER               ROOM
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               SIZE
 
      LOGICAL               ISSHAD
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKINSR' )
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
         CALL CHKOUT ( 'EKINSR' )
         RETURN
      END IF
 
C
C     Look up the integer metadata page and page base for the segment.
C     Given the base address, we can read the pertinent metadata in
C     one shot.
C
      CALL ZZEKMLOC ( HANDLE,  SEGNO,  MP,  MBASE  )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'EKINSR' )
         RETURN
      END IF
 
      CALL DASRDI ( HANDLE,  MBASE+1,  MBASE+SDSCSZ,  SEGDSC )
 
C
C     We'll need to know how many columns the segment has in order to
C     compute the size of the record pointer.  The record pointer
C     contains DPTBAS items plus two elements for each column.
C
      NCOLS  =  SEGDSC ( NCIDX )
      SIZE   =  DPTBAS + NCOLS
 
C
C     We're assuming the record pointer can fit on an integer page.
C     If this is not the case, we've got a bug.
C
      IF ( SIZE .GT. IPSIZE ) THEN
 
         CALL SETMSG ( 'Record pointer requires # integer words; '  //
     .                 'EK software assumes size is <= #.  This is '//
     .                 'an EK software bug.  Contact NAIF.'          )
         CALL ERRINT ( '#',  SIZE                                    )
         CALL ERRINT ( '#',  IPSIZE                                  )
         CALL SIGERR ( 'SPICE(BUG)'                                  )
         CALL CHKOUT ( 'EKINSR'                                      )
         RETURN
 
      END IF
 
C
C     Check the number of records already present.  RECNO must not
C     exceed this count by more than 1.
C
      NREC  =  SEGDSC ( NRIDX )
 
      IF (  ( RECNO .LT. 1 ) .OR. ( RECNO .GT. NREC+1 )  ) THEN
 
         CALL SETMSG ( 'Record number = #; valid range is 1:#.' )
         CALL ERRINT ( '#',  RECNO                              )
         CALL ERRINT ( '#',  NREC+1                             )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                    )
         CALL CHKOUT ( 'EKINSR'                                 )
         RETURN
 
      END IF
 
C
C     Find the last integer data page and the last word in use in that
C     page.  If there's enough room, we can store the record pointer
C     in the current page.
C
      LASTP  =  SEGDSC ( LIPIDX )
      LASTW  =  SEGDSC ( LIWIDX )
      ROOM   =  IPSIZE - LASTW
 
C
C     Initialize the record pointer:  set the record's status and
C     set the data pointers to indicate no data is present.  To
C     determine the status, we must know whether the parent file is
C     shadowed.
C
      CALL CLEARI ( IPSIZE,                RECPTR )
      CALL FILLI  ( UNINIT, IPSIZE-DPTBAS, RECPTR )
 
      CALL EKSHDW ( HANDLE, ISSHAD )
 
      IF ( ISSHAD ) THEN
         RECPTR ( STAIDX )  =  NEW
      ELSE
         RECPTR ( STAIDX )  =  OLD
      END IF
 
C
C     Find a place to write the record pointer.
C
      IF ( SIZE .LE. ROOM ) THEN
C
C        Just write the record pointer into the current integer page.
C
         CALL ZZEKPGBS ( INT, LASTP, BASE )
 
         RECBAS = BASE + LASTW
 
         CALL DASUDI ( HANDLE, RECBAS+1, RECBAS+SIZE, RECPTR )
 
C
C        Update the page's metadata to reflect the addition.  The
C        page gains a link.
C
         CALL DASRDI ( HANDLE, BASE+ILCIDX, BASE+ILCIDX, NLINKS   )
         CALL DASUDI ( HANDLE, BASE+ILCIDX, BASE+ILCIDX, NLINKS+1 )
 
C
C        The last integer word in use has changed too.
C
         SEGDSC ( LIWIDX )  =  SEGDSC ( LIWIDX ) + SIZE
 
 
      ELSE
C
C        Allocate an integer page.
C
         CALL ZZEKAPS  ( HANDLE, SEGDSC, INT, .FALSE., P, RECBAS )
 
C
C        Write out the record pointer.
C
         CALL DASUDI ( HANDLE, RECBAS+1, RECBAS+SIZE, RECPTR )
 
C
C        Update the page's metadata to reflect the addition.  The
C        page starts out with one link.
C
         CALL DASUDI ( HANDLE, RECBAS+ILCIDX, RECBAS+ILCIDX, 1 )
 
C
C        Update the segment's metadata to reflect the addition of a
C        data page.  The last page in use is the one we just wrote to.
C        The last word in use is the last word of the record pointer.
C
         SEGDSC ( LIPIDX )  =  P
         SEGDSC ( LIWIDX )  =  SIZE
 
      END IF
 
C
C     Update the segment's metadata to reflect the addition of the
C     new record.  The base address of the record is inserted into
C     the data record tree at index RECNO.  The record count gets
C     incremented.
C
      CALL ZZEKTRIN ( HANDLE,  SEGDSC(RTIDX),  RECNO,  RECBAS )
 
      SEGDSC ( NRIDX )  =  SEGDSC ( NRIDX ) + 1
 
C
C     If the segment is shadowed but no backup segment exists yet, we
C     need to create one.  We'll let ZZEKRBCK take care of the details.
C     Note that for data additions, the input argument COLDSC is
C     ignored.
C
      CALL ZZEKRBCK ( 'ADD', HANDLE, SEGDSC, COLDSC, RECNO )
 
C
C     Write out the updated segment descriptor.
C
      CALL DASUDI ( HANDLE,  MBASE+1,  MBASE+SDSCSZ,  SEGDSC )
 
 
      CALL CHKOUT ( 'EKINSR' )
      RETURN
      END
