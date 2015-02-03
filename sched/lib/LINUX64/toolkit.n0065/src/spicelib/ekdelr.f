C$Procedure      EKDELR ( EK, delete record from segment )
 
      SUBROUTINE EKDELR ( HANDLE, SEGNO, RECNO )
 
C$ Abstract
C
C     Delete a specified record from a specified E-kernel segment.
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
 
 
      INCLUDE 'ekbool.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekcnamsz.inc'
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
C     SEGNO          is the number of the segment from which to
C                    delete the specified record.
C
C     RECNO          is the index of the record to delete.  RECNO must
C                    be in the range 1 : NREC, where NREC is the
C                    number of records in the segment prior to the
C                    insertion.
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
C     This routine operates by side effects:  it deletes a record
C     from an EK segment.  Deleting a record implies:
C
C        1) All column entries in the record are deleted.
C
C        2) Link counts are decremented for data pages containing
C           column entries in the record to be deleted.  Pages whose
C           link counts drop to zero are freed.
C
C        3) All column indexes are updated for the parent segment.
C
C        4) The link count is decremented for the page containing the
C           record pointer structure of the record to be deleted.  If
C           the link count drops to zero, the page is freed.
C
C        5) The pointer to the deleted record is deleted from the
C           record tree for the parent segment.
C
C        6) The segment's metadata is updated to reflect the new
C           record count.
C
C$ Examples
C
C     1)  Suppose the second segment of an EK file designated by
C         HANDLE contains 5 records:
C
C            +-----------------+
C            |     Record 1    |
C            +-----------------+
C            |     Record 2    |
C            +-----------------+
C            |     Record 3    |
C            +-----------------+
C            |     Record 4    |
C            +-----------------+
C            |     Record 5    |
C            +-----------------+
C
C         Then the call
C
C            CALL EKDELR ( HANDLE, 2, 3 )
C
C         deletes the third record from the segment, leaving the
C         segment's contents as follows:
C
C            +-----------------+
C            |     Record 1    |
C            +-----------------+
C            |     Record 2    |
C            +-----------------+
C            |     Record 4    |
C            +-----------------+
C            |     Record 5    |
C            +-----------------+
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
C-    Beta Version 1.0.0, 19-DEC-1995 (NJB)
C
C-&
 
C$ Index_Entries
C
C     delete record from an EK segment
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Non-SPICELIB functions
C
      INTEGER               ZZEKRP2N
 
C
C     Local variables
C
      CHARACTER*(CNAMSZ)    COLUMN
 
      INTEGER               BASE
      INTEGER               CLASS
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               DSCBAS
      INTEGER               I
      INTEGER               MBASE
      INTEGER               MP
      INTEGER               NCOLS
      INTEGER               NLINKS
      INTEGER               NREC
      INTEGER               P
      INTEGER               RECPTR
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               UNIT
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKDELR' )
      END IF
 
C
C     Before trying to actually modify the file, do every error
C     check we can.
C
C     Is this file handle valid--is the file open for paged write
C     access?  Signal an error if not.
C
      CALL ZZEKPGCH ( HANDLE, 'WRITE' )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'EKDELR' )
         RETURN
      END IF
 
C
C     Look up the integer metadata page and page base for the segment.
C     Given the base address, we can read the pertinent metadata in
C     one shot.
C
      CALL ZZEKMLOC ( HANDLE,  SEGNO,  MP,  MBASE  )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'EKDELR' )
         RETURN
      END IF
 
      CALL DASRDI ( HANDLE,  MBASE+1,  MBASE+SDSCSZ,  SEGDSC )
 
C
C     In case the target EK is shadowed, let the shadow system know
C     about the deletion.  This must be done before the data is
C     deleted.  The argument COLDSC is unused on this call.
C
      CALL ZZEKRBCK ( 'DELETE', HANDLE, SEGDSC, COLDSC, RECNO )
 
C
C     We'll need to know how many columns the segment has in order to
C     compute the size of the record pointer.  The record pointer
C     contains DPTBAS items plus two elements for each column.
C
      NCOLS  =  SEGDSC ( NCIDX )
 
C
C     Check the number of records already present.  RECNO must not
C     exceed this count.
C
      NREC  =  SEGDSC ( NRIDX )
 
      IF (  ( RECNO .LT. 1 ) .OR. ( RECNO .GT. NREC )  ) THEN
 
         CALL SETMSG ( 'Record number = #; valid range is 1:#.' )
         CALL ERRINT ( '#',  RECNO                              )
         CALL ERRINT ( '#',  NREC                               )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                    )
         CALL CHKOUT ( 'EKDELR'                                 )
         RETURN
 
      END IF
 
C
C     Delete all of the column entries in the record.  The deletion
C     routines handle updating column indexes and freeing unlinked
C     pages.
C
      CALL ZZEKTRDP ( HANDLE, SEGDSC(RTIDX), RECNO, RECPTR )
 
      DO I =  1, NCOLS
C
C        Get the descriptor of the Ith column.
C
         DSCBAS  =  MBASE  +  CDOFF  +  (I-1)*CDSCSZ
 
         CALL DASRDI ( HANDLE, DSCBAS+1, DSCBAS+CDSCSZ, COLDSC )
 
         CLASS   =  COLDSC(CLSIDX)
 
C
C        Delete the entry in the current column.
C
         IF ( CLASS .EQ. 1 ) THEN
 
            CALL ZZEKDE01 ( HANDLE, SEGDSC, COLDSC, RECPTR )
 
 
         ELSE IF ( CLASS .EQ. 2 ) THEN
 
            CALL ZZEKDE02 ( HANDLE, SEGDSC, COLDSC, RECPTR )
 
 
         ELSE IF ( CLASS .EQ. 3 ) THEN
 
            CALL ZZEKDE03 ( HANDLE, SEGDSC, COLDSC, RECPTR )
 
 
         ELSE IF ( CLASS .EQ. 4 ) THEN
 
            CALL ZZEKDE04 ( HANDLE, SEGDSC, COLDSC, RECPTR )
 
 
         ELSE IF ( CLASS .EQ. 5 ) THEN
 
            CALL ZZEKDE05 ( HANDLE, SEGDSC, COLDSC, RECPTR )
 
 
         ELSE IF ( CLASS .EQ. 6 ) THEN
 
            CALL ZZEKDE06 ( HANDLE, SEGDSC, COLDSC, RECPTR )
 
 
         ELSE
C
C           This is an unsupported class.
C
            RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )
 
            CALL DASHLU    ( HANDLE, UNIT )
            CALL ZZEKCNAM  ( HANDLE, COLDSC, COLUMN )
 
 
            CALL SETMSG ( 'Class # from input column descriptor '  //
     .                    'is not supported.  COLUMN = #; '        //
     .                    'RECNO = #; SEGNO = #; EK = #.'           )
            CALL ERRINT ( '#',  CLASS                               )
            CALL ERRCH  ( '#',  COLUMN                              )
            CALL ERRINT ( '#',  RECNO                               )
            CALL ERRINT ( '#',  SEGDSC(SNOIDX)                      )
            CALL ERRFNM ( '#',  UNIT                                )
            CALL SIGERR ( 'SPICE(NOCLASS)'                          )
            CALL CHKOUT ( 'EKDELR'                                  )
            RETURN
 
         END IF
 
      END DO
 
C
C     Find the page containing the record pointer.
C
      CALL ZZEKPGPG ( INT, RECPTR+1, P, BASE )
 
C
C     Get the link count for the page.  If we have more
C     than one link to the page, decrement the link count.  If
C     we're down to one link, this deletion will finish off the
C     page:  we'll deallocate it.
C
      CALL ZZEKGLNK ( HANDLE, INT, P, NLINKS )
 
      IF ( NLINKS .GT. 1 ) THEN
 
         CALL ZZEKSLNK ( HANDLE, INT, P, NLINKS-1 )
 
      ELSE
C
C        If we removed the last item from the page, we can delete
C        the page.  ZZEKDPS adjusts the segment's metadata
C        to reflect the deallocation.
C
         CALL ZZEKDPS ( HANDLE, SEGDSC, INT, P )
 
      END IF
 
C
C     The entry corresponding to the record is deleted from
C     the data record tree at index RECNO.  The record count gets
C     decremented.
C
      CALL ZZEKTRDL ( HANDLE,  SEGDSC(RTIDX),  RECNO )
 
      SEGDSC ( NRIDX )  =  SEGDSC ( NRIDX ) - 1
 
C
C     Write out the updated segment descriptor.
C
      CALL DASUDI ( HANDLE,  MBASE+1,  MBASE+SDSCSZ,  SEGDSC )
 
 
      CALL CHKOUT ( 'EKDELR' )
      RETURN
      END
