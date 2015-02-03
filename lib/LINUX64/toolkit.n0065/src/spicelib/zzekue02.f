C$Procedure      ZZEKUE02 ( EK, update column entry, class 2 )
 
      SUBROUTINE ZZEKUE02 (  HANDLE,  SEGDSC,  COLDSC,
     .                       RECPTR,  DVAL,    ISNULL  )
 
C$ Abstract
C
C     Update a specified class 2 column entry in an EK record.
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
      INCLUDE 'ekdatpag.inc'
      INCLUDE 'ekrecptr.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               RECPTR
      DOUBLE PRECISION      DVAL
      LOGICAL               ISNULL
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     SEGDSC     I   Segment descriptor.
C     COLDSC     I   Column descriptor.
C     RECPTR     I   Record pointer.
C     DVAL       I   Double precision value.
C     ISNULL     I   Null flag.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle of an EK open for write access.
C
C     SEGDSC         is the descriptor of the segment containing
C                    the specified column entry.
C
C     COLDSC         is the descriptor of the column containing
C                    the specified column entry.
C
C     RECPTR         is a pointer to the record containing the column
C                    entry to update.
C
C     DVAL           is the double precision value with which to update
C                    the specified column entry.
C
C     ISNULL         is a logical flag indicating whether the value
C                    of the specified column entry is to be set to NULL.
C                    If so, the input DVAL is ignored.
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
C     2)  If an I/O error occurs while reading or writing the indicated
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
C     This routine operates by side effects:  it updates a column entry
C     in an EK segment.  This routine does not participate in shadowing
C     functions.  If the target EK is shadowed, the caller is
C     responsible for performing necessary backup operations.  If the
C     target EK is not shadowed, the target record's status is not
C     modified.
C
C     If the column containing the entry is indexed, the corresponding
C     index is updated.
C
C     The changes made by this routine to the target EK file become
C     permanent when the file is closed.  Failure to close the file
C     properly will leave it in an indeterminate state.
C
C$ Examples
C
C     See EKUCED.
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
C        Removed redundant calls to CHKIN.
C
C-    Beta Version 1.0.0, 27-SEP-1995 (NJB)
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
      INTEGER               DATPTR
      INTEGER               IDXTYP
      INTEGER               NCOLS
      INTEGER               NLINKS
      INTEGER               P
      INTEGER               PBASE
      INTEGER               PTRLOC
      INTEGER               RECNO
      INTEGER               UNIT
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKUE02' )
      END IF
 
C
C     Is this file handle valid--is the file open for paged write
C     access?  Signal an error if not.
C
      CALL ZZEKPGCH ( HANDLE, 'WRITE' )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZEKUE02' )
         RETURN
      END IF
 
C
C     We'll need to know how many columns the segment has in order to
C     compute the size of the record pointer.  The record pointer
C     contains DPTBAS items plus two elements for each column.
C
      NCOLS  =  SEGDSC ( NCIDX )
 
C
C     Compute the data pointer location.
C
      PTRLOC  =  RECPTR + DPTBAS + COLDSC(ORDIDX)
 
      CALL DASRDI ( HANDLE, PTRLOC, PTRLOC, DATPTR )
 
 
      IF ( DATPTR .GT. 0 ) THEN
C
C        The column entry is non-null.  Determine whether the column is
C        indexed.
C
         IDXTYP  =  COLDSC(IXTIDX)
 
         IF ( IDXTYP .EQ. 1 ) THEN
C
C           The column has a type 1 index.  Delete the index entry
C           for this column.  Create an index entry for the new value.
C
            CALL ZZEKIXDL ( HANDLE,  SEGDSC,  COLDSC,  RECPTR )
 
            CALL ZZEKIID1 ( HANDLE,  SEGDSC,  COLDSC,  DVAL,
     .                      RECPTR,  ISNULL                  )
 
         ELSE IF ( IDXTYP .NE. IFALSE ) THEN
 
            CALL SETMSG ( 'Column having index # in segment # has '   //
     .                    'index type #.'                             )
            CALL ERRINT ( '#',  COLDSC(ORDIDX)                        )
            CALL ERRINT ( '#',  SEGDSC(SNOIDX)                        )
            CALL ERRINT ( '#',  IDXTYP                                )
            CALL SIGERR ( 'SPICE(INVALIDTYPE)'                        )
            CALL CHKOUT ( 'ZZEKUE02'                                  )
            RETURN
 
         END IF
 
C
C        If the new value is null, set the data pointer to indicate a
C        null value.  Otherwise, overwrite the old value with the new
C        one.
C
         IF ( ISNULL ) THEN
C
C           The data location used by the previous value is no longer
C           needed, so we have one less link to this page.
C
            CALL ZZEKPGPG ( DP,     DATPTR, P,      PBASE    )
            CALL ZZEKGLNK ( HANDLE, DP,     P,      NLINKS   )
            CALL ZZEKSLNK ( HANDLE, DP,     P,      NLINKS-1 )
 
            CALL DASUDI   ( HANDLE, PTRLOC, PTRLOC, NULL     )
 
         ELSE
C
C           No link counts change; we just have a new value.
C
            CALL DASUDD ( HANDLE, DATPTR, DATPTR, DVAL )
 
         END IF
 
 
 
      ELSE IF ( DATPTR .EQ. NULL ) THEN
C
C        If the new entry is null too, there's nothing to do.
C        We don't have to adjust link counts or indexes.
C
C        If the new entry is non-null, we must add a new column entry,
C        since no space was reserved for the old one.  The column
C        index entry must be cleaned up, if the column is indexed.
C
         IF ( .NOT. ISNULL ) THEN
 
            IDXTYP  =  COLDSC(IXTIDX)
 
            IF ( IDXTYP .EQ. 1 ) THEN
C
C              The column has a type 1 index.  Delete the index entry
C              for this column.
C
               CALL ZZEKIXDL ( HANDLE,  SEGDSC,  COLDSC, RECPTR )
 
 
            ELSE IF ( IDXTYP .NE. IFALSE ) THEN
 
               CALL SETMSG ( 'Column having index # in segment # has '//
     .                       'index type #.'                          )
               CALL ERRINT ( '#',  COLDSC(ORDIDX)                     )
               CALL ERRINT ( '#',  SEGDSC(SNOIDX)                     )
               CALL ERRINT ( '#',  IDXTYP                             )
               CALL SIGERR ( 'SPICE(INVALIDTYPE)'                     )
               CALL CHKOUT ( 'ZZEKUE02'                               )
               RETURN
 
            END IF
 
C
C           We don't need to decrement the link count for this page.
C           Just add the new value to the column.  But first, set the
C           data pointer to indicate an uninitialized value, so the
C           data addition routine doesn't choke.
C
            CALL DASUDI   (  HANDLE,  PTRLOC,  PTRLOC,  UNINIT )
 
            CALL ZZEKAD02 (  HANDLE,  SEGDSC,  COLDSC,
     .                       RECPTR,  DVAL,    ISNULL  )
 
         END IF
 
 
      ELSE IF (      ( DATPTR .EQ. UNINIT )
     .          .OR. ( DATPTR .EQ. NOBACK )  ) THEN
C
C        There is no current column entry.  Just add a new entry.
C
         CALL ZZEKAD02 ( HANDLE, SEGDSC, COLDSC, RECPTR, DVAL, ISNULL )
 
 
      ELSE
C
C        The data pointer is corrupted.
C
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )
         CALL DASHLU ( HANDLE, UNIT )
 
 
         CALL SETMSG ( 'Data pointer is corrupted. SEGNO = #; '  //
     .                 'COLIDX =  #; RECNO = #; EK = #'          )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                      )
         CALL ERRINT ( '#',  COLDSC(ORDIDX)                      )
         CALL ERRINT ( '#',  RECNO                               )
         CALL ERRFNM ( '#',  UNIT                                )
         CALL SIGERR ( 'SPICE(BUG)'                              )
         CALL CHKOUT ( 'ZZEKUE02'                                )
         RETURN
 
      END IF
 
 
      CALL CHKOUT ( 'ZZEKUE02' )
      RETURN
      END
