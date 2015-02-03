C$Procedure     ZZEKAD02 ( EK, add data to class 2 column )
 
      SUBROUTINE ZZEKAD02 (  HANDLE,  SEGDSC,  COLDSC,
     .                       RECPTR,  DVAL,    ISNULL  )
 
C$ Abstract
C
C     Add a column entry to a specified record in a class 2 column.
C     Class 2 columns contain scalar double precision values.
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
 
      INCLUDE 'ekbool.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekdatpag.inc'
      INCLUDE 'ekrecptr.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               SEGDSC ( * )
      INTEGER               COLDSC ( * )
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
C     SEGDSC         is the descriptor of the segment in which
C                    the specified column entry is to be written.
C
C     COLDSC         is the descriptor of the column in which
C                    the specified column entry is to be written.
C
C     RECPTR         is a pointer to the record containing the column
C                    entry to be written.
C
C     DVAL           is the double precision value that will be written
C                    to the specified column entry.
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
C         called by this routine.  The file is not modified.
C
C     2)  If the ordinal position of the column specified by COLDSC
C         is out of range, the error SPICE(INVALIDINDEX) is signalled.
C         The file is not modified.
C
C     3)  If the input flag ISNULL is .TRUE. but the target column
C         does not allow nulls, the error SPICE(BADATTRIBUTE) is
C         signalled.  The file is not modified.
C
C     4)  If RECPTR is invalid, a DAS addressing error may occur.  The
C         error in *not* trapped in advance.  This routine assumes that
C         a valid value of RECPTR has been supplied by the caller.
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
C     This routine operates by side effects:  it sets the value of a
C     column entry in an EK segment.  If the column is indexed, the
C     index is updated to reflect the presence of the new entry.  This
C     routine is intended to set values of uninitialized column entries
C     only.  To update existing entries, use the ZZEKUExx routines, or
C     at the user level, the EKUCEx routines.
C
C     This routine does not participate in shadowing functions.  If the
C     target EK is shadowed, the caller is responsible for performing
C     necessary backup operations.  If the target EK is not shadowed,
C     the target record's status is not modified.
C
C     The changes made by this routine to the target EK file become
C     permanent when the file is closed.  Failure to close the file
C     properly will leave it in an indeterminate state.
C
C$ Examples
C
C     See EKACED.
C
C$ Restrictions
C
C     1) This routine cannot be used to update existing column entries.
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
C-    Beta Version 1.0.0, 27-SEP-1995 (NJB)
C
C-&
 
 
C
C     Non-SPICELIB functions
C
      INTEGER               ZZEKRP2N
 
C
C     Local variables
C
      INTEGER               COLIDX
      INTEGER               DATPTR
      INTEGER               ITYPE
      INTEGER               LASTW
      INTEGER               MBASE
      INTEGER               NCOLS
      INTEGER               NLINKS
      INTEGER               P
      INTEGER               PBASE
      INTEGER               PTRLOC
      INTEGER               RECNO
 
C
C     Use discovery check-in.
C
C
C     Make sure the column exists.
C
      NCOLS   =  SEGDSC ( NCIDX  )
      COLIDX  =  COLDSC ( ORDIDX )
 
      IF (  ( COLIDX .LT. 1 ) .OR. ( COLIDX .GT. NCOLS )  ) THEN
 
         CALL CHKIN  ( 'ZZEKAD02'                              )
         CALL SETMSG ( 'Column index = #; valid range is 1:#.' )
         CALL ERRINT ( '#',  COLIDX                            )
         CALL ERRINT ( '#',  NCOLS                             )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                   )
         CALL CHKOUT ( 'ZZEKAD02'                              )
         RETURN
 
      END IF
 
C
C     If the value is null, make sure that nulls are permitted
C     in this column.
C
      IF (  ISNULL  .AND.  ( COLDSC(NFLIDX) .NE. ITRUE )  ) THEN
 
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )
 
         CALL CHKIN  ( 'ZZEKAD02'                                  )
         CALL SETMSG ( 'Column having index # in segment # does '  //
     .                 'not allow nulls, but a null value was '    //
     .                 'supplied for the element in record #.'     )
         CALL ERRINT ( '#',  COLIDX                                )
         CALL ERRINT ( '#',  RECNO                                 )
         CALL SIGERR ( 'SPICE(BADATTRIBUTE)'                       )
         CALL CHKOUT ( 'ZZEKAD02'                                  )
         RETURN
 
      END IF
 
C
C     Compute the data pointer location.  Check the data pointer to
C     make sure the column entry we're writing to is uninitialized.
C
      PTRLOC  =  RECPTR + DPTBAS + COLIDX
 
      CALL DASRDI ( HANDLE, PTRLOC, PTRLOC, DATPTR )
 
 
      IF (  ( DATPTR .NE. UNINIT ) .AND. ( DATPTR .NE. NOBACK )  ) THEN
 
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )
 
         CALL CHKIN  ( 'ZZEKAD02'                                  )
         CALL SETMSG ( 'Column having index # in segment # has '   //
     .                 'non-empty element in record #.'            )
         CALL ERRINT ( '#',  COLIDX                                )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                        )
         CALL ERRINT ( '#',  RECNO                                 )
         CALL SIGERR ( 'SPICE(NONEMPTYENTRY)'                      )
         CALL CHKOUT ( 'ZZEKAD02'                                  )
         RETURN
 
      END IF
 
 
      IF ( ISNULL ) THEN
C
C        All we need do is set the data pointer.  The segment's
C        metadata are not affected.
C
         CALL DASUDI ( HANDLE, PTRLOC, PTRLOC, NULL )
 
 
      ELSE
C
C        Decide where to write the data value.  If there's room left
C        in the last double precision data page, append the value there.
C
         LASTW  =  SEGDSC ( LDWIDX )
 
 
         IF ( LASTW .LT. DPSIZE ) THEN
C
C           There's room in the current page.  Set the data pointer
C           and count, and write the value out to the first free
C           location.
C
            P       =   SEGDSC(LDPIDX)
 
            CALL ZZEKPGBS ( DP,  P,  PBASE )
 
            DATPTR  =   PBASE + LASTW + 1
 
            CALL DASUDI ( HANDLE, PTRLOC, PTRLOC, DATPTR )
            CALL DASUDD ( HANDLE, DATPTR, DATPTR, DVAL   )
 
            CALL ZZEKGLNK ( HANDLE, DP, P, NLINKS   )
            CALL ZZEKSLNK ( HANDLE, DP, P, NLINKS+1 )
 
C
C           The last double precision word in use must be updated.
C
            SEGDSC ( LDWIDX )  =  LASTW + 1
 
 
         ELSE
C
C           Allocate a data page.  Write the data value into the
C           first word of the new page.
C
            CALL ZZEKAPS  ( HANDLE, SEGDSC,  DP,     .FALSE., P, PBASE )
            CALL DASUDD   ( HANDLE, PBASE+1, PBASE+1, DVAL             )
 
C
C           The page containing the data item now has one link.
C
            CALL ZZEKSLNK ( HANDLE, DP, P, 1 )
 
C
C           The last d.p. page and word in use must be updated.
C
            SEGDSC ( LDPIDX )  =  P
            SEGDSC ( LDWIDX )  =  1
 
C
C           The record pointer must point to this data item.
C
            CALL DASUDI ( HANDLE, PTRLOC, PTRLOC, PBASE+1 )
 
         END IF
 
      END IF
 
C
C     Write out the updated segment descriptor.
C
      MBASE  =  SEGDSC(IMDIDX)
 
      CALL DASUDI ( HANDLE, MBASE+1, MBASE+SDSCSZ, SEGDSC )
 
C
C     If the column is indexed, we must update the index to account
C     for the new element.
C
      ITYPE  =  COLDSC(IXTIDX)
 
      IF ( ITYPE .NE. IFALSE ) THEN
C
C        The column is indexed.
C
         IF ( ITYPE .EQ. 1 ) THEN
C
C           The column has a type 1 index.  Insert the record number
C           of the current element at the appropriate location.
C
            CALL ZZEKIID1 (  HANDLE,  SEGDSC,  COLDSC,
     .                       DVAL,    RECPTR,  ISNULL  )
 
         ELSE
 
            CALL CHKIN  ( 'ZZEKAD02'                                  )
            CALL SETMSG ( 'Column having index # in segment # has '   //
     .                    'index type #.'                             )
            CALL ERRINT ( '#',  COLIDX                                )
            CALL ERRINT ( '#',  SEGDSC(SNOIDX)                        )
            CALL ERRINT ( '#',  ITYPE                                 )
            CALL SIGERR ( 'SPICE(INVALIDTYPE)'                        )
            CALL CHKOUT ( 'ZZEKAD02'                                  )
            RETURN
 
         END IF
 
      END IF
 
      RETURN
      END
