C$Procedure     ZZEKAD04 ( EK, add data to class 4 column )
 
      SUBROUTINE ZZEKAD04 (  HANDLE,  SEGDSC,  COLDSC,
     .                       RECPTR,  NVALS,   IVALS,  ISNULL  )
 
C$ Abstract
C
C     Add a column entry to a specified record in a class 4 column.
C     The entries of class 4 columns are arrays of integer values.
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
C     PRIVATE
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
      INTEGER               NVALS
      INTEGER               IVALS  ( * )
      LOGICAL               ISNULL
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     SEGDSC     I   Segment descriptor.
C     COLDSC     I   Column descriptor.
C     RECPTR     I   Record pointer.
C     NVALS      I   Number of values to add to column.
C     IVALS      I   Integer values to add to column.
C     ISNULL     I   Flag indicating whether column entry is null.
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
C     See EKACEI.
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
C-    Beta Version 1.0.0, 26-SEP-1995 (NJB)
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
      INTEGER               LASTW
      INTEGER               MBASE
      INTEGER               NCOLS
      INTEGER               NLINKS
      INTEGER               NREC
      INTEGER               NWRITE
      INTEGER               P
      INTEGER               P2
      INTEGER               PBASE
      INTEGER               PTRLOC
      INTEGER               RECNO
      INTEGER               REMAIN
      INTEGER               ROOM
      INTEGER               START
 
      LOGICAL               FSTPAG
 
C
C     Use discovery check-in.
C
      NREC    =  SEGDSC ( NRIDX  )
      COLIDX  =  COLDSC ( ORDIDX )
 
C
C     Make sure the column exists.
C
      NCOLS  =  SEGDSC ( NCIDX )
 
      IF (  ( COLIDX .LT. 1 ) .OR. ( COLIDX .GT. NCOLS )  ) THEN
 
         CALL CHKIN  ( 'ZZEKAD04'                              )
         CALL SETMSG ( 'Column index = #; valid range is 1:#.' )
         CALL ERRINT ( '#',  COLIDX                            )
         CALL ERRINT ( '#',  NREC                              )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                   )
         CALL CHKOUT ( 'ZZEKAD04'                              )
         RETURN
 
      END IF
 
C
C     If the value is null, make sure that nulls are permitted
C     in this column.
C
      IF (  ISNULL  .AND.  ( COLDSC(NFLIDX) .NE. ITRUE )  ) THEN
 
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )
 
         CALL CHKIN  ( 'ZZEKAD04'                                  )
         CALL SETMSG ( 'Column having index # in segment # does '  //
     .                 'not allow nulls, but a null value was '    //
     .                 'supplied for the element in record #.'     )
         CALL ERRINT ( '#',  COLIDX                                )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                        )
         CALL ERRINT ( '#',  RECNO                                 )
         CALL SIGERR ( 'SPICE(BADATTRIBUTE)'                       )
         CALL CHKOUT ( 'ZZEKAD04'                                  )
         RETURN
 
      END IF
 
C
C     Check NVALS.  If the column has fixed-size entries, NVALS must
C     match the declared entry size.  In all cases, NVALS must be
C     positive.
C
      IF ( NVALS .LT. 1 ) THEN
 
         CALL CHKIN  ( 'ZZEKAD04'                                  )
         CALL SETMSG ( 'COLIDX = #;  segment = #; NVALS = #;  '  //
     .                 'NVALS must be positive '                   )
         CALL ERRINT ( '#',  COLIDX                                )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                        )
         CALL ERRINT ( '#',  NVALS                                 )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                       )
         CALL CHKOUT ( 'ZZEKAD04'                                  )
         RETURN
 
      END IF
 
 
      IF ( COLDSC(SIZIDX) .NE. IFALSE ) THEN
 
         IF ( NVALS .NE. COLDSC(SIZIDX) ) THEN
 
            CALL CHKIN  ( 'ZZEKAD04'                                   )
            CALL SETMSG ( 'COLIDX = #;  segment = #; NVALS = #; '     //
     .                    'declared entry size = #.  Sizes must match.')
            CALL ERRINT ( '#',  COLIDX                                 )
            CALL ERRINT ( '#',  SEGDSC(SNOIDX)                         )
            CALL ERRINT ( '#',  NVALS                                  )
            CALL ERRINT ( '#',  COLDSC(SIZIDX)                         )
            CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                        )
            CALL CHKOUT ( 'ZZEKAD04'                                   )
            RETURN
 
         END IF
 
      END IF
 
C
C     Compute the data pointer location.
C
      PTRLOC  =  RECPTR + DPTBAS + COLIDX
 
 
      IF ( ISNULL ) THEN
C
C        All we need do is set the data pointer.  The segment's
C        metadata are not affected.
C
         CALL DASUDI ( HANDLE, PTRLOC, PTRLOC, NULL )
 
 
      ELSE
 
         LASTW   =  SEGDSC ( LIWIDX )
         ROOM    =  IPSIZE - LASTW
         REMAIN  =  NVALS
         START   =  1
         FSTPAG  =  .TRUE.
 
 
         DO WHILE ( REMAIN .GT. 0 )
C
C           Decide where to write the data values.  In order to write
C           to the current page, we require enough room for the count
C           and at least one column entry element.
C
            IF ( ROOM .GE. 2 ) THEN
C
C              There's room in the current page.  If this is the first
C              page this entry is written on, set the data pointer
C              and count.  Write as much of the value as possible to
C              the current page.
C
               P       =   SEGDSC(LIPIDX)
 
               CALL ZZEKPGBS ( INT,  P,  PBASE )
 
               DATPTR  =  PBASE + LASTW + 1
 
 
               IF ( FSTPAG ) THEN
 
                  CALL DASUDI ( HANDLE, PTRLOC, PTRLOC, DATPTR )
                  CALL DASUDI ( HANDLE, DATPTR, DATPTR, NVALS  )
 
                  ROOM    =  ROOM   - 1
                  DATPTR  =  DATPTR + 1
 
               END IF
 
 
               NWRITE  =  MIN ( REMAIN, ROOM )
 
               CALL DASUDI ( HANDLE,
     .                       DATPTR,
     .                       DATPTR+NWRITE-1,
     .                       IVALS ( START )  )
 
               REMAIN  =  REMAIN - NWRITE
               ROOM    =  ROOM   - NWRITE
               START   =  START  + NWRITE
 
C
C              The page containing the data item gains a link.
C
               CALL ZZEKGLNK ( HANDLE, INT, P, NLINKS   )
               CALL ZZEKSLNK ( HANDLE, INT, P, NLINKS+1 )
 
C
C              The last integer word in use must be updated.  Account
C              for the count, if this is the first page on which the
C              current entry is written.
C
               IF ( FSTPAG ) THEN
                  SEGDSC ( LIWIDX )  =  LASTW + 1 + NWRITE
                  FSTPAG             =  .FALSE.
               ELSE
                  SEGDSC ( LIWIDX )  =  LASTW + NWRITE
               END IF
 
 
            ELSE
C
C              Allocate a data page.  If this is not the first data
C              page written to, link the previous page to the current
C              one.
C
               CALL ZZEKAPS ( HANDLE, SEGDSC, INT, .FALSE., P2, PBASE )
 
               IF ( .NOT. FSTPAG ) THEN
                  CALL ZZEKSFWD ( HANDLE, INT, P, P2 )
               END IF
 
C
C              The last integer page and word in use must be updated.
C
               P                  =  P2
               LASTW              =  0
               SEGDSC ( LIPIDX )  =  P
               SEGDSC ( LIWIDX )  =  LASTW
               ROOM               =  IPSIZE
 
C
C              Make sure the link count is zeroed out.
C
               CALL ZZEKSLNK ( HANDLE, INT, P, 0 )
 
            END IF
 
         END DO
 
 
      END IF
 
C
C     Write out the updated segment descriptor.
C
      MBASE  =  SEGDSC(IMDIDX)
 
      CALL DASUDI ( HANDLE, MBASE+1, MBASE+SDSCSZ, SEGDSC )
 
C
C     Class 4 columns are not indexed, so we need not update any
C     index to account for the new element.
C
 
      RETURN
      END
