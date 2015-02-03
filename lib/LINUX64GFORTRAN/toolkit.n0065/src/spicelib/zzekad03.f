C$Procedure     ZZEKAD03 ( EK, add data to class 3 column )
 
      SUBROUTINE ZZEKAD03 (  HANDLE,  SEGDSC,  COLDSC,
     .                       RECPTR,  CVAL,    ISNULL  )
 
C$ Abstract
C
C     Add a column entry to a class 3 column in a specified EK record.
C     Class 3 columns contain scalar, character values.
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
      CHARACTER*(*)         CVAL
      LOGICAL               ISNULL
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     SEGDSC     I   Segment descriptor.
C     COLDSC     I   Column descriptor.
C     RECPTR     I   Record pointer.
C     CVAL       I   Character string value.
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
C     CVAL           is the character string value that will be written
C                    to the specified column entry.
C
C     ISNULL         is a logical flag indicating whether the value
C                    of the specified column entry is to be set to NULL.
C                    If so, the input CVAL is ignored.
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
C     See EKACEC.
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
C     SPICELIB functions
C
      INTEGER               RTRIM
 
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
      INTEGER               LNB
      INTEGER               MBASE
      INTEGER               N
      INTEGER               NCOLS
      INTEGER               NLINKS
      INTEGER               NWRITE
      INTEGER               P
      INTEGER               P2
      INTEGER               PBASE
      INTEGER               PCOUNT
      INTEGER               POS
      INTEGER               PRVBAS
      INTEGER               PTRLOC
      INTEGER               RECNO
      INTEGER               STRLEN
 
      LOGICAL               FIXLEN
 
C
C     Use discovery check-in.
C
C     Make sure the column exists.
C
      NCOLS   =  SEGDSC ( NCIDX  )
      COLIDX  =  COLDSC ( ORDIDX )
 
      IF (  ( COLIDX .LT. 1 ) .OR. ( COLIDX .GT. NCOLS )  ) THEN
 
         CALL CHKIN  ( 'ZZEKAD03'                              )
         CALL SETMSG ( 'Column index = #; valid range is 1:#.' )
         CALL ERRINT ( '#',  COLIDX                            )
         CALL ERRINT ( '#',  NCOLS                             )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                   )
         CALL CHKOUT ( 'ZZEKAD03'                              )
         RETURN
 
      END IF
 
C
C     If the value is null, make sure that nulls are permitted
C     in this column.
C
      IF (  ISNULL  .AND.  ( COLDSC(NFLIDX) .NE. ITRUE )  ) THEN
 
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )
 
         CALL CHKIN  ( 'ZZEKAD03'                                  )
         CALL SETMSG ( 'Column having index # in segment # does '  //
     .                 'not allow nulls, but a null value was '    //
     .                 'supplied for the element in record #.'     )
         CALL ERRINT ( '#',  COLIDX                                )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                        )
         CALL ERRINT ( '#',  RECNO                                 )
         CALL SIGERR ( 'SPICE(BADATTRIBUTE)'                       )
         CALL CHKOUT ( 'ZZEKAD03'                                  )
         RETURN
 
      END IF
 
C
C     Decide the length of the string value.  If the column contains
C     variable-length strings, the effective length of the string is
C     just the non-blank length of CVAL.  Otherwise, the effective
C     string length is the minimum of the non-blank length and the
C     column's declared string length.  We don't store trailing blanks.
C
      FIXLEN  =  COLDSC(LENIDX) .NE. IFALSE
      LNB     =  RTRIM ( CVAL )
 
      IF ( FIXLEN ) THEN
         STRLEN  =  MIN (  COLDSC(LENIDX),  LNB )
      ELSE
         STRLEN  =  LNB
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
C
C        Write out the data value.  If we run out of room in the
C        page we're writing to, we allocate a new page and link
C        the previous page to it.
C
         N       =  STRLEN
         POS     =  1
         LASTW   =  SEGDSC ( LCWIDX )
         P       =  SEGDSC ( LCPIDX )
         PCOUNT  =  0
 
 
         DO WHILE ( N .GT. 0 )
C
C           Write as much data as possible into the current page.
C
 
            IF ( LASTW .LT. CPSIZE-ENCSIZ ) THEN
C
C              There's room in the current page.  We never split an
C              encoded character count across pages, and we always
C              write at least one data character to the current page.
C              This practice is slightly wasteful of space but greatly
C              simplifies our logic.
C
C              Keep track of the number of pages our string spans.
C
               PCOUNT  =  PCOUNT + 1
 
C
C              If this is the first data page, write the data pointer
C              into the record pointer and the character count into
C              the data page.
C
               IF ( PCOUNT .EQ. 1 ) THEN
 
                  CALL ZZEKPGBS ( CHR, P, PBASE )
 
                  DATPTR  =  PBASE + LASTW + 1
 
                  CALL DASUDI  ( HANDLE, PTRLOC, PTRLOC, DATPTR )
                  CALL ZZEKSEI ( HANDLE, DATPTR, STRLEN )
C
C                 Advance the data pointer to the first data
C                 character's position.  The last word in use
C                 increases as well.
C
                  DATPTR  =  DATPTR + ENCSIZ
                  LASTW   =  LASTW  + ENCSIZ
 
               ELSE
C
C                 We still need the data pointer.
C
                  DATPTR  =  PBASE + 1
 
               END IF
 
C
C              Compute the number of characters to write into this page,
C              and write that number of characters.
C
               NWRITE  =  MIN ( CPSIZE-LASTW, N )
 
               CALL DASUDC (  HANDLE,   DATPTR,   DATPTR+NWRITE-1,
     .                        1,        NWRITE,   CVAL(POS:)        )
 
               N       =  N   -  NWRITE
               POS     =  POS +  NWRITE
 
C
C              The page containing the data item gains a link.
C
               CALL ZZEKGLNK ( HANDLE, CHR, P, NLINKS   )
               CALL ZZEKSLNK ( HANDLE, CHR, P, NLINKS+1 )
 
C
C              The last character word in use must be updated.
C
               LASTW              =  LASTW + NWRITE
               SEGDSC ( LCWIDX )  =  LASTW
 
C
C              Retain the base address of this data page.
C
               PRVBAS  =  PBASE
 
 
            ELSE
C
C              Allocate a data page.  If this is not the first data
C              page written to, link the previous page to the current
C              one.
C
               CALL ZZEKAPS ( HANDLE, SEGDSC, CHR, .FALSE., P2, PBASE )
 
               IF ( PCOUNT .GT. 0 ) THEN
                  CALL ZZEKSFWD ( HANDLE, CHR, P, P2 )
               END IF
 
C
C              The last character page and word in use must be updated.
C
               P                  =  P2
               LASTW              =  0
               SEGDSC ( LCPIDX )  =  P
               SEGDSC ( LCWIDX )  =  LASTW
 
C
C              Make sure the link count is zeroed out.
C
               CALL ZZEKSLNK ( HANDLE, CHR, P, 0 )
 
            END IF
 
         END DO
 
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
            CALL ZZEKIIC1 (  HANDLE,  SEGDSC,  COLDSC,
     .                       CVAL,    RECPTR,  ISNULL   )
 
         ELSE
 
            CALL CHKIN  ( 'ZZEKAD03'                                  )
            CALL SETMSG ( 'Column having index # in segment # has '   //
     .                    'index type #.'                             )
            CALL ERRINT ( '#',  COLIDX                                )
            CALL ERRINT ( '#',  SEGDSC(SNOIDX)                        )
            CALL ERRINT ( '#',  ITYPE                                 )
            CALL SIGERR ( 'SPICE(INVALIDTYPE)'                        )
            CALL CHKOUT ( 'ZZEKAD03'                                  )
            RETURN
 
         END IF
 
      END IF
 
      RETURN
      END
