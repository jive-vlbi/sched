C$Procedure     ZZEKAD06 ( EK, add data to class 6 column )
 
      SUBROUTINE ZZEKAD06 (  HANDLE,  SEGDSC,  COLDSC,
     .                       RECPTR,  NVALS,   CVALS,  ISNULL  )
 
C$ Abstract
C
C     Add a column entry to a specified record in a class 6 column.
C     The entries of class 6 columns are arrays of character string
C     values.
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
      CHARACTER*(*)         CVALS  ( * )
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
C     CVALS      I   Character values to add to column.
C     ISNULL     I   Flag indicating whether column entry is null.
C
C$ Detailed_Input
C
C     HANDLE         is the handle of an EK file that is open for write
C                    access.
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
C     CVALS          are, respectively, the number of values to add to
C                    the specified column and the set of values
C                    themselves.  The data values are written into the
C                    specified column and record.
C
C                    If the  column has fixed-size entries, then NVALS
C                    must equal the entry size for the specified column.
C
C                    Only one value can be added to a virtual column.
C
C
C     ISNULL         is a logical flag indicating whether the entry is
C                    null.  If ISNULL is .FALSE., the column entry
C                    defined by NVALS and CVALS is added to the
C                    specified kernel file.
C
C                    If ISNULL is .TRUE., NVALS and CVALS are ignored.
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
C-    Beta Version 1.0.0, 23-OCT-1995 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
 
C
C     Non-SPICELIB functions
C
      INTEGER               ZZEKRP2N
 
C
C     Local parameters
C
      INTEGER               MAXPAD
      PARAMETER           ( MAXPAD = 100 )
 
C
C     Local variables
C
      CHARACTER*(MAXPAD)    PADBUF
 
      INTEGER               COLIDX
      INTEGER               CVLEN
      INTEGER               DATPTR
      INTEGER               ELTIDX
      INTEGER               LASTW
      INTEGER               MBASE
      INTEGER               MNROOM
      INTEGER               N
      INTEGER               NCHRS
      INTEGER               NCOLS
      INTEGER               NLINKS
      INTEGER               NP
      INTEGER               NPAD
      INTEGER               NREC
      INTEGER               NWRITE
      INTEGER               P
      INTEGER               P2
      INTEGER               PADLEN
      INTEGER               PBASE
      INTEGER               POS
      INTEGER               PTRLOC
      INTEGER               RECNO
      INTEGER               REMAIN
      INTEGER               ROOM
      INTEGER               STRLEN
      INTEGER               WP
 
      LOGICAL               FIRST
      LOGICAL               FSTPAG
      LOGICAL               PAD
 
C
C     Saved variables
C
      SAVE
 
C
C     Initial values
C
      DATA                  FIRST  / .TRUE. /
 
 
C
C     Use discovery check-in.
C
      IF ( FIRST ) THEN
         PADBUF = ' '
         FIRST  = .FALSE.
      END IF
 
C
C     Make sure the record exists.
C
      NREC    =  SEGDSC ( NRIDX  )
      COLIDX  =  COLDSC ( ORDIDX )
 
C
C     Make sure the column exists.
C
      NCOLS  =  SEGDSC ( NCIDX )
 
      IF (  ( COLIDX .LT. 1 ) .OR. ( COLIDX .GT. NCOLS )  ) THEN
 
         CALL CHKIN  ( 'ZZEKAD06'                              )
         CALL SETMSG ( 'Column index = #; valid range is 1:#.' )
         CALL ERRINT ( '#',  COLIDX                            )
         CALL ERRINT ( '#',  NREC                              )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                   )
         CALL CHKOUT ( 'ZZEKAD06'                              )
         RETURN
 
      END IF
 
C
C     If the value is null, make sure that nulls are permitted
C     in this column.
C
      IF (  ISNULL  .AND.  ( COLDSC(NFLIDX) .NE. ITRUE )  ) THEN
 
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )
 
         CALL CHKIN  ( 'ZZEKAD06'                                  )
         CALL SETMSG ( 'Column having index # in segment # does '  //
     .                 'not allow nulls, but a null value was '    //
     .                 'supplied for the element in record #.'     )
         CALL ERRINT ( '#',  COLIDX                                )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                        )
         CALL ERRINT ( '#',  RECNO                                 )
         CALL SIGERR ( 'SPICE(BADATTRIBUTE)'                       )
         CALL CHKOUT ( 'ZZEKAD06'                                  )
         RETURN
 
      END IF
 
C
C     Check NVALS.  If the column has fixed-size entries, NVALS must
C     match the declared entry size.  In all cases, NVALS must be
C     positive.
C
      IF ( NVALS .LT. 1 ) THEN
 
         CALL CHKIN  ( 'ZZEKAD06'                                  )
         CALL SETMSG ( 'COLIDX = #;  segment = #; NVALS = #;  '  //
     .                 'NVALS must be positive '                   )
         CALL ERRINT ( '#',  COLIDX                                )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                        )
         CALL ERRINT ( '#',  NVALS                                 )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                       )
         CALL CHKOUT ( 'ZZEKAD06'                                  )
         RETURN
 
      END IF
 
 
      IF ( COLDSC(SIZIDX) .NE. IFALSE ) THEN
 
         IF ( NVALS .NE. COLDSC(SIZIDX) ) THEN
 
            CALL CHKIN  ( 'ZZEKAD06'                                   )
            CALL SETMSG ( 'COLIDX = #;  segment = #; NVALS = #; '     //
     .                    'declared entry size = #.  Sizes must match.')
            CALL ERRINT ( '#',  COLIDX                                 )
            CALL ERRINT ( '#',  SEGDSC(SNOIDX)                         )
            CALL ERRINT ( '#',  NVALS                                  )
            CALL ERRINT ( '#',  COLDSC(SIZIDX)                         )
            CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                        )
            CALL CHKOUT ( 'ZZEKAD06'                                   )
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
C
C        Decide now whether we will need to pad the input entry
C        elements with trailing blanks, and if so how much padding
C        we'll need.
C
         STRLEN  =  COLDSC ( LENIDX   )
         CVLEN   =  LEN    ( CVALS(1) )
         PAD     =  CVLEN  .LT.  STRLEN
 
         IF ( PAD ) THEN
            PADLEN  =  STRLEN - CVLEN
         END IF
 
         LASTW   =  SEGDSC ( LCWIDX )
         ROOM    =  CPSIZE - LASTW
         FSTPAG  =  .TRUE.
 
C
C        Initialize the page base and target data pointer, if possible.
C        If the current page is full, these functions will be performed
C        below in the code section in which a new page is allocated.
C
         IF ( LASTW .LT. CPSIZE ) THEN
 
            P       =  SEGDSC(LCPIDX)
 
            CALL ZZEKPGBS ( CHR,  P,  PBASE )
 
            DATPTR  =  PBASE + LASTW + 1
 
         END IF
 
 
         ELTIDX  =  1
 
         DO WHILE (  ( ELTIDX .LE. NVALS ) .AND. ( .NOT. FAILED() )  )
C
C           Write out the element having index ELTIDX.
C
            POS     =  0
            REMAIN  =  STRLEN
 
 
            DO WHILE ( REMAIN .GT. 0 )
C
C              Decide where to write the data values.  In order to write
C              a new entry, we require enough room for the count
C              and at least one character of data.
C
               IF ( FSTPAG ) THEN
                  MNROOM  =  1 + ENCSIZ
               ELSE
                  MNROOM  =  1
               END IF
 
 
               IF ( ROOM  .GE.  MNROOM ) THEN
C
C                 There's room in the current page.  If this is the
C                 first page this entry is written on, set the data
C                 pointer and count.  Write as much of the value as
C                 possible to the current page.
C
                  IF ( FSTPAG ) THEN
 
                     CALL DASUDI  ( HANDLE, PTRLOC, PTRLOC, DATPTR )
                     CALL ZZEKSEI ( HANDLE, DATPTR, NVALS          )
 
                     ROOM    =  ROOM   - ENCSIZ
                     DATPTR  =  DATPTR + ENCSIZ
 
C
C                    The first page containing some or all of the data
C                    item gains a link.
C
                     CALL ZZEKGLNK ( HANDLE, CHR, P, NLINKS   )
                     CALL ZZEKSLNK ( HANDLE, CHR, P, NLINKS+1 )
 
                  END IF
 
C
C                 Write the characters we can fit onto the current page.
C
                  NWRITE  =  MIN ( REMAIN, ROOM )
                  N       =  NWRITE
 
                  DO WHILE ( N .GT. 0 )
 
                     IF ( POS .LT. CVLEN )   THEN
C
C                       Take data from the input string CVALS(ELTIDX).
C
                        NCHRS  =  MIN ( N, CVLEN - POS )
 
                        CALL DASUDC ( HANDLE,
     .                                DATPTR,
     .                                DATPTR + NCHRS - 1,
     .                                POS    + 1,
     .                                POS    + NCHRS,
     .                                CVALS( ELTIDX )     )
 
                        N      =  N      - NCHRS
                        POS    =  POS    + NCHRS
                        DATPTR =  DATPTR + NCHRS
 
 
                     ELSE IF ( PAD ) THEN
C
C                       We must add trailing blanks to the column
C                       entry at this point.
C
                        NPAD  =  MIN ( N, PADLEN )
                        NP    =  NPAD
 
                        DO WHILE ( NP .GT. 0 )
 
                           WP  =  MIN ( NP, MAXPAD )
 
                           CALL DASUDC ( HANDLE,
     .                                   DATPTR,
     .                                   DATPTR + WP - 1,
     .                                   1,
     .                                   WP,
     .                                   PADBUF          )
 
                           NP      =  NP     - WP
                           DATPTR  =  DATPTR + WP
 
                        END DO
 
                        N   =   N   - NPAD
                        POS =   POS + NPAD
 
                     END IF
 
                  END DO
C
C                 We've written all we can to the current page.
C
                  REMAIN  =  REMAIN - NWRITE
                  ROOM    =  ROOM   - NWRITE
 
C
C                 The last character word in use must be updated.
C                 Account for the count, if this is the first page on
C                 which the current entry is written.
C
                  IF ( FSTPAG ) THEN
 
                     LASTW              =  LASTW + ENCSIZ + NWRITE
                     SEGDSC ( LCWIDX )  =  LASTW
                     FSTPAG             =  .FALSE.
 
                  ELSE
 
                     LASTW              =  LASTW + NWRITE
                     SEGDSC ( LCWIDX )  =  LASTW
 
                  END IF
 
 
               ELSE
C
C                 Allocate a character data page.  If this is not the
C                 first data page written to, link the previous page to
C                 the current one.
C
                  CALL ZZEKAPS ( HANDLE,   SEGDSC,  CHR,
     .                           .FALSE.,  P2,      PBASE  )
 
                  IF ( .NOT. FSTPAG ) THEN
                     CALL ZZEKSFWD ( HANDLE, CHR, P, P2 )
                  END IF
 
                  P                  =  P2
                  LASTW              =  0
                  SEGDSC ( LCPIDX )  =  P
                  SEGDSC ( LCWIDX )  =  LASTW
 
                  ROOM               =  CPSIZE
                  DATPTR             =  PBASE  +  1
 
C
C                 Set the link count.  If this is the first page
C                 onto which the input column entry is written,
C                 just zero out the count; the count will be set above.
C                 Additional pages get one link.
C
                  IF ( FSTPAG ) THEN
                     NLINKS  =  0
                  ELSE
                     NLINKS  =  1
                  END IF
 
                  CALL ZZEKSLNK ( HANDLE, CHR, P, NLINKS )
 
               END IF
 
 
            END DO
C
C           We've written out the current element.
C
 
            ELTIDX  =  ELTIDX + 1
 
         END DO
 
 
      END IF
 
C
C     Write out the updated segment descriptor.
C
      MBASE  =  SEGDSC(IMDIDX)
 
      CALL DASUDI ( HANDLE, MBASE+1, MBASE+SDSCSZ, SEGDSC )
 
C
C     Class 6 columns are not indexed, so we need not update any
C     index to account for the new element.
C
 
      RETURN
      END
