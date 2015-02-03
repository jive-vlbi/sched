C$Procedure     ZZEKAC01 ( EK, add class 1 column to segment )
 
      SUBROUTINE ZZEKAC01 (  HANDLE,  SEGDSC,  COLDSC,  IVALS,
     .                       NLFLGS,  RCPTRS,  WKINDX         )
 
C$ Abstract
C
C     Add an entire class 1 column to an EK segment.
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
 
 
      INCLUDE  'ekbool.inc'
      INCLUDE  'ekcoldsc.inc'
      INCLUDE  'ekcnamsz.inc'
      INCLUDE  'ekdatpag.inc'
      INCLUDE  'ekpage.inc'
      INCLUDE  'ekrecptr.inc'
      INCLUDE  'eksegdsc.inc'
      INCLUDE  'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               SEGDSC ( * )
      INTEGER               COLDSC ( * )
      INTEGER               IVALS  ( * )
      LOGICAL               NLFLGS ( * )
      INTEGER               RCPTRS ( * )
      INTEGER               WKINDX ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle attached to new EK file.
C     SEGDSC     I   Segment descriptor.
C     COLDSC     I   Column descriptor.
C     IVALS      I   Integer values to add to column.
C     NLFLGS     I   Array of null flags for column entries.
C     RCPTRS     I   Array of record pointers for segment.
C     WKINDX    I-O  Work space for column index.
C
C$ Detailed_Input
C
C     HANDLE         the handle of an EK file that is open for writing.
C                    A `begin segment for fast load' operation must
C                    have already been performed for the designated
C                    segment.
C
C     SEGDSC         is a descriptor for the segment to which data is
C                    to be added.  The segment descriptor is not
C                    updated by this routine, but some fields in the
C                    descriptor will become invalid after this routine
C                    returns.
C
C     COLDSC         is a descriptor for the column to be added.  The
C                    column attributes must be filled in, but any
C                    pointers may be uninitialized.
C
C     IVALS          is an array containing the entire set of column
C                    entries for the specified column.  The entries
C                    are listed in row-order:  the column entry for the
C                    first row of the segment is first, followed by the
C                    column entry for the second row, and so on.  The
C                    number of column entries must match the declared
C                    number of rows in the segment.  Elements must be
C                    allocated for each column entry, including null
C                    entries.
C
C     NLFLGS         is an array of logical flags indicating whether
C                    the corresponding entries are null.  If the Ith
C                    element of NLFLGS is .FALSE., the Ith column entry
C                    defined by IVALS is added to the specified segment
C                    in the specified kernel file.
C
C                    If the Ith element of NLFGLS is .TRUE., the
C                    contents of the Ith column entry are undefined.
C
C                    NLFLGS is used only for columns that allow null
C                    values; it's ignored for other columns.
C
C     RCPTRS         is an array of record pointers for the input
C                    segment.  These pointers are base addresses of the
C                    `record pointer structures' for the segment.
C                    These pointers are used instead of record numbers
C                    in column indexes:  the indexes map ordinal
C                    positions to record pointers.
C
C     WKINDX         is a work space array used for building a column
C                    index.  If the column is indexed, the dimension of
C                    WKINDX must be at NROWS, where NROWS is the number
C                    of rows in the column.  If the column is not
C                    indexed, this work space is not used, so the
C                    dimension may be any positive value.
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
C     2)  If an I/O error occurs while reading or writing the indicated
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
C     done by sequential calls to EKACEI, but has the drawback that
C     the caller must use more memory for the routine's inputs.  This
C     routine cannot be used to add data to a partially completed
C     column.
C
C$ Examples
C
C     See EKACLI.
C
C$ Restrictions
C
C     1)  This routine assumes the EK scratch area has been set up
C         properly for a fast load operation.  This routine writes
C         to the EK scratch area as well.
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
C-    SPICELIB Version 1.1.0, 22-JUL-1996 (NJB)
C
C        Bug fix:  case of 100% null data values is now handled
C        correctly.  Previous version line was changed from "Beta"
C        to "SPICELIB."
C
C-    SPICELIB Version 1.0.0, 25-SEP-1995 (NJB)
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 22-JUL-1996 (NJB)
C
C        Bug fix:  case of 100% null data values is now handled
C        correctly.  The test to determine when to write a page
C        was fixed to handle this case.
C
C        Previous version line was changed from "Beta"
C        to "SPICELIB."
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = IPSIZE )
 
C
C     Local variables
C
      CHARACTER*(CNAMSZ)    COLUMN
 
      INTEGER               ADRBUF ( BUFSIZ )
      INTEGER               BUFPTR
      INTEGER               CLASS
      INTEGER               COLIDX
      INTEGER               DSCBAS
      INTEGER               FROM
      INTEGER               I
      INTEGER               IDXTYP
      INTEGER               P
      INTEGER               PAGE   ( PGSIZI )
      INTEGER               PBASE
      INTEGER               MBASE
      INTEGER               N
      INTEGER               NDATA
      INTEGER               NNULL
      INTEGER               NROWS
      INTEGER               NULPTR
      INTEGER               NWRITE
      INTEGER               REMAIN
      INTEGER               TO
      INTEGER               TREE
 
      LOGICAL               INDEXD
      LOGICAL               NULLOK
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKAC01' )
      END IF
 
C
C     Grab the column's attributes.
C
      CLASS  =  COLDSC( CLSIDX )
      IDXTYP =  COLDSC( IXTIDX )
      NULPTR =  COLDSC( NFLIDX )
      COLIDX =  COLDSC ( ORDIDX )
 
      NULLOK =  NULPTR .NE. IFALSE
      INDEXD =  IDXTYP .NE. IFALSE
 
C
C     This column had better be class 1.
C
      IF ( CLASS .NE. 1 ) THEN
 
         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )
 
         CALL SETMSG ( 'Column class code # found in descriptor for ' //
     .                 'column #.  Class should be 1.'                 )
         CALL ERRINT ( '#', CLASS                                      )
         CALL ERRCH  ( '#', COLUMN                                     )
         CALL SIGERR ( 'SPICE(NOCLASS)'                                )
         CALL CHKOUT ( 'ZZEKAC01'                                      )
         RETURN
 
      END IF
 
C
C     If the column is indexed, the index type should be 1; we don't
C     know how to create any other type of index.
C
      IF  (  INDEXD  .AND.  ( IDXTYP .NE. 1 )  ) THEN
 
         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )
 
         CALL SETMSG ( 'Index type code # found in descriptor for '   //
     .                 'column #.  Code should be 1.'                  )
         CALL ERRINT ( '#', IDXTYP                                     )
         CALL ERRCH  ( '#', COLUMN                                     )
         CALL SIGERR ( 'SPICE(UNRECOGNIZEDTYPE)'                       )
         CALL CHKOUT ( 'ZZEKAC01'                                      )
         RETURN
 
      END IF
 
C
C     Push the column's ordinal index on the stack.  This allows us
C     to identify the column the addresses belong to.
C
      CALL ZZEKSPSH ( 1, COLIDX )
 
C
C     Find the number of rows in the segment.
C
      NROWS   =  SEGDSC ( NRIDX )
 
C
C     Decide how many pages we'll need to store the data.  Also
C     record the number of data values to write.
C
      IF ( NULLOK ) THEN
C
C        Count the non-null rows; these are the ones that will take
C        up space.
C
         NNULL  =  0
 
         DO I = 1, NROWS
 
            IF ( NLFLGS(I) ) THEN
               NNULL  =  NNULL + 1
            END IF
 
         END DO
 
         NDATA  =  NROWS - NNULL
 
 
      ELSE
 
         NDATA  =  NROWS
 
      END IF
 
 
      IF ( NDATA .GT. 0 ) THEN
C
C        There's some data to write, so allocate a page.  Also
C        prepare a data buffer to be written out as a page.
C
         CALL ZZEKAPS  ( HANDLE, SEGDSC, INT, .FALSE., P, PBASE )
         CALL CLEARI   ( PGSIZI, PAGE )
 
      END IF
 
C
C     Write the input data out to the target file a page at a time.
C     Null values don't get written.
C
C     While we're at it, we'll push onto the EK stack the addresses
C     of the column entries.  We use the constant NULL rather than an
C     address to represent null entries.
C
C     We'll use FROM to indicate the element of IVALS we're
C     considering, TO to indicate the element of PAGE to write
C     to, and BUFPTR to indicate the element of ADRBUF to write
C     addresses to.  The variable N indicates the number of data
C     items in the current page.
C
 
      REMAIN  =  NROWS
      FROM    =  1
      TO      =  1
      BUFPTR  =  1
      NWRITE  =  0
      N       =  0
 
      DO WHILE ( REMAIN .GT. 0 )
 
         IF (  NULLOK  .AND.  ( NLFLGS(FROM) )  ) THEN
 
            ADRBUF(BUFPTR) =  NULL
 
         ELSE
 
            ADRBUF(BUFPTR) =  TO      +  PBASE
            PAGE(TO)       =  IVALS(FROM)
            TO             =  TO      +  1
            NWRITE         =  NWRITE  +  1
            N              =  N       +  1
 
         END IF
 
 
         FROM    =  FROM   + 1
         REMAIN  =  REMAIN - 1
 
 
         IF (  ( BUFPTR .EQ. BUFSIZ ) .OR. ( REMAIN .EQ. 0 )  ) THEN
C
C           The address buffer is full or we're out of input values
C           to look at, so push the buffer contents on the stack.
C
            CALL ZZEKSPSH ( BUFPTR, ADRBUF )
            BUFPTR  =  1
 
         ELSE
 
            BUFPTR  =  BUFPTR + 1
 
         END IF
 
 
         IF (  (   N      .EQ. IPSIZE ) .OR.
     .         ( ( NWRITE .EQ. NDATA  ) .AND. ( NDATA  .NE. 0 ) ) ) THEN
C
C           Either the current data page is full, or we've buffered
C           the last of the available data.  It's time to write out the
C           current page.  First set the link count.
C
            PAGE ( ILCIDX ) =  N
 
C
C           Write out the data page.
C
            CALL ZZEKPGWI ( HANDLE, P, PAGE  )
 
C
C           If there's more data to write, allocate another page.
C
            IF ( NWRITE .LT. NDATA ) THEN
 
               CALL ZZEKAPS ( HANDLE, SEGDSC, INT, .FALSE., P, PBASE )
               CALL CLEARI  ( PGSIZI, PAGE )
 
               N   =  0
               TO  =  1
 
            END IF
 
         END IF
 
      END DO
 
C
C     If the column is supposed to have an index, now is the time to
C     build that index.  We'll find the order vector for the input
C     values, overwrite the elements of the order vector with the
C     corresponding elements of the input array of record pointers, then
C     load this sorted copy of the record pointer array into a tree in
C     one shot.
C
      IF ( INDEXD ) THEN
 
         CALL ZZEKORDI ( IVALS,  NULLOK, NLFLGS, NROWS, WKINDX )
 
         DO I = 1, NROWS
            WKINDX(I) =  RCPTRS( WKINDX(I) )
         END DO
 
         CALL ZZEKTRIT ( HANDLE, TREE                )
         CALL ZZEKTR1S ( HANDLE, TREE, NROWS, WKINDX )
 
C
C        Update the segment's metadata to point to the index.  The
C        pointer indicates the root page of the tree.
C
         MBASE   =  SEGDSC ( IMDIDX )
         DSCBAS  =  MBASE  +  SDSCSZ  +  (COLIDX-1)*CDSCSZ
 
         CALL DASUDI ( HANDLE,  DSCBAS+IXPIDX,  DSCBAS+IXPIDX,  TREE )
 
      END IF
 
 
      CALL CHKOUT ( 'ZZEKAC01' )
      RETURN
      END
