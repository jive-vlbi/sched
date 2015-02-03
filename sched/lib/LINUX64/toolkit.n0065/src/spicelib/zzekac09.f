C$Procedure     ZZEKAC09 ( EK, add class 9 column to segment )
 
      SUBROUTINE ZZEKAC09 (  HANDLE,  SEGDSC,  COLDSC,
     .                       CVALS,   NLFLGS,  WKINDX  )
 
C$ Abstract
C
C     Add an entire class 9 column to an EK segment.
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
      INCLUDE  'ekclas09.inc'
      INCLUDE  'ekcnamsz.inc'
      INCLUDE  'ekcoldsc.inc'
      INCLUDE  'ekdatpag.inc'
      INCLUDE  'ekpage.inc'
      INCLUDE  'eksegdsc.inc'
      INCLUDE  'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               SEGDSC ( * )
      INTEGER               COLDSC ( * )
      CHARACTER*(*)         CVALS  ( * )
      LOGICAL               NLFLGS ( * )
      INTEGER               WKINDX ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle attached to new EK file.
C     SEGDSC     I   Segment descriptor.
C     COLDSC     I   Column descriptor.
C     CVALS      I   Character values to add to column.
C     NLFLGS     I   Array of null flags for column entries.
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
C     CVALS          is an array containing the entire set of column
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
C                    defined by CVALS is added to the specified segment
C                    in the specified kernel file.
C
C                    If the Ith element of NLFGLS is .TRUE., the
C                    contents of the Ith column entry are undefined.
C
C                    NLFLGS is used only for columns that allow null
C                    values; it's ignored for other columns.
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
C     done by sequential calls to EKACEC, but has the drawback that
C     the caller must use more memory for the routine's inputs.  This
C     routine cannot be used to add data to a partially completed
C     column.
C
C     Class 9 columns have fixed record counts, and contain
C     fixed-length strings.
C
C$ Examples
C
C     See EKACLC.
C
C$ Restrictions
C
C     1)  This routine assumes the EK file has been set up
C         properly for a fast load operation.
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
C-    Beta Version 1.0.0, 09-NOV-1995 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
      CHARACTER*(CNAMSZ)    COLUMN
      CHARACTER*(PGSIZC)    PAGE
 
      INTEGER               CLASS
      INTEGER               CMBASE
      INTEGER               COLIDX
      INTEGER               DATBAS
      INTEGER               DSCBAS
      INTEGER               FROM
      INTEGER               IDXBAS
      INTEGER               IDXPAG
      INTEGER               IDXTYP
      INTEGER               P
      INTEGER               L
      INTEGER               MBASE
      INTEGER               NFLBAS
      INTEGER               NFLPAG
      INTEGER               NPAGE
      INTEGER               NROWS
      INTEGER               NULPTR
      INTEGER               SPP
      INTEGER               TO
 
      LOGICAL               FIXLEN
      LOGICAL               INDEXD
      LOGICAL               NULLOK
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKAC09' )
      END IF
 
C
C     Grab the column's attributes.  Initialize the maximum non-blank
C     width of the column.
C
      CLASS  =  COLDSC ( CLSIDX )
      IDXTYP =  COLDSC ( IXTIDX )
      NULPTR =  COLDSC ( NFLIDX )
      COLIDX =  COLDSC ( ORDIDX )
      L      =  COLDSC ( LENIDX )
 
      NULLOK =  NULPTR .NE. IFALSE
      INDEXD =  IDXTYP .NE. IFALSE
      FIXLEN =  L      .NE. IFALSE
 
C
C     This column had better be class 9.
C
      IF ( CLASS .NE. 9 ) THEN
 
         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )
 
         CALL SETMSG ( 'Column class code # found in descriptor for ' //
     .                 'column #.  Class should be 9.'                 )
         CALL ERRINT ( '#', CLASS                                      )
         CALL ERRCH  ( '#', COLUMN                                     )
         CALL SIGERR ( 'SPICE(NOCLASS)'                                )
         CALL CHKOUT ( 'ZZEKAC09'                                      )
         RETURN
 
      END IF
 
C
C     Make sure the column has fixed-length strings.
C
      IF  ( .NOT. FIXLEN ) THEN
 
         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )
 
         CALL SETMSG ( 'Column # has variable string length; class '  //
     .                 '9 supports fixed-length strings only.'         )
         CALL ERRCH  ( '#', COLUMN                                     )
         CALL SIGERR ( 'SPICE(BUG)'                                    )
         CALL CHKOUT ( 'ZZEKAC09'                                      )
         RETURN
 
      END IF
 
C
C     Check the input string length.
C
      IF (      ( L .LT. 0             )
     .     .OR. ( L .GT. LEN(CVALS(1)) )
     .     .OR. ( L .GT. CPSIZE        )  ) THEN
 
         CALL SETMSG ( 'String length # is just plain wrong.' )
         CALL ERRINT ( '#',  L                                )
         CALL SIGERR ( 'SPICE(INVALIDSIZE)'                   )
         CALL CHKOUT ( 'ZZEKAC09'                             )
         RETURN
 
      END IF
 
C
C     Compute the number of strings we can hold in one page.
C
      SPP  =  CPSIZE / L
 
C
C     Find the number of rows in the segment.
C
      NROWS = SEGDSC ( NRIDX )
 
C
C     Decide how many pages are required to hold the array, and
C     allocate that many new, contiguous pages.
C
      NPAGE  =  ( NROWS + SPP - 1 ) / SPP
 
      CALL ZZEKACPS ( HANDLE, SEGDSC, CHR, NPAGE, P, DATBAS )
 
C
C     We'll use FROM to indicate the element of CVALS we're
C     considering and TO to indicate the element of PAGE to write
C     to.
C
      TO   =  1
      PAGE =  ' '
 
 
      DO FROM = 1, NROWS
C
C        The Assignment.
C
         IF (  ( .NOT. NULLOK ) .OR. ( .NOT. NLFLGS(FROM) )  ) THEN
 
            PAGE( TO : TO+L-1 ) =  CVALS(FROM)
 
         END IF
 
         TO  =  TO + L
 
 
         IF (      ( TO   .GT. CPSIZE-L+1 )
     .        .OR. ( FROM .EQ. NROWS      )  ) THEN
C
C           Either the current data page is full, or we've buffered
C           the last of the available data.  It's time to write out the
C           current page.
C
            CALL ZZEKPGWC ( HANDLE, P, PAGE  )
 
C
C           Set the link count.
C
            CALL ZZEKSLNK ( HANDLE, CHR, P,  (TO-L)/L  )
 
C
C           Next page.
C
            P   =  P + 1
            TO  =  1
 
         END IF
 
      END DO
 
C
C     Update the column's metadata area to point to the data array.
C
      CMBASE = COLDSC(METIDX)
 
      CALL DASUDI ( HANDLE, CMBASE+DBIX09, CMBASE+DBIX09, DATBAS )
 
C
C     If the column is supposed to have an index, now is the time to
C     build that index.  Type 2 indexes are just order vectors.
C
      IF ( INDEXD ) THEN
C
C        Compute the order vector.
C
         CALL ZZEKORDC ( CVALS,  NULLOK, NLFLGS, NROWS, WKINDX )
 
C
C        Write out the index.
C
         CALL ZZEKWPAI ( HANDLE, SEGDSC, NROWS, WKINDX, IDXPAG, IDXBAS )
 
C
C        Update the column's metadata to point to the index.  The
C        pointer indicates base address of the index.  Also set the
C        index type in the column descriptor.
C
         MBASE   =  SEGDSC ( IMDIDX )
         DSCBAS  =  MBASE  +  SDSCSZ  +  (COLIDX-1)*CDSCSZ
 
         CALL DASUDI ( HANDLE,  DSCBAS+IXPIDX,  DSCBAS+IXPIDX,  IDXBAS )
         CALL DASUDI ( HANDLE,  DSCBAS+IXTIDX,  DSCBAS+IXTIDX,  2      )
 
      END IF
 
 
      IF ( NULLOK ) THEN
C
C        Nulls are allowed.  Write out the null flag array.
C
         CALL ZZEKWPAL ( HANDLE, SEGDSC, NROWS, NLFLGS, NFLPAG, NFLBAS )
 
C
C        Update the column's metadata area to point to the null flag
C        array.
C
         CALL DASUDI ( HANDLE, CMBASE+NFIX09, CMBASE+NFIX09, NFLBAS )
 
      END IF
 
      CALL CHKOUT ( 'ZZEKAC09' )
      RETURN
      END
