C$Procedure     ZZEKAC04 ( EK, add class 4 column to segment )
 
      SUBROUTINE ZZEKAC04 (  HANDLE,  SEGDSC,  COLDSC,
     .                       IVALS,   ENTSZS,  NLFLGS  )
 
C$ Abstract
C
C     Add an entire class 4 column to an EK segment.
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
      INTEGER               ENTSZS ( * )
      LOGICAL               NLFLGS ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle attached to new EK file.
C     SEGDSC     I   Segment descriptor.
C     COLDSC     I   Column descriptor.
C     IVALS      I   Integer values to add to column.
C     ENTSZS     I   Array of sizes of column entries.
C     NLFLGS     I   Array of null flags for column entries.
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
C     ENTSZS         is an array containing sizes of column entries.
C                    The Ith element of ENTSZS gives the size of the
C                    Ith column entry.  ENTSZS is used only for columns
C                    having variable-size entries.  For such columns,
C                    the dimension of ENTSZS must be at least NROWS.
C                    The size of null entries should be set to zero.
C
C                    For columns having fixed-size entries, the
C                    dimension of this array may be any positive value.
C
C     IVALS          is an array containing the entire set of column
C                    entries for the specified column.  The entries
C                    are listed in row-order:  the column entry for the
C                    first row of the segment is first, followed by the
C                    column entry for the second row, and so on.  The
C                    number of column entries must match the declared
C                    number of rows in the segment.  For columns having
C                    fixed-size entries, a null entry must be allocated
C                    the same amount of space occupied by a non-null
C                    entry in the array IVALS.  For columns having
C                    variable-size entries, null entries do not require
C                    any space in the IVALS array, but in any case must
C                    have their allocated space described correctly by
C                    the corresponding element of the ENTSZS array
C                    (described below).
C
C     ENTSZS         is an array containing sizes of column entries.
C                    The Ith element of ENTSZS gives the size of the
C                    Ith column entry.  ENTSZS is used only for columns
C                    having variable-size entries.  For such columns,
C                    the dimension of ENTSZS must be at least NROWS.
C                    The size of null entries should be set to zero.
C
C                    For columns having fixed-size entries, the
C                    dimension of this array may be any positive value.
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
C     2)  Only one segment can be created at a time using the fast
C         load routines.
C
C     3)  No other EK operation may interrupt a fast load.  For
C         example, it is not valid to issue a query while a fast load
C         is in progress.
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
C-    SPICELIB Version 1.0.0, 26-SEP-1995 (NJB)
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
      INTEGER               CURSIZ
      INTEGER               FROM
      INTEGER               I
      INTEGER               N
      INTEGER               NDATA
      INTEGER               NELT
      INTEGER               NLINK
      INTEGER               NROWS
      INTEGER               NULPTR
      INTEGER               P
      INTEGER               P2
      INTEGER               PAGE   ( PGSIZI )
      INTEGER               PBASE
      INTEGER               REMAIN
      INTEGER               ROW
      INTEGER               SIZE
      INTEGER               TO
 
      LOGICAL               CNTINU
      LOGICAL               FIXSIZ
      LOGICAL               NEWREQ
      LOGICAL               NULLOK
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKAC04' )
      END IF
 
C
C     Grab the column's attributes.
C
      CLASS  =  COLDSC( CLSIDX )
      NULPTR =  COLDSC( NFLIDX )
      COLIDX =  COLDSC( ORDIDX )
      SIZE   =  COLDSC( SIZIDX )
 
      NULLOK =  NULPTR .NE. IFALSE
      FIXSIZ =  SIZE   .NE. IFALSE
 
C
C     This column had better be class 4.
C
      IF ( CLASS .NE. 4 ) THEN
 
         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )
 
         CALL SETMSG ( 'Column class code # found in descriptor for ' //
     .                 'column #.  Class should be 4.'                 )
         CALL ERRINT ( '#', CLASS                                      )
         CALL ERRCH  ( '#', COLUMN                                     )
         CALL SIGERR ( 'SPICE(NOCLASS)'                                )
         CALL CHKOUT ( 'ZZEKAC04'                                      )
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
C     Record the number of data values to write.
C
      IF ( NULLOK ) THEN
C
C        Sum the sizes of the non-null column entries; these are the
C        ones that will take up space.
C
         NDATA  =  0
 
         DO I = 1, NROWS
 
            IF ( .NOT. NLFLGS(I) ) THEN
 
               IF ( FIXSIZ ) THEN
                  NDATA  =  NDATA + SIZE
               ELSE
                  NDATA  =  NDATA + ENTSZS(I)
               END IF
 
            END IF
 
         END DO
 
 
      ELSE
 
 
         IF ( FIXSIZ ) THEN
 
            NDATA  =  NROWS * SIZE
 
         ELSE
 
            NDATA  =  0
 
            DO I = 1, NROWS
               NDATA  =  NDATA + ENTSZS(I)
            END DO
 
         END IF
 
 
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
C     addresses to.  The variable NELT is the count of the column entry
C     elements written for the current entry.  The variable N indicates
C     the number of integers written to the current page.
C
 
      REMAIN  =  NDATA
      FROM    =  1
      TO      =  1
      BUFPTR  =  1
      ROW     =  1
      NELT    =  1
      N       =  0
      NLINK   =  0
 
 
      DO WHILE ( ROW .LE. NROWS )
 
C
C        NEWREQ is set to TRUE if we discover that the next column
C        entry must start on a new page.
C
         NEWREQ  =  .FALSE.
 
 
         IF (  NULLOK  .AND.  ( NLFLGS(ROW) )  ) THEN
 
 
            IF ( FIXSIZ ) THEN
               CURSIZ  =  SIZE
            ELSE
               CURSIZ  =  ENTSZS(ROW)
            END IF
 
 
            FROM           =  FROM   +  CURSIZ
            ADRBUF(BUFPTR) =  NULL
            BUFPTR         =  BUFPTR +  1
            ROW            =  ROW    +  1
            NELT           =  1
            CNTINU         =  .FALSE.
 
 
         ELSE
 
 
            IF ( NELT .EQ. 1 ) THEN
C
C              We're about to write out a new column entry.  We must
C              insert the element count into the page before writing the
C              data.  The link count for the current page must be
C              incremented to account for this new entry.
C
C              At this point, we're guaranteed at least two free
C              spaces in the current page.
C
               IF ( FIXSIZ ) THEN
                  CURSIZ  =  SIZE
               ELSE
                  CURSIZ  =  ENTSZS(ROW)
               END IF
 
               ADRBUF(BUFPTR) =  TO     +  PBASE
               BUFPTR         =  BUFPTR + 1
               PAGE(TO)       =  CURSIZ
               TO             =  TO     + 1
               N              =  N      + 1
               NLINK          =  NLINK  + 1
 
            END IF
 
C
C           At this point, there's at least one free space in the
C           current page.
C
            PAGE(TO)  =  IVALS(FROM)
            TO        =  TO      +  1
            N         =  N       +  1
            FROM      =  FROM    +  1
            REMAIN    =  REMAIN  -  1
 
C
C           Decide whether we must continue the current entry on another
C           data page.
C
            CNTINU  =  ( NELT .LT. CURSIZ ) .AND. ( N .EQ. IPSIZE )
 
            IF ( NELT .EQ. CURSIZ ) THEN
C
C              The current element is the last of the current column
C              entry.
C
C              Determine whether we must start the next column entry on
C              a new page.  To start a column entry on the current page,
C              we must have enough room for the element count and at
C              least the first entry element.
C
               IF ( REMAIN .GT. 0 ) THEN
                  NEWREQ   =   N  .GT. ( IPSIZE - 2 )
               END IF
 
 
               NELT  =  1
               ROW   =  ROW   +  1
 
            ELSE
 
               NELT  =  NELT  +  1
 
            END IF
 
         END IF
 
 
         IF (  ( BUFPTR .GT. BUFSIZ ) .OR. ( ROW .GT. NROWS )  ) THEN
C
C           The address buffer is full or we're out of input values
C           to look at, so push the buffer contents on the stack.
C
            CALL ZZEKSPSH ( BUFPTR-1, ADRBUF )
            BUFPTR  =  1
 
         END IF
 
 
         IF (         CNTINU
     .       .OR.     NEWREQ
     .       .OR. ( ( ROW .GT. NROWS ) .AND. ( NDATA .GT. 0 ) )  ) THEN
 
C
C           It's time to write out the current page.  First set the link
C           count.
C
            PAGE ( ILCIDX ) =  NLINK
 
C
C           Write out the data page.
C
            CALL ZZEKPGWI ( HANDLE, P, PAGE  )
 
C
C           If there's more data to write, allocate another page.
C
            IF ( REMAIN .GT. 0 ) THEN
 
               CALL ZZEKAPS ( HANDLE, SEGDSC, INT, .FALSE., P2, PBASE )
               CALL CLEARI  ( PGSIZI, PAGE )
 
               N      =  0
               NLINK  =  0
               TO     =  1
 
C
C              If we're continuing an element from the previous page,
C              link the previous page to the current one.
C
               IF ( CNTINU ) THEN
                  CALL ZZEKSFWD ( HANDLE, INT, P, P2 )
               END IF
 
               P   =  P2
 
            END IF
C
C           We've allocated a new data page if we needed one.
C
 
         END IF
C
C        We've written out the last completed data page.
C
 
      END DO
C
C     We've processed all entries of the input array.
C
 
      CALL CHKOUT ( 'ZZEKAC04' )
      RETURN
      END
