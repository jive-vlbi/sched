C$Procedure      ZZEKJSRT ( EK, join row set union sort )
 
      SUBROUTINE ZZEKJSRT ( NJRS,    UBASES,  NORDER,  OTABS,
     .                      OCOLS,   OELTS,   SENSES,  STHAN,
     .                      STSDSC,  STDTPT,  DTPOOL,  DTDSCS,  ORDBAS )
 
C$ Abstract
C
C     Sort the row vectors of a join row set union, given an order
C     relation defined by a set of qualified order-by columns.
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
      INCLUDE 'ekjrs.inc'
      INCLUDE 'ekopcd.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'ekquery.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'
 
 
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )
 
      INTEGER               NJRS
      INTEGER               UBASES ( * )
      INTEGER               NORDER
      INTEGER               OTABS  ( * )
      INTEGER               OCOLS  ( * )
      INTEGER               OELTS  ( * )
      INTEGER               SENSES ( * )
      INTEGER               STHAN  ( * )
      INTEGER               STSDSC ( SDSCSZ, * )
      INTEGER               STDTPT ( * )
      INTEGER               DTPOOL ( 2,      LBPOOL : * )
      INTEGER               DTDSCS ( CDSCSZ,          * )
      INTEGER               ORDBAS
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NJRS       I   Number of join row sets in union.
C     UBASES     I   Base addresses of join row sets of union.
C     NORDER     I   Number of order-by columns.
C     OTABS      I   Order-by table indices relative to FROM clause.
C     OCOLS      I   Order-by column indices.
C     OELTS      I   Order-by element indices.
C     SENSES     I   Order directions.
C     STHAN      I   Handles of loaded files from segment table.
C     STSDSC     I   Array of descriptors of loaded segments.
C     STDTPT     I   Array of pointers to column descriptors.
C     DTPOOL     I   Column descriptor table pool.
C     DTDSCS     I   Column descriptor table.
C     ORDBAS     O   Scratch area base address for order vector.
C
C$ Detailed_Input
C
C     NJRS,
C     UBASES         are, respectively, the number of join row sets in
C                    the input join row set union, and the base
C                    addresses of those join row sets.
C
C     NORDER         is the number of order-by columns used to define
C                    the order relation used for sorting.
C
C     OTABS          is an array of indices identifying the parent
C                    tables of the order-by columns.  These indices
C                    are the ordinal positions of the parent tables
C                    in the FROM clause of the query to which the
C                    input joint row set corresponds.
C
C     OCOLS          is an array of indices identifying the order-by
C                    columns.  These indices are the ordinal positions
C                    of the columns in their virtual parent tables.
C                    The order of columns in virtual tables is set
C                    when EKs are loaded by the routine EKLEF.  The
C                    Nth element of OCOLS applies to the Nth order-by
C                    column.
C
C     OELTS          is an array of element indices identifying the
C                    order-by column entry elements to use when making
C                    order comparisons.  These indices are ignored for
C                    scalar order-by columns, but must be set properly
C                    for vector-valued order-by columns.  For example,
C                    if an order-by column has size 5, one could make
C                    order comparisons using the third elements of
C                    entries in this column.  The Nth element of OELTS
C                    applies to the Nth order-by column.
C
C     SENSES         is an array of parameters indicating the ordering
C                    sense for each order-by column.  An ordering sense
C                    can be ascending (the default) or descending.  The
C                    values indicating these senses are EQASND and
C                    EQDSND respectively.  These parameters are defined
C                    in the include file ekquery.inc.  The Nth element
C                    of SENSES applies to the Nth order-by column.
C
C     STHAN          is an array of EK handles corresponding to loaded
C                    segments.  STHAN is expected to be the array of
C                    the same name maintained by EKQMGR.
C
C     STSDSC         is an array of descriptors of loaded segments.
C                    STSDSC is expected to be the array of the same name
C                    maintained by EKQMGR.
C
C     STDTPT         is an array of pointers that map segments to lists
C                    of column descriptors in the column descriptor
C                    pool.  The Nth element of STDTPT is the head node
C                    number for the column descriptor list of the Nth
C                    loaded segment.  The column descriptor list is
C                    indexed by the linked list pool DTPOOL.  STDTPT is
C                    expected to be the array of the same name
C                    maintained by EKQMGR.
C
C     DTPOOL         is a linked list pool used to index the column
C                    descriptor array DTDSCS.  DTPOOL is expected to be
C                    the array of the same name maintained by EKQMGR.
C
C     DTDSCS         is an array of column descriptors for each loaded
C                    column.  There is a separate descriptor for each
C                    column in each segment.  The Nth node of DTPOOL
C                    is considered to point to the Nth element of
C                    DTDSCS.  DTDSCS is expected to be the array of the
C                    same name maintained by EKQMGR.
C
C$ Detailed_Output
C
C     ORDBAS         is the scratch area base address of the order
C                    vector created by this routine.  This address is
C                    the predecessor of the first scratch area address
C                    occupied by the order vector.
C
C                    The order vector indicates the order of the row
C                    vectors of the input join row set union, where the
C                    order relation is defined by the order-by columns,
C                    column entry element indices, and order senses.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the number of order-by columns NORDER is non-positive,
C         the error SPICE(INVALIDCOUNT) is signalled.
C
C     2)  If an I/O error occurs while attempting to create an order
C         vector for the specified row set, the error will be diagnosed
C         by routines called by this routine.
C
C     3)  If the first order-by column descriptor in the list has
C         an invalid data type code, the error SPICE(INVALIDTYPE)
C         is signalled.
C$ Files
C
C     The input join row set is presumed to refer to EK files currently
C     loaded via EKLEF.
C
C$ Particulars
C
C     This routine writes to the EK scratch area an order vector for the
C     specified join row set union.  The order vector is written in
C     ascending order starting at the location following ORDBAS.  The
C     order relation is defined by the order-by columns, column entry
C     element indices, and order senses.
C
C$ Examples
C
C     See EKGC.
C
C$ Restrictions
C
C     1)  This routine modifies the EK scratch area, and therefore
C         should not be used by routines outside of the EK system.
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
C-    SPICELIB Version 2.1.0, 07-AUG-2006 (NJB)
C
C        Bug fix:  added intialization of variable PRVBAS to support
C                  operation under the Macintosh Intel Fortran
C                  compiler. Note that this bug did not affect
C                  operation of this routine on other platforms.
C
C-    SPICELIB Version 2.0.0, 09-SEP-2005 (NJB)
C
C        Increased buffer size parameter LIMIT1 from 25K to 250K.
C        Declared large buffers SAVED to prevent memory errors 
C        under CYGWIN.
C
C-    SPICELIB Version 1.1.0, 18-JUN-1999 (WLT)
C
C        Removed several redundant calls to CHKIN
C
C-    Beta Version 1.0.0, 19-OCT-1995 (NJB)
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 2.1.0, 07-AUG-2006 (NJB)
C
C        Bug fix:  added intialization of variable PRVBAS to support
C                  operation under the Macintosh Intel Fortran
C                  compiler. Note that this bug did not affect
C                  operation of this routine on other platforms. The
C                  statement referencing the uninitialized variable
C                  was:
C
C           IF (  ( I .EQ. 1 ) .OR. ( SGVBAS .NE. PRVBAS )  ) THEN
C       
C        In the previous version of the code, PRVBAS is uninitialized
C        when the loop counter I is 1.  PRVBAS *is* initialized when I
C        is greater than 1, so the logical value of the IF expression
C        is not affected by the lack of proper intialization.
C
C        However, the Intel Fortran compiler for the Mac flags a runtime
C        error when the above code is exercised.  So PRVBAS is now 
C        initialized prior to the above IF statement.
C
C
C-    SPICELIB Version 2.0.0, 08-SEP-2005 (NJB)
C
C        Increased buffer size parameter LIMIT1 from 25K to 250K.
C        Declared large buffers SAVED to prevent memory errors 
C        under CYGWIN.  The saved buffers are
C
C          CDAT
C          DDAT
C          IDAT
C          NF
C          ORDVEC
C
C-& 
 
C
C     SPICELIB functions
C
      INTEGER               LNKNXT
      LOGICAL               RETURN
 
C
C     Other functions
C
      LOGICAL               ZZEKVCMP
 
C
C     Other local parameters
C
      INTEGER               INISUB
      PARAMETER           ( INISUB = 32 )
 
      INTEGER               LIMIT1
      PARAMETER           ( LIMIT1 = 250000 )
 
C
C     Local variables
C
      CHARACTER*(INISUB)    C1
      CHARACTER*(INISUB)    C2
      CHARACTER*(INISUB)    CDAT   ( LIMIT1 )
      CHARACTER*1           NF     ( LIMIT1 )
 
      DOUBLE PRECISION      D1
      DOUBLE PRECISION      D2
      DOUBLE PRECISION      DDAT   ( LIMIT1 )
 
      INTEGER               ADDRJ
      INTEGER               ADDRJG
      INTEGER               CPRIME
      INTEGER               COLPTR
      INTEGER               CVLEN
      INTEGER               DTYPE
      INTEGER               ELTIDX
      INTEGER               GAP
      INTEGER               HANDLE
      INTEGER               I1
      INTEGER               I2
      INTEGER               I
      INTEGER               IDAT   ( LIMIT1 )
      INTEGER               J
      INTEGER               JG
      INTEGER               NR
      INTEGER               NRLOC
      INTEGER               NROWS
      INTEGER               NTAB
      INTEGER               ORDVEC ( LIMIT1 )
      INTEGER               PRVBAS
      INTEGER               ROW
      INTEGER               RJ
      INTEGER               RJG
      INTEGER               ROWVEC ( MAXTAB + 1 )
      INTEGER               RVECJ  ( MAXTAB + 1 )
      INTEGER               RVECJG ( MAXTAB + 1 )
      INTEGER               RVSIZE
      INTEGER               RWVBAS
      INTEGER               SEG
      INTEGER               SEGVEC ( MAXTAB )
      INTEGER               SGVBAS
      INTEGER               SVECJ  ( MAXTAB )
      INTEGER               SVECJG ( MAXTAB )
      INTEGER               SVSIZE
      INTEGER               TABLOC
      INTEGER               TPRIME
      INTEGER               UNIT
 
      LOGICAL               BRUTE
      LOGICAL               FOUND
      LOGICAL               JLE
      LOGICAL               N1
      LOGICAL               N2
      LOGICAL               NFJ
      LOGICAL               NFJG
      LOGICAL               NULL
      LOGICAL               TRUNC

C
C     Saved variables
C
C     The following variables are saved in order to prevent
C     memory errors under Cygwin and in shared object libraries
C     under various Unix systems.
C
      SAVE                  CDAT
      SAVE                  DDAT
      SAVE                  IDAT
      SAVE                  NF
      SAVE                  ORDVEC
 
C
C     Statement functions
C
      LOGICAL               INTEQ
      LOGICAL               DPEQ
      LOGICAL               CHREQ
      LOGICAL               INTGE
      LOGICAL               INTLE
      LOGICAL               DPGE
      LOGICAL               DPLE
      LOGICAL               CHRGE
      LOGICAL               CHRLE
 
C
C
C     The following functions test whether two column entries
C     are equal.  In the integer and d.p. cases, the test is conclusive.
C     In the character case, the test indicates whether the initial
C     substrings consisting of the first INISUB characters of each of
C     the two entries are equal.
C
 
      INTEQ ( N1, N2, I1, I2 ) =            ( N1   .AND.    N2  )  .OR.
     .                              (       (.NOT. (N1 .OR. N2) )
     .                                .AND. ( I1 .EQ. I2        )  )
 
      DPEQ  ( N1, N2, D1, D2 ) =            ( N1   .AND.    N2  )  .OR.
     .                              (       (.NOT. (N1 .OR. N2) )
     .                                .AND. ( D1 .EQ. D2        )  )
 
      CHREQ ( N1, N2, C1, C2 ) =            ( N1   .AND.    N2  )  .OR.
     .                              (       (.NOT. (N1 .OR. N2) )
     .                                .AND. ( C1 .EQ. C2        )  )
 
 
C
C     The following functions indicate whether the first of two column
C     entries is less than or equal to the second.  In the integer and
C     d.p. cases, the test is conclusive.  In the character case, the
C     test indicates whether the initial substring consisting of the
C     first INISUB characters of the first entry is less than or equal
C     to the corresponding initial substring of length INISUB of the
C     second entry.
C
      INTGE  ( N1, N2, I1, I2 ) =
     .
     .     N2  .OR.  (       ( .NOT. ( N1 .OR. N2 )  )
     .                 .AND. (         I1 .GE. I2    )    )
 
      INTLE  ( N1, N2, I1, I2 ) =
     .
     .     N1  .OR.  (       ( .NOT. ( N1 .OR. N2 )  )
     .                 .AND. (         I1 .LE. I2    )    )
 
 
 
      DPGE  ( N1, N2, D1, D2 ) =
     .
     .     N2  .OR.  (       ( .NOT. ( N1 .OR. N2 )  )
     .                 .AND. (         D1 .GE. D2    )    )
 
      DPLE  ( N1, N2, D1, D2 ) =
     .
     .     N1  .OR.  (       ( .NOT. ( N1 .OR. N2 )  )
     .                 .AND. (         D1 .LE. D2    )    )
 
 
      CHRGE ( N1, N2, C1, C2 ) =
     .
     .     N2  .OR.  (       ( .NOT. ( N1 .OR. N2 )  )
     .                 .AND. (         C1 .GE. C2    )    )
 
      CHRLE ( N1, N2, C1, C2 ) =
     .
     .     N1  .OR.  (       ( .NOT. ( N1 .OR. N2 )  )
     .                 .AND. (         C1 .LE. C2    )    )
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKJSRT' )
      END IF
 
C
C     If there are no order-by columns, that's an error.
C
      IF ( NORDER .LT. 1 ) THEN
 
         CALL SETMSG ( 'Number of order-by columns must be '      //
     .                 'positive but was #.'                       )
         CALL ERRINT ( '#',  NORDER                                )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                       )
         CALL CHKOUT ( 'ZZEKJSRT'                                  )
         RETURN
 
      END IF
 
C
C     We split the sorting job up into two cases:
C
C        1)  If the number of rows to be sorted is not too large,
C            we can gain speed by reading data from the primary
C            order-by column into memory and sorting the row number
C            array in memory.
C
C        2)  If there's too much data for option (1) to handle,
C            we just read data from the order-by columns as needed.
C            This algorithm is simple, but very slow, since many
C            DAS reads of individual column entries are required.
C
C
C     Find out how many rows are in the join row set union.
C
      NROWS = 0
 
      DO I = 1, NJRS
 
         NRLOC  =  UBASES(I) + JRCIDX
 
         CALL ZZEKSRD ( NRLOC, NRLOC, NR )
 
         NROWS  =  NROWS     + NR
 
      END DO
 
C
C     Get the number of tables in the cartesian product represented
C     by the join row set union.  The number of tables in the first
C     join row set suffices.
C
      TABLOC = UBASES(1) + JTCIDX
 
      CALL ZZEKSRD ( TABLOC, TABLOC, NTAB )
 
      SVSIZE = NTAB
      RVSIZE = NTAB + 1
 
C
C     We can get the data types of the order-by columns from the
C     segment vector of the first row vector in the first join row set.
C     Initialize addressing in the join row set union so we can look up
C     the locations of these vectors.
C
      CALL ZZEKVSET ( NJRS,      UBASES                 )
      CALL ZZEKVCAL ( 1,         RWVBAS,         SGVBAS )
      CALL ZZEKSRD  ( SGVBAS+1,  SGVBAS+SVSIZE,  SEGVEC )
 
      TPRIME  =  OTABS(1)
      CPRIME  =  OCOLS(1)
      SEG     =  SEGVEC( TPRIME )
      COLPTR  =  STDTPT( SEG    )
 
      DO I = 2, CPRIME
         COLPTR = LNKNXT( COLPTR, DTPOOL )
      END DO
 
      DTYPE = DTDSCS( TYPIDX, COLPTR )
 
 
      IF ( NROWS .LE. LIMIT1 ) THEN
C
C        Case 1.
C
C        We have a small enough quantity of data that we may be able
C        to speed up sorting by using memory.  Here's the plan:
C
C        We'll read data for the primary order-by column into memory.
C        The `primary' column is the one whose index appears first
C        in the input list of column indices.  We'll also maintain a
C        null flag array for the primary column.  If we can figure out
C        the order relation between two rows by looking at entries in
C        the primary order-by column, fine.  Otherwise, we let ZZEKVCMP
C        perform the comparison.
C
C        We'll sort the set of row vector numbers of the matching rows
C        in parallel with our data sort.
C
C        Character columns present a special case:  their string length
C        can get pretty big, and it could take a lot of memory to store
C        their column entries.  We compromise here:  we store only the
C        first INISUB chararacters of each character column entry.  If
C        we can't decide the order of two strings based on these initial
C        substrings, we let ZZEKVCMP handle the matter.
C
C        Read the primary column data.  Keep track of whether we've
C        truncated any strings.
C
         TRUNC  = .FALSE.
         PRVBAS = -1

         DO I = 1, NROWS
 
 
            CALL ZZEKVCAL ( I, RWVBAS, SGVBAS )
 
 
            IF (  ( I .EQ. 1 ) .OR. ( SGVBAS .NE. PRVBAS )  ) THEN
 
               CALL ZZEKSRD  ( SGVBAS+1,  SGVBAS+SVSIZE,  SEGVEC )
 
               SEG     =  SEGVEC( TPRIME )
               HANDLE  =  STHAN ( SEG    )
               COLPTR  =  STDTPT( SEG    )
 
               DO J = 2, CPRIME
                  COLPTR = LNKNXT( COLPTR, DTPOOL )
               END DO
 
            END IF
 
 
            CALL ZZEKSRD  ( RWVBAS+1,  RWVBAS+RVSIZE,  ROWVEC )
 
            ROW     =  ROWVEC( TPRIME )
            ELTIDX  =  OELTS ( CPRIME )
 
 
            IF ( DTYPE .EQ. CHR ) THEN
 
               CALL ZZEKRSC ( HANDLE,
     .                        STSDSC(1,SEG),
     .                        DTDSCS(1,COLPTR),
     .                        ROW,
     .                        ELTIDX,
     .                        CVLEN,
     .                        CDAT(I),
     .                        NULL,
     .                        FOUND            )
 
 
               IF ( .NOT. FOUND ) THEN
 
                  CALL DASHLU (  HANDLE, UNIT )
 
                  CALL SETMSG ( 'EK = #; SEG = #; ROW = #; COLIDX ' //
     .                          '= #; ELT = #; column entry elt '   //
     .                          'was not found.'                    )
                  CALL ERRFNM ( '#',  UNIT                          )
                  CALL ERRINT ( '#',  SEG                           )
                  CALL ERRINT ( '#',  ROW                           )
                  CALL ERRINT ( '#',  DTDSCS(ORDIDX,COLPTR)         )
                  CALL ERRINT ( '#',  ELTIDX                        )
                  CALL SIGERR ( 'SPICE(BUG)'                        )
                  CALL CHKOUT ( 'ZZEKJSRT'                          )
                  RETURN
 
               END IF
 
               TRUNC = TRUNC .OR. ( CVLEN  .GT.  INISUB )
 
 
            ELSE IF (  ( DTYPE .EQ. DP ) .OR. ( DTYPE .EQ. TIME ) ) THEN
 
               CALL ZZEKRSD ( HANDLE,
     .                        STSDSC(1,SEG),
     .                        DTDSCS(1,COLPTR),
     .                        ROW,
     .                        ELTIDX,
     .                        DDAT(I),
     .                        NULL,
     .                        FOUND              )
 
 
               IF ( .NOT. FOUND ) THEN
 
                  CALL DASHLU (  HANDLE, UNIT )
 
                  CALL SETMSG ( 'EK = #; SEG = #; ROW = #; COLIDX ' //
     .                          '= #; ELT = #; column entry elt '   //
     .                          'was not found.'                    )
                  CALL ERRFNM ( '#',  UNIT                          )
                  CALL ERRINT ( '#',  SEG                           )
                  CALL ERRINT ( '#',  ROW                           )
                  CALL ERRINT ( '#',  DTDSCS(ORDIDX,COLPTR)         )
                  CALL ERRINT ( '#',  ELTIDX                        )
                  CALL SIGERR ( 'SPICE(BUG)'                        )
                  CALL CHKOUT ( 'ZZEKJSRT'                          )
                  RETURN
 
               END IF
 
 
 
            ELSE IF (  DTYPE .EQ. INT ) THEN
 
               CALL ZZEKRSI ( HANDLE,
     .                        STSDSC(1,SEG),
     .                        DTDSCS(1,COLPTR),
     .                        ROW,
     .                        ELTIDX,
     .                        IDAT(I),
     .                        NULL,
     .                        FOUND             )
 
 
               IF ( .NOT. FOUND ) THEN
 
                  CALL DASHLU (  HANDLE, UNIT )
 
                  CALL SETMSG ( 'EK = #; SEG = #; ROW = #; COLIDX ' //
     .                          '= #; ELT = #; column entry elt '   //
     .                          'was not found.'                    )
                  CALL ERRFNM ( '#',  UNIT                          )
                  CALL ERRINT ( '#',  SEG                           )
                  CALL ERRINT ( '#',  ROW                           )
                  CALL ERRINT ( '#',  DTDSCS(ORDIDX,COLPTR)         )
                  CALL ERRINT ( '#',  ELTIDX                        )
                  CALL SIGERR ( 'SPICE(BUG)'                        )
                  CALL CHKOUT ( 'ZZEKJSRT'                          )
                  RETURN
 
               END IF
 
 
            ELSE
C
C              We must have a bogus column descriptor.
C
               CALL SETMSG ( 'Unrecognized data type # for first ' //
     .                       'column.'                              )
               CALL ERRINT ( '#', DTYPE                             )
               CALL SIGERR ( 'SPICE(INVALIDTYPE)'                   )
               CALL CHKOUT ( 'ZZEKJSRT'                             )
               RETURN
 
            END IF
 
C
C           Set the character null flag for the current column entry.
C
            IF ( NULL ) THEN
               NF(I)  =  CTRUE
            ELSE
               NF(I)  =  CFALSE
            END IF
 
 
            PRVBAS  =  SGVBAS
 
         END DO
 
C
C        Initialize the order vector.
C
         DO I = 1, NROWS
            ORDVEC(I) = I
         END DO
 
C
C        At this point, we've read in the data for the primary order-by
C        column, and also have set the null flag array for the column.
C        We're ready to proceed with our sort.
C
         GAP = NROWS / 2
 
         DO WHILE ( GAP .GT. 0 )
 
            DO I = GAP+1, NROWS
 
               J = I - GAP
 
               DO WHILE ( J .GT. 0 )
 
                  JG = J + GAP
C
C                 Compare the Jth and JGth rows of the row set.  The
C                 logical JLE is TRUE when the Jth element is less than
C                 or equal to the JGth.  If the Jth and JGth elements
C                 compare equal, and there is more than one order-by
C                 column or if we've truncated string data, we'll have
C                 to go on and make a conclusive test.  Otherwise, we
C                 can set JLE based on the data we've read.
C
C                 Set the data array indices of the Jth and JGth
C                 elements, as indicated by the order vector.
C
                  RJ    =   ORDVEC(J)
                  RJG   =   ORDVEC(JG)
 
                  NFJ   =   NF(RJ )  .EQ. CTRUE
                  NFJG  =   NF(RJG)  .EQ. CTRUE
 
C
C                 Start out hoping for the best:  that we won't have
C                 to do a brute-force comparison.
C
                  BRUTE = .FALSE.
 
 
                  IF ( DTYPE .EQ. INT ) THEN
 
                     IF ( NORDER .EQ. 1 ) THEN
C
C                       We can make a decision based on the data in
C                       memory.
C
                        IF ( SENSES(1) .EQ. EQASND ) THEN
 
                           JLE = INTLE( NFJ, NFJG, IDAT(RJ), IDAT(RJG) )
                        ELSE
                           JLE = INTGE( NFJ, NFJG, IDAT(RJ), IDAT(RJG) )
                        END IF
 
 
                     ELSE IF ( .NOT. INTEQ ( NFJ,      NFJG,
     .                                       IDAT(RJ), IDAT(RJG) ) )THEN
C
C                       If the items we're comparing are unequal, we can
C                       still make a decision.
C
                        IF ( SENSES(1) .EQ. EQASND ) THEN
 
                           JLE = INTLE( NFJ, NFJG, IDAT(RJ), IDAT(RJG) )
                        ELSE
                           JLE = INTGE( NFJ, NFJG, IDAT(RJ), IDAT(RJG) )
                        END IF
 
 
                     ELSE
C
C                       Otherwise, we'll have to look at values in the
C                       other order-by columns.  Get the segment and
C                       row vectors to be compared.
C
                        BRUTE  =  .TRUE.
 
                     END IF
 
 
 
                  ELSE IF (     ( DTYPE .EQ. DP   )
     .                     .OR. ( DTYPE .EQ. TIME )  ) THEN
 
C
C                    The D.P. case parallels the integer case.
C
                     IF ( NORDER .EQ. 1 ) THEN
 
                        IF ( SENSES(1) .EQ. EQASND ) THEN
 
                           JLE = DPLE( NFJ, NFJG, DDAT(RJ), DDAT(RJG) )
                        ELSE
                           JLE = DPGE( NFJ, NFJG, DDAT(RJ), DDAT(RJG) )
                        END IF
 
 
                     ELSE IF ( .NOT. DPEQ ( NFJ,      NFJG,
     .                                      DDAT(RJ), DDAT(RJG) )  )THEN
 
                        IF ( SENSES(1) .EQ. EQASND ) THEN
 
                           JLE = DPLE( NFJ, NFJG, DDAT(RJ), DDAT(RJG) )
                        ELSE
                           JLE = DPGE( NFJ, NFJG, DDAT(RJ), DDAT(RJG) )
                        END IF
 
 
                     ELSE
C
C                       Otherwise, we'll have to look at values in the
C                       other order-by columns.  Get the segment and
C                       row vectors to be compared.
C
                        BRUTE  =  .TRUE.
 
                     END IF
 
 
                  ELSE
 
C
C                    In the character case where there is one order-by
C                    column, equality is a problem unless no truncation
C                    occurred.
C
                     IF ( ( NORDER .EQ. 1 ) .AND. ( .NOT. TRUNC ) ) THEN
 
                        IF ( SENSES(1) .EQ. EQASND ) THEN
 
                           JLE = CHRLE( NFJ, NFJG, CDAT(RJ), CDAT(RJG) )
                        ELSE
                           JLE = CHRGE( NFJ, NFJG, CDAT(RJ), CDAT(RJG) )
                        END IF
 
 
                     ELSE IF ( .NOT. CHREQ ( NFJ,      NFJG,
     .                                       CDAT(RJ), CDAT(RJG) ) )THEN
C
C                       If the items we're comparing are unequal, we can
C                       still make a decision.
C
                        IF ( SENSES(1) .EQ. EQASND ) THEN
 
                           JLE = CHRLE( NFJ, NFJG, CDAT(RJ), CDAT(RJG) )
                        ELSE
                           JLE = CHRGE( NFJ, NFJG, CDAT(RJ), CDAT(RJG) )
                        END IF
 
 
                     ELSE
C
C                       Otherwise, we'll have to look at values in the
C                       other order-by columns.  Get the segment and
C                       row vectors to be compared.
C
                        BRUTE  =  .TRUE.
 
                     END IF
 
 
                  END IF
 
 
                  IF ( BRUTE ) THEN
 
                     CALL ZZEKVCAL  ( RJ,       RWVBAS,        SGVBAS )
                     CALL ZZEKSRD   ( SGVBAS+1, SGVBAS+SVSIZE, SVECJ  )
                     CALL ZZEKSRD   ( RWVBAS+1, RWVBAS+RVSIZE, RVECJ  )
 
                     CALL ZZEKVCAL  ( RJG,      RWVBAS,        SGVBAS )
                     CALL ZZEKSRD   ( SGVBAS+1, SGVBAS+SVSIZE, SVECJG )
                     CALL ZZEKSRD   ( RWVBAS+1, RWVBAS+RVSIZE, RVECJG )
 
                     JLE = ZZEKVCMP ( LE,       NORDER,        OTABS,
     .                                OCOLS,    OELTS,         SENSES,
     .                                STHAN,    STSDSC,        STDTPT,
     .                                DTPOOL,   DTDSCS,        SVECJ,
     .                                RVECJ,    SVECJG,        RVECJG )
                  END IF
 
C
C                 At this point, JLE is set.
C
                  IF ( JLE ) THEN
 
                     J = 0
 
                  ELSE
C
C                    Swap the Jth and JGth elements of the order vector.
C
                     CALL SWAPI ( ORDVEC(J), ORDVEC(JG) )
 
                  END IF
 
                  J = J - GAP
 
               END DO
 
            END DO
 
C
C           The following division guarantees loop termination, even
C           if a DAS error occurs.
C
            GAP = GAP / 2
 
         END DO
 
C
C        We've sorted the row numbers in Case 1.  Push the order vector
C        onto the scratch area stack.
C
         CALL ZZEKSTOP ( ORDBAS )
 
         CALL ZZEKSPSH ( NROWS, ORDVEC )
 
 
      ELSE
 
C
C        Case 2.
C
C        Well, we really have a lot of data.  Don't try to read it into
C        memory.  Build the order vector in the scratch area.
C
         CALL ZZEKSTOP ( ORDBAS )
 
         DO I = 1, NROWS
            CALL ZZEKSPSH ( 1, I )
         END DO
 
C
C        Re-order the order vector elements to reflect the order of the
C        corresponding rows. This uses the Shell Sort algorithm, but
C        swaps the elements of the order vector instead of the rows
C        themselves.
C
         GAP = NROWS / 2
 
         DO WHILE ( GAP .GT. 0 )
 
            DO I = GAP+1, NROWS
 
               J = I - GAP
 
               DO WHILE ( J .GT. 0 )
 
                  JG = J + GAP
C
C                 Set the indices of the Jth and JGth
C                 row vectors, as indicated by the order vector.
C
                  CALL ZZEKSRD ( ORDBAS+J,  ORDBAS+J,  RJ  )
                  CALL ZZEKSRD ( ORDBAS+JG, ORDBAS+JG, RJG )
 
C
C                 Compare the two row vectors.
C
                  CALL ZZEKVCAL  ( RJ,       RWVBAS,        SGVBAS )
                  CALL ZZEKSRD   ( SGVBAS+1, SGVBAS+SVSIZE, SVECJ  )
                  CALL ZZEKSRD   ( RWVBAS+1, RWVBAS+RVSIZE, RVECJ  )
 
                  CALL ZZEKVCAL  ( RJG,      RWVBAS,        SGVBAS )
                  CALL ZZEKSRD   ( SGVBAS+1, SGVBAS+SVSIZE, SVECJG )
                  CALL ZZEKSRD   ( RWVBAS+1, RWVBAS+RVSIZE, RVECJG )
 
                  IF (  ZZEKVCMP ( LE,       NORDER,   OTABS,
     .                             OCOLS,    OELTS,    SENSES,
     .                             STHAN,    STSDSC,   STDTPT,
     .                             DTPOOL,   DTDSCS,   SVECJ,
     .                             RVECJ,    SVECJG,   RVECJG )  ) THEN
                     J = 0
 
                  ELSE
C
C                    Swap the order vectors's Jth and JGth elements.
C
                     ADDRJ  =  ORDBAS+J
                     ADDRJG =  ORDBAS+JG
 
                     CALL ZZEKSUPD ( ADDRJ,  ADDRJ,  RJG )
                     CALL ZZEKSUPD ( ADDRJG, ADDRJG, RJ  )
 
                  END IF
 
                  J = J - GAP
 
               END DO
 
            END DO
 
C
C           The following division guarantees loop termination, even
C           if a DAS error occurs.
C
            GAP = GAP / 2
 
         END DO
C
C        We've sorted the row numbers for case (2).
C
 
      END IF
C
C     We've sorted the row numbers, no matter how many there were.
C
 
      CALL CHKOUT ( 'ZZEKJSRT' )
      RETURN
      END
