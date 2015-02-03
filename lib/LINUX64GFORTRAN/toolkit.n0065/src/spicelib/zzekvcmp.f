C$Procedure      ZZEKVCMP ( EK, row vector comparison )
 
      LOGICAL FUNCTION ZZEKVCMP ( OP,      NCOLS,   TABS,    COLS,
     .                            ELTS,    SENSES,  STHAN,   STSDSC,
     .                            STDTPT,  DTPOOL,  DTDSCS,  SGVEC1,
     .                            RWVEC1,  SGVEC2,  RWVEC2           )
 
C$ Abstract
C
C     Compare two row vectors, using dictionary ordering on a
C     specified list of columns as the order relation.
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
 
 
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekopcd.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'ekquery.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL =  -5 )
 
      INTEGER               OP
      INTEGER               NCOLS
      INTEGER               TABS   ( * )
      INTEGER               COLS   ( * )
      INTEGER               ELTS   ( * )
      INTEGER               SENSES ( * )
      INTEGER               STHAN  ( * )
      INTEGER               STSDSC ( SDSCSZ, * )
      INTEGER               STDTPT ( * )
      INTEGER               DTPOOL ( 2, LBPOOL : * )
      INTEGER               DTDSCS ( CDSCSZ, * )
      INTEGER               SGVEC1 ( * )
      INTEGER               RWVEC1 ( * )
      INTEGER               SGVEC2 ( * )
      INTEGER               RWVEC2 ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     OP         I   Code for relational operator.
C     NCOLS      I   Number of columns used to define order relation.
C     TABS       I   Indices of tables containing order-by columns.
C     COLS       I   Indices of order-by columns within parent tables.
C     ELTS       I   Element indices.
C     SENSES     I   Order senses.
C     STHAN      I   Array of handles of loaded EKs.
C     STSDSC     I   Array of segment descriptors.
C     STDTPT     I   Array of pointers to column descriptors.
C     DTPOOL     I   Column descriptor index pool.
C     DTDSCS     I   Array of column descriptors.
C     SGVEC1     I   First segment vector.
C     RWVEC1     I   First row vector.
C     SGVEC2     I   Second segment vector.
C     RWVEC2     I   Second row vector.
C
C     The function returns .TRUE. if and only if the two rows
C     satisfy the order relation specified by the input arguments.
C
C$ Detailed_Input
C
C     OP             is an integer code representing a binary operator
C                    that expresses an order relation.  The allowed
C                    values of OP are the parameters
C
C                       EQ
C                       GE
C                       GT
C                       LE
C                       LT
C                       NE
C
C                    This routine test whether the input rows satisfy
C                    the order relation
C
C                       <row1> OP <row2>
C
C
C     NCOLS          is the number of columns used to define a
C                    dictionary ordering.
C
C     TABS           is an array of indices identifying the parent
C                    tables of the order-by columns.  These indices
C                    are the ordinal positions of the parent tables
C                    in the FROM clause of the query to which the
C                    input joint row set corresponds.
C
C     COLS           is an array of indices identifying the order-by
C                    columns.  These indices are the ordinal positions
C                    of the columns in their virtual parent tables.
C                    The order of columns in virtual tables is set
C                    when EKs are loaded by the routine EKLEF.  The
C                    Nth element of COLS applies to the Nth order-by
C                    column.
C
C     ELTS           is an array of element indices identifying the
C                    order-by column entry elements to use when making
C                    order comparisons.  These indices are ignored for
C                    scalar order-by columns, but must be set properly
C                    for vector-valued order-by columns.  For example,
C                    if an order-by column has size 5, one could make
C                    order comparisons using the third elements of
C                    entries in this column.  The Nth element of ELTS
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
C     SEGVC1,
C     ROWVC1         are, respectively, a segment vector and a row
C                    vector that define the first row to be compared.
C                    The segment vector qualifies the row vector.
C
C     SEGVC2,
C     ROWVC2         are, respectively, a segment vector and a row
C                    vector that define the second row to be compared.
C
C$ Detailed_Output
C
C     The function returns .TRUE. if and only if the two row vectors
C     satisfy the order relation specified by the input arguments:
C
C        <row 1> OP <row 2>
C
C$ Parameters
C
C     Within the EK system, relational operators used in EK queries are
C     represented by integer codes.  The codes and their meanings are
C     listed below.
C
C     Relational expressions in EK queries have the form
C
C        <column name> <operator> <value>
C
C     For columns containing numeric values, the operators
C
C        EQ,  GE,  GT,  LE,  LT,  NE
C
C     may be used; these operators have the same meanings as their
C     Fortran counterparts.  In the character case, the same operators
C     may be used; the meanings of the parameters
C
C        GE,  GT,  LE,  LT
C
C     match those of the Fortran lexical functions
C
C        LGE, LGT, LLE, LLT
C
C     Null values are considered to precede all non-null values.
C
C$ Exceptions
C
C     1)  If the either of input file handles is invalid, the error
C         will be diagnosed by routines called by this routine.
C         The function value is .FALSE. in this case.
C
C     2)  If an I/O error occurs while attempting to find the address
C         range of the specified column entry element, the error will
C         be diagnosed by routines called by this routine.  The
C         function value is .FALSE. in this case.
C
C     3)  If any of the input segment descriptors, column descriptors,
C         or row numbers are invalid, this routine may fail in
C         unpredictable, but possibly spectacular, ways.  Except
C         as described in this header section, no attempt is made to
C         handle these errors.
C
C     4)  If the data type code in the input column descriptor is not
C         recognized, the error SPICE(INVALIDDATATYPE) is signalled.
C         The function value is .FALSE. in this case.
C
C     5)  If the relational operator code OP is not recognized, the
C         error SPICE(UNNATURALRELATION) is signalled.
C         The function value is .FALSE. in this case.
C
C$ Files
C
C     This routine indirectly references EK files loaded via EKLEF.
C
C$ Particulars
C
C     This routine is an EK utility intended to centralize a frequently
C     performed comparison operation.
C
C$ Examples
C
C     See ZZEKJSRT.
C
C$ Restrictions
C
C     1)  This routine must execute quickly.  Therefore, it checks in
C         only if it detects an error.  If an error is signalled by a
C         routine called by this routine, this routine will not appear
C         in the SPICELIB traceback display.  Also, in the interest
C         of speed, this routine does not test the value of the SPICELIB
C         function RETURN upon entry.
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
C-    Beta Version 1.0.0, 19-OCT-1995 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               LNKNXT
 
C
C     Non-SPICELIB functions
C
      INTEGER               ZZEKECMP
 
C
C     Local variables
C
      INTEGER               CLDSCS ( CDSCSZ, 2 )
      INTEGER               COL
      INTEGER               COLIDX
      INTEGER               COLPTR ( 2 )
      INTEGER               DTYPE  ( 2 )
      INTEGER               ELIDXS ( 2 )
      INTEGER               HANS   ( 2 )
      INTEGER               I
      INTEGER               J
      INTEGER               REL
      INTEGER               ROWS   ( 2 )
      INTEGER               SEGS   ( 2 )
      INTEGER               SGDSCS ( SDSCSZ, 2 )
      INTEGER               TABIDX
 
 
C
C     Use discovery check-in for speed.
C
 
C
C     The function value defaults to .FALSE.
C
      ZZEKVCMP = .FALSE.
 
C
C     The input column descriptors identify the columns to be used
C     to define an order relation on the input rows.  The order
C     relation is `dictionary' ordering:  if the elements of the
C     first n columns of both rows are equal, the corresponding
C     elements in the (n+1)st columns are compared to attempt to
C     break the tie.
C
C     The first step is to determine the relation that holds between
C     the rows.  We start out assuming we have equality.
C
      REL  =  EQ
      COL  =  1
 
      DO WHILE (  ( COL .LE. NCOLS ) .AND. ( REL .EQ. EQ )  )
C
C        Compare the entries in the two rows in the columns indicated
C        by the Nth column descriptor pair.
C
         TABIDX   =  TABS(COL)
         COLIDX   =  COLS(COL)
 
         SEGS(1)  =  SGVEC1( TABIDX )
         SEGS(2)  =  SGVEC2( TABIDX )
         ROWS(1)  =  RWVEC1( TABIDX )
         ROWS(2)  =  RWVEC2( TABIDX )
 
C
C        Identify the handles, segment descriptors, and column
C        descriptors we'll use to apply the constraint having index
C        COL.
C
         DO I = 1, 2
 
            HANS(I)    =  STHAN ( SEGS(I) )
            COLPTR(I)  =  STDTPT( SEGS(I) )
 
            DO J = 2, COLIDX
               COLPTR(I) = LNKNXT( COLPTR(I), DTPOOL )
            END DO
 
            CALL MOVEI ( DTDSCS( 1, COLPTR(I) ),  CDSCSZ,  CLDSCS(1,I) )
            CALL MOVEI ( STSDSC( 1, SEGS(I)   ),  SDSCSZ,  SGDSCS(1,I) )
 
            DTYPE  (I)  =  DTDSCS ( TYPIDX, COLPTR(I) )
            ELIDXS (I)  =  ELTS   ( COL )
 
         END DO
 
 
         IF ( DTYPE(1) .EQ. DTYPE(2) ) THEN
C
C           Find the order of the rows according to the order-by
C           column having index COL.  If the order sense for this
C           column is descending, adjust REL to reflect this.
C
            REL  =  ZZEKECMP ( HANS, SGDSCS, CLDSCS, ROWS, ELIDXS )
 
            IF ( SENSES(COL) .EQ. EQDSND ) THEN
 
               IF ( REL .EQ. LT ) THEN
                  REL = GT
               ELSE IF ( REL .EQ. GT ) THEN
                  REL = LT
               END IF
 
            END IF
 
 
         ELSE
 
            CALL CHKIN  ( 'ZZEKVCMP'                            )
            CALL SETMSG ( 'Data type mismatch for order-by '    //
     .                    'column having index #; type for '    //
     .                    'segment # = #; type for segment '    //
     .                       '# is #'                           )
            CALL ERRINT ( '#', COL                              )
            CALL ERRINT ( '#', SEGS(1)                          )
            CALL ERRINT ( '#', DTYPE (1)                        )
            CALL ERRINT ( '#', SEGS(2)                          )
            CALL ERRINT ( '#', DTYPE (2)                        )
            CALL SIGERR ( 'SPICE(BUG)'                          )
            CALL CHKOUT ( 'ZZEKVCMP'                            )
            RETURN
 
         END IF
 
 
         COL  =  COL + 1
 
      END DO
 
 
C
C     Determine the truth of the input relational expression.
C
      IF ( OP .EQ. EQ )   THEN
 
         ZZEKVCMP  =  REL .EQ. EQ
 
 
      ELSE IF ( OP .EQ. LT )   THEN
 
         ZZEKVCMP  =  REL .EQ. LT
 
 
      ELSE IF ( OP .EQ. LE )   THEN
 
         ZZEKVCMP  =  REL .NE. GT
 
 
      ELSE IF ( OP .EQ. GT )   THEN
 
         ZZEKVCMP  =  REL .EQ. GT
 
 
      ELSE IF ( OP .EQ. GE )   THEN
 
         ZZEKVCMP  =  REL .NE. LT
 
 
      ELSE IF ( OP .EQ. NE )   THEN
 
         ZZEKVCMP  =  REL .NE. EQ
 
 
      ELSE
C
C        Sorry, we couldn't resist.
C
         ZZEKVCMP  =  .FALSE.
 
         CALL CHKIN  ( 'ZZEKVCMP'                                      )
         CALL SETMSG ( 'The relational operator # was not recognized.' )
         CALL ERRINT ( '#',  OP                                        )
         CALL SIGERR ( 'SPICE(UNNATURALRELATION)'                      )
         CALL CHKOUT ( 'ZZEKVCMP'                                      )
         RETURN
 
      END IF
 
 
      RETURN
      END
