C$Procedure  ZZEKKEY  ( EK, determine key column )
 
      SUBROUTINE ZZEKKEY ( HANDLE,  SEGDSC,  NROWS,   NCNSTR,  CLIDXS,
     .                     DSCLST,  OPS,     DTYPES,  CHRBUF,  CBEGS,
     .                     CENDS,   DVALS,   IVALS,   ACTIVE,  KEY,
     .                     KEYDSC,  BEGIDX,  ENDIDX,  FOUND          )
 
C$ Abstract
C
C     Determine the key column to use when searching an EK segment
C     for rows matching query constraints.
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
      INCLUDE 'ekopcd.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               NROWS
      INTEGER               NCNSTR
      INTEGER               CLIDXS ( * )
      INTEGER               DSCLST ( CDSCSZ, * )
      INTEGER               OPS    ( * )
      INTEGER               DTYPES ( * )
      CHARACTER*(*)         CHRBUF
      INTEGER               CBEGS  ( * )
      INTEGER               CENDS  ( * )
      DOUBLE PRECISION      DVALS  ( * )
      INTEGER               IVALS  ( * )
      LOGICAL               ACTIVE ( * )
      INTEGER               KEY
      INTEGER               KEYDSC ( * )
      INTEGER               BEGIDX
      INTEGER               ENDIDX
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of EK file containing segment.
C     SEGDSC     I   Segment descriptor.
C     NROWS      I   Number of rows in segment.
C     NCNSTR     I   Number of relational constraints in query.
C     CLIDXS     I   Column attribute table indices for columns.
C     DSCLST     I   Array of column descriptors for constraints.
C     OPS        I   Operations used in query constraints.
C     DTYPES     I   Data types of scalar values used in constraints.
C     CHRBUF     I   Buffer containting query tokens.
C     CBEGS      I   Begin indices of character query tokens.
C     CENDS      I   End indices of character query tokens.
C     DVALS      I   D.p. values used in query constraints.
C     IVALS      I   Integer values used in query constraints.
C     ACTIVE    I-O  Array of flags indicating applicable constraints.
C     KEY        O   Index of key column.
C     KEYDSC     O   Descriptor of key column.
C     BEGIDX     O   Begin index of candidate row set.
C     ENDIDX     O   End index of candidate row set.
C     FOUND      O   Flag indicating whether a key column was found.
C     MAXCON     P   Maximum number of constraints allowed in query.
C     CDSCSZ     P   Column descriptor size.
C
C$ Detailed_Input
C
C     HANDLE         is the handle of the EK file containing the
C                    segment currently being searched for matching rows.
C
C     SEGDSC         the descriptor of the segment.
C
C     NROWS          is the number of rows in the segment designated by
C                    HANDLE and SEGDSC.
C
C     NCNSTR         is the number of input relational constraints.
C                    The input arrays CLIDXS, DSCLST, OPS, CHRBUF,
C                    CBEGS, CENDS, DVALS, and IVALS define these
C                    constraints.  Not all of the constraints may
C                    be applicable; the applicable constraints are
C                    identified by the input argument ACTIVE, which
C                    is described below.  Each *applicable* constraint
C                    has the form
C
C                       <table1.column1> <op> <value>
C
C     CLIDXS         is an array of column indices; the Nth index
C                    identifies the column on the left hand side of the
C                    Nth constraint.  Each index indicates the ordinal
C                    position of the attribute information for the
C                    corresponding column within the column attribute
C                    list for the column's parent table.  See the
C                    local variable declarations in EKBSR for further
C                    information on the column attribute list.
C
C     DSCLST         is an array of column descriptors for the columns
C                    referenced in the input constraints.  The Ith
C                    descriptor corresponds to the Ith constraint.
C
C
C     OPS            are relational operators used in the input
C                    constraints.  The elements of OPS are any of the
C                    integer parameters
C
C                       EQ, GE, GT, LE, LT, NE, LIKE, UNLIKE
C
C                    The Nth element of OPS corresponds to the Nth
C                    constraint.
C
C     DTYPES         is an array of data type codes for the values on
C                    the right hand sides of the input constraints.
C                    The Ith element of DTYPES applies to the Ith
C                    constraint.
C
C     CHRBUF,
C     CBEGS,
C     CENDS          are, respectively, a string containing character
C                    tokens representing values on the right hand sides
C                    of query constraints, and arrays of begin and end
C                    indices of these tokens within CHRBUF.  If the Nth
C                    constraint has a character value on the right hand
C                    side, that value is CHRBUF( CBEGS(N) : CENDS(N) ).
C                    For constraints whose right hand sides do not
C                    specify character values, the corresponding
C                    elements of CBEGS and CENDS are not used.
C
C     DVALS,
C     IVALS          are, respectively, arrays of double precision and
C                    integer values appearing on the right hand sides of
C                    input constraints.  The contents of DVALS and IVALS
C                    are meaningful only for those constraints whose
C                    right hand sides specify values having these data
C                    types.
C
C     ACTIVE         is an array of logical flags indicating which
C                    constraints are currently applicable.  The Nth
C                    element of ACTIVE indicates whether or not to apply
C                    the Nth constraint:  if ACTIVE(N) is .TRUE., the
C                    constraint is applicable, otherwise it isn't.
C
C                    The elements of the other input arguments that
C                    define constraints are defined when the
C                    corresponding element of ACTIVE is .TRUE.  For
C                    example, when the second constraint is not active,
C                    the second column descriptor in DSCLST may not be
C                    defined.
C
C                    Only constraints relating column entries to literal
C                    values may be active.
C
C$ Detailed_Output
C
C     ACTIVE         indicates, on output, which constraints are still
C                    active.  All constraints satisfied by the candidate
C                    row set are turned off on output.
C
C     KEY            is the index of the key column.  This index is
C                    taken from the input argument CLIDXS.
C
C     KEYDSC         is the column descriptor for the key column.  Note
C                    that this descriptor indicates whether the key
C                    column is indexed.
C
C     BEGIDX,
C     ENDIDX         are, respectively, begin and end indices for the
C                    candidate matching rows in the segment being
C                    searched.  These indices refer to positions in the
C                    key column's index:  the candidate rows are pointed
C                    to index elements having indices ranging from
C                    BEGIDX to ENDIDX, inclusively.  The actual
C                    candidate rows are referred to with one level of
C                    indirection.
C
C                    If the constraints on the key column entirely
C                    eliminate all rows in the segment, the returned
C                    values of BEGIDX and ENDIDX are, respectively, 1
C                    and 0.
C
C     FOUND          is a logical flag indicating whether a key column
C                    was determined.  The other outputs of this routine
C                    are valid only if a key column was found.  This
C                    routine will fail to find a key column if there are
C                    no active constraints on indexed columns.
C
C$ Parameters
C
C     MAXCON         is the maximum number of constraints that may
C                    be used in a query.
C
C     CDSCSZ         is the size of a column descriptor.
C
C$ Exceptions
C
C     1)  If the segment contains no indexed columns on which there are
C         active constraints, the output argument FOUND is set to
C         .FALSE.  The other output arguments are undefined in this
C         case.
C
C     2)  If the constraints on the key column entirely eliminate all
C         rows in the segment, the returned values of BEGIDX and ENDIDX
C         are, respectively, 1 and 0.
C
C     3)  If the number of input constraints is out of range, the error
C         SPICE(INVALIDCOUNT) is signalled.
C
C$ Files
C
C     See the description of the input argument HANDLE.
C
C$ Particulars
C
C     The EKSRCH algorithm for finding rows matching a given set
C     of constraints attempts to use constraints on indexed columns
C     to enable the matching process to be performed efficiently.
C     The idea is to find the indexed column whose constraints limit
C     the possible set of matching rows to the smallest number; then
C     to linearly search through this set of candidate rows to see
C     which ones satisfy the remaining applicable constraints.  The
C     column used to initially limit the set of candidate rows is
C     called the `key column'.  The constraints on the key column that
C     are of interest are ones involving order relations or equality:
C     these constraints use the operators
C
C        EQ  GE  GT  LE  LT
C
C     Note that the NE operator is not of much use here.
C
C     The set of candidate rows simultaneously satisifies all such
C     constraints on the key column, and therefore is the intersection
C     of the set of rows satisfying each such constraint.  This method
C     of selecting candidate rows can rapidly eliminate large numbers of
C     rows from consideration, because the index on the key column can
C     be employed in finding rows that match constraints involving order
C     relations:  the start and end indices of such rows can be found
C     by a binary, rather than linear, search.
C
C     A segment may have multiple indexed columns on which there are
C     constraints involving order or equality relations; in this case
C     the column whose constraints are most restrictive is selected as
C     the key column.
C
C     It may also happen that a segment contains no indexed columns.
C     In such a case, the key column is not useful for narrowing the
C     set of candidate rows.  The first column of the segment is
C     arbitrarily selected as the key column in this case.
C
C$ Examples
C
C     See EKSRCH.
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
C-    SPICELIB Version 1.2.0, 26-JUL-1996 (NJB)
C
C        Added check of FAILED after calls to the EK search functions.
C
C-    SPICELIB Version 1.1.0, 17-APR-1996 (WLT)
C
C        Removed spurious periods that appeared at the
C        end of lines 524 and 577 in previous edition.
C
C-    Beta Version 1.0.0, 23-OCT-1995 (NJB)
C
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 1.2.0, 26-JUL-1996 (NJB)
C
C        Added check of FAILED after calls to the EK search functions.
C-&

 
C
C     SPICELIB functions
C
      INTEGER               CARDI
      INTEGER               ORDI
 
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Other functions
C
      INTEGER               ZZEKILLE
      INTEGER               ZZEKILLT
 
C
C     Local parameters
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
 
C
C     Local variables
C
      INTEGER               B
      INTEGER               BEST
      INTEGER               COL
      INTEGER               CONMAP ( MAXCON )
      INTEGER               DTYPE
      INTEGER               E
      INTEGER               ELTIDX
      INTEGER               I
      INTEGER               IDXSET ( LBCELL : MAXCON )
      INTEGER               J
      INTEGER               LASTLE
      INTEGER               LASTLT
      INTEGER               MAXPTR
      INTEGER               MINPTR
      INTEGER               NMATCH
 
      LOGICAL               ELIM
      LOGICAL               FND
      LOGICAL               INDEXD
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKKEY' )
      END IF
 
C
C     There's no key column to begin with.
C
      FOUND = .FALSE.
 
 
      IF (  ( NCNSTR .LT. 0 ) .OR. ( NCNSTR .GT. MAXCON )  ) THEN
 
         CALL SETMSG ( 'The number of constraints was #; valid '  //
     .                 'range is 0:#'                              )
         CALL ERRINT ( '#',   NCNSTR                               )
         CALL ERRINT ( '#',   MAXCON                               )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                       )
         CALL CHKOUT ( 'ZZEKKEY'                                   )
         RETURN
 
      END IF
 
C
C     Make a set out of the indices of indexed columns referenced
C     in active constraints.  Maintain a mapping from each column
C     to the index of some constraint that references that column.
C
      CALL SSIZEI ( MAXCON, IDXSET )
 
 
      DO I = 1, NCNSTR
 
         IF ( ACTIVE(I) ) THEN
 
            INDEXD  =  DSCLST(IXTIDX,I) .NE. IFALSE
 
            IF ( INDEXD ) THEN
               CALL INSRTI ( CLIDXS(I), IDXSET )
            END IF
 
         END IF
 
      END DO
 
 
      DO I = 1, CARDI(IDXSET)
 
         FND = .FALSE.
         J   =  1
 
         DO WHILE (  ( J .LE. NCNSTR ) .AND. ( .NOT. FND )  )
 
            IF (   ACTIVE(J) .AND. ( CLIDXS(J) .EQ. IDXSET(I) )  ) THEN
               FND       =   .TRUE.
               CONMAP(I) =   J
            ELSE
               J         =   J + 1
            END IF
 
         END DO
 
      END DO
 
 
C
C     We finish up now if there are no indexed columns
C     on which there are active constraints.
C
      IF ( CARDI(IDXSET) .EQ. 0 ) THEN
 
         CALL CHKOUT ( 'ZZEKKEY' )
         RETURN
 
      END IF
 
C
C     For each column in the `indexed' set, find out how many
C     candidate rows we'd have if we picked that column as the key
C     column.  If we find that the constraints on some column eliminate
C     all matching rows, we can stop.
C
      BEGIDX  =  1
      ENDIDX  =  NROWS
      BEST    =  IDXSET(1)
      NMATCH  =  NROWS
 
      ELIM    = .FALSE.
      ELTIDX  =  1
 
      DO WHILE (  ( ELTIDX .LE. CARDI(IDXSET) )  .AND. ( .NOT. ELIM )  )
C
C        Get the attribute list pointer for the current column.
C
         COL   = IDXSET(ELTIDX)
 
C
C        Set the initial values of MINPTR, MAXPTR, and NMATCH
C
         MINPTR       =  1
         MAXPTR       =  NROWS
         I            =  1
 
         DO WHILE (  ( I .LE. NCNSTR ) .AND. ( .NOT. ELIM )  )
C
C           For each constraint, increase MINPTR or decrease MAXPTR
C           if the constraint allows us to do so.
C
 
            IF (  ( CLIDXS(I) .EQ. COL ) .AND. ( ACTIVE(I) )  )  THEN
C
C              The Ith constraint is active and applies to this column.
C
C              If the column has character type, set the bounds of the
C              token on the right hand side of the constraint.
C              Otherwise, set the bounds to default valid values to
C              avoid subscript bounds errors.
C
               DTYPE  =  DSCLST( TYPIDX, I )
 
               IF ( DTYPE .EQ. CHR ) THEN
                  B = CBEGS(I)
                  E = CENDS(I)
               ELSE
                  B  =  1
                  E  =  1
               END IF
 
C
C              At this point, MINPTR and MAXPTR are in the range
C              1:NROWS, and MINPTR is less than or equal to MAXPTR.
C
 
               IF ( OPS(I) .EQ. LT ) THEN
C
C                 Find the index of the pointer to the last row
C                 whose value in this column is less than the
C                 value cited in the Ith constraint.
C
                  LASTLT  =  ZZEKILLT ( HANDLE,         SEGDSC,
     .                                  DSCLST(1,I),    NROWS,
     .                                  DTYPES(I),      CHRBUF(B:E),
     .                                  DVALS(I),       IVALS(I)  )
C
C                 If all column elements were greater than or equal
C                 to the specified value, MAXPTR will be set to zero.
C
                  MAXPTR  =  MIN ( LASTLT, MAXPTR )
                  ELIM    =  MAXPTR .EQ. 0
 
 
               ELSE IF ( OPS(I) .EQ. LE ) THEN
C
C                 Find the index of the pointer to the last row
C                 whose value in this column is less or equal to
C                 the value cited in the Ith constraint.
C
                  LASTLE  =  ZZEKILLE ( HANDLE,         SEGDSC,
     .                                  DSCLST(1,I),    NROWS,
     .                                  DTYPES(I),      CHRBUF(B:E),
     .                                  DVALS(I),       IVALS(I)    )
 
                  MAXPTR  =  MIN ( LASTLE, MAXPTR )
                  ELIM    =  MAXPTR .EQ. 0
 
 
               ELSE IF ( OPS(I) .EQ. EQ ) THEN
C
C                 Find both the pointer to the last row whose
C                 value in this column is less than the value cited in
C                 the Ith constraint, and the pointer to the last row
C                 whose value in this column is less than or equal to
C                 the value cited in the Ith constraint.  The
C                 successor of the former pointer, together with
C                 the latter pointer, bound the range of pointers
C                 to possible matching rows.
C
                  LASTLT  =  ZZEKILLT ( HANDLE,         SEGDSC,
     .                                  DSCLST(1,I),    NROWS,
     .                                  DTYPES(I),      CHRBUF(B:E),
     .                                  DVALS(I),       IVALS(I)    )

 
                  LASTLE  =  ZZEKILLE ( HANDLE,         SEGDSC,
     .                                  DSCLST(1,I),    NROWS,
     .                                  DTYPES(I),      CHRBUF(B:E),
     .                                  DVALS(I),       IVALS(I)    )
 
                  IF ( LASTLT .LT. LASTLE ) THEN
C
C                    There is at least one row whose value in the
C                    current column matches the value cited in the Ith
C                    constraint, and LASTLE is the index of the pointer
C                    to the last such row.  The successor of LASTLT is
C                    the first pointer to such a row (even if LASTLT is
C                    zero).
C
                     MINPTR = MAX ( LASTLT+1, MINPTR )
                     MAXPTR = MIN ( LASTLE,   MAXPTR )
 
                  ELSE
C
C                    No rows match this constraint.
C
                     ELIM  =  .TRUE.
 
                  END IF
 
 
               ELSE IF ( OPS(I) .EQ. GT ) THEN
C
C                 Find the index of the pointer to the last row
C                 whose value in this column is less or equal to
C                 the value cited in the Ith constraint.  The index of
C                 the pointer to the first row satisfying all of the
C                 constraints on this column is the successor of
C                 this pointer or a greater pointer.
C
                  LASTLE  =  ZZEKILLE ( HANDLE,         SEGDSC,
     .                                  DSCLST(1,I),    NROWS,
     .                                  DTYPES(I),      CHRBUF(B:E),
     .                                  DVALS(I),       IVALS(I)    )
 
                  MINPTR  =  MAX ( LASTLE + 1,  MINPTR )
                  ELIM    =  LASTLE .EQ. NROWS
 
 
               ELSE IF ( OPS(I) .EQ. GE ) THEN
C
C                 Find the index of the pointer to the last row
C                 whose value in this column is less than the
C                 value cited in the Ith constraint.  The index of the
C                 pointer to the first row satisfying all of the
C                 constraints on this column is the successor of
C                 this pointer or a greater pointer.
C
                  LASTLT  =  ZZEKILLT ( HANDLE,         SEGDSC,
     .                                  DSCLST(1,I),    NROWS,
     .                                  DTYPES(I),      CHRBUF(B:E),
     .                                  DVALS(I),       IVALS(I)    )   

                  MINPTR  =  MAX ( LASTLT + 1, MINPTR )
                  ELIM    =  LASTLT .EQ. NROWS
 
               END IF
C
C              We've checked the Ith constraint to see whether
C              it applied to the current column, and if it did,
C              we adjusted MINPTR and MAXPTR to reflect the
C              constraint.
C
            END IF
C
C           We've applied the Ith constraint, if it was active.
C
            IF ( MINPTR .GT. MAXPTR ) THEN
               ELIM  =  .TRUE.
            END IF
 
 
            IF ( .NOT. ELIM ) THEN
               I  =  I  +  1
            END IF

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZEKKEY' )
               RETURN
            END IF
            
         END DO
 
C
C        We've applied all of active, applicable constraints to column
C        COL.  If these constraints did not eliminate the current
C        segment entirely, save the number of candidate rows we'd have
C        if we kept this column as the key column.
C
         IF ( .NOT. ELIM ) THEN
 
            NMATCH  =  MAXPTR - MINPTR + 1
 
            IF (  NMATCH .LT. ( ENDIDX - BEGIDX + 1 )  ) THEN
C
C              This is our new key column, until a better one comes
C              along.
C
               BEST    =  COL
               BEGIDX  =  MINPTR
               ENDIDX  =  MAXPTR
 
            END IF
 
            ELTIDX  =  ELTIDX + 1
 
         END IF
 
 
      END DO
 
 
      IF ( ELIM ) THEN
C
C        If the segment was eliminated by constraints on the last column
C        we looked at, set BEGIDX and ENDIDX to indicate the absence of
C        matching rows.
C
         KEY     =  COL
         BEGIDX  =  1
         ENDIDX  =  0
 
      ELSE
C
C        BEST, BEGIDX, and ENDIDX are set to reflect the key column.
C        Set KEY and grab the descriptor of the key column.
C
         KEY     =  BEST
 
      END IF
 
 
      I   =  CONMAP(  ORDI( KEY, IDXSET )  )
 
      CALL MOVEI ( DSCLST(1,I), CDSCSZ, KEYDSC )
 
 
C
C     De-activate constraints on the key column that we've already
C     applied.
C
      DO I = 1, NCNSTR
 
         IF (  ACTIVE(I)  .AND.  ( CLIDXS(I) .EQ. KEY )  )  THEN
 
            IF (       ( OPS(I) .EQ. LT )
     .           .OR.  ( OPS(I) .EQ. LE )
     .           .OR.  ( OPS(I) .EQ. EQ )
     .           .OR.  ( OPS(I) .EQ. GE )
     .           .OR.  ( OPS(I) .EQ. GT )  ) THEN
 
C
C              This constraint is met by the candidate rows; we can
C              turn it off.
C
               ACTIVE(I)  =  .FALSE.
 
            END IF
 
         END IF
 
      END DO
 
C
C     At this point, we've found a key column.
C
      FOUND = .TRUE.
 
      CALL CHKOUT ( 'ZZEKKEY' )
      RETURN
      END
