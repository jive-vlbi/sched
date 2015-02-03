C$Procedure  ZZEKSEMC ( Private: EK, semantically check encoded query )
 
      SUBROUTINE ZZEKSEMC ( QUERY, EQRYI, EQRYC, ERROR, ERRMSG, ERRPTR )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Semantically check an encoded EK query.
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
 
 
      INCLUDE 'ekattdsc.inc'
      INCLUDE 'ekbool.inc'
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekopcd.inc'
      INCLUDE 'ekquery.inc'
      INCLUDE 'ektnamsz.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      CHARACTER*(*)         QUERY
      INTEGER               EQRYI ( LBCELL : * )
      CHARACTER*(*)         EQRYC
      LOGICAL               ERROR
      CHARACTER*(*)         ERRMSG
      INTEGER               ERRPTR
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     QUERY      I   Original query.
C     EQRYI     I-O  Integer component of query.
C     EQRYC     I-O  Character component of query.
C     ERROR      O   Error flag.
C     ERRMSG     O   Semantic error message.
C     ERRPTR     O   Position in query where error was detected.
C
C$ Detailed_Input
C
C     QUERY          is the original query from which EQRYI and EQRYC
C                    were obtained.  QUERY is used only for
C                    construction of error messages.
C
C     EQRYI          is the integer portion of an encoded EK query.
C                    The query must have been parsed and must have
C                    its table and column names resolved.  Time values
C                    must also have been resolved.
C
C     EQRYC          is the character portion of an encoded EK query.
C
C$ Detailed_Output
C
C     EQRYI          is the integer portion of an encoded EK query.
C                    On output, semantic checking will have been
C                    performed.
C
C     EQRYC          is the character portion of an encoded EK query.
C
C     ERROR          is a logical flag indicating whether a semantic
C                    error was detected.
C
C     ERRMSG         is an error message describing a semantic error,
C                    if such an error was detected.  If ERROR is
C                    returned .FALSE., then ERRPTR is undefined.
C
C     ERRPTR         is the character position in the original query
C                    at which a semantic error was detected, if the
C                    input query contains a semantic error.  This
C                    index refers to the offending lexeme's position in
C                    the original query represented by the input encoded
C                    query.  If ERROR is returned .FALSE., ERRPTR is
C                    undefined.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If no EK files are loaded at the time this routine is called,
C         the error will be diagnosed by routines called by this
C         routine.
C
C     2)  If the input query is not initialized, the error will be
C         diagnosed by routines called by this routine.  The outputs
C         will not be modified.
C
C     3)  If the input query has not had its names resolved, the error
C         SPICE(UNRESOLVEDNAMES) will be signalled.  The outputs
C         will not be modified.
C
C     4)  If the input query contains time values that have not been
C         resolved, the error SPICE(UNRESOLVEDTIMES) will be signalled.
C         The outputs will not be modified.
C
C     5)  If any sort of semantic error is detected in the input query,
C         the output flag ERROR is set, an error message is returned,
C         and LXBEG and LXEND are set to indicate the location of the
C         first lexeme at which an error was detected.
C
C         The checks performed by this routine are listed below:
C
C           - Constraints comparing values from two columns must
C             refer to columns having identical data types, or else
C             both types must be numeric.
C
C           - Constraints comparing values from a column with literal
C             values must refer to columns having the data type of the
C             literal value.
C
C           - The LIKE and NOT LIKE operators may be used only with
C             string values.
C
C           - Columns named in constraints must be scalar-valued.
C
C           - Columns named as order-by columns must be scalar-valued.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The semantic checking performed by this routine is dependent on
C     the kernels loaded at the time this routine is called.
C
C     This routine assumes that encoded EK query architecture version
C     1 is to be used with the query to be initialized; this routine
C     will not work with any other architecture version.
C
C$ Examples
C
C     See EKFIND.
C
C$ Restrictions
C
C     1) Loading or unloading EK files between name resolution of the
C        the input query and passing the query to this routine will
C        invalidate the checking done by this routine, and may cause
C        the routine to fail.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
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
C     Local parameters
C
      INTEGER               SHORT
      PARAMETER           ( SHORT  = 32 )
 
C
C     Local variables
C
 
      CHARACTER*(TNAMSZ)    ALIAS
      CHARACTER*(CNAMSZ)    COLNAM
      CHARACTER*(TNAMSZ)    LHSTAB
      CHARACTER*(TNAMSZ)    ORDTAB
      CHARACTER*(TNAMSZ)    RHSTAB
      CHARACTER*(SHORT)     TYPSTR ( 4 )
 
      INTEGER               ATTDSC ( ADSCSZ )
      INTEGER               BASE
      INTEGER               CNSTYP
      INTEGER               COLIDX
      INTEGER               I
      INTEGER               IRSOLV
      INTEGER               LHSSIZ
      INTEGER               LHSTYP
      INTEGER               LXB    ( 2 )
      INTEGER               LXE    ( 2 )
      INTEGER               NCNJ
      INTEGER               NCNS
      INTEGER               NORD
      INTEGER               NTAB
      INTEGER               OPCODE
      INTEGER               RHSSIZ
      INTEGER               RHSTYP
      INTEGER               TABIDX
      INTEGER               TRSOLV
 
      LOGICAL               LIKEOP
      LOGICAL               NULVAL
 
C
C     Saved variables
C
      SAVE                  TYPSTR
 
C
C     Initial values
C
      DATA                  TYPSTR  /  'CHARACTER',
     .                                 'DOUBLE PRECISION',
     .                                 'INTEGER',
     .                                 'TIME'               /
 
C
C     Use discovery check-in.
C
      ERROR   =  .FALSE.
      ERRMSG  = ' '
      ERRPTR  = 0
 
      CALL ZZEKREQI ( EQRYI, 'NAMES_RESOLVED', IRSOLV )
 
      IF ( FAILED() ) THEN
         RETURN
      END IF
 
      IF ( IRSOLV .EQ. IFALSE ) THEN
 
         CALL CHKIN  ( 'ZZEKSEMC'                                   )
         CALL SETMSG ( 'Encoded query has not had names resolved.'  )
         CALL SIGERR ( 'SPICE(UNRESOLVEDNAMES)'                     )
         CALL CHKOUT ( 'ZZEKSEMC'                                   )
         RETURN
 
      END IF
 
 
      CALL ZZEKREQI ( EQRYI, 'TIMES_RESOLVED', TRSOLV )
 
      IF ( FAILED() ) THEN
         RETURN
      END IF
 
      IF ( TRSOLV .EQ. IFALSE ) THEN
 
         CALL CHKIN  ( 'ZZEKSEMC'                                   )
         CALL SETMSG ( 'Encoded query has not had time values '     //
     .                 'resolved.'                                  )
         CALL SIGERR ( 'SPICE(UNRESOLVEDTIMES)'                     )
         CALL CHKOUT ( 'ZZEKSEMC'                                   )
         RETURN
 
      END IF
 
C
C     Get the important counts from the query.
C
      CALL ZZEKREQI ( EQRYI, 'NUM_TABLES',        NTAB )
      CALL ZZEKREQI ( EQRYI, 'NUM_CONSTRAINTS',   NCNS )
      CALL ZZEKREQI ( EQRYI, 'NUM_CONJUNCTIONS',  NCNJ )
      CALL ZZEKREQI ( EQRYI, 'NUM_ORDERBY_COLS',  NORD )
 
C
C     Perform semantic checks applicable to constraints.
C
      DO I = 1, NCNS
C
C        Calculate the base address of the constraint.
C
         BASE    =  EQVBAS + (NTAB * 2 * EQVDSZ) + (I-1)*EQCDSZ
 
C
C        Obtain the constraint type.
C
         CNSTYP  =  EQRYI ( BASE + EQCTYP )
 
C
C        Get the index of the table containing the LHS column, and get
C        the index of this column within that table.  Look up the
C        table name.
C
         TABIDX  =  EQRYI ( BASE + EQLTAB - 1 + EQTORD )
         COLIDX  =  EQRYI ( BASE + EQLCOL - 1 + EQCIDX )
         LXB(1)  =  EQRYI ( BASE + EQLCOL - 1 + EQBLEX )
         LXE(1)  =  EQRYI ( BASE + EQLCOL - 1 + EQELEX )
 
         CALL ZZEKQTAB ( EQRYI, EQRYC, TABIDX, LHSTAB, ALIAS )
 
C
C        Look up the name and attributes of the column on the LHS of the
C        constraint.
C
         CALL EKCII ( LHSTAB, COLIDX, COLNAM, ATTDSC )
 
         LHSTYP  =  ATTDSC(ATTTYP)
         LHSSIZ  =  ATTDSC(ATTSIZ)
 
         IF ( LHSSIZ .NE. 1 ) THEN
 
            ERROR  =  .TRUE.
            ERRMSG =  'Non-scalar column <#> having size # found '  //
     .                'in query constraint.'
            CALL REPMC  ( ERRMSG, '#', QUERY(LXB(1):LXE(2)), ERRMSG )
            CALL REPMI  ( ERRMSG, '#', LHSSIZ,               ERRMSG )
 
            ERRPTR =  LXB(1)
            RETURN
 
         END IF
 
C
C        Get the operator for the current constraint.
C
         OPCODE  =  EQRYI ( BASE + EQOPCD )
 
C
C        Decide whether the constraint is an `IS NULL' or `IS NOT NULL'
C        test.
C
         NULVAL  =  ( OPCODE .EQ. ISNULL ) .OR. ( OPCODE .EQ. NOTNUL )
 
C
C        Check for use of the LIKE or NOT LIKE operators.  These
C        operators may be used only if the LHS column has character
C        type.
C
         LIKEOP  =  ( OPCODE .EQ. LIKE ) .OR. ( OPCODE .EQ. UNLIKE )
 
 
         IF (  LIKEOP  .AND.  ( LHSTYP .NE. CHR )  ) THEN
 
            ERROR  =  .TRUE.
            ERRMSG = 'LIKE and NOT LIKE operators may be used only ' //
     .               'with character columns.  Column <#> has type '//
     .               '#.'
 
            CALL REPMC ( ERRMSG, '#', QUERY(LXB(1):LXE(1)),  ERRMSG )
            CALL REPMC ( ERRMSG, '#', TYPSTR(LHSTYP),        ERRMSG )
 
            ERRPTR  =  LXB(1)
            RETURN
 
         END IF
 
 
C
C        If the constraint compares two columns, get the same
C        information for the RHS column.
C
         IF ( CNSTYP .EQ. EQCOL ) THEN
 
            TABIDX  =  EQRYI ( BASE + EQRTAB - 1 + EQTORD )
            COLIDX  =  EQRYI ( BASE + EQRCOL - 1 + EQCIDX )
            LXB(2)  =  EQRYI ( BASE + EQRCOL - 1 + EQBLEX )
            LXE(2)  =  EQRYI ( BASE + EQRCOL - 1 + EQELEX )
 
            CALL ZZEKQTAB ( EQRYI, EQRYC, TABIDX, RHSTAB, ALIAS )
 
C
C           Look up the name and attributes of the column on the RHS of
C           the constraint.
C
            CALL EKCII ( RHSTAB, COLIDX, COLNAM, ATTDSC )
 
            RHSTYP  =  ATTDSC(2)
            RHSSIZ  =  ATTDSC(4)
 
            IF ( RHSSIZ .NE. 1 ) THEN
 
               ERROR  =  .TRUE.
               ERRMSG =  'Non-scalar column <#> having size # found '//
     .                   'in query constraint.'
               CALL REPMC  ( ERRMSG, '#', QUERY(LXB(1):LXE(2)), ERRMSG )
               CALL REPMI  ( ERRMSG, '#', RHSSIZ,               ERRMSG )
 
               ERRPTR =  LXB(2)
               RETURN
 
            END IF
 
C
C           Check for data type mismatch.
C
            IF ( RHSTYP .NE. LHSTYP ) THEN
C
C              The only allowed mismatch is between integers and
C              d.p. numbers.
C
               IF (      ( LHSTYP .EQ. TIME )
     .              .OR. ( LHSTYP .EQ. CHR  )
     .              .OR. ( RHSTYP .EQ. TIME )
     .              .OR. ( RHSTYP .EQ. CHR  )  ) THEN
 
                  ERROR  =  .TRUE.
                  ERRMSG = 'Data type mismatch: column <#> has data ' //
     .                     'type #; column <#> has data type #.'
 
                  CALL REPMC (ERRMSG, '#', QUERY(LXB(1):LXE(1)), ERRMSG)
                  CALL REPMC (ERRMSG, '#', TYPSTR(LHSTYP),       ERRMSG)
                  CALL REPMC (ERRMSG, '#', QUERY(LXB(2):LXE(2)), ERRMSG)
                  CALL REPMC (ERRMSG, '#', TYPSTR(RHSTYP),       ERRMSG)
 
                  ERRPTR = LXB(2)
                  RETURN
 
               END IF
 
            END IF
 
 
         ELSE
C
C           The constraint compares a column against a value.  If the
C           operator is `IS NULL' or `IS NOT NULL', there are no
C           further semantic checks to be made.
C
            IF ( NULVAL ) THEN
               RETURN
            END IF
 
C
C           Get the data type of the value on the RHS.
C
            RHSTYP  =  EQRYI ( BASE + EQBVAL - 1 + EQDTYP )
            LXB(2)  =  EQRYI ( BASE + EQBVAL - 1 + EQBLEX )
            LXE(2)  =  EQRYI ( BASE + EQBVAL - 1 + EQELEX )
 
            IF ( RHSTYP .NE. LHSTYP ) THEN
C
C              The only allowed mismatch is between integers and
C              d.p. numbers.
C
               IF (       ( LHSTYP .EQ. TIME )
     .               .OR. ( LHSTYP .EQ. CHR  )
     .               .OR. ( RHSTYP .EQ. TIME )
     .               .OR. ( RHSTYP .EQ. CHR  )  ) THEN
 
                  ERROR  =  .TRUE.
                  ERRMSG = 'Data type mismatch: column <#> has data ' //
     .                     'type #; value <#> has data type #.'
 
                  CALL REPMC (ERRMSG, '#', QUERY(LXB(1):LXE(1)), ERRMSG)
                  CALL REPMC (ERRMSG, '#', TYPSTR(LHSTYP),       ERRMSG)
                  CALL REPMC (ERRMSG, '#', QUERY(LXB(2):LXE(2)), ERRMSG)
                  CALL REPMC (ERRMSG, '#', TYPSTR(RHSTYP),       ERRMSG)
 
                  ERRPTR = LXB(2)
                  RETURN
 
               END IF
 
            END IF
 
 
         END IF
 
C
C        We've finished the checks on the current constraint.
C
      END DO
 
C
C     Now check the order-by columns, if any are present.  These
C     columns must have scalar type.
C
      DO I = 1, NORD
C
C        Get the query column descriptor for the Ith order-by column.
C
         BASE   =       EQVBAS
     .              +   NTAB * 2 * EQVDSZ
     .              +   NCNJ
     .              +   NCNS     * EQCDSZ
     .              +   (I-1)    * EQODSZ
C
C        Look up the attributes of the column.  It's the size we're
C        after.
C
         TABIDX  =  EQRYI ( BASE              + EQTORD )
         COLIDX  =  EQRYI ( BASE + EQOCOL - 1 + EQCIDX )
         LXB(1)  =  EQRYI ( BASE + EQOCOL - 1 + EQBLEX )
         LXE(1)  =  EQRYI ( BASE + EQOCOL - 1 + EQELEX )
 
         CALL ZZEKQTAB ( EQRYI, EQRYC, TABIDX, ORDTAB, ALIAS )
 
 
         CALL EKCII ( ORDTAB, COLIDX, COLNAM, ATTDSC )
 
 
         IF ( ATTDSC(4) .NE. 1 ) THEN
 
            ERROR  =  .TRUE.
            ERRMSG =  'Non-scalar column <#> having size # found '  //
     .                'in order-by column.'
            CALL REPMC  ( ERRMSG, '#', QUERY(LXB(1):LXE(2)), ERRMSG )
            CALL REPMI  ( ERRMSG, '#', ATTDSC(4),            ERRMSG )
 
            ERRPTR =  LXB(1)
            RETURN
 
         END IF
 
      END DO
 
C
C     Indicate completion of semantic checking.
C
      CALL ZZEKWEQI (  'SEM_CHECKED',  ITRUE,  EQRYI  )
 
      RETURN
      END
