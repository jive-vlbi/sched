C$Procedure  ZZEKPCOL ( Private: EK, parse column name )
 
      SUBROUTINE ZZEKPCOL ( QCOL,    EQRYI,   EQRYC,   TABLE,  ALIAS,
     .                      TABIDX,  COLUMN,  COLIDX,  ERROR,  ERRMSG )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Given an encoded query, parse the name of a column appearing in
C     that query, returning full particulars concerning the column and
C     its parent table.
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
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'ekquery.inc'
      INCLUDE 'ektnamsz.inc'
      INCLUDE 'ektokn.inc'
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      CHARACTER*(*)         QCOL
      INTEGER               EQRYI ( LBCELL : * )
      CHARACTER*(*)         EQRYC
      CHARACTER*(*)         TABLE
      CHARACTER*(*)         ALIAS
      INTEGER               TABIDX
      CHARACTER*(*)         COLUMN
      INTEGER               COLIDX
      LOGICAL               ERROR
      CHARACTER*(*)         ERRMSG
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     QCOL       I   Column name, possibly qualified.
C     EQRYI      I   Integer component of query.
C     EQRYC      I   Character component of query.
C     TABLE      O   Name of table qualifying column.
C     ALIAS      O   Alias of table, if present.
C     TABIDX     O   Index of TABLE in FROM clause, if known.
C     COLUMN     O   Name of QCOL, unqualified.
C     COLIDX     O   Index of QCOL within its parent virtual table.
C     ERROR      O   Error flag.
C     ERRMSG     O   Parse error message.
C
C$ Detailed_Input
C
C     QCOL           is a column name from an EK query.  QCOL may be
C                    qualified by a table name, in which case it
C                    conforms to the sytax
C
C                       <identifier> . <identifer>
C
C                    or QCOL may be unqualified, in which case it
C                    is simply an <identifier> in the EK query language.
C
C
C     EQRYI          is the integer portion of an encoded EK query.
C                    The query must have been parsed.
C
C     EQRYC          is the character portion of an encoded EK query.
C
C$ Detailed_Output
C
C     TABLE          is the name of the table containing the column
C                    identified by QCOL.  If QCOL contains a table name
C                    to begin with, TABLE is that name, converted to
C                    upper case.
C
C     ALIAS          is the alias of the table containing the column
C                    identified by QCOL, if an alias for that table is
C                    present in the input query.  If QCOL contains a
C                    table alias to begin with, TABLE is that alias,
C                    converted to upper case.
C
C     TABIDX         is the ordinal position in the FROM clause of the
C                    input query of the table containing the column
C                    designated by QCOL.
C
C     COLUMN         is the name of the column designated by QCOL,
C                    converted to upper case.
C
C     COLIDX         is the ordinal position column designated by QCOL
C                    with respect to the virtual table containing that
C                    column.
C
C     ERROR          is a logical flag indicating whether QCOL was
C                    parsed correctly.  The previous list of outputs
C                    are undefined if a parse error occurred.  ERROR
C                    is returned .TRUE. if a parse error occurred,
C                    .FALSE. otherwise.
C
C     ERRMSG         is a character string describing the cause of a
C                    parse error, if such an error occurred.  Otherwise,
C                    ERRMSG is returned blank.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input query is not initialized, the error will be
C         diagnosed by routines called by this routine.  The outputs
C         will not be modified.
C
C     2)  If the input query has not been semantically checked, the
C         error SPICE(NOTSEMCHECKED) will be signalled.  The outputs
C         will not be modified.
C
C     3)  If the input QCOL does not parse as a qualified or
C         unqualified column name, the error flag and message will
C         indicate that a parse error occurred.  No error will be
C         signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine supports parsing of the SELECT clause of EK
C     queries by higher-level routines.  This routine is
C     superseded by the SPICELIB routine EKPSEL.
C
C$ Examples
C
C     1)  Suppose that EQRYI and EQRYC have been obtained by
C         encoding the query
C
C            'SELECT T1.COL1, T2.COL2 FROM TABLE1 T1, TABLE2 T2'
C
C         Suppose also that the table TABLE1 contains two columns
C         named COL1 and COL2, and that the columns occur in that
C         order in the table.
C
C         Then the call
C
C            CALL ZZEKPCOL ( 'T1.COL', EQRYI,  EQRYC,  TABLE, ALIAS,
C           .                TABIDX,   COLUMN, COLIDX, ERROR, ERRMSG )
C
C         will return
C
C            TABLE  =  'TABLE1'
C            ALIAS  =  'T1'
C            TABIDX =  1
C            COLUMN =  'COL1'
C            COLIDX =  1
C            ERROR  =  .FALSE.
C            ERRMSG =  ' '
C
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
C     N.J. Bachman       (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 11-OCT-1995 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               ISRCHC
      LOGICAL               FAILED
 
C
C     Local parameters
C
      INTEGER               LONG
      PARAMETER           ( LONG   = 160 )
 
C
C     Local variables
C
      CHARACTER*(TNAMSZ)    ALSLST ( MAXTAB )
      CHARACTER*(LONG)      CHRBUF
      CHARACTER*(TNAMSZ)    TABLST ( MAXTAB )
      CHARACTER*(CNAMSZ)    TMPCOL
      CHARACTER*(TNAMSZ)    TMPTAB
 
      DOUBLE PRECISION      NUMVLS ( 3 )
 
      INTEGER               ATTDSC ( ADSCSZ )
      INTEGER               CC
      INTEGER               CHBEGS ( 3 )
      INTEGER               CHENDS ( 3 )
      INTEGER               I
      INTEGER               ICHECK
      INTEGER               J
      INTEGER               LXBEGS ( 3 )
      INTEGER               LXENDS ( 3 )
      INTEGER               NMATCH
      INTEGER               NTAB
      INTEGER               NTOKEN
      INTEGER               TOKENS ( 3 )
      INTEGER               VALUES ( 3 )
 
      LOGICAL               FND
      LOGICAL               QUAL
 
 
C
C     Use discovery check-in.
C
      ERROR   =  .FALSE.
      ERRMSG  = ' '
 
 
      CALL ZZEKREQI ( EQRYI, 'SEM_CHECKED', ICHECK )
 
      IF ( FAILED() ) THEN
         RETURN
      END IF
 
C
C     Make sure the encoded query is in order before proceeding.
C
      IF ( ICHECK .EQ. IFALSE ) THEN
 
         CALL CHKIN  ( 'ZZEKPCOL'                                     )
         CALL SETMSG ( 'Encoded query has not yet been semantically '//
     .                 'checked.'                                     )
         CALL SIGERR ( 'SPICE(NOTSEMCHECKED)'                         )
         CALL CHKOUT ( 'ZZEKPCOL'                                     )
         RETURN
 
      END IF
 
C
C     Scan the input column name.  There are only two valid token
C     sequences possible:
C
C        <identifier>
C
C        <identifier> . <identifier>
C
C     ZZEKSCAN should therefore return 1 or 3 tokens.
C
      CALL ZZEKSCAN ( QCOL,   3,      0,      NTOKEN, TOKENS,
     .                LXBEGS, LXENDS, VALUES, NUMVLS, CHRBUF,
     .                CHBEGS, CHENDS, ERROR,  ERRMSG          )
 
      IF ( ERROR ) THEN
         RETURN
      END IF
 
 
      IF ( NTOKEN .EQ. 1 ) THEN
 
         IF ( TOKENS(1) .NE. TKID ) THEN
 
            ERROR  = .TRUE.
            ERRMSG = 'Invalid column name; name should consist of ' //
     .               'an identifier.'
            RETURN
 
         END IF
 
         CALL UCASE ( QCOL, COLUMN )
         QUAL   =  .FALSE.
 
 
      ELSE IF ( NTOKEN .EQ. 3 ) THEN
 
         IF ( TOKENS(1) .NE. TKID  ) THEN
 
            ERROR  = .TRUE.
            ERRMSG = 'Invalid table name; name should consist of ' //
     .               'an identifier.'
            RETURN
 
         ELSE IF ( TOKENS(2) .NE. TKDOT ) THEN
 
            ERROR  = .TRUE.
            ERRMSG = 'Invalid qualified column name; table name ' //
     .               'should be followed by a period.'
            RETURN
 
         ELSE IF ( TOKENS(3) .NE. TKID ) THEN
 
            ERROR  = .TRUE.
            ERRMSG = 'Invalid column name; name should consist of ' //
     .               'an identifier.'
            RETURN
 
         END IF
 
 
         I       =  VALUES(1)
         J       =  VALUES(3)
         TMPTAB  =  CHRBUF ( CHBEGS(I) : CHENDS(I) )
         COLUMN  =  CHRBUF ( CHBEGS(J) : CHENDS(J) )
         QUAL    = .TRUE.
 
 
      ELSE
 
         ERROR  = .TRUE.
         ERRMSG = 'Invalid tokens present in qualified column name. ' //
     .            'Valid syntax is <column> or <table>.<column>'
         RETURN
 
      END IF
 
C
C     At this point, COLUMN and QUAL are set.  If a qualifying table
C     or alias was supplied, that string is stored in TMPTAB.  Both
C     COLUMN and TMPTAB are in upper case.
C
C     If we got this far, we'll need to look up the table names and
C     aliases from the query.
C
      CALL ZZEKREQI ( EQRYI,  'NUM_TABLES', NTAB   )
 
      DO I = 1, NTAB
         CALL ZZEKQTAB ( EQRYI, EQRYC, I, TABLST(I), ALSLST(I) )
      END DO
 
C
C     If QCOL contains a table name, look for that name in the
C     table list, and if necessary, in the alias list.
C
      IF ( QUAL ) THEN
 
         TABIDX  =  ISRCHC ( TMPTAB, NTAB, TABLST )
 
         IF ( TABIDX .EQ. 0 ) THEN
            TABIDX = ISRCHC ( TMPTAB, NTAB, ALSLST )
         END IF
 
C
C        If we didn't find the table name in either list, it's just
C        plain wrong.
C
         IF ( TABIDX .EQ. 0 ) THEN
            ERROR  =  .TRUE.
            ERRMSG =  'Table name <#> does not match table or alias '//
     .                'from query.'
            CALL REPMC ( ERRMSG, '#', TMPTAB, ERRMSG )
            RETURN
         END IF
 
C
C        At this point, TABIDX is valid.  Locate the column within
C        the table.
C
         CALL EKCCNT ( TABLST(TABIDX), CC )
 
         IF ( FAILED() ) THEN
            RETURN
         END IF
 
 
         FND   =  .FALSE.
         J     =   1
 
         DO WHILE (  ( J .LE. CC ) .AND. ( .NOT. FND )  )
 
            CALL EKCII ( TABLST(TABIDX), J, TMPCOL, ATTDSC )
 
            IF ( TMPCOL .EQ. COLUMN ) THEN
               COLIDX  =  J
               FND     =  .TRUE.
            ELSE
               J       = J + 1
            END IF
 
         END DO
 
 
         IF ( .NOT. FND ) THEN
 
            ERROR  =  .TRUE.
            ERRMSG =  'Column name <#> does not appear in the '//
     .                'qualifying table <#>.'
            CALL REPMC ( ERRMSG, '#', COLUMN, ERRMSG )
            CALL REPMC ( ERRMSG, '#', TMPTAB, ERRMSG )
            RETURN
 
         END IF
C
C        At this point, TABIDX and COLIDX are set correctly.
C
 
      ELSE
C
C        No qualifying table name was supplied.  COLUMN had better
C        be a unique column name among the set of columns belong to
C        tables in the FROM clause of the input query.  Check the
C        columns for each table in the FROM clause, looking for
C        matches.
C
         NMATCH  =  0
 
         DO I  = 1, NTAB
 
            CALL EKCCNT ( TABLST(I), CC )
 
            IF ( FAILED() ) THEN
               RETURN
            END IF
 
            DO J = 1, CC
 
               CALL EKCII ( TABLST(I), J, TMPCOL, ATTDSC )
 
               IF (  TMPCOL .EQ. COLUMN ) THEN
                  NMATCH = NMATCH + 1
                  COLUMN = TMPCOL
                  COLIDX = J
                  TABIDX = I
               END IF
 
            END DO
 
         END DO
C
C        Check to see whether we have the unique identification we're
C        hoping for.
C
         IF ( NMATCH .EQ. 0 ) THEN
 
            ERROR  =  .TRUE.
            ERRMSG =  'Column name <#> does not appear in any table '//
     .                'in FROM clause of query.'
            CALL REPMC ( ERRMSG, '#', COLUMN, ERRMSG )
            RETURN
 
         ELSE IF ( NMATCH .GT. 1 ) THEN
 
            ERROR  =  .TRUE.
            ERRMSG =  'Column name <#> is ambiguous without '//
     .                'a qualifying table name.'
            CALL REPMC ( ERRMSG, '#', COLUMN, ERRMSG )
            RETURN
 
         END IF
C
C        At this point, COLUMN, TABIDX and COLIDX are set correctly.
C
 
      END IF
 
C
C     At this point, COLUMN, TABIDX and COLIDX are set correctly,
C     regardless of whether the input name was qualified.  Fill the rest
C     of our output variables.
C
      TABLE  =  TABLST ( TABIDX )
      ALIAS  =  ALSLST ( TABIDX )
 
      RETURN
      END
