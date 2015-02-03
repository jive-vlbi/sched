C$Procedure      ZZEKPARS ( EK, parse tokenized EK query )
 
      SUBROUTINE ZZEKPARS (  QUERY,   NTOKEN,  LXBEGS,  LXENDS,
     .                       TOKENS,  VALUES,  NUMVLS,  CHRBUF,
     .                       CHBEGS,  CHENDS,  EQRYI,   EQRYC,
     .                       EQRYD,   ERROR,   PRSERR          )
 
      IMPLICIT NONE
 
C$ Abstract
C
C     Parse an EK query that has been scanned and tokenized.
C     Represent the result as an encoded EK query.
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
C     PARSE
C
C$ Declarations
 
      INCLUDE 'ektokn.inc'
      INCLUDE 'ekkeyw.inc'
      INCLUDE 'ekbool.inc'
      INCLUDE 'ektype.inc'
      INCLUDE 'ekopcd.inc'
      INCLUDE 'ekquery.inc'
      INCLUDE 'ekqlimit.inc'
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      CHARACTER*(*)         QUERY
      INTEGER               NTOKEN
      INTEGER               LXBEGS ( * )
      INTEGER               LXENDS ( * )
      INTEGER               TOKENS ( * )
      INTEGER               VALUES ( * )
      DOUBLE PRECISION      NUMVLS ( * )
      CHARACTER*(*)         CHRBUF
      INTEGER               CHBEGS ( * )
      INTEGER               CHENDS ( * )
      INTEGER               EQRYI  ( LBCELL : * )
      CHARACTER*(*)         EQRYC
      DOUBLE PRECISION      EQRYD  ( * )
      LOGICAL               ERROR
      CHARACTER*(*)         PRSERR
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     QUERY      I   Query in string form.
C     NTOKEN     I   Number of tokens in query.
C     LXBEGS,
C     LXENDS     I   Lexeme begin and end positions in QUERY.
C     TOKENS     I   Token codes.
C     VALUES     I   Values associated with tokens.
C     NUMVLS     I   Buffer containing numeric token values.
C     CHRBUF     I   Buffer containing string token values.
C     CHBEGS,
C     CHENDS     I   String token begin and end character positions.
C     EQRYI,
C     EQRYC,
C     EQRYD      O   Parsed query and string and number value buffers.
C     ERROR      O   Flag indicating whether query parsed correctly.
C     PRSERR     O   Parse error description.
C
C$ Detailed_Input
C
C     QUERY          is a string containing the original input query.
C                    QUERY is used only for creating error messages.
C
C     NTOKEN         is the number of tokens in the input query.
C
C     LXBEGS,
C     LXENDS         are lexeme begin and end pointers; the Ith
C                    lexeme in the query is
C
C                       QUERY ( LXBEGS(I) : LXENDS(I) )
C
C                    (Lexemes are strings that correspond to tokens
C                     in the language.)
C
C     TOKENS         is an array of token codes.  The Ith element of
C                    TOKENS represents the Ith token in the scanned
C                    query.
C
C     VALUES         is an array of values associated with tokens; the
C                    Ith element of VALUES corresponds to the Ith
C                    token.  Keywords, for example, are distinguished
C                    by codes in the VALUES array.  Literal numeric
C                    and string tokens use the VALUES array to point
C                    to elements of NUMVLS or CHBEGS and CHENDS,
C                    respectively.  Some tokens don't need to use
C                    VALUES, but to simplify indexing, each token gets
C                    an element of this array.
C
C     NUMVLS         is an array of double precision numbers used to
C                    store the values corresponding to literal numeric
C                    tokens.
C
C     CHRBUF         is a string used to store the values of literal
C                    string tokens.
C
C     CHBEGS,
C     CHENDS         are pairs of begin and end pointers into CHRBUF.
C                    These pointers delimit character values
C                    associated with literal string tokens.
C
C
C$ Detailed_Output
C
C     EQRYI,
C     EQRYC,
C     EQRYD          are the integer, character, and numeric portions
C                    of an encoded form of the input query.  The
C                    SELECT, FROM, WHERE, and ORDER BY clauses of the
C                    input query are all represented in this encoding.
C                    WHERE clause constraints have been normalized.
C
C                    Normalized queries have their constraints grouped
C                    into a disjunction of conjunctions of relational
C                    expressions, as symbolized below:
C
C                          ( <rel_exp_1_1> and <rel_exp_1_2> and ... )
C                       or ( <rel_exp_2_1> and <rel_exp_2_2> and ... )
C                                     .
C                                     .
C                                     .
C                       or ( <rel_exp_N_1> and <rel_exp_N_2> and ... )
C
C     ERROR,
C     PRSERR         are, respectively, a flag indicating whether the
C                    input query parsed correctly, and a message
C                    describing the parse error, if one occurred.  If
C                    no error occurred, ERROR is .FALSE. and PRSERR
C                    is blank.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  Most of the exceptions that can occur on a call to
C         ZZEKPARS are caused by errors in the input query.  ZZEKPARS
C         attempts to diagnose these via the output error flag and
C         error message, instead of signalling errors.  The
C         error messages that ZZEKPARS can return are listed below.
C         In the messages shown, the symbol # is used to designate
C         a marker for which a value can be substituted in an actual
C         message.
C
C
C            The BY keyword was not found following the
C            ORDER keyword.
C
C            Invalid keyword at location #.
C            Actual token was: #
C
C            Table or column name expected at location
C            #. Actual token was: #
C
C            Table name expected at location #.
C            Actual token was: #
C
C            Column name expected at location #.
C            Actual token was: #
C
C            Table alias, comma, or keyword expected at
C            location #. Actual token was: #
C
C            Comma or keyword expected at
C            location #. Actual token was: #
C
C            Comma expected at location #. Actual token was: #
C
C            PRSERR  =  More tokens were expected in query.
C
C            The keyword # was expected at location
C            #. Actual token was: #
C
C            Invalid token at location #. Token was: #
C
C            PRSERR  =  Number of tables in "FROM" clause exceeds
C            allowed maximum of #.
C
C            PRSERR  =  Number of order-by columns exceeds allowed
C            maximum of #.
C
C            PRSERR  =  Number of SELECT columns exceeds allowed
C            maximum of #.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine determines whether a query is syntactically correct;
C     it tranforms correct queries into the EK system's encoded query
C     representation.
C
C     The encoded queries output by this routine are not ready for
C     execution; they still must undergo name resolution, time value
C     conversion, and semantic checking.  See EKFIND for an example of
C     the normal sequence of query processing.
C
C$ Examples
C
C     See the header of EKFIND for examples of valid and invalid
C     queries.
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
C-    SPICELIB Version 4.1.0, 15-OCT-1996 (NJB)
C
C        Bug fix: default order sense was not encoded when ORDER-BY
C        clause was not the last clause of the query.
C
C-    SPICELIB Version 4.0.0, 17-NOV-1995 (NJB)
C
C        Complete re-write for architecture 3.
C
C-&
 
 
C$ Index_Entries
C
C     parse EK query
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 4.1.0, 15-OCT-1996 (NJB)
C
C        Bug fix: default order sense was not encoded when ORDER-BY
C        clause was not the last clause of the query.  The old algorithm
C        assumed that no clauses followed the ORDER-BY clause, which
C        at one time was a limitation of the EK query language.
C
C-&
 
 
C
C     SPICELIB functions
C
      INTEGER               CARDI
 
      LOGICAL               FAILED
      LOGICAL               RETURN
 
 
C
C     Local parameters
C
 
C
C     State parameters
C
      INTEGER               FROM
      PARAMETER           ( FROM    =            0 )
 
      INTEGER               FRMTAB
      PARAMETER           ( FRMTAB  =   FROM   + 1 )
 
      INTEGER               FRMCNT
      PARAMETER           ( FRMCNT  =   FRMTAB + 1 )
 
      INTEGER               FRMALS
      PARAMETER           ( FRMALS  =   FRMCNT + 1 )
 
      INTEGER               WHERE
      PARAMETER           ( WHERE   =   FRMALS + 1 )
 
      INTEGER               ORDER
      PARAMETER           ( ORDER   =   WHERE  + 1 )
 
      INTEGER               ORDRBY
      PARAMETER           ( ORDRBY  =   ORDER  + 1 )
 
      INTEGER               ORDTAB
      PARAMETER           ( ORDTAB  =   ORDRBY + 1 )
 
      INTEGER               ORDNAM
      PARAMETER           ( ORDNAM  =   ORDTAB + 1 )
 
      INTEGER               ORDCOL
      PARAMETER           ( ORDCOL  =   ORDNAM + 1 )
 
      INTEGER               ORDSNS
      PARAMETER           ( ORDSNS  =   ORDCOL + 1 )
 
      INTEGER               SELKEY
      PARAMETER           ( SELKEY  =   ORDSNS + 1 )
 
      INTEGER               SELECT
      PARAMETER           ( SELECT  =   SELKEY + 1 )
 
      INTEGER               SELTAB
      PARAMETER           ( SELTAB  =   SELECT + 1 )
 
      INTEGER               SELNAM
      PARAMETER           ( SELNAM  =   SELTAB + 1 )
 
      INTEGER               SELCOL
      PARAMETER           ( SELCOL  =   SELNAM + 1 )
 
      INTEGER               TERM
      PARAMETER           ( TERM    =   SELCOL + 1 )
 
C
C     Other local parameters
C
      INTEGER               MAXREL
      PARAMETER           ( MAXREL =  200 )
 
      INTEGER               TYPLEN
      PARAMETER           ( TYPLEN =   32 )
 
      INTEGER               KEYLEN
      PARAMETER           ( KEYLEN =   32 )
 
C
C     Local variables
C
      CHARACTER*(TYPLEN)    ERRTYP
      CHARACTER*(KEYLEN)    EXPKEY
 
      INTEGER               ALSDSC ( EQVDSZ )
      INTEGER               B
      INTEGER               COLDSC ( EQVDSZ )
      INTEGER               E
      INTEGER               I
      INTEGER               J
      INTEGER               L
      INTEGER               LXB
      INTEGER               LXE
      INTEGER               NAMDSC ( EQVDSZ )
      INTEGER               NCNSTR
      INTEGER               NORDER
      INTEGER               NSEL
      INTEGER               NTABS
      INTEGER               STATE
      INTEGER               TABDSC ( EQVDSZ )
      INTEGER               TOKEN
      INTEGER               TOKNUM
      INTEGER               VALDSC ( EQVDSZ )
 
      LOGICAL               FND
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKPARS' )
      END IF
 
C
C     Initialize the encoded query each time, for safety.
C
      CALL ZZEKQINI ( EQIMIN, MAXQNM, EQRYI, EQRYC, EQRYD )
 
      IF ( FAILED() ) THEN
         ERROR  =  .TRUE.
         PRSERR =  'SPICE(BUG):  encoded query init failed.'
         CALL CHKOUT ( 'ZZEKPARS' )
         RETURN
      END IF
 
C
C     The structure of a query is
C
C        <QUERY>                 =>    <SELECT clause> <FROM clause>
C                                      <WHERE clause> <ORDER BY clause>
C
C        <SELECT clause>         =>    SELECT <select list>
C
C        <select list>           =>    <column entry>
C                                    | <select list>, <column entry>
C
C        <column entry>          =>    <table name>.<column name>
C                                    | <column name>
C
C        <FROM clause>           =>    <table name list>
C
C        <table name list>       =>    <table entry>
C                                    | <table name list>, <table entry>
C
C        <table entry>           =>    <table name>
C                                    | <table name> <table alias>
C
C        <WHERE clause>          =>    WHERE <constraint expression>
C                                    | <NIL>
C
C        <ORDER BY clause>       =>    ORDER BY <order-by list>
C                                    | <NIL>
C
C        <order-by list>         =>    <order-by column entry>
C                                    | <order-by list>,
C                                      <order-by column entry>
C
C        <order-by column entry> =>    <column entry> <order>
C
C        <order>                 =>    ASC
C                                    | DESC
C                                    | <NIL>
C
C
C
C     We'll parse the clauses of the query in the following order:
C
C        FROM
C        WHERE     (if present)
C        ORDER BY  (if present)
C        SELECT
C
C
      CALL ZZEKTLOC ( TKKEY,   KWFROM,  NTOKEN,  TOKENS,  VALUES,
     .                TOKNUM,  FND                                )
 
      IF ( .NOT. FND ) THEN
 
         ERROR   =  .TRUE.
         ERRTYP  =  'FROM_NOT_FOUND'
         STATE   =   TERM
 
      ELSE
 
         STATE   =  FROM
         NTABS   =  0
         NSEL    =  0
         NCNSTR  =  0
         NORDER  =  0
         ERROR   =  .FALSE.
         PRSERR  =  ' '
         ERRTYP  =  ' '
 
      END IF
 
 
      DO WHILE ( STATE .NE. TERM )
C
C        Advance to the next token, if there is one.
C
         TOKNUM  =  TOKNUM + 1
 
         IF ( TOKNUM .GT. NTOKEN ) THEN
C
C           We're out of tokens.  Set the token value to indicate
C           `end of query'.
C
            TOKEN  =  TKEOQ
 
         ELSE
 
            TOKEN  =  TOKENS(TOKNUM)
 
         END IF
 
C
C        Perform semantic actions based on the state and current token.
C
         IF ( STATE .EQ. FROM ) THEN
C
C           We expect to see an identifier representing a table name.
C           No other tokens are allowed.
C
            IF ( TOKEN .EQ. TKID ) THEN
C
C              We've found a table name (as far as we can tell at
C              this point).  Make sure we haven't exceeded the limit
C              for table names; if not, add the appropriate information
C              to the encoded query.
C
               NTABS  =   NTABS  +  1
 
 
               IF ( NTABS .GT. MAXTAB ) THEN
 
                  ERROR   =  .TRUE.
                  ERRTYP  =  'TOO_MANY_TABLES'
                  STATE   =  TERM
 
               ELSE
 
                  I       =   VALUES(TOKNUM)
                  B       =   CHBEGS(I)
                  E       =   CHENDS(I)
                  L       =   E  -  B   +  1
                  LXB     =   LXBEGS(TOKNUM)
                  LXE     =   LXENDS(TOKNUM)
 
 
                  CALL ZZEKINQC ( CHRBUF( B : E ),
     .                            L,
     .                            LXB,
     .                            LXE,
     .                            EQRYI,
     .                            EQRYC,
     .                            TABDSC          )
 
C
C                 Append the table descriptor to the integer part of the
C                 query.
C
                  DO J = 1, EQVDSZ
                     CALL APPNDI ( TABDSC(J), EQRYI )
                  END DO
 
C
C                 Add a place-holder value descriptor to reserve
C                 space for an alias descriptor for this table.  If an
C                 actual alias is supplied, we'll update this
C                 descriptor.
C
                  CALL CLEARI ( EQVDSZ, ALSDSC )
                  ALSDSC(EQDTYP)  =  CHR
 
                  DO J = 1, EQVDSZ
                     CALL APPNDI ( ALSDSC(J), EQRYI )
                  END DO
 
C
C                 Update the table count in the encoded query.
C
                  CALL ZZEKWEQI ( 'NUM_TABLES', NTABS, EQRYI )
 
 
                  STATE  =  FRMTAB
 
               END IF
 
 
            ELSE IF ( TOKEN .EQ. TKEOQ ) THEN
 
               ERROR   =  .TRUE.
               ERRTYP  =  'MORE_TOKENS_EXP'
               STATE   =  TERM
 
 
            ELSE
C
C              We've got the wrong kind of token here.
C
               ERROR   =  .TRUE.
               ERRTYP  =  'TABLE_EXP'
               STATE   =  TERM
 
            END IF
C
C           State is a member of {FRMTAB, TERM}.
C
 
         ELSE IF ( STATE .EQ. FRMTAB ) THEN
C
C           We should see a comma, an alias, one of the SELECT,
C           WHERE or ORDER keywords, or the end of the query.
C
            IF ( TOKEN .EQ. TKEOQ ) THEN
C
C              We're out of tokens.  It's time to parse the
C              WHERE clause.
C
               STATE   =  WHERE
 
 
            ELSE IF ( TOKEN .EQ. TKCOMA ) THEN
C
C              It's time to look for another table name.
C
               STATE  =  FROM
 
 
            ELSE IF ( TOKEN .EQ. TKID ) THEN
C
C              We've got an alias.  Add this string to the encoded
C              query.
C
               I   =   VALUES(TOKNUM)
               B   =   CHBEGS(I)
               E   =   CHENDS(I)
               L   =   E  -  B   +  1
               LXB =   LXBEGS(TOKNUM)
               LXE =   LXENDS(TOKNUM)
 
               CALL ZZEKINQC ( CHRBUF( B : E ),
     .                         L,
     .                         LXB,
     .                         LXE,
     .                         EQRYI,
     .                         EQRYC,
     .                         ALSDSC                        )
 
C
C              Update the place-holder alias descriptor in the integer
C              part of the query.
C
               CALL MOVEI ( ALSDSC,
     .                      EQVDSZ,
     .                      EQRYI(  CARDI(EQRYI) - EQVDSZ + 1  )   )
 
 
               STATE  =  FRMALS
 
 
            ELSE IF (  TOKEN .EQ.  TKKEY  )  THEN
C
C              The last table name in the FROM clause is followed by
C              a keyword.  SELECT, WHERE and ORDER are the only valid
C              possibilities.
C
               IF (       ( VALUES(TOKNUM) .NE. KWWHER )
     .              .AND. ( VALUES(TOKNUM) .NE. KWSEL  )
     .              .AND. ( VALUES(TOKNUM) .NE. KWORDR )  ) THEN
C
C                 We've got a keyword we don't want here.
C
                  ERROR  =  .TRUE.
                  ERRTYP =  'BAD_KEYWORD'
                  STATE  =  TERM
 
               ELSE
C
C                 Parse the WHERE clause.
C
                  STATE  =  WHERE
 
               END IF
 
 
            ELSE
C
C              We've got the wrong kind of token altogether.
C
               ERROR  =  .TRUE.
               ERRTYP =  'ALIAS_EXP'
               STATE  =  TERM
 
            END IF
 
C
C           STATE is a member of {FROM, FRMALS, WHERE, TERM}.
C
 
 
 
         ELSE IF ( STATE .EQ. FRMALS ) THEN
C
C           We should see a comma, the SELECT, WHERE or ORDER
C           keywords, or the end of the query.
C
            IF ( TOKEN .EQ. TKEOQ ) THEN
C
C              We're out of tokens.  It's time to parse the
C              WHERE clause.
C
               STATE   =  WHERE
 
 
            ELSE IF ( TOKEN .EQ. TKCOMA ) THEN
C
C              It's time to look for another table name.
C
               STATE  =  FROM
 
 
            ELSE IF (  TOKEN .EQ.  TKKEY  )  THEN
C
C              The last table name in the FROM clause is followed by
C              a keyword.  SELECT, WHERE and ORDER are the only valid
C              possibilities.
C
               IF (       ( VALUES(TOKNUM) .NE. KWWHER )
     .              .AND. ( VALUES(TOKNUM) .NE. KWSEL  )
     .              .AND. ( VALUES(TOKNUM) .NE. KWORDR )  ) THEN
C
C                 We've got a keyword we don't want here.
C
                  ERROR  =  .TRUE.
                  ERRTYP =  'BAD_KEYWORD'
                  STATE  =  TERM
 
               ELSE
C
C                 Parse the WHERE clause.
C
                  STATE  =  WHERE
 
               END IF
 
 
            ELSE
C
C              We've got the wrong kind of token altogether.
C
               ERROR  =  .TRUE.
               ERRTYP =  'COMMA_OR_KEY_EXP'
               STATE  =  TERM
 
            END IF
C
C           STATE is a member of {FROM, WHERE, TERM}.
C
 
 
         ELSE IF ( STATE .EQ. SELKEY ) THEN
C
C           It's time to parse the SELECT clause.  We'll need to
C           locate the SELECT keyword.
C
            CALL ZZEKTLOC ( TKKEY,   KWSEL,  NTOKEN,  TOKENS,  VALUES,
     .                      TOKNUM,  FND                               )
 
            IF ( .NOT. FND ) THEN
 
               ERROR   =  .TRUE.
               ERRTYP  =  'SELECT_NOT_FOUND'
               STATE   =   TERM
 
            ELSE
 
               STATE   =  SELECT
 
            END IF
 
 
         ELSE IF ( STATE .EQ. SELECT ) THEN
C
C           We must see either the * token, the ALL keyword,
C           or an identifier here.  The identifier may be a lone
C           column name, or it may be a column name qualified by a
C           table name or alias.
C
C           For the moment, we don't support the * or ALL options.
C
            IF ( TOKEN .EQ. TKID ) THEN
C
C              We've found a name (as far as we can tell at this point).
C              Make sure we haven't exceeded the limit for SELECT
C              column names; if not, store the name string in the
C              encoded query, and save the descriptor until we've
C              figured out whether we're looking at a column name or
C              table name.
C
               NSEL  =  NSEL  +  1
 
               IF ( NSEL.GT. MAXSEL ) THEN
 
                  ERROR   =  .TRUE.
                  ERRTYP  =  'TOO_MANY_SEL_COLS'
                  STATE   = TERM
 
               ELSE
 
                  I       =   VALUES(TOKNUM)
                  B       =   CHBEGS(I)
                  E       =   CHENDS(I)
                  L       =   E  -  B   +  1
                  LXB     =   LXBEGS(TOKNUM)
                  LXE     =   LXENDS(TOKNUM)
 
 
                  CALL ZZEKINQC ( CHRBUF( B : E ),
     .                            L,
     .                            LXB,
     .                            LXE,
     .                            EQRYI,
     .                            EQRYC,
     .                            NAMDSC          )
 
C
C                 Add a place-holder value descriptor to reserve
C                 space for a table descriptor for this name.  If it
C                 turns out that the current name is a table name, we'll
C                 update this descriptor.
C
                  CALL CLEARI ( EQVDSZ, VALDSC )
                  VALDSC(EQDTYP)  =  CHR
 
                  DO J = 1, EQVDSZ
                     CALL APPNDI ( VALDSC(J), EQRYI )
                  END DO
 
C
C                 Update the SELECT column count in the encoded query.
C
                  CALL ZZEKWEQI ( 'NUM_SELECT_COLS', NSEL, EQRYI )
 
 
                  STATE  =  SELNAM
 
               END IF
 
 
            ELSE IF ( TOKEN .EQ. TKEOQ ) THEN
 
               ERROR  =  .TRUE.
               ERRTYP =  'MORE_TOKENS_EXP'
               STATE  =  TERM
 
 
            ELSE
C
C              We've got the wrong kind of token here.
C
               ERROR  =  .TRUE.
               ERRTYP =  'TABLE_OR_COLUMN_EXP'
               STATE  =  TERM
 
            END IF
C
C           State is a member of {SELNAM, TERM}.
C
 
 
         ELSE IF ( STATE .EQ. SELNAM ) THEN
C
C           We've seen a SELECT column name, or else the name
C           of a table qualifying a SELECT column name.
C
            IF ( TOKEN .EQ. TKEOQ ) THEN
C
C              The name we picked up was an unqualified column
C              name.  Append the saved name descriptor to the encoded
C              query.
C
               DO J = 1, EQVDSZ
                  CALL APPNDI ( NAMDSC(J), EQRYI )
               END DO
 
               STATE   =   TERM
 
 
            ELSE IF ( TOKEN .EQ. TKCOMA ) THEN
C
C              The name we picked up was an unqualified column
C              name.  Append the saved name descriptor to the encoded
C              query.  Another name should follow.
C
               DO J = 1, EQVDSZ
                  CALL APPNDI ( NAMDSC(J), EQRYI )
               END DO
 
               STATE  =  SELECT
 
 
            ELSE IF ( TOKEN .EQ. TKDOT ) THEN
C
C              The name we picked up was a table name or alias.  A
C              column name should follow.
C
               STATE  =  SELTAB
 
 
            ELSE IF ( TOKEN .EQ. TKKEY ) THEN
C
C              We have the last column name in the SELECT clause.
C
C              Append the saved name descriptor to the encoded
C              query.
C
               DO J = 1, EQVDSZ
                  CALL APPNDI ( NAMDSC(J), EQRYI )
               END DO
 
C
C              The last column name in the SELECT clause is followed by
C              a keyword.  FROM, WHERE and ORDER are the only valid
C              possibilities.
C
               IF (      ( VALUES(TOKNUM) .EQ. KWWHER )
     .              .OR. ( VALUES(TOKNUM) .EQ. KWFROM )
     .              .OR. ( VALUES(TOKNUM) .EQ. KWORDR )  ) THEN
C
C                 We're done with the SELECT clause.
C
                  STATE  =  TERM
 
               ELSE
C
C                 We've got a keyword we don't want here.
C
                  ERROR  =  .TRUE.
                  ERRTYP =  'BAD_KEYWORD'
                  STATE  =  TERM
 
               END IF
 
            ELSE
C
C              We've got the wrong kind of token here.
C
               ERROR  =  .TRUE.
               ERRTYP =  'BAD_TOKEN'
               STATE  =  TERM
 
            END IF
C
C           STATE is a member of {SELECT, SELTAB, TERM}.
C
 
 
         ELSE IF ( STATE .EQ. SELTAB ) THEN
C
C           We've picked up a qualifying table name for a SELECT
C           column.  We must see a column name here.
C
            IF ( TOKEN .EQ. TKID ) THEN
C
C              Update the place-holder table name descriptor in the
C              encoded query.
C
               CALL MOVEI ( NAMDSC,
     .                      EQVDSZ,
     .                      EQRYI(  CARDI(EQRYI) - EQVDSZ + 1  )   )
 
C
C              Add the column name to the character part of the
C              encoded query.
C
               I   =   VALUES(TOKNUM)
               B   =   CHBEGS(I)
               E   =   CHENDS(I)
               L   =   E  -  B   +  1
               LXB =   LXBEGS(TOKNUM)
               LXE =   LXENDS(TOKNUM)
 
               CALL ZZEKINQC ( CHRBUF( B : E ),
     .                         L,
     .                         LXB,
     .                         LXE,
     .                         EQRYI,
     .                         EQRYC,
     .                         COLDSC           )
 
C
C              Add the descriptor for the column name to the encoded
C              query.
C
               DO J = 1, EQVDSZ
                  CALL APPNDI ( COLDSC(J), EQRYI )
               END DO
 
 
               STATE  =  SELCOL
 
 
            ELSE IF ( TOKEN .EQ. TKEOQ ) THEN
 
               ERROR  =  .TRUE.
               ERRTYP =  'MORE_TOKENS_EXP'
               STATE  =  TERM
 
            ELSE
 
               ERROR  =  .TRUE.
               ERRTYP =  'COLUMN_EXP'
               STATE  =  TERM
 
            END IF
C
C           STATE is a member of {SELCOL, TERM}.
C
 
 
         ELSE IF ( STATE .EQ. SELCOL ) THEN
C
C           We've picked up a qualified column name.  At this point,
C           we should see a keyword, a comma, or the end of the
C           query.
C
            IF ( TOKEN .EQ. TKKEY ) THEN
C
C              The last column name in the SELECT clause is followed by
C              a keyword.  FROM, WHERE and ORDER are the only valid
C              possibilities.
C
               IF (      ( VALUES(TOKNUM) .EQ. KWWHER )
     .              .OR. ( VALUES(TOKNUM) .EQ. KWFROM )
     .              .OR. ( VALUES(TOKNUM) .EQ. KWORDR )  ) THEN
C
C                 We're done with the SELECT clause.
C
                  STATE  =  TERM
 
               ELSE
C
C                 We've got a keyword we don't want here.
C
                  ERROR  =  .TRUE.
                  ERRTYP =  'BAD_KEYWORD'
                  STATE  =  TERM
 
               END IF
 
 
            ELSE IF ( TOKEN .EQ. TKCOMA ) THEN
C
C              We expect another SELECT column.
C
               STATE  =  SELECT
 
 
            ELSE IF ( TOKEN .EQ. TKEOQ ) THEN
C
C              We're done with the SELECT clause.
C
               STATE  =  TERM
 
            ELSE
 
               ERROR  =  .TRUE.
               ERRTYP =  'COMMA_OR_KEY_EXP'
               STATE  =  TERM
 
            END IF
C
C           STATE is a member of {SELECT, TERM}.
C
 
 
         ELSE IF ( STATE .EQ. WHERE ) THEN
C
C           The WHERE clause is optional.  See whether we have one.  The
C           clause is started by a WHERE keyword.
C
            CALL ZZEKTLOC ( TKKEY,   KWWHER,  NTOKEN,  TOKENS,  VALUES,
     .                      TOKNUM,  FND                               )
 
 
            IF ( FND ) THEN
C
C              We're going to hand off the list of tokens that comprise
C              the WHERE clause of the query to a routine that will
C              parse the tokens and form a list of relational
C              constraints.  Once this is done, all we have to do here
C              is check the validity of the column names and the values
C              used in the constraints.
C
               CALL ZZEKNRML ( QUERY,
     .                         NTOKEN,
     .                         LXBEGS,
     .                         LXENDS,
     .                         TOKENS,
     .                         VALUES,
     .                         NUMVLS,
     .                         CHRBUF,
     .                         CHBEGS,
     .                         CHENDS,
     .                         EQRYI,
     .                         EQRYC,
     .                         EQRYD,
     .                         ERROR,
     .                         PRSERR )
 
 
               IF ( ERROR ) THEN
 
                  ERRTYP  =   'WHERE_ERROR'
                  STATE   =   TERM
 
               ELSE
C
C                 Parse the ORDER BY clause, if one is present.
C
                  STATE   =   ORDER
 
               END IF
 
 
            ELSE
C
C              Parse the ORDER BY clause, if one is present.
C
               STATE   =   ORDER
 
            END IF
C
C           STATE is a member of {ORDER, TERM}.
C
 
 
         ELSE IF ( STATE .EQ. ORDER ) THEN
C
C           The ORDER BY clause is optional.  See whether we have one.
C           The clause is started by an ORDER keyword.
C
            CALL ZZEKTLOC ( TKKEY,   KWORDR,  NTOKEN,  TOKENS,  VALUES,
     .                      TOKNUM,  FND                               )
 
 
            IF ( FND ) THEN
C
C              The BY keyword should follow the ORDER keyword.
C
               IF ( TOKNUM .LT. NTOKEN ) THEN
 
                  TOKNUM  =  TOKNUM + 1
 
                  IF (       (  TOKENS(TOKNUM) .EQ. TKKEY )
     .                 .AND. (  VALUES(TOKNUM) .EQ. KWBY  )  ) THEN
C
C                    We're ready to parse the ORDER BY clause.
C
                     STATE  =  ORDRBY
 
                  ELSE
C
C                    No BY keyword followed the ORDER keyword.
C
                     ERROR  =  .TRUE.
                     ERRTYP =  'BY_EXPECTED'
                     STATE  =  TERM
 
                  END IF
 
 
               ELSE
C
C                 We're out of tokens where we shouldn't be.
C
                  ERROR  =  .TRUE.
                  ERRTYP =  'BY_EXPECTED'
                  STATE  =  TERM
 
               END IF
 
 
            ELSE
C
C              We're ready to go on to the SELECT clause.
C
               STATE  =  SELKEY
 
            END IF
C
C           STATE is a member of {ORDRBY, SELKEY, TERM}.
C
 
 
 
         ELSE IF ( STATE .EQ. ORDRBY ) THEN
C
C           We must see a name in the order column list here.
C           The name may be a lone column name, or it may be a column
C           name qualified by a table name or alias.
C
            IF ( TOKEN .EQ. TKID ) THEN
C
C              We've found a name (as far as we can tell at this point).
C              Make sure we haven't exceeded the limit for order-by
C              column names; if not, store the name string in the
C              encoded query, and save the descriptor until we've
C              figured out whether we're looking at a column name or
C              table name.
C
               NORDER  =  NORDER  +  1
 
               IF ( NORDER .GT. MAXORD ) THEN
 
                  ERROR   =  .TRUE.
                  ERRTYP  =  'TOO_MANY_ORD_COLS'
                  STATE   = TERM
 
               ELSE
 
                  I       =   VALUES(TOKNUM)
                  B       =   CHBEGS(I)
                  E       =   CHENDS(I)
                  L       =   E  -  B   +  1
                  LXB     =   LXBEGS(TOKNUM)
                  LXE     =   LXENDS(TOKNUM)
 
 
                  CALL ZZEKINQC ( CHRBUF( B : E ),
     .                            L,
     .                            LXB,
     .                            LXE,
     .                            EQRYI,
     .                            EQRYC,
     .                            NAMDSC          )
 
C
C                 Add a place-holder value descriptor to reserve
C                 space for a table descriptor for this name.  If it
C                 turns out that the current name is a table name, we'll
C                 update this descriptor.
C
                  CALL CLEARI ( EQVDSZ, VALDSC )
                  VALDSC(EQDTYP)  =  CHR
 
                  DO J = 1, EQVDSZ
                     CALL APPNDI ( VALDSC(J), EQRYI )
                  END DO
 
C
C                 Update the order-by column count in the encoded query.
C
                  CALL ZZEKWEQI ( 'NUM_ORDERBY_COLS', NORDER, EQRYI )
 
 
                  STATE  =  ORDNAM
 
               END IF
 
 
            ELSE IF ( TOKEN .EQ. TKEOQ ) THEN
 
               ERROR  =  .TRUE.
               ERRTYP =  'MORE_TOKENS_EXP'
               STATE  =  TERM
 
 
            ELSE
C
C              We've got the wrong kind of token here.
C
               ERROR  =  .TRUE.
               ERRTYP =  'TABLE_OR_COLUMN_EXP'
               STATE  =  TERM
 
            END IF
C
C           State is a member of {ORDNAM, TERM}.
C
 
 
         ELSE IF ( STATE .EQ. ORDNAM ) THEN
C
C           We've seen an order-by column name, or else the name
C           of a table qualifying an order-by column name.
C
            IF ( TOKEN .EQ. TKEOQ ) THEN
C
C              The name we picked up was an unqualified column
C              name.  Append the saved name descriptor to the encoded
C              query.
C
               DO J = 1, EQVDSZ
                  CALL APPNDI ( NAMDSC(J), EQRYI )
               END DO
 
C
C              Since no ASCENDING or DESCENDING sense keyword was
C              supplied, append the default value ASCENDING to the
C              order-by column descriptor in the encoded query.
C
               CALL APPNDI ( EQASND, EQRYI )
 
C
C              We're done with the ORDER BY clause; go on to parse the
C              SELECT clause.
C
               STATE   =  SELKEY
 
 
            ELSE IF ( TOKEN .EQ. TKCOMA ) THEN
C
C              The name we picked up was an unqualified column
C              name.  Append the saved name descriptor to the encoded
C              query.  Another name should follow.
C
               DO J = 1, EQVDSZ
                  CALL APPNDI ( NAMDSC(J), EQRYI )
               END DO
 
C
C              Since no ASCENDING or DESCENDING sense keyword was
C              supplied, append the default value ASCENDING to the
C              order-by column descriptor in the encoded query.
C
               CALL APPNDI ( EQASND, EQRYI )
 
               STATE  = ORDRBY
 
 
            ELSE IF ( TOKEN .EQ. TKDOT ) THEN
C
C              The name we picked up was a table name or alias.  A
C              column name should follow.
C
               STATE = ORDTAB
 
 
            ELSE IF ( TOKEN .EQ. TKKEY ) THEN
C
C              We have a column name, which may be followed by a
C              keyword indicating the sense of the ordering, or may
C              be followed by a keyword starting a new clause.
C
C              Append the saved name descriptor to the encoded
C              query.
C
               DO J = 1, EQVDSZ
                  CALL APPNDI ( NAMDSC(J), EQRYI )
               END DO
 
C
C              Set the sense descriptor according to the keyword we've
C              picked up.  After this, we're ready to look for another
C              order-by column.
C
               IF ( VALUES(TOKNUM) .EQ. KWASND ) THEN
 
                  CALL APPNDI ( EQASND, EQRYI )
 
                  STATE = ORDSNS
 
 
               ELSE IF ( VALUES(TOKNUM) .EQ. KWDSND ) THEN
 
                  CALL APPNDI ( EQDSND, EQRYI )
 
                  STATE = ORDSNS
 
 
               ELSE IF (      ( VALUES(TOKNUM) .EQ. KWWHER )
     .                   .OR. ( VALUES(TOKNUM) .EQ. KWFROM )
     .                   .OR. ( VALUES(TOKNUM) .EQ. KWSEL  )  ) THEN
 
C
C                 Since no ASCENDING or DESCENDING sense keyword was
C                 supplied, append the default value ASCENDING to the
C                 order-by column descriptor in the encoded query.
C
                  CALL APPNDI ( EQASND, EQRYI )
 
C
C                 We're done with the ORDER BY clause.  Go on to
C                 parse the SELECT clause.
C
                  STATE  =  SELKEY
 
               ELSE
C
C                 We've got a keyword we don't want here.
C
                  ERROR  =  .TRUE.
                  ERRTYP =  'BAD_KEYWORD'
                  STATE  =  TERM
 
               END IF
 
 
            ELSE
C
C              We've got the wrong kind of token here.
C
               ERROR  =  .TRUE.
               ERRTYP =  'BAD_TOKEN'
               STATE  =  TERM
 
            END IF
C
C           STATE is a member of {ORDRBY, ORDTAB, ORDSNS, SELKEY, TERM}.
C
 
 
         ELSE IF ( STATE .EQ. ORDTAB ) THEN
C
C           We've picked up a qualifying table name for an order-by
C           column.  We must see a column name here.
C
            IF ( TOKEN .EQ. TKID ) THEN
C
C              Update the place-holder table name descriptor in the
C              encoded query.
C
               CALL MOVEI ( NAMDSC,
     .                      EQVDSZ,
     .                      EQRYI(  CARDI(EQRYI) - EQVDSZ + 1  )   )
 
C
C              Add the column name to the character part of the
C              encoded query.
C
               I   =   VALUES(TOKNUM)
               B   =   CHBEGS(I)
               E   =   CHENDS(I)
               L   =   E  -  B   +  1
               LXB =   LXBEGS(TOKNUM)
               LXE =   LXENDS(TOKNUM)
 
               CALL ZZEKINQC ( CHRBUF( B : E ),
     .                         L,
     .                         LXB,
     .                         LXE,
     .                         EQRYI,
     .                         EQRYC,
     .                         COLDSC           )
 
C
C              Add the descriptor for the column name to the encoded
C              query.
C
               DO J = 1, EQVDSZ
                  CALL APPNDI ( COLDSC(J), EQRYI )
               END DO
 
 
               STATE  =  ORDCOL
 
 
            ELSE IF ( TOKEN .EQ. TKEOQ ) THEN
 
               ERROR  =  .TRUE.
               ERRTYP =  'MORE_TOKENS_EXP'
               STATE  =  TERM
 
            ELSE
 
               ERROR  =  .TRUE.
               ERRTYP =  'COLUMN_EXP'
               STATE  =  TERM
 
            END IF
C
C           STATE is a member of {ORDCOL, TERM}.
C
 
 
         ELSE IF ( STATE .EQ. ORDCOL ) THEN
C
C           We've picked up a qualified column name.  At this point,
C           we should see a sense keyword, a comma, the end of the
C           query, or one of the FROM, SELECT, or WHERE keywords.
C
            IF ( TOKEN .EQ. TKKEY ) THEN
 
               IF ( VALUES(TOKNUM) .EQ. KWASND ) THEN
C
C                 The ASCENDING keyword has been supplied.  After this,
C                 look for another column.
C
                  CALL APPNDI ( EQASND, EQRYI )
 
                  STATE  =  ORDSNS
 
 
               ELSE IF ( VALUES(TOKNUM) .EQ. KWDSND ) THEN
C
C                 The DESCENDING keyword has been supplied.  After this,
C                 look for another column.
C
                  CALL APPNDI ( EQDSND, EQRYI )
 
                  STATE  =  ORDSNS
 
 
               ELSE IF (      ( VALUES(TOKNUM) .EQ. KWWHER )
     .                   .OR. ( VALUES(TOKNUM) .EQ. KWFROM )
     .                   .OR. ( VALUES(TOKNUM) .EQ. KWSEL  )  ) THEN
C
C                 We're done with the ORDER BY clause.  Go on to
C                 parse the SELECT clause.
C
                  STATE  =  SELKEY
 
               ELSE
 
                  ERROR  =  .TRUE.
                  ERRTYP =  'BAD_KEYWORD'
                  STATE  =  TERM
 
               END IF
 
 
            ELSE IF ( TOKEN .EQ. TKCOMA ) THEN
C
C              The ASCENDING keyword is implied.
C
               CALL APPNDI ( EQASND, EQRYI )
 
               STATE  =  ORDRBY
 
 
            ELSE IF ( TOKEN .EQ. TKEOQ ) THEN
C
C              The ASCENDING keyword is implied.
C
               CALL APPNDI ( EQASND, EQRYI )
 
C
C              We're done with the ORDER BY clause.  Parse the SELECT
C              clause.
C
               STATE  =  SELKEY
 
            ELSE
 
               ERROR  =  .TRUE.
               ERRTYP =  'COMMA_OR_KEY_EXP'
               STATE  =  TERM
 
            END IF
C
C           STATE is a member of {ORDRBY, ORDSNS, SELKEY, TERM}.
C
 
 
         ELSE IF ( STATE .EQ. ORDSNS ) THEN
C
C           We've picked up an order sense keyword.  At this point,
C           we should see comma or the end of the query, or one of the
C           FROM, SELECT, or WHERE keywords.
C
            IF ( TOKEN .EQ. TKCOMA ) THEN
C
C              We're ready to look for another column.
C
               STATE  =  ORDRBY
 
 
            ELSE IF ( TOKEN .EQ. TKEOQ ) THEN
C
C              We're done with the ORDER BY clause.  Parse the SELECT
C              clause.
C
               STATE  =  SELKEY
 
 
            ELSE IF ( TOKEN .EQ. TKKEY ) THEN
 
               IF (      ( VALUES(TOKNUM) .EQ. KWWHER )
     .              .OR. ( VALUES(TOKNUM) .EQ. KWFROM )
     .              .OR. ( VALUES(TOKNUM) .EQ. KWSEL  )  ) THEN
C
C                 We're done with the ORDER BY clause.  Go on to
C                 parse the SELECT clause.
C
                  STATE  =  SELKEY
 
               ELSE
 
                  ERROR  =  .TRUE.
                  ERRTYP =  'BAD_KEYWORD'
                  STATE  =  TERM
 
               END IF
 
 
            ELSE
 
               ERROR  =  .TRUE.
               ERRTYP =  'COMMA_EXP'
               STATE  =  TERM
 
            END IF
C
C           STATE is a member of {ORDRBY, SELKEY, TERM}.
C
 
 
         ELSE
C
C           Somehow, we've reached an invalid state.
C
            ERROR  = .TRUE.
            PRSERR = 'SPICE(BUG) -- Invalid state reached in EK parser.'
            STATE  =  TERM
 
         END IF
C
C           STATE is a member of {ORDRBY, TERM}.
C
 
      END DO
C
C     At this point, either an error has been detected, or the query
C     has been parsed, and the query is represented in encoded form
C     in the outputs EQRYI, EQRYC, and EQRYD.
C
 
C
C     We centralize construction of error messages in the following
C     section.
C
      IF ( ERROR ) THEN
 
         IF ( ERRTYP .EQ. 'FROM_NOT_FOUND' ) THEN
 
            PRSERR = 'Every query must contain a FROM clause. ' //
     .               'The FROM keyword was not found.'
 
 
         ELSE IF ( ERRTYP .EQ. 'SELECT_NOT_FOUND' ) THEN
 
            PRSERR = 'Every query must contain a SELECT clause. ' //
     .               'The SELECT keyword was not found.'
 
 
         ELSE IF ( ERRTYP .EQ. 'BY_EXPECTED' ) THEN
 
            PRSERR = 'The BY keyword was not found following the ' //
     .               'ORDER keyword.'
 
 
         ELSE IF ( ERRTYP .EQ. 'BAD_KEYWORD' ) THEN
 
            LXB    =  LXBEGS(TOKNUM)
            LXE    =  LXENDS(TOKNUM)
            PRSERR = 'Invalid keyword at location #. '  //
     .               'Actual token was: #'
            CALL REPMI ( PRSERR, '#', LXB,            PRSERR )
            CALL REPMC ( PRSERR, '#', QUERY(LXB:LXE), PRSERR )
 
 
         ELSE IF ( ERRTYP .EQ. 'TABLE_OR_COLUMN_EXP' ) THEN
 
            LXB    =  LXBEGS(TOKNUM)
            LXE    =  LXENDS(TOKNUM)
            PRSERR = 'Table or column name expected at location ' //
     .               '#. Actual token was: #'
            CALL REPMI ( PRSERR, '#', LXB,            PRSERR )
            CALL REPMC ( PRSERR, '#', QUERY(LXB:LXE), PRSERR )
 
 
         ELSE IF ( ERRTYP .EQ. 'TABLE_EXP' ) THEN
 
            LXB    =  LXBEGS(TOKNUM)
            LXE    =  LXENDS(TOKNUM)
            PRSERR = 'Table name expected at location #. '  //
     .               'Actual token was: #'
            CALL REPMI ( PRSERR, '#', LXB,            PRSERR )
            CALL REPMC ( PRSERR, '#', QUERY(LXB:LXE), PRSERR )
 
 
         ELSE IF ( ERRTYP .EQ. 'COLUMN_EXP' ) THEN
 
            LXB    =  LXBEGS(TOKNUM)
            LXE    =  LXENDS(TOKNUM)
            PRSERR = 'Column name expected at location #. '  //
     .               'Actual token was: #'
            CALL REPMI ( PRSERR, '#', LXB,            PRSERR )
            CALL REPMC ( PRSERR, '#', QUERY(LXB:LXE), PRSERR )
 
 
         ELSE IF ( ERRTYP .EQ. 'ALIAS_EXP' ) THEN
 
            LXB    =  LXBEGS(TOKNUM)
            LXE    =  LXENDS(TOKNUM)
            PRSERR = 'Table alias, comma, or keyword expected at ' //
     .               'location #. Actual token was: #'
            CALL REPMI ( PRSERR, '#', LXB,            PRSERR )
            CALL REPMC ( PRSERR, '#', QUERY(LXB:LXE), PRSERR )
 
 
         ELSE IF ( ERRTYP .EQ. 'COMMA_OR_KEY_EXP' ) THEN
 
            LXB    =  LXBEGS(TOKNUM)
            LXE    =  LXENDS(TOKNUM)
            PRSERR = 'Comma or keyword expected at ' //
     .               'location #. Actual token was: #'
            CALL REPMI ( PRSERR, '#', LXB,            PRSERR )
            CALL REPMC ( PRSERR, '#', QUERY(LXB:LXE), PRSERR )
 
 
         ELSE IF ( ERRTYP .EQ. 'COMMA_EXP' ) THEN
 
            LXB    =  LXBEGS(TOKNUM)
            LXE    =  LXENDS(TOKNUM)
            PRSERR = 'Comma expected at location #. Actual token was: #'
            CALL REPMI ( PRSERR, '#', LXB,            PRSERR )
            CALL REPMC ( PRSERR, '#', QUERY(LXB:LXE), PRSERR )
 
 
         ELSE IF ( ERRTYP .EQ. 'MORE_TOKENS_EXP' ) THEN
 
            PRSERR  =  'More tokens were expected in query.'
 
 
         ELSE IF ( ERRTYP .EQ. 'KEYWORD_EXP' ) THEN
 
            PRSERR = 'The keyword # was expected at location ' //
     .               '#. Actual token was: #'
            CALL REPMC ( PRSERR, '#', EXPKEY,         PRSERR )
            CALL REPMI ( PRSERR, '#', LXB,            PRSERR )
            CALL REPMC ( PRSERR, '#', QUERY(LXB:LXE), PRSERR )
 
 
         ELSE IF ( ERRTYP .EQ. 'BAD_TOKEN' ) THEN
 
            LXB    =  LXBEGS(TOKNUM)
            LXE    =  LXENDS(TOKNUM)
            PRSERR = 'Invalid token at location #. Token was: #'
            CALL REPMI ( PRSERR, '#', LXB,            PRSERR )
            CALL REPMC ( PRSERR, '#', QUERY(LXB:LXE), PRSERR )
 
 
         ELSE IF ( ERRTYP .EQ. 'TOO_MANY_TABLES' ) THEN
 
            PRSERR  =  'Number of tables in "FROM" clause exceeds ' //
     .                 'allowed maximum of #.'
            CALL REPMI  ( PRSERR, '#', MAXTAB, PRSERR )
 
 
         ELSE IF ( ERRTYP .EQ. 'TOO_MANY_ORD_COLS' ) THEN
 
            PRSERR  =  'Number of order-by columns exceeds allowed ' //
     .                 'maximum of #.'
            CALL REPMI  ( PRSERR, '#', MAXTAB, PRSERR )
 
 
         ELSE IF ( ERRTYP .EQ. 'TOO_MANY_SEL_COLS' ) THEN
 
            PRSERR  =  'Number of SELECT columns exceeds allowed ' //
     .                 'maximum of #.'
            CALL REPMI  ( PRSERR, '#', MAXSEL, PRSERR )
 
 
         ELSE IF ( ERRTYP .NE. 'WHERE_ERROR' ) THEN
 
            PRSERR  =  'SPICE(BUG)--Unrecognized error type.  Type ' //
     .                 'was #.'
            CALL REPMC ( PRSERR, '#', ERRTYP, PRSERR )
 
         END IF
 
 
 
      ELSE
C
C        Indicate that parsing is complete.
C
         CALL ZZEKWEQI ( 'PARSED', ITRUE, EQRYI )
 
      END IF
 
 
      CALL CHKOUT ( 'ZZEKPARS' )
      RETURN
      END
