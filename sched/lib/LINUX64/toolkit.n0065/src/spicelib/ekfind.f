C$Procedure      EKFIND ( EK, find data )
 
      SUBROUTINE EKFIND ( QUERY, NMROWS, ERROR, ERRMSG )
 
C$ Abstract
C
C     Find E-kernel data that satisfy a set of constraints.
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
C     SEARCH
C
C$ Declarations
 
      INCLUDE 'ekquery.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'ektype.inc'
      INCLUDE 'ekopcd.inc'
      INCLUDE 'ekcnamsz.inc'
 
      CHARACTER*(*)         QUERY
      INTEGER               NMROWS
      LOGICAL               ERROR
      CHARACTER*(*)         ERRMSG
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     QUERY      I   Query specifying data to be found.
C     NMROWS     O   Number of matching rows.
C     ERROR      O   Flag indicating whether query parsed correctly.
C     ERRMSG     O   Parse error description.
C
C$ Detailed_Input
C
C     QUERY          is a character string that specifies a set of EK
C                    data to select from those present in currently
C                    loaded EK files.  The selected data will be
C                    retrievable via the EK fetch routines EKGC, EKGD,
C                    and EKGI.
C
C                    The query consists of four clauses, the third and
C                    fourth of which are optional.  The general form
C                    of a query is
C
C                       SELECT <column list>
C                       FROM <table list>
C                       [WHERE <constraint list>]
C                       [ORDER BY <ORDER BY column list>]
C
C                    where brackets indicate optional items.  The
C                    elements of the query shown above are called,
C                    respectively, the `SELECT clause', the
C                    `FROM clause', the `WHERE clause', and the
C                    `ORDER BY clause'.  The result of a query may be
C                    thought of as a new table, whose columns are those
C                    specified in the SELECT clause, whose rows are
C                    those satisfying the constraints of the WHERE
C                    clause, and whose rows are ordered according to
C                    the ORDER BY clause.
C
C                    The SELECT clause specifies a list of columns
C                    from which data are to be selected.  In a simple
C                    (non-join) query, these columns must belong to
C                    the single table specified in the FROM clause.
C
C                    The form of a SELECT clause is
C
C                       SELECT <column name> [ ,<column name>...]
C
C                    In queries having multiple tables in the FROM
C                    clause, column names are ambiguous if they occur
C                    in more than one table in the FROM clause.  Such
C                    column names must be qualified with table
C                    identifiers.  These identifiers may be the names of
C                    the tables to which the columns belong, or table
C                    `aliases', names (usually short ones) associated
C                    with tables in the FROM clause.  Table aliases have
C                    duration limited to the execution of the query to
C                    which they belong.
C
C                    The form of a qualified column name is
C
C                       <table name>.<column name>
C
C                    or
C
C                       <table alias>.<column name>
C
C
C                    The FROM clause specifies the tables from which
C                    data are to be selected.  In simple queries, only
C                    one table is listed.  In this case the form of
C                    the FROM clause is
C
C                       FROM <table name>
C
C                    In queries involving multiple tables, the form of
C                    the FROM clause becomes
C
C                       FROM <table name> [<table alias>]
C                            [ , <table name> [<table alias>] ... ]
C
C                    The aliases associated with the table names must
C                    be distinct and must not be the actual names of
C                    loaded EK tables.
C
C                    Queries involving multiple tables are called
C                    `joins'.
C
C                    The meaning of a FROM clause containing multiple
C                    tables is that the output is to be a subset of
C                    the rows of the Cartesian product of the listed
C                    tables.  Normally, WHERE clause constraints are
C                    supplied to reduce the selected rows to a set of
C                    interest.
C
C                    The most common example of a join is a query with
C                    two tables listed in the FROM clause, and a WHERE
C                    clause constraint enforcing equality of members
C                    of a column in the first table with members of
C                    column in the second table.  Such a query is
C                    called an `equi-join'.  A join in which columns
C                    of different tables are related by an inequality
C                    is called a `non-equi-join'.  Any type of join
C                    other than an equi-join may be very slow to
C                    evaluate, due to the large number of elements that
C                    may be contained in the Cartesian
C                    product of the listed tables.
C
C                    The WHERE clause lists constraints that must
C                    be met by each row satisfying the query.  The
C                    constraints are specified as a logical combination
C                    of relational expressions.  The form of the
C                    constraint list is
C
C                       WHERE <constraint expression>
C
C                    where each <constraint expression> consists of one
C                    or more simple relational expressions of the form
C
C                       <column name> <operator> <RHS symbol>
C
C                    where
C
C                       <RHS symbol>
C
C                    is a column name, a literal value, or the special 
C                    symbol
C 
C                       NULL              
C
C                    and
C
C                       <operator>
C
C                    is any of
C
C                       EQ, GE, GT, LE, LIKE, LT, NE, NOT LIKE, <, <=,
C                       =, >, >=, !=, <>
C
C                    For comparison with null values, the special
C                    syntaxes
C
C                       <column name> IS NULL
C                       <column name> IS NOT NULL
C
C                    are allowed, in addition to the standard 
C                    comparison syntaxes using the equality or 
C                    inequality operators.
C
C                    The LIKE operator allows comparison of a string
C                    value against a template.  The template syntax
C                    is that allowed by the SPICELIB routine MATCHI.
C                    Templates may include literal characters, the
C                    wild string marker '*', and the wild character
C                    marker '%'.  Case is significant in templates.
C
C                    Templates are bracketed by quote characters, just
C                    as are literal strings.
C
C                    The query language also supports the BETWEEN and
C                    NOT BETWEEN constructs
C
C                       <column> BETWEEN <symbol 1> AND <symbol 2>
C
C                       <column> NOT BETWEEN <symbol 1> AND <symbol 2>
C
C                    The tokens
C
C                       <symbol 1>
C                       <symbol 2>
C
C                    may be literal values or column names.
C
C                    The BETWEEN operator considers values that match
C                    the bounds to satisfy the condition:  the BETWEEN
C                    operator tests for inclusion in the closed interval
C                    defined by the bounds.
C
C                    In the WHERE clause, simple relational expressions
C                    may be combined using the logical operators AND,
C                    OR, and NOT, as in the Fortran programming
C                    language.  Parentheses may be used to enforce a
C                    desired order of evaluation of logical expressions.
C
C                    The expression syntax is NOT symmetric:  literal
C                    values must not appear on the left hand side of the
C                    operators that apply to them.
C
C                    The columns named in a constraint clause must
C                    belong to the tables listed in the FROM clause.
C                    If the query is a join, qualifying table names or
C                    aliases are required wherever their omission would
C                    result in ambiguity.
C
C                    Data types of the columns or constants used on the
C                    right-hand-sides of operators must match the data
C                    types of the corresponding columns on the
C                    left-hand-sides, except that comparison of integer
C                    and double precision quantities is permitted.
C
C                    Literal strings used in constraints are always
C                    bracketed by quotes.  Either single  quotes (')
C                    or double quotes (") may be used, but the same
C                    quote character must be used to start and end any
C                    literal string. Within character string values,
C                    quote characters must be doubled in order to be
C                    recognized.  Case is significant in character
C                    except in comparisions using the LIKE and NOT LIKE
C                    operators, which ignore case:  the expression
C
C                       ANIMAL LIKE "*A*"
C
C                    would be considered true when ANIMAL takes the
C                    value
C
C                       "cat"
C
C                    Time values are considered to be strings and
C                    require bracketing quotes.  Currently, the
C                    only time values allowed are UTC times in ISO
C                    format, UTC times represented in forms accepted by
C                    the SPICELIB routine TPARSE, and SCLK strings in
C                    NAIF format.
C
C                    The ORDER BY clause indicates which columns to
C                    use to order the output generated by the query.
C                    The columns in the ORDER BY clause define a
C                    dictionary ordering, with the first listed column
C                    acting as a primary key, the second column acting
C                    as a secondary key, and so on.
C
C                    For each ORDER BY column, the keywords ASC or DESC
C                    may be supplied to indicate whether the items in
C                    that column are to be listed in ascending or
C                    descending order.  Ascending order is the default.
C                    The direction in which data items increase is
C                    referred to as the `order sense'.
C
C                    The ORDER BY clause, if present, must appear
C                    last in the query.
C
C                    The form of the ORDER BY clause is
C
C                       ORDER BY <column name> [<order sense>]
C                                [ ,<column name> [<order sense>]...]
C
C                    Rows satisfying the query constraints will be
C                    returned so that the entries of the first column
C                    specified in the ORDER BY clause will be appear in
C                    the order specified by the order sense keyword,
C                    which is assumed to be ASC if absent.  When entries
C                    in the first through Nth ORDER BY column are equal,
C                    the entries in the (N+1)st ORDER BY column
C                    determine the order of the rows, and so on.
C
C                    As in the WHERE clause, column names must be
C                    qualified by table names or table aliases where
C                    they would otherwise be ambiguous.
C
C                    The query language is word-oriented, and some
C                    indicate whether the words are reserved.  Reserved
C                    words must be separated from other words by white
C                    space.  It is not necessary to use white space
C                    to separate words and punctuation characters.
C                    The list of reserved words is
C
C                       AND
C                       BETWEEN
C                       BY
C                       COLUMN
C                       EQ
C                       FROM
C                       GE
C                       GT
C                       IS
C                       LE
C                       LT
C                       LIKE
C                       NE
C                       NOT
C                       NULL
C                       OR
C                       ORDER
C                       SELECT
C                       WHERE
C
C                    The left and right parenthesis characters are also
C                    reserved; they may not be used in queries outside
C                    of quoted strings.
C
C                    Case is not significant in queries, except within
C                    literal strings.
C
C$ Detailed_Output
C
C     NMROWS         is the number of rows that match the query
C                    criteria.  NMROWS is defined if and only if
C                    ERROR is returned .FALSE.
C
C     ERROR          is a logical flag indicating whether the query
C                    failed to parse correctly.
C
C     ERRMSG         is a character string that describes EKFIND's
C                    diagnosis of a parse error, should one occur.
C                    Otherwise, ERRMSG will be returned blank.
C
C$ Parameters
C
C     See the include files.
C
C$ Exceptions
C
C     1)  Most of the exceptions that can occur on a call to
C         EKFIND are caused by errors in the input query.  EKFIND
C         attempts to diagnose these via the output error flag and
C         error message, instead of signalling errors.  The following
C         classes of errors are detected:
C
C            Scanning errors---these result from badly formed query
C            in which EKFIND could not identify all of the tokens.
C            When these errors occur, EKFIND may be too confused to
C            give a helpful diagnostic message.
C
C            Parsing errors---these result from a badly formed
C            query that EKFIND was able to separate into tokens
C            but that EKFIND determined to be syntactically invalid:
C
C            Name resolution errors---these result from referencing
C            invalid or ambiguous column or table names in a query.
C
C            Time resolution errors---these result from use of time
C            strings that cannot be parsed.
C
C            Semantic errors---these result from a syntactically
C            valid query that violates a limit or a restriction on
C            values used in a query.
C
C
C     Some problems with queries are not trapped by EKFIND but
C     instead cause errors to be signalled.  These are listed below.
C
C
C     2)  If no E-kernels are loaded at the time this routine is called,
C         an error will be signalled by routines called by this routine.
C
C     3)  If a leapseconds kernel is is not loaded before this routine
C         is called, UTC time values may not be used in queries.  If
C         they are, an error will be signalled by routines called by
C         this routine.
C
C     4)  If an SCLK kernel for the appropriate spacecraft clock
C         has not been loaded before this routine is called, SCLK
C         values for that clock may not be used in queries.  If
C         they are, an error will be signalled by routines called by
C         this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine operates almost entirely by side effects:  it
C     prepares the EK fetch routines to return event data that
C     satisfy the input query.  See the header of the routine
C     EKQMGR or the EK Required Reading for examples of use of this
C     routine in conjunction with the EK fetch routines.
C
C$ Examples
C
C     1)  Examples of strings containing syntactically valid queries:
C
C            SELECT COL1 FROM TAB1
C
C            select col1 from tab1 where col1 gt 5
C
C            SELECT COL2 FROM TAB1 WHERE COL2 > 5.7D0 ORDER BY COL2
C
C            SELECT COL2 FROM TAB1 WHERE COL1 != 5
C
C            SELECT COL2 FROM TAB1 WHERE COL1 GE COL2
C
C            SELECT COL1, COL2, COL3 FROM TAB1 ORDER BY COL1
C
C            SELECT COL3 FROM TAB1 WHERE COL5 EQ "ABC"
C
C            SELECT COL3 FROM TAB1 WHERE COL5 = 'ABC'
C
C            SELECT COL3 FROM TAB1 WHERE COL5 LIKE 'A*'
C
C            SELECT COL3 FROM TAB1 WHERE COL5 LIKE 'A%%'
C
C            SELECT COL4 FROM TAB1 WHERE COL4 = '1995 JAN 1 12:38:09.7'
C
C            SELECT COL4 FROM TAB1 WHERE COL4 = "1995 JAN 1 12:38:09.7"
C
C            SELECT COL4 FROM TAB1 WHERE
C            COL4 NE 'GLL SCLK 02724646:67:7:2'
C
C            SELECT COL1 FROM TAB1 WHERE COL1 != NULL
C
C            SELECT COL1 FROM TAB1 WHERE COL1 IS NULL
C
C            SELECT COL1 FROM TAB1 WHERE COL1 IS NOT NULL
C
C            SELECT COL1, COL2, COL3 FROM TAB1
C            WHERE (COL1 BETWEEN 4 AND 6) AND (COL3 NOT LIKE "A%%")
C            ORDER BY COL1, COL3
C
C            SELECT COL4 FROM TAB1
C            WHERE COL4 BETWEEN "1995 JAN 1 12:38" AND
C            "October 23, 1995"
C
C            SELECT COL1, COL2 FROM TAB1 WHERE
C            NOT (    ( ( COL1 <  COL2 ) AND ( COL1 > 5   ) )  OR
C                     ( ( COL1 >= COL2 ) AND ( COL2 <= 10 ) )      )
C
C
C            SELECT T1.COL1, T1.COL2, T2.COL2, T2.COL3
C            FROM TABLE1 T1, TABLE2 T2
C            WHERE T1.COL1 = T2.COL1
C            AND T1.COL2 > 5
C            ORDER BY T1.COL1, T2.COL2
C
C
C     2)  Examples of syntactically invalid queries:
C
C            SELECT TIME WHERE TIME
C            LT 1991 JAN 1                      {FROM clause is absent}
C
C            select time from table1 where
C            time lt 1991 jan 1                 {time string is not
C                                                quoted}
C
C            select time from table1
C            where time .lt. '1991 jan 1'       {operator should be lt}
C
C            select cmd from table1
C            where "cmd,6tmchg" != cmd          {value is on left side
C                                                of operator}
C
C            select event_type from table1
C            where event_type eq ""             {quoted string is empty
C                                                ---use " " to indicate
C                                                a blank string}
C
C            select event_type from table1
C            where event_type = "COMMENT"
C            order TIME                         {ORDER BY phrase is
C                                                lacking BY keyword}
C
C            select COL1 from table where
C            where COL1 eq MOC_EVENT            {literal string on
C                                                right-hand-side of
C                                                operator is not quoted}
C
C
C
C         In the following examples, we'll assume that the program
C         calling EKFIND has loaded an EK containing two segments
C         having columns having the following names and attributes:
C
C
C          TABLE1:
C          ==========
C
C            Column name        Data type         Size       Indexed?
C            -----------        ---------         ----       --------
C            EVENT_TYPE         CHARACTER*32      1          YES
C            EVENT_PARAMETERS   CHARACTER*(*)     1          NO
C            COMMENT            CHARACTER*80      VARIABLE   NO
C
C
C          TABLE2:
C          ==========
C
C            Column name        Data type         Size       Indexed?
C            -----------        ---------         ----       --------
C            EVENT_TYPE         CHARACTER*32      1          YES
C            EVENT_PARAMETERS   CHARACTER*80      1          NO
C            COMMENT            CHARACTER*80      VARIABLE   NO
C            COMMAND            CHARACTER*80      1          YES
C
C
C         Then the following queries are semantically invalid:
C
C            SELECT EVENT_PARAMETERS
C            FROM TABLE1
C            WHERE EVENT_DURATION = 7.0         {No column called
C                                                EVENT_DURATION
C                                                is present in a loaded
C                                                EK}
C
C            SELECT COMMENT FROM TABLE2
C            WHERE COMMENT EQ "N/A"             {The COMMENT column does
C                                                not have size 1 and
C                                                therefore cannot be
C                                                referenced in a query}
C
C$ Restrictions
C
C     1)  A leapseconds kernel must be loaded before this routine may
C         be called, if UTC time values are used in input queries.
C
C     2)  An appropriate SCLK kernel must be loaded before this routine
C         may be called, if SCLK values are used in input queries.
C
C     3)  Data found in response to a query become unavailable
C         when a fast load is initiated via EKIFLD.  Any desired
C         fetches of the data must be performed before a fast
C         load or any other operation that modifies the EK scratch
C         area is initiated.  
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
C-    SPICELIB Version 1.0.4, 18-MAY-2010 (BVS)
C
C        Removed "C$" marker from text in the header.
C
C-    SPICELIB Version 1.0.3, 19-DEC-2001 (NJB)
C
C        Restrictions section was updated.
C
C-    SPICELIB Version 1.0.2, 14-JAN-1997 (NJB)
C
C        Syntax descriptions for comparisons using null values have been
C        added.  The $Examples section was augmented with sample queries
C        demonstrating use of the IS NULL and IS NOT NULL comparison 
C        operators.
C
C-    SPICELIB Version 1.0.1, 16-AUG-1996 (NJB)
C
C        Exceptions section of header was updated to indicate that
C        calling this routine while no E-kernels are loaded will cause
C        an error to be signalled.  Previous version line was changed
C        from "Beta" to "SPICELIB," and the previous version was
C        corrected to 1.0.0.
C
C-    SPICELIB Version 1.0.0, 24-OCT-1995 (NJB)
C
C-&
 
 
C$ Index_Entries
C
C     find EK data
C     issue EK query
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
 
C
C     Local parameters
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
C
C     Storage limits:
C
      INTEGER               MAXCOL
      PARAMETER           ( MAXCOL = 100 )
 
 
C
C     Local variables
C
      CHARACTER*(MAXCLN)    CHRBUF
      CHARACTER*(MAXCLN)    EQRYC
 
      DOUBLE PRECISION      EQRYD  ( MAXQNM )
      DOUBLE PRECISION      NUMVLS ( MAXQNM )
 
      INTEGER               CHBEGS ( MAXTOK )
      INTEGER               CHENDS ( MAXTOK )
      INTEGER               EQRYI  ( LBCELL : EQIMIN )
      INTEGER               ERRPTR
      INTEGER               LXBEGS ( MAXTOK )
      INTEGER               LXENDS ( MAXTOK )
      INTEGER               NTOKEN
      INTEGER               TOKENS ( MAXTOK )
      INTEGER               VALUES ( MAXTOK )
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKFIND' )
      END IF
 
C
C     Initialize the encoded query each time, for safety.
C
      CALL ZZEKQINI ( EQIMIN, MAXQNM, EQRYI, EQRYC, EQRYD )
 
C
C     Find the tokens in the input query.
C
      CALL ZZEKSCAN ( QUERY,  MAXTOK, MAXQNM, NTOKEN, TOKENS,
     .                LXBEGS, LXENDS, VALUES, NUMVLS, CHRBUF,
     .                CHBEGS, CHENDS, ERROR,  ERRMSG         )
 
      IF ( ERROR ) THEN
         CALL CHKOUT ( 'EKFIND' )
         RETURN
      END IF
 
 
C
C     Now parse the query.
C
      CALL ZZEKPARS (  QUERY,   NTOKEN,  LXBEGS,  LXENDS,
     .                 TOKENS,  VALUES,  NUMVLS,  CHRBUF,
     .                 CHBEGS,  CHENDS,  EQRYI,   EQRYC,
     .                 EQRYD,   ERROR,   ERRMSG          )
 
      IF ( ERROR ) THEN
         CALL CHKOUT ( 'EKFIND' )
         RETURN
      END IF
 
 
C
C     Resolve names.
C
      CALL ZZEKNRES ( QUERY, EQRYI, EQRYC,
     .                       ERROR, ERRMSG, ERRPTR )
 
      IF ( ERROR ) THEN
         CALL CHKOUT ( 'EKFIND' )
         RETURN
      END IF
 
 
C
C     Resolve time values, if necessary.
C
      CALL ZZEKTRES ( QUERY, EQRYI,  EQRYC,  EQRYD,
     .                       ERROR,  ERRMSG, ERRPTR  )
 
      IF ( ERROR ) THEN
         CALL CHKOUT ( 'EKFIND' )
         RETURN
      END IF
 
 
C
C     Perform semantic checks.
C
      CALL ZZEKSEMC ( QUERY, EQRYI, EQRYC,
     .                       ERROR, ERRMSG, ERRPTR )
 
      IF ( ERROR ) THEN
         CALL CHKOUT ( 'EKFIND' )
         RETURN
      END IF
 
C
C     If we arrived here, the encoded query is ready for execution.
C     Find the data satisfying the constraints.
C
      CALL EKSRCH ( EQRYI, EQRYC, EQRYD, NMROWS, ERROR, ERRMSG )
 
 
      CALL CHKOUT ( 'EKFIND' )
      RETURN
      END
