C$Procedure      ZZEKNRML ( EK, normalize WHERE clause )
 
      SUBROUTINE ZZEKNRML ( QUERY,   NTOKEN,  LXBEGS,  LXENDS,  TOKENS,
     .                      VALUES,  NUMVLS,  CHRBUF,  CHBEGS,  CHENDS,
     .                      EQRYI,   EQRYC,   EQRYD,   ERROR,   PRSERR )
 
C$ Abstract
C
C     Convert the WHERE clause of an EK query to a normalized form.
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
 
      INCLUDE 'ekkeyw.inc'
      INCLUDE 'ekopcd.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'ekquery.inc'
      INCLUDE 'ektokn.inc'
      INCLUDE 'ektype.inc'
 
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
C     QUERY      I   Input EK query.
C     NTOKEN     I   Number of tokens in query.
C     LXBEGS     I   Start positions of lexemes comprising WHERE clause.
C     LXENDS     I   End positions of lexemes comprising WHERE clause.
C     TOKENS     I   Tokens comprising query.
C     VALUES     I   Values associated with tokens.
C     NUMVLS     I   Buffer containing numeric token values.
C     CHRBUF     I   Buffer containing string token values.
C     CHBEGS,
C     CHENDS     I   String token begin and end character positions.
C     EQRYI,
C     EQRYC,
C     EQRYD      O   Parsed query and string and number value buffers.
C     ERROR      O   Parse error flag.
C     PRSERR     O   Parse error message.
C
C$ Detailed_Input
C
C     QUERY          is an EK query to be parsed.  The tokens of the
C                    query have been found already.  See the header
C                    of the subroutine EKFIND for a detailed
C                    description of the EK query language.
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
C$ Detailed_Output
C
C     EQRYI,
C     EQRYC,
C     EQRYD          are the integer, character, and numeric portions
C                    of an encoded form of the input query.  The WHERE
C                    clause of the input query is represented in this
C                    encoding.  The WHERE clause constraints have been
C                    normalized.
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
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     No matter how ridiculous the query passed to ZZEKNRML, the
C     routine diagnoses errors via the output arguments ERROR and
C     PRSERR.  No errors are signalled.  The possible error messages
C     returned by this routine are:
C
C
C        Conjunction table is full.
C
C        Disjunction table is full.
C
C        Empty WHERE clause.
C
C        Missing WHERE keyword.
C
C        More tokens expected.
C
C        NULL values are not allowed in BETWEEN or NOT BETWEEN clauses.
C
C        NULL values can only be used with the operators
C        "IS NULL", "NOT NULL" or equivalents.
C
C        Relation table is full.
C
C        Stack is full.
C
C        Syntax error:  badly formed WHERE clause.
C
C        Token following BETWEEN operator is invalid.
C
C        Token following NOT operator was invalid.
C
C        Token must be followed by a comparison operator.
C
C        Token must be followed by the AND operator.
C
C        Token sequence must be followed by a value.
C
C        Tokens were missing from comparison relation.
C
C        Tokens were missing from logical expression.
C
C        Too few tokens in WHERE clause.
C
C        Too many tokens in query; max allowed is #.
C
C        Unexpected keyword # found at location #.
C
C        Unexpected right parenthesis found.
C
C        Unexpected token # found at location #.
C
C        Unexpected token found following valid expression.
C
C        Unexpected token found.
C
C        WHERE clause ran out of tokens unexpectedly.
C        This may be due to an extra left parenthesis.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Here is the grammar for the EK WHERE clause:
C
C        <WHERE clause>           =>   WHERE <relational expression>
C
C
C        <relational expression>  =>   <simple expression>
C
C                                      <NULL value expression>
C
C                                    | NOT <relational expression>
C
C                                    |   ( <relational expression> )
C
C                                    |     <relational expression>
C                                      AND <relational expression>
C
C                                    |     <relational expression>
C                                      OR  <relational expression>
C
C
C        <simple expression>      =>   <LHS> <operator> <RHS>
C
C                                    | <LHS> BETWEEN     <RHS> AND <RHS>
C
C                                    | <LHS> NOT BETWEEN <RHS> AND <RHS>
C
C
C        <NULL value expression>  =>   <LHS> <Null operator> NULL
C
C
C        <LHS>                    =>   <name>
C
C
C        <RHS>                    =>   <name>
C                                    | <value>
C
C
C        <name>                   =>   <identifier> . <identifier>
C                                    | <identifier>
C
C
C        <operator>               =>   EQ
C                                    | GE
C                                    | GT
C                                    | LE
C                                    | LT
C                                    | NE
C                                    | LIKE
C                                    | NOT LIKE
C                                    | =
C                                    | >=
C                                    | >
C                                    | <=
C                                    | <
C                                    | !=
C                                    | <>
C
C
C        <NULL operator>         =>    IS
C                                    | IS NOT
C                                    | EQ
C                                    | NE
C                                    | =
C                                    | !=
C                                    | <>
C
C
C        <value>                 =>    <character value>
C                                    | <d.p. value>
C                                    | <integer value>
C
C$ Examples
C
C     1)  This routine breaks down the constraints of the WHERE clause
C
C             WHERE        ( ( COL1 EQ VAL1 ) OR ( COL2 NE VAL2 ) )
C                     AND  ( ( COL3 LE VAL3 ) OR ( COL4 GT VAL4 ) )
C
C         as
C
C                          (  ( COL1 EQ VAL1 ) AND ( COL3 LE VAL3 ) )
C                     OR   (  ( COL1 EQ VAL1 ) AND ( COL4 GT VAL4 ) )
C                     OR   (  ( COL2 NE VAL2 ) AND ( COL3 LE VAL3 ) )
C                     OR   (  ( COL2 NE VAL2 ) AND ( COL4 GT VAL4 ) )
C
C
C
C     2)  This routine breaks down the constraints of the WHERE clause
C
C             WHERE  NOT ( ( COL1 EQ VAL1 ) OR ( COL2 NE VAL2 ) )
C
C          as
C                          ( COL1 NE VAL1 ) AND ( COL3 EQ VAL3 )
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
C-    Beta Version 3.0.0, 17-NOV-1995 (NJB)
C
C        Significantly re-written for architecture 3.
C
C-&
 
C
C
C     SPICELIB functions
C
      INTEGER               CARDI
      INTEGER               ISRCHI
      INTEGER               LNKHL
      INTEGER               LNKNFN
      INTEGER               LNKNXT
      INTEGER               LNKPRV
      INTEGER               LNKTL
 
      LOGICAL               RETURN
 
C
C
C     Local parameters
C
      INTEGER               NIL
      PARAMETER           ( NIL   =  0 )
 
C
C     Data structure bounds:
C
 
C
C     MAXREL is the maximum number of relations that can be handled
C     by this routine.
C
      INTEGER               MAXREL
      PARAMETER           ( MAXREL =   10*MAXTOK  )
 
C
C     MAXMET is the maximum number of meta-tokens making up any
C     expression.
C
      INTEGER               MAXMET
      PARAMETER           ( MAXMET =   10*MAXTOK )
 
C
C     LBPOOL is the lower bound of the second index of a linked list
C     pool array.
C
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )
 
C
C
C     Stack parameters:
C
C
      INTEGER               MAXSTK
      PARAMETER           ( MAXSTK = 500 )
 
 
C
C     Operator parameters:
C
C
C     NLOGOP is the number of recognized logical operators.  These
C     are AND, OR, and NOT.
C
      INTEGER               NLOGOP
      PARAMETER           ( NLOGOP =  3 )
 
C
C     NRELOP is the number of arithmetic and character comparision
C     operators.
C
      INTEGER               NRELOP
      PARAMETER           ( NRELOP = 8 )
 
C
C
C     Meta-token codes, excluding codes for relational operators:
C
      INTEGER               RGROUP
      PARAMETER           ( RGROUP = -1 )
 
      INTEGER               LGROUP
      PARAMETER           ( LGROUP = RGROUP - 1 )
 
      INTEGER               BTWEEN
      PARAMETER           ( BTWEEN = LGROUP - 1 )
 
      INTEGER               NOTBTW
      PARAMETER           ( NOTBTW = BTWEEN - 1 )
 
      INTEGER               BTWEXP
      PARAMETER           ( BTWEXP = NOTBTW - 1 )
 
      INTEGER               NAME
      PARAMETER           ( NAME   = BTWEXP - 1 )
 
      INTEGER               IDENT
      PARAMETER           ( IDENT  = NAME   - 1 )
 
      INTEGER               VALUE
      PARAMETER           ( VALUE  = IDENT  - 1 )
 
      INTEGER               PERIOD
      PARAMETER           ( PERIOD = VALUE  - 1 )
 
      INTEGER               AND
      PARAMETER           ( AND    = PERIOD - 1 )
 
      INTEGER               OR
      PARAMETER           ( OR     = AND    - 1 )
 
      INTEGER               NOT
      PARAMETER           ( NOT    = OR     - 1 )
 
      INTEGER               EXPR
      PARAMETER           ( EXPR   = NOT    - 1 )
 
C
C     Number of keywords that can terminate a WHERE clause.
C
      INTEGER               NENDKW
      PARAMETER           ( NENDKW = 3 )
 
C
C
C     State parameters:
C
      INTEGER               PUSH
      PARAMETER           ( PUSH   =          0 )
 
      INTEGER               POP
      PARAMETER           ( POP    = PUSH   + 1 )
 
      INTEGER               PARSE
      PARAMETER           ( PARSE  = POP    + 1 )
 
      INTEGER               REDUCD
      PARAMETER           ( REDUCD = PARSE  + 1 )
 
      INTEGER               TERM
      PARAMETER           ( TERM   = REDUCD + 1 )
 
 
C
C
C     'Pop condition' codes:
C
      INTEGER               NONE
      PARAMETER           ( NONE   = 0 )
 
      INTEGER               REDUCE
      PARAMETER           ( REDUCE = 1 )
 
      INTEGER               ENDGRP
      PARAMETER           ( ENDGRP = REDUCE + 1 )
 
C
C     Token descriptor size:
C
      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ = EQVDSZ + 1 )
 
C
C     Local variables
C
 
C
C     Each comparison relation is expressed by three tokens, so the
C     comparison relations are represented by a 3 x MAXREL array.  The
C     first and third elements of each row of RELS are array indices
C     that point into the input array TOKENS; the middle element
C     of each row is an operator code.  The set of triples representing
C     comparison relations is indexed by a doubly linked list pool.
C     Each conjunction of comparison relations is represented by a
C     linked list of pointers to entries in the RELS array.  These
C     pointers are contained in the CJPTRS array.  The pointers are
C     linked via entries in the double linked list pool CJPOOL.
C
      INTEGER               RELS   ( 3,  MAXREL          )
      INTEGER               RLPOOL ( 2,  LBPOOL : MAXREL )
 
      INTEGER               CJPOOL ( 2,  LBPOOL : MAXREL )
      INTEGER               CJPTRS (              MAXREL )
 
C
C     Each normalized expression is a disjunction of conjunctions.  Each
C     such disjunction is represented by a linked list of nodes
C     associated with pointers to entries in the CJPOOL array.  DJPTRS
C     is the parallel array used to associate each node of a disjunction
C     with the head node of a conjunction list in CJPOOL.
C
      INTEGER               DJPOOL ( 2,  LBPOOL : MAXREL )
      INTEGER               DJPTRS (              MAXREL )
 
C
C     Meta-tokens are groups of tokens that comprise syntactic units
C     in a query.  Each symbol that appears on the left hand side of
C     a production rule in the grammar corresponds to a type of
C     meta-token.
C
C     Throughout the parsing process, the meta-tokens representing the
C     query are organized as a linked list.  Each meta-token is also
C     associated with a more detailed classification MTCODE.
C
C     For each meta-token that represents an identifier, a value,
C     a name, or an expression, there is a corresponding element of
C     MTEXPR.  This element contains a pointer to a token or to a
C     normalized expression.  In the latter case, the pointer is the
C     head node of a list in the disjunction table.
C
      INTEGER               MTPOOL ( 2,  LBPOOL : MAXTOK )
      INTEGER               MTCODE (              MAXTOK )
      INTEGER               MTEXPR (              MAXTOK )
 
C
C     Stack variables
C
C     These variables have the following meanings:
C
C        MSTART is the node number of the first meta-token of
C        the current expression being parsed.
C
C        NMETA is the number of meta-tokens in the query.
C
C        POPCND is the `pop condition'.  This is a code indicating
C        what event must occur to trigger popping the current state.
C        The two events that can cause the state to be popped are
C        the execution of a reduction and encountering a right grouper.
C
      INTEGER               MSTART ( MAXSTK )
      INTEGER               POPCND ( MAXSTK )
      INTEGER               NMETA
 
C
C
C     Other local variables
C
      INTEGER               B
      INTEGER               CJ(4)
      INTEGER               CJNODE
      INTEGER               COLPTR
      INTEGER               DJ(2)
      INTEGER               DJNODE
      INTEGER               DJTAIL
      INTEGER               DSPOOL ( 2, LBPOOL : MAXREL )
      INTEGER               DSCBUF ( DSCSIZ,     MAXREL )
      INTEGER               E
      INTEGER               ENDKW  ( NENDKW )
      INTEGER               ENDLOC
      INTEGER               EXPRHD
      INTEGER               FIRST
      INTEGER               FOURTH
      INTEGER               HEAD1
      INTEGER               HEAD2
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               LEVEL
      INTEGER               LXB
      INTEGER               LXE
      INTEGER               METAHD
      INTEGER               NCONJ
      INTEGER               NEWCJ
      INTEGER               NEWDJ
      INTEGER               NEWREL
      INTEGER               NEXT
      INTEGER               NODE
      INTEGER               NRELS
      INTEGER               OP
      INTEGER               PREV
      INTEGER               REL(4)
      INTEGER               RELPTR
      INTEGER               RELSET ( LBCELL : MAXREL )
      INTEGER               RETCND
      INTEGER               RHSPTR
      INTEGER               SECOND
      INTEGER               SKIP
      INTEGER               SIZES  ( MAXCON )
      INTEGER               START
      INTEGER               STATE
      INTEGER               TABPTR
      INTEGER               TAIL
      INTEGER               THIRD
      INTEGER               TYPE
      INTEGER               WHRBEG
      INTEGER               WHREND
      INTEGER               WHRSIZ
      INTEGER               CMPCDE ( NRELOP     )
      INTEGER               CMPNEG ( NRELOP     )
      INTEGER               CMPOPS ( NRELOP - 1 )
      INTEGER               LOGCDE ( NLOGOP     )
      INTEGER               LOGOPS ( NLOGOP     )
 
      LOGICAL               DONOW
      LOGICAL               FND
      LOGICAL               QUAL
 
C
C     Saved variables
C
      SAVE
 
C
C
C     Initial values
C
      DATA  (  LOGOPS(I), LOGCDE(I),  I = 1, NLOGOP )    /
     .
     .         KWAND,   AND,
     .         KWOR,    OR,
     .         KWNOT,   NOT   /
 
C
C     Note:  there is no "UNLIKE" keyword, but there is an UNLIKE
C     operator, which is the complement of the LIKE operator.
C
      DATA  (  CMPOPS(I), CMPCDE(I), CMPNEG(I), I = 1, NRELOP-1 )    /
     .
     .         KWEQ,      EQ,       NE,
     .         KWGE,      GE,       LT,
     .         KWGT,      GT,       LE,
     .         KWLE,      LE,       GT,
     .         KWLT,      LT,       GE,
     .         KWNE,      NE,       EQ,
     .         KWLIKE,    LIKE,     UNLIKE      /
 
      DATA     CMPCDE(NRELOP), CMPNEG(NRELOP)   /
     .                    UNLIKE,   LIKE        /
 
 
      DATA     ENDKW   /  KWFROM, KWORDR, KWSEL /
 
C
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKNRML' )
      END IF
 
C
C     No error at this point.
C
      ERROR  = .FALSE.
      PRSERR = ' '
 
      IF ( NTOKEN .GT. MAXTOK ) THEN
         ERROR  = .TRUE.
         PRSERR = 'Too many tokens in query; max allowed is #.'
         CALL REPMI  ( PRSERR, '#', MAXTOK, PRSERR )
         CALL CHKOUT ( 'ZZEKNRML'                  )
         RETURN
      END IF
 
C
C     Find out the start and end indices of the tokens comprising the
C     WHERE clause.  If there are no tokens in the WHERE clause, we may
C     as well go home.
C
      CALL ZZEKTLOC ( TKKEY,   KWWHER,  NTOKEN,  TOKENS,  VALUES,
     .                WHRBEG,  FND                                )
 
      WHRBEG  =  WHRBEG + 1
 
      IF ( .NOT. FND ) THEN
         ERROR   =  .TRUE.
         PRSERR  =  'Missing WHERE keyword.'
         CALL CHKOUT ( 'ZZEKNRML' )
         RETURN
      END IF
 
C
C     The WHERE clause is terminated by the end of the query or by
C     the first keyword of the set {SELECT, FROM, ORDER} that follows
C     the WHERE keyword.
C
      WHREND  =  NTOKEN
 
      DO I = 1, 3
 
         CALL ZZEKTLOC ( TKKEY,   ENDKW(I),  NTOKEN,  TOKENS,  VALUES,
     .                   ENDLOC,  FND                                 )
 
         IF ( FND ) THEN
 
            IF (       ( ENDLOC .LT. WHREND )
     .           .AND. ( ENDLOC .GT. WHRBEG )  ) THEN
 
               WHREND  =  ENDLOC-1
            END IF
 
         END IF
 
      END DO
 
 
      WHRSIZ  =  WHREND  -  WHRBEG  +  1
 
      IF ( WHRSIZ .EQ. 0 ) THEN
         ERROR   =  .TRUE.
         PRSERR  =  'Empty WHERE clause.'
         CALL CHKOUT ( 'ZZEKNRML' )
         RETURN
      END IF
 
C
C     Initialize the pools.
C
      CALL LNKINI ( MAXREL, RLPOOL )
      CALL LNKINI ( MAXREL, CJPOOL )
      CALL LNKINI ( MAXREL, DJPOOL )
      CALL LNKINI ( MAXTOK, MTPOOL )
      CALL LNKINI ( MAXREL, DSPOOL )
 
C
C     Loop through our token list and classify the tokens.  Initialize
C     the meta-token list.
C
      NMETA  =  0
      TAIL   =  NIL
 
      I      =  WHRBEG
 
      DO WHILE ( I .LE. WHREND )
C
C        Allocate a node and link it in at the tail of the meta-token
C        list.
C
         CALL LNKAN  ( MTPOOL, NODE         )
         CALL LNKILA ( TAIL,   NODE, MTPOOL )
         TAIL         = NODE
 
C
C        Each meta-token's expression pointer points to its original
C        token index, by default.
C
         MTEXPR(NODE) = I
 
 
         IF ( TOKENS(I) .EQ. TKLPAR  ) THEN
 
            MTCODE(NODE) = LGROUP
 
 
         ELSE IF ( TOKENS(I) .EQ. TKRPAR ) THEN
 
            MTCODE(NODE) = RGROUP
 
 
         ELSE IF (       ( TOKENS(I) .EQ. TKINT  )
     .             .OR.  ( TOKENS(I) .EQ. TKDP   )  ) THEN
C
C           Numeric values must be added to the encoded query.  We
C           allocate a descriptor from the descriptor pool for
C           each identifier.  The expression pointer for the
C           identifier points to the descriptor.  Note:  the
C           allocation should be safe, since we've checked the total
C           number of tokens in the query.
C
            MTCODE(NODE) = VALUE
 
            CALL LNKAN ( DSPOOL, MTEXPR(NODE) )
 
            IF ( TOKENS(I) .EQ. TKINT ) THEN
               TYPE = INT
            ELSE
               TYPE = DP
            END IF
 
            CALL ZZEKINQN ( NUMVLS(VALUES(I)),
     .                      TYPE,
     .                      LXBEGS(I),
     .                      LXENDS(I),
     .                      EQRYI,
     .                      EQRYD,
     .                      DSCBUF( 1, MTEXPR(NODE) )  )
 
C
C           Set the descriptor to indicate that it represents a value.
C
            DSCBUF( DSCSIZ, MTEXPR(NODE) )  =  VALUE
 
 
         ELSE IF ( TOKENS(I) .EQ. TKQSTR ) THEN
C
C           The treatment of strings is analogous to that of numbers.
C
            MTCODE(NODE) = VALUE
 
            CALL LNKAN ( DSPOOL, MTEXPR(NODE) )
 
            B  =  CHBEGS( VALUES(I) )
            E  =  CHENDS( VALUES(I) )
 
            CALL ZZEKINQC (  CHRBUF(B:E),
     .                       E - B + 1,
     .                       LXBEGS(I),
     .                       LXENDS(I),
     .                       EQRYI,
     .                       EQRYC,
     .                       DSCBUF( 1, MTEXPR(NODE) )  )
 
C
C           Set the descriptor to indicate that it represents a value.
C
            DSCBUF( DSCSIZ, MTEXPR(NODE) )  =  VALUE
 
 
         ELSE IF ( TOKENS(I) .EQ. TKID ) THEN
C
C           Identifiers must be added to the encoded query.  We
C           allocate a descriptor from the descriptor pool for
C           each identifier.  The expression pointer for the
C           identifier points to the descriptor.
C
            MTCODE(NODE) = IDENT
 
            CALL LNKAN ( DSPOOL, MTEXPR(NODE) )
 
            B  =  CHBEGS( VALUES(I) )
            E  =  CHENDS( VALUES(I) )
 
            CALL ZZEKINQC (  CHRBUF(B:E),
     .                       E - B + 1,
     .                       LXBEGS(I),
     .                       LXENDS(I),
     .                       EQRYI,
     .                       EQRYC,
     .                       DSCBUF( 1, MTEXPR(NODE) )  )
 
C
C           Set the descriptor to indicate that it represents an
C           identifier.
C
            DSCBUF( DSCSIZ, MTEXPR(NODE) )  =  IDENT
 
 
         ELSE IF ( TOKENS(I) .EQ. TKDOT ) THEN
 
            MTCODE(NODE) = PERIOD
 
 
         ELSE IF ( TOKENS(I) .EQ. TKKEY  ) THEN
C
C           We have a keyword.  Identify it and locate the corresponding
C           code.
C
            J  =  ISRCHI ( VALUES(I), NLOGOP,   LOGOPS )
            K  =  ISRCHI ( VALUES(I), NRELOP-1, CMPOPS )
 
 
            IF ( J .GT. 0 ) THEN
C
C              We have a logical operator, unless we have the NOT LIKE
C               or NOT BETWEEN sequence.
C
               IF ( LOGCDE(J) .NE. NOT ) THEN
 
                  MTCODE(NODE) = LOGCDE(J)
 
               ELSE
 
                  IF ( I .LE. WHREND ) THEN
 
                     IF (       ( TOKENS(I+1) .EQ. TKKEY  )
     .                    .AND. ( VALUES(I+1) .EQ. KWLIKE )  ) THEN
C
C                       Replace the NOT LIKE sequence with the
C                       UNLIKE operator.  Skip over the LIKE token.
C
                        MTCODE(NODE)  =  UNLIKE
                        I             =  I + 1
 
                     ELSE IF (       ( TOKENS(I+1) .EQ. TKKEY  )
     .                         .AND. ( VALUES(I+1) .EQ. KWBETW )  ) THEN
C
C                       Replace the NOT BETWEEN sequence with the
C                       NOTBTW operator.  Skip over the BETWEEN token.
C
                        MTCODE(NODE)  =  NOTBTW
                        I             =  I + 1
 
                     ELSE
                        MTCODE(NODE)  =  NOT
                     END IF
 
                  ELSE
                     MTCODE(NODE) = NOT
                  END IF
 
               END IF
 
 
            ELSE IF ( K .GT. 0 ) THEN
               MTCODE(NODE) = CMPCDE(K)
 
 
            ELSE IF ( VALUES(I) .EQ. KWBETW ) THEN
               MTCODE(NODE) = BTWEEN
 
 
            ELSE IF ( VALUES(I) .EQ. KWIS ) THEN
C
C              The token IS translates to EQ; the token sequence
C              IS NOT translates to NE.
C
               IF ( I .LT. WHREND ) THEN
 
                  IF (       ( TOKENS(I+1) .EQ. TKKEY )
     .                 .AND. ( VALUES(I+1) .EQ. KWNOT )  ) THEN
C
C                    We have an IS NOT sequence.  Skip over the NOT
C                    token; indicate the sequence with a single NE
C                    meta-token.
C
                     MTCODE(NODE) = NE
                     I            = I + 1
                  ELSE
                     MTCODE(NODE) = EQ
                  END IF
 
               ELSE
                  MTCODE(NODE) = EQ
               END IF
 
 
            ELSE IF ( VALUES(I) .EQ. KWNULL ) THEN
C
C              The expression pointer for null values is NIL.
C
               MTCODE(NODE) = VALUE
               MTEXPR(NODE) = NIL
 
            ELSE
C
C              Sorry, that was the last chance for valid keywords.
C
               LXB     =  LXBEGS(I)
               LXE     =  LXENDS(I)
               ERROR   =  .TRUE.
               PRSERR  =  'Unexpected keyword # found at location #.'
               CALL REPMC  ( PRSERR, '#', QUERY(LXB:LXE), PRSERR )
               CALL REPMI  ( PRSERR, '#', LXB,            PRSERR )
               CALL CHKOUT ( 'ZZEKNRML'                          )
               RETURN
 
            END IF
 
         ELSE
C
C           Sorry, that was the last chance, period.
C
            LXB     =  LXBEGS(I)
            LXE     =  LXENDS(I)
            ERROR   =  .TRUE.
            PRSERR  =  'Unexpected token # found at location #.'
            CALL REPMC  ( PRSERR, '#', QUERY(LXB:LXE), PRSERR )
            CALL REPMI  ( PRSERR, '#', LXB,            PRSERR )
            CALL CHKOUT ( 'ZZEKNRML'                          )
            RETURN
 
         END IF
C
C        At this point, we've classified the Ith token.  MTCODE(NODE)
C        is the meta-token code for this token.
C
         I     = I     + 1
         NMETA = NMETA + 1
 
      END DO
 
C
C     Initialize the head of the meta-token list.
C
      METAHD = LNKHL( TAIL, MTPOOL )
 
C
C     Filter out extraneous parentheses around column names or
C     values.
C
      NODE = METAHD
 
      DO WHILE ( NODE .GT. 0 )
 
         IF (      ( MTCODE(NODE) .EQ. NAME  )
     .        .OR. ( MTCODE(NODE) .EQ. VALUE )  )  THEN
C
C           If the current metatoken is bracketed by parentheses,
C           remove them and update the metatoken count accordingly.
C
            PREV  =  LNKPRV( NODE, MTPOOL )
            NEXT  =  LNKNXT( NODE, MTPOOL )
 
            IF (  ( PREV .GT. 0 ) .AND. ( NEXT .GT. 0 )  ) THEN
 
               IF (       ( MTCODE( PREV ) .EQ. LGROUP )
     .              .AND. ( MTCODE( NEXT ) .EQ. RGROUP )   ) THEN
 
                  CALL LNKFSL ( PREV, PREV, MTPOOL )
                  CALL LNKFSL ( NEXT, NEXT, MTPOOL )
                  METAHD  =   LNKHL ( NODE, MTPOOL )
                  NMETA   =   NMETA - 2
C
C                 We don't advance the current token in this case
C                 because there may be more parentheses to remove.
C
               ELSE
C
C                 This token is not bracketed by parentheses; look at
C                 the next metatoken.
C
                  NODE  =  NEXT
 
               END IF
 
            ELSE
C
C              This token is not bracketed by tokens on both sides; look
C              at the next metatoken.  It's ok for the next token to be
C              NIL.
C
               NODE  =  NEXT
 
            END IF
 
         ELSE
C
C           The current token is not a name or value; look at the next
C           token.
C
            NODE  =  LNKNXT( NODE, MTPOOL )
 
         END IF
 
      END DO
 
 
C
C
C     Now it's time to parse our expression.  We will validate the
C     expression by using our grammar rules to condense groups of
C     meta-tokens that correspond to the right-hand sides of grammatical
C     rules into meta-tokens that correspond to the left-hand sides
C     of those same rules.  Each such application of a grammar rule
C     is called a `reduction.'  When we're left with a single
C     meta-token of type <relational expression>, we're done.
C
C     If, before reaching the desired final state, we get to a point
C     where no reductions can be performed, we have a syntax error.
C
C     As parsing advances, we'll start to get meta-tokens that are
C     logical expressions.  Each logical expression will be represented
C     by a data structure that organizes the expression in a way that
C     we'll refer to as `normalized':  the expression will be
C     represented as a disjuction of conjunctions, for example
C
C        ( A AND B AND C ) OR ( D AND E ) OR ( F ) OR ( G AND H AND I )
C
C     Each metatoken that represents a logical expression will
C     refer to it through a pointer which is a member of the MTEXPR
C     array.
C
 
      IF ( WHRSIZ .LT. 3 ) THEN
 
         ERROR  = .TRUE.
         PRSERR = 'Too few tokens in WHERE clause.'
         CALL CHKOUT ( 'ZZEKNRML' )
         RETURN
 
      ELSE
 
         LEVEL          =  1
         MSTART(LEVEL)  =  METAHD
         POPCND(LEVEL)  =  NONE
         STATE          =  PARSE
 
      END IF
 
 
 
      DO WHILE ( STATE .NE. TERM )
 
 
         IF ( STATE .EQ. PARSE ) THEN
C
C           If the input query is valid, we're looking at the leftmost
C           meta-token of an expression that matches the right-hand
C           side of one of the grammar rules.  Referring back to the
C           rules, we see that there are only a few meta-tokens that are
C           valid as the first token of such an expression:
C
C              - A left grouper
C              - An identifier
C              - A name
C              - An expression
C              - A unary operator (`NOT' )
C
C           We'll see if we can perform a reduction.  The reductions
C           that are possible depend on how many meta-tokens are
C           present in the expression we're looking at.
C
C           FIRST is the node number of the first meta-token to look
C           at, in an attempt to perform a reduction.  SECOND, THIRD,
C           and FOURTH have the obvious meanings; some of these may
C           be 0.
C
            FIRST   =  MSTART ( LEVEL )
 
            IF ( FIRST .GT. 0 ) THEN
               SECOND  =  LNKNXT ( FIRST,  MTPOOL )
            ELSE
               SECOND  =  NIL
            END IF
 
            IF ( SECOND .GT. 0 ) THEN
               THIRD   =  LNKNXT ( SECOND, MTPOOL )
            ELSE
               THIRD   =  NIL
            END IF
 
            IF ( THIRD .GT. 0 ) THEN
               FOURTH  =  LNKNXT ( THIRD,  MTPOOL )
            ELSE
               FOURTH  =  NIL
            END IF
 
 
 
            IF ( FIRST .LE. 0 ) THEN
C
C              This never happens to good commands.
C
               ERROR  = .TRUE.
               PRSERR = 'WHERE clause ran out of tokens '            //
     .                  'unexpectedly.  This may be due to an '      //
     .                  'extra left parenthesis.'
               CALL CHKOUT ( 'ZZEKNRML' )
               RETURN
 
            END IF
 
C
C           We have at least one meta-token to work with.  We'll
C           take different actions depending on its type.
C
 
            IF ( MTCODE(FIRST) .EQ. IDENT ) THEN
C
C              This is a simple case to deal with:  in valid queries,
C              we have either the sequence
C
C                 <identifier> . <identifier>
C
C              or
C
C                 <identifier>
C
C              Both of these token sequences represent a column name;
C              in the former case, the name is qualified by a table
C              name, in the latter, the column name is unqualified.
C              If the table name is absent, we'll simply save a null
C              descriptor for it.  The descriptors will be linked, with
C              the table descriptor coming first, and the NAME token
C              resulting from reducing this token sequence will point to
C              the list of descriptors via the MTEXPR pointer.
C
C
               IF ( THIRD .GT. 0 ) THEN
C
C                 We can look at the following two tokens.
C
                  IF (       ( MTCODE(SECOND) .EQ. PERIOD )
     .                 .AND. ( MTCODE(THIRD)  .EQ. IDENT  )  ) THEN
 
                     QUAL  =  .TRUE.
                  ELSE
                     QUAL  =  .FALSE.
                  END IF
 
               ELSE
C
C                 There aren't enough tokens for this name to be
C                 qualified.
C
                  QUAL  =  .FALSE.
 
               END IF
 
 
               IF ( QUAL ) THEN
C
C                 We have a fully qualified column name.  Hook up the
C                 table and column name descriptors.
C
                  TABPTR  =  MTEXPR(FIRST)
                  COLPTR  =  MTEXPR(THIRD)
 
                  CALL LNKILA ( TABPTR, COLPTR, DSPOOL )
 
C
C                 Reduce the expression to a <name> metatoken.
C
                  MTCODE(FIRST)  =  NAME
 
                  CALL LNKFSL( SECOND, THIRD, MTPOOL )
 
                  NMETA          =  NMETA - 2
 
               ELSE
C
C                 We have an unqualified column name.  Allocate a table
C                 descriptor.  Set the table descriptor to indicate a
C                 null character descriptor.  Link this descriptor in
C                 before the column descriptor.
C
                  CALL LNKAN ( DSPOOL, TABPTR )
 
                  CALL CLEARI ( DSCSIZ, DSCBUF(1,TABPTR) )
                  DSCBUF(EQCTYP,TABPTR)  =  CHR
                  DSCBUF(DSCSIZ,TABPTR)  =  IDENT
 
                  COLPTR  =  MTEXPR(FIRST)
 
                  CALL LNKILA ( TABPTR, COLPTR, DSPOOL )
 
C
C                 Reduce the expression to a <name> metatoken.
C                 The reduction doesn't change the number of metatokens.
C
                  MTEXPR(FIRST)  =  TABPTR
                  MTCODE(FIRST)  =  NAME
 
               END IF
 
C
C              Decide the next state.
C
               STATE  =  REDUCD
 
 
 
            ELSE IF ( MTCODE(FIRST) .EQ. VALUE ) THEN
C
C              If the query is valid, the sequence of meta-tokens
C              should be one of
C
C                 <value>  AND  <name>
C                 <value>  AND  <value>
C
C              Both of these reduce to the symbol <BETWEEN expr>.
C
C
               IF (  THIRD .LE. 0 ) THEN
 
                  ERROR  = .TRUE.
                  PRSERR = 'Tokens were missing from comparison '   //
     .                     'relation.'
                  CALL CHKOUT ( 'ZZEKNRML' )
                  RETURN
 
               END IF
 
C
C              Null values are not allowed in BETWEEN expressions.
C
               IF (      ( MTEXPR(FIRST) .EQ. NIL )
     .              .OR. ( MTEXPR(THIRD) .EQ. NIL )  )  THEN
 
                  ERROR  = .TRUE.
                  PRSERR = 'NULL values are not allowed in BETWEEN ' //
     .                     'or NOT BETWEEN clauses.'
                  CALL CHKOUT ( 'ZZEKNRML' )
                  RETURN
 
               END IF
 
 
               IF ( MTCODE(THIRD)  .EQ. IDENT ) THEN
C
C                 We'll need to reduce the IDENT before proceeding.
C
                  START   =  THIRD
                  RETCND  =  REDUCE
                  STATE   =  PUSH
 
 
               ELSE IF (      (       MTCODE(SECOND) .EQ. AND      )
     .                  .AND. (     ( MTCODE(THIRD)  .EQ. NAME  )
     .                         .OR. ( MTCODE(THIRD)  .EQ. VALUE )  )   )
     .         THEN
C
C                 This sequence of tokens, when seen in the PARSE
C                 state, is a set of value bounds for a BETWEEN or
C                 NOT BETWEEN expression.  Note that this token sequence
C                 can occur elsewhere, but not in the PARSE state.
C                 This is because the meta-token sequences
C
C                    <value>  AND  <name>
C                    <value>  AND  <value>
C
C                 occur at the start of the RHS of only two
C                 productions, namely
C
C                    <BETWEEN expr>  =>  <value>  AND  <name>
C                    <BETWEEN expr>  =>  <value>  AND  <value>
C
C
C                 Hook up the name or value descriptors.
C
                  CALL LNKILB ( MTEXPR(FIRST), MTEXPR(THIRD), DSPOOL )
 
                  MTCODE(FIRST)  =  BTWEXP
 
                  CALL LNKFSL ( SECOND, THIRD, MTPOOL )
 
                  NMETA          =  NMETA - 2
 
C
C                 Decide the next state.
C
                  STATE  =  REDUCD
 
 
               ELSE IF ( MTCODE(SECOND) .GT. 0 ) THEN
C
C                 The third meta-token is in the wrong place at the
C                 wrong time.
C
                  ERROR  =  .TRUE.
                  PRSERR =  'Token sequence must be followed '      //
     .                      'by a value.'
 
                  CALL CHKOUT ( 'ZZEKNRML' )
                  RETURN
 
 
               ELSE
C
C                 The second meta-token is supposed to be the AND token,
C                 but it's actually something else.
C
                  ERROR  =  .TRUE.
                  PRSERR =  'Token must be followed by the AND ' //
     .                      'operator.'
 
                  CALL CHKOUT ( 'ZZEKNRML' )
                  RETURN
 
               END IF
 
 
 
 
            ELSE IF ( MTCODE(FIRST) .EQ. NAME ) THEN
C
C              If the query is valid, the sequence of meta-tokens
C              should be any of
C
C                 <name>  <comparison operator> <value>
C                 <name>  <comparison operator> <name>
C                 <name>  <comparison operator> <ident>
C
C              or
C
C                 <name>  AND  <name>
C                 <name>  AND  <value>
C                 <name>  AND  <ident>
C
C              or
C
C                 <name>  BETWEEN  <BETWEEN expr>
C                 <name>  BETWEEN  <name>
C                 <name>  BETWEEN  <value>
C                 <name>  BETWEEN  <ident>
C
C              or
C
C                 <name>  <NOT BETWEEN>  <BETWEEN expr>
C                 <name>  <NOT BETWEEN>  <name>
C                 <name>  <NOT BETWEEN>  <value>
C                 <name>  <NOT BETWEEN>  <ident>
C
C              There must be at least three meta-tokens here.
C
C
               IF (  THIRD .LE. 0 ) THEN
 
                  ERROR  = .TRUE.
                  PRSERR = 'Tokens were missing from comparison '   //
     .                     'relation.'
                  CALL CHKOUT ( 'ZZEKNRML' )
                  RETURN
 
               END IF
 
 
               IF ( MTCODE(THIRD)  .EQ. IDENT ) THEN
C
C                 We'll need to reduce the IDENT before proceeding.
C
                  START   =  THIRD
                  RETCND  =  REDUCE
                  STATE   =  PUSH
 
 
               ELSE IF (      (       MTCODE(SECOND) .EQ. AND      )
     .                  .AND. (     ( MTCODE(THIRD)  .EQ. NAME  )
     .                         .OR. ( MTCODE(THIRD)  .EQ. VALUE )  )   )
     .         THEN
C
C                 This sequence of tokens, when seen in the PARSE
C                 state, is a set of value bounds for a BETWEEN or
C                 NOT BETWEEN expression.  Note that this token sequence
C                 can occur elsewhere, but not in the PARSE state.
C                 This is because the meta-token sequences
C
C                    <name>  AND  <name>
C                    <name>  AND  <value>
C
C                 occur at the start of the RHS of only two
C                 productions, namely
C
C                    <BETWEEN expr>  =>  <name>   AND  <name>
C                    <BETWEEN expr>  =>  <name>   AND  <value>
C
C
C                 Null values are not allowed in BETWEEN expressions.
C
                  IF (      ( MTEXPR(FIRST) .EQ. NIL )
     .                 .OR. ( MTEXPR(THIRD) .EQ. NIL )  )  THEN
 
                     ERROR  = .TRUE.
                     PRSERR = 'NULL values are not allowed in ' //
     .                        'BETWEEN or NOT BETWEEN clauses.'
                     CALL CHKOUT ( 'ZZEKNRML' )
                     RETURN
 
                  END IF
 
C
C                 Hook up the name or value descriptors.
C
                  CALL LNKILB ( MTEXPR(FIRST), MTEXPR(THIRD), DSPOOL )
 
                  MTCODE(FIRST)  =  BTWEXP
 
                  CALL LNKFSL ( SECOND, THIRD, MTPOOL )
 
                  NMETA          =  NMETA - 2
 
C
C                 Decide the next state.
C
                  STATE  =  REDUCD
 
 
 
               ELSE IF (      (        MTCODE(SECOND) .GT. 0       )
     .                  .AND. (      ( MTCODE(THIRD)  .EQ. NAME  )
     .                          .OR. ( MTCODE(THIRD)  .EQ. VALUE ) )   )
     .         THEN
C
C                 Positive meta-token codes denote comparison
C                 operators.
C
C                 We have an arithmetic, string, or column comparison
C                 expression.  This is a trivial normalized
C                 relational expression.  All we have to do
C                 is store the expression in the relation table,
C                 and free the second and third meta-tokens.
C
                  IF ( LNKNFN(RLPOOL) .LT. 1 ) THEN
                     ERROR  = .TRUE.
                     PRSERR = 'Relation table is full.'
                     CALL CHKOUT ( 'ZZEKNRML' )
                     RETURN
                  END IF
 
                  CALL LNKAN ( RLPOOL, NEWREL )
 
                  RELS(1,NEWREL) = MTEXPR(FIRST )
                  RELS(2,NEWREL) = MTCODE(SECOND)
                  RELS(3,NEWREL) = MTEXPR(THIRD )
 
 
                  CALL LNKFSL ( SECOND, THIRD, MTPOOL )
                  NMETA  =  NMETA - 2
 
C
C                 Now allocate an entry in the conjunction pool
C                 and make this entry point to the relation table
C                 entry.
C
                  IF ( LNKNFN(CJPOOL) .LT. 1 ) THEN
                     ERROR  = .TRUE.
                     PRSERR = 'Conjunction table is full.'
                     CALL CHKOUT ( 'ZZEKNRML' )
                     RETURN
                  END IF
 
                  CALL LNKAN ( CJPOOL, NEWCJ )
 
 
                  CJPTRS(NEWCJ)  =  NEWREL
 
C
C                 Now allocate an entry in the disjunction pool
C                 and make this entry point to the conjunction pool
C                 entry.
C
                  IF ( LNKNFN(DJPOOL) .LT. 1 ) THEN
                     ERROR  = .TRUE.
                     PRSERR = 'Disjunction table is full.'
                     CALL CHKOUT ( 'ZZEKNRML' )
                     RETURN
                  END IF
 
                  CALL LNKAN ( DJPOOL, NEWDJ )
 
                  DJPTRS(NEWDJ)  =  NEWCJ
 
C
C                 Change the type of the first meta-token to EXPR and
C                 have that meta-token point to this table entry.  Bag
C                 the other two meta-tokens.
C
                  MTCODE(FIRST)  =  EXPR
                  MTEXPR(FIRST)  =  NEWDJ
 
C
C                 Decide the next state.
C
                  STATE  =  REDUCD
 
 
               ELSE IF (      ( MTCODE(SECOND) .EQ. BTWEEN )
     .                   .OR. ( MTCODE(SECOND) .EQ. NOTBTW )  ) THEN
C
C                 If the command is syntactically correct, the
C                 meta-token sequence should be one of:
C
C                    <name>  <BETWEEN>      <BETWEEN expr>
C                    <name>  <BETWEEN>      <value>
C                    <name>  <BETWEEN>      <name>
C                    <name>  <NOT BETWEEN>  <BETWEEN expr>
C                    <name>  <NOT BETWEEN>  <value>
C                    <name>  <NOT BETWEEN>  <name>
C
C
                  IF (       ( MTCODE(SECOND) .EQ. BTWEEN )
     .                 .AND. ( MTCODE(THIRD)  .EQ. BTWEXP )  ) THEN
C
C                    It's a BETWEEN comparison.  We treat this as a
C                    disjunction of conjunctions of comparison
C                    relations:
C                                      <name>   >=   <item1>
C                              AND     <name>   <=   <item2>
C
C                         OR
C                                      <name>   <=   <item1>
C                              AND     <name>   >=   <item2>
C
C                    where item1 and item2 are specified by the
C                    descriptors belonging to the third meta-token.
C
                     DO I = 1, 4
 
                        IF (  ( I .EQ. 1 ) .OR. ( I .EQ. 3 )  ) THEN
 
                           K  =  MTEXPR(THIRD)
 
                        ELSE
C
C                          We need the descriptor pointer for the RHS
C                          item.  This descriptor is linked to the tail
C                          of the descriptor for the LHS item.  The
C                          number of nodes to skip over depends on
C                          whether the LHS item is a name or value.
C
                           K  =  MTEXPR(THIRD)
 
                           IF ( DSCBUF(DSCSIZ,K) .EQ. IDENT ) THEN
                              SKIP = 1
                           ELSE
                              SKIP = 0
                           END IF
 
                           DO J = 1, SKIP + 1
                              K  =  LNKNXT( K, DSPOOL )
                           END DO
 
                        END IF
 
 
                        IF ( LNKNFN(RLPOOL) .LT. 1 ) THEN
                           ERROR  = .TRUE.
                           PRSERR = 'Relation table is full.'
                           CALL CHKOUT ( 'ZZEKNRML' )
                           RETURN
                        END IF
 
                        CALL LNKAN ( RLPOOL, REL(I) )
 
 
                        RELS( 1, REL(I) ) = MTEXPR(FIRST)
 
                        IF ( ( I .EQ. 1 ) .OR. ( I .EQ. 4 )  ) THEN
                           RELS( 2, REL(I) ) = GE
                        ELSE
                           RELS( 2, REL(I) ) = LE
                        END IF
 
                        RELS( 3, REL(I) ) = K
 
 
                        IF ( LNKNFN(CJPOOL) .LT. 1 ) THEN
                           ERROR  = .TRUE.
                           PRSERR = 'Conjunction table is full.'
                           CALL CHKOUT ( 'ZZEKNRML' )
                           RETURN
                        END IF
 
                        CALL LNKAN ( CJPOOL, CJ(I) )
 
                        CJPTRS(CJ(I))  =  REL(I)
 
                     END DO
 
C
C                    Link the conjunction nodes to form the two
C                    conjunctions shown above.
C
                     CALL LNKILA ( CJ(1), CJ(2), CJPOOL )
                     CALL LNKILA ( CJ(3), CJ(4), CJPOOL )
 
C
C                    Allocate disjunction pool entries and make them
C                    point to the two respective conjunctions.
C
                     DO I = 1, 2
 
                        IF ( LNKNFN(DJPOOL) .LT. 1 ) THEN
                           ERROR  = .TRUE.
                           PRSERR = 'Disjunction table is full.'
                           CALL CHKOUT ( 'ZZEKNRML' )
                           RETURN
                        END IF
 
                        CALL LNKAN ( DJPOOL, DJ(I) )
                        DJPTRS(DJ(I))  =  CJ( 2*I - 1 )
 
                     END DO
 
C
C                    Finally, link the disjunction pool entries, and
C                    create an <expression> meta-token.  Free the unused
C                    meta-tokens.
C
                     CALL LNKILA ( DJ(1), DJ(2), DJPOOL )
 
                     MTCODE(FIRST)  =  EXPR
                     MTEXPR(FIRST)  =  DJ(1)
 
                     CALL LNKFSL ( SECOND, THIRD, MTPOOL )
                     NMETA  =  NMETA - 2
 
C
C                    Decide the next state.
C
                     STATE  =  REDUCD
 
 
                  ELSE IF (       ( MTCODE(SECOND) .EQ. NOTBTW )
     .                      .AND. ( MTCODE(THIRD)  .EQ. BTWEXP )  ) THEN
C
C                    It's a NOT BETWEEN comparison.  We treat
C                    this as a disjunction of conjunctions of comparison
C                    relations:
C
C                                      <name>   <   <item1>
C                              AND     <name>   <   <item2>
C
C                         OR
C                                      <name>   >   <item1>
C                              AND     <name>   >   <item2>
C
C                    where item1 and item2 are specified by the
C                    descriptors belonging to the third meta-token.
C
C                    The actions here are closely analogous to those
C                    for the BETWEEN case.
C
                     DO I = 1, 4
 
                        IF (  ( I .EQ. 1 ) .OR. ( I .EQ. 3 )  ) THEN
 
                           K  =  MTEXPR(THIRD)
 
                        ELSE
C
C                          We need the descriptor pointer for the RHS
C                          item.  This descriptor is linked to the tail
C                          of the descriptor for the LHS item.  The
C                          number of nodes to skip over depends on
C                          whether the LHS item is a name or value.
C
                           K  =  MTEXPR(THIRD)
 
                           IF ( DSCBUF(DSCSIZ,K) .EQ. IDENT ) THEN
                              SKIP = 1
                           ELSE
                              SKIP = 0
                           END IF
 
                           DO J = 1, SKIP + 1
                              K  =  LNKNXT( K, DSPOOL )
                           END DO
 
                        END IF
 
 
                        IF ( LNKNFN(RLPOOL) .LT. 1 ) THEN
                           ERROR  = .TRUE.
                           PRSERR = 'Relation table is full.'
                           CALL CHKOUT ( 'ZZEKNRML' )
                           RETURN
                        END IF
 
                        CALL LNKAN ( RLPOOL, REL(I) )
 
 
                        RELS( 1, REL(I) ) = MTEXPR(FIRST)
 
                        IF ( I .LE. 2 ) THEN
                           RELS( 2, REL(I) ) = LT
                        ELSE
                           RELS( 2, REL(I) ) = GT
                        END IF
 
                        RELS( 3, REL(I) ) = K
 
 
                        IF ( LNKNFN(CJPOOL) .LT. 1 ) THEN
                           ERROR  = .TRUE.
                           PRSERR = 'Conjunction table is full.'
                           CALL CHKOUT ( 'ZZEKNRML' )
                           RETURN
                        END IF
 
                        CALL LNKAN ( CJPOOL, CJ(I) )
 
                        CJPTRS(CJ(I))  =  REL(I)
 
                     END DO
 
C
C                    Link the conjunction nodes to form the two
C                    conjunctions shown above.
C
                     CALL LNKILA ( CJ(1), CJ(2), CJPOOL )
                     CALL LNKILA ( CJ(3), CJ(4), CJPOOL )
 
C
C                    Allocate disjunction pool entries and make them
C                    point to the two respective conjunctions.
C
                     DO I = 1, 2
 
                        IF ( LNKNFN(DJPOOL) .LT. 1 ) THEN
                           ERROR  = .TRUE.
                           PRSERR = 'Disjunction table is full.'
                           CALL CHKOUT ( 'ZZEKNRML' )
                           RETURN
                        END IF
 
                        CALL LNKAN ( DJPOOL, DJ(I) )
                        DJPTRS(DJ(I))  =  CJ( 2*I - 1 )
 
                     END DO
 
C
C                    Finally, link the disjunction pool entries, and
C                    create an <expression> meta-token.  Free the unused
C                    meta-tokens.
C
                     CALL LNKILA ( DJ(1), DJ(2), DJPOOL )
 
                     MTCODE(FIRST)  =  EXPR
                     MTEXPR(FIRST)  =  DJ(1)
 
                     CALL LNKFSL ( SECOND, THIRD, MTPOOL )
                     NMETA  =  NMETA - 2
C
C                    Decide the next state.
C
                     STATE  =  REDUCD
 
 
                  ELSE IF (      ( MTCODE(THIRD) .EQ. NAME  )
     .                      .OR. ( MTCODE(THIRD) .EQ. VALUE )  )  THEN
C
C                    If the third meta-token is anything other than
C                    <BETWEEN expr>, we'll have to parse the portion of
C                    the query following the BETWEEN keyword before
C                    reducing the <BETWEEN> or <NOT BETWEEN> expression.
C
                     START   =  THIRD
                     RETCND  =  REDUCE
                     STATE   =  PUSH
 
                  ELSE
 
                     ERROR  =  .TRUE.
                     PRSERR = 'Token following BETWEEN operator is ' //
     .                        'invalid.'
                     CALL CHKOUT ( 'ZZEKNRML' )
                     RETURN
 
                  END IF
 
 
               ELSE IF ( MTCODE(SECOND) .GT. 0 ) THEN
C
C                 The third meta-token is in the wrong place at the
C                 wrong time.
C
                  ERROR  =  .TRUE.
                  PRSERR =  'Token sequence must be followed '      //
     .                      'by a value.'
 
                  CALL CHKOUT ( 'ZZEKNRML' )
                  RETURN
 
 
               ELSE
C
C                 The second meta-token is supposed to be a comparison
C                 operator, but it's actually something else.
C
                  ERROR  =  .TRUE.
                  PRSERR =  'Token must be followed by a '          //
     .                      'comparison operator.'
 
                  CALL CHKOUT ( 'ZZEKNRML' )
                  RETURN
 
               END IF
 
 
 
            ELSE IF ( MTCODE(FIRST) .EQ. EXPR ) THEN
C
C              If the query is valid, the sequence of meta-tokens
C              should be one of
C
C                 <expression>
C                 <expression>   )
C                 <expression>   OR   <expression>
C                 <expression>   OR   NAME
C                 <expression>   OR   IDENT
C                 <expression>   OR   NOT
C                 <expression>   OR   (
C                 <expression>   AND  <expression>
C                 <expression>   AND  NAME
C                 <expression>   AND  IDENT
C                 <expression>   AND  NOT
C                 <expression>   AND  (
C
               IF ( SECOND .LE. 0 ) THEN
C
C                 This is the last state we pass through
C                 before exiting the loop.  However, some syntax errors
C                 can get us here as well.
C
                  IF (  ( LEVEL .GT. 1 ) .OR. ( NMETA .GT. 1 )  ) THEN
 
                     ERROR  = .TRUE.
                     PRSERR = 'More tokens expected.'
                     CALL CHKOUT ( 'ZZEKNRML' )
                     RETURN
 
                  END IF
 
                  STATE = TERM
 
 
               ELSE IF ( MTCODE(SECOND) .EQ. RGROUP ) THEN
C
C                 We've reached the end of a `parenthesized'
C                 expression.
C
                  IF (       ( LEVEL         .GT. 1      )
     .                 .AND. ( POPCND(LEVEL) .EQ. ENDGRP )  ) THEN
C
C                    Time to pop the state.
C
                     STATE = POP
 
                  ELSE
C
C                    There should not be a right grouper here.
C
                     ERROR  =  .TRUE.
                     PRSERR = 'Unexpected right parenthesis '      //
     .                        'found.'
                     CALL CHKOUT ( 'ZZEKNRML' )
                     RETURN
 
                  END IF
 
 
C
C              In all other cases, there must be at least three
C              meta-tokens here.  Make sure there are.
C
               ELSE IF ( THIRD .LE. 0 ) THEN
 
                  ERROR  =  .TRUE.
                  PRSERR =  'More tokens expected.'
                  CALL CHKOUT ( 'ZZEKNRML' )
                  RETURN
 
C
C              Take care of the cases that will require reducing a sub-
C              expression before reducing the current expression.
C
               ELSE IF (      ( MTCODE(THIRD) .EQ. IDENT  )
     .                   .OR. ( MTCODE(THIRD) .EQ. NAME   )
     .                   .OR. ( MTCODE(THIRD) .EQ. NOT    )  ) THEN
 
                  START   =  THIRD
                  RETCND  =  REDUCE
                  STATE   =  PUSH
 
 
               ELSE IF ( MTCODE(THIRD) .EQ. LGROUP ) THEN
C
C                 We'll have to push our state before continuing.
C
                  START   =  FOURTH
                  RETCND  =  ENDGRP
                  STATE   =  PUSH
 
C
C              Now continue with the interesting cases.
C
               ELSE IF (       ( MTCODE(FIRST)  .EQ. EXPR )
     .                   .AND. ( MTCODE(SECOND) .EQ. OR   )
     .                   .AND. ( MTCODE(THIRD)  .EQ. EXPR )  )  THEN
C
C                 We have a disjunction of two normalized
C                 expressions.  We're not ready to perform a
C                 reduction yet; we need to see whether there's
C                 a higher priority operator, namely AND, on the
C                 right of the second expression.
C
                  DONOW = .TRUE.
 
                  IF ( FOURTH .GT. 0 ) THEN
 
                     IF ( MTCODE(FOURTH) .EQ. AND ) THEN
C
C                       The third token is already spoken for:
C                       the expression involving the operator
C                       to its right must be processed first.
C
                        DONOW = .FALSE.
 
                     END IF
 
                  END IF
 
                  IF ( DONOW ) THEN
C
C                    This is an easy case to handle:
C                    we can form the resulting normalized
C                    expression by just linking together the two
C                    lists in the disjunction table.
C
                     DJ(1) = MTEXPR(FIRST)
                     DJ(2) = MTEXPR(THIRD)
 
                     CALL LNKILB ( DJ(1), DJ(2), DJPOOL )
C
C                    The first meta-token will point to the resulting
C                    normalized expression; we'll discard the other
C                    two meta-tokens.
C
                     CALL LNKFSL ( SECOND, THIRD, MTPOOL )
                     NMETA = NMETA - 2
 
C
C                    MTEXPR(FIRST) and MTCODE(FIRST) are already
C                    set correctly.  All we need to do is determine
C                    our next state.  The next state defaults to
C                    PARSE; the other possibility is POP.
C
                     STATE  =  REDUCD
 
                  ELSE
C
C                    We'll have to reduce the expression on the right
C                    of the third meta-token before coming back to
C                    this expression.  Get ready to push our state.
C
C                    The condition that must be met in order to pop our
C                    state will be that we've performed a reduction.
C
                     RETCND = REDUCE
                     START  = THIRD
                     STATE  = PUSH
 
                  END IF
C
C                 Either we've reduced an OR expression, in which case
C                 the state has been set to PARSE or POP, or we've
C                 found a sub-expression that must be reduced before
C                 we attack the current expression, in which case the
C                 state has been set to PUSH.
C
 
               ELSE IF (       ( MTCODE(FIRST)  .EQ. EXPR )
     .                   .AND. ( MTCODE(SECOND) .EQ. AND  )
     .                   .AND. ( MTCODE(THIRD)  .EQ. EXPR )  )  THEN
C
C                 We have the conjunction of two normalized
C                 expressions.  This case requires application of
C                 DeMorgan's laws to convert the expression to a
C                 normalized form.
C
C                 If we have two normalized expressions, say
C
C                    EXPR1 =        ( A11 and A12 and ... )
C                                or ( A21 and A22 and ... )
C                                              .
C                                              .
C                                              .
C                                or ( AM1 and AM2 and ... )
C
C
C                    EXPR2 =        ( B11 and B12 and ... )
C                                or ( B21 and B22 and ... )
C                                              .
C                                              .
C                                              .
C                                or ( BN1 and BN2 and ... )
C
C
C
C                 Then ( EXPR1 and EXPR2 ) =
C
C
C                        or       {  (     ( AI1 and AI2 and ... )
C                    I = 1,...,M       and ( BJ1 and BJ2 and ... ) )  }
C                    J = 1,...,N
C
C
C                 We have the conjunction of two normalized
C                 So, to represent the normalized expression resulting
C                 from the conjuction of the expressions represented by
C                 the meta-tokens FIRST and THIRD, we will loop through
C                 each disjunction list and form the disjunction of all
C                 conjunctions of pairs of conjunctions, one of which is
C                 from the first expression and one of which is from the
C                 second.  After doing this, we'll clean up the
C                 conjunction and disjunction pools by freeing the
C                 elements in those pools used by the original two
C                 meta-tokens FIRST and THIRD.
C
C
                  DJ(1)    = MTEXPR(FIRST)
                  DJTAIL = NIL
 
                  DO WHILE ( DJ(1) .GT. 0 )
 
                     DJ(2) = MTEXPR(THIRD)
 
                     DO WHILE ( DJ(2) .GT. 0 )
C
C                       Allocate a new disjunction table entry,
C                       and create a new conjunction that represents
C                       the conjunction of the conjunction lists
C                       pointed to by DJ(1) and DJ(2).
C
                        IF ( LNKNFN(DJPOOL) .LT. 1 ) THEN
                           ERROR  = .TRUE.
                           PRSERR = 'Disjunction table is full.'
                           CALL CHKOUT ( 'ZZEKNRML' )
                           RETURN
                        END IF
 
                        CALL LNKAN ( DJPOOL, NEWDJ )
 
C
C                       Make copies of the conjunction lists pointed
C                       to by DJ(1) and DJ(2).
C
                        CJ(1) = DJPTRS(DJ(1))
                        TAIL  = NIL
 
                        DO WHILE ( CJ(1) .GT. 0 )
 
                           IF ( LNKNFN(CJPOOL) .LT. 1 ) THEN
                              ERROR  = .TRUE.
                              PRSERR = 'Conjunction table is full.'
                              CALL CHKOUT ( 'ZZEKNRML' )
                              RETURN
                           END IF
 
                           CALL LNKAN  (  CJPOOL, NEWCJ          )
                           CALL LNKILA (  TAIL,   NEWCJ, CJPOOL  )
                           TAIL         = NEWCJ
                           CJPTRS(TAIL) = CJPTRS(CJ(1))
 
                           CJ(1)        = LNKNXT ( CJ(1), CJPOOL )
 
                        END DO
 
                        HEAD1 = LNKHL( TAIL, CJPOOL )
 
 
                        CJ(2) = DJPTRS(DJ(2))
                        TAIL  = NIL
 
                        DO WHILE ( CJ(2) .GT. 0 )
 
                           IF ( LNKNFN(CJPOOL) .LT. 1 ) THEN
                              ERROR  = .TRUE.
                              PRSERR = 'Conjunction table is full.'
                              CALL CHKOUT ( 'ZZEKNRML' )
                              RETURN
                           END IF
 
                           CALL LNKAN  (  CJPOOL, NEWCJ          )
                           CALL LNKILA (  TAIL,   NEWCJ, CJPOOL  )
                           TAIL         = NEWCJ
                           CJPTRS(TAIL) = CJPTRS(CJ(2))
 
                           CJ(2)        = LNKNXT ( CJ(2), CJPOOL )
 
                        END DO
 
                        HEAD2 = LNKHL( TAIL, CJPOOL )
 
C
C                       Now link these copies and make NEWDJ point to
C                       the resulting list.
C
                        CALL LNKILB ( HEAD1, HEAD2, CJPOOL )
 
                        DJPTRS(NEWDJ) = HEAD1
 
C
C                       Link NEWDJ in at the tail of the disjunction
C                       list.
C
                        CALL LNKILA  ( DJTAIL, NEWDJ, DJPOOL )
                        DJTAIL = NEWDJ
 
                        DJ(2)  = LNKNXT ( DJ(2),   DJPOOL )
 
                     END DO
 
                     DJ(1) = LNKNXT ( DJ(1), DJPOOL )
 
                  END DO
 
C
C                 We've now created the new normalized expression that
C                 represents the conjunction of our original two
C                 expressions.
C
C                 Before continuing, we should clean up the entries in
C                 the disjunction and conjunction pools used by the
C                 original expressions.  We can save a little work
C                 by linking those entries before freeing them.
C
                  CALL LNKILB ( MTEXPR(FIRST), MTEXPR(THIRD), DJPOOL )
 
                  DJNODE = MTEXPR(FIRST)
 
                  DO WHILE ( DJNODE .GT. 0 )
C
C                    Free the conjunction list pointed to by DJNODE.
C
                     CJNODE = DJPTRS(DJNODE)
 
                     CALL LNKFSL (  CJNODE,
     .                              LNKTL(CJNODE,CJPOOL),
     .                              CJPOOL                )
 
                     DJNODE = LNKNXT( DJNODE, DJPOOL )
 
                  END DO
C
C                 Free the disjunction list that starts with
C                 MTEXPR(FIRST).
C
                  CALL LNKFSL (  MTEXPR(FIRST),
     .                           LNKTL( MTEXPR(FIRST), DJPOOL ),
     .                           DJPOOL                          )
 
C
C                 NEWDJ is the tail node of the list of disjunctions
C                 we've just finished.  The first meta-token should
C                 point to the head of this disjunction list.
C
                  MTEXPR(FIRST)  =  LNKHL ( NEWDJ, DJPOOL )
 
C
C                 We no longer need the other two meta-tokens.
C
                  CALL LNKFSL ( SECOND, THIRD, MTPOOL )
                  NMETA = NMETA - 2
 
C
C                 Decide the next state.
C
                  STATE  =  REDUCD
 
               ELSE
C
C                 There are no other valid cases in which the first
C                 meta-token is an expression.
C
                  ERROR  =  .TRUE.
                  PRSERR =  'Unexpected token found following '       //
     .                      'valid expression.'
                  CALL CHKOUT ( 'ZZEKNRML' )
                  RETURN
 
               END IF
 
 
 
            ELSE IF ( MTCODE(FIRST) .EQ. NOT ) THEN
C
C              There are four valid token sequences that we could
C              see here:
C
C                 NOT  <expression>
C                 NOT  IDENT
C                 NOT  NAME
C                 NOT  NOT
C                 NOT  (
C
               IF ( SECOND .LE. 0 ) THEN
 
                  ERROR  = .TRUE.
                  PRSERR = 'Tokens were missing from logical '        //
     .                     'expression.'
                  CALL CHKOUT ( 'ZZEKNRML' )
                  RETURN
 
               ELSE IF ( MTCODE(SECOND) .EQ. LGROUP ) THEN
C
C                 We'll have to push our state before continuing.
C
                  START   =  THIRD
                  RETCND  =  ENDGRP
                  STATE   =  PUSH
 
 
               ELSE IF (      ( MTCODE(SECOND) .EQ. NOT   )
     .                   .OR. ( MTCODE(SECOND) .EQ. IDENT )
     .                   .OR. ( MTCODE(SECOND) .EQ. NAME  )  )  THEN
 
                  START   =  SECOND
                  RETCND  =  REDUCE
                  STATE   =  PUSH
 
 
               ELSE IF ( MTCODE(SECOND) .EQ. EXPR ) THEN
C
C                 We have the negation of a normalized expression. Since
C                 the NOT operator has higher precedence than any other,
C                 we need not concern ourselves with the token on the
C                 right of the expression.
C
C                 This case requires application of DeMorgan's laws to
C                 convert the expression to a normalized form.
C
C
C                 If we have a normalized expression, say
C
C                    EXPR  =         ( A11 and A12 and ... )
C                                 or ( A21 and A22 and ... )
C                                              .
C                                              .
C                                              .
C                                 or ( AM1 and AM2 and ... )
C
C                 Then (using the tilde to express negation):
C
C                    ~EXPR =          ( ~A11 or ~A12 or ... )
C                                 and ( ~A21 or ~A22 or ... )
C                                              .
C                                              .
C                                              .
C                                 and ( ~AM1 or ~AM2 or ... )
C
C                 Since each parenthesized expression above is a
C                 normalized expression, we can convert the conjunction
C                 of any of these expressions and a second normalized
C                 expression to normalized form using the method of the
C                 AND case above.
C
C                 We'll first build the expression
C
C                    ( ~A11 or ~A12 or ... )
C
C                 and then combine the others with it, one by one.
C                 When we're all done, we'll negate the operators used
C                 in the comparison relations.
C
C                 The pointer EXPRHD will denote the head of the
C                 combined normalized expression.
C
                  DJNODE = MTEXPR(SECOND)
 
                  CJNODE = DJPTRS(DJNODE)
                  TAIL   = NIL
 
                  DO WHILE ( CJNODE .GT. 0 )
C
C                    Create a new singleton disjunction list
C                    that points to the relation pointed to by
C                    CJNODE.
C
                     IF ( LNKNFN(DJPOOL) .LT. 1 ) THEN
                        ERROR  = .TRUE.
                        PRSERR = 'Disjunction table is full.'
                        CALL CHKOUT ( 'ZZEKNRML' )
                        RETURN
                     END IF
 
                     CALL LNKAN ( DJPOOL, NEWDJ )
 
                     IF ( LNKNFN(CJPOOL) .LT. 1  ) THEN
                        ERROR  = .TRUE.
                        PRSERR = 'Conjunction table is full.'
                        CALL CHKOUT ( 'ZZEKNRML' )
                        RETURN
                     END IF
 
                     CALL LNKAN ( CJPOOL, NEWCJ )
 
                     CJPTRS(NEWCJ) = CJPTRS(CJNODE)
                     DJPTRS(NEWDJ) = NEWCJ
C
C                    Now link the new singleton disjunction list in
C                    at the tail of the disjunction list that
C                    parallels the conjunction list we're currently
C                    traversing.
C
                     CALL LNKILA ( TAIL, NEWDJ, DJPOOL )
                     TAIL   = NEWDJ
 
                     CJNODE = LNKNXT ( CJNODE, CJPOOL )
 
                  END DO
 
C
C                 Keep track of the head of the new normalized
C                 expression.
C
                  EXPRHD = LNKHL ( TAIL, DJPOOL )
 
C
C                 Now, for every remaining conjunction in the original
C                 expression, we'll form the normalized expression
C                 resulting from the conjunction of its negation and
C                 of our cumulative normalized expression.  As mentioned
C                 before, we won't negate the comparison operators
C                 just yet.
C
C
                  DJNODE = LNKNXT ( DJNODE, DJPOOL )
 
                  DO WHILE ( DJNODE .GT. 0 )
C
C                    Loop through our existing cumulative
C                    expression and the latest conjunction, forming
C                    all pairwise conjunctions.
C
                     DJ(1)    = EXPRHD
                     DJTAIL = NIL
 
                     DO WHILE ( DJ(1) .GT. 0 )
 
                        CJ(2) = DJPTRS(DJNODE)
 
                        DO WHILE ( CJ(2) .GT. 0 )
C
C                          Make a copy of the conjunction list pointed
C                          to by DJPTRS(DJ(1)).
C
                           CJNODE = DJPTRS(DJ(1))
                           TAIL   = NIL
 
                           DO WHILE ( CJNODE .GT. 0 )
 
                              IF ( LNKNFN(CJPOOL) .LT. 1 ) THEN
                                 ERROR  = .TRUE.
                                 PRSERR = 'Conjunction table is full.'
                                 CALL CHKOUT ( 'ZZEKNRML' )
                                 RETURN
                              END IF
 
                              CALL LNKAN  ( CJPOOL, NEWCJ         )
                              CALL LNKILA ( TAIL,   NEWCJ, CJPOOL )
 
                              CJPTRS(NEWCJ)  = CJPTRS(CJNODE)
                              TAIL           = NEWCJ
                              CJNODE         = LNKNXT ( CJNODE, CJPOOL )
 
                           END DO
 
                           CJ(1) = LNKHL ( TAIL, CJPOOL )
 
C
C                          Allocate a new conjunction table entry for
C                          the conjunction of the expressions
C                          pointed to by CJ(1) and CJ(2).  Allocate a
C                          new disjunction table entry to point to this
C                          new conjunction.
C
                           IF ( LNKNFN(CJPOOL) .LT. 1 ) THEN
                              ERROR  = .TRUE.
                              PRSERR = 'Conjunction table is full.'
                              CALL CHKOUT ( 'ZZEKNRML' )
                              RETURN
                           END IF
 
                           CALL LNKAN ( CJPOOL, NEWCJ )
                           CJPTRS(NEWCJ)  =  CJPTRS(CJ(2))
 
                           IF ( LNKNFN(DJPOOL) .LT. 1  ) THEN
                              ERROR  = .TRUE.
                              PRSERR = 'Disjunction table is full.'
                              CALL CHKOUT ( 'ZZEKNRML' )
                              RETURN
                           END IF
 
                           CALL LNKAN ( DJPOOL, NEWDJ )
 
C
C                          Hook everything up.
C
                           CALL LNKILB ( CJ(1), NEWCJ, CJPOOL )
 
                           DJPTRS(NEWDJ) =  CJ(1)
 
                           CALL LNKILA ( DJTAIL, NEWDJ, DJPOOL )
                           DJTAIL        =  NEWDJ
 
                           CJ(2)         =  LNKNXT ( CJ(2), CJPOOL )
 
                        END DO
 
                        DJ(1)    = LNKNXT ( DJ(1), DJPOOL )
 
                     END DO
 
C
C                    Before going on, clean up the conjunction and
C                    disjunction pool entries used by our last
C                    version of the cumulative expression.
C
                     DJ(1) = EXPRHD
 
                     DO WHILE ( DJ(1) .GT. 0 )
 
                        CJ(1) = DJPTRS(DJ(1))
                        CJ(2) = LNKTL ( CJ(1), CJPOOL )
 
                        CALL LNKFSL ( CJ(1), CJ(2), CJPOOL )
 
                        DJ(1) = LNKNXT ( DJ(1), DJPOOL )
 
                     END DO
 
                     CALL LNKFSL (  EXPRHD,
     .                              LNKTL( EXPRHD, DJPOOL ),
     .                              DJPOOL                    )
 
C
C                    Set EXPRHD to be the head of our updated,
C                    cumulative expression.  Start to work on the
C                    next conjunction.
C
                     EXPRHD = LNKHL  ( DJTAIL, DJPOOL )
                     DJNODE = LNKNXT ( DJNODE, DJPOOL )
 
                  END DO
 
C
C                 EXPRHD now points to a new expression that will
C                 represent the negation of the expression pointed
C                 to by MTEXPR(SECOND), as soon as we negate the
C                 comparison operators referenced in the expression.
C                 Take care of this last step now.  To make sure that
C                 we negate each operator exactly once, we build a set
C                 of relations to be negated, then negate each relation
C                 in the set.
C
                  CALL SSIZEI ( MAXREL, RELSET )
 
                  DJNODE = MTEXPR(SECOND)
 
 
                  DO WHILE ( DJNODE .GT. 0 )
 
                     CJNODE = DJPTRS(DJNODE)
 
                     DO WHILE ( CJNODE .GT. 0 )
 
                        RELPTR = CJPTRS(CJNODE)
                        CALL INSRTI ( RELPTR, RELSET )
 
                        CJNODE = LNKNXT( CJNODE, CJPOOL )
 
                     END DO
 
                     DJNODE = LNKNXT ( DJNODE, DJPOOL )
 
                  END DO
 
 
                  DO I = 1, CARDI(RELSET)
 
                     RELPTR          =  RELSET( I )
                     J               =  ISRCHI( RELS(2,RELPTR),
     .                                          NRELOP,
     .                                          CMPCDE         )
                     RELS(2,RELPTR)  =  CMPNEG( J )
 
                  END DO
 
C
C                 Set the pointer of the first meta-token to point
C                 to our normalized expression, and change the
C                 meta-token's code to <expr>.
C
                  MTEXPR(FIRST) = EXPRHD
                  MTCODE(FIRST) = EXPR
 
C
C                 Get rid of the second meta-token, and determine the
C                 next state.
C
                  CALL LNKFSL ( SECOND, SECOND, MTPOOL )
                  NMETA  =  NMETA - 1
 
                  STATE  =  REDUCD
 
 
               ELSE
C
C                 The second token is invalid in this context.
C
                  ERROR  = .TRUE.
                  PRSERR = 'Token following NOT operator was invalid.'
                  CALL CHKOUT ( 'ZZEKNRML' )
                  RETURN
 
               END IF
C
C              This is the end of the NOT case.
C
 
 
            ELSE IF ( MTCODE(FIRST) .EQ. LGROUP ) THEN
C
C              We're looking at the start of a `parenthesized'
C              sub-expression.
C
C              Push our state, and start parsing at meta-token
C              SECOND.  The condition for popping our state will be
C              that we encounter a right grouper.
C
               RETCND = ENDGRP
               START  = SECOND
               STATE  = PUSH
 
 
            ELSE
C
C              Only a syntax error could get us here.
C
               ERROR  =  .TRUE.
               PRSERR = 'Unexpected token found.'
               CALL CHKOUT ( 'ZZEKNRML' )
               RETURN
 
            END IF
 
C
C           This is the end of the code for the PARSE state.  We've
C           determined the next parsing state at this point.
C
 
         ELSE IF ( STATE .EQ. REDUCD ) THEN
C
C           A reduction has been done.  Decide the next state.
C
            STATE  =  REDUCD
 
            IF ( POPCND(LEVEL) .EQ. REDUCE ) THEN
               STATE         = POP
            ELSE
               MSTART(LEVEL) = FIRST
               STATE         = PARSE
            END IF
 
 
 
         ELSE IF ( STATE .EQ. PUSH ) THEN
C
C           Increment the stack level, and save the current
C           starting point and pop condition.
C
            LEVEL  =  LEVEL + 1
 
            IF ( LEVEL .GT. MAXSTK ) THEN
               ERROR          = .TRUE.
               PRSERR         = 'Stack is full'
               STATE          =  TERM
            ELSE
               MSTART(LEVEL)  =  START
               POPCND(LEVEL)  =  RETCND
               STATE          =  PARSE
            END IF
 
 
         ELSE IF ( STATE .EQ. POP ) THEN
C
C           If we can, pop the state.
C
            IF ( LEVEL .GT. 1 ) THEN
 
               IF ( POPCND(LEVEL) .EQ. ENDGRP ) THEN
C
C                 If we're popping the state because we encountered a
C                 right grouper, we have a meta-token sequence that
C                 looks like this:
C
C                    (  EXPR  )
C
C                        ^    ^
C                     FIRST  SECOND
C
C                 We need to remove the grouping tokens, taking care to
C                 update the starting token at the next lower level, if
C                 the left grouper was the starting token.
C
                  PREV   =  LNKPRV ( FIRST, MTPOOL )
 
                  IF ( MSTART(LEVEL-1) .EQ. PREV ) THEN
                     MSTART(LEVEL-1) = FIRST
                  END IF
 
                  IF ( METAHD .EQ. PREV ) THEN
                     METAHD = FIRST
                  END IF
 
                  CALL LNKFSL ( PREV,   PREV,   MTPOOL )
                  CALL LNKFSL ( SECOND, SECOND, MTPOOL )
 
                  NMETA  =  NMETA - 2
 
               END IF
 
               LEVEL = LEVEL - 1
               STATE = PARSE
 
            ELSE
               ERROR  =  .TRUE.
               PRSERR =  'Syntax error:  badly formed WHERE clause.'
               CALL CHKOUT ( 'ZZEKNRML' )
               RETURN
            END IF
 
 
         END IF
 
C
C        We've considered all states.
C
 
      END DO
 
 
C
C     At this point, there should be a single meta-token of type EXPR.
C     This meta-token should point to a normalized expression.  We'll
C     set the encoded query to represent this expression.  For each
C     constraint, we'll add a constraint descriptor to the encoded
C     query.  We'll also update the count of constraints, the count of
C     conjunctions, and we'll add a list of conjunction sizes.
C
      DJNODE = MTEXPR(FIRST)
      NCONJ  = 0
      NRELS  = 0
 
      DO WHILE ( DJNODE .GT. 0 )
 
         NCONJ         =  NCONJ + 1
         SIZES(NCONJ)  =  0
         CJNODE        =  DJPTRS(DJNODE)
 
         DO WHILE ( CJNODE .GT. 0 )
 
            NRELS          =  NRELS        + 1
            SIZES (NCONJ)  =  SIZES(NCONJ) + 1
            RELPTR         =  CJPTRS(CJNODE)
 
            TABPTR         =  RELS(1,RELPTR)
            OP             =  RELS(2,RELPTR)
            RHSPTR         =  RELS(3,RELPTR)
 
C
C           Add a constraint descriptor to the encoded query.  The
C           structure of these descriptors is documented in the include
C           file for encoded query parameters.
C
C           First, save space for the constraint type.  We'll fill this
C           in after finding out what's on the right hand side.
C
            CALL APPNDI ( 0, EQRYI )
            K  =  CARDI ( EQRYI )
 
C
C           Next, add name descriptors for the table and column on
C           the left-hand side.  These descriptors are linked and
C           pointed to by NAMPTR.
C
            DO I = 1, EQVDSZ
               CALL APPNDI (  DSCBUF( I, TABPTR ),  EQRYI  )
            END DO
 
            COLPTR  =  LNKNXT( TABPTR, DSPOOL )
 
            DO I = 1, EQVDSZ
               CALL APPNDI (  DSCBUF( I, COLPTR ),  EQRYI  )
            END DO
 
C
C           What happens next depends on whether the query has a null
C           value on the right hand side.  This is indicated by the
C           relation's value pointer being NIL.
C
            IF ( RHSPTR .EQ. NIL ) THEN
C
C              For constraints involving null values, we change the
C              operator to ISNULL or NOTNUL as appropriate.
C
               IF ( OP .EQ. EQ ) THEN
                  OP  =  ISNULL
 
               ELSE IF ( OP .EQ. NE ) THEN
                  OP  =  NOTNUL
 
               ELSE
 
                  ERROR  =  .TRUE.
                  PRSERR =  'NULL values can only be used with the ' //
     .                      'operators "IS NULL", "NOT NULL", '      //
     .                      'or equivalents.'
                  CALL CHKOUT ( 'ZZEKNRML' )
                  RETURN
 
               END IF
 
C
C              Set the operator code.
C
               CALL APPNDI ( OP, EQRYI )
 
C
C              Pad the constraint descriptor up to the full length.
C
               DO I = 1, 2*EQVDSZ
                  CALL APPNDI ( 0, EQRYI )
               END DO
 
C
C              Set the descriptor's type by updating the reserved
C              location.
C
               EQRYI(K)  =  EQVAL
 
 
            ELSE
C
C              For `normal' constraints, that is, constraints that don't
C              involve null values, we set the operator code, then
C              fill in the information describing the RHS of the
C              constraint.
C
               CALL APPNDI ( OP, EQRYI )
 
               IF (  DSCBUF( DSCSIZ, RHSPTR ) .EQ. VALUE ) THEN
C
C                 The RHS contains a value.  Append the descriptor
C                 for the value, then pad the constraint descriptor.
C
                  DO I = 1, EQVDSZ
                     CALL APPNDI (  DSCBUF( I, RHSPTR ),  EQRYI  )
                  END DO
 
                  DO I = 1, EQVDSZ
                     CALL APPNDI (  0,  EQRYI  )
                  END DO
 
C
C                 Set the descriptor's type by updating the reserved
C                 location.
C
                  EQRYI(K)  =  EQVAL
 
 
               ELSE
C
C                 The RHS contains a column name.  Append the
C                 descriptors for the table and column.
C
                  DO I = 1, EQVDSZ
                     CALL APPNDI (  DSCBUF( I, RHSPTR ),  EQRYI  )
                  END DO
 
                  COLPTR  =  LNKNXT( RHSPTR, DSPOOL )
 
                  DO I = 1, EQVDSZ
                     CALL APPNDI (  DSCBUF( I, COLPTR ),  EQRYI  )
                  END DO
 
C
C                 Set the descriptor's type by updating the reserved
C                 location.
C
                  EQRYI(K)  =  EQCOL
 
               END IF
 
            END IF
C
C           We've updated the encoded query to reflect the current
C           constraint relation.
C
            CJNODE = LNKNXT ( CJNODE, CJPOOL )
 
         END DO
C
C        We've set the array element SIZES(NCONJ).
C
         DJNODE = LNKNXT ( DJNODE, DJPOOL )
 
      END DO
 
C
C     Set the counts of constraints and conjunctions in the encoded
C     query.
C
      CALL ZZEKWEQI ( 'NUM_CONSTRAINTS',  NRELS, EQRYI )
      CALL ZZEKWEQI ( 'NUM_CONJUNCTIONS', NCONJ, EQRYI )
 
C
C     Add the conjunction size list to the encoded query.
C
      DO I = 1, NCONJ
         CALL APPNDI ( SIZES(I), EQRYI )
      END DO
 
 
      CALL CHKOUT ( 'ZZEKNRML' )
      RETURN
      END
