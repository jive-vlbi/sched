C$Procedure      ZZEKSCAN ( EK, scan query )
 
      SUBROUTINE ZZEKSCAN (  QUERY,  MAXNTK, MAXNUM, NTOKEN, TOKENS,
     .                       LXBEGS, LXENDS, VALUES, NUMVLS, CHRBUF,
     .                       CHBEGS, CHENDS, SCNERR, ERRMSG         )
 
C$ Abstract
C
C     Scan tokens in an EK query.
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
 
 
      INCLUDE 'ekkeyw.inc'
      INCLUDE 'ektokn.inc'
      INCLUDE 'ekqlimit.inc'
 
      CHARACTER*(*)         QUERY
      INTEGER               MAXNTK
      INTEGER               MAXNUM
      INTEGER               NTOKEN
      INTEGER               TOKENS ( * )
      INTEGER               LXBEGS ( * )
      INTEGER               LXENDS ( * )
      INTEGER               VALUES ( * )
      DOUBLE PRECISION      NUMVLS ( * )
      CHARACTER*(*)         CHRBUF
      INTEGER               CHBEGS ( * )
      INTEGER               CHENDS ( * )
      LOGICAL               SCNERR
      CHARACTER*(*)         ERRMSG
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     QUERY      I   Query specifying events to be found.
C     MAXNTK     I   Maximum number of tokens to return.
C     MAXNUM     I   Maximum number of numeric tokens allowed.
C     NTOKEN     O   Number of tokens returned.
C     TOKENS     O   Code numbers of identified tokens.
C     LXBEGS,
C     LXENDS     O   Start and end locations of lexemes in query.
C     VALUES     O   Token values or value pointers, as needed.
C     NUMVLS     O   Buffer containing values of numeric tokens.
C     CHRBUF     O   Buffer containing string tokens and identifiers.
C     CHBEGS     O   Begin locations of string tokens in CHRBUF.
C     CHENDS     O   End locations of string tokens in CHRBUF.
C     SCNERR     O   Flag indicating whether query parsed correctly.
C     ERRMSG     O   Scan error description.
C
C$ Detailed_Input
C
C     QUERY          is character string containing an EK query.  See
C                    the header of the subroutine EKFIND for a
C                    detailed description of the EK query language.
C
C     MAXNTK         is the maximum number of tokens that may occur
C                    in QUERY.
C
C     MAXNUM         is the maximum number of tokens representing
C                    numeric values that may occur in QUERY.
C
C$ Detailed_Output
C
C     NTOKEN         is the number of tokens found in the input QUERY.
C                    This number will be less than or equal to MAXNTK.
C
C     TOKENS         is an array of codes for the tokens found in QUERY.
C                    The parameter values for these codes are not part
C                    of the EKSCAN specification; however, these values
C                    must be kept consistent with those used by EKPARS.
C                    The caller of EKSCAN should declare TOKENS with
C                    dimension MAXNTK.
C
C     LXBEGS,
C     LXENDS         are, respectively, arrays of begin and end pointers
C                    for the lexemes occuring in QUERY.  Lexemes are the
C                    strings in QUERY that correspond to tokens.  For
C                    example, '4.9D0' and '3' are both lexemes that map
C                    to the token <number>.
C
C     VALUES         is an array of token values.  The Ith element of
C                    VALUES refers to the Ith token.
C
C                    If the Ith token is a number, the Ith element of
C                    VALUES is a pointer into the NUMVLS array where
C                    the value of the number is stored.  The Ith token
C                    code indicates whether the number was a signed
C                    integer or d.p. number.
C
C                    If the Ith token is a keyword, the Ith element of
C                    VALUES is the code for that keyword.
C
C                    If the Ith token is a quoted string, the Ith
C                    element of VALUES is the common index in the arrays
C                    CHBEGS and CHENDS where the begin and end positions
C                    in CHRBUF of the parsed identifier are stored.
C                    Identifiers are converted to upper case when they
C                    are scanned.
C
C                    If the Ith token is an identifier, the Ith element
C                    of VALUES has the same role as in the case of a
C                    quoted string.
C
C                    If the Ith token is a special character, the Ith
C                    element of values is undefined; the value of
C                    TOKENS is the value of ICHAR() applied to the
C                    character.
C
C                    The caller of EKSCAN should declare VALUES with
C                    dimension MAXNTK.
C
C
C     NUMVLS         is an array of numeric values of parsed numeric
C                    tokens.  The caller of EKSCAN should declare
C                    NUMVLS with dimension at least MAXNUM.
C
C     CHRBUF         is a character string used to contain the values
C                    of literal string tokens and identifiers.  The
C                    value MAXQRY is guaranteed to be a safe length for
C                    CHRBUF, though the caller of EKSCAN can probably
C                    get away with less.
C
C                    The reason for the existence of CHRBUF is that
C                    the lexemes representing quoted strings may contain
C                    doubled quote characters representing embedded
C                    quotes; these characters are undoubled when the
C                    lexemes are parsed.  Hence the parsed quoted
C                    strings are not necessarily substrings of the
C                    original lexemes from which they are derived.
C
C     CHBEGS,
C     CHENDS         are, respectively, arrays of begin and end pointers
C                    for parsed quoted strings and identifiers stored in
C                    CHRBUF.
C
C     SCNERR         is a logical flag which is set to .TRUE. if a
C                    scanning error is detected, and is set to .FALSE.
C                    otherwise.  If SCNERR is returned .TRUE., all
C                    outputs save ERRMSG are undefined.
C
C     ERRMSG         is an error message that describes the cause of
C                    a scanning error, if such an error is detected.
C                    When SCNERR is returned .FALSE., ERRMSG is set to
C                    blank.
C
C$ Parameters
C
C     See the include files.
C
C$ Exceptions
C
C     Error free.
C
C     This routine set the error flag ERROR to .TRUE. and returns an
C     error message in the event that a syntax error precludes scanning
C     the input string.  Note that incorrect queries may scan
C     successfully; it is the responsibility of the caller to ensure
C     syntactic and semantic correctness of queries.
C
C     The following error messages are returned by this routine:
C
C        'No table list preceded first keyword.'
C        'Column clause and WHERE keyword are missing.'
C        'WHERE keyword is missing.'
C        'Too many tokens in query; max allowed is #.'
C        'Column list was empty.'
C        'Quoted string in positions #:# is empty.'
C        'Unexpected token found in query: #'
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine breaks up a valid EK query into an array of
C     individual tokens in order to facilitate parsing.
C
C     Time values and quoted strings are treated as single tokens.
C
C$ Examples
C
C     1)  Examples of strings containing lexically valid queries
C         are:
C
C            FROM TIME * WHERE TIME LT 'MO SCLK 15328997.121'
C
C            from time, event_type where event_type eq "MOC_EVENT"
C
C            FROM * WHERE TIME GE "1994 MAR 1" AND  IDCODE EQ -94030
C
C            FROM  *   WHERE
C                  TIME        GE   "1994 MAR 1"
C            AND   TIME        LE   '1-MAR-1994 18:4:1'
C            AND   EVENT_TYPE  LIKE '*PMIRR*'
C
C            FROM * WHERE TIME LT "MO SCLK 15328997.121" ORDER BY TIME
C
C            from col_1 col_2 col_3 where time lt '2010'
C
C            from col_1 col_2 col_3
C
C            from *
C
C            from * order by event_type
C
C         For a query to be semantically valid, all of the column names
C         referenced in the query must be present in at least one
C         loaded E-kernel.
C
C
C     2)  Examples of lexically invalid queries are:
C
C            from time where time lt
C            1991 jan 1                         {time string is not
C                                                quoted}
C
C            from time * where time
C             .lt. 1991 jan 1                   {operator should be lt}
C
C
C            from event_type * where
C            event_type eq ""                   {quoted string is empty}
C
C            from event_type ^ where
C            event_type eq "cmd"                {unexpected token}
C
C            from column1 where
C            column1 eq  3c                     {invalid numeric token}
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
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.0.1, 22-OCT-1996 (NJB)
C
C        Corrected miscellaneous errors in the header.
C
C-    SPICELIB Version 3.0.0, 14-NOV-1995 (NJB)
C
C        Complete re-write for architecture 3.
C
C-&
 
 
C$ Index_Entries
C
C     scan EK query
C     find tokens in EK query
C
C-&
 
 
C$ Revisions
C
C-    Beta Version 3.0.0, 14-NOV-1995 (NJB)
C
C        Complete re-write for architecture 3.
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               BSRCHC
      INTEGER               FRSTPC
      INTEGER               ISRCHC
      INTEGER               RTRIM
 
      LOGICAL               BEINT
      LOGICAL               RETURN
 
 
C
C     Local parameters
C
      CHARACTER*(*)         DQUOTE
      PARAMETER           ( DQUOTE  = '"' )
 
      CHARACTER*(*)         SQUOTE
      PARAMETER           ( SQUOTE  = '''' )
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE =   80 )
 
      INTEGER               NXTTOK
      PARAMETER           ( NXTTOK = 1 )
 
      INTEGER               NEWTOK
      PARAMETER           ( NEWTOK = NXTTOK + 1 )
 
      INTEGER               TERM
      PARAMETER           ( TERM   = NEWTOK + 1 )
 
      INTEGER               QSTR
      PARAMETER           ( QSTR   = TERM   + 1 )
 
      INTEGER               DOT
      PARAMETER           ( DOT    = QSTR   + 1 )
 
      INTEGER               NUMBER
      PARAMETER           ( NUMBER = DOT    + 1 )
 
      INTEGER               IDENT
      PARAMETER           ( IDENT  = NUMBER + 1 )
 
      INTEGER               SPCIAL
      PARAMETER           ( SPCIAL = IDENT + 1 )
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               MXSPEC
      PARAMETER           ( MXSPEC = 512 )
 
C
C     Number of tokens made up of special characters:
C
      INTEGER               NSPEC
      PARAMETER           ( NSPEC  = 13 )
 
C
C     Max length of any such token:
C
      INTEGER               SPCLEN
      PARAMETER           ( SPCLEN = 2 )
 
C
C     Local variables
C
      CHARACTER*(1)         CHR
      CHARACTER*(SPCLEN)    SPCSTR ( NSPEC )
      CHARACTER*(LNSIZE)    HDCHRS
      CHARACTER*(KWLEN)     KEYWDS ( NKEYWD )
      CHARACTER*(LNSIZE)    TLCHRS
      CHARACTER*(MAXQRY)    TQUERY
 
      INTEGER               CHCARD
      INTEGER               CPTR
      INTEGER               I
      INTEGER               IDSPEC ( LBCELL : MXSPEC )
      INTEGER               J
      INTEGER               KWVALS ( NKEYWD )
      INTEGER               L
      INTEGER               LAST
      INTEGER               LENGTH
      INTEGER               NCHARS
      INTEGER               NNUMS
      INTEGER               NSTRS
      INTEGER               PTR
      INTEGER               ROOM
      INTEGER               STATE
      INTEGER               SPCTOK ( NSPEC )
      INTEGER               SPCVAL ( NSPEC )
 
      LOGICAL               PASS1
 
C
C     Statement Functions
C
      LOGICAL               ISALPH
      LOGICAL               ISDIGT
      LOGICAL               ISDOT
      LOGICAL               ISSIGN
      LOGICAL               ISNUM
      LOGICAL               ISQUOT
 
 
C
C     Saved variables
C
      SAVE
 
C
C     Initial values
C
 
C
C     These keyword declarations must be made in alphabetical order!
C
      DATA  (  KEYWDS(I), KWVALS(I), I = 1, 10 )      /
     .           'ALL',          KWALL,
     .           'AND',          KWAND,
     .           'ASC ',         KWASND,
     .           'AVG ',         KWAVG,
     .           'BETWEEN',      KWBETW,
     .           'BY',           KWBY,
     .           'COUNT',        KWCNT,
     .           'DESC',         KWDSND,
     .           'DISTINCT',     KWDSTN,
     .           'EQ',           KWEQ                 /
 
      DATA  (  KEYWDS(I), KWVALS(I), I = 11, 20 )     /
     .           'FROM',         KWFROM,
     .           'GE',           KWGE,
     .           'GROUP',        KWGRP,
     .           'GT',           KWGT,
     .           'HAVING',       KWHAV,
     .           'IS',           KWIS,
     .           'LE',           KWLE,
     .           'LIKE',         KWLIKE,
     .           'LT',           KWLT,
     .           'MAX',          KWMAX                /
 
      DATA  (  KEYWDS(I), KWVALS(I), I = 21, NKEYWD ) /
     .           'MIN',          KWMIN,
     .           'NE',           KWNE,
     .           'NOT',          KWNOT,
     .           'NULL',         KWNULL,
     .           'OR',           KWOR,
     .           'ORDER',        KWORDR,
     .           'SELECT',       KWSEL,
     .           'SUM',          KWSUM,
     .           'WHERE',        KWWHER               /
 
C
C     The following tokens are sequences of special characters.  Some
C     of these are synonyms for keywords; some have other meanings.  In
C     this data statement, the longer sequences must precede the shorter
C     ones, in order for the matching algorithm to work properly.
C
      DATA  (  SPCSTR(I), SPCTOK(I), SPCVAL(I), I = 1, NSPEC )  /
     .           '!=',      TKKEY,    KWNE,
     .           '^=',      TKKEY,    KWNE,
     .           '<>',      TKKEY,    KWNE,
     .           '<=',      TKKEY,    KWLE,
     .           '>=',      TKKEY,    KWGE,
     .           '<',       TKKEY,    KWLT,
     .           '>',       TKKEY,    KWGT,
     .           '=',       TKKEY,    KWEQ,
     .           '(',       TKLPAR,   0,
     .           ')',       TKRPAR,   0,
     .           ',',       TKCOMA,   0,
     .           '.',       TKDOT,    0,
     .           '*',       TKSTAR,   0                         /
 
 
      DATA        PASS1              /  .TRUE.  /
 
 
C
C     Statement Function Definitions
C
      ISALPH ( CHR )  =       (      ( ICHAR(CHR) .GE. ICHAR('A') )
     .                         .AND. ( ICHAR(CHR) .LE. ICHAR('Z') ) )
     .                  .OR.  (      ( ICHAR(CHR) .GE. ICHAR('a') )
     .                         .AND. ( ICHAR(CHR) .LE. ICHAR('z') ) )
 
 
      ISDIGT ( CHR )  =       (      ( ICHAR(CHR) .GE. ICHAR('0') )
     .                         .AND. ( ICHAR(CHR) .LE. ICHAR('9') ) )
 
 
      ISDOT  ( CHR )  =     ( CHR .EQ. '.' )
 
 
      ISSIGN ( CHR )  =     ( CHR .EQ. '+' )    .OR. ( CHR .EQ. '-' )
 
 
      ISNUM  ( CHR )  =       ISDIGT( CHR )     .OR.   ISSIGN ( CHR )
     .                                          .OR.  ( CHR .EQ. '.' )
 
 
      ISQUOT ( CHR )  =     ( CHR .EQ. SQUOTE ) .OR. ( CHR .EQ. DQUOTE )
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKSCAN' )
      END IF
 
C
C     The first time through, set up our identifier character set.
C
      IF ( PASS1 ) THEN
C
C        Each identifier must start with a letter (of either case).
C        The subsequent characters must be letters, numbers, dollar
C        signs or underscores.
C
         HDCHRS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' //
     .            'abcdefghijklmnopqrstuvwxyz'
 
         TLCHRS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' //
     .            'abcdefghijklmnopqrstuvwxyz' //
     .            '0123456789'                 //
     .            '$_'
 
 
         CALL SSIZEI ( MXSPEC,         IDSPEC )
         CALL LXCSID ( HDCHRS, TLCHRS, IDSPEC )
 
         PASS1  =  .FALSE.
 
      END IF
 
C
C     We'll work with a local copy of the query.
C
      L       =  RTRIM ( QUERY )
      TQUERY  =  QUERY(:L)
 
C
C     Initialize pointers and counts.
C
      CPTR   =  1
      NNUMS  =  0
      NSTRS  =  0
      CHCARD =  0
      NTOKEN =  0
 
C
C     Start out in the token search state.
C
      STATE  =  NXTTOK
 
      DO WHILE ( STATE .NE. TERM )
 
 
         IF ( STATE .EQ. NXTTOK ) THEN
C
C           In our initial state, we're looking for a new token.
C           We stop when we have enough characters to determine
C           which kind of token we have, or if we run out of
C           characters.
C
C           Set our character pointer to the beginning of the next
C           token.
C
            IF ( NTOKEN .GT. 0 ) THEN
              CPTR =  LXENDS(NTOKEN) + 1
            END IF
 
 
            IF ( CPTR .GT. L ) THEN
 
               STATE  =  TERM
 
            ELSE
 
 
               DO WHILE (       ( TQUERY(CPTR:CPTR) .EQ. ' ' )
     .                    .AND. ( CPTR              .LT. L   )  )
 
                  CPTR  =  CPTR + 1
               END DO
 
 
               IF ( TQUERY(CPTR:CPTR) .EQ. ' ' ) THEN
C
C                 We're out of non-blank characters to look at.
C
                  STATE  =  TERM
 
               ELSE
 
                  CHR    =  TQUERY(CPTR:CPTR)
                  STATE  =  NEWTOK
 
               END IF
 
 
            END IF
C
C           STATE is in the set {NEWTOK, TERM}.
C
 
 
         ELSE IF ( STATE .EQ. NEWTOK ) THEN
C
C           If we got this far, we have the initial character of
C           something that could be a valid token.  We test for
C
C              - quoted strings
C              - numbers
C              - identifiers
C              - special symbols
C
C           in that order.  Of course, we must have room in our output
C           arrays for the token.
C
            IF ( NTOKEN .EQ. MAXNTK ) THEN
 
               ERRMSG  =  'Maximum allowed number of tokens is #; at '//
     .                    'least # tokens are present in QUERY.'
               CALL REPMI ( ERRMSG, '#', MAXNTK,   ERRMSG )
               CALL REPMI ( ERRMSG, '#', MAXNTK+1, ERRMSG )
 
               SCNERR = .TRUE.
               CALL CHKOUT ( 'ZZEKSCAN' )
               RETURN
 
            END IF
 
 
            IF ( ISQUOT(CHR) ) THEN
 
               STATE = QSTR
 
            ELSE IF ( ISDOT(CHR) ) THEN
 
               STATE = DOT
 
            ELSE IF ( ISNUM(CHR) ) THEN
 
               STATE = NUMBER
 
            ELSE IF ( ISALPH(CHR) ) THEN
 
               STATE = IDENT
 
            ELSE
 
               STATE = SPCIAL
 
            END IF
 
C
C           At this point, the next value of STATE has been determined.
C           STATE is in the set
C
C              {QSTR, NUMBER, IDENT, SPCIAL}
C
 
 
         ELSE IF ( STATE .EQ. QSTR ) THEN
C
C           Look for a quoted string starting at location CPTR.
C           Use the current character as the quote character.
C
            CALL LXQSTR ( TQUERY, CHR, CPTR, LAST, NCHARS )
 
            IF ( NCHARS .EQ. 0 ) THEN
 
               ERRMSG = 'Invalid quoted string at location #.'
               CALL REPMI ( ERRMSG, '#', CPTR, ERRMSG )
 
               SCNERR = .TRUE.
               CALL CHKOUT ( 'ZZEKSCAN' )
               RETURN
 
            END IF
 
C
C           We've located a quoted string lexeme.  Parse the lexeme
C           and obtain the corresponding string value.  First make
C           sure we have enough room for the parsed string.
C
            ROOM  =  LEN(CHRBUF)-CHCARD
 
            IF ( NCHARS .GT. ROOM )  THEN
 
               ERRMSG  =  'Insufficient space to store quoted '    //
     .                    'string at location #; # chars needed; ' //
     .                    'only # are available.'
 
               CALL REPMI  ( ERRMSG, '#', CPTR,    ERRMSG )
               CALL REPMI  ( ERRMSG, '#', NCHARS,  ERRMSG )
               CALL REPMI  ( ERRMSG, '#', ROOM,    ERRMSG )
 
               SCNERR = .TRUE.
               CALL CHKOUT ( 'ZZEKSCAN' )
               RETURN
 
            END IF
 
 
            CALL PARSQS ( TQUERY(CPTR:CPTR+NCHARS-1),
     .                    CHR,
     .                    CHRBUF(CHCARD+1:),
     .                    LENGTH,
     .                    SCNERR,
     .                    ERRMSG,
     .                    PTR              )
 
 
            IF ( SCNERR ) THEN
 
               CALL PREFIX ( '#', 2, ERRMSG )
 
               CALL REPMC  ( ERRMSG,
     .                       '#',
     .                       'Error occurred while parsing '    //
     .                       'quoted string token at location #:',
     .                       ERRMSG                        )
 
               CALL REPMI  ( ERRMSG, '#', CPTR, ERRMSG )
 
               SCNERR = .TRUE.
 
               CALL CHKOUT ( 'ZZEKSCAN' )
               RETURN
 
            END IF
 
C
C           We've found a valid quoted string.  Set our outputs.
C
            NTOKEN          =  NTOKEN + 1
            TOKENS(NTOKEN)  =  TKQSTR
            NSTRS           =  NSTRS  + 1
            VALUES(NTOKEN)  =  NSTRS
            CHBEGS(NSTRS)   =  CHCARD + 1
            CHENDS(NSTRS)   =  CHCARD + LENGTH
            CHCARD          =  CHENDS(NSTRS)
            LXBEGS(NTOKEN)  =  CPTR
            LXENDS(NTOKEN)  =  LAST
 
            STATE           =  NXTTOK
 
C
C           STATE is now NXTTOK.
C
 
 
         ELSE IF ( STATE .EQ. DOT ) THEN
C
C           The token begins with a period.  We could be looking at
C           a floating point number, or we could be looking at a
C           period in a compound identifier.
C
C           Look for a number starting at location CPTR.
C
            CALL LX4NUM ( TQUERY, CPTR, LAST, NCHARS )
 
            IF ( NCHARS .GT. 0 ) THEN
               STATE  =  NUMBER
            ELSE
               STATE  =  SPCIAL
            END IF
 
C
C           STATE has been set to NUMBER or SPCIAL.  CPTR and NTOKEN
C           remain unchanged.
C
 
         ELSE IF ( STATE .EQ. NUMBER ) THEN
C
C           Look for a number starting at location CPTR.
C
            CALL LX4NUM ( TQUERY, CPTR, LAST, NCHARS )
 
 
            IF ( NCHARS .EQ. 0 ) THEN
 
               ERRMSG = 'Invalid numeric token at location #.'
               CALL REPMI ( ERRMSG, '#', CPTR, ERRMSG )
 
               SCNERR = .TRUE.
               CALL CHKOUT ( 'ZZEKSCAN' )
               RETURN
 
            END IF
 
C
C           Parse the token, but only do so if there's enough
C           room to store the result.
C
            ROOM  =  MAXNUM - NNUMS
 
            IF ( ROOM .LT. 1 ) THEN
 
               ERRMSG  =  'Insufficient space to store value of ' //
     .                    'number at location #; # elements are ' //
     .                    'available in the NUMVLS array; # are ' //
     .                    'required.'
 
               CALL REPMI  ( ERRMSG, '#', CPTR,     ERRMSG )
               CALL REPMI  ( ERRMSG, '#', MAXNUM,   ERRMSG )
               CALL REPMI  ( ERRMSG, '#', MAXNUM+1, ERRMSG )
 
               SCNERR = .TRUE.
               CALL CHKOUT ( 'ZZEKSCAN' )
               RETURN
 
            END IF
 
 
            CALL NPARSD ( TQUERY(CPTR:LAST),
     .                    NUMVLS(NNUMS+1),
     .                    ERRMSG,
     .                    PTR                 )
 
 
            IF ( ERRMSG .NE. ' ' ) THEN
C
C              This check is done for safety; by construction, we
C              should always have a valid number if LX4NUM
C              thinks we have a valid number, so in fact ERRMSG
C              should always be blank.
C
 
               CALL PREFIX ( '#', 2, ERRMSG )
 
               CALL REPMC  ( ERRMSG,
     .                       '#',
     .                       'Error found in numeric ' //
     .                       'token at location #:',
     .                       ERRMSG                        )
 
               CALL REPMI  ( ERRMSG, '#', CPTR+PTR-1, ERRMSG )
 
               SCNERR = .TRUE.
               CALL CHKOUT ( 'ZZEKSCAN' )
               RETURN
 
            END IF
 
C
C           We found a valid numeric token.  We distinguish
C           between integers and d.p. numbers; set the token
C           to the most restrictive category possible.
C
            NTOKEN  =  NTOKEN + 1
 
            IF (   BEINT( TQUERY(CPTR:LAST) )    ) THEN
               TOKENS(NTOKEN) =  TKINT
            ELSE
               TOKENS(NTOKEN) =  TKDP
            END IF
 
C
C           Set the rest of our outputs.
C
            NNUMS           =  NNUMS  + 1
            VALUES(NTOKEN)  =  NNUMS
            LXBEGS(NTOKEN)  =  CPTR
            LXENDS(NTOKEN)  =  LAST
 
            STATE           =  NXTTOK
 
C
C           STATE is now NXTTOK.
C
 
 
         ELSE IF ( STATE .EQ. IDENT ) THEN
C
C           Look for an identifier starting at location CPTR.
C
            CALL LXIDNT ( IDSPEC, TQUERY, CPTR, LAST, NCHARS )
 
 
            IF ( NCHARS .EQ. 0 ) THEN
C
C              This check is done for safety; by construction, we
C              should always have a valid identifier of at least one
C              character if we get to the IDENT state, so in fact
C              NCHARS should never equal zero.
C
               ERRMSG = 'Invalid identifier at location #.'
               CALL REPMI ( ERRMSG, '#', CPTR, ERRMSG )
 
               SCNERR = .TRUE.
               CALL CHKOUT ( 'ZZEKSCAN' )
               RETURN
 
            END IF
 
C
C           We've located an identifier lexeme.  Make sure we have
C           enough room for the string.
C
            ROOM  =  LEN(CHRBUF)-CHCARD
 
            IF ( NCHARS .GT. ROOM )  THEN
 
               ERRMSG  =  'Insufficient space to store identifier '//
     .                    'string at location #; # chars needed; ' //
     .                    'only # are available.'
 
               CALL REPMI  ( ERRMSG, '#', CPTR,   ERRMSG )
               CALL REPMI  ( ERRMSG, '#', NCHARS, ERRMSG )
               CALL REPMI  ( ERRMSG, '#', ROOM,   ERRMSG )
 
               SCNERR = .TRUE.
               CALL CHKOUT ( 'ZZEKSCAN' )
               RETURN
 
            END IF
 
C
C           We've found a valid identifier or keyword.  Set our
C           outputs.  Convert the string to upper case.
C
            NTOKEN =  NTOKEN + 1
 
            CALL UCASE (  TQUERY( CPTR:LAST ),
     .                    CHRBUF( CHCARD+1:CHCARD+NCHARS )  )
 
            I      =  BSRCHC ( CHRBUF( CHCARD+1 : CHCARD+NCHARS ),
     .                         NKEYWD,
     .                         KEYWDS                            )
 
 
            IF ( I .GT. 0 ) THEN
C
C              It's a keyword.
C
               TOKENS(NTOKEN)  =  TKKEY
               VALUES(NTOKEN)  =  KWVALS(I)
               LXBEGS(NTOKEN)  =  CPTR
               LXENDS(NTOKEN)  =  LAST
 
               STATE           =  NXTTOK
 
            ELSE
C
C              It's an identifier.
C
               NSTRS           =  NSTRS  + 1
               CHBEGS(NSTRS)   =  CHCARD + 1
               CHENDS(NSTRS)   =  CHCARD + NCHARS
               CHCARD          =  CHENDS(NSTRS)
               TOKENS(NTOKEN)  =  TKID
               VALUES(NTOKEN)  =  NSTRS
               LXBEGS(NTOKEN)  =  CPTR
               LXENDS(NTOKEN)  =  LAST
 
               STATE           =  NXTTOK
 
C
C              We finished scanning an identifier.
C
C              STATE is set to NXTTOK.
C
            END IF
C
C           We scanned a keyword or an identifier.
C
C           STATE is set to NXTTOK.
C
 
 
         ELSE IF ( STATE .EQ. SPCIAL ) THEN
C
C           Look for a valid token starting with a special character at
C           location CPTR. We attempt to match the longest possible
C           special token.
C
            I  =  MIN ( SPCLEN,   L - CPTR + 1 )
            J  =  0
 
            DO WHILE (  ( I .GE. 1 ) .AND. ( J .EQ. 0 ) )
 
               LAST =  CPTR + I - 1
               J    =  ISRCHC ( TQUERY(CPTR:LAST), NSPEC, SPCSTR )
 
               IF ( J .EQ. 0 ) THEN
                  I  =  I - 1
               END IF
 
            END DO
 
 
            IF ( J .GT. 0 ) THEN
C
C              We've identified a valid token.
C
               NTOKEN =  NTOKEN + 1
 
               TOKENS(NTOKEN)  =  SPCTOK(J)
               VALUES(NTOKEN)  =  SPCVAL(J)
               LXBEGS(NTOKEN)  =  CPTR
               LXENDS(NTOKEN)  =  CPTR - 1 + RTRIM( SPCSTR(J) )
 
               STATE           =  NXTTOK
 
            ELSE
 
               ERRMSG = 'Invalid character found at location #. '
               CALL REPMI  ( ERRMSG, '#', CPTR, ERRMSG )
C
C              If the offending character is printable, include it
C              in the error message.  Otherwise, include the integer
C              code for the character.
C
               IF ( FRSTPC(CHR) .GT. 0 ) THEN
 
                  CALL SUFFIX ( '<character> = ''#''',   2, ERRMSG )
                  CALL REPMC  ( ERRMSG, '#', CHR,           ERRMSG )
 
               ELSE
 
                  CALL SUFFIX ( 'ICHAR(<character>) = #', 2, ERRMSG )
                  CALL REPMI  ( ERRMSG, '#', ICHAR(CHR),     ERRMSG )
 
               END IF
 
               SCNERR = .TRUE.
               CALL CHKOUT ( 'ZZEKSCAN' )
               RETURN
 
            END IF
C
C           STATE is now NXTTOK.
C
 
         END IF
 
      END DO
 
C
C     If we got this far, we've found the tokens in the query.
C
      SCNERR  =  .FALSE.
      ERRMSG  =  ' '
 
      CALL CHKOUT ( 'ZZEKSCAN' )
      RETURN
      END
