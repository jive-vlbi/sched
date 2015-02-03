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
C
C     Include Section:  EK Encoded Query Internal Parameters
C
C        ekquery.inc  Version 3    16-NOV-1995 (NJB)
C
C           Updated to reflect increased value of MAXCON in
C           ekqlimit.inc.
C
C        ekquery.inc  Version 2    03-AUG-1995 (NJB)
C
C           Updated to support representation of the SELECT clause.
C
C
C        ekquery.inc  Version 1    12-JAN-1995 (NJB)
C
C
C     An encoded EK query is an abstract data type implemented
C     as an integer cell, along with a double precision cell and
C     a character string.  The d.p. cell and string contain numeric
C     and string values from the query string represented by the
C     encoded query.
C
C     The parameters in this file are intended for use only by the
C     EK encoded query access routines.  Callers of EK routines should
C     not use these parameters.
C
C     The following parameters are indices of specified elements
C     in the integer portion of the encoded query.
C
C     Encoded query architecture type:
C
      INTEGER               EQARCH
      PARAMETER           ( EQARCH = 2 )
 
C
C     `Name resolution' consists of:
C
C        - Verifying existence of tables:  any table names listed
C          in the FROM clause of a query must be loaded.
C
C        - Validating table aliases used to qualify column names.
C
C        - Verifying existence of columns and obtaining data types
C          for columns.
C
C        - Setting data type codes for literal values in the encoded
C          query.
C
C        - Checking consistency of operators and operand data types.
C
C        - Making sure unqualified column names are unambiguous.
C
C        - For constraints, mapping the table names used to qualify
C          column names to the ordinal position in the FROM clause
C          of the corresponding table.
C
C
C     Initialization status---this flag indicates whether the encoded
C     query has been initialized.  Values are ITRUE or IFALSE.  See the
C     include file ekbool.inc for parameter values.
C
      INTEGER               EQINIT
      PARAMETER           ( EQINIT = EQARCH + 1 )
 
C
C     Parse status---this flag indicates whether the parsing operation
C     that produced an encoded query has been completed. Values are
C     ITRUE or IFALSE.
C
      INTEGER               EQPARS
      PARAMETER           ( EQPARS = EQINIT + 1 )
 
C
C     Name resolution status---this flag indicates whether names
C     have been resolved in an encoded query.  Values are ITRUE or
C     IFALSE.
C
      INTEGER               EQNRES
      PARAMETER           ( EQNRES = EQPARS + 1 )
 
C
C     Time resolution status---this flag indicates whether time values
C     have been resolved in an encoded query.  Time resolution
C     consists of converting strings representing time values to ET.
C     Values of the status are ITRUE or IFALSE.
C
      INTEGER               EQTRES
      PARAMETER           ( EQTRES = EQNRES + 1 )
 
C
C     Semantic check status---this flag indicates whether semantic
C     checking of constraints has been performed.
C
      INTEGER               EQSCHK
      PARAMETER           ( EQSCHK = EQTRES + 1 )
 
C
C     Number of tables specified in FROM clause:
C
      INTEGER               EQNTAB
      PARAMETER           ( EQNTAB = EQSCHK + 1 )
 
C
C     Number of constraints in query:
C
      INTEGER               EQNCNS
      PARAMETER           ( EQNCNS = EQNTAB + 1 )
 
C
C     A special value is used to indicate the `maximal' constraint---
C     one that logically cannot be satisfied.  If the constraints
C     are equivalent to the maximal constraint, the location EQNCNS
C     is assigned the value EQMXML
C
      INTEGER               EQMXML
      PARAMETER           ( EQMXML = -1 )
 
C
C     Number of constraint conjunctions:
C
      INTEGER               EQNCNJ
      PARAMETER           ( EQNCNJ = EQNCNS + 1 )
 
C
C     Number of order-by columns:
C
      INTEGER               EQNORD
      PARAMETER           ( EQNORD = EQNCNJ + 1 )
 
C
C     Number of SELECT columns:
C
      INTEGER               EQNSEL
      PARAMETER           ( EQNSEL = EQNORD + 1 )
 
C
C     Size of double precision buffer:
C
      INTEGER               EQNSIZ
      PARAMETER           ( EQNSIZ = EQNSEL + 1 )
 
C
C     `Free' pointer into double precision buffer:
C
      INTEGER               EQNPTR
      PARAMETER           ( EQNPTR = EQNSIZ + 1 )
 
C
C     Size of character string buffer:
C
      INTEGER               EQCSIZ
      PARAMETER           ( EQCSIZ = EQNPTR + 1 )
 
C
C     `Free' pointer into character string buffer:
C
      INTEGER               EQCPTR
      PARAMETER           ( EQCPTR = EQCSIZ + 1 )
 
C
C     The following four base pointers will be valid after a query
C     has been parsed:
C
C     Base pointer for SELECT column descriptors:
C
      INTEGER               EQBSEL
      PARAMETER           ( EQBSEL = EQCPTR + 1 )
 
C
C     Base pointer for constraint descriptors:
C
      INTEGER               EQBCON
      PARAMETER           ( EQBCON = EQBSEL + 1 )
 
C
C     Base pointer for conjunction sizes:
C
      INTEGER               EQBCNJ
      PARAMETER           ( EQBCNJ = EQBCON + 1 )
 
C
C     Base pointer for order-by column descriptors:
C
      INTEGER               EQBORD
      PARAMETER           ( EQBORD = EQBCON + 1 )
 
 
C
C     After the quantities named above, the integer array contains
C     series of descriptors for tables, constraints, and order-by
C     columns, as well as a list of `conjunction sizes'---that is,
C     the sizes of the groups of constraints that form conjunctions,
C     after the input query has been re-arranged as a disjunction of
C     conjunctions of constraints.
C
      INTEGER               EQVBAS
      PARAMETER           ( EQVBAS = EQBORD )
 
C
C     The offsets of specific elements within descriptors are
C     parameterized. The base addresses of the descriptors themselves
C     must be  calculated using the counts and sizes of the items
C     preceding them.
C
C     A diagram of the structure of the variable-size portion of the
C     integer array is shown below:
C
C
C        +-------------------------------------+
C        | Fixed-size portion of encoded query |
C        +-------------------------------------+
C        |         Encoded FROM clause         |
C        +-------------------------------------+
C        |      Encoded constraint clause      |
C        +-------------------------------------+
C        |          Conjunction sizes          |
C        +-------------------------------------+
C        |       Encoded ORDER BY clause       |
C        +-------------------------------------+
C        |        Encoded SELECT clause        |
C        +-------------------------------------+
C
C
C     Value Descriptors
C     ----------------
C
C     In order to discuss the various descriptors below, we'll make use
C     of sub-structures called `value descriptors'.  These descriptors
C     come in two flavors:  character and double precision.  For
C     strings, a descriptor is a set of begin and end pointers that
C     indicate the location of the string in the character portion of an
C     encoded query, along with the begin and end pointers for the
C     corresponding lexeme in the original query.  The pointers are set
C     to zero when they are not in use, for example if they refer to an
C     optional lexeme that did not appear in the input query.
C
C     All value descriptors start with a data type indicator; values
C     are from ektype.inc.  Integer and time values are referred to
C     by double precision descriptors.
C
C     Parameters for string value descriptor elements:
C
      INTEGER               EQDTYP
      PARAMETER           ( EQDTYP = 1 )
 
      INTEGER               EQBLEX
      PARAMETER           ( EQBLEX = EQDTYP + 1 )
 
      INTEGER               EQELEX
      PARAMETER           ( EQELEX = EQBLEX + 1 )
 
      INTEGER               EQBSTR
      PARAMETER           ( EQBSTR = EQELEX + 1 )
 
      INTEGER               EQESTR
      PARAMETER           ( EQESTR = EQBSTR + 1 )
 
C
C     Numeric value descriptors are similar to those for string values,
C     the difference being that they have only one pointer to the value
C     they represent.  This pointer is the index of the value in the
C     encoded query's numeric buffer.
C
      INTEGER               EQVPTR
      PARAMETER           ( EQVPTR = EQELEX + 1 )
 
C
C     All value descriptors have the same size.  In order to allow
C     table descriptors to have the same size as value descriptors,
C     we include an extra element in the descriptor.
C
      INTEGER               EQVDSZ
      PARAMETER           ( EQVDSZ = 6 )
 
 
C
C     Column Descriptors
C     -----------------
C
C     Each column descriptor consists of a character descriptor for the
C     name of the column, followed by an index, which gives the ordinal
C     position of the column in the logical table to which the column
C     belongs.  The index element is filled in during name resolution.
C
      INTEGER               EQBCOL
      PARAMETER           ( EQBCOL = 1 )
 
      INTEGER               EQCIDX
      PARAMETER           ( EQCIDX = EQVDSZ )
 
C
C     Table Descriptors
C     -----------------
C
C     Each table descriptor consists of a character descriptor for the
C     name of the table, followed by an index, which gives the ordinal
C     position of the table in the FROM clause in the original query.
C     The index element is filled in during name resolution.  Aliases
C     and table names have identical descriptor structures.
C
      INTEGER               EQBTAB
      PARAMETER           ( EQBTAB = 1 )
 
      INTEGER               EQTORD
      PARAMETER           ( EQTORD = EQVDSZ )
 
C
C     Constraint descriptors
C     ------------------
C
C     Each constraint is characterized by:
C
C        - A code indicating whether the constraint compares values
C          in two columns or the value in a column and a literal
C          value.  The values of this element are EQCOL and EQVAL.
C
      INTEGER               EQCTYP
      PARAMETER           ( EQCTYP = 1 )
 
      INTEGER               EQCOL
      PARAMETER           ( EQCOL  = 1 )
 
      INTEGER               EQVAL
      PARAMETER           ( EQVAL  = 2 )
 
C
C
C        - A descriptor for the table used to qualify the
C          column name on the left side of the constraint.
C
      INTEGER               EQLTAB
      PARAMETER           ( EQLTAB = EQCTYP + 1 )
 
C
C        - A character value descriptor for the column name on the left
C          side of the query.
C
      INTEGER               EQLCOL
      PARAMETER           ( EQLCOL = EQLTAB + EQVDSZ )
 
C
C        - An operator code indicating the relational operator used
C          in the constraint.
C
      INTEGER               EQOPCD
      PARAMETER           ( EQOPCD = EQLCOL + EQVDSZ )
 
C
C        If the constraint compares values from two columns, the
C        next items are table and column name descriptors that apply to
C        the column named on the right side of the relational operator.
C
      INTEGER               EQRTAB
      PARAMETER           ( EQRTAB = EQOPCD + 1 )
 
      INTEGER               EQRCOL
      PARAMETER           ( EQRCOL = EQRTAB + EQVDSZ )
 
 
C
C        If the constraint has a literal value on the right side, the
C        operator code is followed by...
C
C        - a value descriptor.
C
      INTEGER               EQBVAL
      PARAMETER           ( EQBVAL = EQOPCD + 1 )
 
 
C
C        - Size of a constraint descriptor:
C
      INTEGER               EQCDSZ
      PARAMETER           ( EQCDSZ = 2 + 4*EQVDSZ )
 
 
C
C     Conjunction sizes
C     -----------------
C
C     The size of each conjunction of constraints occupies a single
C     integer.
C
C
C
C
C     Order-by Column Descriptors
C     ---------------------------
C
C     Each order-by column descriptor contains descriptors for
C     the table containing the column and for the name of the column
C     itself; one additional element is used to indicate the direction
C     of the ordering (ascending vs descending).
C
      INTEGER               EQOTAB
      PARAMETER           ( EQOTAB = 1 )
 
      INTEGER               EQOCOL
      PARAMETER           ( EQOCOL = EQOTAB + EQVDSZ )
 
C
C        - The last integer in the descriptor indicates whether the
C          order direction is ascending or descending.
C
      INTEGER               EQODIR
      PARAMETER           ( EQODIR = EQOCOL + EQVDSZ )
C
C        - Size of an order-by column descriptor:
C
      INTEGER               EQODSZ
      PARAMETER           ( EQODSZ = 1 + 2*EQVDSZ )
 
C
C     Codes indicating sense of ordering (ascending vs descending):
C
      INTEGER               EQASND
      PARAMETER           ( EQASND = 0 )
 
      INTEGER               EQDSND
      PARAMETER           ( EQDSND = 1 )
 
C
C     SELECT Column Descriptors
C     ---------------------------
C
C     Each SELECT column descriptor contains descriptors for
C     the table containing the column and for the name of the column
C     itself.
C
      INTEGER               EQSTAB
      PARAMETER           ( EQSTAB = 1 )
 
      INTEGER               EQSCOL
      PARAMETER           ( EQSCOL = EQSTAB + EQVDSZ )
 
C
C        - Size of a SELECT column descriptor:
C
      INTEGER               EQSDSZ
      PARAMETER           ( EQSDSZ = 2*EQVDSZ )
 
 
C
C     Miscellaneous parameters:
C
C
C     EQIMIN is the minimum size of the integer portion of
C     an encoded query.  EQIMIN depends on the parameters
C
C        MAXTAB
C        MAXCON
C        MAXORD
C        MAXSEL
C
C     all of which are declared in the include file ekqlimit.inc.
C     The functional definition of EQIMIN is:
C
C     INTEGER               EQIMIN
C     PARAMETER           ( EQIMIN =   EQVBAS
C    .                              +  MAXTAB * EQVDSZ * 2
C    .                              +  MAXCON * EQCDSZ
C    .                              +  MAXCON
C    .                              +  MAXORD * EQODSZ
C    .                              +  MAXSEL * EQSDSZ     )
C
 
      INTEGER               EQIMIN
      PARAMETER           ( EQIMIN =   EQVBAS
     .                              +  10     * EQVDSZ * 2
     .                              +  1000   * EQCDSZ
     .                              +  1000
     .                              +  10     * EQODSZ
     .                              +  50     * EQSDSZ     )
 
C
C     End Include Section:  EK Encoded Query Internal Parameters
C
