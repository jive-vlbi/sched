C$Procedure      ZZEKPDEC ( EK, parse column declaration )
 
      SUBROUTINE ZZEKPDEC ( DECL, PARDSC )

      IMPLICIT NONE
 
C$ Abstract
C
C     Parse a declaration of a new EK column.
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
      INCLUDE 'ektype.inc'
 
      CHARACTER*(*)         DECL
      INTEGER               PARDSC ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     DECL       I   String containing column declaration.
C     PARDSC     O   Partial column descriptor.
C
C$ Detailed_Input
C
C     DECL           is a character string containing a column
C                    declaration.  Column declarations are strings that
C                    contain `keyword=value' assignments that define
C                    the attributes of the columns to which they apply.
C
C                    The attributes of a column defined by a
C                    declaration are:
C
C                       DATA TYPE
C                       <size>
C                       <is the column indexed?>
C                       <are null values allowed?>
C                       <is the column fixed-count?>
C
C                    The form of a column declaration is
C
C                      .'DATATYPE     = <type descriptor>,'           //
C                      .'[SIZE        = <size descriptor>],'          //
C                      .'[INDEXED     = <boolean>],'                  //
C                      .'[NULLS_OK    = <boolean>]'                   //
C                      .'[FIXED_COUNT = <boolean>]'
C
C                    The order of the assignments does not matter.
C
C                    Here <type descriptor> can be any of
C
C                       CHARACTER*<integer length>
C                       CHARACTER*(*)
C                       DOUBLE PRECISION
C                       INTEGER
C
C                    and the optional <size descriptor> can be either of
C
C                       <integer size>
C                       VARIABLE
C
C                    Character columns may not have both variable 
C                    string length and variable size.
C
C                    The column entry size defaults to 1 if the size
C                    descriptor is omitted.
C
C                    The optional clauses using the INDEXED, NULLS_OK,
C                    and FIXED_COUNT keywords take the values
C
C                       TRUE
C                       FALSE
C
C                    on the right-hand-sides of the equal signs.
C
C                    The INDEXED clause indicates that the column is
C                    indexed.  If the clause is omitted, the column is
C                    not indexed.  Only scalar-valued columns can be
C                    indexed.
C
C                    The NULLS_OK indicates that null values are
C                    permitted in the column; if the clause is omitted,
C                    null values are not permitted in the column.
C
C                    The FIXED_COUNT clause indicates that the column
C                    has a fixed number of entries; no records may be
C                    added to or deleted from the column.  If any
C                    column in a segment has a fixed record count, all
C                    columns in the segment must have the FIXED_COUNT
C                    attribute.
C
C                    FIXED_COUNT columns may be loaded only by the
C                    fast load routines.
C
C                    Unless the FIXED_COUNT keyword is used, the column
C                    does not have a fixed record count.
C
C                    Commas are required to separate the assignments
C                    within declarations.  White space is optional.
C                    Case is not significant.
C
C$ Detailed_Output
C
C     PARDSC         is an integer array that specifies the attributes
C                    of the column.  PARDSC is basically a
C                    partially-filled-in column descriptor:  it
C                    doesn't contain any pointer information.  In the
C                    locations where a column descriptor would contain
C                    an index pointer or a null flag array pointer,
C                    PARDSC contains boolean values indicating whether
C                    these items are supposed to be filled in later.
C
C                    The elements of PARDSC that are filled in upon
C                    return from this routine are:
C
C                       -- Class.  The column class is automatically
C                          determined from the declared attributes.
C
C                       -- Data type.
C
C                       -- String length, if applicable.  Variable-
C                          length strings are represented by a length
C                          specification of IFALSE.
C
C                       -- Column entry size.  Variable-size entries
C                          are represented by a size specification of
C                          IFALSE.
C
C                       -- The column's index type.  This element,
C                          which in a normal column descriptor contains
C                          an index type code, takes the boolean value
C                          ITRUE if the column is indexed and IFALSE
C                          otherwise.
C
C                       -- The column's null flag.  This element takes
C                          the boolean value ITRUE if the column can
C                          contain null values and is set to IFALSE
C                          otherwise.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input declaration does not conform to the specification
C         given in $Detailed_Input, the error SPICE(BADCOLUMNDECL) is
C         signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is a utility that supports EK writing subroutines.
C
C$ Examples
C
C     1)  Parse a declaration of an indexed column of 80-character
C         strings, in which null values are allowed:
C
C            CALL ZZEKPDEC ( 'DATATYPE = CHARACTER*80, '   //
C           .                'SIZE     = 1,'               //
C           .                'INDEXED  = TRUE',            //
C           .                'NULLS_OK = TRUE',
C           .                 PARDSC                       )
C
C
C        When ZZEKPDEC returns, the values of its output column
C        descriptor will be
C
C        When ZZEKPDEC returns, the value of its output argument
C        PARDSC will be
C
C           +---------------+
C           |       3       |  Class
C           +---------------+
C           |     <CHR>     |  Data type
C           +---------------+
C           |      80       |  String length
C           +---------------+
C           |       1       |  Size
C           +---------------+
C           |       0       |  Base addres of column name (not yet set)
C           +---------------+
C           |     ITRUE     |  Index type (ITRUE means col is indexed)
C           +---------------+
C           |       0       |  Index pointer
C           +---------------+
C           |     ITRUE     |  Null flag  (ITRUE means nulls are
C           +---------------+  allowed)
C           |       0       |  Ordinal position of column in segment
C           +---------------+
C           |       0       |  Metadata pointer
C           +---------------+
C           |       0       |  (Reserved)
C           +---------------+
C
C
C
C     2)  Parse a declaration of a variable-size column of 80-character
C         strings:
C
C            CALL ZZEKPDEC ( 'DATATYPE =  CHARACTER*80, '   //
C           .                'SIZE     =  VARIABLE',
C           .                 PARDSC                        )
C
C        When ZZEKPDEC returns, the value of its output argument
C        PARDSC will be
C
C           +---------------+
C           |       3       |  Class
C           +---------------+
C           |     <CHR>     |  Data type
C           +---------------+
C           |      80       |  String length
C           +---------------+
C           |    IFALSE     |  Size (IFALSE indicates variable size)
C           +---------------+
C           |       0       |  Base addres of column name (not yet set)
C           +---------------+
C           |     IFALSE    |  Index type (IFALSE means unindexed col)
C           +---------------+
C           |       0       |  Index pointer
C           +---------------+
C           |     IFALSE    |  Null flag  (IFALSE means nulls are not
C           +---------------+  allowed)
C           |       0       |  Ordinal position of column in segment
C           +---------------+
C           |       0       |  Metadata pointer
C           +---------------+
C           |       0       |  (Reserved)
C           +---------------+
C
C
C$ Restrictions
C
C     1) Currently does not diagnose extraneous keyword assignments.
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
C-    SPICELIB Version 1.1.0, 14-SEP-2005 (NJB)
C
C        Bug fix:  several error handling logic blocks were
C        missing SIGERR calls; these have been corrected.
C
C        Bug fix:  No diagnostic was issued for a declaration
C        of a variable-size, variable-string-length column.
C        This has been corrected.
C
C-    Beta Version 1.0.0, 16-NOV-1995 (NJB)
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               EQSTR
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               NATTR
      PARAMETER           ( NATTR  =   5 )
 
      INTEGER               MAXTOK
      PARAMETER           ( MAXTOK =  20 )
 
      INTEGER               TOKLEN
      PARAMETER           ( TOKLEN =  32 )
 
      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 320 )
 
      INTEGER               NRKEY
      PARAMETER           ( NRKEY  =   1 )
 
C
C     Parameters naming indices of keywords in the attribute list
C     ATTKEY:
C
      INTEGER               TYPPOS
      PARAMETER           ( TYPPOS =          1 )
 
      INTEGER               SIZPOS
      PARAMETER           ( SIZPOS = TYPPOS + 1 )
 
      INTEGER               IDXPOS
      PARAMETER           ( IDXPOS = SIZPOS + 1 )
 
      INTEGER               NFLPOS
      PARAMETER           ( NFLPOS = IDXPOS + 1 )
 
      INTEGER               FXCPOS
      PARAMETER           ( FXCPOS = NFLPOS + 1 )
 
 
C
C     Local variables
C
      CHARACTER*(TOKLEN)    ATTKEY ( NATTR )
      CHARACTER*(MSGLEN)    MSG
      CHARACTER*(TOKLEN)    TOKENS ( MAXTOK )
 
      INTEGER               ATTLOC ( CDSCSZ )
      INTEGER               I
      INTEGER               J
      INTEGER               N
      INTEGER               PTR
      INTEGER               REQKEY ( NRKEY  )
      INTEGER               TOKLOC
 
      LOGICAL               ATTFND ( CDSCSZ )
      LOGICAL               FOUND
 
C
C     Saved variables
C
      SAVE                  ATTKEY
      SAVE                  REQKEY
 
 
C
C     Initial values
C
      DATA                  ATTKEY         /
     .
     .                      'DATATYPE',
     .                      'SIZE',
     .                      'INDEXED',
     .                      'NULLS_OK',
     .                      'FIXED_COUNT'  /
 
 
      DATA                  REQKEY         /
     .                      1              /
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKPDEC' )
      END IF
 
C
C     Start with a clean slate.
C
      CALL CLEARI ( CDSCSZ, PARDSC )
 
C
C     Our declaration language has been cleverly designed so that the
C     characters
C
C        ','
C        '='
C
C     act as delimiters that LPARSM can make use
C     of.  LPARSM will hand us back a token list that contains these
C     pairs of consecutive tokens:
C
C        +----------------------+
C        | CLASS                |
C        +----------------------+
C        | <integer>            |
C        +----------------------+
C
C        +----------------------+
C        | DATATYPE             |
C        +----------------------+
C        | <type>               |
C        +----------------------+
C
C        +----------------------+
C        | SIZE                 |
C        +----------------------+
C        | <size specification> |  ( 'VARIABLE' or <integer> )
C        +----------------------+
C
C        +----------------------+
C        | INDEXED              |  (fixed-size columns only, optional)
C        +----------------------+
C        | <TRUE/FALSE>         |
C        +----------------------+
C
C        +----------------------+
C        | NULLS_OK             |  (optional)
C        +----------------------+
C        | <TRUE/FALSE>         |
C        +----------------------+
C
C
C     The order of the token pairs is not necessarily as shown.
C
C
      CALL LPARSM ( DECL, ',=', MAXTOK, N, TOKENS )
 
C
C     Make sure the tokens are in upper case.  They are already
C     left-justified.
C
      DO I = 1, N
         CALL UCASE( TOKENS(I), TOKENS(I) )
      END DO
 
C
C     See which clauses are present in the declaration, and keep track
C     of the token indices of the keywords that start the clauses.
C
      DO I = 1, NATTR
         ATTFND(I) = .FALSE.
      END DO
 
      DO I = 1, N
 
         J     = 1
         FOUND = .FALSE.
 
         DO WHILE (  ( J .LE. NATTR ) .AND. ( .NOT. FOUND )  )
 
            IF ( TOKENS(I) .EQ. ATTKEY(J) ) THEN
               FOUND     = .TRUE.
               ATTFND(J) = .TRUE.
               ATTLOC(J) =  I
            ELSE
               J         =  J + 1
            END IF
 
         END DO
 
      END DO
 
C
C     Make sure we got the required keyword tokens we were expecting.
C
      DO I = 1, NRKEY
 
         IF (  .NOT. ATTFND( REQKEY(I) ) ) THEN
 
            CALL SETMSG ( 'Required keyword # was not found in ' //
     .                    'column declaration #.'                )
            CALL ERRCH  ( '#',  ATTKEY( REQKEY(I) )              )
            CALL ERRCH  ( '#',  DECL                             )
            CALL SIGERR ( 'SPICE(BADCOLUMDECL)'                  )
            CALL CHKOUT ( 'ZZEKPDEC'                             )
            RETURN
 
         END IF
 
      END DO
 
C
C     If we got this far, we can start to fill in the data type
C     descriptor.  Starting at the location of the DATATYPE keyword,
C     we should see one of the following token sequences:
C
C        DATATYPE  =  DOUBLE PRECISION
C        DATATYPE  =  INTEGER
C        DATATYPE  =  TIME
C        DATATYPE  =  CHARACTER*<integer>
C        DATATYPE  =  CHARACTER*(<integer>)
C        DATATYPE  =  CHARACTER**
C        DATATYPE  =  CHARACTER*(*)
C
C     The character declarations may have white space surrounding
C     the length specifier.
C
C     Find the location where the data type token should be.
C
      TOKLOC = ATTLOC(TYPPOS) + 1
 
      IF ( N .LT. TOKLOC ) THEN
         CALL SETMSG (  'Column data type specification did not '     //
     .                  'follow "DATATYPE" keyword in declaration #.'  )
         CALL ERRCH  ( '#', DECL                                       )
         CALL SIGERR ( 'SPICE(BADCOLUMNDECL)'                          )
         CALL CHKOUT ( 'ZZEKPDEC'                                      )
         RETURN
      END IF
 
      IF ( TOKENS(TOKLOC) .EQ. 'INTEGER' ) THEN
 
         PARDSC(TYPIDX)  =  INT
         PARDSC(LENIDX)  =  1
 
      ELSE IF (  EQSTR( TOKENS(TOKLOC), 'DOUBLE PRECISION' )  ) THEN
 
         PARDSC(TYPIDX)  =  DP
         PARDSC(LENIDX)  =  1
 
      ELSE IF (  EQSTR( TOKENS(TOKLOC), 'TIME' )  ) THEN
 
         PARDSC(TYPIDX)  =  TIME
         PARDSC(LENIDX)  =  1
 
      ELSE IF (  TOKENS(TOKLOC)(1:9) .EQ. 'CHARACTER'  ) THEN
 
         PARDSC(TYPIDX)  =  CHR
C
C        To simplify picking up the length specification, compress
C        out blanks and parentheses.  This should leave us with
C        a token of the form
C
C           CHARACTER*<integer>
C
C        or
C
C           CHARACTER**
C
C
         CALL CMPRSS ( ' ', 0, TOKENS(TOKLOC), TOKENS(TOKLOC) )
         CALL CMPRSS ( '(', 0, TOKENS(TOKLOC), TOKENS(TOKLOC) )
         CALL CMPRSS ( ')', 0, TOKENS(TOKLOC), TOKENS(TOKLOC) )
 
 
         IF ( TOKENS(TOKLOC)(10:10) .NE. '*' ) THEN
 
            CALL SETMSG ( 'Required asterisk missing from character ' //
     .                    'column declaration:  #  in declaration:  ' //
     .                    '#'                                          )
            CALL ERRCH  ( '#', TOKENS(TOKLOC)                          )
            CALL ERRCH  ( '#', DECL                                    )
            CALL SIGERR ( 'SPICE(BADCOLUMNDECL)'                       )
            CALL CHKOUT ( 'ZZEKPDEC'                                   )
            RETURN
 
         END IF
 
 
         IF ( TOKENS(TOKLOC)(11:11)  .EQ.  '*'  ) THEN
C
C           The string length is variable.
C
            PARDSC(LENIDX) = IFALSE
 
         ELSE
C
C           The portion of the token following the asterisk should be a
C           string length.
C
            MSG = ' '
            CALL NPARSI (TOKENS(TOKLOC)(11:), PARDSC(LENIDX), MSG, PTR )
 
            IF ( MSG .NE. ' ' ) THEN
 
               CALL SETMSG ( 'String length specification # didn''t ' //
     .                       'parse as an integer in declaration   #'  )
               CALL ERRCH  ( '#',  TOKENS(TOKLOC)(11:)                 )
               CALL ERRCH  ( '#',  DECL                                )
               CALL SIGERR ( 'SPICE(BADCOLUMNDECL)'                    )
               CALL CHKOUT ( 'ZZEKPDEC'                                )
               RETURN
 
            END IF
 
         END IF
 
      ELSE
C
C        The type specification is invalid.
C
         CALL SETMSG ( 'Data type specification # is unrecognized in '//
     .                 'declaration #.'                                )
         CALL ERRCH  ( '#', TOKENS(TOKLOC)                             )
         CALL ERRCH  ( '#', DECL                                       )
         CALL SIGERR ( 'SPICE(BADCOLUMNDECL)'                          )
         CALL CHKOUT ( 'ZZEKPDEC'                                      )
         RETURN
 
      END IF
 
 
C
C     Next, parse the size specification, if we have one.  If it's
C     valid, it's either the string 'VARIABLE' or it's an integer.
C
      IF ( ATTFND(SIZPOS) ) THEN
 
         TOKLOC  =  ATTLOC(SIZPOS) + 1
 
         IF ( N .LT. TOKLOC ) THEN

            CALL SETMSG (  'Column size specification did not follow '//
     .                     '"SIZE" keyword in declaration #.'          )
            CALL ERRCH  ( '#', DECL                                    )
            CALL SIGERR ( 'SPICE(BADCOLUMNDECL)'                       )
            CALL CHKOUT ( 'ZZEKPDEC'                                   )
            RETURN

         END IF
 
 
         IF ( TOKENS(TOKLOC) .EQ. 'VARIABLE' ) THEN
C
C           Variable size entries are not allowed for CHARACTER*(*)
C           columns.
C
            IF ( PARDSC(TYPIDX) .EQ. CHR ) THEN
               
               IF ( PARDSC(LENIDX) .EQ. IFALSE ) THEN

                  CALL SETMSG (  'Column size specification was ' //
     .                           'VARIABLE for a CHARACTER*(*) '  //
     .                           'column in  declaration #.'      )
                  CALL ERRCH  ( '#', DECL                         )
                  CALL SIGERR ( 'SPICE(BADCOLUMNDECL)'            )
                  CALL CHKOUT ( 'ZZEKPDEC'                        )
                  RETURN

               END IF

            END IF


            PARDSC(SIZIDX) = IFALSE
 
         ELSE
 
            CALL NPARSI ( TOKENS(TOKLOC), PARDSC(SIZIDX), MSG,  PTR )
 
            IF ( MSG .NE. ' ' ) THEN
 
               CALL SETMSG ( 'Column element size  specification # '  //
     .                       'didn''t parse as an integer in '        //
     .                       'in declaration #'                        )
               CALL ERRCH  ( '#',  TOKENS(TOKLOC)                      )
               CALL ERRCH  ( '#',  DECL                                )
               CALL SIGERR ( 'SPICE(BADCOLUMNDECL)'                    )
               CALL CHKOUT ( 'ZZEKPDEC'                                )
               RETURN
 
            END IF
 
         END IF
 
      ELSE
C
C        If the size is not specified, it defaults to 1.
C
         PARDSC(SIZIDX) = 1
 
      END IF
 
C
C     The data type and entry size determine the column's class.
C
      IF ( PARDSC(TYPIDX) .EQ. CHR ) THEN
C
C        The character classes are 3 for scalars, 6 for arrays.
C
         IF ( PARDSC(SIZIDX) .EQ. 1 ) THEN
            PARDSC(CLSIDX) =  3
         ELSE
            PARDSC(CLSIDX) =  6
         END IF
 
 
      ELSE IF ( PARDSC(TYPIDX) .EQ. INT ) THEN
C
C        The integer classes are 1 for scalars, 4 for arrays.
C
         IF ( PARDSC(SIZIDX) .EQ. 1 ) THEN
            PARDSC(CLSIDX) =  1
         ELSE
            PARDSC(CLSIDX) =  4
         END IF
 
 
      ELSE IF (      ( PARDSC(TYPIDX) .EQ. DP   )
     .          .OR. ( PARDSC(TYPIDX) .EQ. TIME )  ) THEN
C
C        The d.p. classes are 2 for scalars, 6 for arrays.  TIME
C        values are represented using d.p. classes as well.
C
         IF ( PARDSC(SIZIDX) .EQ. 1 ) THEN
            PARDSC(CLSIDX) =  2
         ELSE
            PARDSC(CLSIDX) =  5
         END IF
 
      END IF
 
C
C     Parse the `NULLS_OK' clause, if we have one.
C
      IF ( ATTFND(NFLPOS) ) THEN
 
         TOKLOC = ATTLOC(NFLPOS) + 1
 
         IF ( N .LT. TOKLOC ) THEN
 
            CALL SETMSG (  'Boolean value did not follow '            //
     .                     '"NULLS_OK" keyword in declaration #.'      )
            CALL ERRCH  ( '#', DECL                                    )
            CALL SIGERR ( 'SPICE(BADCOLUMNDECL)'                       )
            CALL CHKOUT ( 'ZZEKPDEC'                                   )
            RETURN
 
         END IF
 
         IF ( TOKENS(TOKLOC) .EQ. 'TRUE' ) THEN
 
            PARDSC(NFLIDX)  =  ITRUE
 
         ELSE IF ( TOKENS(TOKLOC) .EQ. 'FALSE' ) THEN
 
            PARDSC(NFLIDX)  =  IFALSE
 
         ELSE
 
            CALL SETMSG ( 'Invalid token # follows NULLS_OK keyword ' //
     .                    'in declaration #. '                         )
            CALL ERRCH  ( '#',  TOKENS(TOKLOC)                         )
            CALL ERRCH  ( '#',  DECL                                   )
            CALL SIGERR ( 'SPICE(BADCOLUMNDECL)'                       )
            CALL CHKOUT ( 'ZZEKPDEC'                                   )
            RETURN
 
         END IF
 
 
      ELSE
C
C        As a default, nulls are not allowed.
C
         PARDSC(NFLIDX) = IFALSE
 
      END IF
 
 
 
C
C
C     Parse the `INDEXED' clause, if we have one.
C
      IF ( ATTFND(IDXPOS) ) THEN
 
         TOKLOC = ATTLOC(IDXPOS) + 1
 
         IF ( N .LT. TOKLOC ) THEN
 
            CALL SETMSG (  'Boolean value did not follow '            //
     .                     '"INDEXED" keyword in declaration #.'       )
            CALL ERRCH  ( '#', DECL                                    )
            CALL SIGERR ( 'SPICE(BADCOLUMNDECL)'                       )
            CALL CHKOUT ( 'ZZEKPDEC'                                   )
            RETURN
 
         END IF
 
 
         IF ( TOKENS(TOKLOC) .EQ. 'TRUE' ) THEN
C
C           If we have a fixed-size column whose size is 1, then it's
C           possible to index that column.  Otherwise, we should not
C           have an `INDEXED' clause.
C
            IF ( PARDSC(SIZIDX) .NE. 1 ) THEN
 
               CALL SETMSG ( 'Non-scalar columns cannot be indexed. ' //
     .                       'Declaration was #.'                      )
               CALL ERRCH  ( '#',  DECL                                )
               CALL SIGERR ( 'SPICE(BADCOLUMNDECL)'                    )
               CALL CHKOUT ( 'ZZEKPDEC'                                )
               RETURN
 
            END IF
 
            PARDSC(IXTIDX)  =  ITRUE
 
         ELSE IF ( TOKENS(TOKLOC) .EQ. 'FALSE' ) THEN
 
            PARDSC(IXTIDX)  =  IFALSE
 
         ELSE
 
            CALL SETMSG ( 'Invalid token # follows INDEXED keyword '  //
     .                    'in declaration #. '                         )
            CALL ERRCH  ( '#',  TOKENS(TOKLOC)                         )
            CALL ERRCH  ( '#',  DECL                                   )
            CALL SIGERR ( 'SPICE(BADCOLUMNDECL)'                       )
            CALL CHKOUT ( 'ZZEKPDEC'                                   )
            RETURN
 
         END IF
 
 
      ELSE
C
C        As a default, the column is not indexed.
C
         PARDSC(IXTIDX) = IFALSE
 
      END IF
 
 
 
 
C
C     Parse the `FIXED_COUNT' clause, if we have one.
C
      IF ( ATTFND(FXCPOS) ) THEN
 
         TOKLOC = ATTLOC(FXCPOS) + 1
 
         IF ( N .LT. TOKLOC ) THEN
 
            CALL SETMSG (  'Boolean value did not follow '            //
     .                     '"FIXED_COUNT" keyword in declaration #.'   )
            CALL ERRCH  ( '#', DECL                                    )
            CALL SIGERR ( 'SPICE(BADCOLUMNDECL)'                       )
            CALL CHKOUT ( 'ZZEKPDEC'                                   )
            RETURN
 
         END IF
 
         IF ( TOKENS(TOKLOC) .EQ. 'TRUE' ) THEN
C
C           The column is a fixed-count column.  Only scalar columns
C           are permitted to have fixed count.  We adjust the column
C           class to indicate fixed-count columns.
C
            IF ( PARDSC(CLSIDX) .EQ. 1 ) THEN
C
C              Map scalar integers.
C
               PARDSC(CLSIDX) =  7
 
            ELSE IF ( PARDSC(CLSIDX) .EQ. 2 ) THEN
C
C              Map scalar d.p. numbers.
C
               PARDSC(CLSIDX) =  8
 
            ELSE IF ( PARDSC(CLSIDX) .EQ. 3 ) THEN
C
C              Map scalar strings.
C
               PARDSC(CLSIDX) =  9
 
            ELSE
 
               CALL SETMSG ( 'FIXED_COUNT attribute used in non-'     //
     .                       'scalar column declaration #. '           )
               CALL ERRCH  ( '#',  DECL                                )
               CALL SIGERR ( 'SPICE(BADCOLUMNDECL)'                    )
               CALL CHKOUT ( 'ZZEKPDEC'                                )
               RETURN
 
            END IF
 
 
         ELSE IF ( TOKENS(TOKLOC) .NE. 'FALSE' ) THEN
C
C           No action is required if the FIXED_COUNT keyword is set
C           to FALSE, but no value other than FALSE or TRUE may appear
C           on the RHS.
C
            CALL SETMSG ( 'Invalid token # follows NULLS_OK keyword ' //
     .                    'in declaration #. '                         )
            CALL ERRCH  ( '#',  TOKENS(TOKLOC)                         )
            CALL ERRCH  ( '#',  DECL                                   )
            CALL SIGERR ( 'SPICE(BADCOLUMNDECL)'                       )
            CALL CHKOUT ( 'ZZEKPDEC'                                   )
            RETURN
 
         END IF
 
 
      END IF
 
 
      CALL CHKOUT ( 'ZZEKPDEC' )
      RETURN
      END
