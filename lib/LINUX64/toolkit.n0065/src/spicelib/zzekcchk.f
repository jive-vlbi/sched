C$Procedure  ZZEKCCHK ( Private: EK, check names in encoded query )
 
      SUBROUTINE ZZEKCCHK (  QUERY,  EQRYI,  EQRYC,  NTAB,    TABLST,
     .                       ALSLST, BASE,   ERROR,  ERRMSG,  ERRPTR  )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Check and resolve a specified column name in an encoded EK query.
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
C     None.
C
C$ Keywords
C
C     PRIVATE
C
C$ Declarations
 
      INCLUDE 'ekattdsc.inc'
      INCLUDE 'ekbool.inc'
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'ekquery.inc'
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      CHARACTER*(*)         QUERY
      INTEGER               EQRYI ( LBCELL : * )
      CHARACTER*(*)         EQRYC
      INTEGER               NTAB
      CHARACTER*(*)         TABLST ( * )
      CHARACTER*(*)         ALSLST ( * )
      INTEGER               BASE
      LOGICAL               ERROR
      CHARACTER*(*)         ERRMSG
      INTEGER               ERRPTR
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     EQRYI     I-O  Integer component of query.
C     EQRYC     I-O  Character component of query.
C     NTAB       I   Number of tables in FROM clause.
C     TABLST     I   List of table names.
C     ALSLST     I   List of table aliases.
C     BASE       I   Base address of table/column descriptor pair.
C     ERROR      O   Error flag.
C     ERRMSG     O   Error message.
C     ERRPTR     O   Position in query where error was detected.
C
C$ Detailed_Input
C
C     QUERY          is the original query from which EQRYI and EQRYC
C                    were obtained.  QUERY is used only for
C                    construction of error messages.
C
C     EQRYI          is the integer portion of an encoded EK query.
C                    The query must have been parsed.
C
C     EQRYC          is the character portion of an encoded EK query.
C
C     NTAB           is the number of tables in the FROM clause of
C                    the input query.
C
C     TABLST         is a list of table names present in the FROM
C                    clause of the input query.
C
C     ALSLST         is a list of table aliases present in the FROM
C                    clause of the input query.  Absent aliases are
C                    represented by blank strings.
C
C     BASE           is the base address of a pair of descriptors
C                    for a qualified column.  The column may appear on
C                    either side of a query constraint, or it may
C                    appear in an order-by clause.
C
C$ Detailed_Output
C
C     EQRYI          is the integer portion of an encoded EK query.
C                    On output, the specified column will have been
C                    resolved and checked.  Specifically, the table
C                    descriptor will have the ordinal position of the
C                    table in the FROM clause filled in, and the
C                    index of the column within the virtual table
C                    containing it will be filled in.
C
C     EQRYC          is the character portion of an encoded EK query.
C
C     ERROR          is a logical flag indicating whether an error was
C                    detected.  The error could be a name resolution
C                    error or a semantic error.
C
C     ERRMSG         is an error message describing an error in the
C                    input query, if one was detected.  If ERROR is
C                    returned .FALSE., then ERRPTR is undefined.
C
C     ERRPTR         is the character position in the original query
C                    at which an error was detected, if an error was
C                    found.  This index refers to the offending lexeme's
C                    position in the original query represented by the
C                    input encoded query.  If ERROR is returned .FALSE.,
C                    ERRPTR is undefined.
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
C     2)  If the input query has not been parsed, the error
C         SPICE(QUERYNOTPARSED) will be signalled.  The outputs
C         will not be modified.
C
C     3)  If any sort of name resolution error or semantic error is
C         detected in the input query, the output flag ERROR is set,
C         and an error message is returned.  The checks performed by
C         this routine are listed below:
C
C           - No column name may be qualified with a name that is not
C             the name or alias of a table in the FROM clause.
C
C           - Each qualified column must be present in the table
C             indicated by its qualifying name.
C
C           - Each unqualified column name must be the name of a
C             column present in exactly one of the tables listed in the
C             FROM clause.
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine exists for the sole purpose of centralizing code
C     used in multiple places by the name resolver ZZEKNRES.
C
C     This routine assumes that encoded EK query architecture version
C     1 is to be used with the query to be initialized; this routine
C     will not work with any other architecture version.
C
C$ Examples
C
C     See ZZEKNRES.
C
C$ Restrictions
C
C     1) This routine relies on the internals of the EK encoded query
C        structure.
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
C-    SPICELIB Version 1.1.0, 15-OCT-1996 (NJB)
C
C        Error checking of column string bounds was added.
C
C-    SPICELIB Version 1.0.0, 26-SEP-1995 (NJB)
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 15-OCT-1996 (NJB)
C
C        Error checking of column string bounds was added.  These
C        bounds should never be out of range, but if they are, the
C        error diagnosis should be more graceful than a memory
C        violation.
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
 
C
C     Local variables
C
      CHARACTER*(CNAMSZ)    COLUMN
      CHARACTER*(1)         CTOUCH
 
      INTEGER               ATTDSC ( ADSCSZ )
      INTEGER               CB
      INTEGER               CC
      INTEGER               CE
      INTEGER               COLIDX
      INTEGER               I
      INTEGER               IPARSE
      INTEGER               J
      INTEGER               LXB ( 2 )
      INTEGER               NMATCH
      INTEGER               TABIDX
      INTEGER               TB
      INTEGER               TE
 
      LOGICAL               FND
      LOGICAL               NONAME
 
C
C     No error to start with.
C
      ERROR  =  .FALSE.
      ERRMSG =  ' '
      ERRPTR =  0
 
      CTOUCH =  QUERY(1:1)
 
C
C     The query must have been parsed at this point, or it's no go.
C
      CALL ZZEKREQI ( EQRYI, 'PARSED', IPARSE )
 
      IF ( FAILED() ) THEN
         RETURN
      END IF
 
      IF ( IPARSE .EQ. IFALSE ) THEN
 
         CALL CHKIN  ( 'ZZEKCCHK'                            )
         CALL SETMSG ( 'Encoded query has not been parsed.'  )
         CALL SIGERR ( 'SPICE(QUERYNOTPARSED)'               )
         CALL CHKOUT ( 'ZZEKCCHK'                            )
         RETURN
 
      END IF
 
C
C     Get the name and lexeme pointers for both the table and column.
C     Decide whether a table name was supplied.
C
      TB      =  EQRYI ( BASE + EQBSTR )
      TE      =  EQRYI ( BASE + EQESTR )
      LXB(1)  =  EQRYI ( BASE + EQBLEX )
 
      CB      =  EQRYI ( BASE + EQVDSZ + EQBSTR )
      CE      =  EQRYI ( BASE + EQVDSZ + EQESTR )
      LXB(2)  =  EQRYI ( BASE + EQVDSZ + EQBLEX )
 
      IF (  ( CB .LE. 0 )  .OR.  ( CE .LE. 0 )  ) THEN
 
         CALL CHKIN  ( 'ZZEKCCHK'                                )
         CALL SETMSG ( 'Invalid string bounds #:# for column.  ' //
     .                 'Column name descriptor base is #.'       )
         CALL ERRINT ( '#',  CB                                  )
         CALL ERRINT ( '#',  CE                                  )
         CALL ERRINT ( '#',  BASE                                )
         CALL SIGERR ( 'SPICE(BUG)'                              )
         CALL CHKOUT ( 'ZZEKCCHK'                                )
         RETURN
 
      END IF
 
 
      NONAME  =  TB .EQ. 0
 
 
      IF ( NONAME ) THEN
C
C        If no table name is present, search for the LHS column among
C        the tables in the FROM clause.  If exactly one table
C        contains the column, that table is considered to be the
C        qualifying table.  Otherwise, the qualification is in error.
C
         NMATCH = 0
 
         DO I = 1, NTAB
 
            CALL EKCCNT ( TABLST(I), CC  )
 
            DO J = 1, CC
 
               CALL EKCII ( TABLST(I),  J,  COLUMN,  ATTDSC )
 
               IF (  EQRYC(CB:CE) .EQ. COLUMN ) THEN
                  NMATCH  =  NMATCH + 1
                  COLIDX  =  J
                  TABIDX  =  I
               END IF
 
            END DO
 
         END DO
 
 
         IF ( NMATCH .EQ. 0 ) THEN
 
            ERROR  = .TRUE.
            ERRMSG = 'Column <#> is not present in any table in FROM '//
     .               'clause.'
 
            CALL REPMC ( ERRMSG, '#', EQRYC(CB:CE), ERRMSG )
 
            ERRPTR = LXB(2)
            RETURN
 
 
         ELSE IF ( NMATCH .GT. 1 ) THEN
 
            ERROR  = .TRUE.
            ERRMSG = 'Column name <#> is ambiguous; a qualifying '//
     .               'table name or alias is required.'
 
            CALL REPMC ( ERRMSG, '#', EQRYC(CB:CE), ERRMSG )
 
            ERRPTR = LXB(2)
            RETURN
 
 
         END IF
 
 
 
      ELSE
 
C
C        Find the qualifying name in the FROM table list.  If the
C        name is not there, look in the alias list.
C
         TABIDX  =  ISRCHC ( EQRYC(TB:TE), NTAB, TABLST )
 
         IF ( TABIDX .EQ. 0 ) THEN
            TABIDX  =  ISRCHC ( EQRYC(TB:TE), NTAB, ALSLST )
         END IF
 
C
C        If the table name wasn't in either list, we can't use it.
C
         IF ( TABIDX .EQ. 0 ) THEN
 
            ERROR  = .TRUE.
            ERRMSG = 'Table name <#> is not present in FROM clause.'
 
            CALL REPMC ( ERRMSG, '#', EQRYC(TB:TE), ERRMSG )
 
            ERRPTR = LXB(1)
            RETURN
 
         END IF
 
C
C        Check the column.  This column must be present in the
C        table that qualifies it.
C
         CALL EKCCNT ( TABLST(TABIDX), CC  )
 
 
         FND  =  .FALSE.
         I    =   1
 
         DO WHILE (  ( I .LE. CC )  .AND. ( .NOT. FND )  )
 
            CALL EKCII ( TABLST(TABIDX),  I,  COLUMN,  ATTDSC )
 
            IF ( EQRYC(CB:CE) .EQ. COLUMN ) THEN
               FND     =  .TRUE.
               COLIDX  =  I
            ELSE
               I       =  I + 1
            END IF
 
         END DO
 
 
         IF ( .NOT. FND ) THEN
 
            ERROR  =  .TRUE.
            ERRMSG =  'Column <#> does not exist in table <#>.'
 
            CALL REPMC ( ERRMSG, '#', EQRYC(CB:CE), ERRMSG )
            CALL REPMC ( ERRMSG, '#', EQRYC(TB:TE), ERRMSG )
 
            ERRPTR = LXB(2)
            RETURN
 
         END IF
 
 
      END IF
 
 
C
C     If we got this far, the table and column check out.  Fill in the
C     table and column indices in their respective descriptors.
C
      EQRYI ( BASE +          EQTORD )  =  TABIDX
      EQRYI ( BASE + EQVDSZ + EQCIDX )  =  COLIDX
 
      RETURN
      END
