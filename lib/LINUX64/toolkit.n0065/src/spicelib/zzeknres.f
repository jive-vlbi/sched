C$Procedure  ZZEKNRES ( Private: EK, resolve names in encoded query )
 
      SUBROUTINE ZZEKNRES ( QUERY, EQRYI, EQRYC, ERROR, ERRMSG, ERRPTR )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Resolve and semantically check table names, aliases, and column
C     names in an encoded EK query.
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
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'ekquery.inc'
      INCLUDE 'ektnamsz.inc'
 
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
C     EQRYI     I-O  Integer component of query.
C     EQRYC     I-O  Character component of query.
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
C$ Detailed_Output
C
C     EQRYI          is the integer portion of an encoded EK query.
C                    On output, all names have been resolved, and
C                    table names, aliases, and column names have
C                    been semantically checked.
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
C           - All tables named in the FROM clause must be loaded
C             in the EK system.
C
C           - All aliases in the FROM clause must be distinct.
C
C           - No alias may be the name of a table in the FROM clause,
C             unless it is identical to the name of the table it is
C             associated with.
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
C     Resolution of table names involves finding each table's ordinal
C     position in the FROM clause, and setting the table's descriptor
C     to record that position.  The same is done for column descriptors.
C
C$ Examples
C
C     See EKFIND.
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
C-    Beta Version 1.0.0, 17-OCT-1995 (NJB)
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
      INTEGER               SHORT
      PARAMETER           ( SHORT  = 32 )
 
C
C     Local variables
C
      CHARACTER*(TNAMSZ)    ALIAS  ( MAXTAB )
      CHARACTER*(TNAMSZ)    LTABLE
      CHARACTER*(TNAMSZ)    TABLE  ( MAXTAB )
 
      INTEGER               BASE
      INTEGER               CC     ( MAXTAB )
      INTEGER               CNSTYP
      INTEGER               I
      INTEGER               IPARSE
      INTEGER               J
      INTEGER               LXB
      INTEGER               LXE
      INTEGER               NCNJ
      INTEGER               NCNS
      INTEGER               NLOAD
      INTEGER               NORD
      INTEGER               NSEL
      INTEGER               NTAB
 
      LOGICAL               FND
 
C
C     No error to start with.
C
      ERROR  =  .FALSE.
      ERRMSG =  ' '
      ERRPTR =  0
 
C
C     The query must have been parsed at this point, or it's no go.
C
      CALL ZZEKREQI ( EQRYI, 'PARSED', IPARSE )
 
      IF ( FAILED() ) THEN
         RETURN
      END IF
 
      IF ( IPARSE .EQ. IFALSE ) THEN
 
         CALL CHKIN  ( 'ZZEKNRES'                            )
         CALL SETMSG ( 'Encoded query has not been parsed.'  )
         CALL SIGERR ( 'SPICE(QUERYNOTPARSED)'               )
         CALL CHKOUT ( 'ZZEKNRES'                            )
         RETURN
 
      END IF
 
C
C     Get the important counts from the query.
C
      CALL ZZEKREQI ( EQRYI, 'NUM_TABLES',       NTAB )
      CALL ZZEKREQI ( EQRYI, 'NUM_CONSTRAINTS',  NCNS )
      CALL ZZEKREQI ( EQRYI, 'NUM_CONJUNCTIONS', NCNJ )
      CALL ZZEKREQI ( EQRYI, 'NUM_ORDERBY_COLS', NORD )
      CALL ZZEKREQI ( EQRYI, 'NUM_SELECT_COLS',  NSEL )
 
C
C     Start out by fetching the table names and their aliases.
C
      DO I = 1, NTAB
         CALL ZZEKQTAB ( EQRYI, EQRYC, I, TABLE(I), ALIAS(I) )
      END DO
 
C
C     Make sure that the aliases are distinct.  Rather than sorting
C     them, we'll check them in left-to-right order.
C
      DO I = 1, NTAB - 1
 
         DO J = I+1, NTAB
 
            IF (      (  ALIAS(I) .EQ. ALIAS(J)  )
     .          .AND. (  ALIAS(I) .NE. ' '       )  ) THEN
 
               ERROR  =  .TRUE.
               ERRMSG =  'Non-distinct alias <#> was found.'
 
               BASE   =  EQVBAS  +  ( (J-1)*2 + 1 ) * EQVDSZ
               LXB    =  EQRYI ( BASE + EQBLEX )
               LXE    =  EQRYI ( BASE + EQELEX )
 
               CALL REPMC ( ERRMSG, '#', QUERY(LXB:LXE), ERRMSG )
 
               ERRPTR = LXB
 
               RETURN
 
            END IF
C
C           We've checked the Jth alias for a match.
C
         END DO
 
      END DO
 
C
C     Make sure that no alias matches a table name other than that of
C     the table it corresponds to.
C
      DO I = 1, NTAB
 
         J  =  ISRCHC ( ALIAS(I), NTAB, TABLE )
 
         IF ( J .NE. 0 ) THEN
 
            IF ( J .NE. I ) THEN
 
               ERROR  =  .TRUE.
               ERRMSG =  'Alias <#> conflicts with table name.'
 
               BASE   =  EQVBAS  +  ( (I-1)*2 + 1 ) * EQVDSZ
               LXB    =  EQRYI ( BASE + EQBLEX )
               LXE    =  EQRYI ( BASE + EQELEX )
 
               CALL REPMC ( ERRMSG, '#', QUERY(LXB:LXE), ERRMSG )
 
               ERRPTR = LXB
 
               RETURN
 
            END IF
 
         END IF
 
      END DO
 
 
C
C     Make sure that all of the tables are loaded in the EK system.
C
      CALL EKNTAB ( NLOAD )
 
      DO I = 1, NTAB
 
 
         FND  =  .FALSE.
         J    =   1
 
         DO WHILE (  ( J .LE. NLOAD ) .AND. ( .NOT. FND )  )
 
            CALL EKTNAM ( J, LTABLE )
 
            IF (  TABLE(I) .EQ. LTABLE  ) THEN
C
C              When we find a loaded table, save the column count for
C              that table.
C
               FND  =  .TRUE.
               CALL EKCCNT ( TABLE, CC(I) )
            ELSE
               J    =   J + 1
            END IF
 
         END DO
 
 
         IF ( .NOT. FND ) THEN
 
            ERROR  = .TRUE.
            ERRMSG =  'Table <#> is not currently loaded.'
C
C           In order to set the error pointer, we'll need the
C           lexeme begin value for the offending table.
C
            BASE   =  EQVBAS  +   (I-1)* 2 * EQVDSZ
 
            LXB    =  EQRYI ( BASE + EQBLEX )
            LXE    =  EQRYI ( BASE + EQELEX )
 
            CALL REPMC ( ERRMSG, '#', QUERY(LXB:LXE), ERRMSG )
 
            ERRPTR = LXB
 
            RETURN
 
         END IF
 
 
      END DO
 
C
C     At this point, the tables and aliases are deemed correct.  For
C     safety, fill in each table and alias descriptor with its
C     ordinal position.
C
      DO I = 1, NTAB
 
         BASE  =  EQVBAS + (I-1)*2*EQVDSZ
 
         EQRYI( BASE          + EQTORD ) = I
         EQRYI( BASE + EQVDSZ + EQTORD ) = I
 
      END DO
 
 
C
C     Check the column names used in the constraints.
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
C        Check the column and table on the LHS of the constraint.
C
         CALL ZZEKCCHK ( QUERY,  EQRYI,   EQRYC,
     .                   NTAB,   TABLE,   ALIAS,  BASE+EQLTAB-1,
     .                   ERROR,  ERRMSG,  ERRPTR                )
 
         IF ( ERROR ) THEN
            RETURN
         END IF
 
 
         IF ( CNSTYP .EQ. EQCOL ) THEN
C
C           Check the column and table on the RHS of the constraint.
C
            CALL ZZEKCCHK ( QUERY,  EQRYI,   EQRYC,
     .                      NTAB,   TABLE,   ALIAS,  BASE+EQRTAB-1,
     .                      ERROR,  ERRMSG,  ERRPTR                )
 
            IF ( ERROR ) THEN
               RETURN
            END IF
 
         END IF
 
 
      END DO
 
 
C
C     Do the same checks and assignments for the SELECT columns.
C
      DO I = 1, NSEL
C
C        Calculate the base address of the SELECT column descriptor.
C
         BASE   =       EQVBAS
     .              +   NTAB * 2 * EQVDSZ
     .              +   NCNJ
     .              +   NCNS     * EQCDSZ
     .              +   NORD     * EQODSZ
     .              +   (I-1)    * EQSDSZ
 
         CALL ZZEKCCHK ( QUERY,  EQRYI,   EQRYC,
     .                   NTAB,   TABLE,   ALIAS,  BASE,
     .                   ERROR,  ERRMSG,  ERRPTR        )
 
         IF ( ERROR ) THEN
            RETURN
         END IF
 
      END DO
 
 
C
C     Do the same checks and assignments for the order-by columns.
C
      DO I = 1, NORD
C
C        Calculate the base address of the order-by column descriptor.
C
         BASE   =       EQVBAS
     .              +   NTAB * 2 * EQVDSZ
     .              +   NCNJ
     .              +   NCNS     * EQCDSZ
     .              +   (I-1)    * EQODSZ
 
         CALL ZZEKCCHK ( QUERY,  EQRYI,   EQRYC,
     .                   NTAB,   TABLE,   ALIAS,  BASE,
     .                   ERROR,  ERRMSG,  ERRPTR        )
 
         IF ( ERROR ) THEN
            RETURN
         END IF
 
      END DO
 
 
C
C     Indicate completion of name resolution.
C
      CALL ZZEKWEQI ( 'NAMES_RESOLVED',  ITRUE,  EQRYI  )
 
      END
