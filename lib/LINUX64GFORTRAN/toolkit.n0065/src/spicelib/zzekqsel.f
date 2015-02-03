C$Procedure  ZZEKQSEL ( Private: EK, read SELECT columns from query )
 
      SUBROUTINE ZZEKQSEL ( EQRYI,  EQRYC,   N,       LXBEG,  LXEND,
     .                      TABLE,  TABIDX,  COLUMN,  COLIDX        )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Read a specified SELECT clause table and column name from an
C     encoded EK query.
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
 
      INCLUDE 'ekquery.inc'
      INCLUDE 'ekbool.inc'
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               EQRYI ( LBCELL : * )
      CHARACTER*(*)         EQRYC
      INTEGER               N
      INTEGER               LXBEG
      INTEGER               LXEND
      CHARACTER*(*)         TABLE
      INTEGER               TABIDX
      CHARACTER*(*)         COLUMN
      INTEGER               COLIDX
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     EQRYI      I   Integer component of query.
C     EQRYC      I   Character component of query.
C     N          I   Index within SELECT clause of table name to read.
C     LXBEG      O   Begin index in query of selected item.
C     LXEND      O   End index in query of selected item.
C     TABLE      O   Name of table qualifying Nth SELECT column.
C     TABIDX     O   Index of TABLE in FROM clause, if known.
C     COLUMN     O   Nth column in SELECT clause of query.
C     TABIDX     O   Index of column in TABLE, if known.
C
C$ Detailed_Input
C
C     EQRYI          is the integer portion of an encoded EK query.
C                    The query must have been parsed.
C
C     EQRYC          is the character portion of an encoded EK query.
C
C     N              is the index, within the SELECT clause of the
C                    query, of the table whose name is to be fetched.
C
C$ Detailed_Output
C
C     LXBEG,
C     LXEND          are, respectively, the begin and end indices in
C                    the original query of the selected item.
C
C     TABLE          is the table name or alias associated with the Nth
C                    column in the SELECT clause of an the input
C                    encoded query.  If the Nth column is unqualified,
C                    TABLE is returned blank.
C
C     TABIDX         is the ordinal position in the FROM clause of the
C                    input query of the table containing the Nth order-
C                    by column.  TABIDX is meaningful only if name
C                    resolution has not been performed on the input
C                    query; otherwise, TABIDX is returned as zero.
C
C     COLUMN         is the name of the Nth column in the SELECT
C                    clause of an the input encoded query.
C
C     COLIDX         is the ordinal position of the Nth column in the
C                    SELECT clause with respect to the virtual table
C                    designated by TABLE.  This index is available only
C                    if the query has already had names resolved;
C                    otherwise, COLIDX is returned as zero.
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
C     2)  If the input query has not been parsed, the error
C         SPICE(UNPARSEDQUERY) will be signalled.  The outputs
C         will not be modified.
C
C     3)  If the index N is less than 1 or greater than the number of
C         columns in the SELECT clause, the error SPICE(INVALIDINDEX)
C         will be signalled.  The outputs
C         will not be modified.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The call
C
C        CALL ZZEKREQI (  EQRYI,  'NUM_SELECT_COLS',  N  )
C
C     may be used to get the SELECT column count from an encoded
C     query.
C
C$ Examples
C
C     See EKSRCH.
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
      LOGICAL               FAILED
 
C
C     Local variables
C
      INTEGER               BASE
      INTEGER               BUFLEN
      INTEGER               CB
      INTEGER               CE
      INTEGER               IPARSE
      INTEGER               NCNJ
      INTEGER               NCNS
      INTEGER               NORD
      INTEGER               NSEL
      INTEGER               NTAB
      INTEGER               RESOLV
      INTEGER               TB
      INTEGER               TE
 
 
C
C     Use discovery check-in.
C
 
      CALL ZZEKREQI ( EQRYI, 'PARSED', IPARSE )
 
      IF ( FAILED() ) THEN
         RETURN
      END IF
 
      IF ( IPARSE .EQ. IFALSE ) THEN
 
         CALL CHKIN  ( 'ZZEKQSEL'                                )
         CALL SETMSG ( 'Encoded query has not yet been parsed.'  )
         CALL SIGERR ( 'SPICE(UNPARSEDQUERY)'                    )
         CALL CHKOUT ( 'ZZEKQSEL'                                )
         RETURN
 
      END IF
 
 
      CALL ZZEKREQI ( EQRYI,  'NUM_SELECT_COLS',   NSEL   )
 
      IF (  ( N .LT. 1 ) .OR. ( N .GT. NSEL )  ) THEN
 
         CALL CHKIN  ( 'ZZEKQSEL'                                  )
         CALL SETMSG ( 'Column index # is out of valid range 1:#.' )
         CALL ERRINT ( '#',  N                                     )
         CALL ERRINT ( '#',  NSEL                                  )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                       )
         CALL CHKOUT ( 'ZZEKQSEL'                                  )
         RETURN
 
      END IF
 
 
      CALL ZZEKREQI ( EQRYI,  'NUM_TABLES',         NTAB   )
      CALL ZZEKREQI ( EQRYI,  'NUM_CONJUNCTIONS',   NCNJ   )
      CALL ZZEKREQI ( EQRYI,  'NUM_CONSTRAINTS',    NCNS   )
      CALL ZZEKREQI ( EQRYI,  'NUM_ORDERBY_COLS',   NORD   )
      CALL ZZEKREQI ( EQRYI,  'CHR_BUF_SIZE',       BUFLEN )
 
C
C     The lexeme begin and end values start out as invalid values.
C     We'll set these when we discover what form the SELECT item has.
C
 
C
      LXBEG  =  0
      LXEND  =  0
 
C
C     Get the Nth table and column from the query.  The table
C     descriptor lies beyond the fixed-size portion of the query, the
C     conjunction size list, the constraint descriptors, the order-by
C     column descriptors, as well as the (N-1) previous SELECT column
C     descriptors.
C
      BASE   =        EQVBAS
     .            +   NTAB * 2 * EQVDSZ
     .            +   NCNJ
     .            +   NCNS     * EQCDSZ
     .            +   NORD     * EQODSZ
     .            +   (N-1)    * EQSDSZ
 
 
C
C     Pick up the column name first.
C
      CB     =  EQRYI ( BASE + EQVDSZ + EQBSTR )
      CE     =  EQRYI ( BASE + EQVDSZ + EQESTR )
 
      IF (       ( CB .GT. 0      )
     .     .AND. ( CE .GT. 0      )
     .     .AND. ( CB .LE. BUFLEN )
     .     .AND. ( CE .LE. BUFLEN )
     .     .AND. ( CB .LE. CE     )  ) THEN
 
         COLUMN = EQRYC(CB:CE)
 
C
C        The end of the column name is always the end of the SELECT
C        item, at least until we handle more general expressions.
C
         LXEND  =  EQRYI ( BASE + EQVDSZ + EQELEX )
 
      ELSE
C
C        We should never see invalid pointers in a parsed, encoded
C        query, but let's not take chances.
C
         CALL CHKIN  ( 'ZZEKQSEL'                                )
         CALL SETMSG ( 'Invalid string bounds #:# for column #.' )
         CALL ERRINT ( '#',  CB                                  )
         CALL ERRINT ( '#',  CE                                  )
         CALL ERRINT ( '#',  N                                   )
         CALL SIGERR ( 'SPICE(BUG)'                              )
         CALL CHKOUT ( 'ZZEKQSEL'                                )
         RETURN
 
      END IF
 
C
C     Same deal for the qualifying table or alias, except that the begin
C     pointer is set to zero if there's no name.
C
      TB     =  EQRYI ( BASE + EQBSTR )
      TE     =  EQRYI ( BASE + EQESTR )
 
      IF ( TB .GT. 0 ) THEN
 
         IF (       ( TE .GT. 0      )
     .        .AND. ( TB .LE. BUFLEN )
     .        .AND. ( TE .LE. BUFLEN )
     .        .AND. ( TB .LE. TE     )  ) THEN
 
            TABLE  = EQRYC(TB:TE)
 
C
C           The start position of the table name is the start of
C           the SELECT item.
C
            LXBEG  =  EQRYI ( BASE + EQBLEX )
 
 
         ELSE
C
C           If the first pointer is non-zero, both pointers should have
C           been valid.
C
            CALL CHKIN  ( 'ZZEKQSEL'                                   )
            CALL SETMSG ( 'Invalid string bounds #:# for the table '  //
     .                    'qualifying column #.'                       )
            CALL ERRINT ( '#',  TB                                     )
            CALL ERRINT ( '#',  TE                                     )
            CALL ERRINT ( '#',  N                                      )
            CALL SIGERR ( 'SPICE(BUG)'                                 )
            CALL CHKOUT ( 'ZZEKQSEL'                                   )
            RETURN
 
         END IF
 
 
      ELSE
C
C        No table was supplied.
C
         TABLE  =  ' '
 
C
C        The start position of the column name is the start of
C        the SELECT item.
C
         LXBEG  =  EQRYI ( BASE + EQVDSZ + EQBLEX )
 
      END IF
 
C
C     If names have been resolved already, we can determine the index
C     of the table to which the specified order-by column belongs.
C
      CALL ZZEKREQI ( EQRYI,  'NAMES_RESOLVED',   RESOLV )
 
      IF ( RESOLV .EQ. ITRUE ) THEN
C
C        The qualifying table's index in the FROM clause is available.
C        So is the index of the column within the table.
C
         TABIDX  =  EQRYI( BASE + EQTORD              )
         COLIDX  =  EQRYI( BASE + EQSCOL - 1 + EQCIDX )
 
      ELSE
 
         TABIDX  =  0
         COLIDX  =  0
 
      END IF
 
      RETURN
      END
