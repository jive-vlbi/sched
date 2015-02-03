C$Procedure  ZZEKQTAB ( Private: EK, read table names from query )
 
      SUBROUTINE ZZEKQTAB ( EQRYI, EQRYC, N, TABLE, ALIAS )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Read table names and aliases from an encoded EK query.
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
      CHARACTER*(*)         TABLE
      CHARACTER*(*)         ALIAS
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     EQRYI      I   Integer component of query.
C     EQRYC      I   Character component of query.
C     N          I   Index within FROM clause of table name to read.
C     TABLE      O   Name of Nth table in FROM clause.
C     ALIAS      O   Alias of Nth table in FROM clause.
C
C$ Detailed_Input
C
C     EQRYI          is the integer portion of an encoded EK query.
C                    The query must have been parsed.
C
C     EQRYC          is the character portion of an encoded EK query.
C
C     N              is the index, within the FROM clause of the query,
C                    of the table whose name is to be fetched.
C
C$ Detailed_Output
C
C     TABLE          is the name of the Nth table in the FROM clause of
C                    the input encoded query.
C
C     ALIAS          is the alias of TABLE.  If no alias for TABLE is
C                    present, ALIAS is returned blank.
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
C         tables in the FROM clause, the error SPICE(INVALIDINDEX)
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
C        CALL ZZEKREQI (  EQRYI,  'NUM_TABLES',  N  )
C
C     may be used to get the FROM table count from an encoded query.
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
      INTEGER               AB
      INTEGER               AE
      INTEGER               BASE
      INTEGER               BUFLEN
      INTEGER               IPARSE
      INTEGER               NTAB
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
 
         CALL CHKIN  ( 'ZZEKQTAB'                                )
         CALL SETMSG ( 'Encoded query has not yet been parsed.'  )
         CALL SIGERR ( 'SPICE(UNPARSEDQUERY)'                    )
         CALL CHKOUT ( 'ZZEKQTAB'                                )
         RETURN
 
      END IF
 
      CALL ZZEKREQI ( EQRYI, 'CHR_BUF_SIZE', BUFLEN )
      CALL ZZEKREQI ( EQRYI, 'NUM_TABLES',   NTAB   )
 
 
      IF (  ( N .LT. 1 ) .OR. ( N .GT. NTAB )  ) THEN
 
         CALL CHKIN  ( 'ZZEKQTAB'                                  )
         CALL SETMSG ( 'Table index # is out of valid range 1:#.'  )
         CALL ERRINT ( '#',  N                                     )
         CALL ERRINT ( '#',  NTAB                                  )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                       )
         CALL CHKOUT ( 'ZZEKQTAB'                                  )
         RETURN
 
      END IF
 
C
C     Get the Nth table and alias from the query.  The table
C     descriptor lies beyond the fixed-size portion of the query, as
C     well as the (N-1) previous descriptors, each one of which has
C     size 2*EQVDSZ.
C
      BASE   =  EQVBAS + (N-1)*2*EQVDSZ
 
      TB     =  EQRYI ( BASE + EQBSTR )
      TE     =  EQRYI ( BASE + EQESTR )
 
      IF (       ( TB .GT. 0      )
     .     .AND. ( TE .GT. 0      )
     .     .AND. ( TB .LE. BUFLEN )
     .     .AND. ( TE .LE. BUFLEN )
     .     .AND. ( TB .LE. TE     )  ) THEN
 
         TABLE = EQRYC(TB:TE)
 
      ELSE
C
C        We should never see invalid pointers in a parsed, encoded
C        query, but let's not take chances.
C
         CALL CHKIN  ( 'ZZEKQTAB'                                )
         CALL SETMSG ( 'Invalid string bounds #:# for table #.'  )
         CALL ERRINT ( '#',  TB                                  )
         CALL ERRINT ( '#',  TE                                  )
         CALL ERRINT ( '#',  N                                   )
         CALL SIGERR ( 'SPICE(BUG)'                              )
         CALL CHKOUT ( 'ZZEKQTAB'                                )
         RETURN
 
      END IF
 
C
C     Same deal for the alias, except that the begin pointer is
C     set to zero if there's no alias.
C
      AB     =  EQRYI ( BASE + EQVDSZ + EQBSTR )
      AE     =  EQRYI ( BASE + EQVDSZ + EQESTR )
 
      IF ( AB .GT. 0 ) THEN
 
         IF (       ( AE .GT. 0      )
     .        .AND. ( AB .LE. BUFLEN )
     .        .AND. ( AE .LE. BUFLEN )
     .        .AND. ( AB .LE. AE     )  ) THEN
 
            ALIAS  = EQRYC(AB:AE)
 
         ELSE
C
C           If the first pointer is non-zero, both pointers should have
C           been valid.
C
            CALL CHKIN  ( 'ZZEKQTAB'                                   )
            CALL SETMSG ( 'Invalid string bounds #:# for the alias '  //
     .                    'of table #.'                                )
            CALL ERRINT ( '#',  AB                                     )
            CALL ERRINT ( '#',  AE                                     )
            CALL ERRINT ( '#',  N                                      )
            CALL SIGERR ( 'SPICE(BUG)'                                 )
            CALL CHKOUT ( 'ZZEKQTAB'                                   )
            RETURN
 
         END IF
 
      ELSE
C
C        No alias was supplied.
C
         ALIAS  =  ' '
 
      END IF
 
      RETURN
      END
