C$Procedure  ZZEKQCNJ ( Private: EK, read conjunction sizes from query )
 
      SUBROUTINE ZZEKQCNJ ( EQRYI, N, SIZE )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Read conjunction sizes from an encoded EK query.
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
      INTEGER               N
      INTEGER               SIZE
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     EQRYI      I   Integer component of query.
C     N          I   Index within FROM clause of table name to read.
C     SIZE       O   Size of Nth conjunction in WHERE clause.
C
C$ Detailed_Input
C
C     EQRYI          is the integer portion of an encoded EK query.
C                    The query must have been parsed.
C
C     N              is the index, within the FROM clause of the query,
C                    of the table whose name is to be fetched.
C
C$ Detailed_Output
C
C     SIZE           is the size of the Nth conjunction of
C                    constraints in the input encoded query.  The size
C                    applies to the constraints after `normalization'.
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
C         conjunctions in the query, the error SPICE(INVALIDINDEX)
C         will be signalled.  The outputs will not be modified.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The call
C
C        CALL ZZEKREQI (  EQRYI,  'NUM_CONJUNCTIONS',  N  )
C
C     may be used to get the conjunction count from an encoded query.
C
C     This routine assumes that encoded EK query architecture version
C     1 is to be used with the query to be initialized; this routine
C     will not work with any other architecture version.
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
      INTEGER               IPARSE
      INTEGER               LOC
      INTEGER               NCNJ
      INTEGER               NCNS
      INTEGER               NTAB
 
C
C     Use discovery check-in.
C
 
      CALL ZZEKREQI ( EQRYI, 'PARSED', IPARSE )
 
      IF ( FAILED() ) THEN
         RETURN
      END IF
 
      IF ( IPARSE .EQ. IFALSE ) THEN
 
         CALL CHKIN  ( 'ZZEKQCNJ'                                )
         CALL SETMSG ( 'Encoded query has not yet been parsed.'  )
         CALL SIGERR ( 'SPICE(UNPARSEDQUERY)'                    )
         CALL CHKOUT ( 'ZZEKQCNJ'                                )
         RETURN
 
      END IF
 
      CALL ZZEKREQI ( EQRYI, 'NUM_TABLES',         NTAB   )
      CALL ZZEKREQI ( EQRYI, 'NUM_CONJUNCTIONS',   NCNJ   )
      CALL ZZEKREQI ( EQRYI, 'NUM_CONSTRAINTS',    NCNS   )
 
 
      IF (  ( N .LT. 1 ) .OR. ( N .GT. NCNJ )  ) THEN
 
         CALL CHKIN  ( 'ZZEKQCNJ'                                  )
         CALL SETMSG ( 'Table index # is out of valid range 1:#.'  )
         CALL ERRINT ( '#',  N                                     )
         CALL ERRINT ( '#',  NCNJ                                  )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                       )
         CALL CHKOUT ( 'ZZEKQCNJ'                                  )
         RETURN
 
      END IF
 
C
C     Compute the location of the requested conjunction size value.
C
      LOC    =     EQVBAS
     .           + NTAB  *  2  *  EQVDSZ
     .           + NCNS        *  EQCDSZ
     .           + N
 
      SIZE   =  EQRYI ( LOC )
 
 
      RETURN
      END
