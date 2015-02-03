C$Procedure      ZZEKVMCH ( EK, vector match )
 
      LOGICAL FUNCTION ZZEKVMCH ( NCNSTR, ACTIVE,
     .                            LHANS,  LSDSCS, LCDSCS, LROWS, LELTS,
     .                            OPS,
     .                            RHANS,  RSDSCS, RCDSCS, RROWS, RELTS )
 
C$ Abstract
C
C     Determine whether a vector of constraints involving comparisons of
C     specified EK column elements is satisfied.
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
      INCLUDE 'ekopcd.inc'
      INCLUDE 'ektype.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ekwild.inc'
      INCLUDE 'ekqlimit.inc'
 
 
      INTEGER               NCNSTR
      LOGICAL               ACTIVE ( * )
      INTEGER               LHANS  ( * )
      INTEGER               LSDSCS ( SDSCSZ, * )
      INTEGER               LCDSCS ( CDSCSZ, * )
      INTEGER               LROWS  ( * )
      INTEGER               LELTS  ( * )
      INTEGER               OPS    ( * )
      INTEGER               RHANS  ( * )
      INTEGER               RSDSCS ( SDSCSZ, * )
      INTEGER               RCDSCS ( CDSCSZ, * )
      INTEGER               RROWS  ( * )
      INTEGER               RELTS  ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NCNSTR     I   Number of join constraints.
C     ACTIVE     I   Array of flags indicating applicable constraints.
C     LHANS      I   Handles of EKs for columns on LHS's of constraints.
C     LSDSCS     I   Descriptors of segments on LHS's of constraints.
C     LCDSCS     I   Column descriptors for LHS's of constraints.
C     LROWS      I   Row numbers for LHS's of constraints.
C     LCOLS      I   Column names for LHS's of constraints.
C     LELTS      I   Column element indices for LHS's of constraints.
C     OPS        I   Code for relational operator in constraints.
C     RHAN       I   Handles of EKs for columns on RHS's of constraints.
C     RSDSCS     I   Descriptors of segments on RHS's of constraints.
C     RCDSCS     I   Column descriptors for RHS's of constraints.
C     RROWS      I   Row numbers for RHS's of constraints.
C     RCOLS      I   Column names for RHS's of constraints.
C     RELTS      I   Column element indices for RHS's of constraints.
C
C     The function returns .TRUE. if and only if all of the relational
C     constraints specified by the input arguments are satisfied.
C
C$ Detailed_Input
C
C     NCNSTR         is the number of input join constraints.   Each
C                    input constraint relates two EK column entries;
C                    abstractly, the form of the constraints is:
C
C                       <col entry 1> <relational op> <col entry 2>
C
C                    The compared entries are defined by handles,
C                    segment base addresses, column descriptors, and row
C                    numbers.
C
C     ACTIVE         is an array of logical flags indicating which
C                    constraints are currently applicable.  The Nth
C                    element of ACTIVE indicates whether or not to apply
C                    the Nth constraint:  if ACTIVE(N) is .TRUE., the
C                    constraint is applicable, otherwise it isn't.
C
C                    The elements of the other input arguments that
C                    define constraints are defined when the
C                    corresponding element of ACTIVE is .TRUE.  For
C                    example, when the second constraint is not active,
C                    the second column descriptor in LDSCRS may not be
C                    defined.
C
C     LHANS          is an array of EK file handles for the left-hand-
C                    sides of the constraints.
C
C     LSDSCS         is an array of segment descriptors for the
C                    left-hand-sides of the constraints.
C
C     LDSCRS         is an array of column descriptors for the
C                    left-hand-sides of the constraints.
C
C     LROWS          is an array of row numbers for the left-hand-sides
C                    of the constraints.
C
C     LELTS          is an array of column entry element indices for the
C                    left-hand-sides of the constraints.  These
C                    indices are ignored unless the columns they apply
C                    to are array-valued.
C
C     OPS            is an array of relational operators used in the
C                    input constraints.  The elements of OPS are any of
C                    the integer parameters
C
C                       EQ, GE, GT, LE, LT, NE, LIKE, ISNULL, NOTNUL
C
C                    The Ith element of OPS corresponds to the Ith
C                    constraint.
C
C     RHANS          is an array of EK file handles for the right-hand-
C                    sides of the constraints.
C
C     RSDSCS         is an array of segment descriptors for the
C                    right-hand-sides of the constraints.
C
C     RDSCRS         is an array of column descriptors for the
C                    right-hand-sides of the constraints.
C
C     RROWS          is an array of row numbers for the right-hand-sides
C                    of the constraints.
C
C     RELTS          is an array of column entry element indices for the
C                    right-hand-sides of the constraints.  These
C                    indices are ignored unless the columns they apply
C                    to are array-valued.
C
C
C$ Detailed_Output
C
C     The function returns .TRUE. if and only if all of the relational
C     constraints specified by the input arguments are satisfied.
C
C$ Parameters
C
C     Within the EK system, operators used in EK queries are
C     represented by integer codes.  The codes and their meanings are
C     listed below.
C
C     Relational expressions in EK queries have the form
C
C        <column name> <operator> <value>
C
C     For columns containing numeric values, the operators
C
C        EQ,  GE,  GT,  LE,  LT,  NE
C
C     may be used; these operators have the same meanings as their
C     Fortran counterparts.  For columns containing character values,
C     the list of allowed operators includes those in the above list,
C     and in addition includes the operator
C
C        LIKE
C
C     which is used to compare strings to a template.  In the character
C     case, the meanings of the parameters
C
C        GE,  GT,  LE,  LT
C
C     match those of the Fortran lexical functions
C
C        LGE, LGT, LLE, LLT
C
C
C     The additional unary operators
C
C        ISNULL, NOTNUL
C
C     are used to test whether a value of any type is null.
C
C
C$ Exceptions
C
C     1)  If any of the input file handles is invalid, the error
C         will be diagnosed by routines called by this routine.
C         The function value is .FALSE. in this case.
C
C     2)  If an I/O error occurs while attempting to find the address
C         range of a column entry element, the error will
C         be diagnosed by routines called by this routine.  The
C         function value is .FALSE. in this case.
C
C     3)  If any of the input segment descriptors, column descriptors,
C         or row numbers are invalid, this routine may fail in
C         unpredictable, but possibly spectacular, ways.  Except
C         as described in this header section, no attempt is made to
C         handle these errors.
C
C     4)  If the data type code in an input column descriptor is not
C         recognized, the error SPICE(INVALIDDATATYPE) is signalled.
C         The function value is .FALSE. in this case.
C
C     5)  If a relational operator code is not recognized, the
C         error SPICE(UNNATURALRELATION) is signalled.
C         The function value is .FALSE. in this case.
C
C$ Files
C
C     See the descriptions of the arguments LHAN and RHAN in
C     $Detailed_Input.
C
C$ Particulars
C
C     This routine is an EK utility intended to centralize a frequently
C     performed comparison operation.
C
C$ Examples
C
C     See EKSRCH.
C
C$ Restrictions
C
C     1)  This routine must execute quickly.  Therefore, it checks in
C         only if it detects an error.  If an error is signalled by a
C         routine called by this routine, this routine will not appear
C         in the SPICELIB traceback display.  Also, in the interest
C         of speed, this routine does not test the value of the SPICELIB
C         function RETURN upon entry.
C
C     2)  This routine depends on the requested comparison to have
C         been semantically checked. Semantically invalid comparisons
C         are treated as bugs.
C
C     3)  Only the first MAXSTR characters of character strings are
C         used in comparisons.
CC
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
C-    SPICELIB Version 1.1.0, 01-JUN-2010 (NJB)
C
C        Bug fix: subscript out of range error caused by
C        column entry strings longer than MAXLEN has been
C        corrected. Also updated Restrictions header section.
C
C-    Beta Version 1.0.0, 11-OCT-1995 (NJB)
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               MATCHI
 
C
C     Non-SPICELIB functions
C
      INTEGER               ZZEKECMP
 
C
C     Local variables
C
      CHARACTER*(MAXSTR)    CVAL   ( 2 )
 
      INTEGER               CLDSCS ( CDSCSZ, 2 )
      INTEGER               CMPLEN ( 2 )
      INTEGER               CVLEN  ( 2 )
      INTEGER               ELTS   ( 2 )
      INTEGER               HANS   ( 2 )
      INTEGER               I
      INTEGER               N
      INTEGER               REL
      INTEGER               ROWS   ( 2 )
      INTEGER               SGDSCS ( SDSCSZ, 2 )
      INTEGER               UNIT
 
      LOGICAL               FOUND
      LOGICAL               NULL   ( 2 )
 
C
C     Use discovery check-in for speed.  Don't check RETURN.
C
C     The function value defaults to .TRUE.  As we test the constraints,
C     we may find one that the input row vector doesn't satisfy, at
C     which point we can terminate the comparison.
C
      ZZEKVMCH  =  .TRUE.
      N         =   1
 
      DO WHILE  (  ( N .LE. NCNSTR ) .AND. ( ZZEKVMCH )  )
 
 
         IF ( ACTIVE(N) ) THEN
C
C           Apply the Nth join constraint to the input row vector.
C
C           Compare the entries in the two rows in the columns indicated
C           by the Nth column descriptor pair.  To do this, find the
C           address ranges for each column entry.  We don't check the
C           found flag because every column entry has at least one
C           element.
C
C
C           We'll start out setting REL to EQ.  If we find out
C           otherwise, we'll change it.
C
            HANS(1)  =  LHANS(N)
            HANS(2)  =  RHANS(N)
 
            CALL MOVEI ( LSDSCS(1,N), SDSCSZ, SGDSCS(1,1) )
            CALL MOVEI ( RSDSCS(1,N), SDSCSZ, SGDSCS(1,2) )
 
            ROWS(1)  =  LROWS(N)
            ROWS(2)  =  RROWS(N)
 
            ELTS(1)  =  LELTS(N)
            ELTS(2)  =  RELTS(N)
 
            CALL MOVEI ( LCDSCS(1,N), CDSCSZ, CLDSCS(1,1) )
            CALL MOVEI ( RCDSCS(1,N), CDSCSZ, CLDSCS(1,2) )
 
 
            REL  =  ZZEKECMP ( HANS,  SGDSCS,  CLDSCS,  ROWS,  ELTS  )
 
C
C           Determine the truth of the Nth input relational expression,
C           and set ZZEKVMCH accordingly.
C
            IF ( OPS(N) .EQ. EQ )   THEN
 
               ZZEKVMCH  =  REL .EQ. EQ
 
 
            ELSE IF ( OPS(N) .EQ. LT )   THEN
 
               ZZEKVMCH  =   REL .EQ. LT
 
 
            ELSE IF ( OPS(N) .EQ. LE )   THEN
 
               ZZEKVMCH  =   REL .NE. GT
 
 
            ELSE IF ( OPS(N) .EQ. GT )   THEN
 
               ZZEKVMCH  =   REL .EQ. GT
 
 
            ELSE IF ( OPS(N) .EQ. GE )   THEN
 
               ZZEKVMCH  =   REL .NE. LT
 
 
            ELSE IF ( OPS(N) .EQ. NE )   THEN
 
               ZZEKVMCH  =   REL .NE. EQ
 
 
            ELSE IF (       ( OPS(N)            .EQ. LIKE )
     .                .AND. ( CLDSCS(TYPIDX,1)  .EQ. CHR  )  ) THEN
 
 
               DO I = 1, 2
 
                  CALL ZZEKRSC (  HANS(I),
     .                            SGDSCS(1,I),
     .                            CLDSCS(1,I),
     .                            ROWS(I),
     .                            ELTS(I),
     .                            CVLEN(I),
     .                            CVAL(I),
     .                            NULL(I),
     .                            FOUND           )
 
 
                  IF ( .NOT. FOUND ) THEN
 
                     CALL DASHLU ( HANS(I), UNIT )
 
                     CALL CHKIN  ( 'ZZEKVMCH'                       )
                     CALL SETMSG ( 'EK = #; COLIDX = #; ROW = #; ' //
     .                             'ELTIDX  = #.  Column entry  '  //
     .                             'element was not found.' )
                     CALL ERRFNM ( '#',  UNIT                       )
                     CALL ERRINT ( '#',  CLDSCS(ORDIDX,I)           )
                     CALL ERRINT ( '#',  ROWS(I)                    )
                     CALL ERRINT ( '#',  ELTS(I)                    )
                     CALL SIGERR ( 'SPICE(INVALIDINDEX)'            )
                     CALL CHKOUT ( 'ZZEKVMCH'                       )
                     RETURN
 
                  END IF

                  IF (  FOUND  .AND. ( .NOT. NULL(I) )  ) THEN

                     CMPLEN(I) = MIN ( CVLEN(I), MAXSTR )
                  ELSE
                     CMPLEN(I) = 0
                  END IF
 
               END DO
 
 
               ZZEKVMCH  =  MATCHI ( CVAL(1)( :CMPLEN(1) ),
     .                               CVAL(2)( :CMPLEN(2) ),
     .                               WSTR,
     .                               WCHR                  )
 
 
 
            ELSE IF (       ( OPS(N)            .EQ. UNLIKE )
     .                .AND. ( CLDSCS(TYPIDX,1)  .EQ. CHR    )  ) THEN
 
               DO I = 1, 2
 
                  CALL ZZEKRSC (  HANS(I),
     .                            SGDSCS(1,I),
     .                            CLDSCS(1,I),
     .                            ROWS(I),
     .                            ELTS(I),
     .                            CVLEN(I),
     .                            CVAL(I),
     .                            NULL(I),
     .                            FOUND      )
 
 
                  IF ( .NOT. FOUND ) THEN
 
                     CALL DASHLU ( HANS(I), UNIT )
 
                     CALL CHKIN  ( 'ZZEKVMCH'                       )
                     CALL SETMSG ( 'EK = #; COLIDX = #; ROW = #; ' //
     .                             'ELTIDX  = #.  Column entry  '  //
     .                             'element was not found.' )
                     CALL ERRFNM ( '#',  UNIT                       )
                     CALL ERRINT ( '#',  CLDSCS(ORDIDX,I)           )
                     CALL ERRINT ( '#',  ROWS(I)                    )
                     CALL ERRINT ( '#',  ELTS(I)                    )
                     CALL SIGERR ( 'SPICE(INVALIDINDEX)'            )
                     CALL CHKOUT ( 'ZZEKVMCH'                       )
                     RETURN
 
                  END IF
 

                  IF (  FOUND  .AND. ( .NOT. NULL(I) )  ) THEN

                     CMPLEN(I) = MIN ( CVLEN(I), MAXSTR )
                  ELSE
                     CMPLEN(I) = 0
                  END IF

               END DO
 
 
               ZZEKVMCH  =  .NOT. MATCHI ( CVAL(1)( :CMPLEN(1) ),
     .                                     CVAL(2)( :CMPLEN(2) ),
     .                                     WSTR,
     .                                     WCHR                 )
 
 
            ELSE
C
C              Sorry, we couldn't resist.
C
               ZZEKVMCH  =  .FALSE.
 
               CALL CHKIN  ( 'ZZEKVMCH'                            )
               CALL SETMSG ( 'The relational operator # was not ' //
     .                       'recognized.'                         )
               CALL ERRINT ( '#',  OPS(N)                          )
               CALL SIGERR ( 'SPICE(UNNATURALRELATION)'            )
               CALL CHKOUT ( 'ZZEKVMCH'                            )
               RETURN
 
            END IF
 
 
         END IF
 
C
C        We've completed the test for the Nth constraint, if that
C        constraint was active.
C
         N = N + 1
 
      END DO
 
 
      RETURN
      END
