C$Procedure  ZZEKJTST  ( Test join candidates )
 
      SUBROUTINE ZZEKJTST ( SEGVEC,  JBASE1,  NT1,     RB1,     NR1,
     .                      JBASE2,  NT2,     RB2,     NR2,     NJCNST,
     .                      ACTIVE,  CPIDX1,  CLIDX1,  ELTS1,   OPS,
     .                      CPIDX2,  CLIDX2,  ELTS2,   STHAN,   STSDSC,
     .                      STDTPT,  DTPOOL,  DTDSCS,  FOUND,   ROWVEC )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Test a set of candidate row vectors, all corresponding to the same
C     segment vector, against join constraints.
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
 
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekjrs.inc'
      INCLUDE 'ekopcd.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'ekquery.inc'
      INCLUDE 'eksegdsc.inc'
 
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )
 
      INTEGER               SEGVEC ( * )
      INTEGER               JBASE1
      INTEGER               NT1
      INTEGER               RB1
      INTEGER               NR1
      INTEGER               JBASE2
      INTEGER               NT2
      INTEGER               RB2
      INTEGER               NR2
      INTEGER               NJCNST
      LOGICAL               ACTIVE ( * )
      INTEGER               CPIDX1 ( * )
      INTEGER               CLIDX1 ( * )
      INTEGER               ELTS1  ( * )
      INTEGER               OPS    ( * )
      INTEGER               CPIDX2 ( * )
      INTEGER               CLIDX2 ( * )
      INTEGER               ELTS2  ( * )
      INTEGER               STHAN  ( * )
      INTEGER               STSDSC ( SDSCSZ,          * )
      INTEGER               STDTPT ( * )
      INTEGER               DTPOOL ( 2,      LBPOOL : * )
      INTEGER               DTDSCS ( CDSCSZ,          * )
      LOGICAL               FOUND
      INTEGER               ROWVEC ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Entry points
C     --------  ---  --------------------------------------------------
C     SEGVEC     I   ZZEKJPRP
C     JBASE1     I   ZZEKJPRP
C     NT1        I   ZZEKJPRP
C     RB1        I   ZZEKJPRP
C     NR1        I   ZZEKJPRP
C     JBASE2     I   ZZEKJPRP
C     NT2        I   ZZEKJPRP
C     RB2        I   ZZEKJPRP
C     NR2        I   ZZEKJPRP
C     NJCNST     I   ZZEKJPRP
C     ACTIVE     I   ZZEKJPRP
C     CPIDX1     I   ZZEKJPRP
C     CLIDX1     I   ZZEKJPRP
C     ELTS1      I   ZZEKJPRP
C     OPS        I   ZZEKJPRP
C     CPIDX2     I   ZZEKJPRP
C     CLIDX2     I   ZZEKJPRP
C     ELTS2      I   ZZEKJPRP
C     STHAN      I   ZZEKJPRP
C     STSDSC     I   ZZEKJPRP
C     STDTPT     I   ZZEKJPRP
C     DTPOOL     I   ZZEKJPRP
C     DTDSCS     I   ZZEKJPRP
C     FOUND      O   ZZEKJNXT
C     ROWVEC     O   ZZEKJNXT
C
C$ Detailed_Input
C
C     See the entry points for a discussion of their inputs.
C
C$ Detailed_Output
C
C     See the entry points for a discussion of their inputs.
C
C$ Parameters
C
C     See the include files.
C
C$ Exceptions
C
C     1)  If this routine is called directly, the error
C         SPICE(BOGUSENTRY) is signalled.
C
C     See the entry points for discussions of exceptions pertaining to
C     those routines.
C
C$ Files
C
C     1)  This routine uses the EK scratch area, which employs a scratch
C         DAS file.
C
C$ Particulars
C
C     This suite of routines enables the EK system to execute table
C     joins with reasonable efficiency.  These routines make use of
C     join constraints to limit the number of joined row vectors that
C     must be considered in computing a join.
C
C     These routines deal with a limited case of the join problem:
C     the inputs define, for both join row sets participating in the
C     join, row vectors that are qualified by a single segment vector.
C     Thus this routine is meant to be called once for every pair of
C     segment vectors to be considered in executing the join.
C
C     The layout of a join row set in the EK scratch area is shown
C     in the include file for the join row set parameters.
C
C$ Examples
C
C     To use these routines, the normal sequence of actions is to
C     call ZZEKJPRP once to initialize them, and then to call
C     ZZEKJNXT in a loop to retrieve the row vectors satisfying
C     the join constraints.  See ZZEKJOIN for an example application.
C
C$ Restrictions
C
C     1)  This routine should not be called by routines outside of the
C         EK system.
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
C-    SPICELIB Version 2.0.0, 20-JUL-1998 (NJB)
C
C        Modified entry point ZZEKJPRP to set CASE to EMPTY when either 
C        input row count is zero.  Modified entry point ZZEKJNXT to
C        set FOUND to .FALSE. on the first pass when CASE is EMPTY.
C
C-    Beta Version 1.0.0, 11-OCT-1995 (NJB)
C
C-&
 
 
C
C     SPICELIB functions
C
      INTEGER               LNKNXT
      LOGICAL               RETURN
      
C
C     Other functions
C
      LOGICAL               ZZEKRCMP
      LOGICAL               ZZEKVMCH
 
C
C     Local parameters
C
      INTEGER               EQUI
      PARAMETER           ( EQUI   = 1 )
 
      INTEGER               NONEQ
      PARAMETER           ( NONEQ  = 2 )
 
      INTEGER               NOLUCK
      PARAMETER           ( NOLUCK = 3 )
 
      INTEGER               EMPTY
      PARAMETER           ( EMPTY  = 4 )
      
C
C     Local variables
C
      INTEGER               ADDRSS
      INTEGER               BASE
      INTEGER               CASE
      INTEGER               CNSTR
      INTEGER               DTPTR
      INTEGER               I
      INTEGER               J
      INTEGER               JBASE
      INTEGER               K
      INTEGER               LBASE
      INTEGER               LCOL
      INTEGER               LCUR
      INTEGER               LDSCRS ( CDSCSZ, MXJCON )
      INTEGER               LELT
      INTEGER               LELTS  (         MXJCON )
      INTEGER               LHANS  (         MXJCON )
      LOGICAL               LOCACT (         MXJCON )
      INTEGER               LOVBAS
      INTEGER               LPTR
      INTEGER               LROW
      INTEGER               LROWS  (         MXJCON )
      INTEGER               LRVIDX
      INTEGER               LSDSC  ( SDSCSZ, MXJCON )
      INTEGER               LSEG
      INTEGER               LTAB
      INTEGER               MINIRV ( 2 )
      INTEGER               OFFSET
      INTEGER               NR
      INTEGER               NT
      INTEGER               NT3
      INTEGER               RB
      INTEGER               RBASE
      INTEGER               RCOL
      INTEGER               RDSCRS ( CDSCSZ, MXJCON )
      INTEGER               RELT
      INTEGER               RELTS  (         MXJCON )
      INTEGER               RHANS  (         MXJCON )
      INTEGER               ROVBAS
      INTEGER               RPTR
      INTEGER               RROW
      INTEGER               RROWS  (         MXJCON )
      INTEGER               RRVIDX
      INTEGER               RSDSC  ( SDSCSZ, MXJCON )
      INTEGER               RSEG
      INTEGER               RTAB
      INTEGER               SVBAS1
      INTEGER               SVBAS2
      INTEGER               SVCP1  ( MXJCON )
      INTEGER               SVCP2  ( MXJCON )
      INTEGER               SVOPS  ( MXJCON )
      INTEGER               SVNCON
      INTEGER               SVNR1
      INTEGER               SVNR2
      INTEGER               SVNT1
      INTEGER               SVNT2
      INTEGER               SVRB1
      INTEGER               SVRB2
      INTEGER               TAB
      INTEGER               TOP
 
      LOGICAL               DONE
      LOGICAL               FND
      LOGICAL               LSMALL
 
C
C     Saved variables
C
      SAVE
 
 
      CALL CHKIN  ( 'ZZEKJTST'          )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'ZZEKJTST'          )
 
      RETURN
 
 
 
 
 
 
 
 
C$Procedure  ZZEKJPRP  ( Prepare join condition test )
 
      ENTRY ZZEKJPRP ( SEGVEC,  JBASE1,  NT1,     RB1,     NR1,
     .                 JBASE2,  NT2,     RB2,     NR2,     NJCNST,
     .                 ACTIVE,  CPIDX1,  CLIDX1,  ELTS1,   OPS,
     .                 CPIDX2,  CLIDX2,  ELTS2,   STHAN,   STSDSC,
     .                 STDTPT,  DTPOOL,  DTDSCS                    )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Prepare to test a set of candidate row vectors, all corresponding
C     to the same segment vector, against join constraints.
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
C     UTILITY
C
C$ Declarations
C
C     INTEGER               LBPOOL
C     PARAMETER           ( LBPOOL = -5 )
C
C     INTEGER               SEGVEC ( * )
C     INTEGER               JBASE1
C     INTEGER               NT1
C     INTEGER               RB1
C     INTEGER               NR1
C     INTEGER               JBASE2
C     INTEGER               NT2
C     INTEGER               RB2
C     INTEGER               NR2
C     INTEGER               NJCNST
C     LOGICAL               ACTIVE ( * )
C     INTEGER               CPIDX1 ( * )
C     INTEGER               CLIDX1 ( * )
C     INTEGER               OPS    ( * )
C     INTEGER               CPIDX2 ( * )
C     INTEGER               CLIDX2 ( * )
C     INTEGER               STHAN  ( * )
C     INTEGER               STSDSC ( 3, * )
C     INTEGER               STDTPT ( * )
C     INTEGER               DTPOOL ( 2,      LBPOOL : * )
C     INTEGER               DTDSCS ( CDSCSZ,          * )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     SEGVEC     I   Composite segment vector for joined table.
C     JBASE1     I   Scratch area base address for first join row set.
C     NT1        I   Width of first table.
C     RB1        I   Row vector base address from first join row set.
C     NR1        I   Number of row vectors from first join row set.
C     JBASE2     I   Scratch area base address for second join row set.
C     NT2        I   Width of second table.
C     RB2        I   Row vector base address from second join row set.
C     NR2        I   Number of row vectors from second join row set.
C     JBASE1     I   Scratch area base address of first join row set.
C     JBASE2     I   Scratch area base address of second join row set.
C     NJCNST     I   Number of join constraints.
C     ACTIVE     I   Array of flags indicating applicable constraints.
C     CPIDX1     I   Cross product indices for LHS's of constraints.
C     CLIDX1     I   Column indices for LHS's of constraints.
C     OPS        I   Operator codes for constraints.
C     CPIDX2     I   Cross product indices for RHS's of constraints.
C     CLIDX2     I   Column indices for RHS's of constraints.
C     STHAN      I   Array of EK handles corresponding to segments.
C     STSDSC     I   Array of segment descriptors.
C     STDTPT     I   Array of set table column descriptor pointers.
C     DTPOOL     I   Linked list pool for column descriptors.
C     DTDSCS     I   Array of column descriptors.
C
C$ Detailed_Input
C
C     SEGVEC         is a composite segment vector for the output row
C                    vectors resulting from the join done by these
C                    routines.  SEGVEC has been created by suffixing
C                    a segment vector from the second input join row
C                    set onto a segment vector from the first join row
C                    set.
C
C     JBASE1         is the EK scratch area base address of the first
C                    input join row set.  This address is one less than
C                    the first address occupied by the join row set.
C                    See the $Particulars section for a description of
C                    join row sets.
C
C     NT1            is the number of tables in the first join row set.
C
C     RB1            is the scratch area base address of the considered
C                    row vectors from the first join row set.  This
C                    address is base-relative:  JBASE1+RB1 is the actual
C                    base address of the row vectors.
C
C     NR1            is the number of rows in the considered portion of
C                    the first join row set.  The portion in question
C                    is the set of row vectors corresponding to a
C                    single segment vector, namely, the one occupying
C                    the first NT1 elements of SEGVEC.
C
C     JBASE2,
C     NT2,
C     RB2,
C     NR2            are analogous quantities to JBASE1, NT2, RB1, and
C                    NR1; the quantities here apply to the second input
C                    join row set.  The segment vector qualifying the
C                    input row vectors from the second join row set
C                    occupies elements NT1+1 through NT1+NT2 of SEGVEC.
C
C
C     NJCNST         is the number of join constraints that must be
C                    satisfied by the output join row set.  Each of the
C                    input arrays CPIDX1, CLIDX1, OPS, CPIDX2, and
C                    CLIDX2 contains NJCNST elements.
C
C     ACTIVE         is an array of logical flags indicating which
C                    constraints are currently applicable.  The Nth
C                    element of ACTIVE indicates whether or not to apply
C                    the Nth constraint:  if ACTIVE(N) is .TRUE., the
C                    constraint is applicable, otherwise it isn't.
C
C                    In order for a join constraint to be active, it
C                    must relate a column in the first join row set
C                    to a column in the second join row set.  The LHS
C                    and RHS of the constraint need not refer
C                    to the first and second join row sets respectively.
C
C                    The elements of the other input arguments that
C                    define constraints are defined when the
C                    corresponding element of ACTIVE is .TRUE.  For
C                    example, when the second constraint is not active,
C                    the second column descriptor in DTDSCS may not be
C                    defined.
C
C     CPIDX1,
C     CLIDX1         are, respectively, a set of cross product indices
C                    and column indices that define the columns on the
C                    left-hand sides of the input constraints.  If the
C                    first input join row set contains rows from NT1
C                    tables and the second input join row set contains
C                    rows from NT2 tables, then there are (NT1+NT2)
C                    components in the cross product of the tables
C                    specified by the input join row sets.  We'll index
C                    these from 1 to (NT1+NT2), with table 1 being the
C                    first table of the first input join row set, table
C                    2 being the second table of the first input join
C                    row set, table (NT1+1) being the first table of the
C                    second input join row set, and so on.  Each element
C                    of the argument CPIDX1 designates a table by this
C                    counting scheme.  The corresponding element of the
C                    argument CLIDX1 is the index of a column in the
C                    specified table.  The index is the ordinal position
C                    of the column's attributes in the column attribute
C                    list for the table containing the column.
C
C
C     ELTS1          is an array of element indices that apply to the
C                    columns on the left-hand-sides of constraints.  The
C                    Ith element of ELTS1 is the column entry index
C                    that applies to the Ith constraint.
C
C     OPS            is an array of relational operator codes.  The
C                    Ith code applies to the Ith join constraint.
C
C     CPIDX2,
C     CLIDX2         are, respectively, a set of cross product indices
C                    and column indices that define the columns on the
C                    right-hand sides of the input constraints.  The
C                    meanings of these arrays are analogous to those
C                    of CPIDX1 and CLIDX1.  Note that the indices are
C                    relative to the combined table of width NT1+NT2,
C                    *not* to the second table.
C
C     ELTS2          is an array of element indices that apply to the
C                    columns on the right-hand-sides of constraints.
C                    The Ith element of ELTS2 is the column entry index
C                    that applies to the Ith constraint.
C
C     STHAN          is an array of EK file handles.  The Ith element
C                    of STHAN is the handle of the EK containing the
C                    Ith loaded segment.
C
C     STSDSC         is an array of segment descriptors for all
C                    loaded segments.
C
C     STDTPT         is an array of descriptor table pointers all of
C                    the loaded segments.  For the Ith loaded segment,
C
C                       STDTPT(I)
C
C                    contains the node number of the descriptor entry
C                    of the first column in the Ith segment, where the
C                    order of columns is determined by the order in
C                    which the columns appear in the parent table's
C                    column attribute list.
C
C     DTPOOL,
C     DTDSCS         are, respectively, the linked list pool for
C                    the column descriptor array and the column
C                    descriptor array itself.  The latter contains
C                    a descriptor for each loaded column.
C
C$ Detailed_Output
C
C     None.  This routine operates entirely by side effects.
C
C$ Parameters
C
C     See the include files.
C
C$ Exceptions
C
C     1) This routine
C
C     All other error checking must be performed by the caller of this
C     routine.  Presently, that caller is ZZEKJOIN.
C
C$ Files
C
C     1)  This routine uses the EK scratch area, which employs a scratch
C         DAS file.
C
C$ Particulars
C
C     This routine prepares ZZEKJNXT to return row vectors satisfying
C     a specified set of join constraints.  The principal job of this
C     routine is to determine key columns to guide the order in which
C     candidate row vectors are tested.  When key columns are
C     available, this routine produces order vectors for those columns.
C
C     This routine writes to the EK scratch area.  The caller of this
C     routine must take this fact into account, because this routine
C     will normally be called during the construction of a join row set,
C     and scratch area addresses claimed by this routine will be
C     interspersed with those owned by the caller.
C
C     The territory occupied by this routine may be reclaimed later by
C     `squeezing' unused addresses out of the final join row set.  This
C     operation can be performed by ZZEKJSQZ.
C
C$ Examples
C
C     See ZZEKJOIN.
C
C$ Restrictions
C
C     1)  This routine should not be called by routines outside of the
C         EK system.
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
C-    SPICELIB Version 2.0.0, 20-JUL-1998 (NJB)
C
C        Modified entry point to set CASE to EMPTY when either 
C        input row count is zero.  
C
C-    Beta Version 1.0.0, 11-OCT-1995 (NJB)
C
C-&
 
      IF ( RETURN() ) THEN
         RETURN
      END IF
      
      CALL CHKIN ( 'ZZEKJPRP' )
      
C
C     We don't validate the inputs; these must be checked by ZZEKJOIN, 
C     the only routine that should call this one.
C
C     Not much preparation is required if either input row count is
C     zero, since the cartesian product will be zero.
C
      IF (  ( NR1 .EQ. 0 )  .OR.  ( NR2 .EQ. 0 )  ) THEN
         
         CASE = EMPTY
         
         CALL CHKOUT ( 'ZZEKJPRP' )
         RETURN
         
      END IF

C
C     Set the table count and segment vector count for the output join
C     row set.
C
      NT3  =  NT1  +  NT2
 
 
C
C     Create handle, segment base, and column descriptor
C     arrays for both sides of each active relational constraint.
C
      DO J = 1, NJCNST
 
         IF ( ACTIVE(J) ) THEN
 
            LTAB     =  CPIDX1( J    )
            RTAB     =  CPIDX2( J    )
            LSEG     =  SEGVEC( LTAB )
            RSEG     =  SEGVEC( RTAB )
            LHANS(J) =  STHAN ( LSEG )
            RHANS(J) =  STHAN ( RSEG )
 
            CALL MOVEI ( STSDSC(1,LSEG), SDSCSZ, LSDSC(1,J) )
            CALL MOVEI ( STSDSC(1,RSEG), SDSCSZ, RSDSC(1,J) )
 
            DTPTR   =  STDTPT(LSEG)
 
            DO K = 2, CLIDX1(J)
               DTPTR  =  LNKNXT( DTPTR, DTPOOL )
            END DO
 
            CALL MOVEI ( DTDSCS(1,DTPTR), CDSCSZ,  LDSCRS(1,J) )
 
 
            DTPTR   =  STDTPT(RSEG)
 
            DO K = 2, CLIDX2(J)
               DTPTR  =  LNKNXT( DTPTR, DTPOOL )
            END DO
 
 
            CALL MOVEI ( DTDSCS(1,DTPTR), CDSCSZ, RDSCRS(1,J) )
 
         END IF
 
      END DO
 
 
C
C     Our objective is to limit as far as possible the number of
C     row vectors that have to be tested against the join constraints.
C
C     We break the problem down into cases as follows:
C
C        1)  Try to find a pair of columns related by an equi-join
C            constraint.  If such a pair is found, sort each input
C            join row set using the appropriate column as a key.
C            We then can fairly rapidly compare row vectors for
C            equality in the columns to which the equi-join constraint
C            applies, and limit the application of the remaining tests
C            to row vectors that satisfy the first test.
C
C        2)  If no equi-join constraints are available, look for
C            join constraints using the operators LE, LT, GE, or GT.
C            Sort as in (1); then apply the rest of the constraints.
C
C        3)  Hard luck:  the only constraints we have (if any) involve
C            the operators NE, LIKE, or UNLIKE, none of which are
C            helpful.  Test every row vector.
C
C
C     First step:  We try to find a pair of columns related by an
C     equi-join constraint.
C
 
      CASE  =  NOLUCK
 
      J     =  1
      FND   =  .FALSE.
 
      DO WHILE (  ( J .LE. NJCNST )  .AND.  ( .NOT. FND )  )
 
         IF (  ACTIVE(J)  .AND.  ( OPS(J) .EQ. EQ )  ) THEN
C
C           Good deal, we've got an equi-join constraint.  Save the
C           index of this constraint.
C
            CASE    =  EQUI
            CNSTR   =  J
            FND     =  .TRUE.
         ELSE
            J       =  J + 1
         END IF
 
      END DO
 
 
      IF ( CASE .EQ. NOLUCK ) THEN
 
         J     =  1
         FND   =  .FALSE.
 
         DO WHILE (  ( J .LE. NJCNST )  .AND.  ( .NOT. FND )  )
 
            IF ( ACTIVE(J) ) THEN
 
               IF (      ( OPS(J) .EQ. LT )
     .              .OR. ( OPS(J) .EQ. LE )
     .              .OR. ( OPS(J) .EQ. GE )
     .              .OR. ( OPS(J) .EQ. GT ) ) THEN
C
C                 We've got a non-equi-join constraint.  Save the
C                 index of this constraint.
C
                  CASE    =  NONEQ
                  CNSTR   =  J
                  FND     =  .TRUE.
 
               END IF
 
            END IF
 
            IF ( .NOT. FND ) THEN
               J  =  J  +  1
            END IF
 
         END DO
 
      END IF
 
 
C
C     At this point, we know which case we've got.  If we've picked
C     a distinguished constraint, produce order vectors for each
C     set of input rows vectors, using the keys defined by the
C     join constraint.
C
      IF ( CASE .NE. NOLUCK ) THEN
 
C
C        Produce an order vector for the column on the left side of
C        the CNSTR constraint.  We'll do this by turning the set of
C        row vectors we want to sort into a join row set.  We'll
C        create the join row set metadata and just make it point to
C        the collection of row vectors we wish to sort.  Consult the
C        join row set include file for a picture of the data structure
C        we're creating.
C
         CALL ZZEKSTOP ( LBASE )
 
         LTAB  =  CPIDX1(CNSTR)
         LCOL  =  CLIDX1(CNSTR)
         LELT  =  ELTS1 (CNSTR)
 
C
C        Set JBASE to the base address of the join row set containing
C        the table indicated by LTAB.  Set NT, NR and RB to indicate,
C        respectively, the number of tables in this join row set, the
C        number of rows in the join row set, and the base address of the
C        relevant row vector set.  If LTAB is in the second join row
C        set, we'll adjust TAB to indicate position relative to the set
C        of tables defining the second join row set.
C
         IF ( LTAB .LE. NT1 ) THEN
            JBASE  =  JBASE1
            NT     =  NT1
            NR     =  NR1
            RB     =  RB1
            TAB    =  LTAB
         ELSE
            JBASE  =  JBASE2
            NT     =  NT2
            NR     =  NR2
            RB     =  RB2
            TAB    =  LTAB   -  NT1
         END IF
 
C
C        Save the dimensions and base addresses we'll need later.
C
         SVBAS1 =  JBASE
         SVNT1  =  NT
         SVRB1  =  RB
         SVNR1  =  NR
 
         CALL ZZEKSPSH (  1,    0            )
         CALL ZZEKSPSH (  1,    NR           )
         CALL ZZEKSPSH (  1,    1            )
         CALL ZZEKSPSH (  1,    1            )
         CALL ZZEKSPSH (  1,    SEGVEC(LTAB) )
         CALL ZZEKSPSH (  1,    7            )
         CALL ZZEKSPSH (  1,    NR           )
 
         DO I = 1, NR
C
C           Grab the row pointer in position TAB from the Ith row
C           vector from the join row set containing the parent table
C           of the LHS constraint column.
C
            BASE = JBASE  + RB  + (I-1)*(NT+1)
 
            CALL ZZEKSRD ( BASE+TAB, BASE+TAB, MINIRV )
C
C           Fill in the segment vector pointer for the new very
C           narrow row vector.
C
            MINIRV(2) = JSVBAS
C
C           Append to the join row set under construction.
C
            CALL ZZEKSPSH ( 2, MINIRV )
 
         END DO
 
         CALL ZZEKSTOP ( TOP )
         CALL ZZEKSUPD ( LBASE+JSZIDX,  LBASE+JSZIDX,  TOP-LBASE )
 
         CALL ZZEKJSRT (  1,       LBASE,   1,       1,
     .                    LCOL,    LELT,    EQASND,  STHAN,
     .                    STSDSC,  STDTPT,  DTPOOL,  DTDSCS,  LOVBAS  )
 
 
C
C        Produce an order vector for the column on the right side of
C        the CNSTR constraint.
C
         CALL ZZEKSTOP ( RBASE )
 
         RTAB  =  CPIDX2(CNSTR)
         RCOL  =  CLIDX2(CNSTR)
         RELT  =  ELTS2 (CNSTR)
 
C
C        Set JBASE to the base address of the join row set containing
C        the table indicated by RTAB.  Set NT, NR and RB to indicate,
C        respectively, the number of tables in this join row set, the
C        number of rows in the join row set, and the base address of the
C        relevant row vector set.  If RTAB is in the second join row
C        set, we'll adjust TAB to indicate position relative to the set
C        of tables defining the second join row set.
C
         IF ( RTAB .LE. NT1 ) THEN
            JBASE  =  JBASE1
            NT     =  NT1
            NR     =  NR1
            RB     =  RB1
            TAB    =  RTAB
         ELSE
            JBASE  =  JBASE2
            NT     =  NT2
            NR     =  NR2
            RB     =  RB2
            TAB    =  RTAB   -  NT1
         END IF
 
C
C        Save the dimensions and base addresses we'll need later.
C
         SVBAS2 =  JBASE
         SVNT2  =  NT
         SVRB2  =  RB
         SVNR2  =  NR
 
         CALL ZZEKSPSH (  1,    0             )
         CALL ZZEKSPSH (  1,    NR            )
         CALL ZZEKSPSH (  1,    1             )
         CALL ZZEKSPSH (  1,    1             )
         CALL ZZEKSPSH (  1,    SEGVEC(RTAB)  )
         CALL ZZEKSPSH (  1,    7             )
         CALL ZZEKSPSH (  1,    NR            )
 
         DO I = 1, NR
C
C           Grab the row pointer in position TAB from the Ith row
C           vector from the join row set containing the parent table
C           of the RHS constraint column.
C
            BASE = JBASE + RB + (I-1)*(NT+1)
 
            CALL ZZEKSRD ( BASE+TAB, BASE+TAB, MINIRV )
C
C           Fill in the segment vector pointer for the new very
C           narrow row vector.
C
            MINIRV(2) = JSVBAS
C
C           Append to the join row set under construction.
C
            CALL ZZEKSPSH ( 2, MINIRV )
 
         END DO
 
         CALL ZZEKSTOP ( TOP )
         CALL ZZEKSUPD ( RBASE+JSZIDX,  RBASE+JSZIDX,  TOP-RBASE )
 
         CALL ZZEKJSRT (  1,       RBASE,   1,       1,
     .                    RCOL,    RELT,    EQASND,  STHAN,
     .                    STSDSC,  STDTPT,  DTPOOL,  DTDSCS,  ROVBAS  )
 
C
C        Keep a local copy of the active constraint flags, deactivating
C        the distinguished one.
C
         DO I = 1, NJCNST
            LOCACT(I) = ACTIVE(I)
         END DO
 
         LOCACT(CNSTR) = .FALSE.
 
 
      ELSE
C
C        This is the `no luck' case.  Save all of the constraints.
C
         DO I = 1, NJCNST
            LOCACT(I) = ACTIVE(I)
         END DO
 
C
C        Save the counts pertaining to the input join row sets.
C
         SVNT1  =  NT1
         SVNT2  =  NT2
         SVNR1  =  NR1
         SVNR2  =  NR2
         SVRB1  =  RB1
         SVRB2  =  RB2
         SVBAS1 =  JBASE1
         SVBAS2 =  JBASE2
 
      END IF
 
C
C     In the non-equi-join case, record whether the join constraint
C     requires the left side to be less than, or less than or equal to,
C     the right side.
C
      IF ( CASE .EQ. NONEQ ) THEN
         LSMALL  =  ( OPS(CNSTR) .EQ. LT ) .OR. ( OPS(CNSTR) .EQ. LE )
      END IF
 
C
C     Keep our own copy of the relational constraints, except for the
C     column indices, which are used only in this routine.
C
      SVNCON =  NJCNST
 
      DO I = 1, SVNCON
 
         SVCP1(I) = CPIDX1(I)
         SVOPS(I) = OPS(I)
         SVCP2(I) = CPIDX2(I)
 
      END DO
 
C
C     Initialize the pointers we'll use to keep track of the
C     row vectors we'll be comparing.  Initialize the DONE flag
C     as well.
C
      LPTR  = 1
      LCUR  = 1
      RPTR  = 1
      DONE  = .FALSE.
 
 
      CALL CHKOUT ( 'ZZEKJPRP' )
      RETURN
 
 
 
 
 
 
 
C$Procedure  ZZEKJNXT  ( Return next join row vector )
 
      ENTRY ZZEKJNXT ( FOUND, ROWVEC )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Return row vectors resulting from the join of two collections
C     of row vectors from two join row sets.
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
C
C     LOGICAL               FOUND
C     INTEGER               ROWVEC ( * )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     FOUND      O   Flag indicating whether a row vector was found.
C     ROWVEC     O   Row vector matching join constraints.
C
C$ Detailed_Input
C
C     None.  Inputs are set up by calling ZZEKJPRP.
C
C$ Detailed_Output
C
C     FOUND          is a logical flag indicating whether a row vector
C                    was found on the current call to this routine.
C
C     ROWVEC         is a row vector that satisfies the join
C                    constraints specified by the last set-up call to
C                    ZZEKJPRP.  ROWVEC is a composite of two row
C                    vectors from the join row sets specified by inputs
C                    to ZZEKJPRP.  This row vector does not have the
C                    segment vector pointer filled in.  ROWVEC is
C                    valid only when FOUND is TRUE.
C
C$ Parameters
C
C     See the include files.
C
C$ Exceptions
C
C     All error checking must be performed by the caller of this
C     routine.  Presently, that caller is ZZEKJOIN.
C
C$ Files
C
C     1)  This routine uses the EK scratch area, which employs a scratch
C         DAS file.
C
C$ Particulars
C
C     This routine takes advantage of the preparation performed by
C     ZZEKJPRP to find with reasonable efficiency row vectors satisfying
C     a specified set of join constraints.
C
C$ Examples
C
C     The normal usage of this routine is to call it repeatedly to
C     retrieve one row vector at a time, after setting up the
C     operation by calling ZZEKJPRP:
C
C         CALL ZZEKJPRP ( ... )
C
C         CALL ZZEKJNXT ( FOUND, ROWVEC )
C
C         DO WHILE ( FOUND )
C
C            .
C            .
C            .
C
C            CALL ZZEKJNXT ( FOUND, ROWVEC )
C
C         END DO
C
C
C$ Restrictions
C
C     1)  This routine should not be called by routines outside of the
C         EK system.
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
C-    SPICELIB Version 2.0.0, 20-JUL-1998 (NJB)
C
C        Modified entry point ZZEKJNXT to set FOUND to .FALSE. on the 
C        first pass when CASE is EMPTY.
C
C-    Beta Version 1.0.0, 08-AUG-1995 (NJB)
C
C-&
 
 
C
C     No row vector found to start with.
C
      FOUND = .FALSE.
 
C
C     The action we take depends on the join constraint situation.
C     Handle the "empty" case first.
C
      IF ( CASE .EQ. EMPTY ) THEN
      
         RETURN
         
      ELSE IF ( CASE .EQ. EQUI ) THEN
 
 
         DO WHILE (  ( .NOT. DONE )  .AND.  ( .NOT. FOUND )  )
C
C           At this point, LCUR and RPTR should point to the current
C           pair of order vector entries to use.  We should always have
C
C              1     <  LPTR  <  SVNR1
C                    -        -
C
C              LPTR  <  LCUR  <  SVNR1
C                    -        -
C
C              1     <  RPTR  <  SVNR2
C                    -        -
C
C           here.
C
C           Look up the next set of row vector indices.  Get the row
C           numbers in the join columns for each order vector in our
C           mini-join row sets that we created for sorting.
C
            CALL ZZEKSRD ( LOVBAS+LCUR,  LOVBAS+LCUR,  LRVIDX )
            CALL ZZEKSRD ( ROVBAS+RPTR,  ROVBAS+RPTR,  RRVIDX )
 
            ADDRSS  =  LBASE   +   7   +   2*(LRVIDX-1) + 1
            CALL ZZEKSRD ( ADDRSS, ADDRSS, LROW )
 
            ADDRSS  =  RBASE   +   7   +   2*(RRVIDX-1) + 1
            CALL ZZEKSRD ( ADDRSS, ADDRSS, RROW )
 
C
C           Compare column entries, and advance the pointers as
C           required.
C
            IF (  ZZEKRCMP( LT,
     .                      1,
     .                      LHANS (    CNSTR ),
     .                      LSDSC ( 1, CNSTR ),
     .                      LDSCRS( 1, CNSTR ),
     .                      LROW,
     .                      LELT,
     .                      RHANS (    CNSTR ),
     .                      RSDSC ( 1, CNSTR ),
     .                      RDSCRS( 1, CNSTR ),
     .                      RROW,
     .                      RELT               )  ) THEN
C
C
C              The `left' key entry is smaller.  Advance the bottom
C              pointer on the left side.
C
               IF ( LPTR .LT. SVNR1 ) THEN
                  LPTR  =  LPTR + 1
                  LCUR  =  LPTR
               ELSE
                  DONE  =  .TRUE.
               END IF
 
 
 
            ELSE IF (  ZZEKRCMP( EQ,
     .                           1,
     .                           LHANS (    CNSTR ),
     .                           LSDSC ( 1, CNSTR ),
     .                           LDSCRS( 1, CNSTR ),
     .                           LROW,
     .                           LELT,
     .                           RHANS (    CNSTR ),
     .                           RSDSC ( 1, CNSTR ),
     .                           RDSCRS( 1, CNSTR ),
     .                           RROW,
     .                           RELT               )  ) THEN
C
C
C              The `left' key entry is equal.  Form a composite
C              row vector and test it against the full set of active
C              constraints.
C
               IF ( SVCP1(CNSTR) .LE. SVNT1 ) THEN
C
C                 The parent table of the column on the LHS of our
C                 equi-join constraint belongs to the first join
C                 row set.
C
                  J  =  1
                  K  =  SVNT1 + 1
 
               ELSE
 
                  J  =  SVNT2 + 1
                  K  =  1
 
               END IF
 
 
               OFFSET  =  SVRB1  +  ( LRVIDX - 1 ) * ( SVNT1 + 1 )
 
               CALL ZZEKSRD ( SVBAS1+OFFSET+1,
     .                        SVBAS1+OFFSET+SVNT1, ROWVEC(J) )
 
               OFFSET  =  SVRB2  +  ( RRVIDX - 1 ) * ( SVNT2 + 1 )
 
               CALL ZZEKSRD ( SVBAS2+OFFSET+1,
     .                        SVBAS2+OFFSET+SVNT2,  ROWVEC(K) )
 
C
C              Create row arrays for both sides of each active
C              relational constraint.
C
               DO J = 1, SVNCON
 
                  IF ( LOCACT(J) ) THEN
                     LTAB      =  SVCP1( J    )
                     RTAB      =  SVCP2( J    )
                     LROWS(J)  =  ROWVEC( LTAB )
                     RROWS(J)  =  ROWVEC( RTAB )
                  END IF
 
               END DO
 
               FOUND = ZZEKVMCH ( SVNCON, LOCACT,
     .                            LHANS,  LSDSC,  LDSCRS, LROWS, LELTS,
     .                            SVOPS,
     .                            RHANS,  RSDSC,  RDSCRS, RROWS, RELTS )
 
C
C              Update the pointers.
C
               IF ( LCUR .LT. SVNR1 ) THEN
 
                  LCUR  =  LCUR + 1
 
 
               ELSE IF (        ( LCUR .EQ. SVNR1 )
     .                    .AND. ( RPTR .LT. SVNR2 )  ) THEN
C
C                 We've compared every left hand entry from RPTR
C                 upwards to the right hand entry.  Time to work on
C                 the next right hand entry.
C
                  RPTR  =  RPTR + 1
                  LCUR  =  LPTR
 
               ELSE
C
C                 LCUR and RPTR point to the last entries in their
C                 respective row sets.
C
                  DONE  =  .TRUE.
 
               END IF
 
 
 
            ELSE
C
C              The current left key entry is greater than that
C              on the right.  It's time to look at the next entry
C              on the right, if possible.
C
               IF ( RPTR .LT. SVNR2 ) THEN
                  RPTR  =  RPTR + 1
                  LCUR  =  LPTR
               ELSE
                  DONE  =  .TRUE.
               END IF
 
            END IF
 
C
C           At this point, we've advanced at least one of LPTR, RPTR,
C           or LCUR, or else we've set DONE to .TRUE.
C
         END DO
 
 
 
      ELSE IF ( CASE .EQ. NONEQ ) THEN
C
C        This is the non-equi-join case.
C
         DO WHILE (  ( .NOT. DONE )  .AND.  ( .NOT. FOUND )  )
C
C           At this point, LPTR and RPTR should point to the current
C           pair of order vector entries to use.  We should always have
C
C              1     <  LPTR  <  SVNR1
C                    -        -
C
C              1     <  RPTR  <  SVNR2
C                    -        -
C
C           here.
C
C           Look up the next set of row vector indices.  Get the row
C           numbers in the join columns for each order vector in our
C           mini-join row sets that we created for sorting.
C
            CALL ZZEKSRD ( LOVBAS+LPTR,  LOVBAS+LPTR,  LRVIDX )
            CALL ZZEKSRD ( ROVBAS+RPTR,  ROVBAS+RPTR,  RRVIDX )
 
            ADDRSS  =  LBASE   +   7   +   2*(LRVIDX-1) + 1
            CALL ZZEKSRD ( ADDRSS, ADDRSS, LROW )
 
            ADDRSS  =  RBASE   +   7   +   2*(RRVIDX-1) + 1
            CALL ZZEKSRD ( ADDRSS, ADDRSS, RROW )
 
C
C           Compare column entries, and advance the pointers as
C           required.
C
            IF (  ZZEKRCMP( SVOPS(CNSTR),
     .                      1,
     .                      LHANS (    CNSTR ),
     .                      LSDSC ( 1, CNSTR ),
     .                      LDSCRS( 1, CNSTR ),
     .                      LROW,
     .                      LELT,
     .                      RHANS (    CNSTR ),
     .                      RSDSC ( 1, CNSTR ),
     .                      RDSCRS( 1, CNSTR ),
     .                      RROW,
     .                      RELT               )  ) THEN
C
C
C              This pair of row vectors satisfies the join constraint.
C              Form a composite row vector and test it against the full
C              set of active constraints.
C
               IF ( SVCP1(CNSTR) .LE. SVNT1 ) THEN
C
C                 The parent table of the column on the LHS of our
C                 equi-join constraint belongs to the first join
C                 row set.
C
                  J  =  1
                  K  =  SVNT1 + 1
 
               ELSE
 
                  J  =  SVNT2 + 1
                  K  =  1
 
               END IF
 
 
               OFFSET  =  SVRB1  +  ( LRVIDX - 1 ) * ( SVNT1 + 1 )
 
               CALL ZZEKSRD ( SVBAS1+OFFSET+1,
     .                        SVBAS1+OFFSET+SVNT1, ROWVEC(J) )
 
               OFFSET  =  SVRB2  +  ( RRVIDX - 1 ) * ( SVNT2 + 1 )
 
               CALL ZZEKSRD ( SVBAS2+OFFSET+1,
     .                        SVBAS2+OFFSET+SVNT2,  ROWVEC(K) )
 
 
C
C              Create row arrays for both sides of each active
C              relational constraint.
C
               DO J = 1, SVNCON
 
                  IF ( LOCACT(J) ) THEN
                     LTAB      =  SVCP1( J    )
                     RTAB      =  SVCP2( J    )
                     LROWS(J)  =  ROWVEC( LTAB )
                     RROWS(J)  =  ROWVEC( RTAB )
                  END IF
 
               END DO
 
 
               FOUND = ZZEKVMCH ( SVNCON, LOCACT,
     .                            LHANS,  LSDSC,  LDSCRS, LROWS, LELTS,
     .                            SVOPS,
     .                            RHANS,  RSDSC,  RDSCRS, RROWS, RELTS )
 
 
               IF ( LSMALL ) THEN
C
C                 The `left' key entry is smaller.  All higher-indexed
C                 rows on the right side also satisfy the join
C                 constraint, combined with the current left hand side.
C
                  IF ( RPTR .LT. SVNR2 ) THEN
 
                     RPTR  =  RPTR + 1
 
                  ELSE IF ( LPTR .LT. SVNR1 ) THEN
 
                     LPTR  =  LPTR + 1
                     RPTR  =  1
 
                  ELSE
 
                     DONE  =  .TRUE.
 
                  END IF
 
 
               ELSE
C
C                 The `right' key entry is smaller.  All higher-indexed
C                 rows on the left side also satisfy the join
C                 constraint, combined with the current right hand side.
C
                  IF ( LPTR .LT. SVNR1 ) THEN
 
                     LPTR  =  LPTR + 1
 
                  ELSE IF ( RPTR .LT. SVNR2 ) THEN
 
                     RPTR  =  RPTR + 1
                     LPTR  =  1
 
                  ELSE
 
                     DONE  =  .TRUE.
 
                  END IF
 
 
               END IF
C
C              We incremented LPTR or RPTR, or else we set DONE to
C              .TRUE.
C
 
 
            ELSE
C
C              The constraint was not met by the rows under
C              consideration.
C
               IF ( LSMALL ) THEN
C
C                 If the right side can be incremented, there's a
C                 chance of a match.
C
                  IF ( RPTR .LT. SVNR2 ) THEN
                     RPTR  =  RPTR + 1
                  ELSE
                     DONE  =  .TRUE.
                  END IF
 
 
               ELSE
C
C                 If the left side can be incremented, there's a
C                 chance of a match.
C
                  IF ( LPTR .LT. SVNR1 ) THEN
                     LPTR  =  LPTR + 1
                  ELSE
                     DONE  =  .TRUE.
                  END IF
 
               END IF
C
C              We incremented LPTR or RPTR, or else we set DONE to
C              .TRUE.
C
 
            END IF
 
 
         END DO
 
 
 
      ELSE
 
C
C        We have no order vectors to help us out, so we just loop
C        through every possible combination.  When we find a match,
C        we return immediately, leaving the pointers set to enable
C        continuation of our search when we drop back into the loop
C        on a subsequent call.
C
         DO WHILE ( LPTR .LE. SVNR1 )
 
 
            DO WHILE ( RPTR .LE. SVNR2 )
C
C              Form a composite row vector and test it against the full
C              set of active constraints.
C
               OFFSET  =  SVRB1  +  ( LPTR - 1 ) * ( SVNT1 + 1 )
 
               CALL ZZEKSRD ( SVBAS1+OFFSET+1,
     .                        SVBAS1+OFFSET+SVNT1, ROWVEC  )
 
               OFFSET  =  SVRB2  +  ( RPTR - 1 ) * ( SVNT2 + 1 )
 
               CALL ZZEKSRD ( SVBAS2+OFFSET+1,
     .                        SVBAS2+OFFSET+SVNT2,  ROWVEC(SVNT1+1) )
 
C
C              Create row arrays for both sides of each active
C              relational constraint.
C
               DO J = 1, SVNCON
 
                  IF ( LOCACT(J) ) THEN
                     LTAB      =  SVCP1( J    )
                     RTAB      =  SVCP2( J    )
                     LROWS(J)  =  ROWVEC( LTAB )
                     RROWS(J)  =  ROWVEC( RTAB )
                  END IF
 
               END DO
 
               FOUND = ZZEKVMCH ( SVNCON, LOCACT,
     .                            LHANS,  LSDSC,  LDSCRS, LROWS, LELTS,
     .                            SVOPS,
     .                            RHANS,  RSDSC,  RDSCRS, RROWS, RELTS )
 
               RPTR   =  RPTR + 1
 
               IF ( FOUND ) THEN
                  RETURN
               END IF
 
 
            END DO
 
 
            LPTR  =  LPTR + 1
            RPTR  =  1
 
         END DO
 
 
      END IF
 
 
      RETURN
      END
