C$Procedure  ZZEKJOIN  ( Perform join on two join row sets )
 
      SUBROUTINE ZZEKJOIN ( JBASE1,  JBASE2,  NJCNST,  ACTIVE,
     .                      CPIDX1,  CLIDX1,  ELTS1,   OPS,
     .                      CPIDX2,  CLIDX2,  ELTS2,   STHAN,
     .                      STSDSC,  STDTPT,  DTPOOL,  DTDSCS,
     .                      JBASE3,  NROWS                             )
 
C$ Abstract
C
C     Perform join of two EK join row sets, subject to a specified set
C     of EK join constraints, yielding an EK join row set.
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
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'eksegdsc.inc'
 
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )
 
      INTEGER               JBASE1
      INTEGER               JBASE2
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
      INTEGER               STSDSC ( SDSCSZ, * )
      INTEGER               STDTPT ( * )
      INTEGER               DTPOOL ( 2,      LBPOOL : * )
      INTEGER               DTDSCS ( CDSCSZ,          * )
      INTEGER               JBASE3
      INTEGER               NROWS
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     JBASE1     I   Scratch area base address of first join row set.
C     JBASE2     I   Scratch area base address of second join row set.
C     NJCNST     I   Number of join constraints.
C     ACTIVE     I   Array of flags indicating applicable constraints.
C     CPIDX1     I   Cross product indices for LHS's of constraints.
C     CLIDX1     I   Column indices for LHS's of constraints.
C     ELTS1      I   Column entry elt. indices for LHS'of constraints.
C     OPS        I   Operator codes for constraints.
C     CPIDX2     I   Cross product indices for RHS's of constraints.
C     CLIDX2     I   Column indices for RHS's of constraints.
C     ELTS2      I   Column entry elt. indices for RHS'of constraints.
C     STHAN      I   Array of EK handles corresponding to segments.
C     STSDSC     I   Array of segment descriptors.
C     STDTPT     I   Array of set table column descriptor pointers.
C     DTPOOL     I   Linked list pool for column descriptors.
C     DTDSCS     I   Array of column descriptors.
C     JBASE3     O   Scratch area base address of output join row set.
C     NROWS      O   Number of rows in output join row set.
C     CDSCSZ     P   Size of column descriptor.
C
C$ Detailed_Input
C
C     JBASE1         is the EK scratch area base address of the first
C                    input join row set.  This address is one less than
C                    the first address occupied by the join row set.
C                    See the $Particulars section for a description of
C                    join row sets.
C
C     JBASE2         is the EK scratch area base address of the second
C                    input join row set.  This address is one less than
C                    the first address occupied by the join row set.
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
C     ELTS1          is an array of column entry element indices.  These
C                    indices specify the elements of the LHS column
C                    entries to be used in testing the join constraints.
C                    For scalar columns, the corresponding values of
C                    ELTS1 are ignored.
C
C     OPS            is an array of relational operator codes.  The
C                    Ith code applies to the Ith join constraint.
C
C     CPIDX2,
C     CLIDX2         are, respectively, a set of cross product indices
C                    and column indices that define the columns on the
C                    right-hand sides of the input constraints.  The
C                    meanings of these arrays are analogous to those
C                    of CPIDX1 and CLIDX1.
C
C     ELTS2          is an array of column entry element indices.  These
C                    indices specify the elements of the LHS column
C                    entries to be used in testing the join constraints.
C                    For scalar columns, the corresponding values of
C                    ELTS2 are ignored.
C
C     STHAN          is an array of EK file handles.  The Ith element
C                    of STHAN is the handle of the EK containing the
C                    Ith loaded segment.
C
C     STSDSC         is an array of segment descriptors for all of the
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
C     JBASE3         is the EK scratch area base address of the output
C                    join row set.  This join row set represents that
C                    subset of the Cartesian product of the input
C                    join row sets which satisfies all of the input
C                    join constraints.
C
C     NROWS          is the number of `rows' in the output join row set.
C                    Each such row is actually a vector of rows, one
C                    belonging to each table in the Cartesian product
C                    of tables specified by the join operation.
C
C$ Parameters
C
C     See the include files.
C
C$ Exceptions
C
C     1)  If the number of constaints NCNSTR is out of range, the
C         error SPICE(INVALIDCOUNT) is signalled.
C
C     2)  If the table count in either input join row set is out of
C         range, the error SPICE(INVALIDCOUNT) is signalled.
C
C     3)  If the sum of the table counts of the input join row sets is
C         too large, the error SPICE(INVALIDCOUNT) is signalled.
C
C     4)  If either of cross product table indices for the input
C         constraints is out of range, the error SPICE(INVALIDINDEX) is
C         signalled.
C
C$ Files
C
C     1)  This routine uses the EK scratch area, which employs a scratch
C         DAS file.
C
C$ Particulars
C
C     The purpose of this routine is to compute the set of rows
C     resulting from joining two `join row sets'.  A join row set
C     is a structure in the EK scratch area that represents the
C     result of a table join, subject to constraints.  A join of
C     n tables, subject to constraints, may be computed by joining
C     the join of the first n-1 tables with the nth table; such a
C     procedure is the typical application evisioned for this routine.
C
C     Since all EK rows belong to segments, the set of rows formed by
C     taking the Cartesian product of two tables is actually the union
C     of the sets of rows belonging to the Cartesian products of the
C     possible pairs of segments, where the segments are taken from
C     the two tables being crossed.  Therefore, each join row set is
C     characterized by a list of n-tuples of segments, and by a list of
C     sets of n-tuples of row numbers, one row number set per segment
C     n-tuple.  The segments are identified by a vector of segment
C     list indices, which is called a `segment vector'.  The n-tuples
C     of rows are called `row vectors'.  Each segment vector has a
C     pointer and count that allow addressing the corresponding row
C     vectors.
C
C     Each join row set consists of:
C
C         - a base address in the scratch area
C         - a table count
C         - a segment vector count
C         - a set of segment vectors
C         - a set of segment vector row vector base addresses
C           (these are relative to the base of the join row set)
C         - a set of segment vector row vector counts
C         - a set of row vectors, augmented by offsets of their
C           parent segment vectors (these offsets are at the
C           end of each row vector)
C
C
C     The layout of a join row set in the EK scratch area is shown
C     in the include file for the join row set parameters.
C
C$ Examples
C
C     See EKSRCH.
C
C$ Restrictions
C
C     1)  Relies on the EK scratch area.
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
C-    SPICELIB Version 1.0.1, 20-JUL-1998 (NJB)
C
C        Deleted comment about squeezing out segment vectors without
C        corresponding row vectors; also deleted comment containing
C        a call to ZZEKJSQZ.  
C
C-    Beta Version 1.0.0, 10-OCT-1995 (NJB)
C
C-&
 
 
C
C     Local variables
C
      INTEGER               I
      INTEGER               OFFSET
      INTEGER               NR1
      INTEGER               NR2
      INTEGER               NR3
      INTEGER               NRESV
      INTEGER               NSV1
      INTEGER               NSV2
      INTEGER               NSV3
      INTEGER               NT1
      INTEGER               NT2
      INTEGER               NT3
      INTEGER               RB1
      INTEGER               RB2
      INTEGER               RB3
      INTEGER               ROWVEC ( MXJOIN + 1 )
      INTEGER               S1
      INTEGER               S2
      INTEGER               S3
      INTEGER               SEGVEC ( MXJOIN )
      INTEGER               SGVBAS
      INTEGER               TOP
 
      LOGICAL               FOUND
 
C
C     For speed, we use discovery check-in.  We don't check
C     RETURN at all.
C
C
C     Validate constraint count.
C
      IF (  ( NJCNST .LT. 0 ) .OR. ( NJCNST .GT. MXJCON )  ) THEN
 
         CALL CHKIN  ( 'ZZEKJOIN'                                  )
         CALL SETMSG ( 'Number of join constraints was #; valid ' //
     .                 'range is 0:#'                              )
         CALL ERRINT ( '#',  NJCNST                                )
         CALL ERRINT ( '#',  MXJCON                                )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                       )
         CALL CHKOUT ( 'ZZEKJOIN'                                  )
         RETURN
 
      END IF
 
C
C     Get the table count and segment vector count for each input join
C     row set.
C
      CALL ZZEKSRD (  JBASE1+JTCIDX,  JBASE1+JTCIDX,  NT1   )
      CALL ZZEKSRD (  JBASE1+JSCIDX,  JBASE1+JSCIDX,  NSV1  )
      CALL ZZEKSRD (  JBASE2+JTCIDX,  JBASE2+JTCIDX,  NT2   )
      CALL ZZEKSRD (  JBASE2+JSCIDX,  JBASE2+JSCIDX,  NSV2  )
 
C
C     Set the table count and segment vector count for the output join
C     row set.
C
      NT3  =  NT1  +  NT2
      NSV3 =  NSV1 *  NSV2
 
      IF (  ( NT1 .LT. 1 ) .OR. ( NT2 .GT. MXJOIN-1 )  ) THEN
 
         CALL CHKIN  ( 'ZZEKJOIN'                                      )
         CALL SETMSG ( 'Number tables in first join row set was #; '  //
     .                 'valid range is 1:#'                            )
         CALL ERRINT ( '#',  NT1                                       )
         CALL ERRINT ( '#',  MXJOIN-1                                  )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                           )
         CALL CHKOUT ( 'ZZEKJOIN'                                      )
         RETURN
 
      ELSE IF (  ( NT2 .LT. 1 ) .OR. ( NT2 .GT. MXJOIN-1 )  ) THEN
 
         CALL CHKIN  ( 'ZZEKJOIN'                                      )
         CALL SETMSG ( 'Number tables in second join row set was #; ' //
     .                 'valid range is 1:#'                            )
         CALL ERRINT ( '#',  NT2                                       )
         CALL ERRINT ( '#',  MXJOIN-1                                  )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                           )
         CALL CHKOUT ( 'ZZEKJOIN'                                      )
         RETURN
 
      ELSE IF ( NT3 .GT. MXJOIN ) THEN
 
         CALL CHKIN  ( 'ZZEKJOIN'                                  )
         CALL SETMSG ( 'Number of crossed tables was #; valid '  //
     .                 'range is 0:#'                              )
         CALL ERRINT ( '#',  NT3                                   )
         CALL ERRINT ( '#',  MXJOIN                                )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                       )
         CALL CHKOUT ( 'ZZEKJOIN'                                  )
         RETURN
 
      END IF
 
C
C     Validate cross product indices.  The column indices don't lend
C     themselves to such a convenient check; we'll check those as we
C     use them.
C
      DO I = 1, NJCNST
 
         IF ( ACTIVE(I) ) THEN
 
            IF (       ( CPIDX1(I) .LT. 1   )
     .            .OR. ( CPIDX1(I) .GT. NT3 )  ) THEN
 
               CALL CHKIN  ( 'ZZEKJOIN'                             )
               CALL SETMSG ( 'Cross product table index for left ' //
     .                       'hand side of constraint # was #; '   //
     .                       'valid range is 1:#'                   )
               CALL ERRINT ( '#',  I                                )
               CALL ERRINT ( '#',  CPIDX1(I)                        )
               CALL ERRINT ( '#',  NT3                              )
               CALL SIGERR ( 'SPICE(INVALIDINDEX)'                  )
               CALL CHKOUT ( 'ZZEKJOIN'                             )
               RETURN
 
 
            ELSE IF (      ( CPIDX2(I) .LT. 1   )
     .                .OR. ( CPIDX2(I) .GT. NT3 )  ) THEN
 
               CALL CHKIN  ( 'ZZEKJOIN'                              )
               CALL SETMSG ( 'Cross product table index for right ' //
     .                       'hand side of constraint # was #; '    //
     .                       'valid range is 1:#'                    )
               CALL ERRINT ( '#',  I                                 )
               CALL ERRINT ( '#',  CPIDX2(I)                         )
               CALL ERRINT ( '#',  NT3                               )
               CALL SIGERR ( 'SPICE(INVALIDINDEX)'                   )
               CALL CHKOUT ( 'ZZEKJOIN'                              )
               RETURN
 
            END IF
 
         END IF
 
      END DO
 
C
C     Form the joint row set control area for output join row set.
C
C     The current stack top is the base address of the output join row
C     set.
C
      CALL ZZEKSTOP ( JBASE3  )
C
C     Save room for the size and row vector count
C
      DO I = 1, 2
         CALL ZZEKSPSH ( 1, 0 )
      END DO
 
C
C     The table count and segment vector count come next.
C
      CALL ZZEKSPSH ( 1, NT3  )
      CALL ZZEKSPSH ( 1, NSV3 )
 
C
C     Just reserve room for the segment vectors and the segment vector
C     row set base addresses and counts.
C
      NRESV  =  NSV3 * ( NT3 + 2 )
 
      DO I = 1, NRESV
         CALL ZZEKSPSH ( 1, 0 )
      END DO
 
C
C     Initialize the output segment vector count and the total row
C     count.
C
      S3     =  0
      NROWS  =  0
 
C
C     For every segment vector in the first join row set,
C
      DO S1 = 1, NSV1
C
C        Fill in the first NT1 elements of our composite segment vector
C        with the current segment vector from the first join row set.
C
         OFFSET  =  JSVBAS  +  (S1 - 1) * NT1
 
         CALL ZZEKSRD ( JBASE1+OFFSET+1, JBASE1+OFFSET+NT1, SEGVEC )
 
C
C        Get the row set base address and count for this segment vector.
C
         OFFSET  =  JSVBAS  +  NSV1*NT1  +  2*( S1 - 1 )  +  1
 
         CALL ZZEKSRD ( JBASE1+OFFSET,   JBASE1+OFFSET,   RB1 )
         CALL ZZEKSRD ( JBASE1+OFFSET+1, JBASE1+OFFSET+1, NR1 )
 
C
C        For every segment vector in the second join row set,
C
         DO S2 = 1, NSV2
C
C           Fill in the last NT2 elements of our composite segment
C           vector with the current segment vector from the second join
C           row set.
C
            OFFSET = JSVBAS  +  (S2 - 1) * NT2
 
            CALL ZZEKSRD ( JBASE2+OFFSET+1,
     .                     JBASE2+OFFSET+NT2, SEGVEC(NT1+1) )
 
C
C           Write this segment vector to the output join row set.
C
            S3      =  S3      +  1
            SGVBAS  =  JSVBAS  + ( S3 - 1 ) * NT3
 
            CALL ZZEKSUPD ( JBASE3+SGVBAS+1, JBASE3+SGVBAS+NT3, SEGVEC )
 
C
C           Get the row set base address and count for this segment
C           vector.
C
            OFFSET  =  JSVBAS  +  NSV2*NT2 +  2*( S2 - 1 )  +  1
 
            CALL ZZEKSRD ( JBASE2+OFFSET,   JBASE2+OFFSET,   RB2 )
            CALL ZZEKSRD ( JBASE2+OFFSET+1, JBASE2+OFFSET+1, NR2 )
 
C
C           It's time to decide which row vectors corresponding to
C           our two segment vectors satisfy the join constraints.
C           We pass off the job of determining which row vectors to
C           consider to the subroutine pair ZZEKJPRP (join preparation)
C           and ZZEKJNXT (get next joined row vector).
C
C           We defer establishing the base address of the output
C           row vector set until the join reduction is done, since
C           the join operation will use the scratch area.
C
 
            CALL ZZEKJPRP ( SEGVEC,
     .                      JBASE1,  NT1,     RB1,     NR1,
     .                      JBASE2,  NT2,     RB2,     NR2,
     .                      NJCNST,  ACTIVE,
     .                      CPIDX1,  CLIDX1,  ELTS1,
     .                      OPS,
     .                      CPIDX2,  CLIDX2,  ELTS2,
     .                      STHAN,   STSDSC,  STDTPT,  DTPOOL,  DTDSCS )
 
C
C           Initialize the row count for the current output segment
C           vector.  Also set the segment vector row set base address.
C
            NR3     =  0
 
            CALL ZZEKSTOP ( TOP )
 
            RB3     =  TOP     -  JBASE3
            OFFSET  =  JSVBAS  +  NSV3*NT3  +  ( S3 - 1 ) * 2   +   1
 
            CALL ZZEKSUPD ( JBASE3+OFFSET, JBASE3+OFFSET, RB3 )
 
C
C           Fetch the row vectors that satisfy the join constraints.
C
            NR3  =  0
            CALL ZZEKJNXT ( FOUND, ROWVEC )
 
 
            DO WHILE ( FOUND )
C
C              Append the base offset of the parent segment vector
C              to the row vector.  The base offset is one less than
C              the base-relative address of the segment vector.
C
               NR3                 =   NR3  +  1
               ROWVEC ( NT3 + 1 )  =   SGVBAS
 
C
C              Add this vector to the output join row set.  Get the
C              next row vector.
C
               CALL ZZEKSPSH ( NT3 + 1, ROWVEC )
               CALL ZZEKJNXT ( FOUND,   ROWVEC )
 
            END DO
 
C
C           At this point, we've tested every row corresponding to the
C           current segment vector.  Update the row count for this
C           segment vector.
C
            OFFSET  =  JSVBAS   +  NSV3*NT3  +  ( S3 - 1 ) * 2   +   2
 
            CALL ZZEKSUPD ( JBASE3+OFFSET, JBASE3+OFFSET, NR3 )
 
C
C           Keep the overall row total up to date.
C
            NROWS  =  NROWS + NR3
  
         END DO
 
      END DO
 
C
C     Fill in the row count and size values in the output join row
C     set.
C
      CALL ZZEKSTOP ( TOP                                       )
      CALL ZZEKSUPD ( JBASE3+JSZIDX, JBASE3+JSZIDX, TOP-JBASE3  )
      CALL ZZEKSUPD ( JBASE3+JRCIDX, JBASE3+JRCIDX, NROWS       )
 
C
C     We've constructed the output join row set resulting from
C     joining the input row sets.
C
 
      RETURN
      END
