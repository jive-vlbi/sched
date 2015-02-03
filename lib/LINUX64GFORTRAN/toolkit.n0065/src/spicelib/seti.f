C$Procedure            SETI ( Compare integer sets )
 
      LOGICAL FUNCTION SETI ( A, OP, B )
 
C$ Abstract
C
C     Given a relational operator, compare two integer sets.
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
C     CELLS, SETS
C
C$ Keywords
C
C     CELLS, SETS
C
C$ Declarations
 
      INTEGER                    LBCELL
      PARAMETER                ( LBCELL = -5 )
 
      INTEGER           A      ( LBCELL:* )
      CHARACTER*(*)     OP
      INTEGER           B      ( LBCELL:* )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      A          I   First set.
C      OP         I   Comparison operator.
C      B          I   Second set.
C
C      The function returns the result of the comparison: A (OP) B.
C
C$ Detailed_Input
C
C
C      A           is a set.
C
C
C      OP          is a comparison operator, indicating the way in
C                  which the input sets are to be compared. OP may
C                  be any of the following:
C
C                      Operator             Meaning
C                      --------  -------------------------------------
C                        '='     A = B is true if A and B are equal
C                                (contain the same elements).
C
C                        '<>'    A <> B is true if A and B are not
C                                equal.
C
C                        '<='    A <= B is true if A is a subset of B.
C
C                        '<'     A < B is true if A is a proper subset
C                                of B.
C
C                        '>='    A >= B is true if B is a subset of A.
C
C                        '>'     A > B is true if B is a proper subset
C                                of A.
C
C                        '&'     A & B is true if A and B have one or
C                                more elements in common. (The
C                                intersection of the two sets in
C                                non-empty.)
C
C                        '~'     A ~ B is true if A and B are disjoint
C                                sets.
C
C      B           is a set.
C
C$ Detailed_Output
C
C      The function returns the result of the comparison: A (OP) B.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      None.
C
C$ Examples
C
C      1) In the following example, SETx is used to repeat an operation
C         for as long as the integer set FINISHED remains a proper
C         subset of the integer set PLANNED.
C
C            DO WHILE ( SETx ( FINISHED, '<', PLANNED ) )
C               .
C               .
C            END DO
C
C
C      2) In the following example, let the integer sets A, B, and C
C         contain the elements listed below. Let E be an empty integer
C         set.
C
C              A        B        C
C            ---      ---      ---
C              1        1        1
C              2        3        3
C              3
C              4
C
C      Then all of the following expressions are true.
C
C            SETI ( B, '=',  C )      "B is equal to C"
C            SETI ( A, '<>', C )      "A is not equal to C"
C            SETI ( A, '>',  B )      "A is a proper superset of B"
C            SETI ( B, '<=', C )      "B is a subset of C"
C            SETI ( C, '<=', B )      "C is a subset of B"
C            SETI ( A, '<=', A )      "A is a subset of A"
C            SETI ( E, '<=', B )      "E is a subset of B"
C            SETI ( E, '<',  B )      "E is a proper subset of B"
C            SETI ( E, '<=', E )      "E is a subset of E"
C            SETI ( A, '&',  B )      "A has elements in common with B."
C            SETI ( B, '&',  C )      "B has elements in common with C."
C
C      And all of the following are false.
C
C            SETI ( B, '<>',  C )      "B is not equal to C"
C            SETI ( A, '=',   C )      "A is equal to C"
C            SETI ( A, '<',   B )      "A is a proper subset of B"
C            SETI ( B, '<',   C )      "B is a proper subset of C"
C            SETI ( B, '>=',  A )      "B is a superset of A"
C            SETI ( A, '>',   A )      "A is a proper superset of A"
C            SETI ( E, '>=',  A )      "E is a superset of A"
C            SETI ( E, '<',   E )      "E is a proper subset of E"
C            SETI ( A, '~',   B )      "A and B are disjoint sets."
C
C$ Restrictions
C
C      None.
C
C$ Exceptions
C
C      If the set relational operator is not recognized, the error
C      SPICE(INVALIDOPERATION) is signalled.
C
C$ Files
C
C      None.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      H.A. Neilan     (JPL)
C      W.L. Taber      (JPL)
C      I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
C
C       Set the default function value to either 0, 0.0D0, .FALSE.,
C       or blank depending on the type of the function.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     compare integer sets
C
C-&
 
 
 
 
 
C$ Revisions
C
C-    Beta Version 2.0.0, 11-JAN-1989 (WLT) (HAN)
C
C       The old version was not compatible with the error handling
C       mechanism. Taking the difference of sets A and B caused an
C       overflow of the set DIFF, whose dimension was one. The method of
C       determining the function value has been redesigned, and the
C       difference of the sets is no longer computed.
C
C       The new routine recognizes two new operators, '~' and '&'.
C       If the operator is not recognized, an error is now signalled.
C
C-&
 
 
C
C     SPICELIB functions
C
      INTEGER          CARDI
      LOGICAL          RETURN
 
C
C     Local variables
C
      INTEGER          CARDA
      INTEGER          CARDB
      INTEGER          CONDLT
      INTEGER          CONDEQ
      INTEGER          CONDGT
      INTEGER          CONDOA
      INTEGER          CONDOB
      INTEGER          CONDAB
      INTEGER          INDEXA
      INTEGER          INDEXB
      INTEGER          COND
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         SETI = .FALSE.
         RETURN
      ELSE
         CALL CHKIN ( 'SETI' )
         SETI = .FALSE.
      END IF
 
 
C
C     Obtain the cardinality of the sets.
C
      CARDA = CARDI ( A )
      CARDB = CARDI ( B )
 
 
C
C     The easiest way to compare two sets is to list them side by side
C     as shown below:
C
C     Set A  Set B
C     -----  -----
C       1      1
C              2
C       3      3
C       4      4
C       5
C              6
C       7      7
C
C     When listed this way, one can easily determine intersections,
C     differences, and unions.  Moreover, to determine if one set
C     is a subset of another, if they are equal, etc, one can just
C     inspect the two lists.
C
C     We can mimick this in an algorithm.  The main trick is to figure
C     out how to list the sets in this way.  Once we know how to
C     list them, we can simply adapt the listing algorithm to get
C     a comparison algorithm.
C
C     By the time we get this far, we know that our sets have distinct
C     elements and they are ordered. To write out the list above,
C     we start at the beginning of both sets (they're ordered,
C     remember?).  Look at the next element of A and the next element
C     of B ( to start out ``next'' means ``first'' ).  If the item
C     from A is smaller it should be written and space should be left
C     in the B column. If they are the same write them both.  Otherwise,
C     the item from B is smaller, so  leave space in the A column and
C     write the item from B.  Continue until you run out of items in
C     one of the sets.  Then just write down all those remaining in the
C     other set in the appropriate column.  This is what the loop
C     below does.
C
C
C           NEXTA = 1
C           NEXTB = 1
C
C           DO WHILE (       ( NEXTA .LT. CARD(A) )
C          .           .AND. ( NEXTB .LT. CARD(B) ) )
C
C              IF ( A(NEXTA) .LT. B(NEXTB) ) THEN
C
C                 WRITE (UNIT,*) A(NEXTA),   SPACES
C                 NEXTA = NEXTA + 1
C
C              ELSE IF ( A(NEXTA) .EQ. B(NEXTB) ) THEN
C
C                 WRITE (UNIT,*) A(NEXTA),   B(NEXTB)
C                 NEXTA = NEXTA + 1
C                 NEXTB = NEXTB + 1
C
C              ELSE
C
C                 WRITE (UNIT,*) SPACES, B(NEXTB)
C                 NEXTB = NEXTB + 1
C
C              END IF
C           END DO
C
C           DO NEXTA = 1, CARD(A)
C              WRITE (UNIT,*) A(NEXTA),SPACES
C           END DO
C
C           DO NEXTB = 1, CARD(B)
C              WRITE (UNIT,*) B(NEXTB),SPACES
C           END DO
C
C
C     This also gives us a way to compare the elements of the two
C     sets one item at a time.  Instead of writing the items, we
C     can make a decision as to whether or not the sets have the
C     relationship we are interested in.
C
C     At the beginning of the loop we assume that the two sets are
C     related in the way we want.  Once the comparison has been made
C     we can decide if they are still related in that way.  If not,
C     we can RETURN .FALSE.  Using psuedo-code the loop is modified
C     as shown below.
C
C           NEXTA = 1
C           NEXTB = 1
C
C           DO WHILE (       ( NEXTA .LT. CARD(A) )
C          .           .AND. ( NEXTB .LT. CARD(B) ) )
C
C              IF ( A(NEXTA) .LT. B(NEXTB) ) THEN
C
C                 RELATED = RELATIONSHIP_OF_INTEREST(A<B)
C                 NEXTA   = NEXTA + 1
C
C              ELSE IF ( A(NEXTA) .EQ. B(NEXTB) ) THEN
C
C                 RELATED = RELATIONSHIP_OF_INTEREST(A=B)
C                 NEXTA   = NEXTA + 1
C                 NEXTB   = NEXTB + 1
C
C              ELSE
C
C                 RELATED = RELATIONSHIP_OF_INTEREST(A>B)
C                 NEXTB   = NEXTB + 1
C
C              END IF
C
C              IF ( SURE_NOW(RELATED) ) THEN
C                 RETURN with the correct value.
C              ELSE
C                 Keep going.
C              END IF
C
C           END DO
C
 
 
 
 
C
C     Using the cardinality of the two sets, some function
C     values can be determined right away. If the cardinality
C     is not enough, we need to set up some conditions for the
C     loop which compares the individual elements of the sets.
C
 
 
C
C     A cannot be a proper subset of B if the cardinality of A is
C     greater than or equal to the cardinality of B.
C
      IF      ( OP .EQ. '<' ) THEN
 
              IF ( CARDA .GE. CARDB ) THEN
                 SETI = .FALSE.
                 CALL CHKOUT ( 'SETI' )
                 RETURN
              ELSE
                 CONDLT = 0
                 CONDEQ = 1
                 CONDGT = 1
                 CONDOA = 0
                 CONDOB = 1
                 CONDAB = 1
              END IF
 
 
C
C     A cannot be a subset of B if A contains more elements than B.
C
      ELSE IF ( OP .EQ. '<=' ) THEN
 
              IF ( CARDA .GT. CARDB ) THEN
                 SETI = .FALSE.
                 CALL CHKOUT ( 'SETI' )
                 RETURN
               ELSE
                 CONDLT = 0
                 CONDEQ = 1
                 CONDGT = 1
                 CONDOA = 0
                 CONDOB = 1
                 CONDAB = 1
               END IF
 
 
C
C     If the cardinality of the two sets is not equal, there's no way
C     that the two sets could be equal.
C
      ELSE IF ( OP .EQ. '=' ) THEN
 
               IF ( CARDA .NE. CARDB ) THEN
                  SETI = .FALSE.
                  CALL CHKOUT ( 'SETI' )
                  RETURN
               ELSE
                  CONDLT = 0
                  CONDEQ = 1
                  CONDGT = 0
                  CONDOA = 0
                  CONDOB = 0
                  CONDAB = 1
               END IF
 
 
C
C     If the cardinality of the two sets is not equal, the sets
C     are not equal.
C
      ELSE IF ( OP .EQ. '<>' ) THEN
 
              IF ( CARDA .NE. CARDB ) THEN
                 SETI = .TRUE.
                 CALL CHKOUT ( 'SETI' )
                 RETURN
              ELSE
                 CONDLT = 2
                 CONDEQ = 1
                 CONDGT = 2
                 CONDOA = 0
                 CONDOB = 0
                 CONDAB = 0
              END IF
 
 
C
C     B cannot be a proper subset of A if the cardinality of A is less
C     than or equal to the cardinality of B.
C
      ELSE IF ( OP .EQ. '>' ) THEN
 
              IF ( CARDA .LE. CARDB ) THEN
                 SETI = .FALSE.
                 CALL CHKOUT ( 'SETI' )
                 RETURN
              ELSE
                 CONDLT = 1
                 CONDEQ = 1
                 CONDGT = 0
                 CONDOA = 1
                 CONDOB = 0
                 CONDAB = 1
              END IF
C
C     B cannot be a subset of A if B contains more elements than A.
C
      ELSE IF ( OP .EQ. '>=' ) THEN
 
              IF ( CARDA .LT. CARDB ) THEN
                 SETI = .FALSE.
                 CALL CHKOUT ( 'SETI' )
                 RETURN
              ELSE
                 CONDLT = 1
                 CONDEQ = 1
                 CONDGT = 0
                 CONDOA = 1
                 CONDOB = 0
                 CONDAB = 1
              END IF
C
C     If the cardinality of one of the sets is zero, they can't
C     possibly have any elements in common.
C
      ELSE IF ( OP .EQ. '&' ) THEN
 
              IF ( ( CARDA .EQ. 0 ) .OR. ( CARDB .EQ. 0 ) ) THEN
                 SETI = .FALSE.
                 CALL CHKOUT ( 'SETI' )
                 RETURN
              ELSE
                 CONDLT = 1
                 CONDEQ = 2
                 CONDGT = 1
                 CONDOA = 0
                 CONDOB = 0
              END IF
 
 
C
C     If either A or B is the null set, the two sets are disjoint.
C
      ELSE IF ( OP .EQ. '~' ) THEN
 
              IF ( ( CARDA .EQ. 0 ) .OR. ( CARDB .EQ. 0 ) ) THEN
                 SETI = .TRUE.
                 CALL CHKOUT ( 'SETI' )
                 RETURN
              ELSE
                 CONDLT = 1
                 CONDEQ = 0
                 CONDGT = 1
                 CONDOA = 1
                 CONDOB = 1
              END IF
 
C
C     If the relational operator is not recognized, signal an
C     error.
C
 
      ELSE
             CALL SETMSG ( 'Relational operator, *, is not '         //
     .                     'recognized.'                              )
             CALL ERRCH  ( '*', OP                   )
             CALL SIGERR ( 'SPICE(INVALIDOPERATION)' )
             CALL CHKOUT ( 'SETI' )
             RETURN
      END IF
 
 
C
C     Initialize counters used for checking the elements of the sets.
C
      INDEXA    = 1
      INDEXB    = 1
      COND      = 0
 
 
C
C     If we've come this far we need to check the elements of the
C     sets to determine the function value.
C
      DO WHILE ( ( INDEXA .LE. CARDA ) .AND. ( INDEXB .LE. CARDB ) )
 
         IF      ( A (INDEXA) .LT. B (INDEXB) )  THEN
 
                   COND   = CONDLT
                   INDEXA = INDEXA + 1
 
         ELSE IF ( A (INDEXA) .EQ. B (INDEXB) )  THEN
 
                   COND   = CONDEQ
                   INDEXA = INDEXA + 1
                   INDEXB = INDEXB + 1
 
         ELSE
 
                   COND   = CONDGT
                   INDEXB = INDEXB + 1
 
         END IF
 
 
C
C        At this point, there are several cases which allow us to
C        determine the function value without continuing to compare
C        the elements of the sets:
C
C        1. If the operator is '~' and a common element was found,
C           the sets are not disjoint ( COND = 0 ).
C
C        2. If the operator is '&' and a common element was found,
C           the sets have at least one common element ( COND = 2 ).
C
C        3. If the sets are being compared for containment, and the
C           first element of the "contained" set is less than the first
C           element of the "containing" set, the "contained" set
C           cannot be a subset of the "containing" set ( COND = 0 ).
C
C        4. If the operator is '=' and the elements being compared are
C           not equal, the sets are not equal ( COND = 0 ).
C
C        5. If the operator is '<>' and the elements being compared are
C           not equal, the sets are not equal ( COND = 2 ).
C
C
         IF      ( COND .EQ. 0 ) THEN
 
                  SETI = .FALSE.
                  CALL CHKOUT ( 'SETI' )
                  RETURN
 
         ELSE IF ( COND .EQ. 2 ) THEN
 
                  SETI = .TRUE.
                  CALL CHKOUT ( 'SETI' )
                  RETURN
 
         END IF
 
      END DO
 
 
C
C     We've exited the loop, so now we need to make a decision based on
C     what's left over.
C
 
C
C     We've gone through all of set B and there are elements left in
C     A.
C
      IF      ( INDEXA .LE. CARDA ) THEN
                COND = CONDOA
C
C     We've gone through all of set A and there are elements left in
C     B.
C
      ELSE IF ( INDEXB .LE. CARDB ) THEN
                COND = CONDOB
C
C     We've gone through both the sets.
C
      ELSE
                COND = CONDAB
      END IF
 
 
 
C
C     Determine the value of SETI from the results.
C
      SETI = ( COND .EQ. 1 )
 
 
 
      CALL CHKOUT ( 'SETI' )
      RETURN
      END
