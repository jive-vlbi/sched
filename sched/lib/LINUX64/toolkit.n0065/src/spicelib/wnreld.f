C$Procedure      WNRELD ( Compare two DP windows )
 
      LOGICAL FUNCTION  WNRELD ( A, OP, B )
 
C$ Abstract
C
C      Compare two double precision windows.
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
C      WINDOWS
C
C$ Keywords
C
C      WINDOWS
C
C$ Declarations
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      DOUBLE PRECISION      A        ( LBCELL:* )
      CHARACTER*(*)         OP
      DOUBLE PRECISION      B        ( LBCELL:* )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      A          I   First window.
C      OP         I   Comparison operator.
C      B          I   Second window.
C
C      The function returns the result of comparison: A (OP) B.
C
C$ Detailed_Input
C
C      A,
C      B           are windows, each of which contains zero or more
C                  intervals.
C
C      OP          is a comparison operator, indicating the way in
C                  which the input sets are to be compared. OP may
C                  be any of the following:
C
C                      Operator             Meaning
C                      --------  -------------------------------------
C                        '='     A = B is true if A and B are equal
C                                (contain the same intervals).
C
C                        '<>'    A <> B is true if A and B are not
C                                equal.
C
C                        '<='    A <= B is true if A is a subset of B.
C
C                        '<'     A < B is true is A is a proper subset
C                                of B.
C
C                        '>='    A >= B is true if B is a subset of A.
C
C                        '>'     A > B is true if B is a proper subset
C                                of A.
C
C$ Detailed_Output
C
C      The function returns the result of the comparison.
C
C$ Parameters
C
C      None.
C
C$ Particulars
C
C      This function is true whenever the specified relationship
C      between the input windows, A and B, is satisfied. For example,
C      the expression
C
C           WNRELD ( NEEDED, '<=', AVAIL )
C
C      is true whenever the window NEEDED is a subset of the window
C      AVAIL. One window is a subset of another window if each of
C      the intervals in the first window is included in one of the
C      intervals in the second window. In addition, the first window
C      is a proper subset of the second if the second window contains
C      at least one point not contained in the first window. (Thus,
C      '<' implies '<=', and '>' implies '>='.)
C
C      The following pairs of expressions are equivalent.
C
C           WNRELD ( A, '>', B )
C           WNRELD ( B, '<', A )
C
C           WNRELD ( A, '>=', B )
C           WNRELD ( B, '<=', A )
C
C$ Examples
C
C      Let A contain the intervals
C
C            [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]
C
C      Let B and C contain the intervals
C
C            [ 1, 2 ]  [ 9, 9 ]  [ 24, 27 ]
C
C      Let D contain the intervals
C
C            [ 5, 10 ]  [ 15, 25 ]
C
C      Finally, let E and F be empty windows (containing no intervals).
C
C      Because B and C contain the same intervals,
C
C            WNRELD ( B, '=',  C )
C            WNRELD ( B, '<=', C )
C            WNRELD ( B, '>=', C )
C
C      are all true, while
C
C            WNRELD ( B, '<>', C )
C
C      is false. Because neither B nor C contains any points not also
C      contained by the other, neither is a proper subset of the other.
C      Thus,
C
C            WNRELD ( B, '<', C )
C            WNRELD ( B, '>', C )
C
C      are both false.
C
C      Every point contained in B and C is also contained in A. Thus,
C
C            WNRELD ( B, '<=', A )
C            WNRELD ( A, '>=', C )
C
C      are both true. In addition, A contains points not contained in
C      B and C. (That is, the differences A-B and A-C are not empty.)
C      Thus, B and C are peoper subsets of A as well, and
C
C            WNRELD ( B, '<', A )
C            WNRELD ( A, '>', B )
C
C      are both true.
C
C      Although A and D have points in common, neither contains the
C      other. Thus
C
C            WNRELD ( A, '=',  D )
C            WNRELD ( A, '<=', D )
C            WNRELD ( A, '>=', D )
C
C      are all false.
C
C      In addition, any window is equal to itself, a subset of itself,
C      and a superset of itself. Thus,
C
C            WNRELD ( A, '=',  A )
C            WNRELD ( A, '<=', A )
C            WNRELD ( A, '>=', A )
C
C      are always true. However, no window is a proper subset or a
C      proper superset of itself. Thus,
C
C            WNRELD ( A, '<', A )
C            WNRELD ( A, '>', A )
C
C      are always false.
C
C      Finally, an empty window is a a proper subset of any window
C      except another empty window. Thus,
C
C            WNRELD ( E, '<', A )
C
C      is true, but
C
C            WNRELD ( E, '<', F )
C
C      is false.
C
C$ Exceptions
C
C      If the relational operator is not recognized, the error
C      SPICE(INVALIDOPERATION) is signalled.
C
C$ Files
C
C      None.
C
C$ Restrictions
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
C-     SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
C
C         Set the default function value to either 0, 0.0D0, .FALSE.,
C         or blank depending on the type of the function.
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     compare two d.p. windows
C
C-&
 
 
 
C$ Revisions
C
C-     Beta Version 2.0.0, 2-FEB-1989 (HAN)
C
C         If the relational operator is not recognized, an error is
C         signalled. The previous version returned .FALSE. as the
C         function value, and no error was signalled.
C
C         Also, the Required_Reading section has been changed to
C         include WINDOWS as the required reading for the module.
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      INTEGER               CARDD
      LOGICAL               WNINCD
 
C
C     Local variables
C
      INTEGER               ACARD
      INTEGER               BCARD
      LOGICAL               EQUAL
      LOGICAL               SUBSET
      INTEGER               I
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         WNRELD = .FALSE.
         RETURN
      ELSE
         CALL CHKIN ( 'WNRELD' )
         WNRELD = .FALSE.
      END IF
 
C
C     Find the cardinality of the input windows.
C
      ACARD  = CARDD ( A )
      BCARD  = CARDD ( B )
 
C
C     A and B are equal if they contain exactly the same intervals.
C     We need to know this for nearly every relationship, so find out
C     before going any further.
C
         IF ( ACARD .NE. BCARD ) THEN
            EQUAL = .FALSE.
 
         ELSE
            EQUAL = .TRUE.
 
            DO I = 1, ACARD
               EQUAL = EQUAL .AND. ( A(I) .EQ. B(I) )
            END DO
         END IF
 
C
C     Simple equality and inequality are trivial at this point.
C
      IF ( OP .EQ. '=' ) THEN
 
         WNRELD = EQUAL
 
      ELSE IF ( OP .EQ. '<>' ) THEN
 
         WNRELD = .NOT. EQUAL
 
C
C     Subsets are a little trickier. A is a subset of B if every
C     interval in A is included in B. In addition, A is a proper
C     subset if A and B are not equal.
C
      ELSE IF ( OP .EQ. '<='  .OR.  OP .EQ. '<' ) THEN
 
         SUBSET = .TRUE.
 
         DO I = 1, ACARD, 2
            SUBSET = SUBSET .AND. ( WNINCD ( A(I), A(I+1), B ) )
         END DO
 
         IF ( OP .EQ. '<=' ) THEN
            WNRELD = SUBSET
 
         ELSE
            WNRELD = SUBSET .AND. ( .NOT. EQUAL )
         END IF
 
C
C     A and B change places here...
C
      ELSE IF ( OP .EQ. '>='  .OR.  OP .EQ. '>' ) THEN
 
         SUBSET = .TRUE.
 
         DO I = 1, BCARD, 2
            SUBSET = SUBSET .AND. ( WNINCD ( B(I), B(I+1), A ) )
         END DO
 
         IF ( OP .EQ. '>=' ) THEN
            WNRELD = SUBSET
 
         ELSE
            WNRELD = SUBSET .AND. ( .NOT. EQUAL )
         END IF
 
C
C     An unrecognized operator always fails.
C
      ELSE
 
         CALL SETMSG ( 'Relational operator, *, is not '         //
     .                 'recognized.'                              )
         CALL ERRCH  ( '*', OP                   )
         CALL SIGERR ( 'SPICE(INVALIDOPERATION)' )
         CALL CHKOUT ( 'WNRELD' )
         RETURN
 
      END IF
 
 
      CALL CHKOUT ( 'WNRELD' )
      RETURN
      END
