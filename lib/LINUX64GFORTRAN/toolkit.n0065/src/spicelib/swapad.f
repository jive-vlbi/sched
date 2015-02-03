C$Procedure      SWAPAD ( Swap elements within a DP array )
 
      SUBROUTINE SWAPAD ( N, LOCN, M, LOCM, ARRAY )
 
C$ Abstract
C
C     Swap (exchange) two non-intersecting groups of contiguous
C     elements of a double precision array.
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
C     ARRAY
C
C$ Declarations
 
      INTEGER               N
      INTEGER               LOCN
      INTEGER               M
      INTEGER               LOCM
      DOUBLE PRECISION      ARRAY  ( * )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     N          I   Number of elements in the first group.
C     LOCN       I   Location of the first group.
C     M          I   Number of elements in the second group.
C     LOCM       I   Location of the second group.
C     ARRAY     I/O  The array.
C
C$ Detailed_Input
C
C     N,
C     LOCN        define the first group of elements to be exchanged:
C                 ARRAY(LOCN) through ARRAY(LOCN+N-1).
C
C     M,
C     LOCM        define the second group of elements to be exchanged:
C                 ARRAY(LOCM) through ARRAY(LOCM+M-1). These must be
C                 distinct from the first group.
C
C     ARRAY       on input contains both groups of elements in their
C                 original locations.
C
C$ Detailed_Output
C
C     ARRAY       on output contains the input array with the indicated
C                 groups of elements exchanged.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the elements to be swapped are not distinct, the error
C        SPICE(NOTDISTINCT) is signalled.
C
C     2) If LOCN or LOCM is less than one, the error
C        SPICE(INVALIDINDEX) is signalled.
C
C     3) If the number of elements to be swapped is less than zero,
C        the error SPICE(INVALIDARGUMENT) is signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     If N [M] is zero, the second [first] group is removed from
C     its current location and inserted in front of ARRAY(LOCN)
C     [ARRAY(LOCM)]. Thus, to move the second [first] group to the
C     front of the list, set N [M] and LOCN [LOCM] to zero and one
C     respectively. To move the group to the end of the list, set
C     N [M] and LOCN [LOCM] to zero and one more than the number of
C     elements in the array.
C
C     All of the elements to be swapped must be distinct.
C
C$ Examples
C
C     Let ARRAY contain the following elements.
C
C           Roosevelt
C           Truman
C           Eisenhower
C           Kennedy
C           Johnson
C           Nixon
C           Ford
C           Carter
C           Reagan
C           Cuomo
C
C     Then the following calls
C
C           CALL SWAPAC (  1,  2,  2,  7,  ARRAY )
C           CALL SWAPAC (  3,  1,  3,  8,  ARRAY )
C           CALL SWAPAC (  3,  4,  0,  1,  ARRAY )
C           CALL SWAPAC (  2,  4,  0,  11, ARRAY )
C
C     yield the following arrays respectively.
C
C           [1]          [2]          [3]          [4]
C
C           Roosevelt    Carter       Kennedy      Roosevelt
C           Ford         Reagan       Johnson      Truman
C           Carter       Cuomo        Nixon        Eisenhower
C           Eisenhower   Kennedy      Roosevelt    Nixon
C           Kennedy      Johnson      Truman       Ford
C           Johnson      Nixon        Eisenhower   Carter
C           Nixon        Ford         Ford         Reagan
C           Truman       Roosevelt    Carter       Cuomo
C           Reagan       Truman       Reagan       Kennedy
C           Cuomo        Eisenhower   Cuomo        Johnson
C
C     The following calls
C
C           CALL SWAPAC ( 3, 2, 4, 5, ARRAY )
C           CALL SWAPAC ( 4, 5, 3, 2, ARRAY )
C
C     yield the following arrays. Note that the resulting arrays
C     are equivalent.
C
C           [1]          [2]
C
C           Roosevelt    Roosevelt
C           Johnson      Johnson
C           Nixon        Nixon
C           Ford         Ford
C           Carter       Carter
C           Truman       Truman
C           Eisenhower   Eisenhower
C           Kennedy      Kennedy
C           Reagan       Reagan
C           Cuomo        Cuomo
C
C
C     The calls
C
C           CALL SWAPAC ( 3,  5, 4,  6, ARRAY )
C           CALL SWAPAC ( 3, -3, 3, 10, ARRAY )
C
C     signal the errors
C
C           SPICE(NOTDISTINCT)
C           SPICE(INVALIDINDEX)
C
C     respectively.
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
C     N.J. Bachman    (JPL)
C     H.A. Neilan     (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.1, 18-MAY-2010 (BVS)
C
C        Removed "C$" markers from text in the header.
C
C-    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in CYCLAD call.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     swap elements within a d.p. array
C
C-&
 
 
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in CYCLAD call.
C
C-    Beta Version 2.0.0, 3-JAN-1989 (HAN)
C
C        The "Particulars" section stated that by setting N [M]
C        to zero, the second [first] group is removed from its current
C        location and inserted in front of ARRAY(LOCM) [ARRAY(LOCN)].
C        That statement was incorrect. Insertion occurs in front of
C        ARRAY(LOCN) [ARRAY(LOCM)]. The section has been corrected.
C
C        New checks for locations were added. LOCN and LOCM must be
C        greater than one, not zero as specified before. If they are
C        not, and error is signalled.
C
C        More examples were added to the "Examples" section, and
C        the long error messages were revised.
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
 
C
C     Local variables
C
      INTEGER               NN
      INTEGER               LN
      INTEGER               NM
      INTEGER               LM
 
      INTEGER               DIRECT
      INTEGER               EXTRA
 
      CHARACTER*1           DIR
      INTEGER               BEGSUB
      INTEGER               NSUB
 
      INTEGER               I
 
 
 
C
C     We will assume that LOCN and N refer to the earlier group of
C     elements, LOCM and M to the later group. (We can always make
C     this true by exchanging their values.) We also assume that
C     all the elements to be swapped are distinct. (That is, LOCM
C     is greater than or equal to LOCN+N.)
C
C     It's easy enough to swap elements on a one-to-one basis, but
C     what about the ones left over? Without extra storage, they can
C     be moved one at a time; but each such move requires moving every
C     element between the origin and destination as well. For large
C     arrays, this is clearly unacceptable.
C
C     In the figure below, the array on the left contains two groups
C     of elements which are to be swapped. We can begin by swapping the
C     leading elements of each group one-for-one.
C
C        +--------------+        +--------------+
C        |              |        |              |
C        +--------------+        +--------------+
C        | Adam         |        | Barney       |
C        +--------------+        +--------------+
C        | Alvin        |        | Betty        |
C        +--------------+        +--------------+
C        |              |        |              |  <---+
C        +--------------+        +--------------+      |
C        |              |        |              |      |
C        +--------------+        +--------------+      |
C        | Barney       |        | Adam         |      |
C        +--------------+        +--------------+      |
C        | Betty        |        | Alvin        |      |
C        +--------------+        +--------------+      |
C        | Bill         |        | Bill         |      |
C        +--------------+        +--------------+      |
C        | Bob          |        | Bob          |  <---+
C        +--------------+        +--------------+
C        |              |        |              |
C        +--------------+        +--------------+
C        |              |        |              |
C        +--------------+        +--------------+
C
C     Notice that cycling the indicated sub-array forward twice brings
C     the remaining elements to their proper locations. This is most
C     fortunate, because cycling the elements of an array is a linear
C     operation. (See CYCLAx for details.)
C
C     And what if the extra elements are in the first group?
C
C        +--------------+        +--------------+
C        |              |        |              |
C        +--------------+        +--------------+
C        | Barney       |        | Adam         |
C        +--------------+        +--------------+
C        | Betty        |        | Alvin        |
C        +--------------+        +--------------+
C        | Bill         |        | Bill         |  <---+
C        +--------------+        +--------------+      |
C        | Bob          |        | Bob          |      |
C        +--------------+        +--------------+      |
C        |              |        |              |      |
C        +--------------+        +--------------+      |
C        |              |        |              |      |
C        +--------------+        +--------------+      |
C        | Adam         |        | Barney       |      |
C        +--------------+        +--------------+      |
C        | Alvin        |        | Betty        |  <---+
C        +--------------+        +--------------+
C        |              |        |              |
C        +--------------+        +--------------+
C        |              |        |              |
C        +--------------+        +--------------+
C
C     In this case, the indicated sub-array must be cycled backward
C     in order to bring the extra elements to their proper places.
C
C     The algorithm is:
C
C        1) Let DIRECT be the smaller of N and M, and let EXTRA
C           be the absolute value of the difference (N-M).
C
C        2) Exchange DIRECT elements directly.
C
C        3) Determine the direction of the cycle: forward when N < M,
C           backward when N > M.
C
C        4) Determine the sub-array to be cycled. It begins at element
C           (LOCN+DIRECT) and contains (LOCM-LOCN) + (M-DIRECT) elements
C
C        5) Cycle the sub-array EXTRA times in the indicated direction.
C
 
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SWAPAD' )
      END IF
 
 
C
C     Check to see if the inputs are valid.
C
 
      IF ( N .LT. 0 ) THEN
 
         CALL SETMSG ( 'Number of elements in the first group is *.' )
         CALL ERRINT ( '*', N                   )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)' )
         CALL CHKOUT ( 'SWAPAD' )
         RETURN
 
      ELSE IF ( M .LT. 0 ) THEN
 
         CALL SETMSG ( 'Number of elements in the second group is *.' )
         CALL ERRINT ( '*', M                   )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)' )
         CALL CHKOUT ( 'SWAPAD' )
         RETURN
 
      ELSE IF ( LOCN .LT. 1 ) THEN
 
         CALL SETMSG ( 'Location of the first group is *.' )
         CALL ERRINT ( '*', LOCN             )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)' )
         CALL CHKOUT ( 'SWAPAD' )
         RETURN
 
      ELSE IF ( LOCM .LT. 1 ) THEN
 
         CALL SETMSG ( 'Location of the second group is *.' )
         CALL ERRINT ( '*', LOCM             )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)' )
         CALL CHKOUT ( 'SWAPAD' )
         RETURN
 
      END IF
 
 
C
C     Make sure we have the groups in the right order.
C
      IF ( LOCN .LT. LOCM ) THEN
         LN = LOCN
         LM = LOCM
         NN = N
         NM = M
      ELSE
         LN = LOCM
         LM = LOCN
         NN = M
         NM = N
      END IF
 
 
C
C     The elements must be distinct.
C
      IF ( LM .LT. LN + NN ) THEN
         CALL SETMSG ( 'Elements to be swapped are not distinct.' )
         CALL SIGERR ( 'SPICE(NOTDISTINCT)' )
         CALL CHKOUT ( 'SWAPAD' )
         RETURN
      END IF
 
C
C     Direct exchange.
C
      DIRECT = MIN ( NN, NM )
 
      DO I = 0, DIRECT-1
         CALL SWAPD ( ARRAY(LN+I), ARRAY(LM+I) )
      END DO
 
C
C     Cycle.
C
      EXTRA = ABS ( NN - NM )
 
      IF ( EXTRA .GT. 0 ) THEN
 
         IF ( NN .LT. NM ) THEN
            DIR = 'F'
         ELSE
            DIR = 'B'
         END IF
 
         BEGSUB = LN + DIRECT
         NSUB   = (LM - LN) + (NM - DIRECT)
 
         CALL CYADIP ( NSUB, DIR, EXTRA, ARRAY(BEGSUB) )
 
      END IF
 
      CALL CHKOUT ( 'SWAPAD' )
      RETURN
      END
