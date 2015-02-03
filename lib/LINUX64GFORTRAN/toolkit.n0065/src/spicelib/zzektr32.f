C$Procedure      ZZEKTR32 ( EK tree, 3-2 merge )
 
      SUBROUTINE ZZEKTR32 ( HANDLE,  TREE,    LEFT,    MIDDLE,
     .                      RIGHT,   PARENT,  LPKIDX,  UNDRFL )
 
C$ Abstract
C
C     Execute a 3-2 merge:  merge three neighboring sibling nodes, two
C     of which contain the minimum number of keys and one of which
C     has an underflow of one key, into two nodes, each one
C     approximately full.
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
 
      INCLUDE 'ektree.inc'
      INCLUDE 'ekpage.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               TREE
      INTEGER               LEFT
      INTEGER               MIDDLE
      INTEGER               RIGHT
      INTEGER               PARENT
      INTEGER               LPKIDX
      LOGICAL               UNDRFL
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     TREE       I   Root of tree.
C     LEFT       I   Left sibling node.
C     MIDDLE     I   Middle sibling node.
C     RIGHT      I   Right sibling node.
C     PARENT     I   Common parent node.
C     LPKIDX     I   Node-relative index of left parent key of MIDDLE.
C     UNDRFL     O   Underflow flag.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle of an EK open for write access.
C
C     TREE           is the root node number of the tree of interest.
C
C     LEFT           is the node number of the left node of a trio of
C                    siblings.  LEFT either contains the minimum
C                    allowed number of keys  or is underflowing by
C                    one key.
C
C     MIDDLE         is the node number of the middle node of a trio of
C                    siblings.  MIDDLE either contains the minimum
C                    allowed number of keys  or is underflowing by
C                    one key.
C
C     RIGHT          is the node number of the right node of a trio of
C                    siblings.  The total number of keys in nodes
C                    LEFT, MIDDLE and RIGHT amounts to an underflow of 1
C                    key.
C
C     PARENT         is the node number of the common parent of LEFT
C                    and RIGHT.
C
C     LPKIDX         is the node-relative index within PARENT of the
C                    left parent key of MIDDLE.  This key is the
C                    immediate predecessor of the first key in the
C                    subtree headed by MIDDLE.
C
C$ Detailed_Output
C
C     UNDRFL         is a logical flag indicating whether the parent
C                    node underflowed as a result of the 3-2 merge.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If HANDLE is invalid, the error will be diagnosed by routines
C         called by this routine.  The file will not be modified.
C
C     2)  If an I/O error occurs while reading the indicated file, the
C         error will be diagnosed by routines called by this routine.
C
C     3)  If LEFT and RIGHT are not neighboring siblings, the error
C         SPICE(BUG) is signalled.
C
C     4)  If either LEFT or RIGHT are not children of PARENT, the error
C         SPICE(BUG) is signalled.
C
C     5)  If the sum of the number of keys in LEFT and RIGHT does not
C         correspond to an underflow of exactly 1 key, the error
C         SPICE(BUG) is signalled.
C
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     Deletions from an EK tree start at a leaf node.  If the node
C     underflows, the EK system attempts to shuffle keys at the leaf
C     level to resolve the underflow.  That attempt failing, the system
C     delegates the problem upward to the next higher level.
C
C     There are two ways to resolve underflow in a non-root node:
C     balance the underflowing node with one of its closest siblings
C     (see ZZEKTRBN), or if the closest siblings contain the minimum
C     number of keys, execute a 3-2 merge.
C
C     A 3-2 merge involves deletion of the middle node of a trio of
C     neighboring siblings, two of which contain the minimum
C     number of keys and one of which has an underflow of one key,
C     and redistributing the keys between the two remaining nodes
C     and their common parent so that each of the two remaining siblings
C     contains the maximum number of keys.  The parent loses a key in
C     the process.
C
C     After the 3-2 merge, the tree is balanced and the siblings
C     satisfy the key count invariants.  However, the parent of the
C     siblings may underflow by one key.
C
C     Below are the gory details concerning the actions of this routine.
C     All of the parameters referred to here (in capital letters) are
C     defined in the include file ektree.inc.
C
C     In a 3-2 merge:
C
C        - The left parent key of the middle child is rotated down
C          into the left child and is appended on the right to the key
C          set of that child.  The left parent key's data pointer moves
C          along with the key.  The child pointers of this parent key
C          do not move along with the key; these pointers point to the
C          left and middle child nodes.
C
C        - The keys of the middle child are divided into three sets:
C          a set to be rotated left through the parent node into the
C          left child, a singleton set consisting of a key to be moved
C          up into the parent, and a set of keys to be rotated right
C          through the parent into the right child.  The sizes of the
C          leftmost and rightmost of these sets differ by at most 1.
C
C        - The number of keys that are rotated left is picked so that
C          after the rotation, the size of the left node will be
C          (3*MNKEYC)/2.  The data pointers of these keys move along
C          with the keys.  All of the left and right child pointers
C          of these keys move along with the keys into the left child.
C
C        - The singleton key in the child moves up into the parent
C          node.  Its data pointer moves with it.  After the move into
C          the parent node, the left child pointer of this key points
C          to the left child; the right child pointer points to the
C          right child.
C
C        - The right parent key of the middle child is rotated right
C          into the right child.  The data pointer of this key moves
C          with the key.  The child pointers of this parent key
C          do not move along with the key; these pointers point to the
C          middle and right child nodes.
C
C        - The remaining keys in the middle child are rotated right
C          into the right child; these become the leftmost keys of
C          that child.  The data pointers of these keys move along
C          with them.  The child pointers of these keys also move
C          along with them.
C
C        - The right child ends up with
C
C             (3*MNKEYC) - (3*MNKEYC)/2
C
C          keys.  This may be deduced from the facts that the original
C          three children had between them an underflow of one key, the
C          parent lost a key, and the left child has (3*MNKEYC)/2 keys.
C
C          Since
C
C             MNKEYC    =  MNKIDC - 1
C                       =  (  ( 2*MXKIDC + 1 ) / 3   )  -  1
C                       =  ( 2*MXKIDC - 2 ) / 3
C                       =  ( 2*MXKEYC ) / 3
C
C          we have
C
C             3*MNKEYC  <  2*MXKEYC
C                       -
C
C          If 3*MNKEYC is odd, we have strict inequality and also
C
C
C             3*MNKEYC  =  2 * ( (3*MNKEYC)/2 )  +  1
C
C          so
C
C             3*MNKEYC  + 1  =  2 * ( (3*MNKEYC)/2 )  +  2
C
C                            =  2 * (  (3*MNKEYC)/2  +  1 )
C
C                            <  2 * MXKEYC
C                            -
C
C          So in this case, the larger of the child nodes, which has
C          size
C
C             (3*MNKEYC)/2  +  1
C
C          has a key count no greater than MXKEYC.
C
C          If 3*MNKEYC is even, then the left and right child are the
C          same size, and the inequality
C
C             3*MNKEYC  <  2*MXKEYC
C                       -
C
C          implies directly that both nodes have size no greater than
C          MXKEYC.
C
C          Since both child nodes have size approximately 3/2 * MNKEYC,
C          and since MNKEYC is approximately 40, the minimum size
C          constraints on the child nodes are easily met.
C
C
C     As the above description shows, the parent loses a key as a
C     result of a 3-2 merge.  This may cause the parent to underflow;
C     if it does, the underflow at the parent's level must be resolved.
C
C$ Examples
C
C     See ZZEKTRDL.
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
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 16-NOV-1995 (NJB)
C
C-&
 
 
 
C
C     Non-SPICELIB functions
C
      INTEGER               ZZEKTRBS
 
C
C     Local parameters
C
      INTEGER               LSIZE
      PARAMETER           ( LSIZE  = (3*MNKEYC)/2 )
 
      INTEGER               RSIZE
      PARAMETER           ( RSIZE  =  3*MNKEYC - LSIZE )
 
C
C     Local variables
C
      INTEGER               C1PAGE ( PGSIZI )
      INTEGER               C2PAGE ( PGSIZI )
      INTEGER               C3PAGE ( PGSIZI )
      INTEGER               DATBAS
      INTEGER               I
      INTEGER               KEYBAS
      INTEGER               KIDBAS
      INTEGER               LEFTSZ
      INTEGER               LMIDSZ
      INTEGER               LPKEY
      INTEGER               LSIB
      INTEGER               MIDSIZ
      INTEGER               MSIB
      INTEGER               N
      INTEGER               NLKEYS
      INTEGER               NMKEYS
      INTEGER               NNODE
      INTEGER               NPKEYS
      INTEGER               NRKEYS
      INTEGER               PPAGE ( PGSIZI )
      INTEGER               PSIZE
      INTEGER               RBASE
      INTEGER               RMIDSZ
      INTEGER               ROOT
      INTEGER               RPKEY
      INTEGER               RSHIFT
      INTEGER               RSIB
      INTEGER               SIZBAS
      INTEGER               SUM
 
 
C
C     Use discovery check-in for speed.
C
C     The plan is to take three sibling nodes, two of which contain
C     the minimum number of keys and one of which is underflowing by one
C     key, and to merge these nodes into two nodes.  This process
C     reduces the number of nodes in the parent by one and may cause the
C     parent to underflow.
C
C     After the merge, the sum of the numbers of keys in the two
C     children will be exactly (3*MNKEYC).  The numbers of keys in the
C     left and right nodes will be, respectively:
C
C
C        LSIZE  =   INT (  (3*MNKEYC)/2  )
C        RSIZE  =   (3*MNKEYC)  - LSIZE
C
C     We need to be sure that LSIZE and RSIZE are in the range
C
C        MNKEYC : MXKEYC
C
C
C     The definition of LSIZE implies that
C
C        LSIZE  =  MNKEYC + INT ( MNKEYC/2 )
C
C
C     so
C
C        MNKEYC + INT ( MNKEYC/2 )  <  LSIZE <  (3/2)*MNKEYC
C                                   -        -
C
C     and since
C
C        MNKEYC  =  MNKIDC - 1
C                =  INT ( ( 2*MXKIDC + 1 ) / 3 ) - 1
C                =  INT ( ( 2*MXKEYC + 3 ) / 3 ) - 1
C                =  INT ( ( 2*MXKEYC     ) / 3 )
C
C     we have
C
C        (3/2) * MNKEYC  =  (3/2) * INT ( (2*MXKEYC) / 3 )  <  MXKEYC
C                                                           -
C
C     Thus  LSIZE is guaranteed to be in range.
C
C     When MNKEYC is even, RSIZE is equal to LSIZE and thus is
C     within bounds.  When MNKEYC is odd, RSIZE exceeds LSIZE by 1, so
C
C        MNKEYC  <  RSIZE
C
C
C     It remains to be shown that
C
C        RSIZE   <  MXKEYC
C                -
C
C     when MNKEYC is odd.  When this is the case, the quantity
C
C        (3/2) * MNKEYC
C
C     is not an integer and therefore is strictly less than MXKEYC.
C     This quantity is also greater than LSIZE, so we conclude that
C
C        LSIZE  <  MXKEYC - 1
C               -
C
C     Since RSIZE exceeds LSIZE by 1, we have
C
C        RSIZE  <  MXKEYC
C               -
C
C     as we claimed.
C
C
C     All right, read in the child and parent pages.
C
      CALL ZZEKPGRI ( HANDLE, LEFT,   C1PAGE )
      CALL ZZEKPGRI ( HANDLE, MIDDLE, C2PAGE )
      CALL ZZEKPGRI ( HANDLE, RIGHT,  C3PAGE )
      CALL ZZEKPGRI ( HANDLE, PARENT, PPAGE  )
 
C
C     The actual addresses in the parent node depend on whether the
C     parent is the root.  Compute the necessary bases to avoid a lot
C     of cases.
C
      ROOT  =  TREE
 
      IF ( PARENT .EQ. ROOT ) THEN
         KEYBAS  =  TRKEYR
         DATBAS  =  TRDATR
         KIDBAS  =  TRKIDR
         SIZBAS  =  TRNKR
      ELSE
         KEYBAS  =  TRKEYC
         DATBAS  =  TRDATC
         KIDBAS  =  TRKIDC
         SIZBAS  =  TRNKC
      END IF
 
C
C     Check the left parent key of the middle child.
C
      PSIZE =  PPAGE ( SIZBAS )
 
      IF (  ( LPKIDX .LT. 1 ) .OR. ( LPKIDX .GT. PSIZE-1 )  )  THEN
 
         CALL CHKIN  ( 'ZZEKTR32'                                )
         CALL SETMSG ( 'Left parent key of MIDDLE is out of '   //
     .                 'range.  Value is #; valid range is 1:#'  )
         CALL ERRINT ( '#',   LPKIDX                             )
         CALL ERRINT ( '#',   PSIZE-1                            )
         CALL SIGERR ( 'SPICE(BUG)'                              )
         CALL CHKOUT ( 'ZZEKTR32'                                )
         RETURN
 
      END IF
 
C
C     Retain the left and right parent key values of the middle child.
C
      LPKEY  =  PPAGE( KEYBAS + LPKIDX    )
      RPKEY  =  PPAGE( KEYBAS + LPKIDX + 1)
 
C
C     Verify that LEFT, MIDDLE, and RIGHT are siblings, and that PARENT
C     is their common parent.
C
      LSIB  =  PPAGE ( KIDBAS+LPKIDX   )
      MSIB  =  PPAGE ( KIDBAS+LPKIDX+1 )
      RSIB  =  PPAGE ( KIDBAS+LPKIDX+2 )
 
 
      IF (      ( LSIB .NE. LEFT   )
     .     .OR. ( MSIB .NE. MIDDLE )
     .     .OR. ( RSIB .NE. RIGHT  )  ) THEN
 
         CALL CHKIN  ( 'ZZEKTR32'                                      )
         CALL SETMSG ( 'LEFT, RIGHT, MIDDLE, PARENT, and PKIDX are '  //
     .                 'inconsistent. LEFT = #; MIDDLE = #; '         //
     .                 'RIGHT = #; PARENT = #; LPKIDX = #; LSIB '     //
     .                 'derived from PARENT = #; MSIB = #; RSIB = #.'  )
         CALL ERRINT ( '#',   LEFT                                     )
         CALL ERRINT ( '#',   MIDDLE                                   )
         CALL ERRINT ( '#',   RIGHT                                    )
         CALL ERRINT ( '#',   PARENT                                   )
         CALL ERRINT ( '#',   LPKIDX                                   )
         CALL ERRINT ( '#',   LSIB                                     )
         CALL ERRINT ( '#',   MSIB                                     )
         CALL ERRINT ( '#',   RSIB                                     )
         CALL SIGERR ( 'SPICE(BUG)'                                    )
         CALL CHKOUT ( 'ZZEKTR32'                                      )
         RETURN
 
      END IF
 
C
C     Get the number of keys in the parent.
C
      IF ( PARENT .EQ. ROOT ) THEN
         NPKEYS = PPAGE ( TRNKR )
      ELSE
         NPKEYS = PPAGE ( TRNKC )
      END IF
 
C
C     Get the number of keys in each child.
C
      NLKEYS  =  C1PAGE ( TRNKC )
      NMKEYS  =  C2PAGE ( TRNKC )
      NRKEYS  =  C3PAGE ( TRNKC )
 
      SUM     =  NLKEYS + NMKEYS + NRKEYS
 
C
C     The sum of the number of keys in the three input nodes must
C     sum exactly to value representing an underflow level of 1 key.
C
      IF (  SUM  .NE.  ( 3*MNKEYC - 1 )  ) THEN
 
         CALL CHKIN  ( 'ZZEKTR32'                                      )
         CALL SETMSG ( 'Number of keys in nodes LEFT = #; in MIDDLE ' //
     .                 '= #; in RIGHT = #; counts summing to # were ' //
     .                 'expected.'                                     )
         CALL ERRINT ( '#',  NLKEYS                                    )
         CALL ERRINT ( '#',  NMKEYS                                    )
         CALL ERRINT ( '#',  NRKEYS                                    )
         CALL ERRINT ( '#',  3*MNKEYC-1                                )
         CALL SIGERR ( 'SPICE(BUG)'                                    )
         CALL CHKOUT ( 'ZZEKTR32'                                      )
         RETURN
 
      END IF
 
C
C     We're set to carry out the merge.  Here's an overview of what
C     gets moved where.
C
C        The left parent key of the middle node moves into the left
C        node, at the end of the node.
C
C        The first N-1 keys and N child pointers of the middle node get
C        moved into the left node, where
C
C           N  =  LSIZE - ( 1 + NLKEYS ) + 1
C
C        The Nth key of the middle node moves into the parent,
C        replacing the left parent key of the middle node.
C
C        The right parent key of the middle node moves into the right
C        node, at the beginning of the node.
C
C        The keys from position N+1 onward in the middle node, as
C        well as all of the remaining child pointers, move into the
C        right node, at the beginning.
C
C        The right parent key's location is filled in by shifting
C        the keys, data pointers, and child pointers in the parent
C        to the left by one position.  The child pointer removed by this
C        operation is the pointer to the middle child.
C
C        The middle child node disappears.
C
C     Before re-arranging things, we'll need to have on hand the key
C     counts for various sets of keys.  We'll use the variable LEFTSZ
C     for the number of keys in the subtree headed by LEFT.  We'll
C     use the variable LMIDSZ to refer to the `subtree' headed by
C     the set of keys in the middle node that will be shifted into
C     the left child.  The variable RMSIZE will represent the size of
C     the key set moved from the middle child into the right child.
C     MIDSIZ will be the key count for the subtree headed by the middle
C     child.
C
C     Consistent with usage above, the variable N will represent
C     the index of the key in the middle node that will rapturously
C     ascend into the parent.
C
 
      IF ( LPKIDX .EQ. 1 ) THEN
         LEFTSZ  =  LPKEY - 1
      ELSE
         LEFTSZ  =  LPKEY - PPAGE(KEYBAS+LPKIDX-1) - 1
      END IF
 
      N       =  LSIZE  -  ( 1 + NLKEYS ) + 1
 
      LMIDSZ  =  C2PAGE(TRKEYC+N)  - 1
      MIDSIZ  =  RPKEY  -  LPKEY   - 1
      RMIDSZ  =  MIDSIZ -  LMIDSZ  - 1
 
C
C     Move the left parent key into the left child.  The key itself
C     doesn't really move; its value is simply re-assigned.  The
C     data pointer is copied, however.  The child pointer at location
C     LSIZE+1 is unaffected by this move.
C
      C1PAGE ( TRKEYC + NLKEYS + 1 )  =  LEFTSZ + 1
      C1PAGE ( TRDATC + NLKEYS + 1 )  =  PPAGE ( DATBAS + LPKIDX )
 
C
C     Move the first N-1 keys and data pointers, and the first N
C     child pointers, from the middle child into the left
C     child.  The moved keys will gain LEFTSZ + 1 predecessors.
C
      DO I = 1, N-1
         C1PAGE(TRKEYC+NLKEYS+1+I) =  C2PAGE(TRKEYC+I) +  LEFTSZ + 1
      END DO
 
      CALL MOVEI ( C2PAGE(TRDATC+1),  N-1,  C1PAGE(TRDATC+NLKEYS+2) )
      CALL MOVEI ( C2PAGE(TRKIDC+1),  N,    C1PAGE(TRKIDC+NLKEYS+2) )
 
C
C     Set the key count in the left child.
C
      C1PAGE ( TRNKC )  =  LSIZE
 
C
C     The left child is complete.  Now it's time to set up the right
C     child.  First off, we'll shift the node's contents to the right
C     by the number of new keys we're going to insert.  Shift the
C     rightmost elements first.  The shifted keys will gain RMIDSZ+1
C     predecessors, so  we adjust the keys as we shift them.
C
      RSHIFT  =  NMKEYS - N + 1
 
      DO I = NRKEYS, 1, -1
         C3PAGE(TRKEYC+RSHIFT+I) =  C3PAGE(TRKEYC+I) + RMIDSZ + 1
      END DO
 
      DO I = NRKEYS, 1, -1
         C3PAGE(TRDATC+RSHIFT+I) =  C3PAGE(TRDATC+I)
      END DO
 
      DO I = NRKEYS+1, 1, -1
         C3PAGE(TRKIDC+RSHIFT+I) =  C3PAGE(TRKIDC+I)
      END DO
 
C
C     The key at location RSHIFT receives the former right parent key
C     of the middle child.   The key value is simply assigned; the
C     data pointer is copied.  The child pointer at location RSHIFT
C     will be set later.
C
      C3PAGE ( TRKEYC + RSHIFT )  =  RMIDSZ + 1
      C3PAGE ( TRDATC + RSHIFT )  =  PPAGE ( DATBAS+LPKIDX+1 )
 
C
C     The first RSHIFT-1 locations in the right child are filled in
C     with data from the middle child.  The moved keys lose LMIDSZ+1
C     precedessors.
C
      DO I = 1, RSHIFT-1
         C3PAGE(TRKEYC+I) = C2PAGE(TRKEYC+N+I) - LMIDSZ - 1
      END DO
 
      CALL MOVEI ( C2PAGE(TRDATC+N+1), RSHIFT-1, C3PAGE(TRDATC+1) )
      CALL MOVEI ( C2PAGE(TRKIDC+N+1), RSHIFT,   C3PAGE(TRKIDC+1) )
 
C
C     Update the key count in the right child.
C
      C3PAGE ( TRNKC )  =  RSIZE
 
C
C     The right child is complete.  It's time to update the parent.
C
C     The key at location N in the middle child replaces the left parent
C     key.  The key value is actually re-assigned; the data pointer does
C     move.  The left parent key increases by the number of keys moved
C     into the subtree headed by the left child.
C
      PPAGE ( KEYBAS + LPKIDX )  =  LPKEY + LMIDSZ + 1
      PPAGE ( DATBAS + LPKIDX )  =  C2PAGE( TRDATC + N )
 
C
C     The parent keys, data pointers, and child pointers at locations
C     LPKIDX+2 onward get shifted left by one position.  The keys lose
C     no  predecessors as the result of these re-arrangements.
C
      DO I = LPKIDX+1, NPKEYS-1
         PPAGE(KEYBAS+I)  =  PPAGE(KEYBAS+I+1)
      END DO
 
      DO I = LPKIDX+1, NPKEYS-1
         PPAGE(DATBAS+I)  =  PPAGE(DATBAS+I+1)
      END DO
 
      DO I = LPKIDX+1, NPKEYS
         PPAGE(KIDBAS+I)  =  PPAGE(KIDBAS+I+1)
      END DO
 
C
C     Zero out the freed locations.
C
      PPAGE ( KEYBAS+NPKEYS   ) = 0
      PPAGE ( DATBAS+NPKEYS   ) = 0
      PPAGE ( KIDBAS+NPKEYS+1 ) = 0
 
C
C     The only required change to the parent's metadata is
C     updating the key count.  At this point, we can set the
C     underflow flag, depending on the status of the parent.
C
      IF ( PARENT .EQ. ROOT ) THEN
 
         PPAGE ( TRNKR )  =  PPAGE ( TRNKR ) - 1
         UNDRFL           =  PPAGE ( TRNKR )   .EQ.   0
 
      ELSE
 
         PPAGE ( TRNKC )  =  PPAGE ( TRNKC ) - 1
         UNDRFL           =  PPAGE ( TRNKC )   .EQ.   MNKEYC - 1
 
      END IF
 
C
C     The last change we must make is to update the node count in
C     the root.
C
      IF ( PARENT .EQ. ROOT ) THEN
 
         PPAGE ( TRNNOD ) =  PPAGE ( TRNNOD ) - 1
 
      ELSE
C
C        We won't read in the whole root page; we'll just get the
C        base address of the root and update the affected location.
C
         RBASE  =  ZZEKTRBS ( ROOT )
 
         CALL DASRDI ( HANDLE, RBASE+TRNNOD, RBASE+TRNNOD, NNODE   )
         CALL DASUDI ( HANDLE, RBASE+TRNNOD, RBASE+TRNNOD, NNODE-1 )
 
      END IF
 
C
C     Write out our updates.
C
      CALL ZZEKPGWI ( HANDLE,  PARENT,  PPAGE  )
      CALL ZZEKPGWI ( HANDLE,  LEFT,    C1PAGE )
      CALL ZZEKPGWI ( HANDLE,  RIGHT,   C3PAGE )
 
C
C     Free the page used by the middle child.
C
      CALL ZZEKPGFR ( HANDLE, INT, MIDDLE )
 
 
      RETURN
      END
