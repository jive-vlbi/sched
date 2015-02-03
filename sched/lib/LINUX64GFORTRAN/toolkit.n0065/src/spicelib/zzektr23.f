C$Procedure      ZZEKTR23 ( EK tree, 2-3 split )
 
      SUBROUTINE ZZEKTR23 ( HANDLE,  TREE,   LEFT,   RIGHT,
     .                      PARENT,  PKIDX,  OVERFL        )
 
C$ Abstract
C
C     Execute a 2-3 split:  split two sibling nodes into three nodes,
C     each one approximately 2/3 full.
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
      INTEGER               RIGHT
      INTEGER               PARENT
      INTEGER               PKIDX
      LOGICAL               OVERFL
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     TREE       I   Root of tree.
C     LEFT       I   Left sibling node.
C     RIGHT      I   Right sibling node.
C     PARENT     I   Common parent node.
C     PKIDX      I   Node-relative index of parent key.
C     OVERFL     O   Overflow flag.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle of an EK open for write access.
C
C     TREE           is the root node number of the tree of interest.
C
C     LEFT           is the node number of the left node of a pair of
C                    siblings.  LEFT is either full or overflowing by
C                    one key.
C
C     RIGHT          is the node number of the right node of a pair of
C                    siblings.  The total number of keys in nodes
C                    LEFT and RIGHT amounts to an overflow of 1 key.
C
C     PARENT         is the node number of the common parent of LEFT
C                    and RIGHT.
C
C     PKIDX          is the node-relative index in PARENT of the key
C                    that sits between nodes LEFT and RIGHT.
C
C$ Detailed_Output
C
C     OVERFL         is a logical flag indicating whether the parent
C                    node overflowed as a result of the 2-3 split.
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
C         correspond to an overflow of exactly 1 key, the error
C         SPICE(BUG) is signalled.
C
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     Insertions into an EK tree start at a leaf node.  If the node
C     overflows, the EK system attempts to shuffle keys at the leaf
C     level to resolve the overflow.  That attempt failing, the system
C     delegates the problem upward to the next higher level.
C
C     There are two ways to resolve overflow in a non-root node:
C     balance the overflowing node with one of its closest siblings
C     (see ZZEKTRBN), or if the closest siblings are full, execute a 2-3
C     split.
C
C     A 2-3 split involves creation of a new sibling node between two
C     siblings, one of which is full and one of which contains one
C     excess key, and redistributing the keys between the three nodes
C     and their common parent so that each of the three siblings is
C     approximately two-thirds full.  The parent gains a key in the
C     process.
C
C     After the 2-3 split, the tree is balanced and the siblings
C     satisfy the key count invariants.  However, the parent of the
C     siblings may overflow by one key.
C
C     Below are the gory details concerning the actions of this routine.
C     All of the parameters referred to here (in capital letters) are
C     defined in the include file ektree.inc.
C
C     In a 2-3 split:
C
C        - The leftmost (2*MXKEYC)/3 keys of the left child remain in
C          that child.
C
C        - The rest of the keys in the left child node are rotated
C          through the parent into the middle child.  The last of these
C          rotated keys remains in the parent.  The others become the
C          leftmost keys of the middle child.
C
C        - The data values associated with the rotated keys of the
C          left child are moved along with the keys.
C
C        - All but the leftmost of the left child pointers associated
C          with the rotated keys of the left child are moved along with
C          the keys.  The leftmost of these pointers remains in the left
C          child node.
C
C        - The right child pointers associated with the rotated keys
C          of the left child node move along with the keys, except for
C          the right child pointer of the leftmost key of the rotated
C          set.  This leftmost key ends up in the parent, but its right
C          child pointer becomes the leftmost left child pointer of the
C          center sibling.
C
C        - The key from the left child node that is rotated into the
C          parent loses both of its original child pointers; these
C          are replaced by pointers to the left and center siblings.
C
C        - The parent key that originally sat between the left and
C          right siblings is moved down into the center sibling, along
C          with its data value.  It becomes the immediate successor of
C          the set of nodes rotated into the center from the left child.
C
C        - The actions taken to rotate keys from the right child are
C          basically symmetric with those that apply to the left child,
C          except that the number of keys left in the right node is
C          (2*MXKEYC+2)/3, and these keys are shifted to the left side
C          of the right node.  The rightmost key of the rotated set
C          contributed by the right child is placed in the parent as
C          the successor of the key moved into the parent from the left
C          child.  The rest of the rotated set become successors of
C          the key moved into the middle child from the parent.
C
C        - The middle child ends up with (2*MXKEYC+1)/3 keys.  This
C          may be deduced from the facts that the original two children
C          had between them an overflow of one key, the parent gained
C          a key, and the expression
C
C             2*MXKEYC     2*MXKEYC+1     2*MXKEYC+2
C             --------  +  ----------  +  ----------
C                 3             3             3
C
C          where integer division is performed, yields one less than
C          the same expression when real division is performed (since
C          exactly one of the numerators is a multiple of 3).  So the
C          above expression evaluates to
C
C             2*MXKEYC
C
C          which is exactly one less than the number of keys in the
C          original two siblings.
C
C          Since
C
C             MNKEYC  =  MNKIDC - 1
C                     =  (  ( 2*MXKIDC + 1 ) / 3   )  -  1
C                     =  ( 2*MXKIDC - 2 ) / 3
C                     =  ( 2*MXKEYC ) / 3
C
C          we see that the smallest of the new child nodes has at
C          least the minimum allowed number of keys.  The constraint
C          on the maximum is met as well, since the maximum is
C          approximately 3/2 times the minimum, and the minimum is
C          approximately 40.
C
C
C     As the above description shows, the parent gains a key as a
C     result of a 2-3 split.  This may cause the parent to overflow;
C     if it does, the overflow at the parent's level must be resolved.
C
C$ Examples
C
C     See ZZEKTRIN.
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
C     Local variables
C
      INTEGER               BASE
      INTEGER               C1PAGE ( PGSIZI )
      INTEGER               C2PAGE ( PGSIZI )
      INTEGER               C3PAGE ( PGSIZI )
      INTEGER               DATBAS
      INTEGER               I
      INTEGER               KEYBAS
      INTEGER               KIDBAS
      INTEGER               LDELTA
      INTEGER               LNMOVE
      INTEGER               LSHIFT
      INTEGER               LSIB
      INTEGER               LSIZE
      INTEGER               LTRSIZ
      INTEGER               MSIZE
      INTEGER               NEW
      INTEGER               NLKEYS
      INTEGER               NNODE
      INTEGER               NPKEYS
      INTEGER               NRKEYS
      INTEGER               PPAGE ( PGSIZI )
      INTEGER               RBASE
      INTEGER               RDELTA
      INTEGER               RNMOVE
      INTEGER               ROOT
      INTEGER               RSHIFT
      INTEGER               RSIB
      INTEGER               RSIZE
      INTEGER               SUM
 
 
C
C     Use discovery check-in for speed.
C
C     The plan is to take two sibling nodes, one of which is full and
C     one of which is overflowing by 1 key, and to split off about
C     one third of the keys from each one into a new node.  The new
C     node will be a child of the common parent of the input nodes and
C     will be inserted between them.
C
C     After the split, the sum of the numbers of keys in the three
C     children will be exactly 2*MXKEYC.  The numbers of keys in the
C     left, middle, and right nodes will be, respectively:
C
      LSIZE  =   (2*MXKEYC    )/3
      MSIZE  =   (2*MXKEYC + 1)/3
      RSIZE  =   (2*MXKEYC + 2)/3
 
C
C     Note that exactly one of the numerators above is a multiple of 3,
C     so the sum of the above numbers is 1 less than if real division
C     were performed.  Therefore, the sum of the numbers of keys in the
C     child nodes is 2*MXKEYC.  The parent will contain one more node
C     than it did before the split:  the key originally between LEFT and
C     RIGHT will be moved down into the middle child, and the
C     smallest key moved from LEFT and the largest key moved from RIGHT
C     will go into PARENT.
C
      CALL ZZEKPGRI ( HANDLE, LEFT,   C1PAGE )
      CALL ZZEKPGRI ( HANDLE, RIGHT,  C2PAGE )
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
      ELSE
         KEYBAS  =  TRKEYC
         DATBAS  =  TRDATC
         KIDBAS  =  TRKIDC
      END IF
 
C
C     Verify that LEFT and RIGHT are siblings, and that PARENT is
C     their common parent.
C
      LSIB  =  PPAGE ( KIDBAS+PKIDX   )
      RSIB  =  PPAGE ( KIDBAS+PKIDX+1 )
 
      IF (  ( LSIB .NE. LEFT ) .OR. ( RSIB .NE. RIGHT )  ) THEN
 
         CALL CHKIN  ( 'ZZEKTR23'                               )
         CALL SETMSG ( 'LEFT, RIGHT, PARENT, and PKIDX are '   //
     .                 'inconsistent. LEFT = #; RIGHT = #; '   //
     .                 'PARENT = #; PKIDX = #; LSIB derived '  //
     .                 'from PARENT = #; RSIB = #.'             )
         CALL ERRINT ( '#',   LEFT                              )
         CALL ERRINT ( '#',   RIGHT                             )
         CALL ERRINT ( '#',   PARENT                            )
         CALL ERRINT ( '#',   PKIDX                             )
         CALL ERRINT ( '#',   LSIB                              )
         CALL ERRINT ( '#',   RSIB                              )
         CALL SIGERR ( 'SPICE(BUG)'                             )
         CALL CHKOUT ( 'ZZEKTR23'                               )
         RETURN
 
      END IF
 
      NLKEYS  =  C1PAGE ( TRNKC )
      NRKEYS  =  C2PAGE ( TRNKC )
      SUM     =  NLKEYS + NRKEYS
 
C
C     The sum of the number of keys in the two input nodes must
C     sum exactly to the value representing an overflow level of 1 key.
C
      IF (  SUM  .NE.  ( 2*MXKEYC + 1 )  ) THEN
 
         CALL CHKIN  ( 'ZZEKTR23'                                    )
         CALL SETMSG ( 'Number of keys in LEFT = #; number of keys '//
     .                 'in right = #; but sum should be #.'          )
         CALL ERRINT ( '#',  LEFT                                    )
         CALL ERRINT ( '#',  RIGHT                                   )
         CALL ERRINT ( '#',  2*MXKEYC+1                              )
         CALL SIGERR ( 'SPICE(BUG)'                                  )
         CALL CHKOUT ( 'ZZEKTR23'                                    )
         RETURN
 
      END IF
 
C
C     Allocate a new page.  This page will become the right sibling
C     of LEFT and the left sibling of RIGHT.
C
      CALL ZZEKPGAL ( HANDLE, INT,   NEW, BASE )
      CALL CLEARI   ( PGSIZI, C3PAGE )
 
C
C     It's time to set up the keys in the middle child.  First, we'll
C     take the last LSHIFT keys from the left node, where
C
      LSHIFT  =  NLKEYS - ( LSIZE + 1 )
 
C
C     When these keys are moved, they lose LDELTA predecessors, where
C     LDELTA is the size of the key set preceding and including the key
C     at location LSIZE + 1.  The size of this subtree is just the
C     key value at location LSIZE+1.
C
      LDELTA  =  C1PAGE ( TRKEYC + LSIZE + 1 )
 
      DO I = 1, LSHIFT
         C3PAGE(TRKEYC+I) =  C1PAGE(TRKEYC+LSIZE+1+I) -  LDELTA
      END DO
 
      CALL MOVEI ( C1PAGE(TRDATC+LSIZE+2), LSHIFT,   C3PAGE(TRDATC+1) )
      CALL MOVEI ( C1PAGE(TRKIDC+LSIZE+2), LSHIFT+1, C3PAGE(TRKIDC+1) )
 
 
 
C
C     Compute the size of the tree headed by the left subnode.  We'll
C     need this shortly.  The size of this tree is one less than the
C     difference of the parent key and its predecessor, if any.
C
      IF ( PKIDX .EQ. 1 ) THEN
         LTRSIZ  =  PPAGE ( KEYBAS + 1 ) -  1
      ELSE
         LTRSIZ  =  PPAGE(KEYBAS+PKIDX)  -  PPAGE(KEYBAS+PKIDX-1)  -  1
      END IF
 
C
C     The next item to add to the middle child is the middle key
C     from the parent.  The data pointer is copied; the key value is
C     simply set.  The value of the key is one more than the size of
C     the entire key set (including descendants) we moved into the
C     middle from the left.  LNMOVE is the size of this key set.
C
C     No child pointer is copied.
C
      LNMOVE                  = LTRSIZ - LDELTA
      C3PAGE(TRKEYC+LSHIFT+1) = LNMOVE + 1
 
 
      C3PAGE(TRDATC+LSHIFT+1) =  PPAGE ( DATBAS + PKIDX )
 
C
C     Now we copy keys from the right child into the middle.  We'll
C     take the first RSHIFT keys from the right node, where
C
      RSHIFT  =  NRKEYS - ( RSIZE + 1 )
 
C
C     When these keys are moved, they gain RDELTA predecessors, where
C     RDELTA is the size of the key set already in the middle node.
C
      RDELTA  =  LNMOVE + 1
 
      DO I = 1, RSHIFT
         C3PAGE(TRKEYC+LSHIFT+1+I) = C2PAGE(TRKEYC+I) + RDELTA
      END DO
 
      CALL MOVEI ( C2PAGE(TRDATC+1), RSHIFT,   C3PAGE(TRDATC+LSHIFT+2) )
      CALL MOVEI ( C2PAGE(TRKIDC+1), RSHIFT+1, C3PAGE(TRKIDC+LSHIFT+2) )
 
C
C     Save the size of the entire key set moved into the middle from
C     the right.
C
      RNMOVE  =  C2PAGE(TRKEYC+RSHIFT+1) - 1
 
C
C     Set the key count in the new child.
C
      C3PAGE ( TRNKC  )  =  MSIZE
 
C
C     The middle child is complete.
C
C     The next step is to set up the parent node.  The original parent
C     key at index PKIDX is replaced by the key from the left child
C     at location LSIZE + 1.  The following parent keys are shifted
C     right by one location, making room for a second key following
C     the one at PKIDX.  This newly freed slot is filled in with the
C     key at location RSHIFT+1 in the right child.
C
C     The keys in the parent to the right of position PKIDX+1 gain no
C     predecessors as the result of these re-arrangements.
C
C     Get the number of keys in the parent.
C
      IF ( PARENT .EQ. ROOT ) THEN
         NPKEYS = PPAGE ( TRNKR )
      ELSE
         NPKEYS = PPAGE ( TRNKC )
      END IF
 
C
C     Make room for the new key.  Shift elements starting from the
C     right.
C
      DO I = NPKEYS,  PKIDX+1,  -1
         PPAGE(KEYBAS+I+1)  =  PPAGE(KEYBAS+I)
      END DO
 
      DO I = NPKEYS,  PKIDX+1,  -1
         PPAGE(DATBAS+I+1)  =  PPAGE(DATBAS+I)
      END DO
 
      DO I = NPKEYS+1,  PKIDX+1,  -1
         PPAGE(KIDBAS+I+1)  =  PPAGE(KIDBAS+I)
      END DO
 
C
C     Copy in the data pointer from the left child.  Note that
C     no child pointer comes along.
C
      PPAGE ( DATBAS+PKIDX )  =  C1PAGE ( TRDATC + LSIZE + 1 )
 
C
C     Set the key value at PKIDX.  The value exceeds that of the
C     preceding key, if any, by one more than the size of the subtree
C     headed by the left child.  That size is one less than
C     LDELTA, since LDELTA includes the key at location LSIZE+1.
C
      IF ( PKIDX .EQ. 1 ) THEN
         PPAGE ( KEYBAS+PKIDX ) =  LDELTA
      ELSE
         PPAGE ( KEYBAS+PKIDX ) =  PPAGE ( KEYBAS+PKIDX-1 ) + LDELTA
      END IF
 
C
C     Copy in the data pointer from the right child.  Again, note that
C     no child pointer comes along.
C
      PPAGE ( DATBAS+PKIDX+1 ) = C2PAGE ( TRDATC + RSHIFT + 1 )
 
C
C     Set the key value at PKIDX+1.  The value exceeds that of the
C     preceding key by one more than the size of the subtree headed by
C     the middle child.
C
      PPAGE ( KEYBAS+PKIDX+1 ) =    PPAGE (KEYBAS+PKIDX)
     .                            + LNMOVE
     .                            + RNMOVE
     .                            + 2
 
C
C     The child pointer at PKIDX+1 does get set:  it points to the
C     middle child.
C
      PPAGE ( KIDBAS+PKIDX+1 ) = NEW
 
C
C     Remarkably, the only required change to the parent's metadata is
C     updating the key count.  At this point, we can set the overflow
C     flag, depending on the status of the parent.
C
      IF ( PARENT .EQ. ROOT ) THEN
 
         PPAGE ( TRNKR )  =  PPAGE ( TRNKR ) + 1
         OVERFL           =  PPAGE ( TRNKR )   .EQ.   MXKEYR + 1
 
      ELSE
 
         PPAGE ( TRNKC )  =  PPAGE ( TRNKC ) + 1
         OVERFL           =  PPAGE ( TRNKC )   .EQ.   MXKEYC + 1
 
      END IF
 
C
C     Update the metadata in the first child.  This node has lost
C     just enough keys to give it size LSIZE.
C
      C1PAGE ( TRNKC  )  =  LSIZE
 
C
C     For safety, clean out the vacated key and pointer locations.
C     Clear the overflow addresses as well.
C
      CALL CLEARI ( MXKEYC+1-LSIZE,      C1PAGE(TRKEYC + LSIZE + 1)  )
      CALL CLEARI ( MXKEYC+1-LSIZE,      C1PAGE(TRDATC + LSIZE + 1)  )
      CALL CLEARI ( MXKIDC+1-(LSIZE+1),  C1PAGE(TRKIDC + LSIZE + 2)  )
 
C
C     The first child is set.
C
C     To adjust the second child, we must shift the keys and pointers
C     left to fill in the vacated space.  The keys in this second child
C     must be adjusted to account for the loss of the predecessors
C     moved to the middle child and the parent.
C
C     Shift elements starting from the left.
C
      DO I = 1,  RSIZE
         C2PAGE(TRKEYC+I)  =  C2PAGE(TRKEYC+RSHIFT+1+I) - ( RNMOVE + 1 )
      END DO
 
      DO I = 1,  RSIZE
         C2PAGE(TRDATC+I)  =  C2PAGE(TRDATC+RSHIFT+1+I)
      END DO
 
      DO I = 1,  RSIZE+1
         C2PAGE(TRKIDC+I)  =  C2PAGE(TRKIDC+RSHIFT+1+I)
      END DO
 
C
C     Update the key count in the second child.  This node has lost
C     just enough keys to give it size RSIZE.
C
      C2PAGE ( TRNKC  )  =  RSIZE
 
C
C     For safety, clean out the vacated key and pointer locations.
C     Clear the overflow addresses as well.
C
      CALL CLEARI ( MXKEYC+1-RSIZE,      C2PAGE(TRKEYC + RSIZE + 1)  )
      CALL CLEARI ( MXKEYC+1-RSIZE,      C2PAGE(TRDATC + RSIZE + 1)  )
      CALL CLEARI ( MXKIDC+1-(RSIZE+1),  C2PAGE(TRKIDC + RSIZE + 2)  )
 
C
C     The second child is set.
C
C     The last change we must make is to update the node count in
C     the root.
C
      IF ( PARENT .EQ. ROOT ) THEN
 
         PPAGE ( TRNNOD ) =  PPAGE ( TRNNOD ) + 1
 
      ELSE
C
C        We won't read in the whole root page; we'll just get the
C        base address of the root and update the affected location.
C
         RBASE  =  ZZEKTRBS ( ROOT )
 
         CALL DASRDI ( HANDLE, RBASE+TRNNOD, RBASE+TRNNOD, NNODE   )
         CALL DASUDI ( HANDLE, RBASE+TRNNOD, RBASE+TRNNOD, NNODE+1 )
 
      END IF
 
C
C     Write out our updates.
C
      CALL ZZEKPGWI ( HANDLE,  PARENT,  PPAGE  )
      CALL ZZEKPGWI ( HANDLE,  LEFT,    C1PAGE )
      CALL ZZEKPGWI ( HANDLE,  RIGHT,   C2PAGE )
      CALL ZZEKPGWI ( HANDLE,  NEW,     C3PAGE )
 
      RETURN
      END
