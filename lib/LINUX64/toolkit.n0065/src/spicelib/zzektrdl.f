C$Procedure      ZZEKTRDL ( EK tree, delete value )
 
      SUBROUTINE ZZEKTRDL ( HANDLE, TREE, KEY )
 
C$ Abstract
C
C     Delete a value from an EK tree at a specified location.
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
 
      INCLUDE 'ekpage.inc'
      INCLUDE 'ektree.inc'
 
      INTEGER               HANDLE
      INTEGER               TREE
      INTEGER               KEY
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     TREE       I   Root of tree.
C     KEY        I   Key at which to delete value.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle of an EK open for write access.
C
C     TREE           is the root node number of the tree of interest.
C
C     KEY            is an absolute key indicating the deletion
C                    location.  In EK trees, absolute keys are just
C                    ordinal positions relative to the leftmost element
C                    of the tree, with the leftmost element having
C                    position 1.  So setting KEY to 10, for example,
C                    indicates that the input VALUE is the 10th item in
C                    the tree.
C
C                    KEY must be in the range 1 : NKEYS, where
C                    NKEYS is the number of keys in the tree prior to
C                    the deletion.
C
C$ Detailed_Output
C
C     None.  See $Particulars for a description of the effect of this
C     routine.
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
C     2)  If an I/O error occurs while reading or writing the indicated
C         file, the error will be diagnosed by routines called by this
C         routine.
C
C     3)  If the input key is out of range, the error is diagnosed by
C         routines called by this routine.
C
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This routine deletes a value from an EK tree at a specified
C     location.  The successor of the value and all higher-indexed
C     values have their indexes decremented.  Since keys are stored in
C     subtree-relative form, the only keys actually modified by the
C     deletion itself are higher-indexed keys in the node from which
C     the deletion is done, and higher-indexed keys in the chain of
C     ancestors of this node.
C
C     The deletion is always done from a leaf node.  If KEY is not in a
C     leaf node, the value corresponding to KEY is swapped with that of
C     an immediate neighbor, and the neighbor is deleted.  This is
C     possible because every key is either in a leaf or has the property
C     that its predecessor and successor are both located in leaf nodes.
C
C     The deletion is not the end of the story, however:  it's possible
C     that the node from which the deletion is done (the `target node')
C     will underflow.  If underflow occurs, this routine will restore
C     the tree to its normal form as follows:
C
C        1)  If a neighbor of the target node contains at least one more
C            key than the minimum allowed number, data will be `rotated'
C            from the neighbor node, through the target's parent,
C            and into the target.  The deletion is complete at this
C            point.
C
C        2)  If the target node has only one neighbor, but that neighbor
C            is neighbor to a sibling that can contribute a key, data
C            will be rotated from the second sibling, through the
C            siblings' parent, into the first sibling, and then from
C            the first sibling through the target's parent, and into
C            the target.  The deletion is complete at this point.
C
C        3)  If the target is not a child of the root, and if
C            the target has two neighbors, but neither neighbor has a
C            key to spare, then the target node and its neighbors will
C            be merged into two nodes:   this is called a `3-2 merge'.
C            The parent node is modified appropriately so that all
C            values are in the proper order and all subtree-relative
C            keys are correct.  This `3-2 merge' decreases the number
C            of values in the parent by one.  If the decrease does not
C            cause an underflow in the parent, the deletion is complete.
C
C            If the target has only one neighbor, and both the neighbor
C            and the neighbor's other neighbor (which always exists)
C            contain the minimum number of keys, these three nodes are
C            combined into two via a 3-2 merge.
C
C        4)  If the parent underflows as a result of a 3-2 merge, the
C            solution process is repeated at the parent's level.  The
C            process iterates until the underflow is resolved or a
C            child of the root underflows.
C
C        5)  If a child of the root underflows, the problem is solved
C            by balancing keys with a neighbor if possible.  Balancing
C            cannot be done only if the root has only two children, and
C            these contain the minimum number of keys.  In this case,
C            the contents of the two children of the root are moved
C            into the root and the children are eliminated.  The
C            children of the child nodes become children of the root.
C            This is the only case in which the tree grows shorter.
C
C            The process of collapsing two child nodes into the root is
C            called a `3-1 merge'.  After a 3-1 merge is performed, the
C            number of values in each node is within bounds.
C
C
C     An EK tree is always balanced after a deletion:  all leaf nodes
C     are at the same level.
C
C$ Examples
C
C     See EKDELR.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     1)  Knuth, Donald E.  "The Art of Computer Programming, Volume
C         3/Sorting and Searching" 1973, pp 471-479.
C
C         EK trees are closely related to the B* trees described by
C         Knuth.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 23-OCT-1995 (NJB)
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
 
C
C     Other functions
C
      INTEGER               ZZEKTRNK
 
C
C     Local parameters
C
      INTEGER               TERM
      PARAMETER           ( TERM   = 1 )
 
      INTEGER               LCHECK
      PARAMETER           ( LCHECK = TERM   + 1 )
 
      INTEGER               RCHECK
      PARAMETER           ( RCHECK = LCHECK + 1 )
 
      INTEGER               BALNCE
      PARAMETER           ( BALNCE = RCHECK + 1 )
 
      INTEGER               MERG32
      PARAMETER           ( MERG32 = BALNCE + 1 )
 
      INTEGER               MERG31
      PARAMETER           ( MERG31 = MERG32 + 1 )
 
      INTEGER               LLCHCK
      PARAMETER           ( LLCHCK = MERG31 + 1 )
 
      INTEGER               RRCHCK
      PARAMETER           ( RRCHCK = LLCHCK + 1 )
 
C
C     Local variables
C
      INTEGER               IDX
      INTEGER               LEFT
      INTEGER               LEVEL
      INTEGER               LKEY
      INTEGER               LLKEY
      INTEGER               LLSIB
      INTEGER               LNKEY
      INTEGER               LNODE
      INTEGER               LPIDX
      INTEGER               LPKEY
      INTEGER               LRKEY
      INTEGER               LRSIB
      INTEGER               LSIB
      INTEGER               MNODE
      INTEGER               NKEYS
      INTEGER               NODE
      INTEGER               NOFFST
      INTEGER               PARENT
      INTEGER               PKEY
      INTEGER               POFFST
      INTEGER               PTR
      INTEGER               RIGHT
      INTEGER               RKEY
      INTEGER               RLKEY
      INTEGER               RLSIB
      INTEGER               RNODE
      INTEGER               ROOT
      INTEGER               RPIDX
      INTEGER               RPKEY
      INTEGER               RRKEY
      INTEGER               RRSIB
      INTEGER               RSIB
      INTEGER               STATE
      INTEGER               TRGKEY
      INTEGER               TRUST
 
      LOGICAL               UNDRFL
 
 
C
C     Use discovery check-in for speed.
C
C     Set the variable ROOT, so we'll have something mnemonic to go
C     by when referring to the root node.
C
      ROOT  =  TREE
 
C
C     Work with a local copy of the input key.
C
      LKEY  =  KEY
 
C
C     The first step is to delete the key from the tree without
C     balancing.  This step may cause a node to underflow.  We'll
C     handle the underflow later.
C
      CALL ZZEKTRUD ( HANDLE, TREE, LKEY, TRGKEY, UNDRFL )
 
      IF ( FAILED() ) THEN
         RETURN
      END IF
 
C
C     If the deletion didn't result in an underflow, we're done.
C
      IF ( .NOT. UNDRFL ) THEN
         RETURN
      END IF
 
C
C     Handle node underflows, as required.  We describe our approach
C     below.  If any step fails, we try the next step.  We proceed
C     until we succeed in resolving the underflow.
C
C        1) If an immediate sibling can contribute a key, balance NODE
C           with that sibling.
C
C        2) If both left and right siblings exist, but neither can
C           contribute a key, execute a 3-2 merge.
C
C        3) If the left sibling has its own left sibling, and if that
C           second left sibling can contribute a key, rotate a key
C           from that sibling into NODE's left sibling.  Then execute
C           (1).
C
C        4) If the left sibling has its own left sibling, and if that
C           second left sibling cannot contribute a key, execute a 3-2
C           merge using NODE as the rightmost child.
C
C        5) Same as (3), except on the right side.
C
C        6) Same as (4), except on the right side.
C
C        7) Arrival at this step implies that NODE is a child of the
C           root and has one sibling.  Execute a 3-1 merge.
C
 
      STATE = LCHECK
 
 
      DO WHILE ( STATE .NE. TERM )
 
 
         IF ( STATE .EQ. LCHECK ) THEN
C
C           Look up the node containing the target key TRGKEY.  This
C           is where the underflow occurred; note that this node may
C           be different from the one that contained LKEY.
C
            CALL ZZEKTRLK (  HANDLE,  TREE,    TRGKEY,  IDX,
     .                       NODE,    NOFFST,  LEVEL,   PTR  )
 
C
C           Look up the siblings of NODE.  If either sibling exists
C           and has a surplus of keys, we can remove the underflow
C           by balancing.
C
            CALL ZZEKTRSB (  HANDLE,  TREE,  TRGKEY,
     .                       LSIB,    LKEY,  RSIB,    RKEY  )
 
 
            IF ( LSIB .GT. 0 ) THEN
 
               NKEYS = ZZEKTRNK ( HANDLE, TREE, LSIB )
 
               IF ( NKEYS .GT. MNKEYC ) THEN
C
C                 The left sibling can contribute a key.
C
                  LNKEY  =  LKEY
                  LNODE  =  LSIB
                  RNODE  =  NODE
 
                  STATE  =  BALNCE
 
 
               ELSE IF ( RSIB .GT. 0 ) THEN
C
C                 The left sibling cannot help with balancing, but
C                 the right sibling may be able to.
C
                  STATE  =  RCHECK
 
 
               ELSE
C
C                 The right sibling does not exist; the only chance
C                 of balancing will come from the left sibling of
C                 LSIB, if such a sibling exists.
C
                  STATE  =  LLCHCK
 
               END IF
 
 
            ELSE
C
C              There is no left sibling, so there must be a right
C              sibling.  Examine it.
C
               STATE = RCHECK
 
            END IF
 
 
         ELSE IF ( STATE .EQ. RCHECK ) THEN
C
C           See whether there's a node surplus in the right sibling
C           The left sibling has already been checked and found wanting,
C           or wasn't found at all.
C
            NKEYS = ZZEKTRNK ( HANDLE, TREE, RSIB )
 
            IF ( NKEYS .GT. MNKEYC ) THEN
C
C              The right sibling can contribute a key.
C
               LNKEY  =  TRGKEY
               LNODE  =  NODE
               RNODE  =  RSIB
 
               STATE  =  BALNCE
 
 
            ELSE IF ( LSIB .GT. 0 ) THEN
C
C              NODE has siblings on both sides, and each one contains
C              the minimum number of keys.  Execute a 3-2 merge.
C
               LNKEY   =  LKEY
               LNODE   =  LSIB
               MNODE   =  NODE
               RNODE   =  RSIB
 
               STATE   =  MERG32
 
            ELSE
C
C              Look for the right sibling of the right sibling.
C
               STATE  =  RRCHCK
 
            END IF
 
 
         ELSE IF ( STATE .EQ. LLCHCK ) THEN
C
C           See whether the left sibling has its own left sibling.
C
            CALL ZZEKTRSB (  HANDLE,  TREE,   LKEY,
     .                       LLSIB,   LLKEY,  LRSIB,  LRKEY  )
 
 
            IF ( LLSIB .GT. 0 ) THEN
 
               NKEYS = ZZEKTRNK ( HANDLE, TREE, LLSIB )
 
 
               IF ( NKEYS .GT. MNKEYC ) THEN
C
C                 The left**2 sibling can contribute a key.  Rotate
C                 this key into the left sibling.  We'll need the
C                 parent and index of left parent key of LSIB in order
C                 to do this rotation.
C
                  CALL ZZEKTRPI ( HANDLE,  TREE,    LKEY,   PARENT,
     .                            PKEY,    POFFST,  LPIDX,  LPKEY,
     .                            LLSIB,   RPIDX,   RPKEY,  LRSIB  )
 
                  CALL ZZEKTRRK ( HANDLE,  TREE,    LLSIB,  LSIB,
     .                            PARENT,  LPIDX,   1              )
 
C
C                 Now LSIB has a one-key surplus, so we can balance
C                 LSIB and NODE.
C
                  LNKEY  =  LKEY
                  LNODE  =  LSIB
                  RNODE  =  NODE
 
                  STATE  =  BALNCE
 
 
               ELSE
C
C                 The left**2 sibling contains the minimum allowed
C                 number of keys.  Execute a 3-2 merge, with NODE
C                 as the right node.
C
                  LNKEY   =  LLKEY
                  LNODE   =  LLSIB
                  MNODE   =  LSIB
                  RNODE   =  NODE
 
                  STATE   =  MERG32
 
               END IF
 
 
            ELSE
C
C              LSIB and NODE are the only children of their parent.
C              The parent must be the root.  Also, LSIB and NODE
C              together contain the one less than twice the minimum
C              allowed number of keys.  Execute a 3-1 merge.
C
               LNODE  =  LSIB
               RNODE  =  NODE
 
               STATE  =  MERG31
 
            END IF
 
 
 
         ELSE IF ( STATE .EQ. RRCHCK ) THEN
C
C           See whether the right sibling has its own right sibling.
C
            CALL ZZEKTRSB (  HANDLE,  TREE,   RKEY,
     .                       RLSIB,   RLKEY,  RRSIB,  RRKEY  )
 
 
            IF ( RRSIB .GT. 0 ) THEN
 
               NKEYS = ZZEKTRNK ( HANDLE, TREE, RRSIB )
 
 
               IF ( NKEYS .GT. MNKEYC ) THEN
C
C                 The right**2 sibling can contribute a key.  Rotate
C                 this key into the right sibling.  We'll need the
C                 parent and index of the right parent key of RSIB in
C                 order to do this rotation.
C
                  CALL ZZEKTRPI ( HANDLE,  TREE,    RKEY,   PARENT,
     .                            PKEY,    POFFST,  LPIDX,  LPKEY,
     .                            RLSIB,   RPIDX,   RPKEY,  RRSIB  )
 
                  CALL ZZEKTRRK ( HANDLE,  TREE,    RSIB,   RRSIB,
     .                            PARENT,  RPIDX,   -1             )
 
C
C                 Now RSIB has a one-key surplus, so we can balance
C                 RSIB and NODE.
C
                  LNKEY  =  TRGKEY
                  LNODE  =  NODE
                  RNODE  =  RSIB
 
                  STATE  =  BALNCE
 
 
               ELSE
C
C                 The right**2 sibling contains the minimum allowed
C                 number of keys.  Execute a 3-2 merge, with NODE
C                 as the left node.
C
                  LNKEY   =  TRGKEY
                  LNODE   =  NODE
                  MNODE   =  RSIB
                  RNODE   =  RRSIB
 
                  STATE   =  MERG32
 
               END IF
 
 
            ELSE
C
C              RSIB and NODE are the only children of their parent.
C              The parent must be the root.  Also, RSIB and NODE
C              together contain one less than twice the minimum allowed
C              number of keys.  Execute a 3-1 merge.
C
               LNODE  =  NODE
               RNODE  =  RSIB
 
               STATE  =  MERG31
 
            END IF
 
 
         ELSE IF ( STATE .EQ. BALNCE ) THEN
C
C           LNODE has a right sibling, and between the two nodes,
C           there are enough keys to accommodate the underflow.  After
C           balancing these nodes, we're done.
C
            CALL ZZEKTRPI ( HANDLE,  TREE,    LNKEY,   PARENT,
     .                      PKEY,    POFFST,  LPIDX,   LPKEY,
     .                      RLSIB,   RPIDX,   RPKEY,   RRSIB  )
 
C
C           The common parent of the nodes is PARENT.  The right parent
C           key of the left node is at location RPIDX.  We're ready to
C           balance the nodes.
C
            CALL ZZEKTRBN ( HANDLE, TREE, LNODE, RNODE, PARENT, RPIDX )
 
            STATE  =  TERM
 
 
         ELSE IF ( STATE .EQ. MERG32 ) THEN
C
C           LNODE, MNODE, and RNODE are siblings, and between the three
C           nodes, there's an underflow of one key.  Merge these three
C           nodes into two.  This merging process removes a key from the
C           parent; the parent may underflow as a result.
C
C           After executing the 3-2 merge, to ensure that we reference
C           the parent correctly, we'll obtain a fresh key from the
C           parent.
C
C           To start with, we'll get a trusted key from the
C           leftmost node LNODE.  The first key of LNODE won't be
C           touched by the merge.
C
            CALL ZZEKTRKI ( HANDLE,  TREE,    LNKEY,   1,   TRUST )
 
            CALL ZZEKTRPI ( HANDLE,  TREE,    LNKEY,   PARENT,
     .                      PKEY,    POFFST,  LPIDX,   LPKEY,
     .                      RLSIB,   RPIDX,   RPKEY,   RRSIB  )
 
C
C           The right parent key of the left node is the left parent
C           key of the middle node.  The index of this key is required
C           by ZZEKTR32.
C
            CALL ZZEKTR32 ( HANDLE,  TREE,    LNODE,  MNODE,
     .                      RNODE,   PARENT,  RPIDX,  UNDRFL  )
 
 
            IF ( UNDRFL ) THEN
C
C              We'll need to handle underflow in the parent.
C              The parent should be correctly identified by the
C              parent of TRUST.
C
C              Note that a 3-2 merge can't create an underflow in
C              the parent if the parent is the root:  the parent
C              contains at least one key after this merge.
C
               CALL ZZEKTRPI ( HANDLE, TREE,   TRUST,  PARENT,
     .                         PKEY,   POFFST, LPIDX,  LPKEY,
     .                         LEFT,   RPIDX,  RPKEY,  RIGHT  )
 
               TRGKEY =  PKEY
               STATE  =  LCHECK
 
            ELSE
               STATE  =  TERM
            END IF
 
 
         ELSE IF ( STATE .EQ. MERG31 ) THEN
C
C           We've got an underflow in the two children of the root.
C           Move all of the keys from these children into the root.
C           The root contains the maximum allowed number of keys
C           after this merge.
C
            CALL ZZEKTR31 ( HANDLE, TREE )
            STATE  =  TERM
 
         END IF
 
      END DO
 
      RETURN
      END
