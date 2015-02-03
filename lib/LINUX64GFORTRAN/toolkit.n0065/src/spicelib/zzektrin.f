C$Procedure      ZZEKTRIN ( EK tree, insert value )
 
      SUBROUTINE ZZEKTRIN ( HANDLE, TREE, KEY, VALUE )
 
C$ Abstract
C
C     Insert a value into an EK tree at a specified location.
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
      INTEGER               VALUE
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     TREE       I   Root of tree.
C     KEY        I   Key at which to insert value.
C     VALUE      I   Value to insert.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle of an EK open for write access.
C
C     TREE           is the root node number of the tree of interest.
C
C     KEY            is an absolute key indicating the insertion
C                    location.  In EK trees, absolute keys are just
C                    ordinal positions relative to the leftmost element
C                    of the tree, with the leftmost element having
C                    position 1.  So setting KEY to 10, for example,
C                    indicates that the input VALUE is the 10th item in
C                    the tree.
C
C                    KEY must be in the range 1 : (NKEYS+1), where
C                    NKEYS is the number of keys in the tree prior to
C                    the insertion.
C
C     VALUE          is an integer value to be inserted into the
C                    specified tree at the ordinal position KEY.
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
C     This routine inserts a value into an EK tree at a specified
C     location.  If the location is occupied, the value previously at
C     that location and all higher-indexed values have their indexes
C     incremented.  Since keys are stored in subtree-relative form,
C     the only keys actually modified by the insertion itself are
C     higher-indexed keys in the node into which the insertion is done,
C     and higher-indexed keys in the chain of ancestors of this node.
C
C     The insertion is not the end of the story, however:  it's possible
C     that the node at which the insertion is done (the `target node')
C     will overflow.  If overflow occurs, this routine will restore the
C     tree to its normal form as follows:
C
C        1)  If a neighbor of the target node has room, data will be
C           `rotated' from the target node, through the target's parent,
C            and into the neighbor.  The insertion is complete at this
C            point.
C
C        2)  If no neighbor has room, then the target node and a
C            neighbor are split and recombined into three nodes:  this
C            is called a `2-3 split'.  The parent node is modified
C            appropriately so that all values are in the proper order
C            and all subtree-relative keys are correct.  This 2-3 split
C            increases the number of values in the parent by one.  If
C            the increase does not cause an overflow in the parent, the
C            insertion is complete.
C
C        3)  If the parent overflows as a result of a 2-3 split, the
C            solution process is repeated at the parent's level.  The
C            process iterates until the overflow is resolved or the
C            root overflows.
C
C        4)  If the root overflows, the root is split into two children
C            and a new root node; the new root contains a single value.
C            The children of the old root become children of the two
C            new child nodes of the new root.  This is the only
C            case in which the tree grows taller.
C
C            The process of splitting the root is called a `1-3 split'.
C            After a 1-3 split is performed, the number of values in
C            each node is within bounds.
C
C
C     An EK tree is always balanced after an insertion:  all leaf nodes
C     are at the same level.
C
C$ Examples
C
C     See EKINSR.
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
C-    Beta Version 1.0.0, 01-NOV-1995 (NJB)
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
 
      INTEGER               SPLT23
      PARAMETER           ( SPLT23 = BALNCE + 1 )
 
      INTEGER               SPLT13
      PARAMETER           ( SPLT13 = SPLT23 + 1 )
 
 
C
C     Local variables
C
      INTEGER               IDX
      INTEGER               LEFT
      INTEGER               LEVEL
      INTEGER               LKEY
      INTEGER               LNODE
      INTEGER               LPIDX
      INTEGER               LPKEY
      INTEGER               LVAL
      INTEGER               NKEYS
      INTEGER               NODE
      INTEGER               NOFFST
      INTEGER               NSIZE
      INTEGER               PARENT
      INTEGER               PKEY
      INTEGER               PKIDX
      INTEGER               POFFST
      INTEGER               RIGHT
      INTEGER               RNODE
      INTEGER               ROOT
      INTEGER               RPIDX
      INTEGER               RPKEY
      INTEGER               STATE
      INTEGER               TRUST
 
      LOGICAL               OVERFL
 
 
C
C     Use discovery check-in for speed.
C
C     Set the variable ROOT, so we'll have something mnemonic to go
C     by when referring to the root node.
C
      ROOT  =  TREE
 
C
C     Work with local copies of the input key and value.
C
      LKEY  =  KEY
      LVAL  =  VALUE
 
C
C     The first step is to insert the key into the tree without
C     balancing.  This step may cause a node to overflow.  We'll
C     handle the overflow later.  In general, the probability of
C     overflow is low:  each overflow creates at least one new node,
C     but the ratio of nodes to keys is very small.
C
      CALL ZZEKTRUI ( HANDLE, TREE, LKEY, LVAL, OVERFL )
 
      IF ( FAILED() ) THEN
         RETURN
      END IF
 
C
C     If the insertion didn't result in an overflow, we're done.
C
      IF ( .NOT. OVERFL ) THEN
         RETURN
      END IF
 
C
C     Handle node overflows, as required.
C
      STATE = LCHECK
 
 
      DO WHILE ( STATE .NE. TERM )
 
 
         IF ( STATE .EQ. LCHECK ) THEN
C
C           Look up the node containing LKEY.
C
            CALL ZZEKTRLK (  HANDLE,  TREE,    LKEY,   IDX,
     .                       NODE,    NOFFST,  LEVEL,  LVAL  )
 
            IF ( NODE .EQ. ROOT ) THEN
 
               STATE  =  SPLT13
 
            ELSE
C
C              See if there's room in the left sibling.  Of course,
C              there must be a left sibling in order for there to be
C              room.
C
               CALL ZZEKTRPI ( HANDLE, TREE,   LKEY,   PARENT,
     .                         PKEY,   POFFST, LPIDX,  LPKEY,
     .                         LEFT,   RPIDX,  RPKEY,  RIGHT  )
 
               IF ( LEFT .GT. 0 ) THEN
 
                  NKEYS = ZZEKTRNK ( HANDLE, TREE, LEFT )
 
                  IF ( NKEYS .LT. MXKEYC ) THEN
                     LNODE  =  LEFT
                     RNODE  =  NODE
                     PKIDX  =  LPIDX
                     STATE  =  BALNCE
                  ELSE
                     STATE  =  RCHECK
                  END IF
 
               ELSE
                  STATE = RCHECK
               END IF
 
 
            END IF
 
 
         ELSE IF ( STATE .EQ. RCHECK ) THEN
C
C           See whether there's room in the right sibling, if there
C           is a right sibling.  The left sibling has already been
C           checked and found wanting.
C
            IF ( RIGHT .GT. 0 ) THEN
 
               NKEYS = ZZEKTRNK ( HANDLE, TREE, RIGHT )
 
               IF ( NKEYS .LT. MXKEYC ) THEN
 
                  LNODE  =  NODE
                  RNODE  =  RIGHT
                  PKIDX  =  RPIDX
 
                  STATE  =  BALNCE
 
               ELSE
 
                  LNODE  =  NODE
                  RNODE  =  RIGHT
                  PKIDX  =  RPIDX
                  STATE  =  SPLT23
 
               END IF
 
            ELSE
C
C              The left sibling is full, but at least it's there.
C
               LNODE  =  LEFT
               RNODE  =  NODE
               PKIDX  =  LPIDX
               STATE  =  SPLT23
 
            END IF
 
 
         ELSE IF ( STATE .EQ. BALNCE ) THEN
C
C           LNODE has a right sibling, and between the two nodes,
C           there's enough room to accommodate the overflow.  After
C           balancing these nodes, we're done.
C
            CALL ZZEKTRBN ( HANDLE, TREE, LNODE, RNODE, PARENT, PKIDX )
 
            STATE  =  TERM
 
 
         ELSE IF ( STATE .EQ. SPLT23 ) THEN
C
C           LNODE has a right sibling, and between the two nodes,
C           there's an overflow of one key.  Split these two nodes
C           into three.  This splitting process adds a key to the
C           parent; the parent may overflow as a result.
C
C           After executing the 2-3 split, to ensure that we reference
C           the parent correctly, we'll obtain a fresh key from the
C           parent.  The old key PKEY may not be in the parent any more;
C           this key may have been rotated into the middle node created
C           by the 2-3 split.
C
C           To start with, we'll get a trusted key from the
C           original node NODE.  If NODE got mapped to LNODE,
C           then the first key in NODE will be unchanged by
C           the 2-3 split.  If NODE got mapped to RNODE, then
C           the last key in NODE will be unchanged.
C
            IF ( NODE .EQ. LNODE ) THEN
C
C              Save the first key from NODE.
C
               CALL ZZEKTRKI ( HANDLE, TREE, LKEY, 1, TRUST )
 
            ELSE
C
C              Save the last key from NODE.
C
               NSIZE  =  ZZEKTRNK ( HANDLE, TREE, NODE )
 
               CALL ZZEKTRKI ( HANDLE, TREE, LKEY, NSIZE, TRUST )
 
            END IF
 
 
            CALL ZZEKTR23 ( HANDLE, TREE,  LNODE, RNODE,
     .                      PARENT, PKIDX, OVERFL       )
 
 
            IF ( OVERFL ) THEN
 
               IF ( PARENT .EQ. ROOT ) THEN
 
                  STATE   =  SPLT13
 
               ELSE
C
C                 We'll need to handle overflow in the parent.
C                 The parent should be correctly identified by the
C                 parent of TRUST.
C
                  CALL ZZEKTRPI ( HANDLE, TREE,   TRUST,  PARENT,
     .                            PKEY,   POFFST, LPIDX,  LPKEY,
     .                            LEFT,   RPIDX,  RPKEY,  RIGHT  )
 
                  LKEY    =  PKEY
                  STATE   =  LCHECK
 
               END IF
 
 
            ELSE
               STATE  =  TERM
            END IF
 
 
         ELSE IF ( STATE .EQ. SPLT13 ) THEN
C
C           We've got an overflow in the root.  Split the root,
C           creating two new children.  The root contains a single
C           key after this split.
C
            CALL ZZEKTR13 ( HANDLE, TREE )
            STATE  =  TERM
 
         END IF
 
      END DO
 
      RETURN
      END
