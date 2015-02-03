C$Procedure      ZZEKTR1S ( EK tree, one-shot load )
 
      SUBROUTINE ZZEKTR1S ( HANDLE, TREE, SIZE, VALUES )
 
C$ Abstract
C
C     One-shot tree load:  insert an entire array into an empty
C     tree.
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
      INCLUDE 'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               TREE
      INTEGER               SIZE
      INTEGER               VALUES  ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     TREE       I   Root of tree.
C     SIZE       I   Size of tree.
C     VALUES     I   Values to insert.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle of an EK open for write access.
C
C     TREE           is the root node number of the tree of interest.
C                    The tree must be empty.
C
C     SIZE           is the size of the tree to create:  SIZE is the
C                    number of values that will be inserted into the
C                    tree.
C
C     VALUES         is an array of integer values to be inserted into
C                    the tree.
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
C     3)  If the input tree is not empty, the error SPICE(NONEMPTYTREE)
C         is signalled.
C
C     4)  If the depth of the tree needed to hold the number of values
C         indicated by SIZE exceeds the maximum depth limit, the error
C         SPICE(COUNTTOOLARGE) is signalled.
C
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This routine creates an EK tree and loads the tree with the
C     integer values supplied in the array VALUES.  The ordinal
C     positions of the values in the tree correspond to the positions
C     of the values in the input array:  for example, the 10th element
C     of the array is pointed to by the key 10.
C
C     This routine loads a tree much faster than can be done by
C     sequentially loading the set of values by successive calls to
C     ZZEKTRIN.  On the other hand, the caller must declare an array
C     large enough to hold all of the values to be loaded.  Note that
C     a partially full tree cannot be extended using this routine.
C
C$ Examples
C
C     See EKFFLD.
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
C-    Beta Version 1.1.0, 18-JUN-1999 (WLT)
C
C        Removed redundant calls to CHKIN
C
C-    Beta Version 1.0.0, 22-OCT-1995 (NJB)
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Non-SPICELIB functions
C
      INTEGER               ZZEKTRBS
      INTEGER               ZZEKTRSZ
 
C
C     Local variables
C
      INTEGER               BASE
      INTEGER               BASIDX
      INTEGER               BIGSIZ
      INTEGER               CHILD
      INTEGER               D
      INTEGER               DIV
      INTEGER               I
      INTEGER               KEY
      INTEGER               KIDBAS
      INTEGER               LEVEL
      INTEGER               MAXSIZ
      INTEGER               N
      INTEGER               NBIG
      INTEGER               NEXT
      INTEGER               NKEYS
      INTEGER               NKIDS
      INTEGER               NNODES
      INTEGER               NODE
      INTEGER               NPRED
      INTEGER               NSMALL
      INTEGER               PAGE  ( PGSIZI )
      INTEGER               Q
      INTEGER               REQSIZ
      INTEGER               S
      INTEGER               STLSIZ ( TRMXDP )
      INTEGER               STNBIG ( TRMXDP )
      INTEGER               STNEXT ( TRMXDP )
      INTEGER               STNKEY ( TRMXDP )
      INTEGER               STNODE ( TRMXDP )
      INTEGER               STSBSZ ( TRMXDP )
      INTEGER               STNBAS ( TRMXDP )
      INTEGER               SUBD
      INTEGER               SUBSIZ
      INTEGER               TOTNOD
      INTEGER               TSIZE
      INTEGER               UNIT
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKTR1S' )
      END IF
 
C
C     Make sure the input tree is empty.
C
      TSIZE  =  ZZEKTRSZ ( HANDLE, TREE )
 
      IF ( TSIZE .GT. 0 ) THEN
 
         CALL DASHLU (  HANDLE,  UNIT  )
 
         CALL SETMSG ( 'Tree has size #; should be empty.' //
     .                 'EK = #; TREE = #.'                 )
         CALL ERRINT ( '#',  TSIZE                         )
         CALL ERRFNM ( '#',  UNIT                          )
         CALL ERRINT ( '#',  TREE                          )
         CALL SIGERR ( 'SPICE(NONEMPTYTREE)'               )
         CALL CHKOUT ( 'ZZEKTR1S'                          )
         RETURN
 
      END IF
 
C
C     Compute the tree depth required.  The largest tree of a given
C     depth D contains the root node plus S(D) child nodes, where
C
C
C        S(1)  =  1
C
C
C     and if D is at least 2,
C                                    D - 2
C                                    ____
C                                    \              i
C        S(D)  =  MAX_SIZE      *    /      MAX_SIZE
C                         Root       ----           Child
C                                    i = 0
C
C
C                                    D - 2
C                                    ____
C                                    \            i
C              =  MXKIDR        *    /      MXKIDC
C                                    ----
C                                    i = 0
C
C
C                                         D-1
C                                   MXKIDC   -  1
C              =  MXKIDR        *   -------------
C                                    MXKIDC  - 1
C
C
C     If all of these nodes are full, the number of keys that
C     can be held in this tree is
C
C        MXKEYR  +  S(D) * MXKEYC
C
C     We want the minimum value of D such that this expression
C     is greater than or equal to SIZE.
C
 
      TSIZE  =  MXKEYR
      D      =  1
      S      =  1
 
      DO WHILE ( TSIZE .LT. SIZE )
 
         D  =  D  +  1
 
         IF ( D .EQ. 2 ) THEN
 
            S  =  MXKEYR
 
         ELSE
C
C           For computational purposes, the relationship
C
C              S(D+1)  =   MXKIDR  +  MXKIDC * S(D)
C
C           is handy.
C
C
            S  =  MXKIDR  +  MXKIDC * S
 
         END IF
 
         TSIZE   =   MXKEYR  +  S * MXKEYC
 
      END DO
 
C
C     If the tree must be deeper than we expected, we've a problem.
C
      IF ( D .GT. TRMXDP ) THEN
 
         CALL DASHLU (  HANDLE,  UNIT  )
 
         CALL SETMSG ( 'Tree has depth #; max supported depth is #.' //
     .                 'EK = #; TREE = #.'                           )
         CALL ERRINT ( '#',  D                                       )
         CALL ERRINT ( '#',  TRMXDP                                  )
         CALL ERRFNM ( '#',  UNIT                                    )
         CALL ERRINT ( '#',  TREE                                    )
         CALL SIGERR ( 'SPICE(COUNTTOOLARGE)'                        )
         CALL CHKOUT ( 'ZZEKTR1S'                                    )
         RETURN
 
      END IF
 
C
C     The basic error checks are done.  At this point, we can build the
C     tree.
C
C     The approach is to fill in the tree in a top-down fashion.
C     We decide how big each subtree of the root will be; this
C     information allows us to decide which keys actually belong
C     in the root.  Having filled in the root, we repeat the process
C     for each subtree of the root in left-to-right order.
C
C     We use a stack to keep track of the ancestors of the
C     node we're currently considering.  The table below shows the
C     items we save on the stack and the stack variables associated
C     with those items:
C
C
C        Item                                 Stack Variable
C        ----                                 ---------------
C        Node number                          STNODE
C
C        Size, in keys, of the
C        subtree headed by node               STSBSZ
C
C        Number of keys in node               STNKEY
C
C        Larger subtree size                  STLSIZ
C
C        Number of large subtrees             STNBIG
C
C        Index of next subtree to visit       STNEXT
C
C        Base index of node                   STNBAS
C
C
      NODE              =  TREE
      SUBSIZ            =  SIZE
      NEXT              =  1
      LEVEL             =  1
      BASIDX            =  0
 
      DO WHILE ( LEVEL .GT. 0 )
C
C        At this point, LEVEL, NEXT, NODE, SUBSIZ and BASIDX are set.
C
 
         IF ( NEXT .EQ. 1 ) THEN
C
C           This node has not been visited yet.  We'll fill in this
C           node before proceeding to fill in its descendants.  The
C           first step is to compute the number and sizes of the
C           subtrees of this node.
C
C           Decide the large subtree size and the number of subtrees of
C           this node.  The depth SUBD of the subtrees of this node is
C           D - LEVEL.  Each subtree has size bounded by the sizes of
C           the subtree of depth SUBD in which all nodes contain MNKEYC
C           keys and the by the subtree of depth SUBD in which all nodes
C           contain MXKEYC keys.  If this node is not the root and is
C           not a leaf node, the number of subtrees must be between
C           MNKIDC and MXKIDC.
C
            IF ( LEVEL .EQ. 1 ) THEN
C
C              We're working on the root.  The number of subtrees is
C              anywhere between 0 and MXKIDR, inclusive.  We'll create
C              a tree with the minimum number of subtrees of the root.
C
               IF ( D .GT. 1 ) THEN
C
C                 We'll find the number of subtrees of maximum size
C                 that we would need to hold the non-root keys of the
C                 tree.  We'll then determine the actual required sizes
C                 of these subtrees.
C
                  SUBD    =  D - 1
 
                  NNODES  =  0
 
                  DO I = 1, SUBD
                     NNODES =  MXKIDC * NNODES  +  1
                  END DO
 
                  MAXSIZ  =  NNODES * MXKEYC
 
C
C                 If we had NKIDS subtrees of size MAXSIZ, NKIDS
C                 would be the smallest integer such that
C
C                    ( NKIDS - 1 )  +  NKIDS * MAXSIZ  >  SUBSIZ
C                                                      -
C
C                 or equivalently,
C
C                     NKIDS * ( MAXSIZ + 1 )  >  SUBSIZ + 1
C                                             -
C
C                 We'll compute this value of NKIDS.
C
C
                  Q       =  SUBSIZ + 1
                  DIV     =  MAXSIZ + 1
                  NKIDS  =  ( Q + DIV - 1 )  / DIV
 
C
C                 The minimum number of keys we must store in child
C                 nodes is the number of keys in the tree, minus those
C                 that can be accommodated in the root:
C
                  N       =   SUBSIZ - ( NKIDS - 1 )
 
C
C                 Now we can figure out how large the subtrees would
C                 have to be in order to hold N keys, if all subtrees
C                 had the same size.
C
                  BIGSIZ  =  ( N + NKIDS - 1 ) / NKIDS
 
C
C                 We may have more capacity than we need if all subtrees
C                 have size BIGSIZ.  So, we'll allow some subtrees to
C                 have size BIGSIZ-1.  Not all subtrees can have the
C                 smaller size (otherwise BIGSIZ would have been
C                 smaller).  The first NBIG subtrees will have the
C                 larger size.
C
                  NSMALL  =  ( NKIDS * BIGSIZ ) - N
                  NBIG    =    NKIDS - NSMALL
 
                  NKEYS   =  NKIDS  -  1
 
 
               ELSE
C
C                 All keys are in the root.
C
                  NKEYS   =  SIZE
                  NKIDS   =  0
 
               END IF
 
C
C              Read in the root page.
C
               CALL ZZEKPGRI ( HANDLE, TREE, PAGE )
 
C
C              We have enough information to fill in the root node.
C              We'll allocate nodes for the immediate children.
C              There is one key `between' each child pointer.
C
               DO I = 1, NKEYS
C
C                 The Ith key may be found by considering the number
C                 of keys in the subtree between the Ith key and its
C                 predecessor in the root.
C
                  IF ( I .EQ. 1 ) THEN
                     NPRED = 0
                  ELSE
                     NPRED = PAGE( TRKEYR+I-1 )
                  END IF
 
                  IF ( D .GT. 1 ) THEN
C
C                    The tree contains subtrees.
C
                     IF ( I .LE. NBIG ) THEN
                        KEY   =  NPRED  + BIGSIZ + 1
                     ELSE
                        KEY   =  NPRED  + BIGSIZ
                     END IF
 
                  ELSE
 
                     KEY  =  I
 
                  END IF
 
                  PAGE ( TRKEYR + I ) =  KEY
                  PAGE ( TRDATR + I ) =  VALUES ( KEY )
 
               END DO
 
 
               TOTNOD  =  1
 
               DO I = 1, NKIDS
C
C                 Allocate a node for the Ith child.  Store pointers
C                 to these nodes.
C
                  CALL ZZEKPGAL ( HANDLE, INT, CHILD, BASE )
                  PAGE ( TRKIDR + I )  =  CHILD
                  TOTNOD               =  TOTNOD + 1
 
               END DO
 
C
C              Fill in the root's metadata.  There is one item that
C              we'll have to fill in when we're done:  the number of
C              nodes in the tree.  We know the rest of the information
C              now.
C
               PAGE ( TRNKEY )  =  SIZE
               PAGE ( TRDPTH )  =  D
               PAGE ( TRKEYR )  =  NKEYS
               PAGE ( TRNNOD )  =  0
 
C
C              Write out the root.
C
               CALL ZZEKPGWI ( HANDLE, TREE, PAGE )
 
 
            ELSE IF ( LEVEL .LT. D ) THEN
C
C              The current node is a non-leaf child node.
C
               CALL CLEARI ( PGSIZI, PAGE )
C
C              The tree headed by this node has depth D-LEVEL+1 and
C              must hold SUBSIZ keys.  We must figure out the size
C              and number of subtrees of the current node.  Unlike in
C              the case of the root, we must have between MNKIDC
C              and MXKIDC subtrees of this node.  We start out by
C              computing the required subtree size if there were
C              exactly MNKIDC subtrees.  In this case, the total
C              number of keys in the subtrees would be
C
C                 SUBSIZ  -  MNKEYC
C
C
               N       =  SUBSIZ - MNKEYC
               REQSIZ  =  ( N + MNKEYC - 1 )  /  MNKEYC
 
C
C              Compute the maximum allowable number of keys in
C              a subtree.
C
               SUBD    =  D - LEVEL
 
               NNODES  =  0
 
               DO I = 1, SUBD
                  NNODES =  MXKIDC * NNODES  +  1
               END DO
 
               MAXSIZ  =  NNODES * MXKEYC
 
C
C              If the number REQSIZ we came up with is a valid size,
C              we'll be able to get the correct number of children
C              by using subtrees of size REQSIZ and REQSIZ-1.  Note
C              that it's impossible for REQSIZ to be too small,
C              since the smallest possible number of subtrees is
C              MNKIDC.
C
               IF ( REQSIZ .LE. MAXSIZ ) THEN
C
C                 Decide how many large and small subtrees we need.
C
                  NKIDS   =  MNKIDC
                  BIGSIZ  =  REQSIZ
                  NSMALL  =  BIGSIZ * NKIDS  -  N
                  NBIG    =  NKIDS  -  NSMALL
 
               ELSE
C
C
C                 See how many subtrees of size MAXSIZ it would take
C                 to hold the requisite number of keys.  We know the
C                 number is more than MNKIDC.  If we have NKIDS
C                 subtrees of size MAXSIZ, the total number of
C                 keys in the subtree headed by NODE is
C
C                   ( NKIDS - 1 )  +  ( NKIDS * MAXSIZ )
C
C                 or
C
C                     NKIDS * ( MAXSIZ + 1 )   -   1
C
C                 We must find the smallest value of NKIDS such
C                 that the above quantity is greater than or equal
C                 to SUBSIZ.
C
                  Q      =  SUBSIZ + 1
                  DIV    =  MAXSIZ + 1
                  NKIDS  =  ( Q + DIV - 1 )  / DIV
 
C
C                 We know that NKIDS subtrees of size MAXSIZ, plus
C                 NKIDS-1 keys in NODE, can hold at least SUBSIZ
C                 keys.  We now want to find the smallest subtree
C                 size such that NKIDS subtrees of that size,
C                 together with the NKIDS-1 keys in NODE, contain
C                 at least SUBSIZ keys.  The size we seek will
C                 become BIGSIZ, the larger of the two subtree
C                 sizes we'll use.  So BIGSIZ is the smallest
C                 integer such that
C
C                    ( NKIDS - 1 ) + ( NKIDS * BIGSIZ )  >  SUBSIZ
C                                                        -
C
C                 or equivalently
C
C                    BIGSIZ * NKIDS  >  SUBSIZ - NKIDS + 1
C                                    -
C
                  Q       =  SUBSIZ - NKIDS + 1
                  DIV     =  NKIDS
                  BIGSIZ  =  ( Q + DIV - 1 )  / DIV
 
                  NSMALL  =  BIGSIZ * NKIDS  -  Q
                  NBIG    =  NKIDS  -  NSMALL
 
               END IF
 
C
C              Fill in the keys for the current node.
C
               NKEYS   =  NKIDS  -  1
 
               DO I = 1, NKEYS
C
C                 The Ith key may be found by considering the number
C                 of keys in the subtree between the Ith key and its
C                 predecessor in the current node.
C
                  IF ( I .EQ. 1 ) THEN
                     NPRED = BASIDX
                  ELSE
                     NPRED = BASIDX + PAGE( TRKEYC+I-1 )
                  END IF
 
                  IF ( I .LE. NBIG ) THEN
                     KEY   =  NPRED  + BIGSIZ + 1
                  ELSE
                     KEY   =  NPRED  + BIGSIZ
                  END IF
 
                  PAGE ( TRKEYC + I ) =  KEY - BASIDX
                  PAGE ( TRDATC + I ) =  VALUES ( KEY )
 
               END DO
 
 
               DO I = 1, NKIDS
C
C                 Allocate a node for the Ith child.  Store pointers
C                 to these nodes.
C
                  CALL ZZEKPGAL ( HANDLE, INT, CHILD, BASE )
 
                  PAGE ( TRKIDC + I )  =  CHILD
                  TOTNOD               =  TOTNOD + 1
 
               END DO
 
C
C              We can now fill in the metadata for the current node.
C
               PAGE ( TRNKC )  =  NKEYS
 
               CALL ZZEKPGWI ( HANDLE, NODE, PAGE )
 
            END IF
 
C
C           Unless the current node is a leaf node, prepare to visit
C           the first child of the current node.
C
            IF ( LEVEL .LT. D ) THEN
C
C              Push our current state.
C
               STNODE ( LEVEL )  = NODE
               STSBSZ ( LEVEL )  = SUBSIZ
               STNKEY ( LEVEL )  = NKEYS
               STLSIZ ( LEVEL )  = BIGSIZ
               STNBIG ( LEVEL )  = NBIG
               STNEXT ( LEVEL )  = 2
               STNBAS ( LEVEL )  = BASIDX
 
C
C              NEXT is already set to 1.  BASIDX is set, since the
C              base index of the first child is that of the parent.
C
               IF ( LEVEL .EQ. 1 ) THEN
                  KIDBAS  =  TRKIDR
               ELSE
                  KIDBAS  =  TRKIDC
               END IF
 
               LEVEL   =  LEVEL + 1
               NODE    =  PAGE ( KIDBAS+1 )
               SUBSIZ  =  BIGSIZ
 
 
            ELSE IF ( LEVEL .GT. 1 ) THEN
C
C              The current node is a child leaf node.  There are no
C              calculations to do; we simply assign keys and pointers,
C              write out metadata, and pop our state.
C
               NKEYS  =  SUBSIZ
 
               DO I = 1, NKEYS
 
                  KEY                 =  BASIDX + I
                  PAGE ( TRKEYC + I ) =  I
                  PAGE ( TRDATC + I ) =  VALUES ( KEY )
 
               END DO
 
C
C              We can now fill in the metadata for the current node.
C
               PAGE ( TRNKC )  =  NKEYS
 
               CALL ZZEKPGWI ( HANDLE, NODE, PAGE )
 
C
C              A leaf node is a subtree unto itself, and we're
C              done with this subtree.  Pop our state.
C
               LEVEL  =  LEVEL - 1
 
               IF ( LEVEL .GE. 1 ) THEN
 
                  NODE    =  STNODE ( LEVEL )
                  NKEYS   =  STNKEY ( LEVEL )
                  BIGSIZ  =  STLSIZ ( LEVEL )
                  NBIG    =  STNBIG ( LEVEL )
                  NEXT    =  STNEXT ( LEVEL )
                  BASIDX  =  STNBAS ( LEVEL )
                  NKIDS   =  NKEYS + 1
 
C
C                 Read in the current node.
C
                  CALL ZZEKPGRI ( HANDLE, NODE, PAGE )
 
               END IF
 
 
            ELSE
C
C              The only node is the root.  Pop out.
C
               LEVEL  =  0
 
            END IF
C
C           We've decided which node to go to next at this point.
C           At this point, LEVEL, NEXT, NODE, SUBSIZ and BASIDX are set.
C
 
         ELSE
C
C           The current node has been visited already.  Visit the
C           next child, if there is one.
C
            IF ( NEXT .LE. NKIDS ) THEN
C
C              Prepare to visit the next child of the current node.
C
               STNEXT ( LEVEL )   =   NEXT  +  1
 
 
               IF ( LEVEL .EQ. 1 ) THEN
                  KIDBAS  =  TRKIDR
               ELSE
                  KIDBAS  =  TRKIDC
               END IF
 
 
               NODE    =  PAGE ( KIDBAS + NEXT )
 
 
               IF ( NEXT .LE. NBIG ) THEN
                  SUBSIZ  =  STLSIZ(LEVEL)
               ELSE
                  SUBSIZ  =  STLSIZ(LEVEL) - 1
               END IF
 
 
               IF ( NEXT .LE. NBIG+1 ) THEN
 
                  BASIDX  =      STNBAS(LEVEL)
     .                       + ( NEXT-1 ) * STLSIZ(LEVEL)
     .                       + ( NEXT-1 )
 
               ELSE
 
                  BASIDX  =      STNBAS(LEVEL)
     .                       +   NBIG           *   STLSIZ(LEVEL)
     .                       + ( NEXT-NBIG-1 )  * ( STLSIZ(LEVEL) - 1 )
     .                       + ( NEXT-1 )
               END IF
 
               LEVEL  =  LEVEL + 1
               NEXT   =  1
 
C
C              LEVEL, NEXT, NODE, SUBSIZ, and BASIDX are set.
C
 
            ELSE
C
C              We're done with the current subtree.  Pop the stack.
C
               LEVEL   =  LEVEL - 1
 
               IF ( LEVEL .GE. 1 ) THEN
 
                  NODE    =  STNODE ( LEVEL )
                  NKEYS   =  STNKEY ( LEVEL )
                  BIGSIZ  =  STLSIZ ( LEVEL )
                  NBIG    =  STNBIG ( LEVEL )
                  NEXT    =  STNEXT ( LEVEL )
                  BASIDX  =  STNBAS ( LEVEL )
                  NKIDS   =  NKEYS + 1
C
C                 Read in the current node.
C
                  CALL ZZEKPGRI ( HANDLE, NODE, PAGE )
 
               END IF
 
            END IF
 
 
         END IF
 
C
C        On this pass through the loop, we either---
C
C           - Visited a node for the first time and filled in the
C             node.
C
C           - Advanced to a new node that has not yet been visited.
C
C           - Exited from a completed subtree.
C
C        Each of these actions can be performed a finite number of
C        times.  Therefore, we made progress toward loop termination.
C
      END DO
 
 
C
C     The last chore is setting the total number of nodes in the root.
C
      BASE  = ZZEKTRBS ( TREE )
 
      CALL DASUDI ( HANDLE, BASE+TRNNOD, BASE+TRNNOD, TOTNOD )
 
 
      CALL CHKOUT ( 'ZZEKTR1S' )
      RETURN
      END
