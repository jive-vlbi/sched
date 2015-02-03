C$Procedure      ZZEKTRUD ( EK tree, unbalanced deletion )
 
      SUBROUTINE ZZEKTRUD ( HANDLE, TREE, KEY, TRGKEY, UNDRFL )
 
C$ Abstract
C
C     Delete a value from a tree at a specified location without
C     balancing the tree.
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
      INTEGER               TRGKEY
      LOGICAL               UNDRFL
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     TREE       I   Root of tree.
C     KEY        I   Key to delete.
C     TRGKEY     O   Key identifying node from which deletion occurred.
C     UNDRFL     O   Underflow flag.
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
C     TRGKEY         is an absolute key identifying the node from which
C                    the deletion occurred.  This node may be different
C                    from the node that contained KEY before the
C                    deletion; see $Particulars for details.
C
C     UNDRFL         is a logical flag indicating whether the node
C                    at which VALUE was inserted underflowed as a
C                    result.  Child nodes must contain at least
C                    MNKEYC keys; this bound is declared in ektree.inc.
C                    The root node is permitted to become empty.
C
C                    When an underflow condition exists, the tree
C                    violates an invariant.  The underflow must be
C                    resolved before any other insertions or deletions
C                    are performed.
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
C     3)  If the input key is out of range, the error
C         SPICE(INVALIDINDEX) is signalled.
C
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This routine deletes a value from an EK tree at the ordinal
C     position indicated by KEY.  The deletion is always done from a
C     leaf node.  If KEY is not in a leaf node, the value corresponding
C     to KEY is swapped with that of an immediate neighbor, and the
C     neighbor is deleted. This is possible because every key is either
C     in a leaf or has the property that its predecessor and successor
C     are both located in leaf nodes.
C
C     After the deletion, the successor of location from which the
C     deletion actually was done is shifted to the next-lower-indexed
C     position.  The routine updates all affected key counts and key
C     values, both in the target node and all ancestors of the target.
C     Here the target node is the leaf from which the deletion was
C     actually done.
C
C     The caller must balance the tree when underflow occurs.
C
C     Deletion is not quite the opposite of insertion.  Note that the
C     output TRGKEY has no analog in the unbalanced insertion routine
C     ZZEKTRUI.
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
C        Fixed calls to CHKIN and CHKOUT so that the same name
C        is used throught the routine.
C
C-    Beta Version 1.0.0, 20-OCT-1995 (NJB)
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
 
C
C     Local variables
C
      INTEGER               DATPTR
      INTEGER               DEPTH
      INTEGER               I
      INTEGER               KEYIDX
      INTEGER               LEAF
      INTEGER               LEVEL
      INTEGER               LOFFST
      INTEGER               LPAGE  ( PGSIZI )
      INTEGER               LPIDX
      INTEGER               LPIDX2
      INTEGER               LPKEY
      INTEGER               LPKEY2
      INTEGER               LSIB
      INTEGER               LSIB2
      INTEGER               NKEYS
      INTEGER               NLKEYS
      INTEGER               NNODE
      INTEGER               PARENT
      INTEGER               PAREN2
      INTEGER               PKEY
      INTEGER               PKEY2
      INTEGER               POFFS2
      INTEGER               POFFST
      INTEGER               PREV
      INTEGER               ROOT
      INTEGER               RPAGE  ( PGSIZI )
      INTEGER               RPIDX
      INTEGER               RPIDX2
      INTEGER               RPKEY
      INTEGER               RPKEY2
      INTEGER               RSIB
      INTEGER               RSIB2
      INTEGER               TARGET
      INTEGER               TNKEYS
      INTEGER               TOFFST
      INTEGER               TOTKEY
      INTEGER               TPAGE  ( PGSIZI )
      INTEGER               UNIT
 
C
C     Use discovery check-in.
C
C
C     Set the variable ROOT, so we'll have something mnemonic to go
C     by when referring to the root node.
C
      ROOT  =  TREE
 
C
C     We always need to update the root page, so read it now.
C
      CALL ZZEKPGRI ( HANDLE, ROOT, RPAGE )
 
C
C     The allowed range of keys is 1 to TOTKEY, where TOTKEY is the
C     total number of keys already present.
C
      TOTKEY  =  RPAGE ( TRNKEY )
 
      IF (  ( KEY .LT. 1 ) .OR. ( KEY .GT. TOTKEY)  ) THEN
 
         CALL CHKIN  ( 'ZZEKTRUD'                                )
         CALL DASHLU ( HANDLE,  UNIT                             )
         CALL SETMSG ( 'Key = #. Valid range is 1:#.  File = #.' )
         CALL ERRINT ( '#',     KEY                              )
         CALL ERRINT ( '#',     TOTKEY                           )
         CALL ERRFNM ( '#',     UNIT                             )
         CALL CHKOUT ( 'ZZEKTRUD'                                )
         RETURN
 
      END IF
 
C
C     Get the number of nodes in the tree.  Also save the tree's depth.
C
      NNODE  =  RPAGE ( TRNNOD )
      DEPTH  =  RPAGE ( TRDPTH )
 
C
C     Find the point at which the deletion is to occur.  When the
C     tree contains only one node, no search is necessary.
C
      IF ( NNODE .EQ. 1 ) THEN
C
C        This is the simplest case; all we need do is delete the
C        key from the root node.
C
C        Set:
C
C           - The number of keys in the tree
C           - The number of keys in the root
C           - The last key
C           - The data pointer for the last key
C           - The child pointer following the last key
C
C        In the root node, relative keys coincide with absolute keys,
C        so the key value need not be adjusted.
C
         NKEYS             =  TOTKEY
         RPAGE ( TRNKEY )  =  NKEYS  -  1
         RPAGE ( TRNKR  )  =  NKEYS  -  1
 
C
C        Shift the keys, data pointer, and child pointers to the left
C        of the deleted key.  Update the shifted keys.
C
         DO I = KEY, NKEYS-1
            RPAGE ( TRKEYR+I )  =  RPAGE ( TRKEYR+I+1 ) - 1
            RPAGE ( TRDATR+I )  =  RPAGE ( TRDATR+I+1 )
         END DO
 
         DO I = KEY, NKEYS
            RPAGE ( TRKIDR+I )  =  RPAGE ( TRKIDR+I+1 )
         END DO
 
C
C        Zero out the freed entries.
C
         RPAGE ( TRKEYR + NKEYS   )  =  0
         RPAGE ( TRDATR + NKEYS   )  =  0
         RPAGE ( TRKIDR + NKEYS+1 )  =  0
 
C
C        Update the key count.
C
         NKEYS   =  NKEYS - 1
 
C
C        Underflow never occurs in the root; the tree simply becomes
C        empty if no keys are left.
C
         UNDRFL  =  .FALSE.
 
C
C        The first key in the root will serve as the target key,
C        as long as the root isn't empty.
C
         IF ( NKEYS .GT. 0 ) THEN
            TRGKEY  =  RPAGE(TRKEYR+1)
         ELSE
            TRGKEY  =  0
         END IF
 
C
C        Write the page back out, and we're all set.
C
         CALL ZZEKPGWI ( HANDLE, ROOT, RPAGE )
 
 
 
      ELSE IF ( KEY .EQ. TOTKEY ) THEN
C
C        The deleted key is the last key in the tree.  This case
C        is simple, because no remaining keys change as a result of
C        this deletion.
C
         CALL ZZEKTRLK ( HANDLE,  TREE,    KEY,    KEYIDX,
     .                   TARGET,  TOFFST,  LEVEL,  DATPTR  )
 
         IF ( FAILED() ) THEN
            RETURN
         END IF
 
 
         CALL ZZEKPGRI ( HANDLE, TARGET, TPAGE )
 
         NKEYS   =  TPAGE ( TRNKC )
 
C
C        Zero out the freed entries.
C
         TPAGE ( TRKEYC + NKEYS   )  =  0
         TPAGE ( TRDATC + NKEYS   )  =  0
         TPAGE ( TRKIDC + NKEYS+1 )  =  0
 
C
C        Update the key count for this node:
C
         TPAGE ( TRNKC  )  =  TPAGE ( TRNKC ) - 1
 
C
C        Since the key we deleted has no successors, there's no need
C        to adjust any other keys.  We must decrement the total
C        node count in the root, however.
C
         RPAGE ( TRNKEY ) =  TOTKEY - 1
 
C
C        Underflow occurs when the node started out at the minimum
C        key count.
C
         UNDRFL  =  NKEYS .EQ. MNKEYC
 
C
C        The first key in the target page is the target key.  Return
C        an absolute key.
C
         TRGKEY  =  TPAGE(TRKEYC+1)  +  TOFFST
 
C
C        Write the affected pages back out.
C
         CALL ZZEKPGWI ( HANDLE, ROOT,   RPAGE )
         CALL ZZEKPGWI ( HANDLE, TARGET, TPAGE )
 
 
      ELSE
C
C        Locate the item we wish to delete.
C
         CALL ZZEKTRLK ( HANDLE,  TREE,    KEY,    KEYIDX,
     .                   TARGET,  TOFFST,  LEVEL,  DATPTR )
 
 
         IF ( LEVEL .EQ. DEPTH ) THEN
C
C           The node containing KEY is a leaf node, which is what we
C           want.  Deletions always take place at leaf nodes.
C
C           Since we'll have to update the ancestors of TARGET,
C           look up a key in the parent node now.  The order of
C           operations here is delicate; since the deletion
C           we're going to do will temporarily screw up our
C           addressing method, we want to do this look-up while
C           we're sure it will work.
C
            CALL ZZEKTRPI ( HANDLE, TREE,  KEY,  PARENT, PKEY,  POFFST,
     .                      LPIDX,  LPKEY, LSIB, RPIDX,  RPKEY, RSIB   )
 
            IF ( FAILED() ) THEN
               RETURN
            END IF
 
C
C           Read the target page.  Get the key count for this node.
C
            CALL ZZEKPGRI ( HANDLE, TARGET, TPAGE )
 
            TNKEYS = TPAGE ( TRNKC )
 
C
C           Each node is allowed to underflow by 1 element.  If there
C           is already a deficit, OK, that's it.
C
            IF ( TNKEYS .LT. MNKEYC ) THEN
 
               CALL CHKIN  ( 'ZZEKTRUD'                            )
               CALL DASHLU ( HANDLE,  UNIT                         )
               CALL SETMSG ( 'Node = #. Tree = #. File = #. Key ' //
     .                       'count = #; max allowed, including ' //
     .                       'overflow, is #.'                     )
               CALL ERRINT ( '#',   TARGET                         )
               CALL ERRINT ( '#',   TREE                           )
               CALL ERRFNM ( '#',   UNIT                           )
               CALL ERRINT ( '#',   TNKEYS                         )
               CALL ERRINT ( '#',   MXKEYC + 1                     )
               CALL SIGERR ( 'SPICE(BUG)'                          )
               CALL CHKOUT ( 'ZZEKTRUD'                            )
               RETURN
 
            END IF
 
C
C           Shift the keys, data pointers, and child pointers starting
C           at KEY to the left by 1 position.  Careful, move the
C           leftmost elements first.  Update the shifted key values
C           while we're at it.
C
            DO I = KEYIDX, TNKEYS-1
               TPAGE (TRKEYC+I)  =  TPAGE (TRKEYC+I+1) - 1
            END DO
 
            DO I = KEYIDX, TNKEYS-1
               TPAGE (TRDATC+I)  =  TPAGE (TRDATC+I+1)
            END DO
 
            DO I = KEYIDX, TNKEYS
               TPAGE (TRKIDC+I)  =  TPAGE (TRKIDC+I+1)
            END DO
 
C
C           Update the key count for the target node.
C
            TPAGE ( TRNKC ) =  TNKEYS - 1
 
C
C           Underflow occurs when the node started out at the minimum
C           count.
C
            UNDRFL  =  TNKEYS .EQ. MNKEYC
 
C
C           The first key in the target page is the target key.
C
            TRGKEY  =  TPAGE(TRKEYC+1)  +  TOFFST
 
C
C           Write the target page back out.
C
            CALL ZZEKPGWI ( HANDLE, TARGET, TPAGE )
 
 
 
         ELSE
C
C           The node containing KEY is not a leaf node.  Therefore,
C           KEY > 1 and KEY has a predecessor.  This predecessor
C           is guaranteed to reside in a leaf node.  This is simply
C           a property of B*-trees, of which EK trees are a subclass.
C           Find this predecessor.
C
            CALL ZZEKTRLK ( HANDLE,  TREE,    KEY-1,  PREV,
     .                      LEAF,    LOFFST,  LEVEL,  DATPTR  )
 
            IF ( FAILED() ) THEN
               RETURN
            END IF
 
C
C           Since we'll have to update the ancestors of LEAF,
C           look up a key in the parent node now.  The order of
C           operations here is delicate; since the deletion
C           we're going to do will temporarily screw up our
C           addressing method, we want to do this look-up while
C           we're sure it will work.
C
            CALL ZZEKTRPI ( HANDLE, TREE,  KEY-1, PARENT, PKEY,  POFFST,
     .                      LPIDX,  LPKEY, LSIB,  RPIDX,  RPKEY, RSIB  )
 
            IF ( FAILED() ) THEN
               RETURN
            END IF
 
C
C           Since deletions are allowed only in leaf nodes, we'll
C           perform a little sleight-of-code:  We'll move the key's
C           predecessor into the key's location, then remove the
C           predecessor from its leaf node.  The order of the keys
C           is not disturbed by this re-arrangement.
C
C           Moving the key's predecessor into the key's location is
C           accomplished simply by transferring the data pointer.
C
            CALL ZZEKPGRI ( HANDLE, LEAF, LPAGE )
 
 
            IF ( TARGET .EQ. ROOT ) THEN
C
C              The root page has already been read into RPAGE.
C
               RPAGE (TRDATR+KEYIDX)  =  LPAGE (TRDATC+PREV)
 
            ELSE
 
               CALL ZZEKPGRI ( HANDLE, TARGET, TPAGE )
 
               TPAGE (TRDATC+KEYIDX)  =  LPAGE (TRDATC+PREV)
 
            END IF
 
 
C
C           The keys and data pointers in the leaf must be shifted
C           left to account for the deletion.  We'll zero out the
C           freed elements.  All child pointers are NIL and hence need
C           not be shifted.
C
            NLKEYS = LPAGE ( TRNKC )
 
            DO I = PREV, NLKEYS-1
               LPAGE (TRKEYC+I)  =  LPAGE (TRKEYC+I+1) - 1
               LPAGE (TRDATC+I)  =  LPAGE (TRDATC+I+1)
            END DO
 
C
C           Update the key count for the leaf node.
C
            LPAGE ( TRNKC ) =  NLKEYS - 1
 
C
C           Underflow occurs when the leaf node started out at the
C           minimum count.
C
            UNDRFL  =  NLKEYS .EQ. MNKEYC
 
C
C           The first key in the leaf page is the target key.
C
            TRGKEY  =  LPAGE(TRKEYC+1)  +  LOFFST
 
C
C           Write the leaf, and if necessary, the target page back out.
C
            CALL ZZEKPGWI ( HANDLE, LEAF, LPAGE )
 
            IF ( TARGET .NE. ROOT ) THEN
               CALL ZZEKPGWI ( HANDLE, TARGET, TPAGE )
            END IF
 
C
C           The next step will be to update the ancestors of LEAF.
C           For the purposes of this operation, LEAF is the target
C           node.
C
            TARGET  =  LEAF
 
         END IF
 
C
C        We must update the affected keys in every ancestor of TARGET.
C        We've already looked up information for the parent of
C        TARGET.  See the note at the prior call to ZZEKTRPI.
C
         DO WHILE ( PARENT .NE. ROOT )
C
C           Before going to work on the parent, get *its* parent's info.
C           This is the last chance to do so.
C
            CALL ZZEKTRPI ( HANDLE, TREE,   PKEY,   PAREN2,
     .                      PKEY2,  POFFS2, LPIDX2, LPKEY2,
     .                      LSIB2,  RPIDX2, RPKEY2, RSIB2  )
 
C
C           Read the parent node.  All keys from the right parent key
C           onward get decremented.  Remember that there may be no
C           right parent key.
C
            CALL ZZEKPGRI ( HANDLE, PARENT, TPAGE )
 
            TNKEYS  =  TPAGE ( TRNKC  )
 
 
            IF ( RPIDX .GT. 0 ) THEN
 
               DO I = RPIDX, TNKEYS
                  TPAGE(TRKEYC + I)  =  TPAGE(TRKEYC + I) - 1
               END DO
 
C
C              Write the updated page back out.
C
               CALL ZZEKPGWI ( HANDLE, PARENT, TPAGE )
 
            END IF
 
            PARENT = PAREN2
            PKEY   = PKEY2
            RPIDX  = RPIDX2
 
         END DO
 
C
C        Update the keys in the root.  Recall that the root page has
C        already been read into RPAGE.
C
         TNKEYS  =  RPAGE ( TRNKR )
 
 
         IF ( RPIDX .GT. 0 ) THEN
 
            DO I = RPIDX, TNKEYS
               RPAGE(TRKEYR + I)  =  RPAGE(TRKEYR + I) - 1
            END DO
 
         END IF
 
C
C        Update the total key count for the tree.
C
         RPAGE ( TRNKEY )  =  TOTKEY - 1
 
C
C        Write the updated root page back out.
C
         CALL ZZEKPGWI ( HANDLE, ROOT, RPAGE )
 
      END IF
 
 
      RETURN
      END
