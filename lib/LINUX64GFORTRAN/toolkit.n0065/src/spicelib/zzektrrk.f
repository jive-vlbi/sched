C$Procedure      ZZEKTRRK ( EK tree, rotate keys )
 
      SUBROUTINE ZZEKTRRK ( HANDLE,  TREE,   LEFT,  RIGHT,
     .                      PARENT,  PKIDX,  NROT         )
 
C$ Abstract
C
C     Rotate a specified number of keys from one node, through
C     the parent, into a neighboring sibling node.
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
 
      INTEGER               HANDLE
      INTEGER               TREE
      INTEGER               LEFT
      INTEGER               RIGHT
      INTEGER               PARENT
      INTEGER               PKIDX
      INTEGER               NROT
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     TREE       I   Root of tree.
C     LEFT       I   Left node of pair to participate in rotation.
C     RIGHT      I   Right node of pair to participate in rotation.
C     PARENT     I   Parent node of pair to participate in rotation.
C     PKIDX      I   Parent key index.
C     NROT       I   Number of keys to rotate.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle of an EK open for write access.
C
C     TREE           is the root node number of the tree of interest.
C
C     LEFT,
C     RIGHT          are the node numbers of a pair of nodes to
C                    be balanced.  LEFT and RIGHT must be neighboring
C                    subnodes of a common parent.
C
C     PARENT         is the node number of the common parent node of
C                    nodes LEFT, RIGHT.
C
C     PKIDX          is the `parent key index', that is, the
C                    node-relative index of the key in the parent that
C                    sits between PARENT's child node pointers to
C                    nodes LEFT and RIGHT.  The key at location PKIDX
C                    is the immediate successor of the greatest key in
C                    the subnode headed by LEFT.  It is the immediate
C                    predecessor of the least key in the subnode headed
C                    by RIGHT.
C
C     NROT           is the number of keys to rotate.  Positive counts
C                    indicate that keys are to be rotated from node
C                    LEFT to node RIGHT; negative counts indicate
C                    rotation in the reverse direction.
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
C     3)  If either LEFT or RIGHT are actually the root, the error
C         SPICE(BUG) is signalled.
C
C     4)  If LEFT and RIGHT are not neighboring sibling nodes, the
C         error will be diagnosed by routines called by this routine.
C
C     5)  The rotation is not allowed to create an overflow of more
C         than one key in the destination node, not an underflow of
C         more than one key in the source node.  If either restriction
C         is violated, the error SPICE(BUG) is signalled.
C
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     Insertions into and deletions from EK trees can result in
C     overflows or underflows of keys in nodes affected by these
C     operations.  Many times key count invariants can be restored by
C     moving keys from one node into an adjacent sibling node.  This
C     maneuver is called `balancing' the nodes.  The process of moving
C     keys from one node, through the parent, into a neighboring
C     sibling node is called `rotating' the keys.
C
C     Key rotation affects the parent node of the neighboring children
C     because one key of the parent sits between the children.  This
C     `parent key' gets moved into one of the children as keys are
C     rotated.  If the rotation is to the right, the parent key is the
C     largest key of the rotated set; if the rotation is to the left,
C     the parent key is the least of the rotated set.
C
C     When keys are rotated, their data values move along with them.
C     In general, child pointers move along with keys, but there are
C     some tricky points:
C
C        - The left and right child pointers of the parent key don't
C          get updated; they continue to point to the two children
C          LEFT and RIGHT.
C
C        - On a right rotation, the right child pointer of the key that
C          gets moved into the parent key's original position becomes
C          the first left child pointer of the right sibling.  The left
C          child pointer of this key doesn't get moved at all.
C
C        - On a left rotation, the left child pointer of the key that
C          gets moved into the parent key's original position becomes
C          the last right child pointer of the left sibling.  The right
C          child pointer of this key becomes the left child pointer of
C          the first key of RIGHT.
C
C$ Examples
C
C     See ZZEKTRBN.
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
C-    Beta Version 1.0.0, 16-NOV-1995 (NJB)
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
 
C
C     Local variables
C
      INTEGER               DATBAS
      INTEGER               DPAR
      INTEGER               DROTAT
      INTEGER               DSHIFT
      INTEGER               FUTRPK
      INTEGER               I
      INTEGER               KEYBAS
      INTEGER               KIDBAS
      INTEGER               LNKEYS
      INTEGER               LNSIZE
      INTEGER               LPAGE  ( PGSIZI )
      INTEGER               LSIB
      INTEGER               NVOPAR
      INTEGER               PPAGE  ( PGSIZI )
      INTEGER               REMAIN
      INTEGER               RNKEYS
      INTEGER               ROOT
      INTEGER               RPAGE  ( PGSIZI )
      INTEGER               RSIB
      INTEGER               SCHLEP
      INTEGER               SUBSIZ
 
 
C
C     Use discovery check-in for speed.
C
      IF ( NROT .EQ. 0 ) THEN
         RETURN
      END IF
 
 
      ROOT  =  TREE
 
      IF ( ( LEFT .EQ. ROOT ) .OR. ( RIGHT .EQ. ROOT )  ) THEN
 
         CALL CHKIN  ( 'ZZEKTRRK'                                  )
         CALL SETMSG ( 'Input node is root; only children are '   //
     .                 'eligible for key rotation.'                )
         CALL SIGERR ( 'SPICE(BUG)'                                )
         CALL CHKOUT ( 'ZZEKTRRK'                                  )
 
      END IF
 
C
C     Read in the input nodes.
C
      CALL ZZEKPGRI ( HANDLE, LEFT,   LPAGE )
      CALL ZZEKPGRI ( HANDLE, RIGHT,  RPAGE )
      CALL ZZEKPGRI ( HANDLE, PARENT, PPAGE )
 
      IF ( FAILED() ) THEN
         RETURN
      END IF
 
C
C     Set the base index of the parent keys.  This value depends on
C     whether the parent is the root.  Do the same for the pointer
C     bases.
C
      IF ( PARENT .EQ. TREE ) THEN
         KEYBAS = TRKEYR
         DATBAS = TRDATR
         KIDBAS = TRKIDR
      ELSE
         KEYBAS = TRKEYC
         DATBAS = TRDATC
         KIDBAS = TRKIDC
      END IF
 
C
C     Verify that LEFT and RIGHT are siblings, and that PARENT is
C     their common parent.
C
      LSIB  =  PPAGE ( KIDBAS+PKIDX   )
      RSIB  =  PPAGE ( KIDBAS+PKIDX+1 )
 
      IF (  ( LSIB .NE. LEFT ) .OR. ( RSIB .NE. RIGHT )  ) THEN
 
         CALL CHKIN  ( 'ZZEKTRRK'                               )
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
         CALL CHKOUT ( 'ZZEKTRRK'                               )
         RETURN
 
      END IF
 
C
C     Get the key counts for the left and right nodes.
C
      LNKEYS  =  LPAGE ( TRNKC )
      RNKEYS  =  RPAGE ( TRNKC )
 
C
C     The requested rotation will not be permitted to cause an
C     underflow of more than one key in the source node, nor an
C     overflow of more than one key in the destination node.
C
      IF ( NROT .GT. 0 ) THEN
 
         IF (      ( ( LNKEYS - NROT ) .LT. ( MNKEYC - 1 ) )
     .       .OR.  ( ( RNKEYS + NROT ) .GT. ( MXKEYC + 1 ) ) ) THEN
 
            CALL CHKIN  ( 'ZZEKTRRK'                                   )
            CALL SETMSG ( 'Node # and right sibling # contain # and ' //
     .                    '# keys respectively; rotation of # keys '  //
     .                    'to the right will violate the key count '  //
     .                    'bounds of #:#.'                             )
            CALL ERRINT ( '#',  LEFT                                   )
            CALL ERRINT ( '#',  RIGHT                                  )
            CALL ERRINT ( '#',  LNKEYS                                 )
            CALL ERRINT ( '#',  RNKEYS                                 )
            CALL ERRINT ( '#',  NROT                                   )
            CALL ERRINT ( '#',  MNKEYC-1                               )
            CALL ERRINT ( '#',  MXKEYC+1                               )
            CALL SIGERR ( 'SPICE(BUG)'                                 )
            CALL CHKOUT ( 'ZZEKTRRK'                                   )
            RETURN
 
         END IF
 
 
      ELSE IF ( NROT .LT. 0 ) THEN
 
         IF (      ( ( LNKEYS - NROT ) .GT. ( MXKEYC + 1 ) )
     .       .OR.  ( ( RNKEYS + NROT ) .LT. ( MNKEYC - 1 ) ) ) THEN
 
            CALL CHKIN  ( 'ZZEKTRRK'                                   )
            CALL SETMSG ( 'Node # and right sibling # contain # and ' //
     .                    '# keys respectively; rotation of # keys '  //
     .                    'to the left will violate the key count '   //
     .                    'bounds of #:#.'                             )
            CALL ERRINT ( '#',  LEFT                                   )
            CALL ERRINT ( '#',  RIGHT                                  )
            CALL ERRINT ( '#',  LNKEYS                                 )
            CALL ERRINT ( '#',  RNKEYS                                 )
            CALL ERRINT ( '#',  -NROT                                  )
            CALL ERRINT ( '#',  MNKEYC-1                               )
            CALL ERRINT ( '#',  MXKEYC+1                               )
            CALL SIGERR ( 'SPICE(BUG)'                                 )
            CALL CHKOUT ( 'ZZEKTRRK'                                   )
            RETURN
 
         END IF
 
      END IF
 
C
C     Compute the size of the tree headed by the left subnode.  We'll
C     need this later.  The size of this tree is one less than the
C     difference of the parent key and its predecessor, if any.
C
      IF ( PKIDX .EQ. 1 ) THEN
         LNSIZE  =  PPAGE ( KEYBAS + 1 ) -  1
      ELSE
         LNSIZE  =  PPAGE(KEYBAS+PKIDX)  -  PPAGE(KEYBAS+PKIDX-1)  -  1
      END IF
 
C
C     Now, the actions we take depend on whether we must schlep keys
C     to the right or left.
C
      IF ( NROT .GT. 0 ) THEN
C
C        We'll rotate keys to the right.  There are a bunch of numbers
C        to compute first:
C
C           -- The number of keys remaining in the input node:  REMAIN
C
C           -- The size of the subtree headed by the
C              rotated keys:  SUBSIZ
C
C           -- The offset delta to be applied to the rotated
C              keys:  DROTAT
C
C           -- The offset delta to be applied to the keys shifted
C              right in the sibling:  DSHIFT
C
C           -- The new value of the old right parent key,
C              which gets rotated into the sibling:  NVOPAR
C
C           -- The offset delta to apply to the new right parent key,
C              DPAR.  Note that the successors of this key in the
C              parent node remain unchanged.
C
C
         SCHLEP  =  NROT
         REMAIN  =  LNKEYS  -    SCHLEP
 
C
C        The size of the rotated subtree is the original size of the
C        subtree headed by LEFT, minus the value of the key preceding
C        the rotated subtree.  That key, which resides at location
C        REMAIN + 1, is the future right parent key; this key is also
C        the successor of the subtree left behind.
C
         FUTRPK  =  LPAGE  ( TRKEYC+REMAIN+1 )
         SUBSIZ  =  LNSIZE - FUTRPK
 
C
C        The rotated set of keys will no longer be preceded by the
C        set of keys of size NEWRPK that they originally followed.
C
         DROTAT  =  - FUTRPK
 
C
C        The shifted keys in the right sibling get SUBSIZ + 1 new
C        predecessors.
C
         DSHIFT  =   SUBSIZ + 1
 
C
C        The old right parent key will become the successor of the
C        shifted subtree.  Its value is just one greater than the
C        size of this subtree.
C
         NVOPAR  =   DSHIFT
 
C
C        The new parent key has DSHIFT fewer predecessors after
C        the rotation.
C
         DPAR    =   - DSHIFT
 
C
C        It's time for some action.  First of all, shift the keys
C        in the sibling to the right.  Their data pointers and child
C        pointers move along with them.  Update all the keys by
C        applying the shift delta to them.
C
C        Move the rightmost elements of each data component first.
C        Adjust the keys at the same time.  Note that the regions
C        allocated to keys, data pointers, and child pointers occupy
C        non-overlapping addresses, so the order in which we shift
C        these data sets is not important.  Within each data set, we
C        must be careful not to trash occupied addresses.
C
         DO I = RNKEYS, 1, -1
            RPAGE (TRKEYC + I + SCHLEP)  =  RPAGE (TRKEYC + I) + DSHIFT
         END DO
 
         DO I = RNKEYS, 1, -1
            RPAGE (TRDATC + I + SCHLEP)  =  RPAGE (TRDATC + I)
         END DO
 
         DO I = RNKEYS+1, 1, -1
            RPAGE (TRKIDC + I + SCHLEP)  =  RPAGE (TRKIDC + I)
         END DO
 
C
C        `Move' the old parent key to its target destination in the
C        sibling.  Actually, only the data pointer is copied; the key
C        is simply set to its new value.
C
         RPAGE ( TRKEYC + SCHLEP )  =  NVOPAR
         RPAGE ( TRDATC + SCHLEP )  =  PPAGE ( DATBAS + PKIDX )
 
C
C        `Move' the future parent key to its target destination in the
C        parent.  The data pointer is copied; the key is adjusted by
C        the offset delta we've computed.
C
         PPAGE ( DATBAS + PKIDX )  =  LPAGE ( TRDATC + REMAIN  + 1    )
         PPAGE ( KEYBAS + PKIDX )  =  PPAGE ( KEYBAS + PKIDX ) + DPAR
 
C
C        Rotate the subtree following the future parent key to its
C        destination in the sibling.  Update the keys to account for
C        their new offset.
C
         DO I = 1, SCHLEP-1
            RPAGE(TRKEYC+I) = LPAGE(TRKEYC+REMAIN+1+I) + DROTAT
         END DO
 
         CALL MOVEI( LPAGE(TRDATC+REMAIN+2), SCHLEP-1, RPAGE(TRDATC+1) )
         CALL MOVEI( LPAGE(TRKIDC+REMAIN+2), SCHLEP,   RPAGE(TRKIDC+1) )
 
 
C
C        Update the key counts in both the input node and sibling.
C
         LPAGE ( TRNKC )  =  LPAGE ( TRNKC )  -  SCHLEP
         RPAGE ( TRNKC )  =  RPAGE ( TRNKC )  +  SCHLEP
 
C
C        Update the pages in the kernel.
C
         CALL ZZEKPGWI ( HANDLE, PARENT, PPAGE )
         CALL ZZEKPGWI ( HANDLE, LEFT,   LPAGE )
         CALL ZZEKPGWI ( HANDLE, RIGHT,  RPAGE )
 
 
      ELSE
C
C        Rotation to the left is almost, but not quite, a mirror image
C        of rotation to the right.
C
         SCHLEP  =  - NROT
         REMAIN  =    RNKEYS  -  SCHLEP
 
C
C        The size of the rotated subtree is one less than the value of
C        the future parent key.  This key resides at location
C        SCHLEP and is also the predecessor of the subtree
C        left behind.
C
         FUTRPK  =  RPAGE( TRKEYC + SCHLEP )
         SUBSIZ  =  FUTRPK - 1
 
C
C        The rotated set of keys will be preceded by the keys already
C        present in LEFT, as well as the key moved in from the parent
C        node.
C
         DROTAT  =  LNSIZE + 1
 
C
C        The shifted keys in the right sibling lose SUBSIZ + 1
C        predecessors.
C
         DSHIFT  =  - ( SUBSIZ + 1 )
 
C
C        The old parent key will become the successor of the
C        keys already in LEFT; it will be the predecessor of the
C        rotated subtree.
C
         NVOPAR  =   DROTAT
 
C
C        The new parent key has (-DSHIFT) more predecessors after
C        the rotation.
C
         DPAR    =  - DSHIFT
 
C
C        It's time for some action.
C
C        `Move' the old parent key to its target destination in the
C        input node.  Actually, only the data pointer is copied; the key
C        is simply set to its new value.
C
         LPAGE ( TRKEYC + LNKEYS + 1 )  =  NVOPAR
         LPAGE ( TRDATC + LNKEYS + 1 )  =  PPAGE ( DATBAS + PKIDX )
 
C
C        `Move' the future parent key to its target destination in the
C        parent.  The data pointer is copied; the key is adjusted by
C        the offset delta we've computed.
C
         PPAGE ( DATBAS + PKIDX )  =  RPAGE ( TRDATC + SCHLEP )
         PPAGE ( KEYBAS + PKIDX )  =  PPAGE ( KEYBAS + PKIDX  ) + DPAR
 
C
C        Rotate the subtree following the future parent key to its
C        destination in the sibling.  Update the keys to account for
C        their new offset.
C
         CALL MOVEI( RPAGE(TRKEYC+1), SCHLEP-1, LPAGE(TRKEYC+LNKEYS+2) )
         CALL MOVEI( RPAGE(TRDATC+1), SCHLEP-1, LPAGE(TRDATC+LNKEYS+2) )
         CALL MOVEI( RPAGE(TRKIDC+1), SCHLEP,   LPAGE(TRKIDC+LNKEYS+2) )
 
         DO I = 1, SCHLEP-1
            LPAGE(TRKEYC+LNKEYS+1+I) = LPAGE(TRKEYC+LNKEYS+1+I) + DROTAT
         END DO
 
C
C        Shift the remaining elements of the sibling to the left.
C        Their data pointers and child pointers move along with them.
C        Update all the keys by applying the shift delta to them.
C
C        Move the leftmost elements of each data component first.
C        Adjust the keys at the same time.
C
         DO I = 1, REMAIN
            RPAGE (TRKEYC + I)  =  RPAGE (TRKEYC + I + SCHLEP) + DSHIFT
         END DO
 
         DO I = 1, REMAIN
            RPAGE (TRDATC + I)  =  RPAGE (TRDATC + I + SCHLEP)
         END DO
 
         DO I = 1, REMAIN+1
            RPAGE (TRKIDC + I)  =  RPAGE (TRKIDC + I + SCHLEP)
         END DO
 
C
C        Update the key counts in both the input node and sibling.
C
         LPAGE ( TRNKC )  =  LPAGE ( TRNKC )  +  SCHLEP
         RPAGE ( TRNKC )  =  RPAGE ( TRNKC )  -  SCHLEP
 
C
C        Update the pages in the kernel.
C
         CALL ZZEKPGWI ( HANDLE, PARENT, PPAGE )
         CALL ZZEKPGWI ( HANDLE, LEFT,   LPAGE )
         CALL ZZEKPGWI ( HANDLE, RIGHT,  RPAGE )
 
 
      END IF
 
      RETURN
      END
