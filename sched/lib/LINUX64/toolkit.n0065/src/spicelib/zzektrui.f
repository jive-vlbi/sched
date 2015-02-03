C$Procedure      ZZEKTRUI ( EK tree, unbalanced insertion )
 
      SUBROUTINE ZZEKTRUI ( HANDLE, TREE, KEY, VALUE, OVERFL )
 
C$ Abstract
C
C     Insert a value into a tree at a specified location without
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
      INCLUDE 'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               TREE
      INTEGER               KEY
      INTEGER               VALUE
      LOGICAL               OVERFL
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     TREE       I   Root of tree.
C     KEY        I   Key to insert.
C     VALUE      I   Value to insert.
C     OVERFL     O   Overflow flag.
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
C     OVERFL         is a logical flag indicating whether the node
C                    at which VALUE was inserted overflowed as a result.
C                    Nodes contain extra space to temporarily
C                    accommodate an overflow of one value.
C
C                    When an overflow condition exists, the tree
C                    violates an invariant.  The overflow must be
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
C     4)  If the attempted insertion causes overflow in the target node
C         by more than 1 key, the error SPICE(NODETOOFULL) is signalled.
C
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This routine inserts a new value into an EK tree at the ordinal
C     position indicated by KEY.  The insertion is always done in a
C     leaf node.  This is possible because every key is either in a
C     leaf or has the property that its predecessor and successor are
C     both located in leaf nodes.
C
C     If the inserted value is not appended to the tree, the value
C     previously at location KEY is shifted to the next-higher-indexed
C     position.  The routine updates all affected key counts and key
C     values, both in the target node and all ancestors of the target.
C
C     The caller must balance the tree when overflow occurs.
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
C        Removed redunant calls to CHKIN.
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
      INTEGER               DATLOC
      INTEGER               DATPTR
      INTEGER               DEPTH
      INTEGER               I
      INTEGER               IDX
      INTEGER               KEYLOC
      INTEGER               KIDLOC
      INTEGER               LEVEL
      INTEGER               LPIDX
      INTEGER               LPIDX2
      INTEGER               LPKEY
      INTEGER               LPKEY2
      INTEGER               LSIB
      INTEGER               LSIB2
      INTEGER               NEXT
      INTEGER               NKEYS
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
C     The allowed range of keys is 1 to (TOTKEY+1), where TOTKEY is the
C     total number of keys already present.
C
      TOTKEY  =  RPAGE ( TRNKEY )
 
      IF (  ( KEY .LT. 1 ) .OR. ( KEY .GT. TOTKEY+1 )  ) THEN
 
         CALL CHKIN  ( 'ZZEKTRUI'                                )
         CALL DASHLU ( HANDLE,  UNIT                             )
         CALL SETMSG ( 'Key = #. Valid range is 1:#.  File = #.' )
         CALL ERRINT ( '#',     KEY                              )
         CALL ERRINT ( '#',     TOTKEY+1                         )
         CALL ERRFNM ( '#',     UNIT                             )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                     )
         CALL CHKOUT ( 'ZZEKTRUI'                                )
         RETURN
 
      END IF
 
C
C     Get the number of nodes in the tree.  Also save the tree's depth.
C
      NNODE  =  RPAGE ( TRNNOD )
      DEPTH  =  RPAGE ( TRDPTH )
 
C
C     Find the point at which the insertion is to occur.  When the
C     tree contains only one node, no search is necessary.
C
      IF ( NNODE .EQ. 1 ) THEN
C
C        This is the simplest case; all we need do is set up the
C        key in the root node.
C
C        Set:
C
C           - The number of keys in the tree
C           - The number of keys in the root
C           - The last key
C           - The data value for the last key
C           - The child pointer following the last key
C
C        In the root node, relative keys coincide with absolute keys,
C        so the key value need not be adjusted.
C
         NKEYS             =  TOTKEY
         RPAGE ( TRNKEY )  =  NKEYS  +  1
         RPAGE ( TRNKR  )  =  NKEYS  +  1
 
C
C        Shift the keys, data value, and child pointers to the right
C        of the new key.  Update the shifted keys.
C
         DO I = NKEYS, KEY, - 1
            RPAGE ( TRKEYR+I+1 ) = RPAGE ( TRKEYR + I ) + 1
            RPAGE ( TRDATR+I+1 ) = RPAGE ( TRDATR + I )
         END DO
 
         DO I = NKEYS+1, KEY, - 1
            RPAGE ( TRKIDR+I+1 ) = RPAGE ( TRKIDR + I )
         END DO
 
         RPAGE ( TRKEYR + KEY )  =  KEY
         RPAGE ( TRDATR + KEY )  =  VALUE
         RPAGE ( TRKIDR + KEY )  =  0
 
C
C        Update the key count.
C
         NKEYS   =  NKEYS + 1
 
C
C        The node into which the key was inserted was the root.
C
         TARGET  =  ROOT
 
C
C        Overflow occurs when the root started out full.
C
         OVERFL  =  NKEYS .EQ. MXKEYR+1
 
C
C        Write the page back out, and we're all set.
C
         CALL ZZEKPGWI ( HANDLE, ROOT, RPAGE )
 
 
 
      ELSE IF ( KEY .EQ. TOTKEY+1 ) THEN
C
C        The new key will be the last key in the tree.  This case
C        is simple:  the key goes in the last node of the tree.
C        Since every child node contains more than one key, we can
C        find the node by looking up the last key already present.
C
         CALL ZZEKTRLK ( HANDLE,  TREE,    KEY-1,  IDX,
     .                   TARGET,  TOFFST,  LEVEL,  DATPTR  )
 
         IF ( FAILED() ) THEN
            RETURN
         END IF
 
         CALL ZZEKPGRI ( HANDLE, TARGET, TPAGE )
 
         NKEYS   =  TPAGE ( TRNKC )
 
         KEYLOC  =  TRKEYC  +  NKEYS  +  1
         DATLOC  =  TRDATC  +  NKEYS  +  1
         KIDLOC  =  TRKIDC  +  NKEYS  +  1
 
C
C        The last node in the tree is always at the lowest level,
C        so the relative value of the new key can be computed from
C        that of its predecessor.
C
         TPAGE ( KEYLOC )  =  TPAGE ( KEYLOC-1 )  +  1
         TPAGE ( DATLOC )  =  VALUE
         TPAGE ( KIDLOC )  =  0
 
C
C        Update the key count for this node:
C
         TPAGE ( TRNKC  )  =  TPAGE ( TRNKC ) + 1
 
C
C        Since the key we inserted has no successors, there's no need
C        to adjust any other keys.  We must increment the total
C        node count in the root, however.
C
         RPAGE ( TRNKEY ) =  TOTKEY + 1
 
C
C        Overflow occurs when the node started out full.
C
         OVERFL  =  NKEYS .EQ. MXKEYC
 
C
C        Write the affected pages back out.
C
         CALL ZZEKPGWI ( HANDLE, ROOT,   RPAGE )
         CALL ZZEKPGWI ( HANDLE, TARGET, TPAGE )
 
 
      ELSE
C
C        The item we wish to insert will displace the item whose
C        ordinal position is KEY.  Locate this target item.
C
         CALL ZZEKTRLK ( HANDLE,  TREE,    KEY,    NEXT,
     .                   TARGET,  TOFFST,  LEVEL,  DATPTR  )
 
 
         IF ( LEVEL .EQ. DEPTH ) THEN
C
C           The node containing KEY is a leaf node, which is what we
C           want.  Insertions always take place at leaf nodes.
C
C           Since we'll have to update the ancestors of TARGET,
C           look up a key in the parent node now.  The order of
C           operations here is delicate; since the insertion
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
C           Each node is allowed to overflow by 1 element.  If there's
C           no more room, OK, that's it.
C
            IF ( TNKEYS .GT. MXKEYC ) THEN
 
               CALL CHKIN  ( 'ZZEKTRUI'                            )
               CALL DASHLU ( HANDLE,  UNIT                         )
               CALL SETMSG ( 'Node = #. Tree = #. File = #. Key ' //
     .                       'count = #; max allowed, including ' //
     .                       'overflow, is #.'                     )
               CALL ERRINT ( '#',   TARGET                         )
               CALL ERRINT ( '#',   TREE                           )
               CALL ERRFNM ( '#',   UNIT                           )
               CALL ERRINT ( '#',   TNKEYS                         )
               CALL ERRINT ( '#',   MXKEYC + 1                     )
               CALL SIGERR ( 'SPICE(NODETOOFULL)'                  )
               CALL CHKOUT ( 'ZZEKTRUI'                            )
               RETURN
 
            END IF
 
C
C           Shift the keys, data values, and child pointers starting
C           at NEXT over to the right by 1 position.  Careful, move the
C           rightmost elements first.  Update the shifted key values
C           while we're at it.
C
            DO I = TNKEYS, NEXT, -1
               TPAGE (TRKEYC + I + 1)  =  TPAGE (TRKEYC + I)  +  1
            END DO
 
            DO I = TNKEYS, NEXT, -1
               TPAGE (TRDATC + I + 1)  =  TPAGE (TRDATC + I)
            END DO
 
            DO I = TNKEYS+1, NEXT, -1
               TPAGE (TRKIDC + I + 1)  =  TPAGE (TRKIDC + I)
            END DO
 
C
C           The new key simply takes the value of the old one.  The
C           corresponding data value must be set, however.
C
            TPAGE ( TRDATC + NEXT ) = VALUE
 
 
         ELSE
C
C           The node containing KEY is not a leaf node.  Therefore,
C           KEY > 1 and KEY has a predecessor.  This predecessor
C           is guaranteed to reside in a leaf node.  This is simply
C           a property of B*-trees, of which EK trees are a subclass.
C
            CALL ZZEKTRLK ( HANDLE,  TREE,    KEY-1,  PREV,
     .                      TARGET,  TOFFST,  LEVEL,  DATPTR  )
 
            IF ( FAILED() ) THEN
               RETURN
            END IF
 
C
C           Since we'll have to update the ancestors of TARGET,
C           look up a key in the parent node now.  The order of
C           operations here is delicate; since the insertion
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
C           The predecessor of KEY will be the last key present in the
C           node TARGET.  Make sure there's room in the node.
C
            CALL ZZEKPGRI ( HANDLE, TARGET, TPAGE )
 
            TNKEYS = TPAGE ( TRNKC )
 
 
            IF ( TNKEYS .GT. MXKEYC + 1 ) THEN
 
               CALL CHKIN  ( 'ZZEKTRUI'                            )
               CALL DASHLU ( HANDLE,  UNIT                         )
               CALL SETMSG ( 'Node = #. Tree = #. File = #. Key ' //
     .                       'count = #; max allowed, including ' //
     .                       'overflow, is #.'                     )
               CALL ERRINT ( '#',   TARGET                         )
               CALL ERRINT ( '#',   TREE                           )
               CALL ERRFNM ( '#',   UNIT                           )
               CALL ERRINT ( '#',   TNKEYS                         )
               CALL ERRINT ( '#',   MXKEYC + 1                     )
               CALL SIGERR ( 'SPICE(NODETOOFULL)'                  )
               CALL CHKOUT ( 'ZZEKTRUI'                            )
               RETURN
 
            END IF
 
C
C           Set the new key and the corresponding data and child
C           pointers.
C
            TPAGE ( TRKEYC + PREV + 1 )   =   PREV + 1
            TPAGE ( TRDATC + PREV + 1   ) =   VALUE
            TPAGE ( TRKIDC + PREV + 2   ) =   0
 
         END IF
 
C
C        Update the key count for the target node.
C
         TPAGE ( TRNKC ) =  TNKEYS + 1
 
C
C        Overflow occurs when the node started out full.
C
         OVERFL  =  TNKEYS .EQ. MXKEYC
 
C
C        Write the target page back out.
C
         CALL ZZEKPGWI ( HANDLE, TARGET, TPAGE )
 
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
C           onward get incremented.  Remember that there may be no
C           right parent key.
C
            CALL ZZEKPGRI ( HANDLE, PARENT, TPAGE )
 
            TNKEYS  =  TPAGE ( TRNKC  )
 
 
            IF ( RPIDX .GT. 0 ) THEN
 
               DO I = RPIDX, TNKEYS
                  TPAGE(TRKEYC + I)  =  TPAGE(TRKEYC + I) + 1
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
               RPAGE(TRKEYR + I)  =  RPAGE(TRKEYR + I) + 1
            END DO
 
         END IF
 
C
C        Update the total key count for the tree.
C
         RPAGE ( TRNKEY )  =  TOTKEY + 1
 
C
C        Write the updated root page back out.
C
         CALL ZZEKPGWI ( HANDLE, ROOT, RPAGE )
 
      END IF
 
 
      RETURN
      END
