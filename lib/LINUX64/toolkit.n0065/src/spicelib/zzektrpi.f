C$Procedure      ZZEKTRPI ( EK tree, parent information )
 
      SUBROUTINE ZZEKTRPI ( HANDLE, TREE,  KEY,  PARENT, PKEY,  POFFST,
     .                      LPIDX,  LPKEY, LSIB, RPIDX,  RPKEY, RSIB   )
 
C$ Abstract
C
C     Given a key, return general information pertaining to the key's
C     parent node.
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
      INTEGER               PARENT
      INTEGER               PKEY
      INTEGER               POFFST
      INTEGER               LPIDX
      INTEGER               LPKEY
      INTEGER               LSIB
      INTEGER               RPIDX
      INTEGER               RPKEY
      INTEGER               RSIB
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     TREE       I   Root of tree.
C     KEY        I   Key belonging to node of interest.
C     PARENT     O   Parent node of the node containing KEY.
C     PKEY       O   A key in the parent node.
C     POFFST     O   Key offset of the parent node.
C     LPIDX      O   Node-relative index of the left parent key.
C     LPKEY      O   Left parent key.
C     LSIB       O   Node number of left sibling.
C     RPIDX      O   Node-relative index of the right parent key.
C     RPKEY      O   Right parent key.
C     RSIB       O   Node number of right sibling.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle of an EK open for read or write
C                    access.
C
C     TREE           is the root node number of the tree of interest.
C
C     NODE           is the node number of interest.
C
C$ Detailed_Output
C
C     PARENT         is the number of the parent node of the node
C                    containing KEY.  If KEY is in the root, PARENT is
C                    set to zero.
C
C     PKEY           is a key in PARENT.  If PARENT is set to zero,
C                    PKEY is set to zero as well.  PKEY is used to
C                    traverse a chain of ancestors towards the to root.
C
C     POFFST         is the key offset of PARENT; this is the offset
C                    that must be added to the node-relative key
C                    values in PARENT to convert them to absolute keys.
C
C     LPIDX          is the index in PARENT of the key `to the left'
C                    of the node containing KEY.  This key is the
C                    immediate predecessor of the first key in the
C                    subtree headed by the node containing KEY.
C
C                    The key indices in PARENT start at 1.  If PARENT
C                    contains no keys that precede the node containing
C                    KEY, LPIDX is set to zero.
C
C     LPKEY          is the absolute key located in PARENT at index
C                    LPIDX.  If PARENT contains no keys that precede the
C                    node containing KEY, LPKEY is set to zero.
C
C     LSIB           is the number of the left sibling node of the node
C                    containing KEY.  If PARENT contains no keys that
C                    precede the node containing KEY, then the node
C                    containing KEY has no left sibling, and LSIB is
C                    set to zero.
C
C     RPIDX          is the index in PARENT of the key `to the right'
C                    of the node containing KEY.  This key is the
C                    immediate successor of the last key in the
C                    subtree headed by the node containing KEY.
C
C                    If PARENT contains no keys that succeed the node
C                    containing KEY, RPIDX is set to zero.
C
C     RPKEY          is the absolute key located in PARENT at index
C                    RPIDX.  If PARENT contains no keys that succeed the
C                    node containing KEY, RPKEY is set to zero.
C
C     RSIB           is the number of the right sibling node of the node
C                    containing KEY.  If PARENT contains no keys that
C                    succeed the node containing KEY, then the node
C                    containing KEY has no right sibling, and RSIB is
C                    set to zero.
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
C     3)  If the input key is out of range, the error
C         SPICE(INDEXOUTOFRANGE) is signalled.
C
C     4)  If the input key is not found in the tree, the error
C         SPICE(ITEMNOTFOUND) is signalled.  This error most likely
C         indicates the presence of a serious bug in the EK software,
C         or that the input EK file has been corrupted.
C
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This routine supports tree operations that involve identifying
C     the parent node of a specified node.  In particular, this
C     routine supports updating ancestors of a node when an insertion
C     or deletion occurs.
C
C$ Examples
C
C     See ZZEKTRUD, ZZEKTRUI.
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
C-    Beta Version 1.0.0, 23-OCT-1995 (NJB)
C
C-&
 
 
C
C     SPICELIB functions
C
      INTEGER               LSTLEI
 
C
C     Local variables
C
      INTEGER               CHILD
      INTEGER               LKEY
      INTEGER               MAXKEY
      INTEGER               NEWKEY
      INTEGER               OFFSET
      INTEGER               PAGE   ( PGSIZI )
      INTEGER               PREV
      INTEGER               PRVKEY
      INTEGER               TOTKEY
      INTEGER               UNIT
 
      LOGICAL               FOUND
 
C
C     Use discovery check-in in this puppy.
C
C     Nothing found to begin with.
C
      FOUND  =  .FALSE.
 
C
C     Get a local copy of the input key.  We may overwrite the input
C     key when we set PKEY.
C
      LKEY   =   KEY
 
C
C     Start out by reading in the root page.  The node level starts
C     out at 1.
C
      CALL ZZEKPGRI ( HANDLE, TREE, PAGE )
 
      PARENT  =  0
      PKEY    =  0
      POFFST  =  0
      LPIDX   =  0
      LPKEY   =  0
      LSIB    =  0
      RPIDX   =  0
      RPKEY   =  0
      RSIB    =  0
 
C
C     Find out how many keys are in the tree.  If LKEY is outside
C     this range, we won't find it.
C
      TOTKEY  =  PAGE ( TRNKEY )
 
      IF (  ( LKEY .LT. 1 )  .OR.  ( LKEY .GT. TOTKEY )  ) THEN
 
         CALL CHKIN  ( 'ZZEKTRPI'                                      )
         CALL DASHLU ( HANDLE,  UNIT                                   )
         CALL SETMSG ( 'Key = #; valid range = 1:#. Tree = #, file = #')
         CALL ERRINT ( '#',     LKEY                                   )
         CALL ERRINT ( '#',     TOTKEY                                 )
         CALL ERRINT ( '#',     TREE                                   )
         CALL ERRFNM ( '#',     UNIT                                   )
         CALL SIGERR ( 'SPICE(INDEXOUTOFRANGE)'                        )
         CALL CHKOUT ( 'ZZEKTRPI'                                      )
         RETURN
 
      END IF
 
C
C     Find the last key at this level that is less than or equal to
C     the requested key.
C
      PREV   =  LSTLEI (  LKEY,  PAGE(TRNKR),  PAGE(TRKEYR+1)  )
 
      IF ( PREV .GT. 0 ) THEN
         PRVKEY =  PAGE(TRKEYR+PREV)
      ELSE
         PRVKEY =  0
      END IF
 
C
C     If we were lucky enough to get an exact match, we can quit now.
C     The root has no parent so the output values remain set to zero.
C
      IF ( PRVKEY .EQ. LKEY ) THEN
         RETURN
      END IF
 
C
C     Still here?  Traverse the pointer path until we find the key
C     or run out of progeny.
C
      OFFSET  =  PRVKEY
      PARENT  =  TREE
      PKEY    =  PAGE ( TRKEYR + 1 )
      MAXKEY  =  PAGE ( TRNKR      )
 
      IF ( PREV .GT. 0 ) THEN
         LPIDX  =  PREV
         LPKEY  =  PAGE ( TRKEYR + LPIDX )
         LSIB   =  PAGE ( TRKIDR + LPIDX )
      ELSE
         LPIDX  =  0
         LPKEY  =  0
         LSIB   =  0
      END IF
 
      IF ( PREV .LT. MAXKEY ) THEN
         RPIDX  =  PREV + 1
         RPKEY  =  PAGE ( TRKEYR + RPIDX     )
         RSIB   =  PAGE ( TRKIDR + RPIDX + 1 )
      ELSE
         RPIDX  =  0
         RPKEY  =  0
         RSIB   =  0
      END IF
 
 
      CHILD  =  PAGE ( TRKIDR + PREV + 1 )
      FOUND  =  .FALSE.
 
      DO WHILE (  ( CHILD .GT. 0 ) .AND. ( .NOT. FOUND )  )
C
C        Read in the child page.
C
         CALL ZZEKPGRI ( HANDLE, CHILD, PAGE )
 
C
C        Find the last key at this level that is less than or equal to
C        the requested key.  Since the keys we're looking at now are
C        ordinal positions relative to the subtree whose root is the
C        current node, we must subtract from LKEY the position of the
C        node preceding the first key of this subtree.
C
         NEWKEY  =  LKEY - OFFSET
         PREV    =  LSTLEI (  NEWKEY,  PAGE(TRNKC),  PAGE(TRKEYC+1)  )
 
         IF ( PREV .GT. 0 ) THEN
            PRVKEY =  PAGE(TRKEYC+PREV)
         ELSE
            PRVKEY =  0
         END IF
 
C
C        If we were lucky enough to get an exact match, we can quit.
C        The outputs are set.
C
         IF ( PRVKEY .EQ. NEWKEY ) THEN
 
            FOUND  =  .TRUE.
 
         ELSE
C
C           Record information from the current node before we read the
C           next child page.
C
            PARENT  =  CHILD
            POFFST  =  OFFSET
            PKEY    =  PAGE ( TRKEYC + 1 )  +  OFFSET
            MAXKEY  =  PAGE ( TRNKC      )
 
            IF ( PREV .GT. 0 ) THEN
               LPIDX  =  PREV
               LPKEY  =  PAGE ( TRKEYC + LPIDX )
               LSIB   =  PAGE ( TRKIDC + LPIDX )
            ELSE
               LPIDX  =  0
               LPKEY  =  0
               LSIB   =  0
            END IF
 
            IF ( PREV .LT. MAXKEY ) THEN
               RPIDX  =  PREV + 1
               RPKEY  =  PAGE ( TRKEYC + RPIDX     )
               RSIB   =  PAGE ( TRKIDC + RPIDX + 1 )
            ELSE
               RPIDX  =  0
               RPKEY  =  0
               RSIB   =  0
            END IF
 
C
C           Update the offset of the tree headed by CHILD, and set
C           the new child node.
C
            OFFSET  =  PRVKEY + OFFSET
            CHILD   =  PAGE ( TRKIDC + PREV + 1 )
 
         END IF
 
 
      END DO
 
C
C     If we found the key, our outputs are already set.  If not, we've
C     got trouble.
C
      IF ( .NOT. FOUND ) THEN
 
         CALL CHKIN  ( 'ZZEKTRPI'                                      )
         CALL DASHLU ( HANDLE,  UNIT                                   )
         CALL SETMSG ( 'Key #; valid range = 1:#. Tree = #, file = #.'//
     .                 '  Key was not found.  This probably indicates'//
     .                 ' a corrupted file or a bug in the EK code.'    )
         CALL ERRINT ( '#',     LKEY                                   )
         CALL ERRINT ( '#',     TOTKEY                                 )
         CALL ERRINT ( '#',     TREE                                   )
         CALL ERRFNM ( '#',     UNIT                                   )
         CALL SIGERR ( 'SPICE(ITEMNOTFOUND)'                           )
         CALL CHKOUT ( 'ZZEKTRPI'                                      )
         RETURN
 
      END IF
 
      RETURN
      END
