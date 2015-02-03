C$Procedure      ZZEKTRFR ( EK tree, free )
 
      SUBROUTINE ZZEKTRFR ( HANDLE, TREE )
 
      IMPLICIT NONE
 
C$ Abstract
C
C     Free a tree:  deallocate all pages belonging to the tree.
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
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     TREE       I   Root of tree.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle of an EK open for write access.
C
C     TREE           is the root node number of the tree of interest.
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
C     3)  If the input tree is deeper than the maximum allowed depth
C         TRMXDP, the error SPICE(INVALIDFORMAT) is signalled.
C
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This routine cleans up the pages occupied by an EK tree; the pages
C     are deallocated by the EK paging system.  Freeing a tree allows
C     the pages previous occupied by the tree to be used for other
C     purposes.
C
C$ Examples
C
C     1)  Return the pages occupied by the tree whose root node number
C         is TREE.  Assume HANDLE is a file handle of the EK to which
C         the tree belongs:
C
C            CALL EKTRFR ( HANDLE, TREE )
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
C-    SPICELIB Version 1.2.0, 18-JUN-1999 (WLT)
C
C        Removed a redundant call to CHKIN.
C
C-    SPICELIB Version 1.1.0, 14-OCT-1996 (NJB)
C
C        Bug fix:  the original version was untested and had numerous
C        problems.
C
C-    SPICELIB Version 1.0.0, 22-OCT-1995 (NJB)
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 14-OCT-1996 (NJB)
C
C        Bug fix:  the original version was untested and had numerous
C        problems.
C
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Non-SPICELIB functions
C
 
C
C     Local parameters
C
      INTEGER               PARIDX
      PARAMETER           ( PARIDX =  1 )
 
      INTEGER               NKDIDX
      PARAMETER           ( NKDIDX =  2 )
 
      INTEGER               BEGIDX
      PARAMETER           ( BEGIDX =  3 )
 
C
C     Local variables
C
      INTEGER               DEPTH
      INTEGER               FIRST
      INTEGER               KIDBAS
      INTEGER               LEVEL
      INTEGER               NKEYS
      INTEGER               NKIDS
      INTEGER               NODE
      INTEGER               PAGE  ( PGSIZI )
      INTEGER               REMAIN
      INTEGER               STACK ( 3, TRMXDP )
      INTEGER               UNIT
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKTRFR' )
      END IF
 
C
C     Read in the root node.
C
      CALL ZZEKPGRI ( HANDLE, TREE, PAGE )
 
C
C     Check the depth of the tree.  If the tree is deeper than
C     we expected, we've a problem.
C
      DEPTH  =  PAGE ( TRDPTH )
 
      IF ( DEPTH .GT. TRMXDP ) THEN
 
         CALL DASHLU (  HANDLE,  UNIT  )
 
         CALL SETMSG ( 'Tree has depth #; max supported depth is #.' //
     .                 'EK = #; TREE = #.'                           )
         CALL ERRINT ( '#',  DEPTH                                   )
         CALL ERRINT ( '#',  TRMXDP                                  )
         CALL ERRFNM ( '#',  UNIT                                    )
         CALL ERRINT ( '#',  TREE                                    )
         CALL SIGERR ( 'SPICE(INVALIDFORMAT)'                        )
         CALL CHKOUT ( 'ZZEKTRFR'                                    )
         RETURN
 
      END IF

C
C     We traverse the tree in post-order fashion:  at each node,
C     we first delete all of the node's children in left-to-right
C     order, then we delete the node itself.  We use a stack to
C     keep track of the ancestors of the node we're currently
C     considering.
C
      LEVEL   =  1
      REMAIN  =  PAGE ( TRNNOD )
      NODE    =  TREE
 
C
C     Initialize the child count and the location of the first
C     child in the current node.  The child count of the root is
C     one more than the number of keys in the root if the root has
C     children; otherwise, the child count is zero.
C
      NKEYS  =  PAGE ( TRNKR )
      
      IF ( DEPTH .EQ. 1 ) THEN
         NKIDS = 0
      ELSE
         NKIDS = NKEYS + 1
      END IF
      
      FIRST  =  1
 
 
      DO WHILE ( REMAIN .GT. 0 )
C
C        At this point,
C
C           NODE is the current node to consider.
C           NKIDS is the number of children of NODE.
C           FIRST is the index of the first child in NODE.
C
         IF ( NKIDS .GT. 0 ) THEN
C
C           This node has children, so push the current node, the
C           number of children, and the location of the first child on
C           the stack.  Before incrementing the stack level, determine
C           the base address of the child pointers.
C
            IF ( LEVEL .EQ. 1 ) THEN
               KIDBAS  =  TRKIDR
            ELSE
               KIDBAS  =  TRKIDC
            END IF
 
            STACK ( PARIDX, LEVEL ) = NODE
            STACK ( NKDIDX, LEVEL ) = NKIDS
            STACK ( BEGIDX, LEVEL ) = FIRST
            LEVEL                   = LEVEL + 1
            
C
C           Read in the first child node.
C
            NODE  =  PAGE ( KIDBAS + FIRST )
 
            CALL ZZEKPGRI ( HANDLE, NODE, PAGE )
 
C
C           We've never visited this node before, so the node's
C           metadata is valid, and the first child pointer, if any,
C           is at location 1.
C
            NKEYS  =  PAGE ( TRNKC )
            
            IF ( LEVEL .LT. DEPTH ) THEN
               NKIDS  =  NKEYS + 1
            ELSE
               NKIDS  =  0
            END IF
            
            FIRST  =  1
 
 
         ELSE
C
C           This node has no children.  We can free this page.
C
            CALL ZZEKPGFR ( HANDLE, INT, NODE )
 
            REMAIN  =  REMAIN - 1
            
C
C           Obtain the parent node by popping the stack.
C
            LEVEL   =  LEVEL - 1
            
            IF ( LEVEL .GT. 0 ) THEN
            
               NODE    =  STACK ( PARIDX, LEVEL )
               FIRST   =  STACK ( BEGIDX, LEVEL )
               NKIDS   =  STACK ( NKDIDX, LEVEL )
 
C
C              The parent has one less child, and the location of the
C              first child is the successor of the stored location.
C
               NKIDS  =  NKIDS - 1
               FIRST  =  FIRST + 1

C
C              The parent page has been overwritten; read it back in.
C
               CALL ZZEKPGRI ( HANDLE, NODE, PAGE )

            END IF
             
         END IF
C
C        On this pass through the loop, we either visited a node
C        for the first time, or we deleted a node.  Therefore, we
C        made progress toward loop termination.
C
      END DO
 
 
      CALL CHKOUT ( 'ZZEKTRFR' )
      RETURN
      END
