C$Procedure      ZZEKTR13 ( EK tree, 1-3 split )
 
      SUBROUTINE ZZEKTR13 ( HANDLE, TREE )
 
C$ Abstract
C
C     Execute a 1-3 split:  split the root node to create two new
C     children, leaving a single key in the root.
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
C     2)  If an I/O error occurs while reading the indicated file, the
C         error will be diagnosed by routines called by this routine.
C
C     3)  If the number of keys in the root does not correspond to an
C         overflow of exactly 1 key, the error SPICE(BUG) is signalled.
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
C     delegates the problem upward to the next higher level.  Overflow
C     may occur there as well; if it does, the problem gets passed
C     upward again.  If the root overflows, the system makes room by
C     executing what's called a `1-3' split:  the root gets two new
C     children, and all but one of the keys in the root are moved into
C     the new children.  The former children of the root become
C     children of the two new children of the root.
C
C     After the 1-3 split, the tree is balanced and all invariants
C     relating to key counts are restored.
C
C     The tree grows taller by one level as a result of a 1-3 split;
C     this is the only circumstance under which the tree grows taller.
C
C     Below are the gory details concerning the actions of this routine.
C     All of the parameters referred to here (in capital letters) are
C     defined in the include file ektree.inc.
C
C     In a 1-3 split:
C
C        - The leftmost MNKEYC keys of the root are moved into the
C          new left child.
C
C        - The data values associated with the first MNKEYC keys of the
C          root are moved along with the keys.
C
C        - The left child pointers associated with the first MNKEYC keys
C          of the root are moved along with the keys.
C
C        - The right child pointer of the key at location MNKEYC+1 in
C          the root is moved to location MYKEYC+1 in the child pointer
C          array of the left child.
C
C        - The rightmost MNKEYC keys of the root are moved into the
C          new right child.
C
C        - The data values associated with the last MNKEYC keys of the
C          root are moved along with the keys.
C
C        - The left child pointers associated with the last MNKEYC keys
C          of the root are moved along with the keys.
C
C        - The right child pointer of the last in the root is moved to
C          location MYKEYC+1 in the child pointer array of the right
C          child.
C
C        - The left child pointer of the one key left in the root
C          points to the new left child.
C
C        - The right child pointer of the one key left in the root
C          points to the new right child.
C
C     As the above list shows, each of the new children of the root
C     contains the minimum allowed number of keys that a child node
C     may have.  Thus the size constraints on child nodes are met.
C     The root must be non-empty unless the tree is empty; this
C     condition is also met.
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
C-    Beta Version 1.0.0, 26-OCT-1995 (NJB)
C
C-&
 
 
C
C     Local variables
C
      INTEGER               BASE
      INTEGER               C1PAGE ( PGSIZI )
      INTEGER               C2PAGE ( PGSIZI )
      INTEGER               CHILD  ( 2 )
      INTEGER               DELTA
      INTEGER               I
      INTEGER               MIDDLE
      INTEGER               NRKEYS
      INTEGER               ROOT
      INTEGER               RPAGE  ( PGSIZI )
 
C
C     Use discovery check-in for speed.
C
      ROOT    =  TREE
 
      CALL ZZEKPGRI ( HANDLE, ROOT, RPAGE )
 
      NRKEYS  =  RPAGE ( TRNKR )
 
C
C     The number of keys in the root must correspond exactly to an
C     overflow level of 1 key.
C
      IF (  NRKEYS  .NE.  ( MXKEYR + 1 )  ) THEN
 
         CALL CHKIN  ( 'ZZEKTR13'                                 )
         CALL SETMSG ( 'Number of keys in root = #; should be #.' )
         CALL ERRINT ( '#',  NRKEYS                               )
         CALL ERRINT ( '#',  MXKEYR+1                             )
         CALL SIGERR ( 'SPICE(BUG)'                               )
         CALL CHKOUT ( 'ZZEKTR13'                                 )
         RETURN
 
      END IF
 
C
C     Allocate two new pages; these will become children of the root.
C     Each one will be assigned MNKEYC keys.
C
      DO I = 1, 2
         CALL ZZEKPGAL ( HANDLE, INT, CHILD(I), BASE )
      END DO
 
C
C     Set the key count in the first child.
C
      CALL CLEARI ( PGSIZI, C1PAGE )
 
      C1PAGE ( TRNKC )  =  MNKEYC
 
C
C     Copy in the keys, data pointers, and child pointers from the
C     first MNKEYC locations in the root.  Also take the left child
C     pointer of the middle key.
C
      CALL MOVEI ( RPAGE(TRKEYR+1), MNKEYC,    C1PAGE(TRKEYC+1) )
      CALL MOVEI ( RPAGE(TRDATR+1), MNKEYC,    C1PAGE(TRDATC+1) )
      CALL MOVEI ( RPAGE(TRKIDR+1), MNKEYC+1,  C1PAGE(TRKIDC+1) )
 
C
C     Set up the key count in the second child.
C
      CALL CLEARI ( PGSIZI, C2PAGE )
 
      C2PAGE ( TRNKC  )  =  MNKEYC
 
C
C     Copy in the keys, data pointers, and child pointers from the
C     last MNKEYC locations in the root.  Also take the last right
C     child pointer.
C
      MIDDLE = MNKEYC + 1
 
      CALL MOVEI ( RPAGE(TRKEYR+MIDDLE+1), MNKEYC,    C2PAGE(TRKEYC+1) )
      CALL MOVEI ( RPAGE(TRDATR+MIDDLE+1), MNKEYC,    C2PAGE(TRDATC+1) )
      CALL MOVEI ( RPAGE(TRKIDR+MIDDLE+1), MNKEYC+1,  C2PAGE(TRKIDC+1) )
 
C
C     The keys in this second node must be adjusted to account for the
C     loss of the predecessors assigned to the subtree headed by the
C     left child, as well as of the middle key.
C
      DELTA  =  RPAGE ( TRKEYR + MIDDLE )
 
      DO I = 1, MNKEYC
         C2PAGE( TRKEYC + I )  =  C2PAGE( TRKEYC + I ) - DELTA
      END DO
 
C
C     Now the root must be updated.  The root now contains just 1
C     key; that key should be shifted left to the first key location.
C     There are two child pointers; these point to the children just
C     created.  The depth of the tree has increased, as well as the
C     number of nodes in the tree.
C
      RPAGE ( TRKEYR + 1 )  =  RPAGE ( TRKEYR + MIDDLE )
      RPAGE ( TRDATR + 1 )  =  RPAGE ( TRDATR + MIDDLE )
      RPAGE ( TRKIDR + 1 )  =  CHILD(1)
      RPAGE ( TRKIDR + 2 )  =  CHILD(2)
      RPAGE ( TRNKR      )  =  1
      RPAGE ( TRDPTH     )  =  RPAGE ( TRDPTH ) + 1
      RPAGE ( TRNNOD     )  =  RPAGE ( TRNNOD ) + 2
 
      CALL CLEARI ( MXKEYR, RPAGE(TRKEYR+2) )
      CALL CLEARI ( MXKEYR, RPAGE(TRDATR+2) )
      CALL CLEARI ( MXKEYR, RPAGE(TRKIDR+3) )
 
C
C     Write out our updates.
C
      CALL ZZEKPGWI ( HANDLE,  ROOT,      RPAGE  )
      CALL ZZEKPGWI ( HANDLE,  CHILD(1),  C1PAGE )
      CALL ZZEKPGWI ( HANDLE,  CHILD(2),  C2PAGE )
 
      RETURN
      END
