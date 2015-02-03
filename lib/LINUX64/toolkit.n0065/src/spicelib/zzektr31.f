C$Procedure      ZZEKTR31 ( EK tree, 3-1 merge )
 
      SUBROUTINE ZZEKTR31 ( HANDLE, TREE )
 
C$ Abstract
C
C     Execute a 3-1 merge:  move the contents of two children into
C     the root node and delete the children.
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
C     3)  If there is not exactly 1 key in the root at the time this
C         routine is called, the error SPICE(BUG) is signalled.
C
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     Deletions from an EK tree start at a leaf node.  If the node
C     underflows, the EK system attempts to shuffle keys at the leaf
C     level to resolve the underflow.  That attempt failing, the system
C     delegates the problem upward to the next higher level.  Underflow
C     may occur there as well; if it does, the problem gets passed
C     upward again.  If the root has only two children and one of these
C     underflows, the system reduces the height of the tree by
C     executing what's called a `3-1' merge:  the root loses its two
C     children, and all of the keys in the children are moved into
C     the root.  The former grandchildren of the root become
C     children of the root.
C
C     A tree is eligible for a 3-1 merge only if the root has exactly
C     two children, and the sum of the key counts of the children
C     constitutes an underflow of 1 key:  that is, the sum is
C
C        2*MNKEYC - 1
C
C     After the 3-1 merge, the tree is balanced and all invariants
C     relating to key counts are restored.
C
C     The tree grows shorter by one level as a result of a 3-1 merge;
C     this is the only circumstance under which the tree grows shorter.
C
C     Below are the gory details concerning the actions of this routine.
C     All of the parameters referred to here (in capital letters) are
C     defined in the include file ektree.inc.
C
C     In a 3-1 merge:
C
C
C        - The keys of the left child are moved into the root.  These
C          become the leftmost MNKEYC or MNKEYC-1 keys of the root,
C          depending on whether the underflow occurred in the left
C          child.
C
C        - The data values associated with the keys of the left child
C          of the root are moved into the root along with the keys.
C
C        - The left child pointers associated with the keys of the left
C          child of the root are moved into the root along with the
C          keys.
C
C        - The last right child pointer in the left child of the root
C          the root is moved to location NLEFT+1 in the child pointer
C          array of the root, where NLEFT is the number of keys in
C          the former left child.  This pointer overwrites the root's
C          pointer to the left child.
C
C        - The keys of the right child are moved into the root.  These
C          become the rightmost MNKEYC or MNKEYC-1 keys of the root,
C          depending on whether the underflow occurred in the right
C          child.
C
C        - The data values associated with the keys of the right child
C          of the root are moved into the root along with the keys.
C
C        - The left child pointers associated with the keys of the right
C          child of the root are moved into the root along with the
C          keys.  The first of these pointers overwrites the root's
C          pointer to the right child.
C
C        - The last right child pointer in the right child of the root
C          the root is moved to location 2*MNKEYC+1 in the child pointer
C          array of the root.
C
C        - The former children of the root are deleted.
C
C     As the above list shows, the root contains the maximum allowed
C     number of keys after a 3-1 merge.  This is because
C
C        MXKEYR  =  MXKIDR - 1
C
C                =  2 * ( (2*MXKIDC - 2)/3 )
C
C                =  2 * ( (2*MXKIDC + 1)/3  - 1 )
C
C                =  2 * ( MNKIDC - 1)
C
C                =  2 * MNKEYC
C
C     Our assumptions were that there was one key in the root and
C     that the sum of the key counts of the two children of the root
C     was
C
C        ( 2 * MNKEYC ) - 1
C
C     Thus the size constraints on the root node are met.
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
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 27-OCT-1995 (NJB)
C
C-&
 
 
C
C     Local variables
C
      INTEGER               C1PAGE ( PGSIZI )
      INTEGER               C2PAGE ( PGSIZI )
      INTEGER               CHILD  ( 2 )
      INTEGER               DELTA
      INTEGER               I
      INTEGER               MIDDLE
      INTEGER               NLKEYS
      INTEGER               NRKEYS
      INTEGER               ROOT
      INTEGER               RPAGE  ( PGSIZI )
      INTEGER               SUM
 
C
C     Use discovery check-in for speed.
C
      ROOT    =  TREE
 
      CALL ZZEKPGRI ( HANDLE, ROOT, RPAGE )
 
      NRKEYS  =  RPAGE ( TRNKR )
 
C
C     There must be exactly 1 key in the root.
C
      IF ( NRKEYS .NE. 1 ) THEN
 
         CALL CHKIN  ( 'ZZEKTR31'                                 )
         CALL SETMSG ( 'Number of keys in root = #; should be 1.' )
         CALL ERRINT ( '#',  NRKEYS                               )
         CALL SIGERR ( 'SPICE(BUG)'                               )
         CALL CHKOUT ( 'ZZEKTR31'                                 )
         RETURN
 
      END IF
 
C
C     Read in the child pages.  Get the key counts for these pages.
C
      CHILD(1)  =  RPAGE ( TRKIDR + 1 )
      CHILD(2)  =  RPAGE ( TRKIDR + 2 )
 
      CALL ZZEKPGRI ( HANDLE, CHILD(1), C1PAGE )
      CALL ZZEKPGRI ( HANDLE, CHILD(2), C2PAGE )
 
      NLKEYS    =  C1PAGE ( TRNKC )
      NRKEYS    =  C2PAGE ( TRNKC )
      SUM       =  NLKEYS + NRKEYS
 
C
C     The sum of the number of keys in the two input nodes must
C     sum exactly to value representing an underflow level of 1 key.
C
      IF (  SUM  .NE.  ( 2*MNKEYC - 1 )  ) THEN
 
         CALL CHKIN  ( 'ZZEKTR31'                                      )
         CALL SETMSG ( 'Number of keys in nodes LEFT = #; in RIGHT ' //
     .                 '= #; counts summing to # were expected.'       )
         CALL ERRINT ( '#',  NLKEYS                                    )
         CALL ERRINT ( '#',  NRKEYS                                    )
         CALL ERRINT ( '#',  2*MNKEYC-1                                )
         CALL SIGERR ( 'SPICE(BUG)'                                    )
         CALL CHKOUT ( 'ZZEKTR31'                                      )
         RETURN
 
      END IF
 
C
C     Shift the key and data pointer in the root to right to allow
C     insertion of NLKEYS new entries on the left.  The child pointers
C     need not be shifted; they'll be overwritten later.
C
      RPAGE (TRKEYR+NLKEYS+1)  =  RPAGE (TRKEYR+1)
      RPAGE (TRDATR+NLKEYS+1)  =  RPAGE (TRDATR+1)
 
C
C     Copy in the keys, data pointers, and child pointers from the
C     left child into the root.  The number of predecessors of the
C     new keys is unchanged by this operation.
C
      CALL MOVEI ( C1PAGE(TRKEYC+1), NLKEYS,     RPAGE(TRKEYR+1)  )
      CALL MOVEI ( C1PAGE(TRDATC+1), NLKEYS,     RPAGE(TRDATR+1)  )
      CALL MOVEI ( C1PAGE(TRKIDC+1), NLKEYS+1,   RPAGE(TRKIDR+1)  )
 
C
C     Copy in the keys, data pointers, and child pointers from the
C     right child into the root.  The number of predecessors of the
C     new keys is increased by the value of the last key already
C     present.
C
      MIDDLE  =  NLKEYS  +  1
      DELTA   =  RPAGE( TRKEYR + MIDDLE )
 
      DO I = 1, NRKEYS
         RPAGE( TRKEYR+MIDDLE+I )  =  C2PAGE(TRKEYC + I)  +  DELTA
      END DO
 
      CALL MOVEI ( C2PAGE(TRDATC+1), NRKEYS,    RPAGE(TRDATR+MIDDLE+1) )
      CALL MOVEI ( C2PAGE(TRKIDC+1), NRKEYS+1,  RPAGE(TRKIDR+MIDDLE+1) )
 
C
C     Now the root must be updated.  The root now contains
C     the maximum allowed number of keys.  The depth of the tree
C     has decreased, as well as the number of nodes in the tree.
C
      RPAGE ( TRNKR      )  =  MXKEYR
      RPAGE ( TRDPTH     )  =  RPAGE ( TRDPTH ) - 1
      RPAGE ( TRNNOD     )  =  RPAGE ( TRNNOD ) - 2
 
C
C     Write out the updated root.
C
      CALL ZZEKPGWI ( HANDLE, ROOT, RPAGE )
 
C
C     Free the pages occupied by the deleted children.
C
      DO I = 1, 2
         CALL ZZEKPGFR ( HANDLE, INT, CHILD(I)  )
      END DO
 
      RETURN
      END
