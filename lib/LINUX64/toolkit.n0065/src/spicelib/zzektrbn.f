C$Procedure      ZZEKTRBN ( EK tree, balance nodes )
 
      SUBROUTINE ZZEKTRBN ( HANDLE, TREE, LEFT, RIGHT, PARENT, PKIDX )
 
C$ Abstract
C
C     Solve overflow in a node by balancing the node
C     with one of its sibling nodes.
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
 
      INCLUDE  'ektree.inc'
 
      INTEGER               HANDLE
      INTEGER               TREE
      INTEGER               LEFT
      INTEGER               RIGHT
      INTEGER               PARENT
      INTEGER               PKIDX
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     TREE       I   Root of tree.
C     LEFT       I   Left node of pair to be balanced.
C     RIGHT      I   Right node of pair to be balanced.
C     PARENT     I   Parent node of pair to be balanced.
C     PKIDX      I   Parent key index.
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
C
C     5)  The sum of the key counts in LEFT and RIGHT must be between
C         2*MNKEYC and 2*MXKEYC; otherwise the key count invariants
C         cannot be satisfied by balancing.  If the sum fails to meet
C         this condition, the error SPICE(BUG) is signalled.
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
C     maneuver is called `balancing' the nodes.  After balancing, the
C     key counts of the affected nodes differ by at most 1.
C
C     The balancing process also affects the parent node of the
C     neighboring children because one key of the parent sits between
C     the children.  This `parent key' gets moved into one of the
C     children as keys are shifted.  If the shift is to the right, the
C     parent key is the largest key of the shifted set; if the shift
C     is to the left, the parent key is the least of the shifted set.
C
C     When keys are shifted, their data values move along with them.
C     In general, child pointers move along with keys, but there are
C     some tricky points:
C
C        - The left and right child pointers of the parent key don't
C          get updated; they continue to point to the two children
C          LEFT and RIGHT.
C
C        - On a right shift, the right child pointer of the key that
C          gets moved into the parent key's original position becomes
C          the first left child pointer of the right sibling.  The left
C          child pointer of this key doesn't get moved at all.
C
C        - On a left shift, the left child pointer of the key that
C          gets moved into the parent key's original position becomes
C          the last right child pointer of the left sibling.  The right
C          child pointer of this key becomes the left child pointer of
C          the first key of RIGHT.
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
C-    Beta Version 1.0.0, 27-OCT-1995 (NJB)
C
C-&
 
 
C
C     Non-SPICELIB functions
C
      INTEGER               ZZEKTRNK
 
C
C     Local variables
C
      INTEGER               LNKEYS
      INTEGER               RNKEYS
      INTEGER               ROOT
      INTEGER               SCHLEP
      INTEGER               SUM
 
C
C     Use discovery check-in for speed.
C
      ROOT  =  TREE
 
      IF ( ( LEFT .EQ. ROOT ) .OR. ( RIGHT .EQ. ROOT )  ) THEN
 
         CALL CHKIN  ( 'ZZEKTRBN'                                  )
         CALL SETMSG ( 'Input node is root; only children can be ' //
     .                 'balanced.'                                 )
         CALL SIGERR ( 'SPICE(BUG)'                                )
         CALL CHKOUT ( 'ZZEKTRBN'                                  )
 
      END IF
 
C
C     Get the key counts for the left and right nodes.
C
      LNKEYS  =  ZZEKTRNK ( HANDLE, TREE, LEFT  )
      RNKEYS  =  ZZEKTRNK ( HANDLE, TREE, RIGHT )
 
C
C     Balancing the nodes should give each of them a key count in
C     the range of
C
C        MNKEYC : MXKEYC
C
C     If that's not possible, we have a serious problem.
C
 
      SUM  =  LNKEYS + RNKEYS
 
 
      IF (      (  SUM .GT.  2*MXKEYC  )
     .     .OR. (  SUM .LT.  2*MNKEYC  )   )  THEN
 
         CALL CHKIN  ( 'ZZEKTRBN'                                      )
         CALL SETMSG ( 'Node # and right sibling # contain # and # '  //
     .                 'keys respectively; count sum should be in '   //
     .                 'range #:#.'                                    )
         CALL ERRINT ( '#',  LEFT                                      )
         CALL ERRINT ( '#',  RIGHT                                     )
         CALL ERRINT ( '#',  LNKEYS                                    )
         CALL ERRINT ( '#',  RNKEYS                                    )
         CALL ERRINT ( '#',  2*MNKEYC                                  )
         CALL ERRINT ( '#',  2*MXKEYC                                  )
         CALL SIGERR ( 'SPICE(BUG)'                                    )
         CALL CHKOUT ( 'ZZEKTRBN'                                      )
         RETURN
 
      END IF
 
C
C     Now, the actions we take depend on whether we must schlep keys
C     to the right or left.
C
      IF ( LNKEYS .GT. RNKEYS ) THEN
 
         SCHLEP  =        LNKEYS  -  ( (SUM+1) / 2 )
 
      ELSE IF ( LNKEYS .LT. RNKEYS ) THEN
 
         SCHLEP  =  - (   RNKEYS  -  ( (SUM+1) / 2 )   )
 
      ELSE
 
         SCHLEP  =  0
 
      END IF
 
C
C     Rotate the requested number of keys.
C
      CALL ZZEKTRRK ( HANDLE, TREE, LEFT, RIGHT, PARENT, PKIDX, SCHLEP )
 
      RETURN
      END
