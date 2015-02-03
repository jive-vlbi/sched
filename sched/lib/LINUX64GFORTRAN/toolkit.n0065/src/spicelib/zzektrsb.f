C$Procedure      ZZEKTRSB ( EK tree, identify siblings )
 
      SUBROUTINE ZZEKTRSB ( HANDLE, TREE, KEY, LSIB, LKEY, RSIB, RKEY )
 
C$ Abstract
C
C     Identify the immediate siblings of a node:  return a key in each
C     sibling and the siblings' node numbers.
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
 
      INTEGER               HANDLE
      INTEGER               TREE
      INTEGER               KEY
      INTEGER               LSIB
      INTEGER               LKEY
      INTEGER               RSIB
      INTEGER               RKEY
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     TREE       I   Root of tree.
C     KEY        I   Key of interest.
C     LSIB       O   Left sibling node.
C     LKEY       O   Key in left sibling.
C     RSIB       O   Right sibling node.
C     RKEY       O   Key in right sibling.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle of an EK open for read or write
C                    access.
C
C     TREE           is the root node number of the tree of interest.
C
C     KEY            is a key belonging to a node whose sibling nodes
C                    are sought.  KEY is expected to be an absolute,
C                    not node-relative, key.
C
C$ Detailed_Output
C
C     LSIB           is the number of the left sibling node of the node
C                    containing KEY.  If the node containing KEY has no
C                    left sibling, LSIB is set to zero.
C
C     LKEY           is an absolute key in node LSIB.
C
C     RSIB           is the number of the right sibling node of the node
C                    containing KEY.  If the node containing KEY has no
C                    right sibling, RSIB is set to zero.
C
C     RKEY           is an absolute key in node RSIB.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If HANDLE is invalid, the error will be diagnosed by routines
C         called by this routine.
C
C     2)  If an error occurs while looking up the parent of the node
C         containing KEY, the error will be diagnosed by routines
C         called by this routine.  It is not an error for the node
C         containing KEY to have no parent, as long as KEY belongs to
C         the root.
C
C     3)  If an I/O error occurs while reading the indicated file, the
C         error will be diagnosed by routines called by this routine.
C
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This routine is a utility intended for use by other routines in
C     the EKTRxx set.
C
C     The output keys LKEY and RKEY may be used to find the siblings
C     of the sibling nodes LSIB and RSIB.
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
C-    Beta Version 1.0.0, 20-OCT-1995 (NJB)
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
 
C
C     Other functions
C
      INTEGER               ZZEKTRBS
 
C
C     Local variables
C
      INTEGER               ADDRSS
      INTEGER               BASE
      INTEGER               KEYBAS
      INTEGER               KIDBAS
      INTEGER               LOFFST
      INTEGER               LLPIDX
      INTEGER               LPIDX
      INTEGER               LPKEY
      INTEGER               NKBAS
      INTEGER               PARENT
      INTEGER               PKEY
      INTEGER               POFFST
      INTEGER               ROFFST
      INTEGER               RPIDX
      INTEGER               RPKEY
 
C
C     Start out by looking up the parent node.  We get LSIB
C     and RSIB for free.
C
      CALL ZZEKTRPI ( HANDLE, TREE,   KEY,  PARENT, PKEY,  POFFST,
     .                LPIDX,  LPKEY,  LSIB, RPIDX,  RPKEY, RSIB   )
 
      IF ( FAILED() ) THEN
         RETURN
      END IF
 
C
C     Set the base addresses for the child pointers and keys,
C     based on whether the parent is the root.
C
      IF ( PARENT .EQ. TREE ) THEN
         KEYBAS = TRKEYR
         KIDBAS = TRKIDR
         NKBAS  = TRNKR
      ELSE
         KEYBAS = TRKEYC
         KIDBAS = TRKIDC
         NKBAS  = TRNKC
      END IF
 
C
C     We need to find absolute keys in each sibling that exists.
C     To do this, we need the node offset of each sibling node.
C     That offset is the value of the parent key preceding each node,
C     plus the parent's offset.
C
      IF ( LPIDX .GT. 1 ) THEN
C
C        The left parent key has a predecessor.  This predecessor is
C        the immediate predecessor of the left sibling node.
C
         LLPIDX  =  LPIDX  -  1
         BASE    =  ZZEKTRBS ( PARENT )
         ADDRSS  =  BASE   +  KEYBAS  +  LLPIDX
 
         CALL DASRDI ( HANDLE, ADDRSS, ADDRSS, LOFFST )
 
         LOFFST  =  LOFFST + POFFST
 
C
C        Get the first key from the left sibling.  Convert the key
C        to an absolute key.
C
         BASE    =  ZZEKTRBS ( LSIB )
         ADDRSS  =  BASE + TRKEYC + 1
 
         CALL DASRDI ( HANDLE, ADDRSS, ADDRSS, LKEY )
 
         LKEY    =  LKEY + LOFFST
 
 
      ELSE IF ( LPIDX .EQ. 1 ) THEN
C
C        The left parent key is the first key.  The left sibling has
C        no predecessor.
C
C        Get the first key from the left sibling.  Convert the key
C        to an absolute key.
C
         BASE    =  ZZEKTRBS ( LSIB )
         ADDRSS  =  BASE + TRKEYC + 1
 
         CALL DASRDI ( HANDLE, ADDRSS, ADDRSS, LKEY )
 
         LKEY    =  LKEY + POFFST
 
 
      ELSE
C
C        There's no left sibling.  Set the left sibling's key to a
C        value that won't be mistaken for a valid one.
C
         LKEY    =  0
 
      END IF
 
C
C     LKEY is set.  It's time to produce an absolute key for the
C     right sibling.
C
      IF ( RPIDX .GT. 0 ) THEN
C
C        The right parent key exists.  This key is the
C        immediate predecessor of the right sibling node.
C
         ROFFST  =  RPKEY  +  POFFST
 
C
C        Get the first key from the right sibling.  Convert the key
C        to an absolute key.
C
         BASE    =  ZZEKTRBS ( RSIB )
         ADDRSS  =  BASE + TRKEYC + 1
 
         CALL DASRDI ( HANDLE, ADDRSS, ADDRSS, RKEY )
 
         RKEY    =  RKEY + ROFFST
 
 
      ELSE
C
C        There's no right sibling.  Set the right sibling's key to a
C        value that won't be mistaken for a valid one.
C
         RKEY    =  0
 
      END IF
C
C     All outputs are set.
C
 
      RETURN
      END
