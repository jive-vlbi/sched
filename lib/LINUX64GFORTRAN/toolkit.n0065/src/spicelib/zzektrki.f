C$Procedure      ZZEKTRKI ( EK tree, look up key by index )
 
      SUBROUTINE ZZEKTRKI ( HANDLE, TREE, NODKEY, N, KEY )
 
C$ Abstract
C
C     Get a key from a node by index:  return the key having a specified
C     index in a specified node.  The node of interest is identified
C     by a key in the node.
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
      INTEGER               NODKEY
      INTEGER               N
      INTEGER               KEY
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     TREE       I   Root of tree.
C     NODKEY     I   Key identifying node containing key of interest.
C     N          I   Index of key of interest.
C     KEY        O   Key located at index N.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle of an EK open for write access.
C
C     TREE           is the root node number of the tree of interest.
C
C     NODKEY         is an absolute key belonging to the node
C                    containing the key of interest.
C
C                    NODKEY must be in the range 1 : NKEYS, where
C                    NKEYS is the number of keys in the tree.
C
C     N              is the node-relative index of the key of interest.
C                    Indices of keys start at 1.
C
C$ Detailed_Output
C
C     KEY            is the absolute key located at index N within the
C                    node containing NODKEY.
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
C     3)  If the input key is less than 1 or greater than the number
C         of keys in the specified tree, the error SPICE(INVALIDINDEX)
C         is signalled.
C
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This routine allows lookup of keys by index.  It is frequently
C     used by other EK private routines to find the first key of a node.
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
C-    Beta Version 1.0.0, 23-OCT-1995 (NJB)
C
C-&
 
 
C
C     Functions
C
      INTEGER               ZZEKTRBS
      INTEGER               ZZEKTRNK
 
C
C     Local variables
C
      INTEGER               ADDRSS
      INTEGER               BASE
      INTEGER               IDX
      INTEGER               LEVEL
      INTEGER               NODE
      INTEGER               NOFFST
      INTEGER               PTR
      INTEGER               SIZE
 
 
 
 
 
      CALL ZZEKTRLK (  HANDLE,  TREE,    NODKEY,  IDX,
     .                 NODE,    NOFFST,  LEVEL,   PTR   )
 
      SIZE  =  ZZEKTRNK ( HANDLE, TREE,  NODE )
 
C
C     Reject bad indices.
C
      IF (  ( N .LT. 0 ) .OR. ( N .GT. SIZE )  )  THEN
 
         CALL CHKIN  ( 'ZZEKTRKI'                                  )
         CALL SETMSG ( 'Key index = #; valid range in node # is ' //
     .                 '1:#'                                       )
         CALL ERRINT ( '#',  N                                     )
         CALL ERRINT ( '#',  NODE                                  )
         CALL ERRINT ( '#',  SIZE                                  )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                       )
         CALL CHKOUT ( 'ZZEKTRKI'                                  )
         RETURN
 
      END IF
 
 
      BASE  =  ZZEKTRBS ( NODE )
 
 
      IF ( LEVEL .EQ. 1 ) THEN
         ADDRSS = BASE + TRKEYR + N
      ELSE
         ADDRSS = BASE + TRKEYC + N
      END IF
 
 
      CALL DASRDI ( HANDLE, ADDRSS, ADDRSS, KEY )
 
C
C     Map the key from relative to absolute.
C
      KEY  =  KEY + NOFFST
 
 
      END
