C$Procedure      LNKHL ( LNK, head of list )
 
      INTEGER FUNCTION LNKHL ( NODE, POOL )
 
C$ Abstract
C
C     Return the head node of the list containing a specified node.
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
C     LNK
C
C$ Keywords
C
C     LIST
C
C$ Declarations
 
      IMPLICIT NONE
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )
 
      INTEGER               NODE
      INTEGER               POOL ( 2,  LBPOOL : * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NODE       I   Number of a node.
C     POOL       I   A doubly linked list pool.
C     LBPOOL     P   Lower bound of pool column indices.
C
C     The function returns the number of the head node of the list
C     containing NODE.
C
C$ Detailed_Input
C
C     NODE           is the number of a node in POOL.  Normally,
C                    NODE will designate an allocated node, but NODE
C                    is permitted to be less than or equal to zero.
C
C     POOL           is a doubly linked list pool.
C
C$ Detailed_Output
C
C     The function returns the number of the head node of the list
C     containing NODE.  If NODE is non-positive, the function returns
C     zero.
C
C$ Parameters
C
C     LBPOOL        is the lower bound of the column indices of the POOL
C                   array.  The columns indexed LBPOOL to 0 are reserved
C                   as a control area for the pool.
C
C$ Exceptions
C
C     1)  If the NODE is less than or equal to zero, NODE is not
C         considered to be erroneous.  The value 0 is returned.
C
C     2)  If NODE is greater than the size of the pool, the error
C         SPICE(INVALIDNODE) is signalled.  The value 0 is returned.
C
C     3)  If NODE is not the number of an allocated node, the error
C         SPICE(UNALLOCATEDNODE) is signalled.  The value 0 is returned.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine provides a convenient way to find the head of a list
C     in a doubly linked list pool.  The need to find the head of a
C     list arises in applications such as buffer management.  For
C     example, in a system using a "least recently used" buffer
C     replacement policy, the head of a list may point to the most
C     recently accessed buffer element.
C
C$ Examples
C
C     1)  If POOL is a doubly linked list pool that contains the list
C
C            3 <--> 7 <--> 1 <--> 44
C
C         any of function references
C
C            HEAD = LNKHL ( 3,  POOL )
C            HEAD = LNKHL ( 7,  POOL )
C            HEAD = LNKHL ( 44, POOL )
C
C         will assign the value 3 to HEAD.
C
C
C     2)  If POOL is a doubly linked list pool that contains the
C         singleton list consisting of the allocated node
C
C            44
C
C         the function reference
C
C            HEAD = LNKHL ( 44, POOL )
C
C         will assign the value 44 to HEAD.
C
C$ Restrictions
C
C     Linked list pools must be initialized via the routine
C     LNKINI.  Failure to initialize a linked list pool
C     will almost certainly lead to confusing results.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB) (WLT)
C
C-&
 
C$ Index_Entries
C
C     return head of linked list
C
C-&
 
C
C     Local parameters
C
 
C
C     The control area contains 3 elements.  They are:
C
C        The "size" of the pool, that is, the number
C        of nodes in the pool.
C
C        The number of free nodes in the pool.
C
C        The "free pointer," which is the column index of the first free
C        node.
C
C     Parameters defining the row and column indices of these control
C     elements are given below.
C
      INTEGER               SIZROW
      PARAMETER           ( SIZROW =   1 )
 
      INTEGER               SIZCOL
      PARAMETER           ( SIZCOL =   0 )
 
      INTEGER               NFRROW
      PARAMETER           ( NFRROW =   2 )
 
      INTEGER               NFRCOL
      PARAMETER           ( NFRCOL =   0 )
 
      INTEGER               FREROW
      PARAMETER           ( FREROW =   1 )
 
      INTEGER               FRECOL
      PARAMETER           ( FRECOL =  -1 )
 
C
C     Each assigned node consists of a backward pointer and a forward
C     pointer.
C
C        +-------------+       +-------------+       +-------------+
C        |  forward--> |       |  forward--> |       |  forward--> |
C        +-------------+  ...  +-------------+  ...  +-------------+
C        | <--backward |       | <--backward |       | <--backward |
C        +-------------+       +-------------+       +-------------+
C
C            node 1                 node I              node SIZE
C
C
C
      INTEGER               FORWRD
      PARAMETER           ( FORWRD  = 1 )
 
      INTEGER               BCKWRD
      PARAMETER           ( BCKWRD  = 2 )
 
C
C     Free nodes say that that's what they are.  The way they say it
C     is by containing the value FREE in their backward pointers.
C     Needless to say, FREE is a value that cannot be a valid pointer.
C
      INTEGER               FREE
      PARAMETER           ( FREE   = 0 )
 
 
 
C
C     Local variables
C
      INTEGER               PREV
 
 
 
C
C     If the node is non-positive, we regard it as the nil node.
C
      IF ( NODE .LT. 1 ) THEN
 
         LNKHL = 0
         RETURN
 
C
C     If the node is out of range, something's very wrong.
C
      ELSE IF  ( NODE .GT. POOL( SIZROW, SIZCOL ) ) THEN
 
         LNKHL = 0
 
         CALL CHKIN  ( 'LNKHL'                              )
         CALL SETMSG ( 'NODE was #; valid range is 1 to #.' )
         CALL ERRINT ( '#', NODE                            )
         CALL ERRINT ( '#', POOL(SIZROW,SIZCOL)             )
         CALL SIGERR ( 'SPICE(INVALIDNODE)'                 )
         CALL CHKOUT ( 'LNKHL'                              )
         RETURN
 
C
C     We don't do free nodes.
C
      ELSE IF (  POOL( BCKWRD, NODE )  .EQ.  FREE  ) THEN
 
         LNKHL = 0
 
         CALL CHKIN  ( 'LNKHL'                                )
         CALL SETMSG ( 'NODE was #; backward pointer = #; '   //
     .                 'forward pointer = #. "FREE" is #)'    )
         CALL ERRINT ( '#', NODE                              )
         CALL ERRINT ( '#', POOL(BCKWRD,NODE)                 )
         CALL ERRINT ( '#', POOL(FORWRD,NODE)                 )
         CALL ERRINT ( '#', FREE                              )
         CALL SIGERR ( 'SPICE(UNALLOCATEDNODE)'               )
         CALL CHKOUT ( 'LNKHL'                                )
         RETURN
 
      END IF
 
 
C
C     Find the head of the list.
C
      LNKHL = NODE
      PREV  = POOL ( BCKWRD, NODE )
 
      DO WHILE ( PREV .GT. 0 )
 
         LNKHL = PREV
         PREV  = POOL ( BCKWRD, LNKHL )
 
      END DO
 
C
C     LNKHL is now the head of the list.
C
      RETURN
      END
