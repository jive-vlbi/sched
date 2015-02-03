C$Procedure      LNKNXT ( LNK, next node )
 
      INTEGER FUNCTION LNKNXT ( NODE, POOL )
 
C$ Abstract
C
C     Find the node following a specified node in a doubly linked list
C     pool.
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
 
      INTEGER               POOL ( 2,  LBPOOL :  *  )
      INTEGER               NODE
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NODE       I   Number of an allocated node.
C     POOL       I   A doubly linked list pool.
C     LBPOOL     P   Lower bound of pool column indices.
C
C     The function returns the number of the successor of the node
C     indicated by NODE.
C
C$ Detailed_Input
C
C     NODE           is the number of an allocated node in POOL.
C
C     POOL           is a doubly linked list pool.
C
C$ Detailed_Output
C
C     The function returns the number of the successor of the node
C     indicated by NODE.  If NODE is the tail node of a list, the
C     function returns the negative of the node number of the head
C     of the list.
C
C$ Parameters
C
C     LBPOOL        is the lower bound of the column indices of the POOL
C                   array.  The columns indexed LBPOOL to 0 are reserved
C                   as a control area for the pool.
C
C$ Exceptions
C
C     1)  If NODE is the tail node of a list, the function returns the
C         negative of the node number of the head of the list.
C
C     2)  If NODE is not a valid node number, the error
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
C     The raison d'etre of this routine is to allow forward traversal
C     of lists in a doubly linked list pool.
C
C     Traversing a list is often performed in cases where the list is
C     used to index elements of a data structure, and the elements
C     indexed by the list must be searched.
C
C     To traverse a list in backward order, use LNKPRV.
C
C$ Examples
C
C     1)  Let POOL be doubly linked list pool, and let
C
C           3 <--> 7 <--> 1
C
C         be a list in the pool.  The table below shows the effects
C         of function references to LNKNXT, where nodes in this list
C         are used as inputs:
C
C            Function reference               Value Returned
C            ------------------               --------------
C
C            LNKNXT ( 3, POOL )                     7
C            LNKNXT ( 7, POOL )                     1
C            LNKNXT ( 1, POOL )                    -3
C
C
C     2)  Forward traversal of a list:  Let POOL be a doubly linked
C         list pool, and let NODE be an allocated node in the pool.
C         To traverse the list containing NODE in forward order
C         and print out the nodes of the list, we can use the
C         following code fragment:
C
C            C
C            C     Find the head of the list containing NODE.
C            C
C                  NEXT = LNKHL ( NODE, POOL )
C
C            C
C            C     Traverse the list, printing out node numbers
C            C     as we go.
C            C
C                  WRITE (*,*) 'The list, in forward order, is: '
C
C                  DO WHILE ( NEXT .GT. 0 )
C
C                     WRITE (*,*) NEXT
C                     NEXT = LNKNXT ( NEXT, POOL )
C
C                  END DO
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
C     return next node in linked list
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
      PARAMETER           ( FREE   =  0  )
 
 
C
C     If the node is out of range, something's very wrong.
C
      IF (      ( NODE .LT. 1                       )
     .     .OR. ( NODE .GT. POOL( SIZROW, SIZCOL )  )  ) THEN
 
         LNKNXT = 0
 
         CALL CHKIN  ( 'LNKNXT'                             )
         CALL SETMSG ( 'NODE was #; valid range is 1 to #.' )
         CALL ERRINT ( '#', NODE                            )
         CALL ERRINT ( '#', POOL(SIZROW,SIZCOL)             )
         CALL SIGERR ( 'SPICE(INVALIDNODE)'                 )
         CALL CHKOUT ( 'LNKNXT'                             )
         RETURN
 
C
C     We don't do free nodes.
C
      ELSE IF (  POOL( BCKWRD, NODE )  .EQ.  FREE  ) THEN
 
         LNKNXT = 0
 
         CALL CHKIN  ( 'LNKNXT'                                        )
         CALL SETMSG ( 'NODE was #; backward pointer = #; '           //
     .                 'forward pointer = #. "FREE" is #)'             )
         CALL ERRINT ( '#', NODE                                       )
         CALL ERRINT ( '#', POOL(BCKWRD,NODE)                          )
         CALL ERRINT ( '#', POOL(FORWRD,NODE)                          )
         CALL ERRINT ( '#', FREE                                       )
         CALL SIGERR ( 'SPICE(UNALLOCATEDNODE)'                        )
         CALL CHKOUT ( 'LNKNXT'                                        )
         RETURN
 
      END IF
 
C
C     Just return the forward pointer of NODE.
C
      LNKNXT = POOL ( FORWRD, NODE )
 
      RETURN
      END
