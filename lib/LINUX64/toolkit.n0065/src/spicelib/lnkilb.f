C$Procedure      LNKILB ( LNK, insert list before node )
 
      SUBROUTINE LNKILB ( LIST, NEXT, POOL )
 
C$ Abstract
C
C     Insert the list containing a specified node into a another list,
C     preceding a specified node.
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
 
      INTEGER               LIST
      INTEGER               NEXT
      INTEGER               POOL ( 2,  LBPOOL : * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     LIST       I   Node in the list to be inserted.
C     NEXT       I   Node before which a new list is to be inserted.
C     POOL      I-O  A doubly linked list pool.
C     LBPOOL     P   Lower bound of pool column indices.
C
C$ Detailed_Input
C
C     LIST           is a node in the list to be inserted.  The entire
C                    list containing LIST is to be inserted into the
C                    list containing NEXT.  The inserted list will be
C                    located between NEXT and its predecessor, if any.
C
C     NEXT           is a node in a list.  NEXT is permitted to be
C                    nil, in which case POOL is not modified.
C
C     POOL           is a doubly linked list pool.
C
C$ Detailed_Output
C
C     POOL           is the input pool, with the following
C                    modifications:
C
C                       Let HEAD and TAIL be the head and tail nodes of
C                       the list containing LIST.  Then on output
C
C                          -- The successor of TAIL is NEXT.
C
C                          -- The predecessor of NEXT is TAIL.
C
C
C                       Let PREV be the node that on input was the
C                       predecessor of NEXT; if PREV exists, then on
C                       output
C
C                          -- The successor of PREV is HEAD.
C
C                          -- The predecessor of HEAD is PREV.
C
C                       If PREV is nil, the backward pointer of the
C                       inserted sublist is set to the negative of
C                       the tail of the list containing NEXT.
C
C$ Parameters
C
C     LBPOOL        is the lower bound of the column indices of the POOL
C                   array.  The columns indexed LBPOOL to 0 are reserved
C                   as a control area for the pool.
C
C$ Exceptions
C
C     1)  If LIST is not a valid node number, the error
C         SPICE(INVALIDNODE) will be signalled.  POOL will not be
C         modified.
C
C     2)  If NEXT is positive but is not a valid node number, the error
C         SPICE(INVALIDNODE) will be signalled.  POOL will not be
C         modified.
C
C     3)  It is not an error for NEXT to be non-positive; if it is,
C         the call to this routine does not affect the pool.
C
C     4)  If either of LIST or NEXT are valid node numbers but are
C         not allocated, the error SPICE(UNALLOCATEDNODE) will be
C         signalled.  POOL will not be modified.
C
C     5)  If LIST belongs to the same list as does NEXT, this routine
C         may fail in mysterious ways.  For efficiency, this error
C         condition is not checked.
C
C     For efficiency, discovery check-in is used in this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is used for augmenting lists by inserting other
C     lists into them.  The case of insertion of a single allocated
C     node is not special:  this is insertion of a singleton list.
C
C     To insert a list into a list AFTER a specified element, use the
C     routine LNKILA.
C
C$ Examples
C
C     1)  Let POOL be a doubly linked list pool that contains the lists
C
C             3 <--> 7 <--> 1    and    500 <--> 2 <--> 80
C
C         To insert the second list into the first before node 7, use
C         the call
C
C             CALL LNKILB ( 500, 7, POOL )
C
C         The resulting list will be:
C
C             3 <--> 500 <--> 2 <--> 80 <--> 7 <--> 1
C
C
C     2)  Let POOL be a doubly linked list pool that contains 5 nodes.
C         The sequence of calls
C
C            HEAD = 0
C
C            DO I = 1, 5
C               CALL LNKAN  ( POOL, NODE       )
C               CALL LNKILB ( NODE, HEAD, POOL )
C               HEAD = NODE
C            END DO
C
C         builds the list
C
C             5 <--> 4 <--> 3 <--> 2 <--> 1
C
C         Note that the first call to LNKILB does not cause an error
C         to be signalled, even though HEAD is 0 at that point.
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
C     insert sublist into linked list before a node
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
      INTEGER               HEAD
      INTEGER               PREV
      INTEGER               TAIL
 
 
C
C     Use discovery check-in.
C
 
C
C     If NEXT is non-positive, return now.
C
      IF ( NEXT .LE. 0 ) THEN
         RETURN
      END IF
 
C
C     If we arrived here, NEXT and LIST must be valid node numbers.
C     These nodes must be allocated as well.
C
      IF (      ( NEXT .GT. POOL(SIZROW,SIZCOL) )
     .     .OR. ( LIST .LT. 1                   )
     .     .OR. ( LIST .GT. POOL(SIZROW,SIZCOL) )  )  THEN
 
         CALL CHKIN  ( 'LNKILB'                                        )
         CALL SETMSG ( 'NEXT was #.  LIST was #. '                    //
     .                 'Valid range is 1 to #.'                        )
         CALL ERRINT ( '#', NEXT                                       )
         CALL ERRINT ( '#', LIST                                       )
         CALL ERRINT ( '#', POOL(SIZROW, SIZCOL)                       )
         CALL SIGERR ( 'SPICE(INVALIDNODE)'                            )
         CALL CHKOUT ( 'LNKILB'                                        )
         RETURN
 
 
      ELSE IF (       ( POOL(BCKWRD,NEXT)  .EQ.  FREE )
     .          .OR.  ( POOL(BCKWRD,LIST)  .EQ.  FREE )  ) THEN
 
         CALL CHKIN  ( 'LNKILB'                                        )
         CALL SETMSG ( 'Node NEXT: node number = #; '                 //
     .                 'backward pointer = #;  '                      //
     .                 'forward pointer = #. '                        //
     .                 'Node LIST: node number = #; '                 //
     .                 'backward pointer = #;  '                      //
     .                 'forward pointer = #. '                        //
     .                 '("FREE" is #)'                                 )
         CALL ERRINT ( '#', NEXT                                       )
         CALL ERRINT ( '#', POOL(BCKWRD,NEXT)                          )
         CALL ERRINT ( '#', POOL(FORWRD,NEXT)                          )
         CALL ERRINT ( '#', LIST                                       )
         CALL ERRINT ( '#', POOL(BCKWRD,LIST)                          )
         CALL ERRINT ( '#', POOL(FORWRD,LIST)                          )
         CALL ERRINT ( '#', FREE                                       )
         CALL SIGERR ( 'SPICE(UNALLOCATEDNODE)'                        )
         CALL CHKOUT ( 'LNKILB'                                        )
         RETURN
 
      END IF
 
C
C     Find the head and tail of the list containing LIST.
C
      HEAD = LIST
 
      DO WHILE ( POOL(BCKWRD,HEAD) .GT. 0 )
         HEAD = POOL(BCKWRD,HEAD)
      END DO
 
      TAIL = -POOL(BCKWRD,HEAD)
 
C
C     Let PREV be the backward pointer of NEXT.
C
C     Insert TAIL before NEXT.
C
C     If NEXT has a predecessor, HEAD follows it.
C
C     If NEXT has no predecessor, HEAD is the new head of the list.
C     The forward pointer of the tail of the merged list should
C     be set to -HEAD.
C
C     In either case, the backward pointer of HEAD should be set
C     to the backward pointer of NEXT.
C
C
      PREV                  =  POOL( BCKWRD, NEXT )
      POOL( FORWRD, TAIL )  =  NEXT
      POOL( BCKWRD, NEXT )  =  TAIL
 
      IF ( PREV .GT. 0 ) THEN
         POOL( FORWRD,  PREV )  =   HEAD
      ELSE
         POOL( FORWRD, -PREV )  =  -HEAD
      END IF
 
      POOL( BCKWRD, HEAD )  =  PREV
 
      RETURN
      END
