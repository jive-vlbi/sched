C$Procedure      LNKILA ( LNK, insert list after node )
 
      SUBROUTINE LNKILA ( PREV, LIST, POOL )
 
C$ Abstract
C
C     Insert the list containing a specified node into a another list,
C     following a specified node.
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
 
      INTEGER               PREV
      INTEGER               LIST
      INTEGER               POOL ( 2,  LBPOOL : * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     PREV       I   Node after which a new list is to be inserted.
C     LIST       I   Node in the list to be inserted.
C     POOL      I-O  A doubly linked list pool.
C     LBPOOL     P   Lower bound of pool column indices.
C
C$ Detailed_Input
C
C     PREV           is a node in a list.  PREV is permitted to be
C                    nil, in which case POOL is not modified.
C
C     LIST           is a node in the list to be inserted.  The entire
C                    list containing the node LIST is to be inserted
C                    into the list containing PREV.  The inserted list
C                    will be located between PREV and its successor,
C                    if any.
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
C                          -- The successor of PREV is HEAD.
C                          -- The predecessor of HEAD is PREV.
C
C
C                       Let NEXT be the node that on input was the
C                       successor of PREV; if NEXT exists, then on
C                       output
C
C                          -- The successor of TAIL is NEXT.
C                          -- The predecessor of NEXT is TAIL.
C
C                       If NEXT is nil, the forward pointer of the
C                       inserted sublist is set to the negative of
C                       the head of the list containing PREV.
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
C     2)  If PREV is positive but is not a valid node number, the error
C         SPICE(INVALIDNODE) will be signalled.  POOL will not be
C         modified.
C
C     3)  It is not an error for PREV to be non-positive; if it is,
C         the call to this routine does not affect the pool.
C
C     4)  If either of PREV or LIST are valid node numbers but are
C         not allocated, the error SPICE(UNALLOCATEDNODE) will be
C         signalled.  POOL will not be modified.
C
C     5)  If LIST belongs to the same list as does PREV, this routine
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
C     To insert a list into a list BEFORE a specified element, use the
C     routine LNKILB.
C
C$ Examples
C
C     1)  Let POOL be a doubly linked list pool that contains the lists
C
C             3 <--> 7 <--> 1    and    500 <--> 2 <--> 80
C
C         To insert the second list into the first after node 7, use the
C         call
C
C             CALL LNKILA ( 7, 500, POOL )
C
C         The resulting list will be:
C
C             3 <--> 7 <--> 500 <--> 2 <--> 80 <--> 1
C
C
C     2)  Let POOL be a doubly linked list pool that contains 5 nodes.
C         The sequence of calls
C
C            TAIL = 0
C
C            DO I = 1, 5
C               CALL LNKAN  ( POOL, NODE       )
C               CALL LNKILA ( TAIL, NODE, POOL )
C               TAIL = NODE
C            END DO
C
C         builds the list
C
C             1 <--> 2 <--> 3 <--> 4 <--> 5
C
C         Note that the first call to LNKILA does not cause an error
C         to be signalled, even though TAIL is 0 at that point.
C
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
C     insert sublist into linked list after a node
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
      INTEGER               NEXT
      INTEGER               HEAD
      INTEGER               TAIL
 
 
C
C     Use discovery check-in.
C
 
C
C     If PREV is non-positive, return now.
C
      IF ( PREV .LE. 0 ) THEN
         RETURN
      END IF
 
C
C     At this point, PREV and LIST must be a valid node numbers, and
C     both PREV and LIST must be allocated as well.
C
      IF (      ( PREV .GT. POOL(SIZROW,SIZCOL) )
     .     .OR. ( LIST .LT. 1                   )
     .     .OR. ( LIST .GT. POOL(SIZROW,SIZCOL) )  )  THEN
 
         CALL CHKIN  ( 'LNKILA'                                        )
         CALL SETMSG ( 'PREV was #.  LIST was #. '                    //
     .                 'Valid range is 1 to #.'                        )
         CALL ERRINT ( '#', PREV                                       )
         CALL ERRINT ( '#', LIST                                       )
         CALL ERRINT ( '#', POOL(SIZROW, SIZCOL)                       )
         CALL SIGERR ( 'SPICE(INVALIDNODE)'                            )
         CALL CHKOUT ( 'LNKILA'                                        )
         RETURN
 
 
      ELSE IF (       ( POOL(BCKWRD,PREV)  .EQ.  FREE )
     .          .OR.  ( POOL(BCKWRD,LIST)  .EQ.  FREE )  ) THEN
 
         CALL CHKIN  ( 'LNKILA'                                        )
         CALL SETMSG ( 'Node PREV: node number = #; '                 //
     .                 'backward pointer = #;  '                      //
     .                 'forward pointer = #. '                        //
     .                 'Node LIST: node number = #; '                 //
     .                 'backward pointer = #;  '                      //
     .                 'forward pointer = #. '                        //
     .                 '("FREE" is #)'                                 )
         CALL ERRINT ( '#', PREV                                       )
         CALL ERRINT ( '#', POOL(BCKWRD,PREV)                          )
         CALL ERRINT ( '#', POOL(FORWRD,PREV)                          )
         CALL ERRINT ( '#', LIST                                       )
         CALL ERRINT ( '#', POOL(BCKWRD,LIST)                          )
         CALL ERRINT ( '#', POOL(FORWRD,LIST)                          )
         CALL ERRINT ( '#', FREE                                       )
         CALL SIGERR ( 'SPICE(UNALLOCATEDNODE)'                        )
         CALL CHKOUT ( 'LNKILA'                                        )
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
C     Let NEXT be the forward pointer of PREV.
C
C     Insert HEAD after PREV.
C
C     If PREV has a successor, TAIL precedes it.
C
C     If PREV has no successor, TAIL is the new tail of the list.
C     The backward pointer of the head of the merged list should
C     be set to -TAIL.
C
C     In either case, the forward pointer of TAIL should be set
C     to the forward pointer of PREV.
C
      NEXT                  =  POOL( FORWRD, PREV )
      POOL( FORWRD, PREV )  =  HEAD
      POOL( BCKWRD, HEAD )  =  PREV
 
      IF ( NEXT .GT. 0 ) THEN
         POOL( BCKWRD,  NEXT )  =   TAIL
      ELSE
         POOL( BCKWRD, -NEXT )  =  -TAIL
      END IF
 
      POOL( FORWRD, TAIL )  =  NEXT
 
      RETURN
      END
