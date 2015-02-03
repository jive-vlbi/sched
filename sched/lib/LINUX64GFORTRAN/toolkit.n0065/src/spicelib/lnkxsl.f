C$Procedure      LNKXSL ( LNK, extract sublist from list  )
 
      SUBROUTINE LNKXSL ( HEAD, TAIL, POOL )
 
C$ Abstract
C
C     Extract a specified sublist from a list.
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
 
      INTEGER               HEAD
      INTEGER               TAIL
      INTEGER               POOL ( 2,  LBPOOL : * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HEAD,
C     TAIL       I   Head and tail nodes of a sublist to be extracted.
C     POOL      I-O  A doubly linked list pool.
C
C$ Detailed_Input
C
C     HEAD,
C     TAIL           are, respectively, the head and tail nodes of a
C                    sublist to be extracted.
C
C     POOL           is a doubly linked list pool.
C
C$ Detailed_Output
C
C     POOL           is the input pool, with the following
C                    modifications:
C
C                       -- The sublist bounded by HEAD and
C                          by TAIL is now a separate list from
C                          the list that originally contained it.
C
C                       If on input, HEAD was preceded by the node
C                       PREV, and tail was followed by the node
C                       NEXT, then on output
C
C                       -- The successor of PREV is NEXT.
C                       -- The predecessor of NEXT is PREV.
C
C
C$ Parameters
C
C     LBPOOL        is the lower bound of the column indices of the POOL
C                   array.  The columns indexed LBPOOL to 0 are reserved
C                   as a control area for the pool.
C
C$ Exceptions
C
C     1)  If either of HEAD or TAIL are not valid node numbers, the
C         error SPICE(INVALIDNODE) will be signalled.  POOL will not be
C         modified.
C
C     2)  If either of HEAD or TAIL are valid node numbers but are
C         not allocated, the error SPICE(UNALLOCATEDNODE) will be
C         signalled.  POOL will not be modified.
C
C     3)  If TAIL cannot be reached by forward traversal of the list
C         containing HEAD, the error SPICE(INVALIDSUBLIST) is signalled.
C         POOL will not be modified.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Extracting a sublist from a list is necessary when a list is
C     to be re-arranged in some way.  For example, to move a node
C     in a list to the head of the list, the node (which is a
C     singleton sublist) is first extracted from the list containing
C     it, then inserted before the head of the list.
C
C$ Examples
C
C     1)  Let POOL be a doubly linked list pool, and let
C
C            9 <--> 8 <--> 4 <--> 2000 <--> 1
C
C         be a list in POOL.  To extract the sublist
C
C            4 <--> 2000
C
C         the call
C
C            CALL LNKXSL ( 4, 2000, POOL )
C
C         can be used.  After the call is made, POOL will contain the
C         separate lists
C
C            9 <--> 8 <--> 1
C
C         and
C
C            4 <--> 2000
C
C
C     2)  Let POOL be a doubly linked list pool, and let
C
C            9 <--> 8 <--> 4 <--> 2000 <--> 1
C
C         be a list in POOL.  To move the node 2000 to the
C         head of the list, the code fragment
C
C            CALL LNKXSL ( 2000, 2000, POOL )
C            CALL LNKILB ( 2000, 9,    POOL )
C
C         can be used.  The resulting list will be
C
C            2000 <--> 9 <--> 8 <--> 4 <--> 1
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
C     extract sublist of linked list
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
      INTEGER               NODE
      INTEGER               PREV
 
 
C
C     HEAD and TAIL must be valid node numbers.  These nodes
C     must be allocated as well.
C
      IF (      ( HEAD .LT. 1                   )
     .     .OR. ( HEAD .GT. POOL(SIZROW,SIZCOL) )
     .     .OR. ( TAIL .LT. 1                   )
     .     .OR. ( TAIL .GT. POOL(SIZROW,SIZCOL) )  )  THEN
 
         CALL CHKIN  ( 'LNKXSL'                                        )
         CALL SETMSG ( 'HEAD was #.  TAIL was #. '                    //
     .                 'Valid range is 1 to #.'                        )
         CALL ERRINT ( '#', HEAD                                       )
         CALL ERRINT ( '#', TAIL                                       )
         CALL ERRINT ( '#', POOL(SIZROW, SIZCOL)                       )
         CALL SIGERR ( 'SPICE(INVALIDNODE)'                            )
         CALL CHKOUT ( 'LNKXSL'                                        )
         RETURN
 
 
      ELSE IF (       ( POOL(BCKWRD,HEAD)  .EQ.  FREE )
     .          .OR.  ( POOL(BCKWRD,TAIL)  .EQ.  FREE )  ) THEN
 
         CALL CHKIN  ( 'LNKXSL'                                        )
         CALL SETMSG ( 'Node HEAD: node number = #; '                 //
     .                 'backward pointer = #;  '                      //
     .                 'forward pointer = #. '                        //
     .                 'Node TAIL: node number = #; '                 //
     .                 'backward pointer = #;  '                      //
     .                 'forward pointer = #. '                        //
     .                 '("FREE" is #)'                                 )
         CALL ERRINT ( '#', HEAD                                       )
         CALL ERRINT ( '#', POOL(BCKWRD,HEAD)                          )
         CALL ERRINT ( '#', POOL(FORWRD,HEAD)                          )
         CALL ERRINT ( '#', TAIL                                       )
         CALL ERRINT ( '#', POOL(BCKWRD,TAIL)                          )
         CALL ERRINT ( '#', POOL(FORWRD,TAIL)                          )
         CALL ERRINT ( '#', FREE                                       )
         CALL SIGERR ( 'SPICE(UNALLOCATEDNODE)'                        )
         CALL CHKOUT ( 'LNKXSL'                                        )
         RETURN
 
      END IF
 
C
C     Starting at HEAD, search forward, looking for TAIL (apologies to
C     ZZ Top).
C
      NODE = HEAD
 
      DO WHILE (  ( NODE .NE. TAIL ) .AND. ( NODE .GT. 0 )  )
         NODE = POOL ( FORWRD, NODE )
      END DO
 
C
C     If we didn't find TAIL, that's an error.
C
      IF ( NODE .NE. TAIL ) THEN
 
         CALL CHKIN  ( 'LNKXSL'                                        )
         CALL SETMSG ( 'Node # cannot be found by forward '           //
     .                 'traversal, starting at node #.'                )
         CALL ERRINT ( '#', TAIL                                       )
         CALL ERRINT ( '#', HEAD                                       )
         CALL SIGERR ( 'SPICE(INVALIDSUBLIST)'                         )
         CALL CHKOUT ( 'LNKXSL'                                        )
         RETURN
 
      END IF
 
C
C     We reached TAIL.  Extract the sublist between HEAD and TAIL
C     inclusive.
C
C     Find the predecessor of HEAD and the successor of TAIL.
C
      PREV = POOL ( BCKWRD, HEAD )
      NEXT = POOL ( FORWRD, TAIL )
 
C
C     If the input list did not start with HEAD, then we must update
C     the forward pointer of the tail node, as well as the backward
C     pointer of the head node, of the sublist that preceded HEAD.
C
      IF ( PREV .GT. 0 ) THEN
C
C        Update the forward pointer of PREV with the forward pointer of
C        TAIL.
C
C        If TAIL had a successor, the predecessor of HEAD will now
C        point forward to it.  If TAIL was the tail of the input list,
C        the forward pointer of TAIL was the negative of the head of
C        the input list---this is the correct forward pointer for the
C        predecessor of HEAD in this case, since the predecessor of
C        HEAD will become the tail of the main list after the sublist
C        ranging from HEAD to TAIL is removed.
C
         POOL ( FORWRD, PREV )   =   NEXT
 
C
C        If TAIL is the tail of the input list, we must update the
C        backward pointer of the head of the input list to point to
C        the negative of the new tail of the list, which now is PREV.
C
         IF ( NEXT .LE. 0 ) THEN
C
C           In this case, we can read off the number of the head
C           node from NEXT:  it is just -NEXT.
C
            POOL ( BCKWRD, -NEXT )  =  -PREV
 
         END IF
 
      END IF
 
C
C     The portion of the input list that preceded HEAD (if such
C     portion existed) has now been taken care of.
C
C     We now must perform the analogous updates to the portion of
C     the input list that followed TAIL.
C
C     If the input list did not end with TAIL, then we must update
C     the backward pointer of the head node, as well as the forward
C     pointer of the tail node, of the sublist that followed TAIL.
C
      IF ( NEXT .GT. 0 ) THEN
C
C        Update the backward pointer of NEXT with the backward pointer
C        of HEAD.
C
C        If HEAD had a predecessor, the successor of TAIL will now
C        point backward to it.  If HEAD was the head of the input list,
C        the backward pointer of HEAD was the negative of the tail of
C        the input list---this is the correct backward pointer for the
C        successor of TAIL in this case, since the successor of TAIL
C        will become the head of the main list after the sublist
C        ranging from HEAD to TAIL is removed.
C
         POOL ( BCKWRD, NEXT )   =  PREV
 
C
C        If HEAD is the head of the input list, we must update the
C        forward pointer of the tail of the input list to point to
C        the negative of the new head of the list, which now is NEXT.
C
         IF ( PREV .LE. 0 ) THEN
C
C           In this case, we can read off the number of the tail
C           node from PREV:  it is just -PREV.
C
            POOL ( FORWRD, -PREV )  =  -NEXT
 
         END IF
 
      END IF
C
C     The portion of the input list that followed TAIL (if such
C     portion existed) has now been taken care of.
C
 
 
C
C     Cauterize the sublist.
C
      POOL ( BCKWRD, HEAD )  =  -TAIL
      POOL ( FORWRD, TAIL )  =  -HEAD
 
 
      RETURN
      END
