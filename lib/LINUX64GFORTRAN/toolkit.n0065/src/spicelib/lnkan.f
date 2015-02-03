C$Procedure      LNKAN  ( LNK, allocate node )
 
      SUBROUTINE LNKAN ( POOL, NEW )
 
C$ Abstract
C
C     Allocate a node in a doubly linked list pool.
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
 
      INTEGER               POOL ( 2,  LBPOOL : * )
      INTEGER               NEW
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     POOL      I-O  A doubly linked list pool.
C     NEW        O   Number of new node that was allocated.
C     LBPOOL     P   Lower bound of pool column indices.
C
C$ Detailed_Input
C
C     POOL           is a doubly linked list pool.
C
C$ Detailed_Output
C
C     POOL           is the input pool, with the following
C                    modifications:
C
C                       -- NEW is an allocated node:  both the forward
C                          and backward pointers of NEW are -NEW.
C
C                       -- The node that was the successor of NEW on
C                          input is the head of the free list on output.
C
C
C     NEW            is the number of the newly allocated node.
C
C$ Parameters
C
C     LBPOOL        is the lower bound of the column indices of the POOL
C                   array.  The columns indexed LBPOOL to 0 are reserved
C                   as a control area for the pool.
C
C$ Exceptions
C
C     1)  If no free nodes are available for allocation, the error
C         SPICE(NOFREENODES) is signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     In a doubly linked list pool, an `allocated node' is one that has
C     been removed from the free list.  An allocated node may be linked
C     to other nodes or may be unlinked; in the latter case, both the
C     forward and backward pointers of the node will be the negative of
C     the node number.
C
C     A node must be allocated before it can be linked to another
C     node.
C
C$ Examples
C
C     1)  Let POOL be a doubly linked list pool.  To build a new list
C         of ten nodes, the code fragment below can be used:
C
C            C
C            C     We'll use LNKILA ( LNK, insert list after
C            C     a specified node ) to add nodes to the tail of the
C            C     list.
C            C
C                  PREV = 0
C
C                  DO I = 1, 10
C
C                     CALL LNKAN  ( POOL, NODE       )
C                     CALL LNKILA ( PREV, NODE, POOL )
C                     PREV = NODE
C
C                  END DO
C
C
C     2)  In this version of example (1), we check that a sufficient
C         number of free nodes are available before building the list:
C
C            C
C            C     Make sure we have 10 free nodes available.
C            C     Signal an error if not.  Use LNKNFN to obtain
C            C     the number of free nodes.
C            C
C                  IF ( LNKNFN(POOL) .LT. 10 ) THEN
C
C                     CALL SETMSG ( 'Only # free nodes are available '//
C                 .                 'but 10 are required.'            )
C                     CALL ERRINT ( '#', LNKNFN(POOL)                 )
C                     CALL SIGERR ( 'POOL_TOO_SMALL'                  )
C                     RETURN
C
C                  END IF
C
C                     [ Build list ]
C                           .
C                           .
C                           .
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
C     allocate node from linked list pool
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
C     Discovery check-in is used in place of standard SPICE error
C     handling.
C
      IF (  POOL( NFRROW, NFRCOL )  .EQ.  0  ) THEN
 
         CALL CHKIN  ( 'LNKAN'               )
         CALL SETMSG ( 'There are no free nodes left for '
     .   //            'allocating in the supplied linked list '
     .   //            'pool. ' )
         CALL SIGERR ( 'SPICE(NOFREENODES)'  )
         CALL CHKOUT ( 'LNKAN'               )
         RETURN
 
      END IF
 
C
C     The caller gets the first free node.  The forward pointer of
C     this node indicates the next free node.  After this, there's one
C     less free node.
C
      NEW                      =   POOL( FREROW, FRECOL )
 
      POOL( FREROW, FRECOL )   =   POOL( FORWRD, NEW    )
      POOL( NFRROW, NFRCOL )   =   POOL( NFRROW, NFRCOL ) - 1
 
C
C     The forward and backward pointers of the allocated node become
C     the negatives of the node numbers of the head and tail nodes
C     of the list containing NEW.  Since this is a singleton list,
C     both pointers are -NEW.
C
      POOL( FORWRD, NEW )   =   -NEW
      POOL( BCKWRD, NEW )   =   -NEW
 
      RETURN
      END
