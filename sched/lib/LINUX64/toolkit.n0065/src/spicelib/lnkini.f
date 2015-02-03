C$Procedure      LNKINI ( LNK, initialize )
 
      SUBROUTINE LNKINI ( SIZE, POOL )
 
C$ Abstract
C
C     Initialize a doubly linked list pool.
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
 
      INTEGER               SIZE
      INTEGER               POOL ( 2,  LBPOOL : * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     SIZE       I   Number of nodes in the pool.
C     POOL      I-O  An array that is a linked pool on output.
C     LBPOOL     P   Lower bound of pool column indices.
C
C$ Detailed_Input
C
C     SIZE           is the number of nodes in the pool.
C
C     POOL           is an integer array that will contain the linked
C                    pool on output.
C
C$ Detailed_Output
C
C     POOL           is an initialized doubly linked list pool.
C                    The status of the pool is as follows:
C
C                      --  All nodes in the pool are on the free list.
C
C                      --  The free pointer indicates the first node.
C
C                      --  The total node count is set to the input
C                          value, SIZE.
C
C                      --  The free node count is the input value, SIZE.
C
C$ Parameters
C
C     LBPOOL        is the lower bound of the column indices of the POOL
C                   array.  The columns indexed LBPOOL to 0 are reserved
C                   as a control area for the pool.
C
C$ Exceptions
C
C     1)  If the requested number of nodes is nonpositive, the error
C         SPICE(INVALIDCOUNT) is signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     LNKINI must be called once to initialize a doubly linked list
C     pool before the pool is used.  LNKINI can be called at any time
C     to re-initialize a doubly linked list pool.
C
C     The functions
C
C        LNKNFN ( LNK, number of free nodes ) and
C        LNKSIZ ( LNK, size of pool )
C
C     will both return the value PLSIZE if called immediately after a
C     call to LNKINI.
C
C$ Examples
C
C     1)  Let POOL be a doubly linked list pool with a maximum of
C         100 nodes.  POOL should be declared as follows:
C
C            INTEGER               LBPOOL
C            PARAMETER           ( LBPOOL = -5 )
C
C            INTEGER               PLSIZE
C            PARAMETER           ( PLSIZE = 100 )
C
C            INTEGER               POOL ( 2, LBPOOL : PLSIZE )
C
C
C         To initialize POOL, use the call
C
C            CALL LNKINI ( PLSIZE, POOL )
C
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
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB) (WLT)
C
C-&
 
C$ Index_Entries
C
C     initialize linked list pool
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
      INTEGER               I
 
 
C
C     Use discovery check-in.
C
 
C
C     The requested number of nodes must be valid.
C
      IF ( SIZE .LT. 1 ) THEN
 
         CALL CHKIN  ( 'LNKINI'                             )
         CALL SETMSG ( 'A linked list cannot have # nodes.' )
         CALL ERRINT ( '#', SIZE                            )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                )
         CALL CHKOUT ( 'LNKINI'                             )
         RETURN
 
      END IF
 
C
C     Initialize the pool.  The free list occupies the whole pool at
C     this point.
C
 
C
C     POOL( SIZROW, SIZCOL ) is the pool size.
C
      POOL( SIZROW, SIZCOL )  =  SIZE
 
C
C     POOL( NFRROW, NFRCOL ) is the number of free nodes.
C
      POOL( NFRROW, NFRCOL )  =  SIZE
 
C
C     POOL( FREROW, FRECOL) is the "free" pointer.  It points to the
C     first free node, which is node 1.
C
      POOL( FREROW, FRECOL)   =  1
 
C
C     Initialize the backward and forward pointers.  The last forward
C     pointer is zero.  All of the backward pointers contain the value
C     FREE.
C
      DO I = 1, SIZE - 1
         POOL ( FORWRD, I )   =  I + 1
         POOL ( BCKWRD, I )   =  FREE
      END DO
 
      POOL ( FORWRD, SIZE )   =  0
      POOL ( BCKWRD, SIZE )   =  FREE
 
      RETURN
      END
