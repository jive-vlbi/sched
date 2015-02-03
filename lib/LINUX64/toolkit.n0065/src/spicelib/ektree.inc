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
C
C     Include Section:  EK Tree Parameters
C
C        ektree.inc  Version 3    22-OCT-1995 (NJB)
C
C
C     The parameters in this file define the tree structure
C     used by the EK system.  This structure is a variant of the
C     B*-tree structure described in Knuth's book, that is
C
C         Knuth, Donald E.  "The Art of Computer Programming,
C         Volume 3/Sorting and Searching" 1973, pp 471-479.
C
C     The trees used in the EK system differ from generic B*-trees
C     primarily in the way keys are treated.  Rather than storing
C     unique primary key values in each node, EK trees store integer
C     counts that represent the ordinal position of each data value,
C     counting from the lowest indexed element in the subtree whose
C     root is the node in question.  Thus the keys are unique within
C     a node but not across multiple nodes:  in fact the Nth key in
C     every leaf node is N.  The absolute ordinal position of a data
C     item is defined recursively as the sum of the key of the data item
C     and the absolute ordinal position of the data item in the parent
C     node that immediately precedes all elements of the node in
C     question.  This data structure allows EK trees to support lookup
C     of data items based on their ordinal position in a data set.  The
C     two prime applications of this capability in the EK system are:
C
C        1) Using trees to index the records in a table, allowing
C           the Nth record to be located efficiently.
C
C        2) Using trees to implement order vectors that can be
C           maintained when insertions and deletions are done.
C
C
C
C                           Root node
C
C        +--------------------------------------------+
C        |              Tree version code             |
C        +--------------------------------------------+
C        |           Number of nodes in tree          |
C        +--------------------------------------------+
C        |            Number of keys in tree          |
C        +--------------------------------------------+
C        |                Depth of tree               |
C        +--------------------------------------------+
C        |            Number of keys in root          |
C        +--------------------------------------------+
C        |              Space for n keys,             |
C        |                                            |
C        |        n = 2 * INT( ( 2*m - 2 )/3 )        |
C        |                                            |
C        |  where m is the max number of children per |
C        |          node in the child nodes           |
C        +--------------------------------------------+
C        |        Space for n+1 child pointers,       |
C        |         where n is as defined above.       |
C        +--------------------------------------------+
C        |          Space for n data pointers,        |
C        |         where n is as defined above.       |
C        +--------------------------------------------+
C
C
C                           Child node
C
C        +--------------------------------------------+
C        |        Number of keys present in node      |
C        +--------------------------------------------+
C        |              Space for m-1 keys            |
C        +--------------------------------------------+
C        |         Space for m child pointers         |
C        +--------------------------------------------+
C        |         Space for m-1 data pointers        |
C        +--------------------------------------------+
C
C
C
C
C     The following parameters give the maximum number of children
C     allowed in the root and child nodes.  During insertions, the
C     number of children may overflow by 1.
C
C
C     Maximum number of children allowed in a child node:
C
      INTEGER               MXKIDC
      PARAMETER           ( MXKIDC = 63 )
 
C
C     Maximum number of keys allowed in a child node:
C
      INTEGER               MXKEYC
      PARAMETER           ( MXKEYC = MXKIDC - 1 )
 
C
C     Minimum number of children allowed in a child node:
C
      INTEGER               MNKIDC
      PARAMETER           ( MNKIDC =  ( 2*MXKIDC + 1 ) / 3  )
 
C
C     Minimum number of keys allowed in a child node:
C
      INTEGER               MNKEYC
      PARAMETER           ( MNKEYC =  MNKIDC - 1  )
 
C
C     Maximum number of children allowed in the root node:
C
      INTEGER               MXKIDR
      PARAMETER           ( MXKIDR =   2 * ( (2*MXKIDC - 2)/3 )  +  1  )
 
C
C     Maximum number of keys allowed in the root node:
C
      INTEGER               MXKEYR
      PARAMETER           ( MXKEYR =   MXKIDR - 1   )
 
C
C     Minimum number of children allowed in the root node:
C
      INTEGER               MNKIDR
      PARAMETER           ( MNKIDR =  2   )
 
 
C
C
C     The following parameters indicate positions of elements in the
C     tree node structures shown above.
C
C
C     The following parameters are for the root node only:
C
 
C
C     Location of version code:
C
      INTEGER               TRTYPE
      PARAMETER           ( TRTYPE = 1 )
 
C
C     Version code:
C
      INTEGER               TRVERS
      PARAMETER           ( TRVERS = 1 )
 
C
C     Location of node count:
C
      INTEGER               TRNNOD
      PARAMETER           ( TRNNOD = TRTYPE + 1 )
 
C
C     Location of total key count for the tree:
C
      INTEGER               TRNKEY
      PARAMETER           ( TRNKEY = TRNNOD + 1 )
 
C
C     Location of tree depth:
C
      INTEGER               TRDPTH
      PARAMETER           ( TRDPTH = TRNKEY + 1 )
 
C
C     Location of count of keys in root node:
C
      INTEGER               TRNKR
      PARAMETER           ( TRNKR  = TRDPTH + 1 )
 
C
C     Base address of keys in the root node:
C
      INTEGER               TRKEYR
      PARAMETER           ( TRKEYR = TRNKR )
 
C
C     Base address of child pointers in root node:
C
      INTEGER               TRKIDR
      PARAMETER           ( TRKIDR = TRKEYR + MXKEYR + 1 )
 
C
C     Base address of data pointers in the root node (allow room for
C     overflow):
C
      INTEGER               TRDATR
      PARAMETER           ( TRDATR = TRKIDR + MXKIDR + 1 )
 
C
C     Size of root node:
C
      INTEGER               TRSIZR
      PARAMETER           ( TRSIZR = TRDATR + MXKEYR + 1 )
 
 
 
C
C     The following parameters are for child nodes only:
C
C
C     Location of number of keys in node:
C
      INTEGER               TRNKC
      PARAMETER           ( TRNKC  =  1 )
 
C
C     Base address of keys in child nodes:
C
      INTEGER               TRKEYC
      PARAMETER           ( TRKEYC = TRNKC )
 
C
C     Base address of child pointers in child nodes:
C
      INTEGER               TRKIDC
      PARAMETER           ( TRKIDC = TRKEYC + MXKEYC + 1 )
 
C
C     Base address of data pointers in child nodes (allow room
C     for overflow):
C
      INTEGER               TRDATC
      PARAMETER           ( TRDATC = TRKIDC + MXKIDC + 1 )
 
C
C     Size of child node:
C
      INTEGER               TRSIZC
      PARAMETER           ( TRSIZC = TRDATC + MXKEYC + 1 )
 
C
C     A number of EK tree routines must declare stacks of fixed
C     depth; this depth limit imposes a limit on the maximum depth
C     that an EK tree can have.  Because of the large branching
C     factor of EK trees, the depth limit is of no practical
C     importance:  The number of keys that can be held in an EK
C     tree of depth N is
C
C                           N-1
C                     MXKIDC   -  1
C         MXKIDR  *   -------------
C                      MXKIDC  - 1
C
C
C     This formula yields a capacity of over 1 billion keys for a
C     tree of depth 6.
C
      INTEGER               TRMXDP
      PARAMETER           ( TRMXDP = 10 )
 
C
C     End Include Section:  EK Tree Parameters
C
