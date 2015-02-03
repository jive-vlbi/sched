C$Procedure ZZHSC ( Private---Add-only Character Hash )

      SUBROUTINE ZZHSC ( HASHSZ, HEDLST, COLLST, ITEMS,
     .                   ITEM, ITEMAT, NEW, AVAIL )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Manipulate add-only character hash.
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
C     None.
C
C$ Keywords
C
C     PRIVATE UTILITY
C
C$ Declarations
 
      IMPLICIT NONE

      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )

      INTEGER               HASHSZ
      INTEGER               HEDLST  (          * )
      INTEGER               COLLST  ( LBPOOL : * )
      CHARACTER*(*)         ITEMS   (          * )
      CHARACTER*(*)         ITEM
      INTEGER               ITEMAT
      LOGICAL               NEW
      INTEGER               AVAIL
      
C$ Brief_I/O
C
C     Variable  I/O  Entry
C     --------  ---  --------------------------------------------------
C     HASHSZ     I   ZZHSCINI
C     HEDLST    I/O  ZZHSCINI, ZZHSCADD, ZZHSCCHK, ZZHSCINF
C     COLLST    I/O  ZZHSCINI, ZZHSCADD, ZZHSCCHK, ZZHSCAVL, ZZHSCINF
C     ITEMS     I/O  ZZHSCINI, ZZHSCADD, ZZHSCCHK, ZZHSCINF
C     ITEM       I   ZZHSCADD, ZZHSCCHK, ZZHSCINF
C     ITEMAT     O   ZZHSCADD, ZZHSCCHK
C     NEW        O   ZZHSCADD
C     AVAIL      O   ZZHSCAVL, ZZHSCINF
C
C     LBPOOL     P   (All)
C
C$ Detailed_Input
C
C     See the ENTRY points for a discussion of their arguments.
C
C$ Detailed_Output
C
C     See the ENTRY points for a discussion of their arguments.
C
C$ Parameters
C
C     LBPOOL      is the lower bound of the collision list array.
C
C$ Exceptions
C
C     1) If ZZHSC is called directly, the error SPICE(BOGUSENTRY) is
C        signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     ZZHSC should never be called directly, but should instead be
C     accessed only through its entry points.
C
C     The purpose of this routine is to manipulate add-only character
C     hashes used for buffering purposes by various SPICE subsystems.
C     This umbrella has the following entry points:
C
C           ZZHSCINI       Initialize a hash.
C
C           ZZHSCADD       Add an item to a hash.
C
C           ZZHSCCHK       Check if a item is in a hash.
C
C           ZZHSCAVL       Get available room in hash.
C
C           ZZHSCINF       Get h/k information about hash.
C
C     An add-only hash consists of the head node pointer list (HEDLST),
C     hash node collision list (COLLST), and the item list (ITEMS).
C
C     The function ZZHASH2 computes the index of an element in the head
C     node pointer list (HEDLST).
C
C     The HEDLST element at this index contains either 0, indicating
C     that there is not yet data for this hash value in the hash, or
C     the index of the head node and its item in the hash collision
C     (COLLST) and item (ITEMS) lists.
C
C     The COLLST element at the head node index contain either 0,
C     indicating that there is only one item for this hash value in the
C     hash, or the index of the next node and its item in the hash
C     collision (COLLST) and item (ITEMS) lists.
C
C     The COLLST element at the next node index contain either 0,
C     indicating that this is the last item for this hash value in the
C     hash, or the index of the next node and its item in the hash
C     collision (COLLST) and item (ITEMS) lists.
C
C     And so on.
C
C     Pictorially the hash looks like this:
C
C                     List of head   List of hash      List of
C                        nodes        collisions        items
C
C                       HEDLST          COLLST          ITEMS
C
C                                     +---------+
C                                     |         |
C                                     +---------+
C                                         ...
C                                     +---------+
C                                     | 1st Free|
C                                     +---------+
C                                     |  Size   | 
C                     +---------+     +---------+     +---------+
C                     |         |     |         |     |         |
C                     +---------+ !=0 +---------+     +---------+
C                  .->|Head Node| -.  |         |     |         |
C                  |  +---------+  |  +---------+     +---------+
C       ZZHASH2(ITEM) |         |  |  |         |     |         |
C                     +---------+  |  +---------+     +---------+
C                     |         |  `->|Head of  |     |Item     |
C                     |         |     |collision| !=0 |corresp. |
C                     |         |     |list for | -.  |to head  |
C                     |         |     | ITEM    |  |  |of list  |
C                     +---------+     +---------+  |  +---------+
C                     |         |     |         |  |  |         |
C                     +---------+     +---------+  |  +---------+
C                     |         |     |         |<-'  |         |
C                     |         |     |Next Node| !=0 |Next Item|
C                     |         |     |         |--.  |         |
C                     +---------+     +---------+  |  +---------+
C                     |         |     |         |  |  |         |
C                     +---------+     +---------+  |  +---------+
C                     |         |     |         |  |  |         |
C                     +---------+     +---------+  |  +---------+
C                     |         |     |Next Node|<-'  |Next Item|
C                     +---------+     +---------+ etc +---------+
C                        ...              ...            ...
C                     +---------+     +---------+     +---------+
C                     |         |     |         |     |         |
C                     +---------+     +---------+     +---------+
C
C
C$ Examples
C
C     An add-only hash used together with flat data arrays provides a
C     simple, fast-access buffering mechanism. The subsystems that need
C     such buffering would normally initialize the hash, add items to
C     the hash and buffer data associated with items in flat data
C     arrays, and find items in the hash and, if found, use buffered
C     data associated with the items rather than computing/getting
C     their data again.
C
C     An add-only hash is normally used in one of the following ways --
C     set up to never be filled up, stop buffering when filled up, and
C     trash when filled up and start buffering again. Three examples
C     below illustrate each of these ways.
C
C     Example 1: Set up to never be filled up
C     ---------------------------------------
C
C     C
C     C     Parameters: pool lower boundary, hash size and item 
C     C     size.
C     C
C           INTEGER               LBPOOL
C           PARAMETER           ( LBPOOL = -5 )
C     
C           INTEGER               HSHSIZ
C           PARAMETER           ( HSHSIS = 5003 )
C     
C           INTEGER               NAMSIZ
C           PARAMETER           ( NAMSIZ = 32 )
C     
C     C
C     C     Hash arrays and data buffer.
C     C
C           INTEGER               HEDLST  (          HSHSIZ )
C           INTEGER               COLLST  ( LBPOOL : HSHSIZ )
C           CHARACTER*(NAMSIZ)    ITEMS   (          HSHSIZ )
C           INTEGER               DATBUF  (          HSHSIZ )
C     
C     C
C     C     Miscellaneous variables.
C     C
C           CHARACTER*(NAMSIZ)    ITEM
C     
C           INTEGER               ITEMAT
C           INTEGER               MYDATA
C     
C           LOGICAL               FIRST
C           INTEGER               AVAIL
C           LOGICAL               NEW
C     
C     C
C     C     Data items.
C     C
C           DATA FIRST             / .TRUE.  /
C     
C     C
C     C     Initialize hash.
C     C
C           IF ( FIRST ) THEN
C              CALL ZZHSCINI ( HSHSIZ, HEDLST, COLLST )
C              FIRST = .FALSE.
C           END IF
C           ...
C     C
C     C     Check for presence of or add an item to the hash. If the
C     C     item is in the hash, NEW will be .FALSE. If the item is
C     C     not in the hash, it will be added and NEW will be .TRUE.
C     C     This call can fail only if the hash fills up which
C     C     should not happen because it was declared to never fill
C     C     up.
C     C
C           CALL ZZHSCADD ( HEDLST, COLLST, ITEMS, ITEM, 
C          .                                ITEMAT, NEW )
C           IF ( FAILED() ) THEN
C              RETURN
C           END IF
C     
C           IF ( .NOT. NEW ) THEN
C     C
C     C        Simply use buffered value.
C     C
C              MYDATA = DATBUF( ITEMAT )
C     
C           ELSE         
C     C
C     C        Get value.
C     C
C              ...
C     C
C     C        Buffer value.
C     C
C              DATBUF( ITEMAT ) = MYDATA
C     
C           END IF
C     
C     
C
C     Example 2: Stop buffering when filled up
C     ----------------------------------------
C
C     C
C     C     Use the same declarations as the first example.
C     C
C           ...
C     C
C     C     Initialize hash.
C     C
C           IF ( FIRST ) THEN
C              CALL ZZHSCINI ( HSHSIZ, HEDLST, COLLST )
C              FIRST = .FALSE.
C           END IF
C           ...
C     C
C     C     Check if this item is in the hash.
C     C
C           CALL ZZHSCCHK ( HEDLST, COLLST, ITEMS, ITEM, ITEMAT )
C     
C           IF ( ITEMAT .NE. 0 ) THEN
C     C
C     C        Simply use buffered value.
C     C
C              MYDATA = DATBUF( ITEMAT )
C     
C           ELSE
C     C
C     C        Get value.
C     C
C              ...
C     C
C     C        Buffer value, but only if the hash is not full.
C     C
C              CALL ZZHSCAVL( COLLST, AVAIL )
C              IF ( AVAIL .GT. 0 ) THEN
C                 CALL ZZHSCADD ( HEDLST, COLLST, ITEMS, ITEM, 
C          .                                  ITEMAT, NEW )
C                 DATBUF( ITEMAT ) = MYDATA
C              END IF
C     
C           END IF
C
C
C     Example 3: Trash when filled up and start buffering again
C     ---------------------------------------------------------
C
C     C
C     C     Use the same declarations as the first example.
C     C
C           ...
C     C
C     C     Initialize hash.
C     C
C           IF ( FIRST ) THEN
C              CALL ZZHSCINI ( HSHSIZ, HEDLST, COLLST )
C              FIRST = .FALSE.
C           END IF
C           ...
C     C
C     C     Check if this item is in the hash.
C     C
C           CALL ZZHSCCHK ( HEDLST, COLLST, ITEMS, ITEM, ITEMAT )
C     
C           IF ( ITEMAT .NE. 0 ) THEN
C     C
C     C        Simply use buffered value.
C     C
C              MYDATA = DATBUF( ITEMAT )
C     
C           ELSE
C     C
C     C        Get value.
C     C
C              ...
C     C
C     C        Buffer value, if the hash is full trash re-initialize
C     C        it first.
C     C
C              CALL ZZHSCAVL( COLLST, AVAIL )
C              IF ( AVAIL .LE. 0 ) THEN
C                 CALL ZZHSCINI ( HSHSIZ, HEDLST, COLLST )
C              END IF
C              CALL ZZHSCADD ( HEDLST, COLLST, ITEMS, ITEM, 
C          .                                  ITEMAT, NEW )
C              DATBUF( ITEMAT ) = MYDATA
C     
C           END IF
C
C$ Restrictions
C
C     For sake of speed all entry points do no or minimal error
C     checking. It is the responsibility of the caller to ensure that
C     all inputs are properly initialized.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 31-JUL-2013 (BVS)
C
C-&
 
C$ Index_Entries
C
C     manipulate add-only character hash
C
C-& 

C
C     Hash control area items.
C
      INTEGER               SIZIDX
      PARAMETER           ( SIZIDX =   0 )
 
      INTEGER               FREIDX
      PARAMETER           ( FREIDX =  -1 )

C
C     SPICELIB functions.
C
      INTEGER               ZZHASH2

      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local variables.
C
      INTEGER               I
      INTEGER               LOOKAT
      INTEGER               NODE

      LOGICAL               FULL
      LOGICAL               LFOUND

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZHSC' )
      END IF

C
C     Signal bogus entry error and check out.
C
      CALL SIGERR ( 'BOGUSENTRY' )

      CALL CHKOUT ( 'ZZHSC' )

      RETURN


C$Procedure ZZHSCINI ( Private---Initialize Add-only Character Hash )

      ENTRY ZZHSCINI ( HASHSZ, HEDLST, COLLST ) 

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Initialize an add-only character hash.
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
C     None.
C
C$ Keywords
C
C     PRIVATE UTILITY
C
C$ Declarations
C 
C     INTEGER               LBPOOL
C     PARAMETER           ( LBPOOL = -5 )
C
C     INTEGER               HASHSZ
C     INTEGER               HEDLST  (          * )
C     INTEGER               COLLST  ( LBPOOL : * )
C      
C$ Brief_I/O
C
C     Variable  I/O  Entry
C     --------  ---  --------------------------------------------------
C     HASHSZ     I   Hash size
C     HEDLST    I/O  Hash head node pointer list
C     COLLST    I/O  Hash node collision list
C
C     LBPOOL     P   Collision list array lower bound
C
C$ Detailed_Input
C
C     HASHSZ      is the hash size. For efficiency reasons it must be a
C                 prime number in the range from 1 to INTMAX/68 - 1.
C
C     HEDLST
C     COLLST      are the head node pointer list and the node collision
C                 list components of an add-only character hash.
C
C$ Detailed_Output
C
C     HEDLST
C     COLLST      are the head node pointer list and the node collision
C                 list components of an add-only character hash with all
C                 elements of the head node pointer list HEDLST set to
C                 zero and size and first-free elements of the head
C                 node pointer list COLLST set to HASHSZ and 1
C                 correspondingly.
C
C$ Parameters
C
C     LBPOOL      is the lower bound of the collision list array.
C
C$ Exceptions
C
C     1) If HASHSZ is less than 1 or greater than the value that is
C        guaranteed not to result in eventual integer overflow in
C        ZZHASH2, the error will be signaled by ZZHASH2.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     See the header of the umbrella routine ZZHSC.
C
C$ Examples
C
C     See the header of the umbrella routine ZZHSC.
C
C$ Restrictions
C
C     See the header of the umbrella routine ZZHSC.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 31-JUL-2013 (BVS)
C
C-&
 
C$ Index_Entries
C
C     initialize add-only character hash
C
C-& 

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZHSCINI' )
         
C
C     The requested number of nodes must be in the valid range. If it
C     is not, ZZHASH2 will signal an error.
C
      I = ZZHASH2( ' ', HASHSZ )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZHSCINI' )
         RETURN
      END IF

C
C     Wipe out head node pointer list.
C
      DO I = 1, HASHSZ
         HEDLST(I) = 0
      END DO

C
C     Reset control area.
C
      COLLST( SIZIDX ) = HASHSZ
      COLLST( FREIDX ) = 1

      CALL CHKOUT ( 'ZZHSCINI' )

      RETURN


C$Procedure ZZHSCADD ( Private---Add Item to Add-only Character Hash )

      ENTRY ZZHSCADD ( HEDLST, COLLST, ITEMS, ITEM, ITEMAT, NEW )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Find or add, if not present, an item to an add-only character
C     hash.
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
C     None.
C
C$ Keywords
C
C     PRIVATE UTILITY
C
C$ Declarations
C 
C     INTEGER               LBPOOL
C     PARAMETER           ( LBPOOL = -5 )
C
C     INTEGER               HEDLST  (          * )
C     INTEGER               COLLST  ( LBPOOL : * )
C     CHARACTER*(*)         ITEMS   (          * )
C     CHARACTER*(*)         ITEM
C     INTEGER               ITEMAT
C     LOGICAL               NEW
C      
C$ Brief_I/O
C
C     Variable  I/O  Entry
C     --------  ---  --------------------------------------------------
C     HEDLST    I/O  Hash head node pointer list
C     COLLST    I/O  Hash node collision list
C     ITEMS     I/O  Hash item list
C     ITEM       I   Item to be checked for/added
C     ITEMAT     O   Item index in node collision and item lists
C     NEW        O   Flag indicting if item was added
C
C     LBPOOL     P   Collision list array lower bound
C
C$ Detailed_Input
C
C     HEDLST
C     COLLST
C     ITEMS       are the components of an add-only character
C                 hash.
C
C     ITEM        is an item to find and, if needed, add to the hash.
C
C$ Detailed_Output
C
C     HEDLST
C     COLLST
C     ITEMS       are the components of an add-only character
C                 hash with the new item added to it if needed.
C
C     ITEMAT      is the index of the item in the node collision and
C                 item lists. If the item could not be found in or
C                 added to the hash, ITEMAT is set to 0.
C
C     NEW         is a flag indicting if item was added to the hash.
C
C$ Parameters
C
C     LBPOOL      is the lower bound of the collision list array.
C
C$ Exceptions
C
C     1) If the hash is full, the error SPICE(HASHISFULL) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     See the header of the umbrella routine ZZHSC.
C
C$ Examples
C
C     See the header of the umbrella routine ZZHSC.
C
C$ Restrictions
C
C     See the header of the umbrella routine ZZHSC.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 31-JUL-2013 (BVS)
C
C-&
 
C$ Index_Entries
C
C     add an item to add-only character hash
C
C-& 

C
C     Standard SPICE error handling. No checking-in here. We will do it
C     when we have to.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

C
C     Set flag indicating whether the hash is full.
C
      FULL   = COLLST ( FREIDX ) .GT. COLLST ( SIZIDX )

C
C     Use hash function to get index of the head node.
C
      LOOKAT = ZZHASH2 ( ITEM, COLLST ( SIZIDX ) )
      NODE   = HEDLST  ( LOOKAT )

C
C     Set initial values.
C
      LFOUND = .FALSE.
      NEW    = .FALSE.

C
C     See if this item (or one colliding with it in the hash scheme)
C     has already been stored in the item list.
C
      IF ( NODE .GT. 0 ) THEN

C
C        Start at the head node and check each item saved for this hash
C        value until we find a item that matches or run out of items in
C        this conflict resolution list.
C
         DO WHILE ( NODE .GT. 0 .AND. .NOT. LFOUND )

            LFOUND = ITEMS ( NODE ) .EQ. ITEM
            ITEMAT = NODE
            NODE   = COLLST ( NODE )

         END DO

C
C        If we didn't find this item on the conflict resolution list
C        and our hash is not full we will add this item to it.
C
         IF ( .NOT. LFOUND .AND. .NOT. FULL   ) THEN

C
C           Get next free node.
C
            NODE = COLLST ( FREIDX )

C
C           Increment next free node pointer.
C
            COLLST ( FREIDX ) = COLLST ( FREIDX ) + 1

C
C           Set current node pointer to the node we just picked for
C           this item.
C
            COLLST ( ITEMAT ) = NODE

C
C           Set new node pointer to 0, just in case.
C
            COLLST ( NODE ) = 0

C
C           Save item.
C
            ITEMS ( NODE ) = ITEM

C
C           Set output node ID and new and found flags.
C
            ITEMAT =  NODE
            NEW    = .TRUE.
            LFOUND = .TRUE.

         END IF

      ELSE IF ( .NOT. FULL ) THEN

C
C        Nothing like this item (in the hashing sense) has been stored
C        so far and the hash is not full.
C
C        Get next free node.
C
         NODE = COLLST ( FREIDX )

C
C        Increment next free node pointer.
C
         COLLST ( FREIDX ) = COLLST ( FREIDX ) + 1

C
C        Set new node pointer to 0, just in case.
C
         COLLST ( NODE ) = 0

C        
C        Set the head node pointer to this node.
C
         HEDLST ( LOOKAT ) = NODE

C
C        Save item.
C
         ITEMS ( NODE ) = ITEM

C
C        Set output node ID and new and found flags.
C
         ITEMAT =  NODE
         NEW    = .TRUE.
         LFOUND = .TRUE.

      END IF

C
C     Set ITEMAT to 0 if LFOUND is FALSE.
C
      IF ( .NOT. LFOUND ) THEN

         ITEMAT = 0

      END IF

C
C     If the item hash was full and we didn't find this item we've got
C     an error. Report it and return.
C
      IF ( FULL .AND. .NOT. LFOUND ) THEN
         
         CALL CHKIN  ( 'ZZHSCADD' )

         CALL SETMSG ( 'The hash has no room for any more items.') 
         CALL SIGERR ( 'SPICE(HASHISFULL)' )

         CALL CHKOUT ( 'ZZHSCADD' )
         RETURN

      END IF

      RETURN


C$Procedure ZZHSCCHK ( Private---Find Item in Add-only Character Hash )

      ENTRY ZZHSCCHK ( HEDLST, COLLST, ITEMS, ITEM, ITEMAT )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Find an item in an add-only character hash.
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
C     None.
C
C$ Keywords
C
C     PRIVATE UTILITY
C
C$ Declarations
C 
C     INTEGER               LBPOOL
C     PARAMETER           ( LBPOOL = -5 )
C
C     INTEGER               HEDLST  (          * )
C     INTEGER               COLLST  ( LBPOOL : * )
C     CHARACTER*(*)         ITEMS   (          * )
C     CHARACTER*(*)         ITEM
C     INTEGER               ITEMAT
C      
C$ Brief_I/O
C
C     Variable  I/O  Entry
C     --------  ---  --------------------------------------------------
C     HEDLST     I   Hash head node pointer list
C     COLLST     I   Hash node collision list
C     ITEMS      I   Hash item list
C     ITEM       I   Item to find
C     ITEMAT     O   Item index in node collision and item lists
C
C     LBPOOL     P   Collision list array lower bound
C
C$ Detailed_Input
C
C     HEDLST
C     COLLST
C     ITEMS       are the components of an add-only character
C                 hash.
C
C     ITEM        is an item to find in the hash.
C
C$ Detailed_Output
C
C     ITEMAT      is the index of the item in the node collision and
C                 item lists. If item is not in the hash, ITEMAT is set
C                 to 0.
C
C$ Parameters
C
C     LBPOOL      is the lower bound of the collision list array.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     See the header of the umbrella routine ZZHSC.
C
C$ Examples
C
C     See the header of the umbrella routine ZZHSC.
C
C$ Restrictions
C
C     See the header of the umbrella routine ZZHSC.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 31-JUL-2013 (BVS)
C
C-&
 
C$ Index_Entries
C
C     find item in add-only character hash
C
C-& 

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

C
C     Use hash function to get index of the head node.
C
      LOOKAT = ZZHASH2 ( ITEM, COLLST ( SIZIDX ) )
      NODE   = HEDLST  ( LOOKAT )

C
C     Set initial values.
C
      LFOUND  = .FALSE.

C
C     See if this item (or one colliding with it in the hash scheme) is
C     in the item list.
C
      IF ( NODE .GT. 0 ) THEN

C
C        Start at the head node and check each item saved for this hash
C        value until we find a item that matches or run out of items in
C        this conflict resolution list.
C
         DO WHILE ( NODE .GT. 0 .AND. .NOT. LFOUND )

            LFOUND = ITEMS ( NODE ) .EQ. ITEM
            ITEMAT = NODE
            NODE   = COLLST ( NODE )

         END DO

      END IF
         
C
C     If LFOUND is false, set ITEMAT to 0.
C
      IF ( .NOT. LFOUND ) THEN

         ITEMAT = 0

      END IF

      RETURN


C$Procedure ZZHSCAVL ( Private---Get room available in Add-only Hash )

      ENTRY ZZHSCAVL ( COLLST, AVAIL )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Get room available in an add-only character hash.
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
C     None.
C
C$ Keywords
C
C     PRIVATE UTILITY
C
C$ Declarations
C 
C     INTEGER               LBPOOL
C     PARAMETER           ( LBPOOL = -5 )
C
C     INTEGER               COLLST  ( LBPOOL : * )
C     INTEGER               AVAIL
C      
C$ Brief_I/O
C
C     Variable  I/O  Entry
C     --------  ---  --------------------------------------------------
C     COLLST     I   Hash node collision list
C     AVAIL      O   Room available in the hash
C
C     LBPOOL     P   Collision list array lower bound
C
C$ Detailed_Input
C
C     COLLST      is the add-only character hash node collision
C                 list.
C
C$ Detailed_Output
C
C     AVAIL       is the room (number of vacant slots) available in
C                 the hash.
C
C$ Parameters
C
C     LBPOOL      is the lower bound of the collision list array.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     See the header of the umbrella routine ZZHSC.
C
C$ Examples
C
C     See the header of the umbrella routine ZZHSC.
C
C$ Restrictions
C
C     See the header of the umbrella routine ZZHSC.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 31-JUL-2013 (BVS)
C
C-&
 
C$ Index_Entries
C
C     get room available in add-only character hash
C
C-& 

C
C     Set the number of unoccupied slots in the hash.
C
      AVAIL = COLLST ( SIZIDX ) - COLLST ( FREIDX ) + 1

      RETURN


C$Procedure ZZHSCINF ( Private---Get Information about Add-only Hash )

      ENTRY ZZHSCINF ( HEDLST, COLLST, ITEMS, ITEM, AVAIL )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Get information about an add-only character hash.
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
C     None.
C
C$ Keywords
C
C     PRIVATE UTILITY
C
C$ Declarations
C 
C     INTEGER               LBPOOL
C     PARAMETER           ( LBPOOL = -5 )
C
C     INTEGER               HEDLST  (          * )
C     INTEGER               COLLST  ( LBPOOL : * )
C     CHARACTER*(*)         ITEMS   (          * )
C     CHARACTER*(*)         ITEM
C     INTEGER               AVAIL
C      
C$ Brief_I/O
C
C     Variable  I/O  Entry
C     --------  ---  --------------------------------------------------
C     HEDLST     I   Hash head node pointer list
C     COLLST     I   Hash node collision list
C     ITEMS      I   Hash item list
C     ITEM       I   parameter to report
C     AVAIL      O   parameter Value
C
C     LBPOOL     P   Collision list array lower bound
C
C$ Detailed_Input
C
C     HEDLST
C     COLLST
C     ITEMS       are the components of an add-only character
C                 hash.
C
C     ITEM        is the parameter to report:
C
C                    'HASH SIZE'
C                    'USED HEADNODE COUNT'
C                    'UNUSED HEADNODE COUNT'
C                    'USED ITEM COUNT'
C                    'UNUSED ITEM COUNT'
C                    'LONGEST LIST SIZE'
C
C$ Detailed_Output
C
C     AVAIL       is the value of the parameter of interest.
C
C$ Parameters
C
C     LBPOOL      is the lower bound of the collision list array.
C
C$ Exceptions
C
C     1) If the input ITEM is not recognized, the error
C        SPICE(ITEMNOTRECOGNIZED) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     See the header of the umbrella routine ZZHSC.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     See the header of the umbrella routine ZZHSC.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 31-JUL-2013 (BVS)
C
C-&
 
C$ Index_Entries
C
C     get information about add-only character hash
C
C-& 

C
C     Get the hash size.
C
      IF      ( ITEM .EQ. 'HASH SIZE' ) THEN

         AVAIL = COLLST ( SIZIDX )

C
C     Get the count of used nodes in the head list.
C
      ELSE IF ( ITEM .EQ. 'USED HEADNODE COUNT' ) THEN

         AVAIL = 0
         DO I = 1, COLLST ( SIZIDX )
            IF ( HEDLST ( I ) .NE. 0 ) THEN
               AVAIL = AVAIL + 1
            END IF
         END DO

C
C     Get the count of unused nodes in the head list.
C
      ELSE IF ( ITEM .EQ. 'UNUSED HEADNODE COUNT' ) THEN

         AVAIL = 0
         DO I = 1, COLLST ( SIZIDX )
            IF ( HEDLST ( I ) .EQ. 0 ) THEN
               AVAIL = AVAIL + 1
            END IF
         END DO

C
C     Get the count of used slots in the item list.
C
      ELSE IF ( ITEM .EQ. 'USED ITEM COUNT' ) THEN

         AVAIL = COLLST ( FREIDX ) - 1

C
C     Get the count of unused slots in the item list.
C
      ELSE IF ( ITEM .EQ. 'UNUSED ITEM COUNT' ) THEN

         AVAIL = COLLST ( SIZIDX ) - COLLST ( FREIDX ) + 1

C
C     Get the size of the longest item list for any hash value.
C
      ELSE IF ( ITEM .EQ. 'LONGEST LIST SIZE' ) THEN

         AVAIL = 0
         DO I = 1, COLLST ( SIZIDX )
            NODE = HEDLST ( I )
            LOOKAT = 0
            DO WHILE ( NODE .GT. 0 )
               LOOKAT = LOOKAT + 1
               NODE   = COLLST ( NODE )
            END DO
            AVAIL = MAX( AVAIL, LOOKAT )
         END DO

C
C     This parameter is not supported.
C
      ELSE

         AVAIL = 0

         CALL CHKIN  ( 'ZZHSCINF'                           )
         CALL SETMSG ( 'Parameter ''#'' is not recognized.' ) 
         CALL ERRCH  ( '#', ITEM                            )
         CALL SIGERR ( 'SPICE(ITEMNOTRECOGNIZED)'           )
         CALL CHKOUT ( 'ZZHSCINF'                           )

      END IF

      RETURN

      END
