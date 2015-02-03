C$Procedure      ZZEKTRLK ( EK tree, locate key )
 
      SUBROUTINE ZZEKTRLK ( HANDLE, TREE,   KEY,   IDX,
     .                      NODE,   NOFFST, LEVEL, VALUE  )
 
C$ Abstract
C
C     Locate a specified key.  Return metadata describing the node
C     containing the key.
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
 
      INCLUDE 'ekpage.inc'
      INCLUDE 'ektree.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               TREE
      INTEGER               KEY
      INTEGER               IDX
      INTEGER               NODE
      INTEGER               NOFFST
      INTEGER               LEVEL
      INTEGER               VALUE
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     TREE       I   Root of tree.
C     KEY        I   Key corresponding to value.
C     IDX        O   Node-relative index of KEY.
C     NODE       O   Node containing key.
C     NOFFST     O   Offset of NODE.
C     LEVEL      O   Level of NODE.
C     VALUE      O   Value associated with KEY.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle of an EK open for write access.
C
C     TREE           is the root node number of the tree of interest.
C
C     KEY            is an absolute key.  In EK trees, absolute keys are
C                    just ordinal positions relative to the leftmost
C                    element of the tree, with the leftmost element
C                    having position 1.  So setting KEY to 10, for
C                    example, indicates that the output VALUE is the
C                    10th item in the tree.
C
C                    KEY must be in the range 1 : NKEYS, where
C                    NKEYS is the number of keys in the tree.
C
C$ Detailed_Output
C
C     IDX            is the node-relative index of KEY:  this is the
C                    ordinal position of KEY relative to other keys
C                    in the same node.
C
C     NODE           is the number of the node containing KEY.
C
C     NOFFST         is the offset of NODE.  This is the count of the
C                    keys that precede every key in the subtree headed
C                    by NODE.  Adding NOFFST to any relative key stored
C                    in NODE will convert that key to an absolute key.
C
C     LEVEL          is the level of NODE in the tree.  The root is at
C                    level 1, children of the root are at level 2, and
C                    so on.
C
C     VALUE          is the integer value associated with the input key.
C                    Normally, this value is a data pointer.
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
C     3)  If the input key is out of range, the error
C         SPICE(INDEXOUTOFRANGE) is signalled.
C
C
C     4)  If the tree traversal fails to terminate at the leaf node
C         level, the error SPICE(BUG) is signalled.
C
C     5)  If the key is in range, but the key is not found, the error
C         SPICE(BUG) is signalled.
C
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This routine obtains the value assocated with a key, and also
C     returns metadata describing the node containing the key and the
C     key's position in the node.
C
C$ Examples
C
C     See ZZEKTRUI.
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
C-    Beta Version 1.0.0, 26-OCT-1995 (NJB)
C
C-&
 
 
 
C
C     SPICELIB functions
C
      INTEGER               LSTLEI
 
C
C     Local parameters
C
      INTEGER               ACCLEN
      PARAMETER           ( ACCLEN = 15 )
 
C
C     Local variables
C
      CHARACTER*(ACCLEN)    ACCESS
 
      INTEGER               CHILD
      INTEGER               DATBAS
      INTEGER               DEPTH
      INTEGER               MINUS
      INTEGER               NEWKEY
      INTEGER               OLDHAN
      INTEGER               OLDIDX
      INTEGER               OLDLVL
      INTEGER               OLDMAX
      INTEGER               OLDNOD
      INTEGER               OLDNOF
      INTEGER               OLDTRE
      INTEGER               OLDKEY
      INTEGER               OLDVAL
      INTEGER               PAGE   ( PGSIZI )
      INTEGER               PLUS
      INTEGER               PREV
      INTEGER               PRVKEY
      INTEGER               TOTKEY
      INTEGER               UNIT
 
      LOGICAL               FIRST
      LOGICAL               FOUND
      LOGICAL               LEAF
      LOGICAL               SAMKEY
      LOGICAL               SAMTRE
      LOGICAL               RDONLY
 
C
C     Saved variables
C
      SAVE
 
C
C     Initial values
C
      DATA                  FIRST   / .TRUE.   /
 
 
C
C     Use discovery check-in in this puppy.
C
C     Nothing found to begin with.
C
      FOUND  =  .FALSE.
 
 
      IF ( FIRST ) THEN
C
C        Find out the access method for the current file.
C
         CALL DASHAM ( HANDLE, ACCESS )
 
         RDONLY  =    ACCESS .EQ. 'READ'
         SAMKEY  =    .FALSE.
         SAMTRE  =    .FALSE.
         LEAF    =    .FALSE.
         FIRST   =    .FALSE.
 
      ELSE
C
C        See whether we're looking at the same key, or at least
C        the same tree, as last time.  Note that for the tree to
C        be guaranteed to be the same, it must belong to a file open
C        for read access only.
C
         IF ( HANDLE .NE. OLDHAN ) THEN
 
            CALL DASHAM ( HANDLE, ACCESS )
 
            RDONLY  =    ACCESS .EQ. 'READ'
            SAMTRE  =    .FALSE.
            SAMKEY  =    .FALSE.
 
         ELSE
 
            SAMTRE  =  ( TREE  .EQ.  OLDTRE )  .AND.   RDONLY
            SAMKEY  =  ( KEY   .EQ.  OLDKEY )  .AND.   SAMTRE
 
         END IF
 
      END IF
 
C
C     If we're lucky enough to be getting a request for the previously
C     returned key, we're set.  If we've been asked for a key that is
C     very close to the previously requested key, we still may make
C     out pretty well.
C
      IF ( SAMKEY ) THEN
C
C        It's the same key as last time.
C
         IDX     =  OLDIDX
         NODE    =  OLDNOD
         NOFFST  =  OLDNOF
         LEVEL   =  OLDLVL
         VALUE   =  OLDVAL
 
         RETURN
 
 
      ELSE IF ( SAMTRE .AND. LEAF ) THEN
C
C        Compute the margins around the old key.  Keys that fall within
C        the interval defined by the old key and these margins are on
C        the same page as the old key.
C
         PLUS   =  OLDMAX - OLDIDX
         MINUS  =  OLDIDX - 1
 
 
         IF (        (  KEY  .LE.  OLDKEY+PLUS   )
     .         .AND. (  KEY  .GE.  OLDKEY-MINUS  )  ) THEN
C
C           The requested key lies on the same page as the old key.
C
            LEVEL   =  OLDLVL
 
            IF ( LEVEL .EQ. 1 ) THEN
               DATBAS  =  TRDATR
            ELSE
               DATBAS  =  TRDATC
            END IF
 
            IDX     =  OLDIDX + ( KEY - OLDKEY )
            NODE    =  OLDNOD
            NOFFST  =  OLDNOF
            VALUE   =  PAGE ( DATBAS + IDX )
 
            OLDIDX  =  IDX
            OLDKEY  =  KEY
            OLDVAL  =  VALUE
 
            RETURN
 
         END IF
 
      END IF
 
C
C     If we arrived here, we have some actual work to do.
C     Start out by looking at the root page.  Save the tree depth;
C     we'll use this for error checking.
C
      CALL ZZEKPGRI ( HANDLE, TREE, PAGE )
 
      DEPTH  =  PAGE ( TRDPTH )
      LEVEL  =  1
 
C
C     Find out how many keys are in the tree.  If KEY is outside
C     this range, we won't find it.
C
      TOTKEY  =  PAGE ( TRNKEY )
 
      IF (  ( KEY .LT. 1 )  .OR.  ( KEY .GT. TOTKEY )  ) THEN
 
         CALL CHKIN  ( 'ZZEKTRLK'                                      )
         CALL DASHLU ( HANDLE,  UNIT                                   )
         CALL SETMSG ( 'Key = #; valid range = 1:#. Tree = #, file = #')
         CALL ERRINT ( '#',     KEY                                    )
         CALL ERRINT ( '#',     TOTKEY                                 )
         CALL ERRINT ( '#',     TREE                                   )
         CALL ERRFNM ( '#',     UNIT                                   )
         CALL SIGERR ( 'SPICE(INDEXOUTOFRANGE)'                        )
         CALL CHKOUT ( 'ZZEKTRLK'                                      )
         RETURN
 
      END IF
 
C
C     Find the last key at this level that is less than or equal to
C     the requested key.
C
      PREV   =  LSTLEI (  KEY,  PAGE(TRNKR),  PAGE(TRKEYR+1)  )
 
      IF ( PREV .GT. 0 ) THEN
         PRVKEY =  PAGE(TRKEYR+PREV)
      ELSE
         PRVKEY =  0
      END IF
 
C
C     If we were lucky enough to get an exact match, set our outputs
C     and return.  The key offset in the root is zero.
C
      IF ( PRVKEY .EQ. KEY ) THEN
 
         NOFFST  =  0
         IDX     =  PREV
         NODE    =  TREE
         VALUE   =  PAGE ( TRDATR + IDX )
 
         OLDHAN  =  HANDLE
         OLDTRE  =  TREE
         OLDKEY  =  KEY
         OLDNOF  =  NOFFST
         OLDNOD  =  NODE
         OLDIDX  =  IDX
         OLDLVL  =  LEVEL
         OLDVAL  =  VALUE
         OLDMAX  =  PAGE ( TRNKR )
 
         LEAF    =  LEVEL  .EQ.  DEPTH
 
C
C        The root has no parent or siblings, so these values
C        remain set to zero.  The same is true of the parent keys.
C
         RETURN
 
      END IF
 
C
C     Still here?  Traverse the pointer path until we find the key
C     or run out of progeny.
C
      CHILD   =  PAGE ( TRKIDR + PREV + 1 )
      NOFFST  =  PRVKEY
 
 
      DO WHILE (  ( CHILD .GT. 0 ) .AND. ( .NOT. FOUND )  )
C
C        Look up the child node.
C
         CALL ZZEKPGRI ( HANDLE, CHILD, PAGE )
 
         LEVEL  =  LEVEL + 1
 
 
         IF ( LEVEL .GT. DEPTH ) THEN
 
            CALL CHKIN  ( 'ZZEKTRLK'                                   )
            CALL DASHLU ( HANDLE,  UNIT                                )
            CALL SETMSG ( 'Runaway node pointer chain.  Key = #; '//
     .                    'valid range = 1:#. Tree = #, file = #'      )
            CALL ERRINT ( '#',     KEY                                 )
            CALL ERRINT ( '#',     TOTKEY                              )
            CALL ERRINT ( '#',     TREE                                )
            CALL ERRFNM ( '#',     UNIT                                )
            CALL SIGERR ( 'SPICE(BUG)'                                 )
            CALL CHKOUT ( 'ZZEKTRLK'                                   )
            RETURN
 
         END IF
 
C
C        Find the last key at this level that is less than or equal to
C        the requested key.  Since the keys we're looking at now are
C        ordinal positions relative to the subtree whose root is the
C        current node, we must subtract from KEY the position of the
C        node preceding the first key of this subtree.
C
         NEWKEY  =  KEY - NOFFST
         PREV    =  LSTLEI (  NEWKEY,  PAGE(TRNKC),  PAGE(TRKEYC+1)  )
 
         IF ( PREV .GT. 0 ) THEN
            PRVKEY =  PAGE(TRKEYC+PREV)
         ELSE
            PRVKEY =  0
         END IF
 
C
C        If we were lucky enough to get an exact match, set our outputs
C        and return.  The key offset for the current node is stored
C        in NOFFST.
C
         IF ( PRVKEY .EQ. NEWKEY ) THEN
 
            FOUND   =  .TRUE.
            IDX     =  PREV
            NODE    =  CHILD
            VALUE   =  PAGE ( TRDATC + IDX )
 
            OLDHAN  =  HANDLE
            OLDTRE  =  TREE
            OLDKEY  =  KEY
            OLDNOF  =  NOFFST
            OLDNOD  =  NODE
            OLDIDX  =  IDX
            OLDLVL  =  LEVEL
            OLDVAL  =  VALUE
            OLDMAX  =  PAGE ( TRNKC )
 
            LEAF    =  LEVEL  .EQ.  DEPTH
 
         ELSE
 
            CHILD   =  PAGE ( TRKIDC + PREV + 1 )
            NOFFST  =  PRVKEY + NOFFST
 
         END IF
 
      END DO
 
C
C     If we found the key, our outputs are already set.  If not, we've
C     got trouble.
C
      IF ( .NOT. FOUND ) THEN
 
         CALL CHKIN  ( 'ZZEKTRLK'                                      )
         CALL DASHLU ( HANDLE,  UNIT                                   )
         CALL SETMSG ( 'Key #; valid range = 1:#. Tree = #, file = #.'//
     .                 '  Key was not found.  This probably indicates'//
     .                 ' a corrupted file or a bug in the EK code.'    )
         CALL ERRINT ( '#',     KEY                                    )
         CALL ERRINT ( '#',     TOTKEY                                 )
         CALL ERRINT ( '#',     TREE                                   )
         CALL ERRFNM ( '#',     UNIT                                   )
         CALL SIGERR ( 'SPICE(BUG)'                                    )
         CALL CHKOUT ( 'ZZEKTRLK'                                      )
         RETURN
 
      END IF
 
 
      RETURN
      END
