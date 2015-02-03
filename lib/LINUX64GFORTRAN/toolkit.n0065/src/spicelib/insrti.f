C$Procedure      INSRTI ( Insert an item into an integer set )
 
      SUBROUTINE INSRTI ( ITEM, A )
 
C$ Abstract
C
C      Insert an item into an integer set.
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
C      SETS
C
C$ Keywords
C
C      CELLS, SETS
C
C$ Declarations
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               ITEM
      INTEGER               A        ( LBCELL:* )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      ITEM       I   Item to be inserted.
C      A         I/O  Insertion set.
C
C$ Detailed_Input
C
C      ITEM        is an item which is to be inserted into the
C                  specified set. ITEM may or may not already
C                  be an element of the set.
C
C
C      A           is a set.
C
C
C                  On input, A may or may not contain the input item
C                  as an element.
C
C$ Detailed_Output
C
C      A           on output contains the union of the input set and
C                  the singleton set containing the input item, unless
C                  there was not sufficient room in the set for the
C                  item to be included, in which case the set is not
C                  changed and an error is returned.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     1) If the insertion of the element into the set causes an excess
C        of elements, the error SPICE(SETEXCESS) is signaled.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C      None.
C
C$ Examples
C
C      In the following example, the element 'PLUTO' is removed from
C      the character set PLANETS and inserted into the character set
C      ASTEROIDS.
C
C            CALL REMOVC ( 'PLUTO', PLANETS   )
C            CALL INSRTC ( 'PLUTO', ASTEROIDS )
C
C      If 'PLUTO' is not an element of PLANETS, then the contents of
C      PLANETS are not changed. Similarly, if 'PLUTO' is already an
C      element of ASTEROIDS, the contents of ASTEROIDS remain unchanged.
C
C      Because inserting an element into a set can increase the
C      cardinality of the set, an error may occur in the insertion
C      routines.
C
C$ Restrictions
C
C      None.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      N.J. Bachman    (JPL)
C      C.A. Curzon     (JPL)
C      W.L. Taber      (JPL)
C      I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 01-NOV-2005 (NJB)
C
C        Code was modified slightly to keep logical structure parallel
C        to that of INSRTC.
C
C        Long error message was updated to include size of
C        set into which insertion was attempted.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     insert an item into an integer set
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 01-NOV-2005 (NJB)
C
C        Code was modified slightly to keep logical structure parallel
C        to that of INSRTC.
C
C        Long error message was updated to include size of set into
C        which insertion was attempted.
C
C-    Beta Version 1.1.0, 06-JAN-1989 (NJB)
C
C        Calling protocol of EXCESS changed.  Call to SETMSG removed.
C
C-&
 
 
C
C     SPICELIB functions
C
      INTEGER             SIZEI
      INTEGER             CARDI
      INTEGER             LSTLEI
      LOGICAL             RETURN
 
C
C     Local variables
C
 
      INTEGER             SIZE
      INTEGER             CARD
      INTEGER             LAST
      LOGICAL             IN
      INTEGER             I
 
C
C     Set up the error processing.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'INSRTI' )
 
C
C     What are the size and cardinality of the set?
C
      SIZE = SIZEI ( A )
      CARD = CARDI ( A )
 
C
C     Find the last element of the set which would come before the
C     input item. This will be the item itself, if it is already an
C     element of the set.
C
      LAST = LSTLEI ( ITEM, CARD, A(1) )
 
C
C     Is the item already in the set? If not, it needs to be inserted.
C
      IF ( LAST .GT. 0 ) THEN
         IN  =  A(LAST) .EQ. ITEM 
      ELSE
         IN  =  .FALSE.
      END IF
 
      IF ( .NOT. IN ) THEN
C
C        If there is room in the set for the new element, then move
C        the succeeding elements back to make room. And update the
C        cardinality for future reference.
C
         IF ( CARD .LT. SIZE ) THEN
 
            DO I = CARD, LAST+1, -1
               A(I+1) = A(I)
            END DO
 
            A(LAST+1) = ITEM
 
            CALL SCARDI ( CARD+1, A )
 
         ELSE
  
            CALL SETMSG ( 'An element could not be inserted '   //
     .                    'into the set due to lack of space; ' //
     .                    'set size is #.'                      )
            CALL ERRINT ( '#', SIZE                             )
            CALL SIGERR ( 'SPICE(SETEXCESS)'                    )
 
         END IF
 
      END IF
 
      CALL CHKOUT ( 'INSRTI' )
 
      RETURN
      END
