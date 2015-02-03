C$Procedure      REMOVD ( Remove an item from a double precision set )
 
      SUBROUTINE REMOVD ( ITEM, A )
 
C$ Abstract
C
C      Remove an item from a double precision set.
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
 
      DOUBLE PRECISION ITEM
      DOUBLE PRECISION A        ( LBCELL:* )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      ITEM       I   Item to be removed.
C      A         I/O  Removal set.
C      ERROR      O   Error flag.
C
C$ Detailed_Input
C
C      ITEM        is an item which is to be removed from the
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
C      A           on output contains the difference of the input set
C                  and the input item. If the item is not an element of
C                  the set, the set is not changed.
C
C$ Parameters
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
C            CALL REMOVC ( 'PLUTO', PLANETS          )
C            CALL INSRTC ( 'PLUTO', ASTEROIDS, ERROR )
C
C      If 'PLUTO' is not an element of PLANETS, then the contents of
C      PLANETS are not changed. Similarly, if 'PLUTO' is already an
C      element of ASTEROIDS, the contents of ASTEROIDS remain unchanged.
C
C      Because inserting an element into a set can increase the
C      cardinality of the set, the insertion routines return an
C      error flag. The flag is blank if the set is large enough to
C      hold the new element. Otherwise, a message (constructed by
C      the cell routine EXCESS) is returned.
C
C$ Restrictions
C
C      None.
C
C$ Exceptions
C
C     None.
C
C$ Files
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
C     remove an item from a d.p. set
C
C-&
 
 
C$ Revisions
C
C-    Beta Version 2.0.0, 13-MAR-1989 (NJB)
C
C        Now participates in error handling.  References to RETURN,
C        CHKIN, and CHKOUT added.
C
C-&
 
 
C
C     SPICELIB functions
C
      INTEGER               CARDD
      INTEGER               BSRCHD
      LOGICAL               RETURN
C
C     Local variables
C
      INTEGER          CARD
      INTEGER          LOC
      LOGICAL          IN
      INTEGER          I
 
 
C
C     Standard error handling:
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'REMOVD' )
      END IF
C
C     What is the cardinality of the set?
C
      CARD = CARDD ( A )
 
C
C     Determine the location (if any) of the item within the set.
C
      LOC = BSRCHD ( ITEM, CARD, A(1) )
 
C
C     Is the item in the set? If so, it needs to be removed.
C
      IN = ( LOC .GT. 0 )
 
      IF ( IN ) THEN
 
C
C        Move succeeding elements forward to take up the slack left
C        by the departing element. And update the cardinality for
C        future reference.
C
         DO I = LOC, CARD-1
            A(I) = A(I+1)
         END DO
 
         CALL SCARDD ( CARD-1, A )
 
      END IF
 
      CALL CHKOUT( 'REMOVD' )
      RETURN
      END
