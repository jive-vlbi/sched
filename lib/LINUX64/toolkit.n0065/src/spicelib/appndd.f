C$Procedure APPNDD ( Append an item to a double precision cell )
 
      SUBROUTINE APPNDD ( ITEM, CELL )
 
C$ Abstract
C
C      Append an item to a double precision cell.
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
C     CELLS
C
C$ Keywords
C
C     CELLS
C
C$ Declarations
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      DOUBLE PRECISION      ITEM
      DOUBLE PRECISION      CELL ( LBCELL:* )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ITEM       I   The item to append.
C     CELL      I/O  The cell to which ITEM will be appended.
C
C$ Detailed_Input
C
C     ITEM       is a double precision value which is to be appended
C                to CELL.
C
C     CELL       is a double precision cell to which ITEM will be
C                appended.
C
C$ Detailed_Output
C
C     CELL       is a double precision cell in which the last element
C                is ITEM.
C
C$ Parameters
C
C      None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If the cell is not big enough to accommodate the addition
C        of a new element, the error SPICE(CELLTOOSMALL) is signalled.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C      In the following example, the element 34.0D0 is appended to
C      the d.p. cell NUMBERS.
C
C      Before appending 34.0D0, the cell contains:
C
C      NUMBERS (1) =  1.0D0
C      NUMBERS (2) =  1.D0D
C      NUMBERS (3) =  2.0D0
C      NUMBERS (4) =  3.0D0
C      NUMBERS (5) =  5.0D0
C      NUMBERS (6) =  8.0D0
C      NUMBERS (7) = 13.0D0
C      NUMBERS (8) = 21.0D0
C
C      The call
C
C        CALL APPNDD ( 34.0D0, NUMBERS )
C
C      appends the element 34.0D0 at the location NUMBERS (9), and the
C      cardinality is updated.
C
C      If the cell is not big enough to accomodate the addition of
C      the item, an error is signalled. In this case, the cell is not
C      altered.
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
C     H.A. Neilan     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.2, 09-NOV-2006 (WLT)
C
C        Corrected typo in Examples section describing the cell as
C        character instead of d.p.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (HAN)
C
C-&
 
C$ Index_Entries
C
C     append an item to a d.p. cell
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      INTEGER               CARDD
      INTEGER               SIZED
 
C
C     Local variables
C
      INTEGER               NWCARD
 
 
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'APPNDD' )
      END IF
 
 
C
C     Check to see if the cell can accomodate the addition of a
C     new item. If there is room, append the item to the cell and
C     reset the cardinality. If the cell cannot accomodate the
C     addition of a new item, signal an error.
C
 
      NWCARD = CARDD (CELL) + 1
 
      IF (  ( NWCARD )  .LE.  ( SIZED (CELL) )  ) THEN
 
         CELL (NWCARD) = ITEM
         CALL SCARDD ( NWCARD, CELL )
 
      ELSE
 
        CALL SETMSG ( 'The cell cannot accomodate the '          //
     .                'addition of the element *. '                )
 
        CALL ERRDP  ( '*', ITEM )
        CALL SIGERR ( 'SPICE(CELLTOOSMALL)' )
 
      END IF
 
 
      CALL CHKOUT ( 'APPNDD' )
      RETURN
      END
