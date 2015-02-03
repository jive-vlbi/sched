C$Procedure      COPYI ( Copy an integer cell )
 
      SUBROUTINE COPYI ( CELL, COPY )
 
C$ Abstract
C
C      Copy the contents of an integer cell to another cell.
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
C      CELLS
C
C$ Keywords
C
C      CELLS
C
C$ Declarations
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER            CELL     ( LBCELL:* )
      INTEGER            COPY     ( LBCELL:* )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      CELL       I   Cell to be copied.
C      COPY       O   New cell.
C
C$ Detailed_Input
C
C
C      CELL        is a cell.
C
C
C$ Detailed_Output
C
C      COPY        is a cell which contains the same elements as the
C                  input cell, in the same order. If the size (maximum
C                  cardinality) of the output cell is smaller than
C                  the cardinality of the input cell, then only as many
C                  items as will fit in the output cell are copied,
C                  and an error is signalled.
C
C
C$ Parameters
C
C      None.
C
C$ Particulars
C
C     The copy routines (COPYC, COPYD, and COPYI) are used primarily
C     to manipulate working cells, since many routines that use cells
C     (binary set routines, for instance) do not allow cells to be
C     combined or manipulated in place.
C
C$ Examples
C
C     In the following example, COPYC is used to copy the result
C     of the union of two sets (ordered cells) from a temporary
C     working set back into the one of the original set.
C
C           CALL UNIONC ( BODIES, PLANETS, TEMP )
C           CALL COPYC  ( TEMP,   BODIES        )
C
C     If the size of the temporary cell is greater than the size
C     of the original set, the function FAILED should be checked to be
C     sure that no overflow occurred. If BODIES is at least as
C     large as TEMP, no such check is necessary.
C
C$ Restrictions
C
C     None.
C
C$ Exceptions
C
C     1) If the output cell in not large enough to hold the elements
C        of the input cell, the error SPICE(CELLTOOSMALL) is signalled.
C
C$ Files
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     C.A. Curzon     (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
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
C     copy an integer cell
C
C-&
 
 
C$ Revisions
C
C-    Beta Version 1.1.0, 09-JAN-1989 (NJB)
C
C        Calling protocol for EXCESS has been changed.  Call to SETMSG
C        has been removed.
C
C-&
 
 
C
C     SPICELIB functions
C
      INTEGER          SIZEI
      INTEGER          CARDI
      LOGICAL          RETURN
 
C
C     Local variables
C
      INTEGER          SIZE
      INTEGER          CARD
      INTEGER          MOVED
      INTEGER          I
 
 
C
C     Set up the error processing.
C
      IF ( RETURN() ) RETURN
      CALL CHKIN ( 'COPYI' )
 
C
C     We need the cardinality of the input cell, and the size of
C     the output cell.
C
      CARD = CARDI ( CELL )
      SIZE = SIZEI ( COPY )
 
C
C     Start moving the elements, one by one. Stop if the output
C     cell fills up.
C
      MOVED = MIN ( CARD, SIZE )
 
      DO I = 1, MOVED
         COPY(I) = CELL(I)
      END DO
 
C
C     Set the cardinality of the output cell. Report any excess.
C
      CALL SCARDI ( MOVED, COPY )
 
      IF ( CARD .GT. SIZE ) THEN
 
         CALL EXCESS ( CARD - SIZE, 'cell' )
         CALL SIGERR ( 'SPICE(CELLTOOSMALL)' )
         CALL CHKOUT ( 'COPYI' )
         RETURN
 
      END IF
 
      CALL CHKOUT ( 'COPYI' )
      RETURN
      END
