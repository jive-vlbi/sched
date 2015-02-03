C$Procedure SCARDD ( Set the cardinality of a double precision cell )
 
      SUBROUTINE SCARDD ( CARD, CELL )
 
C$ Abstract
C
C      Set the cardinality of a double precision cell.
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
C      CELLS
C
C$ Declarations
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER          CARD
      DOUBLE PRECISION CELL   ( LBCELL:* )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     CARD       I   Cardinality of (number of elements in) the cell.
C     CELL       O   The cell.
C
C$ Detailed_Input
C
C     CARD        is the cardinality of (number of elements in) the
C                 cell.
C
C$ Detailed_Output
C
C
C      CELL        is a cell.
C
C
C                 On output, the cardinality of the cell is CARD.
C
C$ Parameters
C
C      None.
C
C$ Particulars
C
C     The set cardinality (SCARDC, SCARDD, and SCARDI) and set size
C     (SSIZEC, SSIZED, and SSIZEI) routines are typically used to
C     initialize cells for subsequent use. Since all cell routines
C     expect to find the size and cardinality of a cell in place,
C     no cell can be used until both have been set.
C
C     The set cardinality routines are also used by library routines
C     which manipulate cells (including set and window routines) to
C     reset the cardinalities of cells as they gain or lose elements.
C
C$ Examples
C
C     In the example below, the size and cardinality of the character
C     cell FRED are set in the main module of the program FLNSTN.
C     Both are subsequently retrieved, and the cardinality changed,
C     in one of its subroutines, WILMA.
C
C           PROGRAM FLNSTN
C
C           CHARACTER*30     FRED ( LBCELL:100 )
C            .
C            .
C           CALL SSIZEC ( 100, FRED )
C            .
C            .
C           CALL WILMA ( FRED )
C            .
C            .
C           STOP
C           END
C
C
C           SUBROUTINE WILMA ( FRED )
C
C           CHARACTER*(*)      FRED  ( LBCELL:* )
C           INTEGER            SIZE
C           INTEGER            CARD
C
C           INTEGER            CARDC
C           INTEGER            SIZEC
C            .
C            .
C           SIZE = SIZEC ( FRED )
C           CARD = CARDC ( FRED )
C            .
C            .
C           CALL SCARDC ( MIN ( SIZE, CARD ), FRED )
C           CALL EXCESS ( CARD-SIZE, 'cell' )
C            .
C            .
C           RETURN
C           END
C
C
C$ Restrictions
C
C     None.
C
C$ Exceptions
C
C     1)  If the cardinality value supplied is less than 0 or greater
C         than the cell size, the error SPICE(INVALIDCARDINALITY) is
C         signalled.
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
C     set the cardinality of a d.p. cell
C
C-&
 
 
C$ Revisions
C
C-    Beta Version 2.0.0, 13-MAR-1989 (NJB)
C
C        Check for invalid cardinality value added.  An error
C        is signalled if the value is out of range.  Examples
C        updated so as not to refer to the EMPTYx routines, and
C        to show the correct calling protocol for EXCESS.
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
 
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SCARDD' )
      END IF
 
C
C     The cardinality may range from 0 to the size of the cell,
C     inclusive.  Other values will be snubbed.
C
      IF (  ( CARD .LT. 0 ) .OR. ( CARD .GT. INT( CELL(-1) ) )  )  THEN
         CALL SETMSG ( 'Attempt to set cardinality of cell '          //
     .                 'to invalid value.  The value was #.'  )
         CALL ERRINT ( '#', CARD )
         CALL SIGERR ( 'SPICE(INVALIDCARDINALITY)' )
         CALL CHKOUT ( 'SCARDD' )
         RETURN
      END IF
 
 
C
C     Not much to this.
C
      CELL(0) = DBLE ( CARD )
 
      CALL CHKOUT ( 'SCARDD' )
      RETURN
      END
