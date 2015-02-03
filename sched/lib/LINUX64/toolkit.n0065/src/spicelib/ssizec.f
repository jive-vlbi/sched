C$Procedure      SSIZEC ( Set the size of a character cell )
 
      SUBROUTINE SSIZEC ( SIZE, CELL )
 
C$ Abstract
C
C      Set the size (maximum cardinality) of a character cell.
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
 
      INTEGER            SIZE
      CHARACTER*(*)      CELL   ( LBCELL:* )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     SIZE       I   Size (maximum cardinality) of the cell.
C     CELL       O   The cell.
C
C$ Detailed_Input
C
C     SIZE        is the size (maximum number of elements) of the cell.
C
C$ Detailed_Output
C
C
C      CELL        is a cell.
C
C
C                 On output, the size of the cell is SIZE.  The
C                 cardinality of the cell is 0.  The rest of the
C                 control area is zeroed out.
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
C     None.
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
C     set the size of a character cell
C
C-&
 
 
C$ Revisions
C
C-    Beta Version 2.0.0, 13-MAR-1989 (NJB)
C
C        Check for invalid size value added.  An error
C        is signalled if the value is out of range.  The cardinality
C        is now automatically reset to 0.  The rest of the control
C        area is now zeroed out.
C
C        The examples have been updated to illustrate set initialization
C        without the use of the EMPTYx routines, which have been
C        removed from the library.  Errors in the examples have been
C        removed, also.
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
 
C
C     Local variables
C
      INTEGER               I
 
 
 
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SSIZEC' )
      END IF
 
C
C     The size must be non-negative.  Other values will be snubbed.
C
      IF (  SIZE .LT. 0  )  THEN
         CALL SETMSG ( 'Attempt to set size of cell to invalid '      //
     .                 'value.  The value was #.'  )
         CALL ERRINT ( '#', SIZE )
         CALL SIGERR ( 'SPICE(INVALIDSIZE)' )
         CALL CHKOUT ( 'SSIZEC' )
         RETURN
      END IF
 
 
C
C     Not much to this.
C
      CALL ENCHAR ( SIZE, CELL(-1) )
      CALL ENCHAR ( 0,    CELL( 0) )
 
      DO I = LBCELL, -2
         CALL ENCHAR ( 0, CELL(I) )
      END DO
 
      CALL  CHKOUT ( 'SSIZEC' )
      RETURN
      END
