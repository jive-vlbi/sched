C$Procedure      VALIDD ( Validate a double precision set )
 
      SUBROUTINE VALIDD ( SIZE, N, A )
 
C$ Abstract
C
C      Create a valid set from a double precision set array.
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
 
      INTEGER            SIZE
      INTEGER            N
      DOUBLE PRECISION   A       ( LBCELL:* )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      SIZE       I   Size (maximum cardinality) of the set.
C      N          I   Initial no. of (possibly non-distinct) elements.
C      A         I/O  Set to be validated.
C
C$ Detailed_Input
C
C      SIZE        is the maximum cardinality (number of elements)
C                  of the set.
C
C      N           is the number of (possibly non-distinct) elements
C                  initially contained in the array used to maintain
C                  the set. N cannot be greater than the size of the
C                  set.
C
C
C      A           is a set.
C
C
C                  On input, A contains N elements beginning at A(1).
C                  To create a valid set, the elements are ordered,
C                  and duplicate elements are removed. The contents
C                  of A(LBCELL) through A(0) are lost during validation.
C
C$ Detailed_Output
C
C      A           on output, is the set containing the ordered,
C                  distinct values in the input array, ready for
C                  use with other set routines.
C
C$ Parameters
C
C      None.
C
C$ Particulars
C
C      This routine is typically used to turn an array which has been
C      initialized through DATA or I/O statements into a set, which
C      can then be used with the other set routines.
C
C      Because a set is ordered and contains distinct values, to
C      create a set from an array, it is necessary to sort the array
C      into the set and remove duplicates. Once the array has been
C      sorted, duplicate elements (adjacent after sorting) are removed.
C      The size and cardinality of the set are initialized, and the
C      set is ready to go.
C
C      Because validation is done in place, there is no chance of
C      overflow.
C
C$ Examples
C
C      Empty sets may be initialized with the cell routines SSIZEx.
C      Sets may also be initialized from nonempty set arrays.
C      This process, called validation, is done by the set routines
C      VALIDC and VALIDI. In the following example,
C
C            INTEGER      BODIES  ( LBCELL:100 )
C
C            DATA       ( BODIES(I), I=1,8)       /  3, 301,
C           .                                        3, 399,
C           .                                        5, 501,
C           .                                        6, 601,  /
C
C            CALL VALIDI ( 100, 8, BODIES )
C
C      the integer set BODIES is validated. The size of BODIES set to
C      100. The eight elements of the array (stored in elements 1-8)
C      are sorted, and duplicate elements (in this case, the number 3,
C      which appears twice) are removed, and the cardinality of the set
C      is set to the number of distinct elements, now seven. The set is
C      now ready for use with the rest of the set routines.
C
C      The previous contents of elements LBCELL through 0 are lost
C      during the process of validation.
C
C$ Restrictions
C
C      None.
C
C$ Exceptions
C
C      1)   If the size of the set is too small to hold the set
C           BEFORE validation, the error SPICE(INVALIDSIZE) is
C           signalled.  The array A is not modified.
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
C     validate a d.p. set
C
C-&
 
 
C$ Revisions
C
C-    Beta Version 2.0.0, 13-MAR-1989 (NJB)
C
C        Now participates in error handling.  References to RETURN,
C        CHKIN, and CHKOUT added.  Check for adequate set size added.
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
      INTEGER               CARD
 
 
C
C     Standard error handling:
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'VALIDD' )
      END IF
 
C
C     Is the set size big enough?
C
      IF ( N .GT. SIZE ) THEN
 
         CALL SETMSG ( 'Size of un-validated set is too small.  '     //
     .                 'Size is #, size required is #. ' )
 
         CALL ERRINT ( '#', SIZE )
         CALL ERRINT ( '#', N    )
         CALL SIGERR ( 'SPICE(INVALIDSIZE)' )
         CALL CHKOUT ( 'VALIDD' )
         RETURN
      END IF
 
C
C     Just like it says above. Order the array, and remove duplicates.
C
      CARD = N
      CALL RMDUPD ( CARD, A(1) )
 
C
C     Set the size and cardinality of the input set.
C
      CALL SSIZED ( SIZE, A )
      CALL SCARDD ( CARD, A )
 
      CALL CHKOUT( 'VALIDD' )
      RETURN
      END
