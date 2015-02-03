C$Procedure XPSGIP ( Transpose a matrix, general dimension, in place )
 
      SUBROUTINE XPSGIP ( NROW, NCOL, MATRIX )
 
C$ Abstract
C
C     Transpose a matrix of arbitrary size and shape in place.
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
C     MATRIX
C
C$ Declarations
 
      INTEGER            NROW
      INTEGER            NCOL
      DOUBLE PRECISION   MATRIX ( 0 : * )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NROW       I   Number of rows of input matrix.
C     NCOL       I   Number of columns of input matrix.
C     MATRIX    I-O  Matrix to be transposed/transposed matrix.
C
C$ Detailed_Input
C
C     MATRIX     Matrix to be transposed.
C
C     NROW       Number of rows of input matrix MATRIX.
C
C     NCOL       Number of columns of input matrix MATRIX.
C
C$ Detailed_Output
C
C     MATRIX     Transposed matrix:  element (i,j) of the input
C                matrix is element (j,i) of the output matrix.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error Free.
C
C     1) If either NROW or NCOL is less than or equal to zero, no action
C        is taken. The routine simply returns.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine replaces the input matrix MATRIX with its transpose.
C
C     NOTE:  The matrix MATRIX is declared one-dimensional for
C            computational purposes only.  The calling program may
C            declare it as MATRIX(NROW,NCOL) or MATRIX(NCOL,NROW).
C
C            This routine assumes that the elements of the matrix to be
C            transformed are stored in contiguous memory locations as
C            shown here.  On output these elements will be rearranged
C            in consecutive memory locations as shown.
C
C               MATRIX on input      MATRIX on output
C
C                m_11                m_11
C                m_21                m_12
C                m_31                m_13
C                 .                   .
C                 .                   .
C                 .                  m_1ncol
C                m_nrow1             m_21
C                m_12                m_22
C                m_22                m_23
C                m_32                 .
C                 .                   .
C                 .                  m_2ncol
C                 .                   .
C                m_nrow2
C                 .                   .
C
C                 .                   .
C
C                 .                   .
C                m_1ncol
C                m_2ncol             m_nrow1
C                m_3ncol             m_nrow2
C                 .                  m_nrow3
C                 .                   .
C                 .                   .
C                m_nrowncol          m_nrowncol
C
C
C     For those familiar with permutations, this algorithm relies
C     upon the fact that the transposition of a matrix, which has
C     been stored as a 1-dimensional array, is simply the action of a
C     permutation applied to that array.  Since any permutation
C     can be decomposed as a product of disjoint cycles, it is
C     possible to transpose the matrix with only one additional
C     storage register.  However, once a cycle has been computed
C     it is necessary to find the next entry in the array that
C     has not been moved by the permutation.  For this reason the
C     algorithm is slower than would be necessary if the numbers
C     of rows and columns were known in advance.
C
C$ Examples
C
C     This routine is provided for situation where it is convenient to
C     transpose a general two-dimensional matrix
C     in place rather than store the result in a
C     separate array.  Note that the call
C
C        CALL XPOSEG ( MATRIX, NROW, NCOL, MATRIX )
C
C     is not permitted by the ANSI Fortran 77 standard; this routine
C     can be called instead to achieve the same result.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 19-SEP-2006 (EDW)
C
C        Initial version date unknown. Version data entry
C        added this date.
C

C
C-&
 
C$ Index_Entries
C
C     transpose a matrix general
C
C-&
 

C
C     Local Variables
C
      DOUBLE PRECISION      SOURCE
      DOUBLE PRECISION      TEMP
 
      INTEGER               DEST
      INTEGER               K
      INTEGER               M
      INTEGER               MOVED
      INTEGER               N
      INTEGER               NMOVES
      INTEGER               R
      INTEGER               START
 
 
C
C     Take care of dumb cases first.
C
      IF ( ( NROW .LE. 0 ) .OR. ( NCOL .LE. 0 ) ) THEN
         RETURN
      END IF
 
C
C     Use the abbreviations N and M for NROW and NCOL.
C     
      N      = NROW
      M      = NCOL
 
C
C     Set up the upper bound for the number of objects to be moved and
C     initialize the counters.
C
      NMOVES = N*M - 2
      MOVED  = 0
      START  = 1
 
C
C     Until MOVED is equal to NMOVES, there is some matrix element that
C     has not been moved to its proper location in the transpose matrix.
C
      DO WHILE ( MOVED .LT. NMOVES )

         SOURCE  =  MATRIX( START )
         K       =  START / N
         R       =  START - N*K
         DEST    =  R*M   + K
 
C
C        Perform this cycle of the permutation.  We will be done when
C        the destination of the next element is equal to the starting
C        position of the first element to be moved in this cycle.
C
         DO WHILE ( DEST .NE. START )

            TEMP           = MATRIX( DEST )
            MATRIX( DEST ) = SOURCE
            SOURCE         = TEMP
 
            MOVED          = MOVED + 1
            K              = DEST  / N
            R              = DEST  - K*N
            DEST           = M*R   + K
 
         END DO
 
         MATRIX( DEST ) = SOURCE
         DEST           = 0
         MOVED          = MOVED + 1
 
C
C        Find the next element of the matrix that has not already been
C        moved by the transposition operation.
C
         IF ( MOVED .LT. NMOVES ) THEN
 
            DO WHILE ( DEST .NE. START )

               START = START + 1
               K     = START / N
               R     = START - K*N
               DEST  = R*M   + K
 
               DO WHILE ( DEST .GT. START )

                  K     = DEST / N
                  R     = DEST - K*N
                  DEST  = M*R  + K

               END DO
 
            END DO
 
         END IF
 
      END DO

      RETURN
      END
