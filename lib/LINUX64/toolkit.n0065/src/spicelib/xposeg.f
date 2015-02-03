C$Procedure      XPOSEG ( Transpose a matrix, general )
 
      SUBROUTINE XPOSEG ( MATRIX, NROW, NCOL, XPOSEM )
 
C$ Abstract
C
C     Transpose a matrix of arbitrary size (the matrix
C     need not be square).
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
 
      DOUBLE PRECISION   MATRIX ( 0:* )
      INTEGER            NROW
      INTEGER            NCOL
      DOUBLE PRECISION   XPOSEM ( 0:* )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     MATRIX     I   Matrix to be transposed.
C     NROW       I   Number of rows of input matrix M1.
C     NCOL       I   Number of columns of input matrix M1.
C     XPOSEM     O   Transposed matrix.
C
C$ Detailed_Input
C
C     MATRIX     Matrix to be transposed.
C
C     NROW       Number of rows of input matrix M1.
C
C     NCOL       Number of columns of input matrix M1.
C
C$ Detailed_Output
C
C     XPOSEM     O   Transposed matrix.
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
C     This routine transposes the input matrix MATRIX and writes the
C     result to the matrix XPOSEM.  This algorithm is performed in
C     such a way that the transpose can be performed in place.  That
C     is, MATRIX and XPOSEM can use the same storage area in memory.
C
C     NOTE:  The matrices MATRIX and XPOSEM are declared
C            one-dimensional for computational purposes only.  The
C            calling program should declare them as MATRIX(NROW,NCOL)
C            and XPOSEM(NCOL,NROW).
C
C            This routine works on the assumption that the input and
C            output matrices are defined as described above.  More
C            specifically it assuses that the elements of the matrix
C            to be transformed is stored in contiguous memory locations
C            as shown here.  On output these elements will be
C            rearranged in consecutive memory locations as shown.
C
C               MATRIX              XPOSEM
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
C     been stored as a string, is simply the action of a
C     permutation applied to that string.  Since any permutation
C     can be decomposed as a product of disjoint cycles, it is
C     possible to transpose the matrix with only one additional
C     storage register.  However, once a cycle has been computed
C     it is necessary to find the next entry in the string that
C     has not been moved by the permutation.  For this reason the
C     algorithm is slower than would be necessary if the numbers
C     of rows and columns were known in advance.
C
C$ Examples
C
C     This routine is primarily useful when attempting to transpose
C     large matrices, where inplace transposition is important.  For
C     example suppose you have the following declarations
C
C         DOUBLE PRECISION      MATRIX ( 1003, 800  )
C
C     If the transpose of the matrix is needed, it may not be
C     possible to fit a second matrix requiring the same storage
C     into memory.  Instead declare XPOSEM as below and use
C     an equivalence so that the same area of memory is allocated.
C
C         DOUBLE PRECISION      XPOSEM (  800, 1003 )
C         EQUIVALENCE         ( MATRIX (1,1), XPOSEM(1,1) )
C
C     To obtain the transpose simply execute
C
C         CALL XPOSEG ( MATRIX, 1003, 800, XPOSEM )
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
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.3, 22-APR-2010 (NJB)
C
C        Header correction: assertions that the output
C        can overwrite the input have been removed.
C
C-    SPICELIB Version 1.2.2, 4-MAY-1993 (HAN)
C
C        The example listed arguments in the call to XPOSEG incorrectly.
C        The number of rows was listed as the number of columns, and
C        the number of columns was listed as the number of rows.
C
C-    SPICELIB Version 1.2.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.2.0,  6-AUG-1990 (WLT)
C
C        The original version of the routine had a bug.  It worked
C        in place, but the fixed points (1,1) and (n,m) were not
C        moved so that the routine did not work if input and output
C        matrices were different.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
C
C-&
 
C$ Index_Entries
C
C     transpose a matrix general
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 1.2.0,  6-AUG-1990 (WLT)
C
C        The original version of the routine had a bug.  It worked
C        in place, but the fixed points (1,1) and (n,m) were not
C        moved so that the routine did not work if input and output
C        matrices were different.
C
C-    Beta Version 1.1.0, 17-FEB-1989 (WLT) (NJB)
C
C        Example section of header upgraded.  Declarations of unused
C        variables I, J, and COUNT were removed.  Case of negative
C        matrix dimensions now is handled.
C
C-&
 
 
C
C     Local Variables
C
      DOUBLE PRECISION TEMP
      DOUBLE PRECISION SOURCE
 
      INTEGER          DEST
      INTEGER          MOVED
      INTEGER          START
 
      INTEGER          K
      INTEGER          NMOVES
      INTEGER          R
      INTEGER          M
      INTEGER          N
 
 
 
C
C     Take care of dumb cases first.
C
      IF (( NROW .LE. 0 ) .OR. ( NCOL .LE. 0 )) THEN
         RETURN
      END IF
 
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
      DO WHILE (MOVED .LT. NMOVES)
         SOURCE  = MATRIX(START)
         K       = START / N
         R       = START - N*K
         DEST    = R*M   + K
 
C
C        Perform this cycle of the permutation.  We will be done when
C        the destination of the next element is equal to the starting
C        position of the first element to be moved in this cycle.
C
         DO WHILE (DEST .NE. START)
            TEMP         = MATRIX(DEST)
            XPOSEM(DEST) = SOURCE
            SOURCE       = TEMP
 
            MOVED        = MOVED + 1
            K            = DEST  / N
            R            = DEST  - K*N
            DEST         = M*R   + K
 
         END DO
 
         XPOSEM(DEST) = SOURCE
         DEST         = 0
         MOVED        = MOVED + 1
 
 
C
C        Find the next element of the matrix that has not already been
C        moved by the transposition operation.
C
         IF (MOVED .LT. NMOVES) THEN
 
            DO WHILE (DEST .NE. START)
               START = START + 1
               K     = START / N
               R     = START - K*N
               DEST  = R*M   + K
 
               DO WHILE ( DEST .GT. START)
                  K     = DEST / N
                  R     = DEST - K*N
                  DEST  = M*R  + K
               END DO
 
            END DO
 
         END IF
 
      END DO
 
C
C     Just in case this isn't an in-place transpose, move the last
C     element of MATRIX to XPOSEM
C
      XPOSEM(0)     = MATRIX(0    )
      XPOSEM(N*M-1) = MATRIX(N*M-1)
 
      RETURN
      END
