C$Procedure  MTXVG ( Matrix transpose times vector, general dimension )
 
      SUBROUTINE MTXVG ( M1, V2, NC1, NR1R2, VOUT )
 
C$ Abstract
C
C      Multiply the transpose of a matrix and a vector of
C      arbitrary size.
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
C      MATRIX,  VECTOR
C
C$ Declarations
 
      INTEGER            NC1
      INTEGER            NR1R2
      DOUBLE PRECISION   M1   ( NR1R2,NC1 )
      DOUBLE PRECISION   V2   ( NR1R2     )
      DOUBLE PRECISION   VOUT (       NC1 )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C       M1        I     Left-hand matrix whose transpose is to be
C                       multiplied.
C       V2        I     Right-hand vector to be multiplied.
C       NC1       I     Column dimension of M1 and length of VOUT.
C       NR1R2     I     Row dimension of M1 and length of V2.
C       VOUT      O     Product vector M1**T * V2.
C                       VOUT must NOT overwrite either M1 or V2.
C
C$ Detailed_Input
C
C      M1      This is a double precision matrix of arbitrary size whose
C              transpose forms the left-hand matrix of the
C              multiplication.
C
C      V2      This is a double precision vector on the right of the
C              multiplication.
C
C      NC1     This is the column dimension of M1 and length of VOUT.
C
C      NR1R2   This is the row dimension of M1 and length of V2.
C
C$ Detailed_Output
C
C      VOUT    This is the double precision vector which results from
C              the expression
C
C                         T
C              VOUT = (M1)  x V2
C
C              where the T denotes the transpose of M1.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      The code reflects precisely the following mathematical expression
C
C      For each value of the subscript I from 1 to NC1,
C
C      VOUT(I) = Summation from K=1 to NR1R2 of  ( M1(K,I) * V2(K) )
C
C      Note that the reversal of the K and I subscripts in the left-hand
C      matrix M1 is what makes VOUT the product of the TRANSPOSE of M1
C      and not simply of M1 itself.
C
C      Since this subroutine operates on matrices of arbitrary size, it
C      is not feasible to buffer intermediate results.  Thus, VOUT
C      should NOT overwrite either M1 or V2.
C
C$ Examples
C
C                        | 1  2 |
C      Suppose that M1 = | 1  3 |
C                        | 1  4 |
C
C                        | 1 |
C      and that     V2 = | 2 |
C                        | 3 |
C
C      Then calling MTXVG according to the following calling sequence
C
C      CALL MTXVG (M1, V2, 2, 3, VOUT)
C
C      will yield the following vector value for VOUT
C
C      VOUT = | 6  |
C             | 20 |
C
C$ Restrictions
C
C      1) The user is responsible for checking the magnitudes of the
C      elements of M1 and V2 so that a floating point overflow does
C      not occur.
C      2) VOUT not overwrite M1 or V2 or else the intermediate
C      will affect the final result.
C
C$ Exceptions
C
C      Error free.
C
C$ Files
C
C      None.
C
C$ Author_and_Institution
C
C      W.M. Owen       (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
C
C-&
 
C$ Index_Entries
C
C     matrix_transpose times n-dimensional vector
C
C-&
 
 
 
C
C     Local variables
C
      DOUBLE PRECISION SUM
      INTEGER          I,K
 
 
C
C  Perform the matrix-vector multiplication
C
      DO I = 1, NC1
 
         SUM = 0.D0
 
         DO K = 1, NR1R2
            SUM = SUM + M1(K,I)*V2(K)
         END DO
 
         VOUT(I) = SUM
 
      END DO
 
      RETURN
      END
