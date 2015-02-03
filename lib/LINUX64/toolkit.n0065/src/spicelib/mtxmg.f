C$Procedure   MTXMG ( Matrix transpose times matrix, general dimension )
 
      SUBROUTINE MTXMG ( M1, M2, NC1, NR1R2, NC2, MOUT )
 
C$ Abstract
C
C      Multiply the transpose of a matrix with another matrix,
C      both of arbitrary size. (The dimensions of the matrices must be
C      compatible with this multiplication.)
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
C      MATRIX
C
C$ Declarations
 
      INTEGER            NC1
      INTEGER            NR1R2
      INTEGER            NC2
      DOUBLE PRECISION   M1   ( NR1R2,NC1 )
      DOUBLE PRECISION   M2   ( NR1R2,NC2 )
      DOUBLE PRECISION   MOUT ( NC1,  NC2 )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C       M1        I     Left-hand matrix whose transpose is to be
C                       multiplied.
C       M2        I     Right-hand matrix to be multiplied.
C       NC1       I     Column dimension of M1 and row dimension of
C                       MOUT.
C       NR1R2     I     Row dimension of M1 and row dimension of M2.
C       NC2       I     Column dimension of M2 and column dimension of
C                       MOUT.
C       MOUT      O     Product matrix M1**T * M2.
C                       MOUT must NOT overwrite either M1 or M2.
C
C$ Detailed_Input
C
C      M1      This is an double precision matrix of arbitrary dimension
C              whose transpose is the left hand multiplier of a matrix
C              multiplication.
C      M2      This is an double precision matrix of arbitrary dimension
C              whose transpose is the left hand multiplier of a matrix
C              multiplication.
C      NC1     This is the column dimension of M1 and row dimension of
C              MOUT.
C      NR1R2   This is the row dimension of both M1 and M2.
C      NC2     This is the column dimension of both M2 and MOUT.
C
C$ Detailed_Output
C
C      MOUT is a double precision matrix containing the product
C
C                        T
C             MOUT = (M1)   x (M2)
C
C      where the superscript T denotes the transpose of M1.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      The code reflects precisely the following mathematical expression
C
C      For each value of the subscript I from 1 to NC1, and J from 1
C      to NC2:
C
C      MOUT(I,J) = Summation from K=1 to NR1R2 of  ( M1(K,I) * M2(K,J) )
C
C      Note that the reversal of the K and I subscripts in the left-hand
C      matrix M1 is what makes MOUT the product of the TRANSPOSE of M1
C      and not simply of M1 itself.
C
C      Since this subroutine operates on matrices of arbitrary size, it
C      is not possible to buffer intermediate results.  Thus, MOUT
C      should NOT overwrite either M1 or M2.
C
C$ Examples
C
C      Suppose that M1 = | 1  2  3  0 |
C                        | 1  1  1  1 |
C
C      and that     M2 = | 1  2  3 |
C                        | 0  0  0 |
C
C      Then calling MTXMG according to the following calling sequence
C
C      CALL MTXMG (M1, M2, 4, 2, 3, MOUT)
C
C      will yield the following value for MOUT
C
C             | 1  2  3 |
C      MOUT = | 2  4  6 |
C             | 3  6  9 |
C             | 0  0  0 |
C
C$ Restrictions
C
C      1) The user is responsible for checking the magnitudes of the
C      elements of M1 and M2 so that a floating point overflow does
C      not occur.
C      2) MOUT must not overwrite M1 or M2 or else the intermediate
C      will affect the final result.
C
C$ Exceptions
C
C      Error free.
C
C$ Files
C
C      None
C
C$ Author_and_Institution
C
C      N.J. Bachman    (JPL)
C      W.M. Owen       (JPL)
C
C$ Literature_References
C
C      None
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
C     matrix_transpose times matrix n-dimensional_case
C
C-&
 
 
 
C$ Revisions
C
C-     Beta Version 1.1.0, 16-FEB-1989 (NJB)
C
C         Contents of the Exceptions section was changed
C         to "error free" to reflect the decision that the
C         module will never participate in error handling.
C
C         Declaration of unused variable SUM removed.
C
C-&
 
      INTEGER               I
      INTEGER               J
      INTEGER               K
 
C
C  Perform the matrix multiplication
C
      DO I = 1, NC1
         DO J = 1, NC2
 
            MOUT(I,J) = 0.D0
            DO K = 1, NR1R2
               MOUT(I,J) = MOUT(I,J) + M1(K,I)*M2(K,J)
            END DO
 
         END DO
      END DO
C
      RETURN
      END
