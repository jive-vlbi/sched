C$Procedure  MXMTG  ( Matrix times matrix transpose, general dimension )
 
      SUBROUTINE MXMTG ( M1, M2, NR1, NC1C2, NR2, MOUT )
 
C$ Abstract
C
C      Multiply a matrix and the transpose of a matrix, both of
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
C      MATRIX
C
C$ Declarations
 
      INTEGER            NR1
      INTEGER            NC1C2
      INTEGER            NR2
      DOUBLE PRECISION   M1   ( NR1,NC1C2 )
      DOUBLE PRECISION   M2   ( NR2,NC1C2 )
      DOUBLE PRECISION   MOUT ( NR1,NR2   )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C       M1        I     Left-hand matrix to be multiplied.
C       M2        I     Right-hand matrix whose transpose is to be
C                       multiplied.
C       NR1       I     Row dimension of M1 and row dimension of MOUT.
C       NC1C2     I     Column dimension of M1 and column dimension of
C                       M2.
C       NR2       I     Row dimension of M2 and column dimension of
C                       MOUT.
C       MOUT      O     Product matrix M1 * M2**T.
C                       MOUT must not overwrite either M1 or M2.
C
C$ Detailed_Input
C
C      M1      M1 may be any double precision matrix of arbitrary size.
C
C      M2      M2 may be any double precision matrix of arbitrary size.
C              The number of columns in M2 must match the number of
C              columns in M1.
C
C      NR1     The number of rows in both M1 and MOUT.
C
C      NC1C2   The number of columns in M1 and (by necessity) the number
C              of columns of M2.
C
C      NR2     The number of rows in both M2 and the number of columns
C              in MOUT.
C
C$ Detailed_Output
C
C      MOUT    This is a double precision matrix of dimension NR1 x NR2.
C                                                                    T
C              MOUT is the product matrix given by MOUT = (M1) x (M2)
C              where the superscript "T" denotes the transpose matrix.
C
C              MOUT must not overwrite M1 or M2.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      The code reflects precisely the following mathematical expression
C
C      For each value of the subscript I from 1 to NR1, and J from 1
C      to NR2:
C
C      MOUT(I,J) = Summation from K=1 to NC1C2 of  ( M1(I,K) * M2(J,K) )
C
C      Notice that the order of the subscripts of M2 are reversed from
C      what they would be if this routine merely multiplied M1 and M2.
C      It is this transposition of subscripts that makes this routine
C      multiply M1 and the TRANPOSE of M2.
C
C      Since this subroutine operates on matrices of arbitrary size, it
C      is not feasible to buffer intermediate results.  Thus, MOUT
C      should NOT overwrite either M1 or M2.
C
C$ Examples
C
C
C     Let M1 = | 1.0D0  2.0D0  3.0D0 |      NR1   = 2
C              |                     |      NC1C2 = 3
C              | 3.0D0  2.0D0  1.0D0 |      NR2   = 4
C
C
C     Let M2 = | 1.0D0  2.0D0  0.0D0 |
C              |                     |
C              | 2.0D0  1.0D0  2.0D0 |
C              |                     |
C              | 1.0D0  2.0D0  0.0D0 |
C              |                     |
C              | 2.0D0  1.0D0  2.0D0 |
C
C      then the call
C
C      CALL MXMTG ( M1, M2, NR1, NC1C2, NR2, MOUT )
C
C      produces the matrix
C
C
C      MOUT = | 5.0D0  10.0D0  5.0D0  10.0D0 |
C             |                              |
C             | 7.0D0  10.0D0  7.0D0  10.0D0 |
C
C
C$ Restrictions
C
C      No error checking is performed to prevent numeric overflow or
C      underflow.
C
C      No error checking is performed to determine if the input and
C      output matrices have, in fact, been correctly dimensioned.
C
C      The user is responsible for checking the magnitudes of the
C      elements of M1 and M2 so that a floating point overflow does
C      not occur.
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
C     matrix times matrix_transpose n-dimensional_case
C
C-&
 
 
 
C
C     Local variables
C
      DOUBLE PRECISION   SUM
      INTEGER            I
      INTEGER            J
      INTEGER            K
 
C
C  Perform the matrix multiplication
C
      DO I = 1, NR1
 
         DO J = 1, NR2
            SUM = 0.D0
 
            DO K = 1, NC1C2
               SUM = SUM + M1(I,K)*M2(J,K)
            END DO
 
            MOUT(I,J) = SUM
 
         END DO
 
      END DO
 
      RETURN
      END
