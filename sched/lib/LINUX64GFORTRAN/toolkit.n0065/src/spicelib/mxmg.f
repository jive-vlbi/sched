C$Procedure      MXMG ( Matrix times matrix, general dimension )
 
      SUBROUTINE MXMG ( M1, M2, ROW1, COL1, COL2, MOUT )
 
C$ Abstract
C
C      Multiply two double precision matrices of arbitrary size.
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
 
      INTEGER             ROW1
      INTEGER             COL1
      INTEGER             COL2
      DOUBLE PRECISION    M1   ( ROW1, COL1 )
      DOUBLE PRECISION    M2   ( COL1, COL2 )
      DOUBLE PRECISION    MOUT ( ROW1, COL2 )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C       M1        I   ROW1xCOL1 double precision matrix.
C       M2        I   COL1xCOL2 double precision matrix.
C       ROW1      I   Row dimension of M1 (and also MOUT).
C       COL1      I   Column dimension of M1 and row dimension of M2.
C       COL2      I   Column dimension of M2 (and also MOUT).
C       MOUT      O   ROW1xCOL2 double precision matrix.
C
C$ Detailed_Input
C
C      M1         is any double precision matrix of arbitrary size.
C
C      M2         is any double precision matrix of arbitrary size.
C                 The number of rows in M2 must match the number of
C                 columns in M1.
C
C      ROW1       is the number of rows in both M1 and MOUT.
C
C      COL1       is the number of columns in M1 and (by necessity)
C                 the number of rows of M2.
C
C      COL2       is the number of columns in both M2 and MOUT.
C
C$ Detailed_Output
C
C      MOUT       is a a double precision matrix of dimension
C                 ROW1 x COL2. MOUT is the product matrix given
C                 by MOUT = (M1) x (M2). MOUT must not overwrite
C                 M1 or M2.
C
C$ Parameters
C
C      None.
C
C$ Examples
C
C      Let M1 = | 1.0D0  4.0D0 |    and  M2 =  | 1.0D0  3.0D0  5.0D0 |
C               |              |               |                     |
C               | 2.0D0  5.0D0 |               | 2.0D0  4.0D0  6.0D0 |
C               |              |
C               | 3.0D0  6.0D0 |
C
C
C      and   ROW1   = 3
C            COL1   = 2
C            COL2   = 3
C
C      Then the call
C
C      CALL MXMG ( M1, M2, ROW1, COL1, COL2, MOUT )
C
C      produces the matrix
C
C      MOUT = |  9.0D0  19.0D0  29.0D0 |
C             |                        |
C             | 12.0D0  26.0D0  40.0D0 |
C             |                        |
C             | 15.0D0  33.0D0  51.0D0 |
C
C$ Particulars
C
C      The code reflects precisely the following mathematical expression
C
C      For each value of the subscript I from 1 to NC1, and J from 1
C      to COL2:
C
C      MOUT(I,J) = Summation from K=1 to ROW1R2 of  ( M1(I,K) * M2(K,J)
C
C      Since this subroutine operates on matrices of arbitrary size, it
C      is not feasible to buffer intermediate results.  Thus, MOUT
C      should NOT overwrite either M1 or M2.
C
C$ Restrictions
C
C      1) No error checking is performed to prevent numeric overflow or
C      underflow.
C
C      2) No error checking performed to determine if the input and
C      output matrices have, in fact, been correctly dimensioned.
C
C      3) MOUT should not overwrite M1 or M2.
C
C$ Exceptions
C
C     Error free.
C
C     1) If COL1 < 1, the elements of the matrix MOUT are set equal to
C        zero.
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
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
C
C-&
 
C$ Index_Entries
C
C     matrix times matrix n-dimensional_case
C
C-&
 
 
 
      DOUBLE PRECISION SUM
      INTEGER I,J,K
C
C  Perform the matrix multiplication
C
      DO I = 1, ROW1
         DO J = 1, COL2
            SUM = 0.D0
            DO K = 1, COL1
               SUM = SUM + M1(I,K)*M2(K,J)
            END DO
            MOUT(I,J) = SUM
         END DO
      END DO
C
      RETURN
      END
