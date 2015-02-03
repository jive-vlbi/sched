C$Procedure  VTMV ( Vector transpose times matrix times vector, 3 dim )
 
      DOUBLE PRECISION FUNCTION VTMV ( V1, MATRIX, V2 )
 
C$ Abstract
C
C      Multiply the transpose of a 3-dimensional column vector,
C      a 3x3 matrix, and a 3-dimensional column vector.
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
C      None.
C
C$ Keywords
C
C      MATRIX,  VECTOR
C
C$ Declarations
 
      DOUBLE PRECISION   V1     (   3 )
      DOUBLE PRECISION   MATRIX ( 3,3 )
      DOUBLE PRECISION   V2     (   3 )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C       V1        I     3 dimensional double precision column vector.
C       MATRIX    I     3x3 double precision matrix.
C       V2        I     3 dimensional double precision column vector.
C
C       The function returns the result of (V1**T * MATRIX * V2 ).
C
C$ Detailed_Input
C
C      V1         This may be any 3-dimensional, double precision
C                 column vector.
C
C      MATRIX     This may be any 3x3, double precision matrix.
C
C      V2         This may be any 3-dimensional, double precision
C                 column vector.
C
C$ Detailed_Output
C
C      The function returns the double precision value of the equation
C      (V1**T * MATRIX * V2 ).
C
C      Notice that VTMV is actually the dot product of the vector
C      resulting from multiplying the transpose of V1 and MATRIX and the
C      vector V2.
C
C$ Parameters
C
C      None.
C
C$ Particulars
C
C      This routine implements the following vector/matrix/vector
C      multiplication:
C
C                      T
C         VTMV = |   V1   | |          |  |  |
C                           |  MATRIX  |  |V2|
C                           |          |  |  |
C
C      V1 is a column vector which becomes a row vector when transposed.
C      V2 is a column vector.
C
C      No checking is performed to determine whether floating point
C      overflow has occurred.
C
C$ Examples
C
C      If  V1 = | 2.0D0 |   MATRIX = |  0.0D0  1.0D0  0.0D0 |
C               |       |            |                      |
C               | 4.0D0 |            | -1.0D0  0.0D0  0.0D0 |
C               |       |            |                      |
C               | 6.0D0 |            |  0.0D0  0.0D0  1.0D0 |
C
C          V2 = | 1.0D0 |
C               |       |
C               | 1.0D0 |
C               |       |
C               | 1.0D0 |
C
C      then function value is equal to 4.0D0.
C
C$ Restrictions
C
C      None.
C
C$ Exceptions
C
C     Error free.
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
C     3-dimensional vector_transpose times matrix times vector
C
C-&
 
      INTEGER K,L
 
      VTMV = 0.D0
      DO K=1,3
         DO L=1,3
            VTMV = VTMV + V1(K) * MATRIX(K,L) * V2(L)
         END DO
      END DO
 
      RETURN
      END
