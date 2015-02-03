C$Procedure      XPOSE ( Transpose a matrix, 3x3 )
 
      SUBROUTINE XPOSE ( M1, MOUT )
 
C$ Abstract
C
C     Transpose a 3x3 matrix.
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
 
      DOUBLE PRECISION  M1   ( 3,3 )
      DOUBLE PRECISION  MOUT ( 3,3 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     M1        I     Matrix to be transposed.
C     MOUT      O    Transposed matrix (M1)**T.
C
C$ Detailed_Input
C
C     M1      This variable may contain any double precision 3x3
C             matrix.
C
C$ Detailed_Output
C
C     MOUT    This variable is a double precision, 3x3 matrix which
C             contains the transpose of M1.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     XPOSE first copies the diagonal elements of M1 to MOUT.  Then
C     the off-diagonal elements are transposed using a temporary
C     variable in the following order: (1,2) <---> (2,1),
C     (1,3) <---> (3,1) and finally (2,3) <---> (3,2).
C
C$ Examples
C
C     Given below is one example of a matrix M1 with the output matrix
C     MOUT which is implied by M1.
C
C           | 1  2  3 |                | 1  0  0 |
C      M1 = | 0  4  5 |   then  MOUT = | 2  4  6 |
C           | 0  6  0 |                | 3  5  0 |
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
C     W.M. Owen       (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.2, 23-APR-2010 (NJB)
C
C        Header correction: assertions that the output
C        can overwrite the input have been removed.
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
C     transpose a 3x3_matrix
C
C-&
 
      DOUBLE PRECISION TEMP
C
C  Move the three diagonal elements from M1 to MOUT
C
      MOUT(1,1) = M1(1,1)
      MOUT(2,2) = M1(2,2)
      MOUT(3,3) = M1(3,3)
C
C  Switch the three pairs of off-diagonal elements
C
      TEMP = M1(1,2)
      MOUT(1,2) = M1(2,1)
      MOUT(2,1) = TEMP
C
      TEMP = M1(1,3)
      MOUT(1,3) = M1(3,1)
      MOUT(3,1) = TEMP
C
      TEMP = M1(2,3)
      MOUT(2,3) = M1(3,2)
      MOUT(3,2) = TEMP
C
      RETURN
      END
