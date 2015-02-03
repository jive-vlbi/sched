C$Procedure      INVERT ( Invert a 3x3 matrix )
 
      SUBROUTINE INVERT ( M1, MOUT )
 
C$ Abstract
C
C      Generate the inverse of a 3x3 matrix.
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
C     MATRIX,  MATH
C
C$ Declarations
 
      DOUBLE PRECISION   M1   ( 3,3 )
      DOUBLE PRECISION   MOUT ( 3,3 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     M1         I   Matrix to be inverted.
C     MOUT       O   Inverted matrix (M1)**-1.  If M1 is singular, then
C                    MOUT will be the zero matrix. 
C
C$ Detailed_Input
C
C     M1    An arbitrary 3x3 matrix.  The limits on the size of
C           elements of M1 are determined by the process of calculating
C           the cofactors of each element of the matrix.  For a 3x3
C           matrix this amounts to the differencing of two terms, each
C           of which consists of the multiplication of two matrix
C           elements.  This multiplication must not exceed the range of
C           double precision numbers or else an overflow error will
C           occur.
C
C$ Detailed_Output
C
C     MOUT  is the inverse of M1 and is calculated explicitly using
C           the matrix of cofactors.  MOUT is set to be the zero matrix
C           if M1 is singular.
C
C$ Parameters
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
C$ Particulars
C
C     First the determinant is explicitly calculated using the
C     fundamental definition of the determinant.  If this value is less
C     that 10**-16 then the matrix is deemed to be singular and the
C     output value is filled with zeros.  Otherwise, the output matrix
C     is calculated an element at a time by generating the cofactor of
C     each element.  Finally, each element in the matrix of cofactors
C     is multiplied by the reciprocal of the determinant and the result
C     is the inverse of the original matrix.
C
C     NO INTERNAL CHECKING ON THE INPUT MATRIX M1 IS PERFORMED EXCEPT
C     ON THE SIZE OF ITS DETERMINANT.  THUS IT IS POSSIBLE TO GENERATE
C     A FLOATING POINT OVERFLOW OR UNDERFLOW IN THE PROCESS OF
C     CALCULATING THE MATRIX OF COFACTORS.
C
C$ Examples
C
C     Suppose that M1 is given by the following matrix equation:
C
C             | 0   -1    0 |
C        M1 = | 0.5  0    0 |  
C             | 0    0    1 |   
C
C     If INVERT is called according to the FORTRAN code:
C
C        CALL INVERT (M1, M1)
C
C     then M1 will be set to be:
C
C             | 0    2    0 |
C        M1 = |-1    0    0 |
C             | 0    0    1 |
C
C$ Restrictions
C
C     The input matrix must be such that generating the cofactors will
C     not cause a floating point overflow or underflow.  The strictness
C     of this condition depends, of course, on the computer
C     installation and the resultant maximum and minimum values of
C     double precision numbers.
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
C-    SPICELIB Version 1.0.2, 22-APR-2010 (NJB)
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
C     invert a 3x3_matrix
C
C-&
 
      DOUBLE PRECISION   DET
      DOUBLE PRECISION   MTEMP(3,3)
      DOUBLE PRECISION   MDET
      DOUBLE PRECISION   INVDET
C
C  Find the determinant of M1 and check for singularity
C
      MDET = DET(M1)
      IF ( DABS(MDET) .LT. 1.D-16 ) THEN
         CALL FILLD ( 0.D0, 9, MOUT )
         RETURN
      END IF
C
C  Get the cofactors of each element of M1
C
      MTEMP(1,1) =  (M1(2,2)*M1(3,3) - M1(3,2)*M1(2,3))
      MTEMP(1,2) = -(M1(1,2)*M1(3,3) - M1(3,2)*M1(1,3))
      MTEMP(1,3) =  (M1(1,2)*M1(2,3) - M1(2,2)*M1(1,3))
      MTEMP(2,1) = -(M1(2,1)*M1(3,3) - M1(3,1)*M1(2,3))
      MTEMP(2,2) =  (M1(1,1)*M1(3,3) - M1(3,1)*M1(1,3))
      MTEMP(2,3) = -(M1(1,1)*M1(2,3) - M1(2,1)*M1(1,3))
      MTEMP(3,1) =  (M1(2,1)*M1(3,2) - M1(3,1)*M1(2,2))
      MTEMP(3,2) = -(M1(1,1)*M1(3,2) - M1(3,1)*M1(1,2))
      MTEMP(3,3) =  (M1(1,1)*M1(2,2) - M1(2,1)*M1(1,2))
C
C  Multiply the cofactor matrix by 1/MDET to obtain the inverse
C
      INVDET = 1.D0 / MDET
      CALL VSCLG (INVDET, MTEMP, 9, MOUT)
C
      RETURN
      END
