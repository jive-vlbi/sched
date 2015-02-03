C$Procedure  DET  ( Determinant of a double precision 3x3 matrix )
 
      DOUBLE PRECISION FUNCTION  DET ( M1 )
 
C$ Abstract
C
C      Compute the determinant of a double precision 3x3 matrix.
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
C      MATRIX,  MATH
C
C$ Declarations
 
      DOUBLE PRECISION    M1 ( 3,3 )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C       M1        I     Matrix whose determinant is to be found.
C
C$ Detailed_Input
C
C      M1      This variable may be any double precision, 3x3 matrix.
C
C$ Detailed_Output
C
C      DET   This is the value of the determinant found by direct
C            application of the definition of the determinant.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      DET calculates the determinant of M1 in a single arithmetic
C      expression which is, effectively, the expansion of M1 about its
C      first row.  Since the calculation of the determinant involves
C      the multiplication of numbers whose magnitudes are unrestricted,
C      there is the possibility of floating point overflow or underflow.
C      NO error checking or recovery is implemented in this routine.
C
C$ Examples
C
C           | 1  2  3 |
C      M1 = | 4  5  6 |   ---->   DET(M1) = 0
C           | 7  8  9 |
C
C           | 1  2  3 |
C      M1 = | 0  5  6 |   ---->   DET(M1) = 45
C           | 0  0  9 |
C
C$ Restrictions
C
C      No checking is implemented to determine whether M1 will cause
C      overflow or underflow in the process of calculating the
C      determinant.  In most cases, this will not pose a problem.
C      The user is required to determine if M1 is suitable matrix
C      for DET to operate on.
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
C     determinant of a d.p. 3x3_matrix
C
C-&
 
      DET =   M1(1,1) * ( M1(2,2)*M1(3,3) - M1(2,3)*M1(3,2) )
     .      - M1(1,2) * ( M1(2,1)*M1(3,3) - M1(2,3)*M1(3,1) )
     .      + M1(1,3) * ( M1(2,1)*M1(3,2) - M1(2,2)*M1(3,1) )
C
      RETURN
      END
