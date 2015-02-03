C$Procedure      MEQU  ( Matrix equal to another, 3x3 )
 
      SUBROUTINE MEQU ( M1, MOUT )
 
C$ Abstract
C
C      Set one double precision 3x3 matrix equal to another.
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
C      ASSIGNMENT,  MATRIX
C
C$ Declarations
 
      DOUBLE PRECISION   M1   ( 3,3 )
      DOUBLE PRECISION   MOUT ( 3,3 )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      M1         I     Input matrix.
C      MOUT       O     Output matrix equal to M1.
C
C$ Detailed_Input
C
C      M1      This is an arbitrary input 3x3 matrix.  There are no
C              restrictions on what it may contain.
C
C$ Detailed_Output
C
C      MOUT    This 3x3 matrix is set to be equal to M1.
C
C$ Parameters
C
C      None.
C
C$ Particulars
C
C      None.
C
C$ Examples
C
C      If  M1 = | 1.0D0   0.0D0   0.0D0 |
C               |                       |
C               | 0.0D0   1.0D0   0.0D0 |
C               |                       |
C               | 0.0D0   0.0D0   1.0D0 |
C
C      the call
C
C      CALL MEQU ( M1, MOUT )
C
C      produces the matrix
C
C       MOUT =  | 1.0D0   0.0D0   0.0D0 |
C               |                       |
C               | 0.0D0   1.0D0   0.0D0 |
C               |                       |
C               | 0.0D0   0.0D0   1.0D0 |
C
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
C     equal to another 3x3_matrix
C
C-&
 
 
      CALL MOVED (M1, 9, MOUT)
C
      RETURN
      END
