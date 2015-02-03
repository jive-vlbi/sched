C$Procedure      VSCLG ( Vector scaling, general dimension )
 
      SUBROUTINE VSCLG ( S, V1, NDIM, VOUT )
 
C$ Abstract
C
C     Multiply a scalar and a double precision vector of arbitrary
C     dimension.
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
C     VECTOR
C
C$ Declarations
 
      INTEGER            NDIM
      DOUBLE PRECISION   S
      DOUBLE PRECISION   V1   ( NDIM )
      DOUBLE PRECISION   VOUT ( NDIM )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     S          I   Scalar to multiply a vector.
C     V1         I   Vector to be multiplied.
C     NDIM       I   Dimension of V1 (and also VOUT).
C     VOUT       O   Product vector, S*V1.
C
C$ Detailed_Input
C
C     S      is a double precision scalar.
C
C     V1     is a double precision vector of arbitrary dimension.
C
C     NDIM   is the dimension of V1 (and VOUT).
C
C$ Detailed_Output
C
C     VOUT   is a double precision vector of arbitrary dimension
C            containing the product of the scalar with the vector V1.
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
C     For each value of the index I from 1 to NDIM, this subroutine
C     performs the following multiplication
C
C        VOUT(I) = S * V1(I)
C
C     No error checking is performed to guard against numeric overflow
C     or underflow. 
C
C$ Examples
C
C     The following table shows the results of VSCLG from various
C     inputs.
C
C        V1                 S           NDIM        VOUT
C        ----------------------------------------------------------
C        (1, 2, -3, 4)      3            4         ( 3,  6, -9, 12)
C        (1, 2, -3, 4)      0            4         ( 0,  0,  0,  0)
C        (1, 2, -3, 4)     -1            4         (-3, -6,  9,-12)
C
C$ Restrictions
C
C     No error checking is performed to guard against numeric overflow.
C     The programmer is thus required to insure that the values in V1
C     and S are reasonable and will not cause overflow.
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
C     n-dimensional vector scaling
C
C-&
 
 
      INTEGER I
 
      DO I=1,NDIM
         VOUT(I) = S * V1(I)
      END DO
 
      RETURN
      END
