C$Procedure      VLCOM3 ( Vector linear combination, 3 dimensions )
 
      SUBROUTINE VLCOM3 ( A, V1, B, V2, C, V3, SUM )
 
C$ Abstract
C
C      This subroutine computes the vector linear combination
C      A*V1 + B*V2 + C*V3 of double precision, 3-dimensional vectors.
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
C      VECTOR
C
C$ Declarations
 
      DOUBLE PRECISION   A
      DOUBLE PRECISION   V1  ( 3 )
      DOUBLE PRECISION   B
      DOUBLE PRECISION   V2  ( 3 )
      DOUBLE PRECISION   C
      DOUBLE PRECISION   V3  ( 3 )
      DOUBLE PRECISION   SUM ( 3 )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      A          I   Coefficient of V1
C      V1         I   Vector in 3-space
C      B          I   Coefficient of V2
C      V2         I   Vector in 3-space
C      C          I   Coefficient of V3
C      V3         I   Vector in 3-space
C      SUM        O   Linear Vector Combination A*V1 + B*V2 + C*V3
C
C$ Detailed_Input
C
C      A     is a double precision number.
C
C      V1    is a double precision 3-dimensional vector.
C
C      B     is a double precision number.
C
C      V2    is a double precision 3-dimensional vector.
C
C      C     is a double precision number.
C
C      V3    is a double precision 3-dimensional vector.
C
C$ Detailed_Output
C
C      SUM   is a double precision 3-dimensional vector which contains
C            the linear combination A*V1 + B*V2 + C*V3
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
C      For each index from 1 to 3, this routine implements in FORTRAN
C      code the expression:
C
C      SUM(I) = A*V1(I) + B*V2(I) + C*V3(I)
C
C      No error checking is performed to guard against numeric overflow.
C
C$ Examples
C
C      Often one has the components (A,B,C) of a vector in terms
C      of a basis V1, V2, V3.  The vector represented by (A,B,C) can
C      be obtained immediately from the call
C
C      CALL VLCOM3 ( A, V1, B, V2, C, V3,   VECTOR )
C
C$ Restrictions
C
C      None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 1-NOV-1990 (WLT)
C
C-&
 
C$ Index_Entries
C
C     linear combination of three 3-dimensional vectors
C
C-&
 
 
      SUM(1) = A*V1(1) + B*V2(1) + C*V3(1)
      SUM(2) = A*V1(2) + B*V2(2) + C*V3(2)
      SUM(3) = A*V1(3) + B*V2(3) + C*V3(3)
 
      RETURN
      END
