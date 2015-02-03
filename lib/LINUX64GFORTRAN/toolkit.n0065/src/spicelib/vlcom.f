C$Procedure      VLCOM ( Vector linear combination, 3 dimensions )
 
      SUBROUTINE VLCOM ( A, V1, B, V2, SUM )
 
C$ Abstract
C
C      Compute a vector linear combination of two double precision,
C      3-dimensional vectors.
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
C      VECTOR
C
C$ Declarations
 
      DOUBLE PRECISION   A
      DOUBLE PRECISION   V1  ( 3 )
      DOUBLE PRECISION   B
      DOUBLE PRECISION   V2  ( 3 )
      DOUBLE PRECISION   SUM ( 3 )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      A          I   Coefficient of V1
C      V1         I   Vector in 3-space
C      B          I   Coefficient of V2
C      V2         I   Vector in 3-space
C      SUM        O   Linear Vector Combination A*V1 + B*V2
C
C$ Detailed_Input
C
C      A     This double precision variable multiplies V1.
C      V1    This is an arbitrary, double precision 3-dimensional
C            vector.
C      B     This double precision variable multiplies V2.
C      V2    This is an arbitrary, double precision 3-dimensional
C            vector.
C
C$ Detailed_Output
C
C      SUM   is an arbitrary, double precision 3-dimensional vector
C            which contains the linear combination A*V1 + B*V2.
C
C$ Parameters
C
C      None.
C
C$ Particulars
C
C      For each index from 1 to 3, this routine implements in FORTRAN
C      code the expression:
C
C      SUM(I) = A*V1(I) + B*V2(I)
C
C      No error checking is performed to guard against numeric overflow.
C
C$ Examples
C
C      To generate a sequence of points on an ellipse with major
C      and minor axis vectors MAJOR and MINOR, one could use the
C      following code fragment
C
C            STEP = TWOPI()/ N
C            ANG  = 0.0D0
C
C            DO I = 0,N
C
C               CALL VLCOM ( DCOS(ANG),MAJOR,  DSIN(ANG),MINOR,  POINT )
C
C               do something with the ellipse point just constructed
C
C               ANG = ANG + STEP
C
C            END DO
C
C      As a second example, suppose that U and V are orthonormal vectors
C      that form a basis of a plane. Moreover suppose that we wish to
C      project a vector X onto this plane, we could use the following
C      call inserts this projection into PROJ.
C
C            CALL VLCOM ( VDOT(X,V),V,   VDOT(X,U),U,    PROJ )
C
C
C$ Restrictions
C
C      No error checking is performed to guard against numeric overflow
C      or underflow.  The user is responsible for insuring that the
C      input values are reasonable.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C      None
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
C
C-&
 
C$ Index_Entries
C
C     linear combination of two 3-dimensional vectors
C
C-&
 
 
C$ Revisions
C
C-     Beta Version 1.0.1, 1-Feb-1989 (WLT)
C
C      Example section of header upgraded.
C
C-&
 
 
      SUM(1) = A*V1(1) + B*V2(1)
      SUM(2) = A*V1(2) + B*V2(2)
      SUM(3) = A*V1(3) + B*V2(3)
 
      RETURN
      END
